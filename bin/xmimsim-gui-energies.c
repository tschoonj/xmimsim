/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <config.h>
#include "xmimsim-gui.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-energies.h"
#include "xmimsim-gui-discrete-energy-dialog.h"
#include "xmimsim-gui-continuous-energy-dialog.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include <math.h>
#include "xmi_aux.h"
#include "xmi_private.h"
#include "xmi_ebel.h"
#include <glib/gstdio.h>
#include <search.h>
#include <string.h>


enum {
	DISCRETE,
	CONTINUOUS
};

struct energyWidget {
	int kind;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *scaleButton;
	GtkWidget *clearButton;
	GtkWidget *main_window;
	GtkListStore *store;
	GtkWidget *tree;
};

static int xmi_read_energies_from_ascii_file_discrete(gchar *filename, struct xmi_energy_discrete **energies, unsigned int start_line, unsigned int nlines);
static int xmi_read_energies_from_ascii_file_continuous(gchar *filename, struct xmi_energy_continuous **energies, unsigned int start_line, unsigned int nlines);


void repopulate_discrete_energies(GtkListStore *store, struct xmi_excitation *excitation) {
	GtkTreeIter iter;
	int i;

	gtk_list_store_clear(store);
	for (i = 0 ; i < excitation->n_discrete ; i++) {
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
			ENERGY_COLUMN, excitation->discrete[i].energy,
			HOR_INTENSITY_COLUMN, excitation->discrete[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, excitation->discrete[i].vertical_intensity,
			SIGMA_X_COLUMN, excitation->discrete[i].sigma_x,
			SIGMA_XP_COLUMN, excitation->discrete[i].sigma_xp,
			SIGMA_Y_COLUMN, excitation->discrete[i].sigma_y,
			SIGMA_YP_COLUMN, excitation->discrete[i].sigma_yp,
			DISTRIBUTION_TYPE_COLUMN, excitation->discrete[i].distribution_type,
			SCALE_PARAMETER_COLUMN, excitation->discrete[i].scale_parameter,
			-1);
	}
}

void repopulate_continuous_energies(GtkListStore *store, struct xmi_excitation *excitation) {
	GtkTreeIter iter;
	int i;

	gtk_list_store_clear(store);
	for (i = 0 ; i < excitation->n_continuous ; i++) {
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
			ENERGY_COLUMN, excitation->continuous[i].energy,
			HOR_INTENSITY_COLUMN, excitation->continuous[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, excitation->continuous[i].vertical_intensity,
			SIGMA_X_COLUMN, excitation->continuous[i].sigma_x,
			SIGMA_XP_COLUMN, excitation->continuous[i].sigma_xp,
			SIGMA_Y_COLUMN, excitation->continuous[i].sigma_y,
			SIGMA_YP_COLUMN, excitation->continuous[i].sigma_yp,
			-1);
	}
}


static void energy_print_distribution_type(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gint type;
	gchar *text = NULL;

	gtk_tree_model_get(tree_model,iter, DISTRIBUTION_TYPE_COLUMN, &type,-1);

	if (type == XMI_DISCRETE_MONOCHROMATIC) {
		text = g_strdup("Monochromatic");
	}
	else if (type == XMI_DISCRETE_GAUSSIAN) {
		text = g_strdup("Gaussian");
	}
	else if (type == XMI_DISCRETE_LORENTZIAN) {
		text = g_strdup("Lorentzian");
	}

	g_object_set(G_OBJECT(renderer), "text", text, NULL);

	g_free(text);

	return;
}

static void energy_print_scale_parameter(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gint type;
	gdouble scale_parameter;
	gchar *text = NULL;


	gtk_tree_model_get(tree_model,iter, DISTRIBUTION_TYPE_COLUMN, &type,-1);

	if (type == XMI_DISCRETE_MONOCHROMATIC) {
		text = g_strdup("n/a");
	}
	else if (type == XMI_DISCRETE_GAUSSIAN || type == XMI_DISCRETE_LORENTZIAN) {
		gtk_tree_model_get(tree_model,iter, SCALE_PARAMETER_COLUMN, &scale_parameter,-1);
		text = g_strdup_printf("%g", scale_parameter);
	}

	g_object_set(G_OBJECT(renderer), "text", text, NULL);

	g_free(text);

	return;
}

static void energy_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gdouble value;
	gchar *double_text;

	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value,-1);

	double_text = g_strdup_printf("%g",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);

	return;

}

static void clear_button_clicked_cb(GtkWidget *widget, struct energyWidget *eb) {
	int kind = eb->kind;
	GtkListStore *store = eb->store;

	gtk_list_store_clear(store);

	if (kind == DISCRETE) {
		update_undo_buffer(DISCRETE_ENERGY_CLEAR, NULL);
	}
	else if (kind == CONTINUOUS) {
		update_undo_buffer(CONTINUOUS_ENERGY_CLEAR, NULL);
	}

}

static void scale_entry_changed_cb(GtkWidget *scaleEntry, GtkWidget *okButton) {
	double value;
	char *textPtr,*endPtr,*lastPtr;

	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(scaleEntry));
	value=strtod(textPtr, &endPtr);
	lastPtr = textPtr + strlen(textPtr);

	if (strlen(textPtr) == 0 || lastPtr != endPtr || value <= 0.0) {
		gtk_widget_modify_base(scaleEntry, GTK_STATE_NORMAL, &red);
		gtk_widget_set_sensitive(okButton, FALSE);
	}
	else {
		gtk_widget_modify_base(scaleEntry, GTK_STATE_NORMAL, NULL);
		gtk_widget_set_sensitive(okButton, TRUE);
	}
	return;
}

static void scale_button_clicked_cb(GtkWidget *widget, struct energyWidget *eb) {
	int kind = eb->kind;

	//Launch dialog to select the scale factor
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Intensity scale factor", GTK_WINDOW(eb->main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *hbox = gtk_hbox_new(FALSE, 2);
	GtkWidget *label = gtk_label_new("Intensity scale factor");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	GtkWidget *entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(entry), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox), entry, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(content_area), hbox, FALSE, FALSE,0);
	gtk_container_set_border_width(GTK_CONTAINER(dialog),5);
	gtk_widget_show_all(hbox);
	gtk_widget_set_size_request(dialog, 300, -1);

	GtkWidget *okButton = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
	gtk_widget_set_sensitive(okButton, FALSE);
	g_signal_connect(G_OBJECT(entry), "changed", G_CALLBACK(scale_entry_changed_cb), (gpointer) okButton);

	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	gtk_widget_set_can_default(okButton, TRUE);
	gtk_widget_grab_default(okButton);
	int rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (rv != GTK_RESPONSE_ACCEPT) {
		gtk_widget_destroy(dialog);
		return;
	}

	const char *textPtr;
	double value;
	int i;

	textPtr = gtk_entry_get_text(GTK_ENTRY(entry));
	value = strtod(textPtr, NULL);
	gtk_widget_destroy(dialog);
	GtkTreeIter iter;

	GArray *selected_indices = xmi_msim_gui_utils_tree_view_get_selected_indices(GTK_TREE_VIEW(eb->tree));
	struct energiesUndoInfo *eui = (struct energiesUndoInfo *) g_malloc(sizeof(struct energiesUndoInfo));
	eui->scale_value = value;
	eui->indices = selected_indices;

	if (kind == DISCRETE) {
		update_undo_buffer(DISCRETE_ENERGY_SCALE, (void *) eui);
	}
	else if (kind == CONTINUOUS) {
		update_undo_buffer(CONTINUOUS_ENERGY_SCALE, (void *) eui);
	}

	for (i = 0 ; i < selected_indices->len ; i++){
		GtkTreePath *path = gtk_tree_path_new_from_indices(g_array_index(selected_indices, int, i), -1);
		gtk_tree_model_get_iter(GTK_TREE_MODEL(eb->store), &iter, path);
		gtk_tree_path_free(path);
		double hor_intensity, ver_intensity;
		gtk_tree_model_get(GTK_TREE_MODEL(eb->store), &iter,
			HOR_INTENSITY_COLUMN, &hor_intensity,
			VER_INTENSITY_COLUMN, &ver_intensity,
			-1);
		hor_intensity *= value;
		ver_intensity *= value;
		gtk_list_store_set(eb->store, &iter,
		HOR_INTENSITY_COLUMN, hor_intensity,
		VER_INTENSITY_COLUMN, ver_intensity,
		-1);
	}

	g_array_free(selected_indices, TRUE);
}


static void radio_button_toggled_cb(GtkToggleButton *button, GtkWidget *spinner){
	if (gtk_toggle_button_get_active(button))
		gtk_widget_set_sensitive(spinner, TRUE);
	else
		gtk_widget_set_sensitive(spinner, FALSE);
}

static void import_button_clicked_cb(GtkWidget *widget, struct energyWidget *eb) {
	GtkWidget *main_window = eb->main_window;
	int kind = eb->kind;


	//first launch a message box
	GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Import spectrum from file");
	gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
	"Files must be ascii files consisting of rows with either 2, 3 or 7 elements. "
	"First element must contain the energy (in keV). "
	"Second element must be the intensity: if there are only two elements, it is assumed to be unpolarized. "
	"If three elements are found, then the second and third elements are assumed to correspond to the horizontal and vertical polarized intensities. "
	"Seven elements are considered to be identical to the three elements case with additionally the source size x and y, as well as the source divergence x and y. "
	"Empty lines are ignored."
	);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_CLOSE) {
		gtk_widget_destroy(dialog);
		return;
	}
	gtk_widget_destroy(dialog);

	//open filechooser without filters
	XmiMsimGuiFileChooserDialog *file_dialog = xmi_msim_gui_file_chooser_dialog_new ("Open File",
                 GTK_WINDOW(main_window),
                 GTK_FILE_CHOOSER_ACTION_OPEN,
                 GTK_STOCK_OPEN,
                 GTK_STOCK_CANCEL
                 );

	xmi_msim_gui_file_chooser_dialog_set_modal(file_dialog, TRUE);

	//add widget
	GtkWidget *start_at_begin;
	GtkWidget *start_at_line;
	GtkWidget *start_at_line_spinner;
	GtkWidget *read_all_lines;
	GtkWidget *read_only_lines;
	GtkWidget *read_only_lines_spinner;
	GtkWidget *vbox, *hbox;

	vbox = gtk_vbox_new(FALSE, 1);

	start_at_begin = gtk_radio_button_new_with_label_from_widget(NULL,"Start at first line");
	start_at_line = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(start_at_begin), "Start at line:");
	GtkAdjustment *adj = GTK_ADJUSTMENT(gtk_adjustment_new(1,1,10000,1,10,0));
	start_at_line_spinner = gtk_spin_button_new(adj, 1, 0);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(start_at_line_spinner), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), start_at_line, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), start_at_line_spinner, FALSE, FALSE, 1);
	g_signal_connect(G_OBJECT(start_at_line), "toggled",G_CALLBACK(radio_button_toggled_cb),start_at_line_spinner);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(start_at_begin), TRUE);
	gtk_widget_set_sensitive(start_at_line_spinner, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), start_at_begin, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(), FALSE, FALSE, 1);

	read_all_lines = gtk_radio_button_new_with_label_from_widget(NULL,"Read all lines");
	read_only_lines = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(read_all_lines), "Number of lines to be read:");
	adj = GTK_ADJUSTMENT(gtk_adjustment_new(1,1,10000,1,10,0));
	read_only_lines_spinner = gtk_spin_button_new(adj, 1, 0);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(read_only_lines_spinner), GTK_UPDATE_IF_VALID);
	hbox = gtk_hbox_new(FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), read_only_lines, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), read_only_lines_spinner, FALSE, FALSE, 1);
	g_signal_connect(G_OBJECT(read_only_lines), "toggled",G_CALLBACK(radio_button_toggled_cb),read_only_lines_spinner);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(read_all_lines), TRUE);
	gtk_widget_set_sensitive(read_only_lines_spinner, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), read_all_lines, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

	gtk_widget_show_all(vbox);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(file_dialog), vbox);


	if (xmi_msim_gui_file_chooser_dialog_run(file_dialog) == GTK_RESPONSE_ACCEPT) {
		gchar *filename;

		int start_line, nlines;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(start_at_begin))) {
			start_line = 1;
		}
		else {
			start_line = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(start_at_line_spinner));
		}

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(read_all_lines))) {
			nlines = -1;
		}
		else {
			nlines = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(read_only_lines_spinner));
		}

		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(file_dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
		dialog = NULL;
		int rv;
		struct energiesUndoInfo *eui = (struct energiesUndoInfo *) g_malloc(sizeof(struct energiesUndoInfo));
		
		
		if (kind == DISCRETE)
    			rv = xmi_read_energies_from_ascii_file_discrete(filename, &eui->energy_disc, start_line, nlines);
		else
    			rv = xmi_read_energies_from_ascii_file_continuous(filename, &eui->energy_cont, start_line, nlines);
		if (rv > 0) {
			//success
			g_debug("File %s read in successfully\n", filename);

			eui->n_energy_disc = eui->n_energy_cont = rv;

			//now ask if we have to add or replace...
			int rv2;
			if ((kind == DISCRETE && current->xi->excitation->n_discrete > 0) ||
				(kind == CONTINUOUS && current->xi->excitation->n_continuous > 0)) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "Add spectrum from file to current spectrum or replace it completely?");
				gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_ADD, GTK_RESPONSE_OK, GTK_STOCK_REFRESH, GTK_RESPONSE_CANCEL, NULL);
				GtkWidget *button = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
				xmi_msim_gui_utils_update_button_text(button, "Replace");
				//this may not work on all platforms -> Mac OS X
				gtk_window_set_deletable(GTK_WINDOW(dialog), FALSE);

				rv2 = gtk_dialog_run (GTK_DIALOG (dialog));
			}
			else {
				rv2 = GTK_RESPONSE_CANCEL;
			}
			if (rv2 == GTK_RESPONSE_OK) {
				//add
				int i;
				if (kind == DISCRETE) {
					if (current->xi->excitation->n_discrete > 0) {
						for (i = 0 ; i < current->xi->excitation->n_discrete ; i++) {
							if (bsearch(eui->energy_disc+i, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
								gtk_widget_destroy(dialog);
								dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
								gtk_dialog_run(GTK_DIALOG(dialog));
								gtk_widget_destroy(dialog);
								return;
							}
						}
					}
					update_undo_buffer(DISCRETE_ENERGY_IMPORT_ADD, (void *) eui);
				}
				else if (kind == CONTINUOUS) {
					if (current->xi->excitation->n_continuous > 0) {
						for (i = 0 ; i < current->xi->excitation->n_continuous ; i++) {
							if (bsearch(eui->energy_cont+i, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
								gtk_widget_destroy(dialog);
								dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
								gtk_dialog_run(GTK_DIALOG(dialog));
								gtk_widget_destroy(dialog);
								return;
							}
						}
					}
					update_undo_buffer(CONTINUOUS_ENERGY_IMPORT_ADD, (void *) eui);
				}
			}
			else if (rv2 == GTK_RESPONSE_CANCEL) {
				//replace -> no need to check for duplicates here
				if (kind == DISCRETE) {
					update_undo_buffer(DISCRETE_ENERGY_IMPORT_REPLACE, (void *) eui);
				}
				else if (kind == CONTINUOUS) {
					update_undo_buffer(CONTINUOUS_ENERGY_IMPORT_REPLACE, (void *) eui);
				}
			}
			else {
				gtk_widget_destroy(dialog);
				g_free (filename);
				return;
			}
			if (kind == DISCRETE) {
				struct xmi_excitation exc;
				exc.n_discrete = eui->n_energy_disc;
				exc.discrete = eui->energy_disc;
				repopulate_discrete_energies(eb->store, &exc);
			}
			else if (kind == CONTINUOUS) {
				struct xmi_excitation exc;
				exc.n_continuous = eui->n_energy_cont;
				exc.continuous = eui->energy_cont;
				repopulate_continuous_energies(eb->store, &exc);
			}
			adjust_save_buttons();
			if (dialog)
				gtk_widget_destroy(dialog);
			g_free (filename);

			return;
		}
		else if (rv == 0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "No valid values found in file %s\nNumber of columns must be 2, 3 or 7!\n", filename);
		}
		else if (rv == -1) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not open file %s",filename);
		}
		else if (rv == -2) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not close file %s",filename);
		}
		else if (rv == -3) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Syntax error in file %s\nNumber of columns must be 2, 3 or 7!\n", filename);
		}
		else if (rv == -4) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Duplicate energies found in %s. The energies of the lines or intervals in the first column must be unique\n", filename);
		}
		else if (rv == -5) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Intensity problem detected in %s. Two consecutive intensity densities cannot both have a total intensity of zero\n", filename);
		}
		g_free (filename);
		gtk_dialog_run(GTK_DIALOG(dialog));
  	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
	}
	return;
}

static void energy_selection_changed_cb (GtkTreeSelection *selection, struct energyWidget *eb) {
	int nselected = gtk_tree_selection_count_selected_rows(selection);

	switch (nselected) {
		case 0:
			gtk_widget_set_sensitive(eb->deleteButton,FALSE);
			gtk_widget_set_sensitive(eb->editButton,FALSE);
			gtk_widget_set_sensitive(eb->scaleButton,FALSE);
			break;
		case 1:
			gtk_widget_set_sensitive(eb->editButton,TRUE);
			gtk_widget_set_sensitive(eb->deleteButton,TRUE);
			gtk_widget_set_sensitive(eb->scaleButton,TRUE);
			break;
		default:
			gtk_widget_set_sensitive(eb->deleteButton,TRUE);
			gtk_widget_set_sensitive(eb->scaleButton,TRUE);
			gtk_widget_set_sensitive(eb->editButton,FALSE);
	}

	return;
}

static void row_deleted_or_inserted_cb(GtkTreeModel *tree_model, struct energyWidget *eb) {
	gint kids = gtk_tree_model_iter_n_children(tree_model, NULL);

	if (kids > 0) {
		gtk_widget_set_sensitive(eb->clearButton, TRUE);
	}
	else {
		gtk_widget_set_sensitive(eb->clearButton, FALSE);
	}
}

static void row_deleted_cb (GtkTreeModel *tree_model, GtkTreePath  *path, struct energyWidget *eb) {
       row_deleted_or_inserted_cb(tree_model, eb);
}

static void row_inserted_cb (GtkTreeModel *tree_model, GtkTreePath  *path, GtkTreeIter *iter, struct energyWidget *eb) {
       row_deleted_or_inserted_cb(tree_model, eb);
}

static void energy_delete_button_clicked_cb(GtkWidget *widget, struct energyWidget *eb) {
	int kind = eb->kind;
	int i;
	GtkTreeIter iter;

	GArray *selected_indices = xmi_msim_gui_utils_tree_view_get_selected_indices(GTK_TREE_VIEW(eb->tree));
	struct energiesUndoInfo *eui = (struct energiesUndoInfo *) g_malloc(sizeof(struct energiesUndoInfo));
	eui->indices = selected_indices;

	if (kind == DISCRETE) {
		update_undo_buffer(DISCRETE_ENERGY_DELETE, (void *) eui);
	}
	else if (kind == CONTINUOUS) {
		update_undo_buffer(CONTINUOUS_ENERGY_DELETE, (void *) eui);
	}

	for (i = selected_indices->len - 1 ; i >= 0 ; i--) {
		GtkTreePath *path = gtk_tree_path_new_from_indices(g_array_index(selected_indices, int, i), -1);
		gtk_tree_model_get_iter(GTK_TREE_MODEL(eb->store), &iter, path);
		gtk_tree_path_free(path);
		gtk_list_store_remove(eb->store, &iter);
	}

	adjust_save_buttons();

	return;
}

static gboolean energy_backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, struct energyWidget *eb) {
	if (event->keyval == gdk_keyval_from_name("BackSpace") &&
		gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(GTK_TREE_VIEW(eb->tree))) > 0) {
		energy_delete_button_clicked_cb(widget, eb);
		return TRUE;
	}

	return FALSE;
}

static void energy_add_button_clicked_cb(GtkWidget *widget, struct energyWidget *eb) {
	int kind = eb->kind;

	GtkWidget *dialog;

	if (kind == DISCRETE) {
		dialog = xmi_msim_gui_discrete_energy_dialog_new(GTK_WINDOW(eb->main_window), XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_ADD);
	}
	else if (kind == CONTINUOUS) {
		dialog = xmi_msim_gui_continuous_energy_dialog_new(GTK_WINDOW(eb->main_window), XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_ADD);
	}

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		struct energiesUndoInfo *eui = (struct energiesUndoInfo *) g_malloc(sizeof(struct energiesUndoInfo));
		if (kind == DISCRETE) {
			eui->energy_disc = xmi_msim_gui_discrete_energy_dialog_get_discrete_energy(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(dialog));
			//check if the energy is not present already
			if (current->xi->excitation->n_discrete > 0 && bsearch(eui->energy_disc, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy line: the energy already exists in the list of lines.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				update_undo_buffer(DISCRETE_ENERGY_ADD, eui);
				repopulate_discrete_energies(eb->store, current->xi->excitation);
			}
		}
		else if (kind == CONTINUOUS) {
			eui->energy_cont = xmi_msim_gui_continuous_energy_dialog_get_continuous_energy(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(dialog));
			//check if the energy is not present already
			if (current->xi->excitation->n_continuous > 0 && bsearch(eui->energy_cont, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy interval: the energy already exists in the list of intervals.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				update_undo_buffer(CONTINUOUS_ENERGY_ADD, eui);
				repopulate_continuous_energies(eb->store, current->xi->excitation);
			}
		}
	}
	gtk_widget_destroy(dialog);
	adjust_save_buttons();

	return;
}


static void energy_edit_button_clicked_cb(GtkWidget *widget, struct energyWidget *eb) {
	int kind = eb->kind;
	GtkWidget *dialog;

	// get currently selected index
	GArray *selected_indices = xmi_msim_gui_utils_tree_view_get_selected_indices(GTK_TREE_VIEW(eb->tree));
	int selected_index = g_array_index(selected_indices, int, 0); 
	g_array_free(selected_indices, TRUE);

	if (kind == DISCRETE) {
		dialog = xmi_msim_gui_discrete_energy_dialog_new(GTK_WINDOW(eb->main_window), XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_EDIT);
		xmi_msim_gui_discrete_energy_dialog_set_discrete_energy(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(dialog), &current->xi->excitation->discrete[selected_index]);
		
	}
	else if (kind == CONTINUOUS) {
		dialog = xmi_msim_gui_continuous_energy_dialog_new(GTK_WINDOW(eb->main_window), XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_EDIT);
		xmi_msim_gui_continuous_energy_dialog_set_continuous_energy(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(dialog), &current->xi->excitation->continuous[selected_index]);
		
	}

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		struct energiesUndoInfo *eui = (struct energiesUndoInfo *) g_malloc(sizeof(struct energiesUndoInfo));
		eui->index = selected_index;
		if (kind == DISCRETE) {
			eui->energy_disc = xmi_msim_gui_discrete_energy_dialog_get_discrete_energy(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(dialog));
			//check if the energy is not present already
			if (xmi_cmp_struct_xmi_energy_discrete(&current->xi->excitation->discrete[selected_index], eui->energy_disc) != 0 && bsearch(eui->energy_disc, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not modify energy line: the energy already exists in the list of lines.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				update_undo_buffer(DISCRETE_ENERGY_EDIT, eui);
				repopulate_discrete_energies(eb->store, current->xi->excitation);
			}
		}
		else if (kind == CONTINUOUS) {
			eui->energy_cont = xmi_msim_gui_continuous_energy_dialog_get_continuous_energy(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(dialog));
			//check if the energy is not present already
			if (xmi_cmp_struct_xmi_energy_continuous(&current->xi->excitation->continuous[selected_index], eui->energy_cont) != 0 && bsearch(eui->energy_cont, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
				GtkWidget *error_dialog = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy interval: the energy already exists in the list of intervals.");
				gtk_dialog_run(GTK_DIALOG(error_dialog));
				gtk_widget_destroy(error_dialog);
			}
			else {
				update_undo_buffer(CONTINUOUS_ENERGY_EDIT, eui);
				repopulate_continuous_energies(eb->store, current->xi->excitation);
			}
		}
	}
	gtk_widget_destroy(dialog);
	adjust_save_buttons();

	return;
}

static void energy_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, struct energyWidget *eb) {
	energy_edit_button_clicked_cb(NULL, eb);	
}

static void energy_right_click_menu_delete_cb(GtkWidget *button, struct energyWidget *eb) {
	energy_delete_button_clicked_cb(NULL, eb);
	return;
}

static void energy_right_click_menu_select_all_cb(GtkWidget *button, GtkWidget *tree) {
	GtkTreeSelection *selection;
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_select_all(selection);
	return;
}

static void create_popup_menu(GtkWidget *tree, GdkEventButton *event, struct energyWidget *eb) {
	GtkWidget *menu, *menuitem;


	menu = gtk_menu_new();
	menuitem = gtk_image_menu_item_new_from_stock(GTK_STOCK_DELETE, NULL);
	//count how many rows are selected
	GtkTreeSelection *selection;
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	if (gtk_tree_selection_count_selected_rows(selection) == 0) {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	else {
		g_signal_connect(menuitem, "activate", G_CALLBACK(energy_right_click_menu_delete_cb), eb);
	}
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);
	menuitem = gtk_image_menu_item_new_from_stock(GTK_STOCK_SELECT_ALL, NULL);
	//count how many rows are in the treeview
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree));
	if (gtk_tree_model_iter_n_children(model, NULL) == 0) {
		gtk_widget_set_sensitive(menuitem, FALSE);
	}
	else {
		g_signal_connect(menuitem, "activate", G_CALLBACK(energy_right_click_menu_select_all_cb), (gpointer) tree);
	}
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), menuitem);

	gtk_widget_show_all(menu);

	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, event != NULL ? event->button : 0, gdk_event_get_time((GdkEvent *) event));
}

static gboolean energy_popup_menu_cb(GtkWidget *tree, struct energyWidget *eb) {
	create_popup_menu(tree, NULL, eb);

	return TRUE;
}

static gboolean energy_right_click_cb(GtkWidget *tree, GdkEventButton *event, struct energyWidget *eb) {
	if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
		//if clicked layer is not selected -> select it
		GtkTreeSelection *selection;
		selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
		GtkTreePath *path;
		if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(tree), (gint) event->x, (gint) event->y, &path, NULL, NULL, NULL) &&
		    !gtk_tree_selection_path_is_selected(selection, path)) {
			gtk_tree_selection_select_path(selection, path);
			gtk_tree_path_free(path);
		}
		create_popup_menu(tree, event, eb);
		return TRUE;
	}
	return FALSE;

}

static struct energiesWidget *initialize_single_energies(void *energies, int n_energies, int kind, GtkWidget *main_window) {
	GtkListStore *store;
	GtkTreeIter iter;
	GtkWidget *tree;
	GtkWidget *mainbox;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkWidget *scrolledWindow;
	GtkTreeSelection *select;
	GtkWidget *buttonbox;
	GtkWidget *editButton;
	GtkWidget *addButton;
	GtkWidget *importButton;
	//GtkWidget *EbelButton;
	GtkWidget *deleteButton;
	GtkWidget *clearButton;
	GtkWidget *scaleButton;
	int i;

	struct xmi_energy_discrete *energies_d;
	struct xmi_energy_continuous *energies_c;
	if (kind == DISCRETE)
		energies_d = (struct xmi_energy_discrete *) energies;
	else
		energies_c = (struct xmi_energy_continuous *) energies;

	struct energiesWidget *rv;
	struct energyWidget *eb;

	mainbox = gtk_hbox_new(FALSE, 5);

	if (kind == DISCRETE)
		store = gtk_list_store_new(NCOLUMNS_ENERGIES, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_INT, G_TYPE_DOUBLE);
	else
		store = gtk_list_store_new(NCOLUMNS_ENERGIES-2, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE);

	if (kind == DISCRETE) {
		for (i = 0 ; i < n_energies ; i++) {
			gtk_list_store_append(store,&iter);
			gtk_list_store_set(store, &iter,
			ENERGY_COLUMN, energies_d[i].energy,
			HOR_INTENSITY_COLUMN, energies_d[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, energies_d[i].vertical_intensity,
			SIGMA_X_COLUMN, energies_d[i].sigma_x,
			SIGMA_XP_COLUMN,energies_d[i].sigma_xp,
			SIGMA_Y_COLUMN,energies_d[i].sigma_y,
			SIGMA_YP_COLUMN,energies_d[i].sigma_yp,
			DISTRIBUTION_TYPE_COLUMN,energies_d[i].distribution_type,
			SCALE_PARAMETER_COLUMN,energies_d[i].scale_parameter,
			-1
			);
		}
	}
	else {
		for (i = 0 ; i < n_energies ; i++) {
			gtk_list_store_append(store,&iter);
			gtk_list_store_set(store, &iter,
			ENERGY_COLUMN, energies_c[i].energy,
			HOR_INTENSITY_COLUMN, energies_c[i].horizontal_intensity,
			VER_INTENSITY_COLUMN, energies_c[i].vertical_intensity,
			SIGMA_X_COLUMN, energies_c[i].sigma_x,
			SIGMA_XP_COLUMN,energies_c[i].sigma_xp,
			SIGMA_Y_COLUMN,energies_c[i].sigma_y,
			SIGMA_YP_COLUMN,energies_c[i].sigma_yp,
			-1
			);
		}
	}

	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Energy (keV)", renderer,"text",ENERGY_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Energy (keV)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(ENERGY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Horizontal intensity (ph/s)", renderer,"text",HOR_INTENSITY_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	if (kind == DISCRETE)
		gtk_tree_view_column_set_title(column, "Horizontal intensity (ph/s)");
	else
		gtk_tree_view_column_set_title(column, "Horizontal intensity (ph/s/keV)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(HOR_INTENSITY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Vertical intensity (ph/s)", renderer,"text",VER_INTENSITY_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	if (kind == DISCRETE)
		gtk_tree_view_column_set_title(column, "Vertical intensity (ph/s)");
	else
		gtk_tree_view_column_set_title(column, "Vertical intensity (ph/s/keV)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(VER_INTENSITY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma x (cm)", renderer,"text",SIGMA_X_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma x (cm)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_X_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma y (cm)", renderer,"text",SIGMA_Y_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma y (cm)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_Y_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma xp (rad)", renderer,"text",SIGMA_XP_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma xp (rad)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_XP_COLUMN),NULL);


	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma yp (rad)", renderer,"text",SIGMA_YP_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma yp (rad)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_YP_COLUMN),NULL);

	if (kind == DISCRETE) {
		renderer = gtk_cell_renderer_text_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		column = gtk_tree_view_column_new();
		gtk_tree_view_column_set_title(column, "Distribution type");
		gtk_tree_view_column_set_resizable(column,TRUE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
		gtk_tree_view_column_pack_start(column, renderer, TRUE);
		gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_distribution_type,NULL ,NULL);

		renderer = gtk_cell_renderer_text_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		column = gtk_tree_view_column_new();
		gtk_tree_view_column_set_title(column, "Scale parameter (keV)");
		gtk_tree_view_column_set_resizable(column,TRUE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
		gtk_tree_view_column_pack_start(column, renderer, TRUE);
		gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_scale_parameter,NULL ,NULL);


	}

	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	gtk_widget_set_size_request(scrolledWindow, -1,230);
	gtk_container_add(GTK_CONTAINER(scrolledWindow), tree);
	GtkWidget *tree_frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(tree_frame), scrolledWindow);
	gtk_box_pack_start(GTK_BOX(mainbox), tree_frame, TRUE, TRUE, 3);

	eb = (struct energyWidget *) g_malloc(sizeof(struct energyWidget));
	eb->kind = kind;
	eb->store = store;
	eb->tree = tree;

	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE);
	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(energy_selection_changed_cb), (gpointer) eb);

	buttonbox = gtk_vbox_new(FALSE, 5);
	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(energy_add_button_clicked_cb) , (gpointer) eb);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	g_signal_connect(G_OBJECT(editButton), "clicked", G_CALLBACK(energy_edit_button_clicked_cb) , (gpointer) eb);
	deleteButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	g_signal_connect(G_OBJECT(deleteButton), "clicked", G_CALLBACK(energy_delete_button_clicked_cb) , (gpointer) eb);

	g_signal_connect(G_OBJECT(tree), "row-activated", G_CALLBACK(energy_row_activated_cb), (gpointer) eb);
	g_signal_connect(G_OBJECT(tree), "key-press-event", G_CALLBACK(energy_backspace_key_clicked), (gpointer) eb);
	g_signal_connect(G_OBJECT(tree), "button-press-event", G_CALLBACK(energy_right_click_cb), (gpointer) eb);
	g_signal_connect(G_OBJECT(tree), "popup-menu", G_CALLBACK(energy_popup_menu_cb), (gpointer) eb);


	eb->editButton = editButton;
	eb->deleteButton = deleteButton;
	eb->main_window = main_window;

	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), editButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), deleteButton, FALSE, FALSE, 3);
	GtkWidget *new_buttonbox = gtk_vbox_new(TRUE, 0);
	gtk_box_pack_start(GTK_BOX(new_buttonbox), buttonbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mainbox), new_buttonbox, FALSE, FALSE, 2);

	buttonbox = gtk_vbox_new(FALSE, 5);
	importButton = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	xmi_msim_gui_utils_update_button_text(importButton, "Import");

	g_signal_connect(G_OBJECT(importButton), "clicked", G_CALLBACK(import_button_clicked_cb), (gpointer) eb);
	gtk_box_pack_start(GTK_BOX(buttonbox), importButton, FALSE, FALSE, 3);

	clearButton = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	g_signal_connect(G_OBJECT(clearButton), "clicked", G_CALLBACK(clear_button_clicked_cb), (gpointer) eb);
	gtk_box_pack_start(GTK_BOX(buttonbox), clearButton, FALSE, FALSE, 3);
	eb->clearButton = clearButton;

	scaleButton = gtk_button_new_from_stock(GTK_STOCK_REFRESH);
	xmi_msim_gui_utils_update_button_text(scaleButton, "Scale");
	g_signal_connect(G_OBJECT(scaleButton), "clicked", G_CALLBACK(scale_button_clicked_cb), (gpointer) eb);
	gtk_box_pack_start(GTK_BOX(buttonbox), scaleButton, FALSE, FALSE, 3);
	eb->scaleButton = scaleButton;


	new_buttonbox = gtk_vbox_new(TRUE, 0);
	gtk_box_pack_start(GTK_BOX(new_buttonbox), buttonbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mainbox), new_buttonbox, FALSE, FALSE, 2);

	gtk_widget_set_sensitive(editButton, FALSE);
	gtk_widget_set_sensitive(deleteButton, FALSE);
	gtk_widget_set_sensitive(scaleButton, FALSE);
	if (gtk_tree_model_iter_n_children(GTK_TREE_MODEL(store), NULL) > 0) {
		gtk_widget_set_sensitive(clearButton, TRUE);
	}
	else {
		gtk_widget_set_sensitive(clearButton, FALSE);
	}

	g_signal_connect(G_OBJECT(store), "row-inserted", G_CALLBACK(row_inserted_cb), (gpointer) eb);
	g_signal_connect(G_OBJECT(store), "row-deleted", G_CALLBACK(row_deleted_cb), (gpointer) eb);

	rv = (struct energiesWidget *) g_malloc(sizeof(struct energiesWidget));
	rv->store=store;
	rv->widget=mainbox;

	return rv;
}

GtkWidget *initialize_energies(struct xmi_excitation *excitation, GtkWidget *main_window, struct energiesWidget **discWidget, struct energiesWidget **contWidget) {
	GtkWidget *mainvbox;
	GtkWidget *separator;

	mainvbox = gtk_vbox_new(FALSE, 5);
	gtk_container_set_border_width(GTK_CONTAINER(mainvbox), 10);

	//discrete first...
	*discWidget = initialize_single_energies((void *) excitation->discrete, excitation->n_discrete,DISCRETE, main_window);
	*contWidget = initialize_single_energies((void *) excitation->continuous, excitation->n_continuous,CONTINUOUS, main_window);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Discrete energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), (*discWidget)->widget, FALSE, FALSE, 2);
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainvbox), separator, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Continuous energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), (*contWidget)->widget, FALSE, FALSE, 2);

	return mainvbox;
}


static int xmi_read_energies_from_ascii_file_discrete(gchar *filename, struct xmi_energy_discrete **energies, unsigned int start_line, unsigned int nlines) {
	FILE *fp;
	struct xmi_energy_discrete *xe = NULL;
#ifdef G_OS_WIN32
	unsigned int nxe = 0;
#else
	size_t nxe = 0;
#endif
	if ((fp = fopen(filename, "r")) == NULL) {
		g_warning("Could not open file %s\n", filename);
		return -1;
	}

	//read line per line...
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	int values;
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_y;
	double sigma_xp;
	double sigma_yp;
	struct xmi_energy_discrete temp;
	unsigned int lines_read = 0;


	while ((linelen = getline(&line, &linecap, fp)) > -1) {
		lines_read++;
		if (lines_read < start_line)
			continue;
		//ignore empty lines
		if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
			nlines--;
			continue;
		}
		if (nlines == nxe)
			break;
		values = sscanf(line,"%lg %lg %lg %lg %lg %lg %lg", &energy, &horizontal_intensity, &vertical_intensity, &sigma_x, &sigma_y, &sigma_xp, &sigma_yp);
		temp.sigma_x = 0.0;
		temp.sigma_y = 0.0;
		temp.sigma_xp = 0.0;
		temp.sigma_yp = 0.0;
		temp.distribution_type = XMI_DISCRETE_MONOCHROMATIC;
		temp.scale_parameter = 0.0;

		switch (values) {
			case 7:
				temp.sigma_x = sigma_x;
				temp.sigma_y = sigma_y;
				temp.sigma_xp = sigma_xp;
				temp.sigma_yp = sigma_yp;
			case 3:
				temp.horizontal_intensity = horizontal_intensity;
				temp.vertical_intensity = vertical_intensity;
				temp.energy = energy;
				break;
			case 2:
				temp.horizontal_intensity = horizontal_intensity/2.0;
				temp.vertical_intensity = horizontal_intensity/2.0;
				temp.energy = energy;
				break;
			default:
				g_warning("Syntax error in file %s at line %i after reading %u lines of %i requested\nNumber of columns must be 2, 3 or 7!\n", filename, lines_read, (unsigned int) nxe, nlines);
				return -3;
		};
		//ignore the useless lines
		if (temp.energy <= 0.0000000001 || temp.energy > 200.0 || temp.horizontal_intensity + temp.vertical_intensity <= 0.000000001 || temp.horizontal_intensity < -0.0000000001 || temp.vertical_intensity < -0.0000000001) {
			nlines--;
			continue;
		}
		if (nlines == nxe)
			break;
		if (nxe == 0) {
			xe = (struct xmi_energy_discrete *) g_realloc(xe, sizeof(struct xmi_energy_discrete) * ++nxe);
			xe[0] = temp;
		}
		else {
			//make sure the value was not already in the list
			struct xmi_energy_discrete *find_res;
#ifdef G_OS_WIN32
			if((find_res = (struct xmi_energy_discrete *) _lfind(&temp, xe, &nxe, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) == NULL)
#else
			if((find_res = (struct xmi_energy_discrete *) lfind(&temp, xe, &nxe, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete)) == NULL)
#endif
				{
				xe = (struct xmi_energy_discrete *) g_realloc(xe, sizeof(struct xmi_energy_discrete) * ++nxe);
				xe[nxe-1] = temp;
			}
			else  {
				g_warning("Warning: Duplicate discrete line energy detected\nAdding to existing discrete line\n");
				find_res->horizontal_intensity += temp.horizontal_intensity;
				find_res->vertical_intensity += temp.vertical_intensity;
			}
		}
		if (nxe == nlines)
			break;
	}


	if (fclose(fp) != 0) {
		g_warning("Could not close file %s\n", filename);
		return -2;
	}


	//sort
	if (nxe > 1) {
		qsort(xe, nxe, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete);
	}

	*energies = xe;

	return nxe;
}


static int xmi_read_energies_from_ascii_file_continuous(gchar *filename, struct xmi_energy_continuous **energies, unsigned int start_line, unsigned int nlines) {
	FILE *fp;
	struct xmi_energy_continuous *xe = NULL;
#ifdef G_OS_WIN32
	unsigned int nxe = 0;
#else
	size_t nxe = 0;
#endif
	if ((fp = fopen(filename, "r")) == NULL) {
		g_warning("Could not open file %s\n", filename);
		return -1;
	}

	//read line per line...
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	int values;
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
	double sigma_x;
	double sigma_y;
	double sigma_xp;
	double sigma_yp;
	struct xmi_energy_continuous temp;
	unsigned int lines_read = 0;


	while ((linelen = getline(&line, &linecap, fp)) > -1) {
		lines_read++;
		if (lines_read < start_line)
			continue;
		//ignore empty lines
		if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
			nlines--;
			continue;
		}
		if (nlines == nxe)
			break;
		values = sscanf(line,"%lg %lg %lg %lg %lg %lg %lg", &energy, &horizontal_intensity, &vertical_intensity, &sigma_x, &sigma_y, &sigma_xp, &sigma_yp);
		temp.sigma_x = 0.0;
		temp.sigma_y = 0.0;
		temp.sigma_xp = 0.0;
		temp.sigma_yp = 0.0;

		switch (values) {
			case 7:
				temp.sigma_x = sigma_x;
				temp.sigma_y = sigma_y;
				temp.sigma_xp = sigma_xp;
				temp.sigma_yp = sigma_yp;
			case 3:
				temp.horizontal_intensity = horizontal_intensity;
				temp.vertical_intensity = vertical_intensity;
				temp.energy = energy;
				break;
			case 2:
				temp.horizontal_intensity = horizontal_intensity/2.0;
				temp.vertical_intensity = horizontal_intensity/2.0;
				temp.energy = energy;
				break;
			default:
				g_warning("Syntax error in file %s at line %i after reading %u lines of %i requested\nNumber of columns must be 2, 3 or 7!\n", filename, lines_read, (unsigned int) nxe, nlines);
				return -3;
		};

		//ignore the useless lines
		if (temp.energy <= 0.0 || temp.energy > 200.0 || temp.horizontal_intensity + temp.vertical_intensity < 0.0 || temp.horizontal_intensity < 0.0 || temp.vertical_intensity < 0.0) {
			nlines--;
			continue;
		}
		if (nlines == nxe)
			break;

		if (nxe == 0) {
			xe = (struct xmi_energy_continuous *) g_realloc(xe, sizeof(struct xmi_energy_continuous) * ++nxe);
			xe[0] = temp;
		}
		else {
			//make sure the value was not already in the list
#ifdef G_OS_WIN32
			if(_lfind(&temp, xe, &nxe, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) == NULL)
#else
			if(lfind(&temp, xe, &nxe, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) == NULL)
#endif
				{
				xe = (struct xmi_energy_continuous *) g_realloc(xe, sizeof(struct xmi_energy_continuous) * ++nxe);
				xe[nxe-1] = temp;
			}
			else  {
				g_warning("Duplicate energies found in %s\n", filename);
				return -4;
			}
		}
		if (nxe == nlines)
			break;
	}


	if (fclose(fp) != 0) {
		g_warning("Could not close file %s\n", filename);
		return -2;
	}

	//sort
	if (nxe > 1) {
		qsort(xe, nxe, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous);
	}

	if (nxe > 2) {
		unsigned int i;
		for (i = 0 ; i < nxe-1 ; i++) {
			if (xe[i].horizontal_intensity + xe[i].vertical_intensity + xe[i+1].horizontal_intensity + xe[i+1].vertical_intensity == 0.0) {
				g_warning("Error: Two consecutive continuous intensity densities cannot both have a total intensity of zero\n");
				return -5;

			}
		}
	}

	*energies = xe;

	return nxe;
}
