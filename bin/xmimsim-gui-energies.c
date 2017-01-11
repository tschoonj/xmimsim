/*
Copyright (C) 2010-2011 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-energies.h"
#include "xmimsim-gui-results.h"
#include "xmimsim-gui-fonts.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include <math.h>
#include "xmi_aux.h"
#include "xmi_private.h"
#include "xmi_ebel.h"
#include <glib/gstdio.h>
#include <search.h>
#include <string.h>


struct energyDialog {
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *energyEntry;
	GtkWidget *hor_intensityEntry;
	GtkWidget *ver_intensityEntry;
	GtkWidget *sigma_xEntry;
	GtkWidget *sigma_yEntry;
	GtkWidget *sigma_xpEntry;
	GtkWidget *sigma_ypEntry;
	GtkWidget *distribution_typeCombo;
	GtkWidget *scale_parameterEntry;
	GtkWidget *scale_parameterLabel;
	GtkWidget *scale_parameterBox;
	GtkWidget *window;
	gulong energyGulong;
	gulong hor_intensityGulong;
	gulong ver_intensityGulong;
	gulong sigma_xGulong;
	gulong sigma_yGulong;
	gulong sigma_xpGulong;
	gulong sigma_ypGulong;
	gulong distribution_typeGulong;
	gulong scale_parameterGulong;
};

struct xmi_energy_discrete *energy_disc;
struct xmi_energy_continuous *energy_cont;
struct energyDialog *energyDialog_disc;
struct energyDialog *energyDialog_cont;

enum {
	ENERGY_ADD,
	ENERGY_EDIT
};

enum {
	DISCRETE,
	CONTINUOUS
};

int addOrEdit;
int discOrCont;
int current_index;
int current_nindices;
int *delete_current_indices = NULL;
int delete_current_nindices;


struct energiesWidget *contWidget;
struct energiesWidget *discWidget;



struct energyButtons {
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *scaleButton;
	GtkWidget *clearButton;
};



struct kind_and_window {
	int kind;
	GtkWidget *main_window;
};


static int xmi_read_energies_from_ascii_file_discrete(gchar *filename, struct xmi_energy_discrete **energies, unsigned int start_line, unsigned int nlines);
static int xmi_read_energies_from_ascii_file_continuous(gchar *filename, struct xmi_energy_continuous **energies, unsigned int start_line, unsigned int nlines);




static gboolean delete_layer_widget(GtkWidget *widget, GdkEvent *event, gpointer data) {
	return TRUE;
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

static void clear_button_clicked_cb(GtkWidget *widget, struct kind_and_window *k_a_w) {
	int kind = k_a_w->kind;

	if (kind == DISCRETE) {
		gtk_list_store_clear(discWidget->store);
		update_undo_buffer(DISCRETE_ENERGY_CLEAR, NULL);
	}
	else if (kind == CONTINUOUS) {
		gtk_list_store_clear(contWidget->store);
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

static void scale_button_clicked_cb(GtkWidget *widget, struct kind_and_window *k_a_w) {
	int kind = k_a_w->kind;

	//Launch dialog to select the scale factor
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Intensity scale factor", GTK_WINDOW(k_a_w->main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

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
	int delete_current_nindices_local = delete_current_nindices;

	if (kind == DISCRETE) {
		update_undo_buffer(DISCRETE_ENERGY_SCALE, (GtkWidget *) &value);
		for (i = 0 ; i < delete_current_nindices_local ; i++){
			GtkTreePath *path = gtk_tree_path_new_from_indices(delete_current_indices[i], -1);
			gtk_tree_model_get_iter(GTK_TREE_MODEL(discWidget->store), &iter, path);
			gtk_tree_path_free(path);
			gtk_list_store_set(discWidget->store, &iter,
			HOR_INTENSITY_COLUMN, (current)->xi->excitation->discrete[delete_current_indices[i]].horizontal_intensity,
			VER_INTENSITY_COLUMN, (current)->xi->excitation->discrete[delete_current_indices[i]].vertical_intensity,
			-1);
		}
	}
	else if (kind == CONTINUOUS) {
		update_undo_buffer(CONTINUOUS_ENERGY_SCALE, (GtkWidget *) &value);
		for (i = 0 ; i < delete_current_nindices_local ; i++){
			GtkTreePath *path = gtk_tree_path_new_from_indices(delete_current_indices[i], -1);
			gtk_tree_model_get_iter(GTK_TREE_MODEL(contWidget->store), &iter, path);
			gtk_tree_path_free(path);
			gtk_list_store_set(contWidget->store, &iter,
			HOR_INTENSITY_COLUMN, (current)->xi->excitation->continuous[delete_current_indices[i]].horizontal_intensity,
			VER_INTENSITY_COLUMN, (current)->xi->excitation->continuous[delete_current_indices[i]].vertical_intensity,
			-1);
		}
	}


}


static void radio_button_toggled_cb(GtkToggleButton *button, GtkWidget *spinner){
	if (gtk_toggle_button_get_active(button))
		gtk_widget_set_sensitive(spinner, TRUE);
	else
		gtk_widget_set_sensitive(spinner, FALSE);
}

static void import_button_clicked_cb(GtkWidget *widget, struct kind_and_window *k_a_w) {
	GtkWidget *main_window = k_a_w->main_window;
	int kind = k_a_w->kind;


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
	dialog = gtk_file_chooser_dialog_new ("Open File",
                 GTK_WINDOW(main_window),
                 GTK_FILE_CHOOSER_ACTION_OPEN,
                 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                 GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                 NULL);

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

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
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog), vbox);


	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
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

		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_widget_destroy (dialog);
		dialog = NULL;
		int rv;
		if (kind == DISCRETE)
    			rv = xmi_read_energies_from_ascii_file_discrete(filename, &energy_disc, start_line, nlines);
		else
    			rv = xmi_read_energies_from_ascii_file_continuous(filename, &energy_cont, start_line, nlines);
		if (rv > 0) {
			//success
			g_fprintf(stdout,"File %s read in successfully\n",filename);

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
							if (bsearch(energy_disc+i, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
								gtk_widget_destroy(dialog);
								dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
								gtk_dialog_run(GTK_DIALOG(dialog));
								gtk_widget_destroy(dialog);
								g_free(energy_disc);
								energy_disc = NULL;
								return;
							}
						}
					}
					update_undo_buffer(DISCRETE_ENERGY_IMPORT_ADD, (GtkWidget *) GINT_TO_POINTER(rv));
				}
				else if (kind == CONTINUOUS) {
					if (current->xi->excitation->n_continuous > 0) {
						for (i = 0 ; i < current->xi->excitation->n_continuous ; i++) {
							if (bsearch(energy_cont+i, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
								gtk_widget_destroy(dialog);
								dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy lines: one or more of the new energies exist already in the list of lines.");
								gtk_dialog_run(GTK_DIALOG(dialog));
								gtk_widget_destroy(dialog);
								g_free(energy_cont);
								energy_cont= NULL;
								return;
							}
						}
					}
					update_undo_buffer(CONTINUOUS_ENERGY_IMPORT_ADD, (GtkWidget *) GINT_TO_POINTER(rv));
				}
			}
			else if (rv2 == GTK_RESPONSE_CANCEL) {
				//replace -> no need to check for duplicates here
				if (kind == DISCRETE) {
					update_undo_buffer(DISCRETE_ENERGY_IMPORT_REPLACE, (GtkWidget *) GINT_TO_POINTER(rv));
				}
				else if (kind == CONTINUOUS) {
					update_undo_buffer(CONTINUOUS_ENERGY_IMPORT_REPLACE, (GtkWidget *) GINT_TO_POINTER(rv));
				}
			}
			else {
				gtk_widget_destroy(dialog);
				g_free (filename);
				return;
			}
			if (kind == DISCRETE) {
				gtk_list_store_clear(discWidget->store);
				int i;
				GtkTreeIter iter;
				for (i = 0 ; i < (current)->xi->excitation->n_discrete ; i++) {
					gtk_list_store_append(discWidget->store, &iter);
					gtk_list_store_set(discWidget->store, &iter,
					ENERGY_COLUMN, (current)->xi->excitation->discrete[i].energy,
					HOR_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].horizontal_intensity,
					VER_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].vertical_intensity,
					SIGMA_X_COLUMN, (current)->xi->excitation->discrete[i].sigma_x,
					SIGMA_XP_COLUMN,(current)->xi->excitation->discrete[i].sigma_xp,
					SIGMA_Y_COLUMN,(current)->xi->excitation->discrete[i].sigma_y,
					SIGMA_YP_COLUMN,(current)->xi->excitation->discrete[i].sigma_yp,
					DISTRIBUTION_TYPE_COLUMN,(current)->xi->excitation->discrete[i].distribution_type,
					SCALE_PARAMETER_COLUMN,(current)->xi->excitation->discrete[i].scale_parameter,
					-1);
				}
			}
			else if (kind == CONTINUOUS) {
				gtk_list_store_clear(contWidget->store);
				int i;
				GtkTreeIter iter;
				for (i = 0 ; i < (current)->xi->excitation->n_continuous ; i++) {
					gtk_list_store_append(contWidget->store, &iter);
					gtk_list_store_set(contWidget->store, &iter,
					ENERGY_COLUMN, (current)->xi->excitation->continuous[i].energy,
					HOR_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].horizontal_intensity,
					VER_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].vertical_intensity,
					SIGMA_X_COLUMN, (current)->xi->excitation->continuous[i].sigma_x,
					SIGMA_XP_COLUMN,(current)->xi->excitation->continuous[i].sigma_xp,
					SIGMA_Y_COLUMN,(current)->xi->excitation->continuous[i].sigma_y,
					SIGMA_YP_COLUMN,(current)->xi->excitation->continuous[i].sigma_yp,
					-1);
				}
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
	if (dialog)
		gtk_widget_destroy (dialog);
	if (kind == DISCRETE)
		energy_disc = NULL;
	else if (kind == CONTINUOUS)
		energy_cont = NULL;

	return;
}

static void energy_ok_cancel_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct energyDialog *ew = (struct energyDialog *) data;

	if (widget == ew->okButton) {
		//ok clicked
	}
	else if (widget == ew->cancelButton) {
		//cancel clicked
		//if Entries are red, make them white again
		gtk_widget_modify_base(ew->energyEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->hor_intensityEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->ver_intensityEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_xEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_yEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_xpEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_ypEntry, GTK_STATE_NORMAL,NULL);


		if (discOrCont == DISCRETE) {
			g_free(energy_disc);
			energy_disc = NULL;
		}
		else if (discOrCont == CONTINUOUS) {
			g_free(energy_cont);
			energy_cont = NULL;
		}
	}

	gtk_widget_hide(ew->window);


	return;
}



void energy_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	struct energyButtons *eb = (struct energyButtons *) data;
	GtkTreeIter temp_iter;
	GtkTreeModel *model = gtk_tree_view_get_model(gtk_tree_selection_get_tree_view(selection));
	gboolean valid;

	int nselected = gtk_tree_selection_count_selected_rows(selection);

	//fprintf(stdout, "energy_selection_changed_cb: %i\n",nselected);

	switch (nselected) {
		case 0:
			gtk_widget_set_sensitive(eb->deleteButton,FALSE);
			gtk_widget_set_sensitive(eb->editButton,FALSE);
			gtk_widget_set_sensitive(eb->scaleButton,FALSE);
			break;
		case 1:
			valid = gtk_tree_model_get_iter_first(model, &temp_iter);
			current_index = 0;
			current_nindices = 0;
			while (valid) {
				if (gtk_tree_selection_iter_is_selected(selection,&temp_iter)) {
					current_index = current_nindices;
				}
				current_nindices++;
				valid = gtk_tree_model_iter_next(model, &temp_iter);
			}
			gtk_widget_set_sensitive(eb->editButton,TRUE);
		default:
			//multiple selected
			if (delete_current_indices)
				g_free(delete_current_indices);
			gtk_widget_set_sensitive(eb->deleteButton,TRUE);
			gtk_widget_set_sensitive(eb->scaleButton,TRUE);
			if (nselected > 1)
				gtk_widget_set_sensitive(eb->editButton,FALSE);
			delete_current_indices = (int *) g_malloc(sizeof(int)*nselected);
			delete_current_nindices = nselected;
			int i = 0, j = 0;
			valid = gtk_tree_model_get_iter_first(model, &temp_iter);
			while (valid) {
				if (gtk_tree_selection_iter_is_selected(selection,&temp_iter)) {
					delete_current_indices[i++] = j;
				}
				j++;
				valid = gtk_tree_model_iter_next(model, &temp_iter);
			}
			break;
	}


	return;
}

static void energy_window_changed_cb(GtkWidget *widget, gpointer data) {
	struct energyDialog *ew =  (struct energyDialog *) data;
	char *textPtr1, *textPtr2, *textPtr3, *textPtr4, *textPtr5, *textPtr6, *textPtr7, *textPtr8;
	char *endPtr1, *endPtr2, *endPtr3, *endPtr4, *endPtr5, *endPtr6, *endPtr7, *endPtr8;
	char *lastPtr1, *lastPtr4, *lastPtr5, *lastPtr6, *lastPtr7, *lastPtr8;

	int ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8;
	double value1, value2, value3, value4, value5, value6, value7, value8;






	textPtr1 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->energyEntry));
	textPtr2 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->hor_intensityEntry));
	textPtr3 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->ver_intensityEntry));
	textPtr4 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_xEntry));
	textPtr5 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_yEntry));
	textPtr6 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_xpEntry));
	textPtr7 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_ypEntry));

	if(discOrCont == DISCRETE && gtk_combo_box_get_active(GTK_COMBO_BOX(ew->distribution_typeCombo)) != XMI_DISCRETE_MONOCHROMATIC) {
		textPtr8 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->scale_parameterEntry));
	}


#define energy_short1(n,my_entry) value ## n = strtod(textPtr ## n, &endPtr ## n);\
	lastPtr ## n = textPtr ## n + strlen(textPtr ## n);\
	if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n > 0.0) \
		ok ## n = 1;\
	else\
		ok ## n = 0;\
	if (widget == my_entry) {\
		if (ok ## n)\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,NULL);\
		else {\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&red);\
			gtk_widget_set_sensitive(ew->okButton, FALSE);\
		}\
	}

#define energy_short2(n,my_entry) value ## n = strtod(textPtr ## n, &endPtr ## n);\
	lastPtr ## n = textPtr ## n + strlen(textPtr ## n);\
	if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n >= 0.0) \
		ok ## n = 1;\
	else\
		ok ## n = 0;\
	if (widget == my_entry) {\
		if (ok ## n)\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,NULL);\
		else {\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&red);\
			gtk_widget_set_sensitive(ew->okButton, FALSE);\
		}\
	}

#define energy_short3(n,my_entry) value ## n = strtod(textPtr ## n, &endPtr ## n);\
	lastPtr ## n = textPtr ## n + strlen(textPtr ## n);\
	if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n > 0.0 && value ## n <= 200.0) \
		ok ## n = 1;\
	else\
		ok ## n = 0;\
	if (widget == my_entry) {\
		if (ok ## n)\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,NULL);\
		else {\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&red);\
			gtk_widget_set_sensitive(ew->okButton, FALSE);\
		}\
	}

	energy_short3(1, ew->energyEntry)
	//energy_short1(2, ew->hor_intensityEntry)
	//energy_short1(3, ew->ver_intensityEntry)
	energy_short2(4, ew->sigma_xEntry)
	energy_short2(5, ew->sigma_yEntry)
	energy_short2(6, ew->sigma_xpEntry)
	energy_short2(7, ew->sigma_ypEntry)

	if(discOrCont == DISCRETE && gtk_combo_box_get_active(GTK_COMBO_BOX(ew->distribution_typeCombo)) != XMI_DISCRETE_MONOCHROMATIC) {
		energy_short1(8, ew->scale_parameterEntry)
	}
	else {
		ok8 = 1;
	}

	value2 = strtod(textPtr2, &endPtr2);
	value3 = strtod(textPtr3, &endPtr3);

	if (value2 > 0.0)
		ok2 = 1;
	else if (strlen(textPtr2) == 0)
		ok2 = 0;
	else if (textPtr2 + strlen(textPtr2) != endPtr2)
		ok2 = -1;
	else if (value2 == 0.0)
		ok2 = -2;
	else
		ok2 = -1;

	if (value3 > 0.0)
		ok3 = 1;
	else if (strlen(textPtr3) == 0)
		ok3 = 0;
	else if (textPtr3 + strlen(textPtr3) != endPtr3)
		ok3 = -1;
	else if (value3 == 0.0)
		ok3 = -2;
	else
		ok3 = -1;


	if (ok2 == 1 || ok2 == 0)
		gtk_widget_modify_base(ew->hor_intensityEntry, GTK_STATE_NORMAL,NULL);
	else if (ok2 == -1)
		gtk_widget_modify_base(ew->hor_intensityEntry, GTK_STATE_NORMAL,&red);

	if (ok3 == 1 || ok3 == 0)
		gtk_widget_modify_base(ew->ver_intensityEntry, GTK_STATE_NORMAL,NULL);
	else if (ok3 == -1)
		gtk_widget_modify_base(ew->ver_intensityEntry, GTK_STATE_NORMAL,&red);

	if ((ok2 == 1 && ok3 == -2 && discOrCont == DISCRETE)||(ok2 == -2 && ok3 == 1 && discOrCont == DISCRETE)) {
		ok2 = ok3 = 1;
		gtk_widget_modify_base(ew->hor_intensityEntry, GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->ver_intensityEntry, GTK_STATE_NORMAL,NULL);
	}
	else if ((ok2 == -2 && ok3 == -2 && discOrCont == DISCRETE)) {
		ok2 = ok3 = 0;
		gtk_widget_modify_base(ew->hor_intensityEntry, GTK_STATE_NORMAL,&red);
		gtk_widget_modify_base(ew->ver_intensityEntry, GTK_STATE_NORMAL,&red);
	}

	if (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 && ok8) {
		gtk_widget_set_sensitive(ew->okButton, TRUE);
		if (discOrCont == DISCRETE) {
			energy_disc->energy = value1;
			energy_disc->horizontal_intensity = value2;
			energy_disc->vertical_intensity = value3;
			energy_disc->sigma_x = value4;
			energy_disc->sigma_y = value5;
			energy_disc->sigma_xp = value6;
			energy_disc->sigma_yp = value7;
			energy_disc->distribution_type = gtk_combo_box_get_active(GTK_COMBO_BOX(ew->distribution_typeCombo));
			if(energy_disc->distribution_type != XMI_DISCRETE_MONOCHROMATIC) {
				energy_disc->scale_parameter = value8;
			}
			else {
				energy_disc->scale_parameter = 0.0;
			}
		}
		else if (discOrCont == CONTINUOUS) {
			energy_cont->energy = value1;
			energy_cont->horizontal_intensity = value2;
			energy_cont->vertical_intensity = value3;
			energy_cont->sigma_x = value4;
			energy_cont->sigma_y = value5;
			energy_cont->sigma_xp = value6;
			energy_cont->sigma_yp = value7;
		}
	}
	else
		gtk_widget_set_sensitive(ew->okButton, FALSE);

	return;
}
static void row_deleted_or_inserted_cb(GtkTreeModel *tree_model, struct energyButtons *eb) {
	gint kids = gtk_tree_model_iter_n_children(tree_model, NULL);

	if (kids > 0) {
		gtk_widget_set_sensitive(eb->clearButton, TRUE);
	}
	else {
		gtk_widget_set_sensitive(eb->clearButton, FALSE);
	}
}

static void row_deleted_cb (GtkTreeModel *tree_model, GtkTreePath  *path, struct energyButtons *eb) {
       row_deleted_or_inserted_cb(tree_model, eb);
}

static void row_inserted_cb (GtkTreeModel *tree_model, GtkTreePath  *path, GtkTreeIter *iter, struct energyButtons *eb) {
       row_deleted_or_inserted_cb(tree_model, eb);
}

void energy_delete_button_clicked_cb(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);
	int i;
	GtkTreeIter iter;
	int delete_current_nindices_local = delete_current_nindices;

	if (kind == DISCRETE) {
		//update undo buffer... and then the store
		update_undo_buffer(DISCRETE_ENERGY_DELETE,NULL);
		for (i = delete_current_nindices_local-1 ; i >= 0 ; i--){
			GtkTreePath *path = gtk_tree_path_new_from_indices(delete_current_indices[i], -1);
			gtk_tree_model_get_iter(GTK_TREE_MODEL(discWidget->store), &iter, path);
			gtk_tree_path_free(path);
			gtk_list_store_remove(discWidget->store, &iter);
		}
	}
	else if (kind == CONTINUOUS) {
		update_undo_buffer(CONTINUOUS_ENERGY_DELETE,NULL);
		for (i = delete_current_nindices_local-1 ; i >= 0 ; i--){
			GtkTreePath *path = gtk_tree_path_new_from_indices(delete_current_indices[i], -1);
			gtk_tree_model_get_iter(GTK_TREE_MODEL(contWidget->store), &iter, path);
			gtk_tree_path_free(path);
			gtk_list_store_remove(contWidget->store, &iter);
		}
	}

	adjust_save_buttons();

	return;
}

static gboolean energy_backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, gpointer data) {
	if (event->keyval == gdk_keyval_from_name("BackSpace")) {
		energy_delete_button_clicked_cb(widget,data);
		return TRUE;
	}

	return FALSE;
}
void energy_add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

	if (kind == DISCRETE)
		energy_disc = (struct xmi_energy_discrete *) g_malloc(sizeof(struct xmi_energy_discrete));
	else
		energy_cont = (struct xmi_energy_continuous *) g_malloc(sizeof(struct xmi_energy_continuous));


	addOrEdit = ENERGY_ADD;
	discOrCont = kind;


	gtk_widget_show_all(kind == DISCRETE ? energyDialog_disc->window :  energyDialog_cont->window);
	return;
}


void energy_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer data) {
	int kind = GPOINTER_TO_INT(data);
	if (kind == DISCRETE) {
		energy_disc = (struct xmi_energy_discrete *) xmi_memdup(current->xi->excitation->discrete+current_index, sizeof(struct xmi_energy_discrete));
	}
	else if (kind == CONTINUOUS) {
		energy_cont = (struct xmi_energy_continuous *) xmi_memdup(current->xi->excitation->continuous+current_index, sizeof(struct xmi_energy_continuous));
	}
	else {
		fprintf(stderr,"Invalid kind detected\n");
		exit(1);
	}
	addOrEdit = ENERGY_EDIT;
	discOrCont = kind;

	gtk_widget_show_all(kind == DISCRETE ? energyDialog_disc->window :  energyDialog_cont->window);
	return;
}

void energy_edit_button_clicked_cb(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

#if DEBUG == 1
	fprintf(stdout,"current_index: %i\n",current_index);
	fprintf(stdout,"current discrete hor intensity: %g\n",current->xi->excitation->discrete[0].horizontal_intensity);
#endif

	if (kind == DISCRETE) {
		energy_disc = (struct xmi_energy_discrete *) xmi_memdup(current->xi->excitation->discrete+current_index, sizeof(struct xmi_energy_discrete));
	}
	else if (kind == CONTINUOUS) {
		energy_cont = (struct xmi_energy_continuous *) xmi_memdup(current->xi->excitation->continuous+current_index, sizeof(struct xmi_energy_continuous));
	}
	else {
		fprintf(stderr,"Invalid kind detected\n");
		exit(1);
	}
	addOrEdit = ENERGY_EDIT;
	discOrCont = kind;

	gtk_widget_show_all(kind == DISCRETE ? energyDialog_disc->window :  energyDialog_cont->window);

	return;
}

static void energy_right_click_menu_delete_cb(GtkWidget *button, gpointer kind) {
	energy_delete_button_clicked_cb(NULL, kind);
	return;
}

static void energy_right_click_menu_select_all_cb(GtkWidget *button, GtkWidget *tree) {
	GtkTreeSelection *selection;
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_select_all(selection);
	return;
}

static void create_popup_menu(GtkWidget *tree, GdkEventButton *event, gpointer kind) {
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
		g_signal_connect(menuitem, "activate", G_CALLBACK(energy_right_click_menu_delete_cb), kind);
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
static gboolean energy_popup_menu_cb(GtkWidget *tree, gpointer kind) {
	create_popup_menu(tree, NULL, kind);

	return TRUE;
}

static gboolean energy_right_click_cb(GtkWidget *tree, GdkEventButton *event, gpointer kind) {
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
		create_popup_menu(tree, event, kind);
		return TRUE;
	}
	return FALSE;

}

struct energiesWidget *initialize_single_energies(void *energies, int n_energies, int kind, GtkWidget *main_window) {
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
	struct energyButtons *eb;

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
	gtk_box_pack_start(GTK_BOX(mainbox),scrolledWindow, TRUE, TRUE,3 );

	eb = (struct energyButtons *) g_malloc(sizeof(struct energyButtons));

	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE);
	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(energy_selection_changed_cb), (gpointer) eb);

	buttonbox = gtk_vbox_new(FALSE, 5);
	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(energy_add_button_clicked_cb) , GINT_TO_POINTER(kind));
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	g_signal_connect(G_OBJECT(editButton), "clicked", G_CALLBACK(energy_edit_button_clicked_cb) , GINT_TO_POINTER(kind));
	deleteButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	g_signal_connect(G_OBJECT(deleteButton), "clicked", G_CALLBACK(energy_delete_button_clicked_cb) , GINT_TO_POINTER(kind));

	g_signal_connect(G_OBJECT(tree), "row-activated", G_CALLBACK(energy_row_activated_cb), GINT_TO_POINTER(kind));
	g_signal_connect(G_OBJECT(tree), "key-press-event", G_CALLBACK(energy_backspace_key_clicked), GINT_TO_POINTER(kind));
	g_signal_connect(G_OBJECT(tree), "button-press-event", G_CALLBACK(energy_right_click_cb), GINT_TO_POINTER(kind));
	g_signal_connect(G_OBJECT(tree), "popup-menu", G_CALLBACK(energy_popup_menu_cb), GINT_TO_POINTER(kind));


	eb->editButton = editButton;
	eb->deleteButton = deleteButton;


	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), editButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), deleteButton, FALSE, FALSE, 3);
	GtkWidget *new_buttonbox = gtk_vbox_new(TRUE, 0);
	gtk_box_pack_start(GTK_BOX(new_buttonbox), buttonbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mainbox), new_buttonbox, FALSE, FALSE, 2);

	buttonbox = gtk_vbox_new(FALSE, 5);
	importButton = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	xmi_msim_gui_utils_update_button_text(importButton, "Import");

	struct kind_and_window *k_a_w = (struct kind_and_window *) g_malloc(sizeof(struct kind_and_window));
	k_a_w->kind = kind;
	k_a_w->main_window = main_window;
	g_signal_connect(G_OBJECT(importButton), "clicked", G_CALLBACK(import_button_clicked_cb), (gpointer) k_a_w);
	gtk_box_pack_start(GTK_BOX(buttonbox), importButton, FALSE, FALSE, 3);

	//EbelButton = gtk_button_new_from_stock(XMI_STOCK_RADIATION_WARNING);
	//gtk_box_pack_start(GTK_BOX(buttonbox), EbelButton, TRUE, FALSE, 3);

	clearButton = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	g_signal_connect(G_OBJECT(clearButton), "clicked", G_CALLBACK(clear_button_clicked_cb), (gpointer) k_a_w);
	gtk_box_pack_start(GTK_BOX(buttonbox), clearButton, FALSE, FALSE, 3);
	eb->clearButton = clearButton;

	scaleButton = gtk_button_new_from_stock(GTK_STOCK_REFRESH);
	xmi_msim_gui_utils_update_button_text(scaleButton, "Scale");
	g_signal_connect(G_OBJECT(scaleButton), "clicked", G_CALLBACK(scale_button_clicked_cb), (gpointer) k_a_w);
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

void energy_window_hide_cb(GtkWidget *widget, GtkWidget *window) {
	int i;
	GtkTreeIter iter;



	//window is hiding
	if (discOrCont == DISCRETE && energy_disc == NULL) {
		return;
	}
	else if (discOrCont == CONTINUOUS && energy_cont == NULL) {
		return;
	}

	if (addOrEdit == ENERGY_ADD) {
		//update undo buffer and afterwards update store
		if (discOrCont == DISCRETE) {
			//check if the energy is not present already
			if (current->xi->excitation->n_discrete > 0 && bsearch(energy_disc, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy line: the energy already exists in the list of lines.");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				g_free(energy_disc);
				energy_disc = NULL;
				return;
			}
			update_undo_buffer(DISCRETE_ENERGY_ADD,NULL);

		}
		else if (discOrCont == CONTINUOUS) {
			//check if the energy is not present already
			if (current->xi->excitation->n_continuous > 0 && bsearch(energy_cont, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add new energy interval: the energy already exists in the list of intervals.");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				g_free(energy_cont);
				energy_cont = NULL;
				return;
			}
			update_undo_buffer(CONTINUOUS_ENERGY_ADD,NULL);

		}
	}
	else if (addOrEdit == ENERGY_EDIT) {
		//update undo buffer and afterwards update store
		if (discOrCont == DISCRETE) {
			//check if the energy is not present already
			if (xmi_cmp_struct_xmi_energy_discrete(current->xi->excitation->discrete+current_index,energy_disc) != 0  && bsearch(energy_disc, current->xi->excitation->discrete, current->xi->excitation->n_discrete, sizeof(struct xmi_energy_discrete), xmi_cmp_struct_xmi_energy_discrete) != NULL) {
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not modify energy line: the energy already exists in the list of lines.");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				g_free(energy_disc);
				energy_disc = NULL;
				return;
			}
			update_undo_buffer(DISCRETE_ENERGY_EDIT,NULL);

		}
		else if (discOrCont == CONTINUOUS) {
			//check if the energy is not present already
			if (xmi_cmp_struct_xmi_energy_continuous(current->xi->excitation->continuous+current_index,energy_cont) != 0 && bsearch(energy_cont, current->xi->excitation->continuous, current->xi->excitation->n_continuous, sizeof(struct xmi_energy_continuous), xmi_cmp_struct_xmi_energy_continuous) != NULL) {
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not modify energy interval: the energy already exists in the list of intervals.");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				g_free(energy_cont);
				energy_cont = NULL;
				return;
			}
			update_undo_buffer(CONTINUOUS_ENERGY_EDIT,NULL);

		}
	}

	if (discOrCont == DISCRETE) {
		gtk_list_store_clear(discWidget->store);
		for (i = 0 ; i < (current)->xi->excitation->n_discrete ; i++) {
			gtk_list_store_append(discWidget->store, &iter);
			gtk_list_store_set(discWidget->store, &iter,
				ENERGY_COLUMN, (current)->xi->excitation->discrete[i].energy,
				HOR_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].horizontal_intensity,
				VER_INTENSITY_COLUMN, (current)->xi->excitation->discrete[i].vertical_intensity,
				SIGMA_X_COLUMN, (current)->xi->excitation->discrete[i].sigma_x,
				SIGMA_XP_COLUMN,(current)->xi->excitation->discrete[i].sigma_xp,
				SIGMA_Y_COLUMN,(current)->xi->excitation->discrete[i].sigma_y,
				SIGMA_YP_COLUMN,(current)->xi->excitation->discrete[i].sigma_yp,
				DISTRIBUTION_TYPE_COLUMN,(current)->xi->excitation->discrete[i].distribution_type,
				SCALE_PARAMETER_COLUMN,(current)->xi->excitation->discrete[i].scale_parameter,
				-1);
		}
	}
	else if (discOrCont == CONTINUOUS) {
		gtk_list_store_clear(contWidget->store);
		for (i = 0 ; i < (current)->xi->excitation->n_continuous ; i++) {
			gtk_list_store_append(contWidget->store, &iter);
			gtk_list_store_set(contWidget->store, &iter,
				ENERGY_COLUMN, (current)->xi->excitation->continuous[i].energy,
				HOR_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].horizontal_intensity,
				VER_INTENSITY_COLUMN, (current)->xi->excitation->continuous[i].vertical_intensity,
				SIGMA_X_COLUMN, (current)->xi->excitation->continuous[i].sigma_x,
				SIGMA_XP_COLUMN,(current)->xi->excitation->continuous[i].sigma_xp,
				SIGMA_Y_COLUMN,(current)->xi->excitation->continuous[i].sigma_y,
				SIGMA_YP_COLUMN,(current)->xi->excitation->continuous[i].sigma_yp,
				-1);
		}
	}

	adjust_save_buttons();
}


void energy_window_show_cb(GtkWidget *widget, gpointer data) {
	struct energyDialog *ew = (struct energyDialog *) data;
	char *buffer;



	g_signal_handler_block(G_OBJECT(ew->energyEntry),ew->energyGulong);
	g_signal_handler_block(G_OBJECT(ew->hor_intensityEntry),ew->hor_intensityGulong);
	g_signal_handler_block(G_OBJECT(ew->ver_intensityEntry),ew->ver_intensityGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_xEntry),ew->sigma_xGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_yEntry),ew->sigma_yGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_xpEntry),ew->sigma_xpGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_ypEntry),ew->sigma_ypGulong);
	if (discOrCont == DISCRETE) {
		g_signal_handler_block(G_OBJECT(ew->scale_parameterEntry),ew->scale_parameterGulong);
		g_signal_handler_block(G_OBJECT(ew->distribution_typeCombo),ew->distribution_typeGulong);
	}



	if (addOrEdit == ENERGY_ADD) {
		//add mode
		//set everything to zero
		gtk_widget_set_sensitive(ew->okButton,FALSE);
		gtk_entry_set_text(GTK_ENTRY(ew->energyEntry),"");
		gtk_entry_set_text(GTK_ENTRY(ew->hor_intensityEntry),"");
		gtk_entry_set_text(GTK_ENTRY(ew->ver_intensityEntry),"");
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_xEntry),"");
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_yEntry),"");
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_xpEntry),"");
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_ypEntry),"");
		if (discOrCont == DISCRETE) {
			gtk_combo_box_set_active(GTK_COMBO_BOX(ew->distribution_typeCombo), XMI_DISCRETE_MONOCHROMATIC);
			gtk_widget_hide(ew->scale_parameterBox);
			gtk_entry_set_text(GTK_ENTRY(ew->scale_parameterEntry),"");
			gtk_widget_set_sensitive(ew->scale_parameterEntry, FALSE);
		}
		gtk_widget_modify_base(ew->energyEntry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->hor_intensityEntry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->ver_intensityEntry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_xEntry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_yEntry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_xpEntry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(ew->sigma_ypEntry,GTK_STATE_NORMAL,NULL);
		if (discOrCont == DISCRETE) {
			gtk_widget_modify_base(ew->scale_parameterEntry,GTK_STATE_NORMAL,NULL);
		}
	}
	else if (addOrEdit == ENERGY_EDIT){
		//edit mode
		//set values
		if (discOrCont == DISCRETE) {
			gtk_widget_set_sensitive(ew->okButton,TRUE);
			buffer = g_strdup_printf("%g",energy_disc->energy);
			gtk_entry_set_text(GTK_ENTRY(ew->energyEntry),buffer);
			buffer = g_strdup_printf("%g",energy_disc->horizontal_intensity);
			gtk_entry_set_text(GTK_ENTRY(ew->hor_intensityEntry),buffer);
			buffer = g_strdup_printf("%g",energy_disc->vertical_intensity);
			gtk_entry_set_text(GTK_ENTRY(ew->ver_intensityEntry),buffer);
			buffer = g_strdup_printf("%g",energy_disc->sigma_x);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_xEntry),buffer);
			buffer = g_strdup_printf("%g",energy_disc->sigma_y);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_yEntry),buffer);
			buffer = g_strdup_printf("%g",energy_disc->sigma_xp);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_xpEntry),buffer);
			buffer = g_strdup_printf("%g",energy_disc->sigma_yp);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_ypEntry),buffer);
			gtk_combo_box_set_active(GTK_COMBO_BOX(ew->distribution_typeCombo), energy_disc->distribution_type);
			if (energy_disc->distribution_type == XMI_DISCRETE_MONOCHROMATIC) {
				gtk_widget_hide(ew->scale_parameterBox);
				//gtk_entry_set_text(GTK_ENTRY(ew->scale_parameterEntry), "");
				//gtk_widget_set_sensitive(ew->scale_parameterEntry, FALSE);
			}
			else if (energy_disc->distribution_type == XMI_DISCRETE_GAUSSIAN){
				buffer = g_strdup_printf("%g",energy_disc->scale_parameter);
				gtk_entry_set_text(GTK_ENTRY(ew->scale_parameterEntry),buffer);
				gtk_widget_set_sensitive(ew->scale_parameterEntry, TRUE);
				gtk_label_set_text(GTK_LABEL(ew->scale_parameterLabel),"Standard deviation (keV)");
				gtk_widget_show_all(ew->scale_parameterBox);
			}
			else if (energy_disc->distribution_type == XMI_DISCRETE_LORENTZIAN){
				buffer = g_strdup_printf("%g",energy_disc->scale_parameter);
				gtk_entry_set_text(GTK_ENTRY(ew->scale_parameterEntry),buffer);
				gtk_widget_set_sensitive(ew->scale_parameterEntry, TRUE);
				gtk_label_set_text(GTK_LABEL(ew->scale_parameterLabel),"Scale parameter (keV)");
				gtk_widget_show_all(ew->scale_parameterBox);
			}
		}
		else if (discOrCont == CONTINUOUS) {
			gtk_widget_set_sensitive(ew->okButton,TRUE);
			buffer = g_strdup_printf("%g",energy_cont->energy);
			gtk_entry_set_text(GTK_ENTRY(ew->energyEntry),buffer);
			buffer = g_strdup_printf("%g",energy_cont->horizontal_intensity);
			gtk_entry_set_text(GTK_ENTRY(ew->hor_intensityEntry),buffer);
			buffer = g_strdup_printf("%g",energy_cont->vertical_intensity);
			gtk_entry_set_text(GTK_ENTRY(ew->ver_intensityEntry),buffer);
			buffer = g_strdup_printf("%g",energy_cont->sigma_x);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_xEntry),buffer);
			buffer = g_strdup_printf("%g",energy_cont->sigma_y);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_yEntry),buffer);
			buffer = g_strdup_printf("%g",energy_cont->sigma_xp);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_xpEntry),buffer);
			buffer = g_strdup_printf("%g",energy_cont->sigma_yp);
			gtk_entry_set_text(GTK_ENTRY(ew->sigma_ypEntry),buffer);
		}
	}

	g_signal_handler_unblock(G_OBJECT(ew->energyEntry),ew->energyGulong);
	g_signal_handler_unblock(G_OBJECT(ew->hor_intensityEntry),ew->hor_intensityGulong);
	g_signal_handler_unblock(G_OBJECT(ew->ver_intensityEntry),ew->ver_intensityGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_xEntry),ew->sigma_xGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_yEntry),ew->sigma_yGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_xpEntry),ew->sigma_xpGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_ypEntry),ew->sigma_ypGulong);
	if (discOrCont == DISCRETE) {
		g_signal_handler_unblock(G_OBJECT(ew->scale_parameterEntry),ew->scale_parameterGulong);
		g_signal_handler_unblock(G_OBJECT(ew->distribution_typeCombo),ew->distribution_typeGulong);
	}

	gtk_widget_grab_default(ew->okButton);

	return;
}

static void distribution_type_changed_cb(GtkComboBox *combobox, struct energyDialog *ed) {
	gint active = gtk_combo_box_get_active(combobox);

	g_signal_handler_block(G_OBJECT(ed->scale_parameterEntry),ed->scale_parameterGulong);
	gtk_widget_modify_base(ed->scale_parameterEntry,GTK_STATE_NORMAL,NULL);
	gtk_entry_set_text(GTK_ENTRY(ed->scale_parameterEntry),"");
	g_signal_handler_unblock(G_OBJECT(ed->scale_parameterEntry),ed->scale_parameterGulong);

	if (active == XMI_DISCRETE_MONOCHROMATIC) {
		//monochromatic
		gtk_widget_hide(ed->scale_parameterBox);
	}
	else if (active == XMI_DISCRETE_GAUSSIAN){
		gtk_widget_set_sensitive(ed->scale_parameterEntry, TRUE);
		gtk_label_set_text(GTK_LABEL(ed->scale_parameterLabel),"Standard deviation (keV)");
		gtk_widget_show_all(ed->scale_parameterBox);
	}
	else if (active == XMI_DISCRETE_LORENTZIAN){
		gtk_widget_set_sensitive(ed->scale_parameterEntry, TRUE);
		gtk_label_set_text(GTK_LABEL(ed->scale_parameterLabel),"Scale parameter (keV)");
		gtk_widget_show_all(ed->scale_parameterBox);
	}

	energy_window_changed_cb(NULL, ed);

	return;
}

static struct energyDialog *initialize_energy_widget(GtkWidget *main_window,int kind) {
	GtkWidget *window;
	GtkWidget *mainVBox;
	GtkWidget *HBox;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *entry;
	struct energyDialog *rv;
	GtkWidget *label;
	GtkWidget *separator;

	rv= (struct energyDialog *) g_malloc(sizeof(struct energyDialog));


	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	rv->window = window;
	gtk_window_set_title(GTK_WINDOW(window), "Modify energy");
	gtk_window_set_default_size(GTK_WINDOW(window),420,300);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	g_signal_connect(G_OBJECT(window), "show",G_CALLBACK(energy_window_show_cb), (gpointer) rv);
	g_signal_connect(G_OBJECT(window), "hide",G_CALLBACK(energy_window_hide_cb), main_window);
	g_signal_connect(G_OBJECT(window), "delete-event",G_CALLBACK(delete_layer_widget), NULL);

	mainVBox = gtk_vbox_new(FALSE, 5);
	gtk_container_set_border_width(GTK_CONTAINER(mainVBox),5);
	gtk_container_add(GTK_CONTAINER(window), mainVBox);

	//Energy
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Energy (keV)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->energyGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->energyEntry = entry;

	//horizontal intensity
	HBox = gtk_hbox_new(FALSE,2);
	if (kind == DISCRETE)
		label = gtk_label_new("Horizontally polarized intensity (ph/s)");
	else
		label = gtk_label_new("Horizontally polarized intensity (ph/s/keV)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->hor_intensityGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->hor_intensityEntry = entry;

	//vertical intensity
	HBox = gtk_hbox_new(FALSE,2);
	if (kind == DISCRETE)
		label = gtk_label_new("Vertically polarized intensity (ph/s)");
	else
		label = gtk_label_new("Vertically polarized intensity (ph/s/keV)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->ver_intensityGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->ver_intensityEntry = entry;

	//source size x
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source size x (cm)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->sigma_xGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_xEntry = entry;

	//source size y
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source size y (cm)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->sigma_yGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_yEntry = entry;

	//source divergence x
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source divergence x (rad)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->sigma_xpGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_xpEntry = entry;

	//source divergence y
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source divergence y (rad)");
	entry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
	rv->sigma_ypGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_ypEntry = entry;

	if (kind == DISCRETE) {
		//distribution type
		HBox = gtk_hbox_new(FALSE,2);
		label = gtk_label_new("Energy distribution type");
		rv->distribution_typeCombo = gtk_combo_box_text_new();
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(rv->distribution_typeCombo), "Monochromatic");
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(rv->distribution_typeCombo), "Gaussian");
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(rv->distribution_typeCombo), "Lorentzian");
		rv->distribution_typeGulong = g_signal_connect(G_OBJECT(rv->distribution_typeCombo), "changed", G_CALLBACK(distribution_type_changed_cb), (gpointer) rv);
		gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
		gtk_box_pack_end(GTK_BOX(HBox), rv->distribution_typeCombo, FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

		//scale parameter
		HBox = gtk_hbox_new(FALSE,2);
		label = gtk_label_new("Scale parameter (keV)");
		entry = gtk_entry_new();
		gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
		rv->scale_parameterGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
		gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
		gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
		gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
		rv->scale_parameterEntry = entry;
		rv->scale_parameterLabel = label;
		rv->scale_parameterBox = HBox;
	}
	//separator
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), separator, FALSE, FALSE, 3);

	//ok, cancel...
	okButton = gtk_button_new_from_stock(GTK_STOCK_OK);
	cancelButton = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
	g_signal_connect(G_OBJECT(cancelButton),"clicked", G_CALLBACK(energy_ok_cancel_button_clicked_cb), (gpointer) rv);
	g_signal_connect(G_OBJECT(okButton),"clicked", G_CALLBACK(energy_ok_cancel_button_clicked_cb), (gpointer) rv);
	HBox = gtk_hbox_new(TRUE,2);
	gtk_box_pack_start(GTK_BOX(HBox), okButton, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), cancelButton, TRUE , FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->okButton = okButton;
	rv->cancelButton = cancelButton;
	gtk_widget_set_can_default(okButton, TRUE);
	gtk_widget_grab_default(okButton);

	return rv;
}


GtkWidget *initialize_energies(struct xmi_excitation *excitation, GtkWidget *main_window) {
	GtkWidget *mainvbox;
	GtkWidget *separator;



	//dialog initialization first
	energyDialog_disc = initialize_energy_widget(main_window, DISCRETE);
	energyDialog_cont = initialize_energy_widget(main_window, CONTINUOUS);

	mainvbox = gtk_vbox_new(FALSE, 5);
	gtk_container_set_border_width(GTK_CONTAINER(mainvbox), 10);

	//discrete first...
	discWidget = initialize_single_energies((void *) excitation->discrete, excitation->n_discrete,DISCRETE, main_window);
	contWidget = initialize_single_energies((void *) excitation->continuous, excitation->n_continuous,CONTINUOUS, main_window);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Discrete energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), discWidget->widget, FALSE, FALSE, 2);
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainvbox), separator, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Continuous energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), contWidget->widget, FALSE, FALSE, 2);




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
		g_fprintf(stderr,"Could not open file %s\n", filename);
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
				g_fprintf(stderr,"Syntax error in file %s at line %i after reading %u lines of %i requested\nNumber of columns must be 2, 3 or 7!\n", filename, lines_read, (unsigned int) nxe, nlines);
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
				g_fprintf(stderr,"Warning: Duplicate discrete line energy detected\nAdding to existing discrete line\n");
				find_res->horizontal_intensity += temp.horizontal_intensity;
				find_res->vertical_intensity += temp.vertical_intensity;
			}
		}
		if (nxe == nlines)
			break;
	}


	if (fclose(fp) != 0) {
		g_fprintf(stderr,"Could not close file %s\n", filename);
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
		g_fprintf(stderr,"Could not open file %s\n", filename);
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
				g_fprintf (stderr,"Syntax error in file %s at line %i after reading %u lines of %i requested\nNumber of columns must be 2, 3 or 7!\n", filename, lines_read, (unsigned int) nxe, nlines);
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
				g_fprintf(stderr,"Duplicate energies found in %s\n", filename);
				return -4;
			}
		}
		if (nxe == nlines)
			break;
	}


	if (fclose(fp) != 0) {
		g_fprintf(stderr,"Could not close file %s\n", filename);
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
				g_fprintf(stderr, "Error: Two consecutive continuous intensity densities cannot both have a total intensity of zero\n");
				return -5;

			}
		}
	}

	*energies = xe;

	return nxe;
}
