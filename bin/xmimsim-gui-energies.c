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



#include "xmimsim-gui-energies.h"
#include "xmimsim-gui.h"
#include "xmimsim-gui-layer.h"
#include <stdlib.h>
#include "xmi_aux.h"
#include <string.h>


struct energyWidget {
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *energyEntry;
	GtkWidget *hor_intensityEntry;
	GtkWidget *ver_intensityEntry;
	GtkWidget *sigma_xEntry;
	GtkWidget *sigma_yEntry;
	GtkWidget *sigma_xpEntry;
	GtkWidget *sigma_ypEntry;
	GtkWidget *window;
	gulong energyGulong;
	gulong hor_intensityGulong;
	gulong ver_intensityGulong;
	gulong sigma_xGulong;
	gulong sigma_yGulong;
	gulong sigma_xpGulong;
	gulong sigma_ypGulong;
};

struct xmi_energy *energy;
struct energyWidget *energyWidget_disc;
struct energyWidget *energyWidget_cont;

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
static GtkTreeIter current_iter;


struct energiesWidget *contWidget;
struct energiesWidget *discWidget;



struct energyButtons {
	GtkWidget *editButton;
	GtkWidget *deleteButton;
};

static int xmi_read_energies_from_ascii_file(gchar *filename, struct xmi_energy **energies);
static gboolean delete_layer_widget(GtkWidget *widget, GdkEvent *event, gpointer data) {
	return TRUE;
}


static void energy_print_double(GtkTreeViewColumn *column, GtkCellRenderer *renderer, GtkTreeModel *tree_model, GtkTreeIter *iter, gpointer data) {
	gdouble value;
	gchar *double_text;

	gtk_tree_model_get(tree_model,iter, GPOINTER_TO_INT(data), &value,-1);

	double_text = g_strdup_printf("%lg",value);
	g_object_set(G_OBJECT(renderer), "text", double_text, NULL);

	g_free(double_text);

	return;

}

static void clear_button_clicked_cb(GtkWidget *widget, GtkWidget *main_window) {
	gtk_list_store_clear(discWidget->store);
	update_undo_buffer(DISCRETE_ENERGY_CLEAR, NULL);


}
static void import_button_clicked_cb(GtkWidget *widget, GtkWidget *main_window) {
	
	//first launch a message box
	GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Import spectrum from file");
	gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), 
	"Files must be ascii files consisting of rows with either 2, 3 or 7 elements. "
	"First element must contain the energy (in keV). "
	"Second element must be the intensity: if there are only two elements, it is assumed to be unpolarized. "
	"If three elements are found, then the second and third elements are assumed to correspond to the horizontal and vertical polarized intensities. "
	"Seven elements are considered to be identical to the three elements case with additionally the source size x and y, as well as the source divergence x and y. "
	"Empty lines are ignored."
	);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_BUTTONS_CLOSE) {
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

	

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		gchar *filename;

		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_widget_destroy (dialog);	
    		int rv = xmi_read_energies_from_ascii_file(filename, &energy);
		if (rv > 0) {
			//success
			g_fprintf(stdout,"File %s read in successfully\n",filename);
			//now ask if we have to add or replace...
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "Add spectrum from file to current spectrum or replace it completely?");
			gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_ADD, GTK_RESPONSE_OK, GTK_STOCK_REFRESH, GTK_RESPONSE_CANCEL, NULL);
			GtkWidget *button = my_gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
			GList *children = gtk_container_get_children(GTK_CONTAINER(button));
			GtkWidget *temp = g_list_nth_data(children, 0);
			g_list_free(children);
			children = gtk_container_get_children(GTK_CONTAINER(temp));
			temp = g_list_nth_data(children, 0);
			g_list_free(children);
			children = gtk_container_get_children(GTK_CONTAINER(temp));
			gtk_label_set_text(GTK_LABEL(g_list_nth_data(children,1)), "Replace");
			g_list_free(children);
			//this may not work on all platforms -> Mac OS X
			gtk_window_set_deletable(GTK_WINDOW(dialog), FALSE);
		
			int rv2 = gtk_dialog_run (GTK_DIALOG (dialog));
			if (rv2 == GTK_RESPONSE_OK) {
				//add	
				update_undo_buffer(DISCRETE_ENERGY_IMPORT_ADD, GINT_TO_POINTER(rv));
			}
			else if (rv2 == GTK_RESPONSE_CANCEL) {
				//replace
				update_undo_buffer(DISCRETE_ENERGY_IMPORT_REPLACE, GINT_TO_POINTER(rv));
			}
			else {
				gtk_widget_destroy(dialog);
				g_free (filename);
				return;
			}
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
				-1);
			}
			adjust_save_buttons();
			gtk_widget_destroy(dialog);
			g_free (filename);

			return;
		}
		else if (rv == 0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "No valid values found in file %s\nNumber of columns must be 2, 3 or 7!\n", filename);
		}
		else if (rv == -1) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not open file %s",filename);
		}
		else if (rv == -2) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not close file %s",filename);
		}
		else if (rv == -3) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Syntax error in file %s\nNumber of columns must be 2, 3 or 7!\n", filename);
		}
		g_free (filename);
		gtk_dialog_run(GTK_DIALOG(dialog));
  	}
	gtk_widget_destroy (dialog);	
	energy = NULL;

	return;	
}

static void energy_ok_cancel_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct energyWidget *ew = (struct energyWidget *) data;
	
	if (widget == ew->okButton) {
		//ok clicked
	} 
	else if (widget == ew->cancelButton) {
		//cancel clicked
		//if Entries are red, make them white again
		gtk_widget_modify_base(ew->energyEntry, GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->hor_intensityEntry, GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->ver_intensityEntry, GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_xEntry, GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_yEntry, GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_xpEntry, GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_ypEntry, GTK_STATE_NORMAL,&white);
		

		free(energy);
		energy = NULL;
	}

	gtk_widget_hide_all(ew->window);


	return;
}



void energy_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	struct energyButtons *eb = (struct energyButtons *) data;
	GtkTreeIter temp_iter;
	GtkTreeModel *model;
	gboolean valid;


	if (gtk_tree_selection_get_selected(selection, &model, &current_iter)) {
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
		gtk_widget_set_sensitive(eb->deleteButton,TRUE);
		gtk_widget_set_sensitive(eb->editButton,TRUE);
	}
	else {
		gtk_widget_set_sensitive(eb->deleteButton,FALSE);
		gtk_widget_set_sensitive(eb->editButton,FALSE);
	}

	return;
}

void energy_window_changed_cb(GtkWidget *widget, gpointer data) {
	struct energyWidget *ew =  (struct energyWidget *) data;
	char *textPtr1, *textPtr2, *textPtr3, *textPtr4, *textPtr5, *textPtr6, *textPtr7;
	char *endPtr1, *endPtr2, *endPtr3, *endPtr4, *endPtr5, *endPtr6, *endPtr7;
	char *lastPtr1, *lastPtr2, *lastPtr3, *lastPtr4, *lastPtr5, *lastPtr6, *lastPtr7;

	int ok1, ok2, ok3, ok4, ok5, ok6, ok7;
	double value1, value2, value3, value4, value5, value6, value7;

	
	



	textPtr1 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->energyEntry));
	textPtr2 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->hor_intensityEntry));
	textPtr3 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->ver_intensityEntry));
	textPtr4 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_xEntry));
	textPtr5 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_yEntry));
	textPtr6 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_xpEntry));
	textPtr7 = (char *) gtk_entry_get_text(GTK_ENTRY(ew->sigma_ypEntry));

#define energy_short(n,my_entry) value ## n = strtod(textPtr ## n, &endPtr ## n);\
	lastPtr ## n = textPtr ## n + strlen(textPtr ## n);\
	if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0) \
		ok ## n = 1;\
	else\
		ok ## n = 0;\
	if (widget == my_entry) {\
		if (ok ## n)\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&white);\
		else {\
			gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&red);\
			gtk_widget_set_sensitive(ew->okButton, FALSE);\
		}\
	}

	energy_short(1, ew->energyEntry)
	energy_short(2, ew->hor_intensityEntry)
	energy_short(3, ew->ver_intensityEntry)
	energy_short(4, ew->sigma_xEntry)
	energy_short(5, ew->sigma_yEntry)
	energy_short(6, ew->sigma_xpEntry)
	energy_short(7, ew->sigma_ypEntry)

	if (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7) {
		gtk_widget_set_sensitive(ew->okButton, TRUE);
		energy->energy = value1;
		energy->horizontal_intensity = value2;
		energy->vertical_intensity = value3;
		energy->sigma_x = value4;
		energy->sigma_y = value5;
		energy->sigma_xp = value6;
		energy->sigma_yp = value7;
	}

	return;
}

void energy_delete_button_clicked_cb(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);
	int i;

	if (kind == DISCRETE) {
		//update undo buffer... and then the store
		gtk_list_store_remove(discWidget->store, &current_iter);
		update_undo_buffer(DISCRETE_ENERGY_DELETE,NULL);
	}
	else if (kind == CONTINUOUS) {
		gtk_list_store_remove(contWidget->store, &current_iter);
		update_undo_buffer(CONTINUOUS_ENERGY_DELETE,NULL);
	}

	adjust_save_buttons();
	
	return;
}

void energy_add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

	energy = (struct xmi_energy *) malloc(sizeof(struct xmi_energy));
	addOrEdit = ENERGY_ADD;
	discOrCont = kind; 


	gtk_widget_show_all(kind == DISCRETE ? energyWidget_disc->window :  energyWidget_cont->window);
	return;
}

void energy_edit_button_clicked_cb(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

#if DEBUG == 1
	fprintf(stdout,"current_index: %i\n",current_index);
	fprintf(stdout,"current discrete hor intensity: %lg\n",current->xi->excitation->discrete[0].horizontal_intensity);
#endif

	if (kind == DISCRETE) {
		energy = xmi_memdup(current->xi->excitation->discrete+current_index, sizeof(struct xmi_energy));	
	}
	else if (kind == CONTINUOUS) {
		energy = xmi_memdup(current->xi->excitation->continuous+current_index, sizeof(struct xmi_energy));	
	}
	else {
		fprintf(stderr,"Invalid kind detected\n");
		exit(1);
	}
	addOrEdit = ENERGY_EDIT;
	discOrCont = kind; 

	gtk_widget_show_all(kind == DISCRETE ? energyWidget_disc->window :  energyWidget_cont->window);
	
	return;
}


struct energiesWidget *initialize_single_energies(struct xmi_energy *energies, int n_energies, int kind, GtkWidget *main_window) {
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
	GtkWidget *EbelButton;
	GtkWidget *deleteButton;
	GtkWidget *clearButton;
	int i;

	struct energiesWidget *rv;
	struct energyButtons *eb;

	mainbox = gtk_hbox_new(FALSE, 5);

	store = gtk_list_store_new(NCOLUMNS_ENERGIES, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE);

	for (i = 0 ; i < n_energies ; i++) {
		gtk_list_store_append(store,&iter);
		gtk_list_store_set(store, &iter,
		ENERGY_COLUMN, energies[i].energy,
		HOR_INTENSITY_COLUMN, energies[i].horizontal_intensity,
		VER_INTENSITY_COLUMN, energies[i].vertical_intensity,
		SIGMA_X_COLUMN, energies[i].sigma_x,
		SIGMA_XP_COLUMN,energies[i].sigma_xp,
		SIGMA_Y_COLUMN,energies[i].sigma_y,
		SIGMA_YP_COLUMN,energies[i].sigma_yp,
		-1
		);
	}

	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Energy (keV)", renderer,"text",ENERGY_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Energy (keV)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(ENERGY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Horizontal intensity (ph/s)", renderer,"text",HOR_INTENSITY_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Horizontal intensity (ph/s)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(HOR_INTENSITY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Vertical intensity (ph/s)", renderer,"text",VER_INTENSITY_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Vertical intensity (ph/s)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(VER_INTENSITY_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma x (cm)", renderer,"text",SIGMA_X_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma x (cm)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_X_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma y (cm)", renderer,"text",SIGMA_Y_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma y (cm)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_Y_COLUMN),NULL);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma xp (rad)", renderer,"text",SIGMA_XP_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma xp (rad)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_XP_COLUMN),NULL);


	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	//column = gtk_tree_view_column_new_with_attributes("Sigma yp (rad)", renderer,"text",SIGMA_YP_COLUMN,NULL);
	column = gtk_tree_view_column_new();
	gtk_tree_view_column_set_title(column, "Sigma yp (rad)");
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_cell_data_func(column, renderer, energy_print_double, GINT_TO_POINTER(SIGMA_YP_COLUMN),NULL);

	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	gtk_widget_set_size_request(scrolledWindow, 660,100);
	gtk_container_add(GTK_CONTAINER(scrolledWindow), tree);
	gtk_box_pack_start(GTK_BOX(mainbox),scrolledWindow, FALSE, FALSE,3 );

	eb = (struct energyButtons *) malloc(sizeof(struct energyButtons));

	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(energy_selection_changed_cb), (gpointer) eb);

	buttonbox = gtk_vbox_new(FALSE, 5);
	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(energy_add_button_clicked_cb) , GINT_TO_POINTER(kind));
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	g_signal_connect(G_OBJECT(editButton), "clicked", G_CALLBACK(energy_edit_button_clicked_cb) , GINT_TO_POINTER(kind));
	deleteButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	g_signal_connect(G_OBJECT(deleteButton), "clicked", G_CALLBACK(energy_delete_button_clicked_cb) , GINT_TO_POINTER(kind));
	
	eb->editButton = editButton;
	eb->deleteButton = deleteButton;


	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), editButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), deleteButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainbox), buttonbox, FALSE, FALSE, 2);

	buttonbox = gtk_vbox_new(FALSE, 5);
	importButton = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	GList *children = gtk_container_get_children(GTK_CONTAINER(importButton));
	GtkWidget *temp = g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	temp = g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	gtk_label_set_text(GTK_LABEL(g_list_nth_data(children,1)), "Import");
	g_list_free(children);
	g_signal_connect(G_OBJECT(importButton), "clicked", G_CALLBACK(import_button_clicked_cb), main_window);
	gtk_box_pack_start(GTK_BOX(buttonbox), importButton, TRUE, FALSE, 3);

	EbelButton = gtk_button_new_from_stock(XMI_STOCK_RADIATION_WARNING);
	gtk_box_pack_start(GTK_BOX(buttonbox), EbelButton, TRUE, FALSE, 3);

	clearButton = gtk_button_new_from_stock(GTK_STOCK_CLEAR);
	g_signal_connect(G_OBJECT(clearButton), "clicked", G_CALLBACK(clear_button_clicked_cb), NULL);
	gtk_box_pack_start(GTK_BOX(buttonbox), clearButton, TRUE, FALSE, 3);
	


	gtk_box_pack_start(GTK_BOX(mainbox), buttonbox, FALSE, FALSE, 2);

	gtk_widget_set_sensitive(editButton, FALSE);
	gtk_widget_set_sensitive(deleteButton, FALSE);

	rv = (struct energiesWidget *) malloc(sizeof(struct energiesWidget));
	rv->store=store;
	rv->widget=mainbox;
	
	return rv;
}

void energy_window_hide_cb(GtkWidget *widget, gpointer data) {
	int i;
	GtkTreeIter iter;



	//window is hiding
	if (energy == NULL) {
		return;
	}	

	if (addOrEdit == ENERGY_ADD) {
		//update undo buffer and afterwards update store
		if (discOrCont == DISCRETE) {
			update_undo_buffer(DISCRETE_ENERGY_ADD,NULL);

		}
		else if (discOrCont == CONTINUOUS) {
			update_undo_buffer(CONTINUOUS_ENERGY_ADD,NULL);

		}
	} 
	else if (addOrEdit == ENERGY_EDIT) {
		//update undo buffer and afterwards update store
		if (discOrCont == DISCRETE) {
			update_undo_buffer(DISCRETE_ENERGY_EDIT,NULL);

		}
		else if (discOrCont == CONTINUOUS) {
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
	struct energyWidget *ew = (struct energyWidget *) data;
	char buffer[512];


	g_signal_handler_block(G_OBJECT(ew->energyEntry),ew->energyGulong);
	g_signal_handler_block(G_OBJECT(ew->hor_intensityEntry),ew->hor_intensityGulong);
	g_signal_handler_block(G_OBJECT(ew->ver_intensityEntry),ew->ver_intensityGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_xEntry),ew->sigma_xGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_yEntry),ew->sigma_yGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_xpEntry),ew->sigma_xpGulong);
	g_signal_handler_block(G_OBJECT(ew->sigma_ypEntry),ew->sigma_ypGulong);



	
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
		gtk_widget_modify_base(ew->energyEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->hor_intensityEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->ver_intensityEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_xEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_yEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_xpEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(ew->sigma_ypEntry,GTK_STATE_NORMAL,&white);
	}
	else if (addOrEdit == ENERGY_EDIT){
		//edit mode
		//set values
		gtk_widget_set_sensitive(ew->okButton,TRUE);
		sprintf(buffer,"%lf",energy->energy);
		gtk_entry_set_text(GTK_ENTRY(ew->energyEntry),buffer);
		sprintf(buffer,"%lg",energy->horizontal_intensity);
		gtk_entry_set_text(GTK_ENTRY(ew->hor_intensityEntry),buffer);
		sprintf(buffer,"%lg",energy->vertical_intensity);
		gtk_entry_set_text(GTK_ENTRY(ew->ver_intensityEntry),buffer);
		sprintf(buffer,"%lg",energy->sigma_x);
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_xEntry),buffer);
		sprintf(buffer,"%lg",energy->sigma_y);
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_yEntry),buffer);
		sprintf(buffer,"%lg",energy->sigma_xp);
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_xpEntry),buffer);
		sprintf(buffer,"%lg",energy->sigma_yp);
		gtk_entry_set_text(GTK_ENTRY(ew->sigma_ypEntry),buffer);
	}

	g_signal_handler_unblock(G_OBJECT(ew->energyEntry),ew->energyGulong);
	g_signal_handler_unblock(G_OBJECT(ew->hor_intensityEntry),ew->hor_intensityGulong);
	g_signal_handler_unblock(G_OBJECT(ew->ver_intensityEntry),ew->ver_intensityGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_xEntry),ew->sigma_xGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_yEntry),ew->sigma_yGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_xpEntry),ew->sigma_xpGulong);
	g_signal_handler_unblock(G_OBJECT(ew->sigma_ypEntry),ew->sigma_ypGulong);

	return;	
}

static struct energyWidget *initialize_energy_widget(GtkWidget *main_window) {
	GtkWidget *window;
	GtkWidget *mainVBox;
	GtkWidget *HBox;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *entry;
	struct energyWidget *rv;
	GtkWidget *label;
	GtkWidget *separator;

	rv= (struct energyWidget *) malloc(sizeof(struct energyWidget));


	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	rv->window = window;
	gtk_window_set_title(GTK_WINDOW(window), "Modify energy");
	gtk_window_set_default_size(GTK_WINDOW(window),420,300);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	g_signal_connect(G_OBJECT(window), "show",G_CALLBACK(energy_window_show_cb), (gpointer) rv);
	g_signal_connect(G_OBJECT(window), "hide",G_CALLBACK(energy_window_hide_cb), (gpointer) rv);
	g_signal_connect(G_OBJECT(window), "delete-event",G_CALLBACK(delete_layer_widget), NULL);

	mainVBox = gtk_vbox_new(FALSE, 5);
	gtk_container_set_border_width(GTK_CONTAINER(mainVBox),5);
	gtk_container_add(GTK_CONTAINER(window), mainVBox);

	//Energy
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Energy (keV)");
	entry = gtk_entry_new();
	rv->energyGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->energyEntry = entry;	

	//horizontal intensity
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Horizontally polarized intensity (ph/s)");
	entry = gtk_entry_new();
	rv->hor_intensityGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->hor_intensityEntry = entry;	

	//vertical intensity
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Vertically polarized intensity (ph/s)");
	entry = gtk_entry_new();
	rv->ver_intensityGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->ver_intensityEntry = entry;	

	//source size x
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source size x (cm)");
	entry = gtk_entry_new();
	rv->sigma_xGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_xEntry = entry;	

	//source size y
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source size y (cm)");
	entry = gtk_entry_new();
	rv->sigma_yGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_yEntry = entry;	

	//source divergence x
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source divergence x (rad)");
	entry = gtk_entry_new();
	rv->sigma_xpGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_xpEntry = entry;	

	//source divergence y
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Source divergence y (rad)");
	entry = gtk_entry_new();
	rv->sigma_ypGulong = g_signal_connect(G_OBJECT(entry),"changed",G_CALLBACK(energy_window_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), entry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	rv->sigma_ypEntry = entry;	

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


	return rv;
}


GtkWidget *initialize_energies(struct xmi_excitation *excitation, GtkWidget *main_window) {
	GtkWidget *mainvbox;
	GtkWidget *separator;



	//dialog initialization first
	energyWidget_disc = initialize_energy_widget(main_window); 
	energyWidget_cont = initialize_energy_widget(main_window); 

	mainvbox = gtk_vbox_new(FALSE, 5);

	//discrete first...
	discWidget = initialize_single_energies(excitation->discrete, excitation->n_discrete,DISCRETE, main_window);
	contWidget = initialize_single_energies(excitation->continuous, excitation->n_continuous,CONTINUOUS, main_window);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Discrete energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), discWidget->widget, FALSE, FALSE, 2);
	//separator = gtk_hseparator_new();
	//gtk_box_pack_start(GTK_BOX(mainvbox), separator, FALSE, FALSE, 3);
	//gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Continuous energies"), FALSE, FALSE, 2);
	//gtk_box_pack_start(GTK_BOX(mainvbox), contWidget->widget, FALSE, FALSE, 2);



	return mainvbox;
}


static int xmi_read_energies_from_ascii_file(gchar *filename, struct xmi_energy **energies) {
	FILE *fp;
	struct xmi_energy *xe = NULL;
	int nxe = 0;

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


	while ((linelen = getline(&line, &linecap, fp)) > -1) {
		//ignore empty lines
		if (linelen == 0 || strlen(g_strstrip(line)) == 0)
			continue;
		values = sscanf(line,"%lf %lf %lf %lf %lf %lf %lf", &energy, &horizontal_intensity, &vertical_intensity, &sigma_x, &sigma_y, &sigma_xp, &sigma_yp);
		xe = (struct xmi_energy *) realloc(xe, sizeof(struct xmi_energy) * ++nxe);
		xe[nxe-1].sigma_x = 0.0;
		xe[nxe-1].sigma_y = 0.0;
		xe[nxe-1].sigma_xp = 0.0;
		xe[nxe-1].sigma_yp = 0.0;

		switch (values) {
			case 7:
				xe[nxe-1].sigma_x = sigma_x;
				xe[nxe-1].sigma_y = sigma_y;
				xe[nxe-1].sigma_xp = sigma_xp;
				xe[nxe-1].sigma_yp = sigma_yp;
			case 3: 
				xe[nxe-1].horizontal_intensity = horizontal_intensity;
				xe[nxe-1].vertical_intensity = vertical_intensity;
				xe[nxe-1].energy = energy;
				break;
			case 2:
				xe[nxe-1].horizontal_intensity = horizontal_intensity/2.0;
				xe[nxe-1].vertical_intensity = horizontal_intensity/2.0;
				xe[nxe-1].energy = energy;
				break;
			default:
				g_fprintf(stderr,"Syntax error in file %s\nNumber of columns must be 2, 3 or 7!\n", filename);
				return -3;
		};
	}


	if (fclose(fp) != 0) {
		g_fprintf(stderr,"Could not close file %s\n", filename);
		return -2;
	}

	*energies = xe;

	return nxe;
}





