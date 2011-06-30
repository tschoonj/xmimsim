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
GtkTreeIter current_iter;


struct energiesWidget *contWidget;
struct energiesWidget *discWidget;



struct energyButtons {
	GtkWidget *editButton;
	GtkWidget *deleteButton;
};

static gboolean delete_layer_widget(GtkWidget *widget, GdkEvent *event, gpointer data) {
	return TRUE;
}

void energy_ok_cancel_button_clicked_cb(GtkWidget *widget, gpointer data) {
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

	gtk_widget_hide(ew->window);


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

	if(check_changeables() == 1 && xmi_validate_input(current->xi) == 0 ) {
		gtk_widget_set_sensitive(saveW,TRUE);
		gtk_widget_set_sensitive(save_asW,TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),TRUE);
	}
	else {
		gtk_widget_set_sensitive(saveW,FALSE);
		gtk_widget_set_sensitive(save_asW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);
	}

	
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


struct energiesWidget *initialize_single_energies(struct xmi_energy *energies, int n_energies, int kind) {
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
	GtkWidget *deleteButton;
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
	column = gtk_tree_view_column_new_with_attributes("Energy (keV)", renderer,"text",ENERGY_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Horizontal intensity (ph/s)", renderer,"text",HOR_INTENSITY_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Vertical intensity (ph/s)", renderer,"text",VER_INTENSITY_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Sigma x (cm)", renderer,"text",SIGMA_X_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Sigma y (cm)", renderer,"text",SIGMA_Y_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Sigma xp (rad)", renderer,"text",SIGMA_XP_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Sigma yp (rad)", renderer,"text",SIGMA_YP_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	gtk_widget_set_size_request(scrolledWindow, 700,100);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolledWindow), tree);
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
	
	if(check_changeables() == 1 && xmi_validate_input(current->xi) == 0 ) {
		gtk_widget_set_sensitive(saveW,TRUE);
		gtk_widget_set_sensitive(save_asW,TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),TRUE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),TRUE);
	}
	else {
		gtk_widget_set_sensitive(saveW,FALSE);
		gtk_widget_set_sensitive(save_asW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveT),FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(saveasT),FALSE);
	}

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

struct energyWidget *initialize_energy_widget() {
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


GtkWidget *initialize_energies(struct xmi_excitation *excitation) {
	GtkWidget *mainvbox;
	GtkWidget *separator;



	//dialog initialization first
	energyWidget_disc = initialize_energy_widget(); 
	energyWidget_cont = initialize_energy_widget(); 

	mainvbox = gtk_vbox_new(FALSE, 5);

	//discrete first...
	discWidget = initialize_single_energies(excitation->discrete, excitation->n_discrete,DISCRETE);
	contWidget = initialize_single_energies(excitation->continuous, excitation->n_continuous,CONTINUOUS);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Discrete energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), discWidget->widget, FALSE, FALSE, 2);
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainvbox), separator, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainvbox), gtk_label_new("Continuous energies"), FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainvbox), contWidget->widget, FALSE, FALSE, 2);



	return mainvbox;
}
