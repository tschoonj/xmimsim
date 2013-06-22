/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-layer.h"
#include "xmimsim-gui.h"
#include <stdlib.h>
#include <string.h>
#include "xraylib.h"
#include "xmi_aux.h"


extern GdkColor white;
extern GdkColor red;



enum {
	SYMBOL_COLUMN,
	WEIGHT_COLUMN,
	N_COLUMNS_LAYER
};

struct compoundWidget {
	GtkWidget *dialog;
	GtkWidget *compoundEntry;
	GtkWidget *weightEntry;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	int kind;
	struct layerWidget *lw;
	int index;
};

struct add_data {
	GtkListStore *store;
	struct xmi_layer **layer;
	struct compoundWidget *cw;
	GtkWidget *sumEntry;
	GtkWidget *tree;
	GtkTreeSelection *select;
}; 


typedef struct _ResponseData ResponseData;

struct _ResponseData {
	gint response_id;
};

static void response_data_free (gpointer data) {
	g_slice_free (ResponseData, data);
}

static ResponseData* get_response_data (GtkWidget *widget, gboolean   create) {
	ResponseData *ad = g_object_get_data (G_OBJECT (widget),"gtk-dialog-response-data");

       	if (ad == NULL && create) {
		ad = g_slice_new (ResponseData);
		g_object_set_data_full (G_OBJECT (widget), g_intern_static_string("gtk-dialog-response-data"), ad, response_data_free);
	}

	return ad;
}

GtkWidget* my_gtk_dialog_get_widget_for_response (GtkDialog *dialog,gint       response_id) {
	GList *children;
	GList *tmp_list;

	g_return_val_if_fail (GTK_IS_DIALOG (dialog), NULL);

	children = gtk_container_get_children (GTK_CONTAINER (dialog->action_area));

	tmp_list = children;
	while (tmp_list != NULL) {
		GtkWidget *widget = tmp_list->data;
		ResponseData *rd = get_response_data (widget, FALSE);

		if (rd && rd->response_id == response_id) {
			g_list_free (children);
			return widget;
		}

		tmp_list = g_list_next (tmp_list);
	}

	g_list_free (children);

	return NULL;
}




static gboolean delete_layer_widget(GtkWidget *widget, GdkEvent *event, gpointer data) {
	return TRUE;
}

static void element_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	gboolean valid;
	int index,nindices;
	struct layerWidget *ad = (struct layerWidget *) data;


	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(selection, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}

		gtk_widget_set_sensitive(ad->editButton, TRUE);
		gtk_widget_set_sensitive(ad->removeButton, TRUE);

	}
	else {
		gtk_widget_set_sensitive(ad->editButton, FALSE);
		gtk_widget_set_sensitive(ad->removeButton, FALSE);
	}
	return;
}
void window_show_cb(GtkWidget *window, gpointer data) {

	struct layerWidget *lw = (struct layerWidget *) data;
	char buffer[512];
	int i;
	GtkTreeIter iter;

#if DEBUG == 1
	fprintf(stdout,"window is showing\n");
#endif

	//let's have a look at the value of my_layer
#if DEBUG == 1
	if (*(lw->my_layer) == NULL) {
		fprintf(stdout,"ADD button clicked\n");
	}
	else
		fprintf(stdout,"EDIT button clicked\n");
#endif

	g_signal_handler_block(G_OBJECT(lw->densityEntry),lw->densityG);
	g_signal_handler_block(G_OBJECT(lw->thicknessEntry),lw->thicknessG);


	if (*(lw->my_layer) != NULL) {
		//editing layer
		//density
		sprintf(buffer,"%g", (*(lw->my_layer))->density);
		gtk_entry_set_text(GTK_ENTRY(lw->densityEntry), buffer);
		//thickness
		sprintf(buffer,"%g", (*(lw->my_layer))->thickness);
		gtk_entry_set_text(GTK_ENTRY(lw->thicknessEntry), buffer);
		//sum
#if DEBUG == 1
		fprintf(stdout,"n_elements: %i\n",(*(lw->my_layer))->n_elements);
		fprintf(stdout,"first element: %lf\n",(*(lw->my_layer))->weight[0]);
#endif
		sprintf(buffer,"<span weight=\"bold\">%lg</span>", xmi_sum_double((*(lw->my_layer))->weight,(*(lw->my_layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(lw->sumEntry), buffer);

		//fill up the different elements
		gtk_list_store_clear(lw->store);
		for (i = 0 ; i < (*(lw->my_layer))->n_elements ; i++) {
			gtk_list_store_append(lw->store, &iter);
			gtk_list_store_set(lw->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(lw->my_layer))->Z[i]),
				WEIGHT_COLUMN,  (*(lw->my_layer))->weight[i]*100.0,
				-1
			);
		} 


	}
	else {
		//clear it
		gtk_list_store_clear(lw->store);
		gtk_label_set_markup(GTK_LABEL(lw->sumEntry),"<span weight=\"bold\">0.0</span>");
		gtk_entry_set_text(GTK_ENTRY(lw->densityEntry),"");
		gtk_entry_set_text(GTK_ENTRY(lw->thicknessEntry),"");
		gtk_widget_set_sensitive(lw->okButton, FALSE);
		gtk_widget_modify_base(lw->densityEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(lw->thicknessEntry,GTK_STATE_NORMAL,&white);
	}

	g_signal_handler_unblock(G_OBJECT(lw->densityEntry),lw->densityG);
	g_signal_handler_unblock(G_OBJECT(lw->thicknessEntry),lw->thicknessG);

	
	gtk_widget_grab_default(lw->okButton);

}

void normalize_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;
	double sum;
	int i;
	GtkTreeIter iter;

	if ((*(ad->layer))->n_elements > 0) {
		sum = xmi_sum_double((*(ad->layer))->weight,(*(ad->layer))->n_elements );
		gtk_label_set_markup(GTK_LABEL(ad->sumEntry),"<span weight=\"bold\">100.0</span>");
		xmi_scale_double((*(ad->layer))->weight,(*(ad->layer))->n_elements, 1.0/sum);	

		gtk_list_store_clear(ad->store);
		for (i = 0 ; i < (*(ad->layer))->n_elements ; i++) {
			gtk_list_store_append(ad->store, &iter);
			gtk_list_store_set(ad->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(ad->layer))->Z[i]),
				WEIGHT_COLUMN,  (*(ad->layer))->weight[i]*100.0,
				-1
			);
		} 
	}
}


void density_thickness_changed_cb(GtkWidget *widget, gpointer data) {
	char *textPtr,*textPtr2,*endPtr,*lastPtr,*endPtr2,*lastPtr2, *textPtr3, *lastPtr3,*endPtr3 ;
	struct layerWidget *lw = (struct layerWidget *) data;
	int density_ok, thickness_ok, layer_ok;
	double density, thickness, sum;

	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(lw->densityEntry));
	textPtr2 = (char *) gtk_entry_get_text(GTK_ENTRY(lw->thicknessEntry));
	textPtr3 = (char *) gtk_label_get_text(GTK_LABEL(lw->sumEntry));

	density = strtod(textPtr, &endPtr);
	thickness = strtod(textPtr2, &endPtr2);
	sum = strtod(textPtr3, &endPtr3);
	
	lastPtr = textPtr + strlen(textPtr);
	lastPtr2 = textPtr2 + strlen(textPtr2);
	lastPtr3 = textPtr3 + strlen(textPtr3);

	if (lastPtr == endPtr && density > 0.0)
		density_ok = 1;
	else
		density_ok = 0;

	if (lastPtr2 == endPtr2 && thickness > 0.0)
		thickness_ok = 1;
	else
		thickness_ok = 0;

	if (lastPtr3 == endPtr3 && sum > 0.0)
		layer_ok = 1;
	else
		layer_ok = 0;

	if (widget == lw->densityEntry) {
		if (density_ok)
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
		else {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(lw->okButton,FALSE);
		}
	}
	else if (widget ==  lw->thicknessEntry) {
		if (thickness_ok) 
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
		else {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(lw->okButton,FALSE);
		}
	}

	if (thickness_ok && density_ok && layer_ok) {
		gtk_widget_set_sensitive(lw->okButton,TRUE);
		(*(lw->my_layer))->thickness = thickness;
		(*(lw->my_layer))->density = density;
	}

	return;
}



void ok_cancel_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct layerWidget *lw = (struct layerWidget *) data;

	if (lw->okButton == widget) {
		//ok button clicked
#if DEBUG == 1
		fprintf(stdout,"ok button clicked in layerwidget\n");
#endif
	}
	else if (lw->cancelButton == widget) {
		//cancel button clicked
#if DEBUG == 1
		fprintf(stdout,"cancel button clicked in layerwidget\n");
#endif
		if (*(lw->my_layer)) {
			xmi_free_layer(*(lw->my_layer));
			free(*(lw->my_layer));
		}
		*(lw->my_layer) = NULL;
	}


	gtk_widget_hide_all(lw->window);	

}

enum {
	CW_ADD,
	CW_EDIT,
};



void add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;

	//make entries empty and disable OK button
	gtk_entry_set_text(GTK_ENTRY(ad->cw->compoundEntry), "");
	gtk_entry_set_text(GTK_ENTRY(ad->cw->weightEntry), "");
	gtk_widget_set_sensitive(ad->cw->okButton,FALSE);
	gtk_entry_set_editable(GTK_ENTRY(ad->cw->compoundEntry), TRUE);
	ad->cw->kind = CW_ADD;
	


	gtk_widget_show_all(ad->cw->dialog);
	return;	
}

void edit_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	gboolean valid;
	int index,nindices;
	gchar *element;
	double weight;
	char buffer[512];
	
	//get selection
	//the olde switcharooooo
	if (gtk_tree_selection_get_selected(ad->select, &model, &iter)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(ad->select, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}
		//get data from selected
		gtk_tree_model_get(model, &iter, SYMBOL_COLUMN,  &element, WEIGHT_COLUMN, &weight,  -1 );

		//put it in dialog
#if DEBUG == 1
		fprintf(stdout,"Editing element: %s and weight: %lf\n",element, weight);
#endif
		gtk_widget_set_sensitive(ad->cw->okButton,TRUE);
		gtk_entry_set_editable(GTK_ENTRY(ad->cw->compoundEntry), FALSE);
		ad->cw->kind = CW_EDIT;
		sprintf(buffer,"%g", weight);
		gtk_entry_set_text(GTK_ENTRY(ad->cw->compoundEntry), element);
		g_free(element);
		gtk_entry_set_text(GTK_ENTRY(ad->cw->weightEntry), buffer);
		ad->cw->index = index;

		gtk_widget_show_all(ad->cw->dialog);
	}

	return;
}

void predef_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct layerWidget *lw = (struct layerWidget *) data;
	GtkWidget *dialog;
	char **list = GetCompoundDataNISTList(NULL);
	GtkWidget *content_area, *vbox;

	dialog = gtk_dialog_new_with_buttons("NIST composition", GTK_WINDOW(gtk_widget_get_toplevel(widget)), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	
	content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
	vbox = gtk_vbox_new(FALSE,2);
	gtk_box_pack_start(GTK_BOX(vbox), gtk_label_new("Select a composition from the list"),TRUE, FALSE, 2);

#if GTK_CHECK_VERSION(2,24,0)
	GtkWidget *listW = gtk_combo_box_text_new();
#else
	GtkWidget *listW = gtk_combo_box_new_text();
#endif

	int i;
	for (i = 0 ; list[i] != NULL ; i++) {
#if GTK_CHECK_VERSION(2,24,0)
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(listW), list[i]);
#else
		gtk_combo_box_append_text(GTK_COMBO_BOX(listW), list[i]);
#endif
		xrlFree(list[i]);
	}
	xrlFree(list);
	gtk_combo_box_set_active(GTK_COMBO_BOX(listW), 0);
	gtk_box_pack_start(GTK_BOX(vbox), listW, TRUE, FALSE, 2);
	gtk_widget_show_all(vbox);
	gtk_container_add (GTK_CONTAINER (content_area), vbox);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		int nistCompNumber = gtk_combo_box_get_active(GTK_COMBO_BOX(listW));	
		struct compoundDataNIST *cdn = GetCompoundDataNISTByIndex(nistCompNumber);
		if (cdn == NULL) {
			fprintf(stderr,"Fatal error in GetCompoundDataNISTListByIndex\n");
			exit(1);
		}
		double thickness;
		if (*(lw->my_layer) != NULL) {
			thickness = (*(lw->my_layer))->thickness;
			if ((*(lw->my_layer))->n_elements > 0) {
				free((*(lw->my_layer))->Z);
				free((*(lw->my_layer))->weight);
			}
			free(*(lw->my_layer));
		}
		double sum = xmi_sum_double(cdn->massFractions, cdn->nElements);
		xmi_scale_double(cdn->massFractions, cdn->nElements, 1.0/sum);
		*(lw->my_layer) = compoundDataNIST2xmi_layer(cdn);

		(*(lw->my_layer))->thickness = thickness;
		FreeCompoundDataNIST(cdn);
		//update store
		gtk_list_store_clear(lw->store);
		GtkTreeIter iter;
		for (i = 0 ; i < (*(lw->my_layer))->n_elements ; i++) {
			gtk_list_store_append(lw->store, &iter);
			gtk_list_store_set(lw->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(lw->my_layer))->Z[i]),
				WEIGHT_COLUMN,  (*(lw->my_layer))->weight[i]*100.0,
				-1
			);
		} 
		char buffer[512];
		sprintf(buffer,"<span weight=\"bold\">%lg</span>", xmi_sum_double((*(lw->my_layer))->weight,(*(lw->my_layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(lw->sumEntry), buffer);
		sprintf(buffer,"%g", (*(lw->my_layer))->density);
		gtk_entry_set_text(GTK_ENTRY(lw->densityEntry), buffer);
	}
	gtk_widget_destroy(dialog);
}
void remove_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	gboolean valid;
	int index,nindices;
	int i;
	char buffer[512];

	if (gtk_tree_selection_get_selected(ad->select, &model, &iter)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(ad->select, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}

		for (i = index ; i < nindices ; i++) {
			(*(ad->cw->lw->my_layer))->weight[i] =(*(ad->cw->lw->my_layer))->weight[i+1];
			(*(ad->cw->lw->my_layer))->Z[i] =(*(ad->cw->lw->my_layer))->Z[i+1];
		}
		(*(ad->cw->lw->my_layer))->weight = (double *) realloc((*(ad->cw->lw->my_layer))->weight, sizeof(double)*((*(ad->cw->lw->my_layer))->n_elements-1));
		(*(ad->cw->lw->my_layer))->Z = (int *) realloc((*(ad->cw->lw->my_layer))->Z, sizeof(int)*((*(ad->cw->lw->my_layer))->n_elements-1));
		(*(ad->cw->lw->my_layer))->n_elements--;
		gtk_list_store_remove(ad->store, &iter);
		sprintf(buffer,"<span weight=\"bold\">%lg</span>", xmi_sum_double((*(ad->cw->lw->my_layer))->weight,(*(ad->cw->lw->my_layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(ad->cw->lw->sumEntry), buffer);
		if ((*(ad->cw->lw->my_layer))->n_elements == 0)
			gtk_widget_set_sensitive(ad->cw->lw->okButton, FALSE);
		else{
			//select next line if available
			if (index == nindices -1)
				gtk_tree_selection_select_path(ad->select,gtk_tree_path_new_from_indices(nindices-2,-1));
			else 
				gtk_tree_selection_select_path(ad->select,gtk_tree_path_new_from_indices(index,-1));

		}
	}
}

void dialog_hide_cb(GtkWidget *widget, gpointer data) {
	struct compoundWidget * cw = (struct compoundWidget *) data;

	//check if OK button in layerwidget should be activated



}


void dialog_show_cb(GtkWidget *widget, gpointer data) {
	struct compoundWidget * cw = (struct compoundWidget *) data;

	gtk_widget_modify_base(cw->compoundEntry,GTK_STATE_NORMAL,&white);
	gtk_widget_modify_base(cw->weightEntry,GTK_STATE_NORMAL,&white);
	gtk_dialog_set_default_response(GTK_DIALOG(widget), GTK_RESPONSE_ACCEPT);

}




void compound_changed(GtkWidget * widget, gpointer data) {
	char *textPtr,*textPtr2,*endPtr,*lastPtr;
	struct compoundWidget *cw = (struct compoundWidget *) data;
	struct compoundData *cd;
	double weight;

	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(cw->compoundEntry));
	textPtr2 = (char *) gtk_entry_get_text(GTK_ENTRY(cw->weightEntry));
	weight = strtod(textPtr2, &endPtr);
	cd = CompoundParser(textPtr); 

#if DEBUG == 1
	fprintf(stdout,"weight: %lf\n",weight);

#endif

	lastPtr = textPtr2 + strlen(textPtr2);

#if DEBUG == 1
	fprintf(stdout,"lastPtr: %p\n",lastPtr);
	fprintf(stdout,"endPtr: %p\n",endPtr);
#endif

	if (widget == cw->compoundEntry) {
		if (cd) {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
			FreeCompoundData(cd);
		}
		else {
			//bad value
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(cw->okButton,FALSE);
		}
		if (cd && lastPtr == endPtr && weight > 0.0) {
			gtk_widget_set_sensitive(cw->okButton,TRUE);
		}
	}
	else if (widget == cw->weightEntry) {
		if (lastPtr == endPtr && weight > 0.0) {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
		}
		else {
			//bad value
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(cw->okButton,FALSE);
		}
		if (cd && lastPtr == endPtr && weight > 0.0) {
			gtk_widget_set_sensitive(cw->okButton,TRUE);
			FreeCompoundData(cd);
		}
	}



	

}



void dialog_buttons_clicked_cb (GtkDialog *dialog, gint response_id, gpointer data) {

	struct compoundWidget *cw = (struct compoundWidget *) data; 
	struct compoundData *cd, *cd2, *cdsum;
	char *textPtr;
	double weight;
	double density, thickness;
	int i;
	GtkTreeIter iter;
	char buffer[512];

#if DEBUG == 1
	fprintf(stdout,"Entering dialog_buttons_clicked_cb\n");
#endif
	if (response_id == GTK_RESPONSE_REJECT) {
		gtk_widget_hide_all(GTK_WIDGET(dialog));
	}

	else if (response_id == GTK_RESPONSE_ACCEPT) {
		//update
		if (cw->kind == CW_ADD) {

#if DEBUG == 1
	fprintf(stdout,"Before mallocing cd2\n");
#endif
			//adding something
			if ((cd2 = CompoundParser(gtk_entry_get_text(GTK_ENTRY(cw->compoundEntry)))) == NULL) {
				fprintf(stdout,"compoundParser error in dialog_buttons_clicked_cb\n");
				exit(1);
			}
#if DEBUG == 1
	fprintf(stdout,"After calling CompoundParser: compound is %s\n",gtk_entry_get_text(GTK_ENTRY(cw->compoundEntry)));
#endif
			weight = strtod(gtk_entry_get_text(GTK_ENTRY(cw->weightEntry)),NULL);
#if DEBUG == 1
	fprintf(stdout,"After calling CompoundParser: weight is %lf\n",weight);
#endif
			if (*(cw->lw->my_layer) != NULL && (*(cw->lw->my_layer))->n_elements > 0) {
				//copy xmi_layer to compoundData and add current contents
				cd = xmi_layer2compoundData(*(cw->lw->my_layer)  );
				//calculate sum
				cdsum = add_compound_data(*cd, 1.0, *cd2, weight/100.0);
				density =(*(cw->lw->my_layer))->density; 
				thickness=(*(cw->lw->my_layer))->thickness; 
				xmi_free_layer(*(cw->lw->my_layer));
				free( *(cw->lw->my_layer));
				*(cw->lw->my_layer) = compoundData2xmi_layer (cdsum);
				(*(cw->lw->my_layer))->thickness = thickness;
				(*(cw->lw->my_layer))->density = density;
				FreeCompoundData(cdsum);
				FreeCompoundData(cd);
			}
			else if (*(cw->lw->my_layer) == NULL) {
				*(cw->lw->my_layer) = compoundData2xmi_layer (cd2);
				(*(cw->lw->my_layer))->thickness = 0.0;
				(*(cw->lw->my_layer))->density = 0.0;
				xmi_scale_double((*(cw->lw->my_layer))->weight,(*(cw->lw->my_layer))->n_elements, weight/100.0);	
			}
			else if ((*(cw->lw->my_layer))->n_elements == 0) {
				free( *(cw->lw->my_layer));
				*(cw->lw->my_layer) = compoundData2xmi_layer (cd2);
				xmi_scale_double((*(cw->lw->my_layer))->weight,(*(cw->lw->my_layer))->n_elements, weight/100.0);	
			}
			else {
				fprintf(stdout,"error in dialog_buttons_clicked_cb\n");
				exit(1);
			}
			FreeCompoundData(cd2);
		}
		else if (cw->kind == CW_EDIT) {
			weight = strtod(gtk_entry_get_text(GTK_ENTRY(cw->weightEntry)),NULL);
			(*(cw->lw->my_layer))->weight[cw->index] = weight/100.0;
		}



		//update store
		gtk_list_store_clear(cw->lw->store);
		for (i = 0 ; i < (*(cw->lw->my_layer))->n_elements ; i++) {
			gtk_list_store_append(cw->lw->store, &iter);
			gtk_list_store_set(cw->lw->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(cw->lw->my_layer))->Z[i]),
				WEIGHT_COLUMN,  (*(cw->lw->my_layer))->weight[i]*100.0,
				-1
			);
		} 
		sprintf(buffer,"<span weight=\"bold\">%lg</span>", xmi_sum_double((*(cw->lw->my_layer))->weight,(*(cw->lw->my_layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(cw->lw->sumEntry), buffer);




		gtk_widget_hide_all(GTK_WIDGET(dialog));
	
	}


}

struct compoundWidget *initialize_compound_widget(struct layerWidget *lw, GtkWindow *parent) {

	GtkWidget *dialog = gtk_dialog_new_with_buttons ("Enter/modify a compound", parent, GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

	GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *HBox;
	GtkWidget *compoundEntry;
	GtkWidget *weightEntry;
	GtkWidget *label;
	GtkWidget *okButton=my_gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
	GtkWidget *cancelButton=my_gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_REJECT);

	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

	struct compoundWidget *rv;

	gtk_widget_set_sensitive(okButton, FALSE);

	rv= (struct compoundWidget *) malloc(sizeof(struct compoundWidget));

	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Compound");
	compoundEntry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(compoundEntry), TRUE);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), compoundEntry, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(contentArea), HBox, FALSE, FALSE, 1);

	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Weight fraction (%)");
	weightEntry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(weightEntry), TRUE);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), weightEntry, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(contentArea), HBox, FALSE, FALSE, 1);

	

	rv->dialog = dialog;
	rv->compoundEntry = compoundEntry;
	rv->weightEntry = weightEntry;
	rv->okButton = okButton;
	rv->cancelButton = cancelButton;
	rv->lw = lw;

	g_signal_connect(G_OBJECT(dialog),"response", G_CALLBACK(dialog_buttons_clicked_cb), rv);
	g_signal_connect(G_OBJECT(compoundEntry), "changed", G_CALLBACK(compound_changed), rv);
	g_signal_connect(G_OBJECT(weightEntry), "changed", G_CALLBACK(compound_changed), rv);
	g_signal_connect(G_OBJECT(dialog),"show", G_CALLBACK(dialog_show_cb), rv);
	g_signal_connect(G_OBJECT(dialog),"hide", G_CALLBACK(density_thickness_changed_cb), rv->lw);
	g_signal_connect(G_OBJECT(dialog),"delete-event",G_CALLBACK(gtk_widget_hide_on_delete), NULL);


	return rv;
}

void element_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data) {
	struct add_data *ad = (struct add_data *) user_data;
	gint *indices;
	gint depth;
	GtkTreeIter iter;
	GtkTreeModel *model;
	gchar *element;
	double weight;
	char buffer[512];

	//indices = gtk_tree_path_get_indices_with_depth(path,&depth);
	indices = gtk_tree_path_get_indices(path);
	depth = gtk_tree_path_get_depth(path);

	gtk_tree_model_get_iter(GTK_TREE_MODEL(ad->store), &iter, path);
	gtk_tree_model_get(GTK_TREE_MODEL(ad->store), &iter,  SYMBOL_COLUMN,  &element, WEIGHT_COLUMN, &weight,  -1 );
	gtk_widget_set_sensitive(ad->cw->okButton,TRUE);
	gtk_entry_set_editable(GTK_ENTRY(ad->cw->compoundEntry), FALSE);
	ad->cw->kind = CW_EDIT;
	sprintf(buffer,"%g", weight);
	gtk_entry_set_text(GTK_ENTRY(ad->cw->compoundEntry), element);
	g_free(element);
	gtk_entry_set_text(GTK_ENTRY(ad->cw->weightEntry), buffer);
	ad->cw->index = indices[0];

	gtk_widget_show_all(ad->cw->dialog);

	return;
}

struct layerWidget * initialize_layer_widget(struct xmi_layer **my_layer, GtkWidget *main_window) {
#if DEBUG == 1
	fprintf(stdout,"Entering initialize_layer_widget\n");
#endif
	struct layerWidget *rv;
	GtkWidget *window;
	GtkWidget *mainVBox;
	GtkWidget *HBox;
	GtkWidget *VBox;
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *removeButton;
	GtkWidget *predefButton;
	GtkWidget *normalizeButton;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *label;
	GtkWidget *sumEntry;
	GtkWidget *densityEntry;
	GtkWidget *thicknessEntry;
	GtkWidget *separator;
	GtkListStore *store;
	GtkWidget *tree;
	GtkTreeSelection *select;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkWidget *scrolledWindow;
	struct xmi_layer *compound;
	struct compoundWidget *cw;
	struct add_data *ad;

	rv = (struct layerWidget *) malloc(sizeof(struct layerWidget));
	ad = (struct add_data *) malloc(sizeof(struct add_data));


	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Modify layer");
	gtk_window_set_default_size(GTK_WINDOW(window),200,200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	g_signal_connect(G_OBJECT(window), "show",G_CALLBACK(window_show_cb), (gpointer) rv);
	g_signal_connect(G_OBJECT(window), "delete-event",G_CALLBACK(delete_layer_widget), NULL);

	//initialize compound
	cw = initialize_compound_widget(rv, GTK_WINDOW(window));	

	mainVBox = gtk_vbox_new(FALSE, 5);
	gtk_container_set_border_width(GTK_CONTAINER(mainVBox),5);
	gtk_container_add(GTK_CONTAINER(window), mainVBox);


	//connect window with widget show and hide signals!!!
	
	//construct store
	store = gtk_list_store_new(N_COLUMNS_LAYER, G_TYPE_STRING, G_TYPE_DOUBLE);
	
	//construct tree
	HBox = gtk_hbox_new(FALSE,5);
	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Element", renderer,"text",SYMBOL_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Weight fraction (%)", renderer,"text",WEIGHT_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	gtk_widget_set_size_request(scrolledWindow, 220,150);
	gtk_container_add(GTK_CONTAINER(scrolledWindow), tree);
	gtk_box_pack_start(GTK_BOX(HBox),scrolledWindow, FALSE, FALSE,3 );

	//selections
	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(select,GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed",
			G_CALLBACK(element_selection_changed_cb),
			(gpointer) rv
		);


	//add/edit/remove
	VBox = gtk_vbox_new(FALSE,5);
	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	removeButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	predefButton = gtk_button_new_with_label("NIST composition");

	gtk_widget_set_sensitive(addButton, TRUE);
	gtk_widget_set_sensitive(editButton, FALSE);
	gtk_widget_set_sensitive(removeButton, FALSE);
	gtk_widget_set_sensitive(predefButton, TRUE);

	gtk_box_pack_start(GTK_BOX(VBox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), editButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), removeButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), predefButton, TRUE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(HBox),VBox, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

	g_signal_connect(G_OBJECT(addButton),"clicked", G_CALLBACK(add_button_clicked_cb), (gpointer) ad );
	g_signal_connect(G_OBJECT(editButton),"clicked", G_CALLBACK(edit_button_clicked_cb), (gpointer) ad );
	g_signal_connect(G_OBJECT(removeButton),"clicked", G_CALLBACK(remove_button_clicked_cb), (gpointer) ad );
	g_signal_connect(G_OBJECT(predefButton),"clicked", G_CALLBACK(predef_button_clicked_cb), (gpointer) rv );
	g_signal_connect(G_OBJECT(tree),"row-activated", G_CALLBACK(element_row_activated_cb), (gpointer) ad);

	//Sum and normalize
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Weights sum (%)");
	sumEntry = gtk_label_new("");
	gtk_label_set_justify(GTK_LABEL(sumEntry), GTK_JUSTIFY_CENTER);
	normalizeButton = gtk_button_new_with_label("Normalize");
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), sumEntry, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), normalizeButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(normalizeButton), "clicked", G_CALLBACK(normalize_button_clicked_cb), (gpointer) ad);

	//separator
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), separator, FALSE, FALSE, 3);

	//Density and thickness
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label),"Density (g/cm<sup>3</sup>)");
	densityEntry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(densityEntry), TRUE);
	rv->densityG =g_signal_connect(G_OBJECT(densityEntry),"changed",G_CALLBACK(density_thickness_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), densityEntry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Thickness (cm)");
	thicknessEntry = gtk_entry_new();
	gtk_entry_set_activates_default(GTK_ENTRY(thicknessEntry), TRUE);
	rv->thicknessG = g_signal_connect(G_OBJECT(thicknessEntry),"changed",G_CALLBACK(density_thickness_changed_cb), (gpointer) rv);
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), thicknessEntry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	
	//separator
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), separator, FALSE, FALSE, 3);

	//ok, cancel...
	okButton = gtk_button_new_from_stock(GTK_STOCK_OK);
	cancelButton = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
	g_signal_connect(G_OBJECT(cancelButton),"clicked", G_CALLBACK(ok_cancel_button_clicked_cb), (gpointer) rv);
	g_signal_connect(G_OBJECT(okButton),"clicked", G_CALLBACK(ok_cancel_button_clicked_cb), (gpointer) rv);
	HBox = gtk_hbox_new(TRUE,2);
	gtk_box_pack_start(GTK_BOX(HBox), okButton, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), cancelButton, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	gtk_widget_set_can_default(okButton, TRUE);
	gtk_widget_grab_default(okButton);

	//end of drawing widgets

#if DEBUG == 1
	fprintf(stdout,"window pointer upon creation: %p\n",window);
#endif



	rv->window = window;
	rv->store = store;
	rv->my_layer = my_layer;
	rv->sumEntry = sumEntry;
	rv->densityEntry = densityEntry;
	rv->thicknessEntry = thicknessEntry;
	rv->editButton = editButton;
	rv->removeButton = removeButton;
	rv->cancelButton = cancelButton;
	rv->okButton = okButton;

	ad->store = store;
	ad->layer = my_layer;
	ad->cw = cw;
	ad->sumEntry = sumEntry;
	ad->select = select;
	ad->tree = tree;

	return rv;
}

