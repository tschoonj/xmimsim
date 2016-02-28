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

#include <config.h>
#include "xmimsim-gui.h"
#include "xmimsim-gui-layer.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-compound-dialog.h"
#include <stdlib.h>
#include <string.h>
#include "xraylib.h"
#include "xmi_aux.h"
#include <glib/gprintf.h>


extern GdkColor white;
extern GdkColor red;


enum {
	SYMBOL_COLUMN,
	WEIGHT_COLUMN,
	N_COLUMNS_LAYER
};

struct add_data {
	GtkListStore *store;
	struct xmi_layer **layer;
	GtkWidget *sumEntry;
	GtkWidget *tree;
	GtkTreeSelection *select;
	gulong densityG;
	GtkWidget *densityEntry;
	GtkWidget *okButton;
	GtkWidget *addToCatalogButton;
};

static gboolean delete_layer_widget(GtkWidget *widget, GdkEvent *event, gpointer data) {
	return TRUE;
}

static void element_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	struct layerWidget *ad = (struct layerWidget *) data;


	int nselected = gtk_tree_selection_count_selected_rows(selection);
	switch (nselected) {
		case 0:
			gtk_widget_set_sensitive(ad->editButton, FALSE);
			gtk_widget_set_sensitive(ad->removeButton, FALSE);
			break;
		case 1:
			gtk_widget_set_sensitive(ad->editButton, TRUE);
			gtk_widget_set_sensitive(ad->removeButton, TRUE);
			break;
		default:
			gtk_widget_set_sensitive(ad->editButton, FALSE);
			gtk_widget_set_sensitive(ad->removeButton, TRUE);
			break;
	}



}

static void window_show_cb(GtkWidget *window, gpointer data) {

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
		gtk_widget_set_sensitive(lw->addToCatalogButton, TRUE);
		gtk_widget_set_sensitive(lw->okButton, TRUE);
	}
	else {
		//clear it
		gtk_list_store_clear(lw->store);
		gtk_label_set_markup(GTK_LABEL(lw->sumEntry),"<span weight=\"bold\">0.0</span>");
		gtk_entry_set_text(GTK_ENTRY(lw->densityEntry),"");
		gtk_entry_set_text(GTK_ENTRY(lw->thicknessEntry),"");
		gtk_widget_set_sensitive(lw->okButton, FALSE);
		gtk_widget_set_sensitive(lw->addToCatalogButton, FALSE);
	}
	gtk_widget_modify_base(lw->densityEntry,GTK_STATE_NORMAL,NULL);
	gtk_widget_modify_base(lw->thicknessEntry,GTK_STATE_NORMAL,NULL);

	g_signal_handler_unblock(G_OBJECT(lw->densityEntry),lw->densityG);
	g_signal_handler_unblock(G_OBJECT(lw->thicknessEntry),lw->thicknessG);


	gtk_widget_grab_default(lw->okButton);

}

static void normalize_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;
	double sum;
	int i;
	GtkTreeIter iter;

	if (*(ad->layer) != NULL && (*(ad->layer))->n_elements > 0) {
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


static void density_thickness_changed_cb(GtkWidget *widget, gpointer data) {
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
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,NULL);
		else {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(lw->okButton,FALSE);
			gtk_widget_set_sensitive(lw->addToCatalogButton,FALSE);
		}
	}
	else if (widget ==  lw->thicknessEntry) {
		if (thickness_ok)
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,NULL);
		else {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(lw->okButton,FALSE);
			gtk_widget_set_sensitive(lw->addToCatalogButton,FALSE);
		}
	}

	if (thickness_ok && density_ok && layer_ok) {
		gtk_widget_set_sensitive(lw->okButton,TRUE);
		gtk_widget_set_sensitive(lw->addToCatalogButton,TRUE);
		(*(lw->my_layer))->thickness = thickness;
		(*(lw->my_layer))->density = density;
	}

	return;
}



static void ok_cancel_button_clicked_cb(GtkWidget *widget, gpointer data) {
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

	gtk_widget_hide(lw->window);
}


static void add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;

	//make entries empty and disable OK button
	GtkWidget *dialog = xmi_msim_gui_compound_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(ad->tree)), XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT);
	//xmi_msim_gui_compound_dialog_set_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog),  element);
	//xmi_msim_gui_compound_dialog_set_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog),  weight);

	int rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (rv == GTK_RESPONSE_ACCEPT) {
		//something was changed
		gchar *compound = xmi_msim_gui_compound_dialog_get_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog));
		gdouble weight = xmi_msim_gui_compound_dialog_get_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog));

		struct compoundData *cd, *cd2, *cdsum;
		if ((cd2 = CompoundParser(compound)) == NULL) {
			fprintf(stderr,"compoundParser error in add_button_clicked_cb\n");
			exit(1);
		}
		if (*(ad->layer) != NULL && (*(ad->layer))->n_elements > 0) {
			//copy xmi_layer to compoundData and add current contents
			cd = xmi_layer2compoundData(*(ad->layer));
			//calculate sum
			cdsum = add_compound_data(*cd, 1.0, *cd2, weight/100.0);
			double density =(*(ad->layer))->density;
			double thickness=(*(ad->layer))->thickness;
			xmi_free_layer(*(ad->layer));
			free(*(ad->layer));
			*(ad->layer) = compoundData2xmi_layer(cdsum);
			(*(ad->layer))->thickness = thickness;
			(*(ad->layer))->density = density;
			FreeCompoundData(cdsum);
			FreeCompoundData(cd);
		}
		else if (*(ad->layer) == NULL || (*(ad->layer))->n_elements == 0) {
			double density = 0.0, thickness = 0.0;
			if ((*(ad->layer))->n_elements == 0) {
				density =(*(ad->layer))->density;
				thickness=(*(ad->layer))->thickness;
				free(*(ad->layer));
			}	
			*(ad->layer) = compoundData2xmi_layer(cd2);
			(*(ad->layer))->thickness = density;
			(*(ad->layer))->density = thickness;
			xmi_scale_double((*(ad->layer))->weight, (*(ad->layer))->n_elements, weight/100.0);
			if ((*(ad->layer))->n_elements == 1) {
				double density = ElementDensity((*(ad->layer))->Z[0]);
				gchar *buffer = g_strdup_printf("%f", density);
				g_signal_handler_block(G_OBJECT(ad->densityEntry), ad->densityG);
				gtk_entry_set_text(GTK_ENTRY(ad->densityEntry), buffer);
				g_signal_handler_unblock(G_OBJECT(ad->densityEntry), ad->densityG);
				(*(ad->layer))->density = 0.0;
				g_free(buffer);
			}
		}
		else {
			fprintf(stdout,"error in dialog_buttons_clicked_cb\n");
			exit(1);
		}
		FreeCompoundData(cd2);

		//update store
		gtk_list_store_clear(ad->store);
		GtkTreeIter iter;
		int i;
		for (i = 0 ; i < (*(ad->layer))->n_elements ; i++) {
			gtk_list_store_append(ad->store, &iter);
			gtk_list_store_set(ad->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(ad->layer))->Z[i]),
				WEIGHT_COLUMN,  (*(ad->layer))->weight[i]*100.0,
				-1
			);
		}
		gchar *buffer = g_strdup_printf("<span weight=\"bold\">%lg</span>", xmi_sum_double((*(ad->layer))->weight,(*(ad->layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(ad->sumEntry), buffer);
		g_free(buffer);
	}

	gtk_widget_destroy(dialog);

	return;
}

static void edit_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;
	GtkTreeIter iter;
	GtkTreeModel *model;
	int *indices;
	gchar *element;
	double weight;
	GList *paths;

	//get selection
	paths = gtk_tree_selection_get_selected_rows(ad->select, &model);
	GtkTreePath *path = (GtkTreePath *) g_list_nth_data(paths, 0);
	indices = gtk_tree_path_get_indices(path);
	gtk_tree_model_get_iter(model, &iter, path);

	//get data from selected
	gtk_tree_model_get(model, &iter, SYMBOL_COLUMN,  &element, WEIGHT_COLUMN, &weight,  -1 );

	//put it in dialog
	GtkWidget *dialog = xmi_msim_gui_compound_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(ad->tree)), XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT);
	xmi_msim_gui_compound_dialog_set_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog),  element);
	xmi_msim_gui_compound_dialog_set_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog),  weight);

	int rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (rv == GTK_RESPONSE_ACCEPT) {
		//something was changed
		(*(ad->layer))->weight[indices[0]] = weight/100.0;

		//update store
		gtk_list_store_clear(ad->store);
		int i;
		for (i = 0 ; i < (*(ad->layer))->n_elements ; i++) {
			gtk_list_store_append(ad->store, &iter);
			gtk_list_store_set(ad->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(ad->layer))->Z[i]),
				WEIGHT_COLUMN,  (*(ad->layer))->weight[i]*100.0,
				-1
			);
		}
		gchar *buffer = g_strdup_printf("<span weight=\"bold\">%lg</span>", xmi_sum_double((*(ad->layer))->weight,(*(ad->layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(ad->sumEntry), buffer);
		g_free(buffer);
	}

	g_free(element);

	gtk_widget_destroy(dialog);


	g_list_free_full(paths, (GDestroyNotify) gtk_tree_path_free);

	return;
}


static void name_entry_changed_cb(GtkEntry *nameEntry, GtkWidget *okButton) {
	if (strlen(g_strstrip(g_strdup(gtk_entry_get_text(nameEntry)))) == 0) {
		//lil memory leak here
		gtk_widget_set_sensitive(okButton, FALSE);
	}
	else {
		gtk_widget_set_sensitive(okButton, TRUE);
	}
	return;
}

static void add_to_catalog_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct layerWidget *lw = (struct layerWidget *) data;
	GtkWidget *dialog;

	GtkWidget *content_area, *vbox;

	dialog = gtk_dialog_new_with_buttons("Add current layer to the catalog", GTK_WINDOW(gtk_widget_get_toplevel(widget)), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
		GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	gtk_widget_set_size_request(dialog, 300,-1);

	content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
	vbox = gtk_vbox_new(FALSE,2);
	gtk_box_pack_start(GTK_BOX(vbox), gtk_label_new("Choose a name for the layer"),TRUE, FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 15);
	GtkWidget *nameEntry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(vbox), nameEntry,TRUE, TRUE, 2);
	GtkWidget *okButton = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
	gtk_widget_set_sensitive(okButton, FALSE);
	g_signal_connect(G_OBJECT(nameEntry),"changed",G_CALLBACK(name_entry_changed_cb), (gpointer) okButton);

	gtk_widget_show_all(vbox);
	gtk_container_add (GTK_CONTAINER (content_area), vbox);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		gchar *layer_name = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(nameEntry))));
		GtkWidget *dialog2;
		if (xmimsim_gui_add_user_defined_layer(*(lw->my_layer), layer_name) == 1) {
			dialog2 = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Layer %s has been added to the catalog", layer_name);
		}
		else {
			dialog2 = gtk_message_dialog_new(GTK_WINDOW(dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add layer %s to the catalog.\nReport this incident to the developers.", layer_name);
		}
		gtk_dialog_run(GTK_DIALOG(dialog2));
		gtk_widget_destroy(dialog2);
		g_free(layer_name);
	}
	gtk_widget_destroy(dialog);
	return;
}

static void predef_combo_changed_cb(GtkWidget *comboBox, GtkToggleButton *radio) {
	gtk_toggle_button_set_active(radio, TRUE);
}

static void predef_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct layerWidget *lw = (struct layerWidget *) data;
	GtkWidget *dialog;
	char **list = GetCompoundDataNISTList(NULL);
	GtkWidget *content_area, *vbox;

	dialog = gtk_dialog_new_with_buttons("Select a layer composition", GTK_WINDOW(gtk_widget_get_toplevel(widget)), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
		GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

	content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
	vbox = gtk_vbox_new(FALSE,2);

	GtkWidget *nist_radio = gtk_radio_button_new_with_label_from_widget(NULL, "NIST compositions");
	//gtk_box_pack_start(GTK_BOX(vbox), gtk_label_new("NIST compositions"),TRUE, FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 15);

	GtkWidget *listW = gtk_combo_box_text_new();

	int i;
	for (i = 0 ; list[i] != NULL ; i++) {
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(listW), list[i]);
		xrlFree(list[i]);
	}
	xrlFree(list);
	gtk_combo_box_set_active(GTK_COMBO_BOX(listW), 0);
	//gtk_container_add(GTK_CONTAINER(nist_radio), );
	gtk_box_pack_start(GTK_BOX(vbox), nist_radio, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), listW, TRUE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(), TRUE, FALSE, 2);

	GtkWidget *user_radio = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(nist_radio), "User-defined compositions");
	GtkWidget *list_userW = gtk_combo_box_text_new();

	list = xmimsim_gui_get_user_defined_layer_names();
	if (list != NULL && g_strv_length(list) > 0) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(user_radio), TRUE);
		for (i = 0 ; list[i] != NULL ; i++) {
			gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(list_userW), list[i]);
			g_free(list[i]);
		}
		g_free(list);
		gtk_combo_box_set_active(GTK_COMBO_BOX(list_userW), 0);
	}
	else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nist_radio), TRUE);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(list_userW), "(Empty)");
		gtk_widget_set_sensitive(user_radio, FALSE);
		gtk_combo_box_set_active(GTK_COMBO_BOX(list_userW), 0);
		gtk_widget_set_sensitive(list_userW, FALSE);
	}
	gtk_box_pack_start(GTK_BOX(vbox), user_radio, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), list_userW, TRUE, FALSE, 2);
	gtk_widget_show_all(vbox);


	gtk_container_add (GTK_CONTAINER (content_area), vbox);
	g_signal_connect(G_OBJECT(listW), "changed", G_CALLBACK(predef_combo_changed_cb), nist_radio);
	g_signal_connect(G_OBJECT(list_userW), "changed", G_CALLBACK(predef_combo_changed_cb), user_radio);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(nist_radio))) {
			int nistCompNumber = gtk_combo_box_get_active(GTK_COMBO_BOX(listW));
			struct compoundDataNIST *cdn = GetCompoundDataNISTByIndex(nistCompNumber);
			if (cdn == NULL) {
				fprintf(stderr,"Fatal error in GetCompoundDataNISTListByIndex\n");
				exit(1);
			}
			double thickness = 0.0;
			if (*(lw->my_layer) != NULL) {
				thickness = (*(lw->my_layer))->thickness;
				if ((*(lw->my_layer))->n_elements > 0) {
					free((*(lw->my_layer))->Z);
					free((*(lw->my_layer))->weight);
				}
				free(*(lw->my_layer));
			}
			else if (gtk_widget_get_sensitive(lw->okButton)){
				//get thickness from widget
				thickness = strtod(gtk_entry_get_text(GTK_ENTRY(lw->thicknessEntry)),NULL);
			}
			else
				thickness = 0.0;

			double sum = xmi_sum_double(cdn->massFractions, cdn->nElements);
			xmi_scale_double(cdn->massFractions, cdn->nElements, 1.0/sum);
			*(lw->my_layer) = compoundDataNIST2xmi_layer(cdn);

			//this next line is fishy
			(*(lw->my_layer))->thickness = thickness;
			FreeCompoundDataNIST(cdn);
		}
		else {
			gchar *user_comp;
			user_comp = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(list_userW));
			struct xmi_layer *xl = xmimsim_gui_get_user_defined_layer(user_comp);
			g_free(user_comp);
			if (xl == NULL) {
				fprintf(stderr,"Fatal error in xmimsim_gui_get_user_defined_layer\n");
				exit(1);
			}
			if (*(lw->my_layer)) {
				xmi_free_layer(*(lw->my_layer));
				free(*(lw->my_layer));
			}
			*(lw->my_layer) = xl;

		}
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
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(user_radio))) {
			sprintf(buffer,"%g", (*(lw->my_layer))->thickness);
			gtk_entry_set_text(GTK_ENTRY(lw->thicknessEntry), buffer);
		}
	}
	gtk_widget_destroy(dialog);
}

static void remove_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct add_data *ad = (struct add_data *) data;
	GtkTreeIter iter;
	int i;
	char buffer[512];
	GtkTreePath *path;

	int n_elements = (*(ad->layer))->n_elements;
	int j;

	for (i = n_elements-1 ; i >= 0 ; i--) {
		path = gtk_tree_path_new_from_indices(i, -1);
		if (gtk_tree_selection_path_is_selected(ad->select, path)) {
			//row is selected
			gtk_tree_model_get_iter (GTK_TREE_MODEL (ad->store),  &iter, path);
			gtk_list_store_remove(ad->store, &iter);
			//remove element from my_layer
			for (j = i ; j < (*(ad->layer))->n_elements-1 ; j++) {
				(*(ad->layer))->weight[j] =(*(ad->layer))->weight[j+1];
				(*(ad->layer))->Z[j] =(*(ad->layer))->Z[j+1];

			}
			(*(ad->layer))->weight = (double *) realloc((*(ad->layer))->weight, sizeof(double)*((*(ad->layer))->n_elements-1));
			(*(ad->layer))->Z = (int *) realloc((*(ad->layer))->Z, sizeof(int)*((*(ad->layer))->n_elements-1));
			(*(ad->layer))->n_elements--;
		}
		gtk_tree_path_free(path);
	}
	sprintf(buffer,"<span weight=\"bold\">%lg</span>", xmi_sum_double((*(ad->layer))->weight, (*(ad->layer))->n_elements )*100.0);
	gtk_label_set_markup(GTK_LABEL(ad->sumEntry), buffer);
	if ((*(ad->layer))->n_elements == 0) {
		gtk_widget_set_sensitive(ad->okButton, FALSE);
		gtk_widget_set_sensitive(ad->addToCatalogButton, FALSE);
	}
/*	else{
		//select next line if available
		if (index == nindices -1)
			gtk_tree_selection_select_path(ad->select,gtk_tree_path_new_from_indices(nindices-2,-1));
		else
			gtk_tree_selection_select_path(ad->select,gtk_tree_path_new_from_indices(index,-1));

	}*/

}

static gboolean backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, gpointer data) {
	if (event->keyval == gdk_keyval_from_name("BackSpace")) {
		remove_button_clicked_cb(widget,data);
		return TRUE;
	}

	return FALSE;
}

static void element_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data) {
	struct add_data *ad = (struct add_data *) user_data;
	gint *indices;
	GtkTreeIter iter;
	gchar *element;
	double weight;

	indices = gtk_tree_path_get_indices(path);

	gtk_tree_model_get_iter(GTK_TREE_MODEL(ad->store), &iter, path);
	gtk_tree_model_get(GTK_TREE_MODEL(ad->store), &iter,  SYMBOL_COLUMN,  &element, WEIGHT_COLUMN, &weight,  -1 );

	GtkWidget *dialog = xmi_msim_gui_compound_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(tree_view))), XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT);
	xmi_msim_gui_compound_dialog_set_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog),  element);
	xmi_msim_gui_compound_dialog_set_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog),  weight);

	int rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (rv == GTK_RESPONSE_ACCEPT) {
		//something was changed
		(*(ad->layer))->weight[indices[0]] = weight/100.0;

		//update store
		gtk_list_store_clear(ad->store);
		int i;
		for (i = 0 ; i < (*(ad->layer))->n_elements ; i++) {
			gtk_list_store_append(ad->store, &iter);
			gtk_list_store_set(ad->store, &iter,
				SYMBOL_COLUMN, 	AtomicNumberToSymbol((*(ad->layer))->Z[i]),
				WEIGHT_COLUMN,  (*(ad->layer))->weight[i]*100.0,
				-1
			);
		}
		gchar *buffer = g_strdup_printf("<span weight=\"bold\">%lg</span>", xmi_sum_double((*(ad->layer))->weight,(*(ad->layer))->n_elements )*100.0);
		gtk_label_set_markup(GTK_LABEL(ad->sumEntry), buffer);
		g_free(buffer);
	}

	g_free(element);

	gtk_widget_destroy(dialog);

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
	GtkWidget *addToCatalogButton;
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
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Element", renderer,"text",SYMBOL_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
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
	gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE);
	g_signal_connect(G_OBJECT(select), "changed",
			G_CALLBACK(element_selection_changed_cb),
			(gpointer) rv
		);


	//add/edit/remove
	VBox = gtk_vbox_new(FALSE,5);
	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	removeButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	predefButton = gtk_button_new_with_label("Load from catalog");
	addToCatalogButton = gtk_button_new_with_label("Add to catalog");

	gtk_widget_set_sensitive(addButton, TRUE);
	gtk_widget_set_sensitive(editButton, FALSE);
	gtk_widget_set_sensitive(removeButton, FALSE);
	gtk_widget_set_sensitive(predefButton, TRUE);
	gtk_widget_set_sensitive(addToCatalogButton, FALSE);

	gtk_box_pack_start(GTK_BOX(VBox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), editButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), removeButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), predefButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), addToCatalogButton, TRUE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(HBox),VBox, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

	g_signal_connect(G_OBJECT(addButton),"clicked", G_CALLBACK(add_button_clicked_cb), (gpointer) ad );
	g_signal_connect(G_OBJECT(editButton),"clicked", G_CALLBACK(edit_button_clicked_cb), (gpointer) ad );
	g_signal_connect(G_OBJECT(removeButton),"clicked", G_CALLBACK(remove_button_clicked_cb), (gpointer) ad );
	g_signal_connect(G_OBJECT(predefButton),"clicked", G_CALLBACK(predef_button_clicked_cb), (gpointer) rv );
	g_signal_connect(G_OBJECT(addToCatalogButton),"clicked", G_CALLBACK(add_to_catalog_button_clicked_cb), (gpointer) rv );
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
	rv->addToCatalogButton = addToCatalogButton;

	ad->store = store;
	ad->layer = my_layer;
	ad->sumEntry = sumEntry;
	ad->densityEntry = densityEntry;
	ad->select = select;
	ad->tree = tree;
	ad->densityG = rv->densityG;
	ad->okButton = okButton;
	ad->addToCatalogButton = addToCatalogButton;


	g_signal_connect(G_OBJECT(tree), "key-press-event", G_CALLBACK(backspace_key_clicked), (gpointer) ad);

	return rv;
}
