#include "xmimsim-gui-layer.h"
#include <stdlib.h>
#include <string.h>
#include "xraylib.h"


extern GdkColor white;
extern GdkColor red;



enum {
	SYMBOL_COLUMN,
	WEIGHT_COLUMN,
	N_COLUMNS_LAYER
};


void window_show_cb(GtkWidget *window, gpointer data) {

#if DEBUG == 1
	fprintf(stdout,"window is showing\n");
#endif


}

void ok_cancel_button_clicked_cb(GtkWidget *widget, gpointer data) {
	GtkWidget *window = (GtkWidget *) data;

	gtk_widget_hide_all(window);	

}

void add_edit_button_clicked_cb(GtkWidget *widget, gpointer data) {
	GtkWidget *window = (GtkWidget *) data;

	gtk_widget_show_all(window);
	
}


struct add_edit_data {
	GtkListStore *store;
	struct xmi_layer *layer;
	GtkWidget *compoundWidget;
}; 


void add_button_clicked_cb(GtkWidget *button, gpointer data) {

}

struct compoundWidget {
	GtkWidget *dialog;
	GtkWidget *compoundEntry;
	GtkWidget *weightEntry;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
};

void compound_changed(GtkWidget * widget, gpointer data) {
	char *textPtr,*textPtr2,*endPtr,*lastPtr;
	struct compoundWidget *cw = (struct compoundWidget *) data;
	struct compoundData cd;
	double weight;
	int cp_rv;

	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(cw->compoundEntry));
	textPtr2 = (char *) gtk_entry_get_text(GTK_ENTRY(cw->weightEntry));
	weight = strtod(textPtr2, &endPtr);
	cp_rv = CompoundParser(textPtr, &cd); 

#if DEBUG == 1
	fprintf(stdout,"weight: %lf\n",weight);

#endif

	lastPtr = textPtr2 + strlen(textPtr2);

#if DEBUG == 1
	fprintf(stdout,"lastPtr: %p\n",lastPtr);
	fprintf(stdout,"endPtr: %p\n",endPtr);
#endif

	if (widget == cw->compoundEntry) {
		if (cp_rv == 1) {
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
			FREE_COMPOUND_DATA(cd);
		}
		else {
			//bad value
			gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			gtk_widget_set_sensitive(cw->okButton,FALSE);
		}
		if (cp_rv == 1 && lastPtr == endPtr && weight > 0.0)
			gtk_widget_set_sensitive(cw->okButton,TRUE);
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
		if (cp_rv == 1 && lastPtr == endPtr && weight > 0.0) {
			gtk_widget_set_sensitive(cw->okButton,TRUE);
			FREE_COMPOUND_DATA(cd);
		}
	}



	

}




void dialog_buttons_clicked_cb (GtkDialog *dialog, gint response_id, gpointer data) {

	if (response_id == GTK_RESPONSE_REJECT) {
		gtk_widget_hide_all(GTK_WIDGET(dialog));
	}


}


struct compoundWidget *initialize_compound_widget(struct xmi_layer **compound, GtkWindow *parent) {

	GtkWidget *dialog = gtk_dialog_new_with_buttons ("Enter/modify a compound", parent, GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

	GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *HBox;
	GtkWidget *compoundEntry;
	GtkWidget *weightEntry;
	GtkWidget *label;
	GtkWidget *okButton=gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
	GtkWidget *cancelButton=gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_REJECT);
	struct compoundWidget *rv;

	gtk_widget_set_sensitive(okButton, FALSE);

	rv= (struct compoundWidget *) malloc(sizeof(struct compoundWidget));

	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Compound");
	compoundEntry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), compoundEntry, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(contentArea), HBox, FALSE, FALSE, 1);

	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Weightfraction (%)");
	weightEntry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(HBox), weightEntry, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(contentArea), HBox, FALSE, FALSE, 1);

	

	rv->dialog = dialog;
	rv->compoundEntry = compoundEntry;
	rv->weightEntry = weightEntry;
	rv->okButton = okButton;
	rv->cancelButton = cancelButton;

	g_signal_connect(G_OBJECT(dialog),"response", G_CALLBACK(dialog_buttons_clicked_cb), rv);
	g_signal_connect(G_OBJECT(compoundEntry), "changed", G_CALLBACK(compound_changed), rv);
	g_signal_connect(G_OBJECT(weightEntry), "changed", G_CALLBACK(compound_changed), rv);


	return rv;
}


struct layerWidget * initialize_layer_widget(struct xmi_layer **layer) {
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
	GtkWidget *normalizeButton;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *label;
	GtkWidget *sumEntry;
	GtkWidget *densityEntry;
	GtkWidget *thicknessEntry;
	GtkWidget *separator;
	GtkListStore *store;
	GtkTreeIter iter;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkWidget *scrolledWindow;
	struct xmi_layer *compound;
	struct compoundWidget *cw;

	rv = (struct layerWidget *) malloc(sizeof(struct layerWidget));
	


	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Modify layer");
	gtk_window_set_default_size(GTK_WINDOW(window),400,400);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	g_signal_connect(G_OBJECT(window), "show",G_CALLBACK(window_show_cb), NULL);

	//initialize compound
	cw = initialize_compound_widget(&compound, GTK_WINDOW(window));	

	mainVBox = gtk_vbox_new(FALSE, 5);
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
	gtk_widget_set_size_request(scrolledWindow, 150,150);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolledWindow), tree);
	gtk_box_pack_start(GTK_BOX(HBox),scrolledWindow, FALSE, FALSE,3 );

	//add/edit/remove
	VBox = gtk_vbox_new(FALSE,5);
	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	removeButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);

	gtk_box_pack_start(GTK_BOX(VBox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), editButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(VBox), removeButton, TRUE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(HBox),VBox, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

	g_signal_connect(G_OBJECT(addButton),"clicked", G_CALLBACK(add_edit_button_clicked_cb), (gpointer) cw->dialog );

	//Sum and normalize
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Weights sum");
	sumEntry = gtk_entry_new();
	normalizeButton = gtk_button_new_with_label("Normalize");
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), sumEntry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), normalizeButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

	//separator
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), separator, FALSE, FALSE, 3);

	//Density and thickness
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Density");
	densityEntry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), densityEntry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	HBox = gtk_hbox_new(FALSE,2);
	label = gtk_label_new("Thickness");
	thicknessEntry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), thicknessEntry, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
	
	//separator
	separator = gtk_hseparator_new();
	gtk_box_pack_start(GTK_BOX(mainVBox), separator, FALSE, FALSE, 3);

	//ok, cancel...
	okButton = gtk_button_new_from_stock(GTK_STOCK_OK);
	cancelButton = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
	g_signal_connect(G_OBJECT(cancelButton),"clicked", G_CALLBACK(ok_cancel_button_clicked_cb), (gpointer) window);
	HBox = gtk_hbox_new(FALSE,2);
	gtk_box_pack_start(GTK_BOX(HBox), okButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(HBox), cancelButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

	//end of drawing widgets

#if DEBUG == 1
	fprintf(stdout,"window pointer upon creation: %p\n",window);
#endif



	rv->window = window;
	rv->store = store;


	return rv;
}

