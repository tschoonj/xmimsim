#include "xmimsim-gui-layer.h"
#include <string.h>
#include <stdio.h>
#include "xmi_xml.h"
#include "xmi_data_structs.h"
#include <stdlib.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <xraylib.h>


/*
 *
 * Widgets
 *
 */


//general
static GtkWidget *outputfileW;
static GtkWidget *n_photons_intervalW;
static GtkWidget *n_photons_lineW;
static GtkWidget *n_interactions_trajectoryW;
static GtkWidget *undoW;
static GtkWidget *redoW;
static GtkToolItem *newT;
static GtkToolItem *openT;
static GtkToolItem *saveasT;
static GtkToolItem *saveT;
static GtkToolItem *undoT;
static GtkToolItem *redoT;
struct layerWidget *layerW;


//composition
static GtkWidget *compositionW;
static GtkListStore *compositionL;



/*
 *
 * gulongs
 *
 */

static gulong n_photons_intervalG;
static gulong n_photons_lineG;
static gulong n_interactions_trajectoryG;


/*
 *
 *  xmi_composition structs
 *
 */

struct xmi_composition *compositionS;
struct xmi_composition *exc_compositionS;
struct xmi_composition *det_compositionS;
struct xmi_composition *crystal_compositionS;
struct xmi_layer *layer;





/*
 *
 * undo buffer
 *
 */

struct undo_single {
	struct xmi_input *xi;
	char message[512];
	int kind;
	GtkWidget *widget;
};

static struct undo_single *redo_buffer;
static struct undo_single *current;
static struct undo_single *last;


/*
 *
 * Gregex patterns
 *
 */

GRegex *pos_int;


/*
 *
 * Gdk colors
 *
 */

GdkColor white = {.red = (guint16) 65535, .green = (guint16) 65535, .blue = (guint16) 65535};
GdkColor red = {.red = (guint16) 65535, .green = (guint16) 1000, .blue = (guint16) 1000};




/*
 *
 * enums
 *
 */

enum {
	OUTPUTFILE,
	N_PHOTONS_INTERVAL,
	N_PHOTONS_LINE,
	N_INTERACTIONS_TRAJECTORY,
	COMPOSITION,
	COMPOSITION_ORDER,
	COMPOSITION_REFERENCE,
	COMPOSITION_DELETE,
	COMPOSITION_ADD,
	COMPOSITION_EDIT,
};


enum {
	N_ELEMENTS_COLUMN,
	ELEMENTS_COLUMN,
	DENSITY_COLUMN,
	THICKNESS_COLUMN,
	REFERENCE_COLUMN,
	NCOLUMNS_MATRIX
};

struct matrix_data {
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *topButton;
	GtkWidget *upButton;
	GtkWidget *downButton;
	GtkWidget *bottomButton;
};

static void update_undo_buffer(int kind, GtkWidget *widget);

static void layer_widget_hide_cb(GtkWidget *window, gpointer data) {
	struct layerWidget *lw = (struct layerWidget *) data;	
	struct xmi_composition *composition;
	GtkTreeIter iter;
	GtkListStore *store;
	char *elementString;
	int i,j;
	int updateKind;

#if DEBUG == 1
	fprintf(stdout,"layerWidget is hiding\n");
#endif

	if (lw->matrixKind == COMPOSITION) {
		composition = compositionS;
		store = compositionL;
		if (lw->AddOrEdit == LW_ADD) 
			updateKind = COMPOSITION_ADD;
		else if (lw->AddOrEdit == LW_EDIT) 
			updateKind = COMPOSITION_EDIT;
	}

	if (*(lw->my_layer) != NULL) {
		//OK button was clicked
		//in case of editing and changing nothing.. undo should not be triggered
		//requires xmi_compare_layer... too lazy for now
		if (lw->AddOrEdit == LW_ADD) {
			//adding layer
			composition->layers = (struct xmi_layer*) realloc(composition->layers, sizeof(struct xmi_layer)*(++composition->n_layers));
			xmi_copy_layer2(layer,composition->layers+composition->n_layers-1);

			gtk_list_store_append(store, &iter);
			i = composition->n_layers-1;
			elementString = (char *) malloc(sizeof(char)* (composition->layers[i].n_elements*5));
			elementString[0] = '\0';
			for (j = 0 ; j < composition->layers[i].n_elements ; j++) {
				strcat(elementString,AtomicNumberToSymbol(composition->layers[i].Z[j]));
				if (j != composition->layers[i].n_elements-1) {
					strcat(elementString,", ");
				}
			}
			if (lw->matrixKind == COMPOSITION) {
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					REFERENCE_COLUMN, FALSE,
					-1
					);
			}
			else {
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					-1
					);
			}
	
			free(elementString);
	

		}
		else if (lw->AddOrEdit == LW_EDIT) {
			//editing layer
			i = lw->layerNumber;
			free(composition->layers[i].Z);
			free(composition->layers[i].weight);
			xmi_copy_layer2(layer,composition->layers+lw->layerNumber);

			elementString = (char *) malloc(sizeof(char)* (composition->layers[i].n_elements*5));
			elementString[0] = '\0';
			for (j = 0 ; j < composition->layers[i].n_elements ; j++) {
				strcat(elementString,AtomicNumberToSymbol(composition->layers[i].Z[j]));
				if (j != composition->layers[i].n_elements-1) {
					strcat(elementString,", ");
				}
			}
			if (lw->matrixKind == COMPOSITION) {
				gtk_list_store_set(store, &(lw->iter),
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == composition->reference_layer) ? TRUE : FALSE ,
					-1
					);
			}
			else {
				gtk_list_store_set(store, &(lw->iter),
					N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,composition->layers[i].density,
					THICKNESS_COLUMN,composition->layers[i].thickness,
					-1
					);
			}
	
			free(elementString);
		}
		update_undo_buffer(updateKind, (GtkWidget *) store);	
	}  	
	else
		return;




}





static void layer_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	int n_elements;
	gchar *elements;
	double density,thickness;
	gboolean valid;
	int index,nindices;
	struct matrix_data *md = (struct matrix_data *) data;


	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
#if DEBUG == 1
		gtk_tree_model_get(model, &iter, N_ELEMENTS_COLUMN, &n_elements, ELEMENTS_COLUMN, &elements, DENSITY_COLUMN, &density, THICKNESS_COLUMN, &thickness, -1);
		fprintf(stdout,"n_elements: %i elements: %s density: %lf thickness: %lf\n",n_elements, elements, density, thickness);	
		g_free(elements);
#endif
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

		if (index == 0) {
			gtk_widget_set_sensitive(md->downButton,TRUE);
			gtk_widget_set_sensitive(md->bottomButton,TRUE);
			gtk_widget_set_sensitive(md->upButton,FALSE);
			gtk_widget_set_sensitive(md->topButton,FALSE);
		}
		else if(index == nindices-1) {
			gtk_widget_set_sensitive(md->downButton,FALSE);
			gtk_widget_set_sensitive(md->bottomButton,FALSE);
			gtk_widget_set_sensitive(md->upButton,TRUE);
			gtk_widget_set_sensitive(md->topButton,TRUE);
		}
		else {
			gtk_widget_set_sensitive(md->downButton,TRUE);
			gtk_widget_set_sensitive(md->bottomButton,TRUE);
			gtk_widget_set_sensitive(md->upButton,TRUE);
			gtk_widget_set_sensitive(md->topButton,TRUE);
		}
		gtk_widget_set_sensitive(md->deleteButton,TRUE);
		gtk_widget_set_sensitive(md->editButton,TRUE);
		gtk_widget_set_sensitive(md->addButton,TRUE);

	}
	else {
		gtk_widget_set_sensitive(md->downButton,FALSE);
		gtk_widget_set_sensitive(md->bottomButton,FALSE);
		gtk_widget_set_sensitive(md->upButton,FALSE);
		gtk_widget_set_sensitive(md->topButton,FALSE);
		gtk_widget_set_sensitive(md->deleteButton,FALSE);
		gtk_widget_set_sensitive(md->editButton,FALSE);
		gtk_widget_set_sensitive(md->addButton,TRUE);
	}


	return;
} 

struct matrix_reorder {
	int kind;
	GtkTreeSelection *select;
	GtkListStore *store;
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *topButton;
	GtkWidget *upButton;
	GtkWidget *downButton;
	GtkWidget *bottomButton;
};


void layer_reordering_cb(GtkTreeModel *tree_model, GtkTreePath  *path,  GtkTreeIter  *iter, gpointer new_order, gpointer user_data) {
	struct matrix_reorder *mr = (struct matrix_reorder *) user_data;	
	GtkTreeModel *model;
	GtkTreeIter iter2,temp_iter;
	int index, nindices;
	gboolean valid;

#if DEBUG == 1
	fprintf(stdout,"Layer reordering detected\n");
#endif

	//after pushing the move buttons... their sensivity needs to be checked
	if (gtk_tree_selection_get_selected(mr->select, &model, &iter2)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(mr->select, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}

		if (index == 0) {
			gtk_widget_set_sensitive(mr->downButton,TRUE);
			gtk_widget_set_sensitive(mr->bottomButton,TRUE);
			gtk_widget_set_sensitive(mr->upButton,FALSE);
			gtk_widget_set_sensitive(mr->topButton,FALSE);
		}
		else if(index == nindices-1) {
			gtk_widget_set_sensitive(mr->downButton,FALSE);
			gtk_widget_set_sensitive(mr->bottomButton,FALSE);
			gtk_widget_set_sensitive(mr->upButton,TRUE);
			gtk_widget_set_sensitive(mr->topButton,TRUE);
		}
		else {
			gtk_widget_set_sensitive(mr->downButton,TRUE);
			gtk_widget_set_sensitive(mr->bottomButton,TRUE);
			gtk_widget_set_sensitive(mr->upButton,TRUE);
			gtk_widget_set_sensitive(mr->topButton,TRUE);
		}
		gtk_widget_set_sensitive(mr->deleteButton,TRUE);
		gtk_widget_set_sensitive(mr->editButton,TRUE);
		
	}
}

enum {
	BUTTON_UP,
	BUTTON_DOWN,
	BUTTON_TOP,
	BUTTON_BOTTOM,
	BUTTON_DELETE,
	BUTTON_EDIT,
	BUTTON_ADD,
};

struct matrix_button {
	int buttonKind;
	int matrixKind;
	GtkTreeSelection *select;
	GtkListStore *store;
};


static void layers_button_clicked_cb(GtkWidget *widget, gpointer data) {
	struct matrix_button *mb= (struct matrix_button *) data;
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	gboolean valid;
	int index,nindices;
	int n_elements;
	gchar *elements;
	double density,thickness;
	gint *indices;
	int i;
	struct xmi_layer temp;
	struct xmi_composition *composition;

#if DEBUG == 1
	if (mb->buttonKind == BUTTON_TOP)
		fprintf(stdout,"Top button clicked\n" );
	else if (mb->buttonKind == BUTTON_DOWN)
		fprintf(stdout,"Down button clicked\n" );
	else if (mb->buttonKind == BUTTON_BOTTOM)
		fprintf(stdout,"Bottom button clicked\n" );
	else if (mb->buttonKind == BUTTON_UP)
		fprintf(stdout,"Up button clicked\n" );
	else if (mb->buttonKind == BUTTON_DELETE)
		fprintf(stdout,"Delete button clicked\n" );
#endif
	if (mb->matrixKind == COMPOSITION)
		composition = compositionS;

	if (mb->buttonKind == BUTTON_ADD) {
		//add line... testing only for now...
#if DEBUG == 1
		fprintf(stdout,"window pointer before showing it: %p\n",layerW->window);
#endif
		layer = NULL;
		layerW->AddOrEdit = LW_ADD;
		layerW->matrixKind = mb->matrixKind;
		gtk_widget_show_all(layerW->window);
#if DEBUG == 1
		fprintf(stdout,"After widget show command\n" );
#endif
		return;
	}

	//the olde switcharooooo
	if (gtk_tree_selection_get_selected(mb->select, &model, &iter)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		index = 0;
		nindices = 0;
		while(valid) {
			if (gtk_tree_selection_iter_is_selected(mb->select, &temp_iter)) {
#if DEBUG == 1
				fprintf(stdout,"Index: %i\n",nindices);
#endif
				index = nindices;
			}
			nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}
		indices = (gint *) malloc(sizeof(gint)*nindices);
		for (i = 0 ; i < nindices ; i++) 
			indices[i] = i;

		if (mb->buttonKind == BUTTON_TOP) {
			temp = composition->layers[index]; 
			indices[0] = index;
			for (i = 1 ; i < index+1 ; i++) {
				indices[i] = i-1;

			}
			for (i = index ; i > 0 ; i--) {
				composition->layers[i] = composition->layers[i-1];
			}
			composition->layers[0] = temp;

			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = 1;
				}
				else if (composition->reference_layer < index+1)
					composition->reference_layer++;
			}
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_BOTTOM) {
			temp = composition->layers[index]; 
			indices[nindices-1] = index;
			for (i = nindices-2 ; i >= index ; i--)
				indices[i] = i+1;
			for (i = index ; i < nindices-1 ; i++)
				composition->layers[i] = composition->layers[i+1];
			composition->layers[nindices-1] = temp;

			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = nindices;
				}
				else if (composition->reference_layer > index-1)
					composition->reference_layer--;
			}	
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_UP) {
			temp = composition->layers[index]; 
			composition->layers[index] = composition->layers[index-1];
			composition->layers[index-1] = temp;
			indices[index-1] = index;
			indices[index] = index-1;

			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = index;
				}
				else if (composition->reference_layer == index)
					composition->reference_layer++;
			}
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_DOWN) {
			temp = composition->layers[index]; 
			composition->layers[index] = composition->layers[index+1];
			composition->layers[index+1] = temp;
			indices[index+1] = index;
			indices[index] = index+1;
			if (mb->matrixKind == COMPOSITION) {
				if (composition->reference_layer == index+1) {
					//reference_layer is moved...
					composition->reference_layer = index+2;
				}
				else if (composition->reference_layer == index+2)
					composition->reference_layer--;
			}
			gtk_list_store_reorder(mb->store,indices);	
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_ORDER, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_DELETE) {
			//delete the selected line
			//watch out for reference layer... for now... leave it to the user
			//update composition
			xmi_free_layer(composition->layers+i);
			for (i = index ;  i < nindices ; i++)
				composition->layers[i] = composition->layers[i+1];
			composition->layers = (struct xmi_layer*) realloc(composition->layers, sizeof(struct xmi_layer)*(nindices-1));
			composition->n_layers--;
			gtk_list_store_remove(mb->store,&iter);
			if (mb->matrixKind == COMPOSITION)
				update_undo_buffer(COMPOSITION_DELETE, (GtkWidget*) mb->store);
		}
		else if (mb->buttonKind == BUTTON_EDIT) {
			//add line... testing only for now...
#if DEBUG == 1
			fprintf(stdout,"window pointer before showing it: %p\n",layerW->window);
#endif
			//should work with a copy instead of the real thing
		//	layer = composition->layers+index;	
			xmi_copy_layer(composition->layers+index,&layer);
			layerW->AddOrEdit = LW_EDIT;
			layerW->matrixKind = mb->matrixKind;
			layerW->layerNumber = index;
			layerW->iter = iter;
			gtk_widget_show_all(layerW->window);
		}


		else {
			fprintf(stdout,"Unknown button clicked\n");
			exit(1);
		}

	}


	return;
}

struct reference_toggle {
	GtkListStore *store;
	GtkTreeModel *model;
};

static void reference_layer_toggled_cb(GtkCellRendererToggle *renderer, gchar *path, gpointer data) {
	struct reference_toggle *rt = (struct reference_toggle *) data;
	GtkTreeIter iter,iter2;
	GValue reference = {0};
	int i;
	gboolean valid;
	GtkTreePath *tpath, *tpath2;
	int new_reference;


#if DEBUG == 1
	fprintf(stdout,"reference_layer_toggled: path: %s\n",path);
#endif

	tpath=gtk_tree_path_new_from_string(path);
	
	//convert path to iter
	if (gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(rt->store), &iter, path) == FALSE) {
		fprintf(stdout,"Error in reference_layer_toggled_cb\n");
		exit(1);
	}

	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(rt->store), &iter2);

	while (valid) {
		tpath2=gtk_tree_model_get_path(GTK_TREE_MODEL(rt->store), &iter2);
		g_value_init(&reference, G_TYPE_BOOLEAN);
		g_value_set_boolean(&reference, (gtk_tree_path_compare(tpath,tpath2) == 0)?TRUE:FALSE);
		gtk_list_store_set_value(rt->store, &iter2, REFERENCE_COLUMN, &reference);
		g_value_unset(&reference);
		gtk_tree_path_free(tpath2);	
		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(rt->store), &iter2);
	}
	gtk_tree_path_free(tpath);	

	//update compositionS if necessary
	new_reference = 1+ (int) g_ascii_strtoll(path, NULL, 10);
	if (new_reference != compositionS->reference_layer) {
		compositionS->reference_layer = new_reference;
		//update undo buffer
		update_undo_buffer(COMPOSITION_REFERENCE, (GtkWidget *) rt->store);
	}
}

void matrix_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data) {

#if DEBUG == 1
	fprintf(stdout,"row activation detected\n");
#endif

	return;
}




GtkWidget *initialize_matrix(struct xmi_composition *composition, int kind) {
	GtkListStore *store;
	GtkTreeIter iter;
	GtkWidget *tree;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	char *elementString;
	GtkTreeSelection *select;
	GtkWidget *mainbox, *mainbox2;
	GtkWidget *buttonbox;
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *deleteButton;
	GtkWidget *topButton;
	GtkWidget *upButton;
	GtkWidget *downButton;
	GtkWidget *bottomButton;
	GtkRequisition size;
	struct matrix_data *md;
	struct matrix_button *mb;
	struct reference_toggle *rt;
	struct matrix_reorder *mr;
	GtkWidget *scrolledWindow;

	int i,j;

	size.width = 350;
	size.height = 80;
	

	mainbox = gtk_hbox_new(FALSE, 5);
	mainbox2 = gtk_vbox_new(FALSE, 5);


	if (kind == COMPOSITION)
		store = gtk_list_store_new(NCOLUMNS_MATRIX, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_BOOLEAN);
	else
		store = gtk_list_store_new(NCOLUMNS_MATRIX-1, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE);



	for (i=0 ; i < composition->n_layers ; i++) {
		gtk_list_store_append(store, &iter);
		elementString = (char *) malloc(sizeof(char)* (composition->layers[i].n_elements*5));
		elementString[0] = '\0';
		for (j = 0 ; j < composition->layers[i].n_elements ; j++) {
			strcat(elementString,AtomicNumberToSymbol(composition->layers[i].Z[j]));
			if (j != composition->layers[i].n_elements-1) {
				strcat(elementString,", ");
			}
		}
		if (kind == COMPOSITION) {
			gtk_list_store_set(store, &iter,
				N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
				ELEMENTS_COLUMN,elementString,
				DENSITY_COLUMN,composition->layers[i].density,
				THICKNESS_COLUMN,composition->layers[i].thickness,
				REFERENCE_COLUMN,(i+1 == composition->reference_layer) ? TRUE : FALSE,
				-1
				);
		}
		else {
			gtk_list_store_set(store, &iter,
				N_ELEMENTS_COLUMN, composition->layers[i].n_elements,
				ELEMENTS_COLUMN,elementString,
				DENSITY_COLUMN,composition->layers[i].density,
				THICKNESS_COLUMN,composition->layers[i].thickness,
				-1
				);
		}
	
		free(elementString);
	}

	tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Number of elements", renderer,"text",N_ELEMENTS_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Elements", renderer,"text",ELEMENTS_COLUMN,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Density", renderer,"text",DENSITY_COLUMN,NULL);
	//gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	renderer = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Thickness", renderer,"text",THICKNESS_COLUMN,NULL);
	//gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_AUTOSIZE);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);

	g_signal_connect(G_OBJECT(tree), "row-activated", G_CALLBACK(matrix_row_activated_cb), NULL);
	
	if (kind == COMPOSITION) {
		rt = (struct reference_toggle *) malloc(sizeof(struct reference_toggle));
		rt->store = store;
		rt->model = (GtkTreeModel *) tree;
		renderer = gtk_cell_renderer_toggle_new();
		gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
		gtk_cell_renderer_toggle_set_radio(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		gtk_cell_renderer_toggle_set_activatable(GTK_CELL_RENDERER_TOGGLE(renderer), TRUE);
		g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(reference_layer_toggled_cb), rt);
		column = gtk_tree_view_column_new_with_attributes("Reference layer?", renderer,"active",REFERENCE_COLUMN,NULL);
		gtk_tree_view_column_set_resizable(column,TRUE);
		gtk_tree_view_column_set_alignment(column, 0.5);
		gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	}
	scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_size_request(scrolledWindow,&size);
	gtk_widget_set_size_request(scrolledWindow, 550,100);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolledWindow), tree);
	gtk_box_pack_start(GTK_BOX(mainbox),scrolledWindow, FALSE, FALSE,3 );
	
	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));

	buttonbox = gtk_vbox_new(FALSE, 5);
	topButton = gtk_button_new_from_stock(GTK_STOCK_GOTO_TOP);
	upButton = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	downButton = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	bottomButton = gtk_button_new_from_stock(GTK_STOCK_GOTO_BOTTOM);
	gtk_box_pack_start(GTK_BOX(buttonbox), topButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), upButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), downButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), bottomButton, FALSE, FALSE, 3);
	//set insensitive at start
	gtk_widget_set_sensitive(topButton, FALSE);
	gtk_widget_set_sensitive(upButton, FALSE);
	gtk_widget_set_sensitive(downButton, FALSE);
	gtk_widget_set_sensitive(bottomButton, FALSE);
	//signals -> let's create some nice memory leaks..
	//TOP button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_TOP;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(topButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//BOTTOM button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_BOTTOM;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(bottomButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//UP button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_UP;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(upButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);
	//DOWN button
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_DOWN;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(downButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);

	



	gtk_box_pack_start(GTK_BOX(mainbox), buttonbox, FALSE, FALSE, 2);


	buttonbox = gtk_vbox_new(FALSE, 5);

	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	deleteButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);

	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), editButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), deleteButton, TRUE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(mainbox), buttonbox, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainbox2), mainbox, FALSE, FALSE, 2);

	gtk_widget_set_sensitive(editButton, FALSE);
	gtk_widget_set_sensitive(deleteButton, FALSE);
	//DELETE
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_DELETE;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(deleteButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);

	//ADD
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_ADD;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(addButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);

	//EDIT
	mb = (struct matrix_button *) malloc(sizeof(struct matrix_button));
	mb->buttonKind=BUTTON_EDIT;
	mb->matrixKind=kind;
	mb->select=select;
	mb->store=store;
	g_signal_connect(G_OBJECT(editButton),"clicked", G_CALLBACK(layers_button_clicked_cb), (gpointer) mb);





	md = (struct matrix_data*) malloc(sizeof(struct matrix_data));
	md->topButton = topButton;
	md->upButton = upButton;
	md->downButton = downButton;
	md->bottomButton = bottomButton;
	md->editButton = editButton;
	md->addButton = addButton;
	md->deleteButton = deleteButton;

	gtk_tree_selection_set_mode(select,GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed",
			G_CALLBACK(layer_selection_changed_cb),
			(gpointer) md
		);

	mr = (struct matrix_reorder *) malloc(sizeof(struct matrix_reorder));
	mr->kind=COMPOSITION;
	mr->select=select;
	mr->store=store;
	mr->topButton = topButton;
	mr->upButton = upButton;
	mr->downButton = downButton;
	mr->bottomButton = bottomButton;
	mr->editButton = editButton;
	mr->addButton = addButton;
	mr->deleteButton = deleteButton;
	g_signal_connect(G_OBJECT(store), "rows-reordered", G_CALLBACK(layer_reordering_cb), (gpointer) mr);	
	if (kind == COMPOSITION) {
		compositionW = tree;
		xmi_copy_composition(composition,&compositionS);	
		compositionL = store;
	}
	return mainbox2;
}






static void undo_menu_click(GtkWidget *widget, gpointer data) {
	char buffer[512];
	GtkListStore *store;
	GtkTreeIter iter;
	char *elementString;
	int i,j;


	//restore previous state
	//current-- && copy changes
#if DEBUG == 1
	fprintf(stdout,"Undo button clicked\n");
#endif


	switch (current->kind) {
		case N_PHOTONS_INTERVAL:
			sprintf(buffer,"%li",(current-1)->xi->general->n_photons_interval);
#if DEBUG == 1
			fprintf(stdout,"n_photons_interval undo to %s\n",buffer);
#endif
			g_signal_handler_block(G_OBJECT(current->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			sprintf(buffer,"%li",(current-1)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT(current->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			sprintf(buffer,"%i",(current-1)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT(current->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY(current->widget),buffer);
			g_signal_handler_unblock(G_OBJECT(current->widget), n_interactions_trajectoryG);
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_REFERENCE:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
			//clear list and repopulate
			store = (GtkListStore *) (current)->widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer in undo: %p\n",store);
#endif
			gtk_list_store_clear(store);
			for (i=0 ; i < (current-1)->xi->composition->n_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current-1)->xi->composition->layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current-1)->xi->composition->layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current-1)->xi->composition->layers[i].Z[j]));
					if (j != (current-1)->xi->composition->layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current-1)->xi->composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current-1)->xi->composition->layers[i].density,
					THICKNESS_COLUMN,(current-1)->xi->composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == (current-1)->xi->composition->reference_layer) ? TRUE : FALSE,
					-1
					);
				free(elementString);
			}
#if DEBUG == 1

#endif
			xmi_free_composition(compositionS);
			xmi_copy_composition((current-1)->xi->composition, &compositionS);
			break;
	}
	if (current-1 != redo_buffer) {
		sprintf(buffer,"Undo: %s",(current-1)->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);		
		gtk_tool_item_set_tooltip_text(undoT,buffer);
	}
	else {
		sprintf(buffer,"Undo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);		
		gtk_tool_item_set_tooltip_text(undoT,buffer);
		gtk_widget_set_sensitive(undoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	}
	//update redo
	sprintf(buffer,"Redo: %s",current->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);		
	gtk_tool_item_set_tooltip_text(redoT,buffer);
	gtk_widget_set_sensitive(redoW,TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),TRUE);

	current--;

#if DEBUG == 1
		fprintf(stdout,"After undo click\n");
		fprintf(stdout,"current: %p\n",current);
		fprintf(stdout,"last: %p\n",last);
#endif

}

static void redo_menu_click(GtkWidget *widget, gpointer data) {
	char buffer[512];
	GtkListStore *store;
	GtkTreeIter iter;
	char *elementString;
	int i,j;

#if DEBUG == 1
	fprintf(stdout,"Redo button clicked: current kind: %i\n",current->kind);
#endif
	switch ((current+1)->kind) {
		case N_PHOTONS_INTERVAL:
			sprintf(buffer,"%li",(current+1)->xi->general->n_photons_interval);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_photons_intervalG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_photons_intervalG);
			break;
		case N_PHOTONS_LINE:
			sprintf(buffer,"%li",(current+1)->xi->general->n_photons_line);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_photons_lineG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_photons_lineG);
			break;
		case N_INTERACTIONS_TRAJECTORY:
			sprintf(buffer,"%i",(current+1)->xi->general->n_interactions_trajectory);
			g_signal_handler_block(G_OBJECT((current+1)->widget), n_interactions_trajectoryG);
			gtk_entry_set_text(GTK_ENTRY((current+1)->widget),buffer);
			g_signal_handler_unblock(G_OBJECT((current+1)->widget), n_interactions_trajectoryG);
			break;
		case COMPOSITION_ORDER:
		case COMPOSITION_REFERENCE:
		case COMPOSITION_DELETE:
		case COMPOSITION_ADD:
		case COMPOSITION_EDIT:
			//clear list and repopulate
			store = (GtkListStore *) (current+1)->widget;
			gtk_list_store_clear(store);
			for (i=0 ; i < (current+1)->xi->composition->n_layers ; i++) {
				gtk_list_store_append(store, &iter);
				elementString = (char *) malloc(sizeof(char)* ((current+1)->xi->composition->layers[i].n_elements*5));
				elementString[0] = '\0';
				for (j = 0 ; j < (current+1)->xi->composition->layers[i].n_elements ; j++) {
					strcat(elementString,AtomicNumberToSymbol((current+1)->xi->composition->layers[i].Z[j]));
					if (j != (current+1)->xi->composition->layers[i].n_elements-1) {
						strcat(elementString,", ");
					}
				}
				gtk_list_store_set(store, &iter,
					N_ELEMENTS_COLUMN, (current+1)->xi->composition->layers[i].n_elements,
					ELEMENTS_COLUMN,elementString,
					DENSITY_COLUMN,(current+1)->xi->composition->layers[i].density,
					THICKNESS_COLUMN,(current+1)->xi->composition->layers[i].thickness,
					REFERENCE_COLUMN,(i+1 == (current+1)->xi->composition->reference_layer) ? TRUE : FALSE,
					-1
					);
				free(elementString);
			}
			xmi_free_composition(compositionS);
			xmi_copy_composition((current+1)->xi->composition, &compositionS);
			break;
	

	}

	
	sprintf(buffer,"Undo: %s",(current+1)->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);		
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	gtk_widget_set_sensitive(undoW,TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);



	current++;
	if (current == last) {
		sprintf(buffer,"Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);		
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}
	else {
		sprintf(buffer,"Redo: %s",current->message);
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);		
		gtk_tool_item_set_tooltip_text(redoT,buffer);
	}

}

static void file_menu_click(GtkWidget *widget, gpointer data) {

#if DEBUG == 1
	g_print("%s\n",(char *) data);
#endif

	if (strcmp((char *)data, "quit") == 0)
		gtk_main_quit();


}




static void pos_int_changed(GtkWidget *widget, gpointer data) {
	int kind = GPOINTER_TO_INT(data);
	char buffer[512];
	char *textPtr;


#if DEBUG == 1
	g_print("Widget: %i\n",kind);
#endif

	switch (kind) {
		case N_PHOTONS_INTERVAL:
		case N_PHOTONS_LINE:
		case N_INTERACTIONS_TRAJECTORY:
			textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(widget));
			if (g_regex_match(pos_int,textPtr,0,NULL) == TRUE ){
				//ok
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&white);
				update_undo_buffer(kind, widget);
			}
			else {
				//bad value
				gtk_widget_modify_base(widget,GTK_STATE_NORMAL,&red);
			}
			break;
		default:
			g_print("Unknown kind detected. Aborting\n");
			exit(1);
	}


}




static gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
	g_print("delete event occured\n");
	return FALSE;

}


static void update_undo_buffer(int kind, GtkWidget *widget) {
	char buffer[512];
	ptrdiff_t last_diff, current_diff;
	struct undo_single *tempPtr;

	//two cases... 
	//current == last (NO REDO(s) used)
	//last > current
	
	last_diff = last - redo_buffer;
	current_diff = current - redo_buffer;
	
	if (last > current) {
#if DEBUG == 1
		fprintf(stdout,"last > current\n");
#endif
		for (tempPtr = current+1 ; tempPtr == last ; tempPtr++)
			xmi_free_input(tempPtr->xi);
		//disable redo
		sprintf(buffer,"Redo");
		gtk_menu_item_set_label(GTK_MENU_ITEM(redoW),buffer);
		gtk_widget_set_sensitive(redoW,FALSE);
		gtk_tool_item_set_tooltip_text(redoT,buffer);
		gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	}

	redo_buffer = (struct undo_single *) realloc(redo_buffer,(current-redo_buffer+2)*sizeof(struct undo_single));
#if DEBUG == 1
	fprintf(stdout,"After redo_buffer realloc\n");
#endif
	last = redo_buffer + last_diff;
	current = redo_buffer + current_diff;
	last = current+1;
	xmi_copy_input(current->xi, &(last->xi));
	switch (kind) {
		case N_PHOTONS_INTERVAL:
			strcpy(last->message,"change of number of photons per interval");
			last->xi->general->n_photons_interval = strtol((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL,10);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_PHOTONS_LINE:
			strcpy(last->message,"change of number of photons per line");
			last->xi->general->n_photons_line = strtol((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL,10);	
			last->kind = kind;
			last->widget = widget;
			break;
		case N_INTERACTIONS_TRAJECTORY:
			strcpy(last->message,"change of number of interactions per trajectory");
			last->xi->general->n_interactions_trajectory = strtol((char *) gtk_entry_get_text(GTK_ENTRY(widget)),NULL,10);	
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_ORDER:
			strcpy(last->message,"change of composition ordering");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
#if DEBUG == 1
			fprintf(stdout,"store pointer during update: %p\n",last->widget);
#endif
			break;
		case COMPOSITION_REFERENCE:
			strcpy(last->message, "change of reference layer");
			last->xi->composition->reference_layer = compositionS->reference_layer;
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_DELETE:
			strcpy(last->message,"removal of layer");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_ADD:
			strcpy(last->message,"addition of layer");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
			break;
		case COMPOSITION_EDIT:
			strcpy(last->message,"editing of layer");
			xmi_free_composition(last->xi->composition);
			xmi_copy_composition(compositionS, &(last->xi->composition));
			last->kind = kind;
			last->widget = widget;
			break;
			
	} 
	current = last;
	sprintf(buffer,"Undo: %s",last->message);
	gtk_menu_item_set_label(GTK_MENU_ITEM(undoW),buffer);
	gtk_widget_set_sensitive(undoW,TRUE);
	gtk_tool_item_set_tooltip_text(undoT,buffer);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),TRUE);




	
}




int main (int argc, char *argv[]) {

	GtkWidget *window;
	GtkWidget *Main_vbox;

	GtkWidget *menubar;
	GtkWidget *filemenu;
	GtkWidget *file;
	GtkWidget *new;
	GtkWidget *open;
	GtkWidget *save;
	GtkWidget *save_as;
	GtkWidget *quit;
	GtkWidget *editmenu;
	GtkWidget *edit;
	GtkWidget *notebook;
	GtkWidget *frame;
	GtkWidget *superframe;
	GtkWidget *label;
	GtkWidget *vbox_notebook;
	GtkWidget *hbox_text_label;
	GtkWidget *text;
	GtkWidget *toolbar;
	GtkWidget *scrolled_window;
	GtkWidget *tempW;
	char buffer[512];

	//should be changed later using a cpp macro that will point to the data folder of the package
	//windows will be quite complicated here I'm sure...
	//
	char default_file1[] = "/Volumes/Home/schoon/github/xmimsim/example/example1.xml";
	char default_file2[] = "/Users/schoon/github/xmimsim/example/example1.xml";


/*
 *
 *
 *
 *  Initialize
 *
 *
 *
 */

	//let's use the default C locale
	gtk_disable_setlocale();
	//g_type_init
	g_type_init();


	//initialize undo system
	redo_buffer = (struct undo_single *) malloc(sizeof(struct undo_single ));		
	current = redo_buffer;
	last = current;



#if DEBUG == 1
	fprintf(stdout,"Initial undo buffer pointers\n");
	fprintf(stdout,"redo_buffer: %p\n",redo_buffer);
	fprintf(stdout,"current: %p\n",current);
	fprintf(stdout,"last: %p\n",last);
#endif


	//start by reading in the default file -> command-line args later to be arranged
	if (xmi_read_input_xml(default_file1, &(current->xi)) == 0 && xmi_read_input_xml(default_file2, &(current->xi)) == 0) {
		fprintf(stderr,"Could not read in default xml file\n");
		return 1;
	}


	//initialize regex patterns
	/*
	if (regcomp(&pos_int,"^[1-9][0-9]*$" , REG_EXTENDED | REG_NOSUB) != 0) {
		fprintf(stderr,"Error compiling regex pattern pos_int\n");
		return 1;
	}
	*/
	pos_int = g_regex_new("^[1-9][0-9]*$", G_REGEX_EXTENDED,0, NULL);





	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window),"XMI MSIM");
	gtk_window_set_default_size(GTK_WINDOW(window),800,800);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);

	Main_vbox = gtk_vbox_new(FALSE,0);
	gtk_container_add(GTK_CONTAINER(window),Main_vbox);

	menubar = gtk_menu_bar_new();
	filemenu = gtk_menu_new();

	file = gtk_menu_item_new_with_label("File");
	//new = gtk_menu_item_new_with_label("New");
	new = gtk_image_menu_item_new_from_stock(GTK_STOCK_NEW,NULL);
	open = gtk_image_menu_item_new_from_stock(GTK_STOCK_OPEN,NULL);
	save = gtk_image_menu_item_new_from_stock(GTK_STOCK_SAVE,NULL);
	save_as = gtk_image_menu_item_new_from_stock(GTK_STOCK_SAVE_AS,NULL);
	quit = gtk_image_menu_item_new_from_stock(GTK_STOCK_QUIT,NULL);

	gtk_menu_item_set_submenu(GTK_MENU_ITEM(file),filemenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),new);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),open);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),save);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),save_as);
	gtk_menu_shell_append(GTK_MENU_SHELL(filemenu),quit);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),file);
	g_signal_connect(G_OBJECT(quit),"activate",G_CALLBACK(file_menu_click),(gpointer) "quit");
	g_signal_connect(G_OBJECT(open),"activate",G_CALLBACK(file_menu_click),(gpointer) "open");
	g_signal_connect(G_OBJECT(save),"activate",G_CALLBACK(file_menu_click),(gpointer) "save");
	g_signal_connect(G_OBJECT(save_as),"activate",G_CALLBACK(file_menu_click),(gpointer) "save as");
	g_signal_connect(G_OBJECT(new),"activate",G_CALLBACK(file_menu_click),(gpointer) "new");
	editmenu = gtk_menu_new();
	edit = gtk_menu_item_new_with_label("Edit");
	undoW = gtk_image_menu_item_new_from_stock(GTK_STOCK_UNDO,NULL);
	g_signal_connect(G_OBJECT(undoW),"activate",G_CALLBACK(undo_menu_click),NULL);
	redoW = gtk_image_menu_item_new_from_stock(GTK_STOCK_REDO,NULL);
	g_signal_connect(G_OBJECT(redoW),"activate",G_CALLBACK(redo_menu_click),NULL);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(edit),editmenu);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),undoW);
	gtk_menu_shell_append(GTK_MENU_SHELL(editmenu),redoW);
	gtk_menu_shell_append(GTK_MENU_SHELL(menubar),edit);
	//both should be greyed out in the beginning
	gtk_widget_set_sensitive(undoW,FALSE);
	gtk_widget_set_sensitive(redoW,FALSE);

	gtk_box_pack_start(GTK_BOX(Main_vbox), menubar, FALSE, FALSE, 3);


	//toolbar
	toolbar = gtk_toolbar_new();
	newT = gtk_tool_button_new_from_stock(GTK_STOCK_NEW);
	openT = gtk_tool_button_new_from_stock(GTK_STOCK_OPEN);
	saveasT = gtk_tool_button_new_from_stock(GTK_STOCK_SAVE_AS);
	saveT = gtk_tool_button_new_from_stock(GTK_STOCK_SAVE);
	undoT = gtk_tool_button_new_from_stock(GTK_STOCK_UNDO);
	redoT = gtk_tool_button_new_from_stock(GTK_STOCK_REDO);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), newT,(gint) 0);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), openT,(gint) 1);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), saveasT,(gint) 2);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), saveT,(gint) 3);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), undoT,(gint) 4);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), redoT,(gint) 5);
	gtk_widget_set_sensitive(GTK_WIDGET(undoT),FALSE);
	g_signal_connect(G_OBJECT(undoT),"clicked",G_CALLBACK(undo_menu_click),NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(redoT),FALSE);
	g_signal_connect(G_OBJECT(redoT),"clicked",G_CALLBACK(redo_menu_click),NULL);

	gtk_box_pack_start(GTK_BOX(Main_vbox), toolbar, FALSE, FALSE, 3);


	g_signal_connect_swapped(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit),NULL);
	g_signal_connect(window,"delete-event",G_CALLBACK(delete_event),NULL);

	//notebook
	notebook = gtk_notebook_new();
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_widget_show(notebook);

	frame = gtk_frame_new("General");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">General</span>");

	//Append general
	superframe = gtk_vbox_new(FALSE,5);
	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	//gtk_container_add(GTK_CONTAINER(notebook),vbox_notebook);
	//Outputfile
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Outputfile");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	outputfileW = text;
	gtk_entry_set_text(GTK_ENTRY(text),current->xi->general->outputfile);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_photons_interval
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per interval");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_photons_intervalW = text;
	sprintf(buffer,"%li",current->xi->general->n_photons_interval);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	n_photons_intervalG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), GINT_TO_POINTER(N_PHOTONS_INTERVAL)  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_photons_line
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per discrete line");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_photons_lineW = text;
	sprintf(buffer,"%li",current->xi->general->n_photons_line);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	n_photons_lineG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), GINT_TO_POINTER(N_PHOTONS_LINE)  );
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_interactions_trajectory
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of interactions per trajectory");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	n_interactions_trajectoryW = text;
	sprintf(buffer,"%i",current->xi->general->n_interactions_trajectory);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	n_interactions_trajectoryG = g_signal_connect(G_OBJECT(text),"changed",G_CALLBACK(pos_int_changed), GINT_TO_POINTER(N_INTERACTIONS_TRAJECTORY));
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);

	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);


	label = gtk_label_new("Input parameters");
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_window, label);
	gtk_box_pack_start(GTK_BOX(Main_vbox), notebook, TRUE, TRUE, 3);

	//composition
	tempW = initialize_matrix(current->xi->composition, COMPOSITION); 

	//initialize layer widget
	layerW = initialize_layer_widget(&layer);
	g_signal_connect(G_OBJECT(layerW->window),"hide",G_CALLBACK(layer_widget_hide_cb), (gpointer) layerW);


	frame = gtk_frame_new("Composition");

	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Composition</span>");
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_container_add(GTK_CONTAINER(frame),tempW);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,5);


	gtk_widget_show_all(window);



	gtk_main();




	return 0;
}
