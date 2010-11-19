#include <gtk/gtk.h>
#include <string.h>
#include <stdio.h>
#include "xmi_xml.h"
#include "xmi_data_structs.h"
#include <stdlib.h>
#include <glib.h>


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


//composition
static GtkWidget *compositionW;



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
};


enum {
	N_ELEMENTS_COLUMN,
	ELEMENTS_COLUMN,
	DENSITY_COLUMN,
	THICKNESS_COLUMN,
	NCOLUMNS_MATRIX
};


GtkWidget *initialize_matrix(struct xmi_composition *composition) {
	GtkTreeStore *store;
	GtkTreeIter iter;

	int i;


	store = gtk_list_store_new(NCOLUMNS_MATRIX, G_TYPE_INT, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE);
/*
	for (i=0 ; i < composition

	gtk_list_store_append(store, &iter,
				N_ELEMENTS_COLUMN, "Number of elements",
				ELEMENTS_COLUMN,"Elements",
				DENSITY_COLUMN,"Density",
				THICKNESS_COLUMN,"Thickness",
				-1
				);

*/
}






static void undo_menu_click(GtkWidget *widget, gpointer data) {
	char buffer[512];


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



static void update_undo_buffer(int kind, GtkWidget *widget);

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
	
	//if (current == last || current == redo_buffer) {
		//realloc: (current-redo_buffer+2)
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
	gtk_window_set_default_size(GTK_WINDOW(window),700,150);
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



	gtk_widget_show_all(window);



	gtk_main();




	return 0;
}
