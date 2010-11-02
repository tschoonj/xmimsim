#include <gtk/gtk.h>
#include <string.h>
#include <stdio.h>
#include "xmi_xml.h"

static void file_menu_click(GtkWidget *widget, gpointer data) {

	g_print("%s\n",(char *) data);

	if (strcmp((char *)data, "quit") == 0)
		gtk_main_quit();


}

static gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
	g_print("delete event occured\n");
	return FALSE;

}

static struct xmi_input *current;



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
	GtkWidget *notebook;
	GtkWidget *frame;
	GtkWidget *label;
	GtkWidget *vbox_notebook;
	GtkWidget *hbox_text_label;
	GtkWidget *text;
	char buffer[512];

	//should be changed later using a cpp macro that will point to the data folder of the package
	//windows will be quite complicated here I'm sure...
	//
	char default_file1[] = "/Volumes/Home/schoon/github/xmimsim/example/example1.xml";
	char default_file2[] = "/Users/schoon/github/xmimsim/example/example1.xml";

	//start by reading in the default file -> command-line args later to be arranged
	if (xmi_read_input_xml(default_file1, &current) == 0 && xmi_read_input_xml(default_file2, &current) == 0) {
		fprintf(stderr,"Could not read in default xml file\n");
		return 1;
	}



	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window),"XMI MSIM");
	gtk_window_set_default_size(GTK_WINDOW(window),230,150);
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
	gtk_box_pack_start(GTK_BOX(Main_vbox), menubar, FALSE, FALSE, 3);


	g_signal_connect_swapped(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit),NULL);
	g_signal_connect(window,"delete-event",G_CALLBACK(delete_event),NULL);

	//notebook
	notebook = gtk_notebook_new();
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_widget_show(notebook);

	//Append general
	vbox_notebook = gtk_vbox_new(FALSE,5);
	//gtk_container_add(GTK_CONTAINER(notebook),vbox_notebook);
	//Outputfile
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Outputfile");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(text),current->general->outputfile);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_photons_interval
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per interval");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	sprintf(buffer,"%li",current->general->n_photons_interval);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_photons_line
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of photons per discrete line");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	sprintf(buffer,"%li",current->general->n_photons_line);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	//n_interactions_trajectory
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Number of interactions per trajectory");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	text = gtk_entry_new();
	sprintf(buffer,"%i",current->general->n_interactions_trajectory);
	gtk_entry_set_text(GTK_ENTRY(text),buffer);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),text,FALSE,FALSE,0);
	



	label = gtk_label_new("General settings");
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox_notebook, label);
	gtk_box_pack_start(GTK_BOX(Main_vbox), notebook, TRUE, TRUE, 3);



	gtk_widget_show_all(window);



	gtk_main();




	return 0;
}
