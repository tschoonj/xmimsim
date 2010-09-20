#include <gtk/gtk.h>
#include <string.h>

static void file_menu_click(GtkWidget *widget, gpointer data) {

	g_print("%s\n",(char *) data);

	if (strcmp((char *)data, "quit") == 0)
		gtk_main_quit();


}

static gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
	g_print("delete event occured\n");
	return FALSE;

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

	gtk_widget_show_all(window);



	gtk_main();




	return 0;
}
