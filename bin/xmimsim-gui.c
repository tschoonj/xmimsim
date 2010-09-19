#include <gtk/gtk.h>



int main (int argc, char *argv[]) {

	GtkWidget *window;

	gtk_init(&argc, &argv);

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window),"XMI MSIM");
	gtk_window_set_default_size(GTK_WINDOW(window),230,150);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_widget_show(window);


	g_signal_connect_swapped(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit),NULL);

	gtk_main();




	return 0;
}
