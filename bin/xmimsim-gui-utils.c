#include "xmimsim-gui-utils.h"
#include <math.h>

double xmi_msim_gui_utils_get_solid_angle_from_slits(struct xmi_geometry *geometry) {
	//calculate solid angle based on slits
	double solid_angle = 4.0 * atan(geometry->slit_size_x * geometry->slit_size_y/(2.0*geometry->d_source_slit*sqrt(4.0 * geometry->d_source_slit * geometry->d_source_slit + geometry->slit_size_x * geometry->slit_size_x + geometry->slit_size_y + geometry->slit_size_y)));

	return solid_angle;
}

void xmi_msim_gui_utils_update_button_text(GtkWidget *button, const gchar *text) {
	//this function is a hack and may not work on Gtk3
	GList *children = gtk_container_get_children(GTK_CONTAINER(button));
	GtkWidget *temp = (GtkWidget *) g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	temp = (GtkWidget *) g_list_nth_data(children, 0);
	g_list_free(children);
	children = gtk_container_get_children(GTK_CONTAINER(temp));
	gtk_label_set_text(GTK_LABEL((GtkWidget *) g_list_nth_data(children,1)), text);
	g_list_free(children);
	return;
}

XmiColor white_plot;
XmiColor blue_plot;
XmiColor red_plot;
XmiColor green_plot;
XmiColor black_plot;
XmiColor purple_plot;
XmiColor yellow_plot;
XmiColor pink_plot;

#if GTK_MAJOR_VERSION == 3
	#define COLOR_INIT(color) color ## _plot = new Gdk::RGBA(#color);
#else
	#define COLOR_INIT(color) gdk_color_parse(#color, &color ## _plot);\
			gdk_colormap_alloc_color(gdk_colormap_get_system(), &color ## _plot,FALSE,TRUE);
#endif

void xmi_msim_gui_utils_init_colors() {
	/*initialize colors*/
	COLOR_INIT(white);
	COLOR_INIT(blue);
	COLOR_INIT(red);
	COLOR_INIT(green);
	COLOR_INIT(black);
	COLOR_INIT(purple);
	COLOR_INIT(yellow);
	COLOR_INIT(pink);
}
