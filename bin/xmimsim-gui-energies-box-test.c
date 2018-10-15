/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-energies-box.h"
#include "xmimsim-gui-colors.h"
#include "xmi_xml.h"

static gboolean delete_event( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   data ) {
	return FALSE;
}

static void destroy( GtkWidget *widget,
                     gpointer   data ) {
    gtk_main_quit ();
}

static void changed(XmiMsimGuiEnergiesBox *eb, gchar *change, gpointer data) {
	xmi_excitation *excitation = xmi_msim_gui_energies_box_get_excitation(eb);

	fprintf(stdout, "change: %s\n", change);
	fprintf(stdout, "discrete\n");
	int i;
	for (i = 0 ; i < excitation->n_discrete ; i++) {
		fprintf(stdout, "Energy %i: %g\n",i,excitation->discrete[i].energy);
		fprintf(stdout, "Horizontal intensity: %g\n",excitation->discrete[i].horizontal_intensity);
		fprintf(stdout, "Vertical intensity: %g\n",excitation->discrete[i].vertical_intensity);
		fprintf(stdout, "sigma_x: %g\n",excitation->discrete[i].sigma_x);
		fprintf(stdout, "sigma_xp: %g\n",excitation->discrete[i].sigma_xp);
		fprintf(stdout, "sigma_y: %g\n",excitation->discrete[i].sigma_y);
		fprintf(stdout, "sigma_yp: %g\n",excitation->discrete[i].sigma_yp);
		fprintf(stdout, "distribution_type: %i\n",excitation->discrete[i].distribution_type);
		if (excitation->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC)
			fprintf(stdout, "scale_parameter: %g\n",excitation->discrete[i].scale_parameter);
	}

	fprintf(stdout, "continuous\n");
	for (i = 0 ; i < excitation->n_continuous ; i++) {
		fprintf(stdout, "Energy %i: %g\n",i,excitation->continuous[i].energy);
		fprintf(stdout, "Horizontal intensity: %g\n",excitation->continuous[i].horizontal_intensity);
		fprintf(stdout, "Vertical intensity: %g\n",excitation->continuous[i].vertical_intensity);
		fprintf(stdout, "sigma_x: %g\n",excitation->continuous[i].sigma_x);
		fprintf(stdout, "sigma_xp: %g\n",excitation->continuous[i].sigma_xp);
		fprintf(stdout, "sigma_y: %g\n",excitation->continuous[i].sigma_y);
		fprintf(stdout, "sigma_yp: %g\n",excitation->continuous[i].sigma_yp);
	}
	fprintf(stdout, "\n");
	xmi_free_excitation(excitation);
}

int main(int argc, char *argv[]) {


	gtk_init(&argc, &argv);
	xmi_msim_gui_utils_init_colors();
	xmi_xmlLoadCatalog();

	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(window), 1200, 1200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	g_signal_connect (window, "delete-event",
		      G_CALLBACK (delete_event), NULL);
	g_signal_connect (window, "destroy",
		      G_CALLBACK (destroy), NULL);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	GtkWidget *eb = xmi_msim_gui_energies_box_new();
	gtk_container_add(GTK_CONTAINER(window), eb);
	g_signal_connect(G_OBJECT(eb), "changed", G_CALLBACK(changed), NULL);

	/*GError *error = NULL;
	if (xmi_msim_gui_xmso_results_scrolled_window_load_from_file(XMI_MSIM_GUI_XMSO_RESULTS_SCROLLED_WINDOW(xmso_sw), argv[1], &error) == FALSE) {
		fprintf(stderr, "FATAL ERROR: %s\n", error->message);
		return 1;
	}*/

	gtk_widget_show_all(window);

	gtk_main();
	return 0;
}
