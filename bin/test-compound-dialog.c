#include "xmimsim-gui-compound-dialog.h"
#include <stdio.h>


int main(int argc, char *argv[]) {

	gtk_init(&argc, &argv);

	fprintf(stderr, "Before xmi_msim_gui_compound_dialog_new\n");
	GtkWidget *dialog = xmi_msim_gui_compound_dialog_new(NULL, XMI_MSIM_GUI_COMPOUND_DIALOG_ADD);

	fprintf(stderr, "Before gtk_dialog_run\n");
	int rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (rv == GTK_RESPONSE_ACCEPT) {
		//something was changed
		gchar *compound = xmi_msim_gui_compound_dialog_get_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog));
		gdouble weight = xmi_msim_gui_compound_dialog_get_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(dialog));
		fprintf(stdout, "compound: %s\n", compound);
		fprintf(stdout, "weight: %f\n", weight);
	}
	gtk_widget_destroy(dialog);
	return 0;
}
