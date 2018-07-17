/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

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
#include <xraylib.h>
#include "xmi_aux.h"
#include "xmimsim-gui-xmsa-viewer-window.h"
#include "xmimsim-gui-export-canvas-dialog.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-colors.h"
#include <gtkmm-plplot/plot2d.h>
#include <gtkmm-plplot/canvas.h>
#include "xmi_gobject.h"

class Plot2DBatch: public Gtk::PLplot::Plot2D {
	public:
	Plot2DBatch(
		const Glib::ustring &axis_title_x,
		const Glib::ustring &axis_title_y) :
		Gtk::PLplot::Plot2D(axis_title_x, axis_title_y) {}
	virtual void on_double_press(double x, double y) override {
		// do nothing -> we will connect a signal handler
	}

};

struct fluor_data {
	int atomic_number;
	GPtrArray *line_types;
};

static int compare_fluor_data(const void *f1, const void *f2) {
	struct fluor_data **ff1 = (struct fluor_data **) f1;
	struct fluor_data **ff2 = (struct fluor_data **) f2;

	return (*ff1)->atomic_number - (*ff2)->atomic_number;
}

extern "C" struct _XmiMsimGuiXmsaViewerWindow {
	GtkApplicationWindow parent_instance;
	GtkWidget *parent_windowW;
	GtkWidget *roi_radioW;
	GtkWidget *roi_channel_radioW;
	GtkWidget *roi_energy_radioW;
	GtkWidget *roi_start_channel_spinnerW;
	GtkWidget *roi_start_channel_labelW;
	GtkWidget *roi_end_channel_spinnerW;
	GtkWidget *roi_end_channel_labelW;
	GtkWidget *roi_start_energy_spinnerW;
	GtkWidget *roi_start_energy_labelW;
	GtkWidget *roi_end_energy_spinnerW;
	GtkWidget *roi_end_energy_labelW;
	GtkWidget *roi_conv_radioW;
	GtkWidget *roi_unconv_radioW;
	GtkWidget *roi_interactions_labelW;
	GtkWidget *roi_interactions_comboW;
	GtkWidget *roi_cumulative_radioW;
	GtkWidget *roi_individual_radioW;
	GtkWidget *roi_linearW;
	GtkWidget *roi_log10W;
	GtkWidget *xrf_radioW;
	GtkWidget *xrf_element_labelW;
	GtkWidget *xrf_element_comboW;
	GtkWidget *xrf_line_labelW;
	GtkWidget *xrf_line_comboW;
	GtkWidget *xrf_interactions_labelW;
	GtkWidget *xrf_interactions_comboW;
	GtkWidget *xrf_cumulative_radioW;
	GtkWidget *xrf_individual_radioW;
	GtkWidget *xrf_linearW;
	GtkWidget *xrf_log10W;
	GtkWidget *xaxis_titleW;
	GtkWidget *yaxis_titleW;
	GtkWidget *okButton;
	GtkWidget *imageButton;
	GtkWidget *exportButton;
	Gtk::PLplot::Canvas *canvas;
	Gtk::PLplot::Plot *plot_window;
	GPtrArray *fd;
	gulong roi_start_channel_spinnerG;
	gulong roi_end_channel_spinnerG;
	gulong roi_start_energy_spinnerG;
	gulong roi_end_energy_spinnerG;
	double *x;
	double *y;
	double *z;
	struct xmi_archive *archive;
};

extern "C" struct _XmiMsimGuiXmsaViewerWindowClass {
	GtkApplicationWindowClass parent_class;
};

G_DEFINE_TYPE(XmiMsimGuiXmsaViewerWindow, xmi_msim_gui_xmsa_viewer_window, GTK_TYPE_APPLICATION_WINDOW)

static void xmi_msim_gui_xmsa_viewer_window_set_property (GObject          *object,
                                                   guint             prop_id,
                                                   const GValue     *value,
                                                   GParamSpec       *pspec);

static void xmi_msim_gui_xmsa_viewer_window_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_xmsa_viewer_window_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_xmsa_viewer_window_finalize(GObject *gobject) {
	XmiMsimGuiXmsaViewerWindow *window = XMI_MSIM_GUI_XMSA_VIEWER_WINDOW(gobject);

	g_free(window->x);
	g_free(window->y);
	g_free(window->z);
	xmi_free_archive(window->archive);
	g_ptr_array_free(window->fd, TRUE);

	G_OBJECT_CLASS(xmi_msim_gui_xmsa_viewer_window_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_xmsa_viewer_window_constructed(GObject *obj);

static void xmi_msim_gui_xmsa_viewer_window_class_init(XmiMsimGuiXmsaViewerWindowClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_xmsa_viewer_window_dispose;
	object_class->finalize = xmi_msim_gui_xmsa_viewer_window_finalize;
	object_class->set_property = xmi_msim_gui_xmsa_viewer_window_set_property;
	object_class->constructed = xmi_msim_gui_xmsa_viewer_window_constructed;

	g_object_class_install_property(object_class,
		1,
		g_param_spec_pointer(
			"archive",
			"archive",
			"archive",
    			(GParamFlags) (G_PARAM_WRITABLE | G_PARAM_CONSTRUCT)
		)
	);
}

static GPtrArray* get_fluor_data(struct xmi_archive *archive) {

	gboolean found;
	unsigned int loc = 0;
	unsigned int i,i2,j,k,l;
	GPtrArray *rv = g_ptr_array_new_with_free_func(
		[](gpointer data) {
			struct fluor_data *fd = (struct fluor_data *) data;
			g_ptr_array_free(fd->line_types, TRUE);
			g_free(fd);
			return;
		});

	for (i = 0 ; i <= archive->nsteps1 ; i++) {
	for (i2 = 0 ; i2 <= archive->nsteps2 ; i2++) {
		for (j = 0 ; j < archive->output[i][i2]->nvar_red_history ; j++) {
			found = FALSE;
			for (k = 0 ; k < rv->len ; k++) {
				if (archive->output[i][i2]->var_red_history[j].atomic_number == ((struct fluor_data *) g_ptr_array_index(rv, k))->atomic_number) {
					found = TRUE;
					loc = k;
					break;
				}
			}
			if (!found) {
				//add new element to array
				struct fluor_data *fd = (struct fluor_data *) g_malloc(sizeof(struct fluor_data));
				fd->atomic_number = archive->output[i][i2]->var_red_history[j].atomic_number;
				unsigned int n_lines = archive->output[i][i2]->var_red_history[j].n_lines;
				fd->line_types = g_ptr_array_new_full(n_lines, g_free);
				for (k = 0 ; k < n_lines ; k++) {
					g_ptr_array_add(fd->line_types, g_strdup(archive->output[i][i2]->var_red_history[j].lines[k].line_type));
				}
				g_ptr_array_add(rv, fd);
			}
			else {
				//element found -> check for new lines
				for (k = 0 ; k < archive->output[i][i2]->var_red_history[j].n_lines ; k++) {
					found = FALSE;
					for (l = 0 ; l < ((struct fluor_data *) g_ptr_array_index(rv, loc))->line_types->len ; l++) {
						if (strcmp(archive->output[i][i2]->var_red_history[j].lines[k].line_type, (gchar *) g_ptr_array_index(((struct fluor_data *) g_ptr_array_index(rv, loc))->line_types, l)) != 0) {
							found = TRUE;
							break;
						}
					}
					if (!found) {
						//extend array
						g_ptr_array_add(((struct fluor_data *) g_ptr_array_index(rv, loc))->line_types, g_strdup(archive->output[i][i2]->var_red_history[j].lines[k].line_type));
					}
				}
			}
		}
		for (j = 0 ; j < archive->output[i][i2]->nbrute_force_history ; j++) {
			found = FALSE;
			for (k = 0 ; k < rv->len ; k++) {
				if (archive->output[i][i2]->brute_force_history[j].atomic_number == ((struct fluor_data *) g_ptr_array_index(rv, k))->atomic_number) {
					found = TRUE;
					loc = k;
					break;
				}
			}
			if (!found) {
				//add new element to array
				struct fluor_data *fd = (struct fluor_data *) g_malloc(sizeof(struct fluor_data));
				fd->atomic_number = archive->output[i][i2]->brute_force_history[j].atomic_number;
				unsigned int n_lines = archive->output[i][i2]->brute_force_history[j].n_lines;
				fd->line_types = g_ptr_array_new_full(n_lines, g_free);
				for (k = 0 ; k < n_lines ; k++) {
					g_ptr_array_add(fd->line_types, g_strdup(archive->output[i][i2]->brute_force_history[j].lines[k].line_type));
				}
				g_ptr_array_add(rv, fd);
			}
			else {
				//element found -> check for new lines
				for (k = 0 ; k < archive->output[i][i2]->brute_force_history[j].n_lines ; k++) {
					found = FALSE;
					for (l = 0 ; l < ((struct fluor_data *) g_ptr_array_index(rv, loc))->line_types->len ; l++) {
						if (strcmp(archive->output[i][i2]->brute_force_history[j].lines[k].line_type, (gchar *) g_ptr_array_index(((struct fluor_data *) g_ptr_array_index(rv, loc))->line_types, l)) != 0) {
							found = TRUE;
							break;
						}
					}
					if (!found) {
						//extend array
						g_ptr_array_add(((struct fluor_data *) g_ptr_array_index(rv, loc))->line_types, g_strdup(archive->output[i][i2]->brute_force_history[j].lines[k].line_type));
					}
				}
			}
		}
	}
	}
	//qsort everything
	for (i = 0 ; i < rv->len ; i++) {
		g_ptr_array_sort(((struct fluor_data *) g_ptr_array_index(rv, i))->line_types, compare_string);
	}
	g_ptr_array_sort(rv, compare_fluor_data);

	return rv;
}

static void plot_archive_data_2D(XmiMsimGuiXmsaViewerWindow *self);
static void plot_archive_data_3D(XmiMsimGuiXmsaViewerWindow *self);

static void plot_archive_data_cb(XmiMsimGuiXmsaViewerWindow *self) {
	if (self->archive->xpath2)
		plot_archive_data_3D(self);
	else
		plot_archive_data_2D(self);
	return;
}

static void roi_xrf_toggled_cb(XmiMsimGuiXmsaViewerWindow *self, GtkToggleButton *roi_radioW) {
	if (gtk_toggle_button_get_active(roi_radioW)) {
		//ROI mode
		gtk_widget_set_sensitive(self->roi_channel_radioW, TRUE);
		gtk_widget_set_sensitive(self->roi_energy_radioW, TRUE);
		gtk_widget_set_sensitive(self->roi_start_channel_labelW, TRUE);
		gtk_widget_set_sensitive(self->roi_end_channel_labelW, TRUE);
		gtk_widget_set_sensitive(self->roi_start_energy_labelW, TRUE);
		gtk_widget_set_sensitive(self->roi_end_energy_labelW, TRUE);
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_channel_radioW))) {
			gtk_widget_set_sensitive(self->roi_start_channel_spinnerW, TRUE);
			gtk_widget_set_sensitive(self->roi_end_channel_spinnerW, TRUE);
		}
		else {
			gtk_widget_set_sensitive(self->roi_start_energy_spinnerW, TRUE);
			gtk_widget_set_sensitive(self->roi_end_energy_spinnerW, TRUE);
		}
		gtk_widget_set_sensitive(self->roi_conv_radioW, TRUE);
		gtk_widget_set_sensitive(self->roi_unconv_radioW, TRUE);
		gtk_widget_set_sensitive(self->roi_interactions_labelW, TRUE);
		gtk_widget_set_sensitive(self->roi_interactions_comboW, TRUE);
		gtk_widget_set_sensitive(self->roi_cumulative_radioW, TRUE);
		gtk_widget_set_sensitive(self->roi_individual_radioW, TRUE);
		if (!self->archive->xpath2) {
			gtk_widget_set_sensitive(self->roi_linearW, TRUE);
			gtk_widget_set_sensitive(self->roi_log10W, TRUE);
		}
		gtk_widget_set_sensitive(self->xrf_element_labelW, FALSE);
		gtk_widget_set_sensitive(self->xrf_element_comboW, FALSE);
		gtk_widget_set_sensitive(self->xrf_line_labelW, FALSE);
		gtk_widget_set_sensitive(self->xrf_line_comboW, FALSE);
		gtk_widget_set_sensitive(self->xrf_interactions_labelW, FALSE);
		gtk_widget_set_sensitive(self->xrf_interactions_comboW, FALSE);
		gtk_widget_set_sensitive(self->xrf_cumulative_radioW, FALSE);
		gtk_widget_set_sensitive(self->xrf_individual_radioW, FALSE);
		gtk_widget_set_sensitive(self->xrf_linearW, FALSE);
		gtk_widget_set_sensitive(self->xrf_log10W, FALSE);
	}
	else {
		//XRF mode
		gtk_widget_set_sensitive(self->roi_channel_radioW, FALSE);
		gtk_widget_set_sensitive(self->roi_energy_radioW, FALSE);
		gtk_widget_set_sensitive(self->roi_start_channel_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_start_channel_labelW, FALSE);
		gtk_widget_set_sensitive(self->roi_end_channel_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_end_channel_labelW, FALSE);
		gtk_widget_set_sensitive(self->roi_start_energy_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_start_energy_labelW, FALSE);
		gtk_widget_set_sensitive(self->roi_end_energy_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_end_energy_labelW, FALSE);
		gtk_widget_set_sensitive(self->roi_conv_radioW, FALSE);
		gtk_widget_set_sensitive(self->roi_unconv_radioW, FALSE);
		gtk_widget_set_sensitive(self->roi_interactions_labelW, FALSE);
		gtk_widget_set_sensitive(self->roi_interactions_comboW, FALSE);
		gtk_widget_set_sensitive(self->roi_cumulative_radioW, FALSE);
		gtk_widget_set_sensitive(self->roi_individual_radioW, FALSE);
		gtk_widget_set_sensitive(self->roi_linearW, FALSE);
		gtk_widget_set_sensitive(self->roi_log10W, FALSE);
		gtk_widget_set_sensitive(self->xrf_element_labelW, TRUE);
		gtk_widget_set_sensitive(self->xrf_element_comboW, TRUE);
		gtk_widget_set_sensitive(self->xrf_line_labelW, TRUE);
		gtk_widget_set_sensitive(self->xrf_interactions_labelW, TRUE);
		gtk_widget_set_sensitive(self->xrf_interactions_comboW, TRUE);
		gtk_widget_set_sensitive(self->xrf_cumulative_radioW, TRUE);
		gtk_widget_set_sensitive(self->xrf_individual_radioW, TRUE);
		if (gtk_combo_box_get_active(GTK_COMBO_BOX(self->xrf_element_comboW)) > 0)
			gtk_widget_set_sensitive(self->xrf_line_comboW, TRUE);
		else
			gtk_widget_set_sensitive(self->xrf_line_comboW, FALSE);
		if (!self->archive->xpath2) {
			gtk_widget_set_sensitive(self->xrf_linearW, TRUE);
			gtk_widget_set_sensitive(self->xrf_log10W, TRUE);
		}
	}


	plot_archive_data_cb(self);
	return;
}

static void roi_channel_energy_toggled_cb(XmiMsimGuiXmsaViewerWindow *self, GtkToggleButton *roi_channel_radioW) {

	if (gtk_toggle_button_get_active(roi_channel_radioW)) {
		gtk_widget_set_sensitive(self->roi_start_channel_spinnerW, TRUE);
		gtk_widget_set_sensitive(self->roi_end_channel_spinnerW, TRUE);
		gtk_widget_set_sensitive(self->roi_start_energy_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_end_energy_spinnerW, FALSE);
	}
	else {
		gtk_widget_set_sensitive(self->roi_start_channel_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_end_channel_spinnerW, FALSE);
		gtk_widget_set_sensitive(self->roi_start_energy_spinnerW, TRUE);
		gtk_widget_set_sensitive(self->roi_end_energy_spinnerW, TRUE);
	}

	plot_archive_data_cb(self);
	return;
}

static void xrf_element_changed_cb(XmiMsimGuiXmsaViewerWindow *self, GtkComboBox *xrf_element_comboW) {
	//clear xrf_line_comboW
	int i;

	g_signal_handlers_block_by_func(G_OBJECT(self->xrf_line_comboW), (void *) plot_archive_data_cb, (gpointer) self);

	for (i = gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(self->xrf_line_comboW)),NULL)-1 ;  i > 0; i--) {
		gtk_combo_box_text_remove(GTK_COMBO_BOX_TEXT(self->xrf_line_comboW), i);
	}

	if (gtk_combo_box_get_active(xrf_element_comboW) == 0) {
		//ALL elements selected
		gtk_widget_set_sensitive(self->xrf_line_comboW, FALSE);
	}
	else {
		gtk_widget_set_sensitive(self->xrf_line_comboW, TRUE);
		i = gtk_combo_box_get_active(GTK_COMBO_BOX(xrf_element_comboW))-1;
		int j;
		for (j = 0 ; j < ((struct fluor_data *) g_ptr_array_index(self->fd, i))->line_types->len ; j++) {
			gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->xrf_line_comboW), (gchar *) g_ptr_array_index(((struct fluor_data *) g_ptr_array_index(self->fd, i))->line_types, j));
		}
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(self->xrf_line_comboW), 0);
	g_signal_handlers_unblock_by_func(G_OBJECT(self->xrf_line_comboW), (void *) plot_archive_data_cb, (gpointer) self);
	plot_archive_data_cb(self);
	return;
}

static gboolean plot_archive_data_cb_helper(XmiMsimGuiXmsaViewerWindow *self) {
	plot_archive_data_cb(self);
	return FALSE;
}

static void roi_start_channel_changed_cb(XmiMsimGuiXmsaViewerWindow *self, GtkSpinButton *roi_start_channel_spinnerW) {
	gint value_start = gtk_spin_button_get_value_as_int(roi_start_channel_spinnerW);

	g_signal_handler_block(self->roi_end_channel_spinnerW, self->roi_end_channel_spinnerG);
	gtk_spin_button_set_range(GTK_SPIN_BUTTON(self->roi_end_channel_spinnerW), (double) value_start, (double) self->archive->output[0][0]->input->detector->nchannels-1);
	g_signal_handler_unblock(self->roi_end_channel_spinnerW, self->roi_end_channel_spinnerG);

	g_idle_add((GSourceFunc) plot_archive_data_cb_helper, (gpointer) self);
	return;
}

static void roi_start_energy_changed_cb(XmiMsimGuiXmsaViewerWindow *self, GtkSpinButton *roi_start_energy_spinnerW) {
	gint value_start = gtk_spin_button_get_value_as_int(roi_start_energy_spinnerW);

	g_signal_handler_block(self->roi_end_energy_spinnerW, self->roi_end_energy_spinnerG);
	gtk_spin_button_set_range(GTK_SPIN_BUTTON(self->roi_end_energy_spinnerW), (double) value_start, (self->archive->output[0][0]->input->detector->nchannels-1)*(self->archive->input[0][0]->detector->gain)+(self->archive->input[0][0]->detector->zero));
	g_signal_handler_unblock(self->roi_end_energy_spinnerW, self->roi_end_energy_spinnerG);

	g_idle_add((GSourceFunc) plot_archive_data_cb_helper, (gpointer) self);
	return;
}

static void roi_end_changed_cb(XmiMsimGuiXmsaViewerWindow *self) {
	g_idle_add((GSourceFunc) plot_archive_data_cb_helper, (gpointer) self);
	return;
}

static void save_archive_plot(XmiMsimGuiXmsaViewerWindow *self) {
	XmiMsimGuiFileChooserDialog *dialog;

	dialog = xmi_msim_gui_export_canvas_dialog_new(
		"Export plot as",
		GTK_WINDOW(self)
		);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		GError *error = NULL;
		if (!xmi_msim_gui_export_canvas_dialog_save(dialog, self->canvas, &error)) {
			xmi_msim_gui_file_chooser_dialog_destroy(dialog);
			GtkWidget *info_dialog = gtk_message_dialog_new(GTK_WINDOW(self), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error exporting plot");
			gtk_message_dialog_format_secondary_markup(GTK_MESSAGE_DIALOG(info_dialog), "%s", error->message);

			gtk_dialog_run(GTK_DIALOG(info_dialog));
			gtk_widget_destroy(info_dialog);

			g_error_free(error);
			return;
		}
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);

	return;
}

static void export_archive_plot_printf_error_dialog(XmiMsimGuiXmsaViewerWindow *self, gchar *filename, GError *error, GFileOutputStream *file_stream) {
	GtkWidget *message_dialog = gtk_message_dialog_new(GTK_WINDOW(self), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not write to %s.", filename);
	gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(message_dialog), "%s", error->message);
	gtk_dialog_run(GTK_DIALOG(message_dialog));
	g_error_free(error);
	g_free(filename);
	g_object_unref(file_stream);
	gtk_widget_destroy(message_dialog);
}

static void export_archive_plot(XmiMsimGuiXmsaViewerWindow *self) {
	//fire up file dialog
	XmiMsimGuiFileChooserDialog *dialog  = xmi_msim_gui_file_chooser_dialog_new(
		"Select the filename of the CSV file",
		GTK_WINDOW(self),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel");
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.csv");
	gtk_file_filter_add_pattern(filter,"*.CSV");
	gtk_file_filter_set_name(filter,"CSV files");
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gchar *filename;

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
		xmi_msim_gui_utils_ensure_extension(&filename, ".csv");
		//open file
		GFile *file = g_file_new_for_path(filename);
		GError *error = NULL;
		GFileOutputStream *file_stream = g_file_replace(file, NULL, FALSE, G_FILE_CREATE_NONE, NULL, &error);
		g_object_unref(file);
		if (file_stream == NULL) {
			GtkWidget *message_dialog = gtk_message_dialog_new(GTK_WINDOW(self), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not open %s for writing.", filename);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(message_dialog), "%s", error->message);
			gtk_dialog_run(GTK_DIALOG(message_dialog));
			g_error_free(error);
			g_free(filename);
			gtk_widget_destroy(message_dialog);
			return;
		}
		int i,j;
		if (self->archive->xpath2) {
			//3D
			//ROW1
			//start with an empty block
			if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, ",")) {
				export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
				return;
			}
			for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
				if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, "%g", self->x[i*(self->archive->nsteps2+1)])) {
					export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
					return;
				}
				if (i == self->archive->nsteps1) {
					if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, "\n")) {
						export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
						return;
					}
				}
				else {
					if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, ",")) {
						export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
						return;
					}
				}
			}
			//OTHER ROWS
			for (j = 0 ; j <= self->archive->nsteps2 ; j++) {
				if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, "%g,", self->y[j])) {
					export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
					return;
				}
				for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
					if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, "%g", self->z[i*(self->archive->nsteps2+1)+j])) {
						export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
						return;
					}
					if (i == self->archive->nsteps1) {
						if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, "\n")) {
							export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
							return;
						}
					}
					else {
						if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, ",")) {
							export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
							return;
						}
					}
				}
			}
		}
		else {
			//2D
			for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
				//this %g may cause serious trouble on Windows...
				if (!g_output_stream_printf(G_OUTPUT_STREAM(file_stream), NULL, NULL, &error, "%g,%g\n", self->x[i], self->y[i])) {
					export_archive_plot_printf_error_dialog(self, filename, error, file_stream);
					return;
				}
			}
		}
		g_object_unref(file_stream);

		GtkWidget *message_dialog = gtk_message_dialog_new(GTK_WINDOW(self), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Successfully created CSV file %s.", filename);
		gtk_dialog_run(GTK_DIALOG(message_dialog));
		gtk_widget_destroy(message_dialog);

		g_free (filename);
	} else {
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
	}
	return;
}

static void axis_title_changed_cb(XmiMsimGuiXmsaViewerWindow *self, GtkEntry *axis_titleW) {
	self->plot_window->set_axis_title_x(gtk_entry_get_text(GTK_ENTRY(self->xaxis_titleW)));
	self->plot_window->set_axis_title_y(gtk_entry_get_text(GTK_ENTRY(self->yaxis_titleW)));
}

static void xmi_msim_gui_xmsa_viewer_window_constructed(GObject *obj) {
	XmiMsimGuiXmsaViewerWindow *self = XMI_MSIM_GUI_XMSA_VIEWER_WINDOW(obj);

	//find all unique elements and lines
	int i;
	gchar *interaction;

	self->fd = get_fluor_data(self->archive);

	GtkWidget *mainHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_container_set_border_width(GTK_CONTAINER(mainHBox),5);
	gtk_container_add(GTK_CONTAINER(self), mainHBox);

	GtkWidget *mainVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, FALSE, FALSE, 1);
	GtkWidget *lilVBox;
	GtkWidget *lilHBox;

	//roi
	lilVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	self->roi_radioW = gtk_radio_button_new_from_widget(NULL);
	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<b>Region of interest integration</b>");
	gtk_container_add(GTK_CONTAINER(self->roi_radioW), label);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_radioW), FALSE);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->roi_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	GtkWidget *tinyVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
	gtk_box_set_homogeneous(GTK_BOX(tinyVBox), TRUE);
	self->roi_channel_radioW = gtk_radio_button_new(NULL);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_channel_radioW), FALSE);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->roi_channel_radioW, FALSE, FALSE, 0);

	self->roi_start_channel_labelW = gtk_label_new("First channel");
	self->roi_end_channel_labelW = gtk_label_new("Last channel");
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_start_channel_labelW, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_end_channel_labelW, TRUE, FALSE, 2);
	gtk_container_add(GTK_CONTAINER(self->roi_channel_radioW), tinyVBox);

	tinyVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	self->roi_start_channel_spinnerW = gtk_spin_button_new_with_range(0, self->archive->output[0][0]->input->detector->nchannels-2, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(self->roi_start_channel_spinnerW), GTK_UPDATE_IF_VALID);
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_start_channel_spinnerW, FALSE, FALSE, 2);
	self->roi_end_channel_spinnerW = gtk_spin_button_new_with_range(1, self->archive->output[0][0]->input->detector->nchannels-1, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(self->roi_end_channel_spinnerW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(self->roi_end_channel_spinnerW), self->archive->output[0][0]->input->detector->nchannels-1);
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_end_channel_spinnerW, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(lilHBox), tinyVBox, FALSE, FALSE, 2);
	gtk_widget_set_margin_start(lilHBox, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 0);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	tinyVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 8);
	gtk_box_set_homogeneous(GTK_BOX(tinyVBox), TRUE);
	self->roi_energy_radioW = gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(self->roi_channel_radioW));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_energy_radioW), FALSE);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->roi_energy_radioW, FALSE, FALSE, 0);

	self->roi_start_energy_labelW = gtk_label_new("First energy");
	self->roi_end_energy_labelW = gtk_label_new("Last energy");
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_start_energy_labelW, TRUE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_end_energy_labelW, TRUE, FALSE, 2);
	gtk_container_add(GTK_CONTAINER(self->roi_energy_radioW), tinyVBox);

	tinyVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	double energy_min = 0.0;
	double energy_max = (self->archive->output[0][0]->input->detector->nchannels-2)*(self->archive->input[0][0]->detector->gain)+(self->archive->input[0][0]->detector->zero);
	self->roi_start_energy_spinnerW = gtk_spin_button_new_with_range(energy_min, energy_max, 0.01);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(self->roi_start_energy_spinnerW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(self->roi_start_energy_spinnerW), 2);
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_start_energy_spinnerW, FALSE, FALSE, 2);
	energy_min = self->archive->input[0][0]->detector->gain;
	energy_max = (self->archive->output[0][0]->input->detector->nchannels-1)*(self->archive->input[0][0]->detector->gain)+(self->archive->input[0][0]->detector->zero);
	self->roi_end_energy_spinnerW = gtk_spin_button_new_with_range(energy_min, energy_max, 0.01);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(self->roi_end_energy_spinnerW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(self->roi_end_energy_spinnerW), energy_max);
	gtk_spin_button_set_digits(GTK_SPIN_BUTTON(self->roi_end_energy_spinnerW), 2);
	gtk_box_pack_start(GTK_BOX(tinyVBox), self->roi_end_energy_spinnerW, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(lilHBox), tinyVBox, FALSE, FALSE, 2);
	gtk_widget_set_margin_start(lilHBox, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 0);

	self->roi_conv_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "Use convoluted spectra");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_conv_radioW), TRUE);
	gtk_widget_set_margin_start(self->roi_conv_radioW, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->roi_conv_radioW, FALSE, FALSE, 2);

	self->roi_unconv_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(self->roi_conv_radioW), "Use unconvoluted spectra");
	gtk_widget_set_margin_start(self->roi_unconv_radioW, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->roi_unconv_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	self->roi_interactions_labelW = gtk_label_new("Number of interactions");
	gtk_widget_set_margin_start(self->roi_interactions_labelW, 20);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->roi_interactions_labelW, FALSE, FALSE, 3);
	self->roi_interactions_comboW = gtk_combo_box_text_new();
	if (self->archive->output[0][0]->use_zero_interactions)
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->roi_interactions_comboW), "0");
	for (i = 1 ; i <= self->archive->output[0][0]->ninteractions ; i++) {
		interaction = g_strdup_printf("%i", i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->roi_interactions_comboW), interaction);
		g_free(interaction);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(self->roi_interactions_comboW), gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(self->roi_interactions_comboW)),NULL)-1);
	gtk_box_pack_end(GTK_BOX(lilHBox), self->roi_interactions_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	self->roi_cumulative_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "Cumulative interaction contributions");
	gtk_widget_set_margin_start(self->roi_cumulative_radioW, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->roi_cumulative_radioW, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_cumulative_radioW), TRUE);
	self->roi_individual_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(self->roi_cumulative_radioW), "Individual interaction contributions");
	gtk_widget_set_margin_start(self->roi_individual_radioW, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->roi_individual_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	self->roi_linearW = gtk_radio_button_new_with_label_from_widget(NULL, "Linear");
	gtk_widget_set_margin_start(self->roi_linearW, 20);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->roi_linearW, FALSE, FALSE, 0);
	self->roi_log10W = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(self->roi_linearW), "Logarithmic");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_linearW), TRUE);
	gtk_box_pack_end(GTK_BOX(lilHBox), self->roi_log10W, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(mainVBox), lilVBox, TRUE, FALSE, 2);



	//xrf
	lilVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	self->xrf_radioW= gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(self->roi_radioW));
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<b>X-ray fluorescence lines</b>");
	gtk_container_add(GTK_CONTAINER(self->xrf_radioW), label);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->xrf_radioW), TRUE);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->xrf_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	self->xrf_element_labelW = gtk_label_new("Element");
	gtk_widget_set_margin_start(self->xrf_element_labelW, 20);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->xrf_element_labelW, FALSE, FALSE, 3);
	gchar *element;
	self->xrf_element_comboW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->xrf_element_comboW), "All");
	for (i = 0 ; i < self->fd->len ; i++) {
		element = AtomicNumberToSymbol(((struct fluor_data *) g_ptr_array_index(self->fd, i))->atomic_number);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->xrf_element_comboW), element);
		xrlFree(element);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(self->xrf_element_comboW), 0);
	gtk_box_pack_end(GTK_BOX(lilHBox), self->xrf_element_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	self->xrf_line_labelW = gtk_label_new("XRF line");
	gtk_widget_set_margin_start(self->xrf_line_labelW, 20);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->xrf_line_labelW, FALSE, FALSE, 3);
	self->xrf_line_comboW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->xrf_line_comboW), "All");
	gtk_combo_box_set_active(GTK_COMBO_BOX(self->xrf_line_comboW), 0);
	gtk_box_pack_end(GTK_BOX(lilHBox), self->xrf_line_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	self->xrf_interactions_labelW = gtk_label_new("Number of interactions");
	gtk_widget_set_margin_start(self->xrf_interactions_labelW, 20);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->xrf_interactions_labelW, FALSE, FALSE, 3);
	self->xrf_interactions_comboW = gtk_combo_box_text_new();
	for (i = 1 ; i <= self->archive->output[0][0]->ninteractions ; i++) {
		interaction = g_strdup_printf("%i", i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(self->xrf_interactions_comboW), interaction);
		g_free(interaction);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(self->xrf_interactions_comboW), gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(self->xrf_interactions_comboW)),NULL)-1);
	gtk_box_pack_end(GTK_BOX(lilHBox), self->xrf_interactions_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	self->xrf_cumulative_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "Cumulative interaction contributions");
	gtk_widget_set_margin_start(self->xrf_cumulative_radioW, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->xrf_cumulative_radioW, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->xrf_cumulative_radioW), TRUE);
	self->xrf_individual_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(self->xrf_cumulative_radioW), "Individual interaction contributions");
	gtk_widget_set_margin_start(self->xrf_individual_radioW, 20);
	gtk_box_pack_start(GTK_BOX(lilVBox), self->xrf_individual_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	self->xrf_linearW = gtk_radio_button_new_with_label_from_widget(NULL, "Linear");
	gtk_widget_set_margin_start(self->xrf_linearW, 20);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->xrf_linearW, FALSE, FALSE, 0);
	self->xrf_log10W = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(self->xrf_linearW), "Logarithmic");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->xrf_linearW), TRUE);
	gtk_box_pack_end(GTK_BOX(lilHBox), self->xrf_log10W, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(mainVBox), lilVBox, TRUE, FALSE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(lilHBox), TRUE);
	self->okButton = gtk_button_new_with_mnemonic("_Ok");
	gtk_box_pack_start(GTK_BOX(lilHBox), self->okButton, TRUE, TRUE, 2);
	self->imageButton = gtk_button_new_with_label("Save image");
	gtk_box_pack_start(GTK_BOX(lilHBox), self->imageButton, TRUE, TRUE, 2);
	self->exportButton = gtk_button_new_with_label("Export as CSV");
	gtk_box_pack_start(GTK_BOX(lilHBox), self->exportButton, TRUE, TRUE, 2);
	gtk_box_pack_end(GTK_BOX(mainVBox), lilHBox, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(mainVBox), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 4);


	gtk_box_pack_start(GTK_BOX(mainHBox), gtk_separator_new(GTK_ORIENTATION_VERTICAL), FALSE, FALSE, 4);
	//canvas
	mainVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	self->canvas = Gtk::manage(new Gtk::PLplot::Canvas());
	self->canvas->set_hexpand(true);
	self->canvas->set_vexpand(true);
	GtkWidget *aspect_frame = gtk_aspect_frame_new("", 0.5, 0.5, 842.0/595.0, FALSE);
	gtk_widget_set_hexpand(aspect_frame, TRUE);
	gtk_widget_set_vexpand(aspect_frame, TRUE);
	gtk_container_add(GTK_CONTAINER(aspect_frame), GTK_WIDGET(self->canvas->gobj()));
	gtk_box_pack_start(GTK_BOX(mainVBox), aspect_frame, TRUE, TRUE, 2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	label = gtk_label_new("X-axis title");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	self->xaxis_titleW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->xaxis_titleW), TRUE);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->xaxis_titleW, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox),lilHBox,FALSE,FALSE,2);

	lilHBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	label = gtk_label_new("Y-axis title");
	gtk_box_pack_start(GTK_BOX(lilHBox), label, FALSE, FALSE, 2);
	self->yaxis_titleW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(self->yaxis_titleW), TRUE);
	gtk_box_pack_start(GTK_BOX(lilHBox), self->yaxis_titleW, TRUE, TRUE, 2);
	gtk_box_pack_start(GTK_BOX(mainVBox), lilHBox, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, TRUE, TRUE, 2);

	gtk_entry_set_text(GTK_ENTRY(self->xaxis_titleW), self->archive->xpath1);
	if (self->archive->xpath2) {
		gtk_entry_set_text(GTK_ENTRY(self->yaxis_titleW), self->archive->xpath2);
		gtk_widget_set_sensitive(self->roi_linearW, FALSE);
		gtk_widget_set_sensitive(self->roi_log10W, FALSE);
		gtk_widget_set_sensitive(self->xrf_linearW, FALSE);
		gtk_widget_set_sensitive(self->xrf_log10W, FALSE);
	}
	else {
		gtk_entry_set_text(GTK_ENTRY(self->yaxis_titleW), "Intensity");
	}


	//default sensitivities
	gtk_widget_set_sensitive(self->xrf_line_comboW, FALSE);
	gtk_widget_set_sensitive(self->roi_channel_radioW, FALSE);
	gtk_widget_set_sensitive(self->roi_energy_radioW, FALSE);
	gtk_widget_set_sensitive(self->roi_start_channel_spinnerW, FALSE);
	gtk_widget_set_sensitive(self->roi_start_channel_labelW, FALSE);
	gtk_widget_set_sensitive(self->roi_end_channel_spinnerW, FALSE);
	gtk_widget_set_sensitive(self->roi_end_channel_labelW, FALSE);
	gtk_widget_set_sensitive(self->roi_start_energy_spinnerW, FALSE);
	gtk_widget_set_sensitive(self->roi_start_energy_labelW, FALSE);
	gtk_widget_set_sensitive(self->roi_end_energy_spinnerW, FALSE);
	gtk_widget_set_sensitive(self->roi_end_energy_labelW, FALSE);
	gtk_widget_set_sensitive(self->roi_conv_radioW, FALSE);
	gtk_widget_set_sensitive(self->roi_unconv_radioW, FALSE);
	gtk_widget_set_sensitive(self->roi_interactions_labelW, FALSE);
	gtk_widget_set_sensitive(self->roi_interactions_comboW, FALSE);
	gtk_widget_set_sensitive(self->roi_cumulative_radioW, FALSE);
	gtk_widget_set_sensitive(self->roi_individual_radioW, FALSE);
	gtk_widget_set_sensitive(self->roi_linearW, FALSE);
	gtk_widget_set_sensitive(self->roi_log10W, FALSE);

	int j;
	if (self->archive->xpath2) {
		//3D
		self->x = (double *) g_malloc(sizeof(double)*(self->archive->nsteps1+1)*(self->archive->nsteps2+1));
		self->y = (double *) g_malloc(sizeof(double)*(self->archive->nsteps1+1)*(self->archive->nsteps2+1));
		self->z = (double *) g_malloc(sizeof(double)*(self->archive->nsteps1+1)*(self->archive->nsteps2+1));
		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			for (j = 0 ; j <= self->archive->nsteps2 ; j++) {
				self->x[i*(self->archive->nsteps2+1)+j] = self->archive->start_value1 + (self->archive->end_value1 - self->archive->start_value1)*i/self->archive->nsteps1;
				self->y[i*(self->archive->nsteps2+1)+j] = self->archive->start_value2 + (self->archive->end_value2 - self->archive->start_value2)*j/self->archive->nsteps2;
			}
		}
	}
	else {
		//2D
		self->x = (double *) g_malloc(sizeof(double)*(self->archive->nsteps1+1));
		self->y = (double *) g_malloc(sizeof(double)*(self->archive->nsteps1+1));
		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			self->x[i] = self->archive->start_value1 + (self->archive->end_value1 - self->archive->start_value1)*i/self->archive->nsteps1;
		}
	}


	//callbacks registration
	g_signal_connect_swapped(G_OBJECT(self->roi_radioW), "toggled", G_CALLBACK(roi_xrf_toggled_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->roi_channel_radioW), "toggled", G_CALLBACK(roi_channel_energy_toggled_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->xrf_element_comboW), "changed", G_CALLBACK(xrf_element_changed_cb), self);
	self->roi_start_channel_spinnerG = g_signal_connect_swapped(G_OBJECT(self->roi_start_channel_spinnerW), "value-changed", G_CALLBACK(roi_start_channel_changed_cb), self);
	self->roi_start_energy_spinnerG = g_signal_connect_swapped(G_OBJECT(self->roi_start_energy_spinnerW), "value-changed", G_CALLBACK(roi_start_energy_changed_cb), self);

	self->roi_end_channel_spinnerG = g_signal_connect_swapped(G_OBJECT(self->roi_end_channel_spinnerW), "value-changed", G_CALLBACK(roi_end_changed_cb), self);
	self->roi_end_energy_spinnerG = g_signal_connect_swapped(G_OBJECT(self->roi_end_energy_spinnerW), "value-changed", G_CALLBACK(roi_end_changed_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->roi_conv_radioW), "toggled", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->roi_interactions_comboW), "changed", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->roi_cumulative_radioW), "toggled", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->xrf_line_comboW), "changed", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->xrf_interactions_comboW), "changed", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->xrf_cumulative_radioW), "toggled", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->roi_linearW), "toggled", G_CALLBACK(plot_archive_data_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->xrf_linearW), "toggled", G_CALLBACK(plot_archive_data_cb), self);

	g_signal_connect_swapped(G_OBJECT(self->okButton), "clicked", G_CALLBACK(gtk_widget_destroy), self);

	g_signal_connect_swapped(G_OBJECT(self->imageButton), "clicked", G_CALLBACK(save_archive_plot), self);
	g_signal_connect_swapped(G_OBJECT(self->exportButton), "clicked", G_CALLBACK(export_archive_plot), self);
	g_signal_connect_swapped(G_OBJECT(self->xaxis_titleW), "changed", G_CALLBACK(axis_title_changed_cb), self);
	g_signal_connect_swapped(G_OBJECT(self->yaxis_titleW), "changed", G_CALLBACK(axis_title_changed_cb), self);

	gtk_widget_show_all(mainHBox);

	//if there is no fluor_data, disable the XRF mode
	if (self->fd->len == 0) {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->roi_radioW), TRUE);
		gtk_widget_set_sensitive(self->xrf_radioW, FALSE);
		return;
	}


	plot_archive_data_cb(self);
	G_OBJECT_CLASS(xmi_msim_gui_xmsa_viewer_window_parent_class)->constructed(obj);
}

static void close_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	gtk_widget_destroy(GTK_WIDGET(user_data));
}

static void minimize_activated(GSimpleAction *action, GVariant *parameter, gpointer user_data) {
	gtk_window_iconify(GTK_WINDOW(user_data));
}

static GActionEntry win_entries[] = {
	{"close", close_activated, NULL, NULL, NULL},
	{"minimize", minimize_activated, NULL, NULL, NULL},
};

static void xmi_msim_gui_xmsa_viewer_window_init(XmiMsimGuiXmsaViewerWindow *self) {
	g_action_map_add_action_entries(G_ACTION_MAP(self), win_entries, G_N_ELEMENTS(win_entries), self);
}

GtkWidget* xmi_msim_gui_xmsa_viewer_window_new(XmiMsimGuiApplication *app, struct xmi_archive *archive) {
	g_return_val_if_fail(archive != NULL, NULL);

	XmiMsimGuiXmsaViewerWindow *rv = XMI_MSIM_GUI_XMSA_VIEWER_WINDOW(
					g_object_new(XMI_MSIM_GUI_TYPE_XMSA_VIEWER_WINDOW,
					"application", app,
					"archive", archive,
					"title", "Batch mode plot",
					"window-position", GTK_WIN_POS_CENTER,
					"modal", TRUE,
					"default-width", 1100,
					"resizable", FALSE,
					NULL)
				);

	return GTK_WIDGET(rv);
}

static void xmi_msim_gui_xmsa_viewer_window_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiXmsaViewerWindow *window= XMI_MSIM_GUI_XMSA_VIEWER_WINDOW(object);

  switch (prop_id) {
    case 1:
      window->archive =  (struct xmi_archive *) g_value_get_pointer(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

static void plot_archive_data_2D(XmiMsimGuiXmsaViewerWindow *self) {
	//first section will deal with generating the x- and y-values
	double *x, *y;
	int i,j,k,l;
	gchar *buffer;
	gdouble minval = 1E50;

	x = self->x;
	y = self->y;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_radioW))) {
		//ROI mode
		gboolean cumulative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_cumulative_radioW));
		gboolean convoluted = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_conv_radioW));
		gint start_channel, end_channel;
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_channel_radioW))) {
			start_channel = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(self->roi_start_channel_spinnerW));
			end_channel = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(self->roi_end_channel_spinnerW));
		}
		else {
			gdouble start_energy = gtk_spin_button_get_value(GTK_SPIN_BUTTON(self->roi_start_energy_spinnerW));
			gdouble end_energy = gtk_spin_button_get_value(GTK_SPIN_BUTTON(self->roi_end_energy_spinnerW));
			start_channel = (int) ((start_energy - self->archive->input[0][0]->detector->zero)/self->archive->input[0][0]->detector->gain);
			end_channel = (int) ((end_energy - self->archive->input[0][0]->detector->zero)/self->archive->input[0][0]->detector->gain);
		}
		buffer = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(self->roi_interactions_comboW));
		gint interaction = strtol(buffer, NULL, 10);
		g_free(buffer);

		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			double yval = 0.0;
			//the spectra are already stored cumulative
			for (k = start_channel ; k <= end_channel ; k++) {
				if (convoluted) {
					yval += self->archive->output[i][0]->channels_conv[interaction][k];
					if (!cumulative && interaction > (self->archive->output[i][0]->use_zero_interactions ? 0 : 1))
						yval -= self->archive->output[i][0]->channels_conv[interaction-1][k];
				}
				else {
					yval += self->archive->output[i][0]->channels_unconv[interaction][k];
					if (!cumulative && interaction > (self->archive->output[i][0]->use_zero_interactions ? 0 : 1))
						yval -= self->archive->output[i][0]->channels_unconv[interaction-1][k];
				}
			}

			y[i] = MAX(yval, 0);
			if (yval > 0.0 && yval < minval)
				minval = yval;
		}
	}
	else {
		//XRF mode
		struct xmi_fluorescence_line_counts **history = NULL;
		int *nhistory = NULL;

		nhistory = (int *) g_malloc(sizeof(int)*(self->archive->nsteps1+1));
		history = (struct xmi_fluorescence_line_counts **) g_malloc(sizeof(struct xmi_fluorescence_line_counts *)*(self->archive->nsteps1+1));

		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			if (self->archive->output[i][0]->nvar_red_history > 0) {
				history[i] = self->archive->output[i][0]->var_red_history;
				nhistory[i] = self->archive->output[i][0]->nvar_red_history;
			}
			else if (self->archive->output[i][0]->nbrute_force_history > 0) {
				history[i] = self->archive->output[i][0]->brute_force_history;
				nhistory[i] = self->archive->output[i][0]->nbrute_force_history;
			}
			else {
				history[i] = NULL;
				nhistory[i] = 0;
			}
		}


		gboolean cumulative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_cumulative_radioW));
		gchar *line_type = NULL;
		int atomic_number_index;
		if (gtk_combo_box_get_active(GTK_COMBO_BOX(self->xrf_line_comboW)) == 0)
			line_type = NULL;
		else
			line_type = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(self->xrf_line_comboW));
		buffer = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(self->xrf_interactions_comboW));
		gint interaction = strtol(buffer, NULL, 10);
		g_free(buffer);
		atomic_number_index = gtk_combo_box_get_active(GTK_COMBO_BOX(self->xrf_element_comboW))-1;

		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			double yval = 0.0;
			if (atomic_number_index == -1) {
				//Element = All
				//XRF line = All
				if (cumulative) {
					for (j = 0 ; j < nhistory[i] ; j++) {
						for (k = 0 ; k < history[i][j].n_lines ; k++) {
							for (l = 0 ; l < history[i][j].lines[k].n_interactions ; l++) {
								if (history[i][j].lines[k].interactions[l].interaction_number <= interaction)
									yval += history[i][j].lines[k].interactions[l].counts;
							}
						}
					}
				}
				else {
					for (j = 0 ; j < nhistory[i] ; j++) {
						for (k = 0 ; k < history[i][j].n_lines ; k++) {
							for (l = 0 ; l < history[i][j].lines[k].n_interactions ; l++) {
								if (history[i][j].lines[k].interactions[l].interaction_number == interaction)
									yval += history[i][j].lines[k].interactions[l].counts;
							}
						}
					}
				}
			}
			else {
				//specific element selected
				if (line_type == NULL) {
					//XRF line = All
					for (j = 0 ; j < nhistory[i] ; j++) {
						if (history[i][j].atomic_number == ((struct fluor_data *) g_ptr_array_index(self->fd, atomic_number_index))->atomic_number) {
							for (k = 0 ; k < history[i][j].n_lines ; k++) {
								for (l = 0 ; l < history[i][j].lines[k].n_interactions ; l++) {
									if ((!cumulative && history[i][j].lines[k].interactions[l].interaction_number == interaction) || (cumulative && history[i][j].lines[k].interactions[l].interaction_number <= interaction))
										yval += history[i][j].lines[k].interactions[l].counts;
								}
							}

						}
					}
				}
				else {
					//XRF line = specific
					for (j = 0 ; j < nhistory[i] ; j++) {
						if (history[i][j].atomic_number == ((struct fluor_data *) g_ptr_array_index(self->fd, atomic_number_index))->atomic_number) {
							for (k = 0 ; k < history[i][j].n_lines ; k++) {
								if (strcmp(history[i][j].lines[k].line_type, line_type) == 0) {
									for (l = 0 ; l < history[i][j].lines[k].n_interactions ; l++) {
										if ((!cumulative && history[i][j].lines[k].interactions[l].interaction_number == interaction) || (cumulative && history[i][j].lines[k].interactions[l].interaction_number <= interaction))
											yval += history[i][j].lines[k].interactions[l].counts;
									}
								}
							}
						}
					}
				}
			}
			y[i] = yval;
			if (yval > 0.0 && yval < minval)
				minval = yval;
		}
		g_free(history);
		g_free(nhistory);
	}

	minval = MIN(minval, 1.0);

	if ((gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_radioW)) && gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_log10W))) || (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_radioW)) && gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_log10W)))) {
		//log10 selected
		for (i = 0 ; i < (self->archive->nsteps1+1) ; i++) {
			if (y[i] < minval)
				y[i] = minval;
		}
	}


	//y values have been calculated -> plot
	try {
		self->canvas->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}
	//add box with default settings
	Plot2DBatch *plot_window = Gtk::manage(new Plot2DBatch(gtk_entry_get_text(GTK_ENTRY(self->xaxis_titleW)), gtk_entry_get_text(GTK_ENTRY(self->yaxis_titleW))));
	plot_window->hide();
	plot_window->hide_legend();
	plot_window->set_box_style(Gtk::PLplot::BoxStyle::BOX_TICKS_TICK_LABELS_MAIN_AXES_MAJOR_TICK_GRID);
	self->canvas->add_plot(*plot_window);

	double real_ymax = xmi_maxval_double(y,self->archive->nsteps1+1);
	double real_ymin = xmi_minval_double(y,self->archive->nsteps1+1);

	double plot_ymax = 0.95*(real_ymax-real_ymin)/0.9 + real_ymin;
	double plot_ymin = -0.05*(real_ymax-real_ymin)/0.9 + real_ymin;
	double plot_xmin = -0.05*(x[self->archive->nsteps1]-x[0])/0.9 + x[0];
	double plot_xmax = 0.95*(x[self->archive->nsteps1]-x[0])/0.9 + x[0];

	if (x[0] >= 0) {
		plot_xmin = MAX(0, plot_xmin);
	}
	plot_ymin = MAX(0, plot_ymin);

	if (real_ymax == 0.0) {
		//if y is zero everywhere
		plot_ymax = 1.0;
		plot_ymin = 0.0;
	}

	plot_window->show();
	self->plot_window = plot_window;

	std::vector<double> x_vals(x, x + self->archive->nsteps1+1);
	std::vector<double> y_vals(y, y + self->archive->nsteps1+1);
	if (plot_window->get_axis_logarithmic_y()) {
		std::for_each(std::begin(y_vals), std::end(y_vals), [real_ymin](double &a) { if (a < MAX(pow(10.0,log10(real_ymin)*0.95), 1.0)) a = MAX(pow(10.0,log10(real_ymin)*0.95), 1.0);});
	}

	Gtk::PLplot::PlotData2D *dataset= Gtk::manage(
		new Gtk::PLplot::PlotData2D(
			x_vals,
			y_vals
		)
	);
	dataset->set_color(Gdk::RGBA(&blue_plot));
	dataset->set_line_width(2.0);
	dataset->set_symbol_color(Gdk::RGBA(&red_plot));
	dataset->set_symbol("•");
	dataset->set_symbol_height_scale_factor(2.0);
	dataset->show();
	plot_window->add_data(*dataset);
	if ((gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_radioW)) && gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_linearW))) || (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_radioW)) && gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_linearW)))) {
		plot_window->set_axis_logarithmic_y(false);
		plot_window->signal_double_press().connect([plot_window, plot_xmin, plot_xmax, plot_ymin, plot_ymax](double x, double y){
			plot_window->set_region(plot_xmin, plot_xmax, plot_ymin, plot_ymax);
		});
		plot_window->set_region(plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	}
	else {
		plot_window->set_axis_logarithmic_y(true);
		plot_window->signal_double_press().connect([plot_window, plot_xmin, plot_xmax, real_ymin, real_ymax](double x, double y){
			plot_window->set_region(plot_xmin, plot_xmax, MAX(pow(10.0,log10(real_ymin)*0.95), 1.0), pow(10.0,log10(real_ymax)*1.05));
		});
		plot_window->set_region(plot_xmin, plot_xmax, MAX(pow(10.0,log10(real_ymin)*0.95), 1.0), pow(10.0,log10(real_ymax)*1.05));
	}

	return;
}

static void plot_archive_data_3D(XmiMsimGuiXmsaViewerWindow *self) {
	//first section will deal with generating the x-, y- and z-values
	int i,j,k,l,i2;
	gchar *buffer;
	gdouble minval = 1.0E50;
	double *x, *y, *z;

	x = self->x;
	y = self->y;
	z = self->z;

	double *xx = (double *) g_malloc(sizeof(double ) * (self->archive->nsteps1+1));
	double **zz = (double **) g_malloc(sizeof(double *) * (self->archive->nsteps1+1));
	for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
		zz[i] = z + i * (self->archive->nsteps2+1);
		xx[i] = self->archive->start_value1 + (self->archive->end_value1 - self->archive->start_value1)*i/self->archive->nsteps1;
	}

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_radioW))) {
		//ROI mode
		gboolean cumulative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_cumulative_radioW));
		gboolean convoluted = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_conv_radioW));
		gint start_channel, end_channel;
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_channel_radioW))) {
			start_channel = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(self->roi_start_channel_spinnerW));
			end_channel = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(self->roi_end_channel_spinnerW));
		}
		else {
			gdouble start_energy = gtk_spin_button_get_value(GTK_SPIN_BUTTON(self->roi_start_energy_spinnerW));
			gdouble end_energy = gtk_spin_button_get_value(GTK_SPIN_BUTTON(self->roi_end_energy_spinnerW));
			start_channel = (int) ((start_energy - self->archive->input[0][0]->detector->zero)/self->archive->input[0][0]->detector->gain);
			end_channel = (int) ((end_energy - self->archive->input[0][0]->detector->zero)/self->archive->input[0][0]->detector->gain);
		}
		buffer = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(self->roi_interactions_comboW));
		gint interaction = strtol(buffer, NULL, 10);
		g_free(buffer);

		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
		for (i2 = 0 ; i2 <= self->archive->nsteps2 ; i2++) {
			double zval = 0.0;
			//the spectra are already stored cumulative
			for (k = start_channel ; k <= end_channel ; k++) {
				if (convoluted) {
					zval += self->archive->output[i][i2]->channels_conv[interaction][k];
					if (!cumulative && interaction > (self->archive->output[i][i2]->use_zero_interactions ? 0 : 1))
						zval -= self->archive->output[i][i2]->channels_conv[interaction-1][k];
				}
				else {
					zval += self->archive->output[i][i2]->channels_unconv[interaction][k];
					if (!cumulative && interaction > (self->archive->output[i][i2]->use_zero_interactions ? 0 : 1))
						zval -= self->archive->output[i][i2]->channels_unconv[interaction-1][k];
				}
			}

			z[i*(self->archive->nsteps2+1)+i2] = MAX(zval, 0);
			if (zval > 0.0 && zval < minval)
				minval = zval;
		}
		}
	}
	else {
		//XRF mode
		struct xmi_fluorescence_line_counts ***history = NULL;
		int **nhistory = NULL;

		nhistory = (int **) g_malloc(sizeof(int*)*(self->archive->nsteps1+1));
		history = (struct xmi_fluorescence_line_counts ***) g_malloc(sizeof(struct xmi_fluorescence_line_counts **)*(self->archive->nsteps1+1));

		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			nhistory[i] = (int *) g_malloc(sizeof(int)*(self->archive->nsteps2+1));
			history[i] = (struct xmi_fluorescence_line_counts **) g_malloc(sizeof(struct xmi_fluorescence_line_counts *)*(self->archive->nsteps2+1));
			for (i2 = 0 ; i2 <= self->archive->nsteps2 ; i2++) {
				if (self->archive->output[i][i2]->nvar_red_history > 0) {
					history[i][i2] = self->archive->output[i][i2]->var_red_history;
					nhistory[i][i2] = self->archive->output[i][i2]->nvar_red_history;
				}
				else if (self->archive->output[i][i2]->nbrute_force_history > 0) {
					history[i][i2] = self->archive->output[i][i2]->brute_force_history;
					nhistory[i][i2] = self->archive->output[i][i2]->nbrute_force_history;
				}
				else {
					history[i][i2] = NULL;
					nhistory[i][i2] = 0;
				}
			}
		}

		gboolean cumulative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_cumulative_radioW));
		gchar *line_type = NULL;
		int atomic_number_index;
		if (gtk_combo_box_get_active(GTK_COMBO_BOX(self->xrf_line_comboW)) == 0)
			line_type = NULL;
		else
			line_type = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(self->xrf_line_comboW));
		buffer = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(self->xrf_interactions_comboW));
		gint interaction = strtol(buffer, NULL, 10);
		g_free(buffer);
		atomic_number_index = gtk_combo_box_get_active(GTK_COMBO_BOX(self->xrf_element_comboW))-1;

		for (i = 0 ; i <= self->archive->nsteps1 ; i++) {
			for (i2 = 0 ; i2 <= self->archive->nsteps2 ; i2++) {
				double zval = 0.0;
				if (atomic_number_index == -1) {
					//Element = All
					//XRF line = All
					if (cumulative) {
						for (j = 0 ; j < nhistory[i][i2] ; j++) {
							for (k = 0 ; k < history[i][i2][j].n_lines ; k++) {
								for (l = 0 ; l < history[i][i2][j].lines[k].n_interactions ; l++) {
									if (history[i][i2][j].lines[k].interactions[l].interaction_number <= interaction)
										zval += history[i][i2][j].lines[k].interactions[l].counts;
								}
							}
						}
					}
					else {
						for (j = 0 ; j < nhistory[i][i2] ; j++) {
							for (k = 0 ; k < history[i][i2][j].n_lines ; k++) {
								for (l = 0 ; l < history[i][i2][j].lines[k].n_interactions ; l++) {
									if (history[i][i2][j].lines[k].interactions[l].interaction_number == interaction)
										zval += history[i][i2][j].lines[k].interactions[l].counts;
								}
							}
						}
					}
				}
				else {
					//specific element selected
					if (line_type == NULL) {
						//XRF line = All
						for (j = 0 ; j < nhistory[i][i2] ; j++) {
							if (history[i][i2][j].atomic_number == ((struct fluor_data *) g_ptr_array_index(self->fd, atomic_number_index))->atomic_number) {
								for (k = 0 ; k < history[i][i2][j].n_lines ; k++) {
									for (l = 0 ; l < history[i][i2][j].lines[k].n_interactions ; l++) {
										if ((!cumulative && history[i][i2][j].lines[k].interactions[l].interaction_number == interaction) || (cumulative && history[i][i2][j].lines[k].interactions[l].interaction_number <= interaction))
											zval += history[i][i2][j].lines[k].interactions[l].counts;
									}
								}
							}
						}
					}
					else {
						//XRF line = specific
						for (j = 0 ; j < nhistory[i][i2] ; j++) {
							if (history[i][i2][j].atomic_number == ((struct fluor_data *) g_ptr_array_index(self->fd, atomic_number_index))->atomic_number) {
								for (k = 0 ; k < history[i][i2][j].n_lines ; k++) {
									if (strcmp(history[i][i2][j].lines[k].line_type, line_type) == 0) {
										for (l = 0 ; l < history[i][i2][j].lines[k].n_interactions ; l++) {
											if ((!cumulative && history[i][i2][j].lines[k].interactions[l].interaction_number == interaction) || (cumulative && history[i][i2][j].lines[k].interactions[l].interaction_number <= interaction))
												zval += history[i][i2][j].lines[k].interactions[l].counts;
										}
									}
								}
							}
						}
					}
				}
				if (zval > 0.0 && zval < minval)
					minval = zval;
				z[i*(self->archive->nsteps2+1)+i2] = zval;
			}
			g_free(nhistory[i]);
			g_free(history[i]);
		}
		g_free(nhistory);
		g_free(history);
	}


	if ((gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_radioW)) && gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->roi_log10W))) || (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_radioW)) && gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(self->xrf_log10W)))) {
		//log10 selected
		for (i = 0 ; i < (self->archive->nsteps1+1)*(self->archive->nsteps2+1) ; i++) {
			if (z[i] < minval)
				z[i] = minval;
		}
	}

	try {
		self->canvas->remove_plot(0);
	}
	catch (Gtk::PLplot::Exception &e) {
		//this will fail if there's no plot available
	}

	//create first the dataset
	Gtk::PLplot::PlotDataSurface *dataset = Gtk::manage(new Gtk::PLplot::PlotDataSurface(
          std::vector<double>(xx, xx + self->archive->nsteps1+1),
          std::vector<double>(y, y + self->archive->nsteps2+1),
          zz
        ));

	Gtk::PLplot::PlotContourShades *plot_window = Gtk::manage(new Gtk::PLplot::PlotContourShades(*dataset,
	  gtk_entry_get_text(GTK_ENTRY(self->xaxis_titleW)),
	  gtk_entry_get_text(GTK_ENTRY(self->yaxis_titleW)),
	  "",
          8,
	  Gtk::PLplot::ColormapPalette::BLUE_RED));

	plot_window->set_colorbar_title("Intensity");

	self->plot_window = plot_window;
	self->canvas->add_plot(*plot_window);

	g_free(zz);
	g_free(xx);
}
