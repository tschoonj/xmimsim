/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-tools.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-prefs.h"
#include "xmi_xslt.h"
#include "xmi_xml.h"
#include "xmi_xrmc.h"
#include "xmi_main.h"
#include <string.h>

struct xmi_tools {
	GtkWidget *window;
	GtkWidget *entry;
	GtkWidget *entry1;
	GtkWidget *entry2;
	GtkWidget *entry3;
	GtkWidget *button1;
	GtkWidget *button2;
	GtkWidget *apply;
	GtkWidget *spinner;
	GtkWidget *xmsi_fileW;
	GtkWidget *xrmc_folderW;
	GtkWidget *enable_pileupW;
	GtkWidget *enable_poissonW;
	GtkWidget *spinner1;
	GtkWidget *spinner2;
	GtkWidget *label1;
	GtkWidget *label2;
};


struct xmsa_to_xmso_data {
	gchar *xmsafile;
	gchar *xmsofile;
	int step1;
	int step2;
};

static GtkWidget *get_window(const gchar *title, GtkWidget *main_window) {
	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), title);
	gtk_window_set_default_size(GTK_WINDOW(window), 500, 300);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	gtk_container_set_border_width(GTK_CONTAINER(window), 3);

	return window;
}

static GtkWidget* get_description_frame(const gchar *description) {
	GtkWidget *frame = gtk_frame_new(NULL);
	GtkWidget *label = gtk_label_new(description);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label),GTK_JUSTIFY_CENTER);
	gtk_container_add(GTK_CONTAINER(frame), label);
	gtk_widget_set_halign(label, GTK_ALIGN_FILL);
	gtk_widget_set_valign(label, GTK_ALIGN_FILL);
	gtk_widget_set_halign(frame, GTK_ALIGN_FILL);
	gtk_widget_set_valign(frame, GTK_ALIGN_FILL);
	gtk_widget_set_hexpand(label, TRUE);
	gtk_widget_set_vexpand(label, TRUE);
	gtk_widget_set_hexpand(frame, TRUE);
	gtk_widget_set_vexpand(frame, TRUE);
	
	return frame;
}

static gpointer xmsa_to_xmso_thread(struct xmsa_to_xmso_data *xtxd) {
	return GINT_TO_POINTER(xmi_xmsa_to_xmso_xslt(xtxd->xmsafile, xtxd->xmsofile, xtxd->step1, xtxd->step2));
}
static void xmso_open_button_clicked_cb(GtkButton *button, gpointer data) {

	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	gchar *filename;
	struct xmi_tools *xt = (struct xmi_tools *) data;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmso");
	gtk_file_filter_add_pattern(filter,"*.XMSO");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select an XMI-MSIM outputfile",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"_Open",
		"_Cancel"
	);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER (dialog));
		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);
		g_free(filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
}
static void xmso_full_open_button_clicked_cb(GtkButton *button, gpointer data) {

	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	gchar *filename;
	struct xmi_tools *xt = (struct xmi_tools *) data;
	struct xmi_output *output;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmso");
	gtk_file_filter_add_pattern(filter,"*.XMSO");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select an XMI-MSIM outputfile",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"_Open",
		"_Cancel"
	);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
		//read the file
		GError *error = NULL;
		if (xmi_read_output_xml(filename, &output, &error) == 0) {
			GtkWidget *message_dialog = gtk_message_dialog_new(
				GTK_WINDOW(xt->window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
	       			GTK_MESSAGE_ERROR,
	       			GTK_BUTTONS_CLOSE,
	       			"An error occured while processing %s", filename
                		);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
			g_error_free(error);
     			gtk_dialog_run(GTK_DIALOG(message_dialog));
			gtk_widget_destroy(message_dialog);
			return ;
		}
		//set the spinner
		gtk_widget_set_sensitive(xt->spinner, TRUE);
		GtkAdjustment *adj = GTK_ADJUSTMENT(gtk_adjustment_new(output->ninteractions, output->use_zero_interactions ? 0 : 1, output->ninteractions, 1 , 1, 0));
		gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(xt->spinner), GTK_ADJUSTMENT(adj));
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(xt->spinner),output->ninteractions);
		//free everything
		xmi_free_output(output);

		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);
		g_free(filename);
	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
	}
}
static void xmsa_full_open_button_clicked_cb(GtkButton *button, gpointer data) {

	XmiMsimGuiFileChooserDialog *dialog;
	GtkFileFilter *filter;
	gchar *filename;
	struct xmi_tools *xt = (struct xmi_tools *) data;
	struct xmi_archive *archive;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsa");
	gtk_file_filter_add_pattern(filter,"*.XMSA");
	gtk_file_filter_set_name(filter,"XMI-MSIM archives");

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select an XMI-MSIM archive",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"_Open",
		"_Cancel"
	);

	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
		//read the file
		GtkWidget *job_dialog = xmi_msim_gui_utils_long_job_dialog(xt->window, "<b>Reading XMSA file</b>");
		gtk_widget_show_all(job_dialog);
		GdkCursor* watchCursor = gdk_cursor_new_for_display(gdk_display_get_default(), GDK_WATCH);
		gdk_window_set_cursor(gtk_widget_get_window(job_dialog), watchCursor);

		while(gtk_events_pending())
			gtk_main_iteration();

		struct read_xmsa_data *rxd = (struct read_xmsa_data *) g_malloc(sizeof(struct read_xmsa_data));
		rxd->filename = filename;
		rxd->archive = &archive;
		GThread *xmsa_thread = g_thread_new(NULL, (GThreadFunc) xmi_msim_gui_utils_read_xmsa_thread, (gpointer) rxd);
		while(gtk_events_pending())
			gtk_main_iteration();

		int xmsa_thread_rv = GPOINTER_TO_INT(g_thread_join(xmsa_thread));
		g_free(rxd);
		gdk_window_set_cursor(gtk_widget_get_window(job_dialog), NULL);
		if (!xmsa_thread_rv) {
			gtk_widget_destroy(job_dialog);
			GtkWidget *message_dialog = gtk_message_dialog_new(
				GTK_WINDOW(xt->window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not read file %s", filename
			);
	    		gtk_dialog_run(GTK_DIALOG(message_dialog));
			gtk_widget_destroy(message_dialog);
			return ;
		}

		//set the spinner
		gtk_widget_set_sensitive(xt->spinner1, TRUE);
		GtkAdjustment *adj = GTK_ADJUSTMENT(gtk_adjustment_new(0, 0, archive->nsteps1, 1 , 1, 0));
		gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(xt->spinner1), GTK_ADJUSTMENT(adj));
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(xt->spinner1), 0);
		gchar *label_text = g_strdup_printf("XPath1: %s", archive->xpath1);
		gtk_label_set_text(GTK_LABEL(xt->label1), label_text);
		g_free(label_text);

		if (archive->xpath2) {
			gtk_widget_set_sensitive(xt->spinner2, TRUE);
			adj = GTK_ADJUSTMENT(gtk_adjustment_new(0, 0, archive->nsteps2, 1 , 1, 0));
			gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(xt->spinner2), GTK_ADJUSTMENT(adj));
			gtk_spin_button_set_value(GTK_SPIN_BUTTON(xt->spinner2), 0);
			label_text = g_strdup_printf("XPath2: %s", archive->xpath2);
			gtk_label_set_text(GTK_LABEL(xt->label2), label_text);
			g_free(label_text);
		}
		else {
			gtk_widget_set_sensitive(xt->spinner2, FALSE);
			gtk_label_set_text(GTK_LABEL(xt->label2), "XPath2: not found");
		}
		gtk_widget_set_sensitive(xt->button1, TRUE);
		gtk_widget_set_sensitive(xt->button2, TRUE);

		//free everything
		xmi_free_archive(archive);
		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);

		g_free(filename);
	}
	else {
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
	}
}

#define SAVE_BUTTON_CLICKED_CALLBACK(prefix, filter1, filter2, filter_name, file_type, extension) \
static void prefix ## _save_button_clicked_cb(GtkButton *button, gpointer data) { \
	XmiMsimGuiFileChooserDialog *dialog; \
	GtkFileFilter *filter; \
	gchar *filename; \
	\
	filter = gtk_file_filter_new(); \
	gtk_file_filter_add_pattern(filter, filter1); \
	gtk_file_filter_add_pattern(filter, filter2); \
	gtk_file_filter_set_name(filter, filter_name); \
	struct xmi_tools *xt = (struct xmi_tools *) data; \
	\
	dialog = xmi_msim_gui_file_chooser_dialog_new("Select the name for the new " file_type, \
		GTK_WINDOW(xt->window), \
		GTK_FILE_CHOOSER_ACTION_SAVE, \
		"_Save", \
		"_Cancel" \
	); \
	\
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE); \
	\
	gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE); \
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter); \
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) { \
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog)); \
		xmi_msim_gui_utils_ensure_extension(&filename, extension); \
		\
		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename); \
		g_free (filename); \
	} \
	\
	xmi_msim_gui_file_chooser_dialog_destroy(dialog); \
}

SAVE_BUTTON_CLICKED_CALLBACK(xmsi, "*.xmsi", "*.XMSI", "XMI-MSIM inputfiles", "XMI-MSIM inputfile", ".xmsi")
SAVE_BUTTON_CLICKED_CALLBACK(xmso, "*.xmso", "*.XMSO", "XMI-MSIM outputfiles", "XMI-MSIM outputfile", ".xmso")
SAVE_BUTTON_CLICKED_CALLBACK(csv, "*.csv", "*.CSV", "Comma separated values files", "CSV file", ".csv")
SAVE_BUTTON_CLICKED_CALLBACK(html, "*.html", "*.HTML", "Hypertext Markup Language files", "HTML file", ".html")
SAVE_BUTTON_CLICKED_CALLBACK(svg, "*.svg", "*.SVG", "Scalable Vector Graphics files", "SVG file", ".svg")
SAVE_BUTTON_CLICKED_CALLBACK(spe, "*.spe", "*.SPE", "SPE files", "SPE file", ".spe")

static void xmsi2xrmc_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	gchar *xmsi_file = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(xt->xmsi_fileW));
	gchar *xrmc_folder = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(xt->xrmc_folderW));

	if (xmsi_file == NULL || xrmc_folder == NULL) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSI file and an XRMC folder must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	struct xmi_main_options options;
	options.use_M_lines = 1;
	options.use_cascade_auger = 1;
	options.use_cascade_radiative = 1;
	options.use_variance_reduction = 1;
	options.use_sum_peaks = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->enable_pileupW)) == TRUE ? 1 : 0;
	options.use_poisson = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->enable_poissonW)) == TRUE ? 1 : 0;
	options.use_escape_peaks = 1;
	options.verbose = 0;
	options.use_opencl = 0;
	options.extra_verbose = 0;

	gchar *input_file = NULL;
	gchar *source_file = NULL;
	gchar *sample_file = NULL;
	gchar *composition_file = NULL;
	gchar *spectrum_file = NULL;
	gchar *geom3d_file = NULL;
	gchar *quadric_file = NULL;
	gchar *detector_file = NULL;
	gchar *convoluted_file = NULL;

	input_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "input.dat");
	source_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "source.dat");
	sample_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "sample.dat");
	composition_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "composition.dat");
	spectrum_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "spectrum.dat");
	geom3d_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "geom3d.dat");
	quadric_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "quadric.dat");
	detector_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "detector.dat");
	convoluted_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "%s", xrmc_folder, "convoluted_spectra.dat");
	//
	//read in the inputfile
	struct xmi_input *input;
	GError *error = NULL;
	int rv = xmi_read_input_xml(xmsi_file, &input, &error);

	if (rv != 1) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"Could not read in XMSI file %s\nAborting...",
                	xmsi_file);
		gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
		g_error_free(error);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}
	if (xmi_copy_input_to_xrmc(input,
				input_file,
				composition_file,
				detector_file,
				geom3d_file,
				quadric_file,
				sample_file,
				source_file,
				spectrum_file,
				convoluted_file,
				NULL,
				/*struct xmi_layer *collimator*/ NULL,
				options,
				0.0
				) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"Could not convert %s to XRMC files in %s\nAborting...",
                	xmsi_file, xrmc_folder);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}
	dialog = gtk_message_dialog_new_with_markup(GTK_WINDOW(xt->window),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_INFO,
		GTK_BUTTONS_CLOSE,
		"XMI-MSIM input-file %s was successfully converted to XRMC input-files in %s.\n\nLaunch the simulation using the command:",
		xmsi_file, xrmc_folder);
	GtkWidget *label = gtk_label_new(NULL);
	gchar *label_text = g_strdup_printf("<i>xrmc %s</i>", input_file);
	gtk_label_set_markup(GTK_LABEL(label), label_text);
	g_free(label_text);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_box_pack_start(GTK_BOX(gtk_message_dialog_get_message_area(GTK_MESSAGE_DIALOG(dialog))), label, TRUE, FALSE, 2);
	gtk_widget_show(label);
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy(dialog);

	xmi_free_input(input);
	g_free(input_file);
	g_free(composition_file);
	g_free(detector_file);
	g_free(geom3d_file);
	g_free(quadric_file);
	g_free(sample_file);
	g_free(source_file);
	g_free(spectrum_file);
	g_free(convoluted_file);
}

static void xmso2xmsi_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	//first check if the two first entries are filled
	if (strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry1))) == 0 || strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry2))) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSO file and an XMSI file must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	gchar *xmsofile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	gchar *xmsifile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	gchar *outputfile= (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry3));

	if (strcmp(outputfile,"(optional)") == 0)
		outputfile = NULL;


	gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), FALSE);

	if (!xmi_xmso_to_xmsi_xslt(xmsofile, xmsifile, outputfile)) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
	else {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_INFO,
	       		GTK_BUTTONS_CLOSE,
	       		"The conversion was successfully performed."
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
}

static void xmso2csv_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	//first check if the two first entries are filled
	if (strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry1))) == 0 || strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry2))) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSO file and a CSV file must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	gchar *xmsofile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	gchar *csvfile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	unsigned convoluted;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button1)))
		convoluted = 1;
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button2)))
		convoluted = 0;
	else {
		fprintf(stderr,"Neither button is active. Should not occur\n");
		convoluted = 1;
	}



	gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), FALSE);

	if (!xmi_xmso_to_csv_xslt(xmsofile, csvfile, convoluted)) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
	else {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_INFO,
	       		GTK_BUTTONS_CLOSE,
	       		"The conversion was successfully performed."
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
}

static void xmso2html_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	//first check if the two first entries are filled
	if (strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry1))) == 0 || strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry2))) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSO file and an HTML file must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	gchar *xmsofile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	gchar *htmlfile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	unsigned convoluted;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button1)))
		convoluted = 1;
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button2)))
		convoluted = 0;
	else {
		fprintf(stderr,"Neither button is active. Should not occur\n");
		convoluted = 1;
	}



	gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), FALSE);

	if (!xmi_xmso_to_htm_xslt(xmsofile, htmlfile, convoluted)) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
	else {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_INFO,
	       		GTK_BUTTONS_CLOSE,
	       		"The conversion was successfully performed."
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
}

static void xmso2svg_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	//first check if the two first entries are filled
	if (strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry1))) == 0 || strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry2))) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSO file and an SVG file must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	gchar *xmsofile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	gchar *svgfile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	unsigned convoluted;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button1)))
		convoluted = 1;
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button2)))
		convoluted = 0;
	else {
		fprintf(stderr,"Neither button is active. Should not occur\n");
		convoluted = 1;
	}



	gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), FALSE);

	if (!xmi_xmso_to_svg_xslt(xmsofile, svgfile, convoluted)) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
	else {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_INFO,
	       		GTK_BUTTONS_CLOSE,
	       		"The conversion was successfully performed."
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_destroy(xt->window);
	}
}
static void xmso2spe_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	//first check if the two first entries are filled
	if (strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry1))) == 0 || strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry2))) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSO file and an SPE file must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	gchar *xmsofile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	gchar *spefile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	unsigned convoluted;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button1)))
		convoluted = 1;
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button2)))
		convoluted = 0;
	else {
		fprintf(stderr,"Neither button is active. Should not occur\n");
		convoluted = 1;
	}

	int interaction_number = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(xt->spinner));


	gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), FALSE);

	if (!xmi_xmso_to_spe_xslt(xmsofile, spefile, convoluted, interaction_number)) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), TRUE);
	}
	else {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_INFO,
	       		GTK_BUTTONS_CLOSE,
	       		"The conversion was successfully performed."
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), TRUE);

	}
}
static void xmsa2xmso_apply_button_clicked_cb(GtkButton *button, gpointer data) {
	struct xmi_tools *xt = (struct xmi_tools *) data;
	GtkWidget *dialog;

	//first check if the two first entries are filled
	if (strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry1))) == 0 || strlen(gtk_entry_get_text(GTK_ENTRY(xt->entry2))) == 0) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An XMSA file and an XMSO file must be selected for the conversion to be performed"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		return ;
	}

	gchar *xmsafile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	gchar *xmsofile = (char *) gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	int step1 = -1, step2 = -1;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button1))) {
		step1 = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(xt->spinner1));

		if (strcmp(gtk_label_get_text(GTK_LABEL(xt->label2)), "XPath2: not found") != 0) {
			step2 = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(xt->spinner2));
		}
		else {
			step2 = 0;
		}
	}
	else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button2))) {
		//convert all
		//default values to be used
		xmsofile = g_strdup(xmsofile);
		gchar *dotPtr = strrchr(xmsofile, '.');
		*dotPtr = '\0';
	}
	else {
		fprintf(stderr,"Neither button is active. Should not occur\n");
	}



	gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), FALSE);

	dialog = xmi_msim_gui_utils_long_job_dialog(xt->window, "<b>Converting XMSA file</b>");
	gtk_widget_show_all(dialog);
	GdkCursor* watchCursor = gdk_cursor_new_for_display(gdk_display_get_default(), GDK_WATCH);
	gdk_window_set_cursor(gtk_widget_get_window(dialog), watchCursor);

	while(gtk_events_pending())
		gtk_main_iteration();

	struct xmsa_to_xmso_data *xtxd = (struct xmsa_to_xmso_data *) g_malloc(sizeof(struct xmsa_to_xmso_data));
	xtxd->xmsafile = xmsafile;
	xtxd->xmsofile = xmsofile;
	xtxd->step1 = step1;
	xtxd->step2 = step2;

	GThread *xmsa_thread = g_thread_new(NULL, (GThreadFunc) xmsa_to_xmso_thread, (gpointer) xtxd);

	while(gtk_events_pending())
		gtk_main_iteration();

	int xmsa_thread_rv = GPOINTER_TO_INT(g_thread_join(xmsa_thread));
	g_free(xtxd);
	gdk_window_set_cursor(gtk_widget_get_window(dialog), NULL);
	gtk_widget_destroy(dialog);

	if (!xmsa_thread_rv) {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), TRUE);
	}
	else {
		dialog = gtk_message_dialog_new (GTK_WINDOW(xt->window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_INFO,
	       		GTK_BUTTONS_CLOSE,
	       		"The conversion was successfully performed."
                	);
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_set_sensitive(GTK_WIDGET(xt->apply), TRUE);

	}


	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(xt->button2))) {
		g_free(xmsofile);
	}
}
void xmimsim_gui_xmso2xmsi(GtkMenuItem *menuitem, gpointer data) {

	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt3, *xt4;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt3 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	grid = gtk_grid_new(); // 6 rows ,3 cols
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSO to XMSI", main_window);
	xt4->window = window;

	GtkWidget *frame = get_description_frame("This tool allows for the extraction of an XMI-MSIM inputfile (XMSI) from an XMI-MSIM outputfile (XMSO). It is possible to change the name of the outputfile in the extracted XMSI file");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 3, 1);

	label = gtk_label_new("XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 1, 1, 1);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 1, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("XMSI file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 2, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 2, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmsi_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	label = gtk_label_new("New XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 3, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_entry_set_text(GTK_ENTRY(text), "(optional)");
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 3, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 3, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_save_button_clicked_cb), xt3);
	xt3->window = window;
	xt3->entry = text;
	xt4->entry3 = text;

	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 4, 3, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2xmsi_apply_button_clicked_cb), xt4);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 5, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}

void xmimsim_gui_xmso2csv(GtkMenuItem *menuitem, gpointer data) {

	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt4;
	GtkWidget *button1, *button2;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	grid = gtk_grid_new(); // 7 rows, 3 cols
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSO to CSV", main_window);
	xt4->window = window;

	GtkWidget *frame = get_description_frame("This tool allows for the extraction of the generated spectra as comma separated value files (CSV) from XMI-MSIM outputfiles. Both the convoluted and unconvoluted data are available for extraction");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 3, 1);

	label = gtk_label_new("XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 1, 1, 1);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 1, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("CSV file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 2, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 2, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(csv_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	//checkbutton here
	button1 = gtk_radio_button_new_with_label_from_widget(NULL, "Use spectra after detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button1, 0, 3, 3, 1);
	xt4->button1 = button1;
	button2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(button1), "Use spectra before detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button2, 0, 4, 3, 1);
	xt4->button2 = button2;
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button1), TRUE);

	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 5, 3, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2csv_apply_button_clicked_cb), xt4);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 6, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}

void xmimsim_gui_xmso2html(GtkMenuItem *menuitem, gpointer data) {

	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt4;
	GtkWidget *button1, *button2;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	grid = gtk_grid_new(); // 7 rows, 3 cols
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSO to HTML", main_window);
	xt4->window = window;

	GtkWidget *frame = get_description_frame("This tool allows for the generation of an interactive report of the generated spectra and line intensities as an HTML file, which should be viewable with most browsers. Both the convoluted and unconvoluted data are available for extraction");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 3, 1);

	label = gtk_label_new("XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 1, 1, 1);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 1, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("HTML file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 2, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 2, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(html_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	//checkbutton here
	button1 = gtk_radio_button_new_with_label_from_widget(NULL, "Use spectra after detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button1, 0, 3, 3, 1);
	xt4->button1 = button1;
	button2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(button1), "Use spectra before detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button2, 0, 4, 3, 1);
	xt4->button2 = button2;
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button1), TRUE);

	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 5, 3, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2html_apply_button_clicked_cb), xt4);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 6, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}

void xmimsim_gui_xmso2svg(GtkMenuItem *menuitem, gpointer data) {

	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt4;
	GtkWidget *button1, *button2;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	grid = gtk_grid_new(); // 7 rows, 3 cols
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSO to SVG", main_window);
	xt4->window = window;

	GtkWidget *frame = get_description_frame("This tool allows for the extraction of the generated spectra as scalable vector graphic files (SVG). Both the convoluted and unconvoluted data are available for extraction");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 3, 1);

	label = gtk_label_new("XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 1, 1, 1);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 1, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("SVG file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 2, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 2, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(svg_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	//checkbutton here
	button1 = gtk_radio_button_new_with_label_from_widget(NULL, "Use spectra after detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button1, 0, 3, 3, 1);
	xt4->button1 = button1;
	button2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(button1), "Use spectra before detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button2, 0, 4, 3, 1);
	xt4->button2 = button2;
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button1), TRUE);

	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 5, 3, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2svg_apply_button_clicked_cb), xt4);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 6, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}

void xmimsim_gui_xmso2spe(GtkMenuItem *menuitem, gpointer data) {
	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt4;
	GtkWidget *button1, *button2;
	GtkWidget *spinner;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	grid = gtk_grid_new();
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSO to SPE", main_window);
	xt4->window = window;

	GtkWidget *frame = get_description_frame("This tool allows for the extraction of the generated spectra as SPE files. Both the convoluted and unconvoluted data are available for extraction. Since the XMSO files contain spectra generated for increasing number of interactions, the user is required to indicate the desired spectrum.");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 3, 1);

	label = gtk_label_new("XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 1, 1, 1);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 1, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_full_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("SPE file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 2, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_END);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 2, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(spe_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	//checkbutton here
	button1 = gtk_radio_button_new_with_label_from_widget(NULL, "Use spectra after detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button1, 0, 3, 3, 1);
	xt4->button1 = button1;
	button2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(button1), "Use spectra before detector convolution");
	gtk_grid_attach(GTK_GRID(grid), button2, 0, 4, 3, 1);
	xt4->button2 = button2;
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button1), TRUE);

	//spinner
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	label = gtk_label_new("Number of interactions");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 2);
	spinner = gtk_spin_button_new(NULL, 1, 0);
	gtk_box_pack_start(GTK_BOX(hbox), spinner, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(spinner, FALSE);
	xt1->spinner = spinner;
	xt4->spinner = spinner;
	gtk_grid_attach(GTK_GRID(grid), hbox, 0, 5, 3, 1);

	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 6, 3, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2spe_apply_button_clicked_cb), xt4);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 7, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}

void xmimsim_gui_xmsi2xrmc(GtkMenuItem *menuitem, gpointer data) {
	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *label;
	GtkWidget *xmsi_fileW, *xrmc_folderW;
	GtkWidget *enable_pileupW, *enable_poissonW;
	union xmimsim_prefs_val xpv;

	grid = gtk_grid_new(); // 7 rows, 2 cols
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSI to XRMC inputfiles", main_window);

	GtkWidget *frame = get_description_frame("Convert XMI-MSIM input-files to the corresponding XRMC input-files. Running these new files requires XRMC's XMI-MSIM plug-in. If a collimator was defined in XMI-MSIM, then an equivalent object will be present in the XRMC input-files too. Its default composition will be Pb: change this in the XRMC input-files if necessary.");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 2, 1);

	label = gtk_label_new("XMSI file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	xmsi_fileW = gtk_file_chooser_button_new("Select an XMI-MSIM input-file", GTK_FILE_CHOOSER_ACTION_OPEN);
	gtk_widget_set_halign(xmsi_fileW, GTK_ALIGN_FILL);
	gtk_widget_set_valign(xmsi_fileW, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(xmsi_fileW, TRUE);
	gtk_grid_attach(GTK_GRID(grid), xmsi_fileW, 1, 1, 1, 1);
	GtkFileFilter *filter;
	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.[xX][mM][sS][iI]");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(xmsi_fileW), filter);

	label = gtk_label_new("XRMC folder");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	xrmc_folderW = gtk_file_chooser_button_new("Select a folder in which the XRMC files will be stored", GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_widget_set_halign(xrmc_folderW, GTK_ALIGN_FILL);
	gtk_widget_set_valign(xrmc_folderW, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(xrmc_folderW, TRUE);
	gtk_grid_attach(GTK_GRID(grid), xrmc_folderW, 1, 2, 1, 1);

	enable_pileupW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(enable_pileupW),xpv.b);
	gtk_grid_attach(GTK_GRID(grid), enable_pileupW, 0, 3, 2, 1);

	enable_poissonW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort
		preferences_error_handler(window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(enable_poissonW),xpv.b);
	gtk_grid_attach(GTK_GRID(grid), enable_poissonW, 0, 4, 2, 1);


	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 5, 2, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	struct xmi_tools *xi = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xi->window = window;
	xi->xmsi_fileW = xmsi_fileW;
	xi->xrmc_folderW = xrmc_folderW;
	xi->enable_pileupW = enable_pileupW;
	xi->enable_poissonW = enable_poissonW;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmsi2xrmc_apply_button_clicked_cb), xi);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 6, 2, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}

void xmimsim_gui_xmsa2xmso(GtkMenuItem *menuitem, gpointer data) {
	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *grid;
	GtkWidget *button;
	GtkWidget *button1, *button2;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt4;
	GtkWidget *xpath1_spinner;
	GtkWidget *xpath2_spinner;
	GtkWidget *xpath1_label;
	GtkWidget *xpath2_label;
	GtkWidget *xpath_vbox;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	grid = gtk_grid_new(); // 8 rows, 3 cols
	gtk_grid_set_row_spacing(GTK_GRID(grid), 3);
	gtk_grid_set_column_spacing(GTK_GRID(grid), 3);
	window = get_window("Convert XMSA to XMSO", main_window);
	xt4->window = window;

	GtkWidget *frame = get_description_frame("This tool allows for the extraction of XMI-MSIM output-files (XMSO) from XMI-MSIM archives (XMSA). One can either extract all of them or select individuals by selecting the step(s) corresponding to the XPath parameter(s).");
	gtk_grid_attach(GTK_GRID(grid), frame, 0, 0, 3, 1);

	label = gtk_label_new("XMSA file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 1, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 1, 1, 1);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_FILL);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 1, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmsa_full_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("XMSO file");
	gtk_widget_set_halign(label, GTK_ALIGN_START);
	gtk_widget_set_valign(label, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), label, 0, 2, 1, 1);
	text = gtk_entry_new();
	gtk_widget_set_halign(text, GTK_ALIGN_FILL);
	gtk_widget_set_valign(text, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(text, TRUE);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_grid_attach(GTK_GRID(grid), text, 1, 2, 1, 1);
	button = gtk_button_new_from_icon_name("document-save", GTK_ICON_SIZE_BUTTON);
	gtk_widget_set_halign(button, GTK_ALIGN_FILL);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	gtk_grid_attach(GTK_GRID(grid), button, 2, 2, 1, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	//togglebuttons here
	xpath_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(xpath_vbox), TRUE);
	xpath1_label = gtk_label_new("XPath1:");
	gtk_widget_set_halign(xpath1_label, GTK_ALIGN_START);
	gtk_widget_set_valign(xpath1_label, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(xpath1_label, TRUE);
	xpath2_label = gtk_label_new("XPath2:");
	gtk_widget_set_halign(xpath2_label, GTK_ALIGN_START);
	gtk_widget_set_valign(xpath2_label, GTK_ALIGN_CENTER);
	gtk_widget_set_hexpand(xpath2_label, TRUE);
	gtk_box_pack_start(GTK_BOX(xpath_vbox), xpath1_label, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(xpath_vbox), xpath2_label, TRUE, FALSE, 0);
	gtk_widget_set_halign(xpath_vbox, GTK_ALIGN_FILL);
	gtk_widget_set_valign(xpath_vbox, GTK_ALIGN_FILL);
	gtk_widget_set_hexpand(xpath_vbox, TRUE);
	button1 = gtk_radio_button_new_from_widget(NULL);
	gtk_container_add(GTK_CONTAINER(button1), xpath_vbox);
	gtk_grid_attach(GTK_GRID(grid), button1, 0, 3, 2, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button1), TRUE);
	xt1->label1 = xpath1_label;
	xt1->label2 = xpath2_label;
	xt4->label1 = xpath1_label;
	xt4->label2 = xpath2_label;

	button2 = gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(button1));
	label = gtk_label_new("Convert all");
	gtk_container_add(GTK_CONTAINER(button2), label);
	gtk_grid_attach(GTK_GRID(grid), button2, 0, 5, 2, 1);
	gtk_widget_set_sensitive(button1, FALSE);
	gtk_widget_set_sensitive(button2, FALSE);
	xt1->button1 = button1;
	xt1->button2 = button2;
	xt4->button1 = button1;
	xt4->button2 = button2;

	//spinners
	xpath1_spinner = gtk_spin_button_new(NULL, 1, 0);
	gtk_widget_set_sensitive(xpath1_spinner, FALSE);
	gtk_grid_attach(GTK_GRID(grid), xpath1_spinner, 2, 3, 1, 1);
	xpath2_spinner = gtk_spin_button_new(NULL, 1, 0);
	gtk_widget_set_sensitive(xpath2_spinner, FALSE);
	gtk_grid_attach(GTK_GRID(grid), xpath2_spinner, 2, 4, 1, 1);
	xt1->spinner1 = xpath1_spinner;
	xt1->spinner2 = xpath2_spinner;
	xt4->spinner1 = xpath1_spinner;
	xt4->spinner2 = xpath2_spinner;

	gtk_grid_attach(GTK_GRID(grid), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), 0, 6, 3, 1);

	button = gtk_button_new_with_mnemonic("_Apply");
	gtk_widget_set_halign(button, GTK_ALIGN_CENTER);
	gtk_widget_set_valign(button, GTK_ALIGN_CENTER);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmsa2xmso_apply_button_clicked_cb), xt4);
	gtk_grid_attach(GTK_GRID(grid), button, 0, 7, 3, 1);

	gtk_container_add(GTK_CONTAINER(window), grid);

	gtk_widget_show_all(window);
}
