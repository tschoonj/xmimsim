/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui-batch.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-controls.h"
#include "xmimsim-gui-layer.h"
#include "xmi_main.h"
#include <stdio.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>
#include <string.h>
#include <xraylib.h>
#include <libxml/xmlmemory.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <libxml/catalog.h>
#include "xmi_lines.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include <gtkextra/gtkextra.h>
#include "xmimsim-gui-results.h"
#include <math.h>

struct canvas_data {
	double width;
	double height;
};

struct options_widget {
	GtkWidget *Mlines_prefsW;
	GtkWidget *rad_cascade_prefsW;
	GtkWidget *nonrad_cascade_prefsW;
	GtkWidget *variance_reduction_prefsW;
	GtkWidget *pile_up_prefsW;
	GtkWidget *poisson_prefsW;
	GtkWidget *nchannels_prefsW;
	GtkWidget *superframe;
};

enum xmi_msim_batch_options{
	XMI_MSIM_BATCH_MULTIPLE_OPTIONS,
	XMI_MSIM_BATCH_ONE_OPTION
};

struct batch_window_data {
	int *rv;
	GtkWidget *batch_window;
	GtkWidget *playButton;
	GtkWidget *stopButton;
#ifdef G_OS_UNIX
	GtkWidget *pauseButton;
#endif
	GtkWidget *nthreadsW;
	GtkWidget *progressbarW;
	GtkWidget *controlsLogW;
	GtkTextBuffer *controlsLogB;
	GtkWidget *saveButton;
	GtkWidget *controlsLogFileW;
	GtkWidget *verboseW;
	GtkWidget *exitButton;
	struct xmi_main_options *options;
	GSList *filenames;
	enum xmi_msim_batch_options batch_options;
	gboolean paused;
	GTimer *timer;
	FILE *logFile;
	int i;
	gchar *argv0;
};

struct archive_options_data {
	double start_value;
	double end_value;
	int nsteps;
	gchar *xmso_output_dir;
	gchar *xmso_prefix;
	gchar *xmsi_input_dir;
	gchar *xmsi_prefix;
	gchar *xmsa_file;
	gboolean with_xmsa;
	gboolean with_plot;
	gboolean keep_xmsi;
	gboolean keep_xmso;
};

struct saveButton_clicked_data {
	GtkWidget *entry;
	gchar *title;
	GtkWidget *wizard;
};


struct fluor_data {
	int atomic_number;
	int n_lines;
	gchar **line_types;
};

static int compare_fluor_data(const void *f1, const void *f2) {
	struct fluor_data *ff1 = (struct fluor_data *) f1;
	struct fluor_data *ff2 = (struct fluor_data *) f2;

	return ff1->atomic_number-ff2->atomic_number;
}
static int compare_string(const void *a, const void *b)
{
   return strcmp(*(char **)a, *(char **)b);
}

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct batch_window_data *bwd);
static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd);
static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd);
static struct options_widget *create_options_frame(GtkWidget *main_window);
static void get_fluor_data(struct xmi_archive *archive, struct fluor_data **fdo, int *nfdo);


struct wizard_range_data {
	GtkWidget *wizard;
	GtkWidget *inputdirEntry;
	GtkWidget *inputprefixEntry;
	GtkWidget *outputdirEntry;
	GtkWidget *outputprefixEntry;
	GtkWidget *startEntry;
	GtkWidget *endEntry;
	GtkWidget *nstepsEntry;
	int allowed;
};

struct wizard_plot_data {
	GtkWidget *archive_plotW;
	GtkWidget *archive_modeW;
	GtkWidget *labela;
	GtkWidget *archiveEntry;
	GtkWidget *archivesaveButton;
	GtkWidget *keep_xmsiW;
	GtkWidget *keep_xmsoW;
};

struct wizard_archive_close_data {
	struct wizard_range_data *wrd;
	struct wizard_plot_data *wpd;
	struct options_widget *ow;
	struct archive_options_data *aod;
	struct xmi_main_options *xmo;
	int *rv;
};

static void wizard_archive_cancel(GtkAssistant *wizard, struct wizard_archive_close_data *wacd) {
	*(wacd->rv) = 0;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static void wizard_archive_close(GtkAssistant *wizard, struct wizard_archive_close_data *wacd) {

	//first read the general options
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->ow->Mlines_prefsW)))
		wacd->xmo->use_M_lines = 1;
	else
		wacd->xmo->use_M_lines = 0;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->ow->rad_cascade_prefsW)))
		wacd->xmo->use_cascade_radiative = 1;
	else
		wacd->xmo->use_cascade_radiative = 0;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->ow->nonrad_cascade_prefsW)))
		wacd->xmo->use_cascade_auger = 1;
	else
		wacd->xmo->use_cascade_auger = 0;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->ow->variance_reduction_prefsW)))
		wacd->xmo->use_variance_reduction = 1;
	else
		wacd->xmo->use_variance_reduction = 0;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->ow->pile_up_prefsW)))
		wacd->xmo->use_sum_peaks = 1;
	else
		wacd->xmo->use_sum_peaks = 0;

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->ow->poisson_prefsW)))
		wacd->xmo->use_poisson = 1;
	else
		wacd->xmo->use_poisson = 0;

	wacd->xmo->nchannels = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(wacd->ow->nchannels_prefsW));

	//range parameters
	gchar *buffer = (gchar *) gtk_entry_get_text(GTK_ENTRY(wacd->wrd->inputdirEntry));
	wacd->aod->xmsi_input_dir = g_strdup(buffer);
	buffer = (gchar *) gtk_entry_get_text(GTK_ENTRY(wacd->wrd->outputdirEntry));
	wacd->aod->xmso_output_dir = g_strdup(buffer);
	buffer = (gchar *) gtk_entry_get_text(GTK_ENTRY(wacd->wrd->inputprefixEntry));
	wacd->aod->xmsi_prefix = g_strdup(buffer);
	buffer = (gchar *) gtk_entry_get_text(GTK_ENTRY(wacd->wrd->outputprefixEntry));
	wacd->aod->xmso_prefix = g_strdup(buffer);
	wacd->aod->start_value = strtod(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->startEntry)), NULL);
	wacd->aod->end_value = strtod(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->endEntry)), NULL);
	wacd->aod->nsteps = (int) strtol(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->nstepsEntry)), NULL, 10);

	//plot parameters
	//wacd->wpd	
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->wpd->archive_modeW))) {
		wacd->aod->with_xmsa = TRUE;
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->wpd->archive_plotW))) {
			wacd->aod->with_plot = TRUE;
		}
		else {
			wacd->aod->with_plot = FALSE;
		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->wpd->keep_xmsiW))) {
			wacd->aod->keep_xmsi = TRUE;
		}
		else {
			wacd->aod->keep_xmsi = FALSE;
		}
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wacd->wpd->keep_xmsoW))) {
			wacd->aod->keep_xmso = TRUE;
		}
		else {
			wacd->aod->keep_xmso = FALSE;
		}
		wacd->aod->xmsa_file = g_strdup(gtk_entry_get_text(GTK_ENTRY(wacd->wpd->archiveEntry)));
	}
	else {
		wacd->aod->with_xmsa = FALSE;
		wacd->aod->with_plot = FALSE;
		wacd->aod->keep_xmsi = TRUE;
		wacd->aod->keep_xmso = TRUE;
	}

	*(wacd->rv) = 1;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static gboolean wizard_archive_delete_event(GtkWidget *wizard, GdkEvent *event, struct wizard_archive_close_data *wacd) {
	*(wacd->rv) = 0;
	return FALSE;
}

static void archivesaveButton_clicked_cb(GtkButton *saveButton, GtkEntry *archiveEntry) {
	GtkWidget *dialog  = gtk_file_chooser_dialog_new("Select the filename of the XMSO file", GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(saveButton))), GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsa");
	gtk_file_filter_set_name(filter,"XMI-MSIM archive files");
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gchar *filename;

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(archiveEntry, filename);
		g_free (filename);							
	}
	gtk_widget_destroy(dialog);
	return;
}


static void saveButton_clicked_cb(GtkButton *saveButton, struct saveButton_clicked_data *scd) {
	GtkWidget *dialog  = gtk_file_chooser_dialog_new(scd->title, GTK_WINDOW(scd->wizard), GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL);
	gchar *filename;

	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(scd->entry), filename);
		g_free (filename);							
	}
	gtk_widget_destroy(dialog);
	return;
}


static void wizard_archive_mode_changed_cb(GtkToggleButton *button, struct wizard_plot_data *wpd) {
	if (gtk_toggle_button_get_active(button)) {
		gtk_widget_set_sensitive(wpd->labela, TRUE);
		gtk_widget_set_sensitive(wpd->archiveEntry, TRUE);
		gtk_widget_set_sensitive(wpd->archivesaveButton, TRUE);
		gtk_widget_set_sensitive(wpd->archive_plotW, TRUE);
		gtk_widget_set_sensitive(wpd->keep_xmsiW, TRUE);
		gtk_widget_set_sensitive(wpd->keep_xmsoW, TRUE);
	}
	else {
		gtk_widget_set_sensitive(wpd->labela, FALSE);
		gtk_widget_set_sensitive(wpd->archiveEntry, FALSE);
		gtk_widget_set_sensitive(wpd->archivesaveButton, FALSE);
		gtk_widget_set_sensitive(wpd->archive_plotW, FALSE);
		gtk_widget_set_sensitive(wpd->keep_xmsiW, FALSE);
		gtk_widget_set_sensitive(wpd->keep_xmsoW, FALSE);
	}
	return;
}

static void wizard_range_changed_cb (GtkEditable *entry, struct wizard_range_data *wrd) {
	GtkWidget *vbox = gtk_assistant_get_nth_page(GTK_ASSISTANT(wrd->wizard), gtk_assistant_get_current_page(GTK_ASSISTANT(wrd->wizard)));
	int inputprefix = 0;
	int outputprefix = 0;
	int start_end = 0;
	int nsteps = 0;

	//assuming the dirEntry will always work out
	if (entry == GTK_EDITABLE(wrd->inputprefixEntry) && strlen(gtk_entry_get_text(GTK_ENTRY(wrd->inputprefixEntry))) == 0) {
		gtk_widget_modify_base(wrd->inputprefixEntry,GTK_STATE_NORMAL,&red);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}
	else if (strlen(gtk_entry_get_text(GTK_ENTRY(wrd->inputprefixEntry))) > 0) {
		gtk_widget_modify_base(wrd->inputprefixEntry,GTK_STATE_NORMAL,&white);
		inputprefix = 1;
	}
	if (entry == GTK_EDITABLE(wrd->outputprefixEntry) && strlen(gtk_entry_get_text(GTK_ENTRY(wrd->outputprefixEntry))) == 0) {
		gtk_widget_modify_base(wrd->outputprefixEntry,GTK_STATE_NORMAL,&red);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}
	else if (strlen(gtk_entry_get_text(GTK_ENTRY(wrd->outputprefixEntry))) > 0) {
		gtk_widget_modify_base(wrd->outputprefixEntry,GTK_STATE_NORMAL,&white);
		outputprefix = 1;
	}
	
	char *textPtr,*endPtr,*lastPtr;
	double start, end;
	if (entry == GTK_EDITABLE(wrd->startEntry)) {
		double value;

		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->startEntry));
		value=strtod(textPtr, &endPtr);
		lastPtr = textPtr + strlen(textPtr);
		if (	(strlen(textPtr) == 0 || lastPtr != endPtr) ||
			((wrd->allowed & PARAMETER_STRICT_POSITIVE) && value <= 0.0) ||
			((wrd->allowed & PARAMETER_POSITIVE) && value < 0.0) ||
			((wrd->allowed & PARAMETER_WEIGHT_FRACTION) && (value <= 0.0 || value >= 100.0))
			) {
			gtk_widget_modify_base(wrd->startEntry,GTK_STATE_NORMAL,&red);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		gtk_widget_modify_base(wrd->startEntry,GTK_STATE_NORMAL,&white);
	}
	else if (entry == GTK_EDITABLE(wrd->endEntry)) {
		double value;

		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->endEntry));
		value=strtod(textPtr, &endPtr);
		lastPtr = textPtr + strlen(textPtr);
		if (	(strlen(textPtr) == 0 || lastPtr != endPtr) ||
			((wrd->allowed & PARAMETER_STRICT_POSITIVE) && value <= 0.0) ||
			((wrd->allowed & PARAMETER_POSITIVE) && value <= 0.0) ||
			((wrd->allowed & PARAMETER_WEIGHT_FRACTION) && (value < 0.0 || value > 100.0))
			) {
			gtk_widget_modify_base(wrd->endEntry,GTK_STATE_NORMAL,&red);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		gtk_widget_modify_base(wrd->endEntry,GTK_STATE_NORMAL,&white);
	}
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->nstepsEntry));
	nsteps=strtol(textPtr, &endPtr, 10);
	lastPtr = textPtr + strlen(textPtr);
	if (entry == GTK_EDITABLE(wrd->nstepsEntry) && (strlen(textPtr) == 0 || lastPtr != endPtr|| nsteps < 1)) {
		gtk_widget_modify_base(wrd->nstepsEntry,GTK_STATE_NORMAL,&red);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}
	else if (strlen(textPtr) > 0 && lastPtr == endPtr && nsteps >= 1) {
		gtk_widget_modify_base(wrd->nstepsEntry,GTK_STATE_NORMAL,&white);
	}
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->startEntry));
	start = strtod(textPtr, &endPtr);
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->endEntry));
	end = strtod(textPtr, &endPtr);

	if (end > start) {
		start_end = 1;
		gtk_widget_modify_base(wrd->startEntry,GTK_STATE_NORMAL,&white);
		gtk_widget_modify_base(wrd->endEntry,GTK_STATE_NORMAL,&white);
	}
	else {
		gtk_widget_modify_base(wrd->startEntry,GTK_STATE_NORMAL,&red);
		gtk_widget_modify_base(wrd->endEntry,GTK_STATE_NORMAL,&red);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}

	if (inputprefix*outputprefix*start_end*nsteps > 0) {
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, TRUE);
	}
	else {
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
	}
	return;
}

static int archive_options(GtkWidget *main_window, struct xmi_input *input, struct xmi_main_options *xmo, gchar *filename, gchar *xpath, int allowed, struct archive_options_data *aod) {
	int rv = 0;
	GtkWidget *wizard = gtk_assistant_new();
	gtk_window_set_transient_for(GTK_WINDOW(wizard), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_position (GTK_WINDOW(wizard), GTK_WIN_POS_CENTER);

	gtk_window_set_title(GTK_WINDOW(wizard), "Simulation options");
	//add intro page
	GtkWidget *introLabel = gtk_label_new("Use this wizard to set the simulation options and to set the range of values that the selected parameter will take on. Optionally, you can choose to save the different generated XMSO files into one XMSA file that will be used to produce a graph showing the variation of the intensity of a particular XRF line with respect to the variable parameter.");	
	gtk_label_set_line_wrap(GTK_LABEL(introLabel), TRUE);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), introLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), introLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), introLabel, GTK_ASSISTANT_PAGE_INTRO);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), introLabel, "Introduction");

	//general options
	struct options_widget *ow = create_options_frame(main_window);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), ow->superframe);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), ow->superframe, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), ow->superframe, "General options");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), ow->superframe, TRUE);

	//set range
	struct wizard_range_data *wrd = g_malloc(sizeof(struct wizard_range_data));
	GtkWidget *vbox = gtk_vbox_new(FALSE, 2);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), vbox);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), vbox, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), vbox, "Parameter range and file names");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), vbox, TRUE);
	GtkWidget *hbox, *label;

	gchar *buffer = g_strdup_printf("<b>XPath parameter: %s</b>", xpath);
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), buffer);
	g_free(buffer);
	gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 3);

	hbox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Start");
	GtkWidget *startEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(startEntry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), startEntry, TRUE, TRUE, 1);
	label = gtk_label_new("End");
	GtkWidget *endEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(endEntry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), endEntry, TRUE, TRUE, 1);
	label = gtk_label_new("#Steps");
	GtkWidget *nstepsEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(nstepsEntry), TRUE);
	gtk_entry_set_text(GTK_ENTRY(nstepsEntry), "10");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), nstepsEntry, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 2);
	
	xmlDocPtr doc;
	if ((doc = xmlReadFile(filename,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		g_fprintf(stderr,"xmlReadFile error for %s\n", filename);
		return 0;
	}
	xmlXPathContextPtr context;
	xmlXPathObjectPtr result;

	context = xmlXPathNewContext(doc);
	if (context == NULL) {
		g_fprintf(stderr, "Error in xmlXPathNewContext\n");
		return 0;
	}
	result = xmlXPathEvalExpression(BAD_CAST xpath, context);
	xmlXPathFreeContext(context);
	if (result == NULL) {
		g_fprintf(stderr, "Error in xmlXPathEvalExpression\n");
		return 0;
	}
	if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
		xmlXPathFreeObject(result);
                g_fprintf(stderr, "No result\n");
		return 0;
	}
	xmlNodeSetPtr nodeset = result->nodesetval;
	if (nodeset->nodeNr != 1) {
		g_fprintf(stderr,"More than one result found for xpath expression\n");
		return 0;

	}
	gchar *keyword = (gchar *) xmlNodeListGetString(doc, nodeset->nodeTab[0]->children, 1);
	xmlXPathFreeObject (result);
	xmlFreeDoc(doc);
	double valued;
	int valuei;
	long int valuel;
	double inc = 1.0;

	if (allowed & PARAMETER_DOUBLE) {
		//valued = strtod(keyword, NULL) + 1.0;
		valued = strtod(keyword, NULL);
		buffer = g_strdup_printf("%lg", valued);
		gtk_entry_set_text(GTK_ENTRY(startEntry), buffer);
		g_free(buffer);
		if (allowed & PARAMETER_WEIGHT_FRACTION) {
			while (valued+inc >= 100.0) {
				inc/=10.0;
			}
		}
		buffer = g_strdup_printf("%lg", valued+inc);
		gtk_entry_set_text(GTK_ENTRY(endEntry), buffer);
		g_free(buffer);
	}
	else {
		g_fprintf(stderr, "only PARAMETER_DOUBLE is allowed for now\n");
		return 0;
	}
	xmlFree(keyword);
	

	hbox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Input files directory");
	GtkWidget *inputdirEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(inputdirEntry), FALSE);
	GtkWidget *inputsaveButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), inputdirEntry, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), inputsaveButton, FALSE, FALSE, 1);
	gtk_entry_set_text(GTK_ENTRY(inputdirEntry), g_path_get_dirname(filename));
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 2);

	hbox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Input file prefix");
	GtkWidget *inputprefixEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(inputprefixEntry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), inputprefixEntry, TRUE, TRUE, 1);
	gchar *inputprefix = g_path_get_basename(filename);
	strcpy(strrchr(inputprefix, '.'),"_");
	gtk_entry_set_text(GTK_ENTRY(inputprefixEntry), inputprefix);
	g_free(inputprefix);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 2);

	hbox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Output files directory");
	GtkWidget *outputdirEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(outputdirEntry), FALSE);
	GtkWidget *outputsaveButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), outputdirEntry, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), outputsaveButton, FALSE, FALSE, 1);
	gtk_entry_set_text(GTK_ENTRY(outputdirEntry), g_path_get_dirname(input->general->outputfile));
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 2);

	hbox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Output file prefix");
	GtkWidget *outputprefixEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(outputprefixEntry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), outputprefixEntry, TRUE, TRUE, 1);
	gchar *outputprefix = g_path_get_basename(input->general->outputfile);
	strcpy(strrchr(outputprefix, '.'),"_");
	gtk_entry_set_text(GTK_ENTRY(outputprefixEntry), outputprefix);
	g_free(outputprefix);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 2);

	wrd->wizard = wizard;
	wrd->inputdirEntry = inputdirEntry;
	wrd->inputprefixEntry = inputprefixEntry;
	wrd->outputdirEntry = outputdirEntry;
	wrd->outputprefixEntry = outputprefixEntry;
	wrd->startEntry = startEntry;
	wrd->endEntry = endEntry;
	wrd->nstepsEntry = nstepsEntry;
	wrd->allowed = allowed;

	struct saveButton_clicked_data *scdi = g_malloc(sizeof(struct saveButton_clicked_data));
	scdi->entry = inputdirEntry;
	scdi->title = g_strdup("Select the directory where the input files will be stored");
	scdi->wizard = wizard;
	struct saveButton_clicked_data *scdo = g_malloc(sizeof(struct saveButton_clicked_data));
	scdo->entry = outputdirEntry;
	scdo->title = g_strdup("Select the directory where the output files will be stored");
	scdo->wizard = wizard;
	g_signal_connect(G_OBJECT(inputsaveButton), "clicked", G_CALLBACK(saveButton_clicked_cb), (gpointer) scdi);
	g_signal_connect(G_OBJECT(outputsaveButton), "clicked", G_CALLBACK(saveButton_clicked_cb), (gpointer) scdo);

	g_signal_connect(G_OBJECT(inputdirEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	g_signal_connect(G_OBJECT(inputprefixEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	g_signal_connect(G_OBJECT(outputdirEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	g_signal_connect(G_OBJECT(outputprefixEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	g_signal_connect(G_OBJECT(startEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	g_signal_connect(G_OBJECT(endEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	g_signal_connect(G_OBJECT(nstepsEntry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);	
	//page with archive file and 
	vbox = gtk_vbox_new(FALSE, 2);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), vbox);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), vbox, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), vbox, "Save as archive file and set the plot options");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), vbox, TRUE);
	GtkWidget *archive_modeW = gtk_check_button_new_with_label("Create archive file");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(archive_modeW), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), archive_modeW, TRUE, FALSE, 1);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 3);
	hbox = gtk_hbox_new(FALSE,2);
	GtkWidget *labela = gtk_label_new("XMSA file");
	gtk_box_pack_start(GTK_BOX(hbox), labela, FALSE, FALSE, 2);
	GtkWidget *archiveEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(archiveEntry), FALSE);
	gchar *xmsafile = g_strdup(input->general->outputfile);
	xmsafile[strlen(xmsafile)-1] = 'a';
	gtk_entry_set_text(GTK_ENTRY(archiveEntry), xmsafile);
	g_free(xmsafile);
	gtk_box_pack_start(GTK_BOX(hbox), archiveEntry, TRUE, TRUE, 2);
	GtkWidget *archivesaveButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	g_signal_connect(G_OBJECT(archivesaveButton), "clicked", G_CALLBACK(archivesaveButton_clicked_cb), (gpointer) archiveEntry);
	gtk_box_pack_start(GTK_BOX(hbox), archivesaveButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 1);
	GtkWidget *keep_xmsiW = gtk_check_button_new_with_label("Keep input files after batch simulation");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(keep_xmsiW), FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), keep_xmsiW, TRUE, FALSE, 1);
	GtkWidget *keep_xmsoW = gtk_check_button_new_with_label("Keep output files after batch simulation");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(keep_xmsoW), FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), keep_xmsoW, TRUE, FALSE, 1);
	GtkWidget *archive_plotW = gtk_check_button_new_with_label("Plot after simulations");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(archive_plotW), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), archive_plotW, TRUE, FALSE, 1);


	struct wizard_plot_data *wpd = g_malloc(sizeof(struct wizard_plot_data));
	wpd->labela = labela;
	wpd->archive_plotW = archive_plotW;
	wpd->archive_modeW = archive_modeW;
	wpd->archiveEntry = archiveEntry;
	wpd->archivesaveButton = archivesaveButton;
	wpd->keep_xmsiW = keep_xmsiW;
	wpd->keep_xmsoW = keep_xmsoW;

	g_signal_connect(G_OBJECT(archive_modeW), "toggled", G_CALLBACK(wizard_archive_mode_changed_cb), (gpointer) wpd);

	//add confirmation page
	GtkWidget *confirmationLabel = gtk_label_new("Confirm the options selected on the previous pages and continue with the simulation?");
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), confirmationLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), confirmationLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), confirmationLabel, GTK_ASSISTANT_PAGE_CONFIRM);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), confirmationLabel, "Confirmation");
	
	struct wizard_archive_close_data *wacd = g_malloc(sizeof(struct wizard_archive_close_data));
	wacd->wrd = wrd;
	wacd->wpd = wpd;
	wacd->aod = aod;
	wacd->ow = ow;
	wacd->xmo = xmo;
	wacd->rv = &rv;

	g_signal_connect(G_OBJECT(wizard), "cancel", G_CALLBACK(wizard_archive_cancel), (gpointer) wacd);
	g_signal_connect(G_OBJECT(wizard), "close", G_CALLBACK(wizard_archive_close), (gpointer) wacd);
	g_signal_connect(G_OBJECT(wizard), "delete-event", G_CALLBACK(wizard_archive_delete_event), (gpointer) wacd);
	g_signal_connect(G_OBJECT(wizard), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_widget_show_all(wizard);
	gtk_main();
	return rv;
}


static void parameter_selection_changed_cb (GtkTreeSelection *selection, GtkWidget *okButton) {
	GtkTreeIter iter,temp_iter;
	GtkTreeModel *model;
	gboolean selectable;

	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
		gtk_tree_model_get(model, &iter, INPUT_SELECTABLE_COLUMN, &selectable, -1);
		if (selectable)
			gtk_widget_set_sensitive(okButton, TRUE);
		else
			gtk_widget_set_sensitive(okButton, FALSE);


	}
	else {
		gtk_widget_set_sensitive(okButton, FALSE);
	}
}

static void parameter_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, GtkButton *okButton) {
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
	GtkTreeIter iter;
	gtk_tree_model_get_iter(model, &iter, path);
	gboolean selectable;

	gtk_tree_model_get(model, &iter, INPUT_SELECTABLE_COLUMN, &selectable, -1);
	if (selectable)
		gtk_button_clicked(okButton);
	else {
		gtk_tree_view_expand_row(tree_view, path, FALSE);
	}

	return;
}

static int select_parameter(GtkWidget *window, struct xmi_input *input, gchar **xpath, int *allowed) {
	int rv = 0;
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Select the variable parameter", GTK_WINDOW(window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	GtkWidget *scrolled_window = get_inputfile_treeview(input, 1);


	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	gtk_container_add(GTK_CONTAINER(content_area), scrolled_window);
	gtk_widget_show_all(scrolled_window);
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 3);
	gtk_window_set_default_size(GTK_WINDOW(dialog),500,500);
	GtkWidget *treeview = gtk_bin_get_child(GTK_BIN(scrolled_window));
	GtkWidget *okButton = my_gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

	gtk_widget_set_sensitive(okButton, FALSE);
	GtkTreeSelection *select = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(select,GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(select), "changed",
			G_CALLBACK(parameter_selection_changed_cb),
			(gpointer) okButton
		);
	g_signal_connect(G_OBJECT(treeview), "row-activated", G_CALLBACK(parameter_row_activated_cb), (gpointer) okButton);



	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		rv = 1;
		GtkTreeIter iter;
		GtkTreeModel *model;
		gtk_tree_selection_get_selected(select, &model, &iter);
		//extract xpath expression
		gtk_tree_model_get(model, &iter, INPUT_XPATH_COLUMN, xpath, INPUT_ALLOWED_COLUMN, allowed, -1);
	}
	gtk_widget_destroy(dialog);

	return rv;
}



static void my_gtk_text_buffer_insert_at_cursor_with_tags2(struct batch_window_data *bwd, const gchar *text, gint len, GtkTextTag *first_tag, ...) {
	GtkTextIter iter, start;
	va_list args;
	GtkTextTag *tag;
	GtkTextMark *insert_mark;
	gint start_offset;

	g_return_if_fail(GTK_IS_TEXT_BUFFER(bwd->controlsLogB));
	g_return_if_fail(text != NULL);

	glong time_elapsed = (glong) g_timer_elapsed(bwd->timer,NULL);
	glong hours = time_elapsed / 3600;
	time_elapsed = time_elapsed % 3600;
	glong minutes = time_elapsed / 60;
	glong seconds = time_elapsed % 60;


	gchar *to_print = g_strdup_printf("%02i:%02i:%02i %s",(int) hours, (int) minutes, (int) seconds,text);

	gtk_text_buffer_get_end_iter(bwd->controlsLogB, &iter);

	start_offset = gtk_text_iter_get_offset(&iter);
	gtk_text_buffer_insert(bwd->controlsLogB, &iter, to_print,len);

	g_free(to_print);

	if (first_tag == NULL) {
		gtk_text_buffer_get_end_iter(bwd->controlsLogB, &iter);
		insert_mark = gtk_text_buffer_get_insert(bwd->controlsLogB);
		gtk_text_buffer_place_cursor(bwd->controlsLogB,&iter);
        	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (bwd->controlsLogW),
	        insert_mark, 0.0, FALSE, 0, 1.0);
		return;
	}

	gtk_text_buffer_get_iter_at_offset (bwd->controlsLogB, &start, start_offset);

	va_start(args, first_tag);
	tag = first_tag;
	while (tag) {
		gtk_text_buffer_apply_tag(bwd->controlsLogB, tag, &start, &iter);
		tag = va_arg(args, GtkTextTag*);
	}
	va_end(args);
	
	gtk_text_buffer_get_end_iter(bwd->controlsLogB, &iter);
	insert_mark = gtk_text_buffer_get_insert(bwd->controlsLogB);
	gtk_text_buffer_place_cursor(bwd->controlsLogB,&iter);
       	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (bwd->controlsLogW),
	        insert_mark, 0.0, FALSE, 0, 1.0);

	

	return;
}

static void batch_start_job_recursive(struct batch_window_data *bwd) {
	gchar **argv = NULL;
	gchar *xmimsim_executable;

#ifdef MAC_INTEGRATION
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMIMSIM_EXEC, &xmimsim_executable) == 0) {
		xmimsim_executable = NULL;
	}	
#else
	xmimsim_executable = g_find_program_in_path("xmimsim");
#endif

	int arg_counter = 9;
	//i refers to the file being executed
	int i = bwd->i;

	gchar *pbartext = g_strdup_printf("File %i/%i",i, g_slist_length(bwd->filenames));
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), pbartext);
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), (double) i/(double) g_slist_length(bwd->filenames));
	while(gtk_events_pending())
	    gtk_main_iteration();
	g_free(pbartext);
	


	int j;
	if (bwd->batch_options == XMI_MSIM_BATCH_MULTIPLE_OPTIONS)
		j = i;
	else
		j = 0;

	argv = (gchar **) g_malloc(sizeof(gchar *)*10);
	argv[0] = g_strdup(xmimsim_executable);	

	if (bwd->options[j].use_M_lines) {
		argv[1] = g_strdup("--enable-M-lines");
	}
	else
		argv[1] = g_strdup("--disable-M-lines");

	if (bwd->options[j].use_cascade_radiative) {
		argv[2] = g_strdup("--enable-radiative-cascade");
	}
	else
		argv[2] = g_strdup("--disable-radiative-cascade");

	if (bwd->options[j].use_cascade_auger) {
		argv[3] = g_strdup("--enable-auger-cascade");
	}
	else
		argv[3] = g_strdup("--disable-auger-cascade");

	if (bwd->options[j].use_variance_reduction) {
		argv[4] = g_strdup("--enable-variance-reduction");
	}
	else
		argv[4] = g_strdup("--disable-variance-reduction");

	if (bwd->options[j].use_sum_peaks) {
		argv[5] = g_strdup("--enable-pile-up");
	}
	else
		argv[5] = g_strdup("--disable-pile-up");

	if (bwd->options[j].use_poisson) {
		argv[6] = g_strdup("--enable-poisson");
	}
	else
		argv[6] = g_strdup("--disable-poisson");

	argv[7]	= g_strdup_printf("--set-channels=%i", bwd->options[j].nchannels); 

	if (gtk_combo_box_get_active(GTK_COMBO_BOX(bwd->verboseW)) == 0) {
		argv[8] = g_strdup("--verbose");
	}
	else {
		argv[8] = g_strdup("--very-verbose");
	} 
#ifdef G_OS_WIN32
	//set solid angles and escape ratios files ourself!
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;

	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0) {
		sprintf(buffer,"Could not determine solid angles HDF5 file\n");
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
		}
		return;	
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-solid-angles-data=%s",xmimsim_hdf5_solid_angles);
	arg_counter++;

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		sprintf(buffer,"Could not determine escape ratios HDF5 file\n");
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
		}
		return;	
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-escape-ratios-data=%s",xmimsim_hdf5_escape_ratios);
	arg_counter++;
#endif

	//number of threads
	if (bwd->nthreadsW != NULL) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter++] = g_strdup_printf("--set-threads=%i",(int) gtk_range_get_value(GTK_RANGE(bwd->nthreadsW)));
	}
	argv[arg_counter++] = g_strdup((char *) g_slist_nth_data(bwd->filenames,i));
	argv[arg_counter++] = NULL;

	gchar *wd = g_path_get_dirname((char *) g_slist_nth_data(bwd->filenames,i));
	gboolean spawn_rv;
	gint out_fh, err_fh;
	GError *spawn_error = NULL;
	char buffer[512];

	//spawn
	spawn_rv = g_spawn_async_with_pipes(wd, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL, &xmimsim_pid, NULL, &out_fh, &err_fh, &spawn_error);

	if (spawn_rv == FALSE) {
		//couldn't spawn
		//print messag_ to textbox in red...
		sprintf(buffer,"%s\n",spawn_error->message);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
			fclose(bwd->logFile);
		}
		g_error_free(spawn_error);
		xmimsim_pid = (GPid) -1;
		//Error dialog
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
	        GTK_MESSAGE_ERROR,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution failed with error message %s",buffer
                );
     		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 0;
		gtk_widget_destroy(bwd->batch_window);

		return;
	}
	sprintf(buffer,"%s was started with process id %i\n",argv[0],(int)xmimsim_pid);
	my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,NULL);
	if (bwd->logFile) {
		g_fprintf(bwd->logFile,"%s",buffer);
	}
	g_free(wd);

	bwd->paused=FALSE;

#ifdef G_OS_UNIX
	gtk_widget_set_sensitive(bwd->pauseButton,TRUE);
#endif
	gtk_widget_set_sensitive(bwd->stopButton,TRUE);

	GIOChannel *xmimsim_stdout;
	GIOChannel *xmimsim_stderr;
#ifdef G_OS_WIN32
	xmimsim_stderr= g_io_channel_win32_new_fd(err_fh);
	xmimsim_stdout = g_io_channel_win32_new_fd(out_fh);
#else
	xmimsim_stderr= g_io_channel_unix_new(err_fh);
	xmimsim_stdout = g_io_channel_unix_new(out_fh);
#endif
	bwd->i = i;
	bwd->argv0 = argv[0];
	g_child_watch_add(xmimsim_pid,(GChildWatchFunc) xmimsim_child_watcher_cb, (gpointer) bwd);


	const gchar *encoding = NULL;
	g_get_charset(&encoding);

	g_io_channel_set_encoding(xmimsim_stdout, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stdout,TRUE);
	g_io_add_watch(xmimsim_stdout, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_stdout_watcher, (gpointer) bwd);
	g_io_channel_unref(xmimsim_stdout);

	g_io_channel_set_encoding(xmimsim_stderr, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stderr,TRUE);
	g_io_add_watch(xmimsim_stderr, G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL, (GIOFunc) xmimsim_stderr_watcher, (gpointer) bwd);
	g_io_channel_unref(xmimsim_stderr);

	return;
}



static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char buffer[512];

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);	
		if (pipe_status == G_IO_STATUS_ERROR) {
			sprintf(buffer,"%s with process id %i had an I/O error: %s\n", bwd->argv0, (int) xmimsim_pid,pipe_error->message);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, pipe_string,-1,NULL);
			g_free(pipe_string);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",pipe_string);
			}
		}
		else
			return FALSE;
	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//sprintf(buffer,"%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags2(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char buffer[512];

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);	
		if (pipe_status == G_IO_STATUS_ERROR) {
			sprintf(buffer,"%s with process id %i had an I/O error: %s\n", bwd->argv0, (int) xmimsim_pid,pipe_error->message);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, pipe_string,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			g_free(pipe_string);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//sprintf(buffer,"%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags2(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static int batch_mode(GtkWidget * main_window, struct xmi_main_options *options, GSList *filenames, enum xmi_msim_batch_options);

static void batch_reset_controls(struct batch_window_data *bwd) {
	char buffer[512];
	GtkTextIter start, end;

	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Start simulation");
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 0.0);

	//clear textbuffer
	gtk_text_buffer_get_start_iter (bwd->controlsLogB,&start); 
	gtk_text_buffer_get_end_iter (bwd->controlsLogB,&end); 
	gtk_text_buffer_delete (bwd->controlsLogB,&start,&end); 

	bwd->paused = FALSE;
}


static void choose_logfile(GtkButton *saveButton, struct batch_window_data *bwd) {
	GtkWidget *dialog;
	gchar *filename;

	dialog = gtk_file_chooser_dialog_new("Select a filename for the logfile", GTK_WINDOW(bwd->batch_window), GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(bwd->controlsLogFileW), filename);
		g_free (filename);							
	}
	gtk_widget_destroy(dialog);
	return;
}

static void play_button_clicked(GtkButton *playButton, struct batch_window_data *bwd) {
	char buffer[512];

	//first deal with the pause case
#ifdef G_OS_UNIX
	if (bwd->paused) {
		gtk_widget_set_sensitive(bwd->playButton, FALSE);
		//send SIGCONT	
		int kill_rv;
		char buffer[512];
		gboolean spinning;
		g_timer_continue(bwd->timer);

		kill_rv = kill((pid_t) xmimsim_pid, SIGCONT);
		if (kill_rv == 0) {
			sprintf(buffer, "Process %i was successfully resumed\n",(int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			gtk_widget_set_sensitive(bwd->pauseButton,TRUE);
			bwd->paused = FALSE;
		}
		else {
			sprintf(buffer, "Process %i could not be resumed\n",(int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			gtk_widget_set_sensitive(bwd->playButton,TRUE);
			*(bwd->rv) = 0;
			//should end batch operation!
		}
		return;
	}
#endif
	bwd->paused = FALSE;

	//start_job
	gtk_widget_set_sensitive(bwd->playButton,FALSE);
	gtk_widget_set_sensitive(bwd->saveButton, FALSE);
	gtk_widget_set_sensitive(bwd->controlsLogFileW, FALSE);
	gtk_widget_set_sensitive(bwd->verboseW, FALSE);
	if (bwd->nthreadsW != NULL)
		gtk_widget_set_sensitive(bwd->nthreadsW,FALSE);
	
	batch_reset_controls(bwd);
	bwd->timer = g_timer_new();


	//open logFile if necessary
	gchar *logFileName = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(bwd->controlsLogFileW))));
	if (strlen(logFileName) > 0) {
		if ((bwd->logFile = fopen(logFileName, "w")) == NULL) {
			//could not write to logfile
			return;
		}
	}

	bwd->i = 0;

	batch_start_job_recursive(bwd);


	return;
}

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct batch_window_data *bwd) {
	char buffer[512];
	int success;


	gchar *data = bwd->argv0;


	fprintf(stdout,"xmimsim_child_watcher_cb called with status: %i\n",status);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
#ifdef G_OS_UNIX
	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
#endif

	//windows <-> unix issues here
	//unix allows to obtain more info about the way the process was terminated, windows will just have the exit code (status)
	//conditional compilation here
#ifdef G_OS_UNIX
	if (WIFEXITED(status)) {
		if (WEXITSTATUS(status) == 0) { /* child was terminated due to a call to exit */
			sprintf(buffer,"%s with process id %i exited normally without errors\n", data, (int) xmimsim_pid);
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"success" ),NULL);
			success = 1;
		}
		else {
			sprintf(buffer,"%s with process id %i exited with an error (code: %i)\n",data, (int) xmimsim_pid, WEXITSTATUS(status));
			my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			success = 0;
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to a signal */
		sprintf(buffer, "%s with process id %i was terminated by signal %i\n",data, (int) xmimsim_pid, WTERMSIG(status));
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}
	else {
		sprintf(buffer, "%s with process id %i was terminated in some special way\n",data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		sprintf(buffer,"%s with process id %i exited normally without errors\n", data, (int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"success" ),NULL);
		success = 1;
	}
	else {
		sprintf(buffer,"%s with process id %i exited with an error (code: %i)\n",data, (int) xmimsim_pid, status);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}
#endif

	if (bwd->logFile) {
		g_fprintf(bwd->logFile,"%s",buffer);
	}
	g_spawn_close_pid(xmimsim_pid);
	xmimsim_pid = GPID_INACTIVE;

	bwd->i++;

	if (success == 0) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations completed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		if (bwd->logFile)
			fclose(bwd->logFile);
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
	        GTK_MESSAGE_ERROR,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution failed with error message %s",buffer
	        );
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 0;
		gtk_widget_destroy(bwd->batch_window);
	}
	/*
	else if (success == 0) {
		//simulation failed but moving on to the next one
		bwd->success[bwd->i-1] = 0;
		batch_start_job_recursive(bwd);
	}
	*/
	else if (bwd->i == g_slist_length(bwd->filenames)) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations completed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		if (bwd->logFile)
			fclose(bwd->logFile);
		if (bwd->logFile)
			fclose(bwd->logFile);
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL,
	        GTK_MESSAGE_INFO,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution succeeded");
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 1;
		gtk_widget_destroy(bwd->batch_window);
	}
	else {
		//start the next job
		batch_start_job_recursive(bwd);
	}
		


	return;
}
static void stop_button_clicked(GtkButton *stopButton, struct batch_window_data *bwd) {
	char buffer[512];
	gboolean spinning;

	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
#ifdef G_OS_UNIX
	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
#endif
	//set buttons back in order
	bwd->paused = FALSE;
#ifdef G_OS_UNIX
	int kill_rv;
	
	fprintf(stdout,"stop_button_clicked_cb entered\n");
	kill_rv = kill((pid_t) xmimsim_pid, SIGTERM);
#if !GLIB_CHECK_VERSION (2, 35, 0)
	//starting with 2.36.0 (and some unstable versions before),
	//waitpid is called from within the main loop
	//causing all kinds of trouble if I would call wait here
	//wait(NULL);
	waitpid(xmimsim_pid, NULL, WNOHANG);
#endif
	fprintf(stdout,"stop_button_clicked_cb kill: %i\n",kill_rv);
	if (kill_rv == 0) {
		sprintf(buffer, "Process %i was successfully terminated before completion\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		sprintf(buffer, "Process %i could not be terminated with the SIGTERM signal\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	BOOL terminate_rv;

	terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

	if (terminate_rv == TRUE) {
		sprintf(buffer, "Process %i was successfully terminated\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		sprintf(buffer, "Process %i could not be terminated with the TerminateProcess call\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	}
#endif
	if (bwd->logFile)
		g_fprintf(bwd->logFile,"%s",buffer);

	return;
}

#ifdef G_OS_UNIX
static void pause_button_clicked(GtkButton *pauseButton, struct batch_window_data *bwd) {
	//UNIX only
	
	int kill_rv;
	char buffer[512];
	gboolean spinning;

	g_timer_stop(bwd->timer);

	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
	kill_rv = kill((pid_t) xmimsim_pid, SIGSTOP);
	if (kill_rv == 0) {
		sprintf(buffer, "Process %i was successfully paused. Press the Play button to continue or Stop to kill the process\n",(int) xmimsim_pid);
		my_gtk_text_buffer_insert_at_cursor_with_tags2(bwd, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
		bwd->paused=TRUE;
		if (bwd->logFile)
			g_fprintf(bwd->logFile, "%s", buffer);
		gtk_widget_set_sensitive(bwd->stopButton,TRUE);
		gtk_widget_set_sensitive(bwd->playButton,TRUE);
	}

}
#endif

static struct options_widget *create_options_frame(GtkWidget *main_window) {
	union xmimsim_prefs_val xpv;
	struct options_widget *rv = g_malloc(sizeof(struct options_widget));

	rv->superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(rv->superframe),10);

	rv->Mlines_prefsW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(rv->Mlines_prefsW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_M_LINES, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->Mlines_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->Mlines_prefsW, FALSE, FALSE, 3);

	rv->rad_cascade_prefsW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(rv->rad_cascade_prefsW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->rad_cascade_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->rad_cascade_prefsW, FALSE, FALSE, 3);

	rv->nonrad_cascade_prefsW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(rv->nonrad_cascade_prefsW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->nonrad_cascade_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->nonrad_cascade_prefsW, FALSE, FALSE, 3);

	rv->variance_reduction_prefsW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(rv->variance_reduction_prefsW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->variance_reduction_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->variance_reduction_prefsW, FALSE, FALSE, 3);

	rv->pile_up_prefsW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(rv->pile_up_prefsW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->pile_up_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->pile_up_prefsW, FALSE, FALSE, 3);


	rv->poisson_prefsW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	gtk_widget_set_tooltip_text(rv->poisson_prefsW,"Enabling this feature will add noise according to a Poisson distribution the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rv->poisson_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(rv->superframe),rv->poisson_prefsW, FALSE, FALSE, 3);

	GtkAdjustment *spinner_adj = GTK_ADJUSTMENT(gtk_adjustment_new(2048.0, 10.0, 100000.0, 1.0, 10.0, 0.0));
	rv->nchannels_prefsW = gtk_spin_button_new(spinner_adj, 1, 0);
	gtk_editable_set_editable(GTK_EDITABLE(rv->nchannels_prefsW), TRUE);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(rv->nchannels_prefsW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(rv->nchannels_prefsW), TRUE);
	gtk_entry_set_max_length(GTK_ENTRY(rv->nchannels_prefsW), 7);
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NCHANNELS, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(rv->nchannels_prefsW), (gdouble) xpv.i);
	GtkWidget *hbox = gtk_hbox_new(FALSE, 5);
	GtkWidget *label = gtk_label_new("Number of spectrum channels");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), rv->nchannels_prefsW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(rv->superframe), hbox, FALSE, FALSE, 3);

	return rv;
}

struct wizard_close_data {
	struct xmi_main_options *options;
	struct options_widget **ows;
	int *rv;
};

static void wizard_cancel(GtkAssistant *wizard, struct wizard_close_data *wcd) {
	*(wcd->rv) = 0;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static void wizard_close(GtkAssistant *wizard, struct wizard_close_data *wcd) {
	//collect options from all pages
	int i;

	for (i = 0 ; i < gtk_assistant_get_n_pages(wizard)-2 ; i++) {
		g_fprintf(stdout, "Processing file %i\n",i);
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->Mlines_prefsW)))
			wcd->options[i].use_M_lines = 1;
		else
			wcd->options[i].use_M_lines = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->rad_cascade_prefsW)))
			wcd->options[i].use_cascade_radiative = 1;
		else
			wcd->options[i].use_cascade_radiative = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->nonrad_cascade_prefsW)))
			wcd->options[i].use_cascade_auger = 1;
		else
			wcd->options[i].use_cascade_auger = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->variance_reduction_prefsW)))
			wcd->options[i].use_variance_reduction = 1;
		else
			wcd->options[i].use_variance_reduction = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->pile_up_prefsW)))
			wcd->options[i].use_sum_peaks = 1;
		else
			wcd->options[i].use_sum_peaks = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wcd->ows[i]->poisson_prefsW)))
			wcd->options[i].use_poisson = 1;
		else
			wcd->options[i].use_poisson = 0;

		wcd->options[i].nchannels = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(wcd->ows[i]->nchannels_prefsW));
			
	}
	*(wcd->rv) = 1;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static gboolean wizard_delete_event(GtkWidget *wizard, GdkEvent *event, struct wizard_close_data *wcd) {
	*(wcd->rv) = 0;
	return FALSE;
}

static void batch_window_exit(GtkButton *button, struct batch_window_data *bwd) {
	//should preserve the current rv
	//*(bwd->rv) = 1;
	if (kill_current_job() == 0)
		gtk_widget_destroy(bwd->batch_window);	
	return;
}

static gboolean batch_window_delete_event(GtkWidget *batch_window, GdkEvent *event, struct batch_window_data *bwd) {
	if (kill_current_job() == 0)
		return FALSE;	

	//*(bwd->rv) = 0;
	return TRUE;
}

static int specific_options(GtkWidget *main_window, struct xmi_main_options *options, GSList *filenames) {
	GtkWidget *wizard = gtk_assistant_new();
	gtk_window_set_transient_for(GTK_WINDOW(wizard), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_position (GTK_WINDOW(wizard), GTK_WIN_POS_CENTER);

	gtk_window_set_title(GTK_WINDOW(wizard), "Set simulation options per file");


	//add intro page
	GtkWidget *introLabel = gtk_label_new("Use this wizard to set the simulation options per XMI-MSIM input-file");	
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), introLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), introLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), introLabel, GTK_ASSISTANT_PAGE_INTRO);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), introLabel, "Introduction");

	int i;
	struct options_widget **ows = g_malloc(sizeof(struct options_widget*)*g_slist_length(filenames));
	for (i = 0 ; i < g_slist_length(filenames) ; i++) {
		ows[i] = create_options_frame(main_window);
		gtk_assistant_append_page(GTK_ASSISTANT(wizard), ows[i]->superframe);
		gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), ows[i]->superframe, GTK_ASSISTANT_PAGE_CONTENT);
		gchar *filename = g_strdup_printf("File %i: %s",i+1, g_path_get_basename((char *) g_slist_nth_data(filenames,i)));
		gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), ows[i]->superframe, filename);
		g_free(filename);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), ows[i]->superframe, TRUE);
	}

	//add confirmation page
	GtkWidget *confirmationLabel = gtk_label_new("Confirm the options selected on the previous pages and continue with the simulation?");
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), confirmationLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), confirmationLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), confirmationLabel, GTK_ASSISTANT_PAGE_CONFIRM);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), confirmationLabel, "Confirmation");

	//signal handlers
	struct wizard_close_data *wcd = g_malloc(sizeof(struct wizard_close_data));
	wcd->options = options;
	wcd->ows = ows;
	int rv;
	wcd->rv = &rv;
	g_signal_connect(G_OBJECT(wizard), "cancel", G_CALLBACK(wizard_cancel), (gpointer) wcd);
	g_signal_connect(G_OBJECT(wizard), "close", G_CALLBACK(wizard_close), (gpointer) wcd);
	g_signal_connect(G_OBJECT(wizard), "delete-event", G_CALLBACK(wizard_delete_event), (gpointer) wcd);
	g_signal_connect(G_OBJECT(wizard), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_widget_show_all(wizard);
	gtk_main();
	return rv;
}



static int general_options(GtkWidget *main_window, struct xmi_main_options *options) {
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Set the options for the simulations batch", GTK_WINDOW(main_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	struct options_widget *ow = create_options_frame(main_window);
	gtk_container_add(GTK_CONTAINER(content_area), ow->superframe);
	gtk_widget_show_all(ow->superframe);
	int dialog_rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (dialog_rv == GTK_RESPONSE_ACCEPT) {
		//accepted -> read options from dialog
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->Mlines_prefsW)))
			options->use_M_lines = 1;
		else
			options->use_M_lines = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->rad_cascade_prefsW)))
			options->use_cascade_radiative = 1;
		else
			options->use_cascade_radiative = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->nonrad_cascade_prefsW)))
			options->use_cascade_auger = 1;
		else
			options->use_cascade_auger = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->variance_reduction_prefsW)))
			options->use_variance_reduction = 1;
		else
			options->use_variance_reduction = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->pile_up_prefsW)))
			options->use_sum_peaks = 1;
		else
			options->use_sum_peaks = 0;

		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ow->poisson_prefsW)))
			options->use_poisson = 1;
		else
			options->use_poisson = 0;

		options->nchannels = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(ow->nchannels_prefsW));
	}
	else {
		gtk_widget_destroy(dialog);
		return 0;
	}
	gtk_widget_destroy(dialog);
	return 1;
}  


void batchmode_button_clicked_cb(GtkWidget *button, GtkWidget *window) {

	//g_fprintf(stdout,"Entering batchnode_button_clicked_cb...\n"); 
	//open dialog
	GtkWidget *dialog = gtk_file_chooser_dialog_new("Select one or more files", GTK_WINDOW(window), GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL);
	gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(dialog), TRUE);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	GtkWidget *label = gtk_label_new("If one file is selected then a batch of files will be created based on this file with one variable parameter. Selecting multiple files will result in all these files being executed.");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog), label);	
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsi");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

	if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_ACCEPT) {
		//nothing was selected
   		gtk_widget_destroy (dialog);
		return;
	}

	//extract all selected filenames
	GSList *filenames = gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(dialog));
	int i;
	//for (i = 0 ; i < g_slist_length(filenames) ; i++) {
	//	g_fprintf(stdout,"filename: %s\n", (char *) g_slist_nth_data(filenames,i));
	//}

   	gtk_widget_destroy (dialog);
	struct xmi_main_options *options;
	if (g_slist_length(filenames) > 1) {
		//more than one file selected
		//1) ask if the options will apply to all or if individual job options will be used
		dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "Set options for each input-file separately?");
		int response = gtk_dialog_run(GTK_DIALOG(dialog));
		//2) produce dialog asking for options
		if (response == GTK_RESPONSE_YES) {
   			gtk_widget_destroy (dialog);
			//file specific options
			//g_fprintf(stdout, "yes clicked\n");
			options = malloc(sizeof(struct xmi_main_options)*g_slist_length(filenames));
			int rv = specific_options(window, options, filenames); 
			if (rv == 1) {
				//wizard completed 
				//g_fprintf(stdout,"wizard completed\n");
			}
			else if (rv == 0) {
				//wizard aborted 
				//g_fprintf(stdout,"wizard aborted\n");
				return;
			}
		}
		else if (response == GTK_RESPONSE_NO) {
			//options apply to all
   			gtk_widget_destroy (dialog);
			//g_fprintf(stdout, "no clicked\n");
			options = malloc(sizeof(struct xmi_main_options));
			int rv = general_options(window, options);
			if (rv == 0) {
				return;
			}
			//options are set
		}
		else {
   			gtk_widget_destroy (dialog);
			return;
		}
		//3) launch execution window
		int exec_rv = batch_mode(window, options, filenames, response == GTK_RESPONSE_YES ? XMI_MSIM_BATCH_MULTIPLE_OPTIONS : XMI_MSIM_BATCH_ONE_OPTION);
		//4) display message with result
		//g_fprintf(stdout,"exec_rv: %i\n", exec_rv);
	}
	else {
		//one file selected
		//options apply to all
		//g_fprintf(stdout, "no clicked\n");
		gchar *xpath;
		struct xmi_input *input;
		if (xmi_read_input_xml((gchar *) g_slist_nth_data(filenames, 0), &input) == 0) {
			//error reading inputfile
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading input-file %s. Aborting batch mode", (gchar *) g_slist_nth_data(filenames, 0));
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}

		int allowed;
		int rv = select_parameter(window, input, &xpath, &allowed);
		//g_fprintf(stdout,"select_parameter rv: %i\n", rv);
		if (rv == 1) {
			//g_fprintf(stdout, "xpath: %s\n", xpath);
			//g_fprintf(stdout, "allowed: %i\n", allowed);
		}
		else
			return;
		//generate wizard
		//1) options
		//2) range of parameter
		//3) plot afterwards? Requires saving to XMSA file as well as selecting a particular line
		options = g_malloc(sizeof(struct xmi_main_options));
		struct archive_options_data *aod = g_malloc(sizeof(struct archive_options_data));
		rv = archive_options(window, input, options, (gchar *) g_slist_nth_data(filenames, 0), xpath, allowed, aod);
		if (rv == 0) {
			return;
		}
		//4) generate the new XMSI files
		GSList *filenames_xmsiGSL = NULL;
		gchar **filenames_xmsi = g_malloc(sizeof(gchar *)*(aod->nsteps+2));
		gchar **filenames_xmso = g_malloc(sizeof(gchar *)*(aod->nsteps+2));
		filenames_xmsi[aod->nsteps+1] = NULL;
		filenames_xmso[aod->nsteps+1] = NULL;
		gchar *filename = (gchar *) g_slist_nth_data(filenames, 0);
		gchar *buffer;
		//open inputfile
		xmlDocPtr doc;
		if ((doc = xmlReadFile(filename,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
			g_fprintf(stderr,"xmlReadFile error for %s\n", filename);
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not read file %s. Aborting batch mode", filename);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlXPathContextPtr context;
		xmlXPathObjectPtr result, result2;

		context = xmlXPathNewContext(doc);
		if (context == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		//selected xpath
		result = xmlXPathEvalExpression(BAD_CAST xpath, context);
		if (result == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
			xmlXPathFreeObject(result);
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlNodeSetPtr nodeset = result->nodesetval;
		if (nodeset->nodeNr != 1) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		//outputfile
		result2 = xmlXPathEvalExpression(BAD_CAST "/xmimsim/general/outputfile", context);
		if (result2 == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		if(xmlXPathNodeSetIsEmpty(result2->nodesetval)){
			xmlXPathFreeObject(result2);
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlNodeSetPtr nodeset2 = result2->nodesetval;
		if (nodeset2->nodeNr != 1) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}

		double weight_sum = 0.0;
		double *orig_weight = NULL;
		xmlXPathObjectPtr result3;
		xmlNodePtr grandparent;
		xmlNodeSetPtr grandkids;
		if (allowed & PARAMETER_WEIGHT_FRACTION) {
			//get grandparent of xpath node
			grandparent = nodeset->nodeTab[0]->parent->parent;
			//get all weight_fraction grandchildren
			result3 = xmlXPathNodeEval(grandparent, BAD_CAST "element/weight_fraction", context);
			if (result3 == NULL) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error occurred while evaluating element/weight_fraction.\nAborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			if(xmlXPathNodeSetIsEmpty(result3->nodesetval)){
				xmlXPathFreeObject(result3);
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error: empty nodeset. Aborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			grandkids = result3->nodesetval;
			orig_weight = g_malloc(sizeof(double) * grandkids->nodeNr);
			for (i = 0 ; i < grandkids->nodeNr ; i++) {
				if (grandkids->nodeTab[i] == nodeset->nodeTab[0]) {
					continue;
				}
				buffer = (gchar *) xmlNodeListGetString(doc, grandkids->nodeTab[i]->children, 1);	
				orig_weight[i] = strtod(buffer, NULL);
				weight_sum += orig_weight[i];
				xmlFree(buffer);
			}

		}


		int j;
		for (i = 0 ; i <= aod->nsteps ; i++) {
			buffer = g_strdup_printf("%s%s%s%04i.xmsi", aod->xmsi_input_dir, G_DIR_SEPARATOR_S, aod->xmsi_prefix, i);
			filenames_xmsiGSL = g_slist_append(filenames_xmsiGSL, (gpointer) buffer);
			filenames_xmsi[i] = buffer;
			buffer = g_strdup_printf("%s%s%s%04i.xmso", aod->xmso_output_dir, G_DIR_SEPARATOR_S, aod->xmso_prefix, i);
			filenames_xmso[i] = buffer;
			double value = aod->start_value + i*(aod->end_value-aod->start_value)/(aod->nsteps);
			if (allowed & PARAMETER_WEIGHT_FRACTION) {
				double diff = 100.0-value-weight_sum;
				double new_weight_sum = weight_sum + diff;
				for (j = 0 ; j < grandkids->nodeNr ; j++) {
					if (grandkids->nodeTab[j] == nodeset->nodeTab[0]) {
						continue;
					}
					double new_weight = orig_weight[j]*new_weight_sum/weight_sum;
					buffer = g_strdup_printf("%lg", new_weight);
					xmlNodeSetContent(grandkids->nodeTab[j], BAD_CAST buffer);
					g_free(buffer);
				}
			}
			buffer = g_strdup_printf("%lg", value);
			xmlNodeSetContent(nodeset->nodeTab[0], BAD_CAST buffer);
			g_free(buffer);
			xmlNodeSetContent(nodeset2->nodeTab[0], BAD_CAST filenames_xmso[i]);
			xmlKeepBlanksDefault(0);
			if (xmlSaveFormatFileEnc(filenames_xmsi[i],doc,NULL,1) == -1) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not write to %s. Aborting batch mode", filenames_xmsi[i]);
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
		}
		if (orig_weight)
			g_free(orig_weight);
		xmlXPathFreeContext(context);
		xmlXPathFreeObject(result);
		xmlXPathFreeObject(result2);
		xmlFreeDoc(doc);
		int exec_rv = batch_mode(window, options, filenames_xmsiGSL, XMI_MSIM_BATCH_ONE_OPTION);
		if (exec_rv == 0 || aod->with_xmsa == FALSE) {
			return;
		}
		//read all xmso files that were just created
		struct xmi_output **output = g_malloc(sizeof(struct xmi_output *)*(aod->nsteps+1));
		for (i = 0 ; i <= aod->nsteps ; i++) {
			//g_fprintf(stdout, "Reading %s\n", filenames_xmso[i]);
			if (xmi_read_output_xml(filenames_xmso[i], &output[i]) == 0) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading output-file %s. Aborting batch mode", filenames_xmso[i]);
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}	
		}
		//convert to an archive struct
		struct xmi_archive *archive = xmi_archive_raw2struct(output, aod->start_value, aod->end_value, aod->nsteps, xpath);
		//save to XMSA file
		//g_fprintf(stdout, "Writing %s\n", aod->xmsa_file);
		if (xmi_write_archive_xml(aod->xmsa_file, archive) == 0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error writing to archive-file %s. Aborting batch mode", aod->xmsa_file);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		//g_fprintf(stdout, "Freeing output\n");
		for (i = 0 ; i <= aod->nsteps ; i++)
			xmi_free_output(output[i]);
		g_free(output);

		//deleting files if required
		if (!aod->keep_xmsi) {
			for (i = 0 ; i <= aod->nsteps ; i++)
				g_unlink(filenames_xmsi[i]);
		}
		if (!aod->keep_xmso) {
			for (i = 0 ; i <= aod->nsteps ; i++)
				g_unlink(filenames_xmso[i]);
		}
		g_strfreev(filenames_xmsi);
		g_strfreev(filenames_xmso);
		g_slist_free(filenames_xmsiGSL);

		//test
		/*g_fprintf(stdout, "Freeing archive\n");
		xmi_free_archive(archive);
		g_fprintf(stdout, "Reading archive %s\n", aod->xmsa_file);
		if (xmi_read_archive_xml(aod->xmsa_file, &archive) == 0) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading from archive-file %s. Aborting batch mode", aod->xmsa_file);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}*/
		
		//and plot!
		if (aod->with_plot)
			launch_archive_plot(archive, window);
	}



	return;
}

static int batch_mode(GtkWidget *main_window, struct xmi_main_options *options, GSList *filenames, enum xmi_msim_batch_options batch_options) {
	int rv = 0;
	GtkWidget *batch_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_transient_for(GTK_WINDOW(batch_window), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(batch_window), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(batch_window), TRUE);
	gtk_window_set_position (GTK_WINDOW(batch_window), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(batch_window), "Batch simulation controls");
	gtk_window_set_default_size(GTK_WINDOW(batch_window),500,500);

	struct batch_window_data *bwd = g_malloc(sizeof(struct batch_window_data));
	bwd->batch_window = batch_window;

	GtkWidget *main_vbox = gtk_vbox_new(FALSE,0);
	gtk_container_add(GTK_CONTAINER(batch_window), main_vbox);
	gtk_container_set_border_width(GTK_CONTAINER(batch_window),3);
	GtkWidget *hbox = gtk_hbox_new(FALSE, 2);

	GtkWidget *playButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(playButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY,GTK_ICON_SIZE_DIALOG));
	bwd->playButton = playButton;
	gtk_box_pack_start(GTK_BOX(hbox), playButton, FALSE, FALSE, 2);

#ifdef G_OS_UNIX
	GtkWidget *pauseButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(pauseButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PAUSE,GTK_ICON_SIZE_DIALOG));
	bwd->pauseButton = pauseButton;
	gtk_box_pack_start(GTK_BOX(hbox), pauseButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(pauseButton, FALSE);
#endif

	GtkWidget *stopButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(stopButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_DIALOG));
	bwd->stopButton = stopButton;
	gtk_box_pack_start(GTK_BOX(hbox), stopButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(stopButton, FALSE);

	GtkWidget *nthreadsW = NULL;
	if (xmi_omp_get_max_threads() > 1) {
		GtkWidget *cpuLabel = gtk_label_new("CPUs");
		gtk_box_pack_start(GTK_BOX(hbox), cpuLabel, FALSE, FALSE, 2);
		GtkObject *nthreadsA = gtk_adjustment_new((gdouble) xmi_omp_get_max_threads(), 1.0, (gdouble) xmi_omp_get_max_threads(), 1.0,1.0,0.0);
		nthreadsW = gtk_hscale_new(GTK_ADJUSTMENT(nthreadsA));	
		gtk_scale_set_digits(GTK_SCALE(nthreadsW), 0);
		gtk_scale_set_value_pos(GTK_SCALE(nthreadsW),GTK_POS_TOP);
		gtk_widget_set_size_request(nthreadsW,30,-1);
		gtk_box_pack_start(GTK_BOX(hbox), nthreadsW, TRUE, TRUE, 2);
	}
	bwd->nthreadsW = nthreadsW;

	//add progressbar
	GtkWidget *progressbarW = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbarW), GTK_PROGRESS_LEFT_TO_RIGHT);
	//gtk_widget_set_size_request(progressbarW,-1,10);
	GtkWidget *pvbox = gtk_vbox_new(TRUE,1);
	gtk_box_pack_start(GTK_BOX(pvbox), progressbarW, FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox), pvbox, TRUE, TRUE, 2);
	gtk_widget_set_size_request(progressbarW,-1,30);
	bwd->progressbarW = progressbarW;
	
	gtk_box_pack_start(GTK_BOX(main_vbox), hbox, FALSE, FALSE, 0);

	//output log
	GtkWidget *controlsLogW = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(controlsLogW),GTK_WRAP_WORD);
	gtk_text_view_set_left_margin(GTK_TEXT_VIEW(controlsLogW),3);
	GtkTextBuffer *controlsLogB = gtk_text_view_get_buffer(GTK_TEXT_VIEW(controlsLogW));
	gtk_container_set_border_width(GTK_CONTAINER(controlsLogW),2);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(controlsLogW),FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(controlsLogW),FALSE);
	gtk_text_buffer_create_tag(controlsLogB, "error","foreground","red",NULL);
	gtk_text_buffer_create_tag(controlsLogB, "success","foreground","green",NULL);
	gtk_text_buffer_create_tag(controlsLogB, "pause-continue-stopped","foreground","orange",NULL);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), controlsLogW);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 2);
	gtk_box_pack_start(GTK_BOX(main_vbox), scrolled_window, TRUE, TRUE, 3);
	bwd->controlsLogW = controlsLogW;
	bwd->controlsLogB = controlsLogB;

	//bottom widget
	hbox = gtk_hbox_new(FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 2);
	GtkWidget *saveButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	bwd->saveButton = saveButton;
	gtk_box_pack_start(GTK_BOX(hbox), saveButton, FALSE, FALSE, 2);
	GtkWidget *controlsLogFileW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(controlsLogFileW), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), controlsLogFileW, TRUE, TRUE, 2);
	bwd->controlsLogFileW = controlsLogFileW;

	g_signal_connect(G_OBJECT(saveButton), "clicked", G_CALLBACK(choose_logfile), (gpointer) bwd);

#if GTK_CHECK_VERSION(2,24,0)
	GtkWidget *verboseW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Verbose");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Very verbose");
#else
	GtkWidget *verboseW = gtk_combo_box_new_text();
	gtk_combo_box_append_text(GTK_COMBO_BOX(verboseW),"Verbose");
	gtk_combo_box_append_text(GTK_COMBO_BOX(verboseW),"Very verbose");
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(verboseW),0);
	bwd->verboseW = verboseW;
	gtk_box_pack_start(GTK_BOX(hbox), verboseW, FALSE, FALSE, 2);
	GtkWidget *exitButton = gtk_button_new_from_stock(GTK_STOCK_QUIT);
	bwd->exitButton = exitButton;
	gtk_box_pack_end(GTK_BOX(hbox), exitButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(main_vbox), hbox, FALSE, FALSE, 0);

	bwd->options = options;
	bwd->filenames = filenames;
	bwd->batch_options = batch_options;
	bwd->rv = &rv;


	g_signal_connect(G_OBJECT(batch_window), "delete-event", G_CALLBACK(batch_window_delete_event), (gpointer) bwd);
	g_signal_connect(G_OBJECT(batch_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(exitButton), "clicked", G_CALLBACK(batch_window_exit), (gpointer) bwd);
	g_signal_connect(G_OBJECT(playButton), "clicked", G_CALLBACK(play_button_clicked), (gpointer) bwd);
	g_signal_connect(G_OBJECT(stopButton), "clicked", G_CALLBACK(stop_button_clicked), (gpointer) bwd);
#ifdef G_OS_UNIX
	g_signal_connect(G_OBJECT(pauseButton), "clicked", G_CALLBACK(pause_button_clicked), (gpointer) bwd);
#endif
	bwd->paused = FALSE;
	bwd->logFile = NULL;
	gtk_widget_show_all(batch_window);
	
	gtk_main();

	return rv;
}

GtkWidget *get_inputfile_treeview(struct xmi_input *input, int with_colors) {
	//assume that the input has been validated
	GtkTreeStore *model = gtk_tree_store_new(INPUT_N_COLUMNS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_INT);	
	GtkWidget *treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	int i,j,k;

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Parameter", renderer, "text", INPUT_PARAMETER_COLUMN, NULL);
	if (with_colors) {
		gtk_tree_view_column_add_attribute(column, renderer, "cell-background-set", INPUT_SELECTABLE_COLUMN);
		g_object_set(renderer, "cell-background", "Chartreuse", NULL);
	}
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	renderer = gtk_cell_renderer_text_new();
	my_gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	column = gtk_tree_view_column_new_with_attributes("Value", renderer, "text", INPUT_VALUE_COLUMN, NULL);
	if (with_colors) {
		gtk_tree_view_column_add_attribute(column, renderer, "cell-background-set", INPUT_SELECTABLE_COLUMN);
		g_object_set(renderer, "cell-background", "Chartreuse", NULL);
	}
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);

	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), treeview);

	GtkTreeIter iter1, iter2, iter3, iter4;


	//general
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1, 
		INPUT_PARAMETER_COLUMN, "general",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general",
		-1
	);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "outputfile",
		INPUT_VALUE_COLUMN, input->general->outputfile,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/outputfile",
		-1
	);
	gchar *buffer, *buffer2;
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%li", input->general->n_photons_interval);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_photons_interval",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_photons_interval",
		INPUT_ALLOWED_COLUMN, PARAMETER_LONG | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%li", input->general->n_photons_line);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_photons_line",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_photons_line",
		INPUT_ALLOWED_COLUMN, PARAMETER_LONG | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%i", input->general->n_interactions_trajectory);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_interactions_trajectory",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_interactions_trajectory",
		INPUT_ALLOWED_COLUMN, PARAMETER_INT | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "comments",
		INPUT_VALUE_COLUMN, input->general->comments,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/general/n_interactions_comments",
		-1
	);

	//composition
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1, 
		INPUT_PARAMETER_COLUMN, "composition",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/composition",
		-1
	);

	for (i = 0 ; i < input->composition->n_layers ; i++) {
		buffer = g_strdup_printf("Layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter2, &iter1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->composition->layers[i].n_elements ; j++) {
			buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter3, &iter2);
			gtk_tree_store_set(model, &iter3,
				INPUT_PARAMETER_COLUMN, "element",
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			buffer = AtomicNumberToSymbol(input->composition->layers[i].Z[j]);
			buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%lg", input->composition->layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->composition->layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%lg", input->composition->layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%lg", input->composition->layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/composition/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}
	buffer = g_strdup_printf("%i", input->composition->reference_layer);
	buffer2 = g_strdup_printf("/xmimsim/composition/reference_layer");
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "reference_layer",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, buffer2,
		-1
	);
	g_free(buffer);
	g_free(buffer2);

	//geometry
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "geometry",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry",
		-1
	);

	buffer = g_strdup_printf("%lg", input->geometry->d_sample_source);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "d_sample_source",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/d_sample_source",
		-1
	);
	g_free(buffer);

	buffer = g_strdup_printf("%lg", input->geometry->n_sample_orientation[0]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_sample_orientation_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_sample_orientation/x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->n_sample_orientation[1]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_sample_orientation_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_sample_orientation/y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->n_sample_orientation[2]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_sample_orientation_z",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_sample_orientation/z",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);

	buffer = g_strdup_printf("%lg", input->geometry->p_detector_window[0]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "p_detector_window_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/p_detector_window/x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->p_detector_window[1]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "p_detector_window_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/p_detector_window/y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->p_detector_window[2]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "p_detector_window_z",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/p_detector_window/z",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);

	buffer = g_strdup_printf("%lg", input->geometry->n_detector_orientation[0]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_detector_orientation_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_detector_orientation/x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->n_detector_orientation[1]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_detector_orientation_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_detector_orientation/y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->n_detector_orientation[2]);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "n_detector_orientation_z",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/n_detector_orientation/z",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->area_detector);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "area_detector",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/area_detector",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->collimator_height);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "collimator_height",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, input->geometry->collimator_height > 0.0 && input->geometry->collimator_diameter > 0.0 ? TRUE : FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/collimator_height",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->collimator_diameter);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "collimator_diameter",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, input->geometry->collimator_height > 0.0 && input->geometry->collimator_diameter > 0.0 ? TRUE : FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/collimator_diameter",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->d_source_slit);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "d_source_slit",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/d_source_slit",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->slit_size_x);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "slit_size_x",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/slit_size_x",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	buffer = g_strdup_printf("%lg", input->geometry->slit_size_y);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "slit_size_y",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/geometry/slit_size_y",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);

	//excitation
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "excitation",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/excitation",
		-1
	);
	for (i = 0 ; i < input->excitation->n_discrete ; i++) {
		gtk_tree_store_append(model, &iter2, &iter1);
		buffer = g_strdup_printf("discrete %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]", i+1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].energy);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/energy", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "energy",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, input->excitation->n_discrete == 1 ? TRUE : FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].horizontal_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/horizontal_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "horizontal_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].vertical_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/vertical_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "vertical_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].sigma_x);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_x", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_x",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].sigma_y);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_y", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_y",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].sigma_xp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_xp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_xp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->discrete[i].sigma_yp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/sigma_yp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_yp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/distribution_type", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "distribution_type",
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer2);
		switch (input->excitation->discrete[i].distribution_type) {
			case XMI_DISCRETE_MONOCHROMATIC:
				gtk_tree_store_set(model, &iter3,
					INPUT_VALUE_COLUMN, "monochromatic",
					-1
				);
				break;
			case XMI_DISCRETE_GAUSSIAN:
				gtk_tree_store_set(model, &iter3,
					INPUT_VALUE_COLUMN, "gaussian",
					-1);
				gtk_tree_store_append(model, &iter3, &iter2);
				buffer = g_strdup_printf("%g", input->excitation->discrete[i].scale_parameter);
				buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/scale_parameter", i+1);
				gtk_tree_store_set(model, &iter3,
					INPUT_PARAMETER_COLUMN, "standard_deviation",
					INPUT_SELECTABLE_COLUMN, TRUE,
					INPUT_VALUE_COLUMN, buffer,
					INPUT_XPATH_COLUMN, buffer2,
					INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
					-1
				);
				g_free(buffer);
				g_free(buffer2);
				break;
			case XMI_DISCRETE_LORENTZIAN:
				gtk_tree_store_set(model, &iter3,
					INPUT_VALUE_COLUMN, "lorentzian",
					-1
				);
				gtk_tree_store_append(model, &iter3, &iter2);
				buffer = g_strdup_printf("%g", input->excitation->discrete[i].scale_parameter);
				buffer2 = g_strdup_printf("/xmimsim/excitation/discrete[%i]/scale_parameter", i+1);
				gtk_tree_store_set(model, &iter3,
					INPUT_PARAMETER_COLUMN, "scale_parameter",
					INPUT_SELECTABLE_COLUMN, TRUE,
					INPUT_VALUE_COLUMN, buffer,
					INPUT_XPATH_COLUMN, buffer2,
					INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
					-1
				);
				g_free(buffer);
				g_free(buffer2);
				break;
		}
	}
	for (i = 0 ; i < input->excitation->n_continuous ; i++) {
		gtk_tree_store_append(model, &iter2, &iter1);
		buffer = g_strdup_printf("continuous %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]", i+1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, "continuous",
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].energy);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/energy", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "energy",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].horizontal_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/horizontal_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "horizontal_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].vertical_intensity);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/vertical_intensity", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "vertical_intensity",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].sigma_x);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_x", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_x",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].sigma_y);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_y", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_y",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].sigma_xp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_xp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_xp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
		gtk_tree_store_append(model, &iter3, &iter2);
		buffer = g_strdup_printf("%lg", input->excitation->continuous[i].sigma_yp);
		buffer2 = g_strdup_printf("/xmimsim/excitation/continuous[%i]/sigma_yp", i+1);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "sigma_yp",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}

	//absorbers
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "absorbers",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/absorbers",
		-1
	);
	for (i = 0 ; i < input->absorbers->n_exc_layers ; i++) {
		buffer = g_strdup_printf("Excitation Layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter2, &iter1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->absorbers->exc_layers[i].n_elements ; j++) {
			buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter3, &iter2);
			gtk_tree_store_set(model, &iter3,
				INPUT_PARAMETER_COLUMN, "element",
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			buffer = AtomicNumberToSymbol(input->absorbers->exc_layers[i].Z[j]);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%lg", input->absorbers->exc_layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->absorbers->exc_layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%lg", input->absorbers->exc_layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%lg", input->absorbers->exc_layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/excitation_path/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}
	for (i = 0 ; i < input->absorbers->n_det_layers ; i++) {
		buffer = g_strdup_printf("Detector Layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter2, &iter1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->absorbers->det_layers[i].n_elements ; j++) {
			buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter3, &iter2);
			gtk_tree_store_set(model, &iter3,
				INPUT_PARAMETER_COLUMN, "element",
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			buffer = AtomicNumberToSymbol(input->absorbers->det_layers[i].Z[j]);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%lg", input->absorbers->det_layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->absorbers->det_layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%lg", input->absorbers->det_layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%lg", input->absorbers->det_layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/absorbers/detector_path/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}

	//detector
	gtk_tree_store_append(model, &iter1, NULL);
	gtk_tree_store_set(model, &iter1,
		INPUT_PARAMETER_COLUMN, "detector",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector",
		-1
	);
	gtk_tree_store_append(model, &iter2, &iter1);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "detector_type",
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/detector_type",
		-1
	);
	switch (input->detector->detector_type) {
		case XMI_DETECTOR_SILI:
			gtk_tree_store_set(model, &iter2,
				INPUT_VALUE_COLUMN, "SiLi",
				-1
			);
			break;
		case XMI_DETECTOR_GE:
			gtk_tree_store_set(model, &iter2,
				INPUT_VALUE_COLUMN, "Ge",
				-1
			);
			break;
		case XMI_DETECTOR_SI_SDD:
			gtk_tree_store_set(model, &iter2,
				INPUT_VALUE_COLUMN, "Si Drift Detector",
				-1
			);
			break;
	}
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->live_time);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "live_time",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/live_time",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->pulse_width);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "pulse_width",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/pulse_width",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->gain);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "gain",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/gain",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->zero);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "zero",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/zero",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->fano);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "fano",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/fano",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->noise);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "noise",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, TRUE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/noise",
		INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
		-1
	);
	g_free(buffer);
	gtk_tree_store_append(model, &iter2, &iter1);
	buffer = g_strdup_printf("%lg", input->detector->max_convolution_energy);
	gtk_tree_store_set(model, &iter2,
		INPUT_PARAMETER_COLUMN, "max_convolution_energy",
		INPUT_VALUE_COLUMN, buffer,
		INPUT_SELECTABLE_COLUMN, FALSE,
		INPUT_XPATH_COLUMN, "/xmimsim/detector/max_convolution_energy",
		-1
	);
	g_free(buffer);
	for (i = 0 ; i < input->detector->n_crystal_layers ; i++) {
		buffer = g_strdup_printf("Crystal Layer %i", i+1);
		buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]", i+1);
		gtk_tree_store_append(model, &iter2, &iter1);
		gtk_tree_store_set(model, &iter2,
			INPUT_PARAMETER_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, FALSE,
			INPUT_XPATH_COLUMN, buffer2,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		for (j = 0 ; j < input->detector->crystal_layers[i].n_elements ; j++) {
			buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/element[%i]", i+1, j+1);
			gtk_tree_store_append(model, &iter3, &iter2);
			gtk_tree_store_set(model, &iter3,
				INPUT_PARAMETER_COLUMN, "element",
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			g_free(buffer2);
			buffer = AtomicNumberToSymbol(input->detector->crystal_layers[i].Z[j]);
			buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/element[%i]/atomic_number", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "atomic_number",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				-1
			);
			xrlFree(buffer);
			g_free(buffer2);
			buffer = g_strdup_printf("%lg", input->detector->crystal_layers[i].weight[j]*100.0);
			buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/element[%i]/weight_fraction", i+1, j+1);
			gtk_tree_store_append(model, &iter4, &iter3);
			gtk_tree_store_set(model, &iter4,
				INPUT_PARAMETER_COLUMN, "weight_fraction",
				INPUT_VALUE_COLUMN, buffer,
				INPUT_SELECTABLE_COLUMN, input->detector->crystal_layers[i].n_elements > 1 ? TRUE : FALSE,
				INPUT_XPATH_COLUMN, buffer2,
				INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_WEIGHT_FRACTION,
				-1
			);
			g_free(buffer);
			g_free(buffer2);
		}

		buffer = g_strdup_printf("%lg", input->detector->crystal_layers[i].density);
		buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/density", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "density",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);

		buffer = g_strdup_printf("%lg", input->detector->crystal_layers[i].thickness);
		buffer2 = g_strdup_printf("/xmimsim/detector/crystal/layer[%i]/thickness", i+1);
		gtk_tree_store_append(model, &iter3, &iter2);
		gtk_tree_store_set(model, &iter3,
			INPUT_PARAMETER_COLUMN, "thickness",
			INPUT_VALUE_COLUMN, buffer,
			INPUT_SELECTABLE_COLUMN, TRUE,
			INPUT_XPATH_COLUMN, buffer2,
			INPUT_ALLOWED_COLUMN, PARAMETER_DOUBLE | PARAMETER_STRICT_POSITIVE,
			-1
		);
		g_free(buffer);
		g_free(buffer2);
	}
	return scrolled_window;
}

static void get_fluor_data(struct xmi_archive *archive, struct fluor_data **fdo, int *nfdo) {

	gboolean found;
	int loc = 0;
	int i,j,k,l;
	struct fluor_data *fd = NULL;
	int nfd = 0;

	for (i = 0 ; i <= archive->nsteps ; i++) {
		for (j = 0 ; j < archive->output[i]->nvar_red_history ; j++) {
			found = FALSE;
			for (k = 0 ; k < nfd ; k++) {
				if (archive->output[i]->var_red_history[j].atomic_number == fd[k].atomic_number) {
					found = TRUE;
					loc = k;
					break;
				}
			}
			if (!found) {
				//add new element to array
				fd = g_realloc(fd, sizeof(struct fluor_data)*++nfd);
				fd[nfd-1].atomic_number = archive->output[i]->var_red_history[j].atomic_number;
				fd[nfd-1].n_lines = archive->output[i]->var_red_history[j].n_lines;
				fd[nfd-1].line_types = g_malloc(sizeof(gchar *)*fd[nfd-1].n_lines);
				for (k = 0 ; k < fd[nfd-1].n_lines ; k++) {
					fd[nfd-1].line_types[k]= g_strdup(archive->output[i]->var_red_history[j].lines[k].line_type);
				}
			}
			else {
				//element found -> check for new lines
				for (k = 0 ; k < archive->output[i]->var_red_history[j].n_lines ; k++) {
					found = FALSE;
					for (l = 0 ; l < fd[loc].n_lines ; l++) {
						if (strcmp(archive->output[i]->var_red_history[j].lines[k].line_type, fd[loc].line_types[l]) != 0) {
							found = TRUE;
							break;
						}		
					}
					if (!found) {
						//extend array
						fd[loc].line_types = g_realloc(fd[loc].line_types, sizeof(gchar *)*++fd[loc].n_lines);
						fd[loc].line_types[fd[loc].n_lines-1] = g_strdup(archive->output[i]->var_red_history[j].lines[k].line_type);
					}
				}
			}
		}
		for (j = 0 ; j < archive->output[i]->nbrute_force_history ; j++) {
			found = FALSE;
			for (k = 0 ; k < nfd ; k++) {
				if (archive->output[i]->brute_force_history[j].atomic_number == fd[k].atomic_number) {
					found = TRUE;
					loc = k;
					break;
				}
			}
			if (!found) {
				//add new element to array
				fd = g_realloc(fd, sizeof(struct fluor_data)*++nfd);
				fd[nfd-1].atomic_number = archive->output[i]->brute_force_history[j].atomic_number;
				fd[nfd-1].n_lines = archive->output[i]->brute_force_history[j].n_lines;
				fd[nfd-1].line_types = g_malloc(sizeof(gchar *)*fd[nfd-1].n_lines);
				for (k = 0 ; k < fd[nfd-1].n_lines ; k++) {
					fd[nfd-1].line_types[k]= g_strdup(archive->output[i]->brute_force_history[j].lines[k].line_type);
				}
			}
			else {
				//element found -> check for new lines
				for (k = 0 ; k < archive->output[i]->brute_force_history[j].n_lines ; k++) {
					found = FALSE;
					for (l = 0 ; l < fd[loc].n_lines ; l++) {
						if (strcmp(archive->output[i]->brute_force_history[j].lines[k].line_type, fd[loc].line_types[l]) != 0) {
							found = TRUE;
							break;
						}		
					}
					if (!found) {
						//extend array
						fd[loc].line_types = g_realloc(fd[loc].line_types, sizeof(gchar *)*++fd[loc].n_lines);
						fd[loc].line_types[fd[loc].n_lines-1] = g_strdup(archive->output[i]->brute_force_history[j].lines[k].line_type);
					}
				}
			}
		}
	}
	//qsort everything
	for (i = 0 ; i < nfd ; i++) {
		qsort(fd[i].line_types, fd[i].n_lines, sizeof(gchar *), compare_string);
	}
	qsort(fd, nfd, sizeof(struct fluor_data), compare_fluor_data);

	*fdo = fd;
	*nfdo = nfd;
	return;
}


struct archive_plot_data {
	GtkWidget *roi_radioW;
	GtkWidget *roi_start_spinnerW;
	GtkWidget *roi_start_labelW;
	GtkWidget *roi_end_spinnerW;
	GtkWidget *roi_end_labelW;
	GtkWidget *roi_conv_radioW;
	GtkWidget *roi_unconv_radioW;
	GtkWidget *roi_interactions_labelW;
	GtkWidget *roi_interactions_comboW;
	GtkWidget *roi_cumulative_radioW;
	GtkWidget *roi_individual_radioW;
	GtkWidget *xrf_radioW;
	GtkWidget *xrf_element_labelW;
	GtkWidget *xrf_element_comboW;
	GtkWidget *xrf_line_labelW;
	GtkWidget *xrf_line_comboW;
	GtkWidget *xrf_interactions_labelW;
	GtkWidget *xrf_interactions_comboW;
	GtkWidget *xrf_cumulative_radioW;
	GtkWidget *xrf_individual_radioW;
	GtkWidget *okButton;
	GtkWidget *saveButton;
	GtkWidget *canvas;
	struct xmi_archive *archive;
	struct fluor_data *fd;
	int nfd;
};

static void plot_archive_data_cb(struct archive_plot_data *apd) {
	//first section will deal with generating the x- and y-values
	double *x, *y;
	int i,j,k,l;
	gchar *buffer;

	x = g_malloc(sizeof(double)*(apd->archive->nsteps+1));
	y = g_malloc(sizeof(double)*(apd->archive->nsteps+1));
	
	for (i = 0 ; i <= apd->archive->nsteps ; i++) {
		x[i] = apd->archive->start_value + (apd->archive->end_value - apd->archive->start_value)*i/apd->archive->nsteps;
	}


	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(apd->roi_radioW))) {
		//ROI mode
		gboolean cumulative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(apd->roi_cumulative_radioW));
		gboolean convoluted = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(apd->roi_conv_radioW));
		gint start_channel = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(apd->roi_start_spinnerW));
		gint end_channel = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(apd->roi_end_spinnerW));
#if GTK_CHECK_VERSION(2,24,0)
		buffer = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(apd->roi_interactions_comboW));
#else
		buffer = gtk_combo_box_get_active_text(GTK_COMBO_BOX(apd->roi_interactions_comboW));
#endif
		gint interaction = strtol(buffer, NULL, 10);
		g_free(buffer);

		for (i = 0 ; i <= apd->archive->nsteps ; i++) {
			double yval = 0.0;
			if (cumulative) {
				for (j = apd->archive->output[i]->use_zero_interactions ? 0 : 1 ; j <= interaction ; j++) {
					for (k = start_channel ; k <= end_channel ; k++) {
						if (convoluted)
							yval += apd->archive->output[i]->channels_conv[j][k];
						else
							yval += apd->archive->output[i]->channels_unconv[j][k];
					}
				}
			}
			else {
				for (k = start_channel ; k <= end_channel ; k++) {
					if (convoluted)
						yval += apd->archive->output[i]->channels_conv[interaction][k];
					else
						yval += apd->archive->output[i]->channels_unconv[interaction][k];
				}
			}
			y[i] = yval;	
		}
	}
	else {
		//XRF mode
		gboolean var_red;
		struct xmi_fluorescence_line_counts **history = NULL; 
		int *nhistory = NULL;

		if (apd->archive->output[0]->nvar_red_history > 0) {
			var_red = TRUE;
			nhistory = g_malloc(sizeof(int)*(apd->archive->nsteps+1)); 
			history = g_malloc(sizeof(struct xmi_fluorescence_line_counts *)*(apd->archive->nsteps+1));
			for (i = 0 ; i <= apd->archive->nsteps ; i++) {
				history[i] = apd->archive->output[i]->var_red_history;
				nhistory[i] = apd->archive->output[i]->nvar_red_history;
			}
		}
		else if (apd->archive->output[0]->nbrute_force_history > 0) {
			var_red = FALSE;
			nhistory = g_malloc(sizeof(int)*(apd->archive->nsteps+1)); 
			history = g_malloc(sizeof(struct xmi_fluorescence_line_counts *)*(apd->archive->nsteps+1));
			for (i = 0 ; i <= apd->archive->nsteps ; i++) {
				history[i] = apd->archive->output[i]->brute_force_history;
				nhistory[i] = apd->archive->output[i]->nbrute_force_history;
			}
		}
		gboolean cumulative = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(apd->xrf_cumulative_radioW));
		gchar *line_type = NULL;
		int atomic_number_index;
		if (gtk_combo_box_get_active(GTK_COMBO_BOX(apd->xrf_line_comboW)) == 0)
			line_type = NULL;
#if GTK_CHECK_VERSION(2,24,0)
		else 
			line_type = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(apd->xrf_line_comboW));
		buffer = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(apd->xrf_interactions_comboW));
#else
		else 
			line_type = gtk_combo_box_get_active_text(GTK_COMBO_BOX(apd->xrf_line_comboW));
		buffer = gtk_combo_box_get_active_text(GTK_COMBO_BOX(apd->xrf_interactions_comboW));
#endif
		gint interaction = strtol(buffer, NULL, 10);
		g_free(buffer);
		atomic_number_index = gtk_combo_box_get_active(GTK_COMBO_BOX(apd->xrf_element_comboW))-1;

		for (i = 0 ; i <= apd->archive->nsteps ; i++) {
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
						if (history[i][j].atomic_number == apd->fd[atomic_number_index].atomic_number) {
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
						if (history[i][j].atomic_number == apd->fd[atomic_number_index].atomic_number) {
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
		}
	}

	//y values have been calculated -> plot
	GtkPlotCanvasChild *child;

	GList *list;
	list = GTK_PLOT_CANVAS(apd->canvas)->childs;
	while (list) {
		child = GTK_PLOT_CANVAS_CHILD(list->data);
		gtk_plot_canvas_remove_child(GTK_PLOT_CANVAS(apd->canvas), child);
		list = GTK_PLOT_CANVAS(apd->canvas)->childs;
	}
	//add box with default settings
	GtkWidget *plot_window;
	plot_window = gtk_plot_new_with_size(NULL,.65,.45);
	gtk_plot_set_background(GTK_PLOT(plot_window),&white_plot);
	gtk_plot_hide_legends(GTK_PLOT(plot_window));

	double real_ymax = xmi_maxval_double(y,apd->archive->nsteps+1);
	double real_ymin = xmi_minval_double(y,apd->archive->nsteps+1);

	double plot_ymax = real_ymax + (real_ymax-real_ymin)/10.0;
	double plot_ymin = real_ymin - (real_ymax-real_ymin)/10.0;
	double plot_xmin = x[0] - (x[apd->archive->nsteps]-x[0])/10.0;
	double plot_xmax = x[apd->archive->nsteps] + (x[apd->archive->nsteps]-x[0])/10.0;


	if (real_ymax == 0.0) {
		//if y is zero everywhere
		plot_ymax = 1.0;
		plot_ymin = 0.0;
	}

	/* need a clever algorithm here */
	/* number of ticks should be at least 2 and at most 5 */
	double tickstep = 1E-10;
	double nticks = floor((plot_ymax-plot_ymin)/tickstep);

	while (nticks < 1 || nticks >= 10) {
		tickstep *= 10.0;
		nticks = floor((plot_ymax-plot_ymin)/tickstep);
	} 

	if (nticks == 1.0) {
		tickstep /= 5.0;
	}

	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_Y,tickstep,5);

	tickstep = 1E-10;
	nticks = floor((plot_xmax-plot_xmin)/tickstep);

	while (nticks < 1 || nticks >= 10) {
		tickstep *= 10.0;
		nticks = floor((plot_xmax-plot_xmin)/tickstep);
	} 

	if (nticks == 1.0) {
		tickstep /= 5.0;
	}

	gtk_plot_set_ticks(GTK_PLOT(plot_window), GTK_PLOT_AXIS_X,tickstep,5);

	gtk_plot_set_range(GTK_PLOT(plot_window),plot_xmin, plot_xmax, plot_ymin, plot_ymax);
	gtk_plot_clip_data(GTK_PLOT(plot_window), TRUE);
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP));
	gtk_plot_axis_hide_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT));
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Intensity");
	gtk_plot_axis_set_title(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM), apd->archive->xpath);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",30,90,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	gtk_plot_axis_title_set_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",30,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	
	if (plot_ymax < 10000.0 && plot_ymin > 1.0) {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_FLOAT,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_FLOAT,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",25,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",25,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}
	else {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),GTK_PLOT_LABEL_EXP,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_LEFT),"Helvetica",20,0,NULL,NULL,TRUE,GTK_JUSTIFY_RIGHT);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_RIGHT),"Helvetica",20,0,NULL,NULL,TRUE,GTK_JUSTIFY_LEFT);
	}

	if (plot_xmax < 10000.0 && plot_xmin > 1.0) {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),GTK_PLOT_LABEL_FLOAT,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_FLOAT,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",25,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",25,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	}
	else {
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),GTK_PLOT_LABEL_EXP,1);
        	gtk_plot_axis_set_labels_style(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),GTK_PLOT_LABEL_EXP,1);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),"Helvetica",20,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
		gtk_plot_axis_set_labels_attributes(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_BOTTOM),"Helvetica",20,0,NULL,NULL,TRUE,GTK_JUSTIFY_CENTER);
	}


	gtk_plot_axis_show_labels(gtk_plot_get_axis(GTK_PLOT(plot_window), GTK_PLOT_AXIS_TOP),0);
        gtk_plot_grids_set_visible(GTK_PLOT(plot_window),TRUE,FALSE,TRUE,FALSE);

	child = gtk_plot_canvas_plot_new(GTK_PLOT(plot_window));
        gtk_plot_canvas_put_child(GTK_PLOT_CANVAS(apd->canvas), child, .15,.05,.90,.85);
        gtk_widget_show(plot_window);

	GtkPlotData *dataset;
	dataset = GTK_PLOT_DATA(gtk_plot_data_new());
	gtk_plot_add_data(GTK_PLOT(plot_window),dataset);
	gtk_plot_data_set_numpoints(dataset, apd->archive->nsteps+1);
	gtk_plot_data_set_x(dataset, x);
	gtk_plot_data_set_y(dataset, y);
	gtk_widget_show(GTK_WIDGET(dataset));
	gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,2,&blue_plot);
	gtk_plot_data_set_symbol(dataset, GTK_PLOT_SYMBOL_CIRCLE, GTK_PLOT_SYMBOL_FILLED, 5, 1.0, &red_plot, &red_plot);
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(apd->canvas));
	gtk_widget_queue_draw(GTK_WIDGET(apd->canvas));
	gtk_plot_canvas_refresh(GTK_PLOT_CANVAS(apd->canvas));
	gtk_plot_paint(GTK_PLOT(plot_window));
	gtk_plot_refresh(GTK_PLOT(plot_window),NULL);

	return;
}

static void save_archive_plot(GtkButton *saveButton, GtkWidget *canvas) {
	export_canvas_image(canvas, "Export plot as");
}

static void roi_spin_button_changed_cb(GtkSpinButton *roi_start_spinnerW, struct archive_plot_data *apd) {
	//warning: will have to disable temporarily the plot_archive_data_cb on roi_end_spinnerW
	gint value_start = gtk_spin_button_get_value_as_int(roi_start_spinnerW);
	gint value_end = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(apd->roi_end_spinnerW));

	if (value_end < value_start) {
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(apd->roi_end_spinnerW), (double) value_start);
		return;
	}
	plot_archive_data_cb(apd);
	return;
}

static void xrf_element_changed_cb(GtkComboBox *xrf_element_comboW, struct archive_plot_data *apd) {
	//clear xrf_line_comboW
	int i;

	g_signal_handlers_block_by_func(G_OBJECT(apd->xrf_line_comboW), plot_archive_data_cb, apd);

	for (i = gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(apd->xrf_line_comboW)),NULL)-1 ;  i > 0; i--) {
#if GTK_CHECK_VERSION(2,24,0)
		gtk_combo_box_text_remove(GTK_COMBO_BOX_TEXT(apd->xrf_line_comboW), i);
#else
		gtk_combo_box_remove_text(GTK_COMBO_BOX(apd->xrf_line_comboW), i);
#endif
	}

	if (gtk_combo_box_get_active(xrf_element_comboW) == 0) {
		//ALL elements selected
		gtk_widget_set_sensitive(apd->xrf_line_comboW, FALSE);
	}
	else {
		gtk_widget_set_sensitive(apd->xrf_line_comboW, TRUE);
		int i = gtk_combo_box_get_active(GTK_COMBO_BOX(xrf_element_comboW))-1;
		int j;
		for (j = 0 ; j < apd->fd[i].n_lines ; j++) {
#if GTK_CHECK_VERSION(2,24,0)
			gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(apd->xrf_line_comboW), apd->fd[i].line_types[j]);
#else
			gtk_combo_box_append_text(GTK_COMBO_BOX(apd->xrf_line_comboW), apd->fd[i].line_types[j]);
#endif
		}


	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(apd->xrf_line_comboW), 0);
	g_signal_handlers_unblock_by_func(G_OBJECT(apd->xrf_line_comboW), plot_archive_data_cb, apd);
	plot_archive_data_cb(apd);
	return;
}

static void roi_xrf_toggled_cb(GtkToggleButton *roi_radioW, struct archive_plot_data *apd) {
	if (gtk_toggle_button_get_active(roi_radioW)) {
		//ROI mode
		gtk_widget_set_sensitive(apd->roi_start_spinnerW, TRUE);
		gtk_widget_set_sensitive(apd->roi_start_labelW, TRUE);
		gtk_widget_set_sensitive(apd->roi_end_spinnerW, TRUE);
		gtk_widget_set_sensitive(apd->roi_end_labelW, TRUE);
		gtk_widget_set_sensitive(apd->roi_conv_radioW, TRUE);
		gtk_widget_set_sensitive(apd->roi_unconv_radioW, TRUE);
		gtk_widget_set_sensitive(apd->roi_interactions_labelW, TRUE);
		gtk_widget_set_sensitive(apd->roi_interactions_comboW, TRUE);
		gtk_widget_set_sensitive(apd->roi_cumulative_radioW, TRUE);
		gtk_widget_set_sensitive(apd->roi_individual_radioW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_element_labelW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_element_comboW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_line_labelW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_line_comboW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_interactions_labelW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_interactions_comboW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_cumulative_radioW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_individual_radioW, FALSE);
	}
	else {
		//XRF mode
		gtk_widget_set_sensitive(apd->roi_start_spinnerW, FALSE);
		gtk_widget_set_sensitive(apd->roi_start_labelW, FALSE);
		gtk_widget_set_sensitive(apd->roi_end_spinnerW, FALSE);
		gtk_widget_set_sensitive(apd->roi_end_labelW, FALSE);
		gtk_widget_set_sensitive(apd->roi_conv_radioW, FALSE);
		gtk_widget_set_sensitive(apd->roi_unconv_radioW, FALSE);
		gtk_widget_set_sensitive(apd->roi_interactions_labelW, FALSE);
		gtk_widget_set_sensitive(apd->roi_interactions_comboW, FALSE);
		gtk_widget_set_sensitive(apd->roi_cumulative_radioW, FALSE);
		gtk_widget_set_sensitive(apd->roi_individual_radioW, FALSE);
		gtk_widget_set_sensitive(apd->xrf_element_labelW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_element_comboW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_line_labelW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_interactions_labelW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_interactions_comboW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_cumulative_radioW, TRUE);
		gtk_widget_set_sensitive(apd->xrf_individual_radioW, TRUE);
		if (gtk_combo_box_get_active(GTK_COMBO_BOX(apd->xrf_element_comboW)) > 0)
			gtk_widget_set_sensitive(apd->xrf_line_comboW, TRUE);
		else
			gtk_widget_set_sensitive(apd->xrf_line_comboW, FALSE);
	}


	plot_archive_data_cb(apd);
	return;
}

static gboolean resize_canvas_cb(GtkWidget *canvas, GdkEvent *event, struct canvas_data *cd) {
	gdouble magnifier_height;
	gdouble magnifier_width;


	gint height = canvas->allocation.height;
	gint width = canvas->allocation.width;

	if (width == cd->width && height == cd->height)
		return FALSE;

	magnifier_height = (gdouble) height/(gdouble) GTK_PLOT_A4_W;	
	magnifier_width = (gdouble) width/(gdouble) GTK_PLOT_A4_H;	

	gtk_plot_canvas_set_magnification(GTK_PLOT_CANVAS(canvas),MAX(magnifier_height, magnifier_width));

	gtk_widget_queue_resize(GTK_WIDGET(canvas));
	gtk_plot_canvas_paint(GTK_PLOT_CANVAS(canvas));
	gtk_widget_queue_draw(GTK_WIDGET(canvas));

	cd->height = canvas->allocation.height;
	cd->width = canvas->allocation.width;


	return FALSE;


}

void launch_archive_plot(struct xmi_archive *archive, GtkWidget *main_window) {
	//on quitting this window -> free archive (lot of memory)
	GtkWidget *roi_radioW;
	GtkWidget *roi_start_spinnerW;
	GtkWidget *roi_start_labelW;
	GtkWidget *roi_end_spinnerW;
	GtkWidget *roi_end_labelW;
	GtkWidget *roi_conv_radioW;
	GtkWidget *roi_unconv_radioW;
	GtkWidget *roi_interactions_labelW;
	GtkWidget *roi_interactions_comboW;
	GtkWidget *roi_cumulative_radioW;
	GtkWidget *roi_individual_radioW;
	GtkWidget *xrf_radioW;
	GtkWidget *xrf_element_labelW;
	GtkWidget *xrf_element_comboW;
	GtkWidget *xrf_line_labelW;
	GtkWidget *xrf_line_comboW;
	GtkWidget *xrf_interactions_labelW;
	GtkWidget *xrf_interactions_comboW;
	GtkWidget *xrf_cumulative_radioW;
	GtkWidget *xrf_individual_radioW;
	GtkWidget *okButton;
	GtkWidget *saveButton;
	
	//find all unique elements and lines
	int i;
	gchar *interaction;
	struct fluor_data *fd = NULL;
	int nfd = 0;
	get_fluor_data(archive, &fd, &nfd);
	/*for (i = 0 ; i < nfd ; i++) {
		g_fprintf(stdout, "Element: %i\n", fd[i].atomic_number);
		for (j = 0 ; j < fd[i].n_lines ; j++) {
			g_fprintf(stdout, "\tLine: %s\n", fd[i].line_types[j]);
		}
	}*/
	GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Batch mode plot");
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);

	GtkWidget *mainHBox = gtk_hbox_new(FALSE, 2);
	gtk_container_set_border_width(GTK_CONTAINER(mainHBox),5);
	gtk_container_add(GTK_CONTAINER(window), mainHBox);

	GtkWidget *mainVBox = gtk_vbox_new(FALSE, 2);
	gtk_box_pack_start(GTK_BOX(mainHBox), mainVBox, FALSE, FALSE, 1);
	GtkWidget *lilVBox;
	GtkWidget *lilHBox;
	GtkWidget *align;
	
	lilVBox = gtk_vbox_new(FALSE, 2);
	roi_radioW= gtk_radio_button_new_from_widget(NULL);
	GtkWidget *label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<b>Region of interest integration</b>");
	gtk_container_add(GTK_CONTAINER(roi_radioW), label);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(roi_radioW), FALSE);
	gtk_box_pack_start(GTK_BOX(lilVBox), roi_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_hbox_new(FALSE, 2);
	roi_start_labelW = gtk_label_new("First channel");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_start_labelW);
	gtk_box_pack_start(GTK_BOX(lilHBox), align, FALSE, FALSE, 3);
	roi_start_spinnerW = gtk_spin_button_new_with_range(0, archive->output[0]->nchannels, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(roi_start_spinnerW), GTK_UPDATE_IF_VALID);
	gtk_box_pack_end(GTK_BOX(lilHBox), roi_start_spinnerW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	lilHBox = gtk_hbox_new(FALSE, 2);
	roi_end_labelW = gtk_label_new("Last channel");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_end_labelW);
	gtk_box_pack_start(GTK_BOX(lilHBox), align, FALSE, FALSE, 3);
	roi_end_spinnerW = gtk_spin_button_new_with_range(0, archive->output[0]->nchannels, 1);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(roi_end_spinnerW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(roi_end_spinnerW), archive->output[0]->nchannels);
	gtk_box_pack_end(GTK_BOX(lilHBox), roi_end_spinnerW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	roi_conv_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "Use convoluted spectra");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(roi_conv_radioW), TRUE);
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_conv_radioW);
	gtk_box_pack_start(GTK_BOX(lilVBox), align, FALSE, FALSE, 2);
	
	roi_unconv_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(roi_conv_radioW), "Use unconvoluted spectra");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_unconv_radioW);
	gtk_box_pack_start(GTK_BOX(lilVBox), align, FALSE, FALSE, 2);

	lilHBox = gtk_hbox_new(FALSE, 2);
	roi_interactions_labelW = gtk_label_new("Number of interactions");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_interactions_labelW);
	gtk_box_pack_start(GTK_BOX(lilHBox), align, FALSE, FALSE, 3);
#if GTK_CHECK_VERSION(2,24,0)
	roi_interactions_comboW = gtk_combo_box_text_new();
	if (archive->output[0]->use_zero_interactions)
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(roi_interactions_comboW), "0");
	for (i = 1 ; i <= archive->output[0]->ninteractions ; i++) {
		interaction = g_strdup_printf("%i", i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(roi_interactions_comboW), interaction);
		g_free(interaction);
	}
#else
	roi_interactions_comboW = gtk_combo_box_new_text();
	if (archive->output[0]->use_zero_interactions)
		gtk_combo_box_append_text(GTK_COMBO_BOX(roi_interactions_comboW), "0");
	for (i = 1 ; i <= archive->output[0]->ninteractions ; i++) {
		interaction = g_strdup_printf("%i", i);
		gtk_combo_box_append_text(GTK_COMBO_BOX(roi_interactions_comboW), interaction);
		g_free(interaction);
	}
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(roi_interactions_comboW), gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(roi_interactions_comboW)),NULL)-1);
	gtk_box_pack_end(GTK_BOX(lilHBox), roi_interactions_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	roi_cumulative_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "Cumulative interaction contributions");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_cumulative_radioW);
	gtk_box_pack_start(GTK_BOX(lilVBox), align, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(roi_cumulative_radioW), TRUE);
	roi_individual_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(roi_cumulative_radioW), "Individual interaction contributions");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), roi_individual_radioW);
	gtk_box_pack_start(GTK_BOX(lilVBox), align, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(mainVBox), lilVBox, TRUE, FALSE, 2);

	lilVBox = gtk_vbox_new(FALSE, 2);
	xrf_radioW= gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(roi_radioW));
	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), "<b>X-ray fluorescence lines</b>");
	gtk_container_add(GTK_CONTAINER(xrf_radioW), label);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(xrf_radioW), TRUE);
	gtk_box_pack_start(GTK_BOX(lilVBox), xrf_radioW, FALSE, FALSE, 2);

	lilHBox = gtk_hbox_new(FALSE, 2);
	xrf_element_labelW = gtk_label_new("Element");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), xrf_element_labelW);
	gtk_box_pack_start(GTK_BOX(lilHBox), align, FALSE, FALSE, 3);
	gchar *element;
#if GTK_CHECK_VERSION(2,24,0)
	xrf_element_comboW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(xrf_element_comboW), "All");
	for (i = 0 ; i < nfd ; i++) {
		element = AtomicNumberToSymbol(fd[i].atomic_number);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(xrf_element_comboW), element);
		xrlFree(element);
	}
#else
	xrf_element_comboW = gtk_combo_box_new_text();
	gtk_combo_box_append_text(GTK_COMBO_BOX(xrf_element_comboW), "All");
	for (i = 0 ; i < nfd ; i++) {
		element = AtomicNumberToSymbol(fd[i].atomic_number);
		gtk_combo_box_append_text(GTK_COMBO_BOX(xrf_element_comboW), element);
		xrlFree(element);
	}
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(xrf_element_comboW), 0);
	gtk_box_pack_end(GTK_BOX(lilHBox), xrf_element_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);
	
	lilHBox = gtk_hbox_new(FALSE, 2);
	xrf_line_labelW = gtk_label_new("XRF line");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), xrf_line_labelW);
	gtk_box_pack_start(GTK_BOX(lilHBox), align, FALSE, FALSE, 3);
#if GTK_CHECK_VERSION(2,24,0)
	xrf_line_comboW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(xrf_line_comboW), "All");
#else
	xrf_line_comboW = gtk_combo_box_new_text();
	gtk_combo_box_append_text(GTK_COMBO_BOX(xrf_line_comboW), "All");
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(xrf_line_comboW), 0);
	gtk_box_pack_end(GTK_BOX(lilHBox), xrf_line_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);
	
	lilHBox = gtk_hbox_new(FALSE, 2);
	xrf_interactions_labelW = gtk_label_new("Number of interactions");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), xrf_interactions_labelW);
	gtk_box_pack_start(GTK_BOX(lilHBox), align, FALSE, FALSE, 3);
#if GTK_CHECK_VERSION(2,24,0)
	xrf_interactions_comboW = gtk_combo_box_text_new();
	for (i = 1 ; i <= archive->output[0]->ninteractions ; i++) {
		interaction = g_strdup_printf("%i", i);
		gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(xrf_interactions_comboW), interaction);
		g_free(interaction);
	}
#else
	xrf_interactions_comboW = gtk_combo_box_new_text();
	for (i = 1 ; i <= archive->output[0]->ninteractions ; i++) {
		interaction = g_strdup_printf("%i", i);
		gtk_combo_box_append_text(GTK_COMBO_BOX(xrf_interactions_comboW), interaction);
		g_free(interaction);
	}
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX(xrf_interactions_comboW), gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(xrf_interactions_comboW)),NULL)-1);
	gtk_box_pack_end(GTK_BOX(lilHBox), xrf_interactions_comboW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(lilVBox), lilHBox, FALSE, FALSE, 2);

	xrf_cumulative_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "Cumulative interaction contributions");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), xrf_cumulative_radioW);
	gtk_box_pack_start(GTK_BOX(lilVBox), align, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(xrf_cumulative_radioW), TRUE);
	xrf_individual_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(xrf_cumulative_radioW), "Individual interaction contributions");
	align = gtk_alignment_new(1, 1, 1, 1);
	gtk_alignment_set_padding(GTK_ALIGNMENT(align), 0, 0, 20, 0);
	gtk_container_add(GTK_CONTAINER(align), xrf_individual_radioW);
	gtk_box_pack_start(GTK_BOX(lilVBox), align, FALSE, FALSE, 2);

	gtk_box_pack_start(GTK_BOX(mainVBox), lilVBox, TRUE, FALSE, 2);

	lilHBox = gtk_hbox_new(TRUE, 2);
	okButton = gtk_button_new_from_stock(GTK_STOCK_OK);
	gtk_box_pack_start(GTK_BOX(lilHBox), okButton, FALSE, FALSE, 2);
	saveButton = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_box_pack_start(GTK_BOX(lilHBox), saveButton, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(mainVBox), lilHBox, FALSE, FALSE, 2);
	gtk_box_pack_end(GTK_BOX(mainVBox), gtk_hseparator_new(), FALSE, FALSE, 4);


	//canvas
	GtkWidget *canvas = gtk_plot_canvas_new(GTK_PLOT_A4_H, GTK_PLOT_A4_W, 1.0);
	GTK_PLOT_CANVAS_UNSET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_CAN_SELECT | GTK_PLOT_CANVAS_CAN_SELECT_ITEM); //probably needs to be unset when initializing, but set when data is available
	gtk_plot_canvas_set_background(GTK_PLOT_CANVAS(canvas),&white_plot);
	gtk_box_pack_start(GTK_BOX(mainHBox),canvas,FALSE,FALSE,2);
	

	

	//default sensitivities
	gtk_widget_set_sensitive(xrf_line_comboW, FALSE);
	gtk_widget_set_sensitive(roi_start_spinnerW, FALSE);
	gtk_widget_set_sensitive(roi_start_labelW, FALSE);
	gtk_widget_set_sensitive(roi_end_spinnerW, FALSE);
	gtk_widget_set_sensitive(roi_end_labelW, FALSE);
	gtk_widget_set_sensitive(roi_conv_radioW, FALSE);
	gtk_widget_set_sensitive(roi_unconv_radioW, FALSE);
	gtk_widget_set_sensitive(roi_interactions_labelW, FALSE);
	gtk_widget_set_sensitive(roi_interactions_comboW, FALSE);
	gtk_widget_set_sensitive(roi_cumulative_radioW, FALSE);
	gtk_widget_set_sensitive(roi_individual_radioW, FALSE);


	struct archive_plot_data *apd = g_malloc(sizeof(struct archive_plot_data));
	apd->roi_radioW = roi_radioW;
	apd->roi_start_spinnerW = roi_start_spinnerW;
	apd->roi_start_labelW = roi_start_labelW;
	apd->roi_end_spinnerW = roi_end_spinnerW;
	apd->roi_end_labelW = roi_end_labelW;
	apd->roi_conv_radioW = roi_conv_radioW;
	apd->roi_unconv_radioW = roi_unconv_radioW;
	apd->roi_interactions_labelW = roi_interactions_labelW;
	apd->roi_interactions_comboW = roi_interactions_comboW;
	apd->roi_cumulative_radioW = roi_cumulative_radioW;
	apd->roi_individual_radioW = roi_individual_radioW;
	apd->xrf_radioW = xrf_radioW;
	apd->xrf_element_labelW = xrf_element_labelW;
	apd->xrf_element_comboW = xrf_element_comboW;
	apd->xrf_line_labelW = xrf_line_labelW;
	apd->xrf_line_comboW = xrf_line_comboW;
	apd->xrf_interactions_labelW = xrf_interactions_labelW;
	apd->xrf_interactions_comboW = xrf_interactions_comboW;
	apd->xrf_cumulative_radioW = xrf_cumulative_radioW;
	apd->xrf_individual_radioW = xrf_individual_radioW;
	apd->okButton = okButton;
	apd->saveButton = saveButton;
	apd->canvas = canvas;
	apd->archive = archive;
	apd->nfd = nfd;
	apd->fd = fd;

	//callbacks registration
	g_signal_connect(G_OBJECT(roi_radioW), "toggled", G_CALLBACK(roi_xrf_toggled_cb), apd);
	g_signal_connect(G_OBJECT(xrf_element_comboW), "changed", G_CALLBACK(xrf_element_changed_cb), apd);
	g_signal_connect(G_OBJECT(roi_start_spinnerW), "value-changed", G_CALLBACK(roi_spin_button_changed_cb), apd);

	g_signal_connect_swapped(G_OBJECT(roi_end_spinnerW), "value-changed", G_CALLBACK(plot_archive_data_cb), apd);
	g_signal_connect_swapped(G_OBJECT(roi_conv_radioW), "toggled", G_CALLBACK(plot_archive_data_cb), apd);
	g_signal_connect_swapped(G_OBJECT(roi_interactions_comboW), "changed", G_CALLBACK(plot_archive_data_cb), apd);
	g_signal_connect_swapped(G_OBJECT(roi_cumulative_radioW), "toggled", G_CALLBACK(plot_archive_data_cb), apd);
	g_signal_connect_swapped(G_OBJECT(xrf_line_comboW), "changed", G_CALLBACK(plot_archive_data_cb), apd);
	g_signal_connect_swapped(G_OBJECT(xrf_interactions_comboW), "changed", G_CALLBACK(plot_archive_data_cb), apd);
	g_signal_connect_swapped(G_OBJECT(xrf_cumulative_radioW), "toggled", G_CALLBACK(plot_archive_data_cb), apd);

	g_signal_connect_swapped(G_OBJECT(okButton), "clicked", G_CALLBACK(gtk_widget_destroy), window);
	g_signal_connect_swapped(G_OBJECT(window), "destroy", G_CALLBACK(xmi_free_archive), apd->archive);

	g_signal_connect(G_OBJECT(saveButton), "clicked", G_CALLBACK(save_archive_plot), canvas);
	/*struct canvas_data *cd = g_malloc(sizeof(struct canvas_data));
	cd->width = 0.0;
	cd->height = 0.0;
	g_signal_connect(G_OBJECT(canvas),"expose-event", G_CALLBACK(resize_canvas_cb), cd);
	*/
	gtk_widget_show_all(window);
	plot_archive_data_cb(apd);
}
