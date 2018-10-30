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
#include "xmimsim-gui-batch.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-xmsi-scrolled-window.h"
#include "xmimsim-gui-options-box.h"
#include "xmimsim-gui-xmsa-viewer-window.h"
#include "xmimsim-gui-long-task-window.h"
#include "xmi_job.h"
#include <stdio.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <libxml/xmlmemory.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_private.h"
#include <math.h>
#include <string.h>
#include <unistd.h>

#ifdef MAC_INTEGRATION
	#include "xmi_resources_mac.h"
#endif


#if LIBXML_VERSION < 20901
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
int xmlXPathSetContextNode(xmlNodePtr node, xmlXPathContextPtr ctx);
xmlXPathObjectPtr xmlXPathNodeEval(xmlNodePtr node, const xmlChar *str, xmlXPathContextPtr ctx);

#endif

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmi_google_analytics.h"
#endif

enum xmi_msim_batch_options{
	XMI_MSIM_BATCH_MULTIPLE_OPTIONS,
	XMI_MSIM_BATCH_ONE_OPTION
};

struct batch_window_data {
	int *rv;
	GtkWidget *batch_window;
	GtkWidget *playButton;
	GtkWidget *stopButton;
	GtkWidget *pauseButton;
	GtkWidget *nthreadsW;
	GtkWidget *progressbarW;
	GtkWidget *controlsLogW;
	GtkTextBuffer *controlsLogB;
	GtkWidget *saveButton;
	GtkWidget *controlsLogFileW;
	GtkWidget *verboseW;
	GtkWidget *exitButton;
	xmi_main_options **options;
	GSList *filenames;
	enum xmi_msim_batch_options batch_options;
	GTimer *timer;
	GFileOutputStream *logFile;
	gchar *xmimsim_executable;
	xmi_output ***output;
	gchar **filenames_xmso;
	int nsteps2;
	XmiMsimJob *job;
	GMainLoop *main_loop;
};

struct archive_options_data {
	double start_value1;
	double end_value1;
	int nsteps1;
	double start_value2;
	double end_value2;
	int nsteps2;
	gchar *xmsa_file;
	xmi_main_options *xmo;
};

struct wizard_range_data {
	GtkWidget *wizard;
	GtkWidget *start1Entry;
	GtkWidget *end1Entry;
	GtkWidget *nsteps1Entry;
	GtkWidget *start2Entry;
	GtkWidget *end2Entry;
	GtkWidget *nsteps2Entry;
	int allowed1;
	int allowed2;
	GtkWidget *labela;
	GtkWidget *archiveEntry;
	GtkWidget *archivesaveButton;
	gboolean same_layer;
};

struct wizard_archive_close_data {
	struct wizard_range_data *wrd;
	GtkWidget *ow;
	struct archive_options_data *aod;
	int *rv;
};

static void wizard_archive_cancel(GtkAssistant *wizard, struct wizard_archive_close_data *wacd) {
	*(wacd->rv) = 0;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static void wizard_archive_close(GtkAssistant *wizard, struct wizard_archive_close_data *wacd) {

	//first read the general options
	wacd->aod->xmo = xmi_msim_gui_options_box_get_options(XMI_MSIM_GUI_OPTIONS_BOX(wacd->ow));

	//range parameters
	wacd->aod->start_value1 = strtod(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->start1Entry)), NULL);
	wacd->aod->end_value1 = strtod(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->end1Entry)), NULL);
	wacd->aod->nsteps1 = (int) strtol(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->nsteps1Entry)), NULL, 10);
	if (wacd->wrd->start2Entry) {
		wacd->aod->start_value2 = strtod(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->start2Entry)), NULL);
		wacd->aod->end_value2 = strtod(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->end2Entry)), NULL);
		wacd->aod->nsteps2 = (int) strtol(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->nsteps2Entry)), NULL, 10);
	}
	else {
		wacd->aod->start_value2 = 0.0;
		wacd->aod->end_value2 = 0.0;
		wacd->aod->nsteps2 = 0;
	}
	wacd->aod->xmsa_file = g_strdup(gtk_entry_get_text(GTK_ENTRY(wacd->wrd->archiveEntry)));


	*(wacd->rv) = 1;
	gtk_widget_destroy(GTK_WIDGET(wizard));
	return;
}

static gboolean wizard_archive_delete_event(GtkWidget *wizard, GdkEvent *event, struct wizard_archive_close_data *wacd) {
	*(wacd->rv) = 0;
	return FALSE;
}

static void archivesaveButton_clicked_cb(GtkButton *saveButton, GtkEntry *archiveEntry) {
	XmiMsimGuiFileChooserDialog *dialog  = xmi_msim_gui_file_chooser_dialog_new(
		"Select the filename of the XMSA file",
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(saveButton))),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel");
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsa");
	gtk_file_filter_add_pattern(filter,"*.XMSA");
	gtk_file_filter_set_name(filter,"XMI-MSIM archive files");
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(dialog), gtk_entry_get_text(GTK_ENTRY(archiveEntry)));
	gchar *filename;

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		xmi_msim_gui_utils_ensure_extension(&filename, ".xmsa");
		gtk_entry_set_text(archiveEntry, filename);
		g_free (filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
	return;
}

static void wizard_range_changed_cb(GtkEditable *entry, struct wizard_range_data *wrd) {
	GtkWidget *vbox = gtk_assistant_get_nth_page(GTK_ASSISTANT(wrd->wizard), gtk_assistant_get_current_page(GTK_ASSISTANT(wrd->wizard)));
	int start_end1 = 0;
	int nsteps1 = 0;
	int start_end2 = 0;
	int nsteps2 = 0;
	int one_and_two = 0;

	char *textPtr,*endPtr,*lastPtr;
	double start, end;

	GtkStyleContext *style_context_start1Entry = gtk_widget_get_style_context(GTK_WIDGET(wrd->start1Entry));
	GtkStyleContext *style_context_end1Entry = gtk_widget_get_style_context(GTK_WIDGET(wrd->end1Entry));
	GtkStyleContext *style_context_nsteps1Entry = gtk_widget_get_style_context(GTK_WIDGET(wrd->nsteps1Entry));

	if (!wrd->same_layer)
		one_and_two = 1;

	if (entry == GTK_EDITABLE(wrd->start1Entry)) {
		double value;

		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->start1Entry));
		value=strtod(textPtr, &endPtr);
		lastPtr = textPtr + strlen(textPtr);
		if (	(strlen(textPtr) == 0 || lastPtr != endPtr) ||
			((wrd->allowed1 & PARAMETER_STRICT_POSITIVE) && value <= 0.0) ||
			((wrd->allowed1 & PARAMETER_POSITIVE) && value < 0.0) ||
			((wrd->allowed1 & PARAMETER_WEIGHT_FRACTION) && (value < 0.0 || value >= 100.0))
			) {
      			gtk_style_context_add_class(style_context_start1Entry, "red");
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
      		gtk_style_context_remove_class(style_context_start1Entry, "red");
	}
	else if (entry == GTK_EDITABLE(wrd->end1Entry)) {
		double value;

		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->end1Entry));
		value=strtod(textPtr, &endPtr);
		lastPtr = textPtr + strlen(textPtr);
		if (	(strlen(textPtr) == 0 || lastPtr != endPtr) ||
			((wrd->allowed1 & PARAMETER_STRICT_POSITIVE) && value <= 0.0) ||
			((wrd->allowed1 & PARAMETER_POSITIVE) && value <= 0.0) ||
			((wrd->allowed1 & PARAMETER_WEIGHT_FRACTION) && (value < 0.0 || value > 100.0))
			) {
      			gtk_style_context_add_class(style_context_end1Entry, "red");
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
      		gtk_style_context_remove_class(style_context_end1Entry, "red");
	}
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->nsteps1Entry));
	nsteps1=strtol(textPtr, &endPtr, 10);
	lastPtr = textPtr + strlen(textPtr);
	if (entry == GTK_EDITABLE(wrd->nsteps1Entry) && (strlen(textPtr) == 0 || lastPtr != endPtr|| nsteps1 < 1)) {
      		gtk_style_context_add_class(style_context_nsteps1Entry, "red");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}
	else if (strlen(textPtr) > 0 && lastPtr == endPtr && nsteps1 >= 1) {
      		gtk_style_context_remove_class(style_context_nsteps1Entry, "red");
	}
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->start1Entry));
	start = strtod(textPtr, &endPtr);
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->end1Entry));
	end = strtod(textPtr, &endPtr);

	if (end > start) {
		start_end1 = 1;
      		gtk_style_context_remove_class(style_context_start1Entry, "red");
      		gtk_style_context_remove_class(style_context_end1Entry, "red");
	}
	else {
      		gtk_style_context_add_class(style_context_start1Entry, "red");
      		gtk_style_context_add_class(style_context_end1Entry, "red");
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}

	if (wrd->start2Entry) {
		GtkStyleContext *style_context_start2Entry = gtk_widget_get_style_context(GTK_WIDGET(wrd->start2Entry));
		GtkStyleContext *style_context_end2Entry = gtk_widget_get_style_context(GTK_WIDGET(wrd->end2Entry));
		GtkStyleContext *style_context_nsteps2Entry = gtk_widget_get_style_context(GTK_WIDGET(wrd->nsteps2Entry));

		if (entry == GTK_EDITABLE(wrd->start2Entry)) {
			double value;

			textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->start2Entry));
			value=strtod(textPtr, &endPtr);
			lastPtr = textPtr + strlen(textPtr);
			if (	(strlen(textPtr) == 0 || lastPtr != endPtr) ||
				((wrd->allowed2 & PARAMETER_STRICT_POSITIVE) && value <= 0.0) ||
				((wrd->allowed2 & PARAMETER_POSITIVE) && value < 0.0) ||
				((wrd->allowed2 & PARAMETER_WEIGHT_FRACTION) && (value < 0.0 || value >= 100.0))
				) {
      				gtk_style_context_add_class(style_context_start2Entry, "red");
				gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
				return;
			}
      			gtk_style_context_remove_class(style_context_start2Entry, "red");
		}
		else if (entry == GTK_EDITABLE(wrd->end2Entry)) {
			double value;

			textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->end2Entry));
			value=strtod(textPtr, &endPtr);
			lastPtr = textPtr + strlen(textPtr);
			if (	(strlen(textPtr) == 0 || lastPtr != endPtr) ||
				((wrd->allowed2 & PARAMETER_STRICT_POSITIVE) && value <= 0.0) ||
				((wrd->allowed2 & PARAMETER_POSITIVE) && value <= 0.0) ||
				((wrd->allowed2 & PARAMETER_WEIGHT_FRACTION) && (value < 0.0 || value > 100.0))
				) {
      				gtk_style_context_add_class(style_context_end2Entry, "red");
				gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
				return;
			}
      			gtk_style_context_remove_class(style_context_end2Entry, "red");
		}
		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->nsteps2Entry));
		nsteps2=strtol(textPtr, &endPtr, 10);
		lastPtr = textPtr + strlen(textPtr);
		if (entry == GTK_EDITABLE(wrd->nsteps2Entry) && (strlen(textPtr) == 0 || lastPtr != endPtr|| nsteps2 < 1)) {
      			gtk_style_context_add_class(style_context_nsteps2Entry, "red");
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		else if (strlen(textPtr) > 0 && lastPtr == endPtr && nsteps2 >= 1) {
      			gtk_style_context_remove_class(style_context_nsteps2Entry, "red");
		}
		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->start2Entry));
		start = strtod(textPtr, &endPtr);
		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->end2Entry));
		end = strtod(textPtr, &endPtr);

		if (end > start) {
			start_end2 = 1;
      			gtk_style_context_remove_class(style_context_start2Entry, "red");
      			gtk_style_context_remove_class(style_context_end2Entry, "red");
		}
		else {
      			gtk_style_context_add_class(style_context_start2Entry, "red");
      			gtk_style_context_add_class(style_context_end2Entry, "red");
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		if (wrd->same_layer) {
			//in this case the sum of the end values must be less than or equal to 100
			double value1 = strtod(gtk_entry_get_text(GTK_ENTRY(wrd->end1Entry)), NULL);
			double value2 = strtod(gtk_entry_get_text(GTK_ENTRY(wrd->end2Entry)), NULL);
			if (value1 + value2 > 100.0) {
      				gtk_style_context_add_class(style_context_start1Entry, "red");
      				gtk_style_context_add_class(style_context_end1Entry, "red");
      				gtk_style_context_add_class(style_context_start2Entry, "red");
      				gtk_style_context_add_class(style_context_end2Entry, "red");
				gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
				return;
			}
			else {
      				gtk_style_context_remove_class(style_context_start1Entry, "red");
      				gtk_style_context_remove_class(style_context_end1Entry, "red");
      				gtk_style_context_remove_class(style_context_start2Entry, "red");
      				gtk_style_context_remove_class(style_context_end2Entry, "red");
				one_and_two = 1;
			}
		}
	}
	else {
		start_end2 = 1;
		nsteps2 = 1;
	}

	if (start_end1*nsteps1*start_end2*nsteps2*one_and_two > 0) {
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, TRUE);
	}
	else {
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
	}
	return;
}

static int archive_options(GtkWidget *main_window, xmi_input *input, gchar *filename, gchar *xpath1, gchar *xpath2, int allowed1, int allowed2, struct archive_options_data *aod) {
	int rv = 0;
	GtkWidget *wizard = gtk_assistant_new();
	gtk_window_set_transient_for(GTK_WINDOW(wizard), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(wizard), TRUE);
	gtk_window_set_position (GTK_WINDOW(wizard), GTK_WIN_POS_CENTER);

	gtk_window_set_title(GTK_WINDOW(wizard), "Simulation options");
	//add intro page
	GtkWidget *introLabel = gtk_label_new("Use this wizard to set the simulation options and to set the range of values that the selected parameter(s) will assume.\nAfterwards, the batch simulation interface will be produced: launch the simulations by clicking the \"\xe2\x96\xb8\" button.\nFinally an interface will be produced that will allow the user to inspect the results of the simulations.");
	gtk_label_set_line_wrap(GTK_LABEL(introLabel), TRUE);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), introLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), introLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), introLabel, GTK_ASSISTANT_PAGE_INTRO);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), introLabel, "Introduction");

	//general options
	GtkWidget *ow = xmi_msim_gui_options_box_new();
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), ow);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), ow, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), ow, "General options");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), ow, TRUE);

	//set range
	struct wizard_range_data *wrd = (struct wizard_range_data *) g_malloc(sizeof(struct wizard_range_data));
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), vbox);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), vbox, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), vbox, "Select parameter ranges and file location");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), vbox, TRUE);
	GtkWidget *hbox, *label;

	gchar *buffer;
	if (xpath2)
		buffer = g_strdup_printf("<b>XPath parameter 1: %s</b>", xpath1);
	else
		buffer = g_strdup_printf("<b>XPath parameter: %s</b>", xpath1);

	label = gtk_label_new(NULL);
	gtk_label_set_markup(GTK_LABEL(label), buffer);
	g_free(buffer);
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 10);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 3);

	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	label = gtk_label_new("Start");
	GtkWidget *start1Entry = gtk_entry_new();
	gtk_widget_set_name(start1Entry, "color_entry");
	gtk_editable_set_editable(GTK_EDITABLE(start1Entry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), start1Entry, TRUE, TRUE, 1);
	label = gtk_label_new("End");
	GtkWidget *end1Entry = gtk_entry_new();
	gtk_widget_set_name(end1Entry, "color_entry");
	gtk_editable_set_editable(GTK_EDITABLE(end1Entry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), end1Entry, TRUE, TRUE, 1);
	label = gtk_label_new("#Steps");
	GtkWidget *nsteps1Entry = gtk_entry_new();
	gtk_widget_set_name(nsteps1Entry, "color_entry");
	gtk_editable_set_editable(GTK_EDITABLE(nsteps1Entry), TRUE);
	gtk_entry_set_text(GTK_ENTRY(nsteps1Entry), "10");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), nsteps1Entry, TRUE, TRUE, 1);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);

	GtkWidget *start2Entry = NULL;
	GtkWidget *end2Entry = NULL;
	GtkWidget *nsteps2Entry = NULL;

	if (xpath2 != NULL) {
		buffer = g_strdup_printf("<b>XPath parameter 2: %s</b>", xpath2);
		label = gtk_label_new(NULL);
		gtk_label_set_markup(GTK_LABEL(label), buffer);
		g_free(buffer);
		hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 10);
		gtk_container_set_border_width(GTK_CONTAINER(vbox), 3);

		hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
		label = gtk_label_new("Start");
		start2Entry = gtk_entry_new();
		gtk_widget_set_name(start2Entry, "color_entry");
		gtk_editable_set_editable(GTK_EDITABLE(start2Entry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), start2Entry, TRUE, TRUE, 1);

		label = gtk_label_new("End");
		end2Entry = gtk_entry_new();
		gtk_widget_set_name(end2Entry, "color_entry");
		gtk_editable_set_editable(GTK_EDITABLE(end2Entry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), end2Entry, TRUE, TRUE, 1);

		label = gtk_label_new("#Steps");
		nsteps2Entry = gtk_entry_new();
		gtk_widget_set_name(nsteps2Entry, "color_entry");
		gtk_editable_set_editable(GTK_EDITABLE(nsteps2Entry), TRUE);
		gtk_entry_set_text(GTK_ENTRY(nsteps2Entry), "10");
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), nsteps2Entry, TRUE, TRUE, 1);
		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);
	}



	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	GtkWidget *labela = gtk_label_new("XMSA file");
	gtk_box_pack_start(GTK_BOX(hbox), labela, FALSE, FALSE, 2);
	GtkWidget *archiveEntry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(archiveEntry), FALSE);
	gchar *xmsafile = g_strdup(input->general->outputfile);
	xmsafile[strlen(xmsafile)-1] = 'a';
	gtk_entry_set_text(GTK_ENTRY(archiveEntry), xmsafile);
	g_free(xmsafile);
	gtk_box_pack_start(GTK_BOX(hbox), archiveEntry, TRUE, TRUE, 2);
	GtkWidget *archivesaveButton = gtk_button_new_with_mnemonic("_Save As");
	g_signal_connect(G_OBJECT(archivesaveButton), "clicked", G_CALLBACK(archivesaveButton_clicked_cb), (gpointer) archiveEntry);
	gtk_box_pack_start(GTK_BOX(hbox), archivesaveButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 1);

	xmlDocPtr doc;
	if ((doc = xmlReadFile(filename,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		g_critical("xmlReadFile error for %s", filename);
		return 0;
	}
	xmlXPathContextPtr context;
	xmlXPathObjectPtr result, result2;
	xmlNodeSetPtr nodeset, nodeset2;

	context = xmlXPathNewContext(doc);
	if (context == NULL) {
		g_critical("Error in xmlXPathNewContext");
		return 0;
	}
	result = xmlXPathEvalExpression(BAD_CAST xpath1, context);
	if (result == NULL) {
		g_critical("Error in xmlXPathEvalExpression");
		return 0;
	}
	if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
		xmlXPathFreeObject(result);
                g_critical("No result");
		return 0;
	}
	nodeset = result->nodesetval;
	if (nodeset->nodeNr != 1) {
		g_critical("More than one result found for xpath expression");
		return 0;
	}

	gboolean same_layer = FALSE;
	if (xpath2) {
		result2 = xmlXPathEvalExpression(BAD_CAST xpath2, context);
		if (result2 == NULL) {
			g_critical("Error in xmlXPathEvalExpression");
			return 0;
		}
		if(xmlXPathNodeSetIsEmpty(result2->nodesetval)){
			xmlXPathFreeObject(result2);
        	        g_critical("No result");
			return 0;
		}
		nodeset2 = result2->nodesetval;
		if (nodeset2->nodeNr != 1) {
			g_critical("More than one result found for xpath expression");
			return 0;
		}
		//check if they are weight_fractions and if they belong to the same layer
		if ((allowed1 & PARAMETER_WEIGHT_FRACTION) && (allowed2 & PARAMETER_WEIGHT_FRACTION) && nodeset->nodeTab[0]->parent->parent == nodeset2->nodeTab[0]->parent->parent) {
			same_layer = TRUE;
		}
	}

	xmlXPathFreeContext(context);

	gchar *keyword = (gchar *) xmlNodeListGetString(doc, nodeset->nodeTab[0]->children, 1);
	xmlXPathFreeObject (result);
	double valued;
	//int valuei;
	//long int valuel;
	double inc;

	if (same_layer) {
		if ((allowed1 & PARAMETER_DOUBLE) && (allowed2 & PARAMETER_DOUBLE)) {
			valued = strtod(keyword, NULL);
			buffer = g_strdup_printf("%g", valued);
			gtk_entry_set_text(GTK_ENTRY(start1Entry), buffer);
			g_free(buffer);
			xmlFree(keyword);
			keyword = (gchar *) xmlNodeListGetString(doc, nodeset2->nodeTab[0]->children, 1);
			//valued = strtod(keyword, NULL) + 1.0;
			double valued2 = strtod(keyword, NULL);
			buffer = g_strdup_printf("%g", valued2);
			gtk_entry_set_text(GTK_ENTRY(start2Entry), buffer);
			g_free(buffer);
			inc = 1.0;
			while (valued+valued2+2*inc >= 100.0) {
				inc/=10.0;
			}
			buffer = g_strdup_printf("%g", valued+inc);
			gtk_entry_set_text(GTK_ENTRY(end1Entry), buffer);
			g_free(buffer);
			buffer = g_strdup_printf("%g", valued2+inc);
			gtk_entry_set_text(GTK_ENTRY(end2Entry), buffer);
			g_free(buffer);
		}
	}
	else {
		if (allowed1 & PARAMETER_DOUBLE) {
			//valued = strtod(keyword, NULL) + 1.0;
			valued = strtod(keyword, NULL);
			buffer = g_strdup_printf("%g", valued);
			gtk_entry_set_text(GTK_ENTRY(start1Entry), buffer);
			g_free(buffer);
			inc = 1.0;
			if (allowed1 & PARAMETER_WEIGHT_FRACTION) {
				while (valued+inc >= 100.0) {
				inc/=10.0;
				}
			}
			buffer = g_strdup_printf("%g", valued+inc);
			gtk_entry_set_text(GTK_ENTRY(end1Entry), buffer);
			g_free(buffer);
		}
		else {
			g_critical("only PARAMETER_DOUBLE is allowed for now");
			return 0;
		}
		if (xpath2 && (allowed2 & PARAMETER_DOUBLE)) {
			xmlFree(keyword);
			keyword = (gchar *) xmlNodeListGetString(doc, nodeset2->nodeTab[0]->children, 1);
			//valued = strtod(keyword, NULL) + 1.0;
			valued = strtod(keyword, NULL);
			buffer = g_strdup_printf("%g", valued);
			gtk_entry_set_text(GTK_ENTRY(start2Entry), buffer);
			g_free(buffer);
			inc = 1.0;
			if (allowed2 & PARAMETER_WEIGHT_FRACTION) {
				while (valued+inc >= 100.0) {
				inc/=10.0;
				}
			}
			buffer = g_strdup_printf("%g", valued+inc);
			gtk_entry_set_text(GTK_ENTRY(end2Entry), buffer);
			g_free(buffer);
		}
		else if (xpath2) {
			g_critical("only PARAMETER_DOUBLE is allowed for now");
			return 0;
		}
	}
	xmlFree(keyword);
	xmlFreeDoc(doc);


	wrd->wizard = wizard;
	wrd->start1Entry = start1Entry;
	wrd->end1Entry = end1Entry;
	wrd->nsteps1Entry = nsteps1Entry;
	wrd->start2Entry = start2Entry;
	wrd->end2Entry = end2Entry;
	wrd->nsteps2Entry = nsteps2Entry;
	wrd->allowed1 = allowed1;
	wrd->allowed2 = allowed2;
	wrd->archiveEntry = archiveEntry;
	wrd->same_layer = same_layer;

	g_signal_connect(G_OBJECT(start1Entry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);
	g_signal_connect(G_OBJECT(end1Entry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);
	g_signal_connect(G_OBJECT(nsteps1Entry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);
	if (xpath2) {
		g_signal_connect(G_OBJECT(start2Entry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);
		g_signal_connect(G_OBJECT(end2Entry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);
		g_signal_connect(G_OBJECT(nsteps2Entry), "changed", G_CALLBACK(wizard_range_changed_cb), (gpointer) wrd);
	}

	//add confirmation page
	GtkWidget *confirmationLabel = gtk_label_new("Confirm the options selected on the previous pages and continue with the simulation?");
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), confirmationLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), confirmationLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), confirmationLabel, GTK_ASSISTANT_PAGE_CONFIRM);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), confirmationLabel, "Confirmation");

	struct wizard_archive_close_data *wacd = (struct wizard_archive_close_data *) g_malloc(sizeof(struct wizard_archive_close_data));
	wacd->wrd = wrd;
	wacd->aod = aod;
	wacd->ow = ow;
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
	GtkTreeIter iter;
	GtkTreeModel *model;
	GList *paths;

	gint count = gtk_tree_selection_count_selected_rows(selection);

	if (count == 1) {
		gboolean selectable;
		//one row selected
		//get row
		paths = gtk_tree_selection_get_selected_rows(selection, &model);
		GtkTreePath *path = (GtkTreePath *) g_list_nth_data(paths, 0);
		gtk_tree_model_get_iter(model, &iter, path);

		gtk_tree_model_get(model, &iter, INPUT_SELECTABLE_COLUMN, &selectable, -1);
		g_list_free_full(paths, (GDestroyNotify) gtk_tree_path_free);

		if (selectable)
			gtk_widget_set_sensitive(okButton, TRUE);
		else
			gtk_widget_set_sensitive(okButton, FALSE);
	}
	else if (count == 2) {
		gboolean selectable1, selectable2;
		int allowed1, allowed2;
		GtkTreePath *path1, *path2;
		//two rows selected
		paths = gtk_tree_selection_get_selected_rows(selection, &model);
		path1 = (GtkTreePath *) g_list_nth_data(paths, 0);
		gtk_tree_model_get_iter(model, &iter, path1);

		gtk_tree_model_get(model, &iter, INPUT_SELECTABLE_COLUMN, &selectable1, INPUT_ALLOWED_COLUMN, &allowed1, -1);
		path2 = (GtkTreePath *) g_list_nth_data(paths, 1);
		gtk_tree_model_get_iter(model, &iter, path2);

		gtk_tree_model_get(model, &iter, INPUT_SELECTABLE_COLUMN, &selectable2, INPUT_ALLOWED_COLUMN, &allowed2, -1);

		if (selectable1 && selectable2 && (allowed1 & PARAMETER_WEIGHT_FRACTION) && (allowed2 & PARAMETER_WEIGHT_FRACTION)) {
			//possible problem here
			GtkTreePath *path1up, *path2up;
			path1up = gtk_tree_path_copy(path1);
			gtk_tree_path_up(path1up);
			gtk_tree_path_up(path1up);
			path2up = gtk_tree_path_copy(path2);
			gtk_tree_path_up(path2up);
			gtk_tree_path_up(path2up);
			gtk_tree_model_get_iter(model, &iter, path2up);
			if (gtk_tree_path_compare(path1up, path2up) == 0 && gtk_tree_model_iter_n_children(model, &iter) < 5) {
				//5 because density and thickness are also children
				//aha! not allowed...
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(okButton)), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "When selecting two weight fractions within the same layer, the number of elements in that layer must be at least three.");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				gtk_widget_set_sensitive(okButton, FALSE);
			}
			else {
				gtk_widget_set_sensitive(okButton, TRUE);

			}
			gtk_tree_path_free(path1up);
			gtk_tree_path_free(path2up);
		}
		else if (selectable1 && selectable2) {
			gtk_widget_set_sensitive(okButton, TRUE);
		}
		else {
			gtk_widget_set_sensitive(okButton, FALSE);
		}

		g_list_free_full(paths, (GDestroyNotify) gtk_tree_path_free);
	}
	else if (count == 0) {
		//no rows selected
		gtk_widget_set_sensitive(okButton, FALSE);
	}
	else {
		//too many rows selected
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gtk_widget_get_toplevel(okButton)), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Please select either one or two rows.");
		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		gtk_widget_set_sensitive(okButton, FALSE);
	}
	return;
}

static void parameter_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, GtkButton *okButton) {

	if (gtk_tree_view_row_expanded(tree_view, path)) {
		gtk_tree_view_collapse_row(tree_view, path);
	}
	else {
		gtk_tree_view_expand_row(tree_view, path, FALSE);
	}

	return;
}

static int select_parameter(GtkWidget *window, xmi_input *input, gchar **xpath1, gchar **xpath2, int *allowed1, int *allowed2) {
	int rv = 0;
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Select one or two variable parameters", GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), "_Ok", GTK_RESPONSE_ACCEPT, "_Cancel", GTK_RESPONSE_REJECT, NULL);
	GtkWidget *scrolled_window = xmi_msim_gui_xmsi_scrolled_window_new(input, TRUE);

	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	gtk_container_add(GTK_CONTAINER(content_area), scrolled_window);
	gtk_widget_show_all(scrolled_window);
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 3);
	gtk_window_set_default_size(GTK_WINDOW(dialog), 500, 500);
	GtkTreeView* treeview = xmi_msim_gui_xmsi_scrolled_window_get_tree_view(XMI_MSIM_GUI_XMSI_SCROLLED_WINDOW(scrolled_window));
	GtkWidget *okButton = gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

	gtk_widget_set_sensitive(okButton, FALSE);
	GtkTreeSelection *select = gtk_tree_view_get_selection(treeview);
	gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE);
	g_signal_connect(G_OBJECT(select), "changed",
			G_CALLBACK(parameter_selection_changed_cb),
			(gpointer) okButton
		);
	g_signal_connect(G_OBJECT(treeview), "row-activated", G_CALLBACK(parameter_row_activated_cb), (gpointer) okButton);



	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		rv = 1;
		GtkTreeIter iter;
		GtkTreeModel *model;
		//gtk_tree_selection_get_selected(select, &model, &iter);
		GList *paths = gtk_tree_selection_get_selected_rows(select, &model);
		GtkTreePath *path = (GtkTreePath *) g_list_nth_data(paths, 0);
		gtk_tree_model_get_iter(model, &iter, path);
		//extract xpath expression
		gtk_tree_model_get(model, &iter, INPUT_XPATH_COLUMN, xpath1, INPUT_ALLOWED_COLUMN, allowed1, -1);
		if ((path = (GtkTreePath *) g_list_nth_data(paths, 1)) == NULL) {
			*xpath2 = NULL;
			*allowed2 = 0;
		}
		else {
			gtk_tree_model_get_iter(model, &iter, path);
			gtk_tree_model_get(model, &iter, INPUT_XPATH_COLUMN, xpath2, INPUT_ALLOWED_COLUMN, allowed2, -1);
		}
		g_list_free_full(paths, (GDestroyNotify) gtk_tree_path_free);
	}
	gtk_widget_destroy(dialog);

	return rv;
}

static int batch_mode(GtkWidget * main_window, xmi_main_options **options, GSList *filenames, enum xmi_msim_batch_options, xmi_output ***output, gchar **filenames_xmso, int nsteps2);

static void batch_reset_controls(struct batch_window_data *bwd) {
	GtkTextIter start, end;

	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Start simulation");
	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 0.0);

	//clear textbuffer
	gtk_text_buffer_get_start_iter (bwd->controlsLogB,&start);
	gtk_text_buffer_get_end_iter (bwd->controlsLogB,&end);
	gtk_text_buffer_delete (bwd->controlsLogB,&start,&end);
}


static void choose_logfile(GtkButton *saveButton, struct batch_window_data *bwd) {
	XmiMsimGuiFileChooserDialog *dialog;
	gchar *filename;

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select a filename for the logfile",
		GTK_WINDOW(bwd->batch_window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		"_Save",
		"_Cancel");
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);
	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(bwd->controlsLogFileW), filename);
		g_free (filename);
	}
	xmi_msim_gui_file_chooser_dialog_destroy(dialog);
	return;
}

static void job_stderr_cb(XmiMsimJob *job, const gchar *string, struct batch_window_data *bwd) {
	gchar *buffer = g_strdup_printf("%s\n", string);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	if (bwd->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
	}
	g_free(buffer);
}

static void job_stdout_cb(XmiMsimJob *job, const gchar *string, struct batch_window_data *bwd) {
	gchar *buffer = g_strdup_printf("%s\n", string);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,NULL);
	if (bwd->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
	}
	g_free(buffer);
}

static void job_finished_cb(XmiMsimJob *job, gboolean result, const gchar *string, struct batch_window_data *bwd) {
	gchar *buffer = g_strdup_printf("%s\n", string);
	if (result) {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB), "success"), NULL);
	}
	else {
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB), "error"), NULL);
	}

	if (bwd->logFile) {
		g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
	}
	g_free(buffer);
	g_main_loop_quit(bwd->main_loop);
}

static void play_button_clicked(GtkButton *button, struct batch_window_data *bwd) {


	//first deal with the pause case
	if (bwd->pauseButton && bwd->job && xmi_msim_job_is_suspended(bwd->job)) {
		gint pid;
		gtk_widget_set_sensitive(bwd->playButton, FALSE);
		gboolean resume_rv;
		char *buffer;
		GError *error = NULL;
		g_timer_continue(bwd->timer);

		xmi_msim_job_get_pid(bwd->job, &pid, NULL); // let's assume this won't fail...

		resume_rv = xmi_msim_job_resume(bwd->job, &error);

		if (resume_rv) {
			buffer = g_strdup_printf( "Process %d was successfully resumed\n", pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
			if (bwd->logFile) {
				g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
			}
			gtk_widget_set_sensitive(bwd->pauseButton,TRUE);
		}
		else {
			buffer = g_strdup_printf( "Process %d could not be resumed\n", pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
			}
			gtk_widget_set_sensitive(bwd->playButton,TRUE);
			*(bwd->rv) = 0;
			//should end batch operation!
		}
		g_free(buffer);
		return;
	}
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
		GFile *g_file = g_file_new_for_path(logFileName);
		GError *error = NULL;
		if ((bwd->logFile = g_file_replace(g_file, NULL, FALSE, G_FILE_CREATE_REPLACE_DESTINATION, NULL, &error)) == NULL) {
			g_object_unref(g_file);
			//could not write to logfile
			GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
				(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Error writing to log file %s: %s",
				logFileName,
				error->message
                	);
			g_error_free(error);
     			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			*(bwd->rv) = 0;
			gtk_widget_destroy(bwd->batch_window);
			return;
		}
	}

	gchar *xmimsim_executable;

#ifdef MAC_INTEGRATION
	if (xmi_resources_mac_query(XMI_RESOURCES_MAC_XMIMSIM_EXEC, &xmimsim_executable) == 0) {
		xmimsim_executable = NULL;
	}
#else
	xmimsim_executable = g_find_program_in_path("xmimsim");
#endif

	bwd->xmimsim_executable = xmimsim_executable;

	// big for loop over all jobs
	int file_index;
	gboolean global_success = TRUE;
	for (file_index = 0 ; file_index < g_slist_length(bwd->filenames) ; file_index++) {
		gchar *pbartext = g_strdup_printf("File %i/%i",file_index, g_slist_length(bwd->filenames));
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), pbartext);
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), (double) file_index/(double) g_slist_length(bwd->filenames));
		while(gtk_events_pending())
			gtk_main_iteration();
		g_free(pbartext);

		int j;
		if (bwd->batch_options == XMI_MSIM_BATCH_MULTIPLE_OPTIONS)
			j = file_index;
		else
			j = 0;

		xmi_main_options *options = bwd->options[j];

		gint verbose_level = gtk_combo_box_get_active(GTK_COMBO_BOX(bwd->verboseW));

		if (gtk_combo_box_get_active(GTK_COMBO_BOX(bwd->verboseW)) == 0) {
			options->verbose = 1;
		}
		else {
			options->extra_verbose = 1;
		}

		GError *error = NULL;
		gchar *buffer;

		options->omp_num_threads = (int) gtk_range_get_value(GTK_RANGE(bwd->nthreadsW));

		bwd->job = xmi_msim_job_new(xmimsim_executable, (const char *) g_slist_nth_data(bwd->filenames, file_index), options, NULL, NULL, NULL, NULL, NULL, &error);

		if (error != NULL) {
			buffer = g_strdup_printf("%s\n", error->message);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
			}
			g_error_free(error);
			//Error dialog
			GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
				(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Batch execution could not be started %s",buffer
                	);
     			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			*(bwd->rv) = 0;
			gtk_widget_destroy(bwd->batch_window);

			global_success = FALSE;

			break;
		}
		xmi_msim_job_send_all_stdout_events(bwd->job, TRUE); // we are going to use only the STDOUT_EVENT signals really so...

		// hook up signal handlers
		bwd->main_loop = g_main_loop_new(NULL, FALSE);
		g_signal_connect(G_OBJECT(bwd->job), "stdout-event", G_CALLBACK(job_stdout_cb), bwd);
		g_signal_connect(G_OBJECT(bwd->job), "stderr-event", G_CALLBACK(job_stderr_cb), bwd);
		g_signal_connect(G_OBJECT(bwd->job), "finished-event", G_CALLBACK(job_finished_cb), bwd);

		// start the job
		if (!xmi_msim_job_start(bwd->job, &error)) {
			buffer = g_strdup_printf("%s\n", error->message);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
			}
			g_error_free(error);
			//Error dialog
			GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
				(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Batch execution could not be started %s",buffer
                	);
     			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			*(bwd->rv) = 0;
			gtk_widget_destroy(bwd->batch_window);

			global_success = FALSE;

			break;
		}

		int pid;
		xmi_msim_job_get_pid(bwd->job, &pid, NULL); // let's assume this won't fail...
		buffer = g_strdup_printf("%s was started with process id %d\n", xmimsim_executable, pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,NULL);
		if (bwd->logFile) {
			g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
		}
		g_free(buffer);

		if (bwd->pauseButton)
			gtk_widget_set_sensitive(bwd->pauseButton,TRUE);

		gtk_widget_set_sensitive(bwd->stopButton,TRUE);

		// start the loop
		g_main_loop_run(bwd->main_loop);
		g_main_loop_unref(bwd->main_loop);


		if (!xmi_msim_job_was_successful(bwd->job)) {
			g_object_unref(bwd->job);
			global_success = FALSE;

			break;
		}

		if (bwd->filenames_xmso != NULL) {
			int step1 = file_index / (bwd->nsteps2+1);
			int step2 = file_index % (bwd->nsteps2+1);
			if (xmi_read_output_xml(bwd->filenames_xmso[file_index], &bwd->output[step1][step2], &error) == 0) {
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(bwd->batch_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading output-file %s. Aborting batch mode", bwd->filenames_xmso[file_index]);
				gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
				g_error_free(error);
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			else {
				g_unlink(bwd->filenames_xmso[file_index]);
				g_unlink((char *) g_slist_nth_data(bwd->filenames, file_index));
			}
		}
		g_object_unref(bwd->job);
	}

	bwd->job = NULL;
	g_free(xmimsim_executable);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
	if (bwd->pauseButton)
		gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
	if (bwd->logFile)
		g_object_unref(bwd->logFile);
	bwd->logFile = NULL;

	if (global_success) {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations completed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
	        GTK_MESSAGE_INFO,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution succeeded");
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 1;
		gtk_widget_destroy(bwd->batch_window);
	
	}
	else {
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations failed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
	        GTK_MESSAGE_ERROR,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution failed!"
	        );
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 0;
		gtk_widget_destroy(bwd->batch_window);
	}

	return;
}

static void stop_button_clicked(GtkButton *button, struct batch_window_data *bwd) {
	gchar *buffer;
	gboolean kill_rv;
	GError *error = NULL;
	gint pid;

	xmi_msim_job_get_pid(bwd->job, &pid, NULL);

	gtk_widget_set_sensitive(bwd->stopButton,FALSE);

	if (bwd->pauseButton)
		gtk_widget_set_sensitive(bwd->pauseButton,FALSE);

	kill_rv = xmi_msim_job_stop(bwd->job, &error);

	if (kill_rv == TRUE) {
		buffer = g_strdup_printf( "Process %d was successfully terminated before completion\n", pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %d could not be terminated: %s\n", pid, error->message);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		g_error_free(error);
	}

	if (bwd->logFile)
		g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);

	g_free(buffer);

	return;
}

static void pause_button_clicked(GtkButton *button, struct batch_window_data *bwd) {
	gchar *buffer;
	gboolean kill_rv;
	GError *error = NULL;
	gint pid;

	g_timer_stop(bwd->timer);

	xmi_msim_job_get_pid(bwd->job, &pid, NULL);

	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);

	kill_rv = xmi_msim_job_suspend(bwd->job, &error);

	if (kill_rv == TRUE) {
		buffer = g_strdup_printf( "Process %d was successfully paused. Press the Play button to continue or Stop to kill the process\n", pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
		if (bwd->logFile)
			g_output_stream_write(G_OUTPUT_STREAM(bwd->logFile), buffer, strlen(buffer), NULL, NULL);
		gtk_widget_set_sensitive(bwd->stopButton,TRUE);
		gtk_widget_set_sensitive(bwd->playButton,TRUE);
		g_free(buffer);
	}
}

struct wizard_close_data {
	xmi_main_options **options;
	GtkWidget **ows;
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
		g_debug("Processing file %i\n", i);
		wcd->options[i] = xmi_msim_gui_options_box_get_options(XMI_MSIM_GUI_OPTIONS_BOX(wcd->ows[i]));
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
	if (bwd->job && xmi_msim_job_is_running(bwd->job)) {
		xmi_msim_job_suspend(bwd->job, NULL);
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(bwd->batch_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "A simulation is currently running.\nAre you sure you want to quit?");
		int response = gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		if (response == GTK_RESPONSE_YES) {
			xmi_msim_job_kill(bwd->job, NULL);
			gtk_widget_destroy(bwd->batch_window);
			g_object_unref(bwd->logFile);
		}
		else {
			xmi_msim_job_resume(bwd->job, NULL);
		}
	}
	else {
		gtk_widget_destroy(bwd->batch_window);
		g_object_unref(bwd->logFile);
	}
}

static gboolean batch_window_delete_event(GtkWidget *batch_window, GdkEvent *event, struct batch_window_data *bwd) {
	if (bwd->job && xmi_msim_job_is_running(bwd->job)) {
		xmi_msim_job_suspend(bwd->job, NULL);
		GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(bwd->batch_window), GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "A simulation is currently running.\nAre you sure you want to quit?");
		int response = gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		if (response == GTK_RESPONSE_YES) {
			xmi_msim_job_kill(bwd->job, NULL);
			g_object_unref(bwd->logFile);
			return FALSE;
		}
		xmi_msim_job_resume(bwd->job, NULL);
		return TRUE;
	}
	g_object_unref(bwd->logFile);
	return FALSE;
}

static int specific_options(GtkWidget *main_window, struct wizard_close_data *wcd, GSList *filenames) {
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

	unsigned int i;
	GtkWidget **ows = (GtkWidget **) g_malloc(sizeof(GtkWidget *) * g_slist_length(filenames));
	for (i = 0 ; i < g_slist_length(filenames) ; i++) {
		ows[i] = xmi_msim_gui_options_box_new();
		gtk_assistant_append_page(GTK_ASSISTANT(wizard), ows[i]);
		gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), ows[i], GTK_ASSISTANT_PAGE_CONTENT);
		gchar *filename = g_strdup_printf("File %d: %s",i+1, g_path_get_basename((char *) g_slist_nth_data(filenames,i)));
		gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), ows[i], filename);
		g_free(filename);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), ows[i], TRUE);
	}

	//add confirmation page
	GtkWidget *confirmationLabel = gtk_label_new("Confirm the options selected on the previous pages and continue with the simulation?");
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), confirmationLabel);
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), confirmationLabel, TRUE);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), confirmationLabel, GTK_ASSISTANT_PAGE_CONFIRM);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), confirmationLabel, "Confirmation");

	//signal handlers
	wcd->options = (xmi_main_options **) g_malloc(sizeof(xmi_main_options *) * g_slist_length(filenames));
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



static int general_options(GtkWidget *main_window, xmi_main_options **options) {
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Set the options for the simulations batch", GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), "_Ok", GTK_RESPONSE_ACCEPT, "_Cancel", GTK_RESPONSE_REJECT, NULL);
	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *ow = xmi_msim_gui_options_box_new();
	gtk_container_add(GTK_CONTAINER(content_area), ow);
	gtk_widget_show_all(ow);
	int dialog_rv = gtk_dialog_run(GTK_DIALOG(dialog));

	if (dialog_rv == GTK_RESPONSE_ACCEPT) {
		//accepted -> read options from dialog
		options[0] = xmi_msim_gui_options_box_get_options(XMI_MSIM_GUI_OPTIONS_BOX(ow));
	}
	else {
		gtk_widget_destroy(dialog);
		return 0;
	}
	gtk_widget_destroy(dialog);
	return 1;
}

static void save_archive_callback(GtkWidget *task_window, GAsyncResult *result, gpointer data) {
	GtkWindow *window = gtk_window_get_transient_for(GTK_WINDOW(task_window));
	gdk_window_set_cursor(gtk_widget_get_window(task_window), NULL);
	gtk_widget_destroy(task_window);

	GError *error = NULL;

	xmi_archive *archive = g_task_propagate_pointer(G_TASK(result), &error);

	if (!archive) {
		GtkWidget *dialog = gtk_message_dialog_new(window,
			GTK_DIALOG_DESTROY_WITH_PARENT,
	       		GTK_MESSAGE_ERROR,
	       		GTK_BUTTONS_CLOSE,
	       		"An error occured while performing the conversion"
                	);
		gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
		g_error_free(error);
     		gtk_dialog_run(GTK_DIALOG(dialog));
		gtk_widget_destroy(dialog);
		return;
	}

	GtkWidget *xmsa_window = xmi_msim_gui_xmsa_viewer_window_new(XMI_MSIM_GUI_APPLICATION(g_application_get_default()), archive);
	gtk_window_set_transient_for(GTK_WINDOW(xmsa_window), GTK_WINDOW(window));
	gtk_window_present(GTK_WINDOW(xmsa_window));
}

struct save_archive_data {
	struct archive_options_data *aod;
	gchar *xpath1;
       	gchar *xpath2;
	xmi_output ***output;
};

struct save_archive_update_task_window_data {
	XmiMsimGuiLongTaskWindow *window;
	gchar *text;
};

static gboolean save_archive_update_task_window(struct save_archive_update_task_window_data *data) {
	xmi_msim_gui_long_task_window_set_text(data->window, data->text);
	g_free(data->text);

	return FALSE;
}

static void save_archive_thread(GTask *task, gpointer source_object, gpointer task_data, GCancellable *cancellable) {
	XmiMsimGuiLongTaskWindow *task_window = source_object;
	struct save_archive_data *sad = task_data;
	
	//convert to an archive struct -> this should never fail
	xmi_archive *archive = xmi_archive_raw2struct(
		sad->output,
		sad->aod->start_value1,
		sad->aod->end_value1,
		sad->aod->nsteps1,
		sad->xpath1,
		sad->aod->start_value2,
		sad->aod->end_value2,
		sad->aod->nsteps2,
		sad->xpath2);
	
	
	//save to XMSA file
	struct save_archive_update_task_window_data update_data;
	update_data.window = task_window;
	update_data.text = g_strdup("<b>Saving XMSA file</b>");

	gdk_threads_add_idle((GSourceFunc) save_archive_update_task_window, &update_data);

	GError *error = NULL;

	if (xmi_write_archive_xml(sad->aod->xmsa_file, archive, &error) == 0) {
		xmi_archive_free(archive);
		g_task_return_error(task, error);
		return;
	}

	update_data.text = g_strdup("<b>Freeing XMSO memory</b>");
	gdk_threads_add_idle((GSourceFunc) save_archive_update_task_window, &update_data);

	int i, j;
	for (i = 0 ; i <= sad->aod->nsteps1 ; i++) {
		for (j = 0 ; j <= sad->aod->nsteps2 ; j++) {
			xmi_output_free(sad->output[i][j]);
		}
		g_free(sad->output[i]);
	}
	g_free(sad->output);
	g_free(sad->aod->xmsa_file);
	g_free(sad->aod->xmo);
	g_free(sad->aod);
	g_free(sad->xpath1);
	g_free(sad->xpath2);
	g_task_return_pointer(task, archive, (GDestroyNotify) xmi_archive_free);
}

void batchmode_button_clicked_cb(GtkWidget *button, GtkWidget *window) {

	//open dialog
	XmiMsimGuiFileChooserDialog *file_dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select one or more files",
		GTK_WINDOW(window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"_Open",
		"_Cancel");
	gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(file_dialog), TRUE);
	xmi_msim_gui_file_chooser_dialog_set_modal(file_dialog, TRUE);
	GtkWidget *label = gtk_label_new("If one file is selected then a batch of files will be created based on this file with one or two variable parameters. Selecting multiple files will result in all the selected files being executed.");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(file_dialog), label);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsi");
	gtk_file_filter_add_pattern(filter,"*.XMSI");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(file_dialog), filter);

	if (xmi_msim_gui_file_chooser_dialog_run(file_dialog) != GTK_RESPONSE_ACCEPT) {
		//nothing was selected
   		xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
		return;
	}

	//extract all selected filenames
	GSList *filenames = gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(file_dialog));

	int i;
   	xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
	GtkWidget *dialog;
	xmi_main_options **options;
	if (g_slist_length(filenames) > 1) {
		//more than one file selected
		//1) ask if the options will apply to all or if individual job options will be used
		dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "Set options for each input-file separately?");
		int response = gtk_dialog_run(GTK_DIALOG(dialog));
		//2) produce dialog asking for options
		if (response == GTK_RESPONSE_YES) {
   			gtk_widget_destroy (dialog);
			//file specific options
			struct wizard_close_data *wcd = (struct wizard_close_data *) g_malloc(sizeof(struct wizard_close_data));
			int rv = specific_options(window, wcd, filenames);
			if (rv == 1) {
				//wizard completed
				options = wcd->options;
			}
			else if (rv == 0) {
				//wizard aborted
				return;
			}
#ifdef HAVE_GOOGLE_ANALYTICS
			XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_get_global();
			xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", "MULTIPLE-FILES-MULTIPLE-OPTIONS", NULL);
#endif
		}
		else if (response == GTK_RESPONSE_NO) {
			//options apply to all
   			gtk_widget_destroy (dialog);
			options = (xmi_main_options **) g_malloc(sizeof(xmi_main_options *));
			int rv = general_options(window, options);
			if (rv == 0) {
				return;
			}
			//options are set
#ifdef HAVE_GOOGLE_ANALYTICS
			XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_get_global();
			xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", "MULTIPLE-FILES-SINGLE-OPTION", NULL);
#endif
		}
		else {
   			gtk_widget_destroy (dialog);
			return;
		}
		//3) launch execution window
		batch_mode(window, options, filenames, response == GTK_RESPONSE_YES ? XMI_MSIM_BATCH_MULTIPLE_OPTIONS : XMI_MSIM_BATCH_ONE_OPTION, NULL, NULL, 0);
		//4) display message with result
	}
	else {
		//one file selected
		//options apply to all
		gchar *xpath1, *xpath2;
		xmi_input *input;
		GError *error = NULL;
		if (xmi_read_input_xml((gchar *) g_slist_nth_data(filenames, 0), &input, &error) == 0) {
			//error reading inputfile
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading input-file %s. Aborting batch mode", (gchar *) g_slist_nth_data(filenames, 0));
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
			g_error_free(error);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}

		int allowed1, allowed2;
		int rv = select_parameter(window, input, &xpath1, &xpath2, &allowed1, &allowed2);
		if (rv == 1) {
			g_debug("xpath1: %s", xpath1);
			g_debug("allowed1: %i", allowed1);
			if (xpath2) {
				g_debug("xpath2: %s", xpath2);
				g_debug("allowed2: %i", allowed2);
			}
#ifdef HAVE_GOOGLE_ANALYTICS
			XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_get_global();
			gchar *event_label = NULL;
			if (xpath2)
				event_label = g_strdup_printf("SINGLE-FILE-%s-%s", xpath1, xpath2);
			else
				event_label = g_strdup_printf("SINGLE-FILE-%s", xpath1);
			xmi_msim_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", event_label, NULL);
			g_free(event_label);
#endif
		}
		else
			return;
		//generate wizard
		//1) options
		//2) range of parameter
		//3) plot afterwards? Requires saving to XMSA file as well as selecting a particular line
		struct archive_options_data *aod = g_malloc(sizeof(struct archive_options_data));
		rv = archive_options(window, input, (gchar *) g_slist_nth_data(filenames, 0), xpath1, xpath2, allowed1, allowed2, aod);
		if (rv == 0) {
			return;
		}
		//4) generate the new XMSI files
		GSList *filenames_xmsiGSL = NULL;
		gchar **filenames_xmsi = (gchar **) g_malloc(sizeof(gchar *)*((aod->nsteps1+1)*(aod->nsteps2+1)+1));
		gchar **filenames_xmso = (gchar **) g_malloc(sizeof(gchar *)*((aod->nsteps1+1)*(aod->nsteps2+1)+1));
		filenames_xmsi[(aod->nsteps1+1)*(aod->nsteps2+1)] = NULL;
		filenames_xmso[(aod->nsteps1+1)*(aod->nsteps2+1)] = NULL;
		gchar *filename = (gchar *) g_slist_nth_data(filenames, 0);
		gchar *buffer;
		//open inputfile
		xmlDocPtr doc;
		if ((doc = xmlReadFile(filename,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not read file %s. Aborting batch mode", filename);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlXPathContextPtr context;
		xmlXPathObjectPtr result, result2 = NULL, result3;

		context = xmlXPathNewContext(doc);
		if (context == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		//selected xpath
		result = xmlXPathEvalExpression(BAD_CAST xpath1, context);
		if (result == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
			xmlXPathFreeObject(result);
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlNodeSetPtr nodeset = result->nodesetval;
		if (nodeset->nodeNr != 1) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlNodeSetPtr nodeset2 = NULL;
		if (xpath2) {
			//selected xpath2
			result2 = xmlXPathEvalExpression(BAD_CAST xpath2, context);
			if (result2 == NULL) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			if(xmlXPathNodeSetIsEmpty(result2->nodesetval)){
				xmlXPathFreeObject(result2);
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			nodeset2 = result2->nodesetval;
			if (nodeset2->nodeNr != 1) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
		}

		//outputfile
		result3 = xmlXPathEvalExpression(BAD_CAST "/xmimsim/general/outputfile", context);
		if (result3 == NULL) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		if(xmlXPathNodeSetIsEmpty(result3->nodesetval)){
			xmlXPathFreeObject(result3);
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}
		xmlNodeSetPtr nodeset3 = result3->nodesetval;
		if (nodeset3->nodeNr != 1) {
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error. Aborting batch mode");
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}

		double weight_sum = 0.0;
		double weight_sum2 = 0.0;
		double *orig_weight = NULL;
		double *orig_weight2 = NULL;
		xmlXPathObjectPtr result4;
		xmlNodePtr grandparent;
		xmlNodeSetPtr grandkids, grandkids2;
		if (xpath2 && (allowed1 & PARAMETER_WEIGHT_FRACTION) && (allowed2 & PARAMETER_WEIGHT_FRACTION) && nodeset->nodeTab[0]->parent->parent == nodeset2->nodeTab[0]->parent->parent) {
			//get grandparent of xpath node
			grandparent = nodeset->nodeTab[0]->parent->parent;
			//get all weight_fraction grandchildren
			result4 = xmlXPathNodeEval(grandparent, BAD_CAST "element/weight_fraction", context);
			if (result4 == NULL) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error occurred while evaluating element/weight_fraction.\nAborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			if(xmlXPathNodeSetIsEmpty(result4->nodesetval)){
				xmlXPathFreeObject(result4);
				dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error: empty nodeset. Aborting batch mode");
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			grandkids = result4->nodesetval;
			orig_weight = (double *) g_malloc(sizeof(double) * grandkids->nodeNr);
			for (i = 0 ; i < grandkids->nodeNr ; i++) {
				if (grandkids->nodeTab[i] == nodeset->nodeTab[0] ||
					grandkids->nodeTab[i] == nodeset2->nodeTab[0]) {
					continue;
				}
				buffer = (gchar *) xmlNodeListGetString(doc, grandkids->nodeTab[i]->children, 1);
				orig_weight[i] = strtod(buffer, NULL);
				weight_sum += orig_weight[i];
				xmlFree(buffer);
			}
		}
		else {
			if (allowed1 & PARAMETER_WEIGHT_FRACTION) {
				//get grandparent of xpath node
				grandparent = nodeset->nodeTab[0]->parent->parent;
				//get all weight_fraction grandchildren
				result4 = xmlXPathNodeEval(grandparent, BAD_CAST "element/weight_fraction", context);
				if (result4 == NULL) {
					dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error occurred while evaluating element/weight_fraction.\nAborting batch mode");
					gtk_dialog_run(GTK_DIALOG(dialog));
					gtk_widget_destroy(dialog);
					return;
				}
				if(xmlXPathNodeSetIsEmpty(result4->nodesetval)){
					xmlXPathFreeObject(result4);
					dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error: empty nodeset. Aborting batch mode");
					gtk_dialog_run(GTK_DIALOG(dialog));
					gtk_widget_destroy(dialog);
					return;
				}
				grandkids = result4->nodesetval;
				orig_weight = (double *) g_malloc(sizeof(double) * grandkids->nodeNr);
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
			if (xpath2 && (allowed2 & PARAMETER_WEIGHT_FRACTION)) {
				//get grandparent of xpath node
				grandparent = nodeset2->nodeTab[0]->parent->parent;
				//get all weight_fraction grandchildren
				result4 = xmlXPathNodeEval(grandparent, BAD_CAST "element/weight_fraction", context);
				if (result4 == NULL) {
					dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error occurred while evaluating element/weight_fraction.\nAborting batch mode");
					gtk_dialog_run(GTK_DIALOG(dialog));
					gtk_widget_destroy(dialog);
					return;
				}
				if(xmlXPathNodeSetIsEmpty(result4->nodesetval)){
					xmlXPathFreeObject(result4);
					dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "XPath error: empty nodeset. Aborting batch mode");
					gtk_dialog_run(GTK_DIALOG(dialog));
					gtk_widget_destroy(dialog);
					return;
				}
				grandkids2 = result4->nodesetval;
				orig_weight2 = (double *) g_malloc(sizeof(double) * grandkids2->nodeNr);
				for (i = 0 ; i < grandkids2->nodeNr ; i++) {
					if (grandkids2->nodeTab[i] == nodeset2->nodeTab[0]) {
						continue;
					}
					buffer = (gchar *) xmlNodeListGetString(doc, grandkids2->nodeTab[i]->children, 1);
					orig_weight2[i] = strtod(buffer, NULL);
					weight_sum2 += orig_weight2[i];
					xmlFree(buffer);
				}
			}
		}


		int j,k;
		for (i = 0 ; i <= aod->nsteps1 ; i++) {
			for (j = 0 ; j <= aod->nsteps2 ; j++) {
				buffer = g_strdup_printf("%s%sxmi_%i_%i_%i.xmsi", g_get_tmp_dir(), G_DIR_SEPARATOR_S, (int) getpid(), i, j);
				filenames_xmsiGSL = g_slist_append(filenames_xmsiGSL, (gpointer) buffer);
				filenames_xmsi[i*(aod->nsteps2+1)+j] = buffer;
				buffer = g_strdup_printf("%s%sxmi_%i_%i_%i.xmso", g_get_tmp_dir(), G_DIR_SEPARATOR_S, (int) getpid(), i, j);
				filenames_xmso[i*(aod->nsteps2+1)+j] = buffer;
				double value1 = aod->start_value1 + i*(aod->end_value1-aod->start_value1)/(aod->nsteps1);
				double value2;
				if (xpath2)
					value2 = aod->start_value2 + j*(aod->end_value2-aod->start_value2)/(aod->nsteps2);
				if (xpath2 && (allowed1 & PARAMETER_WEIGHT_FRACTION) && (allowed2 & PARAMETER_WEIGHT_FRACTION) && nodeset->nodeTab[0]->parent->parent == nodeset2->nodeTab[0]->parent->parent) {
					double diff = 100.0-value1-value2-weight_sum;
					double new_weight_sum = weight_sum + diff;
					for (k = 0 ; k < grandkids->nodeNr ; k++) {
						if (grandkids->nodeTab[k] == nodeset->nodeTab[0] ||
							grandkids->nodeTab[k] == nodeset2->nodeTab[0]) {
							continue;
						}
						double new_weight = orig_weight[k]*new_weight_sum/weight_sum;
						buffer = g_strdup_printf("%g", new_weight);
						xmlNodeSetContent(grandkids->nodeTab[k], BAD_CAST buffer);
						g_free(buffer);
					}

				}
				else {
					if (allowed1 & PARAMETER_WEIGHT_FRACTION) {
						double diff = 100.0-value1-weight_sum;
						double new_weight_sum = weight_sum + diff;
						for (k = 0 ; k < grandkids->nodeNr ; k++) {
							if (grandkids->nodeTab[k] == nodeset->nodeTab[0]) {
								continue;
							}
							double new_weight = orig_weight[k]*new_weight_sum/weight_sum;
							buffer = g_strdup_printf("%g", new_weight);
							xmlNodeSetContent(grandkids->nodeTab[k], BAD_CAST buffer);
							g_free(buffer);
						}
					}
					if (xpath2 && (allowed2 & PARAMETER_WEIGHT_FRACTION)) {
						double diff = 100.0-value2-weight_sum2;
						double new_weight_sum = weight_sum2 + diff;
						for (k = 0 ; k < grandkids2->nodeNr ; k++) {
							if (grandkids2->nodeTab[k] == nodeset2->nodeTab[0]) {
								continue;
							}
							double new_weight = orig_weight2[k]*new_weight_sum/weight_sum2;
							buffer = g_strdup_printf("%g", new_weight);
							xmlNodeSetContent(grandkids2->nodeTab[k], BAD_CAST buffer);
							g_free(buffer);
						}
					}
				}
				buffer = g_strdup_printf("%g", value1);
				xmlNodeSetContent(nodeset->nodeTab[0], BAD_CAST buffer);
				g_free(buffer);
				if (xpath2) {
					buffer = g_strdup_printf("%g", value2);
					xmlNodeSetContent(nodeset2->nodeTab[0], BAD_CAST buffer);
					g_free(buffer);
				}
				xmlNodeSetContent(nodeset3->nodeTab[0], BAD_CAST filenames_xmso[i*(aod->nsteps2+1)+j]);
				xmlKeepBlanksDefault(0);
				if (xmlSaveFormatFileEnc(filenames_xmsi[i*(aod->nsteps2+1)+j],doc,NULL,1) == -1) {
					dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not write to %s. Aborting batch mode", filenames_xmsi[i*(aod->nsteps2+1)+j]);
					gtk_dialog_run(GTK_DIALOG(dialog));
					gtk_widget_destroy(dialog);
					return;
				}
			}
		}
		if (orig_weight)
			g_free(orig_weight);
		if (orig_weight2)
			g_free(orig_weight2);
		xmlXPathFreeContext(context);
		xmlXPathFreeObject(result);
		xmlXPathFreeObject(result3);
		xmlFreeDoc(doc);
		xmi_output ***output = (xmi_output ***) g_malloc(sizeof(xmi_output **)*(aod->nsteps1+1));
		for (i = 0 ; i <= aod->nsteps1 ; i++)
			output[i] = (xmi_output **) g_malloc(sizeof(xmi_output *)*(aod->nsteps2+1));
		int exec_rv = batch_mode(window, &aod->xmo, filenames_xmsiGSL, XMI_MSIM_BATCH_ONE_OPTION, output, filenames_xmso, aod->nsteps2);

		g_strfreev(filenames_xmsi);
		g_strfreev(filenames_xmso);
		g_slist_free(filenames_xmsiGSL);

		if (exec_rv == 0) {
			return;
		}

		GtkWidget *task_window = xmi_msim_gui_long_task_window_new(GTK_WINDOW(window));
		xmi_msim_gui_long_task_window_set_text(XMI_MSIM_GUI_LONG_TASK_WINDOW(task_window), "<b>Converting XMSO files to archive</b>");
		gtk_widget_show(task_window);

		GdkCursor* watchCursor = gdk_cursor_new_for_display(gdk_display_get_default(), GDK_WATCH);
		gdk_window_set_cursor(gtk_widget_get_window(task_window), watchCursor);

		GTask *task = g_task_new(task_window, NULL, (GAsyncReadyCallback) save_archive_callback, NULL);
		struct save_archive_data *sad = g_malloc(sizeof(struct save_archive_data));
		sad->aod = aod;
		sad->xpath1 = xpath1;
		sad->xpath2 = xpath2;
		sad->output = output;

		g_task_set_task_data(task, sad, g_free);
		g_task_run_in_thread(task, save_archive_thread);
		g_object_unref(task);
	}
}

static int batch_mode(GtkWidget *main_window, xmi_main_options **options, GSList *filenames, enum xmi_msim_batch_options batch_options, xmi_output ***output, gchar **filenames_xmso, int nsteps2) {
	int rv = 0;
	GtkWidget *batch_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_transient_for(GTK_WINDOW(batch_window), GTK_WINDOW(main_window));
	gtk_window_set_modal(GTK_WINDOW(batch_window), TRUE);
	gtk_window_set_destroy_with_parent(GTK_WINDOW(batch_window), TRUE);
	gtk_window_set_position (GTK_WINDOW(batch_window), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(batch_window), "Batch simulation controls");
	gtk_window_set_default_size(GTK_WINDOW(batch_window),500,500);

	struct batch_window_data *bwd = (struct batch_window_data *) g_malloc(sizeof(struct batch_window_data));
	bwd->batch_window = batch_window;
	bwd->job = NULL;

	GtkWidget *main_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(batch_window), main_vbox);
	gtk_container_set_border_width(GTK_CONTAINER(batch_window),3);
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);

	bwd->playButton = gtk_button_new_from_icon_name("media-playback-start", GTK_ICON_SIZE_DIALOG);
	gtk_box_pack_start(GTK_BOX(hbox), bwd->playButton, FALSE, FALSE, 2);

	if (xmi_msim_job_is_suspend_available()) {
		bwd->pauseButton = gtk_button_new_from_icon_name("media-playback-pause", GTK_ICON_SIZE_DIALOG);
		gtk_box_pack_start(GTK_BOX(hbox), bwd->pauseButton, FALSE, FALSE, 2);
		gtk_widget_set_sensitive(bwd->pauseButton, FALSE);
	}
	else {
		bwd->pauseButton = NULL;
	}

	bwd->stopButton = gtk_button_new_from_icon_name("media-playback-stop", GTK_ICON_SIZE_DIALOG);
	gtk_box_pack_start(GTK_BOX(hbox), bwd->stopButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(bwd->stopButton, FALSE);

	GtkWidget *nthreadsW = NULL;
	if (xmi_omp_get_max_threads() > 1) {
		GtkWidget *cpuLabel = gtk_label_new("CPUs");
		gtk_box_pack_start(GTK_BOX(hbox), cpuLabel, FALSE, FALSE, 2);
		GtkAdjustment *nthreadsA = GTK_ADJUSTMENT(gtk_adjustment_new((gdouble) xmi_omp_get_max_threads(), 1.0, (gdouble) xmi_omp_get_max_threads(), 1.0,1.0,0.0));
		nthreadsW = gtk_scale_new(GTK_ORIENTATION_HORIZONTAL, nthreadsA);
		gtk_scale_set_digits(GTK_SCALE(nthreadsW), 0);
		gtk_scale_set_value_pos(GTK_SCALE(nthreadsW),GTK_POS_TOP);
		gtk_widget_set_size_request(nthreadsW,30,-1);
		gtk_box_pack_start(GTK_BOX(hbox), nthreadsW, TRUE, TRUE, 2);
	}
	bwd->nthreadsW = nthreadsW;

	//add progressbar
	GtkWidget *progressbarW = gtk_progress_bar_new();
	gtk_orientable_set_orientation(GTK_ORIENTABLE(progressbarW), GTK_ORIENTATION_HORIZONTAL);
	gtk_progress_bar_set_show_text(GTK_PROGRESS_BAR(progressbarW), TRUE);
	//gtk_widget_set_size_request(progressbarW,-1,10);
	GtkWidget *pvbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);
	gtk_box_set_homogeneous(GTK_BOX(pvbox), TRUE);
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
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 2);
	GtkWidget *saveButton = gtk_button_new_with_mnemonic("_Save As");
	bwd->saveButton = saveButton;
	gtk_box_pack_start(GTK_BOX(hbox), saveButton, FALSE, FALSE, 2);
	GtkWidget *controlsLogFileW = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(controlsLogFileW), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), controlsLogFileW, TRUE, TRUE, 2);
	bwd->controlsLogFileW = controlsLogFileW;

	g_signal_connect(G_OBJECT(saveButton), "clicked", G_CALLBACK(choose_logfile), (gpointer) bwd);

	GtkWidget *verboseW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Verbose");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Very verbose");
	gtk_combo_box_set_active(GTK_COMBO_BOX(verboseW),0);
	bwd->verboseW = verboseW;
	gtk_box_pack_start(GTK_BOX(hbox), verboseW, FALSE, FALSE, 2);
	GtkWidget *exitButton = gtk_button_new_with_mnemonic("_Quit");
	bwd->exitButton = exitButton;
	gtk_box_pack_end(GTK_BOX(hbox), exitButton, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(main_vbox), hbox, FALSE, FALSE, 0);

	bwd->options = options;
	bwd->filenames = filenames;
	bwd->batch_options = batch_options;
	bwd->rv = &rv;
	bwd->output = output;
	bwd->filenames_xmso = filenames_xmso;
	bwd->nsteps2 = nsteps2;


	g_signal_connect(G_OBJECT(batch_window), "delete-event", G_CALLBACK(batch_window_delete_event), (gpointer) bwd);
	g_signal_connect(G_OBJECT(batch_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(exitButton), "clicked", G_CALLBACK(batch_window_exit), (gpointer) bwd);
	g_signal_connect(G_OBJECT(bwd->playButton), "clicked", G_CALLBACK(play_button_clicked), (gpointer) bwd);
	g_signal_connect(G_OBJECT(bwd->stopButton), "clicked", G_CALLBACK(stop_button_clicked), (gpointer) bwd);
	if (bwd->pauseButton)
		g_signal_connect(G_OBJECT(bwd->pauseButton), "clicked", G_CALLBACK(pause_button_clicked), (gpointer) bwd);
	bwd->logFile = NULL;
	gtk_widget_show_all(batch_window);

	gtk_main();

	return rv;
}

