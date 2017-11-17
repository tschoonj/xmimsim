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
#include "xmimsim-gui.h"
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-batch.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-energies.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-controls.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-xmsi-scrolled-window.h"
#include "xmimsim-gui-options-box.h"
#include "xmimsim-gui-xmsa-viewer-window.h"
#include "xmi_main.h"
#include <stdio.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>
#include <xraylib.h>
#include <libxml/xmlmemory.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <libxml/catalog.h>
#include "xmi_lines.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include "xmi_private.h"
#include <math.h>
#include <string.h>

#ifdef MAC_INTEGRATION
	#include "xmi_resources_mac.h"
#elif defined(G_OS_WIN32)
  	#include <windows.h>
  	#include <winbase.h>
	#include "xmi_registry_win.h"
	#define getpid GetCurrentProcessId
#endif

#ifdef G_OS_UNIX
  #include <sys/types.h>
  #include <sys/wait.h>
#endif

#if defined(G_OS_WIN32)
  #include "xmi_detector.h"
  #include "xmi_solid_angle.h"
  typedef LONG (NTAPI *pNtSuspendProcess )(IN HANDLE ProcessHandle );
  typedef LONG (NTAPI *pNtResumeProcess )(IN HANDLE ProcessHandle );
  static pNtSuspendProcess NtSuspendProcess = NULL;
  static pNtResumeProcess NtResumeProcess = NULL;
  #define real_xmimsim_pid ((int) GetProcessId((HANDLE) xmimsim_pid))
#else
  #define real_xmimsim_pid ((int) xmimsim_pid)
#endif



#if LIBXML_VERSION < 20901
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
int xmlXPathSetContextNode(xmlNodePtr node, xmlXPathContextPtr ctx);
xmlXPathObjectPtr xmlXPathNodeEval(xmlNodePtr node, const xmlChar *str, xmlXPathContextPtr ctx);

#endif

#ifdef HAVE_GOOGLE_ANALYTICS
  #include "xmimsim-gui-google-analytics.h"
#endif

struct canvas_data {
	double width;
	double height;
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
	GtkWidget *pauseButton;
	GtkWidget *nthreadsW;
	GtkWidget *progressbarW;
	GtkWidget *controlsLogW;
	GtkTextBuffer *controlsLogB;
	GtkWidget *saveButton;
	GtkWidget *controlsLogFileW;
	GtkWidget *verboseW;
	GtkWidget *exitButton;
	struct xmi_main_options **options;
	GSList *filenames;
	enum xmi_msim_batch_options batch_options;
	gboolean paused;
	GTimer *timer;
	FILE *logFile;
	unsigned int i;
	gchar *argv0;
	struct xmi_output ***output;
	gchar **filenames_xmso;
	int nsteps2;
};

struct archive_options_data {
	double start_value1;
	double end_value1;
	int nsteps1;
	double start_value2;
	double end_value2;
	int nsteps2;
	gchar *xmsa_file;
	struct xmi_main_options *xmo;
};

static void xmimsim_child_watcher_cb(GPid pid, gint status, struct batch_window_data *bwd);
static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd);
static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd);

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
		GTK_STOCK_SAVE,
		GTK_STOCK_CANCEL);
	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);
	GtkFileFilter *filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsa");
	gtk_file_filter_add_pattern(filter,"*.XMSA");
	gtk_file_filter_set_name(filter,"XMI-MSIM archive files");
	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
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
			gtk_widget_modify_base(wrd->start1Entry,GTK_STATE_NORMAL,&red);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		gtk_widget_modify_base(wrd->start1Entry,GTK_STATE_NORMAL,NULL);
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
			gtk_widget_modify_base(wrd->end1Entry,GTK_STATE_NORMAL,&red);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		gtk_widget_modify_base(wrd->end1Entry,GTK_STATE_NORMAL,NULL);
	}
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->nsteps1Entry));
	nsteps1=strtol(textPtr, &endPtr, 10);
	lastPtr = textPtr + strlen(textPtr);
	if (entry == GTK_EDITABLE(wrd->nsteps1Entry) && (strlen(textPtr) == 0 || lastPtr != endPtr|| nsteps1 < 1)) {
		gtk_widget_modify_base(wrd->nsteps1Entry,GTK_STATE_NORMAL,&red);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}
	else if (strlen(textPtr) > 0 && lastPtr == endPtr && nsteps1 >= 1) {
		gtk_widget_modify_base(wrd->nsteps1Entry,GTK_STATE_NORMAL,NULL);
	}
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->start1Entry));
	start = strtod(textPtr, &endPtr);
	textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->end1Entry));
	end = strtod(textPtr, &endPtr);

	if (end > start) {
		start_end1 = 1;
		gtk_widget_modify_base(wrd->start1Entry,GTK_STATE_NORMAL,NULL);
		gtk_widget_modify_base(wrd->end1Entry,GTK_STATE_NORMAL,NULL);
	}
	else {
		gtk_widget_modify_base(wrd->start1Entry,GTK_STATE_NORMAL,&red);
		gtk_widget_modify_base(wrd->end1Entry,GTK_STATE_NORMAL,&red);
		gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
		return;
	}

	if (wrd->start2Entry) {
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
				gtk_widget_modify_base(wrd->start2Entry,GTK_STATE_NORMAL,&red);
				gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
				return;
			}
			gtk_widget_modify_base(wrd->start2Entry,GTK_STATE_NORMAL,NULL);
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
				gtk_widget_modify_base(wrd->end2Entry,GTK_STATE_NORMAL,&red);
				gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
				return;
			}
			gtk_widget_modify_base(wrd->end2Entry,GTK_STATE_NORMAL,NULL);
		}
		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->nsteps2Entry));
		nsteps2=strtol(textPtr, &endPtr, 10);
		lastPtr = textPtr + strlen(textPtr);
		if (entry == GTK_EDITABLE(wrd->nsteps2Entry) && (strlen(textPtr) == 0 || lastPtr != endPtr|| nsteps2 < 1)) {
			gtk_widget_modify_base(wrd->nsteps2Entry,GTK_STATE_NORMAL,&red);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		else if (strlen(textPtr) > 0 && lastPtr == endPtr && nsteps2 >= 1) {
			gtk_widget_modify_base(wrd->nsteps2Entry,GTK_STATE_NORMAL,NULL);
		}
		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->start2Entry));
		start = strtod(textPtr, &endPtr);
		textPtr = (char *) gtk_entry_get_text(GTK_ENTRY(wrd->end2Entry));
		end = strtod(textPtr, &endPtr);

		if (end > start) {
			start_end2 = 1;
			gtk_widget_modify_base(wrd->start2Entry,GTK_STATE_NORMAL,NULL);
			gtk_widget_modify_base(wrd->end2Entry,GTK_STATE_NORMAL,NULL);
		}
		else {
			gtk_widget_modify_base(wrd->start2Entry,GTK_STATE_NORMAL,&red);
			gtk_widget_modify_base(wrd->end2Entry,GTK_STATE_NORMAL,&red);
			gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
			return;
		}
		if (wrd->same_layer) {
			//in this case the sum of the end values must be less than or equal to 100
			double value1 = strtod(gtk_entry_get_text(GTK_ENTRY(wrd->end1Entry)), NULL);
			double value2 = strtod(gtk_entry_get_text(GTK_ENTRY(wrd->end2Entry)), NULL);
			if (value1 + value2 > 100.0) {
				gtk_widget_modify_base(wrd->start1Entry,GTK_STATE_NORMAL,&red);
				gtk_widget_modify_base(wrd->end1Entry,GTK_STATE_NORMAL,&red);
				gtk_widget_modify_base(wrd->start2Entry,GTK_STATE_NORMAL,&red);
				gtk_widget_modify_base(wrd->end2Entry,GTK_STATE_NORMAL,&red);
				gtk_assistant_set_page_complete(GTK_ASSISTANT(wrd->wizard), vbox, FALSE);
				return;
			}
			else {
				gtk_widget_modify_base(wrd->start1Entry,GTK_STATE_NORMAL,NULL);
				gtk_widget_modify_base(wrd->end1Entry,GTK_STATE_NORMAL,NULL);
				gtk_widget_modify_base(wrd->start2Entry,GTK_STATE_NORMAL,NULL);
				gtk_widget_modify_base(wrd->end2Entry,GTK_STATE_NORMAL,NULL);
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

static int archive_options(GtkWidget *main_window, struct xmi_input *input, gchar *filename, gchar *xpath1, gchar *xpath2, int allowed1, int allowed2, struct archive_options_data *aod) {
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
	GtkWidget *ow = xmi_msim_gui_options_box_new(main_window);
	gtk_assistant_append_page(GTK_ASSISTANT(wizard), ow);
	gtk_assistant_set_page_type(GTK_ASSISTANT(wizard), ow, GTK_ASSISTANT_PAGE_CONTENT);
	gtk_assistant_set_page_title(GTK_ASSISTANT(wizard), ow, "General options");
	gtk_assistant_set_page_complete(GTK_ASSISTANT(wizard), ow, TRUE);

	//set range
	struct wizard_range_data *wrd = (struct wizard_range_data *) g_malloc(sizeof(struct wizard_range_data));
	GtkWidget *vbox = gtk_vbox_new(FALSE, 2);
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
	hbox = gtk_hbox_new(FALSE, 2);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 10);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 3);

	hbox = gtk_hbox_new(FALSE, 2);
	label = gtk_label_new("Start");
	GtkWidget *start1Entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(start1Entry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), start1Entry, TRUE, TRUE, 1);
	label = gtk_label_new("End");
	GtkWidget *end1Entry = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(end1Entry), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(hbox), end1Entry, TRUE, TRUE, 1);
	label = gtk_label_new("#Steps");
	GtkWidget *nsteps1Entry = gtk_entry_new();
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
		hbox = gtk_hbox_new(FALSE, 2);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 10);
		gtk_container_set_border_width(GTK_CONTAINER(vbox), 3);

		hbox = gtk_hbox_new(FALSE, 2);
		label = gtk_label_new("Start");
		start2Entry = gtk_entry_new();
		gtk_editable_set_editable(GTK_EDITABLE(start2Entry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), start2Entry, TRUE, TRUE, 1);

		label = gtk_label_new("End");
		end2Entry = gtk_entry_new();
		gtk_editable_set_editable(GTK_EDITABLE(end2Entry), TRUE);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), end2Entry, TRUE, TRUE, 1);

		label = gtk_label_new("#Steps");
		nsteps2Entry = gtk_entry_new();
		gtk_editable_set_editable(GTK_EDITABLE(nsteps2Entry), TRUE);
		gtk_entry_set_text(GTK_ENTRY(nsteps2Entry), "10");
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
		gtk_box_pack_start(GTK_BOX(hbox), nsteps2Entry, TRUE, TRUE, 1);
		gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 2);
	}



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

	xmlDocPtr doc;
	if ((doc = xmlReadFile(filename,NULL,XML_PARSE_DTDVALID | XML_PARSE_NOBLANKS | XML_PARSE_DTDATTR)) == NULL) {
		g_fprintf(stderr,"xmlReadFile error for %s\n", filename);
		return 0;
	}
	xmlXPathContextPtr context;
	xmlXPathObjectPtr result, result2;
	xmlNodeSetPtr nodeset, nodeset2;

	context = xmlXPathNewContext(doc);
	if (context == NULL) {
		g_fprintf(stderr, "Error in xmlXPathNewContext\n");
		return 0;
	}
	result = xmlXPathEvalExpression(BAD_CAST xpath1, context);
	if (result == NULL) {
		g_fprintf(stderr, "Error in xmlXPathEvalExpression\n");
		return 0;
	}
	if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
		xmlXPathFreeObject(result);
                g_fprintf(stderr, "No result\n");
		return 0;
	}
	nodeset = result->nodesetval;
	if (nodeset->nodeNr != 1) {
		g_fprintf(stderr,"More than one result found for xpath expression\n");
		return 0;
	}

	gboolean same_layer = FALSE;
	if (xpath2) {
		result2 = xmlXPathEvalExpression(BAD_CAST xpath2, context);
		if (result2 == NULL) {
			g_fprintf(stderr, "Error in xmlXPathEvalExpression\n");
			return 0;
		}
		if(xmlXPathNodeSetIsEmpty(result2->nodesetval)){
			xmlXPathFreeObject(result2);
        	        g_fprintf(stderr, "No result\n");
			return 0;
		}
		nodeset2 = result2->nodesetval;
		if (nodeset2->nodeNr != 1) {
			g_fprintf(stderr,"More than one result found for xpath expression\n");
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
			g_fprintf(stderr, "only PARAMETER_DOUBLE is allowed for now\n");
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
			g_fprintf(stderr, "only PARAMETER_DOUBLE is allowed for now\n");
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

static int select_parameter(GtkWidget *window, struct xmi_input *input, gchar **xpath1, gchar **xpath2, int *allowed1, int *allowed2) {
	int rv = 0;
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Select one or two variable parameters", GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
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
			g_fprintf(stdout, "One row selected after OK click\n");
			*xpath2 = NULL;
			*allowed2 = 0;
		}
		else {
			g_fprintf(stdout, "Two rows selected after OK click\n");
			gtk_tree_model_get_iter(model, &iter, path);
			gtk_tree_model_get(model, &iter, INPUT_XPATH_COLUMN, xpath2, INPUT_ALLOWED_COLUMN, allowed2, -1);
		}
		g_list_free_full(paths, (GDestroyNotify) gtk_tree_path_free);
	}
	gtk_widget_destroy(dialog);

	return rv;
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

	int arg_counter = 10;
	argv = (gchar **) g_malloc(sizeof(gchar *)*11);
	argv[0] = g_strdup(xmimsim_executable);

	if (bwd->options[j]->use_M_lines) {
		argv[1] = g_strdup("--enable-M-lines");
	}
	else
		argv[1] = g_strdup("--disable-M-lines");

	if (bwd->options[j]->use_cascade_radiative) {
		argv[2] = g_strdup("--enable-radiative-cascade");
	}
	else
		argv[2] = g_strdup("--disable-radiative-cascade");

	if (bwd->options[j]->use_cascade_auger) {
		argv[3] = g_strdup("--enable-auger-cascade");
	}
	else
		argv[3] = g_strdup("--disable-auger-cascade");

	if (bwd->options[j]->use_variance_reduction) {
		argv[4] = g_strdup("--enable-variance-reduction");
	}
	else
		argv[4] = g_strdup("--disable-variance-reduction");

	if (bwd->options[j]->use_sum_peaks) {
		argv[5] = g_strdup("--enable-pile-up");
	}
	else
		argv[5] = g_strdup("--disable-pile-up");

	if (bwd->options[j]->use_poisson) {
		argv[6] = g_strdup("--enable-poisson");
	}
	else
		argv[6] = g_strdup("--disable-poisson");

	if (gtk_combo_box_get_active(GTK_COMBO_BOX(bwd->verboseW)) == 0) {
		argv[7] = g_strdup("--verbose");
	}
	else {
		argv[7] = g_strdup("--very-verbose");
	}
	if (bwd->options[j]->use_escape_peaks) {
		argv[8] = g_strdup("--enable-escape-peaks");
	}
	else
		argv[8] = g_strdup("--disable-escape-peaks");

	if (bwd->options[j]->use_advanced_compton) {
		argv[9] = g_strdup("--enable-advanced-compton");
	}
	else
		argv[9] = g_strdup("--disable-advanced-compton");

	char *buffer;
#ifdef G_OS_WIN32
	//set solid angles and escape ratios files ourself!
	char *xmimsim_hdf5_solid_angles = NULL;
	char *xmimsim_hdf5_escape_ratios = NULL;

	if (xmi_get_solid_angle_file(&xmimsim_hdf5_solid_angles, 1) == 0) {
		buffer = g_strdup_printf("Could not determine solid angles HDF5 file\n");
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
		}
		return;
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-solid-angles-data=%s",xmimsim_hdf5_solid_angles);
	arg_counter++;

	if (xmi_get_escape_ratios_file(&xmimsim_hdf5_escape_ratios, 1) == 0) {
		buffer = g_strdup_printf("Could not determine escape ratios HDF5 file\n");
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
		}
		return;
	}
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	argv[arg_counter] = g_strdup_printf("--with-escape-ratios-data=%s",xmimsim_hdf5_escape_ratios);
	arg_counter++;
#endif

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
	if (bwd->options[j]->use_opencl) {
		argv[arg_counter++] = g_strdup("--enable-opencl");
	}
	else {
		argv[arg_counter++] = g_strdup("--disable-opencl");
	}
#endif
	if (bwd->options[j]->custom_detector_response != NULL) {
		argv = (gchar **) g_realloc(argv,sizeof(gchar *)*(arg_counter+3));
		argv[arg_counter++] = g_strdup_printf("--custom-detector-response=%s", bwd->options[j]->custom_detector_response);
	}
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

	//spawn
	spawn_rv = g_spawn_async_with_pipes(wd, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD, NULL, NULL, &xmimsim_pid, NULL, &out_fh, &err_fh, &spawn_error);

	if (spawn_rv == FALSE) {
		//couldn't spawn
		//print message to textbox in red...
		buffer = g_strdup_printf("%s\n",spawn_error->message);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		if (bwd->logFile) {
			g_fprintf(bwd->logFile,"%s",buffer);
			fclose(bwd->logFile);
		}
		g_error_free(spawn_error);
		xmimsim_pid = (GPid) -1;
		//Error dialog
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
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
	buffer = g_strdup_printf("%s was started with process id %i\n",argv[0], real_xmimsim_pid);
	xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,NULL);
	if (bwd->logFile) {
		g_fprintf(bwd->logFile,"%s",buffer);
	}
	g_free(wd);

	bwd->paused=FALSE;

	if (bwd->pauseButton)
		gtk_widget_set_sensitive(bwd->pauseButton,TRUE);

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
	g_io_add_watch(xmimsim_stdout, (GIOCondition) (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL), (GIOFunc) xmimsim_stdout_watcher, (gpointer) bwd);
	g_io_channel_unref(xmimsim_stdout);

	g_io_channel_set_encoding(xmimsim_stderr, encoding, NULL);
	g_io_channel_set_close_on_unref(xmimsim_stderr,TRUE);
	g_io_add_watch(xmimsim_stderr, (GIOCondition) (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL), (GIOFunc) xmimsim_stderr_watcher, (gpointer) bwd);
	g_io_channel_unref(xmimsim_stderr);

	return;
}



static gboolean xmimsim_stdout_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char *buffer;

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);
		if (pipe_status == G_IO_STATUS_ERROR) {
			buffer = g_strdup_printf("%s with process id %i had an I/O error: %s\n", bwd->argv0, real_xmimsim_pid,pipe_error->message);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			g_error_free(pipe_error);
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, pipe_string,-1,NULL);
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
		//buffer = g_strdup_printf("%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags2(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static gboolean xmimsim_stderr_watcher(GIOChannel *source, GIOCondition condition, struct batch_window_data *bwd) {
	gchar *pipe_string;
	GError *pipe_error=NULL;
	GIOStatus pipe_status;
	char *buffer;

	if (condition & (G_IO_IN|G_IO_PRI)) {
		/*while (gtk_events_pending ())
		        gtk_main_iteration ();*/
		pipe_status = g_io_channel_read_line (source, &pipe_string, NULL, NULL, &pipe_error);
		if (pipe_status == G_IO_STATUS_ERROR) {
			buffer = g_strdup_printf("%s with process id %i had an I/O error: %s\n", bwd->argv0, real_xmimsim_pid,pipe_error->message);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			g_error_free(pipe_error);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			return FALSE;
		}
		else if (pipe_status == G_IO_STATUS_NORMAL) {
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, pipe_string,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			g_free(pipe_string);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s", pipe_string);
			}
		}
		else
			return FALSE;

	}
	else if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) {
		//hung up...
		//buffer = g_strdup_printf("%s with process id %i had an I/O error: connection hung up\n",(char *) data, (int) xmimsim_pid);
		//my_gtk_text_buffer_insert_at_cursor_with_tags2(controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(controlsLogB),"error" ),NULL);
		return FALSE;
	}

	return TRUE;
}

static int batch_mode(GtkWidget * main_window, struct xmi_main_options **options, GSList *filenames, enum xmi_msim_batch_options, struct xmi_output ***output, gchar **filenames_xmso, int nsteps2);

static void batch_reset_controls(struct batch_window_data *bwd) {
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
	XmiMsimGuiFileChooserDialog *dialog;
	gchar *filename;

	dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select a filename for the logfile",
		GTK_WINDOW(bwd->batch_window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_SAVE,
		GTK_STOCK_CANCEL);
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

static void play_button_clicked(GtkButton *button, struct batch_window_data *bwd) {
	//first deal with the pause case
	if (bwd->pauseButton && bwd->paused) {
		gtk_widget_set_sensitive(bwd->playButton, FALSE);
		//send SIGCONT
		int kill_rv;
		char *buffer;
		g_timer_continue(bwd->timer);

#ifdef G_OS_UNIX
		kill_rv = kill((pid_t) xmimsim_pid, SIGCONT);
#elif defined(G_OS_WIN32)
		kill_rv = (int) NtResumeProcess((HANDLE) xmimsim_pid);
#endif
		if (kill_rv == 0) {
			buffer = g_strdup_printf( "Process %i was successfully resumed\n",(int) real_xmimsim_pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			gtk_widget_set_sensitive(bwd->pauseButton,TRUE);
			bwd->paused = FALSE;
		}
		else {
			buffer = g_strdup_printf( "Process %i could not be resumed\n",(int) real_xmimsim_pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			if (bwd->logFile) {
				g_fprintf(bwd->logFile,"%s",buffer);
			}
			gtk_widget_set_sensitive(bwd->playButton,TRUE);
			*(bwd->rv) = 0;
			//should end batch operation!
		}
		return;
	}
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
	char *buffer;
	int success;


	gchar *data = bwd->argv0;


	fprintf(stdout,"xmimsim_child_watcher_cb called with status: %i\n",status);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
	if (bwd->pauseButton)
		gtk_widget_set_sensitive(bwd->pauseButton,FALSE);

	//windows <-> unix issues here
	//unix allows to obtain more info about the way the process was terminated, windows will just have the exit code (status)
	//conditional compilation here
#ifdef G_OS_UNIX
	if (WIFEXITED(status)) {
		if (WEXITSTATUS(status) == 0) { /* child was terminated due to a call to exit */
			buffer = g_strdup_printf("%s with process id %i exited normally without errors\n", data, real_xmimsim_pid);
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"success" ),NULL);
			success = 1;
		}
		else {
			buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n",data, real_xmimsim_pid, WEXITSTATUS(status));
			xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
			success = 0;
		}
	}
	else if (WIFSIGNALED(status)) { /* child was terminated due to a signal */
		buffer = g_strdup_printf( "%s with process id %i was terminated by signal %i\n",data, real_xmimsim_pid, WTERMSIG(status));
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}
	else {
		buffer = g_strdup_printf( "%s with process id %i was terminated in some special way\n",data, real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
		success = 0;
	}

#elif defined(G_OS_WIN32)
	if (status == 0) {
		buffer = g_strdup_printf("%s with process id %i exited normally without errors\n", data, real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"success" ),NULL);
		success = 1;
	}
	else {
		buffer = g_strdup_printf("%s with process id %i exited with an error (code: %i)\n",data, real_xmimsim_pid, status);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
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
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations failed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		if (bwd->logFile)
			fclose(bwd->logFile);
		GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
		(GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
	        GTK_MESSAGE_ERROR,
	        GTK_BUTTONS_CLOSE,
	        "Batch execution failed with error message %s",buffer
	        );
		gtk_dialog_run (GTK_DIALOG (dialog));
		gtk_widget_destroy(dialog);
		*(bwd->rv) = 0;
		gtk_widget_destroy(bwd->batch_window);
	}
	else if (bwd->i == g_slist_length(bwd->filenames)) {
		GtkWidget *dialog;
		if (bwd->filenames_xmso != NULL) {
			int step1 = (bwd->i-1) / (bwd->nsteps2+1);
			int step2 = (bwd->i-1) % (bwd->nsteps2+1);
			GError *error = NULL;
			if (xmi_read_output_xml(bwd->filenames_xmso[bwd->i-1], &bwd->output[step1][step2], &error) == 0) {
				dialog = gtk_message_dialog_new(GTK_WINDOW(bwd->batch_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading output-file %s. Aborting batch mode", bwd->filenames_xmso[bwd->i-1]);
				gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
				g_error_free(error);
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			else {
				g_unlink(bwd->filenames_xmso[bwd->i-1]);
				g_unlink((char *) g_slist_nth_data(bwd->filenames,bwd->i-1));
			}
		}
		gtk_progress_bar_set_text(GTK_PROGRESS_BAR(bwd->progressbarW), "Simulations completed");
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bwd->progressbarW), 1.0);
		while(gtk_events_pending())
			gtk_main_iteration();
		if (bwd->logFile)
			fclose(bwd->logFile);
		if (bwd->logFile)
			fclose(bwd->logFile);
		dialog = gtk_message_dialog_new (GTK_WINDOW(bwd->batch_window),
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

		if (bwd->filenames_xmso != NULL) {
			int step1 = (bwd->i-1) / (bwd->nsteps2+1);
			int step2 = (bwd->i-1) % (bwd->nsteps2+1);
			GError *error = NULL;
			if (xmi_read_output_xml(bwd->filenames_xmso[bwd->i-1], &bwd->output[step1][step2], &error) == 0) {
				GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(bwd->batch_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading output-file %s. Aborting batch mode", bwd->filenames_xmso[bwd->i-1]);
				gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
				g_error_free(error);
				gtk_dialog_run(GTK_DIALOG(dialog));
				gtk_widget_destroy(dialog);
				return;
			}
			else {
				g_unlink(bwd->filenames_xmso[bwd->i-1]);
				g_unlink((char *) g_slist_nth_data(bwd->filenames,bwd->i-1));
			}
		}
		//start the next job
		batch_start_job_recursive(bwd);
	}



	return;
}
static void stop_button_clicked(GtkButton *button, struct batch_window_data *bwd) {
	char *buffer;

	gtk_widget_set_sensitive(bwd->stopButton,FALSE);

	if (bwd->pauseButton)
		gtk_widget_set_sensitive(bwd->pauseButton,FALSE);

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
		buffer = g_strdup_printf( "Process %i was successfully terminated before completion\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %i could not be terminated with the SIGTERM signal\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	}

#elif defined(G_OS_WIN32)
	BOOL terminate_rv;

	terminate_rv = TerminateProcess((HANDLE) xmimsim_pid, (UINT) 1);

	if (terminate_rv == TRUE) {
		buffer = g_strdup_printf( "Process %i was successfully terminated\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
	}
	else {
		buffer = g_strdup_printf( "Process %i could not be terminated with the TerminateProcess call\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"error" ),NULL);
	}
#endif
	if (bwd->logFile)
		g_fprintf(bwd->logFile,"%s",buffer);

	return;
}

static void pause_button_clicked(GtkButton *button, struct batch_window_data *bwd) {
	//UNIX only

	int kill_rv;
	char *buffer;

	g_timer_stop(bwd->timer);

	gtk_widget_set_sensitive(bwd->pauseButton,FALSE);
	gtk_widget_set_sensitive(bwd->stopButton,FALSE);
#ifdef G_OS_UNIX
	kill_rv = kill((pid_t) xmimsim_pid, SIGSTOP);
#elif defined(G_OS_WIN32)
	kill_rv = (int) NtSuspendProcess((HANDLE) xmimsim_pid);
#endif
	if (kill_rv == 0) {
		buffer = g_strdup_printf( "Process %i was successfully paused. Press the Play button to continue or Stop to kill the process\n", real_xmimsim_pid);
		xmi_msim_gui_utils_text_buffer_insert_at_cursor_with_tags(bwd->controlsLogW, bwd->timer, bwd->controlsLogB, buffer,-1,gtk_text_tag_table_lookup(gtk_text_buffer_get_tag_table(bwd->controlsLogB),"pause-continue-stopped" ),NULL);
		bwd->paused=TRUE;
		if (bwd->logFile)
			g_fprintf(bwd->logFile, "%s", buffer);
		gtk_widget_set_sensitive(bwd->stopButton,TRUE);
		gtk_widget_set_sensitive(bwd->playButton,TRUE);
	}

}

struct wizard_close_data {
	struct xmi_main_options **options;
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
		ows[i] = xmi_msim_gui_options_box_new(main_window);
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
	wcd->options = (struct xmi_main_options **) g_malloc(sizeof(struct xmi_main_options *) * g_slist_length(filenames));
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



static int general_options(GtkWidget *main_window, struct xmi_main_options **options) {
	GtkWidget *dialog = gtk_dialog_new_with_buttons("Set the options for the simulations batch", GTK_WINDOW(main_window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
	GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
	GtkWidget *ow = xmi_msim_gui_options_box_new(main_window);
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


void batchmode_button_clicked_cb(GtkWidget *button, GtkWidget *window) {

	//g_fprintf(stdout,"Entering batchnode_button_clicked_cb...\n");
	//open dialog
	XmiMsimGuiFileChooserDialog *file_dialog = xmi_msim_gui_file_chooser_dialog_new(
		"Select one or more files",
		GTK_WINDOW(window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_OPEN, GTK_STOCK_CANCEL);
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
	//for (i = 0 ; i < g_slist_length(filenames) ; i++) {
	//	g_fprintf(stdout,"filename: %s\n", (char *) g_slist_nth_data(filenames,i));
	//}

   	xmi_msim_gui_file_chooser_dialog_destroy(file_dialog);
	GtkWidget *dialog;
	struct xmi_main_options **options;
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
			const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();
			xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", "MULTIPLE-FILES-MULTIPLE-OPTIONS", NULL);
#endif
		}
		else if (response == GTK_RESPONSE_NO) {
			//options apply to all
   			gtk_widget_destroy (dialog);
			options = (struct xmi_main_options **) g_malloc(sizeof(struct xmi_main_options *));
			int rv = general_options(window, options);
			if (rv == 0) {
				return;
			}
			//options are set
#ifdef HAVE_GOOGLE_ANALYTICS
			const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();
			xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", "MULTIPLE-FILES-SINGLE-OPTION", NULL);
#endif
		}
		else {
   			gtk_widget_destroy (dialog);
			return;
		}
		//3) launch execution window
		batch_mode(window, options, filenames, response == GTK_RESPONSE_YES ? XMI_MSIM_BATCH_MULTIPLE_OPTIONS : XMI_MSIM_BATCH_ONE_OPTION, NULL, NULL, 0);
		//4) display message with result
		//g_fprintf(stdout,"exec_rv: %i\n", exec_rv);
	}
	else {
		//one file selected
		//options apply to all
		//g_fprintf(stdout, "no clicked\n");
		gchar *xpath1, *xpath2;
		struct xmi_input *input;
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
		//g_fprintf(stdout,"select_parameter rv: %i\n", rv);
		if (rv == 1) {
			g_debug("xpath1: %s", xpath1);
			g_debug("allowed1: %i", allowed1);
			if (xpath2) {
				g_debug("xpath2: %s", xpath2);
				g_debug("allowed2: %i", allowed2);
			}
#ifdef HAVE_GOOGLE_ANALYTICS
			const XmiMsimGuiGoogleAnalyticsTracker *tracker = xmi_msim_gui_google_analytics_tracker_get_global();
			gchar *event_label = NULL;
			if (xpath2)
				event_label = g_strdup_printf("SINGLE-FILE-%s-%s", xpath1, xpath2);
			else
				event_label = g_strdup_printf("SINGLE-FILE-%s", xpath1);
			xmi_msim_gui_google_analytics_tracker_send_event(tracker, "XMI-MSIM-GUI", "BATCH-SIMULATION-START", event_label, NULL);
			g_free(event_label);
#endif
		}
		else
			return;
		//generate wizard
		//1) options
		//2) range of parameter
		//3) plot afterwards? Requires saving to XMSA file as well as selecting a particular line
		struct archive_options_data *aod = (struct archive_options_data *) g_malloc(sizeof(struct archive_options_data));
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
			g_fprintf(stderr,"xmlReadFile error for %s\n", filename);
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
		struct xmi_output ***output = (struct xmi_output ***) g_malloc(sizeof(struct xmi_output **)*(aod->nsteps1+1));
		for (i = 0 ; i <= aod->nsteps1 ; i++)
			output[i] = (struct xmi_output **) g_malloc(sizeof(struct xmi_output *)*(aod->nsteps2+1));
		int exec_rv = batch_mode(window, &aod->xmo, filenames_xmsiGSL, XMI_MSIM_BATCH_ONE_OPTION, output, filenames_xmso, aod->nsteps2);
		if (exec_rv == 0) {
			return;
		}

		dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);
		gtk_window_set_decorated(GTK_WINDOW(dialog), FALSE);
		gtk_window_set_transient_for(GTK_WINDOW(dialog), GTK_WINDOW(window));
		gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
		gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
		gtk_window_set_position (GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
		GtkWidget *main_vbox = gtk_vbox_new(FALSE,0);
		label = gtk_label_new(NULL);
		gtk_label_set_markup(GTK_LABEL(label),"<b>Converting XMSO files to archive</b>");
		gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
		gtk_box_pack_start(GTK_BOX(main_vbox), label, TRUE, FALSE, 10);
		gtk_widget_show(label);
		GtkWidget *label2 = gtk_label_new("This may take a while...");
		gtk_box_pack_start(GTK_BOX(main_vbox), label2, FALSE, FALSE, 10);
		gtk_widget_show(label2);
		gtk_widget_show(main_vbox);
		gtk_container_add(GTK_CONTAINER(dialog), main_vbox);
		gtk_container_set_border_width(GTK_CONTAINER(dialog),5);
		gtk_window_set_default_size(GTK_WINDOW(dialog),200,50);
		g_signal_connect(G_OBJECT(dialog), "delete-event", G_CALLBACK(gtk_true), NULL);
		gtk_widget_show_all(dialog);
		GdkCursor* watchCursor = gdk_cursor_new(GDK_WATCH);
		gdk_window_set_cursor(gtk_widget_get_window(dialog), watchCursor);
		while(gtk_events_pending())
		    gtk_main_iteration();


		//convert to an archive struct
		struct xmi_archive *archive = xmi_archive_raw2struct(output, aod->start_value1, aod->end_value1, aod->nsteps1, xpath1, aod->start_value2, aod->end_value2, aod->nsteps2, xpath2);
		//save to XMSA file
		//g_fprintf(stdout, "Writing %s\n", aod->xmsa_file);

		gtk_label_set_markup(GTK_LABEL(label),"<b>Saving XMSA file</b>");
		while(gtk_events_pending())
		    gtk_main_iteration();

		error = NULL;
		if (xmi_write_archive_xml(aod->xmsa_file, archive, &error) == 0) {
			gtk_widget_destroy(dialog);
			dialog = gtk_message_dialog_new(GTK_WINDOW(window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error writing to archive-file %s. Aborting batch mode", aod->xmsa_file);
			gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog), "%s", error->message);
			g_error_free(error);
			gtk_dialog_run(GTK_DIALOG(dialog));
			gtk_widget_destroy(dialog);
			return;
		}

		gtk_label_set_markup(GTK_LABEL(label),"<b>Freeing XMSO memory</b>");
		while(gtk_events_pending())
		    gtk_main_iteration();

		//g_fprintf(stdout, "Freeing output\n");
		for (i = 0 ; i <= aod->nsteps1 ; i++) {
			for (j = 0 ; j <= aod->nsteps2 ; j++) {
				xmi_free_output(output[i][j]);
			}
			g_free(output[i]);
		}
		g_free(output);

		g_strfreev(filenames_xmsi);
		g_strfreev(filenames_xmso);
		g_slist_free(filenames_xmsiGSL);

		//and plot!
		gtk_widget_destroy(dialog);
		launch_archive_plot(archive, window);
	}



	return;
}

static int batch_mode(GtkWidget *main_window, struct xmi_main_options **options, GSList *filenames, enum xmi_msim_batch_options batch_options, struct xmi_output ***output, gchar **filenames_xmso, int nsteps2) {
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

	GtkWidget *main_vbox = gtk_vbox_new(FALSE,0);
	gtk_container_add(GTK_CONTAINER(batch_window), main_vbox);
	gtk_container_set_border_width(GTK_CONTAINER(batch_window),3);
	GtkWidget *hbox = gtk_hbox_new(FALSE, 2);

	bwd->playButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(bwd->playButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY,GTK_ICON_SIZE_DIALOG));
	gtk_box_pack_start(GTK_BOX(hbox), bwd->playButton, FALSE, FALSE, 2);

	bwd->pauseButton = NULL;
#ifdef G_OS_WIN32
	HMODULE ntdll = LoadLibrary( "ntdll.dll" );
	if (ntdll) {
		NtSuspendProcess = (pNtSuspendProcess)GetProcAddress(ntdll, "NtSuspendProcess" );
		NtResumeProcess = (pNtResumeProcess)GetProcAddress(ntdll, "NtResumeProcess" );
		FreeLibrary(ntdll);
		if (NtSuspendProcess != NULL && NtResumeProcess != NULL) {
#endif
			bwd->pauseButton = gtk_button_new();
			gtk_container_add(GTK_CONTAINER(bwd->pauseButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PAUSE,GTK_ICON_SIZE_DIALOG));
			gtk_box_pack_start(GTK_BOX(hbox), bwd->pauseButton, FALSE, FALSE, 2);
			gtk_widget_set_sensitive(bwd->pauseButton, FALSE);
#ifdef G_OS_WIN32
		}
	}
#endif

	bwd->stopButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(bwd->stopButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_DIALOG));
	gtk_box_pack_start(GTK_BOX(hbox), bwd->stopButton, FALSE, FALSE, 2);
	gtk_widget_set_sensitive(bwd->stopButton, FALSE);

	GtkWidget *nthreadsW = NULL;
	if (xmi_omp_get_max_threads() > 1) {
		GtkWidget *cpuLabel = gtk_label_new("CPUs");
		gtk_box_pack_start(GTK_BOX(hbox), cpuLabel, FALSE, FALSE, 2);
		GtkAdjustment *nthreadsA = GTK_ADJUSTMENT(gtk_adjustment_new((gdouble) xmi_omp_get_max_threads(), 1.0, (gdouble) xmi_omp_get_max_threads(), 1.0,1.0,0.0));
		nthreadsW = gtk_hscale_new(GTK_ADJUSTMENT(nthreadsA));
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

	GtkWidget *verboseW = gtk_combo_box_text_new();
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Verbose");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(verboseW),"Very verbose");
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
	bwd->paused = FALSE;
	bwd->logFile = NULL;
	gtk_widget_show_all(batch_window);

	gtk_main();

	return rv;
}

void launch_archive_plot(struct xmi_archive *archive, GtkWidget *main_window) {
	GtkWidget *window = xmi_msim_gui_xmsa_viewer_window_new(main_window, archive);
	gtk_widget_show(window);
}

