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

#include "xmimsim-gui-tools.h"
#include <string.h>
#include "xmi_xslt.h"

struct xmi_tools {
	GtkWidget *window;
	GtkWidget *entry;
	GtkWidget *entry1;
	GtkWidget *entry2;
	GtkWidget *entry3;
	GtkWidget *button1;
	GtkWidget *button2;
	GtkWidget *apply;
};


static void xmso_open_button_clicked_cb(GtkButton *button, gpointer data) {
	
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;
	struct xmi_tools *xt = (struct xmi_tools *) data;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmso");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");

	dialog = gtk_file_chooser_dialog_new("Select an XMI-MSIM outputfile",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);

	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);
		g_free(filename);
	}
	gtk_widget_destroy(dialog);

}
static void xmsi_save_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmsi");
	gtk_file_filter_set_name(filter,"XMI-MSIM inputfiles");
	struct xmi_tools *xt = (struct xmi_tools *) data;

	dialog = gtk_file_chooser_dialog_new("Select the name for the new XMI-MSIM inputfile",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//check extension
		if (strcmp(filename+strlen(filename)-5, ".xmsi") != 0) {
			filename = (gchar *) g_realloc(filename,sizeof(gchar)*(strlen(filename)+6));
			strcat(filename,".xmsi");
		}

		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);
		g_free (filename);							
	}

	gtk_widget_destroy (dialog);
}
static void xmso_save_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.xmso");
	gtk_file_filter_set_name(filter,"XMI-MSIM outputfiles");
	struct xmi_tools *xt = (struct xmi_tools *) data;

	dialog = gtk_file_chooser_dialog_new("Select the name for the new XMI-MSIM outputfile",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//check extension
		if (strcmp(filename+strlen(filename)-5, ".xmso") != 0) {
			filename = (gchar *) g_realloc(filename,sizeof(gchar)*(strlen(filename)+6));
			strcat(filename,".xmso");
		}

		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);
		g_free (filename);							
	}

	gtk_widget_destroy (dialog);
}

static void csv_save_button_clicked_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(filter,"*.csv");
	gtk_file_filter_set_name(filter,"Comma separated values file");
	struct xmi_tools *xt = (struct xmi_tools *) data;

	dialog = gtk_file_chooser_dialog_new("Select the name for the new CSV file",
		GTK_WINDOW(xt->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);

	gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		//check extension
		if (strcmp(filename+strlen(filename)-4, ".csv") != 0) {
			filename = (gchar *) g_realloc(filename,sizeof(gchar)*(strlen(filename)+5));
			strcat(filename,".csv");
		}

		gtk_entry_set_text(GTK_ENTRY(xt->entry), filename);
		g_free (filename);							
	}

	gtk_widget_destroy (dialog);
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

	const gchar *xmsofile = gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	const gchar *xmsifile = gtk_entry_get_text(GTK_ENTRY(xt->entry2));
	const gchar *outputfile= gtk_entry_get_text(GTK_ENTRY(xt->entry3));

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

	const gchar *xmsofile = gtk_entry_get_text(GTK_ENTRY(xt->entry1));
	const gchar *csvfile = gtk_entry_get_text(GTK_ENTRY(xt->entry2));
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

void xmimsim_gui_xmso2xmsi(GtkMenuItem *menuitem, gpointer data) {

	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *master_box, *boxke;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt3, *xt4;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt3 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	master_box = gtk_vbox_new(FALSE,2);
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	xt4->window = window;
	gtk_window_set_title(GTK_WINDOW(window), "Convert XMSO to XMSI");
	//gtk_widget_set_size_request(window,450,250);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	//gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(window), 3);


	GtkWidget *frame = gtk_frame_new(NULL);
	label = gtk_label_new("This tool allows for the extraction of an XMI-MSIM inputfile (XMSI) from an XMI-MSIM outputfile. It is possible to change the name of the outputfile in the extracted XMSI file");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label),GTK_JUSTIFY_CENTER);
	gtk_misc_set_padding(GTK_MISC(label),2,2);
	gtk_container_add(GTK_CONTAINER(frame), label);
	gtk_box_pack_start(GTK_BOX(master_box), frame, FALSE,FALSE,1);


	GtkWidget *table = gtk_table_new(3, 3, FALSE);
	gtk_table_set_row_spacings(GTK_TABLE(table), 3);
	gtk_table_set_col_spacings(GTK_TABLE(table), 3);
	label = gtk_label_new("XMSO file");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 0, 1);
	text = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(text),60);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_table_attach_defaults(GTK_TABLE(table), text, 1, 2, 0, 1);
	button = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	gtk_table_attach_defaults(GTK_TABLE(table), button, 2, 3, 0, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;


	label = gtk_label_new("XMSI file");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 1, 2);
	text = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(text),60);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_table_attach_defaults(GTK_TABLE(table), text, 1, 2, 1, 2);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE);
	gtk_table_attach_defaults(GTK_TABLE(table), button, 2, 3, 1, 2);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmsi_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;

	label = gtk_label_new("New XMSO file");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 2, 3);
	text = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(text), "(optional)");
	gtk_entry_set_width_chars(GTK_ENTRY(text),60);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_table_attach_defaults(GTK_TABLE(table), text, 1, 2, 2, 3);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE);
	gtk_table_attach_defaults(GTK_TABLE(table), button, 2, 3, 2, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_save_button_clicked_cb), xt3);
	xt3->window = window;
	xt3->entry = text;
	xt4->entry3 = text;

	gtk_box_pack_start(GTK_BOX(master_box), table, FALSE ,FALSE,2);

	gtk_box_pack_start(GTK_BOX(master_box), gtk_hseparator_new(), FALSE, FALSE, 2);

	button = gtk_button_new_from_stock(GTK_STOCK_APPLY);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2xmsi_apply_button_clicked_cb), xt4);
	
	boxke = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(boxke), button, TRUE, FALSE, 0);

	gtk_box_pack_start(GTK_BOX(master_box), boxke, FALSE, FALSE,2);

	gtk_container_add(GTK_CONTAINER(window), master_box);


	gtk_widget_show_all(window);
}

void xmimsim_gui_xmso2csv(GtkMenuItem *menuitem, gpointer data) {

	GtkWidget *main_window = (GtkWidget *) data;
	GtkWidget *window;
	GtkWidget *master_box, *boxke;
	GtkWidget *button;
	GtkWidget *label, *text;
	struct xmi_tools *xt1, *xt2, *xt4;
	GtkWidget *button1, *button2;

	xt1 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt2 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));
	xt4 = (struct xmi_tools *) g_malloc(sizeof(struct xmi_tools));

	master_box = gtk_vbox_new(FALSE,2);
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	xt4->window = window;
	gtk_window_set_title(GTK_WINDOW(window), "Convert XMSO to CSV");
	//gtk_widget_set_size_request(window,450,250);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));
	//gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(window), 3);

	GtkWidget *frame = gtk_frame_new(NULL);
	label = gtk_label_new("This tool allows for the extraction of the generated spectra as comma separated value files (CSV)from XMI-MSIM outputfiles. Both the convoluted and unconvoluted data are available for extraction");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label),GTK_JUSTIFY_CENTER);
	gtk_misc_set_padding(GTK_MISC(label),2,2);
	gtk_container_add(GTK_CONTAINER(frame), label);
	gtk_box_pack_start(GTK_BOX(master_box), frame, FALSE,FALSE,1);

	GtkWidget *table = gtk_table_new(3, 3, FALSE);
	gtk_table_set_row_spacings(GTK_TABLE(table), 3);
	gtk_table_set_col_spacings(GTK_TABLE(table), 3);
	label = gtk_label_new("XMSO file");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 0, 1);
	text = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(text),60);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_table_attach_defaults(GTK_TABLE(table), text, 1, 2, 0, 1);
	button = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	gtk_table_attach_defaults(GTK_TABLE(table), button, 2, 3, 0, 1);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso_open_button_clicked_cb), xt1);
	xt1->window = window;
	xt1->entry = text;
	xt4->entry1 = text;

	label = gtk_label_new("CSV file");
	gtk_table_attach_defaults(GTK_TABLE(table), label, 0, 1, 1, 2);
	text = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(text),60);
	gtk_editable_set_editable(GTK_EDITABLE(text), FALSE);
	gtk_table_attach_defaults(GTK_TABLE(table), text, 1, 2, 1, 2);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE);
	gtk_table_attach_defaults(GTK_TABLE(table), button, 2, 3, 1, 2);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(csv_save_button_clicked_cb), xt2);
	xt2->window = window;
	xt2->entry = text;
	xt4->entry2 = text;
	
	gtk_box_pack_start(GTK_BOX(master_box), table, FALSE ,FALSE,2);
	//checkbutton here
	button1 = gtk_radio_button_new_with_label_from_widget(NULL, "Use spectra after detector convolution");
	gtk_box_pack_start(GTK_BOX(master_box), button1, FALSE, FALSE,3);
	xt4->button1 = button1;	
	button2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(button1), "Use spectra before detector convolution");
	gtk_box_pack_start(GTK_BOX(master_box), button2, FALSE, FALSE,3);
	xt4->button2 = button2;	
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button1), TRUE);

	gtk_box_pack_start(GTK_BOX(master_box), gtk_hseparator_new(), FALSE, FALSE, 2);

	button = gtk_button_new_from_stock(GTK_STOCK_APPLY);
	xt4->apply = button;
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(xmso2csv_apply_button_clicked_cb), xt4);
	
	boxke = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(boxke), button, TRUE, FALSE, 0);

	gtk_box_pack_start(GTK_BOX(master_box), boxke, FALSE, FALSE,2);

	gtk_container_add(GTK_CONTAINER(window), master_box);


	gtk_widget_show_all(window);
}
