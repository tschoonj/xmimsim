/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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

#include "xmimsim-gui.h"
#include "xmimsim-gui-controls.h"
#include <glib.h>
#include <string.h>
#include <stdlib.h>

struct window_entry {
	GtkWidget *window;
	GtkWidget *entry;
};


GtkWidget *executableW;
GtkWidget *MlinesW;
GtkWidget *rad_cascadeW;
GtkWidget *nonrad_cascadeW;
GtkWidget *variance_reductionW;
GtkWidget *pile_upW;
GtkWidget *spe_convW;
GtkWidget *spe_uconvW;
GtkWidget *csv_convW;
GtkWidget *csv_uconvW;
GtkWidget *svg_convW;
GtkWidget *svg_uconvW;
GtkWidget *html_convW;
GtkWidget *html_uconvW;

GtkWidget *playButton;
GtkWidget *pauseButton;
GtkWidget *stopButton;

static gboolean executable_file_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	return g_file_test(filter_info->filename,G_FILE_TEST_IS_EXECUTABLE);
}


static void select_executable_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;


	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, executable_file_filter, NULL, NULL);
	gtk_file_filter_set_name(filter,"Executables");
	dialog = gtk_file_chooser_dialog_new ("Open simulation executable",
		GTK_WINDOW((GtkWidget *) data),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
																
	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_entry_set_text(GTK_ENTRY(executableW),filename);
		g_free(filename);
	}
	gtk_widget_destroy(dialog);
	
}

static void select_extra_output_cb(GtkButton *button, gpointer data) {
	GtkWidget *dialog;
	GtkFileFilter *filter=NULL;
	gchar *filename;
	struct window_entry *we = (struct window_entry *) data;
	gchar title[512];

	if (we->entry == spe_convW) {
		//gtk_file_filter_add_pattern(filter,"*.spe");
		//gtk_file_filter_set_name(filter,"SPE spectral data files");
		strcpy(title,"Select the prefix of the SPE files");
	}
	else if (we->entry == svg_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.svg");
		gtk_file_filter_set_name(filter,"Scalable Vector Graphics");
		strcpy(title,"Select the name of the SVG file");
	}
	else if (we->entry == csv_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.csv");
		gtk_file_filter_set_name(filter,"Comma separated value files");
		strcpy(title,"Select the name of the CSV file");
	}
	else if (we->entry == html_convW) {
		filter = gtk_file_filter_new();
		gtk_file_filter_add_pattern(filter, "*.html");
		gtk_file_filter_set_name(filter,"Hypertext Markup Language");
		strcpy(title,"Select the name of the HTML report file");
	}
	
	dialog = gtk_file_chooser_dialog_new(title,
		GTK_WINDOW(we->window),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL
	);
	if (filter)
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);


	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		if (we->entry == svg_convW) {
			if (strcmp(filename+strlen(filename)-4, ".svg") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".svg");
			}
		}
		else if (we->entry == csv_convW) {
			if (strcmp(filename+strlen(filename)-4, ".csv") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+5));
				strcat(filename,".csv");
			}
		}
		else if (we->entry == html_convW) {
			if (strcmp(filename+strlen(filename)-5, ".html") != 0) {
				filename = (gchar *) realloc(filename,sizeof(gchar)*(strlen(filename)+6));
				strcat(filename,".html");
			}
		}
			
		gtk_entry_set_text(GTK_ENTRY(we->entry),filename);
		g_free(filename);

	}
	gtk_widget_destroy(dialog);


}

GtkWidget *init_simulation_controls(GtkWidget *window) {

	GtkWidget *superframe;
	GtkWidget *vbox_notebook;
	GtkWidget *frame;
	GtkWidget *hbox_text_label,*label;
	gchar *xmimsim_executable;
	GtkWidget *scrolled_window;
	GtkWidget *button;
	struct window_entry *we;

	GtkWidget *buttonbox;
	GtkWidget *progressbar;
	GtkWidget *hbox_controls;



	superframe = gtk_vbox_new(FALSE,2);

	frame = gtk_frame_new("Executable");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Executable</span>");


	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);

	//Executable
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Executable");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	label = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	g_signal_connect(G_OBJECT(label),"clicked",G_CALLBACK(select_executable_cb), (gpointer) window);
	xmimsim_executable = g_find_program_in_path("xmimsim");
	executableW = gtk_entry_new();
	gtk_entry_set_width_chars(GTK_ENTRY(executableW),80);
	if (xmimsim_executable == NULL) {
		//bad...
		gtk_entry_set_text(GTK_ENTRY(executableW),"xmimsim");
	}
	else {
		gtk_entry_set_text(GTK_ENTRY(executableW),xmimsim_executable);
	}
	gtk_editable_set_editable(GTK_EDITABLE(executableW), FALSE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),executableW,FALSE,FALSE,0);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);


	//options
	frame = gtk_frame_new("Options");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Options</span>");


	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	MlinesW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(MlinesW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(MlinesW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),MlinesW, TRUE, FALSE, 3);

	rad_cascadeW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(rad_cascadeW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rad_cascadeW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),rad_cascadeW, TRUE, FALSE, 3);

	nonrad_cascadeW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(nonrad_cascadeW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nonrad_cascadeW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),nonrad_cascadeW, TRUE, FALSE, 3);

	variance_reductionW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(variance_reductionW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(variance_reductionW),TRUE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),variance_reductionW, TRUE, FALSE, 3);

	pile_upW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(pile_upW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pile_upW),FALSE);
	gtk_box_pack_start(GTK_BOX(vbox_notebook),pile_upW, TRUE, FALSE, 3);
	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);

	//Exports
	frame = gtk_frame_new("Export results");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Export results</span>");


	vbox_notebook = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(vbox_notebook),10);
	//hdf5 file??? too advanced
	//SPE file
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("SPE file prefix");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	spe_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(spe_convW),"Setting the prefix will result in the generation of SPE type files containing the spectral data. Compatible with PyMca and AXIL. One file is generated per interaction order.");
	gtk_entry_set_width_chars(GTK_ENTRY(spe_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = spe_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(spe_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),spe_convW,FALSE,FALSE,0);

	//SVG files
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Scalable Vector Graphics (SVG) file");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Export the spectra as Scalable Vector Graphics.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Export the spectra as Scalable Vector Graphics.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	svg_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(svg_convW),"Export the spectra as Scalable Vector Graphics.");
	gtk_entry_set_width_chars(GTK_ENTRY(svg_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = svg_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(svg_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),svg_convW,FALSE,FALSE,0);
	
	//CSV files
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Comma Separated Values (CSV) file");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	csv_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(csv_convW),"Export the spectra as Comma Separated Values files. Readable by Microsoft Excel and other programs.");
	gtk_entry_set_width_chars(GTK_ENTRY(csv_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = csv_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(csv_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),csv_convW,FALSE,FALSE,0);
	
	//html files
	hbox_text_label = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(vbox_notebook), hbox_text_label, TRUE, FALSE, 3);
	label = gtk_label_new("Report HTML file");
	gtk_widget_set_tooltip_text(GTK_WIDGET(label),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	gtk_box_pack_start(GTK_BOX(hbox_text_label),label,FALSE,FALSE,0);
	button = gtk_button_new_from_stock(GTK_STOCK_SAVE_AS);
	gtk_widget_set_tooltip_text(GTK_WIDGET(button),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	gtk_box_pack_end(GTK_BOX(hbox_text_label),button,FALSE,FALSE,0);
	html_convW = gtk_entry_new();
	gtk_widget_set_tooltip_text(GTK_WIDGET(html_convW),"Produces an interactive HTML file containing an overview of all the results produced by the simulation: spectra and tables of all the individual XRF lines. Readable with recent versions of Firefox, Chrome and Safari.");
	gtk_entry_set_width_chars(GTK_ENTRY(html_convW),60);
	we = (struct window_entry *) malloc(sizeof(struct window_entry));
	we->entry = html_convW;
	we->window = window;
	g_signal_connect(G_OBJECT(button),"clicked",G_CALLBACK(select_extra_output_cb), (gpointer) we);
	gtk_editable_set_editable(GTK_EDITABLE(html_convW), TRUE);
	gtk_box_pack_end(GTK_BOX(hbox_text_label),html_convW,FALSE,FALSE,0);
	



	gtk_container_add(GTK_CONTAINER(frame),vbox_notebook);

	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);

	//actual controls
	frame = gtk_frame_new("Simulation controls");
	gtk_frame_set_label_align(GTK_FRAME(frame),0.5,0.0);
	gtk_container_set_border_width(GTK_CONTAINER(frame),5);
	gtk_label_set_markup(GTK_LABEL(gtk_frame_get_label_widget(GTK_FRAME(frame))), "<span size=\"large\">Simulation controls</span>");


	//playButton = gtk_button_new_from_stock(GTK_STOCK_MEDIA_PLAY);
	playButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(playButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PLAY,GTK_ICON_SIZE_LARGE_TOOLBAR));
	stopButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(stopButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_STOP,GTK_ICON_SIZE_LARGE_TOOLBAR));
	pauseButton = gtk_button_new();
	gtk_container_add(GTK_CONTAINER(pauseButton),gtk_image_new_from_stock(GTK_STOCK_MEDIA_PAUSE,GTK_ICON_SIZE_LARGE_TOOLBAR));
	buttonbox = gtk_vbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(buttonbox), playButton, FALSE,FALSE,3);
	gtk_box_pack_start(GTK_BOX(buttonbox), pauseButton, FALSE,FALSE,3);
	gtk_box_pack_start(GTK_BOX(buttonbox), stopButton, FALSE,FALSE,3);
	gtk_widget_set_sensitive(pauseButton,FALSE);
	gtk_widget_set_sensitive(stopButton,FALSE);
	hbox_controls = gtk_hbox_new(FALSE,5);
	gtk_box_pack_start(GTK_BOX(hbox_controls), buttonbox, FALSE, FALSE, 3);
	progressbar = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(progressbar), GTK_PROGRESS_LEFT_TO_RIGHT);
	gtk_box_pack_start(GTK_BOX(hbox_controls), progressbar, FALSE, FALSE, 3);

	gtk_container_set_border_width(GTK_CONTAINER(hbox_controls),10);
	gtk_container_add(GTK_CONTAINER(frame),hbox_controls);
	gtk_box_pack_start(GTK_BOX(superframe),frame, FALSE, FALSE,2);




	//finalize widget
	scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);





	return scrolled_window;
}

