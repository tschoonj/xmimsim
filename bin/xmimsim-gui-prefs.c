/*
 * Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>

#include "xmimsim-gui.h"
#include "xmimsim-gui-prefs.h"
#include "xmi_detector.h"
#include "xmi_solid_angle.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <xmi_aux.h>
#ifdef MAC_INTEGRATION
        #import <Foundation/Foundation.h>
#endif

GtkWidget *Mlines_prefsW;
GtkWidget *rad_cascade_prefsW;
GtkWidget *nonrad_cascade_prefsW;
GtkWidget *variance_reduction_prefsW;
GtkWidget *pile_up_prefsW;
GtkWidget *poisson_prefsW;
GtkWidget *escape_peaks_prefsW;
GtkWidget *nchannels_prefsW;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
GtkWidget *opencl_prefsW;
#endif

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
GtkWidget *check_updates_prefsW;
#endif
#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
GtkWidget *notifications_prefsW;
#endif
enum {
	URL_COLUMN_PREFS,
	N_COLUMNS_PREFS
};


static int download_current_index;
static int download_current_nindices;
static GtkTreeIter current_iter;

const gchar * const xmimsim_download_locations[] = {
		"http://lvserver.ugent.be/xmi-msim",
		"http://xmi-msim.s3.amazonaws.com",
		NULL};

static gboolean hdf5_file_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

	int file_kind = xmi_get_hdf5_kind((char *) (filter_info->filename));

	if (kind != file_kind)
		return FALSE;
	return TRUE;
}

static void import_hdf5_data_cb(GtkWidget *window, int kind) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, hdf5_file_filter, GINT_TO_POINTER(kind), NULL);
	if (kind == XMI_HDF5_SOLID_ANGLES) {
		gtk_file_filter_set_name(filter,"XMI-MSIM Solid angles file");
	}
	else if (kind == XMI_HDF5_ESCAPE_RATIOS) {
		gtk_file_filter_set_name(filter,"XMI-MSIM Escape ratios file");
	}
	dialog = gtk_file_chooser_dialog_new ("Open XMI-MSIM HDF5 file",
		GTK_WINDOW(window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	GtkWidget *forceW = gtk_check_button_new_with_label("Force copying");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(forceW), FALSE);
	gtk_widget_show_all(forceW);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog), forceW);


	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gboolean forced = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(forceW));
		gtk_widget_destroy(dialog);
		char *target = NULL;

		if (kind == XMI_HDF5_SOLID_ANGLES) {
			if (xmi_get_solid_angle_file(&target, 1) == 0) {
				dialog = gtk_message_dialog_new (GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not determine the location of the solid angle grids file."
	       			);
	     			gtk_dialog_run (GTK_DIALOG (dialog));
	     			gtk_widget_destroy (dialog);
	     			return;
			}
		}
		else if (kind == XMI_HDF5_ESCAPE_RATIOS) {
			if (xmi_get_escape_ratios_file(&target, 1) == 0) {
				dialog = gtk_message_dialog_new (GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not determine the location of the escape ratios file."
	       			);
	     			gtk_dialog_run (GTK_DIALOG (dialog));
	     			gtk_widget_destroy (dialog);
	     			return;
			}
		}
		int ncopied = xmi_copy_between_hdf5_files(kind, filename, target, NULL, forced ? 1 : 0);

		if (ncopied >=0) {
			dialog = gtk_message_dialog_new (GTK_WINDOW(window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_INFO,
			GTK_BUTTONS_CLOSE,
			"%i groups were successfully copied from %s to %s",
			ncopied,
			filename,
			target
	       		);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
		}
		else {
			dialog = gtk_message_dialog_new (GTK_WINDOW(window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_ERROR,
			GTK_BUTTONS_CLOSE,
			"An error occurred while copying from %s to %s",
			filename,
			target
	       		);
	     		gtk_dialog_run (GTK_DIALOG (dialog));
		}

		g_free(filename);
		free(target);
	}
	gtk_widget_destroy(dialog);


}


static void import_solid_angles_clicked_cb(GtkWidget *button, GtkWidget *window) {
	import_hdf5_data_cb(window, XMI_HDF5_SOLID_ANGLES);
}

static void import_escape_ratios_clicked_cb(GtkWidget *button, GtkWidget *window) {
	import_hdf5_data_cb(window, XMI_HDF5_ESCAPE_RATIOS);
}

static void delete_solid_angles_clicked_cb(GtkWidget *button, gpointer data) {
	GtkWidget *window = (GtkWidget *) data;
	
	//generate dialog
	GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_MODAL, GTK_MESSAGE_WARNING, GTK_BUTTONS_YES_NO,
		"Are you certain you want to delete the solid angles HDF5 file? This operation cannot be undone!");
	gint rv = gtk_dialog_run(GTK_DIALOG(dialog));
	if (rv == GTK_RESPONSE_YES) {
		//delete the file
		char *file=NULL;
		xmi_get_solid_angle_file(&file, 0);
		g_fprintf(stdout, "solid_angle_file: %s\n",file);
		g_unlink(file);
		free(file);
	}
	gtk_widget_destroy(dialog);
}

static void delete_escape_ratios_clicked_cb(GtkWidget *button, gpointer data) {
	GtkWidget *window = (GtkWidget *) data;
	
	//generate dialog
	GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window), GTK_DIALOG_MODAL, GTK_MESSAGE_WARNING, GTK_BUTTONS_YES_NO,
		"Are you certain you want to delete the escape ratios HDF5 file? This operation cannot be undone!");
	gint rv = gtk_dialog_run(GTK_DIALOG(dialog));
	if (rv == GTK_RESPONSE_YES) {
		//delete the file
		char *file=NULL;
		xmi_get_escape_ratios_file(&file, 0);
		g_unlink(file);
		free(file);
	}
	gtk_widget_destroy(dialog);
}


static void url_delete_button_clicked_cb(GtkWidget *widget, gpointer data) {
	GtkListStore *store_prefsL = (GtkListStore *) data;

	gtk_list_store_remove(store_prefsL, &current_iter);

	return;
}

static void url_edited_cb(GtkCellRendererText *cell, gchar *path_string, gchar *new_text, gpointer data) {
	GtkListStore *store_prefsL = (GtkListStore *) data;

	gtk_list_store_set(store_prefsL, &current_iter, URL_COLUMN_PREFS, new_text, -1); 

	return;
}

static void url_add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	GtkTreeIter iter;
	GtkWidget *tree = GTK_WIDGET(data);
	GtkListStore *store_prefsL = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(tree)));

	gtk_list_store_append(store_prefsL, &iter);
	gtk_list_store_set(store_prefsL, &iter, URL_COLUMN_PREFS, "http://", -1);

	GtkTreeViewColumn *column = gtk_tree_view_get_column(GTK_TREE_VIEW(tree), 0);
	GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(store_prefsL), &iter);   
	gtk_widget_realize(tree);
	gtk_tree_view_set_cursor(GTK_TREE_VIEW(tree), path, column, TRUE);
	gtk_widget_grab_focus(tree);
	gtk_tree_path_free(path);

	return;
}

static void url_selection_changed_cb (GtkTreeSelection *selection, gpointer data) {
	GtkWidget *removeButton = GTK_WIDGET(data);
	GtkTreeIter temp_iter;
	GtkTreeModel *model;
	gboolean valid;

	if (gtk_tree_selection_get_selected(selection, &model, &current_iter)) {
		valid = gtk_tree_model_get_iter_first(model, &temp_iter);
		download_current_index = 0;
		download_current_nindices = 0;
		while (valid) {
			if (gtk_tree_selection_iter_is_selected(selection,&temp_iter)) {
				download_current_index = download_current_nindices;
			}
			download_current_nindices++;
			valid = gtk_tree_model_iter_next(model, &temp_iter);
		}
		if (download_current_nindices > 1) {
			gtk_widget_set_sensitive(removeButton, TRUE);
		}
		else {
			gtk_widget_set_sensitive(removeButton, FALSE);
		}
	}
	else {
		gtk_widget_set_sensitive(removeButton, FALSE);
	}

	return;
}


static void preferences_cancel_button_clicked(GtkWidget *button, gpointer data) {
	gtk_widget_destroy(GTK_WIDGET(data));
	return;
}


struct preferences_apply {
	GtkWidget *window;
	GtkTreeModel *model;
};


void preferences_error_handler(GtkWidget *window) {
	GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(window),
		GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR , GTK_BUTTONS_CLOSE, "A serious error occurred while checking\nthe preferences file.\nThe program will abort.");
	gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
	exit(1);
	
}


static void preferences_apply_button_clicked(GtkWidget *button, gpointer data) {
	//read everything and store it in the preferences file
	union xmimsim_prefs_val xpv;
	struct preferences_apply *pa = (struct preferences_apply *) data;

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(Mlines_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_M_LINES, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}
	
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rad_cascade_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(nonrad_cascade_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(variance_reduction_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pile_up_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_PILE_UP, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check_updates_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}

	gchar **urls = NULL;
	GtkTreeIter temp_iter;
	GtkTreeModel *model = pa->model;
	gboolean valid = gtk_tree_model_get_iter_first(model, &temp_iter);

	gint nurls = 0;
	while(valid) {
		nurls++;
		urls = (gchar **) g_realloc(urls, sizeof(gchar *)*(nurls+1));
		gtk_tree_model_get(model, &temp_iter, URL_COLUMN_PREFS, urls+nurls-1, -1);
		valid = gtk_tree_model_iter_next(model, &temp_iter);
	}
	urls[nurls] = NULL;
	
	xpv.ss = urls;
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}
	
	g_strfreev(xpv.ss);	
#endif

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(poisson_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_POISSON, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(escape_peaks_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}


#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(opencl_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_OPENCL, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}
#endif

#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(notifications_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}
#endif

	xpv.i = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(nchannels_prefsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NCHANNELS, xpv) == 0) {
		//abort	
		preferences_error_handler(pa->window);
	}
	gtk_widget_destroy(pa->window);
	return;
}

static int xmimsim_gui_create_prefs_file(GKeyFile *keyfile, gchar *prefs_dir, gchar *prefs_file) {
	gchar *prefs_file_contents;
	
	//file does not exist, is not accessible or is not in key format
	//prepare file with default values
	//g_key_file_set_comment(keyfile, NULL, NULL, "Preferences file for XMI-MSIM",NULL);
	//g_key_file_set_comment(keyfile, NULL, NULL, "This file should be modified through the preferences interface",NULL);
	g_key_file_set_comment(keyfile, NULL, NULL, "Modify this file at your own risk!",NULL);
	g_key_file_set_boolean(keyfile, "Preferences","Check for updates", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","M lines", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Variance reduction", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Pile-up", FALSE);
	g_key_file_set_string_list(keyfile, "Preferences", "Download locations", xmimsim_download_locations, g_strv_length((gchar **) xmimsim_download_locations));

	g_key_file_set_double(keyfile, "Ebel last used", "Tube voltage", 40.0);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube current", 1.0);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube solid angle", 1E-10);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha", 60.0);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube beta", 60.0);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube interval width", 0.1);
	g_key_file_set_integer(keyfile, "Ebel last used", "Tube anode element", 47);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube anode density", 10.5);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube anode thickness", 0.0002);
	g_key_file_set_integer(keyfile, "Ebel last used", "Tube window element", 4);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube window density", 1.848);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube window thickness", 0.0125);
	g_key_file_set_integer(keyfile, "Ebel last used", "Tube filter element", 2);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube filter density", 0.000166);
	g_key_file_set_double(keyfile, "Ebel last used", "Tube filter thickness", 0);
	g_key_file_set_boolean(keyfile, "Ebel last used", "Tube transmission mode", FALSE);
	g_key_file_set_string(keyfile, "Ebel last used", "Tube transmission efficiency file", "(None)");

	g_key_file_set_double(keyfile, "Ebel default", "Tube voltage", 40.0);
	g_key_file_set_double(keyfile, "Ebel default", "Tube current", 1.0);
	g_key_file_set_double(keyfile, "Ebel default", "Tube solid angle", 1E-10);
	g_key_file_set_double(keyfile, "Ebel default", "Tube alpha", 60.0);
	g_key_file_set_double(keyfile, "Ebel default", "Tube beta", 60.0);
	g_key_file_set_double(keyfile, "Ebel default", "Tube interval width", 0.1);
	g_key_file_set_integer(keyfile, "Ebel default", "Tube anode element", 47);
	g_key_file_set_double(keyfile, "Ebel default", "Tube anode density", 10.5);
	g_key_file_set_double(keyfile, "Ebel default", "Tube anode thickness", 0.0002);
	g_key_file_set_integer(keyfile, "Ebel default", "Tube window element", 4);
	g_key_file_set_double(keyfile, "Ebel default", "Tube window density", 1.848);
	g_key_file_set_double(keyfile, "Ebel default", "Tube window thickness", 0.0125);
	g_key_file_set_integer(keyfile, "Ebel default", "Tube filter element", 2);
	g_key_file_set_double(keyfile, "Ebel default", "Tube filter density", 0.000166);
	g_key_file_set_double(keyfile, "Ebel default", "Tube filter thickness", 0);
	g_key_file_set_boolean(keyfile, "Ebel default", "Tube transmission mode", FALSE);
	g_key_file_set_string(keyfile, "Ebel default", "Tube transmission efficiency file", "(None)");




	//save file
	//create dir first if necessary
	if (g_mkdir_with_parents(prefs_dir, 0755) != 0)
		return 0;
	g_free(prefs_dir);
	prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
	if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
		return 0;
	g_free(prefs_file_contents);	


	return 1;
}




int xmimsim_gui_get_prefs(int kind, union xmimsim_prefs_val *prefs) {
	gchar *prefs_file;
	GKeyFile *keyfile;
#ifdef MAC_INTEGRATION
        NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
#endif
	
#ifdef MAC_INTEGRATION
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask,TRUE);
        NSString *documentsDirectory = [paths objectAtIndex:0];
        const gchar *config_dir = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif




	//first check if the preferences file exists!
	gchar *prefs_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM",config_dir);
	prefs_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "preferences.ini",prefs_dir);

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, prefs_file, G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS, NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_dir, prefs_file))
			return 0;
	}
		
	//extract required information from keyfile
	GError *error = NULL;
	gchar *prefs_file_contents;
	switch (kind) {
		case XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Check for updates", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Check for updates not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Check for updates", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
		case XMIMSIM_GUI_PREFS_M_LINES: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "M lines", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"M lines not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","M lines", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
		case XMIMSIM_GUI_PREFS_RAD_CASCADE: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Radiative cascade", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Radiative cascade not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
		case XMIMSIM_GUI_PREFS_NONRAD_CASCADE: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Non-radiative cascade", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Non-radiative cascade not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
		case XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Variance reduction", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Variance reduction not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Variance reduction", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
		case XMIMSIM_GUI_PREFS_PILE_UP: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Pile-up", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Pile-up not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Pile-up", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = FALSE;
			}
			break;
		case XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS:
			prefs->ss = g_key_file_get_string_list(keyfile, "Preferences", "Download locations", NULL, &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Download locations not found in preferences file\n");
				fprintf(stdout,"Number of locations: %i\n",g_strv_length((gchar **) xmimsim_download_locations));
				g_key_file_set_string_list(keyfile, "Preferences", "Download locations", xmimsim_download_locations, g_strv_length((gchar **) xmimsim_download_locations));
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->ss = g_strdupv((gchar **) xmimsim_download_locations);
			}
			break;
		case XMIMSIM_GUI_PREFS_POISSON: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Poisson noise", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Poisson noise not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Poisson noise", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = FALSE;
			}
			break;
		case XMIMSIM_GUI_PREFS_ESCAPE_PEAKS: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Escape peaks", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Escape peaks not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Escape peaks", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
		case XMIMSIM_GUI_PREFS_OPENCL: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "OpenCL", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"OpenCL not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","OpenCL", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = FALSE;
			}
			break;
#endif
		case XMIMSIM_GUI_PREFS_NCHANNELS: 
			prefs->i = g_key_file_get_integer(keyfile, "Preferences", "Number of channels", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Number of channels not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Preferences","Number of channels", 2048);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->i = 2048;
			}
			break;
#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
		case XMIMSIM_GUI_PREFS_NOTIFICATIONS: 
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Notifications", &error);
			if (error != NULL) {
				//error
				fprintf(stderr,"Notifications not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Notifications", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);	
				prefs->b = TRUE;
			}
			break;
#endif
		case XMIMSIM_GUI_EBEL_LAST_USED:
			prefs->xep = g_malloc(sizeof(struct xmi_ebel_parameters));
			prefs->xep->tube_voltage = g_key_file_get_double(keyfile, "Ebel last used", "Tube voltage", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube voltage not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube voltage", 40.0);
				prefs->xep->tube_voltage = 40.0;
				error = NULL;
			}
			prefs->xep->tube_current = g_key_file_get_double(keyfile, "Ebel last used", "Tube current", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube current not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube current", 1.0);
				prefs->xep->tube_current = 1.0;
				error = NULL;
			}
			prefs->xep->tube_solid_angle = g_key_file_get_double(keyfile, "Ebel last used", "Tube solid angle", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube solid angle not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube solid angle", 1E-10);
				prefs->xep->tube_solid_angle = 1E-10;
				error = NULL;
			}
			prefs->xep->alpha = g_key_file_get_double(keyfile, "Ebel last used", "Tube alpha", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube alpha not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha", 60.0);
				prefs->xep->alpha = 60.0;
				error = NULL;
			}
			prefs->xep->beta = g_key_file_get_double(keyfile, "Ebel last used", "Tube beta", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube beta not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube beta", 60.0);
				prefs->xep->beta = 60.0;
				error = NULL;
			}
			prefs->xep->interval_width = g_key_file_get_double(keyfile, "Ebel last used", "Tube interval width", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube interval width not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube interval width", 0.1);
				prefs->xep->interval_width = 0.1;
				error = NULL;
			}
			prefs->xep->anode_Z = g_key_file_get_integer(keyfile, "Ebel last used", "Tube anode element", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube anode element not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Ebel last used", "Tube anode element", 47);
				prefs->xep->anode_Z = 47;
				error = NULL;
			}
			prefs->xep->anode_rho = g_key_file_get_double(keyfile, "Ebel last used", "Tube anode density", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube anode density not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube anode density", 10.5);
				prefs->xep->anode_rho = 10.5;
				error = NULL;
			}
			prefs->xep->anode_thickness = g_key_file_get_double(keyfile, "Ebel last used", "Tube anode thickness", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube anode thickness not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube anode thickness", 0.0002);
				prefs->xep->anode_thickness = 0.0002;
				error = NULL;
			}
			prefs->xep->window_Z = g_key_file_get_integer(keyfile, "Ebel last used", "Tube window element", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube window element not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Ebel last used", "Tube window element", 4);
				prefs->xep->window_Z= 4;
				error = NULL;
			}
			prefs->xep->window_rho = g_key_file_get_double(keyfile, "Ebel last used", "Tube window density", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube window density not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube window density", 1.848);
				prefs->xep->window_rho = 1.848;
				error = NULL;
			}
			prefs->xep->window_thickness = g_key_file_get_double(keyfile, "Ebel last used", "Tube window thickness", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube window thickness not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube window thickness", 0.0125);
				prefs->xep->window_thickness = 0.0125;
				error = NULL;
			}
			prefs->xep->filter_Z = g_key_file_get_integer(keyfile, "Ebel last used", "Tube filter element", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube filter element not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Ebel last used", "Tube filter element", 2);
				prefs->xep->filter_Z = 2;
				error = NULL;
			}
			prefs->xep->filter_rho = g_key_file_get_double(keyfile, "Ebel last used", "Tube filter density", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube filter density not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube filter density", 0.000166);
				prefs->xep->filter_rho = 0.000166;
				error = NULL;
			}
			prefs->xep->filter_thickness = g_key_file_get_double(keyfile, "Ebel last used", "Tube filter thickness", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube filter thickness not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel last used", "Tube filter thickness", 0);
				prefs->xep->filter_thickness = 0;
				error = NULL;
			}
			prefs->xep->transmission_tube = g_key_file_get_boolean(keyfile, "Ebel last used", "Tube transmission mode", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube transmission mode not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Ebel last used", "Tube transmission mode", FALSE);
				prefs->xep->transmission_tube = FALSE;
				error = NULL;
			}
			prefs->xep->transmission_efficiency_file= g_key_file_get_string(keyfile, "Ebel last used", "Tube transmission efficiency file", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel last used Tube transmission efficiency file not found in preferences file\n");
				g_key_file_set_string(keyfile, "Ebel last used", "Tube transmission efficiency file", "(None)");
				prefs->xep->transmission_efficiency_file = g_strdup("(None)");
				error = NULL;
			}
			//save file
			prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
			if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
				return 0;
			g_free(prefs_file_contents);	

			break;
		case XMIMSIM_GUI_EBEL_DEFAULT:
			prefs->xep = g_malloc(sizeof(struct xmi_ebel_parameters));
			prefs->xep->tube_voltage = g_key_file_get_double(keyfile, "Ebel default", "Tube voltage", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube voltage not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube voltage", 40.0);
				prefs->xep->tube_voltage = 40.0;
				error = NULL;
			}
			prefs->xep->tube_current = g_key_file_get_double(keyfile, "Ebel default", "Tube current", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube current not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube current", 1.0);
				prefs->xep->tube_current = 1.0;
				error = NULL;
			}
			prefs->xep->tube_solid_angle = g_key_file_get_double(keyfile, "Ebel default", "Tube solid angle", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube solid angle not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube solid angle", 1E-10);
				prefs->xep->tube_solid_angle = 1E-10;
				error = NULL;
			}
			prefs->xep->alpha = g_key_file_get_double(keyfile, "Ebel default", "Tube alpha", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube alpha not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube alpha", 60.0);
				prefs->xep->alpha = 60.0;
				error = NULL;
			}
			prefs->xep->beta = g_key_file_get_double(keyfile, "Ebel default", "Tube beta", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube beta not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube beta", 60.0);
				prefs->xep->beta = 60.0;
				error = NULL;
			}
			prefs->xep->interval_width = g_key_file_get_double(keyfile, "Ebel default", "Tube interval width", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube interval width not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube interval width", 0.1);
				prefs->xep->interval_width = 0.1;
				error = NULL;
			}
			prefs->xep->anode_Z = g_key_file_get_integer(keyfile, "Ebel default", "Tube anode element", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube anode element not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Ebel default", "Tube anode element", 47);
				prefs->xep->anode_Z = 47;
				error = NULL;
			}
			prefs->xep->anode_rho = g_key_file_get_double(keyfile, "Ebel default", "Tube anode density", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube anode density not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube anode density", 10.5);
				prefs->xep->anode_rho = 10.5;
				error = NULL;
			}
			prefs->xep->anode_thickness = g_key_file_get_double(keyfile, "Ebel default", "Tube anode thickness", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube anode thickness not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube anode thickness", 0.0002);
				prefs->xep->anode_thickness = 0.0002;
				error = NULL;
			}
			prefs->xep->window_Z = g_key_file_get_integer(keyfile, "Ebel default", "Tube window element", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube window element not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Ebel default", "Tube window element", 4);
				prefs->xep->window_Z = 4;
				error = NULL;
			}
			prefs->xep->window_rho = g_key_file_get_double(keyfile, "Ebel default", "Tube window density", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube window density not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube window density", 1.848);
				prefs->xep->window_rho = 1.848;
				error = NULL;
			}
			prefs->xep->window_thickness = g_key_file_get_double(keyfile, "Ebel default", "Tube window thickness", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube window thickness not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube window thickness", 0.0125);
				prefs->xep->window_thickness = 0.0125;
				error = NULL;
			}
			prefs->xep->filter_Z = g_key_file_get_integer(keyfile, "Ebel default", "Tube filter element", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube filter element not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Ebel default", "Tube filter element", 2);
				prefs->xep->filter_Z = 2;
				error = NULL;
			}
			prefs->xep->filter_rho = g_key_file_get_double(keyfile, "Ebel default", "Tube filter density", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube filter density not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube filter density", 0.000166);
				prefs->xep->filter_rho = 0.000166;
				error = NULL;
			}
			prefs->xep->filter_thickness = g_key_file_get_double(keyfile, "Ebel default", "Tube filter thickness", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube filter thickness not found in preferences file\n");
				g_key_file_set_double(keyfile, "Ebel default", "Tube filter thickness", 0);
				prefs->xep->filter_thickness = 0;
				error = NULL;
			}
			prefs->xep->transmission_tube = g_key_file_get_boolean(keyfile, "Ebel default", "Tube transmission mode", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube transmission mode not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Ebel default", "Tube transmission mode", FALSE);
				prefs->xep->transmission_tube = FALSE;
				error = NULL;
			}
			prefs->xep->transmission_efficiency_file= g_key_file_get_string(keyfile, "Ebel default", "Tube transmission efficiency file", &error);
			if (error != NULL) {
				//error
				fprintf(stderr, "Ebel default Tube transmission efficiency file not found in preferences file\n");
				g_key_file_set_string(keyfile, "Ebel default", "Tube transmission efficiency file", "(None)");
				prefs->xep->transmission_efficiency_file = g_strdup("(None)");
				error = NULL;
			}
			//save file
			prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
			if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
				return 0;
			g_free(prefs_file_contents);	

			break;
		default:
			fprintf(stderr,"Unknown preference requested in xmimsim_gui_get_prefs\n");
			return 0;
	}





	g_free(prefs_file);
	g_key_file_free(keyfile);
#ifdef MAC_INTEGRATION
        [pool drain];
#endif
	return 1;
}


int xmimsim_gui_set_prefs(int kind, union xmimsim_prefs_val prefs) {
	gchar *prefs_file;
	GKeyFile *keyfile;
#ifdef MAC_INTEGRATION
        NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
#endif
	
#ifdef MAC_INTEGRATION
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask,TRUE);
        NSString *documentsDirectory = [paths objectAtIndex:0];
        const gchar *config_dir = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif




	//first check if the preferences file exists!
	gchar *prefs_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM",config_dir);
	prefs_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "preferences.ini",prefs_dir);

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, prefs_file, G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS, NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_dir, prefs_file))
			return 0;
	}

	switch (kind) {
		case XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES: 
			g_key_file_set_boolean(keyfile, "Preferences", "Check for updates", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_M_LINES: 
			g_key_file_set_boolean(keyfile, "Preferences","M lines", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_RAD_CASCADE: 
			g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_NONRAD_CASCADE: 
			g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION: 
			g_key_file_set_boolean(keyfile, "Preferences","Variance reduction", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_PILE_UP: 
			g_key_file_set_boolean(keyfile, "Preferences","Pile-up", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS: 
			g_key_file_set_string_list(keyfile, "Preferences", "Download locations",  (const gchar * const *) prefs.ss, (gsize) g_strv_length(prefs.ss));
			break;
		case XMIMSIM_GUI_PREFS_POISSON: 
			g_key_file_set_boolean(keyfile, "Preferences","Poisson noise", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_ESCAPE_PEAKS: 
			g_key_file_set_boolean(keyfile, "Preferences","Escape peaks", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_NCHANNELS: 
			g_key_file_set_integer(keyfile, "Preferences","Number of channels", prefs.i);
			break;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
		case XMIMSIM_GUI_PREFS_OPENCL: 
			g_key_file_set_boolean(keyfile, "Preferences","OpenCL", prefs.b);
			break;
#endif
#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
		case XMIMSIM_GUI_PREFS_NOTIFICATIONS: 
			g_key_file_set_boolean(keyfile, "Preferences","Notifications", prefs.b);
			break;
#endif
		case XMIMSIM_GUI_EBEL_LAST_USED:
			g_key_file_set_double(keyfile, "Ebel last used", "Tube voltage", prefs.xep->tube_voltage);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube current", prefs.xep->tube_current);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube solid angle", prefs.xep->tube_solid_angle);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube alpha", prefs.xep->alpha);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube beta", prefs.xep->beta);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube interval width", prefs.xep->interval_width);
			g_key_file_set_integer(keyfile, "Ebel last used", "Tube anode element", prefs.xep->anode_Z);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube anode density", prefs.xep->anode_rho);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube anode thickness", prefs.xep->anode_thickness);
			g_key_file_set_integer(keyfile, "Ebel last used", "Tube filter element", prefs.xep->filter_Z);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube filter density", prefs.xep->filter_rho);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube filter thickness", prefs.xep->filter_thickness);
			g_key_file_set_integer(keyfile, "Ebel last used", "Tube window element", prefs.xep->window_Z);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube window density", prefs.xep->window_rho);
			g_key_file_set_double(keyfile, "Ebel last used", "Tube window thickness", prefs.xep->window_thickness);
			g_key_file_set_boolean(keyfile, "Ebel last used", "Tube transmission mode", prefs.xep->transmission_tube);
			g_key_file_set_string(keyfile, "Ebel last used", "Tube transmission efficiency file", prefs.xep->transmission_efficiency_file);

			break;
		default:
			fprintf(stderr,"Unknown preference requested in xmimsim_gui_set_prefs\n");
			return 0;
		
	}
	
	//save file
	gchar *prefs_file_contents; 
	prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
	if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
		return 0;
	g_free(prefs_file_contents);	

	g_free(prefs_file);
	g_key_file_free(keyfile);
#ifdef MAC_INTEGRATION
        [pool drain];
#endif
	return 1;

}

void xmimsim_gui_launch_preferences(GtkWidget *widget, gpointer data) {
//void xmimsim_gui_launch_preferences(GtkWidget *main_window, gint page) {
	struct xmi_preferences_data *xpd = (struct xmi_preferences_data *) data;

	GtkWidget *window;
	GtkWidget *main_window = xpd->window;
	gint page = xpd->page;
	GtkWidget *notebook;
	GtkWidget *master_box;

	master_box = gtk_vbox_new(FALSE,2);
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "Preferences");
	gtk_widget_set_size_request(window,450,450);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));

	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	//gtk_window_set_deletable(GTK_WINDOW(window), FALSE);

	notebook = gtk_notebook_new();
	gtk_box_pack_start(GTK_BOX(master_box), notebook, TRUE,TRUE,1);
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_container_add(GTK_CONTAINER(window), master_box);

	

	GtkWidget *label;
	
	union xmimsim_prefs_val xpv;

	GtkWidget *superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);

	label = gtk_label_new("Simulation default options");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Simulation defaults</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_window, label);


	Mlines_prefsW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(Mlines_prefsW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_M_LINES, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(Mlines_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),Mlines_prefsW, FALSE, FALSE, 3);

	rad_cascade_prefsW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(rad_cascade_prefsW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rad_cascade_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),rad_cascade_prefsW, FALSE, FALSE, 3);

	nonrad_cascade_prefsW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(nonrad_cascade_prefsW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nonrad_cascade_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),nonrad_cascade_prefsW, FALSE, FALSE, 3);

	variance_reduction_prefsW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(variance_reduction_prefsW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(variance_reduction_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),variance_reduction_prefsW, FALSE, FALSE, 3);

	pile_up_prefsW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(pile_up_prefsW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pile_up_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),pile_up_prefsW, FALSE, FALSE, 3);

	poisson_prefsW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	gtk_widget_set_tooltip_text(poisson_prefsW,"Enabling this feature will add noise according to a Poisson distribution to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(poisson_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),poisson_prefsW, FALSE, FALSE, 3);

	escape_peaks_prefsW = gtk_check_button_new_with_label("Enable escape peaks support");
	gtk_widget_set_tooltip_text(escape_peaks_prefsW,"Enabling this feature will add fluorescence and Compton escape peaks to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(escape_peaks_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), escape_peaks_prefsW, FALSE, FALSE, 3);

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	opencl_prefsW = gtk_check_button_new_with_label("Enable OpenCL");
	gtk_widget_set_tooltip_text(opencl_prefsW,"Enabling OpenCL will have the simulation use the GPU in order to calculate the solid angle grids, resulting in considerably speed-up. Requires the installation of OpenCL drivers. Consult the website of the manufacturer of your videocard for more information");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_OPENCL, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(opencl_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),opencl_prefsW, FALSE, FALSE, 3);
#endif

	GtkAdjustment *spinner_adj = GTK_ADJUSTMENT(gtk_adjustment_new(2048.0, 10.0, 100000.0, 1.0, 10.0, 0.0));
	nchannels_prefsW = gtk_spin_button_new(spinner_adj, 1, 0);
	gtk_editable_set_editable(GTK_EDITABLE(nchannels_prefsW), TRUE);
	gtk_spin_button_set_update_policy(GTK_SPIN_BUTTON(nchannels_prefsW), GTK_UPDATE_IF_VALID);
	gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(nchannels_prefsW), TRUE);
	gtk_entry_set_max_length(GTK_ENTRY(nchannels_prefsW), 7);
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NCHANNELS, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(nchannels_prefsW), (gdouble) xpv.i);
	GtkWidget *hbox = gtk_hbox_new(FALSE, 5);
	label = gtk_label_new("Number of spectrum channels");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(hbox), nchannels_prefsW, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	

	//second page
	superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Updates</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);
	GtkWidget *buttonbox;

#if defined(RPM_BUILD)
	label = gtk_label_new("XMI-MSIM was built with Redhat Package Manager. All updates should be installed with yum.");
	//gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);

#elif defined(DEB_BUILD)
	label = gtk_label_new("XMI-MSIM was built with Debian Package Manager. All updates should be installed with apt-get or aptitude.");
	//gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);
#elif defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)

	check_updates_prefsW = gtk_check_button_new_with_label("Check for updates on startup");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_updates_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), check_updates_prefsW, TRUE, FALSE, 3);

	label = gtk_label_new("Download locations for updates");
	gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_box_pack_start(GTK_BOX(superframe), label, FALSE, FALSE, 2);

	GtkWidget *updatesboxW = gtk_hbox_new(FALSE,5);
	GtkListStore *store_prefsL;
	GtkTreeViewColumn *column;
	GtkTreeSelection *select;
	GtkCellRenderer *renderer;
	store_prefsL = gtk_list_store_new(N_COLUMNS_PREFS, G_TYPE_STRING);
	GtkWidget *tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store_prefsL));
	renderer = gtk_cell_renderer_text_new();
	g_signal_connect(renderer, "edited", G_CALLBACK(url_edited_cb), (gpointer) store_prefsL);
	g_object_set(renderer, "editable", TRUE, NULL);
	my_gtk_cell_renderer_set_alignment(renderer, 0., 0.5);
	column = gtk_tree_view_column_new_with_attributes("Website", renderer,"text",URL_COLUMN_PREFS,NULL);
	gtk_tree_view_column_set_resizable(column,TRUE);
	gtk_tree_view_column_set_alignment(column, 0.);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree), column);
	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_SINGLE);
	
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 300,150);
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree);
	gtk_box_pack_start(GTK_BOX(updatesboxW),scrolled_window, FALSE, FALSE,3 );


	buttonbox = gtk_vbox_new(FALSE, 5);
	GtkWidget *addButton;
	GtkWidget *editButton;
	GtkWidget *removeButton;

	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
	removeButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);

	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), removeButton, TRUE, FALSE, 3);
	gtk_widget_set_sensitive(removeButton, FALSE);
	g_signal_connect(G_OBJECT(removeButton), "clicked", G_CALLBACK(url_delete_button_clicked_cb) , (gpointer) store_prefsL);
	g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(url_add_button_clicked_cb) , (gpointer) tree);

	gtk_box_pack_start(GTK_BOX(updatesboxW),buttonbox, TRUE, FALSE, 3);

	int i;

	//populate tree
	GtkTreeIter iter;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	for (i= 0 ; i < g_strv_length(xpv.ss) ; i++) {
		gtk_list_store_append(store_prefsL,&iter);
		gtk_list_store_set(store_prefsL, &iter, URL_COLUMN_PREFS, xpv.ss[i], -1);
	}	

	
	gtk_box_pack_start(GTK_BOX(superframe), updatesboxW, FALSE, FALSE, 2);


	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(url_selection_changed_cb), (gpointer) removeButton);

#else
	label = gtk_label_new("XMI-MSIM was built without support for automatic updates. Consider recompiling after installing libcurl and json-glib.");
	//gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);
#endif

	//Third page: advanced
	superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Advanced</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);


	hbox = gtk_hbox_new(FALSE,2);
	GtkWidget *button = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	label = gtk_label_new("Remove the solid angles HDF5 file");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_widget_set_tooltip_text(label,"It is recommended to remove this file when a definitive uninstallation of XMI-MSIM is required, or when this file got somehow corrupted.");
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(delete_solid_angles_clicked_cb), (gpointer) window);

	hbox = gtk_hbox_new(FALSE,2);
	button = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	label = gtk_label_new("Remove the escape ratios HDF5 file");
	gtk_widget_set_tooltip_text(label,"It is recommended to remove this file when a definitive uninstallation of XMI-MSIM is required, or when this file got somehow corrupted.");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(delete_escape_ratios_clicked_cb), (gpointer) window);

	gtk_box_pack_start(GTK_BOX(superframe), gtk_hseparator_new(), FALSE, FALSE, 3);

	hbox = gtk_hbox_new(FALSE,2);
	button = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	label = gtk_label_new("Import solid angle grids");
	gtk_widget_set_tooltip_text(label,"Use this feature to import solid angle grids into your personal default HDF5 file.");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(import_solid_angles_clicked_cb), (gpointer) window);
	
	hbox = gtk_hbox_new(FALSE,2);
	button = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	label = gtk_label_new("Import escape ratios");
	gtk_widget_set_tooltip_text(label,"Use this feature to import detector specific escape ratios into your personal default HDF5 file.");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(import_escape_ratios_clicked_cb), (gpointer) window);
	

	gtk_box_pack_start(GTK_BOX(superframe), gtk_hseparator_new(), FALSE, FALSE, 3);

#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
	notifications_prefsW = gtk_check_button_new_with_label("Enable notifications");
	gtk_widget_set_tooltip_text(notifications_prefsW,"Check this button to enable notifications support");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, &xpv) == 0) {
		//abort	
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(notifications_prefsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),notifications_prefsW, FALSE, FALSE, 3);
#endif

	//back to master_box
	//separator
	//GtkWidget *separator = gtk_hseparator_new();
	//gtk_box_pack_start(GTK_BOX(master_box), separator, FALSE, FALSE, 3);

	//button box
	buttonbox = gtk_hbox_new(TRUE, 2);
	GtkWidget *applyButton, *cancelButton;

	applyButton = gtk_button_new_from_stock(GTK_STOCK_APPLY);
	cancelButton = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
	gtk_box_pack_start(GTK_BOX(buttonbox), applyButton, TRUE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(buttonbox), cancelButton, TRUE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(master_box),buttonbox, FALSE, FALSE, 6);
	g_signal_connect(G_OBJECT(cancelButton), "clicked", G_CALLBACK(preferences_cancel_button_clicked), (gpointer) window);
	struct preferences_apply *pa = (struct preferences_apply *) malloc(sizeof(struct preferences_apply));
	pa->window = window;
#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	pa->model = GTK_TREE_MODEL(store_prefsL);
#else
	pa->model = NULL;
#endif
	g_signal_connect(G_OBJECT(applyButton), "clicked", G_CALLBACK(preferences_apply_button_clicked), (gpointer) pa);

	
	gtk_widget_show_all(window);
}

