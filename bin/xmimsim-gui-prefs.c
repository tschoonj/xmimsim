/*
 * Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze
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

#include <config.h>
#include <stdio.h>
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmi_detector.h"
#include "xmi_solid_angle.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <xmi_aux.h>
#include <string.h>
#ifdef MAC_INTEGRATION
        #import <Foundation/Foundation.h>
#endif

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
#include "xmimsim-gui-updater.h"
#endif

struct prefsWidgets {
	GtkWidget *parent_window;
	GtkWidget *window;
	GtkWidget *MlinesW;
	GtkWidget *rad_cascadeW;
	GtkWidget *nonrad_cascadeW;
	GtkWidget *variance_reductionW;
	GtkWidget *pile_upW;
	GtkWidget *poissonW;
	GtkWidget *escape_peaksW;
	GtkWidget *advanced_comptonW;
	GtkWidget *default_seedsW;
	GtkWidget *custom_detector_responseE;
	GtkWidget *custom_detector_responseB;
	GtkWidget *custom_detector_responseC;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	GtkWidget *openclW;
#endif

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	GtkWidget *check_updatesW; // checkbutton
	GtkWidget *update_urlsW;   // treeview
#endif

#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
	GtkWidget *notificationsW;
#endif
};
enum {
	URL_COLUMN_PREFS,
	STATUS_COLUMN_PREFS,
	N_COLUMNS_PREFS
};


static const gchar * const xmimsim_download_locations[] = {
		"http://lvserver.ugent.be/xmi-msim",
		"https://xmi-msim.tomschoonjans.eu",
		"http://xmi-msim.s3.amazonaws.com",
		NULL};


static void custom_detector_response_toggled_cb(GtkToggleButton *button, struct prefsWidgets *data) {
	if (gtk_toggle_button_get_active(button) == TRUE) {
		gtk_widget_set_sensitive(data->custom_detector_responseE, TRUE);
		gtk_widget_set_sensitive(data->custom_detector_responseB, TRUE);
	}
	else {
		gtk_widget_set_sensitive(data->custom_detector_responseE, FALSE);
		gtk_widget_set_sensitive(data->custom_detector_responseB, FALSE);
	}
}

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
		g_free(target);
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
		g_unlink(file);
		g_free(file);
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
		g_free(file);
	}
	gtk_widget_destroy(dialog);
}


static void url_delete_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree) {
	GtkTreeSelection *selection = gtk_tree_view_get_selection(tree);
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(tree));
	GtkTreeIter iter;

	gtk_tree_selection_get_selected(selection, NULL, &iter);

	gtk_list_store_remove(store, &iter);

	return;
}

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
static void url_edited_cb(GtkCellRendererText *cell, gchar *path_string, gchar *new_text, GtkTreeView *tree) {
	GtkTreeSelection *selection = gtk_tree_view_get_selection(tree);
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(tree));
	GtkTreeIter iter;

	gtk_tree_selection_get_selected(selection, NULL, &iter);

	gtk_list_store_set(store, &iter, URL_COLUMN_PREFS, new_text, -1);
	if (xmi_msim_gui_utils_check_download_url(new_text) == TRUE) {
		gtk_list_store_set(store, &iter, STATUS_COLUMN_PREFS, GTK_STOCK_YES, -1);
	}
	else {
		gtk_list_store_set(store, &iter, STATUS_COLUMN_PREFS, GTK_STOCK_NO, -1);
	}

	return;
}

static void url_add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	GtkTreeIter iter;
	GtkWidget *tree = GTK_WIDGET(data);
	GtkListStore *store_prefsL = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(tree)));

	gtk_list_store_append(store_prefsL, &iter);
	gtk_list_store_set(store_prefsL, &iter, URL_COLUMN_PREFS, "http://", -1);
	gtk_list_store_set(store_prefsL, &iter, STATUS_COLUMN_PREFS, GTK_STOCK_NO, -1);

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

	gtk_widget_set_sensitive(removeButton, gtk_tree_selection_get_selected(selection, NULL, NULL));

	return;
}
#endif


static void preferences_cancel_button_clicked(GtkWidget *button, gpointer data) {
	gtk_widget_destroy(GTK_WIDGET(data));
	return;
}

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
	struct prefsWidgets *pw = (struct prefsWidgets *) data;

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->MlinesW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_M_LINES, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->rad_cascadeW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->nonrad_cascadeW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->variance_reductionW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->pile_upW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_PILE_UP, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

#if defined(HAVE_LIBCURL) && defined(HAVE_JSONGLIB)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->check_updatesW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	gchar **urls = NULL;
	GtkTreeIter temp_iter;
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(pw->update_urlsW));
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
		preferences_error_handler(pw->window);
	}

	g_strfreev(xpv.ss);
#endif

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->poissonW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_POISSON, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->escape_peaksW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->advanced_comptonW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_ADVANCED_COMPTON, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->default_seedsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SEEDS, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->openclW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_OPENCL, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}
#endif

#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
	xpv.b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->notificationsW));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}
#endif

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->custom_detector_responseC)) == TRUE &&
		strlen(gtk_entry_get_text(GTK_ENTRY(pw->custom_detector_responseE))) > 0) {
		xpv.s = g_strdup(gtk_entry_get_text(GTK_ENTRY(pw->custom_detector_responseE)));
	}
	else
		xpv.s = NULL;
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE, xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}

	gtk_widget_destroy(pw->window);
	return;
}

int xmimsim_gui_create_prefs_file(GKeyFile *keyfile, gchar *prefs_file) {
	gchar *prefs_file_contents;

	//These are all the default values for a new preferences file

	//file does not exist, is not accessible or is not in key format
	//prepare file with default values
	g_key_file_set_comment(keyfile, NULL, NULL, "Modify this file at your own risk!",NULL);
	g_key_file_set_boolean(keyfile, "Preferences","Check for updates", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","M lines", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Variance reduction", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Pile-up", FALSE);
	g_key_file_set_boolean(keyfile, "Preferences","Poisson noise", FALSE);
	g_key_file_set_boolean(keyfile, "Preferences","Escape peaks", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Advanced Compton", FALSE);
	g_key_file_set_boolean(keyfile, "Preferences","OpenCL", FALSE);
	g_key_file_set_string_list(keyfile, "Preferences", "Download locations", xmimsim_download_locations, g_strv_length((gchar **) xmimsim_download_locations));
	g_key_file_set_string(keyfile, "Preferences","Custom detector response", "None");

	g_key_file_set_integer(keyfile, "Sources last used", "Page", 0);

	//save file
	//create dir first if necessary
	gchar *prefs_dir = g_path_get_dirname(prefs_file);
	if (g_mkdir_with_parents(prefs_dir, 0755) != 0)
		return 0;
	g_free(prefs_dir);
	prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
	if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
		return 0;
	g_free(prefs_file_contents);


	return 1;
}

static gchar *xmimsim_gui_get_user_defined_layer_filename(void) {
	gchar *file;

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

	gchar *prefs_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM",config_dir);
	file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "user-defined-layers.ini",prefs_dir);
	g_free(prefs_dir);

#ifdef MAC_INTEGRATION
        [pool drain];
#endif
	return file;
}

gchar **xmimsim_gui_get_user_defined_layer_names(void) {
	gchar *ini_file = xmimsim_gui_get_user_defined_layer_filename();
	GKeyFile *keyfile;
	gchar **layer_names;

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, ini_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		//file does not exist!
		g_warning("%s does not exist or could not be opened\n", ini_file);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}

	//so the file exists...
	layer_names = g_key_file_get_groups(keyfile, NULL);

	//sort
	if (g_strv_length(layer_names) > 1) {
		qsort(layer_names, g_strv_length(layer_names), sizeof(gchar*), compare_string);
	}

	g_free(ini_file);
	g_key_file_free(keyfile);

	return layer_names;
}

struct xmi_layer* xmimsim_gui_get_user_defined_layer(const gchar *layer_name) {
	gchar *ini_file = xmimsim_gui_get_user_defined_layer_filename();
	GKeyFile *keyfile;

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, ini_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		//file does not exist!
		g_warning("%s does not exist or could not be opened\n", ini_file);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}

	//see if the layer exists in the file
	if(g_key_file_has_group(keyfile, layer_name) == FALSE) {
		//not found
		g_warning("Layer %s does not exist\n", layer_name);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}

	//so the group exists: read elements, weight fractions, density and thickness

	gsize Zlen;
	GError *error = NULL;
	gint *Z = g_key_file_get_integer_list(keyfile, layer_name, "Z", &Zlen, &error);
	if (Z == NULL || Zlen == 0 || error != NULL) {
		g_warning("Error reading Z in layer %s\n", layer_name);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}
	gsize weightlen;
	gdouble *weight = g_key_file_get_double_list(keyfile, layer_name, "weight", &weightlen, &error);
	if (weight == NULL || weightlen == 0 || error != NULL) {
		g_warning("Error reading weight in layer %s\n", layer_name);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}
	if (Zlen != weightlen) {
		g_warning("Inconsistent lengths of Z and weight for layer %s\n", layer_name);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}
	gdouble density = g_key_file_get_double(keyfile, layer_name, "density", &error);
	if (error != NULL) {
		g_warning("Could not read density of layer %s\n", layer_name);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}
	gdouble thickness = g_key_file_get_double(keyfile, layer_name, "thickness", &error);
	if (error != NULL) {
		g_warning("Could not read thickness of layer %s\n", layer_name);
		g_free(ini_file);
		g_key_file_free(keyfile);
		return NULL;
	}

	struct xmi_layer *layer = (struct xmi_layer *) g_malloc(sizeof(struct xmi_layer));
	//fill her up
	layer->n_elements = (int) Zlen;
	layer->Z = (int *) xmi_memdup(Z, sizeof(int)*Zlen);
	layer->weight = (double *) xmi_memdup(weight, sizeof(double)*Zlen);
	//scale
	xmi_scale_double(layer->weight, layer->n_elements, 1.0/xmi_sum_double(layer->weight, layer->n_elements));
	layer->density = density;
	layer->thickness = thickness;
	g_free(Z);
	g_free(weight);



	g_free(ini_file);
	g_key_file_free(keyfile);

	return layer;
}

int xmimsim_gui_remove_user_defined_layer(const gchar *layer_name) {
	gchar *ini_file = xmimsim_gui_get_user_defined_layer_filename();
	GKeyFile *keyfile;

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, ini_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		//file does not exist!
		g_warning("%s does not exist or could not be opened\n", ini_file);
			return 0;
	}
	if (g_key_file_remove_group(keyfile, layer_name, NULL) == FALSE) {
		g_warning("Layer %s does not exist in %s\n", layer_name, ini_file);
			return 0;
	}

	gchar *prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
	if(!g_file_set_contents(ini_file, prefs_file_contents, -1, NULL))
		return 0;
	g_free(prefs_file_contents);
	g_key_file_free(keyfile);
	g_free(ini_file);
	return 1;
}

int xmimsim_gui_add_user_defined_layer(struct xmi_layer *layer, const gchar *layer_name) {
	gchar *ini_file = xmimsim_gui_get_user_defined_layer_filename();
	GKeyFile *keyfile;

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, ini_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		//file does not exist!
		g_warning("%s does not exist or could not be opened\nTrying to create it...\n", ini_file);
		g_key_file_set_comment(keyfile, NULL, NULL, "Modify this file at your own risk!",NULL);
		//save file
		//create dir first if necessary
		gchar *prefs_dir = g_path_get_dirname(ini_file);
		if (g_mkdir_with_parents(prefs_dir, 0755) != 0)
			return 0;
		g_free(prefs_dir);
	}

	g_key_file_set_integer_list(keyfile, layer_name, "Z", layer->Z, (gsize) layer->n_elements);
	//make sure the weights are scaled
	double *weight = (double *) xmi_memdup(layer->weight, sizeof(double)*layer->n_elements);
	xmi_scale_double(weight, layer->n_elements, 1.0/xmi_sum_double(weight, layer->n_elements));
	g_key_file_set_double_list(keyfile, layer_name, "weight", weight, (gsize) layer->n_elements);
	g_free(weight);
	g_key_file_set_double(keyfile, layer_name, "density", layer->density);
	g_key_file_set_double(keyfile, layer_name, "thickness", layer->thickness);

	gchar *prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
	if(!g_file_set_contents(ini_file, prefs_file_contents, -1, NULL))
		return 0;
	g_free(prefs_file_contents);
	g_key_file_free(keyfile);
	g_free(ini_file);

	return 1;
}
int xmimsim_gui_get_prefs(int kind, union xmimsim_prefs_val *prefs) {
	gchar *prefs_file;
	GKeyFile *keyfile;

	prefs_file = xmimsim_gui_get_preferences_filename();

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, prefs_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_file))
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
				g_warning("Check for updates not found in preferences file\n");
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
				g_warning("M lines not found in preferences file\n");
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
				g_warning("Radiative cascade not found in preferences file\n");
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
				g_warning("Non-radiative cascade not found in preferences file\n");
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
				g_warning("Variance reduction not found in preferences file\n");
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
				g_warning("Pile-up not found in preferences file\n");
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
				g_warning("Download locations not found in preferences file\n");
				g_debug("Number of locations: %i\n",g_strv_length((gchar **) xmimsim_download_locations));
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
				g_warning("Poisson noise not found in preferences file\n");
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
				g_warning("Escape peaks not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Escape peaks", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				prefs->b = TRUE;
			}
			break;
		case XMIMSIM_GUI_PREFS_ADVANCED_COMPTON:
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Advanced Compton", &error);
			if (error != NULL) {
				//error
				g_warning("Advanced Compton not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Advanced Compton", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				prefs->b = FALSE;
			}
			break;
		case XMIMSIM_GUI_PREFS_DEFAULT_SEEDS:
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Default seeds", &error);
			if (error != NULL) {
				//error
				g_warning("Default seeds not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Default seeds", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				prefs->b = FALSE;
			}
			break;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
		case XMIMSIM_GUI_PREFS_OPENCL:
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "OpenCL", &error);
			if (error != NULL) {
				//error
				g_warning("OpenCL not found in preferences file\n");
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
		case XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE:
			{
			gchar *temps = g_key_file_get_string(keyfile, "Preferences", "Custom detector response", &error);
			if (error != NULL) {
				//error
				g_warning("Custom detector response not found in preferences file\n");
				g_key_file_set_string(keyfile, "Preferences","Custom detector response", "None");
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				prefs->s = NULL;
				break;
			}
			if (strcmp(temps, "None") == 0)
				prefs->s = NULL;
			else
				prefs->s = temps;
			}
			break;
#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
		case XMIMSIM_GUI_PREFS_NOTIFICATIONS:
			prefs->b = g_key_file_get_boolean(keyfile, "Preferences", "Notifications", &error);
			if (error != NULL) {
				//error
				g_warning("Notifications not found in preferences file\n");
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
		case XMIMSIM_GUI_SOURCES_LAST_USED:
			prefs->i = g_key_file_get_integer(keyfile, "Sources last used", "Page", &error);
			if (error != NULL) {
				//error
				g_warning("Sources last used Page not found in preferences file\n");
				g_key_file_set_integer(keyfile, "Sources last used", "Page", 0);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				prefs->i = 0;
			}
			else if (prefs->i < 0 || prefs->i > 1) {
				g_warning( "Invalid value detected for Sources last used Page in preferences file\n");
				return 0;
			}
			break;
		default:
			g_warning("Unknown preference requested in xmimsim_gui_get_prefs\n");
			return 0;
	}





	g_free(prefs_file);
	g_key_file_free(keyfile);
	return 1;
}


int xmimsim_gui_set_prefs(int kind, union xmimsim_prefs_val prefs) {
	gchar *prefs_file;
	GKeyFile *keyfile;


	prefs_file = xmimsim_gui_get_preferences_filename();

	keyfile = g_key_file_new();

	if (!g_key_file_load_from_file(keyfile, prefs_file, (GKeyFileFlags) (G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS), NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_file))
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
		case XMIMSIM_GUI_PREFS_ADVANCED_COMPTON:
			g_key_file_set_boolean(keyfile, "Preferences","Advanced Compton", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_DEFAULT_SEEDS:
			g_key_file_set_boolean(keyfile, "Preferences","Default seeds", prefs.b);
			break;
		case XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE:
			if (prefs.s != NULL)
				g_key_file_set_string(keyfile, "Preferences","Custom detector response", prefs.s);
			else
				g_key_file_set_string(keyfile, "Preferences","Custom detector response", "None");
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
		case XMIMSIM_GUI_SOURCES_LAST_USED:
			g_key_file_set_integer(keyfile, "Sources last used", "Page", prefs.i);
			break;
		default:
			g_warning("Unknown preference requested in xmimsim_gui_set_prefs\n");
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

	return 1;
}

static gboolean layers_backspace_key_clicked_cb(GtkWidget *widget, GdkEventKey *event, GtkTreeSelection *select_layers) {
	if (event->keyval == gdk_keyval_from_name("BackSpace")) {
		GtkTreeModel *model;
		GList *selected_rows = gtk_tree_selection_get_selected_rows(select_layers, &model);
		//convert to references
		int n_selected = gtk_tree_selection_count_selected_rows(select_layers);
		GtkTreeRowReference **refs = (GtkTreeRowReference**) g_malloc(sizeof(GtkTreeRowReference*)*n_selected);
		int i;
		GtkTreeIter iter;
		for (i = 0 ; i < n_selected ; i++) {
			refs[i] = gtk_tree_row_reference_new(model, (GtkTreePath *) g_list_nth_data(selected_rows, i));
		}
		for (i = 0 ; i < n_selected ; i++) {
			GtkTreePath *path = gtk_tree_row_reference_get_path(refs[i]);
			gtk_tree_model_get_iter(model, &iter, path);
			gchar *layer_name;
			gtk_tree_model_get(model, &iter, 0, &layer_name, -1);
			gtk_tree_path_free(path);
			gtk_tree_row_reference_free(refs[i]);

			if (xmimsim_gui_remove_user_defined_layer(layer_name) == 1)
				gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
			else {
				//something went very wrong!
				GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(widget))),
					GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_MESSAGE_ERROR,
					GTK_BUTTONS_CLOSE,
					"Error removing layer %s. Please report this incident to the developers.\n",
					layer_name
	       				);
	     			gtk_dialog_run (GTK_DIALOG (dialog));
	     			gtk_widget_destroy (dialog);
				return TRUE;
			}

			g_free(layer_name);
		}

		g_free(refs);
		g_list_free_full (selected_rows, (GDestroyNotify) gtk_tree_path_free);
		return TRUE;
	}

	return FALSE;

}

void xmimsim_gui_launch_preferences(GtkWidget *widget, gpointer data) {
	GtkWidget *window;
	GtkWidget *main_window = GTK_WIDGET(data);
	GtkWidget *notebook;
	GtkWidget *master_box;
	struct prefsWidgets *pw = (struct prefsWidgets *) g_malloc(sizeof(struct prefsWidgets));
	pw->parent_window = main_window;

	master_box = gtk_vbox_new(FALSE,2);
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	pw->window = window;
	gtk_window_set_title(GTK_WINDOW(window), "Preferences");
	gtk_widget_set_size_request(window,500,500);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(window),TRUE);
	gtk_window_set_transient_for(GTK_WINDOW(window), GTK_WINDOW(main_window));

	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	//gtk_window_set_deletable(GTK_WINDOW(window), FALSE);

	notebook = gtk_notebook_new();
	gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), TRUE);
	gtk_box_pack_start(GTK_BOX(master_box), notebook, TRUE,TRUE,1);
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_container_add(GTK_CONTAINER(window), master_box);



	GtkWidget *label;

	union xmimsim_prefs_val xpv;

	GtkWidget *superframe = gtk_vbox_new(TRUE,0);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window), superframe);

	label = gtk_label_new("Simulation default options");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Simulation defaults</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_window, label);


	pw->MlinesW = gtk_check_button_new_with_label("Simulate M-lines");
	gtk_widget_set_tooltip_text(pw->MlinesW,"Enables the simulation of M-lines. Disabling this option may lead to a significant performance increase. Should always be enabled when high atomic number elements are present in the sample.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_M_LINES, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->MlinesW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->MlinesW, TRUE, FALSE, 0);

	pw->rad_cascadeW = gtk_check_button_new_with_label("Simulate the radiative cascade effect");
	gtk_widget_set_tooltip_text(pw->rad_cascadeW,"Enables the simulation of the radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_RAD_CASCADE, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->rad_cascadeW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->rad_cascadeW, TRUE, FALSE, 0);

	pw->nonrad_cascadeW = gtk_check_button_new_with_label("Simulate the non-radiative cascade effect");
	gtk_widget_set_tooltip_text(pw->nonrad_cascadeW,"Enables the simulation of the non-radiative cascade effect (atomic relaxation). Should always be enabled unless one needs to investigate the contribution of the non-radiative cascade effect.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NONRAD_CASCADE, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->nonrad_cascadeW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->nonrad_cascadeW, TRUE, FALSE, 0);

	pw->variance_reductionW = gtk_check_button_new_with_label("Enable variance reduction techniques");
	gtk_widget_set_tooltip_text(pw->variance_reductionW,"Disabling this option enables the brute-force method. Should only be used in combination with a high number of simulated photons.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->variance_reductionW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->variance_reductionW, TRUE, FALSE, 0);

	pw->pile_upW = gtk_check_button_new_with_label("Enable pulse pile-up simulation");
	gtk_widget_set_tooltip_text(pw->pile_upW,"When activated, will estimate detector electronics pulse pile-up. Determined by the pulse width parameter in Detector settings.");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_PILE_UP, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->pile_upW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->pile_upW, TRUE, FALSE, 0);

	pw->poissonW = gtk_check_button_new_with_label("Enable Poisson noise generation");
	gtk_widget_set_tooltip_text(pw->poissonW,"Enabling this feature will add noise according to a Poisson distribution to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_POISSON, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->poissonW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->poissonW, TRUE, FALSE, 0);

	pw->escape_peaksW = gtk_check_button_new_with_label("Enable escape peaks support");
	gtk_widget_set_tooltip_text(pw->escape_peaksW,"Enabling this feature will add fluorescence and Compton escape peaks to the convoluted spectra");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ESCAPE_PEAKS, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->escape_peaksW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->escape_peaksW, TRUE, FALSE, 0);

	pw->advanced_comptonW = gtk_check_button_new_with_label("Enable advanced Compton scattering simulation");
	gtk_widget_set_tooltip_text(pw->advanced_comptonW, "Enabling this feature will improve the simulation of the Compton scattering, and add support for the Compton fluorescence photons. Warning: due to the added complexity, the code will slow down considerably (at least a factor of 2, and increases with higher atomic number)");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_ADVANCED_COMPTON, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->advanced_comptonW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->advanced_comptonW, TRUE, FALSE, 0);

	pw->default_seedsW = gtk_check_button_new_with_label("Enable default seeds support");
	gtk_widget_set_tooltip_text(pw->default_seedsW, "Enabling this feature will set the seeds that will be used during the simulation to default values, instead of random ones. This is useful when exactly reproducible simulation results are required, usually for testing purposes");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SEEDS, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->default_seedsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->default_seedsW, TRUE, FALSE, 0);

#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H)
	pw->openclW = gtk_check_button_new_with_label("Enable OpenCL");
	gtk_widget_set_tooltip_text(pw->openclW, "Enabling OpenCL will have the simulation use the GPU in order to calculate the solid angle grids, resulting in considerably speed-up. Requires the installation of OpenCL drivers. Consult the website of the manufacturer of your videocard for more information");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_OPENCL, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->openclW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->openclW, TRUE, FALSE, 0);
#endif

	GtkWidget *hbox = gtk_hbox_new(FALSE, 0);
	pw->custom_detector_responseC = gtk_check_button_new_with_label("Custom detector response");
	gtk_widget_set_tooltip_text(pw->custom_detector_responseC, "Loads an alternative detector response routine from a dynamically loadable module. This module must export a function called \"xmi_detector_convolute_all_custom\". More information can be found in the manual");
	g_signal_connect(G_OBJECT(pw->custom_detector_responseC), "toggled", G_CALLBACK(custom_detector_response_toggled_cb), (gpointer) pw);
	gtk_box_pack_start(GTK_BOX(hbox), pw->custom_detector_responseC, FALSE, FALSE, 0);
	pw->custom_detector_responseE = gtk_entry_new();
	gtk_editable_set_editable(GTK_EDITABLE(pw->custom_detector_responseE), FALSE);
	gtk_box_pack_start(GTK_BOX(hbox), pw->custom_detector_responseE, TRUE, TRUE, 3);
	pw->custom_detector_responseB = gtk_button_new_from_stock(GTK_STOCK_OPEN);
	g_signal_connect(G_OBJECT(pw->custom_detector_responseB), "clicked", G_CALLBACK(custom_detector_response_clicked_cb), pw->custom_detector_responseE);
	gtk_box_pack_end(GTK_BOX(hbox), pw->custom_detector_responseB, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, TRUE, FALSE, 0);
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	if (xpv.s != NULL) {
		gtk_widget_set_sensitive(pw->custom_detector_responseE, TRUE);
		gtk_widget_set_sensitive(pw->custom_detector_responseB, TRUE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->custom_detector_responseC), TRUE);
		gtk_entry_set_text(GTK_ENTRY(pw->custom_detector_responseE), xpv.s);
	}
	else {
		gtk_widget_set_sensitive(pw->custom_detector_responseE, FALSE);
		gtk_widget_set_sensitive(pw->custom_detector_responseB, FALSE);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->custom_detector_responseC), FALSE);
	}
	g_free(xpv.s);


	//second page
	superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Updates</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);
	GtkWidget *buttonbox;
	unsigned int i;
	GtkTreeIter iter;

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

	pw->check_updatesW = gtk_check_button_new_with_label("Check for updates on startup");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->check_updatesW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe), pw->check_updatesW, TRUE, FALSE, 3);

	label = gtk_label_new("Download locations for updates");
	gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_box_pack_start(GTK_BOX(superframe), label, FALSE, FALSE, 2);

	GtkWidget *updatesboxW = gtk_hbox_new(FALSE,5);
	GtkListStore *store_prefsL;
	GtkTreeViewColumn *column;
	GtkTreeSelection *select;
	GtkCellRenderer *renderer;
	store_prefsL = gtk_list_store_new(N_COLUMNS_PREFS, G_TYPE_STRING, G_TYPE_STRING);
	pw->update_urlsW = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store_prefsL));

	renderer = gtk_cell_renderer_text_new();
	g_signal_connect(renderer, "edited", G_CALLBACK(url_edited_cb), (gpointer) pw->update_urlsW);
	g_object_set(renderer, "editable", TRUE, NULL);
	gtk_cell_renderer_set_alignment(renderer, 0., 0.5);
	column = gtk_tree_view_column_new_with_attributes("Website", renderer,"text",URL_COLUMN_PREFS,NULL);
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_alignment(column, 0.);
	gtk_tree_view_append_column(GTK_TREE_VIEW(pw->update_urlsW), column);
	gtk_tree_view_column_set_expand(column, TRUE);

	renderer = gtk_cell_renderer_pixbuf_new();
	gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
	g_object_set(renderer, "stock-size", GTK_ICON_SIZE_SMALL_TOOLBAR, NULL);
	column = gtk_tree_view_column_new_with_attributes("Status", renderer,"stock-id",STATUS_COLUMN_PREFS,NULL);
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(pw->update_urlsW), column);
	gtk_tree_view_column_set_expand(column, FALSE);




	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(pw->update_urlsW));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_SINGLE);

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 300,150);
	gtk_container_add(GTK_CONTAINER(scrolled_window), pw->update_urlsW);
	gtk_box_pack_start(GTK_BOX(updatesboxW),scrolled_window, FALSE, FALSE,3 );


	buttonbox = gtk_vbox_new(FALSE, 5);
	GtkWidget *addButton;
	GtkWidget *removeButton;

	addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
	removeButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);

	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, TRUE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), removeButton, TRUE, FALSE, 3);
	gtk_widget_set_sensitive(removeButton, FALSE);
	g_signal_connect(G_OBJECT(removeButton), "clicked", G_CALLBACK(url_delete_button_clicked_cb) , (gpointer) pw->update_urlsW);
	g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(url_add_button_clicked_cb) , (gpointer) pw->update_urlsW);

	gtk_box_pack_start(GTK_BOX(updatesboxW),buttonbox, TRUE, FALSE, 3);


	//populate tree
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	for (i= 0 ; i < g_strv_length(xpv.ss) ; i++) {
		gtk_list_store_append(store_prefsL,&iter);
		gtk_list_store_set(store_prefsL, &iter, URL_COLUMN_PREFS, xpv.ss[i], -1);
		if (xmi_msim_gui_utils_check_download_url(xpv.ss[i]) == TRUE) {
			gtk_list_store_set(store_prefsL, &iter, STATUS_COLUMN_PREFS, GTK_STOCK_YES, -1);
		}
		else {
			gtk_list_store_set(store_prefsL, &iter, STATUS_COLUMN_PREFS, GTK_STOCK_NO, -1);
		}
	}

	g_strfreev(xpv.ss);

	gtk_box_pack_start(GTK_BOX(superframe), updatesboxW, FALSE, FALSE, 2);


	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(url_selection_changed_cb), (gpointer) removeButton);

#else
	label = gtk_label_new("XMI-MSIM was built without support for automatic updates. Consider recompiling after installing libcurl and json-glib.");
	//gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);
#endif

	//Third page: user-defined layers
	superframe = gtk_vbox_new(FALSE,5);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">User defined layers</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);
	label = gtk_label_new("Delete layers by selecting them and hitting the backspace key. This operation cannot be undone.");
	//gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);

	gchar **layer_names = xmimsim_gui_get_user_defined_layer_names();
	GtkListStore *store_layersL;
	GtkTreeViewColumn *column_layers;
	GtkTreeSelection *select_layers;
	GtkCellRenderer *renderer_layers;
	store_layersL = gtk_list_store_new(1, G_TYPE_STRING);
	GtkWidget *tree_layers = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store_layersL));
	renderer_layers = gtk_cell_renderer_text_new();
	gtk_cell_renderer_set_alignment(renderer_layers, 0., 0.5);
	column_layers = gtk_tree_view_column_new_with_attributes("Layer name", renderer_layers,"text",0,NULL);
	gtk_tree_view_column_set_resizable(column_layers,TRUE);
	gtk_tree_view_column_set_alignment(column_layers, 0.);
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_layers), column_layers);
	select_layers = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_layers));
	gtk_tree_selection_set_mode(select_layers, GTK_SELECTION_MULTIPLE);
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_set_size_request(scrolled_window, 300,150);
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree_layers);
	gtk_box_pack_start(GTK_BOX(superframe),scrolled_window, TRUE, TRUE,3 );
	//populate tree
	if (layer_names != NULL && g_strv_length(layer_names) > 0) {
		for (i= 0 ; i < g_strv_length(layer_names) ; i++) {
			gtk_list_store_append(store_layersL,&iter);
			gtk_list_store_set(store_layersL, &iter, 0, layer_names[i], -1);
		}
	}
	g_signal_connect(G_OBJECT(tree_layers), "key-press-event", G_CALLBACK(layers_backspace_key_clicked_cb), (gpointer) select_layers);

	//Fourth page: advanced
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
	notificationsW = gtk_check_button_new_with_label("Enable notifications");
	gtk_widget_set_tooltip_text(notificationsW,"Check this button to enable notifications support");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, &xpv) == 0) {
		//abort
		preferences_error_handler(main_window);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(notificationsW),xpv.b);
	gtk_box_pack_start(GTK_BOX(superframe),notificationsW, FALSE, FALSE, 3);
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
	g_signal_connect(G_OBJECT(applyButton), "clicked", G_CALLBACK(preferences_apply_button_clicked), (gpointer) pw);


	gtk_widget_show_all(window);
}

static gboolean detector_response_dlm_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	GtkFileFilter *filter = gtk_file_filter_new();

	gtk_file_filter_add_pattern(filter, "*." G_MODULE_SUFFIX);
	if (gtk_file_filter_filter(filter, filter_info) == TRUE && xmi_check_detector_convolute_plugin((char *) filter_info->filename) == 1)
		return TRUE;

	return FALSE;
}

void custom_detector_response_clicked_cb(GtkToggleButton *button, GtkWidget *entry) {
	GtkWidget *dialog;
	GtkFileFilter *filter;
	gchar *filename;

	filter = gtk_file_filter_new();
	gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME, detector_response_dlm_filter, NULL, NULL);
	gtk_file_filter_set_name(filter,"Detector response DLM");
	dialog = gtk_file_chooser_dialog_new ("Select detector response function DLM",
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(button))),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
		NULL);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_entry_set_text(GTK_ENTRY(entry), filename);
		g_free(filename);
	}
	gtk_widget_destroy(dialog);
}

char *xmimsim_gui_get_preferences_filename() {

#ifdef MAC_INTEGRATION
        NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask,TRUE);
        NSString *documentsDirectory = [paths objectAtIndex:0];
        const gchar *config_dir = [documentsDirectory cStringUsingEncoding:NSUTF8StringEncoding];
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif

	//first check if the preferences file exists!
	gchar *prefs_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM",config_dir);
	gchar *prefs_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "preferences.ini",prefs_dir);
	g_free(prefs_dir);


#ifdef MAC_INTEGRATION
        [pool drain];
#endif
	return prefs_file;
}
