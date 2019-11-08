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
#include "xmimsim-gui-compat.h"
#include "xmimsim-gui-prefs.h"
#include "xmimsim-gui-utils.h"
#include "xmimsim-gui-options-box.h"
#include "xmi_detector.h"
#include "xmi_solid_angle.h"
#include <glib.h>
#include <glib/gstdio.h>
#include <xmi_aux.h>
#include <string.h>

#ifdef MAC_INTEGRATION
#include "xmi_resources_mac.h"
#endif

#if defined(HAVE_LIBSOUP) && defined(HAVE_JSONGLIB)
#include "xmimsim-gui-updater.h"
#endif

struct prefsWidgets {
	GtkWidget *window;
	GtkWidget *options_boxW;
#if defined(HAVE_LIBSOUP) && defined(HAVE_JSONGLIB)
	GtkWidget *check_updatesW; // checkbutton
	GtkWidget *update_urlsW;   // treeview
#endif
	GtkWidget *notificationsW;
	GtkWidget *default_save_folderW;
	GArray *deleted_layers;
	GtkWidget *layers_tree_view;
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

static gboolean hdf5_file_filter(const GtkFileFilterInfo *filter_info, gpointer data) {
	int kind = GPOINTER_TO_INT(data);

	int file_kind = xmi_get_hdf5_kind((char *) (filter_info->filename));

	if (kind != file_kind)
		return FALSE;
	return TRUE;
}

static void import_hdf5_data_cb(GtkWidget *window, int kind) {
	XmiMsimGuiFileChooserDialog *dialog;
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
	dialog = xmi_msim_gui_file_chooser_dialog_new ("Open XMI-MSIM HDF5 file",
		GTK_WINDOW(window),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		"document-open",
		"_Cancel"
		);
	gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);
	GtkWidget *forceW = gtk_check_button_new_with_label("Force copying");
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(forceW), FALSE);
	gtk_widget_show_all(forceW);
	gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog), forceW);


	xmi_msim_gui_file_chooser_dialog_set_modal(dialog, TRUE);

	if (xmi_msim_gui_file_chooser_dialog_run(dialog) == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER (dialog));
		gboolean forced = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(forceW));
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);
		char *target = NULL;
		GtkWidget *message_dialog;
		if (kind == XMI_HDF5_SOLID_ANGLES) {
			if (xmi_get_solid_angle_file(&target, 1) == 0) {
				message_dialog = gtk_message_dialog_new(GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not determine the location of the solid angle grids file."
	       			);
	     			gtk_dialog_run(GTK_DIALOG(message_dialog));
	     			gtk_widget_destroy(message_dialog);
	     			return;
			}
		}
		else if (kind == XMI_HDF5_ESCAPE_RATIOS) {
			if (xmi_get_escape_ratios_file(&target, 1) == 0) {
				message_dialog = gtk_message_dialog_new(GTK_WINDOW(window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Could not determine the location of the escape ratios file."
	       			);
	     			gtk_dialog_run(GTK_DIALOG(message_dialog));
	     			gtk_widget_destroy(message_dialog);
	     			return;
			}
		}
		int ncopied = xmi_copy_between_hdf5_files(kind, filename, target, NULL, forced ? 1 : 0);

		if (ncopied >=0) {
			message_dialog = gtk_message_dialog_new(GTK_WINDOW(window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_INFO,
			GTK_BUTTONS_CLOSE,
			"%i groups were successfully copied from %s to %s",
			ncopied,
			filename,
			target
	       		);
		}
		else {
			message_dialog = gtk_message_dialog_new(GTK_WINDOW(window),
			GTK_DIALOG_DESTROY_WITH_PARENT,
			GTK_MESSAGE_ERROR,
			GTK_BUTTONS_CLOSE,
			"An error occurred while copying from %s to %s",
			filename,
			target
	       		);
		}
	     	gtk_dialog_run(GTK_DIALOG(message_dialog));
	     	gtk_widget_destroy(message_dialog);

		g_free(filename);
		g_free(target);
	}
	else
		xmi_msim_gui_file_chooser_dialog_destroy(dialog);


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


#if defined(HAVE_LIBSOUP) && defined(HAVE_JSONGLIB)
static void url_delete_button_clicked_cb(GtkWidget *widget, GtkTreeView *tree) {
	GtkTreeSelection *selection = gtk_tree_view_get_selection(tree);
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(tree));
	GtkTreeIter iter;

	gtk_tree_selection_get_selected(selection, NULL, &iter);

	gtk_list_store_remove(store, &iter);

	return;
}

static void check_download_ready_cb(GtkListStore *store, GAsyncResult *result, gpointer user_data) {
	GtkTreePath *path = user_data;
	GtkTreeIter iter;
	gtk_tree_model_get_iter(GTK_TREE_MODEL(store), &iter, path);
	gtk_tree_path_free(path);
	if (xmi_msim_gui_updater_check_download_url_finish(store, result)) {
		GdkPixbuf *gtk_yes  = gdk_pixbuf_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-yes.png", NULL);
		gtk_list_store_set(store, &iter,
			STATUS_COLUMN_PREFS, gtk_yes,
			-1);
	}
}

static void url_edited_cb(GtkCellRendererText *cell, gchar *path_string, gchar *new_text, GtkTreeView *tree) {
	GtkTreeSelection *selection = gtk_tree_view_get_selection(tree);
	GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(tree));
	GtkTreeIter iter;

	gtk_tree_selection_get_selected(selection, NULL, &iter);

	GdkPixbuf *gtk_no = gdk_pixbuf_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-no.png", NULL);
	gtk_list_store_set(store, &iter,
		URL_COLUMN_PREFS, new_text,
		STATUS_COLUMN_PREFS, gtk_no,
		-1);

	xmi_msim_gui_updater_check_download_url_async(store, new_text, (GAsyncReadyCallback) check_download_ready_cb, gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter));
}

static void url_add_button_clicked_cb(GtkWidget *widget, gpointer data) {
	GtkTreeIter iter;
	GtkWidget *tree = GTK_WIDGET(data);
	GtkListStore *store_prefsL = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(tree)));
	GdkPixbuf *gtk_no = gdk_pixbuf_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-no.png", NULL);

	gtk_list_store_append(store_prefsL, &iter);
	gtk_list_store_set(store_prefsL, &iter,
		URL_COLUMN_PREFS, "http://",
		STATUS_COLUMN_PREFS, gtk_no,
		-1);

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
	g_application_quit(g_application_get_default());
}


static void preferences_apply_button_clicked(GtkWidget *button, gpointer data) {
	//read everything and store it in the preferences file
	struct prefsWidgets *pw = data;
	GValue xpv = G_VALUE_INIT;

	xmi_msim_gui_options_box_save_to_prefs(XMI_MSIM_GUI_OPTIONS_BOX(pw->options_boxW));	

#if defined(HAVE_LIBSOUP) && defined(HAVE_JSONGLIB)
	g_value_init(&xpv, G_TYPE_BOOLEAN);
	g_value_set_boolean(&xpv, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->check_updatesW)));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}
	g_value_unset(&xpv);

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

	g_value_init(&xpv, G_TYPE_STRV);
	g_value_take_boxed(&xpv, urls);
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, &xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}
	g_value_unset(&xpv);
#endif

	g_value_init(&xpv, G_TYPE_BOOLEAN);
	g_value_set_boolean(&xpv, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pw->notificationsW)));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, &xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}
	g_value_unset(&xpv);

	g_value_init(&xpv, G_TYPE_STRING);
	g_value_take_string(&xpv, gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(pw->default_save_folderW)));
	if (xmimsim_gui_set_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SAVE_FOLDER, &xpv) == 0) {
		//abort
		preferences_error_handler(pw->window);
	}
	g_value_unset(&xpv);

	// delete layers
	int i;

	for (i = 0 ; i < pw->deleted_layers->len ; i++) {
		gchar *layer_name = g_array_index(pw->deleted_layers, gchar *, i);
		if (xmimsim_gui_remove_user_defined_layer(layer_name) != 1) {
			//something went very wrong!
			GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(pw->window),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_ERROR,
				GTK_BUTTONS_CLOSE,
				"Error removing layer %s. Please report this incident to the developers.\n",
				layer_name
       				);
     			gtk_dialog_run (GTK_DIALOG (dialog));
     			gtk_widget_destroy (dialog);
			return;
		}

	}
	g_strfreev((gchar **) pw->deleted_layers->data);
	g_array_free(pw->deleted_layers, FALSE);

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
	g_key_file_set_boolean(keyfile, "Preferences","GPU", TRUE);
	g_key_file_set_boolean(keyfile, "Preferences","Default seeds", FALSE);
	g_key_file_set_string_list(keyfile, "Preferences", "Download locations", xmimsim_download_locations, g_strv_length((gchar **) xmimsim_download_locations));
	g_key_file_set_string(keyfile, "Preferences","Custom detector response", "None");
	g_key_file_set_string(keyfile, "Preferences","Default save folder", g_get_user_special_dir(G_USER_DIRECTORY_DOCUMENTS));
	g_key_file_set_boolean(keyfile, "Preferences","Notifications", TRUE);


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
        const gchar *config_dir = xmi_resources_mac_get_user_data_dir();
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif

	gchar *prefs_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM",config_dir);
	file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "user-defined-layers.ini",prefs_dir);
	g_free(prefs_dir);

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

xmi_layer* xmimsim_gui_get_user_defined_layer(const gchar *layer_name) {
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

	xmi_layer *layer = (xmi_layer *) g_malloc(sizeof(xmi_layer));
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

int xmimsim_gui_add_user_defined_layer(xmi_layer *layer, const gchar *layer_name) {
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

int xmimsim_gui_get_prefs(int kind, GValue *prefs) {
	gchar *prefs_file;
	GKeyFile *keyfile;

	g_value_unset(prefs);

	prefs_file = xmimsim_gui_get_preferences_filename();

	keyfile = g_key_file_new();
	GError *error = NULL;

	if (!g_key_file_load_from_file(keyfile, prefs_file, G_KEY_FILE_KEEP_COMMENTS | G_KEY_FILE_KEEP_TRANSLATIONS, NULL)) {
		if (!xmimsim_gui_create_prefs_file(keyfile, prefs_file))
			return 0;
	}

	//extract required information from keyfile
	gchar *prefs_file_contents;
	switch (kind) {
		case XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Check for updates", &error));
			if (error != NULL) {
				//error
				g_warning("Check for updates not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Check for updates", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_M_LINES:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "M lines", &error));
			if (error != NULL) {
				//error
				g_warning("M lines not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","M lines", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_RAD_CASCADE:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Radiative cascade", &error));
			if (error != NULL) {
				//error
				g_warning("Radiative cascade not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_NONRAD_CASCADE:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Non-radiative cascade", &error));
			if (error != NULL) {
				//error
				g_warning("Non-radiative cascade not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Variance reduction", &error));
			if (error != NULL) {
				//error
				g_warning("Variance reduction not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Variance reduction", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_PILE_UP:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Pile-up", &error));
			if (error != NULL) {
				//error
				g_warning("Pile-up not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Pile-up", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, FALSE);
			}
			break;
		case XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS:
			g_value_init(prefs, G_TYPE_STRV);
			g_value_take_boxed(prefs, g_key_file_get_string_list(keyfile, "Preferences", "Download locations", NULL, &error));
			if (error != NULL) {
				//error
				g_warning("Download locations not found in preferences file\n");
				g_key_file_set_string_list(keyfile, "Preferences", "Download locations", xmimsim_download_locations, g_strv_length((gchar **) xmimsim_download_locations));
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_static_boxed(prefs, xmimsim_download_locations);
			}
			break;
		case XMIMSIM_GUI_PREFS_POISSON:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Poisson noise", &error));
			if (error != NULL) {
				//error
				g_warning("Poisson noise not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Poisson noise", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, FALSE);
			}
			break;
		case XMIMSIM_GUI_PREFS_ESCAPE_PEAKS:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Escape peaks", &error));
			if (error != NULL) {
				//error
				g_warning("Escape peaks not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Escape peaks", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_ADVANCED_COMPTON:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Advanced Compton", &error));
			if (error != NULL) {
				//error
				g_warning("Advanced Compton not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Advanced Compton", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, FALSE);
			}
			break;
		case XMIMSIM_GUI_PREFS_DEFAULT_SEEDS:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Default seeds", &error));
			if (error != NULL) {
				//error
				g_warning("Default seeds not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Default seeds", FALSE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, FALSE);
			}
			break;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H) || defined(HAVE_METAL)
		case XMIMSIM_GUI_PREFS_GPU:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "GPU", &error));
			if (error != NULL) {
				//error
				g_warning("GPU not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","GPU", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
#endif
		case XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE:
			{
			g_value_init(prefs, G_TYPE_STRING);
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
				g_value_set_string(prefs, NULL);
				break;
			}
			if (strcmp(temps, "None") == 0)
				g_value_set_string(prefs, NULL);
			else
				g_value_take_string(prefs, temps);
			}
			break;
		case XMIMSIM_GUI_PREFS_DEFAULT_SAVE_FOLDER:
			{
			g_value_init(prefs, G_TYPE_STRING);
			gchar *temps = g_key_file_get_string(keyfile, "Preferences", "Default save folder", &error);
			if (error != NULL) {
				//error
				g_warning("Default save folder not found in preferences file\n");
				g_key_file_set_string(keyfile, "Preferences","Default save folder", g_get_user_special_dir(G_USER_DIRECTORY_DOCUMENTS));
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_static_string(prefs, g_get_user_special_dir(G_USER_DIRECTORY_DOCUMENTS));
				break;
			}
			g_value_take_string(prefs, temps);
			}
			break;
		case XMIMSIM_GUI_PREFS_NOTIFICATIONS:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Notifications", &error));
			if (error != NULL) {
				//error
				g_warning("Notifications not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Notifications", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
#ifdef HAVE_GOOGLE_ANALYTICS
		case XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_SHOW_STARTUP_DIALOG:
			g_value_init(prefs, G_TYPE_BOOLEAN);
			g_value_set_boolean(prefs, g_key_file_get_boolean(keyfile, "Preferences", "Google Analytics Show Startup Dialog", &error));
			if (error != NULL) {
				//error
				g_warning("Google Analytics Show Startup Dialog not found in preferences file\n");
				g_key_file_set_boolean(keyfile, "Preferences","Google Analytics Show Startup Dialog", TRUE);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_set_boolean(prefs, TRUE);
			}
			break;
		case XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_UUID:
			{
			g_value_init(prefs, G_TYPE_STRING);
			gchar *temps = g_key_file_get_string(keyfile, "Preferences", "Google Analytics UUID", &error);
			if (error != NULL) {
				//error
				g_warning("Google Analytics UUID not found in preferences file\n");
				gchar *uuid = g_uuid_string_random();
				g_key_file_set_string(keyfile, "Preferences", "Google Analytics UUID", uuid);
				//save file
				prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
				if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
					return 0;
				g_free(prefs_file_contents);
				g_value_take_string(prefs, uuid);
				break;
			}
			g_value_take_string(prefs, temps);
			}
			break;
#endif
		default:
			g_warning("Unknown preference requested in xmimsim_gui_get_prefs\n");
			return 0;
	}

	g_free(prefs_file);
	g_key_file_free(keyfile);
	return 1;
}


int xmimsim_gui_set_prefs(int kind, const GValue *prefs) {
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
			g_key_file_set_boolean(keyfile, "Preferences", "Check for updates", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_M_LINES:
			g_key_file_set_boolean(keyfile, "Preferences", "M lines", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_RAD_CASCADE:
			g_key_file_set_boolean(keyfile, "Preferences", "Radiative cascade", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_NONRAD_CASCADE:
			g_key_file_set_boolean(keyfile, "Preferences", "Non-radiative cascade", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_VARIANCE_REDUCTION:
			g_key_file_set_boolean(keyfile, "Preferences", "Variance reduction", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_PILE_UP:
			g_key_file_set_boolean(keyfile, "Preferences", "Pile-up", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS:
			g_key_file_set_string_list(keyfile, "Preferences", "Download locations", g_value_get_boxed(prefs), g_strv_length(g_value_get_boxed(prefs)));
			break;
		case XMIMSIM_GUI_PREFS_POISSON:
			g_key_file_set_boolean(keyfile, "Preferences", "Poisson noise", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_ESCAPE_PEAKS:
			g_key_file_set_boolean(keyfile, "Preferences", "Escape peaks", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_ADVANCED_COMPTON:
			g_key_file_set_boolean(keyfile, "Preferences", "Advanced Compton", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_DEFAULT_SEEDS:
			g_key_file_set_boolean(keyfile, "Preferences", "Default seeds", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_CUSTOM_DETECTOR_RESPONSE:
			if (g_value_get_string(prefs) != NULL)
				g_key_file_set_string(keyfile, "Preferences", "Custom detector response", g_value_get_string(prefs));
			else
				g_key_file_set_string(keyfile, "Preferences", "Custom detector response", "None");
			break;
		case XMIMSIM_GUI_PREFS_DEFAULT_SAVE_FOLDER:
			g_key_file_set_string(keyfile, "Preferences", "Default save folder", g_value_get_string(prefs));
			break;
#if defined(HAVE_OPENCL_CL_H) || defined(HAVE_CL_CL_H) || defined(HAVE_METAL)
		case XMIMSIM_GUI_PREFS_GPU:
			g_key_file_set_boolean(keyfile, "Preferences", "GPU", g_value_get_boolean(prefs));
			break;
#endif
		case XMIMSIM_GUI_PREFS_NOTIFICATIONS:
			g_key_file_set_boolean(keyfile, "Preferences", "Notifications", g_value_get_boolean(prefs));
			break;
#ifdef HAVE_GOOGLE_ANALYTICS
		case XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_SHOW_STARTUP_DIALOG:
			g_key_file_set_boolean(keyfile, "Preferences","Google Analytics Show Startup Dialog", g_value_get_boolean(prefs));
			break;
		case XMIMSIM_GUI_PREFS_GOOGLE_ANALYTICS_UUID:
			g_key_file_set_string(keyfile, "Preferences","Google Analytics UUID", g_value_get_string(prefs));
			break;
#endif
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

static gboolean layers_backspace_key_clicked_cb(GtkWidget *widget, GdkEventKey *event, struct prefsWidgets *ew) {
	if (event->keyval != gdk_keyval_from_name("BackSpace"))
		return FALSE;
	
	GtkTreeSelection *select_layers = gtk_tree_view_get_selection(GTK_TREE_VIEW(ew->layers_tree_view));

	GtkTreeModel *model;
	GList *selected_rows = gtk_tree_selection_get_selected_rows(select_layers, &model);
	int n_selected = g_list_length(selected_rows);
	int i;
	GtkTreeIter iter;
	GtkTreeRowReference **refs = (GtkTreeRowReference**) g_malloc(sizeof(GtkTreeRowReference*)*n_selected);
	for (i = 0 ; i < n_selected ; i++) {
		refs[i] = gtk_tree_row_reference_new(model, (GtkTreePath *) g_list_nth_data(selected_rows, i));
	}

	for (i = 0 ; i < n_selected ; i++) {
		GtkTreePath *path = gtk_tree_row_reference_get_path(refs[i]);;
		gtk_tree_model_get_iter(model, &iter, path);
		gchar *layer_name;
		gtk_tree_model_get(model, &iter, 0, &layer_name, -1);
		g_array_append_val(ew->deleted_layers, layer_name);
		gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
		gtk_tree_row_reference_free(refs[i]);
	}

	g_free(refs);
	g_list_free_full (selected_rows, (GDestroyNotify) gtk_tree_path_free);

	return TRUE;
}

static GtkWidget *preferences_dialog = NULL;

static GtkWidget* create_dialog(void) {
	GtkWidget *window;
	GtkWidget *notebook;
	GtkWidget *master_box;
	struct prefsWidgets *pw = (struct prefsWidgets *) g_malloc(sizeof(struct prefsWidgets));
	pw->deleted_layers = g_array_new(TRUE, FALSE, sizeof(gchar *));

	master_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(master_box), FALSE);
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	pw->window = window;
	gtk_window_set_title(GTK_WINDOW(window), "Preferences");
	gtk_widget_set_size_request(window, 600, 600);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);

	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	//gtk_window_set_deletable(GTK_WINDOW(window), FALSE);

	notebook = gtk_notebook_new();
	gtk_notebook_set_scrollable(GTK_NOTEBOOK(notebook), TRUE);
	gtk_box_pack_start(GTK_BOX(master_box), notebook, TRUE,TRUE,1);
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);
	gtk_container_add(GTK_CONTAINER(window), master_box);



	GtkWidget *label;

	GtkWidget *options_boxW = xmi_msim_gui_options_box_new();
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL,NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled_window), options_boxW);
	pw->options_boxW = options_boxW;

	label = gtk_label_new("Simulation default options");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Simulation defaults</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_window, label);



	//second page
	GtkWidget *superframe = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(superframe), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Updates</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);
	GtkWidget *buttonbox;
	unsigned int i;
	GtkTreeIter iter;
	GValue xpv = G_VALUE_INIT;

#if defined(RPM_BUILD)
	label = gtk_label_new("XMI-MSIM was built with Redhat Package Manager. All updates should be installed with yum.");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);

#elif defined(DEB_BUILD)
	label = gtk_label_new("XMI-MSIM was built with Debian Package Manager. All updates should be installed with apt-get or aptitude.");
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);
#elif defined(HAVE_LIBSOUP) && defined(HAVE_JSONGLIB)

	pw->check_updatesW = gtk_check_button_new_with_label("Check for updates on startup");

	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_CHECK_FOR_UPDATES, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->check_updatesW), g_value_get_boolean(&xpv));
	g_value_unset(&xpv);
	gtk_box_pack_start(GTK_BOX(superframe), pw->check_updatesW, TRUE, FALSE, 3);

	label = gtk_label_new("Download locations for updates");
	gtk_box_pack_start(GTK_BOX(superframe), label, FALSE, FALSE, 2);

	GtkWidget *updatesboxW = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(updatesboxW), FALSE);
	GtkListStore *store_prefsL;
	GtkTreeViewColumn *column;
	GtkTreeSelection *select;
	GtkCellRenderer *renderer;
	store_prefsL = gtk_list_store_new(N_COLUMNS_PREFS, G_TYPE_STRING, GDK_TYPE_PIXBUF);
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
	column = gtk_tree_view_column_new_with_attributes("Status", renderer,
		"pixbuf", STATUS_COLUMN_PREFS,
		NULL);
	gtk_tree_view_column_set_resizable(column, FALSE);
	gtk_tree_view_column_set_alignment(column, 0.5);
	gtk_tree_view_append_column(GTK_TREE_VIEW(pw->update_urlsW), column);
	gtk_tree_view_column_set_expand(column, FALSE);




	select = gtk_tree_view_get_selection(GTK_TREE_VIEW(pw->update_urlsW));
	gtk_tree_selection_set_mode(select, GTK_SELECTION_SINGLE);

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, -1,150);
	gtk_container_add(GTK_CONTAINER(scrolled_window), pw->update_urlsW);
	GtkWidget *frame = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(frame), scrolled_window);
	gtk_box_pack_start(GTK_BOX(updatesboxW), frame, TRUE, TRUE, 3);


	buttonbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(buttonbox), FALSE);
	GtkWidget *addButton;
	GtkWidget *removeButton;

	addButton = gtk_button_new_from_icon_name("list-add", GTK_ICON_SIZE_BUTTON);
	removeButton = gtk_button_new_from_icon_name("list-remove", GTK_ICON_SIZE_BUTTON);

	gtk_box_pack_start(GTK_BOX(buttonbox), addButton, FALSE, FALSE, 3);
	gtk_box_pack_start(GTK_BOX(buttonbox), removeButton, FALSE, FALSE, 3);
	gtk_widget_set_sensitive(removeButton, FALSE);
	g_signal_connect(G_OBJECT(removeButton), "clicked", G_CALLBACK(url_delete_button_clicked_cb) , (gpointer) pw->update_urlsW);
	g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(url_add_button_clicked_cb) , (gpointer) pw->update_urlsW);

	gtk_box_pack_start(GTK_BOX(updatesboxW),buttonbox, FALSE, FALSE, 3);


	//populate tree
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DOWNLOAD_LOCATIONS, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}

	GdkPixbuf *gtk_no = gdk_pixbuf_new_from_resource("/com/github/tschoonj/xmimsim/gui/icons/gtk-no.png", NULL);

	gchar **download_locations = g_value_get_boxed(&xpv);

	for (i= 0 ; i < g_strv_length(download_locations) ; i++) {
		gtk_list_store_append(store_prefsL,&iter);
		gtk_list_store_set(store_prefsL, &iter,
			URL_COLUMN_PREFS, download_locations[i],
			STATUS_COLUMN_PREFS, gtk_no,
			-1);
		xmi_msim_gui_updater_check_download_url_async(store_prefsL, download_locations[i], (GAsyncReadyCallback) check_download_ready_cb, gtk_tree_model_get_path(GTK_TREE_MODEL(store_prefsL), &iter));
	}
	g_value_unset(&xpv);

	gtk_box_pack_start(GTK_BOX(superframe), updatesboxW, FALSE, FALSE, 2);


	g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(url_selection_changed_cb), (gpointer) removeButton);

#else
	label = gtk_label_new("XMI-MSIM was built without support for automatic updates. Consider recompiling after installing libsoup and json-glib.");
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_box_pack_start(GTK_BOX(superframe), label, TRUE, FALSE,1);
#endif

	//Third page: user-defined layers
	superframe = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(superframe), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">User defined layers</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);
	label = gtk_label_new("Delete layers by selecting them and hitting the backspace key.\nThis operation will be executed when clicking the Apply button.");
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
	pw->layers_tree_view = tree_layers;
	select_layers = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_layers));
	gtk_tree_selection_set_mode(select_layers, GTK_SELECTION_MULTIPLE);
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	//gtk_widget_set_size_request(scrolled_window, 300,150);
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree_layers);
	GtkWidget *frame2 = gtk_frame_new(NULL);
	gtk_container_add(GTK_CONTAINER(frame2), scrolled_window);
	gtk_box_pack_start(GTK_BOX(superframe), frame2, TRUE, TRUE, 3);
	//populate tree
	if (layer_names != NULL && g_strv_length(layer_names) > 0) {
		for (i= 0 ; i < g_strv_length(layer_names) ; i++) {
			gtk_list_store_append(store_layersL,&iter);
			gtk_list_store_set(store_layersL, &iter, 0, layer_names[i], -1);
		}
	}
	g_signal_connect(G_OBJECT(tree_layers), "key-press-event", G_CALLBACK(layers_backspace_key_clicked_cb), (gpointer) pw);

	//Fourth page: advanced
	superframe = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_set_homogeneous(GTK_BOX(superframe), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(superframe),10);

	label = gtk_label_new("");
	gtk_label_set_markup(GTK_LABEL(label),"<span size=\"large\">Advanced</span>");

	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), superframe, label);


	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	GtkWidget *button = gtk_button_new_from_icon_name("edit-delete", GTK_ICON_SIZE_BUTTON);
	label = gtk_label_new("Remove the solid angles HDF5 file");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_widget_set_tooltip_text(label,"It is recommended to remove this file when a definitive uninstallation of XMI-MSIM is required, or when this file got somehow corrupted.");
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(delete_solid_angles_clicked_cb), (gpointer) window);

	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	button = gtk_button_new_from_icon_name("edit-delete", GTK_ICON_SIZE_BUTTON);
	label = gtk_label_new("Remove the escape ratios HDF5 file");
	gtk_widget_set_tooltip_text(label,"It is recommended to remove this file when a definitive uninstallation of XMI-MSIM is required, or when this file got somehow corrupted.");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(delete_escape_ratios_clicked_cb), (gpointer) window);

	gtk_box_pack_start(GTK_BOX(superframe), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 3);

	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	label = gtk_label_new("Import solid angle grids");
	gtk_widget_set_tooltip_text(label,"Use this feature to import solid angle grids into your personal default HDF5 file.");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(import_solid_angles_clicked_cb), (gpointer) window);

	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	button = gtk_button_new_from_icon_name("document-open", GTK_ICON_SIZE_BUTTON);
	label = gtk_label_new("Import escape ratios");
	gtk_widget_set_tooltip_text(label,"Use this feature to import detector specific escape ratios into your personal default HDF5 file.");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 1);
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 1);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(import_escape_ratios_clicked_cb), (gpointer) window);

	gtk_box_pack_start(GTK_BOX(superframe), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 3);
	pw->notificationsW = gtk_check_button_new_with_label("Enable notifications");
	gtk_widget_set_tooltip_text(pw->notificationsW,"Check this button to enable notifications support");

	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(pw->notificationsW), g_value_get_boolean(&xpv));
	g_value_unset(&xpv);
	gtk_box_pack_start(GTK_BOX(superframe), pw->notificationsW, FALSE, FALSE, 3);

	gtk_box_pack_start(GTK_BOX(superframe), gtk_separator_new(GTK_ORIENTATION_HORIZONTAL), FALSE, FALSE, 3);
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_set_homogeneous(GTK_BOX(hbox), FALSE);
	label = gtk_label_new("Default save folder");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	pw->default_save_folderW = gtk_file_chooser_button_new("Select the default folder to save new files", GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_widget_set_tooltip_text(pw->default_save_folderW, "Default folder for saving new files");
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_DEFAULT_SAVE_FOLDER, &xpv) == 0) {
		//abort
		preferences_error_handler(NULL);
	}
	gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(pw->default_save_folderW), g_value_get_string(&xpv));
	g_value_unset(&xpv);
	gtk_box_pack_start(GTK_BOX(hbox), pw->default_save_folderW, TRUE, TRUE, 3);
	gtk_box_pack_start(GTK_BOX(superframe), hbox, FALSE, FALSE, 3);

	//button box
	buttonbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
	gtk_box_set_homogeneous(GTK_BOX(buttonbox), TRUE);
	GtkWidget *applyButton, *cancelButton;

	applyButton = gtk_button_new_with_mnemonic("_Apply");
	cancelButton = gtk_button_new_with_mnemonic("_Cancel");
	gtk_box_pack_start(GTK_BOX(buttonbox), applyButton, TRUE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(buttonbox), cancelButton, TRUE,FALSE,2);
	gtk_box_pack_start(GTK_BOX(master_box),buttonbox, FALSE, FALSE, 6);
	g_signal_connect(G_OBJECT(cancelButton), "clicked", G_CALLBACK(preferences_cancel_button_clicked), (gpointer) window);
	g_signal_connect(G_OBJECT(applyButton), "clicked", G_CALLBACK(preferences_apply_button_clicked), (gpointer) pw);

	gtk_widget_show_all(master_box);

	return window;
}

void xmimsim_gui_launch_preferences(GtkWindow *parent) {

	if (preferences_dialog == NULL) {
		preferences_dialog = create_dialog();
		gtk_window_set_application(GTK_WINDOW(preferences_dialog), GTK_APPLICATION(g_application_get_default()));
		g_signal_connect(preferences_dialog, "destroy", G_CALLBACK(gtk_widget_destroyed), &preferences_dialog);
		gtk_window_set_destroy_with_parent(GTK_WINDOW(preferences_dialog), TRUE);
	}

	if (parent != gtk_window_get_transient_for(GTK_WINDOW(preferences_dialog))) {
		gtk_window_set_transient_for(GTK_WINDOW(preferences_dialog), parent);
	}

	gtk_window_present(GTK_WINDOW(preferences_dialog));
}


char *xmimsim_gui_get_preferences_filename(void) {

#ifdef MAC_INTEGRATION
        const gchar *config_dir = xmi_resources_mac_get_user_data_dir();
#else
        const gchar *config_dir = g_get_user_config_dir();
#endif

	//first check if the preferences file exists!
	gchar *prefs_dir = g_strdup_printf("%s" G_DIR_SEPARATOR_S "XMI-MSIM",config_dir);
	gchar *prefs_file = g_strdup_printf("%s" G_DIR_SEPARATOR_S "preferences.ini",prefs_dir);
	g_free(prefs_dir);

	return prefs_file;
}
