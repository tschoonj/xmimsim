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

#include "xmimsim-gui-prefs.h"
#ifdef MAC_INTEGRATION
        #import <Foundation/Foundation.h>
#endif



int xmimsim_gui_get_prefs(int kind, union xmimsim_prefs_val *prefs) {
	gchar *prefs_file;
	GKeyFile *keyfile;
	gchar *prefs_file_contents;
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
		//file does not exist, is not accessible or is not in key format
		//prepare file with default values
		g_key_file_set_comment(keyfile, NULL, NULL, "Preferences file for XMI-MSIM",NULL);
		g_key_file_set_comment(keyfile, NULL, NULL, "This file should be modified through the preferences interface",NULL);
		g_key_file_set_comment(keyfile, NULL, NULL, "Modify this file at your own risk!",NULL);
		g_key_file_set_boolean(keyfile, "Preferences","Check for updates", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","M lines", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","Pile-up", FALSE);
		//save file
		//create dir first if necessary
		if (g_mkdir_with_parents(prefs_dir, 0755) != 0)
			return 0;
		g_free(prefs_dir);
		prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
		if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
			return 0;
		g_free(prefs_file_contents);	
	}
		
	//extract required information from keyfile
	GError *error = NULL;
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
			}
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
	gchar *prefs_file_contents;
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
		//file does not exist, is not accessible or is not in key format
		//prepare file with default values
		g_key_file_set_comment(keyfile, NULL, NULL, "Preferences file for XMI-MSIM",NULL);
		g_key_file_set_comment(keyfile, NULL, NULL, "This file should be modified through the preferences interface",NULL);
		g_key_file_set_comment(keyfile, NULL, NULL, "Modify this file at your own risk!",NULL);
		g_key_file_set_boolean(keyfile, "Preferences","Check for updates", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","M lines", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","Radiative cascade", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","Non-radiative cascade", TRUE);
		g_key_file_set_boolean(keyfile, "Preferences","Pile-up", FALSE);
		//save file
		//create dir first if necessary
		if (g_mkdir_with_parents(prefs_dir, 0755) != 0)
			return 0;
		g_free(prefs_dir);
		prefs_file_contents = g_key_file_to_data(keyfile, NULL, NULL);
		if(!g_file_set_contents(prefs_file, prefs_file_contents, -1, NULL))
			return 0;
		g_free(prefs_file_contents);	
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
		case XMIMSIM_GUI_PREFS_PILE_UP: 
			g_key_file_set_boolean(keyfile, "Preferences","Pile-up", prefs.b);
			break;
		default:
			fprintf(stderr,"Unknown preference requested in xmimsim_gui_set_prefs\n");
			return 0;
		
	}
	
	//save file
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

