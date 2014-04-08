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

#include <config.h>
#include "xmimsim-gui.h"
#include "xmimsim-gui-notifications.h"
#include "xmimsim-gui-prefs.h"
#include <glib/gprintf.h>
#include <glib/gstdio.h>

#ifdef MAC_INTEGRATION
   #import <Foundation/Foundation.h>
#elif defined(HAVE_LIBNOTIFY)
   #include <libnotify/notify.h>
#endif


int xmimsim_notifications_init(void) {
#ifdef MAC_INTEGRATION
	//no initialization necessary here
#elif defined(HAVE_LIBNOTIFY)
	notify_init("XMI-MSIM");
	if (notify_is_initted()) {
		char *ret_name, *ret_vendor;
		if (notify_get_server_info(&ret_name, &ret_vendor, NULL, NULL)) {
			g_fprintf(stdout,"libnotify server name: %s\n", ret_name);
			g_fprintf(stdout,"libnotify server vendor: %s\n", ret_vendor);
			g_free(ret_name);
			g_free(ret_vendor);
		}
		else {
			g_fprintf(stderr, "Could not get notifications server information\n");
		}
	}
	else {
		g_fprintf(stderr, "Could not initiatilize libnotify\n");
	}
#else

#endif
	return 1;
}


int xmimsim_notifications_deliver(char *title, char *text) {
#if defined(MAC_INTEGRATION) || defined(HAVE_LIBNOTIFY)
	union xmimsim_prefs_val xpv;
	if (xmimsim_gui_get_prefs(XMIMSIM_GUI_PREFS_NOTIFICATIONS, &xpv) == 0) {
		//abort 
		return 0;
	}
	if (xpv.b == FALSE) {
		return 1;
	}
#endif

#ifdef MAC_INTEGRATION
//  #if MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_8
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];
  	Class cls = NSClassFromString(@"NSUserNotificationCenter");
  	if (cls) {
		NSUserNotification *notification = [[NSUserNotification alloc] init];
 		[notification setTitle:[NSString stringWithUTF8String:title]];
		[notification setInformativeText:[NSString stringWithUTF8String:text]];
		[notification setSoundName:NSUserNotificationDefaultSoundName];
		NSUserNotificationCenter *center = [NSUserNotificationCenter defaultUserNotificationCenter];
		[center deliverNotification:notification];
  	}
	[pool drain];
//  #endif
#elif defined(HAVE_LIBNOTIFY)
	if (notify_is_initted()) {
		NotifyNotification *notification = notify_notification_new(title, text, XMI_STOCK_LOGO);
		notify_notification_set_urgency(notification, NOTIFY_URGENCY_NORMAL);
		notify_notification_set_timeout(notification, NOTIFY_EXPIRES_DEFAULT);
		GError *error = NULL;
		if(!notify_notification_show(notification, &error)) {
			g_fprintf(stderr,"notification show error message: %s\n", error->message);
			g_error_free(error); 	
		}
		g_object_unref(G_OBJECT(notification));
	}
#else

#endif
	return 1;
}

int xmimsim_notifications_close(void) {
#ifdef MAC_INTEGRATION
	//no closing necessary here
#elif defined(HAVE_LIBNOTIFY)
	if (notify_is_initted())
		notify_uninit();
#else

#endif
	return 1;
}

