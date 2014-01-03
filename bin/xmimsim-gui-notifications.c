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
#include "xmimsim-gui-notifications.h"
#include "xmimsim-gui-prefs.h"

#ifdef MAC_INTEGRATION
   #import <Foundation/Foundation.h>
#endif


int xmimsim_notifications_init(void) {
#ifdef MAC_INTEGRATION
	//no initialization necessary here
#else

#endif
	return 1;
}


int xmimsim_notifications_deliver(char *title, char *text) {
#if defined(MAC_INTEGRATION)
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
#else

#endif
	return 1;
}

int xmimsim_notifications_close(void) {
#ifdef MAC_INTEGRATION
	//no closing necessary here
#else

#endif
	return 1;
}

