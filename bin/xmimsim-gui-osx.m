
#import "xmimsim-gui-osx.h"
#import <ApplicationServices/ApplicationServices.h>
#import <AvailabilityMacros.h>
#import <gdk/gdkquartz.h>

#if !defined(MAC_OS_X_VERSION_10_12) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12

@interface NSWindow(AutomaticWindowTabbing)
+ (void)setAllowsAutomaticWindowTabbing:(BOOL)allow;
@end

#endif

void xmi_msim_gui_osx_app_disable_tabbing(void) {

	if ([NSWindow respondsToSelector:@selector(setAllowsAutomaticWindowTabbing:)]) {
		[NSWindow setAllowsAutomaticWindowTabbing:NO];
	}
	
}

void xmi_msim_gui_osx_nswindow_set_file(GtkWidget *window, const gchar *filename) {
	g_return_if_fail(window != NULL);
	GdkWindow *dwindow = gtk_widget_get_window(window);
	g_return_if_fail(dwindow != NULL); // this happens sometimes for some reason...

	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	NSWindow *qwindow = gdk_quartz_window_get_nswindow(dwindow);
	if (filename != NULL) {
		gchar *uri = g_filename_to_uri(filename, NULL, NULL);
		NSURL *nsurl = [NSURL URLWithString:[NSString stringWithUTF8String:uri]];
		[qwindow setRepresentedURL:nsurl];
		g_free(uri);
	}
	else {
		[qwindow setRepresentedURL:nil];
	}

	[pool drain];
}

void xmi_msim_gui_osx_app_minimize_all(void) {
	[NSApp miniaturizeAll:nil];
}

void xmi_msim_gui_osx_app_bring_to_front(GtkWidget *window) {
	NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(window));

	//bring window to front if necessary
	if ([NSApp isHidden] == YES)
		[NSApp unhide: nil];
	else if ([qwindow isMiniaturized])
		[qwindow deminiaturize:nil];
}

void xmi_msim_gui_osx_app_enable_full_screen(GtkWidget *window) {
	//only works in Lion and newer
	NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(window));
	[qwindow setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
}

void xmi_msim_gui_osx_app_send_notification(const char *title, const char *text) {
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc]init];

	NSUserNotification *notification = [[NSUserNotification alloc] init];
 	[notification setTitle:[NSString stringWithUTF8String:title]];
	[notification setInformativeText:[NSString stringWithUTF8String:text]];
	[notification setSoundName:NSUserNotificationDefaultSoundName];
	NSUserNotificationCenter *center = [NSUserNotificationCenter defaultUserNotificationCenter];
	[center deliverNotification:notification];

	[pool drain];
}
