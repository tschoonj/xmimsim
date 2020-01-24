#import <config.h>
#import "xmimsim-gui-osx.h"
#import <ApplicationServices/ApplicationServices.h>
#import <AvailabilityMacros.h>
#import <gdk/gdkquartz.h>

#if !defined(MAC_OS_X_VERSION_10_12) || MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12

@interface NSWindow(AutomaticWindowTabbing)
+ (void)setAllowsAutomaticWindowTabbing:(BOOL)allow;
@end

#endif

#ifdef MAC_INTEGRATION

@interface _XmiMsimGuiOSXApplicationDelegate : NSObject <NSApplicationDelegate> {
}
@end

@implementation _XmiMsimGuiOSXApplicationDelegate
-(BOOL) application: (NSApplication*) sender openFile: (NSString *) file {
	gchar *utf8_path =  g_strdup([file UTF8String]);
	g_debug("Calling openFile for %s", utf8_path);
	GFile *gfile = g_file_new_for_path(utf8_path);
	g_application_open(g_application_get_default(), &gfile, 1, "");
	g_object_unref(gfile);
	g_free(utf8_path);

	return YES;
}

@end
XmiMsimGuiOSXApplicationDelegate* xmi_msim_gui_osx_app_delegate_new(void) {
	g_debug("Calling xmi_msim_gui_osx_app_delegate_new");
	[NSApp setDelegate: [_XmiMsimGuiOSXApplicationDelegate new]];
	return [NSApp delegate];
}

void xmi_msim_gui_osx_app_delegate_free(XmiMsimGuiOSXApplicationDelegate *delegate) {
	if (delegate)
		[(id) delegate release];
}

#else

XmiMsimGuiOSXApplicationDelegate* xmi_msim_gui_osx_app_delegate_new(void) {
	return NULL;
}

void xmi_msim_gui_osx_app_delegate_free(XmiMsimGuiOSXApplicationDelegate *delegate) {
}

#endif

void xmi_msim_gui_osx_app_disable_tabbing(void) {

	if ([NSWindow respondsToSelector:@selector(setAllowsAutomaticWindowTabbing:)]) {
		[NSWindow setAllowsAutomaticWindowTabbing:NO];
	}
	
}

void xmi_msim_gui_osx_window_set_file(GtkWidget *window, const gchar *filename) {
	g_return_if_fail(window != NULL);
	GdkWindow *dwindow = gtk_widget_get_window(window);
	g_return_if_fail(dwindow != NULL); // this happens sometimes for some reason...

	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	NSWindow *qwindow = gdk_quartz_window_get_nswindow(dwindow);
	if (filename != NULL && g_path_is_absolute(filename)) {
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

void xmi_msim_gui_osx_window_bring_to_front(GtkWidget *window) {
	NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(window));

	//bring window to front if necessary
	if ([NSApp isHidden] == YES)
		[NSApp unhide: nil];
	else if ([qwindow isMiniaturized])
		[qwindow deminiaturize:nil];
}

void xmi_msim_gui_osx_window_enable_full_screen(GtkWidget *window) {
	//only works in Lion and newer
	NSWindow *qwindow = gdk_quartz_window_get_nswindow(gtk_widget_get_window(window));
	[qwindow setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
}