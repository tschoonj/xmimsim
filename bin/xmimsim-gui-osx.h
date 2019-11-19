#ifndef XMIMSIM_GUI_OSX_H
#define XMIMSIM_GUI_OSX_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

void xmi_msim_gui_osx_app_disable_tabbing(void);

void xmi_msim_gui_osx_window_set_file(GtkWidget *window, const gchar *filename);

void xmi_msim_gui_osx_window_bring_to_front(GtkWidget *window);

void xmi_msim_gui_osx_window_enable_full_screen(GtkWidget *window);

typedef void XmiMsimGuiOSXApplicationDelegate;

XmiMsimGuiOSXApplicationDelegate* xmi_msim_gui_osx_app_delegate_new(void);

void xmi_msim_gui_osx_app_delegate_free(XmiMsimGuiOSXApplicationDelegate *delegate);

G_END_DECLS

#endif
