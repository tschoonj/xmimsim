#ifndef XMIMSIM_GUI_OSX_H
#define XMIMSIM_GUI_OSX_H

#include <glib.h>
#include <gtk/gtk.h>

G_BEGIN_DECLS

void xmi_msim_gui_osx_app_disable_tabbing(void);

void xmi_msim_gui_osx_nswindow_set_file(GtkWidget *window, const gchar *filename);

void xmi_msim_gui_osx_app_minimize_all(void);

void xmi_msim_gui_osx_app_bring_to_front(GtkWidget *window);

void xmi_msim_gui_osx_app_enable_full_screen(GtkWidget *window);

// to be removed when migrating to GtkApplication!
void xmi_msim_gui_osx_app_send_notification(const char *title, const char *text);

G_END_DECLS

#endif
