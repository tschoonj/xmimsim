/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_MSIM_GUI_SOURCES_DIALOG_H
#define XMI_MSIM_GUI_SOURCES_DIALOG_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_SOURCES_DIALOG                  (xmi_msim_gui_sources_dialog_get_type ())
#define XMI_MSIM_GUI_SOURCES_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_SOURCES_DIALOG, XmiMsimGuiSourcesDialog))
#define XMI_MSIM_GUI_SOURCES_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_SOURCES_DIALOG, XmiMsimGuiSourcesDialogClass))
#define XMI_MSIM_GUI_IS_SOURCES_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_SOURCES_DIALOG))
#define XMI_MSIM_GUI_IS_SOURCES_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_SOURCES_DIALOG))
#define XMI_MSIM_GUI_SOURCES_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_SOURCES_DIALOG, XmiMsimGuiSourcesDialogClass))

typedef struct _XmiMsimGuiSourcesDialog        XmiMsimGuiSourcesDialog;
typedef struct _XmiMsimGuiSourcesDialogClass   XmiMsimGuiSourcesDialogClass;

GType xmi_msim_gui_sources_dialog_get_type(void) G_GNUC_CONST;

GtkWidget *xmi_msim_gui_sources_dialog_new(GtkWindow *parent, xmi_input *current);

// returns newly allocated currently available xmi_excitation data
xmi_excitation *xmi_msim_gui_sources_dialog_get_raw_data(XmiMsimGuiSourcesDialog *dialog);

const gchar* xmi_msim_gui_sources_dialog_get_active_source_name(XmiMsimGuiSourcesDialog *dialog);

GtkNotebook* xmi_msim_gui_sources_dialog_get_notebook(XmiMsimGuiSourcesDialog *dialog);

G_END_DECLS
#endif


