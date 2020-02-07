/*
Copyright (C) 2019-2020 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX_H
#define XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX_H

#include "xmimsim-gui-xmsi-selection-scrolled-window.h"
#include "xmi_batch.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX 		      (xmi_msim_gui_batch_archive_settings_box_get_type())
#define XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX, XmiMsimGuiBatchArchiveSettingsBox))
#define XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX, XmiMsimGuiBatchArchiveSettingsBoxClass))
#define XMI_MSIM_GUI_IS_BATCH_ARCHIVE_SETTINGS_BOX(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX))
#define XMI_MSIM_GUI_IS_BATCH_ARCHIVE_SETTINGS_BOX_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX))
#define XMI_MSIM_GUI_BATCH_ARCHIVE_SETTINGS_BOX_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_BATCH_ARCHIVE_SETTINGS_BOX, XmiMsimGuiBatchArchiveSettingsBoxClass))

typedef struct _XmiMsimGuiBatchArchiveSettingsBox		XmiMsimGuiBatchArchiveSettingsBox;
typedef struct _XmiMsimGuiBatchArchiveSettingsBoxClass   	XmiMsimGuiBatchArchiveSettingsBoxClass;

GtkWidget* xmi_msim_gui_batch_archive_settings_box_new(GPtrArray *xpath_expressions, const gchar *input_file);

GPtrArray* xmi_msim_gui_batch_archive_settings_box_get_data(XmiMsimGuiBatchArchiveSettingsBox* self);

gchar* xmi_msim_gui_batch_archive_settings_box_get_archive_name(XmiMsimGuiBatchArchiveSettingsBox *self); 
GType xmi_msim_gui_batch_archive_settings_box_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif

