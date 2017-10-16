/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

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


#include <gtk/gtk.h>
#include "xmi_main.h"

#ifndef XMI_MSIM_GUI_OPTIONS_BOX_H
#define XMI_MSIM_GUI_OPTIONS_BOX_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_OPTIONS_BOX 		       (xmi_msim_gui_options_box_get_type())
#define XMI_MSIM_GUI_OPTIONS_BOX(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_OPTIONS_BOX, XmiMsimGuiOptionsBox))
#define XMI_MSIM_GUI_OPTIONS_BOX_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_OPTIONS_BOX, XmiMsimGuiOptionsBoxClass))
#define XMI_MSIM_GUI_IS_OPTIONS_BOX(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_OPTIONS_BOX))
#define XMI_MSIM_GUI_IS_OPTIONS_BOX_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_OPTIONS_BOX))
#define XMI_MSIM_GUI_OPTIONS_BOX_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_OPTIONS_BOX, XmiMsimGuiOptionsBoxClass))

typedef struct _XmiMsimGuiOptionsBox		XmiMsimGuiOptionsBox;
typedef struct _XmiMsimGuiOptionsBoxClass   	XmiMsimGuiOptionsBoxClass;

GtkWidget* xmi_msim_gui_options_box_new(GtkWidget *parent_window);

GType xmi_msim_gui_options_box_get_type(void) G_GNUC_CONST;

struct xmi_main_options* xmi_msim_gui_options_box_get_options(XmiMsimGuiOptionsBox *options_box);

void xmi_msim_gui_options_box_save_to_prefs(XmiMsimGuiOptionsBox *options_box);

G_END_DECLS

#endif

