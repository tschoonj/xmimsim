/*
Copyright (C) 2019 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_GUI_BATCH_CONTROLS_BOX_H
#define XMI_MSIM_GUI_BATCH_CONTROLS_BOX_H

#include "xmi_batch.h"
#include <gtk/gtk.h>

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX 		      (xmi_msim_gui_batch_controls_box_get_type())
#define XMI_MSIM_GUI_BATCH_CONTROLS_BOX(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX, XmiMsimGuiBatchControlsBox))
#define XMI_MSIM_GUI_BATCH_CONTROLS_BOX_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX, XmiMsimGuiBatchControlsBoxClass))
#define XMI_MSIM_GUI_IS_BATCH_CONTROLS_BOX(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX))
#define XMI_MSIM_GUI_IS_BATCH_CONTROLS_BOX_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX))
#define XMI_MSIM_GUI_BATCH_CONTROLS_BOX_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_BATCH_CONTROLS_BOX, XmiMsimGuiBatchControlsBoxClass))

typedef struct _XmiMsimGuiBatchControlsBox		XmiMsimGuiBatchControlsBox;
typedef struct _XmiMsimGuiBatchControlsBoxClass   	XmiMsimGuiBatchControlsBoxClass;

GtkWidget* xmi_msim_gui_batch_controls_box_new(void);

void xmi_msim_gui_batch_controls_box_set_batch_data(XmiMsimGuiBatchControlsBox *self, XmiMsimBatchAbstract *batch_data);

GType xmi_msim_gui_batch_controls_box_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif

