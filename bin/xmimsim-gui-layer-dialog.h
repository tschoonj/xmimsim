/*
Copyright (C) 2016 Tom Schoonjans and Laszlo Vincze

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
#include "xmi_data_structs.h"

#ifndef XMI_MSIM_GUI_LAYER_DIALOG_H
#define XMI_MSIM_GUI_LAYER_DIALOG_H

#define XMI_MSIM_GUI_TYPE_LAYER_DIALOG                  (xmi_msim_gui_layer_dialog_get_type ())
#define XMI_MSIM_GUI_LAYER_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_LAYER_DIALOG, XmiMsimGuiLayerDialog))
#define XMI_MSIM_GUI_LAYER_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_LAYER_DIALOG, XmiMsimGuiLayerDialogClass))
#define XMI_MSIM_GUI_IS_LAYER_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_LAYER_DIALOG))
#define XMI_MSIM_GUI_IS_LAYER_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_LAYER_DIALOG))
#define XMI_MSIM_GUI_LAYER_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_LAYER_DIALOG, XmiMsimGuiLayerDialogClass))

typedef struct _XmiMsimGuiLayerDialog        XmiMsimGuiLayerDialog;
typedef struct _XmiMsimGuiLayerDialogClass   XmiMsimGuiLayerDialogClass;

typedef enum {
  XMI_MSIM_GUI_LAYER_DIALOG_ADD,
  XMI_MSIM_GUI_LAYER_DIALOG_EDIT
} XmiMsimGuiLayerDialogType;

struct _XmiMsimGuiLayerDialog
{
  GtkDialog parent_instance;
  guint layer_dialog_type;

  GtkWidget *addButton;
  GtkWidget *editButton;
  GtkWidget *removeButton;
  GtkWidget *predefButton;
  GtkWidget *addToCatalogButton;
  GtkWidget *normalizeButton;

  GtkWidget *sumLabel;
  GtkWidget *densityEntry;
  GtkWidget *thicknessEntry;

  GtkWidget *compositionTreeView;

  gulong density_changed;
  gulong thickness_changed;
};

struct _XmiMsimGuiLayerDialogClass
{
  GtkDialogClass parent_class;

};

GType xmi_msim_gui_layer_dialog_get_type(void) G_GNUC_CONST;

GtkWidget *xmi_msim_gui_layer_dialog_new(GtkWindow *parent, XmiMsimGuiLayerDialogType type);

void xmi_msim_gui_layer_dialog_set_layer(XmiMsimGuiLayerDialog *dialog, struct xmi_layer *layer);
struct xmi_layer* xmi_msim_gui_layer_dialog_get_layer(XmiMsimGuiLayerDialog *dialog);
#endif

