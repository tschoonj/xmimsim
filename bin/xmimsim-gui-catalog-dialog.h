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


#ifndef XMI_MSIM_GUI_CATALOG_DIALOG_H
#define XMI_MSIM_GUI_CATALOG_DIALOG_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_CATALOG_DIALOG                  (xmi_msim_gui_catalog_dialog_get_type ())
#define XMI_MSIM_GUI_CATALOG_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_CATALOG_DIALOG, XmiMsimGuiCatalogDialog))
#define XMI_MSIM_GUI_CATALOG_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_CATALOG_DIALOG, XmiMsimGuiCatalogDialogClass))
#define XMI_MSIM_GUI_IS_CATALOG_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_CATALOG_DIALOG))
#define XMI_MSIM_GUI_IS_CATALOG_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_CATALOG_DIALOG))
#define XMI_MSIM_GUI_CATALOG_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_CATALOG_DIALOG, XmiMsimGuiCatalogDialogClass))

typedef struct _XmiMsimGuiCatalogDialog        XmiMsimGuiCatalogDialog;
typedef struct _XmiMsimGuiCatalogDialogClass   XmiMsimGuiCatalogDialogClass;

struct _XmiMsimGuiCatalogDialog
{
  GtkDialog parent_instance;
  GtkWidget *nist_radioW;
  GtkWidget *user_radioW;
  GtkWidget *nist_comboW;
  GtkWidget *user_comboW;
};

struct _XmiMsimGuiCatalogDialogClass
{
  GtkDialogClass parent_class;
};

GType xmi_msim_gui_catalog_dialog_get_type(void) G_GNUC_CONST;

GtkWidget *xmi_msim_gui_catalog_dialog_new(GtkWindow *parent);

struct xmi_layer* xmi_msim_gui_catalog_dialog_get_layer(XmiMsimGuiCatalogDialog *dialog);

G_END_DECLS

#endif

