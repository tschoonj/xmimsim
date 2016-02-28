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

#ifndef XMI_MSIM_GUI_COMPOUND_DIALOG_H
#define XMI_MSIM_GUI_COMPOUND_DIALOG_H

#define XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG                  (xmi_msim_gui_compound_dialog_get_type ())
#define XMI_MSIM_GUI_COMPOUND_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG, XmiMsimGuiCompoundDialog))
#define XMI_MSIM_GUI_COMPOUND_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG, XmiMsimGuiCompoundDialogClass))
#define XMI_MSIM_GUI_IS_COMPOUND_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG))
#define XMI_MSIM_GUI_IS_COMPOUND_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG))
#define XMI_MSIM_GUI_COMPOUND_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG, XmiMsimGuiCompoundDialogClass))

typedef struct _XmiMsimGuiCompoundDialog        XmiMsimGuiCompoundDialog;
typedef struct _XmiMsimGuiCompoundDialogClass   XmiMsimGuiCompoundDialogClass;

typedef enum {
  XMI_MSIM_GUI_COMPOUND_DIALOG_ADD,
  XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT
} XmiMsimGuiCompoundDialogType;

struct _XmiMsimGuiCompoundDialog
{ 
  GtkDialog parent_instance;
  GtkWidget *compoundEntry;
  GtkWidget *weightEntry;
  guint compound_dialog_type;
};

struct _XmiMsimGuiCompoundDialogClass
{
  GtkDialogClass parent_class;

};

GType xmi_msim_gui_compound_dialog_get_type(void) G_GNUC_CONST;

GtkWidget *xmi_msim_gui_compound_dialog_new(GtkWindow *parent, XmiMsimGuiCompoundDialogType type);

void xmi_msim_gui_compound_dialog_set_compound(XmiMsimGuiCompoundDialog *dialog, const gchar *compound);
gchar *xmi_msim_gui_compound_dialog_get_compound(XmiMsimGuiCompoundDialog *dialog);

void xmi_msim_gui_compound_dialog_set_weight(XmiMsimGuiCompoundDialog *dialog, gdouble weight);
gdouble xmi_msim_gui_compound_dialog_get_weight(XmiMsimGuiCompoundDialog *dialog);

#endif
