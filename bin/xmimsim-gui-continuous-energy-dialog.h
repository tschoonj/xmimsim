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

#ifndef XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_H
#define XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG                  (xmi_msim_gui_continuous_energy_dialog_get_type ())
#define XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG, XmiMsimGuiContinuousEnergyDialog))
#define XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG, XmiMsimGuiContinuousEnergyDialogClass))
#define XMI_MSIM_GUI_IS_CONTINUOUS_ENERGY_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG))
#define XMI_MSIM_GUI_IS_CONTINUOUS_ENERGY_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG))
#define XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG, XmiMsimGuiContinuousEnergyDialogClass))

typedef struct _XmiMsimGuiContinuousEnergyDialog        XmiMsimGuiContinuousEnergyDialog;
typedef struct _XmiMsimGuiContinuousEnergyDialogClass   XmiMsimGuiContinuousEnergyDialogClass;

typedef enum {
  XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_ADD,
  XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_EDIT
} XmiMsimGuiContinuousEnergyDialogType;

struct _XmiMsimGuiContinuousEnergyDialog
{
  GtkDialog parent_instance;
  guint continuous_energy_dialog_type;

  GtkWidget *energy_entry;
  GtkWidget *hor_intensity_entry;
  GtkWidget *ver_intensity_entry;
  GtkWidget *sigma_x_entry;
  GtkWidget *sigma_y_entry;
  GtkWidget *sigma_xp_entry;
  GtkWidget *sigma_yp_entry;

  gulong energy_changed;
  gulong hor_intensity_changed;
  gulong ver_intensity_changed;
  gulong sigma_x_changed;
  gulong sigma_y_changed;
  gulong sigma_xp_changed;
  gulong sigma_yp_changed;
};

struct _XmiMsimGuiContinuousEnergyDialogClass
{
  GtkDialogClass parent_class;
};

GType xmi_msim_gui_continuous_energy_dialog_get_type(void) G_GNUC_CONST;

GtkWidget *xmi_msim_gui_continuous_energy_dialog_new(GtkWindow *parent, XmiMsimGuiContinuousEnergyDialogType type);

void xmi_msim_gui_continuous_energy_dialog_set_continuous_energy(XmiMsimGuiContinuousEnergyDialog *dialog, const struct xmi_energy_continuous *continuous_energy);
struct xmi_energy_continuous* xmi_msim_gui_continuous_energy_dialog_get_continuous_energy(XmiMsimGuiContinuousEnergyDialog *dialog);

G_END_DECLS
#endif

