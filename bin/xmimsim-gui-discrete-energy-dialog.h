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

#ifndef XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_H
#define XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_H

#include <gtk/gtk.h>
#include "xmi_data_structs.h"

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG                  (xmi_msim_gui_discrete_energy_dialog_get_type ())
#define XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG, XmiMsimGuiDiscreteEnergyDialog))
#define XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG, XmiMsimGuiDiscreteEnergyDialogClass))
#define XMI_MSIM_GUI_IS_DISCRETE_ENERGY_DIALOG(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG))
#define XMI_MSIM_GUI_IS_DISCRETE_ENERGY_DIALOG_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG))
#define XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG, XmiMsimGuiDiscreteEnergyDialogClass))

typedef struct _XmiMsimGuiDiscreteEnergyDialog        XmiMsimGuiDiscreteEnergyDialog;
typedef struct _XmiMsimGuiDiscreteEnergyDialogClass   XmiMsimGuiDiscreteEnergyDialogClass;

typedef enum {
  XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_TYPE_ADD,
  XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_TYPE_EDIT
} XmiMsimGuiDiscreteEnergyDialogType;

struct _XmiMsimGuiDiscreteEnergyDialog
{
  GtkDialog parent_instance;
  guint discrete_energy_dialog_type;

  GtkWidget *energy_entry;
  GtkWidget *hor_intensity_entry;
  GtkWidget *ver_intensity_entry;
  GtkWidget *sigma_x_entry;
  GtkWidget *sigma_y_entry;
  GtkWidget *sigma_xp_entry;
  GtkWidget *sigma_yp_entry;
  GtkWidget *distribution_type_combo;
  GtkWidget *scale_parameter_entry;
  GtkWidget *scale_parameter_label;
  GtkWidget *scale_parameter_box;

  gulong energy_changed;
  gulong hor_intensity_changed;
  gulong ver_intensity_changed;
  gulong sigma_x_changed;
  gulong sigma_y_changed;
  gulong sigma_xp_changed;
  gulong sigma_yp_changed;
  gulong distribution_type_changed;
  gulong scale_parameter_changed;
};

struct _XmiMsimGuiDiscreteEnergyDialogClass
{
  GtkDialogClass parent_class;
};

GType xmi_msim_gui_discrete_energy_dialog_get_type(void) G_GNUC_CONST;

GtkWidget *xmi_msim_gui_discrete_energy_dialog_new(GtkWindow *parent, XmiMsimGuiDiscreteEnergyDialogType type);

void xmi_msim_gui_discrete_energy_dialog_set_discrete_energy(XmiMsimGuiDiscreteEnergyDialog *dialog, const xmi_energy_discrete *discrete_energy);
xmi_energy_discrete* xmi_msim_gui_discrete_energy_dialog_get_discrete_energy(XmiMsimGuiDiscreteEnergyDialog *dialog);

G_END_DECLS
#endif

