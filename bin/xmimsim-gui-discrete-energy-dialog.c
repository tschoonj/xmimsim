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

#include <config.h>
#include "xmimsim-gui-discrete-energy-dialog.h"
#include "xmimsim-gui-type-builtins.h"
#include <string.h>
#include <stdlib.h>

static void xmi_msim_discrete_energy_dialog_set_property (GObject          *object,
                                                          guint             prop_id,
                                                          const GValue     *value,
                                                          GParamSpec       *pspec);
static void xmi_msim_discrete_energy_dialog_get_property (GObject          *object,
                                                          guint             prop_id,
                                                          GValue           *value,
                                                          GParamSpec       *pspec);

static void distribution_type_combo_changed(GtkComboBox *combobox, XmiMsimGuiDiscreteEnergyDialog *dialog);

static void entry_value_changed(GtkWidget *widget, XmiMsimGuiDiscreteEnergyDialog *dialog);

enum {
  PROP_0,
  PROP_DISCRETE_ENERGY_DIALOG_TYPE
};

G_DEFINE_TYPE(XmiMsimGuiDiscreteEnergyDialog, xmi_msim_gui_discrete_energy_dialog, GTK_TYPE_DIALOG)

//implementation

static void xmi_msim_gui_discrete_energy_dialog_class_init(XmiMsimGuiDiscreteEnergyDialogClass *klass) {
  GObjectClass *gobject_class;
  gobject_class = G_OBJECT_CLASS(klass);

  gobject_class->set_property = xmi_msim_discrete_energy_dialog_set_property;
  gobject_class->get_property = xmi_msim_discrete_energy_dialog_get_property;

  g_object_class_install_property(gobject_class,
    PROP_DISCRETE_ENERGY_DIALOG_TYPE,
    g_param_spec_enum("discrete-energy-dialog-type",
    "Discrete Energy Dialog Type",
    "The type of the discrete energy dialog",
    XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG_TYPE,
    XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_ADD,
    (GParamFlags) (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY)));
}

static void xmi_msim_gui_discrete_energy_dialog_init(XmiMsimGuiDiscreteEnergyDialog *dialog) {
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE); // in order to have the scale parameter show and hide while automatically resizing the dialog
  gtk_dialog_add_buttons(GTK_DIALOG(dialog), "_Ok", GTK_RESPONSE_ACCEPT, "_Cancel", GTK_RESPONSE_REJECT, NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
  gtk_window_set_default_size(GTK_WINDOW(dialog), 420, 300);

  GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  GtkWidget *mainVBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
  gtk_box_set_homogeneous(GTK_BOX(mainVBox), FALSE);
  gtk_container_set_border_width(GTK_CONTAINER(mainVBox), 5);
  gtk_container_add(GTK_CONTAINER(contentArea), mainVBox);

  //Energy
  GtkWidget *HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  GtkWidget *label = gtk_label_new("Energy (keV)");
  GtkWidget *energy_entry = gtk_entry_new();
  gtk_widget_set_name(energy_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(energy_entry), TRUE);
  gulong energy_changed = g_signal_connect(G_OBJECT(energy_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), energy_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //horizontal intensity
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Horizontally polarized intensity (ph/s)");
  GtkWidget *hor_intensity_entry = gtk_entry_new();
  gtk_widget_set_name(hor_intensity_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(hor_intensity_entry), TRUE);
  gulong hor_intensity_changed = g_signal_connect(G_OBJECT(hor_intensity_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), hor_intensity_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //vertical intensity
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Vertically polarized intensity (ph/s)");
  GtkWidget *ver_intensity_entry = gtk_entry_new();
  gtk_widget_set_name(ver_intensity_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(ver_intensity_entry), TRUE);
  gulong ver_intensity_changed = g_signal_connect(G_OBJECT(ver_intensity_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), ver_intensity_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source size x
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Source size x (cm)");
  GtkWidget *sigma_x_entry = gtk_entry_new();
  gtk_widget_set_name(sigma_x_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_x_entry), TRUE);
  gulong sigma_x_changed = g_signal_connect(G_OBJECT(sigma_x_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_x_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source size y
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Source size y (cm)");
  GtkWidget *sigma_y_entry = gtk_entry_new();
  gtk_widget_set_name(sigma_y_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_y_entry), TRUE);
  gulong sigma_y_changed = g_signal_connect(G_OBJECT(sigma_y_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_y_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source divergence x
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Source divergence x (rad)");
  GtkWidget *sigma_xp_entry = gtk_entry_new();
  gtk_widget_set_name(sigma_xp_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_xp_entry), TRUE);
  gulong sigma_xp_changed = g_signal_connect(G_OBJECT(sigma_xp_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_xp_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source divergence y
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Source divergence y (rad)");
  GtkWidget *sigma_yp_entry = gtk_entry_new();
  gtk_widget_set_name(sigma_yp_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_yp_entry), TRUE);
  gulong sigma_yp_changed = g_signal_connect(G_OBJECT(sigma_yp_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_yp_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //distribution type
  HBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(HBox), FALSE);
  label = gtk_label_new("Energy distribution type");
  GtkWidget *distribution_type_combo = gtk_combo_box_text_new();
  gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(distribution_type_combo), "Monochromatic");
  gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(distribution_type_combo), "Gaussian");
  gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(distribution_type_combo), "Lorentzian");
  gulong distribution_type_changed = g_signal_connect(G_OBJECT(distribution_type_combo), "changed", G_CALLBACK(distribution_type_combo_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), distribution_type_combo, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //scale parameter
  GtkWidget *scale_parameter_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 2);
  gtk_box_set_homogeneous(GTK_BOX(scale_parameter_box), FALSE);
  GtkWidget *scale_parameter_label = gtk_label_new("Scale parameter (keV)");
  GtkWidget *scale_parameter_entry = gtk_entry_new();
  gtk_widget_set_name(scale_parameter_entry, "color_entry");
  gtk_entry_set_activates_default(GTK_ENTRY(scale_parameter_entry), TRUE);
  gulong scale_parameter_changed = g_signal_connect(G_OBJECT(scale_parameter_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(scale_parameter_box), scale_parameter_label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(scale_parameter_box), scale_parameter_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), scale_parameter_box, FALSE, FALSE, 3);

  // assign instance variables
  dialog->energy_entry = energy_entry;
  dialog->hor_intensity_entry = hor_intensity_entry;
  dialog->ver_intensity_entry = ver_intensity_entry;
  dialog->sigma_x_entry = sigma_x_entry;
  dialog->sigma_y_entry = sigma_y_entry;
  dialog->sigma_xp_entry = sigma_xp_entry;
  dialog->sigma_yp_entry = sigma_yp_entry;
  dialog->distribution_type_combo = distribution_type_combo;
  dialog->scale_parameter_entry = scale_parameter_entry;
  dialog->scale_parameter_label = scale_parameter_label;
  dialog->scale_parameter_box = scale_parameter_box;

  dialog->energy_changed = energy_changed;
  dialog->hor_intensity_changed = hor_intensity_changed;
  dialog->ver_intensity_changed = ver_intensity_changed;
  dialog->sigma_x_changed = sigma_x_changed;
  dialog->sigma_y_changed = sigma_y_changed;
  dialog->sigma_xp_changed = sigma_xp_changed;
  dialog->sigma_yp_changed = sigma_yp_changed;
  dialog->distribution_type_changed = distribution_type_changed;
  dialog->scale_parameter_changed = scale_parameter_changed;

  gtk_widget_show_all(contentArea);

}

static void xmi_msim_discrete_energy_dialog_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiDiscreteEnergyDialog *dialog = XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(object);

  switch (prop_id) {
    case PROP_DISCRETE_ENERGY_DIALOG_TYPE:
      dialog->discrete_energy_dialog_type = g_value_get_enum(value);
      if (dialog->discrete_energy_dialog_type == XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_ADD) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Enter a new discrete energy");
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
        gtk_combo_box_set_active(GTK_COMBO_BOX(dialog->distribution_type_combo), 0);
      }
      else if (dialog->discrete_energy_dialog_type == XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_EDIT) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Modify a discrete energy");
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
      }
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}

static void xmi_msim_discrete_energy_dialog_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiDiscreteEnergyDialog *dialog = XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG(object);

  switch (prop_id) {
    case PROP_DISCRETE_ENERGY_DIALOG_TYPE:
      dialog->discrete_energy_dialog_type = g_value_get_enum(value);
      g_value_set_enum (value, (XmiMsimGuiDiscreteEnergyDialogType) dialog->discrete_energy_dialog_type);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

GtkWidget *xmi_msim_gui_discrete_energy_dialog_new(GtkWindow *parent, XmiMsimGuiDiscreteEnergyDialogType type) {
  GtkWidget *widget;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  widget = GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG,
                                   "discrete-energy-dialog-type", type,
                                   NULL));

  gtk_window_set_transient_for(GTK_WINDOW(widget),
                               GTK_WINDOW(parent));

  return widget;
}

static void distribution_type_combo_changed(GtkComboBox *combobox, XmiMsimGuiDiscreteEnergyDialog *dialog) {

  gint active = gtk_combo_box_get_active(combobox);

  GtkStyleContext *style_context = gtk_widget_get_style_context(dialog->scale_parameter_entry);

  g_signal_handler_block(G_OBJECT(dialog->scale_parameter_entry), dialog->scale_parameter_changed);
  gtk_style_context_remove_class(style_context, "red");
  gtk_entry_set_text(GTK_ENTRY(dialog->scale_parameter_entry), "");
  g_signal_handler_unblock(G_OBJECT(dialog->scale_parameter_entry), dialog->scale_parameter_changed);

  if (active == XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC) {
    gtk_widget_hide(dialog->scale_parameter_box);
  }
  else if (active == XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN){
    gtk_widget_set_sensitive(dialog->scale_parameter_entry, TRUE);
    gtk_label_set_text(GTK_LABEL(dialog->scale_parameter_label), "Standard deviation (keV)");
    gtk_widget_show_all(dialog->scale_parameter_box);
  }
  else if (active == XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN){
    gtk_widget_set_sensitive(dialog->scale_parameter_entry, TRUE);
    gtk_label_set_text(GTK_LABEL(dialog->scale_parameter_label),"Scale parameter (keV)");
    gtk_widget_show_all(dialog->scale_parameter_box);
  }

  entry_value_changed(NULL, dialog);
}

static void entry_value_changed(GtkWidget *widget, XmiMsimGuiDiscreteEnergyDialog *dialog) {
  const char *textPtr1, *textPtr2, *textPtr3, *textPtr4, *textPtr5, *textPtr6, *textPtr7, *textPtr8;
  char *endPtr1, *endPtr2, *endPtr3, *endPtr4, *endPtr5, *endPtr6, *endPtr7, *endPtr8;
  char *lastPtr1, *lastPtr4, *lastPtr5, *lastPtr6, *lastPtr7, *lastPtr8;

  int ok1, ok2, ok3, ok4, ok5, ok6, ok7, ok8;
  double value1, value2, value3, value4, value5, value6, value7, value8;

  GtkStyleContext *style_context = NULL;
  if (widget != NULL)
    style_context = gtk_widget_get_style_context(GTK_WIDGET(widget));

  textPtr1 = gtk_entry_get_text(GTK_ENTRY(dialog->energy_entry));
  textPtr2 = gtk_entry_get_text(GTK_ENTRY(dialog->hor_intensity_entry));
  textPtr3 = gtk_entry_get_text(GTK_ENTRY(dialog->ver_intensity_entry));
  textPtr4 = gtk_entry_get_text(GTK_ENTRY(dialog->sigma_x_entry));
  textPtr5 = gtk_entry_get_text(GTK_ENTRY(dialog->sigma_y_entry));
  textPtr6 = gtk_entry_get_text(GTK_ENTRY(dialog->sigma_xp_entry));
  textPtr7 = gtk_entry_get_text(GTK_ENTRY(dialog->sigma_yp_entry));

  if(gtk_combo_box_get_active(GTK_COMBO_BOX(dialog->distribution_type_combo)) != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC) {
    textPtr8 = gtk_entry_get_text(GTK_ENTRY(dialog->scale_parameter_entry));
  }


#define energy_short1(n,my_entry) value ## n = g_ascii_strtod(textPtr ## n, &endPtr ## n);\
  lastPtr ## n = (char *) textPtr ## n + strlen(textPtr ## n);\
  if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n > 0.0) \
    ok ## n = 1;\
  else\
    ok ## n = 0;\
  if (widget == my_entry) {\
    if (ok ## n)\
      gtk_style_context_remove_class(style_context, "red"); \
    else {\
      gtk_style_context_add_class(style_context, "red"); \
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);\
    }\
  }

#define energy_short2(n,my_entry) value ## n = g_ascii_strtod(textPtr ## n, &endPtr ## n);\
  lastPtr ## n = (char *) textPtr ## n + strlen(textPtr ## n);\
  if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n >= 0.0) \
    ok ## n = 1;\
  else\
    ok ## n = 0;\
  if (widget == my_entry) {\
    if (ok ## n)\
      gtk_style_context_remove_class(style_context, "red"); \
    else {\
      gtk_style_context_add_class(style_context, "red"); \
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);\
    }\
  }

#define energy_short3(n,my_entry) value ## n = g_ascii_strtod(textPtr ## n, &endPtr ## n);\
  lastPtr ## n = (char *) textPtr ## n + strlen(textPtr ## n);\
  if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n > 0.0 && value ## n <= 200.0) \
    ok ## n = 1;\
  else\
    ok ## n = 0;\
  if (widget == my_entry) {\
    if (ok ## n)\
      gtk_style_context_remove_class(style_context, "red"); \
    else {\
      gtk_style_context_add_class(style_context, "red"); \
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);\
    }\
  }

  energy_short3(1, dialog->energy_entry)
  energy_short2(4, dialog->sigma_x_entry)
  energy_short2(5, dialog->sigma_y_entry)
  energy_short2(6, dialog->sigma_xp_entry)
  energy_short2(7, dialog->sigma_yp_entry)

  if(gtk_combo_box_get_active(GTK_COMBO_BOX(dialog->distribution_type_combo)) != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC) {
    energy_short1(8, dialog->scale_parameter_entry)
  }
  else {
    ok8 = 1;
  }

  // investigate the intensities
  value2 = g_ascii_strtod(textPtr2, &endPtr2);
  value3 = g_ascii_strtod(textPtr3, &endPtr3);

  if (textPtr2 + strlen(textPtr2) == endPtr2 && value2 > 0.0)
    ok2 = 2; // proper strictly positive value
  else if (textPtr2 + strlen(textPtr2) == endPtr2 && strlen(textPtr2) > 0 && value2 == 0.0)
    ok2 = 1; // proper 0
  else if (strlen(textPtr2) == 0)
    ok2 = -1; // empty
  else
    ok2 = 0; // bad

  if (textPtr3 + strlen(textPtr3) == endPtr3 && value3 > 0.0)
    ok3 = 2; // proper strictly positive value
  else if (textPtr3 + strlen(textPtr3) == endPtr3 && strlen(textPtr3) > 0 && value3 == 0.0)
    ok3 = 1; // proper 0
  else if (strlen(textPtr3) == 0)
    ok3 = -1; // empty
  else
    ok3 = 0; // bad


  if (abs(ok2) > 0)
    gtk_style_context_remove_class(gtk_widget_get_style_context(dialog->hor_intensity_entry), "red");
  else 
    gtk_style_context_add_class(gtk_widget_get_style_context(dialog->hor_intensity_entry), "red");

  if (abs(ok3) > 0)
    gtk_style_context_remove_class(gtk_widget_get_style_context(dialog->ver_intensity_entry), "red");
  else
    gtk_style_context_add_class(gtk_widget_get_style_context(dialog->ver_intensity_entry), "red");

  if ((ok2 == 2 && ok3 == 1) || (ok2 == 1 && ok3 == 2)) {
    gtk_style_context_remove_class(gtk_widget_get_style_context(dialog->hor_intensity_entry), "red");
    gtk_style_context_remove_class(gtk_widget_get_style_context(dialog->ver_intensity_entry), "red");
  }
  else if ((ok2 == 1 && ok3 == 1)) {
    ok2 = ok3 = 0;
    gtk_style_context_add_class(gtk_widget_get_style_context(dialog->hor_intensity_entry), "red");
    gtk_style_context_add_class(gtk_widget_get_style_context(dialog->ver_intensity_entry), "red");
  }

  if (ok2 < 0)
    ok2 = 0;

  if (ok3 < 0)
    ok3 = 0;

  if (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 && ok8)
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
  else
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);

  return;

}

void xmi_msim_gui_discrete_energy_dialog_set_discrete_energy(XmiMsimGuiDiscreteEnergyDialog *dialog, const xmi_energy_discrete *discrete_energy) {
  gchar *buffer = g_strdup_printf("%g", discrete_energy->energy);
  gtk_entry_set_text(GTK_ENTRY(dialog->energy_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", discrete_energy->horizontal_intensity);
  gtk_entry_set_text(GTK_ENTRY(dialog->hor_intensity_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", discrete_energy->vertical_intensity);
  gtk_entry_set_text(GTK_ENTRY(dialog->ver_intensity_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", discrete_energy->sigma_x);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_x_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", discrete_energy->sigma_y);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_y_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", discrete_energy->sigma_xp);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_xp_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", discrete_energy->sigma_yp);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_yp_entry), buffer);
  g_free(buffer);

  gtk_combo_box_set_active(GTK_COMBO_BOX(dialog->distribution_type_combo), discrete_energy->distribution_type);
  if (discrete_energy->distribution_type != XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC) {
    buffer = g_strdup_printf("%g", discrete_energy->scale_parameter);
    gtk_entry_set_text(GTK_ENTRY(dialog->scale_parameter_entry), buffer);
    g_free(buffer);
  }
}

xmi_energy_discrete* xmi_msim_gui_discrete_energy_dialog_get_discrete_energy(XmiMsimGuiDiscreteEnergyDialog *dialog) {
  xmi_energy_discrete *rv = (xmi_energy_discrete *) g_malloc(sizeof(xmi_energy_discrete));
  rv->energy = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->energy_entry)), NULL);
  rv->horizontal_intensity = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->hor_intensity_entry)), NULL);
  rv->vertical_intensity = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->ver_intensity_entry)), NULL);
  rv->sigma_x = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_x_entry)), NULL);
  rv->sigma_y = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_y_entry)), NULL);
  rv->sigma_xp = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_xp_entry)), NULL);
  rv->sigma_yp = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_yp_entry)), NULL);
  rv->distribution_type = gtk_combo_box_get_active(GTK_COMBO_BOX(dialog->distribution_type_combo));
  rv->scale_parameter = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->scale_parameter_entry)), NULL);

  return rv;
}
