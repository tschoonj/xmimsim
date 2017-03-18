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
#include "xmimsim-gui-continuous-energy-dialog.h"
#include "xmimsim-gui-type-builtins.h"
#include <string.h>

static GdkColor red = {(guint32) 0, (guint16) 65535, (guint16) 1000, (guint16) 1000};

static void xmi_msim_continuous_energy_dialog_set_property (GObject          *object,
                                                          guint             prop_id,
                                                          const GValue     *value,
                                                          GParamSpec       *pspec);
static void xmi_msim_continuous_energy_dialog_get_property (GObject          *object,
                                                          guint             prop_id,
                                                          GValue           *value,
                                                          GParamSpec       *pspec);

static void entry_value_changed(GtkWidget *widget, XmiMsimGuiContinuousEnergyDialog *dialog);

enum {
  PROP_0,
  PROP_CONTINUOUS_ENERGY_DIALOG_TYPE
};

G_DEFINE_TYPE(XmiMsimGuiContinuousEnergyDialog, xmi_msim_gui_continuous_energy_dialog, GTK_TYPE_DIALOG)

//implementation

static void xmi_msim_gui_continuous_energy_dialog_class_init(XmiMsimGuiContinuousEnergyDialogClass *klass) {
  GObjectClass *gobject_class;
  gobject_class = G_OBJECT_CLASS(klass);

  gobject_class->set_property = xmi_msim_continuous_energy_dialog_set_property;
  gobject_class->get_property = xmi_msim_continuous_energy_dialog_get_property;

  g_object_class_install_property(gobject_class,
    PROP_CONTINUOUS_ENERGY_DIALOG_TYPE,
    g_param_spec_enum("continuous-energy-dialog-type",
    "Continuous Energy Dialog Type",
    "The type of the continuous energy dialog",
    XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG_TYPE,
    XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_ADD,
    (GParamFlags) (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY)));
}

static void xmi_msim_gui_continuous_energy_dialog_init(XmiMsimGuiContinuousEnergyDialog *dialog) {
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE); // in order to have the scale parameter show and hide while automatically resizing the dialog
  gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
  gtk_window_set_default_size(GTK_WINDOW(dialog), 420, 300);

  GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  GtkWidget *mainVBox = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(mainVBox), 5);
  gtk_container_add(GTK_CONTAINER(contentArea), mainVBox);

  //Energy
  GtkWidget *HBox = gtk_hbox_new(FALSE, 2);
  GtkWidget *label = gtk_label_new("Energy (keV)");
  GtkWidget *energy_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(energy_entry), TRUE);
  gulong energy_changed = g_signal_connect(G_OBJECT(energy_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), energy_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //horizontal intensity
  HBox = gtk_hbox_new(FALSE, 2);
  label = gtk_label_new("Horizontally polarized intensity (ph/s/keV)");
  GtkWidget *hor_intensity_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(hor_intensity_entry), TRUE);
  gulong hor_intensity_changed = g_signal_connect(G_OBJECT(hor_intensity_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), hor_intensity_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //vertical intensity
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Vertically polarized intensity (ph/s/keV)");
  GtkWidget *ver_intensity_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(ver_intensity_entry), TRUE);
  gulong ver_intensity_changed = g_signal_connect(G_OBJECT(ver_intensity_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), ver_intensity_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source size x
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Source size x (cm)");
  GtkWidget *sigma_x_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_x_entry), TRUE);
  gulong sigma_x_changed = g_signal_connect(G_OBJECT(sigma_x_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_x_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source size y
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Source size y (cm)");
  GtkWidget *sigma_y_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_y_entry), TRUE);
  gulong sigma_y_changed = g_signal_connect(G_OBJECT(sigma_y_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_y_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source divergence x
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Source divergence x (rad)");
  GtkWidget *sigma_xp_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_xp_entry), TRUE);
  gulong sigma_xp_changed = g_signal_connect(G_OBJECT(sigma_xp_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_xp_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  //source divergence y
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Source divergence y (rad)");
  GtkWidget *sigma_yp_entry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(sigma_yp_entry), TRUE);
  gulong sigma_yp_changed = g_signal_connect(G_OBJECT(sigma_yp_entry), "changed", G_CALLBACK(entry_value_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), sigma_yp_entry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  // assign instance variables
  dialog->energy_entry = energy_entry;
  dialog->hor_intensity_entry = hor_intensity_entry;
  dialog->ver_intensity_entry = ver_intensity_entry;
  dialog->sigma_x_entry = sigma_x_entry;
  dialog->sigma_y_entry = sigma_y_entry;
  dialog->sigma_xp_entry = sigma_xp_entry;
  dialog->sigma_yp_entry = sigma_yp_entry;

  dialog->energy_changed = energy_changed;
  dialog->hor_intensity_changed = hor_intensity_changed;
  dialog->ver_intensity_changed = ver_intensity_changed;
  dialog->sigma_x_changed = sigma_x_changed;
  dialog->sigma_y_changed = sigma_y_changed;
  dialog->sigma_xp_changed = sigma_xp_changed;
  dialog->sigma_yp_changed = sigma_yp_changed;

  gtk_widget_show_all(contentArea);

}

static void xmi_msim_continuous_energy_dialog_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiContinuousEnergyDialog *dialog = XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(object);

  switch (prop_id) {
    case PROP_CONTINUOUS_ENERGY_DIALOG_TYPE:
      dialog->continuous_energy_dialog_type = g_value_get_enum(value);
      if (dialog->continuous_energy_dialog_type == XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_ADD) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Enter a new continuous energy");
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
      }
      else if (dialog->continuous_energy_dialog_type == XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_EDIT) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Modify a continuous energy");
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
      }
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}

static void xmi_msim_continuous_energy_dialog_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiContinuousEnergyDialog *dialog = XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG(object);

  switch (prop_id) {
    case PROP_CONTINUOUS_ENERGY_DIALOG_TYPE:
      dialog->continuous_energy_dialog_type = g_value_get_enum(value);
      g_value_set_enum (value, (XmiMsimGuiContinuousEnergyDialogType) dialog->continuous_energy_dialog_type);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

GtkWidget *xmi_msim_gui_continuous_energy_dialog_new(GtkWindow *parent, XmiMsimGuiContinuousEnergyDialogType type) {
  GtkWidget *widget;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  widget = GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG,
                                   "continuous-energy-dialog-type", type,
                                   NULL));

  gtk_window_set_transient_for(GTK_WINDOW(widget),
                               GTK_WINDOW(parent));

  return widget;
}

static void entry_value_changed(GtkWidget *widget, XmiMsimGuiContinuousEnergyDialog *dialog) {
  char *textPtr1, *textPtr2, *textPtr3, *textPtr4, *textPtr5, *textPtr6, *textPtr7;
  char *endPtr1, *endPtr2, *endPtr3, *endPtr4, *endPtr5, *endPtr6, *endPtr7;
  char *lastPtr1, *lastPtr4, *lastPtr5, *lastPtr6, *lastPtr7;

  int ok1, ok2, ok3, ok4, ok5, ok6, ok7;
  double value1, value2, value3, value4, value5, value6, value7, value8;

  textPtr1 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->energy_entry));
  textPtr2 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->hor_intensity_entry));
  textPtr3 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->ver_intensity_entry));
  textPtr4 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->sigma_x_entry));
  textPtr5 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->sigma_y_entry));
  textPtr6 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->sigma_xp_entry));
  textPtr7 = (char *) gtk_entry_get_text(GTK_ENTRY(dialog->sigma_yp_entry));


#define energy_short2(n,my_entry) value ## n = g_ascii_strtod(textPtr ## n, &endPtr ## n);\
  lastPtr ## n = textPtr ## n + strlen(textPtr ## n);\
  if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n >= 0.0) \
    ok ## n = 1;\
  else\
    ok ## n = 0;\
  if (widget == my_entry) {\
    if (ok ## n)\
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL,NULL);\
    else {\
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&red);\
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);\
    }\
  }

#define energy_short3(n,my_entry) value ## n = g_ascii_strtod(textPtr ## n, &endPtr ## n);\
  lastPtr ## n = textPtr ## n + strlen(textPtr ## n);\
  if (lastPtr ## n == endPtr ## n && strcmp(textPtr ## n,"") != 0 && value ## n > 0.0 && value ## n <= 200.0) \
    ok ## n = 1;\
  else\
    ok ## n = 0;\
  if (widget == my_entry) {\
    if (ok ## n)\
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL,NULL);\
    else {\
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL,&red);\
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);\
    }\
  }

  energy_short3(1, dialog->energy_entry)
  energy_short2(4, dialog->sigma_x_entry)
  energy_short2(5, dialog->sigma_y_entry)
  energy_short2(6, dialog->sigma_xp_entry)
  energy_short2(7, dialog->sigma_yp_entry)

  // investigate the intensities
  value2 = g_ascii_strtod(textPtr2, &endPtr2);
  value3 = g_ascii_strtod(textPtr3, &endPtr3);

  if (value2 > 0.0)
    ok2 = 1;
  else if (strlen(textPtr2) == 0)
    ok2 = 0;
  else if (textPtr2 + strlen(textPtr2) != endPtr2)
    ok2 = -1;
  else if (value2 == 0.0)
    ok2 = -2;
  else
    ok2 = -1;

  if (value3 > 0.0)
    ok3 = 1;
  else if (strlen(textPtr3) == 0)
    ok3 = 0;
  else if (textPtr3 + strlen(textPtr3) != endPtr3)
    ok3 = -1;
  else if (value3 == 0.0)
    ok3 = -2;
  else
    ok3 = -1;


  if (ok2 == 1 || ok2 == 0)
    gtk_widget_modify_base(dialog->hor_intensity_entry, GTK_STATE_NORMAL,NULL);
  else if (ok2 == -1)
    gtk_widget_modify_base(dialog->hor_intensity_entry, GTK_STATE_NORMAL,&red);

  if (ok3 == 1 || ok3 == 0)
    gtk_widget_modify_base(dialog->ver_intensity_entry, GTK_STATE_NORMAL,NULL);
  else if (ok3 == -1)
    gtk_widget_modify_base(dialog->ver_intensity_entry, GTK_STATE_NORMAL,&red);

  if (ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7)
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
  else
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);

  return;

}

void xmi_msim_gui_continuous_energy_dialog_set_continuous_energy(XmiMsimGuiContinuousEnergyDialog *dialog, const struct xmi_energy_continuous *continuous_energy) {
  gchar *buffer = g_strdup_printf("%g", continuous_energy->energy);
  gtk_entry_set_text(GTK_ENTRY(dialog->energy_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", continuous_energy->horizontal_intensity);
  gtk_entry_set_text(GTK_ENTRY(dialog->hor_intensity_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", continuous_energy->vertical_intensity);
  gtk_entry_set_text(GTK_ENTRY(dialog->ver_intensity_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", continuous_energy->sigma_x);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_x_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", continuous_energy->sigma_y);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_y_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", continuous_energy->sigma_xp);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_xp_entry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", continuous_energy->sigma_yp);
  gtk_entry_set_text(GTK_ENTRY(dialog->sigma_yp_entry), buffer);
  g_free(buffer);
}

struct xmi_energy_continuous* xmi_msim_gui_continuous_energy_dialog_get_continuous_energy(XmiMsimGuiContinuousEnergyDialog *dialog) {
  struct xmi_energy_continuous *rv = (struct xmi_energy_continuous *) g_malloc(sizeof(struct xmi_energy_continuous));
  rv->energy = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->energy_entry)), NULL);
  rv->horizontal_intensity = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->hor_intensity_entry)), NULL);
  rv->vertical_intensity = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->ver_intensity_entry)), NULL);
  rv->sigma_x = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_x_entry)), NULL);
  rv->sigma_y = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_y_entry)), NULL);
  rv->sigma_xp = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_xp_entry)), NULL);
  rv->sigma_yp = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->sigma_yp_entry)), NULL);

  return rv;
}
