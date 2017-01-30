/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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
#include "xmimsim-gui-compound-dialog.h"
#include "xmimsim-gui-type-builtins.h"
#include <xraylib.h>
#include <stdio.h>
#include <string.h>

static GdkColor red = {(guint32) 0, (guint16) 65535, (guint16) 1000, (guint16) 1000};

static void xmi_msim_compound_dialog_set_property (GObject          *object,
                                                   guint             prop_id,
                                                   const GValue     *value,
                                                   GParamSpec       *pspec);
static void xmi_msim_compound_dialog_get_property (GObject          *object,
                                                   guint             prop_id,
                                                   GValue           *value,
                                                   GParamSpec       *pspec);

static void compound_changed(GtkEditable *widget, gpointer data);

enum {
  PROP_0,
  PROP_COMPOUND_DIALOG_TYPE
};

G_DEFINE_TYPE(XmiMsimGuiCompoundDialog, xmi_msim_gui_compound_dialog, GTK_TYPE_DIALOG)

static void xmi_msim_gui_compound_dialog_class_init(XmiMsimGuiCompoundDialogClass *klass) {
  GObjectClass *gobject_class;
  gobject_class = G_OBJECT_CLASS(klass);

  gobject_class->set_property = xmi_msim_compound_dialog_set_property;
  gobject_class->get_property = xmi_msim_compound_dialog_get_property;

  g_object_class_install_property(gobject_class,
    PROP_COMPOUND_DIALOG_TYPE,
    g_param_spec_enum("compound-dialog-type",
    "Compound Dialog Type",
    "The type of the compound dialog",
    XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG_TYPE,
    XMI_MSIM_GUI_COMPOUND_DIALOG_ADD,
    (GParamFlags) (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY)));


}

static void xmi_msim_gui_compound_dialog_init(XmiMsimGuiCompoundDialog *dialog) {
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
  gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  GtkWidget *HBox;
  GtkWidget *compoundEntry;
  GtkWidget *weightEntry;
  GtkWidget *label;

  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Compound");
  compoundEntry = gtk_entry_new();
  dialog->compoundEntry = compoundEntry;
  gtk_entry_set_activates_default(GTK_ENTRY(compoundEntry), TRUE);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), compoundEntry, FALSE, FALSE, 2);

  gtk_box_pack_start(GTK_BOX(contentArea), HBox, FALSE, FALSE, 1);
  gtk_widget_show_all(HBox);

  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Weight fraction (%)");
  weightEntry = gtk_entry_new();
  dialog->weightEntry = weightEntry;
  gtk_entry_set_activates_default(GTK_ENTRY(weightEntry), TRUE);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), weightEntry, FALSE, FALSE, 2);

  gtk_box_pack_start(GTK_BOX(contentArea), HBox, FALSE, FALSE, 1);
  gtk_widget_show_all(HBox);

  g_signal_connect(G_OBJECT(compoundEntry), "changed", G_CALLBACK(compound_changed), dialog);
  g_signal_connect(G_OBJECT(weightEntry), "changed", G_CALLBACK(compound_changed), dialog);
}

GtkWidget *xmi_msim_gui_compound_dialog_new(GtkWindow *parent, XmiMsimGuiCompoundDialogType type) {
  GtkWidget *widget;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  widget = GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG,
                                   "compound-dialog-type", type,
                                   NULL));

  gtk_window_set_transient_for(GTK_WINDOW(widget),
                               GTK_WINDOW(parent));

  return widget;
}

static void compound_changed(GtkEditable *widget, gpointer data) {
  const char *textPtr,*textPtr2;
  char *endPtr;
  const char *lastPtr;
  XmiMsimGuiCompoundDialog *dialog = (XmiMsimGuiCompoundDialog *) data;
  struct compoundData *cd;
  double weight;

  textPtr = gtk_entry_get_text(GTK_ENTRY(dialog->compoundEntry));
  textPtr2 = gtk_entry_get_text(GTK_ENTRY(dialog->weightEntry));
  weight = g_ascii_strtod(textPtr2, &endPtr);
  cd = CompoundParser(textPtr);

  lastPtr = textPtr2 + strlen(textPtr2);

  if (GTK_WIDGET(widget) == dialog->compoundEntry) {
    if (cd) {
      gtk_widget_modify_base(GTK_WIDGET(widget), GTK_STATE_NORMAL, NULL);
    }
    else {
      //bad value
      gtk_widget_modify_base(GTK_WIDGET(widget), GTK_STATE_NORMAL, &red);
      gtk_widget_set_sensitive(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT), FALSE);
    }
    if (cd && lastPtr == endPtr && weight > 0.0) {
      gtk_widget_set_sensitive(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT), TRUE);
    }
  }
  else if (GTK_WIDGET(widget) == dialog->weightEntry) {
    if (lastPtr == endPtr && weight > 0.0) {
      gtk_widget_modify_base(GTK_WIDGET(widget), GTK_STATE_NORMAL, NULL);
    }
    else {
      //bad value
      gtk_widget_modify_base(GTK_WIDGET(widget), GTK_STATE_NORMAL, &red);
      gtk_widget_set_sensitive(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT), FALSE);
    }
    if (cd && lastPtr == endPtr && weight > 0.0) {
      gtk_widget_set_sensitive(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT), TRUE);
    }
  }
  if (cd)
    FreeCompoundData(cd);
}

static void xmi_msim_compound_dialog_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiCompoundDialog *dialog = XMI_MSIM_GUI_COMPOUND_DIALOG(object);

  switch (prop_id) {
    case PROP_COMPOUND_DIALOG_TYPE:
      dialog->compound_dialog_type = g_value_get_enum(value);
      if (dialog->compound_dialog_type == XMI_MSIM_GUI_COMPOUND_DIALOG_ADD) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Enter a compound");
        gtk_widget_set_sensitive(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT), FALSE);
	gtk_editable_set_editable(GTK_EDITABLE(dialog->compoundEntry), TRUE);
      }
      else if (dialog->compound_dialog_type == XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Modify a compound");
	gtk_editable_set_editable(GTK_EDITABLE(dialog->compoundEntry), FALSE);
	gtk_widget_set_sensitive(dialog->compoundEntry, FALSE);
      }
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}

static void xmi_msim_compound_dialog_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiCompoundDialog *dialog = XMI_MSIM_GUI_COMPOUND_DIALOG(object);

  switch (prop_id) {
    case PROP_COMPOUND_DIALOG_TYPE:
      dialog->compound_dialog_type = g_value_get_enum(value);
      g_value_set_enum (value, (XmiMsimGuiCompoundDialogType) dialog->compound_dialog_type);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

void xmi_msim_gui_compound_dialog_set_compound(XmiMsimGuiCompoundDialog *dialog, const gchar *compound) {
  gtk_entry_set_text(GTK_ENTRY(dialog->compoundEntry), compound);
}

gchar *xmi_msim_gui_compound_dialog_get_compound(XmiMsimGuiCompoundDialog *dialog) {
  return g_strdup(gtk_entry_get_text(GTK_ENTRY(dialog->compoundEntry)));
}

void xmi_msim_gui_compound_dialog_set_weight(XmiMsimGuiCompoundDialog *dialog, gdouble weight) {
  gchar *text = g_strdup_printf("%g", weight);
  gtk_entry_set_text(GTK_ENTRY(dialog->weightEntry), text);
  g_free(text);
}

gdouble xmi_msim_gui_compound_dialog_get_weight(XmiMsimGuiCompoundDialog *dialog) {
  return g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->weightEntry)), NULL);
}
