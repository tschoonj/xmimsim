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

#include <config.h>
#include "xmimsim-gui-catalog-dialog.h"
#include "xmi_aux.h"
#include <xraylib.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "xmimsim-gui-prefs.h"

G_DEFINE_TYPE(XmiMsimGuiCatalogDialog, xmi_msim_gui_catalog_dialog, GTK_TYPE_DIALOG)

static void combo_changed(GtkWidget *comboBox, GtkToggleButton *radio);

static void xmi_msim_gui_catalog_dialog_class_init(XmiMsimGuiCatalogDialogClass *klass) {
  //nothing to init here
}

static void xmi_msim_gui_catalog_dialog_init(XmiMsimGuiCatalogDialog *dialog) {
  gtk_window_set_title(GTK_WINDOW(dialog), "Select a layer from the catalogs");
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);	
  gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
  gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
  gtk_file_chooser_set_do_overwrite_confirmation(GTK_FILE_CHOOSER(dialog), TRUE);

  GtkWidget *content_area = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

  GtkWidget *vbox = gtk_vbox_new(FALSE,2);

  GtkWidget *nist_radioW = gtk_radio_button_new_with_label_from_widget(NULL, "NIST compositions");
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 15);

  GtkWidget *nist_comboW = gtk_combo_box_text_new();

  int i;
  char **list = GetCompoundDataNISTList(NULL);

  for (i = 0 ; list[i] != NULL ; i++) {
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(nist_comboW), list[i]);
    xrlFree(list[i]);
  }
  xrlFree(list);
  gtk_combo_box_set_active(GTK_COMBO_BOX(nist_comboW), 0);
  gtk_box_pack_start(GTK_BOX(vbox), nist_radioW, TRUE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(vbox), nist_comboW, TRUE, FALSE, 2);

  gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(), TRUE, FALSE, 2);

  GtkWidget *user_radioW = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(nist_radioW), "User-defined compositions");
  GtkWidget *user_comboW = gtk_combo_box_text_new();

  list = xmimsim_gui_get_user_defined_layer_names();
  if (list != NULL && g_strv_length(list) > 0) {
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(user_radioW), TRUE);
    for (i = 0 ; list[i] != NULL ; i++) {
      gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(user_comboW), list[i]);
      g_free(list[i]);
    }
    g_free(list);
    gtk_combo_box_set_active(GTK_COMBO_BOX(user_comboW), 0);
  }
  else {
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(nist_radioW), TRUE);
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(user_comboW), "(Empty)");
    gtk_widget_set_sensitive(user_radioW, FALSE);
    gtk_combo_box_set_active(GTK_COMBO_BOX(user_comboW), 0);
    gtk_widget_set_sensitive(user_comboW, FALSE);
  }
  gtk_box_pack_start(GTK_BOX(vbox), user_radioW, TRUE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(vbox), user_comboW, TRUE, FALSE, 2);
  gtk_widget_show_all(vbox);


  gtk_container_add (GTK_CONTAINER (content_area), vbox);
  g_signal_connect(G_OBJECT(nist_comboW), "changed", G_CALLBACK(combo_changed), nist_radioW);
  g_signal_connect(G_OBJECT(user_comboW), "changed", G_CALLBACK(combo_changed), user_radioW);

  dialog->nist_radioW = nist_radioW;
  dialog->user_radioW = user_radioW;
  dialog->nist_comboW = nist_comboW;
  dialog->user_comboW = user_comboW;
}

GtkWidget *xmi_msim_gui_catalog_dialog_new(GtkWindow *parent) {
  GtkWidget *widget;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  widget = GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_CATALOG_DIALOG, NULL));

  gtk_window_set_transient_for(GTK_WINDOW(widget),
                               GTK_WINDOW(parent));

  return widget;
}

static void combo_changed(GtkWidget *comboBox, GtkToggleButton *radio) {
  gtk_toggle_button_set_active(radio, TRUE);
}

struct xmi_layer* xmi_msim_gui_catalog_dialog_get_layer(XmiMsimGuiCatalogDialog *dialog) {
  struct xmi_layer *rv;
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dialog->nist_radioW))) {
    int nistCompNumber = gtk_combo_box_get_active(GTK_COMBO_BOX(dialog->nist_comboW));
    struct compoundDataNIST *cdn = GetCompoundDataNISTByIndex(nistCompNumber);
    if (cdn == NULL) {
      fprintf(stderr,"Fatal error in GetCompoundDataNISTListByIndex\n");
      return NULL;
    }
    // correct for normalization errors in xraylibs
    double sum = xmi_sum_double(cdn->massFractions, cdn->nElements);
    xmi_scale_double(cdn->massFractions, cdn->nElements, 1.0/sum);
    rv = compoundDataNIST2xmi_layer(cdn);
    rv->thickness = 0.0;
    FreeCompoundDataNIST(cdn);
  }
  else {
    gchar *user_comp;
    user_comp = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(dialog->user_comboW));
    rv = xmimsim_gui_get_user_defined_layer(user_comp);
    g_free(user_comp);
    if (rv == NULL) {
      fprintf(stderr,"Fatal error in xmimsim_gui_get_user_defined_layer\n");
      return NULL;
    }
  }
  return rv;
}
