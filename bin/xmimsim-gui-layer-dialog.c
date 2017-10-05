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
#include "xmimsim-gui-layer-dialog.h"
#include "xmimsim-gui-compound-dialog.h"
#include "xmimsim-gui-catalog-dialog.h"
#include "xmimsim-gui-type-builtins.h"
#include "xmimsim-gui-prefs.h"
#include <xraylib.h>
#include <stdio.h>
#include <string.h>
#include "xmi_aux.h"

static GdkColor red = {(guint32) 0, (guint16) 65535, (guint16) 1000, (guint16) 1000};

static void xmi_msim_layer_dialog_set_property (GObject          *object,
                                                guint             prop_id,
                                                const GValue     *value,
                                                GParamSpec       *pspec);
static void xmi_msim_layer_dialog_get_property (GObject          *object,
                                                guint             prop_id,
                                                GValue           *value,
                                                GParamSpec       *pspec);

enum {
	SYMBOL_COLUMN,  //The element symbol (e.g. Fe) G_TYPE_STRING
	WEIGHT_COLUMN,  //The weight of the element, expressed in percentages! G_TYPE_DOUBLE
	ELEMENT_COLUMN, //The atomic number (e.g. 26) G_TYPE_INT
	N_COLUMNS_LAYER
};

enum {
  PROP_0,
  PROP_LAYER_DIALOG_TYPE
};

G_DEFINE_TYPE(XmiMsimGuiLayerDialog, xmi_msim_gui_layer_dialog, GTK_TYPE_DIALOG)

static void density_thickness_changed(GtkWidget *widget, XmiMsimGuiLayerDialog *dialog);

static void xmi_msim_gui_layer_dialog_set_composition(XmiMsimGuiLayerDialog *dialog, int n_elements, int *Z, double *weight);

static void normalize_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog);

static void xmi_msim_gui_layer_dialog_get_composition(XmiMsimGuiLayerDialog *dialog, int *n_elements, int **Z, double **weight);

static void element_row_activated(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, XmiMsimGuiLayerDialog *dialog);

static void add_to_catalog_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog);

static void name_entry_changed(GtkEntry *nameEntry, GtkWidget *okButton);

static void predef_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog);

static void remove_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog);

static void edit_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog);

static void add_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog);

static void element_selection_changed(GtkTreeSelection *selection, XmiMsimGuiLayerDialog *dialog);

static void update_sum(XmiMsimGuiLayerDialog *dialog);

static gboolean backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, XmiMsimGuiLayerDialog *dialog);


//implementation

static void xmi_msim_gui_layer_dialog_class_init(XmiMsimGuiLayerDialogClass *klass) {
  GObjectClass *gobject_class;
  gobject_class = G_OBJECT_CLASS(klass);

  gobject_class->set_property = xmi_msim_layer_dialog_set_property;
  gobject_class->get_property = xmi_msim_layer_dialog_get_property;

  g_object_class_install_property(gobject_class,
    PROP_LAYER_DIALOG_TYPE,
    g_param_spec_enum("layer-dialog-type",
    "Layer Dialog Type",
    "The type of the layer dialog",
    XMI_MSIM_GUI_TYPE_LAYER_DIALOG_TYPE,
    XMI_MSIM_GUI_LAYER_DIALOG_ADD,
    (GParamFlags) (G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY)));
}

static void xmi_msim_gui_layer_dialog_init(XmiMsimGuiLayerDialog *dialog) {
  gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(dialog), TRUE);
  gtk_dialog_add_buttons(GTK_DIALOG(dialog), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
  gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200);

  GtkWidget *contentArea = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
  GtkWidget *mainVBox = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(mainVBox),5);
  gtk_container_add(GTK_CONTAINER(contentArea), mainVBox);

  GtkListStore *store = gtk_list_store_new(N_COLUMNS_LAYER, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_INT);
  //construct tree
  GtkWidget *HBox = gtk_hbox_new(FALSE,5);
  GtkWidget *compositionTreeView = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
  GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
  gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
  GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes("Element", renderer, "text", SYMBOL_COLUMN, NULL);
  gtk_tree_view_column_set_resizable(column, TRUE);
  gtk_tree_view_column_set_alignment(column, 0.5);
  gtk_tree_view_append_column(GTK_TREE_VIEW(compositionTreeView), column);

  renderer = gtk_cell_renderer_text_new();
  gtk_cell_renderer_set_alignment(renderer, 0.5, 0.5);
  column = gtk_tree_view_column_new_with_attributes("Weight fraction (%)", renderer, "text", WEIGHT_COLUMN, NULL);
  gtk_tree_view_column_set_resizable(column, TRUE);
  gtk_tree_view_column_set_alignment(column, 0.5);
  gtk_tree_view_append_column(GTK_TREE_VIEW(compositionTreeView), column);

  GtkWidget *scrolledWindow = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledWindow), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request(scrolledWindow, 220, 150);
  gtk_container_add(GTK_CONTAINER(scrolledWindow), compositionTreeView);
  GtkWidget *frame = gtk_frame_new(NULL);
  gtk_container_add(GTK_FRAME(frame), scrolledWindow);
  gtk_box_pack_start(GTK_BOX(HBox), frame, FALSE, FALSE, 3);

  //selections
  GtkTreeSelection *select = gtk_tree_view_get_selection(GTK_TREE_VIEW(compositionTreeView));
  gtk_tree_selection_set_mode(select, GTK_SELECTION_MULTIPLE);
  g_signal_connect(G_OBJECT(select), "changed", G_CALLBACK(element_selection_changed), (gpointer) dialog);


  //add/edit/remove
  GtkWidget *VBox = gtk_vbox_new(FALSE,5);
  GtkWidget *addButton = gtk_button_new_from_stock(GTK_STOCK_ADD);
  GtkWidget *editButton = gtk_button_new_from_stock(GTK_STOCK_EDIT);
  GtkWidget *removeButton = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
  GtkWidget *predefButton = gtk_button_new_with_label("Load from catalog");
  GtkWidget *addToCatalogButton = gtk_button_new_with_label("Add to catalog");

  gtk_widget_set_sensitive(addButton, TRUE);
  gtk_widget_set_sensitive(editButton, FALSE);
  gtk_widget_set_sensitive(removeButton, FALSE);
  gtk_widget_set_sensitive(predefButton, TRUE);
  gtk_widget_set_sensitive(addToCatalogButton, FALSE);

  gtk_box_pack_start(GTK_BOX(VBox), addButton, TRUE, FALSE, 3);
  gtk_box_pack_start(GTK_BOX(VBox), editButton, TRUE, FALSE, 3);
  gtk_box_pack_start(GTK_BOX(VBox), removeButton, TRUE, FALSE, 3);
  gtk_box_pack_start(GTK_BOX(VBox), predefButton, TRUE, FALSE, 3);
  gtk_box_pack_start(GTK_BOX(VBox), addToCatalogButton, TRUE, FALSE, 3);

  gtk_box_pack_start(GTK_BOX(HBox),VBox, FALSE, FALSE, 3);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  g_signal_connect(G_OBJECT(addButton), "clicked", G_CALLBACK(add_button_clicked), (gpointer) dialog);
  g_signal_connect(G_OBJECT(editButton), "clicked", G_CALLBACK(edit_button_clicked), (gpointer) dialog);
  g_signal_connect(G_OBJECT(removeButton), "clicked", G_CALLBACK(remove_button_clicked), (gpointer) dialog);
  g_signal_connect(G_OBJECT(predefButton), "clicked", G_CALLBACK(predef_button_clicked), (gpointer) dialog);
  g_signal_connect(G_OBJECT(addToCatalogButton), "clicked", G_CALLBACK(add_to_catalog_button_clicked), (gpointer) dialog);
  g_signal_connect(G_OBJECT(compositionTreeView), "row-activated", G_CALLBACK(element_row_activated), (gpointer) dialog);

  //Sum and normalize
  HBox = gtk_hbox_new(FALSE,2);
  GtkWidget *label = gtk_label_new("Weights sum (%)");
  GtkWidget *sumLabel = gtk_label_new("");
  gtk_label_set_justify(GTK_LABEL(sumLabel), GTK_JUSTIFY_CENTER);
  GtkWidget *normalizeButton = gtk_button_new_with_label("Normalize");
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(HBox), sumLabel, TRUE, TRUE, 2);
  gtk_box_pack_start(GTK_BOX(HBox), normalizeButton, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
  g_signal_connect(G_OBJECT(normalizeButton), "clicked", G_CALLBACK(normalize_button_clicked), (gpointer) dialog);

  //separator
  GtkWidget *separator = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(mainVBox), separator, FALSE, FALSE, 3);

  //Density and thickness
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new(NULL);
  gtk_label_set_markup(GTK_LABEL(label),"Density (g/cm<sup>3</sup>)");
  GtkWidget *densityEntry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(densityEntry), TRUE);
  gulong density_changed = g_signal_connect(G_OBJECT(densityEntry), "changed", G_CALLBACK(density_thickness_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), densityEntry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);
  HBox = gtk_hbox_new(FALSE,2);
  label = gtk_label_new("Thickness (cm)");
  GtkWidget *thicknessEntry = gtk_entry_new();
  gtk_entry_set_activates_default(GTK_ENTRY(thicknessEntry), TRUE);
  gulong thickness_changed = g_signal_connect(G_OBJECT(thicknessEntry), "changed", G_CALLBACK(density_thickness_changed), (gpointer) dialog);
  gtk_box_pack_start(GTK_BOX(HBox), label, FALSE, FALSE, 2);
  gtk_box_pack_end(GTK_BOX(HBox), thicknessEntry, FALSE, FALSE, 2);
  gtk_box_pack_start(GTK_BOX(mainVBox), HBox, FALSE, FALSE, 3);

  dialog->addButton = addButton;
  dialog->editButton = editButton;
  dialog->removeButton = removeButton;
  dialog->predefButton = predefButton;
  dialog->addToCatalogButton = addToCatalogButton;
  dialog->normalizeButton = normalizeButton;
  dialog->sumLabel = sumLabel;
  dialog->densityEntry = densityEntry;
  dialog->thicknessEntry = thicknessEntry;
  dialog->compositionTreeView = compositionTreeView;
  dialog->density_changed = density_changed;
  dialog->thickness_changed = thickness_changed;

  g_signal_connect(G_OBJECT(compositionTreeView), "key-press-event", G_CALLBACK(backspace_key_clicked), (gpointer) dialog);

  gtk_widget_show_all(contentArea);

}

static void xmi_msim_layer_dialog_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiLayerDialog *dialog = XMI_MSIM_GUI_LAYER_DIALOG(object);

  switch (prop_id) {
    case PROP_LAYER_DIALOG_TYPE:
      dialog->layer_dialog_type = g_value_get_enum(value);
      if (dialog->layer_dialog_type == XMI_MSIM_GUI_LAYER_DIALOG_ADD) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Enter a new layer");
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
      }
      else if (dialog->layer_dialog_type == XMI_MSIM_GUI_LAYER_DIALOG_EDIT) {
        gtk_window_set_title(GTK_WINDOW(dialog), "Modify a layer");
	gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
      }
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }

}

static void xmi_msim_layer_dialog_get_property(GObject *object, guint prop_id, GValue *value,  GParamSpec *pspec) {

  XmiMsimGuiLayerDialog *dialog = XMI_MSIM_GUI_LAYER_DIALOG(object);

  switch (prop_id) {
    case PROP_LAYER_DIALOG_TYPE:
      dialog->layer_dialog_type = g_value_get_enum(value);
      g_value_set_enum (value, (XmiMsimGuiLayerDialogType) dialog->layer_dialog_type);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
  }
}

GtkWidget *xmi_msim_gui_layer_dialog_new(GtkWindow *parent, XmiMsimGuiLayerDialogType type) {
  GtkWidget *widget;

  g_return_val_if_fail(parent == NULL || GTK_IS_WINDOW(parent), NULL);

  widget = GTK_WIDGET(g_object_new(XMI_MSIM_GUI_TYPE_LAYER_DIALOG,
                                   "layer-dialog-type", type,
                                   NULL));

  gtk_window_set_transient_for(GTK_WINDOW(widget),
                               GTK_WINDOW(parent));

  return widget;
}

static void density_thickness_changed(GtkWidget *widget, XmiMsimGuiLayerDialog *dialog) {
  const char *textPtr, *textPtr2, *lastPtr, *lastPtr2, *textPtr3, *lastPtr3;
  char *endPtr, *endPtr2, *endPtr3;
  int density_ok, thickness_ok, layer_ok;
  double density, thickness, sum;

  textPtr = gtk_entry_get_text(GTK_ENTRY(dialog->densityEntry));
  textPtr2 = gtk_entry_get_text(GTK_ENTRY(dialog->thicknessEntry));
  // sumLabel is used to see it there are elements in the compositionTreeView
  // if no elements are to be found there, the label should always be zero
  textPtr3 = gtk_label_get_text(GTK_LABEL(dialog->sumLabel));

  density = g_ascii_strtod(textPtr, &endPtr);
  thickness = g_ascii_strtod(textPtr2, &endPtr2);
  sum = g_ascii_strtod(textPtr3, &endPtr3);

  lastPtr = textPtr + strlen(textPtr);
  lastPtr2 = textPtr2 + strlen(textPtr2);
  lastPtr3 = textPtr3 + strlen(textPtr3);

  if (lastPtr == endPtr && density > 0.0)
    density_ok = 1;
  else
    density_ok = 0;

  if (lastPtr2 == endPtr2 && thickness > 0.0)
    thickness_ok = 1;
  else
    thickness_ok = 0;

  if (lastPtr3 == endPtr3 && sum > 0.0)
    layer_ok = 1;
  else
    layer_ok = 0;

  if (widget == dialog->densityEntry) {
    if (density_ok)
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL, NULL);
    else {
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL, &red);
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
      gtk_widget_set_sensitive(dialog->addToCatalogButton, FALSE);
    }
  }
  else if (widget == dialog->thicknessEntry) {
    if (thickness_ok)
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL, NULL);
    else {
      gtk_widget_modify_base(widget, GTK_STATE_NORMAL, &red);
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
      gtk_widget_set_sensitive(dialog->addToCatalogButton,FALSE);
    }
  }

  if (thickness_ok && density_ok && layer_ok) {
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
    gtk_widget_set_sensitive(dialog->addToCatalogButton,TRUE);
  }

  return;
}

void xmi_msim_gui_layer_dialog_set_layer(XmiMsimGuiLayerDialog *dialog, struct xmi_layer *layer) {

  gchar *buffer = g_strdup_printf("%g", layer->thickness);
  gtk_entry_set_text(GTK_ENTRY(dialog->thicknessEntry), buffer);
  g_free(buffer);

  buffer = g_strdup_printf("%g", layer->density);
  gtk_entry_set_text(GTK_ENTRY(dialog->densityEntry), buffer);
  g_free(buffer);
  xmi_msim_gui_layer_dialog_set_composition(dialog, layer->n_elements, layer->Z, layer->weight);
}

static void xmi_msim_gui_layer_dialog_set_composition(XmiMsimGuiLayerDialog *dialog, int n_elements, int *Z, double *weight) {
  GtkTreeIter iter;
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(dialog->compositionTreeView)));
  gtk_list_store_clear(store);
  int i;
  for (i = 0 ; i < n_elements ; i++) {
    gtk_list_store_append(store, &iter);
    char *symbol = AtomicNumberToSymbol(Z[i]);
    gtk_list_store_set(store, &iter,
      SYMBOL_COLUMN, symbol,
      WEIGHT_COLUMN, weight[i] * 100.0,
      ELEMENT_COLUMN, Z[i],
      -1);
    xrlFree(symbol);
  }
  gchar *buffer = g_strdup_printf("<span weight=\"bold\">%lg</span>", xmi_sum_double(weight, n_elements) * 100.0);
  gtk_label_set_markup(GTK_LABEL(dialog->sumLabel), buffer);
  if (n_elements == 0) {
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
    gtk_widget_set_sensitive(dialog->addToCatalogButton, FALSE);
  }
  else {
    const char *textPtr = gtk_entry_get_text(GTK_ENTRY(dialog->densityEntry));
    const char *textPtr2 = gtk_entry_get_text(GTK_ENTRY(dialog->thicknessEntry));
    double density = g_ascii_strtod(textPtr, NULL);
    double thickness = g_ascii_strtod(textPtr2, NULL);
    if (density > 0.0 && thickness > 0.0) {
      gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, TRUE);
      gtk_widget_set_sensitive(dialog->addToCatalogButton, TRUE);
    }
  }
  g_free(buffer);
}

static void xmi_msim_gui_layer_dialog_get_composition(XmiMsimGuiLayerDialog *dialog, int *n_elements, int **Z, double **weight) {
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(dialog->compositionTreeView)));
  *n_elements = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(store), NULL);

  if (*n_elements == 0)
    return;

  *Z = (int *) g_malloc(sizeof(int) * *n_elements);
  *weight = (double *) g_malloc(sizeof(double) * *n_elements);
  int i;

  for (i = 0 ; i < *n_elements ; i++) {
    GtkTreePath *path = gtk_tree_path_new_from_indices(i, -1);
    GtkTreeIter iter;
    gtk_tree_model_get_iter(GTK_TREE_MODEL(store), &iter, path);
    double weight_percentage;
    gtk_tree_model_get(GTK_TREE_MODEL(store), &iter, ELEMENT_COLUMN, *Z + i, WEIGHT_COLUMN, &weight_percentage, -1);
    *(*weight + i) = weight_percentage/100.0;
    gtk_tree_path_free(path);
  }
}

struct xmi_layer* xmi_msim_gui_layer_dialog_get_layer(XmiMsimGuiLayerDialog *dialog) {
  struct xmi_layer *rv = (struct xmi_layer *) g_malloc(sizeof(struct xmi_layer));
  xmi_msim_gui_layer_dialog_get_composition(dialog, &rv->n_elements, &rv->Z, &rv->weight);

  rv->density = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->densityEntry)), NULL);
  rv->thickness = g_ascii_strtod(gtk_entry_get_text(GTK_ENTRY(dialog->thicknessEntry)), NULL);

  return rv;
}

static void normalize_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog) {
  double sum;
  int n_elements;
  int *Z;
  double *weight;

  xmi_msim_gui_layer_dialog_get_composition(dialog, &n_elements, &Z, &weight);

  if (n_elements == 0)
    return;

  sum = xmi_sum_double(weight, n_elements);
  xmi_scale_double(weight, n_elements, 1.0/sum);

  xmi_msim_gui_layer_dialog_set_composition(dialog, n_elements, Z, weight);

  g_free(Z);
  g_free(weight);
}

static void element_row_activated(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, XmiMsimGuiLayerDialog *dialog) {
  GtkTreeIter iter;
  gchar *element;
  double weight;
  GtkListStore *store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(dialog->compositionTreeView)));

  gtk_tree_model_get_iter(GTK_TREE_MODEL(store), &iter, path);
  gtk_tree_model_get(GTK_TREE_MODEL(store), &iter, SYMBOL_COLUMN,  &element, WEIGHT_COLUMN, &weight, -1);

  GtkWidget *compound_dialog = xmi_msim_gui_compound_dialog_new(GTK_WINDOW(dialog), XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT);
  xmi_msim_gui_compound_dialog_set_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog), element);
  xmi_msim_gui_compound_dialog_set_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog), weight);

  int rv = gtk_dialog_run(GTK_DIALOG(compound_dialog));

  if (rv == GTK_RESPONSE_ACCEPT) {
    // weight has been modified by user
    weight = xmi_msim_gui_compound_dialog_get_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog));
    // update model
    gtk_list_store_set(store, &iter,
      WEIGHT_COLUMN, weight,
      -1);
    update_sum(dialog);
  }

  g_free(element);

  gtk_widget_destroy(compound_dialog);

  return;
}

static void add_to_catalog_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog) {
  GtkWidget *update_dialog;
  GtkWidget *content_area, *vbox;

  update_dialog = gtk_dialog_new_with_buttons("Add current layer to the catalog",
    GTK_WINDOW(dialog),
    (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
    GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);
  gtk_widget_set_size_request(update_dialog, 300,-1);

  content_area = gtk_dialog_get_content_area(GTK_DIALOG(update_dialog));
  vbox = gtk_vbox_new(FALSE,2);
  gtk_box_pack_start(GTK_BOX(vbox), gtk_label_new("Choose a name for the layer"), TRUE, FALSE, 2);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 15);
  GtkWidget *nameEntry = gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(vbox), nameEntry, TRUE, TRUE, 2);
  GtkWidget *okButton = gtk_dialog_get_widget_for_response(GTK_DIALOG(update_dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_set_sensitive(okButton, FALSE);
  g_signal_connect(G_OBJECT(nameEntry),"changed",G_CALLBACK(name_entry_changed), (gpointer) okButton);

  gtk_widget_show_all(vbox);
  gtk_container_add (GTK_CONTAINER (content_area), vbox);

  if (gtk_dialog_run(GTK_DIALOG(update_dialog)) == GTK_RESPONSE_ACCEPT) {
    gchar *layer_name = g_strstrip(g_strdup(gtk_entry_get_text(GTK_ENTRY(nameEntry))));
    GtkWidget *error_dialog;
    struct xmi_layer *layer = xmi_msim_gui_layer_dialog_get_layer(dialog);

    if (xmimsim_gui_add_user_defined_layer(layer, layer_name) == 1) {
      error_dialog = gtk_message_dialog_new(GTK_WINDOW(update_dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_INFO, GTK_BUTTONS_CLOSE, "Layer %s has been added to the catalog", layer_name);
    }
    else {
      error_dialog = gtk_message_dialog_new(GTK_WINDOW(update_dialog), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Could not add layer %s to the catalog.\nReport this incident to the developers.", layer_name);
    }
    gtk_dialog_run(GTK_DIALOG(error_dialog));
    gtk_widget_destroy(error_dialog);
    g_free(layer_name);
    xmi_free_layer(layer);
    g_free(layer);
  }
  gtk_widget_destroy(update_dialog);

  return;
}

static void name_entry_changed(GtkEntry *nameEntry, GtkWidget *okButton) {
  if (strlen(g_strstrip(g_strdup(gtk_entry_get_text(nameEntry)))) == 0) {
    //little memory leak here
    gtk_widget_set_sensitive(okButton, FALSE);
  }
  else {
    gtk_widget_set_sensitive(okButton, TRUE);
  }
  return;
}

static void predef_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog) {
  GtkWidget *catalog_dialog = xmi_msim_gui_catalog_dialog_new(GTK_WINDOW(dialog));
  gchar *buffer;

  if (gtk_dialog_run(GTK_DIALOG(catalog_dialog)) == GTK_RESPONSE_ACCEPT) {
    struct xmi_layer *layer = xmi_msim_gui_catalog_dialog_get_layer(XMI_MSIM_GUI_CATALOG_DIALOG(catalog_dialog));
    xmi_msim_gui_layer_dialog_set_composition(dialog, layer->n_elements, layer->Z, layer->weight);
    buffer = g_strdup_printf("%g", layer->density);
    gtk_entry_set_text(GTK_ENTRY(dialog->densityEntry), buffer);
    g_free(buffer);
    if (layer->thickness > 0.0) {
      buffer = g_strdup_printf("%g", layer->thickness);
      gtk_entry_set_text(GTK_ENTRY(dialog->thicknessEntry), buffer);
      g_free(buffer);
    }
    xmi_free_layer(layer);
    g_free(layer);
  }
  gtk_widget_destroy(catalog_dialog);
}

static void remove_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog) {
  GtkTreeModel *model;
  GtkTreeSelection *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(dialog->compositionTreeView));
  GList *selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);
  gint n_selected_rows = gtk_tree_selection_count_selected_rows(selection);

  GtkTreeRowReference **refs = (GtkTreeRowReference**) g_malloc(sizeof(GtkTreeRowReference*)*n_selected_rows);
  int i;
  // the following really needs two for loops!!!
  // paths change while rows are being added or removed, while references never change
  for (i = 0 ; i < n_selected_rows ; i++) {
    refs[i] = gtk_tree_row_reference_new(model, (GtkTreePath *) g_list_nth_data(selected_rows, i));
  }
  for (i = 0 ; i < n_selected_rows ; i++) {
    GtkTreePath *path = gtk_tree_row_reference_get_path(refs[i]);
    GtkTreeIter iter;
    gtk_tree_model_get_iter(model, &iter, path);
    //delete row happens here
    gtk_list_store_remove(GTK_LIST_STORE(model), &iter);
    gtk_tree_path_free(path);
    gtk_tree_row_reference_free(refs[i]);
  }

  g_list_free_full(selected_rows, (GDestroyNotify) gtk_tree_path_free);
  g_free(refs);

  //the sum needs to be updated
  update_sum(dialog);

  return;
}

static void edit_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog) {
  GtkTreeIter iter;
  GtkTreeModel *model;
  gchar *element;
  double weight;
  GList *paths;
  GtkTreeSelection *select = gtk_tree_view_get_selection(GTK_TREE_VIEW(dialog->compositionTreeView));

  //get selection
  paths = gtk_tree_selection_get_selected_rows(select, &model);
  GtkTreePath *path = (GtkTreePath *) g_list_nth_data(paths, 0);
  gtk_tree_model_get_iter(model, &iter, path);

  //get data from selected
  gtk_tree_model_get(model, &iter, SYMBOL_COLUMN,  &element, WEIGHT_COLUMN, &weight,  -1);

  //put it in dialog
  GtkWidget *compound_dialog = xmi_msim_gui_compound_dialog_new(GTK_WINDOW(dialog), XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT);
  xmi_msim_gui_compound_dialog_set_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog), element);
  xmi_msim_gui_compound_dialog_set_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog), weight);

  int rv = gtk_dialog_run(GTK_DIALOG(compound_dialog));

  if (rv == GTK_RESPONSE_ACCEPT) {
    //something was changed
    weight = xmi_msim_gui_compound_dialog_get_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog));
    // update model
    gtk_list_store_set(GTK_LIST_STORE(model), &iter,
      WEIGHT_COLUMN, weight,
      -1);
    update_sum(dialog);
  }

  g_free(element);
  g_list_free_full(paths, (GDestroyNotify) gtk_tree_path_free);

  gtk_widget_destroy(compound_dialog);

  return;
}

static void update_sum(XmiMsimGuiLayerDialog *dialog) {
  int n_elements, *Z;
  double *weight;

  xmi_msim_gui_layer_dialog_get_composition(dialog, &n_elements, &Z, &weight);
  gchar *buffer = g_strdup_printf("<span weight=\"bold\">%lg</span>", xmi_sum_double(weight, n_elements) * 100.0);
  gtk_label_set_markup(GTK_LABEL(dialog->sumLabel), buffer);
  if (n_elements == 0) {
    gtk_dialog_set_response_sensitive(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT, FALSE);
    gtk_widget_set_sensitive(dialog->addToCatalogButton, FALSE);
  }
  g_free(buffer);
  if (n_elements > 0) {
    g_free(Z);
    g_free(weight);
  }
  return;
}

static void add_button_clicked(GtkButton *button, XmiMsimGuiLayerDialog *dialog) {
  //make entries empty and disable OK button
  GtkWidget *compound_dialog = xmi_msim_gui_compound_dialog_new(GTK_WINDOW(dialog), XMI_MSIM_GUI_COMPOUND_DIALOG_ADD);

  int rv = gtk_dialog_run(GTK_DIALOG(compound_dialog));

  if (rv == GTK_RESPONSE_ACCEPT) {
    //something was changed
    gchar *compound = xmi_msim_gui_compound_dialog_get_compound(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog));
    gdouble compound_weight = xmi_msim_gui_compound_dialog_get_weight(XMI_MSIM_GUI_COMPOUND_DIALOG(compound_dialog));

    struct compoundData *cd, *cd2, *cdsum;
    cd2 = CompoundParser(compound);

    //get current composition
    int n_elements, *Z;
    double *weight;
    xmi_msim_gui_layer_dialog_get_composition(dialog, &n_elements, &Z, &weight);
    if (n_elements > 0) {
      struct xmi_layer layer = {n_elements, Z, weight, 0.0, 0.0};

      //copy xmi_layer to compoundData and add current contents
      cd = xmi_layer2compoundData(&layer);
      //calculate sum
      cdsum = add_compound_data(*cd, 1.0, *cd2, compound_weight/100.0);
      xmi_msim_gui_layer_dialog_set_composition(dialog, cdsum->nElements, cdsum->Elements, cdsum->massFractions);
      g_free(Z);
      g_free(weight);
      FreeCompoundData(cdsum);
      FreeCompoundData(cd);
    }
    else {
      //list is empty!
      xmi_scale_double(cd2->massFractions, cd2->nElements, compound_weight/100.0);
      if (cd2->nElements == 1) {
        // if compound
        double density = ElementDensity(cd2->Elements[0]);
        gchar *buffer = g_strdup_printf("%f", density);
        g_signal_handler_block(G_OBJECT(dialog->densityEntry), dialog->density_changed);
        gtk_entry_set_text(GTK_ENTRY(dialog->densityEntry), buffer);
        g_signal_handler_unblock(G_OBJECT(dialog->densityEntry), dialog->density_changed);
	g_free(buffer);
      }
      xmi_msim_gui_layer_dialog_set_composition(dialog, cd2->nElements, cd2->Elements, cd2->massFractions);
    }
    FreeCompoundData(cd2);
  }

  gtk_widget_destroy(compound_dialog);

  return;
}

static void element_selection_changed(GtkTreeSelection *selection, XmiMsimGuiLayerDialog *dialog) {
  int nselected = gtk_tree_selection_count_selected_rows(gtk_tree_view_get_selection(GTK_TREE_VIEW(dialog->compositionTreeView)));
  switch (nselected) {
    case 0:
      gtk_widget_set_sensitive(dialog->editButton, FALSE);
      gtk_widget_set_sensitive(dialog->removeButton, FALSE);
      break;
    case 1:
      gtk_widget_set_sensitive(dialog->editButton, TRUE);
      gtk_widget_set_sensitive(dialog->removeButton, TRUE);
      break;
    default:
      gtk_widget_set_sensitive(dialog->editButton, FALSE);
      gtk_widget_set_sensitive(dialog->removeButton, TRUE);
      break;
  }

  return;
}

static gboolean backspace_key_clicked(GtkWidget *widget, GdkEventKey *event, XmiMsimGuiLayerDialog *dialog) {
  if (event->keyval == gdk_keyval_from_name("BackSpace")) {
    remove_button_clicked(NULL, dialog);
    return TRUE;
  }
  return FALSE;
}
