


#include "xmimsim-gui-type-builtins.h"
#include "xmimsim-gui-compound-dialog.h"
#include "xmimsim-gui-layer-dialog.h"
#include "xmimsim-gui-source-abstract.h"

/* enumerations from "xmimsim-gui-compound-dialog.h" */
GType
xmi_msim_gui_compound_dialog_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { XMI_MSIM_GUI_COMPOUND_DIALOG_ADD, "XMI_MSIM_GUI_COMPOUND_DIALOG_ADD", "add" },
      { XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT, "XMI_MSIM_GUI_COMPOUND_DIALOG_EDIT", "edit" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static(g_intern_static_string("XmiMsimGuiCompoundDialogType"), values);
  }
  return etype;
}

/* enumerations from "xmimsim-gui-layer-dialog.h" */
GType
xmi_msim_gui_layer_dialog_type_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { XMI_MSIM_GUI_LAYER_DIALOG_ADD, "XMI_MSIM_GUI_LAYER_DIALOG_ADD", "add" },
      { XMI_MSIM_GUI_LAYER_DIALOG_EDIT, "XMI_MSIM_GUI_LAYER_DIALOG_EDIT", "edit" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static(g_intern_static_string("XmiMsimGuiLayerDialogType"), values);
  }
  return etype;
}

/* enumerations from "xmimsim-gui-source-abstract.h" */
GType
xmi_msim_gui_source_abstract_error_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED, "XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED", "undefined" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static(g_intern_static_string("XmiMsimGuiSourceAbstractError"), values);
  }
  return etype;
}



