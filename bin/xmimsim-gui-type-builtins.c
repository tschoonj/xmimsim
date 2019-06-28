#include "xmimsim-gui-type-builtins.h"
#include "xmimsim-gui-layer-box.h"
#include "xmimsim-gui-compound-dialog.h"
#include "xmimsim-gui-layer-dialog.h"
#include "xmimsim-gui-discrete-energy-dialog.h"
#include "xmimsim-gui-continuous-energy-dialog.h"
#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-xmsi-selection-scrolled-window.h"

/* inspired by https://github.com/endlessm/xapian-glib/blob/master/xapian-glib/xapian-enums.cc#L22 */

#define XMI_DEFINE_ENUM_VALUE(value,nick) \
	  { value, #value, nick },

#define XMI_DEFINE_ENUM_VALUE_FULL(value,value_string,nick) \
	  { value, #value_string, nick },

#define XMI_DEFINE_ENUM_TYPE(TypeName,type_name,values) \
	GType \
	type_name ## _get_type (void) \
{ \
	  static volatile gsize g_define_id__volatile = 0; \
	  if (g_once_init_enter (&g_define_id__volatile)) \
	    { \
		          static const GEnumValue v[] = { \
				          values \
				          { 0, NULL, NULL }, \
				        }; \
		          GType g_define_id = g_enum_register_static (g_intern_static_string (#TypeName), v); \
		          g_once_init_leave (&g_define_id__volatile, g_define_id); \
		        } \
	  return g_define_id__volatile; \
}

#define XMI_DEFINE_FLAGS_TYPE(TypeName,type_name,values) \
	GType \
	type_name ## _get_type (void) \
{ \
	  static volatile gsize g_define_id__volatile = 0; \
	  if (g_once_init_enter (&g_define_id__volatile)) \
	    { \
		          static const GFlagsValue v[] = { \
				          values \
				          { 0, NULL, NULL }, \
				        }; \
		          GType g_define_id = g_flags_register_static (g_intern_static_string (#TypeName), v); \
		          g_once_init_leave (&g_define_id__volatile, g_define_id); \
		        } \
	  return g_define_id__volatile; \
}

XMI_DEFINE_ENUM_TYPE(XmiMsimGuiCompoundDialogType, xmi_msim_gui_compound_dialog_type,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_COMPOUND_DIALOG_TYPE_ADD, "add")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_COMPOUND_DIALOG_TYPE_EDIT, "edit"))

XMI_DEFINE_ENUM_TYPE(XmiMsimGuiContinuousEnergyDialogType, xmi_msim_gui_continuous_energy_dialog_type,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_TYPE_ADD, "add")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_CONTINUOUS_ENERGY_DIALOG_TYPE_EDIT, "edit"))

XMI_DEFINE_ENUM_TYPE(XmiMsimGuiDiscreteEnergyDialogType, xmi_msim_gui_discrete_energy_dialog_type,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_TYPE_ADD, "add")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_DISCRETE_ENERGY_DIALOG_TYPE_EDIT, "edit"))

XMI_DEFINE_ENUM_TYPE(XmiMsimGuiLayerBoxType, xmi_msim_gui_layer_box_type,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_LAYER_BOX_TYPE_SAMPLE_COMPOSITION, "sample-composition")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_LAYER_BOX_TYPE_EXCITATION_ABSORBERS, "excitation-absorbers")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_LAYER_BOX_TYPE_DETECTOR_ABSORBERS, "detector-absorbers")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_LAYER_BOX_TYPE_CRYSTAL_COMPOSITION, "crystal-composition"))

XMI_DEFINE_ENUM_TYPE(XmiMsimGuiLayerDialogType, xmi_msim_gui_layer_dialog_type,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_LAYER_DIALOG_TYPE_ADD, "add")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_LAYER_DIALOG_TYPE_EDIT, "edit"))

XMI_DEFINE_ENUM_TYPE(XmiMsimGuiSourceAbstractError, xmi_msim_gui_source_abstract_error,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED, "method-undefined")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_INVALID_FILENAME, "invalid-filename")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_NO_RAW_DATA, "no-raw-data"))

XMI_DEFINE_FLAGS_TYPE(XmiMsimGuiXmsiSelectionXPathFlags, xmi_msim_gui_xmsi_selection_xpath_flags,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_XMSI_SELECTION_XPATH_DOUBLE, "double")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_XMSI_SELECTION_XPATH_INT, "int")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_XMSI_SELECTION_XPATH_LONG, "long")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_XMSI_SELECTION_XPATH_STRICT_POSITIVE, "strict-positive")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_XMSI_SELECTION_XPATH_POSITIVE, "positive")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_GUI_XMSI_SELECTION_XPATH_WEIGHT_FRACTION, "weight-fraction"))
