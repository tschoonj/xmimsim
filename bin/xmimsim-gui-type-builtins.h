#ifndef __XMIMSIM_GUI_TYPE_BUILTINS_H__
#define __XMIMSIM_GUI_TYPE_BUILTINS_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS
GType xmi_msim_gui_compound_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG_TYPE (xmi_msim_gui_compound_dialog_type_get_type())
GType xmi_msim_gui_continuous_energy_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG_TYPE (xmi_msim_gui_continuous_energy_dialog_type_get_type())
GType xmi_msim_gui_discrete_energy_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG_TYPE (xmi_msim_gui_discrete_energy_dialog_type_get_type())
GType xmi_msim_gui_layer_box_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_LAYER_BOX_TYPE (xmi_msim_gui_layer_box_type_get_type())
GType xmi_msim_gui_layer_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_LAYER_DIALOG_TYPE (xmi_msim_gui_layer_dialog_type_get_type())
GType xmi_msim_gui_source_abstract_error_get_type (void);
#define XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT_ERROR_TYPE (xmi_msim_gui_source_abstract_error_get_type())
GType xmi_msim_gui_xmsi_selection_xpath_flags_get_type (void);
#define XMI_MSIM_GUI_TYPE_XMSI_SELECTION_XPATH_FLAGS (xmi_msim_gui_xmsi_selection_xpath_flags_get_type())

G_END_DECLS
#endif /* __XMIMSIM_GUI_TYPE_BUILTINS_H__ */
