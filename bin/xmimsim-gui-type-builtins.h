


#ifndef __XMIMSIM_GUI_TYPE_BUILTINS_H__
#define __XMIMSIM_GUI_TYPE_BUILTINS_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS

/* enumerations from "xmimsim-gui-compound-dialog.h" */
GType xmi_msim_gui_compound_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_COMPOUND_DIALOG_TYPE (xmi_msim_gui_compound_dialog_type_get_type())
/* enumerations from "xmimsim-gui-layer-dialog.h" */
GType xmi_msim_gui_layer_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_LAYER_DIALOG_TYPE (xmi_msim_gui_layer_dialog_type_get_type())
/* enumerations from "xmimsim-gui-discrete-energy-dialog.h" */
GType xmi_msim_gui_discrete_energy_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_DISCRETE_ENERGY_DIALOG_TYPE (xmi_msim_gui_discrete_energy_dialog_type_get_type())
/* enumerations from "xmimsim-gui-continuous-energy-dialog.h" */
GType xmi_msim_gui_continuous_energy_dialog_type_get_type (void);
#define XMI_MSIM_GUI_TYPE_CONTINUOUS_ENERGY_DIALOG_TYPE (xmi_msim_gui_continuous_energy_dialog_type_get_type())

G_END_DECLS
#endif /* __XMIMSIM_GUI_TYPE_BUILTINS_H__ */



