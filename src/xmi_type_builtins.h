


#ifndef __XMIMSIM_TYPE_BUILTINS_H__
#define __XMIMSIM_TYPE_BUILTINS_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS
GType xmi_msim_job_error_get_type (void);
#define XMI_MSIM_TYPE_JOB_ERROR (xmi_msim_job_error_get_type())
GType xmi_msim_job_special_event_get_type (void);
#define XMI_MSIM_TYPE_JOB_SPECIAL_EVENT (xmi_msim_job_special_event_get_type())
GType xmi_msim_energy_discrete_distribution_get_type (void);
#define XMI_MSIM_TYPE_ENERGY_DISCRETE_DISTRIBUTION (xmi_msim_energy_discrete_distribution_get_type())

G_END_DECLS
#endif /* __XMIMSIM_TYPE_BUILTINS_H__ */



