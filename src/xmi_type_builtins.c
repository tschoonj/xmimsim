


#include "xmi_type_builtins.h"
#include "xmi_job.h"
#include "xmi_batch.h"
#include "xmi_data_structs.h"
#include "xmi_error.h"

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

XMI_DEFINE_ENUM_TYPE(XmiMsimJobError, xmi_msim_job_error,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_ERROR_INVALID_INPUT, "invalid-input")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_ERROR_UNAVAILABLE, "unavailable")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_ERROR_ANOTHER_JOB_RUNNING, "another-job-running")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_ERROR_HDF5, "hdf5")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_ERROR_PROCESS, "process"))

XMI_DEFINE_ENUM_TYPE(XmiMsimBatchError, xmi_msim_batch_error,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_BATCH_ERROR_INVALID_INPUT, "invalid-input")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_BATCH_ERROR_UNAVAILABLE, "unavailable")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_BATCH_ERROR_ANOTHER_BATCH_RUNNING, "another-batch-running")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_BATCH_ERROR_METHOD_UNDEFINED, "method-undefined"))


XMI_DEFINE_ENUM_TYPE(XmiMsimJobSpecialEvent, xmi_msim_job_special_event,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE, "solid-angle")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_SPECIAL_EVENT_SIMULATION, "simulation")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS, "escape-peaks"))

// due to a bug/feature in GObject-Introspection, I cannot pass XmiMsim-prefixed TypeNames if the corresponding enum has been typedeffed into an Xmi-prefixed one...
XMI_DEFINE_ENUM_TYPE(XmiEnergyDiscreteDistribution, xmi_msim_energy_discrete_distribution,
	XMI_DEFINE_ENUM_VALUE(XMI_ENERGY_DISCRETE_DISTRIBUTION_MONOCHROMATIC, "monochromatic")
	XMI_DEFINE_ENUM_VALUE(XMI_ENERGY_DISCRETE_DISTRIBUTION_GAUSSIAN, "gaussian")
	XMI_DEFINE_ENUM_VALUE(XMI_ENERGY_DISCRETE_DISTRIBUTION_LORENTZIAN, "lorentzian"))

XMI_DEFINE_ENUM_TYPE(XmiDetectorConvolutionProfile, xmi_msim_detector_convolution_profile,
	XMI_DEFINE_ENUM_VALUE(XMI_DETECTOR_CONVOLUTION_PROFILE_SILI, "SiLi")
	XMI_DEFINE_ENUM_VALUE(XMI_DETECTOR_CONVOLUTION_PROFILE_GE, "Ge")
	XMI_DEFINE_ENUM_VALUE(XMI_DETECTOR_CONVOLUTION_PROFILE_SI_SDD, "SiSDD"))

XMI_DEFINE_FLAGS_TYPE(XmiInputFlags, xmi_msim_input_flags, 
	XMI_DEFINE_ENUM_VALUE(XMI_INPUT_GENERAL, "general")
	XMI_DEFINE_ENUM_VALUE(XMI_INPUT_COMPOSITION, "composition")
	XMI_DEFINE_ENUM_VALUE(XMI_INPUT_GEOMETRY, "geometry")
	XMI_DEFINE_ENUM_VALUE(XMI_INPUT_EXCITATION, "excitation")
	XMI_DEFINE_ENUM_VALUE(XMI_INPUT_ABSORBERS, "absorbers")
	XMI_DEFINE_ENUM_VALUE(XMI_INPUT_DETECTOR, "detector"))

XMI_DEFINE_ENUM_TYPE(XmiMsimError, xmi_msim_error,
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_ERROR_XML, "XML")
	XMI_DEFINE_ENUM_VALUE(XMI_MSIM_ERROR_TRANSMISSION_EFFICIENCY, "transmission-efficiency"))



