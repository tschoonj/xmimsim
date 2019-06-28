/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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


#ifndef XMI_MSIM_BATCH_H
#define XMI_MSIM_BATCH_H

#include "xmi_job.h"

G_BEGIN_DECLS

#define XMI_MSIM_TYPE_BATCH_ABSTRACT                  (xmi_msim_batch_abstract_get_type())
#define XMI_MSIM_BATCH_ABSTRACT(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_TYPE_BATCH_ABSTRACT, XmiMsimBatchAbstract))
#define XMI_MSIM_BATCH_ABSTRACT_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_TYPE_BATCH_ABSTRACT, XmiMsimBatchAbstractClass))
#define XMI_MSIM_IS_BATCH_ABSTRACT(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_TYPE_BATCH_ABSTRACT))
#define XMI_MSIM_IS_BATCH_ABSTRACT_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_TYPE_BATCH_ABSTRACT))
#define XMI_MSIM_BATCH_ABSTRACT_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_TYPE_BATCH_ABSTRACT, XmiMsimBatchAbstractClass))

typedef struct _XmiMsimBatchAbstract		XmiMsimBatchAbstract;
typedef struct _XmiMsimBatchAbstractPrivate	XmiMsimBatchAbstractPrivate;
typedef struct _XmiMsimBatchAbstractClass   	XmiMsimBatchAbstractClass;

struct _XmiMsimBatchAbstract
{
  GObject parent_instance;
  XmiMsimBatchAbstractPrivate *priv;
};

struct _XmiMsimBatchAbstractClass
{
  GObjectClass parent_class;

  XmiMsimJob* (*get_job) (XmiMsimBatchAbstract *batch, guint job_index, GError **error);
  guint (*get_number_of_jobs) (XmiMsimBatchAbstract *batch);
};

gboolean xmi_msim_batch_abstract_start(XmiMsimBatchAbstract *batch, GError **error);
gboolean xmi_msim_batch_abstract_stop(XmiMsimBatchAbstract *batch, GError **error);
gboolean xmi_msim_batch_abstract_kill(XmiMsimBatchAbstract *batch, GError **error);
gboolean xmi_msim_batch_abstract_suspend(XmiMsimBatchAbstract *batch, GError **error);
gboolean xmi_msim_batch_abstract_resume(XmiMsimBatchAbstract *batch, GError **error);
gboolean xmi_msim_batch_abstract_is_running(XmiMsimBatchAbstract *batch); // if paused, it will return TRUE!
gboolean xmi_msim_batch_abstract_is_suspended(XmiMsimBatchAbstract *batch);
gboolean xmi_msim_batch_abstract_has_finished(XmiMsimBatchAbstract *batch);
gboolean xmi_msim_batch_abstract_was_successful(XmiMsimBatchAbstract *batch);

void xmi_msim_batch_abstract_send_all_stdout_events(XmiMsimBatchAbstract *batch, gboolean setting);
void xmi_msim_batch_abstract_set_executable(XmiMsimBatchAbstract *batch, const gchar *executable);
void xmi_msim_batch_abstract_set_extra_options(XmiMsimBatchAbstract *batch, gchar **extra_options);

typedef enum {
	XMI_MSIM_BATCH_ERROR_INVALID_INPUT,
	XMI_MSIM_BATCH_ERROR_UNAVAILABLE,
	XMI_MSIM_BATCH_ERROR_ANOTHER_BATCH_RUNNING,
	XMI_MSIM_BATCH_ERROR_METHOD_UNDEFINED,
} XmiMsimBatchError;

#define XMI_MSIM_BATCH_ERROR (xmi_msim_batch_error_quark())

GQuark xmi_msim_batch_error_quark(void);

GType xmi_msim_batch_abstract_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_BATCH_SINGLE                  (xmi_msim_batch_single_get_type())
#define XMI_MSIM_BATCH_SINGLE(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_TYPE_BATCH_SINGLE, XmiMsimBatchSingle))
#define XMI_MSIM_BATCH_SINGLE_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_TYPE_BATCH_SINGLE, XmiMsimBatchSingleClass))
#define XMI_MSIM_IS_BATCH_SINGLE(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_TYPE_BATCH_SINGLE))
#define XMI_MSIM_IS_BATCH_SINGLE_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_TYPE_BATCH_SINGLE))
#define XMI_MSIM_BATCH_SINGLE_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_TYPE_BATCH_SINGLE, XmiMsimBatchSingleClass))

typedef struct _XmiMsimBatchSingle		XmiMsimBatchSingle;
typedef struct _XmiMsimBatchSingleClass   	XmiMsimBatchSingleClass;

typedef struct _xmi_batch_single_data xmi_batch_single_data;

/**
 * xmi_batch_single_data:
 * @xpath: XPath expression
 * @start: start value
 * @end: end value
 * @nsteps: number of steps to go from start to end
 *
 * A struct describing a single parameter for a XmiMsimBatchSingle task.
 */
struct _xmi_batch_single_data {
	gchar *xpath;
	gdouble start;
	gdouble end;
	guint nsteps;
};

void xmi_batch_single_data_copy(xmi_batch_single_data *A, xmi_batch_single_data **B);

void xmi_batch_single_data_free(xmi_batch_single_data *A);

XmiMsimBatchAbstract* xmi_msim_batch_single_new(const gchar *xmsi_base_file, GPtrArray *data, xmi_main_options *options, GError **error);

gboolean xmi_msim_batch_single_write_archive(XmiMsimBatchSingle *batch, const char *xmsa_file, GError **error);

GType xmi_msim_batch_single_get_type(void) G_GNUC_CONST;

#define XMI_MSIM_TYPE_BATCH_MULTI                  (xmi_msim_batch_multi_get_type())
#define XMI_MSIM_BATCH_MULTI(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_TYPE_BATCH_MULTI, XmiMsimBatchMulti))
#define XMI_MSIM_BATCH_MULTI_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_TYPE_BATCH_MULTI, XmiMsimBatchMultiClass))
#define XMI_MSIM_IS_BATCH_MULTI(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_TYPE_BATCH_MULTI))
#define XMI_MSIM_IS_BATCH_MULTI_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_TYPE_BATCH_MULTI))
#define XMI_MSIM_BATCH_MULTI_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_TYPE_BATCH_MULTI, XmiMsimBatchMultiClass))

typedef struct _XmiMsimBatchMulti		XmiMsimBatchMulti;
typedef struct _XmiMsimBatchMultiClass   	XmiMsimBatchMultiClass;

XmiMsimBatchAbstract* xmi_msim_batch_multi_new(GPtrArray* xmsi_files, GPtrArray* options);

GType xmi_msim_batch_multi_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif

