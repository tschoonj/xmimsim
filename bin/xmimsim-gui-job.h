/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

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


#include <glib-object.h>
#include "xmi_main.h"

#ifndef XMI_MSIM_GUI_JOB_H
#define XMI_MSIM_GUI_JOB_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_JOB                  (xmi_msim_gui_job_get_type())
#define XMI_MSIM_GUI_JOB(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_JOB, XmiMsimGuiJob))
#define XMI_MSIM_GUI_JOB_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_JOB, XmiMsimGuiJobClass))
#define XMI_MSIM_GUI_IS_JOB(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_JOB))
#define XMI_MSIM_GUI_IS_JOB_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_JOB))
#define XMI_MSIM_GUI_JOB_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_JOB, XmiMsimGuiJobClass))

typedef struct _XmiMsimGuiJob		XmiMsimGuiJob;
typedef struct _XmiMsimGuiJobClass   	XmiMsimGuiJobClass;

XmiMsimGuiJob* xmi_msim_gui_job_new(
	const gchar *xmi_msim_executable,
	const gchar *xmsifile,
	struct xmi_main_options *options,
	const gchar *spe_conv,
	const gchar *csv_conv,
	const gchar *svg_conv,
	const gchar *html_conv,
	int nthreads,
	const gchar **extra_options,
	GError **error
	);

gboolean xmi_msim_gui_job_start(XmiMsimGuiJob *job, GError **error);
gboolean xmi_msim_gui_job_stop(XmiMsimGuiJob *job, GError **error);
gboolean xmi_msim_gui_job_kill(XmiMsimGuiJob *job, GError **error);
gboolean xmi_msim_gui_job_suspend(XmiMsimGuiJob *job, GError **error);
gboolean xmi_msim_gui_job_resume(XmiMsimGuiJob *job, GError **error);
gboolean xmi_msim_gui_job_is_running(XmiMsimGuiJob *job); // if paused, it will return TRUE!
gboolean xmi_msim_gui_job_is_suspended(XmiMsimGuiJob *job);
gboolean xmi_msim_gui_job_has_finished(XmiMsimGuiJob *job);
gboolean xmi_msim_gui_job_was_successful(XmiMsimGuiJob *job);

gboolean xmi_msim_gui_job_get_pid(XmiMsimGuiJob *job, gint *pid, GError **error);

void xmi_msim_gui_job_send_all_stdout_events(XmiMsimGuiJob *job, gboolean setting);

void xmi_msim_gui_job_kill_all();

gboolean xmi_msim_gui_job_is_suspend_available();

typedef enum {
	XMI_MSIM_GUI_JOB_ERROR_INVALID_INPUT,
	XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE,
	XMI_MSIM_GUI_JOB_ERROR_ANOTHER_JOB_RUNNING,
	XMI_MSIM_GUI_JOB_ERROR_HDF5,
	XMI_MSIM_GUI_JOB_ERROR_PROCESS,
} XmiMsimGuiJobError;

typedef enum {
	XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE,
	XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SIMULATION,
	XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS
} XmiMsimGuiJobSpecialEvent;

#define XMI_MSIM_GUI_JOB_ERROR (xmi_msim_gui_job_error_quark())

GQuark xmi_msim_gui_job_error_quark(void);

GType xmi_msim_gui_job_get_type(void) G_GNUC_CONST;

G_END_DECLS

#endif

