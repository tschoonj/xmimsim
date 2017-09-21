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

#ifndef XMI_MSIM_GUI_GOOGLE_ANALYTICS_H
#define XMI_MSIM_GUI_GOOGLE_ANALYTICS_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_GOOGLE_ANALYTICS_TRACKER xmi_msim_gui_google_analytics_tracker_get_type()

G_DECLARE_FINAL_TYPE(XmiMsimGuiGoogleAnalyticsTracker, xmi_msim_gui_google_analytics_tracker, XMI_MSIM_GUI, GOOGLE_ANALYTICS_TRACKER, GObject)

XmiMsimGuiGoogleAnalyticsTracker *xmi_msim_gui_google_analytics_tracker_new(const gchar *uuid);

void xmi_msim_gui_google_analytics_tracker_create_global(const gchar *uuid);

const XmiMsimGuiGoogleAnalyticsTracker *xmi_msim_gui_google_analytics_tracker_get_global();

void xmi_msim_gui_google_analytics_tracker_free_global();

gboolean xmi_msim_gui_google_analytics_tracker_send_event(const XmiMsimGuiGoogleAnalyticsTracker *tracker, const gchar *category, const gchar *action, const gchar *label, const gchar *value);

typedef enum {
	XMI_MSIM_GUI_GOOGLE_ANALYTICS_TRACKER_LIBSOUP,
} XmiMsimGuiGoogleAnalyticsTrackerError;

#define XMI_MSIM_GUI_GOOGLE_ANALYTICS_TRACKER_ERROR (xmi_msim_gui_google_analytics_tracker_error_quark())

GQuark xmi_msim_gui_google_analytics_tracker_error_quark(void);

G_END_DECLS

#endif
