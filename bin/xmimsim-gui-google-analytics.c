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

#include <config.h>
#include "xmimsim-gui-google-analytics.h"
#include "xmimsim-gui-marshal.h"
#include <libsoup/soup.h>
#include <glib.h>
#include <string.h>

#define GOOGLE_ANALYTICS_ENDPOINT "https://www.google-analytics.com/collect"
#define GOOGLE_ANALYTICS_TRACKING_ID "UA-42595764-3"
#define GOOGLE_ANALYTICS_APPLICATION_NAME "XMI-MSIM"
#define GOOGLE_ANALYTICS_APPLICATION_VERSION PACKAGE_VERSION
#define GOOGLE_ANALYTICS_HIT_TYPE "event"

enum {
	AFTER_EVENT,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL];

static XmiMsimGuiGoogleAnalyticsTracker *global_tracker = NULL;

struct _XmiMsimGuiGoogleAnalyticsTracker {
	GObject parent_instance;
	gchar *uuid;
	SoupSession *session;
	gboolean anonymize_ip;
};

G_DEFINE_TYPE(XmiMsimGuiGoogleAnalyticsTracker, xmi_msim_gui_google_analytics_tracker, G_TYPE_OBJECT)

static void xmi_msim_gui_google_analytics_tracker_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_gui_google_analytics_tracker_parent_class)->dispose(gobject);
}

static void xmi_msim_gui_google_analytics_tracker_finalize(GObject *gobject) {
	XmiMsimGuiGoogleAnalyticsTracker *tracker = XMI_MSIM_GUI_GOOGLE_ANALYTICS_TRACKER(gobject);

	g_free(tracker->uuid);
	g_object_unref(tracker->session);

	G_OBJECT_CLASS(xmi_msim_gui_google_analytics_tracker_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_google_analytics_tracker_class_init(XmiMsimGuiGoogleAnalyticsTrackerClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_google_analytics_tracker_dispose;
	object_class->finalize = xmi_msim_gui_google_analytics_tracker_finalize;
	
	signals[AFTER_EVENT] = g_signal_new(
		"after-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		xmi_msim_gui_VOID__POINTER,
		G_TYPE_NONE,
		1,
		G_TYPE_POINTER // GError *
	);

}

static void xmi_msim_gui_google_analytics_tracker_init(XmiMsimGuiGoogleAnalyticsTracker *self) {
	self->session = soup_session_new();
}

XmiMsimGuiGoogleAnalyticsTracker *xmi_msim_gui_google_analytics_tracker_new(const gchar *uuid, gboolean anonymize_ip) {
	XmiMsimGuiGoogleAnalyticsTracker *tracker = XMI_MSIM_GUI_GOOGLE_ANALYTICS_TRACKER(g_object_new(XMI_MSIM_GUI_TYPE_GOOGLE_ANALYTICS_TRACKER, NULL));
	
	// existing UUIDs must be valid
	if (uuid != NULL && !g_uuid_string_is_valid(uuid)) {
		g_critical("Invalid UUID string provided!");
		return NULL;
	}
	else if (uuid == NULL) {
		tracker->uuid = g_uuid_string_random();
	}
	else {
		tracker->uuid = g_strdup(uuid);
	}

	tracker->anonymize_ip = anonymize_ip;

	return tracker;
}

void xmi_msim_gui_google_analytics_tracker_create_global(const gchar *uuid, gboolean anonymize_ip) {
	if (global_tracker != NULL) 
		g_object_unref(global_tracker);

	global_tracker = xmi_msim_gui_google_analytics_tracker_new(uuid, anonymize_ip);
}

const XmiMsimGuiGoogleAnalyticsTracker *xmi_msim_gui_google_analytics_tracker_get_global() {
	return global_tracker;
}

void xmi_msim_gui_google_analytics_tracker_free_global() {
	if (global_tracker != NULL)
		g_object_unref(global_tracker);

	global_tracker = NULL;
}

static void event_callback(SoupSession *session, SoupMessage *msg, XmiMsimGuiGoogleAnalyticsTracker *tracker) {
	g_debug("event_callback status: %d", msg->status_code);
	if (SOUP_STATUS_IS_SUCCESSFUL(msg->status_code)) {
		g_signal_emit((gpointer) tracker, signals[AFTER_EVENT], 0, NULL);
	}
	else {
		g_warning("libsoup error message: %s", msg->reason_phrase);
		GError *error = g_error_new(XMI_MSIM_GUI_GOOGLE_ANALYTICS_TRACKER_ERROR, XMI_MSIM_GUI_GOOGLE_ANALYTICS_TRACKER_LIBSOUP, "libsoup error message: %s", msg->reason_phrase);
		g_signal_emit((gpointer) tracker, signals[AFTER_EVENT], 0, error);
	}
}

gboolean xmi_msim_gui_google_analytics_tracker_send_event(const XmiMsimGuiGoogleAnalyticsTracker *tracker, const gchar *category, const gchar *action, const gchar *label, const gchar *value) {
	
	g_return_val_if_fail(tracker != NULL, FALSE);
	g_return_val_if_fail(category != NULL, FALSE);
	g_return_val_if_fail(strlen(category) < 150, FALSE);
	g_return_val_if_fail(action != NULL, FALSE);
	g_return_val_if_fail(strlen(action) < 500, FALSE);
	g_return_val_if_fail(label == NULL || (label != NULL && strlen(label) < 500), FALSE);

	GHashTable *hash = g_hash_table_new(g_str_hash, g_str_equal);
	g_hash_table_replace(hash, "v", "1"); // protocol version
	g_hash_table_replace(hash, "tid", GOOGLE_ANALYTICS_TRACKING_ID); // tracking id
	if (tracker->anonymize_ip)
		g_hash_table_replace(hash, "aid", "1"); // anonymize ip
	g_hash_table_replace(hash, "cid", tracker->uuid); // client id
	g_hash_table_replace(hash, "t", GOOGLE_ANALYTICS_HIT_TYPE); // hit type
	g_hash_table_replace(hash, "an", GOOGLE_ANALYTICS_APPLICATION_NAME); // app name
	g_hash_table_replace(hash, "av", GOOGLE_ANALYTICS_APPLICATION_VERSION); // app version
	g_hash_table_replace(hash, "ec", (gpointer) category); // event category
	g_hash_table_replace(hash, "ea", (gpointer) action); // event action
	if (label != NULL)
		g_hash_table_replace(hash, "el", (gpointer) label); // event label
	if (value != NULL)
		g_hash_table_replace(hash, "ev", (gpointer) value); // event value


	SoupMessage *msg = soup_form_request_new_from_hash("POST", GOOGLE_ANALYTICS_ENDPOINT, hash); // generate message
	g_return_val_if_fail(msg != NULL, FALSE);
	soup_session_queue_message(tracker->session, msg, (SoupSessionCallback) event_callback, (gpointer) tracker); // queue message

	g_hash_table_destroy(hash);
	return TRUE;
}

GQuark xmi_msim_gui_google_analytics_tracker_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-google-analytics-tracker-error-quark");
}
