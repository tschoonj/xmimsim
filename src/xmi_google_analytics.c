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
#include "xmi_google_analytics.h"
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

enum {
	PROP_0,
	PROP_UUID
};

static guint signals[LAST_SIGNAL];

// this should probably be protected with a mutex...
static XmiMsimGoogleAnalyticsTracker *global_tracker = NULL;

struct _XmiMsimGoogleAnalyticsTracker {
	GObject parent_instance;
	gchar *uuid;
	SoupSession *session;
};

G_DEFINE_TYPE(XmiMsimGoogleAnalyticsTracker, xmi_msim_google_analytics_tracker, G_TYPE_OBJECT)

static void xmi_msim_google_analytics_tracker_dispose(GObject *gobject) {
	G_OBJECT_CLASS(xmi_msim_google_analytics_tracker_parent_class)->dispose(gobject);
}

static void xmi_msim_google_analytics_tracker_finalize(GObject *gobject) {
	XmiMsimGoogleAnalyticsTracker *tracker = XMI_MSIM_GOOGLE_ANALYTICS_TRACKER(gobject);

	g_free(tracker->uuid);
	g_object_unref(tracker->session);

	G_OBJECT_CLASS(xmi_msim_google_analytics_tracker_parent_class)->finalize(gobject);
}

static void xmi_msim_gui_google_analytics_tracker_set_property(GObject *object, guint prop_id, const GValue *value,  GParamSpec *pspec) {
	XmiMsimGoogleAnalyticsTracker *tracker = XMI_MSIM_GOOGLE_ANALYTICS_TRACKER(object);

	switch (prop_id) {
	case 1:
		tracker->uuid = g_value_dup_string(value);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
	}
}

static void xmi_msim_google_analytics_tracker_constructed(GObject *obj) {
	XmiMsimGoogleAnalyticsTracker *tracker = XMI_MSIM_GOOGLE_ANALYTICS_TRACKER(obj);

	// existing UUIDs must be valid
	if (tracker->uuid != NULL && !g_uuid_string_is_valid(tracker->uuid)) {
		g_critical("Invalid UUID string provided! Will use random UUID instead");
		g_free(tracker->uuid);
		tracker->uuid = g_uuid_string_random();
	}
	else if (tracker->uuid == NULL) {
		tracker->uuid = g_uuid_string_random();
	}
}

static void xmi_msim_google_analytics_tracker_class_init(XmiMsimGoogleAnalyticsTrackerClass *klass) {
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_google_analytics_tracker_dispose;
	object_class->finalize = xmi_msim_google_analytics_tracker_finalize;
	object_class->constructed = xmi_msim_google_analytics_tracker_constructed;
	object_class->set_property = xmi_msim_gui_google_analytics_tracker_set_property;
	
	g_object_class_install_property(object_class,
		PROP_UUID,
		g_param_spec_string("uuid",
		"Client UUID",
		"Client UUID",
		NULL,
		G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY)
	);

	/**
	 * XmiMsimGoogleAnalyticsTracker::after-event:
	 * @tracker: The #XmiMsimGoogleAnalyticsTracker object emitting the signal
	 * @message: if an error occurred, the string will contain an appropriate error message, otherwise it will be set to %NULL. The same message will also be printed on the console, even if the signal is not handled.
	 *
	 * Emitted after an tracking event has been sent to the Google Analytics servers.
	 */
	signals[AFTER_EVENT] = g_signal_new(
		"after-event",
		G_TYPE_FROM_CLASS(klass),
		G_SIGNAL_RUN_LAST,
		0, // no default handler
		NULL,
		NULL,
		NULL,
		G_TYPE_NONE,
		1,
		G_TYPE_STRING // string with error message or NULL
	);
}

static void xmi_msim_google_analytics_tracker_init(XmiMsimGoogleAnalyticsTracker *self) {
	self->session = soup_session_new();
}

/**
 * xmi_msim_google_analytics_tracker_new: (constructor)
 * @uuid: (nullable): a valid UUID, or %NULL
 *
 * Instantiate a new google analytics tracker object
 *
 * Returns: (transfer full): the tracker
 */
XmiMsimGoogleAnalyticsTracker *xmi_msim_google_analytics_tracker_new(const gchar *uuid) {
	return XMI_MSIM_GOOGLE_ANALYTICS_TRACKER(g_object_new(XMI_MSIM_TYPE_GOOGLE_ANALYTICS_TRACKER, "uuid", uuid, NULL));
}

/**
 * xmi_msim_google_analytics_tracker_create_global:
 * @uuid: (nullable): a valid UUID, or %NULL
 *
 * Instantiate a new global google analytics tracker object
 */
void xmi_msim_google_analytics_tracker_create_global(const gchar *uuid) {
	if (global_tracker != NULL) 
		g_object_unref(global_tracker);

	global_tracker = xmi_msim_google_analytics_tracker_new(uuid);
}

/**
 * xmi_msim_google_analytics_tracker_get_global:
 *
 * Get the global google analytics tracker object, or %NULL if it hasn't been created yet
 *
 * Returns: (transfer none): the global tracker
 */
XmiMsimGoogleAnalyticsTracker *xmi_msim_google_analytics_tracker_get_global(void) {
	return global_tracker;
}

/**
 * xmi_msim_google_analytics_tracker_free_global:
 *
 * Free the global google analytics tracker object
 */
void xmi_msim_google_analytics_tracker_free_global(void) {
	if (global_tracker != NULL)
		g_object_unref(global_tracker);

	global_tracker = NULL;
}

static void event_callback(SoupSession *session, SoupMessage *msg, XmiMsimGoogleAnalyticsTracker *tracker) {
	if (SOUP_STATUS_IS_SUCCESSFUL(msg->status_code)) {
		g_signal_emit((gpointer) tracker, signals[AFTER_EVENT], 0, NULL);
	}
	else {
		g_warning("libsoup error message: %s", msg->reason_phrase);
		g_signal_emit((gpointer) tracker, signals[AFTER_EVENT], 0, msg->reason_phrase);
	}
}

/**
 * xmi_msim_google_analytics_tracker_send_event:
 * @tracker: (not nullable): the tracker
 * @category: (not nullable): the event category (must be less than 150 characters)
 * @action: (not nullable): the event action (must be less than 500 characters)
 * @label: (nullable): the event action (if present, must be less than 500 characters)
 * @value: (nullable): the event value (if present, must be less than 500 characters)
 *
 * Returns: %TRUE if the message was sent (does not mean it was received!), %FALSE otherwise
 *
 */
gboolean xmi_msim_google_analytics_tracker_send_event(XmiMsimGoogleAnalyticsTracker *tracker, const gchar *category, const gchar *action, const gchar *label, const gchar *value) {
	g_return_val_if_fail(tracker != NULL, FALSE);
	g_return_val_if_fail(category != NULL, FALSE);
	g_return_val_if_fail(strlen(category) < 150, FALSE);
	g_return_val_if_fail(action != NULL, FALSE);
	g_return_val_if_fail(strlen(action) < 500, FALSE);
	g_return_val_if_fail(label == NULL || (label != NULL && strlen(label) < 500), FALSE);

	GHashTable *hash = g_hash_table_new(g_str_hash, g_str_equal);
	g_hash_table_replace(hash, "v", "1"); // protocol version
	g_hash_table_replace(hash, "tid", GOOGLE_ANALYTICS_TRACKING_ID); // tracking id
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
