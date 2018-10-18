


#include <config.h>
#include "xmi_google_analytics.h"

#define TEST_UUID "7c06f818-a4ba-467c-b045-d15e7ec79177"

typedef struct {
	GMainLoop *main_loop;
} SetupData;

static void setup_data(SetupData *data, gconstpointer user_data) {
	data->main_loop = g_main_loop_new(NULL, FALSE);
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
	g_main_loop_unref(data->main_loop);
}

static void after_event_cb(XmiMsimGoogleAnalyticsTracker *tracker, gchar *error_msg, SetupData *data) {
	g_assert_null(error_msg);
	g_main_loop_quit(data->main_loop);
}

static void test_send_from_local_tracker_fixed_uuid(SetupData *data, gconstpointer user_data) {
	XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_new(TEST_UUID);

	g_assert_nonnull(tracker);

	g_signal_connect(G_OBJECT(tracker), "after-event", G_CALLBACK(after_event_cb), data);

	g_assert_true(xmi_msim_google_analytics_tracker_send_event(tracker, "MAKE-CHECK-TEST", "LOCAL TRACKER FIXED UUID", NULL, NULL));

	g_main_loop_run(data->main_loop);
	g_object_unref(tracker);
}

static void test_send_from_global_tracker_fixed_uuid(SetupData *data, gconstpointer user_data) {
	xmi_msim_google_analytics_tracker_create_global(TEST_UUID);
	const XmiMsimGoogleAnalyticsTracker *tracker = xmi_msim_google_analytics_tracker_get_global();

	g_assert_nonnull(tracker);

	const XmiMsimGoogleAnalyticsTracker *tracker2 = xmi_msim_google_analytics_tracker_get_global();

	g_assert(tracker == tracker2);

	g_signal_connect(G_OBJECT(tracker), "after-event", G_CALLBACK(after_event_cb), data);

	g_assert_true(xmi_msim_google_analytics_tracker_send_event(tracker, "MAKE-CHECK-TEST", "GLOBAL TRACKER FIXED UUID", NULL, NULL));

	g_main_loop_run(data->main_loop);
	xmi_msim_google_analytics_tracker_free_global();
	g_assert_null(xmi_msim_google_analytics_tracker_get_global());
}

int main(int argc, char *argv[]) {
	SetupData *data;

	g_test_init(&argc, &argv, NULL);
	
	g_test_add("/google-analytics-tracker/send-from-local-tracker-fixed-uuid",
			SetupData,
			NULL,
			setup_data,
			test_send_from_local_tracker_fixed_uuid,
			teardown_data
			);
	g_test_add("/google-analytics-tracker/send-from-global-tracker-fixed-uuid",
			SetupData,
			NULL,
			setup_data,
			test_send_from_global_tracker_fixed_uuid,
			teardown_data
			);


	return g_test_run();
}
