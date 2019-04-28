#include <config.h>
#include "xmi_job.h"
#include "xmi_msim.h"
#include "libxmimsim-test.h"
#include <unistd.h>

#define COMPOUND "C6H12O6" // sweet, sweet sugar

gchar *extra_options[4] = {
	"--with-hdf5-data=xmimsimdata-" COMPOUND ".h5",
	"--with-solid-angles-data=solid-angles.h5",
	"--with-escape-ratios-data=escape-ratios.h5",
	NULL
};

typedef struct {
	GMainLoop *main_loop;
	xmi_main_options *options;
	gboolean line_found;
	gchar *line;
} SetupData;

static void setup_data(SetupData *data, gconstpointer user_data) {
	data->main_loop = g_main_loop_new(NULL, FALSE);
	xmi_input *input = xmi_input_init_empty();
	// simulate 10M photons brute force
	input->general->n_photons_line = 100000;
	data->options = xmi_main_options_new();
	data->options->use_variance_reduction = FALSE; // brute force!
	data->options->use_escape_peaks = FALSE; // no escape peaks!

	struct compoundData *cd = (struct compoundData *) user_data;
	// add compound to composition
	xmi_layer *layer = compoundData2xmi_layer(cd);
	layer->thickness = 1.0;
	layer->density = 1.0;
	input->composition->n_layers = 1;
	input->composition->layers = layer;
	input->composition->reference_layer = 1;

	input->general->outputfile = g_strdup(COMPOUND "-test.xmso");
	g_assert(xmi_input_validate(input) == 0);
	g_assert_true(xmi_input_write_to_xml_file(input, COMPOUND "-test.xmsi", NULL));
	xmi_input_free(input);
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
	g_main_loop_unref(data->main_loop);
	xmi_main_options_free(data->options);
	g_free(data->line);
}

static void test_fail_finished_cb(XmiMsimJob *job, gboolean result, const gchar *buffer, SetupData *data) {
	g_assert_false(result);
	g_debug("message: %s", buffer);
	g_main_loop_quit(data->main_loop);
}

static void test_succeed_finished_cb(XmiMsimJob *job, gboolean result, const gchar *buffer, SetupData *data) {
	g_assert_true(result);
	g_debug("message: %s", buffer);
	g_main_loop_quit(data->main_loop);
}

static void print_stdout(XmiMsimJob *job, const gchar *string, SetupData *data) {
	g_debug("stdout: %s", string);
	if (g_strcmp0(string, data->line) == 0)
		data->line_found = TRUE;
}

static void print_stderr(XmiMsimJob *job, const gchar *string) {
	g_debug("stderr: %s", string);
}

static void test_special_event_cb(XmiMsimJob *job, XmiMsimJobSpecialEvent event, const gchar *buffer, gpointer data) {
	g_assert_true(xmi_msim_job_is_running(job));
	// this test is running in brute force mode, so we know exactly what to expect
	if (event == XMI_MSIM_JOB_SPECIAL_EVENT_SOLID_ANGLE)
		g_assert(strcmp(buffer, "Solid angle grid redundant") == 0);
	else if (event == XMI_MSIM_JOB_SPECIAL_EVENT_ESCAPE_PEAKS)
		g_assert(strcmp(buffer, "Escape peaks redundant") == 0);
	g_debug("special_event: %s", buffer);
}

static void test_custom_detector_response_non_existent_plugin(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;
	data->options->custom_detector_response = g_strdup("non-existent-plugin." G_MODULE_SUFFIX);

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), data);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	g_assert_true(xmi_msim_job_start(job, &error));

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_job_is_running(job));
	g_assert_false(xmi_msim_job_is_suspended(job));
	g_assert_true(xmi_msim_job_has_finished(job));
	g_assert_false(xmi_msim_job_was_successful(job));
	g_assert_false(xmi_msim_job_stop(job, NULL));
	g_assert_false(xmi_msim_job_suspend(job, NULL));
	g_assert_false(xmi_msim_job_resume(job, NULL));

	g_assert_false(xmi_msim_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
	//g_assert(unlink(COMPOUND "-test.xmso") == 0);
}

static void test_custom_detector_response_plugin1(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;
	data->options->custom_detector_response = g_strdup("../custom-detector-response/.libs/detector-response1." G_MODULE_SUFFIX);
	data->line = g_strdup("Entering detector-response1: xmi_detector_convolute_all_custom");

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_succeed_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), data);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	g_assert_true(xmi_msim_job_start(job, &error));

	g_main_loop_run(data->main_loop);

	g_assert_true(data->line_found);

	g_assert_false(xmi_msim_job_is_running(job));
	g_assert_false(xmi_msim_job_is_suspended(job));
	g_assert_true(xmi_msim_job_has_finished(job));
	g_assert_true(xmi_msim_job_was_successful(job));
	g_assert_false(xmi_msim_job_stop(job, NULL));
	g_assert_false(xmi_msim_job_suspend(job, NULL));
	g_assert_false(xmi_msim_job_resume(job, NULL));

	g_assert_false(xmi_msim_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
	g_assert(unlink(COMPOUND "-test.xmso") == 0);
}

static void test_custom_detector_response_plugin2(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;
	data->options->custom_detector_response = g_strdup("../custom-detector-response/.libs/detector-response2." G_MODULE_SUFFIX);
	data->line = g_strdup("Entering detector-response2: xmi_detector_convolute_all_custom");

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_succeed_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), data);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	g_assert_true(xmi_msim_job_start(job, &error));

	g_main_loop_run(data->main_loop);

	g_assert_true(data->line_found);

	g_assert_false(xmi_msim_job_is_running(job));
	g_assert_false(xmi_msim_job_is_suspended(job));
	g_assert_true(xmi_msim_job_has_finished(job));
	g_assert_true(xmi_msim_job_was_successful(job));
	g_assert_false(xmi_msim_job_stop(job, NULL));
	g_assert_false(xmi_msim_job_suspend(job, NULL));
	g_assert_false(xmi_msim_job_resume(job, NULL));

	g_assert_false(xmi_msim_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
	g_assert(unlink(COMPOUND "-test.xmso") == 0);
}

int main(int argc, char *argv[]) {
	SetupData *data;

	setbuf(stdout,NULL);
#ifdef G_OS_WIN32
	setbuf(stderr,NULL);
#endif
	g_test_init(&argc, &argv, NULL);

	g_assert_true(xmi_msim_job_is_suspend_available());

	// init test
	g_assert(test_init() == 1);

	// start by creating our data file
	xmi_init_hdf5();

	// read compound
	struct compoundData *cd = CompoundParser(COMPOUND, NULL);
	g_assert(cd != NULL);

	// generate appropriate xmimsimdata.h5 file
	gchar data_file[] = "xmimsimdata-" COMPOUND ".h5";
	g_assert(xmi_db(data_file, cd->Elements, cd->nElements));

	g_test_add("/custom-detector-response/non-existent-plugin",
			SetupData,
			cd,
			setup_data,
			test_custom_detector_response_non_existent_plugin,
			teardown_data
			);
	g_test_add("/custom-detector-response/plugin1",
			SetupData,
			cd,
			setup_data,
			test_custom_detector_response_plugin1,
			teardown_data
			);
	g_test_add("/custom-detector-response/plugin2",
			SetupData,
			cd,
			setup_data,
			test_custom_detector_response_plugin2,
			teardown_data
			);
	int rv = g_test_run();

	// cleanup
	FreeCompoundData(cd);
	unlink(data_file);

	return rv;
}
