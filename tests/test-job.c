#include <config.h>
#include "xmi_job.h"
#include "xmi_msim.h"
#include "libxmimsim-test.h"
#include <unistd.h>
#include <glib/gstdio.h>

#define COMPOUND "C6H12O6" // sweet, sweet sugar

gchar *extra_options[4] = {
	"--with-hdf5-data=xmimsimdata-" COMPOUND "\u03B1.h5",
	"--with-solid-angles-data=solid-angles\u03B1.h5",
	"--with-escape-ratios-data=escape-ratios\u03B1.h5",
	NULL
};

typedef struct {
	GMainLoop *main_loop;
	xmi_input *input;
	xmi_main_options *options;
} SetupData;

static void setup_data(SetupData *data, gconstpointer user_data) {
	data->main_loop = g_main_loop_new(NULL, FALSE);
	data->input = xmi_input_init_empty();
	// simulate 10M photons brute force
	data->input->general->n_photons_line = 10000000;
	data->options = xmi_main_options_new();
	data->options->use_variance_reduction = FALSE; // brute force!
	data->options->use_escape_peaks = FALSE; // no escape peaks!

	struct compoundData *cd = (struct compoundData *) user_data;
	// add compound to composition
	xmi_layer *layer = compoundData2xmi_layer(cd);
	layer->thickness = 1.0;
	layer->density = 1.0;
	data->input->composition->n_layers = 1;
	data->input->composition->layers = layer;
	data->input->composition->reference_layer = 1;
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
	g_main_loop_unref(data->main_loop);
	xmi_input_free(data->input);
	xmi_main_options_free(data->options);
}

static void test_no_executable(SetupData *data, gconstpointer user_data) {
	// write input to file -> not necessary here...
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test1\u03B1.xmso");
	g_assert(xmi_input_validate(data->input) == 0);
	g_assert_true(xmi_input_write_to_xml_file(data->input, COMPOUND "-test1\u03B1.xmsi", &error));

	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_NON_EXISTENT_EXEC"),
		COMPOUND "-test1\u03B1.xmsi",
		data->options,
		NULL, NULL, NULL,
		extra_options
		);
	g_assert_nonnull(job);
	g_debug("command: %s", xmi_msim_job_get_command(job));
	g_assert_false(xmi_msim_job_start(job, &error));
	g_assert_nonnull(error);

	g_debug("message: %s", error->message);
	g_debug("code: %d", error->code);
	g_assert(error->domain == G_SPAWN_ERROR);
#ifdef G_OS_WIN32
	/* in glib 2.58.0 this was fixed so it returns G_SPAWN_ERROR_NOENT, just as on Linux and macOS */
	g_assert(error->code == G_SPAWN_ERROR_FAILED || error->code == G_SPAWN_ERROR_NOENT);
#else
	g_assert(error->code == G_SPAWN_ERROR_NOENT);
#endif
	g_assert_false(xmi_msim_job_is_running(job));
	g_assert_false(xmi_msim_job_is_suspended(job));
	g_assert_false(xmi_msim_job_has_finished(job));
	g_assert_false(xmi_msim_job_was_successful(job));
	g_assert_false(xmi_msim_job_stop(job, NULL));
	g_assert_false(xmi_msim_job_suspend(job, NULL));
	g_assert_false(xmi_msim_job_resume(job, NULL));

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);

	g_assert_cmpint(g_unlink(COMPOUND "-test1\u03B1.xmsi"), ==, 0);
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

static void print_stdout(XmiMsimJob *job, const gchar *string) {
	g_debug("stdout: %s", string);
}

static void print_stderr(XmiMsimJob *job, const gchar *string) {
	g_debug("stderr: %s", string);
}

static void test_no_input_file(SetupData *data, gconstpointer user_data) {
	g_test_log_set_fatal_handler(test_log_fatal_false, NULL);
	GError *error = NULL;

	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		"non-existent-file\u03B1.xmsi",
		data->options,
		NULL, NULL, NULL,
		extra_options
		);
	g_assert_nonnull(job);

	g_assert_false(xmi_msim_job_start(job, &error));

	g_assert(error->domain == XMI_MSIM_ERROR);
	g_assert(error->code == XMI_MSIM_ERROR_XML);

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);
	g_error_free(error);
}

static void test_bad_input_file(SetupData *data, gconstpointer user_data) {
	g_test_log_set_fatal_handler(test_log_fatal_false, NULL);
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test3\u03B1.xmso");
	g_assert(xmi_input_validate(data->input) == 0);
	// file is valid now, let's make it invalid...
	data->input->composition->reference_layer = 5;
	g_assert(xmi_input_validate(data->input) != 0);
	g_assert_true(xmi_input_write_to_xml_file(data->input, COMPOUND "-test3\u03B1.xmsi", &error));

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test3\u03B1.xmsi",
		data->options,
		NULL, NULL, NULL,
		extra_options
		);

	g_assert_nonnull(job);
	g_assert_false(xmi_msim_job_start(job, &error));

	g_assert(error->domain == XMI_MSIM_ERROR);
	g_assert(error->code == XMI_MSIM_ERROR_XML);
	g_assert_cmpstr(error->message, ==, "invalid reference_layer value detected");

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);
	g_error_free(error);

	g_assert_cmpint(g_unlink(COMPOUND "-test3\u03B1.xmsi"), ==, 0);
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

static void test_good_input_file_simple(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;
	const gchar *xmsi_file = COMPOUND "-test4\u03B1.xmsi";
	const gchar *xmso_file = COMPOUND "-test4\u03B1.xmso";
	const gchar *spe_conv_prefix = COMPOUND "-test4\u03B2";
	const gchar *csv_conv = COMPOUND "-test4\u03B3.csv";
	const gchar *html_conv = COMPOUND "-test4\u03B5.html";


	data->input->general->outputfile = g_strdup(xmso_file);
	g_assert(xmi_input_validate(data->input) == 0);
	g_assert_true(xmi_input_write_to_xml_file(data->input, xmsi_file, &error));

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		xmsi_file,
		data->options,
		spe_conv_prefix,
		csv_conv,
		html_conv,
		extra_options
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_succeed_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	g_assert_true(xmi_msim_job_start(job, &error));

	g_main_loop_run(data->main_loop);

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

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);

	// test output
  	xmi_output *output = NULL;
  	g_assert_nonnull(output = xmi_output_read_from_xml_file(xmso_file, NULL));
	
  	test_compare_channels_and_csv(output->input->detector->nchannels, output->channels_conv, csv_conv);

	unsigned int i;

  	for (i = output->use_zero_interactions ? 0 : 1 ; i <= output->ninteractions ; i++) {
		gchar *spe_file = g_strdup_printf("%s_%d.spe", spe_conv_prefix, i);

		struct spe_data *sd = read_spe(spe_file);
		g_assert_nonnull(sd);
		g_assert_cmpint(output->input->detector->nchannels, ==, sd->nchannels);

    	//compare channel contents
		unsigned int j;
	    for (j = 0 ; j < sd->nchannels ; j++) {
    		g_assert_cmpfloat(output->channels_conv[i][j], ==, sd->data[j]);
	    }
		g_assert_cmpint(g_unlink(spe_file), ==, 0);
	}

	xmi_output_free(output);

	g_assert_cmpint(g_unlink(xmsi_file), ==, 0);
	g_assert_cmpint(g_unlink(xmso_file), ==, 0);
	g_assert_cmpint(g_unlink(csv_conv), ==, 0);
	g_assert_cmpint(g_unlink(html_conv), ==, 0);
}

static gboolean stop_timeout(XmiMsimJob *job) {
	g_assert_true(xmi_msim_job_stop(job, NULL));
	g_debug("message: job stopped");
	return FALSE;
}

static void test_good_input_file_stop(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test5\u03B1.xmso");
	g_assert(xmi_input_validate(data->input) == 0);
	g_assert_true(xmi_input_write_to_xml_file(data->input, COMPOUND "-test5\u03B1.xmsi", &error));

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test5\u03B1.xmsi",
		data->options,
		NULL, NULL, NULL,
		extra_options
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	g_assert_true(xmi_msim_job_start(job, &error));

	// hook up timeout that will stop the job
	g_timeout_add_seconds(5, (GSourceFunc) stop_timeout, job);

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

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);

	g_assert(g_unlink(COMPOUND "-test5\u03B1.xmsi") == 0);
}

static gboolean suspend_resume_timeout(XmiMsimJob *job) {
	g_assert_true(xmi_msim_job_is_running(job));

	if (xmi_msim_job_is_suspended(job) == FALSE) {
		g_assert_true(xmi_msim_job_suspend(job, NULL));
		g_debug("message: job suspended");
		return TRUE;
	}
	g_assert_true(xmi_msim_job_resume(job, NULL));
	g_debug("message: job resumed");

	return FALSE;
}

static void test_good_input_file_suspend_resume(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test6\u03B1.xmso");
	g_assert(xmi_input_validate(data->input) == 0);
	g_assert_true(xmi_input_write_to_xml_file(data->input, COMPOUND "-test6\u03B1.xmsi", &error));

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test6\u03B1.xmsi",
		data->options,
		NULL, NULL, NULL,
		extra_options
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_succeed_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	gboolean rv = xmi_msim_job_start(job, &error);
	g_assert_no_error(error);
	g_assert_true(rv);

	// hook up timeout that will suspend and resume the job
	g_timeout_add_seconds(5, (GSourceFunc) suspend_resume_timeout, job);

	g_main_loop_run(data->main_loop);

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

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);

	g_assert(g_unlink(COMPOUND "-test6\u03B1.xmsi") == 0);
	g_assert(g_unlink(COMPOUND "-test6\u03B1.xmso") == 0);
}

static gboolean suspend_stop_timeout(XmiMsimJob *job) {
	g_assert_true(xmi_msim_job_is_running(job));

	if (xmi_msim_job_is_suspended(job) == FALSE) {
		g_assert_true(xmi_msim_job_suspend(job, NULL));
		g_debug("message: job suspended");
		return TRUE;
	}
	g_assert_true(xmi_msim_job_stop(job, NULL));
	g_debug("message: job killed");

	return FALSE;
}

static void test_good_input_file_suspend_stop(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test7\u03B1.xmso");
	g_assert(xmi_input_validate(data->input) == 0);
	g_assert_true(xmi_input_write_to_xml_file(data->input, COMPOUND "-test7\u03B1.xmsi", &error));

	// write input to file
	XmiMsimJob* job = xmi_msim_job_new(
		g_getenv("XMIMSIM_EXEC"),
		COMPOUND "-test7\u03B1.xmsi",
		data->options,
		NULL, NULL, NULL,
		extra_options
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_debug("command: %s", xmi_msim_job_get_command(job));
	gboolean rv = xmi_msim_job_start(job, &error);
	g_assert_no_error(error);
	g_assert_true(rv);

	// hook up timeout that will suspend and stop the job
	g_timeout_add_seconds(5, (GSourceFunc) suspend_stop_timeout, job);

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

	g_assert_cmpint(G_OBJECT(job)->ref_count, ==, 1);
	g_object_unref(job);

	g_assert(g_unlink(COMPOUND "-test7\u03B1.xmsi") == 0);
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
	gchar data_file[] = "xmimsimdata-" COMPOUND "\u03B1.h5";
	g_assert(xmi_db(data_file, cd->Elements, cd->nElements));

	g_test_add("/job/non-existent-executable",
			SetupData,
			cd,
			setup_data,
			test_no_executable,
			teardown_data
			);
	g_test_add("/job/non-existent-input-file",
			SetupData,
			cd,
			setup_data,
			test_no_input_file,
			teardown_data
			);

	g_test_add("/job/bad-input-file",
			SetupData,
			cd,
			setup_data,
			test_bad_input_file,
			teardown_data
			);
	g_test_add("/job/good-input-file-simple",
			SetupData,
			cd,
			setup_data,
			test_good_input_file_simple,
			teardown_data
			);
	g_test_add("/job/good-input-file-stop",
			SetupData,
			cd,
			setup_data,
			test_good_input_file_stop,
			teardown_data
			);
	g_test_add("/job/good-input-file-suspend-resume",
			SetupData,
			cd,
			setup_data,
			test_good_input_file_suspend_resume,
			teardown_data
			);
	g_test_add("/job/good-input-file-suspend-stop",
			SetupData,
			cd,
			setup_data,
			test_good_input_file_suspend_stop,
			teardown_data
			);




	int rv = g_test_run();

	// cleanup
	FreeCompoundData(cd);
	g_unlink(data_file);

	return rv;
}
