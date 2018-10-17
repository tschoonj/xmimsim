#include <config.h>
#include "xmimsim-gui-job.h"
#include "xmi_msim.h"
#include "libxmimsim-test.h"
#include <unistd.h>

#define COMPOUND "C6H12O6" // sweet, sweet sugar

#ifdef G_OS_WIN32
  #define XMIMSIM_EXEC "../bin/.libs/xmimsim.exe"
  #define XMIMSIM_NON_EXISTENT_EXEC "../bin/.libs/xmimsim-bad.exe"
#else
  #define XMIMSIM_EXEC "../bin/xmimsim"
  #define XMIMSIM_NON_EXISTENT_EXEC "../bin/xmimsim-bad"
#endif

const gchar *extra_options[4] = {
	"--with-hdf5-data=xmimsimdata-" COMPOUND ".h5",
	"--with-solid-angles-data=solid-angles.h5",
	"--with-escape-ratios-data=escape-ratios.h5",
	NULL
};

typedef struct {
	GMainLoop *main_loop;
	xmi_input *input;
	xmi_main_options *options;
} SetupData;

static void setup_data(SetupData *data, gconstpointer user_data) {
	data->main_loop = g_main_loop_new(NULL, FALSE);
	data->input = xmi_init_empty_input();
	// simulate 10M photons
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
	xmi_free_input(data->input);
}

static void test_no_executable(SetupData *data, gconstpointer user_data) {
	// write input to file -> not necessary here...
	GError *error = NULL;

	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_NON_EXISTENT_EXEC,
		"non-existent-file.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);
	g_assert_false(xmi_msim_gui_job_start(job, &error));

	g_assert(error->domain == G_SPAWN_ERROR);
	g_debug("message: %s", error->message);
	g_debug("code: %d", error->code);
#ifdef G_OS_WIN32
	g_assert(error->code == G_SPAWN_ERROR_FAILED);
#else
	g_assert(error->code == G_SPAWN_ERROR_NOENT);
#endif
	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_false(xmi_msim_gui_job_has_finished(job));
	g_assert_false(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_object_unref(job);
}

static void test_fail_finished_cb(XmiMsimGuiJob *job, gboolean result, const gchar *buffer, SetupData *data) {
	g_assert_false(result);
	g_debug("message: %s", buffer);
	g_main_loop_quit(data->main_loop);
}

static void test_succeed_finished_cb(XmiMsimGuiJob *job, gboolean result, const gchar *buffer, SetupData *data) {
	g_assert_true(result);
	g_debug("message: %s", buffer);
	g_main_loop_quit(data->main_loop);
}

static void print_stdout(XmiMsimGuiJob *job, const gchar *string) {
	g_debug("stdout: %s", string);
}

static void print_stderr(XmiMsimGuiJob *job, const gchar *string) {
	g_debug("stderr: %s", string);
}

static void test_no_input_file(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_EXEC,
		"non-existent-file.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_assert_true(xmi_msim_gui_job_start(job, &error));

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_true(xmi_msim_gui_job_has_finished(job));
	g_assert_false(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_assert_false(xmi_msim_gui_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_GUI_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);
}

static void test_bad_input_file(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test.xmso");
	g_assert(xmi_validate_input(data->input) == 0);
	// file is valid now, let's make it invalid...
	data->input->composition->reference_layer = 5;
	g_assert(xmi_validate_input(data->input) != 0);
	g_assert(xmi_write_input_xml(COMPOUND "-test.xmsi", data->input, &error) == 1);

	// write input to file
	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_EXEC,
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_assert_true(xmi_msim_gui_job_start(job, &error));

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_true(xmi_msim_gui_job_has_finished(job));
	g_assert_false(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_assert_false(xmi_msim_gui_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_GUI_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);
	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
}

static void test_special_event_cb(XmiMsimGuiJob *job, int event, const gchar *buffer, gpointer data) {
	g_assert_true(xmi_msim_gui_job_is_running(job));
	// this test is running in brute force mode, so we know exactly what to expect
	if (event == XMI_MSIM_GUI_JOB_SPECIAL_EVENT_SOLID_ANGLE)
		g_assert(strcmp(buffer, "Solid angle grid redundant") == 0);
	else if (event == XMI_MSIM_GUI_JOB_SPECIAL_EVENT_ESCAPE_PEAKS)
		g_assert(strcmp(buffer, "Escape peaks redundant") == 0);
	else
		g_debug("special_event: %s", buffer);
}

static void test_good_input_file_simple(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test.xmso");
	g_assert(xmi_validate_input(data->input) == 0);
	g_assert(xmi_write_input_xml(COMPOUND "-test.xmsi", data->input, &error) == 1);

	// write input to file
	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_EXEC,
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_succeed_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_assert_true(xmi_msim_gui_job_start(job, &error));

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_true(xmi_msim_gui_job_has_finished(job));
	g_assert_true(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_assert_false(xmi_msim_gui_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_GUI_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
	g_assert(unlink(COMPOUND "-test.xmso") == 0);
}

static gboolean stop_timeout(XmiMsimGuiJob *job) {
	g_assert_true(xmi_msim_gui_job_stop(job, NULL));
	g_debug("message: job stopped");
	return FALSE;
}

static void test_good_input_file_stop(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test.xmso");
	g_assert(xmi_validate_input(data->input) == 0);
	g_assert(xmi_write_input_xml(COMPOUND "-test.xmsi", data->input, &error) == 1);

	// write input to file
	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_EXEC,
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_assert_true(xmi_msim_gui_job_start(job, &error));

	// hook up timeout that will stop the job
	g_timeout_add_seconds(5, (GSourceFunc) stop_timeout, job);

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_true(xmi_msim_gui_job_has_finished(job));
	g_assert_false(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_assert_false(xmi_msim_gui_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_GUI_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
}

static gboolean suspend_resume_timeout(XmiMsimGuiJob *job) {
	g_assert_true(xmi_msim_gui_job_is_running(job));

	if (xmi_msim_gui_job_is_suspended(job) == FALSE) {
		g_assert_true(xmi_msim_gui_job_suspend(job, NULL));
		g_debug("message: job suspended");
		return TRUE;
	}
	g_assert_true(xmi_msim_gui_job_resume(job, NULL));
	g_debug("message: job resumed");

	return FALSE;
}

static void test_good_input_file_suspend_resume(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test.xmso");
	g_assert(xmi_validate_input(data->input) == 0);
	g_assert(xmi_write_input_xml(COMPOUND "-test.xmsi", data->input, &error) == 1);

	// write input to file
	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_EXEC,
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_succeed_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_assert_true(xmi_msim_gui_job_start(job, &error));

	// hook up timeout that will suspend and resume the job
	g_timeout_add_seconds(5, (GSourceFunc) suspend_resume_timeout, job);

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_true(xmi_msim_gui_job_has_finished(job));
	g_assert_true(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_assert_false(xmi_msim_gui_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_GUI_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
	g_assert(unlink(COMPOUND "-test.xmso") == 0);
}

static gboolean suspend_stop_timeout(XmiMsimGuiJob *job) {
	g_assert_true(xmi_msim_gui_job_is_running(job));

	if (xmi_msim_gui_job_is_suspended(job) == FALSE) {
		g_assert_true(xmi_msim_gui_job_suspend(job, NULL));
		g_debug("message: job suspended");
		return TRUE;
	}
	g_assert_true(xmi_msim_gui_job_stop(job, NULL));
	g_debug("message: job killed");

	return FALSE;
}

static void test_good_input_file_suspend_stop(SetupData *data, gconstpointer user_data) {
	GError *error = NULL;

	data->input->general->outputfile = g_strdup(COMPOUND "-test.xmso");
	g_assert(xmi_validate_input(data->input) == 0);
	g_assert(xmi_write_input_xml(COMPOUND "-test.xmsi", data->input, &error) == 1);

	// write input to file
	XmiMsimGuiJob* job = xmi_msim_gui_job_new(
		XMIMSIM_EXEC,
		COMPOUND "-test.xmsi",
		data->options,
		NULL, NULL, NULL, NULL,
		-1,
		extra_options,
		&error
		);
	g_assert_nonnull(job);

	// hook up signals
	g_signal_connect(G_OBJECT(job), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(job), "special-event", G_CALLBACK(test_special_event_cb), data);
	g_signal_connect(G_OBJECT(job), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(job), "stderr-event", G_CALLBACK(print_stderr), NULL);

	g_assert_true(xmi_msim_gui_job_start(job, &error));

	// hook up timeout that will suspend and stop the job
	g_timeout_add_seconds(5, (GSourceFunc) suspend_stop_timeout, job);

	g_main_loop_run(data->main_loop);

	g_assert_false(xmi_msim_gui_job_is_running(job));
	g_assert_false(xmi_msim_gui_job_is_suspended(job));
	g_assert_true(xmi_msim_gui_job_has_finished(job));
	g_assert_false(xmi_msim_gui_job_was_successful(job));
	g_assert_false(xmi_msim_gui_job_stop(job, NULL));
	g_assert_false(xmi_msim_gui_job_suspend(job, NULL));
	g_assert_false(xmi_msim_gui_job_resume(job, NULL));

	g_assert_false(xmi_msim_gui_job_start(job, &error));
	g_assert(error->domain == XMI_MSIM_GUI_JOB_ERROR);
	g_assert(error->code == XMI_MSIM_GUI_JOB_ERROR_UNAVAILABLE);

	g_object_unref(job);

	g_assert(unlink(COMPOUND "-test.xmsi") == 0);
}

int main(int argc, char *argv[]) {
	SetupData *data;

	setbuf(stdout,NULL);
#ifdef G_OS_WIN32
	setbuf(stderr,NULL);
#endif
	g_test_init(&argc, &argv, NULL);

	g_assert_true(xmi_msim_gui_job_is_suspend_available());

	// init test
	g_assert(test_init() == 1);

	// start by creating our data file
	xmi_init_hdf5();

	// read compound
	struct compoundData *cd = CompoundParser(COMPOUND);
	g_assert(cd != NULL);

	// generate appropriate xmimsimdata.h5 file
	gchar data_file[] = "xmimsimdata-" COMPOUND ".h5";
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
	unlink(data_file);


	return rv;
}
