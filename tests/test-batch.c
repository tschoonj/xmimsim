#include <config.h>
#include "xmi_batch.h"
#include "xmi_msim.h"
#include "libxmimsim-test.h"
#include <unistd.h>
#include <glib/gstdio.h>

#define COMPOUND "C6H12O6" // sweet, sweet sugar
#define N_MULTI 5
#define N_SINGLE 6

gchar *extra_options[4] = {
	"--with-hdf5-data=" HDF5_DATA_DIR "/xmimsimdata-" COMPOUND "\u03B1.h5",
	"--with-solid-angles-data=solid-angles\u03B1.h5",
	"--with-escape-ratios-data=escape-ratios\u03B1.h5",
	NULL
};

//#ifdef G_OS_WIN32
/* 
 * do not run this on Windows... it appears to fail a lot...
 */

//int main (int argc, char *argv[]) {
//	return 77; // skip
//}

//#else

// this is a cheap hack to get around the occasional WaitForMultipleObjectsEx warning on Windows
static gboolean test_log_WaitForMultipleObjectsEx(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data) {
#ifdef G_OS_WIN32
	if (g_strcmp0(log_domain, "GLib") == 0 && g_strcmp0(message, "WaitForMultipleObjectsEx failed: The handle is invalid.") == 0) {
		return FALSE;
	}
#endif
	return TRUE;
}

typedef struct {
	GMainLoop *main_loop;
	guint active_job_changed_called;
	guint running_true_called;
	guint running_false_called;
	gint action_after;
} SetupData;

static void running_changed_called(XmiMsimBatchAbstract *batch, GParamSpec *pspec, SetupData *data) {
	gboolean running = xmi_msim_batch_abstract_is_running(batch);
	if (running) {
		data->running_true_called++;
	}
	else
		data->running_false_called++;
}

static void test_fail_finished_cb(XmiMsimBatchAbstract *batch, gboolean result, const gchar *buffer, SetupData *data) {
	g_debug("message: %s", buffer);
	g_assert_false(result);
	g_main_loop_quit(data->main_loop);
}

static void test_success_finished_cb(XmiMsimBatchAbstract *batch, gboolean result, const gchar *buffer, SetupData *data) {
	g_debug("message: %s", buffer);
	g_assert_true(result);
	g_main_loop_quit(data->main_loop);
}

static void print_stdout(XmiMsimBatchAbstract *batch, const gchar *string) {
	g_debug("stdout: %s", string);
}

static void print_stderr(XmiMsimBatchAbstract *batch, const gchar *string) {
	g_debug("stderr: %s", string);
}

static void active_job_changed_cb(XmiMsimBatchAbstract *batch, XmiMsimJob *active_job, SetupData *data) {
	data->active_job_changed_called++;
}

typedef struct {
	GMainLoop *main_loop;
	guint active_job_changed_called;
	guint running_true_called;
	guint running_false_called;
	gint action_after;
	GPtrArray *xmsi_data;
	GPtrArray *single_data;
	xmi_main_options *options;
} SetupDataSingle;

static void setup_data_single(SetupDataSingle *data, gconstpointer user_data) {
	g_debug("Entering setup_data_single");
	data->main_loop = g_main_loop_new(NULL, FALSE);
	data->xmsi_data = g_ptr_array_new_full(N_SINGLE, (GDestroyNotify) xmi_input_free);
	data->single_data = g_ptr_array_new_full(1, (GDestroyNotify) xmi_batch_single_data_free);
	data->options = xmi_main_options_new();
	data->action_after = g_test_rand_int_range(1, N_SINGLE);

	data->options->use_variance_reduction = FALSE; // brute force!
	data->options->use_escape_peaks = FALSE; // no escape peaks!

	struct compoundData *cd = (struct compoundData *) user_data;
	unsigned int i;
	const double base_thickness = 0.0001;

	for (i = 0 ; i < N_SINGLE ; i++) {
		xmi_input *input = xmi_input_init_empty();
		// simulate 10M photons brute force
		input->general->n_photons_line = 10000000;
		// add compound to composition
		xmi_layer *layer = compoundData2xmi_layer(cd);
		layer->thickness = base_thickness * (i + 1);
		layer->density = 1.0;
		input->composition->n_layers = 1;
		input->composition->layers = layer;
		input->composition->reference_layer = 1;

		g_assert(xmi_input_validate(input) == XMI_INPUT_GENERAL);
		g_ptr_array_add(data->xmsi_data, input);
	}

	xmi_batch_single_data *single_data = xmi_batch_single_data_new("/xmimsim/composition/layer[1]/thickness", base_thickness, base_thickness * N_SINGLE, N_SINGLE - 1);
	g_ptr_array_add(data->single_data, single_data);

}

static void setup_data_single2D(SetupDataSingle *data, gconstpointer user_data) {
	g_debug("Entering setup_data_single2D");
	data->main_loop = g_main_loop_new(NULL, FALSE);
	data->xmsi_data = g_ptr_array_new_full(N_SINGLE, (GDestroyNotify) xmi_input_free);
	g_ptr_array_set_size(data->xmsi_data, N_SINGLE);
	data->single_data = g_ptr_array_new_full(2, (GDestroyNotify) xmi_batch_single_data_free);
	data->options = xmi_main_options_new();
	data->action_after = g_test_rand_int_range(1, N_SINGLE);

	data->options->use_variance_reduction = FALSE; // brute force!
	data->options->use_escape_peaks = FALSE; // no escape peaks!

	struct compoundData *cd = (struct compoundData *) user_data;
	unsigned int i, j;

	xmi_batch_single_data *single_data1 = xmi_batch_single_data_new("/xmimsim/geometry/p_detector_window/x", -0.0001, 0.0001, 2 - 1);
	g_ptr_array_add(data->single_data, single_data1);
	xmi_batch_single_data *single_data2 = xmi_batch_single_data_new("/xmimsim/geometry/p_detector_window/z", 99.9999, 100.0001, 3 - 1);
	g_ptr_array_add(data->single_data, single_data2);

	GArray *dims = g_array_sized_new(FALSE, FALSE, sizeof(int), 2);
	int dims_arr[2] = {2, 3};
	g_array_append_vals(dims, dims_arr, 2);

	for (i = 0 ; i < 2; i++) {
		for (j = 0 ; j < 3; j++) {
			xmi_input *input = xmi_input_init_empty();
			// simulate 10M photons brute force
			input->general->n_photons_line = 10000000;
			// add compound to composition
			xmi_layer *layer = compoundData2xmi_layer(cd);
			layer->thickness = 0.1;
			layer->density = 1.0;
			input->composition->n_layers = 1;
			input->composition->layers = layer;
			input->composition->reference_layer = 1;

			double p_detector_window_x = -0.0001 + i * 0.0002;
			double p_detector_window_z = 99.9999 + j * 0.0001;

			input->geometry->p_detector_window[0] =  p_detector_window_x;
			input->geometry->p_detector_window[2] =  p_detector_window_z;

			g_assert(xmi_input_validate(input) == XMI_INPUT_GENERAL);
			GArray *indices = g_array_sized_new(FALSE, FALSE, sizeof(int), 2);
			g_array_append_val(indices, i);
			g_array_append_val(indices, j);

			g_ptr_array_index(data->xmsi_data, xmi_row_major_array_get_offset(dims, indices)) = input;
			g_array_unref(indices);
		}
	}
	
	g_array_unref(dims);
}

static void teardown_data_single(SetupDataSingle *data, gconstpointer user_data) {
	g_debug("Entering teardown_data_single");

	g_main_loop_unref(data->main_loop);
	xmi_main_options_free(data->options);
	g_ptr_array_unref(data->xmsi_data);
	g_ptr_array_unref(data->single_data);
}

static void test_constructor_args_single(SetupDataSingle *data, gconstpointer user_data) {
	XmiMsimBatchAbstract *batch;

	// good
	batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		data->single_data,
		data->options
		);
	g_assert_nonnull(batch);
	g_assert_true(xmi_msim_batch_abstract_is_valid_object(batch));
	g_object_unref(batch);

	// adding a single_data should also work, if done properly...
	GPtrArray *single_data2D = g_ptr_array_new_full(2, (GDestroyNotify) xmi_batch_single_data_free);
	xmi_batch_single_data *single_data2D_0 = xmi_batch_single_data_new("/xmimsim/composition/layer[1]/thickness", 0.1, 0.1, 2 - 1);
	xmi_batch_single_data *single_data2D_1 = xmi_batch_single_data_new("/xmimsim/composition/layer[1]/density", 0.1, 0.1, 3 - 1);
	g_ptr_array_add(single_data2D, single_data2D_0);
	g_ptr_array_add(single_data2D, single_data2D_1);
	
	batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		single_data2D,
		data->options
		);
	g_assert_nonnull(batch);
	g_assert_true(xmi_msim_batch_abstract_is_valid_object(batch));
	g_object_unref(batch);
	g_ptr_array_unref(single_data2D);

	// bad
	g_test_log_set_fatal_handler(test_log_fatal_false, NULL);
	batch = xmi_msim_batch_single_new(
		NULL,
		data->single_data,
		data->options
		);
	g_assert_null(batch);

	batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		NULL,
		data->options
		);
	g_assert_null(batch);

	batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		data->single_data,
		NULL
		);
	g_assert_null(batch);

	single_data2D = g_ptr_array_new_full(2, (GDestroyNotify) xmi_batch_single_data_free);
	single_data2D_0 = xmi_batch_single_data_new("/xmimsim/composition/layer[1]/thickness", 0.1, 0.1, 2 - 1);
	single_data2D_1 = xmi_batch_single_data_new("/xmimsim/composition/layer[1]/density", 0.1, 0.1, 3);
	g_ptr_array_add(single_data2D, single_data2D_0);
	g_ptr_array_add(single_data2D, single_data2D_1);
	
	batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		single_data2D,
		data->options
		);
	g_assert_nonnull(batch);
	g_assert_false(xmi_msim_batch_abstract_is_valid_object(batch));
	g_object_unref(batch);
	g_ptr_array_unref(single_data2D);
}

static void test_no_executable_single(SetupDataSingle *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		data->single_data,
		data->options
		);
	g_assert_nonnull(batch);
	g_assert_true(xmi_msim_batch_abstract_is_valid_object(batch));
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_NON_EXISTENT_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, 0);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_false(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static XmiMsimBatchAbstract* test_good_input_data_single_base(SetupDataSingle *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_single_new(
		data->xmsi_data,
		data->single_data,
		data->options
		);
	g_assert_nonnull(batch);
	g_assert_true(xmi_msim_batch_abstract_is_valid_object(batch));
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_success_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_test_log_set_fatal_handler(test_log_WaitForMultipleObjectsEx, NULL);
	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, N_SINGLE + 1);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_true(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	return batch;
}

static void test_good_input_data_single1D(SetupDataSingle *data, gconstpointer user_data) {
	XmiMsimBatchAbstract *batch = test_good_input_data_single_base(data, user_data);

	// write to archive
	const gchar xmsa_file[] = HDF5_DATA_DIR "/single-batch1D.xmsa";
	g_assert_true(xmi_msim_batch_single_write_archive(XMI_MSIM_BATCH_SINGLE(batch), xmsa_file, NULL));

	xmi_archive *archive = xmi_archive_read_from_xml_file(xmsa_file, NULL);
	g_assert_cmpuint(archive->single_data->len, ==, 1);
	g_assert_cmpuint(archive->output->len, ==, N_SINGLE);
	g_assert_cmpuint(archive->dims->len, ==, 1);
	g_assert_cmpuint(archive->ref_count, ==, 1);
	g_assert_cmpint(g_array_index(archive->dims, int, 0), ==, N_SINGLE);

	g_assert_nonnull(archive);

	xmi_archive_unref(archive);
	g_unlink(xmsa_file);

	g_object_unref(batch);
}

static void test_good_input_data_single2D(SetupDataSingle *data, gconstpointer user_data) {
	XmiMsimBatchAbstract *batch = test_good_input_data_single_base(data, user_data);

	// write to archive
	const gchar xmsa_file[] = HDF5_DATA_DIR "/single-batch2D.xmsa";
	g_assert_true(xmi_msim_batch_single_write_archive(XMI_MSIM_BATCH_SINGLE(batch), xmsa_file, NULL));

	xmi_archive *archive = xmi_archive_read_from_xml_file(xmsa_file, NULL);
	g_assert_cmpuint(archive->single_data->len, ==, 2);
	g_assert_cmpuint(archive->output->len, ==, N_SINGLE);
	g_assert_cmpuint(archive->dims->len, ==, 2);
	g_assert_cmpuint(archive->ref_count, ==, 1);
	g_assert_cmpint(g_array_index(archive->dims, int, 0), ==, 2);
	g_assert_cmpint(g_array_index(archive->dims, int, 1), ==, 3);

	g_assert_nonnull(archive);

	xmi_archive_unref(archive);
	g_unlink(xmsa_file);

	g_object_unref(batch);
}

typedef struct {
	GMainLoop *main_loop;
	guint active_job_changed_called;
	guint running_true_called;
	guint running_false_called;
	gint action_after;
	GPtrArray *xmsi_files;
	GPtrArray *xmso_files;
	GPtrArray *options;
} SetupDataMulti;

static void setup_data_multi(SetupDataMulti *data, gconstpointer user_data) {
	g_debug("Entering setup_data_multi");
	data->main_loop = g_main_loop_new(NULL, FALSE);
	data->xmsi_files = g_ptr_array_new_full(N_MULTI, g_free);
	data->xmso_files = g_ptr_array_new_full(N_MULTI, g_free);
	data->options = g_ptr_array_new_full(N_MULTI, (GDestroyNotify) xmi_main_options_free);
	data->action_after = g_test_rand_int_range(1, N_MULTI);

	xmi_input *input = xmi_input_init_empty();
	// simulate 10M photons brute force
	input->general->n_photons_line = 10000000;
	xmi_main_options *options = xmi_main_options_new();
	options->use_variance_reduction = FALSE; // brute force!
	options->use_escape_peaks = FALSE; // no escape peaks!
	g_ptr_array_add(data->options, options);

	struct compoundData *cd = (struct compoundData *) user_data;
	// add compound to composition
	xmi_layer *layer = compoundData2xmi_layer(cd);
	layer->thickness = 0.0;
	layer->density = 1.0;
	input->composition->n_layers = 1;
	input->composition->layers = layer;
	input->composition->reference_layer = 1;

	unsigned int i;

	for (i = 0 ; i < N_MULTI ; i++) {
		layer->thickness += 0.1;
		gchar tmpl[] = "tmp.XXXXXX";
		mktemp(tmpl);
		gchar *xmsi_file = g_strdup_printf("%s%d\u03B1.xmsi", tmpl, i);
		gchar *xmso_file = g_strdup_printf("%s%d\u03B1.xmso", tmpl, i);
		g_free(input->general->outputfile);
		input->general->outputfile = g_strdup(xmso_file);
		g_assert(xmi_input_validate(input) == 0);
		g_assert_true(xmi_input_write_to_xml_file(input, xmsi_file, NULL));
		g_ptr_array_add(data->xmsi_files, xmsi_file);
		g_ptr_array_add(data->xmso_files, xmso_file);
	}
	xmi_input_free(input);
}

static void teardown_data_multi(SetupDataMulti *data, gconstpointer user_data) {
	g_debug("Entering teardown_data_multi");
	g_main_loop_unref(data->main_loop);
	g_ptr_array_unref(data->options);
	g_ptr_array_foreach(data->xmsi_files, (GFunc) g_unlink, NULL);
	g_ptr_array_unref(data->xmsi_files);
	g_ptr_array_foreach(data->xmso_files, (GFunc) g_unlink, NULL);
	g_ptr_array_unref(data->xmso_files);
}

static void test_constructor_args_multi(SetupDataMulti *data, gconstpointer user_data) {
	XmiMsimBatchAbstract *batch;

	// good
	batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	g_object_unref(batch);

	unsigned int i;
	for (i = 0 ; i < N_MULTI - 1 ; i++) {
		xmi_main_options *copy = NULL;
		xmi_main_options_copy((xmi_main_options *) g_ptr_array_index(data->options, 0), &copy);
		g_ptr_array_add(data->options, copy);
	}
	batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	g_object_unref(batch);
	g_ptr_array_remove_range(data->options, 1, N_MULTI - 1);

	// bad
	g_test_log_set_fatal_handler(test_log_fatal_false, NULL);
	batch = xmi_msim_batch_multi_new(
		NULL,
		data->options
		);
	g_assert_null(batch);

	batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		NULL
		);
	g_assert_null(batch);

	for (i = 0 ; i < N_MULTI - 2 ; i++) {
		xmi_main_options *copy = NULL;
		xmi_main_options_copy((xmi_main_options *) g_ptr_array_index(data->options, 0), &copy);
		g_ptr_array_add(data->options, copy);
		batch = xmi_msim_batch_multi_new(
			data->xmsi_files,
			data->options
			);
		g_assert_null(batch);
	}
}

static void test_no_executable_multi(SetupDataMulti *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_NON_EXISTENT_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, 0);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_false(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static void test_no_input_file_multi(SetupDataMulti *data, gconstpointer user_data) {
	g_test_log_set_fatal_handler(test_log_fatal_false, NULL);
	GError *error = NULL;

	g_ptr_array_insert(data->xmsi_files, 0, g_strdup("non-existent-file.xmsi"));

	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, 0);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_false(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static void test_bad_input_file_multi(SetupDataMulti *data, gconstpointer user_data) {
	g_test_log_set_fatal_handler(test_log_fatal_false, NULL);
	GError *error = NULL;

	// open first file and modify it with some invalid value
	xmi_input *input = xmi_input_read_from_xml_file(g_ptr_array_index(data->xmsi_files, 0), &error);
	g_assert_nonnull(input);
	g_assert(xmi_input_validate(input) == 0);
	// file is valid now, let's make it invalid...
	input->composition->reference_layer = 5;
	g_assert(xmi_input_validate(input) != 0);
	g_assert_true(xmi_input_write_to_xml_file(input, g_ptr_array_index(data->xmsi_files, 0), &error));
	
	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, 0);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_false(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static void test_good_input_files_multi(SetupDataMulti *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_success_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_test_log_set_fatal_handler(test_log_WaitForMultipleObjectsEx, NULL);
	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, N_MULTI + 1);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_true(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static gboolean stop_timeout(XmiMsimBatchAbstract *batch) {
	g_assert_true(xmi_msim_batch_abstract_stop(batch, NULL));
	g_debug("message: batch stopped");
	return FALSE;
}

static void active_job_changed_stop_after_cb(XmiMsimBatchAbstract *batch, XmiMsimJob *active_job, SetupDataMulti *data) {
	if (data->active_job_changed_called == data->action_after) {
		// hook up timeout that will stop a job
		g_timeout_add_seconds(1, (GSourceFunc) stop_timeout, batch);
	}
}

static void test_good_input_files_multi_stop(SetupDataMulti *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_stop_after_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_test_log_set_fatal_handler(test_log_WaitForMultipleObjectsEx, NULL);
	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, data->action_after + 1);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_false(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static gboolean suspend_resume_timeout(XmiMsimBatchAbstract *batch) {
	g_assert_true(xmi_msim_batch_abstract_is_running(batch));

	if (xmi_msim_batch_abstract_is_suspended(batch) == FALSE) {
		g_assert_true(xmi_msim_batch_abstract_suspend(batch, NULL));
		g_debug("message: batch suspended");
		return TRUE;
	}
	g_assert_true(xmi_msim_batch_abstract_resume(batch, NULL));
	g_debug("message: batch resumed");

	return FALSE;
}

static void active_job_changed_suspend_resume_after_cb(XmiMsimBatchAbstract *batch, XmiMsimJob *job, SetupDataMulti *data) {
	if (data->active_job_changed_called == data->action_after) {
		// hook up timeout that will suspend and resume a job
		g_timeout_add_seconds(1, (GSourceFunc) suspend_resume_timeout, batch);
	}
}

static void test_good_input_files_multi_suspend_resume(SetupDataMulti *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_suspend_resume_after_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_success_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_test_log_set_fatal_handler(test_log_WaitForMultipleObjectsEx, NULL);
	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, N_MULTI + 1);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_true(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

static gboolean suspend_stop_timeout(XmiMsimBatchAbstract *batch) {
	g_assert_true(xmi_msim_batch_abstract_is_running(batch));

	if (xmi_msim_batch_abstract_is_suspended(batch) == FALSE) {
		g_assert_true(xmi_msim_batch_abstract_suspend(batch, NULL));
		g_debug("message: batch suspended");
		return TRUE;
	}
	g_assert_true(xmi_msim_batch_abstract_stop(batch, NULL));
	g_debug("message: batch stop");

	return FALSE;
}

static void active_job_changed_suspend_stop_after_cb(XmiMsimBatchAbstract *batch, XmiMsimJob *active_job, SetupDataMulti *data) {
	if (data->active_job_changed_called == data->action_after) {
		// hook up timeout that will suspend and stop a job
		g_timeout_add_seconds(1, (GSourceFunc) suspend_stop_timeout, batch);
	}
}

static void test_good_input_files_multi_suspend_stop(SetupDataMulti *data, gconstpointer user_data) {
	GError *error = NULL;

	XmiMsimBatchAbstract* batch = xmi_msim_batch_multi_new(
		data->xmsi_files,
		data->options
		);
	g_assert_nonnull(batch);
	xmi_msim_batch_abstract_set_executable(batch, g_getenv("XMIMSIM_EXEC"));
	xmi_msim_batch_abstract_set_extra_options(batch, extra_options);

	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_cb), data);
	g_signal_connect(G_OBJECT(batch), "active-job-changed", G_CALLBACK(active_job_changed_suspend_stop_after_cb), data);
	g_signal_connect(G_OBJECT(batch), "finished-event", G_CALLBACK(test_fail_finished_cb), data);
	g_signal_connect(G_OBJECT(batch), "stdout-event", G_CALLBACK(print_stdout), NULL);
	g_signal_connect(G_OBJECT(batch), "stderr-event", G_CALLBACK(print_stderr), NULL);
	g_signal_connect(G_OBJECT(batch), "notify::running", G_CALLBACK(running_changed_called), data);

	g_test_log_set_fatal_handler(test_log_WaitForMultipleObjectsEx, NULL);
	g_assert_true(xmi_msim_batch_abstract_start(batch, &error));
	g_main_loop_run(data->main_loop);

	g_assert_cmpuint(data->active_job_changed_called, ==, data->action_after + 1);
	g_assert_cmpuint(data->running_true_called, ==, 1);
	g_assert_cmpuint(data->running_false_called, ==, 1);
	g_assert_false(xmi_msim_batch_abstract_is_running(batch));
	g_assert_false(xmi_msim_batch_abstract_is_suspended(batch));
	g_assert_true(xmi_msim_batch_abstract_has_finished(batch));
	g_assert_false(xmi_msim_batch_abstract_was_successful(batch));
	g_assert_false(xmi_msim_batch_abstract_stop(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_suspend(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_resume(batch, NULL));
	g_assert_false(xmi_msim_batch_abstract_start(batch, NULL));

	g_object_unref(batch);
}

int main(int argc, char *argv[]) {

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
	gchar data_file[] = HDF5_DATA_DIR "/xmimsimdata-" COMPOUND "\u03B1.h5";
	//if (g_access(data_file, R_OK) != 0)
		g_assert(xmi_db(data_file, cd->Elements, cd->nElements));

	g_test_add("/single-batch/constructor-args",
			SetupDataSingle,
			cd,
			setup_data_single,
			test_constructor_args_single,
			teardown_data_single
			);
	g_test_add("/single-batch/non-existent-executable",
			SetupDataSingle,
			cd,
			setup_data_single,
			test_no_executable_single,
			teardown_data_single
			);
	g_test_add("/single-batch/good-input-data/1D",
			SetupDataSingle,
			cd,
			setup_data_single,
			test_good_input_data_single1D,
			teardown_data_single
			);
	g_test_add("/single-batch/good-input-data/2D",
			SetupDataSingle,
			cd,
			setup_data_single2D,
			test_good_input_data_single2D,
			teardown_data_single
			);
	g_test_add("/multi-batch/constructor-args",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_constructor_args_multi,
			teardown_data_multi
			);
	g_test_add("/multi-batch/non-existent-executable",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_no_executable_multi,
			teardown_data_multi
			);
	g_test_add("/multi-batch/non-existent-input-file",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_no_input_file_multi,
			teardown_data_multi
			);
	g_test_add("/multi-batch/bad-input-file",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_bad_input_file_multi,
			teardown_data_multi
			);
	g_test_add("/multi-batch/good-input-files",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_good_input_files_multi,
			teardown_data_multi
			);
	g_test_add("/multi-batch/good-input-files-stop",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_good_input_files_multi_stop,
			teardown_data_multi
			);
	g_test_add("/multi-batch/good-input-files-suspend-resume",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_good_input_files_multi_suspend_resume,
			teardown_data_multi
			);
	g_test_add("/multi-batch/good-input-files-suspend-stop",
			SetupDataMulti,
			cd,
			setup_data_multi,
			test_good_input_files_multi_suspend_stop,
			teardown_data_multi
			);
	
	int rv = g_test_run();

	// cleanup
	FreeCompoundData(cd);
	unlink(data_file);

	return rv;
}
//#endif
