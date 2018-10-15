#include <config.h>
#include <math.h>
#include "xmi_gobject.h"
#include "xmi_data_structs.h"
#include "libxmimsim-test.h"
#include <unistd.h>

typedef struct {
	xmi_composition *composition;
} SetupDataComposition;

typedef struct {
	xmi_excitation *excitation;
} SetupDataExcitation;

typedef struct {
	xmi_input *input;
} SetupDataInput;

typedef struct {
	xmi_archive *archive;
} SetupDataArchive;

static gboolean composition_equal(xmi_composition *A, xmi_composition *B) {
	int i, j;

	if (A->n_layers != B->n_layers) {
		return FALSE;
	}
	else if (A->reference_layer != B->reference_layer) {
		return FALSE;
	}
	else {
		for (i = 0 ; i < A->n_layers ; i++) {
			if (A->layers[i].n_elements != B->layers[i].n_elements) {
				return FALSE;
			}
			else {
				for (j = 0 ; j < A->layers[i].n_elements ; j++) {
					if (A->layers[i].Z[j] != B->layers[i].Z[j]) {
						return FALSE;
					}
					else if (fabs(A->layers[i].weight[j]- B->layers[i].weight[j])/A->layers[i].weight[j] >XMI_COMPARE_THRESHOLD) {
						return FALSE;
					}
				}
				if (fabs(A->layers[i].density - B->layers[i].density)/A->layers[i].density > XMI_COMPARE_THRESHOLD) {
					return FALSE;
				}
				if (fabs(A->layers[i].thickness- B->layers[i].thickness)/A->layers[i].thickness > XMI_COMPARE_THRESHOLD) {
					return FALSE;
				}
			}
		}
	}
	return TRUE;
}

static gboolean excitation_equal(xmi_excitation *A, xmi_excitation *B) {
	int i, j;
#define XMI_IF_COMPARE_EXCITATION_DISCRETE(a) if (fabs(A->discrete[i].a-B->discrete[i].a)/A->discrete[i].a > XMI_COMPARE_THRESHOLD) {\
					return FALSE;\
				}
	if (A->n_discrete > 0 || B->n_discrete > 0) {
		if (A->n_discrete != B->n_discrete) {
			return FALSE;
		}
		else {
			for (i = 0 ; i < A->n_discrete ; i++) {
				XMI_IF_COMPARE_EXCITATION_DISCRETE(energy)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(horizontal_intensity)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(vertical_intensity)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_x)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_xp)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_y)
				XMI_IF_COMPARE_EXCITATION_DISCRETE(sigma_yp)
				if (A->discrete[i].distribution_type != B->discrete[i].distribution_type) {
					return FALSE;
				}
				else if (A->discrete[i].distribution_type != XMI_DISCRETE_MONOCHROMATIC) {
					XMI_IF_COMPARE_EXCITATION_DISCRETE(scale_parameter)
				}
			}
		}
	}

#define XMI_IF_COMPARE_EXCITATION_CONTINUOUS(a) if (fabs(A->continuous[i].a-B->continuous[i].a)/A->continuous[i].a > XMI_COMPARE_THRESHOLD) {\
					return FALSE;\
				}
	if (A->n_continuous > 0 || B->n_continuous > 0) {
		if (A->n_continuous != B->n_continuous) {
			return FALSE;
		}
		else {
			for (i = 0 ; i < A->n_continuous ; i++) {
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(energy)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(horizontal_intensity)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(vertical_intensity)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_x)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_xp)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_y)
				XMI_IF_COMPARE_EXCITATION_CONTINUOUS(sigma_yp)
			}
		}
	}
	return TRUE;
}

static void setup_data_composition(SetupDataComposition *data, gconstpointer user_data) {
	data->composition = g_malloc(sizeof(xmi_composition));
	data->composition->reference_layer = 1;
	data->composition->n_layers = 1;
	data->composition->layers = g_malloc(sizeof(xmi_layer));
	data->composition->layers[0].n_elements = 2;
	data->composition->layers[0].Z = g_malloc(sizeof(int) * 2);
	data->composition->layers[0].weight = g_malloc(sizeof(double) * 2);
	data->composition->layers[0].Z[0] = 8;
	data->composition->layers[0].Z[1] = 14;
	data->composition->layers[0].weight[0] = 0.4;
	data->composition->layers[0].weight[1] = 0.6;
	data->composition->layers[0].density = 10.0;
	data->composition->layers[0].thickness = 1E-4;
}

static void setup_data_excitation(SetupDataExcitation *data, gconstpointer user_data) {
	data->excitation = g_malloc(sizeof(xmi_excitation));
	data->excitation->n_discrete = 1;
	data->excitation->n_continuous = 0;
	data->excitation->continuous = NULL;
	data->excitation->discrete = g_malloc(sizeof(xmi_energy_discrete));
	data->excitation->discrete[0].energy = 28.0;
	data->excitation->discrete[0].horizontal_intensity= 1E12;
	data->excitation->discrete[0].vertical_intensity= 1E9;
	data->excitation->discrete[0].sigma_x= 0.0;
	data->excitation->discrete[0].sigma_xp= 0.0;
	data->excitation->discrete[0].sigma_y= 0.0;
	data->excitation->discrete[0].sigma_yp= 0.0;
	data->excitation->discrete[0].scale_parameter = 0.0;
	data->excitation->discrete[0].distribution_type = XMI_DISCRETE_MONOCHROMATIC;
}

static void setup_data_input(SetupDataInput *data, gconstpointer user_data) {
	g_assert(xmi_read_input_xml(TEST_XMSI, &data->input, NULL) == 1);
}

static void setup_data_archive(SetupDataArchive *data, gconstpointer user_data) {
	g_assert(xmi_read_archive_xml(TEST_XMSA_1, &data->archive, NULL) == 1);
}

static void teardown_data_composition(SetupDataComposition *data, gconstpointer user_data) {
	xmi_free_composition(data->composition);
}

static void teardown_data_excitation(SetupDataExcitation *data, gconstpointer user_data) {
	xmi_free_excitation(data->excitation);
}

static void teardown_data_input(SetupDataInput *data, gconstpointer user_data) {
	xmi_free_input(data->input);
}

static void teardown_data_archive(SetupDataArchive *data, gconstpointer user_data) {
	xmi_free_archive(data->archive);
}

static void test_composition_static(SetupDataComposition *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_set_static_boxed(&value, data->composition);
	g_assert(data->composition == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_composition_take(SetupDataComposition *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_take_boxed(&value, data->composition);
	g_assert(data->composition == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_composition_null(SetupDataComposition *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_assert_null(data->composition);
	g_value_init(&value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_take_boxed(&value, data->composition);
	g_assert_null(g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_composition(SetupDataComposition *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_COMPOSITION);
	g_value_set_boxed(&value, data->composition);
	g_assert(data->composition != g_value_get_boxed(&value));
	g_assert_true(composition_equal(data->composition, g_value_get_boxed(&value)));
	g_value_unset(&value);
}

static void test_excitation_static(SetupDataExcitation *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_EXCITATION);
	g_value_set_static_boxed(&value, data->excitation);
	g_assert(data->excitation == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_excitation_take(SetupDataExcitation *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_EXCITATION);
	g_value_take_boxed(&value, data->excitation);
	g_assert(data->excitation == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_excitation_null(SetupDataExcitation *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_assert_null(data->excitation);
	g_value_init(&value, XMI_MSIM_TYPE_EXCITATION);
	g_value_take_boxed(&value, data->excitation);
	g_assert_null(g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_excitation(SetupDataExcitation *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_EXCITATION);
	g_value_set_boxed(&value, data->excitation);
	g_assert(data->excitation != g_value_get_boxed(&value));
	g_assert_true(excitation_equal(data->excitation, g_value_get_boxed(&value)));
	g_value_unset(&value);
}

static void test_input_static(SetupDataInput *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_INPUT);
	g_value_set_static_boxed(&value, data->input);
	g_assert(data->input == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_input_take(SetupDataInput *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_INPUT);
	g_value_take_boxed(&value, data->input);
	g_assert(data->input == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_input_null(SetupDataInput *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_assert_null(data->input);
	g_value_init(&value, XMI_MSIM_TYPE_INPUT);
	g_value_take_boxed(&value, data->input);
	g_assert_null(g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_input(SetupDataInput *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_INPUT);
	g_value_set_boxed(&value, data->input);
	g_assert(data->input != g_value_get_boxed(&value));
	g_assert(xmi_compare_input(data->input, g_value_get_boxed(&value)) == 0);
	g_value_unset(&value);
}

static void test_archive_static(SetupDataArchive *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_ARCHIVE);
	g_value_set_static_boxed(&value, data->archive);
	g_assert(data->archive == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_archive_take(SetupDataArchive *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_ARCHIVE);
	g_value_take_boxed(&value, data->archive);
	g_assert(data->archive == g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_archive_null(SetupDataArchive *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_assert_null(data->archive);
	g_value_init(&value, XMI_MSIM_TYPE_ARCHIVE);
	g_value_take_boxed(&value, data->archive);
	g_assert_null(g_value_get_boxed(&value));
	g_value_unset(&value);
}

static void test_archive(SetupDataArchive *data, gconstpointer user_data) {
	GValue value = G_VALUE_INIT;

	g_value_init(&value, XMI_MSIM_TYPE_ARCHIVE);
	g_value_set_boxed(&value, data->archive);
	g_assert(data->archive != g_value_get_boxed(&value));
	g_assert(xmi_compare_archive(data->archive, g_value_get_boxed(&value)) == 0);
	g_value_unset(&value);
}

int main(int argc, char *argv[]) {
	//init test
	g_assert(test_init() == 1);
	g_test_init(&argc, &argv, NULL);

	//download files
	g_assert(test_download_file(TEST_XMSI_URL) == 1);
	g_assert(test_download_file(TEST_XMSA_URL_1) == 1);

	g_test_add("/gobject/composition-static",
			SetupDataComposition,
			NULL,
			setup_data_composition,
			test_composition_static,
			teardown_data_composition
			);
	g_test_add("/gobject/composition-take",
			SetupDataComposition,
			NULL,
			setup_data_composition,
			test_composition_take,
			NULL
			);
	g_test_add("/gobject/composition-null",
			SetupDataComposition,
			NULL,
			NULL,
			test_composition_null,
			teardown_data_composition
			);
	g_test_add("/gobject/composition",
			SetupDataComposition,
			NULL,
			setup_data_composition,
			test_composition,
			teardown_data_composition
			);
	g_test_add("/gobject/excitation-static",
			SetupDataExcitation,
			NULL,
			setup_data_excitation,
			test_excitation_static,
			teardown_data_excitation
			);
	g_test_add("/gobject/excitation-take",
			SetupDataExcitation,
			NULL,
			setup_data_excitation,
			test_excitation_take,
			NULL
			);
	g_test_add("/gobject/excitation-null",
			SetupDataExcitation,
			NULL,
			NULL,
			test_excitation_null,
			teardown_data_excitation
			);
	g_test_add("/gobject/excitation",
			SetupDataExcitation,
			NULL,
			setup_data_excitation,
			test_excitation,
			teardown_data_excitation
			);
	g_test_add("/gobject/input-static",
			SetupDataInput,
			NULL,
			setup_data_input,
			test_input_static,
			teardown_data_input
			);
	g_test_add("/gobject/input-take",
			SetupDataInput,
			NULL,
			setup_data_input,
			test_input_take,
			NULL
			);
	g_test_add("/gobject/input-null",
			SetupDataInput,
			NULL,
			NULL,
			test_input_null,
			teardown_data_input
			);
	g_test_add("/gobject/input",
			SetupDataInput,
			NULL,
			setup_data_input,
			test_input,
			teardown_data_input
			);
	g_test_add("/gobject/archive-static",
			SetupDataArchive,
			NULL,
			setup_data_archive,
			test_archive_static,
			teardown_data_archive
			);
	g_test_add("/gobject/archive-take",
			SetupDataArchive,
			NULL,
			setup_data_archive,
			test_archive_take,
			NULL
			);
	g_test_add("/gobject/archive-null",
			SetupDataArchive,
			NULL,
			NULL,
			test_archive_null,
			teardown_data_archive
			);
	g_test_add("/gobject/archive",
			SetupDataArchive,
			NULL,
			setup_data_archive,
			test_archive,
			teardown_data_archive
			);

	int rv = g_test_run();

	unlink(TEST_XMSI);

	return rv;
}
