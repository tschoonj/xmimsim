#include <config.h>
#include <math.h>
#include "xmi_gobject.h"
#include "xmi_data_structs.h"

typedef struct {
	struct xmi_composition *composition;
} SetupDataComposition;

typedef struct {
	struct xmi_excitation *excitation;
} SetupDataExcitation;

static gboolean composition_equal(struct xmi_composition *A, struct xmi_composition *B) {
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

static gboolean excitation_equal(struct xmi_excitation *A, struct xmi_excitation *B) {
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
	data->composition = g_malloc(sizeof(struct xmi_composition));
	data->composition->reference_layer = 1;
	data->composition->n_layers = 1;
	data->composition->layers = g_malloc(sizeof(struct xmi_layer));
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
	data->excitation = g_malloc(sizeof(struct xmi_excitation));
	data->excitation->n_discrete = 1;
	data->excitation->n_continuous = 0;
	data->excitation->continuous = NULL;
	data->excitation->discrete = g_malloc(sizeof(struct xmi_energy_discrete));
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

static void teardown_data_composition(SetupDataComposition *data, gconstpointer user_data) {
	xmi_free_composition(data->composition);
}

static void teardown_data_excitation(SetupDataExcitation *data, gconstpointer user_data) {
	xmi_free_excitation(data->excitation);
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

int main(int argc, char *argv[]) {
	g_test_init(&argc, &argv, NULL);

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


	return g_test_run();
}
