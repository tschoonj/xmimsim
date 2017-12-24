#include <config.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"
#include <glib.h>
#include <xraylib.h>

typedef struct {
	struct xmi_layer *tube_anode;
	struct xmi_layer *tube_window;
	struct xmi_layer *tube_filter;
	double tube_voltage;
	double tube_current;
	double tube_angle_electron;
	double tube_angle_xray;
	double tube_delta_energy;
	double tube_solid_angle;
	int tube_transmission;
	size_t tube_nefficiencies;
	double *tube_energies;
	double *tube_efficiencies;
} SetupData;

static struct xmi_layer* create_layer(int Z, double rho, double thickness) {
	struct xmi_layer* rv = (struct xmi_layer *) g_malloc(sizeof(struct xmi_layer));
	rv->n_elements = 1;
	rv->Z = (int *) g_malloc(sizeof(int));
	rv->Z[0] = Z;
	rv->weight = (double *) g_malloc(sizeof(double));
	rv->weight[0] = 1.0;
	rv->density = rho;
	rv->thickness = thickness;

	return rv;
}

static void setup_data_good_without_window_without_filter_without_efficiency(SetupData *data, gconstpointer user_data) {
	data->tube_voltage = 9.0;
	data->tube_current = 1.0;
	data->tube_angle_electron = 45.0;
	data->tube_angle_xray = 45.0;
	data->tube_delta_energy = 0.1;
	data->tube_solid_angle = 0.1;
	data->tube_anode = create_layer(47, ElementDensity(47), 1.0);
}

static void setup_data_good_with_window_without_filter_without_efficiency(SetupData *data, gconstpointer user_data) {
	setup_data_good_without_window_without_filter_without_efficiency(data, NULL);
	data->tube_window = create_layer(4, ElementDensity(4), 1E-4);
}

static void setup_data_good_with_window_with_filter_without_efficiency(SetupData *data, gconstpointer user_data) {
	setup_data_good_with_window_without_filter_without_efficiency(data, NULL);
	data->tube_filter = create_layer(6, ElementDensity(6), 1E-3);
}

static void setup_data_good_with_window_with_filter_with_efficiency(SetupData *data, gconstpointer user_data) {
	setup_data_good_with_window_with_filter_without_efficiency(data, user_data);
	g_assert_true(xmi_transmission_efficiency_read(TRANSMISSION_FILE, &data->tube_nefficiencies, &data->tube_energies, &data->tube_efficiencies, NULL));
}

static void setup_data_bad_with_window_with_filter_with_efficiency(SetupData *data, gconstpointer user_data) {
	setup_data_good_with_window_with_filter_without_efficiency(data, user_data);
	g_assert_false(xmi_transmission_efficiency_read("non-existent-file.txt", &data->tube_nefficiencies, &data->tube_energies, &data->tube_efficiencies, NULL));
}

static void test(SetupData *data, gconstpointer user_data) {
	struct xmi_excitation *ebel_spectrum;
	g_assert(xmi_tube_ebel(
		data->tube_anode,
		data->tube_window,
		data->tube_filter,
		data->tube_voltage,
		data->tube_current,
		data->tube_angle_electron,
		data->tube_angle_xray,
		data->tube_delta_energy,
		data->tube_solid_angle,
		data->tube_transmission,
		data->tube_nefficiencies,
		data->tube_energies,
		data->tube_efficiencies,
		&ebel_spectrum
	) == 1);
}

static void teardown_data(SetupData *data, gconstpointer user_data) {
	xmi_free_layer(data->tube_anode);
	xmi_free_layer(data->tube_window);
	xmi_free_layer(data->tube_filter);
	g_free(data->tube_anode);
	g_free(data->tube_window);
	g_free(data->tube_filter);
	g_free(data->tube_energies);
	g_free(data->tube_efficiencies);
}

int main(int argc, char *argv[]) {
	SetupData *data;

	SetErrorMessages(0);

	// init test
	g_assert(test_init() == 1);

	g_test_init(&argc, &argv, NULL);

	g_test_add("/ebel/good-without-window-without-filter-without-efficiency",
		SetupData,
		NULL,
		setup_data_good_without_window_without_filter_without_efficiency,
		test,
		teardown_data
	);
	g_test_add("/ebel/good-with-window-without-filter-without-efficiency",
		SetupData,
		NULL,
		setup_data_good_with_window_without_filter_without_efficiency,
		test,
		teardown_data
	);
	g_test_add("/ebel/good-with-window-with-filter-without-efficiency",
		SetupData,
		NULL,
		setup_data_good_with_window_with_filter_without_efficiency,
		test,
		teardown_data
	);
	g_test_add("/ebel/good-with-window-with-filter-with-efficiency",
		SetupData,
		NULL,
		setup_data_good_with_window_with_filter_with_efficiency,
		test,
		teardown_data
	);
	g_test_add("/ebel/bad-with-window-with-filter-with-efficiency",
		SetupData,
		NULL,
		setup_data_bad_with_window_with_filter_with_efficiency,
		test,
		teardown_data
	);
	return g_test_run();
}
