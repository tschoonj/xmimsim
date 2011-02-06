#include <gtk/gtk.h> 
#include "xmi_data_structs.h"

struct undo_single {
	struct xmi_input *xi;
	char message[512];
	int kind;
	GtkWidget *widget;
	char *filename;
};

/*
 *
 * Gdk colors
 *
 */

extern GdkColor white;
extern GdkColor red;

extern struct undo_single *current;

/*
 *
 * enums
 *
 */

enum {
	OUTPUTFILE,
	N_PHOTONS_INTERVAL,
	N_PHOTONS_LINE,
	N_INTERACTIONS_TRAJECTORY,
	COMPOSITION,
	COMPOSITION_ORDER,
	COMPOSITION_REFERENCE,
	COMPOSITION_DELETE,
	COMPOSITION_ADD,
	COMPOSITION_EDIT,
	OPEN_FILE,
	D_SAMPLE_SOURCE,
	N_SAMPLE_ORIENTATION_X,
	N_SAMPLE_ORIENTATION_Y,
	N_SAMPLE_ORIENTATION_Z,
	P_DETECTOR_WINDOW_X,
	P_DETECTOR_WINDOW_Y,
	P_DETECTOR_WINDOW_Z,
	N_DETECTOR_ORIENTATION_X,
	N_DETECTOR_ORIENTATION_Y,
	N_DETECTOR_ORIENTATION_Z,
	AREA_DETECTOR,
	ACCEPTANCE_DETECTOR,
	D_SOURCE_SLIT,
	SLIT_SIZE_X,
	SLIT_SIZE_Y,
	DISCRETE_ENERGY_ADD,
	DISCRETE_ENERGY_EDIT,
	DISCRETE_ENERGY_DELETE,
	CONTINUOUS_ENERGY_ADD,
	CONTINUOUS_ENERGY_EDIT,
	CONTINUOUS_ENERGY_DELETE,
	EXC_COMPOSITION,
	EXC_COMPOSITION_ORDER,
	EXC_COMPOSITION_DELETE,
	EXC_COMPOSITION_ADD,
	EXC_COMPOSITION_EDIT,
	DET_COMPOSITION,
	DET_COMPOSITION_ORDER,
	DET_COMPOSITION_DELETE,
	DET_COMPOSITION_ADD,
	DET_COMPOSITION_EDIT,
	CRYSTAL_COMPOSITION,
	CRYSTAL_COMPOSITION_ORDER,
	CRYSTAL_COMPOSITION_DELETE,
	CRYSTAL_COMPOSITION_ADD,
	CRYSTAL_COMPOSITION_EDIT,
	DETECTOR_TYPE,
	DETECTOR_GAIN,
	DETECTOR_ZERO,
	DETECTOR_FANO,
	DETECTOR_NOISE,
	DETECTOR_MAX_CONVOLUTION_ENERGY,
};

void update_undo_buffer(int kind, GtkWidget *widget);

int check_changeables(void);


extern GtkWidget *saveW;
extern GtkWidget *save_asW;
extern GtkToolItem *saveasT;
extern GtkToolItem *saveT;
