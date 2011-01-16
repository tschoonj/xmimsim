#include <gtk/gtk.h>
#include "xmi_data_structs.h"

GtkWidget *initialize_energies(struct xmi_excitation *excitation); 

extern struct xmi_energy *energy;
extern int current_index;
extern int current_nindices;

struct energiesWidget {
	GtkListStore *store;
	GtkWidget *widget;
};

extern struct energiesWidget *contWidget;
extern struct energiesWidget *discWidget;

enum {
	ENERGY_COLUMN,
	HOR_INTENSITY_COLUMN,
	VER_INTENSITY_COLUMN,
	SIGMA_X_COLUMN,
	SIGMA_XP_COLUMN,
	SIGMA_Y_COLUMN,
	SIGMA_YP_COLUMN,
	NCOLUMNS_ENERGIES,
};
