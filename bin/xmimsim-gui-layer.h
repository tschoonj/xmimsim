#include <gtk/gtk.h>
#include "xmi_data_structs.h"

enum {
	LW_ADD,
	LW_EDIT,
};

struct layerWidget {
	GtkWidget *window;
	GtkListStore *store;
	struct xmi_layer ** my_layer;
	GtkWidget *sumEntry;
	GtkWidget *densityEntry;
	GtkWidget *thicknessEntry;
	GtkWidget *editButton;
	GtkWidget *okButton;
	GtkWidget *cancelButton;
	GtkWidget *removeButton;
	int matrixKind;
	int AddOrEdit;
	int layerNumber;
	GtkTreeIter iter;
	

};

struct layerWidget * initialize_layer_widget(struct xmi_layer **); 

