#include <gtk/gtk.h>
#include "xmi_data_structs.h"

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
	

};

struct layerWidget * initialize_layer_widget(struct xmi_layer **); 

