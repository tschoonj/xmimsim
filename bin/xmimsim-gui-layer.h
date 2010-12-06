#include <gtk/gtk.h>
#include "xmi_data_structs.h"

struct layerWidget {
	GtkWidget *window;
	GtkListStore *store;
	//something for undo...
	
};

struct layerWidget * initialize_layer_widget(struct xmi_layer *); 

