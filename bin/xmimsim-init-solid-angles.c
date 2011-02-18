#include "xmi_solid_angle.h"
#include "glib.h"


int main(int argc, char *argv[]) {


xmi_create_empty_solid_angle_hdf5_file(g_path_get_basename(XMIMSIM_HDF5_SOLID_ANGLES));

return 0;

}
