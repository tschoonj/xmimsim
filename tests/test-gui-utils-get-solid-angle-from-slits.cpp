#include <config.h>
#include "libxmimsim-test.h"
#include "xmimsim-gui-utils.h"
#include <math.h>
#include <stdio.h>

int main(int argc, char *argv[]) {

  //init test
  g_assert(test_init() == 1);

  struct xmi_geometry geom = {.slit_size_x = 0.1, .slit_size_y = 100.0, .d_source_slit = 5.0};

  double solid_angle = xmi_msim_gui_utils_get_solid_angle_from_slits(&geom);
  fprintf(stdout, "first solid angle %f\n", solid_angle);

  g_assert(fabs(solid_angle - 0.03980015440423797) < 1E-6);

  // flip slits
  geom.slit_size_x = 100.0;
  geom.slit_size_y = 0.1;

  solid_angle = xmi_msim_gui_utils_get_solid_angle_from_slits(&geom);
  fprintf(stdout, "second solid angle %f\n", solid_angle);

  g_assert(fabs(solid_angle - 0.1997936) < 1E-6);

  return 0;
}
