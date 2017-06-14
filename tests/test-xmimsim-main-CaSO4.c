#include <config.h>
#include <xraylib.h>
#include <math.h>
#include "libxmimsim-test.h"
#include "xmi_msim.h"


int main(int argc, char **argv) {

	const char compound[] = "CaSO4";

	// analyze output by comparing results with FPM
	struct xmi_output *output = run_main(compound);

	// use Ca-KL3
	double CaKL3_count = xmi_get_output_counts_for_element_line(output, 20, KL3_LINE);
	fprintf(stdout, "CaKL3_count: %g\n", CaKL3_count);

	// XAS tool result is 1.7015E6
	g_assert(fabs(CaKL3_count-1.725E6)/1.725E6 < 0.01);

	// cleanup
	xmi_free_output(output);

	return 0;
}
