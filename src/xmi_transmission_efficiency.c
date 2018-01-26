/*
Copyright (C) 2017 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "config.h"
#include <glib/gstdio.h>
#include <string.h>
#include "xmi_aux.h"
#include "xmi_private.h"
#include "xmi_error.h"

gboolean xmi_transmission_efficiency_read(const char *filename, size_t *nefficiencies, double **energies, double **efficiencies, GError **error) {

	GArray *eff_x = g_array_new(FALSE, FALSE, sizeof(double));
	GArray *eff_y = g_array_new(FALSE, FALSE, sizeof(double));

	FILE *fp;
	if ((fp = g_fopen(filename, "r")) == NULL) {
		g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_TRANSMISSION_EFFICIENCY, "Could not open %s for reading", filename);
		g_array_free(eff_x, TRUE);
		g_array_free(eff_y, TRUE);
		return FALSE;
	}

	char *line = NULL;
	double energy, efficiency;
	ssize_t linelen;
	size_t linecap = 0;
	int values;
	while ((linelen = getline(&line, &linecap, fp)) > -1) {
		if (linelen == 0 || strlen(g_strstrip(line)) == 0) {
			continue;
		}
		values = sscanf(line,"%lg %lg", &energy, &efficiency);
		if (values != 2 || energy < 0.0 || efficiency < 0.0 || efficiency > 1.0 || (eff_x->len > 0 && energy <= g_array_index(eff_x, double, eff_x->len - 1)) || (eff_x->len == 0 && energy >= 1.0)) {
			g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_TRANSMISSION_EFFICIENCY, "Error reading %s. The transmission efficiency file should contain two columns with energies (keV) in the left column and the transmission efficiency (value between 0 and 1) in the second column. Empty lines are ignored. First energy must be between 0 and 1 keV. The last value must be greater or equal to the tube voltage. At least 10 values are required.", filename);
			g_array_free(eff_x, TRUE);
			g_array_free(eff_y, TRUE);
			return FALSE;
		}
		g_array_append_val(eff_x, energy);
		g_array_append_val(eff_y, efficiency);
		g_free(line);
		line = NULL;
	}
	fclose(fp);

	if (eff_x->len < 10/* || xep->tube_voltage > g_array_index(eff_x, double, eff_x->len - 1)*/) {
		g_set_error(error, XMI_MSIM_ERROR, XMI_MSIM_ERROR_TRANSMISSION_EFFICIENCY, "Error reading %s. The transmission efficiency file should contain two columns with energies (keV) in the left column and the transmission efficiency (value between 0 and 1) in the second column. Empty lines are ignored. First energy must be between 0 and 1 keV. The last value must be greater or equal to the tube voltage. At least 10 values are required.", filename);
		g_array_free(eff_x, TRUE);
		g_array_free(eff_y, TRUE);
		return FALSE;
	}

	*nefficiencies = eff_x->len;
	*energies = (double *) g_array_free(eff_x, FALSE);
	*efficiencies = (double *) g_array_free(eff_y, FALSE);

	return TRUE;
}
