/*
Copyright (C) 2010-2017 Tom Schoonjans and Laszlo Vincze

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

#include "xmi_solid_angle.h"
#include "xmi_xml.h"
#include "xmi_random.h"
#include <unistd.h>
#include "xmi_main.h"

int main(int argc, char *argv[]) {
	int rv;
	xmi_inputFPtr inputFPtr;
	xmi_input *input;

	if (argc != 2)
		return 1;


	//load xml catalog
	if (xmi_xmlLoadCatalog() == 0) {
		return 1;
	}

	rv = xmi_read_input_xml(argv[1],&input);

	if (rv != 1) {
		return 1;
	}

	//copy to the corresponding fortran variable
	xmi_input_C2F(input,&inputFPtr);
	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}


	xmi_test_brute(inputFPtr);


}
