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

#include "config.h"
#include "xmi_data.h"
#include "xmi_aux.h"
#include "xmi_main.h"
#include <glib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>
#include <math.h>
#include <hdf5.h>
#include "xmi_detector.h"
#include "xmi_solid_angle.h"
#include "xmi_xml.h"

#ifdef MAC_INTEGRATION
  #import <Foundation/Foundation.h>
  #include "xmi_resources_mac.h"
#endif

#ifdef G_OS_WIN32
  #include "xmi_registry_win.h"
#endif




char *cascade_group_names[4] = {"No cascade effect", "Non-radiative cascade effect", "Radiative cascade effect", "Full cascade effect"};
char *shell_names[9] = {"K shell", "L1 shell", "L2 shell", "L3 shell", "M1 shell", "M2 shell", "M3 shell", "M4 shell", "M5 shell"};


struct interaction_prob {
	int len;
	double *energies;
	double *Rayl_and_Compt;
};

struct compton_profiles {
	//Fernandez and Scot
	int shell_indices_len;
	int data_len;
	int *shell_indices;
	double *Qs;
	double *profile_partial_cdf;
	double *profile_partial_cdf_inv;
	double *Qs_inv;
	//Vincze
	double *random_numbers;
	double *profile_total_icdf;
};

struct hdf5_vars {
	hid_t file_id;
	hid_t *group_id;
	int n_group_id;
	hid_t dset_id;
	hid_t dspace_id;
};

//fortran call
void xmi_db_Z_specific(double *rayleigh_theta, double *compton_theta, double *energies, double *rs, double *fluor_yield_corr, struct interaction_prob *ip, int nintervals_r, int nintervals_e, int nintervals_e_ip, double *precalc_xrf_cs, struct compton_profiles *cp, int ncompton_profiles, int *Zs, int nZs);
void xmi_db_Z_independent(double *phi, double *thetas, double *rs, int nintervals_theta2, int nintervals_r);


int xmi_get_hdf5_data_file(char **hdf5_filePtr) {

	char *hdf5_file = *hdf5_filePtr;

	if (hdf5_file == NULL) {
		//no option detected
		//first look at default file
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_DATA,&hdf5_file) == 0)
			return 0;


		if (g_access(hdf5_file, R_OK) != 0) {
			g_fprintf(stderr, "HDF5 data file %s found in registry is not accessible\nTrying file in current directory instead\n", hdf5_file);
			if (g_access("xmimsimdata.h5", R_OK) == 0) {
				//look in current folder
				hdf5_file = g_strdup("xmimsimdata.h5");
				*hdf5_filePtr = hdf5_file;
				return 1;
			}
		}
		else {
			*hdf5_filePtr = hdf5_file;
			return 1;
		}
#elif defined(MAC_INTEGRATION)
		if (xmi_resources_mac_query(XMI_RESOURCES_MAC_DATA,&hdf5_file) == 0)
			return 0;


		if (g_access(hdf5_file, R_OK) != 0) {
			fprintf(stderr,"App bundle does not contain the HDF5 data file\n");
			return 0;
		}
		else {
			*hdf5_filePtr = hdf5_file;
			return 1;
		}
#else
		//UNIX mode...
		if (g_access(XMIMSIM_HDF5_DEFAULT, R_OK) == 0)
			hdf5_file = g_strdup(XMIMSIM_HDF5_DEFAULT);
		else if (g_access("xmimsimdata.h5", R_OK) == 0) {
			//look in current folder
			hdf5_file = g_strdup("xmimsimdata.h5");
		}
		else {
			//if not found abort...
			g_fprintf(stderr,"Could not detect the HDF5 data file\nCheck the xmimsim installation or\nuse the --with-hdf5-data option to manually pick the file\n");
			return 0;
		}
		*hdf5_filePtr = hdf5_file;
#endif
	}


	return 1;


}

int xmi_db(char *filename, int *Zs, int nZ) {
	const int nintervals_r = 2000, nintervals_e = 400, /* maxz = 94, */
	nintervals_theta2=200, nintervals_e_ip = 20000;
	const int ncompton_profiles = 10001;

	hid_t file_id;
	hid_t dset_id;
	hid_t dspace_id;
	hid_t group_id, group_id2, group_id3, group_id4;
	hid_t attribute_id;
	hid_t root_group_id;
	hid_t dataspace_id;

	double version = g_ascii_strtod(VERSION, NULL);
	//may have to invert dims
	hsize_t dims[2] = {nintervals_r, nintervals_e};
	hsize_t dims2[2] = {nintervals_r, nintervals_theta2};
	hsize_t dims_corr[1] = {9};
	hsize_t dims_ip[2];
	hsize_t dims_cp[2];
	hsize_t dims_xrf[1] = {-1*M5P5_LINE};
	gchar *elements;
	double *rayleigh_theta, *compton_theta, *energies, *rs, *fluor_yield_corr, *precalc_xrf_cs;
	double *phi, *thetas;
	struct interaction_prob* ip;
	struct compton_profiles* cp;

	int i,j,k,l,m;

	/* Create a new file using default properties. */
	hid_t gcpl = H5Pcreate (H5P_FILE_CREATE);
	H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
	file_id = H5Fcreate(filename, H5F_ACC_TRUNC, gcpl, H5P_DEFAULT);
	H5Pclose (gcpl);

	if (file_id < 0) {
		fprintf(stderr,"Could not create HDF5 XMI-MSIM data file %s\n",filename);
		return 0;
	}
	root_group_id = H5Gopen(file_id, "/", H5P_DEFAULT);

	/* create version attribute */
	dataspace_id = H5Screate(H5S_SCALAR);
	attribute_id = H5Acreate(root_group_id, "version", H5T_NATIVE_DOUBLE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, H5T_NATIVE_DOUBLE, &version);

	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);

	//write kind attribute
	dataspace_id = H5Screate(H5S_SCALAR);
	hid_t atype = H5Tcopy(H5T_C_S1);
	H5Tset_size(atype, strlen("XMI_HDF5_DATA")+1);
	H5Tset_strpad(atype,H5T_STR_NULLTERM);
	attribute_id = H5Acreate2(root_group_id, "kind", atype, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
	H5Awrite(attribute_id, atype, "XMI_HDF5_DATA");

	H5Tclose(atype);
	H5Aclose(attribute_id);
	H5Sclose(dataspace_id);


	H5Gclose(root_group_id);

	//rayleigh_theta
	rayleigh_theta = g_malloc(sizeof(double)*nZ*nintervals_e*nintervals_r);
	if (rayleigh_theta == NULL) {
		g_fprintf(stderr,"Could not allocate memory for rayleigh_theta. Aborting\n");
		return 0;
	}
	//compton_theta
	compton_theta = g_malloc(sizeof(double)*nZ*nintervals_e*nintervals_r);
	if (compton_theta == NULL) {
		g_fprintf(stderr,"Could not allocate memory for compton_theta. Aborting\n");
		return 0;
	}
	energies = g_malloc(sizeof(double)*nintervals_e);
	if (energies == NULL) {
		g_fprintf(stderr,"Could not allocate memory for energies. Aborting\n");
		return 0;
	}
	rs = g_malloc(sizeof(double)*nintervals_r);
	if (rs == NULL) {
		g_fprintf(stderr,"Could not allocate memory for rs. Aborting\n");
		return 0;
	}
	fluor_yield_corr = g_malloc(sizeof(double)*nZ*9);
	if (fluor_yield_corr == NULL) {
		g_fprintf(stderr,"Could not allocate memory for fluor_yield_corr. Aborting\n");
		return 0;
	}
	ip = g_malloc(sizeof(struct interaction_prob)*nZ);
	if (ip == NULL) {
		g_fprintf(stderr,"Could not allocate memory for ip. Aborting\n");
		return 0;
	}
	precalc_xrf_cs = g_malloc(sizeof(double)*nZ*4*9*nZ*-1*M5P5_LINE);
	if (precalc_xrf_cs == NULL) {
		g_fprintf(stderr,"Could not allocate memory for precalc_xrf_cs. Aborting\n");
		return 0;
	}

	cp = g_malloc(sizeof(struct compton_profiles)*nZ);

	xmi_db_Z_specific(rayleigh_theta, compton_theta, energies, rs, fluor_yield_corr, ip, nintervals_r, nintervals_e, nintervals_e_ip, precalc_xrf_cs, cp, ncompton_profiles, Zs, nZ);

	double *rayleigh_theta_slice = (double*) g_malloc(sizeof(double)*nintervals_r*nintervals_e);
	double *compton_theta_slice = (double*) g_malloc(sizeof(double)*nintervals_r*nintervals_e);
	double *fluor_yield_corr_slice = (double*) g_malloc(sizeof(double)*9);
	double *precalc_xrf_cs_slice = (double*) g_malloc(sizeof(double)*-1*M5P5_LINE);

	for (i = 0 ; i < nZ ; i++) {
		//these two nested for loops ensure that I extract the data correctly from these fortran column major ordered arrays
		//AND that they are written to the HDF5 file using column major...
		for (j = 0 ; j < nintervals_r ; j++) {
			for (k = 0 ; k < nintervals_e ; k++) {
				rayleigh_theta_slice[j+k*nintervals_r] = rayleigh_theta[i+nZ*j+nintervals_r*nZ*k];
				compton_theta_slice[j+k*nintervals_r] = compton_theta[i+nZ*j+nintervals_r*nZ*k];
			}
		}
		for (j=0 ; j < 9 ; j++)
			fluor_yield_corr_slice[j] = fluor_yield_corr[i+nZ*j];


		//create group for the element
		elements = g_strdup_printf("%2i", Zs[i]);
		gcpl = H5Pcreate (H5P_GROUP_CREATE);
		H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
		group_id = H5Gcreate(file_id, elements, H5P_DEFAULT, gcpl, H5P_DEFAULT);
		g_free(elements);
		H5Pclose (gcpl);
		group_id2 = H5Gcreate(group_id, "Theta_ICDF", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

		//create rayleigh theta dataset
		dspace_id = H5Screate_simple(2, dims, dims);
		dset_id = H5Dcreate(group_id2, "RayleighTheta_ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, rayleigh_theta_slice);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//create energies dataset
		dspace_id = H5Screate_simple(1, dims+1, dims+1);
		dset_id = H5Dcreate(group_id2, "Energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, energies);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//create random numbers dataset
		dspace_id = H5Screate_simple(1, dims, dims);
		dset_id = H5Dcreate(group_id2, "Random numbers",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, rs);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//create compton theta dataset
		dspace_id = H5Screate_simple(2, dims, dims);
		dset_id = H5Dcreate(group_id2, "ComptonTheta_ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, compton_theta_slice);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		//create corrected fluorescence yields dataset
		dspace_id = H5Screate_simple(1, dims_corr, dims_corr);
		dset_id = H5Dcreate(group_id2, "Corrected fluorescence yields",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, fluor_yield_corr_slice);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

        	//group close -> theta_icdf
		H5Gclose(group_id2);

		//Interaction probabilities
		group_id2 = H5Gcreate(group_id, "Interaction probabilities", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

		dims_ip[0] = 2;
		dims_ip[1] = ip[i].len;
		dspace_id = H5Screate_simple(1, dims_ip+1, dims_ip+1);
		dset_id = H5Dcreate(group_id2, "Energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, ip[i].energies);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);
		dspace_id = H5Screate_simple(2, dims_ip, dims_ip);
		dset_id = H5Dcreate(group_id2, "Rayleigh and Compton probabilities", H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, ip[i].Rayl_and_Compt);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		H5Gclose(group_id2);

		//Compton profiles
		group_id2 = H5Gcreate(group_id, "Compton profiles", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

		dims_cp[0] = cp[i].data_len;
		dims_cp[1] = cp[i].shell_indices_len;
		dspace_id = H5Screate_simple(1, dims_cp+1, dims_cp+1);
		dset_id = H5Dcreate(group_id2, "Shell indices",H5T_NATIVE_INT, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].shell_indices);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dspace_id = H5Screate_simple(1, dims_cp, dims_cp);
		dset_id = H5Dcreate(group_id2, "Qs",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].Qs);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dspace_id = H5Screate_simple(2, dims_cp, dims_cp);
		dset_id = H5Dcreate(group_id2, "Partial profile CDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].profile_partial_cdf);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dspace_id = H5Screate_simple(1, dims_cp, dims_cp);
		dset_id = H5Dcreate(group_id2, "Partial profile CDF inverted",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].profile_partial_cdf_inv);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dspace_id = H5Screate_simple(2, dims_cp, dims_cp);
		dset_id = H5Dcreate(group_id2, "Qs inverted",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].Qs_inv);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dspace_id = H5Screate_simple(1, dims_cp, dims_cp);
		dset_id = H5Dcreate(group_id2, "Random numbers",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].random_numbers);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		dspace_id = H5Screate_simple(1, dims_cp, dims_cp);
		dset_id = H5Dcreate(group_id2, "Total profile ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, cp[i].profile_total_icdf);
		H5Sclose(dspace_id);
		H5Dclose(dset_id);


		H5Gclose(group_id2);

		//precalculated XRF cross sections
		group_id2 = H5Gcreate(group_id, "Precalculated XRF cross sections", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		for (j = XMI_CASCADE_NONE ; j <= XMI_CASCADE_FULL ; j++) {
			group_id3 = H5Gcreate(group_id2, cascade_group_names[j], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
			for (k = 0 ; k <= M5_SHELL ; k++) {
				group_id4 = H5Gcreate(group_id3, shell_names[k], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
				//start loop over elements
				for (l = 0 ; l < nZ ; l++) {
					elements = g_strdup_printf("%2i", Zs[l]);
					for (m = 0 ;  m < -1*M5P5_LINE ; m++) {
						precalc_xrf_cs_slice[m]	= precalc_xrf_cs[i+
									 		nZ*j+
									 		nZ*4*k+
									 		nZ*4*(M5_SHELL+1)*l+
									 		nZ*4*(M5_SHELL+1)*nZ*m
									 		];
					}
					dspace_id = H5Screate_simple(1, dims_xrf, dims_xrf);
					dset_id = H5Dcreate(group_id4, elements,H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
					g_free(elements);
					H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, precalc_xrf_cs_slice);
					H5Sclose(dspace_id);
					H5Dclose(dset_id);
				}
				H5Gclose(group_id4);
			}
			H5Gclose(group_id3);
		}


		H5Gclose(group_id2);

		H5Gclose(group_id);
	}
	//free memory
	g_free(rayleigh_theta);
	g_free(compton_theta);
	g_free(fluor_yield_corr);
	g_free(precalc_xrf_cs);
	for (i=0 ; i < nZ ; i++) {
		xmi_deallocate(ip[i].energies);
		xmi_deallocate(ip[i].Rayl_and_Compt);
	}
	g_free(ip);
	g_free(rayleigh_theta_slice);
	g_free(compton_theta_slice);
	g_free(fluor_yield_corr_slice);
	g_free(precalc_xrf_cs_slice);

	//Z independent part
	phi = (double *) g_malloc(sizeof(double)*nintervals_theta2*nintervals_r);
	if (phi == NULL) {
		g_fprintf(stderr,"Could not allocate memory for phi. Aborting\n");
		return 0;
	}
	//thetas
	thetas = (double *) g_malloc(sizeof(double)*nintervals_theta2);
	if (thetas == NULL) {
		g_fprintf(stderr,"Could not allocate memory for thetas. Aborting\n");
		return 0;
	}

	xmi_db_Z_independent(phi, thetas, rs, nintervals_theta2, nintervals_r);

	//write to hdf5 file
	gcpl = H5Pcreate (H5P_GROUP_CREATE);
	H5Pset_link_creation_order( gcpl, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED );
	group_id = H5Gcreate(file_id, "Phi", H5P_DEFAULT, gcpl, H5P_DEFAULT);
	H5Pclose (gcpl);

	dspace_id = H5Screate_simple(2, dims2, dims2);
	dset_id = H5Dcreate(group_id, "Phi_ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, phi);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, dims2+1, dims2+1);
	dset_id = H5Dcreate(group_id, "Thetas",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, thetas);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, dims2, dims2);
	dset_id = H5Dcreate(group_id, "Random numbers",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, rs);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	H5Gclose(group_id);


	g_free(energies);
	g_free(rs);
	g_free(phi);
	g_free(thetas);
	//close file
	H5Fclose(file_id);

	return 1;
}

struct hdf5_vars *xmi_db_open(char *filename) {

	struct hdf5_vars *rv = g_malloc(sizeof(struct hdf5_vars));
	hid_t attribute_id;

	rv->file_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);

	if (rv->file_id < 0) {
		g_fprintf(stderr,"Cannot open XMI-MSIM HDF5 data file %s for reading\n", filename);
		g_free(rv);
		return NULL;
	}

	attribute_id = H5Aopen(rv->file_id, "kind", H5P_DEFAULT);
	if (attribute_id < 0) {
		//attribute does not exist
		g_fprintf(stderr, "XMI-MSIM HDF5 data file %s does not contain the kind attribute\n", filename);

		H5Fclose(rv->file_id);
		g_free(rv);
		return NULL;
	}

	hid_t atype = H5Aget_type(attribute_id);
	hid_t atype_mem = H5Tget_native_type(atype, H5T_DIR_ASCEND);
	char kind[512];
	H5Aread(attribute_id, atype_mem, kind);
	H5Tclose(atype_mem);
	H5Tclose(atype);
	H5Aclose(attribute_id);

	if (g_strcmp0(kind, "XMI_HDF5_DATA") != 0) {
		//wrong attribute value
		g_fprintf(stderr, "XMI-MSIM HDF5 data file %s does not have the correct kind attribute\n", filename);
		g_fprintf(stderr, "Expected XMI_HDF5_DATA but found %s\n", kind);
		g_fprintf(stderr, "Aborting\n");

		H5Fclose(rv->file_id);
		g_free(rv);
		return NULL;
	}

	attribute_id = H5Aopen(rv->file_id, "version", H5P_DEFAULT);
	if (attribute_id < 0) {
		//attribute does not exist
		g_fprintf(stderr, "XMI-MSIM HDF5 data file %s does not contain the version attribute\n", filename);

		H5Fclose(rv->file_id);
		g_free(rv);
		return NULL;
	}
	//attribute exists -> let's read it
	double version;
	if (H5Aread(attribute_id, H5T_NATIVE_DOUBLE, &version) < 0) {

		g_fprintf(stderr, "XMI-MSIM HDF5 data file %s version tag could not be read\n", filename);

		H5Aclose(attribute_id);
		H5Fclose(rv->file_id);
		g_free(rv);
		return NULL;
	}
	H5Aclose(attribute_id);
	if (version < XMI_DATA_MIN_VERSION) {
		g_fprintf(stderr, "XMI-MSIM HDF5 data file %s version is too old\nGenerate a new file with xmimsim-db\n", filename);
		H5Fclose(rv->file_id);
		g_free(rv);
		return NULL;
	}
	rv->group_id = NULL;
	rv->n_group_id = 0;
	return rv;
}

int xmi_db_open_group(struct hdf5_vars *hv, char *group_name) {
	hid_t open_id;

	if (hv->n_group_id == 0)
		open_id = hv->file_id;
	else
		open_id = hv->group_id[hv->n_group_id-1];

	hid_t group_id = H5Gopen(open_id, group_name, H5P_DEFAULT);
	if (group_id < 0) {
		g_fprintf(stderr,"Could not open group %s for reading\n", group_name);
		return 0;
	}

	hv->group_id = g_realloc(hv->group_id, sizeof(hid_t)*++(hv->n_group_id));
	hv->group_id[hv->n_group_id-1] = group_id;
	return 1;
}

int xmi_db_close_group(struct hdf5_vars *hv) {
	if (H5Gclose(hv->group_id[hv->n_group_id-1]) < 0) {
		g_fprintf(stderr,"Could not close group\n");
		return 0;
	}
	if (hv->n_group_id == 1) {
		g_free(hv->group_id);
		hv->group_id = NULL;
		hv->n_group_id = 0;
	}
	else
		hv->group_id = g_realloc(hv->group_id, sizeof(hid_t)*--(hv->n_group_id));

	return 1;
}

int xmi_db_open_dataset(struct hdf5_vars *hv, char *dataset_name, int *ndims, int **dims) {
	hsize_t ndims5, *dims5;
	int i;

	hv->dset_id = H5Dopen(hv->group_id[hv->n_group_id-1], dataset_name, H5P_DEFAULT);
	if (hv->dset_id < 0) {
		g_fprintf(stderr,"Could not open dataset %s\n", dataset_name);
		return 0;
	}
	hv->dspace_id = H5Dget_space(hv->dset_id);
	if (hv->dspace_id < 0) {
		g_fprintf(stderr,"Could not get dataspace\n");
		return 0;
	}

	ndims5 = H5Sget_simple_extent_ndims(hv->dspace_id);
	dims5 = g_malloc(sizeof(hsize_t)*ndims5);

	H5Sget_simple_extent_dims(hv->dspace_id, dims5, NULL);
	*ndims = (int) ndims5;
	*dims = g_malloc(sizeof(int)**ndims);
	//reverse array dimensions!
	for (i=0 ; i < *ndims ; i++)
		(*dims)[i] = (int) dims5[*ndims-1-i];
	g_free(dims5);
	return 1;
}

int xmi_db_read_dataset(struct hdf5_vars *hv, void *data, int64_t type) {

	H5Dread(hv->dset_id, (hid_t) type, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	H5Sclose(hv->dspace_id);
	H5Dclose(hv->dset_id);

	return 1;
}

int xmi_db_close(struct hdf5_vars *hv) {
	if (hv->n_group_id > 0) {
		g_fprintf(stderr,"Closing hdf5 file before all groups were closed!!!\n");
		return 0;
	}

	H5Fclose(hv->file_id);

	g_free(hv);

	return 1;
}

