/*
Copyright (C) 2010-2013 Tom Schoonjans and Laszlo Vincze

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
#include <stdlib.h>
#include "xmi_hdf5.h"
#include "xmi_aux.h"
#include "xmi_main.h"
#include <glib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>
#include <math.h>
#include <hdf5.h>

#ifdef MAC_INTEGRATION
  #import <Foundation/Foundation.h>
  #include "xmi_resources_mac.h"
#endif

#ifdef G_OS_WIN32
  #include "xmi_registry_win.h"
#endif

struct interaction_prob {
	int len;
	double *energies;
	double *Rayl_and_Compt;
};

//fortran call
void xmi_db_Z_specific(double *rayleigh_theta, double *compton_theta, double *energies, double *rs, double *doppler_pz, double *fluor_yield_corr, struct interaction_prob *ip, int nintervals_r, int nintervals_e, int maxz, int nintervals_e_ip);
void xmi_db_Z_independent(double *rayleigh_phi, double *compton_phi, double *thetas, double *rs, double *energies, int nintervals_theta2, int nintervals_e, int nintervals_r);


int xmi_get_hdf5_data_file(char **hdf5_filePtr) {

	char *hdf5_file = *hdf5_filePtr;

	if (hdf5_file == NULL) {
		//no option detected
		//first look at default file
#ifdef G_OS_WIN32
		if (xmi_registry_win_query(XMI_REGISTRY_WIN_DATA,&hdf5_file) == 0)
			return 0;


		if (g_access(hdf5_file, F_OK | R_OK) != 0) {
			g_fprintf(stderr, "HDF5 data file %s found in registry is not accessible\nTrying file in current directory instead\n", hdf5_file);
			if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0) {
				//look in current folder
				hdf5_file = strdup("xmimsimdata.h5");
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


		if (g_access(hdf5_file, F_OK | R_OK) != 0) {
			fprintf(stderr,"App bundle does not contain the HDF5 data file\n");
			return 0;
		}
		else {
			*hdf5_filePtr = hdf5_file;
			return 1;
		}
#else
		//UNIX mode...
		if (g_access(XMIMSIM_HDF5_DEFAULT, F_OK | R_OK) == 0)
			hdf5_file = strdup(XMIMSIM_HDF5_DEFAULT);
		else if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0) {
			//look in current folder
			hdf5_file = strdup("xmimsimdata.h5");
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

int xmi_db2(char *filename) {
	const int nintervals_r = 2000, nintervals_e = 200, maxz = 94,
	nintervals_theta2=200, nintervals_e_ip = 10000;

	hid_t file_id;
	hid_t dset_id;
	hid_t dspace_id;
	hid_t group_id, group_id2;
	hid_t attribute_id;
	hid_t root_group_id;
	hid_t dataspace_id;

	double version = g_ascii_strtod(VERSION, NULL);
	//may have to invert dims 
	hsize_t dims[2] = {nintervals_r, nintervals_e};
	hsize_t dims2[2] = {nintervals_r, nintervals_theta2};
	hsize_t dims3[3] = {nintervals_r, nintervals_e, nintervals_theta2};
	hsize_t dims_corr[1] = {9};
	hsize_t dims_ip[2];
	char elements[3];
	double *rayleigh_theta, *compton_theta, *energies, *rs, *doppler_pz, *fluor_yield_corr;
	double *rayleigh_phi, * compton_phi, *thetas;
	struct interaction_prob* ip;

	int i,j,k;

	/* Create a new file using default properties. */
	file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
	H5Gclose(root_group_id);

	//rayleigh_theta
	rayleigh_theta = (double *) malloc(sizeof(double)*maxz*nintervals_e*nintervals_r);
	if (rayleigh_theta == NULL) {
		g_fprintf(stderr,"Could not allocate memory for rayleigh_theta. Aborting\n");
		return 0;
	}
	//compton_theta
	compton_theta = (double *) malloc(sizeof(double)*maxz*nintervals_e*nintervals_r);
	if (compton_theta == NULL) {
		g_fprintf(stderr,"Could not allocate memory for compton_theta. Aborting\n");
		return 0;
	}
	energies = (double *) malloc(sizeof(double)*nintervals_e);
	if (energies == NULL) {
		g_fprintf(stderr,"Could not allocate memory for energies. Aborting\n");
		return 0;
	}
	rs = (double *) malloc(sizeof(double)*nintervals_r);
	if (rs == NULL) {
		g_fprintf(stderr,"Could not allocate memory for rs. Aborting\n");
		return 0;
	}
	doppler_pz = (double *) malloc(sizeof(double)*maxz*nintervals_r);
	if (doppler_pz == NULL) {
		g_fprintf(stderr,"Could not allocate memory for doppler_pz. Aborting\n");
		return 0;
	}
	fluor_yield_corr = (double *) malloc(sizeof(double)*maxz*9);
	if (fluor_yield_corr == NULL) {
		g_fprintf(stderr,"Could not allocate memory for fluor_yield_corr. Aborting\n");
		return 0;
	}
	ip = (struct interaction_prob*) malloc(sizeof(struct interaction_prob)*maxz);
	if (ip == NULL) {
		g_fprintf(stderr,"Could not allocate memory for ip. Aborting\n");
		return 0;
	}


	xmi_db_Z_specific(rayleigh_theta, compton_theta, energies, rs, doppler_pz, fluor_yield_corr, ip, nintervals_r, nintervals_e, maxz, nintervals_e_ip);

	double *rayleigh_theta_slice = (double*) malloc(sizeof(double)*nintervals_r*nintervals_e);
	double *compton_theta_slice = (double*) malloc(sizeof(double)*nintervals_r*nintervals_e);
	double *doppler_pz_slice = (double*) malloc(sizeof(double)*nintervals_r);
	double *fluor_yield_corr_slice = (double*) malloc(sizeof(double)*9);
	
	for (i = 1 ; i <= maxz ; i++) {
		//these two nested for loops ensure that I extract the data correctly from these fortran column major ordered arrays
		//AND that they are written to the HDF5 file using column major...
		for (j = 0 ; j < nintervals_r ; j++) {
			for (k = 0 ; k < nintervals_e ; k++) {
				rayleigh_theta_slice[j+k*nintervals_r] = rayleigh_theta[i-1+maxz*j+nintervals_r*maxz*k];
				compton_theta_slice[j+k*nintervals_r] = compton_theta[i-1+maxz*j+nintervals_r*maxz*k];
			}
			doppler_pz_slice[j] = doppler_pz[i-1+maxz*j];
		}
		for (j=0 ; j < 9 ; j++) 
			fluor_yield_corr_slice[j] = fluor_yield_corr[i-1+maxz*j];
	

		//create group for the element
		sprintf(elements,"%2i", i);
		group_id = H5Gcreate(file_id, elements, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
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

		//create doppler dataset
		dspace_id = H5Screate_simple(1, dims, dims);
		dset_id = H5Dcreate(group_id2, "Doppler_pz_ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, doppler_pz_slice);	
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
		
		group_id2 = H5Gcreate(group_id, "Interaction probabilities", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

		dims_ip[0] = 2;
		dims_ip[1] = ip[i-1].len;
		dspace_id = H5Screate_simple(1, dims_ip+1, dims_ip+1);
		dset_id = H5Dcreate(group_id2, "Energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, ip[i-1].energies);	
		H5Sclose(dspace_id);
		H5Dclose(dset_id);
		dspace_id = H5Screate_simple(2, dims_ip, dims_ip);
		dset_id = H5Dcreate(group_id2, "Rayleigh and Compton probabilities", H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, ip[i-1].Rayl_and_Compt);	
		H5Sclose(dspace_id);
		H5Dclose(dset_id);

		H5Gclose(group_id2);
		H5Gclose(group_id);
	}
	//free memory
	free(rayleigh_theta);
	free(compton_theta);
	free(doppler_pz);
	free(fluor_yield_corr);
	for (i=0 ; i < maxz ; i++) {
		xmi_deallocate(ip[i].energies);
		xmi_deallocate(ip[i].Rayl_and_Compt);
	}		
	free(ip);
	free(rayleigh_theta_slice);
	free(compton_theta_slice);
	free(doppler_pz_slice);
	free(fluor_yield_corr_slice);

	//Z independent part
	//rayleigh_phi
	rayleigh_phi = (double *) malloc(sizeof(double)*nintervals_theta2*nintervals_r);
	if (rayleigh_phi == NULL) {
		g_fprintf(stderr,"Could not allocate memory for rayleigh_phi. Aborting\n");
		return 0;
	}
	//compton_phi
	compton_phi = (double *) malloc(sizeof(double)*nintervals_e*nintervals_r*nintervals_theta2);
	if (compton_phi == NULL) {
		g_fprintf(stderr,"Could not allocate memory for compton_phi. Aborting\n");
		return 0;
	}
	//thetas
	thetas = (double *) malloc(sizeof(double)*nintervals_theta2);
	if (compton_phi == NULL) {
		g_fprintf(stderr,"Could not allocate memory for thetas. Aborting\n");
		return 0;
	}

	xmi_db_Z_independent(rayleigh_phi, compton_phi, thetas, rs, energies, nintervals_theta2, nintervals_e, nintervals_r);

	//write to hdf5 file
	group_id = H5Gcreate(file_id, "RayleighPhi", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	dspace_id = H5Screate_simple(2, dims2, dims2);
	dset_id = H5Dcreate(group_id, "RayleighPhi_ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, rayleigh_phi);	
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

	group_id = H5Gcreate(file_id, "ComptonPhi", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	dspace_id = H5Screate_simple(3, dims3, dims3);
	dset_id = H5Dcreate(group_id, "ComptonPhi_ICDF",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, compton_phi);	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, dims3+2, dims3+2);
	dset_id = H5Dcreate(group_id, "Thetas",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, thetas);	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, dims3+1, dims3+1);
	dset_id = H5Dcreate(group_id, "Energies",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, energies);
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	dspace_id = H5Screate_simple(1, dims3, dims3);
	dset_id = H5Dcreate(group_id, "Random numbers",H5T_NATIVE_DOUBLE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,H5P_DEFAULT, rs);	
	H5Sclose(dspace_id);
	H5Dclose(dset_id);

	H5Gclose(group_id);

	free(energies);
	free(rs);
	free(rayleigh_phi);
	free(compton_phi);
	free(thetas);
	//close file
	H5Fclose(file_id);

	return 1;
}
