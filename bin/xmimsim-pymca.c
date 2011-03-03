#include "xmi_main.h"
#include "xmi_aux.h"
#include "xmi_xml.h"
#include <xraylib.h>
#include <glib.h>
#include <glib/gstdio.h>
#include "xmi_pymca.h"
#include "xmi_data_structs.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "xmi_random.h"


int main (int argc, char *argv[]) {

	struct xmi_input *pymca_input = NULL;
	struct xmi_pymca *xp = NULL ;
	struct xmi_layer *matrix;
	double *weights_arr_quant;
	int i,j,k;
	xmi_inputFPtr inputFPtr;
	xmi_hdf5FPtr hdf5FPtr;
	GError *error = NULL;
	GOptionContext *context;
	static gchar *hdf5_file=NULL;
	struct xmi_main_options options;
	
	static GOptionEntry entries[] = {
		{"with-hdf5-data",0,0,G_OPTION_ARG_FILENAME,&hdf5_file,"Select a HDF5 data file (advanced usage)",NULL},
		{NULL}
	};
	double *channels;
	double **channels_conv;
	double zero_sum;
	long int *brute_history;
	double *var_red_history;
	double sum_k, sum_l, sum_temp;
	double sum_k_history, sum_l_history;
	struct xmi_solid_angle *solid_angle_def;
	uid_t uid, euid;
	char *xmi_input_string;


	//change euid to uid
	uid = getuid();
	euid = geteuid();

	//start without privileges
	seteuid(uid);




	options.use_M_lines = 1;
	options.use_self_enhancement = 0;
	options.use_cascade_auger = 1;
	options.use_cascade_radiative = 1;
	options.use_variance_reduction = 1;
	options.use_optimizations = 1;


	//parse options
	context = g_option_context_new ("inputfile");
	g_option_context_add_main_entries (context, entries, NULL);
	g_option_context_set_summary(context, "xmimsim-pymca: a program for the quantification of X-ray fluorescence spectra using inverse Monte-Carlo simulations. Inputfiles should be prepared using PyMCA\n");
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_print ("option parsing failed: %s\n", error->message);
		exit (1);
	}


	if (hdf5_file == NULL) {
		//no option detected
		//first look at default file
		if (g_access(XMIMSIM_HDF5_DEFAULT, F_OK | R_OK) == 0)
			hdf5_file = strdup(XMIMSIM_HDF5_DEFAULT);
		else if (g_access("xmimsimdata.h5", F_OK | R_OK) == 0)
			hdf5_file = strdup("xmimsimdata.h5");
		else {
			//if not found abort...	
			g_printf("Could not detect the HDF5 data file\nCheck the xmimsim installation or\nuse the --with-hdf5-data option to manually pick the file\n");
			exit(1);
		}
	}

	if (argc != 2)
		return 1;

	//start random number acquisition
	if (xmi_start_random_acquisition() == 0) {
		return 1;
	}

	if(xmi_read_input_pymca(argv[1], &pymca_input, &xp) == 0)
		return 1;

#if DEBUG == 1
	xmi_print_input(stdout,pymca_input);
#endif

	//calculate initial 
	xmi_copy_layer(pymca_input->composition->layers + xp->ilay_pymca, &matrix);
	weights_arr_quant = (double *) malloc(sizeof(double)*xp->n_z_arr_quant);

	for (i = 0 ; i < xp->n_z_arr_quant ; i++)
		weights_arr_quant[i] = XMI_PYMCA_START_CONC;

	pymca_input->composition->layers[xp->ilay_pymca] = xmi_ilay_composition_pymca(matrix, xp, weights_arr_quant); 

#if DEBUG == 1
	xmi_print_layer(stdout,pymca_input->composition->layers+xp->ilay_pymca,1);
#endif

	//copy to the corresponding fortran variable
	xmi_input_C2F(pymca_input,&inputFPtr);
	//initialization
	if (xmi_init_input(&inputFPtr) == 0) {
		return 1;
	}
	//initialize HDF5 data
	//read from HDF5 file what needs to be read in
	if (xmi_init_from_hdf5(hdf5_file,inputFPtr,&hdf5FPtr) == 0) {
		fprintf(stderr,"Could not initialize from hdf5 data file\n");
		return 1;
	}	

	xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);


	//check if solid angles are already precalculated
	if (xmi_find_solid_angle_match(XMIMSIM_HDF5_SOLID_ANGLES, pymca_input, &solid_angle_def) == 0)
		return 1;
	if (solid_angle_def == NULL) {
		//doesn't exist yet
		//convert input to string
		if (xmi_write_input_xml_to_string(&xmi_input_string, pymca_input) == 0) {
			return 1;
		}
		xmi_solid_angle_calculation(inputFPtr, &solid_angle_def, xmi_input_string);
		//update hdf5 file
		seteuid(euid);
		if( xmi_update_solid_angle_hdf5_file(XMIMSIM_HDF5_SOLID_ANGLES, solid_angle_def) == 0)
			return 1;
		seteuid(uid);
	}

	//everything is read in... start iteration
	i = 0;

#define ARRAY3D_FORTRAN(array,i,j,k,Ni,Nj,Nk) (array[Nj*Nk*(i-1)+Nk*(j-1)+(k-1)])

	sum_k = sum_l = XMI_PYMCA_CONV_THRESHOLD*10.0;

	channels = NULL;
	brute_history = NULL;
	var_red_history = NULL;

#if DEBUG == 1
	fprintf(stdout,"\n\nEntering iterative loop\n\n");
#endif


	while ((sum_k > XMI_PYMCA_CONV_THRESHOLD) || (sum_l > XMI_PYMCA_CONV_THRESHOLD)) {
		if (channels != NULL)
			free(channels);

		if (brute_history != NULL)
			free(brute_history);

		if (var_red_history != NULL)
			free(var_red_history);

		if (i++ > XMI_PYMCA_MAX_ITERATIONS) {
			fprintf(stderr,"No convergence after %i iterations... Fatal error\n",i);
			return 0;
		}

		//launch simulation
		if (xmi_main_msim(inputFPtr, hdf5FPtr, 1, &channels, xp->nchannels ,options, &brute_history, &var_red_history, solid_angle_def) == 0) {
			fprintf(stderr,"Error in xmi_main_msim\n");
			return 1;
		}
	
		//optimize concentrations
		sum_k = sum_l = 0.0;
		for (j = 0 ; j < xp->n_z_arr_quant ; j++) {
#if DEBUG == 1
			fprintf(stdout,"Element :%i\n",xp->z_arr_quant[j]);
#endif
			//K-lines
			//make history_sum
			sum_k_history = 0.0;
			sum_l_history = 0.0;
			for (k = 0 ; k <= pymca_input->general->n_interactions_trajectory ; k++) {
				sum_k_history += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(KL2_LINE),k,100,385,pymca_input->general->n_interactions_trajectory);
				sum_k_history += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(KL3_LINE),k,100,385,pymca_input->general->n_interactions_trajectory);
			}
			for (k = 0 ; k <= pymca_input->general->n_interactions_trajectory ; k++) {
				sum_l_history += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(L3M4_LINE),k,100,385,pymca_input->general->n_interactions_trajectory);
				sum_l_history += ARRAY3D_FORTRAN(var_red_history, xp->z_arr_quant[j], abs(L3M5_LINE),k,100,385,pymca_input->general->n_interactions_trajectory);
			}
			
			for (k = 0 ; k < xp->n_peaks ; k++) {
				if (xp->z_arr[k] == xp->z_arr_quant[j])
					break;
			}


#if DEBUG == 1
			fprintf(stdout,"sum_k_history: %lf\n",sum_k_history);
			fprintf(stdout,"xp->k_alpha[k]: %lf\n",xp->k_alpha[k]);
#endif



			if (xp->k_alpha[k] > 0.0 && sum_k_history > 0.0  ) {
				sum_temp = 0.0;
				sum_temp += (xp->k_alpha[k]-sum_k_history)*(xp->k_alpha[k]-sum_k_history);
				sum_temp /= xp->k_alpha[k];
				sum_temp /= xp->k_alpha[k];
				sum_k += sum_temp;
				weights_arr_quant[j] *= xp->k_alpha[k]/sum_k_history;
			}
			else if (xp->l_alpha[j] > 0.0 && sum_l_history > 0.0  ) {
				sum_temp = 0.0;
				sum_temp += (xp->l_alpha[k]-sum_l_history)*(xp->l_alpha[k]-sum_l_history);
				sum_temp /= xp->l_alpha[k];
				sum_temp /= xp->l_alpha[k];
				sum_l += sum_temp;
				weights_arr_quant[j] *= xp->l_alpha[k]/sum_l_history;
			}
		}
		//update concentrations in input	
		free(pymca_input->composition->layers[xp->ilay_pymca].Z);
		free(pymca_input->composition->layers[xp->ilay_pymca].weight);
		pymca_input->composition->layers[xp->ilay_pymca] = xmi_ilay_composition_pymca(matrix, xp, weights_arr_quant); 
	
#if DEBUG == 1
		fprintf(stdout,"Iteration: %i\n",i);
		fprintf(stdout,"sum_k: %lf\n",sum_k);
		fprintf(stdout,"sum_l: %lf\n",sum_l);
#endif

		//reload fortran input
		xmi_free_input_F(&inputFPtr);
		xmi_input_C2F(pymca_input, &inputFPtr);
		if (xmi_init_input(&inputFPtr) == 0) {
			return 1;
		}
		xmi_update_input_from_hdf5(inputFPtr, hdf5FPtr);

	}


	zero_sum = xmi_sum_double(channels, xp->nchannels);
	//convolute_spectrum
	channels_conv = (double **) malloc(sizeof(double *)*(pymca_input->general->n_interactions_trajectory+1));
	
	for (i=(zero_sum > 0.0 ? 0 : 1) ; i <= pymca_input->general->n_interactions_trajectory ; i++) {
		xmi_detector_convolute(inputFPtr, hdf5FPtr, channels+i*xp->nchannels, channels_conv+i, xp->nchannels);



	}

	if (xmi_end_random_acquisition() == 0) {
		return 1;
	}

	//write to xml outputfile
	if (xmi_write_output_xml("xmimsim-pymca_debug.xmso", pymca_input, brute_history, options.use_variance_reduction == 1 ? var_red_history : NULL, channels_conv, channels, xp->nchannels, argv[1], zero_sum > 0.0 ? 1 : 0) == 0) {
		return 1;
	}



	return 0;
}


