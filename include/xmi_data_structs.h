#ifndef XMI_DATA_STRUCTS_H
#define XMI_DATA_STRUCTS_H



struct xmi_general {
	int version;
	char *outputfile;
	long int n_photons_interval;
	long int n_photons_line;
	int n_interactions_trajectory;
};


struct xmi_layer {
	int n_elements;
	int *Z;
	double *weight;
	double density;
	double thickness;
};





struct xmi_composition {
	int n_layers;
	struct xmi_layer *layers;
};


struct xmi_geometry {
	double d_sample_source;
	double n_sample_orientation[3];
	double p_detector_window[3];
	double n_detector_orientation[3];
	double area_detector;
	double acceptance_detector;
	double sigma_x;
	double sigma_xp;
	double sigma_y;
	double sigma_yp;
	double d_source_slit;
	double slit_size_x;
	double slit_size_y;
};

struct xmi_energy {
	double energy;
	double horizontal_intensity;
	double vertical_intensity;
};

struct xmi_excitation {
	int n_discrete;
	struct xmi_energy *discrete;
	int n_continuous;
	struct xmi_energy *continuous;
};


struct xmi_absorbers {
	int n_exc_layers;
	struct xmi_layer *exc_layers;
	int n_det_layers;
	struct xmi_layer *det_layers;
};

#define XMI_DETECTOR_SILI 0
#define XMI_DETECTOR_GE 1
#define XMI_DETECTOR_SI_SDD 2



struct xmi_detector {
	int detector_type;
	double gain;
	double zero;
	double fano;
	double noise;
	double max_convolution_energy;
	int n_crystal_layers;
	struct xmi_layer *crystal_layers;
};


struct xmi_input {
	struct xmi_general *general;
	struct xmi_composition *composition;
	struct xmi_geometry *geometry;
	struct xmi_excitation *excitation;
	struct xmi_absorbers *absorbers;
	struct xmi_detector *detector;
};




void xmi_free_input(struct xmi_input *);

int xmi_compare_input(struct xmi_input *A, struct xmi_input *B);

void xmi_copy_input(struct xmi_input *A, struct xmi_input *B);


#endif
