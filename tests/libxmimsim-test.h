#ifndef LIBXMIMSIM_TEST_H
#define LIBXMIMSIM_TEST_H

#include <string.h>
#include <glib.h>
#include "xmi_msim.h"

#define TEST_XMSI_URL "http://github.com/tschoonj/xmimsim/wiki/test.xmsi"
#define TEST_XMSI "test.xmsi"
#define TEST_XMSI_COPY "test-copy.xmsi"

#define TEST_XMSO_URL "http://github.com/tschoonj/xmimsim/wiki/test.xmso"
#define TEST_XMSO "test.xmso"
#define TEST_XMSO_COPY "test-copy.xmso"

#define TEST_XMSA_URL_1 "http://github.com/tschoonj/xmimsim/wiki/CaSO4_28keV_pol_1D.xmsa"
#define TEST_XMSA_1 "CaSO4_28keV_pol_1D.xmsa"
#define TEST_XMSA_COPY_1 "CaSO4_28keV_pol_1D_copy.xmsa"

#define TEST_XMSA_URL_2 "http://github.com/tschoonj/xmimsim/wiki/CaSO4_28keV_pol_2D.xmsa"
#define TEST_XMSA_2 "CaSO4_28keV_pol_2D.xmsa"
#define TEST_XMSA_COPY_2 "CaSO4_28keV_pol_2D_copy.xmsa"

#define TEST_CSV "test.csv"
#define TEST_SVG "test.svg"
#define TEST_HTM "test.htm"
#define TEST_SPE "test.spe"

G_BEGIN_DECLS

int test_download_file(char *file);
int test_init();

struct spe_data {
  unsigned int channel_first;
  unsigned int channel_last;
  unsigned int nchannels;
  double *data;
};

struct spe_data * read_spe(const char *filename);

void free_spe_data(struct spe_data *);

double CS_Total_Layer(struct xmi_layer *layer, double E);

double chi(double E0, double E1, struct xmi_layer *layer, double alpha, double beta);

double fpm(struct xmi_layer *layer, int Z, int line, double w, double E0, double I0, double D, double Adet, double alpha, double beta);

struct xmi_output* run_main(const char *compound);

G_END_DECLS

#endif
