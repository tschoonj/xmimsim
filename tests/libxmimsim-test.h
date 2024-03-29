#ifndef LIBXMIMSIM_TEST_H
#define LIBXMIMSIM_TEST_H

#include <string.h>
#include <glib.h>
#include "xmi_msim.h"

#define TEST_XMSI_URL "https://github.com/tschoonj/xmimsim/wiki/test.xmsi"
#define TEST_XMSI "test.xmsi"
#define TEST_XMSI_COPY "test-copy.xmsi"

#define TEST_XMSO_URL "https://github.com/tschoonj/xmimsim/wiki/test.xmso"
#define TEST_XMSO "test.xmso"
#define TEST_XMSO_COPY "test-copy.xmso"

#define TEST_XMSA_URL_1_OLD "https://github.com/tschoonj/xmimsim/wiki/CaSO4_28keV_pol_1D_old.xmsa"
#define TEST_XMSA_1_OLD "CaSO4_28keV_pol_1D_old.xmsa"
#define TEST_XMSA_COPY_1_OLD "CaSO4_28keV_pol_1D_old_copy.xmsa"

#define TEST_XMSA_URL_2_OLD "https://github.com/tschoonj/xmimsim/wiki/CaSO4_28keV_pol_2D_old.xmsa"
#define TEST_XMSA_2_OLD "CaSO4_28keV_pol_2D_old.xmsa"
#define TEST_XMSA_COPY_2_OLD "CaSO4_28keV_pol_2D_old_copy.xmsa"

#define TEST_XMSA_URL_1_NEW "https://github.com/tschoonj/xmimsim/wiki/CaSO4_28keV_pol_1D_new.xmsa"
#define TEST_XMSA_1_NEW "CaSO4_28keV_pol_1D_new.xmsa"
#define TEST_XMSA_COPY_1_NEW "CaSO4_28keV_pol_1D_new_copy.xmsa"

#define TEST_XMSA_URL_2_NEW "https://github.com/tschoonj/xmimsim/wiki/CaSO4_28keV_pol_2D_new.xmsa"
#define TEST_XMSA_2_NEW "CaSO4_28keV_pol_2D_new.xmsa"
#define TEST_XMSA_COPY_2_NEW "CaSO4_28keV_pol_2D_new_copy.xmsa"

#define TEST_CSV "test.csv"
#define TEST_HTM "test.htm"
#define TEST_SPE "test.spe"

G_BEGIN_DECLS

int test_download_file(char *file);
int test_init(void);

struct spe_data {
  unsigned int channel_first;
  unsigned int channel_last;
  unsigned int nchannels;
  double *data;
};

struct spe_data * read_spe(const char *filename);

void free_spe_data(struct spe_data *);

double CS_Total_Layer(xmi_layer *layer, double E);

double chi(double E0, double E1, xmi_layer *layer, double alpha, double beta);

double fpm(xmi_layer *layer, int Z, int line, double w, double E0, double I0, double D, double Adet, double alpha, double beta);

xmi_output* run_main(const char *compound);

int replace_xml_tag(const char *filename_old, const char *filename_new, const char *xpath_expression, const char *new_value);

int remove_xml_tags(const char *filename_old, const char *filename_new, const char *xpath_expression);

gboolean test_log_fatal_false(const gchar *log_domain, GLogLevelFlags log_level, const gchar *message, gpointer user_data);

void test_compare_channels_and_csv(int nchannels, double **channels, const gchar *csv_file);

void test_compare_channels_and_spe(int nchannels, double *channels, const gchar *spe_file);
G_END_DECLS

#endif
