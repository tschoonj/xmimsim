#ifndef LIBXMIMSIM_TEST_H
#define LIBXMIMSIM_TEST_H

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

int test_download_file(char *file);
int test_init();

#endif
