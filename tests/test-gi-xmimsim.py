from __future__ import print_function
import unittest
import sys
import os
import math
from timeit import default_timer as timer
import logging
logging.basicConfig(format='%(asctime)s %(message)s', level=logging.DEBUG)

try:
    import gi
except ImportError as e:
    sys.exit(77)

# now for some python 2/3 stuff... I am trying to avoid a dependency on six here...
try:
    from urllib import urlretrieve as _urlretrieve
except Exception as e:
    from urllib.request import urlretrieve as _urlretrieve

# check if lxml is available... necessary for some of the XMSI/XMSO tests
try:
    import lxml.etree as etree
    HAVE_LXML = True
except ImportError as e:
    HAVE_LXML = False

# check if xraylib is available... necessary for job tests
try:
    import xraylib as xrl
    HAVE_XRAYLIB = True
except ImportError as e:
    HAVE_XRAYLIB = False

import ssl

openssl_cafile = ssl.get_default_verify_paths().openssl_cafile
print("openssl_cafile: {} -> {}".format(openssl_cafile, os.path.exists(openssl_cafile)))

gi.require_version('XmiMsim', '1.0')
from gi.repository import XmiMsim, GLib, Gio

TEST_XMSI_URL = "https://github.com/tschoonj/xmimsim/wiki/test.xmsi"
TEST_XMSI = "test.xmsi"
TEST_XMSI_COPY = "test-copy.xmsi"

TEST_XMSO_URL = "https://github.com/tschoonj/xmimsim/wiki/test.xmso"
TEST_XMSO = "test.xmso"
TEST_XMSO_COPY = "test-copy.xmso"

SQRT_2 = math.sqrt(2.0)
SQRT_2_2 = math.sqrt(2.0)/2.0

# Load the XML catalog
XmiMsim.xmlLoadCatalog()

def urlretrieve(url):
    url_file = Gio.File.new_for_uri(url)
    outputfile = url_file.get_basename()
    local_outputfilename = GLib.build_filenamev([os.environ['ABS_TOP_SRCDIR'], '..', 'xmimsim.wiki', outputfile])
    local_outputfile = Gio.File.new_for_path(local_outputfilename)
    local_outputfile_copy = Gio.File.new_for_path(outputfile)

    try:
        logging.debug("trying to copy {}".format(local_outputfilename))
        local_outputfile.copy(local_outputfile_copy, Gio.FileCopyFlags.OVERWRITE)
    except Exception as e:
        logging.debug("local copying failed: {}".format(e))
        _urlretrieve(url, outputfile)

@unittest.skipUnless(hasattr(XmiMsim, "GoogleAnalyticsTracker"), "XmiMsim was compiled without support for Google Analytics event tracking")
class TestGoogleAnalyticsTracker(unittest.TestCase):
    TEST_UUID = "7c06f818-a4ba-467c-b045-d15e7ec79177"

    def after_event_cb(self, tracker, msg):
        self.assertIsNone(msg)
        self.main_loop.quit()

    def setUp(self):
        self.main_loop = GLib.MainLoop.new(None, False)

    def tearDown(self):
        del self.main_loop

    def test_send_from_local_tracker_fixed_uuid(self):
        tracker = XmiMsim.GoogleAnalyticsTracker.new(TestGoogleAnalyticsTracker.TEST_UUID)
        self.assertIsNotNone(tracker)
        tracker.connect('after-event', self.after_event_cb)
        self.assertTrue(tracker.send_event("MAKE-CHECK-TEST-GI", "LOCAL TRACKER FIXED UUID"))

        self.main_loop.run()
        del tracker

    def test_send_from_global_tracker_fixed_uuid(self):
        XmiMsim.GoogleAnalyticsTracker.create_global(TestGoogleAnalyticsTracker.TEST_UUID)
        tracker = XmiMsim.GoogleAnalyticsTracker.get_global()
        self.assertIsNotNone(tracker)
        tracker.connect('after-event', self.after_event_cb)
        self.assertTrue(tracker.send_event("MAKE-CHECK-TEST-GI", "GLOBAL TRACKER FIXED UUID"))

        self.main_loop.run()
        XmiMsim.GoogleAnalyticsTracker.free_global()
        self.assertIsNone(XmiMsim.GoogleAnalyticsTracker.get_global())


class TestReadXMSI(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # download XMSI file
        urlretrieve(TEST_XMSI_URL)

    @classmethod
    def tearDownClass(cls):
        # remove the XMSI file
        os.remove(TEST_XMSI)

    def setUp(self):
        self.input = XmiMsim.Input.read_from_xml_file(TEST_XMSI)


    def test_example_file_contents(self):
        self.assertEqual(self.input.validate(), 0)

        # general
        general = self.input.general
        self.assertEqual(general.outputfile, 'test.xmso')
        self.assertEqual(general.n_photons_interval, 10000)
        self.assertEqual(general.n_photons_line, 1000000)
        self.assertEqual(general.n_interactions_trajectory, 4)

        # composition
        composition = self.input.composition
        self.assertEqual(composition.n_layers, 2)

        layer = composition.get_layer(0)
        self.assertEqual(layer.n_elements, 3)
        self.assertEqual(layer.Z[0], 7)
        self.assertAlmostEqual(layer.weight[0], 0.7)
        self.assertEqual(layer.Z[1], 8)
        self.assertAlmostEqual(layer.weight[1], 0.29)
        self.assertEqual(layer.Z[2], 18)
        self.assertAlmostEqual(layer.weight[2], 0.01)
        self.assertAlmostEqual(layer.density, 0.001)
        self.assertAlmostEqual(layer.thickness, 2)

        layer = composition.get_layer(1)
        self.assertEqual(layer.n_elements, 4)
        self.assertEqual(layer.Z[0], 8)
        self.assertAlmostEqual(layer.weight[0], 0.276479)
        self.assertEqual(layer.Z[1], 16)
        self.assertAlmostEqual(layer.weight[1], 0.100464)
        self.assertEqual(layer.Z[2], 29)
        self.assertAlmostEqual(layer.weight[2], 0.199048)
        self.assertEqual(layer.Z[3], 92)
        self.assertAlmostEqual(layer.weight[3], 0.424009)
        self.assertAlmostEqual(layer.density, 2.5)
        self.assertAlmostEqual(layer.thickness, 1)

        self.assertEqual(composition.reference_layer, 2)

        # geometry
        geometry = self.input.geometry
        self.assertAlmostEqual(geometry.d_sample_source, 100.0)
        self.assertAlmostEqual(geometry.n_sample_orientation[0], 0.0)
        self.assertAlmostEqual(geometry.n_sample_orientation[1], SQRT_2_2)
        self.assertAlmostEqual(geometry.n_sample_orientation[2], SQRT_2_2)
        self.assertAlmostEqual(geometry.p_detector_window[0], 0.0)
        self.assertAlmostEqual(geometry.p_detector_window[1], -1.0)
        self.assertAlmostEqual(geometry.p_detector_window[2], 100.0)
        self.assertAlmostEqual(geometry.n_detector_orientation[0], 0.0)
        self.assertAlmostEqual(geometry.n_detector_orientation[1], 1.0)
        self.assertAlmostEqual(geometry.n_detector_orientation[2], 0.0)
        self.assertAlmostEqual(geometry.area_detector, 0.3)
        self.assertAlmostEqual(geometry.collimator_height, 0.0)
        self.assertAlmostEqual(geometry.collimator_diameter, 0.0)
        self.assertAlmostEqual(geometry.d_source_slit, 100.0)
        self.assertAlmostEqual(geometry.slit_size_x, 0.001)
        self.assertAlmostEqual(geometry.slit_size_y, 0.001)

        # excitation
        excitation = self.input.excitation
        self.assertEqual(excitation.n_continuous, 0)
        self.assertEqual(excitation.n_discrete, 1)
        discrete = excitation.get_energy_discrete(0)
        self.assertAlmostEqual(discrete.energy, 20.0)
        self.assertAlmostEqual(discrete.horizontal_intensity, 1E9)
        self.assertAlmostEqual(discrete.vertical_intensity, 1E9)
        self.assertAlmostEqual(discrete.sigma_x, 0.0)
        self.assertAlmostEqual(discrete.sigma_y, 0.0)
        self.assertAlmostEqual(discrete.sigma_xp, 0.0)
        self.assertAlmostEqual(discrete.sigma_yp, 0.0)
        self.assertAlmostEqual(discrete.distribution_type, XmiMsim.EnergyDiscreteDistribution.MONOCHROMATIC)

        # absorbers
        absorbers = self.input.absorbers
        self.assertEqual(absorbers.n_exc_layers, 0)
        self.assertEqual(absorbers.n_det_layers, 1)
        layer = absorbers.get_det_layer(0)
        self.assertEqual(layer.n_elements, 1)
        self.assertEqual(layer.Z[0], 4)
        self.assertAlmostEqual(layer.weight[0], 1.0)
        self.assertAlmostEqual(layer.density, 1.85)
        self.assertAlmostEqual(layer.thickness, 0.002)

        # detector
        detector = self.input.detector
        self.assertEqual(detector.detector_type, XmiMsim.DetectorConvolutionProfile.SILI)
        self.assertAlmostEqual(detector.live_time, 1.0)
        self.assertAlmostEqual(detector.pulse_width, 1.0E-5)
        self.assertEqual(detector.nchannels, 2048)
        self.assertAlmostEqual(detector.gain, 0.02)
        self.assertAlmostEqual(detector.zero, 0.0)
        self.assertAlmostEqual(detector.fano, 0.12)
        self.assertAlmostEqual(detector.noise, 0.1)
        self.assertAlmostEqual(detector.n_crystal_layers, 1)
        layer = detector.get_crystal_layer(0)
        self.assertEqual(layer.n_elements, 1)
        self.assertEqual(layer.Z[0], 14)
        self.assertAlmostEqual(layer.weight[0], 1.0)
        self.assertAlmostEqual(layer.density, 2.33)
        self.assertAlmostEqual(layer.thickness, 0.5)

    def test_copy(self):
        input_copy = self.input.copy()
        self.assertTrue(input_copy.equals(self.input))
        self.assertIsNot(input_copy, self.input)

    def test_reading_non_existent_file(self):
        try:
            XmiMsim.Input.read_from_xml_file("non-existent-file.xmsi")
            self.fail("Reading non-existent must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            #print(err.message)
    
    @unittest.skipUnless(HAVE_LXML, "Install lxml to run this test")
    def test_invalid_tag(self):
        tree = etree.parse(TEST_XMSI)
        item = tree.xpath("/xmimsim/general/n_photons_interval")[0]
        item.text = "hsdhodhoosda"
        etree.ElementTree(tree.getroot()).write(TEST_XMSI_COPY, xml_declaration=True, pretty_print=True)
        try:
            XmiMsim.Input.read_from_xml_file(TEST_XMSI_COPY)
            self.fail("Reading invalid file must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            self.assertEqual(err.message, "error reading in n_photons_interval of xml file")
        finally:
            os.remove(TEST_XMSI_COPY)

    @unittest.skipUnless(HAVE_LXML, "Install lxml to run this test")
    def test_missing_tag(self):
        tree = etree.parse(TEST_XMSI)
        item = tree.xpath("/xmimsim/general/n_photons_interval")[0]
        item.getparent().remove(item)
        etree.ElementTree(tree.getroot()).write(TEST_XMSI_COPY, xml_declaration=True, pretty_print=True)
        try:
            XmiMsim.Input.read_from_xml_file(TEST_XMSI_COPY)
            self.fail("Reading invalid file must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            self.assertTrue(err.message.startswith("Element general content does not follow the DTD, expecting"))
        finally:
            os.remove(TEST_XMSI_COPY)

    def test_new_constructor(self):
        # generate the contents of TEST_XMSI from python
        # general
        general = XmiMsim.General.new('test.xmso', n_photons_interval=10000, n_photons_line=1000000, n_interactions_trajectory=4)
        self.assertTrue(general.equals(self.input.general))

        # composition
        layer0 = XmiMsim.Layer.new([7, 8, 18], [0.7, 0.29, 0.01], 0.001, 2)
        layer1 = XmiMsim.Layer.new([8, 16, 29, 92], [0.276479, 0.100464, 0.199048, 0.424009], 2.5, 1)
        composition = XmiMsim.Composition.new([layer0, layer1], reference_layer=2)
        self.assertTrue(composition.equals(self.input.composition))

        # geometry
        geometry = XmiMsim.Geometry.new( \
                d_sample_source=100.0,\
                n_sample_orientation=[0.0, 1.0, 1.0],\
                p_detector_window=[0.0, -1.0, 100],\
                n_detector_orientation=[0, 1, 0],\
                area_detector=0.3,\
                collimator_height=0.0,\
                collimator_diameter=0.0,\
                d_source_slit=100.0,\
                slit_size_x=0.001,\
                slit_size_y=0.001\
                )
        self.assertTrue(geometry.equals(self.input.geometry))

        # excitation
        excitation = XmiMsim.Excitation.new(discrete=[XmiMsim.EnergyDiscrete.new(20.0, 1E9, 1E9, 0.0, 0.0, 0.0, 0.0, XmiMsim.EnergyDiscreteDistribution.MONOCHROMATIC, 0.0)])
        self.assertTrue(excitation.equals(self.input.excitation))

        # absorbers
        absorbers = XmiMsim.Absorbers.new(det_layers=[XmiMsim.Layer.new(Z=[4], weight=[1.0], density=1.85, thickness=0.002)])
        self.assertTrue(absorbers.equals(self.input.absorbers))

        # detector
        detector = XmiMsim.Detector.new(\
                detector_type=XmiMsim.DetectorConvolutionProfile.SILI, \
                live_time=1.0, \
                pulse_width=1.0E-5, \
                gain=0.02, \
                zero=0, \
                fano=0.12, \
                noise=0.1, \
                nchannels=2048, \
                crystal_layers=[XmiMsim.Layer.new(Z=[14], weight=[1.0], density=2.33, thickness=0.5)] \
                )
        self.assertTrue(detector.equals(self.input.detector))

        # input
        input = XmiMsim.Input.new(general, composition, geometry, excitation, absorbers, detector)
        self.assertIsNot(input.general, general)
        self.assertIsNot(input.composition, composition)
        self.assertIsNot(input.geometry, geometry)
        self.assertIsNot(input.excitation, excitation)
        self.assertIsNot(input.absorbers, absorbers)
        self.assertIsNot(input.detector, detector)
        self.assertEqual(input.validate(), 0)
        self.assertTrue(input.equals(self.input))

class TestWriteXMSI(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # download XMSI file
        urlretrieve(TEST_XMSI_URL)

    @classmethod
    def tearDownClass(cls):
        # remove the XMSI file
        os.remove(TEST_XMSI)

    def setUp(self):
        self.input = XmiMsim.Input.read_from_xml_file(TEST_XMSI)

    def test_write(self):
        self.input.write_to_xml_file(TEST_XMSI_COPY)
        input_copy = XmiMsim.Input.read_from_xml_file(TEST_XMSI_COPY)
        self.assertTrue(input_copy, self.input)
        os.remove(TEST_XMSI_COPY)

    def test_write_to_nonexistent_folder(self):
        try:
            self.input.write_to_xml_file(os.path.join("non-existent-folder", TEST_XMSI_COPY))
            self.fail("Writing an invalid file must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            self.assertTrue("No such file or directory" in err.message)
        
class TestReadXMSO(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # download XMSO file
        urlretrieve(TEST_XMSO_URL)

    @classmethod
    def tearDownClass(cls):
        # remove the XMSO file
        os.remove(TEST_XMSO)

    def setUp(self):
        self.output = XmiMsim.Output.read_from_xml_file(TEST_XMSO)

    def test_validate_input(self):
        self.assertEqual(self.output.input.validate(), 0)

    def test_copy(self):
        output_copy = self.output.copy()
        self.assertEqual(output_copy.input.validate(), 0)
        self.assertTrue(output_copy.input.equals(self.output.input))
        self.assertTrue(output_copy.equals(self.output))

    def test_reading_non_existent_file(self):
        try:
            XmiMsim.Output.read_from_xml_file("non-existent-file.xmso")
            self.fail("Reading non-existent must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            #print(err.message)

    @unittest.skipUnless(HAVE_LXML, "Install lxml to run this test")
    def test_invalid_tag(self):
        tree = etree.parse(TEST_XMSO)
        item = tree.xpath("/xmimsim-results/spectrum_conv/channel[1]/counts[1]")[0]
        item.text = "hsdhodhoosda"
        etree.ElementTree(tree.getroot()).write(TEST_XMSO_COPY, xml_declaration=True, pretty_print=True)
        try:
            XmiMsim.Output.read_from_xml_file(TEST_XMSO_COPY)
            self.fail("Reading invalid file must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            #print("message: {}".format(err.message))
            self.assertEqual(err.message, "could not read counts")
        finally:
            os.remove(TEST_XMSO_COPY)

    @unittest.skipUnless(HAVE_LXML, "Install lxml to run this test")
    def test_missing_tag(self):
        tree = etree.parse(TEST_XMSO)
        item = tree.xpath("/xmimsim-results/brute_force_history")[0]
        item.getparent().remove(item)
        etree.ElementTree(tree.getroot()).write(TEST_XMSO_COPY, xml_declaration=True, pretty_print=True)
        try:
            XmiMsim.Output.read_from_xml_file(TEST_XMSO_COPY)
            self.fail("Reading invalid file must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            #print("message: {}".format(err.message))
            self.assertTrue(err.message.startswith("Element xmimsim-results content does not follow the DTD, expecting"))
        finally:
            os.remove(TEST_XMSO_COPY)

    def test_conv_spectra(self):
        spec0 = self.output.get_spectrum_convoluted(0)
        self.assertFalse(any(spec0))
        spec4 = self.output.get_spectrum_convoluted(4)
        self.assertAlmostEqual(spec4[10], 175.588)

    def test_unconv_spectra(self):
        spec0 = self.output.get_spectrum_unconvoluted(0)
        self.assertFalse(any(spec0))
        spec4 = self.output.get_spectrum_unconvoluted(4)
        self.assertAlmostEqual(spec4[1000], 352636.0)

    def test_history(self):
        # Cu-KL3
        self.assertAlmostEqual(self.output.get_history()[29].lines['KL3'].interactions, [0.0, 341906.0, 80777.5, 3453.95, 142.371])

class TestWriteXMSO(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # download XMSO file
        urlretrieve(TEST_XMSO_URL)

    @classmethod
    def tearDownClass(cls):
        # remove the XMSO file
        os.remove(TEST_XMSO)

    def setUp(self):
        self.output = XmiMsim.Output.read_from_xml_file(TEST_XMSO)

    def test_write(self):
        self.output.write_to_xml_file(TEST_XMSO_COPY)
        output_copy = XmiMsim.Output.read_from_xml_file(TEST_XMSO_COPY)
        self.assertTrue(output_copy, self.output)
        os.remove(TEST_XMSO_COPY)

    def test_write_to_nonexistent_folder(self):
        try:
            self.output.write_to_xml_file(os.path.join("non-existent-folder", TEST_XMSO_COPY))
            self.fail("Writing an invalid file must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))
            self.assertTrue("No such file or directory" in err.message)

@unittest.skipUnless(HAVE_XRAYLIB, "Install xraylib's python bindings to run this test")
class TestJob(unittest.TestCase):

    COMPOUND = "C6H12O6" # sweet, sweet sugar
    EXTRA_OPTIONS = (
    "--with-hdf5-data=xmimsimdata-" + COMPOUND + ".h5",
    "--with-solid-angles-data=solid-angles.h5",
    "--with-escape-ratios-data=escape-ratios.h5"
        )

    @classmethod
    def setUpClass(cls):
        if not XmiMsim.Job.is_suspend_available():
            raise 
        XmiMsim.init_hdf5()
        cls.cd = xrl.CompoundParser(cls.COMPOUND)
        if cls.cd is None:
            raise
        cls.data_file = "xmimsimdata-" + cls.COMPOUND + ".h5"
        logging.debug("data_file: {}".format(cls.data_file))
        if XmiMsim.db(cls.data_file, cls.cd['Elements']) is not 1:
            raise

    @classmethod
    def tearDownClass(cls):
        os.remove(cls.data_file)

    def setUp(self):
        self.main_loop = GLib.MainLoop.new(None, False)
        self.input = XmiMsim.Input.init_empty()
        # simulate 10M photons brute force
        general = self.input.general.copy()
        general.n_photons_line = 10000000
        self.input.set_general(general)
        self.options = XmiMsim.MainOptions.new()
        self.options.use_variance_reduction = False # brute force!
        self.options.use_escape_peaks = False # no escape peaks!

        layer = XmiMsim.Layer.new(type(self).cd['Elements'], type(self).cd['massFractions'], 1.0, 1.0)
        composition = XmiMsim.Composition.new([layer], reference_layer=1)
        self.input.set_composition(composition)

    def test_no_executable(self):
        general = self.input.general.copy()
        general.outputfile = type(self).COMPOUND + "-test.xmso"
        self.input.set_general(general)
        self.assertEqual(self.input.validate(), 0)
        self.assertTrue(self.input.write_to_xml_file(type(self).COMPOUND + "-test.xmsi"))

        try:
            job = XmiMsim.Job.new(
                os.environ["XMIMSIM_NON_EXISTENT_EXEC"],
                type(self).COMPOUND + "-test.xmsi",
                self.options,
                extra_options=type(self).EXTRA_OPTIONS
                )
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        os.remove(type(self).COMPOUND + "-test.xmsi")

    def _test_fail_finished_cb(self, job, result, buffer):
        logging.debug("test_fail_finished_cb time elapsed: {}".format(timer() - self.start))
        logging.debug("message: {}".format(buffer))
        self.main_loop.quit()

    def _print_stdout(self, job, string):
        logging.debug("stdout: {}".format(string))

    def _print_stderr(self, job, string):
        logging.debug("stderr: {}".format(string))

    def test_no_input_file(self):
        try:
            job = XmiMsim.Job.new(
                os.environ["XMIMSIM_EXEC"],
                "non-existent-file.xmsi",
                self.options,
                extra_options=type(self).EXTRA_OPTIONS
                )
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))

    def test_bad_input_file(self):
        general = self.input.general.copy()
        general.outputfile = type(self).COMPOUND + "-test.xmso"
        self.input.set_general(general)
        self.assertEqual(self.input.validate(), 0)
        # invalidate file
        composition = self.input.composition.copy()
        composition.reference_layer = 5
        self.input.set_composition(composition)
        self.assertEqual(self.input.validate(), XmiMsim.InputFlags.COMPOSITION)
        self.assertTrue(self.input.write_to_xml_file(type(self).COMPOUND + "-test.xmsi"))

        try:
            job = XmiMsim.Job.new(
                os.environ["XMIMSIM_EXEC"],
                type(self).COMPOUND + "-test.xmsi",
                self.options,
                extra_options=type(self).EXTRA_OPTIONS
                )
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.Error.quark(), XmiMsim.Error.XML))

        os.remove(type(self).COMPOUND + "-test.xmsi")

    def _test_special_event_cb(self, job, event, buffer):
        self.assertTrue(job.is_running())
        # this test is running in brute force mode, so we know exactly what to expect
        if event == XmiMsim.JobSpecialEvent.SOLID_ANGLE:
            self.assertEqual(buffer, "Solid angle grid redundant")
        elif event == XmiMsim.JobSpecialEvent.ESCAPE_PEAKS:
            self.assertEqual(buffer, "Escape peaks redundant")
        logging.debug("special_event: {}".format(buffer))

    def _test_succeed_finished_cb(self, job, result, buffer):
        logging.debug("test_succeed_finished_cb time elapsed: {}".format(timer() - self.start))
        logging.debug("message: {}".format(buffer))
        self.main_loop.quit()

    def test_good_input_file_simple(self):
        general = self.input.general.copy()
        general.outputfile = type(self).COMPOUND + "-test.xmso"
        self.input.set_general(general)
        self.assertEqual(self.input.validate(), 0)
        self.assertTrue(self.input.write_to_xml_file(type(self).COMPOUND + "-test.xmsi"))

        job = XmiMsim.Job.new(
            os.environ["XMIMSIM_EXEC"],
            type(self).COMPOUND + "-test.xmsi",
            self.options,
            extra_options=type(self).EXTRA_OPTIONS
            )
        self.assertIsNotNone(job)

        # hook up signals
        job.connect('finished-event', self._test_succeed_finished_cb)
        job.connect('special-event', self._test_special_event_cb)
        job.connect('stdout-event', self._print_stdout)
        job.connect('stderr-event', self._print_stderr)

        logging.debug("command: {}".format(job.get_command()))
        job.start()
        self.start = timer()
        self.main_loop.run()

        self.assertFalse(job.is_running())
        self.assertFalse(job.is_suspended())
        self.assertTrue(job.has_finished())
        self.assertTrue(job.was_successful())
        try:
            job.stop()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.suspend()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.resume()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        try:
            job.start()
            self.fail("Starting a job after is has been run already must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        os.remove(type(self).COMPOUND + "-test.xmsi")
        os.remove(type(self).COMPOUND + "-test.xmso")

    def _stop_timeout(self, job):
        self.assertTrue(job.stop())
        logging.debug("message: job stopped")
        return False

    def test_good_input_file_stop(self):
        general = self.input.general.copy()
        general.outputfile = type(self).COMPOUND + "-test.xmso"
        self.input.set_general(general)
        self.assertEqual(self.input.validate(), 0)
        self.assertTrue(self.input.write_to_xml_file(type(self).COMPOUND + "-test.xmsi"))

        job = XmiMsim.Job.new(
            os.environ["XMIMSIM_EXEC"],
            type(self).COMPOUND + "-test.xmsi",
            self.options,
            extra_options=type(self).EXTRA_OPTIONS
            )
        self.assertIsNotNone(job)

        # hook up signals
        job.connect('finished-event', self._test_fail_finished_cb)
        job.connect('special-event', self._test_special_event_cb)
        job.connect('stdout-event', self._print_stdout)
        job.connect('stderr-event', self._print_stderr)

        logging.debug("command: {}".format(job.get_command()))
        job.start()
        self.start = timer()

        GLib.timeout_add_seconds(5, self._stop_timeout, job)

        self.main_loop.run()

        self.assertFalse(job.is_running())
        self.assertFalse(job.is_suspended())
        self.assertTrue(job.has_finished())
        self.assertFalse(job.was_successful())
        try:
            job.stop()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.suspend()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.resume()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        try:
            job.start()
            self.fail("Starting a job after is has been run already must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        os.remove(type(self).COMPOUND + "-test.xmsi")

    def _suspend_resume_timeout(self, job):
        logging.debug("calling _suspend_resume_timeout")
        self.assertTrue(job.is_running())

        if not job.is_suspended():
            self.assertTrue(job.suspend())
            logging.debug("message: job suspended")
            return True

        self.assertTrue(job.resume())
        logging.debug("message: job resumed");

        return False

    def test_good_input_file_suspend_resume(self):
        general = self.input.general.copy()
        general.outputfile = type(self).COMPOUND + "-test.xmso"
        self.input.set_general(general)
        self.assertEqual(self.input.validate(), 0)
        self.assertTrue(self.input.write_to_xml_file(type(self).COMPOUND + "-test.xmsi"))

        job = XmiMsim.Job.new(
            os.environ["XMIMSIM_EXEC"],
            type(self).COMPOUND + "-test.xmsi",
            self.options,
            extra_options=type(self).EXTRA_OPTIONS
            )
        self.assertIsNotNone(job)

        # hook up signals
        job.connect('finished-event', self._test_succeed_finished_cb)
        job.connect('special-event', self._test_special_event_cb)
        job.connect('stdout-event', self._print_stdout)
        job.connect('stderr-event', self._print_stderr)

        logging.debug("command: {}".format(job.get_command()))
        job.start()
        self.start = timer()

        GLib.timeout_add_seconds(5, self._suspend_resume_timeout, job)

        self.main_loop.run()

        self.assertFalse(job.is_running())
        self.assertFalse(job.is_suspended())
        self.assertTrue(job.has_finished())
        self.assertTrue(job.was_successful())
        try:
            job.stop()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.suspend()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.resume()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        try:
            job.start()
            self.fail("Starting a job after is has been run already must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        os.remove(type(self).COMPOUND + "-test.xmsi")
        os.remove(type(self).COMPOUND + "-test.xmso")

    def _suspend_stop_timeout(self, job):
        logging.debug("calling _suspend_stop_timeout")
        self.assertTrue(job.is_running())

        if not job.is_suspended():
            self.assertTrue(job.suspend())
            logging.debug("message: job suspended")
            return True

        self.assertTrue(job.stop())
        logging.debug("message: job killed")

        return False

    def test_good_input_file_suspend_stop(self):
        general = self.input.general.copy()
        general.outputfile = type(self).COMPOUND + "-test.xmso"
        self.input.set_general(general)
        self.assertEqual(self.input.validate(), 0)
        self.assertTrue(self.input.write_to_xml_file(type(self).COMPOUND + "-test.xmsi"))

        job = XmiMsim.Job.new(
            os.environ["XMIMSIM_EXEC"],
            type(self).COMPOUND + "-test.xmsi",
            self.options,
            extra_options=type(self).EXTRA_OPTIONS
            )
        self.assertIsNotNone(job)

        # hook up signals
        job.connect('finished-event', self._test_fail_finished_cb)
        job.connect('special-event', self._test_special_event_cb)
        job.connect('stdout-event', self._print_stdout)
        job.connect('stderr-event', self._print_stderr)

        logging.debug("command: {}".format(job.get_command()))
        job.start()
        self.start = timer()

        GLib.timeout_add_seconds(5, self._suspend_stop_timeout, job)

        self.main_loop.run()

        self.assertFalse(job.is_running())
        self.assertFalse(job.is_suspended())
        self.assertTrue(job.has_finished())
        self.assertFalse(job.was_successful())
        try:
            job.stop()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.suspend()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))
        try:
            job.resume()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        try:
            job.start()
            self.fail("Starting a job after is has been run already must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.JobError.quark(), XmiMsim.JobError.UNAVAILABLE))

        os.remove(type(self).COMPOUND + "-test.xmsi")

@unittest.skipUnless(HAVE_XRAYLIB, "Install xraylib's python bindings to run this test")
class TestBatchSingle(unittest.TestCase):

    COMPOUND = "CaSO4"
    EXTRA_OPTIONS = (
    "--with-hdf5-data="+os.environ['HDF5_DATA_DIR'] + "/xmimsimdata-" + COMPOUND + ".h5",
    "--with-solid-angles-data=solid-angles.h5",
    "--with-escape-ratios-data=escape-ratios.h5"
        )

    @classmethod
    def setUpClass(cls):
        if not XmiMsim.Job.is_suspend_available():
            raise 
        XmiMsim.init_hdf5()
        cls.cd = xrl.CompoundParser(cls.COMPOUND)
        if cls.cd is None:
            raise
        cls.data_file = os.environ['HDF5_DATA_DIR'] + "/xmimsimdata-" + cls.COMPOUND + ".h5"
        logging.debug("data_file: {}".format(cls.data_file))
        if XmiMsim.db(cls.data_file, cls.cd['Elements']) is not 1:
            raise

    @classmethod
    def tearDownClass(cls):
        os.remove(cls.data_file)

    def setUp(self):
        self.main_loop = GLib.MainLoop.new(None, False)
        self.input = XmiMsim.Input.init_empty()
        # simulate 10M photons brute force
        general = self.input.general.copy()
        general.n_photons_line = 10000000
        self.input.set_general(general)
        self.options = XmiMsim.MainOptions.new()
        self.options.use_variance_reduction = False # brute force!
        self.options.use_escape_peaks = False # no escape peaks!

        layer = XmiMsim.Layer.new(type(self).cd['Elements'], type(self).cd['massFractions'], 1.0, 1.0)
        composition = XmiMsim.Composition.new([layer], reference_layer=1)
        self.input.set_composition(composition)

        self.active_job_changed_called = 0

    def _print_stdout(self, batch, string):
        logging.debug("stdout: {}".format(string))

    def _print_stderr(self, batch, string):
        logging.debug("stderr: {}".format(string))

    def _test_succeed_finished_cb(self, batch, result, buffer):
        logging.debug("message: {}".format(buffer))
        self.main_loop.quit()

    def _test_active_job_changed(self, batch, active_job):
        self.active_job_changed_called = self.active_job_changed_called + 1

    @staticmethod
    def _CS_Total_Layer(layer, E):
        rv = 0.0
        for i in zip(layer.Z, layer.weight):
            (Z, weight) = i
            rv += weight * xrl.CS_Total(Z, E)
        return rv

    @staticmethod
    def _chi(E0, E1, layer, alpha, beta):
        mu0 = TestBatchSingle._CS_Total_Layer(layer, E0);
        mu1 = TestBatchSingle._CS_Total_Layer(layer, E1);
        return mu0/math.sin(alpha) + mu1/math.sin(beta);

    @staticmethod
    def _calculate_fpm_intensity(input, theta):
        alpha = math.radians(90.0 - theta)
        beta = math.radians(theta)
        discrete = input.excitation.get_energy_discrete(0)
        I0 = discrete.horizontal_intensity + discrete.vertical_intensity
        E0 = discrete.energy
        layer = input.composition.get_layer(0)

        _theta = math.atan(math.sqrt(input.geometry.area_detector / math.pi) / math.fabs(input.geometry.p_detector_window[1]))
        Omega_DET = 2 * math.pi * (1.0 - math.cos(_theta))
        G = Omega_DET / 4.0 / math.pi / math.sin(alpha)

        rv = []
        
        for i in zip(layer.Z, layer.weight):
            (Z, weight) = i
            chi = TestBatchSingle._chi(E0, xrl.LineEnergy(Z, xrl.KL3_LINE), layer, alpha, beta)
            tmp = I0 * G * weight * xrl.CS_FluorLine_Kissel(Z, xrl.KL3_LINE, E0) * \
                (1.0 - math.exp(-1.0 * chi * layer.density * layer.thickness)) / chi
            rv.append(tmp)
        return rv


    def test_data_single1D(self):

        self.single_data = [XmiMsim.BatchSingleData.new("/xmimsim/geometry/n_sample_orientation/theta_deg", 5.0, 85.0, 10)]
        self.xmsi_data = list()

        for i in range(0, 11):
            input_copy = self.input.copy()
            theta = 5.0 + i * 8.0
            geometry = input_copy.geometry
            n_sample_orientation = [0.0, 
                -1 * math.sin(math.radians(theta)),
                math.cos(math.radians(theta))
                ]
            geometry = XmiMsim.Geometry.new(
                    geometry.d_sample_source,
                    n_sample_orientation,
                    geometry.p_detector_window,
                    geometry.n_detector_orientation,
                    geometry.area_detector,
                    geometry.collimator_height,
                    geometry.collimator_diameter,
                    geometry.d_source_slit,
                    geometry.slit_size_x,
                    geometry.slit_size_y
                    )
            input_copy.set_geometry(geometry)

            fpm = TestBatchSingle._calculate_fpm_intensity(input_copy, theta)

            self.xmsi_data.append(input_copy)

        batch = XmiMsim.BatchSingle.new(self.xmsi_data, self.single_data, self.options)
        batch.set_executable(os.environ['XMIMSIM_EXEC'])
        batch.set_extra_options(type(self).EXTRA_OPTIONS)

        self.assertIsNotNone(batch)
        self.assertTrue(batch.is_valid_object())

        # hook up signals
        batch.connect('active-job-changed', self._test_active_job_changed)
        batch.connect('finished-event', self._test_succeed_finished_cb)
        batch.connect('stdout-event', self._print_stdout)
        batch.connect('stderr-event', self._print_stderr)

        batch.start()

        self.main_loop.run()

        self.assertEqual(self.active_job_changed_called, 12)
        self.assertFalse(batch.is_running())
        self.assertFalse(batch.is_suspended())
        self.assertTrue(batch.has_finished())
        self.assertTrue(batch.was_successful())
        try:
            batch.stop()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.BatchError.quark(), XmiMsim.BatchError.UNAVAILABLE))
        try:
            batch.suspend()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.BatchError.quark(), XmiMsim.BatchError.UNAVAILABLE))
        try:
            batch.resume()
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.BatchError.quark(), XmiMsim.BatchError.UNAVAILABLE))

        try:
            batch.start()
            self.fail("Starting a batch after is has been run already must throw an exception!")
        except GLib.Error as err:
            self.assertTrue(err.matches(XmiMsim.BatchError.quark(), XmiMsim.BatchError.UNAVAILABLE))

        xmsa_file = os.environ['HDF5_DATA_DIR'] + '/single-batch1D.xmsa'
        batch.write_archive(xmsa_file)
        archive = XmiMsim.Archive.read_from_xml_file(xmsa_file)
        self.assertEqual(len(archive.single_data), 1)
        self.assertEqual(len(archive.output), 11)
        self.assertEqual(archive.dims, [11])

        os.remove(xmsa_file)




if __name__ == '__main__':
    #unittest.main(verbosity=2, module=TestBatchSingle())
    unittest.main(verbosity=2)
