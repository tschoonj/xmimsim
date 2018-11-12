from __future__ import print_function
import unittest
import sys
import os
import math

try:
    import gi
except ImportError as e:
    sys.exit(77)

# now for some python 2/3 stuff... I am trying to avoid a dependency on six here...
try:
    from urllib import urlretrieve
except Exception as e:
    from urllib.request import urlretrieve

try:
    import lxml.etree as etree
    HAVE_LXML = True
except ImportError as e:
    HAVE_LXML = False


gi.require_version('XmiMsim', '1.0')
from gi.repository import XmiMsim, GLib

TEST_XMSI_URL = "https://github.com/tschoonj/xmimsim/wiki/test.xmsi"
TEST_XMSI = "test.xmsi"
TEST_XMSI_COPY = "test-copy.xmsi"

SQRT_2 = math.sqrt(2.0)
SQRT_2_2 = math.sqrt(2.0)/2.0

# Load the XML catalog
XmiMsim.xmlLoadCatalog()

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
        urlretrieve(TEST_XMSI_URL, TEST_XMSI)

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

    def test_copying(self):
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
            pass
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
            pass
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
        self.assertEqual(input.validate(), 0)
        self.assertTrue(input.equals(self.input))

if __name__ == '__main__':
    unittest.main(verbosity=2)
