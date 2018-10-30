import unittest
import sys

try:
    import gi
except ImportError as e:
    sys.exit(77)

gi.require_version('XmiMsim', '1.0')
from gi.repository import XmiMsim, GLib

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

if __name__ == '__main__':
    unittest.main(verbosity=2)
