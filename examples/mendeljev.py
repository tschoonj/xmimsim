import xraylib as xrl
import sys
import os
import math
import logging
logging.basicConfig(format='%(asctime)s %(message)s', level=logging.DEBUG)

import gi
gi.require_version('XmiMsim', '1.0')
gi.require_version('XmiMsimGui', '1.0')
from gi.repository import XmiMsim, XmiMsimGui, GLib, Gio, Gtk

OUTPUTFILE = 'mendeljev.xmsa'

XmiMsim.xmlLoadCatalog()

main_loop = GLib.MainLoop.new(None, False)
input = XmiMsim.Input.init_empty()
general = input.general.copy()
general.n_photons_line = 100000 # 1M photons
input.set_general(general)
options = XmiMsim.MainOptions.new()

single_data = [XmiMsim.BatchSingleData.new("/xmimsim/composition/layer[1]/element[1]/atomic_number", 1, 92, 91)]
xmsi_data = list()

for Z in range(1, 93):
    input_copy = input.copy()
    layer = XmiMsim.Layer.new([Z], [1.0], 1.0, 1.0)
    composition = XmiMsim.Composition.new([layer], reference_layer=1)
    input_copy.set_composition(composition)
    xmsi_data.append(input_copy)

batch = XmiMsim.BatchSingle.new(xmsi_data, single_data, options)

assert batch.is_valid_object() == True

def _test_succeed_finished_cb(batch, result, buffer):
    logging.debug("message: {}".format(buffer))
    assert result == True
    main_loop.quit()

def _print_stdout(batch, string):
    logging.debug("stdout: {}".format(string))

def _print_stderr(batch, string):
    logging.debug("stderr: {}".format(string))

batch.connect('finished-event', _test_succeed_finished_cb)
batch.connect('stdout-event', _print_stdout)
batch.connect('stderr-event', _print_stderr)

batch.start()

main_loop.run()

assert batch.was_successful() == True

batch.write_archive(OUTPUTFILE)

XmiMsimGui.init()

archive = batch.props.archive

win = XmiMsimGui.XmsaViewerWindow.new(archive)
win.set_position(Gtk.WindowPosition.CENTER)
win.connect("destroy", Gtk.main_quit)
win.show_all()
Gtk.main()
