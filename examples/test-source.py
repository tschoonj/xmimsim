from gi.repository import XmiMsimGui, XmiMsim, Gtk, GLib
import sys
import numpy as np


class TestSource(XmiMsimGui.SourceAbstract):
    # Optional, will not affect GUI in any way if left out...
    __gtype_name__ = "TestSourcePython"

    # initialize parent first, and add GUI elements
    def __init__(self):
        XmiMsimGui.SourceAbstract.__init__(self)
        print("Calling __init__")
        button = Gtk.Button.new_with_label("Click me!")
        self.add(button)
        self.counter = 1000

    # source name: this will end up in the tab label
    def do_get_source_name(self):
        return "Python Source"

    # the about text: will be shown in the Source About Dialog
    def do_get_about_text(self):
        return "This could very well be some clever text about this source"

    # this method is executed whenever "Update Spectrum" is clicked
    def do_generate(self):
        print("Calling do_generate")
        # if something goes wrong: initialize a new GLib.Error instance and send it along with the after-generate signal
        #    error = GLib.Error.new_literal(XmiMsimGui.SourceAbstractError.quark(), "Error message from Python!", XmiMsimGui.SourceAbstractError.INVALID_FILENAME)
        #    self.emit("after-generate", error)
        #    return
        x = np.linspace(1.0, 50.0, num=2000, dtype=np.double)
        y = self.counter * np.exp(-1.0 * (x - 20.0) * (x - 20.0) / 2.0) / np.sqrt(2.0 * np.pi)
        y[y < 1.0] = 1.0
        self.counter += 100

        excitation = XmiMsim.Excitation.new(discrete=[XmiMsim.EnergyDiscrete.new(20.0, 1E9, 1E9, 0.0, 0.0, 0.0, 0.0, XmiMsim.EnergyDiscreteDistribution.MONOCHROMATIC, 0.0)])

        # this updates the source internal data: raw, plot_x, and plot_y
        self.set_data(excitation, x.tolist(), y.tolist())

        # afterwards emit after-generate with None argument to update the plot window
        self.emit("after-generate", None)

