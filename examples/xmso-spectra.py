from gi.repository import XmiMsimGui, XmiMsim, Gtk, GLib
import sys
import numpy as np


class XMSOSpectraSource(XmiMsimGui.SourceAbstract):
    __gtype_name__ = "XMSOSpectraSource"

    def _xmso_button_file_set(self, button):
        filename = button.get_filename()
        try:
            self.xmso = XmiMsim.Output.read_from_xml_file(filename)
            print("{} was read successfully!".format(filename))
        except GLib.Error as err:
            dialog = Gtk.MessageDialog(self.get_toplevel(),
                Gtk.DialogFlags.MODAL | Gtk.DialogFlags.DESTROY_WITH_PARENT,
                Gtk.MessageType.ERROR, Gtk.ButtonsType.CLOSE, "Could not open XMSO file {}.\nPlease select a different file".format(filename))
            dialog.props.secondary_text = err.message

            dialog.run()
            dialog.destroy()
            self.xmso = None

    # initialize parent first, and add GUI elements
    def __init__(self):
        XmiMsimGui.SourceAbstract.__init__(self)

        self.xmso = None

        grid = Gtk.Grid.new()
        grid.set_hexpand(True)
        grid.set_vexpand(True)
        grid.set_halign(Gtk.Align.FILL)
        grid.set_valign(Gtk.Align.FILL)
        grid.set_border_width(10)
        grid.set_row_spacing(5)
        grid.set_column_spacing(5)
        self.add(grid)

        grid.attach(Gtk.Label.new("XMSO file"), 0, 0, 1, 1)
        self.xmso_button = Gtk.FileChooserButton.new("Select an XMSO file", Gtk.FileChooserAction.OPEN)
        filter = Gtk.FileFilter.new()
        filter.add_pattern("*.xmso")
        filter.add_pattern("*.XMSO")
        filter.set_name("XMI-MSIM outputfiles")
        self.xmso_button.add_filter(filter)
        self.xmso_button.set_hexpand(True)
        self.xmso_button.set_halign(Gtk.Align.FILL)
        self.xmso_button.connect("file-set", self._xmso_button_file_set)
        grid.attach(self.xmso_button, 1, 0, 1, 1)

        grid.attach(Gtk.Separator.new(Gtk.Orientation.HORIZONTAL), 0, 1, 2, 1)

        grid.attach(Gtk.Label.new("Use unconvoluted spectrum"), 0, 2, 1, 1)
        self.unconvoluted_switch = Gtk.Switch.new()
        self.unconvoluted_switch.set_active(True)
        self.unconvoluted_switch.set_halign(Gtk.Align.START)
        grid.attach(self.unconvoluted_switch, 1, 2, 1, 1)

        grid.attach(Gtk.Separator.new(Gtk.Orientation.HORIZONTAL), 0, 3, 2, 1)

        grid.attach(Gtk.Label.new("Degree of linear polarization (%)"), 0, 4, 1, 1)
        self.polarization_spinbutton = Gtk.SpinButton.new_with_range(-100, 100, 1E-2)
        self.polarization_spinbutton.set_value(0.0)
        self.polarization_spinbutton.set_halign(Gtk.Align.START)
        grid.attach(self.polarization_spinbutton, 1, 4, 1, 1)

        # and the scale factor button...
        grid.attach(Gtk.Separator.new(Gtk.Orientation.HORIZONTAL), 0, 5, 2, 1)

        grid.attach(Gtk.Label.new("Scale factor"), 0, 6, 1, 1)
        self.scale_factor_entry = Gtk.Entry.new()
        self.scale_factor_entry.set_text("1")
        self.scale_factor_entry.set_halign(Gtk.Align.START)
        grid.attach(self.scale_factor_entry, 1, 6, 1, 1)


    def do_get_source_name(self):
        return "XMSO Spectra"

    def do_get_about_text(self):
        return """Use this plug-in if you would like to use a spectrum from an XMSO file as excitation source. In most cases you will want to use the unconvoluted spectra. Feel free to specify a degree of linear polarization and a scaling factor"""

    # this method is executed whenever "Update Spectrum" is clicked
    def do_generate(self):
        print("Calling do_generate")

        if self.xmso is None:
            error = GLib.Error.new_literal(XmiMsimGui.SourceAbstractError.quark(), "Please load a valid XMSO file!", XmiMsimGui.SourceAbstractError.INVALID_FILENAME)
            self.emit("after-generate", error)
            return
        try:
            scale_factor = float(self.scale_factor_entry.get_text())
            if scale_factor <= 0.0:
                raise Exception()
        except Exception:
            error = GLib.Error.new_literal(XmiMsimGui.SourceAbstractError.quark(), "Scale factor must be number strictly greater than zero", XmiMsimGui.SourceAbstractError.NO_RAW_DATA)
            self.emit("after-generate", error)
            return

        polarization = self.polarization_spinbutton.get_value() / 100.0

        # get spectrum from xmso
        if self.unconvoluted_switch.get_active():
            spectrum = self.xmso.get_spectrum_unconvoluted(self.xmso.ninteractions)
        else:
            spectrum = self.xmso.get_spectrum_convoluted(self.xmso.ninteractions)

        spectrum = np.array(spectrum) * scale_factor
        energies = np.arange(self.xmso.input.detector.nchannels) * self.xmso.input.detector.gain + self.xmso.input.detector.zero
        spectrum_horizontal = spectrum * (1 + polarization) / 2.0
        spectrum_vertical = spectrum * (1 - polarization) / 2.0

        spectrum[spectrum < 1E-6] = 1E-6

        discrete = []

        for energy, horizontal_intensity, vertical_intensity in zip(energies, spectrum_horizontal, spectrum_vertical):
            if energy < 1.0 or (horizontal_intensity + vertical_intensity) <= 0.0:
                continue

            discrete.append(XmiMsim.EnergyDiscrete.new(energy, horizontal_intensity, vertical_intensity, 0.0, 0.0, 0.0, 0.0, XmiMsim.EnergyDiscreteDistribution.MONOCHROMATIC, 0.0))

        if not discrete:
            error = GLib.Error.new_literal(XmiMsimGui.SourceAbstractError.quark(), "Discrete energy array empty: is the spectrum nothing but zeroes??", XmiMsimGui.SourceAbstractError.NO_RAW_DATA)
            self.emit("after-generate", error)
            return


        excitation = XmiMsim.Excitation.new(discrete=discrete)

        # this updates the source internal data: raw, plot_x, and plot_y
        #self.props.raw_data = excitation
        #self.props.x = energies.tolist()
        #self.props.y = spectrum.tolist()
        self.set_data(excitation, energies.tolist(), spectrum.tolist())


        # afterwards emit after-generate with None argument to update the plot window
        self.emit("after-generate", None)

