from gi.repository import XmiMsimGui, XmiMsim, Gtk, GLib, Gio
import sys
import numpy as np


class PyMcaEbelSource(XmiMsimGui.SourceTubeEbel):
    __gtype_name__ = "PyMcaEbelSource"

    # source name: this will end up in the tab label
    def do_get_source_name(self):
        return "PyMca Ebel"

    # the about text: will be shown in the Source About Dialog
    def do_get_about_text(self):
        return """\
        This source is identical to the regular Ebel source, but it exports spectra in a file that is compatible with the import function of PyMca
        """

    def do_save(self, filename):
        raw_data = self.props.raw_data
        if raw_data is None:
            raise GLib.Error.new_literal(XmiMsimGui.SourceAbstractError.quark(), "No data available for saving. Ensure the model data is valid and click \"Generate spectrum\"", XmiMsimGui.SourceAbstractError.NO_RAW_DATA)

        file = Gio.File.new_for_path(filename)
        type = file.query_file_type(Gio.FileQueryInfoFlags.NONE)
        if type == Gio.FileType.REGULAR:
            # file exists -> delete it!
            file.delete()
        elif type != Gio.FileType.UNKNOWN:
            raise GLib.Error.new_literal(XmiMsimGui.SourceAbstractError.quark(), "Could not save to {}. It appears to exist already but it is not a regular file!".format(filename), XmiMsimGui.SourceAbstractError.INVALID_FILENAME)

        interval_width = self.props.interval_width
        if interval_width == 0.0:
            raise GLib.Error.new_literal(XmiMsimGui.SourceTubeEbelError.quark(), "Invalid interval width. Must be greater than zero!", XmiMsimGui.SourceTubeEbelError.INVALID_DATA)

        _stream = file.create(Gio.FileCreateFlags.PRIVATE)
        stream = Gio.DataOutputStream.new(_stream)

        # write header
        stream.put_string("\"energy\";\"weight\";\"flag\";\"scatter\"\n")

        # calculate sum
        sum = 0.0
        for i in range(raw_data.n_discrete):
            disc = raw_data.get_energy_discrete(i)
            sum += disc.horizontal_intensity + disc.vertical_intensity
        
        for i in range(raw_data.n_continuous):
            cont = raw_data.get_energy_continuous(i)
            sum += (cont.horizontal_intensity + cont.vertical_intensity) * interval_width

        for i in range(raw_data.n_discrete):
            disc = raw_data.get_energy_discrete(i)
            weight = (disc.horizontal_intensity + disc.vertical_intensity) / sum
            energy = disc.energy
            line = "{};{};1;1\n".format(energy, weight)
            stream.put_string(line)
        
        for i in range(raw_data.n_continuous):
            cont = raw_data.get_energy_continuous(i)
            weight = (cont.horizontal_intensity + cont.vertical_intensity) * interval_width / sum
            energy = cont.energy
            line = "{};{};1;0\n".format(energy, weight)
            stream.put_string(line)

        stream.close()

        return True
        
        






