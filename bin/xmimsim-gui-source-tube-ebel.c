/*
Copyright (C) 2016 Tom Schoonjans and Laszlo Vincze

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "xmimsim-gui-source-tube-ebel.h"

struct xmi_ebel_parameters {
	double tube_voltage;
	double tube_current;
	double tube_solid_angle;
	double alpha;
	double beta;
	double interval_width;
	int anode_Z;
	double anode_rho;
	double anode_thickness;
	int window_Z;
	double window_rho;
	double window_thickness;
	int filter_Z;
	double filter_rho;
	double filter_thickness;
	gboolean transmission_tube;
	gchar *transmission_efficiency_file;
	gboolean log10_active;
};

G_DEFINE_TYPE(XmiMsimGuiSourceTubeEbel, xmi_msim_gui_source_tube_ebel, XMI_MSIM_GUI_TYPE_SOURCE_ABSTRACT)

static gboolean xmi_msim_gui_source_tube_ebel_real_generate(XmiMsimGuiSourceAbstract *source, GError **error);

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_source_tube_ebel_dispose(GObject *object);

static void xmi_msim_gui_source_tube_ebel_finalize(GObject *object);

static void xmi_msim_gui_source_tube_ebel_class_init(XmiMsimGuiSourceTubeEbelClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_tube_ebel_dispose;
	object_class->finalize = xmi_msim_gui_source_tube_ebel_finalize;

	XmiMsimGuiSourceAbstractClass *parent_klass = XMI_MSIM_GUI_SOURCE_ABSTRACT_CLASS(klass);

	parent_klass->generate = xmi_msim_gui_source_tube_ebel_real_generate;
	//parent_klass->save = xmi_msim_gui_source_tube_ebel_real_save; // do not override
	parent_klass->get_name = xmi_msim_gui_source_tube_ebel_real_get_name;
	parent_klass->get_about_text = xmi_msim_gui_source_tube_ebel_real_get_about_text;
}

static void xmi_msim_gui_source_tube_ebel_init(XmiMsimGuiSourceTubeEbel *source) {

	GtkWidget *mainVBox = GTK_WIDGET(source);

}

G_MODULE_EXPORT XmiMsimGuiSourceTubeEbel* xmi_msim_gui_source_tube_ebel_new(struct xmi_input *current) {
	XmiMsimGuiSourceTubeEbel *rv = (XmiMsimGuiSourceTubeEbel *) g_object_new(XMI_MSIM_GUI_TYPE_SOURCE_TUBE_EBEL, NULL);
	
	XMI_MSIM_GUI_SOURCE_ABSTRACT(rv)->current = current;

	// load preferences
	// all relevant code to be copied from prefs.c!!!

	return rv;
}

static gboolean xmi_msim_gui_source_tube_ebel_real_generate(XmiMsimGuiSourceAbstract *source, GError **error) {

}

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_name(XmiMsimGuiSourceAbstract *source) {
	static const gchar name[] = "X-ray tube (Ebel model)";

	return name;
}

static const gchar *xmi_msim_gui_source_tube_ebel_real_get_about_text(XmiMsimGuiSourceAbstract *source) {
	static const gchar about_text[] =
		"The model is based on the equations that can be found in "
		"the papers written by Horst Ebel and published in <a href='http://dx.doi.org/10.1002/(SICI)1097-4539(199907/08)28:4<255::AID-XRS347>3.0.CO;2-Y'>X-ray Spectrometry "
		"28 (1999), 255-266</a> and <a href='http://dx.doi.org/10.1002/xrs.610'>X-ray Spectrometry 32 (2003), 46-51</a>.";
	return about_text;
}

static void xmi_msim_gui_source_tube_ebel_dispose(GObject *object) {

	G_OBJECT_CLASS(xmi_msim_gui_source_tube_ebel_parent_class)->dispose(object);
}

static void xmi_msim_gui_source_tube_ebel_finalize(GObject *object) {

	G_OBJECT_CLASS(xmi_msim_gui_source_tube_ebel_parent_class)->finalize(object);
}
