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

#include "xmimsim-gui-source-abstract.h"

G_DEFINE_ABSTRACT_TYPE(XmiMsimGuiSourceAbstract, xmi_msim_gui_source_abstract, GTK_TYPE_VBOX)

static gboolean xmi_msim_gui_source_abstract_real_generate(XmiMsimGuiSourceAbstract *source, GError **error);

static void xmi_msim_gui_source_abstract_real_save(XmiMsimGuiSourceAbstract *source, gchar *filename);

static const gchar *xmi_msim_gui_source_abstract_real_get_name(XmiMsimGuiSourceAbstract *source);

static const gchar *xmi_msim_gui_source_abstract_real_get_about_text(XmiMsimGuiSourceAbstract *source);

static void xmi_msim_gui_source_abstract_dispose(GObject *object);

static void xmi_msim_gui_source_abstract_finalize(GObject *object);

static void xmi_msim_gui_source_abstract_class_init(XmiMsimGuiSourceAbstractClass *klass) {

	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->dispose = xmi_msim_gui_source_abstract_dispose;
	object_class->finalize = xmi_msim_gui_source_abstract_finalize;

	klass->generate = xmi_msim_gui_source_abstract_real_generate;
	klass->save = xmi_msim_gui_source_abstract_real_save;
	klass->get_name = xmi_msim_gui_source_abstract_real_get_name;
	klass->get_about_text = xmi_msim_gui_source_abstract_real_get_about_text;

}

static void xmi_msim_gui_source_abstract_init(XmiMsimGuiSourceAbstract *source) {

	source->plot_data_linear = NULL;	
	source->plot_data_log10 = NULL;	

}

#ifdef HAVE_CXX
Gtk::PLplot::PlotData2D *xmi_msim_gui_source_abstract_get_plot_data(XmiMsimGuiSourceAbstract *source, gboolean log10)
#else
GtkPlotData *xmi_msim_gui_source_abstract_get_plot_data(XmiMsimGuiSourceAbstract *source, gboolean log10)
#endif
	{
	if (log10)
		return source->plot_data_log10;
	else
		return source->plot_data_linear;

}

static void xmi_msim_gui_source_abstract_dispose(GObject *object) {
	// deriving methods should store the data in the preferences file now
}

static void xmi_msim_gui_source_abstract_finalize(GObject *object) {
	XmiMsimGuiSourceAbstract *source = XMI_MSIM_GUI_SOURCE_ABSTRACT(object);
#ifdef HAVE_CXX
	if (source->plot_data_linear)
		delete source->plot_data_linear;
	if (source->plot_data_log10)
		delete source->plot_data_log10;
#else
	if (source->plot_data_linear)
		g_object_unref(source->plot_data_linear);
	if (source->plot_data_log10)
		g_object_unref(source->plot_data_log10);

#endif
}

static gboolean xmi_msim_gui_source_abstract_real_generate(XmiMsimGuiSourceAbstract *source, GError **error) {
	g_warning("XmiMsimGuiSourceAbstract::generate not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	g_set_error(error, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR, XMI_MSIM_GUI_SOURCE_ABSTRACT_ERROR_METHOD_UNDEFINED, "XmiMsimGuiSourceAbstract::generate not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return FALSE;
}

static void xmi_msim_gui_source_abstract_real_save(XmiMsimGuiSourceAbstract *source, gchar *filename) {
	g_warning("XmiMsimGuiSourceAbstract::save not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));

}

static const gchar *xmi_msim_gui_source_abstract_real_get_name(XmiMsimGuiSourceAbstract *source) {
	g_warning("XmiMsimGuiSourceAbstract::get_name not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return NULL;
}

static const gchar *xmi_msim_gui_source_abstract_real_get_about_text(XmiMsimGuiSourceAbstract *source) {
	g_warning("XmiMsimGuiSourceAbstract::get_about_text not implemented for '%s'", g_type_name(G_TYPE_FROM_INSTANCE(source)));
	return NULL;
}

GQuark xmi_msim_gui_source_abstract_error_quark(void) {
	return g_quark_from_static_string("xmi-msim-gui-source-abstract-error-quark");
}

gboolean xmi_msim_gui_source_abstract_generate(XmiMsimGuiSourceAbstract *source, GError **error) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->generate(source, error);
}

void xmi_msim_gui_source_abstract_save(XmiMsimGuiSourceAbstract *source, gchar *filename) {
	XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->save(source, filename);
}

const gchar *xmi_msim_gui_source_abstract_get_name(XmiMsimGuiSourceAbstract *source) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->get_name(source);
}

const gchar *xmi_msim_gui_source_abstract_get_about_text(XmiMsimGuiSourceAbstract *source) {
	return XMI_MSIM_GUI_SOURCE_ABSTRACT_GET_CLASS(source)->get_about_text(source);
}



