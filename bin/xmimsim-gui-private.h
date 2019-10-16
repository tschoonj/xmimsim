/*
Copyright (C) 2018 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMI_MSIM_GUI_PRIVATE_H
#define XMI_MSIM_GUI_PRIVATE_H

#include <gtk/gtk.h>
#include "xmimsim-gui-undo-manager.h"
#include "xmimsim-gui-clipboard-manager.h"
#include "xmi_job.h"
#include "xmimsim-gui-application-window.h"
#include "xmimsim-gui-source-abstract.h"
#include "xmimsim-gui-sources-dialog.h"

G_BEGIN_DECLS

struct _XmiMsimGuiXmsiConfigScrolledWindow {
	GtkScrolledWindow parent_instance;
	XmiMsimGuiUndoManager *undo_manager;
	XmiMsimGuiClipboardManager *clipboard_manager;

	// widgets
	//general
	GtkWidget *outputfileW;
	GtkWidget *n_photons_intervalW;
	GtkWidget *n_photons_lineW;
	GtkWidget *n_interactions_trajectoryW;
	GtkWidget *commentsW;

	//composition
	GtkWidget *compositionW;

	//geometry
	GtkWidget *d_sample_sourceW;
	GtkWidget *n_sample_orientation_xW;
	GtkWidget *n_sample_orientation_yW;
	GtkWidget *n_sample_orientation_zW;
	GtkWidget *p_detector_window_xW;
	GtkWidget *p_detector_window_yW;
	GtkWidget *p_detector_window_zW;
	GtkWidget *n_detector_orientation_xW;
	GtkWidget *n_detector_orientation_yW;
	GtkWidget *n_detector_orientation_zW;
	GtkWidget *area_detectorW;
	GtkWidget *collimator_heightW;
	GtkWidget *collimator_diameterW;
	GtkWidget *d_source_slitW;
	GtkWidget *slit_size_xW;
	GtkWidget *slit_size_yW;
	GtkWidget *geometry_helpW;
	GtkWidget *cs_window;

	gulong geometry_helpG;
	double geometry_help_scale_factor;
	GdkPixbuf *orig_pixbuf;
	GdkPixbuf *current_pixbuf;
	gint old_width;
	gint old_height;
	double *current_coords;
	GHashTable *table;

	//energy
	GtkWidget *energiesW;

	//absorbers
	GtkWidget *exc_compositionW;
	GtkWidget *det_compositionW;

	//detector
	GtkWidget *detector_typeW;
	GtkWidget *detector_gainW;
	GtkWidget *detector_live_timeW;
	GtkWidget *detector_pulse_widthW;
	GtkWidget *detector_zeroW;
	GtkWidget *detector_fanoW;
	GtkWidget *detector_noiseW;
	GtkWidget *detector_nchannelsW;
	GtkWidget *crystal_compositionW;
};

struct _XmiMsimGuiControlsScrolledWindow {
	GtkScrolledWindow parent_instance;
	XmiMsimGuiUndoManager *manager;
	XmiMsimJob *job;

	// widgets
	GtkWidget *executableW;
	GtkWidget *executableB;
	GtkWidget *options_boxW;
	GtkWidget *spe_convW;
	GtkWidget *spe_convB;
	GtkWidget *csv_convW;
	GtkWidget *csv_convB;
	GtkWidget *svg_convW;
	GtkWidget *svg_convB;
	GtkWidget *html_convW;
	GtkWidget *html_convB;

	GtkWidget *playButton;
	GtkWidget *pauseButton;
	GtkWidget *stopButton;

	GtkWidget *controlsLogW;
	GtkTextBuffer *controlsLogB;

	GtkWidget *progressbar_solidW;
	GtkWidget *progressbar_mainW;
	GtkWidget *progressbar_escapeW;

	GtkWidget *image_solid_stopW;
	GtkWidget *image_main_stopW;
	GtkWidget *image_escape_stopW;
	GtkWidget *image_solid_spinnerW;
	GtkWidget *image_main_spinnerW;
	GtkWidget *image_escape_spinnerW;
	GtkWidget *image_solid_yesW;
	GtkWidget *image_main_yesW;
	GtkWidget *image_escape_yesW;
	GtkWidget *image_solid_noW;
	GtkWidget *image_main_noW;
	GtkWidget *image_escape_noW;

	GtkWidget *nthreadsW;
	GTimer *timer;

	gchar *input_file;
	gchar *output_file;
};

struct _XmiMsimGuiApplicationWindow {
	GtkApplicationWindow parent_instance;

	XmiMsimGuiUndoManager *undo_manager;
	XmiMsimGuiClipboardManager *clipboard_manager;
	GtkWidget *notebook;
	GtkWidget *input_page;
	GtkWidget *controls_page;
	GtkWidget *results_page;
};

void after_generate_cb(XmiMsimGuiSourceAbstract *source, GError *error, XmiMsimGuiSourcesDialog *dialog);

#define DATA_COMMON_WEIGHT_FRACTION common_weight_fraction_data_quark()
GQuark common_weight_fraction_data_quark(void);

struct common_weight_fraction_data {
	double weights_sum;
	unsigned int count;
	double inc;
	GPtrArray *all_xpath_data;
};



G_END_DECLS

#endif
