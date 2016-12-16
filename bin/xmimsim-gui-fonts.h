/*
Copyright (C) 2010-2012 Tom Schoonjans and Laszlo Vincze

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

#ifndef XMIMSIM_GUI_FONTS_H
#define XMIMSIM_GUI_FONTS_H

#include "config.h"

#ifdef MAC_INTEGRATION
//Mac OS X native

	#define RESULTS_PLOT_TITLE 30
	#define RESULTS_PLOT_LABELS_LR_FLOAT 25
	#define RESULTS_PLOT_LABELS_LR_EXP 20
	#define RESULTS_PLOT_LABELS_TP 25

	#define SOURCE_PLOT_TITLE 30
	#define SOURCE_PLOT_LABELS_LR_FLOAT 25
	#define SOURCE_PLOT_LABELS_LR_EXP 20
	#define SOURCE_PLOT_LABELS_TP 25

	#define BATCH_2D_PLOT_TITLE 30
	#define BATCH_2D_PLOT_LABELS_LR_FLOAT 25
	#define BATCH_2D_PLOT_LABELS_LR_EXP 20
	#define BATCH_2D_PLOT_LABELS_TB_FLOAT 25
	#define BATCH_2D_PLOT_LABELS_TB_EXP 20

	#define BATCH_3D_PLOT_TITLE 30
	#define BATCH_3D_PLOT_LABELS_LR_FLOAT 20
	#define BATCH_3D_PLOT_LABELS_LR_EXP 20
	#define BATCH_3D_PLOT_LABELS_TB_FLOAT 20
	#define BATCH_3D_PLOT_LABELS_TB_EXP 20

#elif defined(G_OS_WIN32)
//Windows 32/64
	#define RESULTS_PLOT_TITLE 25
	#define RESULTS_PLOT_LABELS_LR_FLOAT 20
	#define RESULTS_PLOT_LABELS_LR_EXP 16
	#define RESULTS_PLOT_LABELS_TP 20

	#define SOURCE_PLOT_TITLE 25
	#define SOURCE_PLOT_LABELS_LR_FLOAT 20
	#define SOURCE_PLOT_LABELS_LR_EXP 14
	#define SOURCE_PLOT_LABELS_TP 20

	#define BATCH_2D_PLOT_TITLE 22
	#define BATCH_2D_PLOT_LABELS_LR_FLOAT 17
	#define BATCH_2D_PLOT_LABELS_LR_EXP 13
	#define BATCH_2D_PLOT_LABELS_TB_FLOAT 17
	#define BATCH_2D_PLOT_LABELS_TB_EXP 13

	#define BATCH_3D_PLOT_TITLE 22
	#define BATCH_3D_PLOT_LABELS_LR_FLOAT 17
	#define BATCH_3D_PLOT_LABELS_LR_EXP 13
	#define BATCH_3D_PLOT_LABELS_TB_FLOAT 17
	#define BATCH_3D_PLOT_LABELS_TB_EXP 13

#else
	//Linux and Mac OS X Unix
	//Perhaps in the future I could include a check for darwin in configure
	//and also use the same values as with MAC_INTEGRATION when doing a non-native build
	#define RESULTS_PLOT_TITLE 20
	#define RESULTS_PLOT_LABELS_LR_FLOAT 20
	#define RESULTS_PLOT_LABELS_LR_EXP 16
	#define RESULTS_PLOT_LABELS_TP 18

	#define SOURCE_PLOT_TITLE 22
	#define SOURCE_PLOT_LABELS_LR_FLOAT 20
	#define SOURCE_PLOT_LABELS_LR_EXP 14
	#define SOURCE_PLOT_LABELS_TP 20

	#define BATCH_2D_PLOT_TITLE 20
	#define BATCH_2D_PLOT_LABELS_LR_FLOAT 17
	#define BATCH_2D_PLOT_LABELS_LR_EXP 13
	#define BATCH_2D_PLOT_LABELS_TB_FLOAT 17
	#define BATCH_2D_PLOT_LABELS_TB_EXP 13

	#define BATCH_3D_PLOT_TITLE 20
	#define BATCH_3D_PLOT_LABELS_LR_FLOAT 17
	#define BATCH_3D_PLOT_LABELS_LR_EXP 13
	#define BATCH_3D_PLOT_LABELS_TB_FLOAT 17
	#define BATCH_3D_PLOT_LABELS_TB_EXP 13

#endif


#endif
