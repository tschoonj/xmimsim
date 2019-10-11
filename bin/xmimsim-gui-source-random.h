/*
Copyright (C) 2016-2017 Tom Schoonjans and Laszlo Vincze

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
#include <gmodule.h>

#ifndef XMI_MSIM_GUI_SOURCE_RANDOM_H
#define XMI_MSIM_GUI_SOURCE_RANDOM_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_SOURCE_RANDOM (xmi_msim_gui_source_random_get_type ())
#define XMI_MSIM_GUI_SOURCE_RANDOM(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_SOURCE_RANDOM, XmiMsimGuiSourceRandom))
#define XMI_MSIM_GUI_SOURCE_RANDOM_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_SOURCE_RANDOM, XmiMsimGuiSourceRandomClass))
#define XMI_MSIM_GUI_IS_SOURCE_RANDOM(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_SOURCE_RANDOM))
#define XMI_MSIM_GUI_IS_SOURCE_RANDOM_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_SOURCE_RANDOM))
#define XMI_MSIM_GUI_SOURCE_RANDOM_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_SOURCE_RANDOM, XmiMsimGuiSourceRandomClass))

typedef struct _XmiMsimGuiSourceRandom XmiMsimGuiSourceRandom;
typedef struct _XmiMsimGuiSourceRandomClass   XmiMsimGuiSourceRandomClass;

GType xmi_msim_gui_source_random_get_type(void) G_GNUC_CONST;

typedef enum {
	XMI_MSIM_GUI_SOURCE_RANDOM_ERROR_INVALID_DATA,
	XMI_MSIM_GUI_SOURCE_RANDOM_ERROR_GENERATE,
	XMI_MSIM_GUI_SOURCE_RANDOM_ERROR_MAXIMUM,
} XmiMsimGuiSourceRandomError;

#define XMI_MSIM_GUI_SOURCE_RANDOM_ERROR (xmi_msim_gui_source_random_error_quark())

GQuark xmi_msim_gui_source_random_error_quark(void);

G_END_DECLS

#endif

