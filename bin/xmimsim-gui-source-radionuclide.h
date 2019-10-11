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

#ifndef XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_H
#define XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_H

G_BEGIN_DECLS

#define XMI_MSIM_GUI_TYPE_SOURCE_RADIONUCLIDE (xmi_msim_gui_source_radionuclide_get_type ())
#define XMI_MSIM_GUI_SOURCE_RADIONUCLIDE(obj)                  (G_TYPE_CHECK_INSTANCE_CAST ((obj), XMI_MSIM_GUI_TYPE_SOURCE_RADIONUCLIDE, XmiMsimGuiSourceRadionuclide))
#define XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), XMI_MSIM_GUI_TYPE_SOURCE_RADIONUCLIDE, XmiMsimGuiSourceRadionuclideClass))
#define XMI_MSIM_GUI_IS_SOURCE_RADIONUCLIDE(obj)               (G_TYPE_CHECK_INSTANCE_TYPE ((obj), XMI_MSIM_GUI_TYPE_SOURCE_RADIONUCLIDE))
#define XMI_MSIM_GUI_IS_SOURCE_RADIONUCLIDE_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE ((klass), XMI_MSIM_GUI_TYPE_SOURCE_RADIONUCLIDE))
#define XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), XMI_MSIM_GUI_TYPE_SOURCE_RADIONUCLIDE, XmiMsimGuiSourceRadionuclideClass))

typedef struct _XmiMsimGuiSourceRadionuclide XmiMsimGuiSourceRadionuclide;
typedef struct _XmiMsimGuiSourceRadionuclideClass   XmiMsimGuiSourceRadionuclideClass;

GType xmi_msim_gui_source_radionuclide_get_type(void) G_GNUC_CONST;

typedef enum {
	XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR_INVALID_DATA,
	XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR_GENERATE,
	XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR_MAXIMUM,
} XmiMsimGuiSourceRadionuclideError;

#define XMI_MSIM_GUI_SOURCE_RADIONUCLIDE_ERROR (xmi_msim_gui_source_radionuclide_error_quark())

GQuark xmi_msim_gui_source_radionuclide_error_quark(void);

G_END_DECLS

#endif
