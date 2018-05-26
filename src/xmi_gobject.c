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

#include <config.h>
#include "xmi_gobject.h"
#include "xmi_data_structs.h"

// taken more or less from ebassi's graphene-gobject.c
#define XMI_MSIM_DEFINE_BOXED_TYPE(TypeName, type_name) \
  static gpointer xmi_msim_ ## type_name ## _copy(gpointer boxed) { \
	struct xmi_ ## type_name *A = boxed; \
	struct xmi_ ## type_name *B = NULL; \
	xmi_copy_ ## type_name (A, &B); \
	return B; \
  } \
  \
  static void xmi_msim_ ## type_name ## _free(gpointer boxed) { \
	xmi_free_ ## type_name((struct xmi_ ## type_name *) boxed); \
  } \
  GType \
    xmi_msim_ ## type_name ## _get_type (void) \
  { \
    static volatile gsize xmi_msim_define_id__volatile = 0; \
    if (g_once_init_enter (&xmi_msim_define_id__volatile)) \
      { \
        GType xmi_msim_define_id = \
          g_boxed_type_register_static (g_intern_static_string (#TypeName), \
                                        (GBoxedCopyFunc) xmi_msim_ ## type_name ## _copy, \
                                        (GBoxedFreeFunc) xmi_msim_ ## type_name ## _free); \
        g_once_init_leave (&xmi_msim_define_id__volatile, xmi_msim_define_id); \
      } \
    return xmi_msim_define_id__volatile; \
  }

XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimComposition, composition);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimExcitation, excitation);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimInput, input);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimArchive, archive);
