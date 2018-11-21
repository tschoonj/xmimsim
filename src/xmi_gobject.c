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
#include "xmi_private.h"
#include "xmi_data_structs.h"

// taken more or less from ebassi's graphene-gobject.c
#define XMI_MSIM_DEFINE_BOXED_TYPE(TypeName, type_name) \
  static gpointer xmi_msim_ ## type_name ## _copy(gpointer boxed) { \
	xmi_ ## type_name *A = boxed; \
	xmi_ ## type_name *B = NULL; \
	xmi_ ## type_name ## _copy(A, &B); \
	return B; \
  } \
  \
  static void xmi_msim_ ## type_name ## _free(gpointer boxed) { \
	xmi_ ## type_name ## _free((xmi_ ## type_name *) boxed); \
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
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimMainOptions, main_options);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimOutput, output);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimGeneral, general);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimGeometry, geometry);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimAbsorbers, absorbers);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimDetector, detector);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimEnergyDiscrete, energy_discrete);
XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimEnergyContinuous, energy_continuous);

// xmi_layer_free is special so it needs special treatment...
static gpointer xmi_msim_layer_copy(gpointer boxed) {
	xmi_layer *A = boxed;
	xmi_layer *B = NULL;
	xmi_layer_copy(A, &B);
	return B;
}

static void xmi_msim_layer_free(gpointer boxed) {
	xmi_layer_free((xmi_layer *) boxed);
	g_free(boxed); // necessary since xmi_layer_free does not free the struct itself, only the Z and weight arrays within...
}

GType xmi_msim_layer_get_type(void) {
	static volatile gsize xmi_msim_define_id__volatile = 0;
	if (g_once_init_enter (&xmi_msim_define_id__volatile)) {
		GType xmi_msim_define_id = g_boxed_type_register_static(g_intern_static_string("XmiMsimLayer"), (GBoxedCopyFunc) xmi_msim_layer_copy, (GBoxedFreeFunc) xmi_msim_layer_free);
		g_once_init_leave (&xmi_msim_define_id__volatile, xmi_msim_define_id);
	}
	return xmi_msim_define_id__volatile;
}

static void xmi_history_element_line_copy(xmi_history_element_line *A, xmi_history_element_line **B) {
	g_return_if_fail(A != NULL && B != NULL);
	g_atomic_int_inc(&A->ref_count);
	*B = A;
}

XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimHistoryElementLine, history_element_line);

static void xmi_history_element_copy(xmi_history_element *A, xmi_history_element **B) {
	g_return_if_fail(A != NULL && B != NULL);
	g_atomic_int_inc(&A->ref_count);
	/*gchar *line_type;
	xmi_history_element_line *line;
	GHashTableIter iter;
	g_hash_table_iter_init(&iter, A->lines);
	while (g_hash_table_iter_next(&iter, (gpointer *) &line_type, (gpointer *) &line)) {
		xmi_history_element_line *line_copy;
		xmi_history_element_line_copy(line, &line_copy);
		g_hash_table_insert(rv->lines, g_strdup(line_type), line_copy);
	}*/
	*B = A;
}

XMI_MSIM_DEFINE_BOXED_TYPE(XmiMsimHistoryElement, history_element);

