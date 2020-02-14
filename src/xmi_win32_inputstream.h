/* GIO - GLib Input, Output and Streaming Library
 *
 * Copyright (C) 2006-2010 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Alexander Larsson <alexl@redhat.com>
 * Author: Tor Lillqvist <tml@iki.fi>
 */

#ifndef __XMI_MSIM_WIN32_INPUT_STREAM_H__
#define __XMI_MSIM_WIN32_INPUT_STREAM_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define XMI_MSIM_TYPE_WIN32_INPUT_STREAM         (xmi_msim_win32_input_stream_get_type ())
#define XMI_MSIM_WIN32_INPUT_STREAM(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), XMI_MSIM_TYPE_WIN32_INPUT_STREAM, XmiMsimWin32InputStream))
#define XMI_MSIM_WIN32_INPUT_STREAM_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), XMI_MSIM_TYPE_WIN32_INPUT_STREAM, XmiMsimWin32InputStreamClass))
#define XMI_MSIM_IS_WIN32_INPUT_STREAM(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), XMI_MSIM_TYPE_WIN32_INPUT_STREAM))
#define XMI_MSIM_IS_WIN32_INPUT_STREAM_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), XMI_MSIM_TYPE_WIN32_INPUT_STREAM))
#define XMI_MSIM_WIN32_INPUT_STREAM_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), XMI_MSIM_TYPE_WIN32_INPUT_STREAM, XmiMsimWin32InputStreamClass))

typedef struct _XmiMsimWin32InputStream         XmiMsimWin32InputStream;
typedef struct _XmiMsimWin32InputStreamClass    XmiMsimWin32InputStreamClass;
typedef struct _XmiMsimWin32InputStreamPrivate  XmiMsimWin32InputStreamPrivate;

G_DEFINE_AUTOPTR_CLEANUP_FUNC(XmiMsimWin32InputStream, g_object_unref)

struct _XmiMsimWin32InputStream
{
  GInputStream parent_instance;

  /*< private >*/
  XmiMsimWin32InputStreamPrivate *priv;
};

struct _XmiMsimWin32InputStreamClass
{
  GInputStreamClass parent_class;

  /*< private >*/
  /* Padding for future expansion */
  void (*_g_reserved1) (void);
  void (*_g_reserved2) (void);
  void (*_g_reserved3) (void);
  void (*_g_reserved4) (void);
  void (*_g_reserved5) (void);
};

GType          xmi_msim_win32_input_stream_get_type         (void) G_GNUC_CONST;

GInputStream * xmi_msim_win32_input_stream_new              (void              *handle,
						      gboolean           close_handle);
void           xmi_msim_win32_input_stream_set_close_handle (XmiMsimWin32InputStream *stream,
						      gboolean           close_handle);
gboolean       xmi_msim_win32_input_stream_get_close_handle (XmiMsimWin32InputStream *stream);
void          *xmi_msim_win32_input_stream_get_handle       (XmiMsimWin32InputStream *stream);

GInputStream * xmi_msim_win32_input_stream_new_from_fd (gint      fd,
				  gboolean  close_fd);
G_END_DECLS

#endif /* __XMI_MSIM_WIN32_INPUT_STREAM_H__ */
