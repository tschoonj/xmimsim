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

#include "config.h"

#include <windows.h>

#include <io.h>

#include <glib.h>
#include "xmi_win32_inputstream.h"

static gboolean
_g_win32_overlap_wait_result (HANDLE           hfile,
                              OVERLAPPED      *overlap,
                              DWORD           *transferred,
                              GCancellable    *cancellable)
{
  GPollFD pollfd[2];
  gboolean result = FALSE;
  gint num, npoll;

#if GLIB_SIZEOF_VOID_P == 8
  pollfd[0].fd = (gint64)overlap->hEvent;
#else
  pollfd[0].fd = (gint)overlap->hEvent;
#endif
  pollfd[0].events = G_IO_IN;
  num = 1;

  if (g_cancellable_make_pollfd (cancellable, &pollfd[1]))
    num++;

loop:
  npoll = g_poll (pollfd, num, -1);
  if (npoll <= 0)
    /* error out, should never happen */
    goto end;

  if (g_cancellable_is_cancelled (cancellable))
    {
      /* CancelIO only cancels pending operations issued by the
       * current thread and since we're doing only sync operations,
       * this is safe.... */
      /* CancelIoEx is only Vista+. Since we have only one overlap
       * operaton on this thread, we can just use: */
      result = CancelIo (hfile);
      g_warn_if_fail (result);
    }

  result = GetOverlappedResult (overlap->hEvent, overlap, transferred, FALSE);
  if (result == FALSE &&
      GetLastError () == ERROR_IO_INCOMPLETE &&
      !g_cancellable_is_cancelled (cancellable))
    goto loop;

end:
  if (num > 1)
    g_cancellable_release_fd (cancellable);

  return result;
}

struct _XmiMsimWin32InputStreamPrivate {
  HANDLE handle;
  gboolean close_handle;
  gint fd;
};

enum {
  PROP_0,
  PROP_HANDLE,
  PROP_CLOSE_HANDLE,
  LAST_PROP
};

static GParamSpec *props[LAST_PROP];

G_DEFINE_TYPE_WITH_PRIVATE (XmiMsimWin32InputStream, xmi_msim_win32_input_stream, G_TYPE_INPUT_STREAM)

static void
xmi_msim_win32_input_stream_set_property (GObject         *object,
				   guint            prop_id,
				   const GValue    *value,
				   GParamSpec      *pspec)
{
  XmiMsimWin32InputStream *win32_stream;

  win32_stream = XMI_MSIM_WIN32_INPUT_STREAM (object);

  switch (prop_id)
    {
    case PROP_HANDLE:
      win32_stream->priv->handle = g_value_get_pointer (value);
      break;
    case PROP_CLOSE_HANDLE:
      win32_stream->priv->close_handle = g_value_get_boolean (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
xmi_msim_win32_input_stream_get_property (GObject    *object,
				   guint       prop_id,
				   GValue     *value,
				   GParamSpec *pspec)
{
  XmiMsimWin32InputStream *win32_stream;

  win32_stream = XMI_MSIM_WIN32_INPUT_STREAM (object);

  switch (prop_id)
    {
    case PROP_HANDLE:
      g_value_set_pointer (value, win32_stream->priv->handle);
      break;
    case PROP_CLOSE_HANDLE:
      g_value_set_boolean (value, win32_stream->priv->close_handle);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static gssize
xmi_msim_win32_input_stream_read (GInputStream  *stream,
			   void          *buffer,
			   gsize          count,
			   GCancellable  *cancellable,
			   GError       **error)
{
  XmiMsimWin32InputStream *win32_stream;
  BOOL res;
  DWORD nbytes, nread;
  OVERLAPPED overlap = { 0, };
  gssize retval = -1;

  win32_stream = XMI_MSIM_WIN32_INPUT_STREAM (stream);

  if (g_cancellable_set_error_if_cancelled (cancellable, error))
    return -1;

  if (count > G_MAXINT)
    nbytes = G_MAXINT;
  else
    nbytes = count;

  overlap.hEvent = CreateEvent (NULL, FALSE, FALSE, NULL);
  g_return_val_if_fail (overlap.hEvent != NULL, -1);

  res = ReadFile (win32_stream->priv->handle, buffer, nbytes, &nread, &overlap);
  if (res)
    retval = nread;
  else
    {
      int errsv = GetLastError ();

      if (errsv == ERROR_IO_PENDING &&
          _g_win32_overlap_wait_result (win32_stream->priv->handle,
                                        &overlap, &nread, cancellable))
        {
          retval = nread;
          goto end;
        }

      if (g_cancellable_set_error_if_cancelled (cancellable, error))
        goto end;

      errsv = GetLastError ();
      if (errsv == ERROR_MORE_DATA)
        {
          /* If a named pipe is being read in message mode and the
           * next message is longer than the nNumberOfBytesToRead
           * parameter specifies, ReadFile returns FALSE and
           * GetLastError returns ERROR_MORE_DATA */
          retval = nread;
          goto end;
        }
      else if (errsv == ERROR_HANDLE_EOF ||
               errsv == ERROR_BROKEN_PIPE)
        {
          /* TODO: the other end of a pipe may call the WriteFile
           * function with nNumberOfBytesToWrite set to zero. In this
           * case, it's not possible for the caller to know if it's
           * broken pipe or a read of 0. Perhaps we should add a
           * is_broken flag for this win32 case.. */
          retval = 0;
        }
      else
        {
          gchar *emsg;

          emsg = g_win32_error_message (errsv);
          g_set_error (error, G_IO_ERROR,
                       g_io_error_from_win32_error (errsv),
                       "Error reading from handle: %s",
                       emsg);
          g_free (emsg);
        }
    }

end:
  CloseHandle (overlap.hEvent);
  return retval;
}

static gboolean
xmi_msim_win32_input_stream_close (GInputStream  *stream,
			   GCancellable  *cancellable,
			   GError       **error)
{
  XmiMsimWin32InputStream *win32_stream;
  BOOL res;

  win32_stream = XMI_MSIM_WIN32_INPUT_STREAM (stream);

  if (!win32_stream->priv->close_handle)
    return TRUE;

  if (win32_stream->priv->fd != -1)
    {
      if (close (win32_stream->priv->fd) < 0)
	{
	  int errsv = errno;

	  g_set_error (error, G_IO_ERROR,
	               g_io_error_from_errno (errsv),
	               "Error closing file descriptor: %s",
	               g_strerror (errsv));
	  return FALSE;
	}
    }
  else
    {
      res = CloseHandle (win32_stream->priv->handle);
      if (!res)
	{
	  int errsv = GetLastError ();
	  gchar *emsg = g_win32_error_message (errsv);

	  g_set_error (error, G_IO_ERROR,
		       g_io_error_from_win32_error (errsv),
		       "Error closing handle: %s",
		       emsg);
	  g_free (emsg);
	  return FALSE;
	}
    }

  return TRUE;
}

static void
xmi_msim_win32_input_stream_class_init (XmiMsimWin32InputStreamClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GInputStreamClass *stream_class = G_INPUT_STREAM_CLASS (klass);

  gobject_class->get_property = xmi_msim_win32_input_stream_get_property;
  gobject_class->set_property = xmi_msim_win32_input_stream_set_property;

  stream_class->read_fn = xmi_msim_win32_input_stream_read;
  stream_class->close_fn = xmi_msim_win32_input_stream_close;

  props[PROP_HANDLE] =
    g_param_spec_pointer ("handle",
                          "File handle",
                          "The file handle to read from",
                          G_PARAM_READABLE |
                          G_PARAM_WRITABLE |
                          G_PARAM_CONSTRUCT_ONLY);

  props[PROP_CLOSE_HANDLE] =
    g_param_spec_boolean ("close-handle",
                          "Close file handle",
                          "Whether to close the file handle when the stream is closed",
                          TRUE,
                          G_PARAM_READABLE |
                          G_PARAM_WRITABLE);

  g_object_class_install_properties (gobject_class, LAST_PROP, props);
}

static void
xmi_msim_win32_input_stream_init (XmiMsimWin32InputStream *win32_stream)
{
  win32_stream->priv = xmi_msim_win32_input_stream_get_instance_private (win32_stream);
  win32_stream->priv->handle = NULL;
  win32_stream->priv->close_handle = TRUE;
  win32_stream->priv->fd = -1;
}

GInputStream *
xmi_msim_win32_input_stream_new (void     *handle,
			  gboolean close_handle)
{
  XmiMsimWin32InputStream *stream;

  g_return_val_if_fail (handle != NULL, NULL);

  stream = g_object_new (XMI_MSIM_TYPE_WIN32_INPUT_STREAM,
			 "handle", handle,
			 "close-handle", close_handle,
			 NULL);

  return G_INPUT_STREAM (stream);
}

void
xmi_msim_win32_input_stream_set_close_handle (XmiMsimWin32InputStream *stream,
				       gboolean          close_handle)
{
  g_return_if_fail (XMI_MSIM_IS_WIN32_INPUT_STREAM (stream));

  close_handle = close_handle != FALSE;
  if (stream->priv->close_handle != close_handle)
    {
      stream->priv->close_handle = close_handle;
      g_object_notify (G_OBJECT (stream), "close-handle");
    }
}

gboolean
xmi_msim_win32_input_stream_get_close_handle (XmiMsimWin32InputStream *stream)
{
  g_return_val_if_fail (XMI_MSIM_IS_WIN32_INPUT_STREAM (stream), FALSE);

  return stream->priv->close_handle;
}

void *
xmi_msim_win32_input_stream_get_handle (XmiMsimWin32InputStream *stream)
{
  g_return_val_if_fail (XMI_MSIM_IS_WIN32_INPUT_STREAM (stream), NULL);

  return stream->priv->handle;
}

GInputStream *
xmi_msim_win32_input_stream_new_from_fd (gint      fd,
				  gboolean  close_fd)
{
  XmiMsimWin32InputStream *win32_stream;

  win32_stream = XMI_MSIM_WIN32_INPUT_STREAM (xmi_msim_win32_input_stream_new ((HANDLE) _get_osfhandle (fd), close_fd));
  win32_stream->priv->fd = fd;

  return (GInputStream*)win32_stream;
}
