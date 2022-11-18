{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

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
}

unit xcb;

interface

const
  External_library = 'xcb'; 

const
  X_PROTOCOL = 11;
  {* Current minor version  }
  X_PROTOCOL_REVISION = 0;
  {* X_TCP_PORT + display number = server port for TCP transport  }
  X_TCP_PORT = 6000;
  {* xcb connection errors because of socket, pipe and other stream errors.  }
  XCB_CONN_ERROR = 1;
  {* xcb connection shutdown because of extension not supported  }
  XCB_CONN_CLOSED_EXT_NOTSUPPORTED = 2;
  {* malloc(), calloc() and realloc() error upon failure, for eg ENOMEM  }
  XCB_CONN_CLOSED_MEM_INSUFFICIENT = 3;
  {* Connection closed, exceeding request length that server accepts.  }
  XCB_CONN_CLOSED_REQ_LEN_EXCEED = 4;
  {* Connection closed, error during parsing display string.  }
  XCB_CONN_CLOSED_PARSE_ERR = 5;
  {* Connection closed because the server does not have a screen matching the display.  }
  XCB_CONN_CLOSED_INVALID_SCREEN = 6;
  {* Connection closed because some FD passing operation failed  }
  XCB_CONN_CLOSED_FDPASSING_FAILED = 7;

  XCB_BUTTON_MASK_1 = 256;
  XCB_BUTTON_MASK_2 = 512;
  XCB_BUTTON_MASK_3 = 1024;
  XCB_BUTTON_MASK_4 = 2048;
  XCB_BUTTON_MASK_5 = 4096;
  XCB_BUTTON_MASK_ANY = 32768;

  XCB_KEY_PRESS = 2;
  XCB_KEY_RELEASE = 3;
  XCB_BUTTON_PRESS = 4;
  XCB_BUTTON_RELEASE = 5;
  XCB_MOTION_NOTIFY = 6;
  XCB_ENTER_NOTIFY = 7;
  XCB_LEAVE_NOTIFY = 8;
  XCB_FOCUS_IN = 9;
  XCB_FOCUS_OUT = 10;

  XCB_NONE = 0;
  {* XCB_COPY_FROM_PARENT can be used for many xcb_create_window parameters  }
  XCB_COPY_FROM_PARENT = 0;
  {* XCB_CURRENT_TIME can be used in most requests that take an xcb_timestamp_t  }
  XCB_CURRENT_TIME = 0;
  {* XCB_NO_SYMBOL fills in unused entries in xcb_keysym_t tables  }
  XCB_NO_SYMBOL = 0;

type
  PLongint = ^longint;
  PSmallInt = ^smallint;
  pbyte = ^byte;
  PWord = ^word;
  PDWord = ^DWord;
  PDouble = ^double;
  integer_t = integer;

  uint8_t = byte;

  int16_t = smallint;
  uint16_t = word;

  uint32_t = longword;
  int32_t = integer;

  uint64_t = QWord;

  xcb_timestamp_t = uint32_t;
  xcb_window_t = uint32_t;
  xcb_button_t = uint8_t;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  Pxcb_connection_t = ^xcb_connection_t;
  xcb_connection_t = record
  end;

  Pxcb_keycode_t = ^xcb_keycode_t;
  xcb_keycode_t = uint8_t;

  Pxcb_query_extension_reply_t = ^xcb_query_extension_reply_t;

  xcb_query_extension_reply_t = record
    response_type: uint8_t;
    pad0: uint8_t;
    sequence: uint16_t;
    length: uint32_t;
    present: uint8_t;
    major_opcode: uint8_t;
    first_event: uint8_t;
    first_error: uint8_t;
  end;

  Pxcb_setup_t = ^xcb_setup_t;

  xcb_setup_t = record
    status: uint8_t;
    pad0: uint8_t;
    protocol_major_version: uint16_t;
    protocol_minor_version: uint16_t;
    length: uint16_t;
    release_number: uint32_t;
    resource_id_base: uint32_t;
    resource_id_mask: uint32_t;
    motion_buffer_size: uint32_t;
    vendor_len: uint16_t;
    maximum_request_length: uint16_t;
    roots_len: uint8_t;
    pixmap_formats_len: uint8_t;
    image_byte_order: uint8_t;
    bitmap_format_bit_order: uint8_t;
    bitmap_format_scanline_unit: uint8_t;
    bitmap_format_scanline_pad: uint8_t;
    min_keycode: xcb_keycode_t;
    max_keycode: xcb_keycode_t;
    pad1: array[0..3] of uint8_t;
  end;

  Pxcb_key_press_event_t = ^xcb_key_press_event_t;

  xcb_key_press_event_t = record
    response_type: uint8_t;
    detail: xcb_keycode_t;
    sequence: uint16_t;
    time: xcb_timestamp_t;
    root: xcb_window_t;
    event: xcb_window_t;
    child: xcb_window_t;
    root_x: int16_t;
    root_y: int16_t;
    event_x: int16_t;
    event_y: int16_t;
    state: uint16_t;
    same_screen: uint8_t;
    pad0: uint8_t;
  end;

  Pxcb_key_release_event_t = ^xcb_key_release_event_t;
  xcb_key_release_event_t = xcb_key_press_event_t;

  Pxcb_button_mask_t = ^xcb_button_mask_t;
  xcb_button_mask_t = longint;

  Pxcb_button_press_event_t = ^xcb_button_press_event_t;

  xcb_button_press_event_t = record
    response_type: uint8_t;
    detail: xcb_button_t;
    sequence: uint16_t;
    time: xcb_timestamp_t;
    root: xcb_window_t;
    event: xcb_window_t;
    child: xcb_window_t;
    root_x: int16_t;
    root_y: int16_t;
    event_x: int16_t;
    event_y: int16_t;
    state: uint16_t;
    same_screen: uint8_t;
    pad0: uint8_t;
  end;

  Pxcb_button_release_event_t = ^xcb_button_release_event_t;
  xcb_button_release_event_t = xcb_button_press_event_t;

  Pxcb_generic_iterator_t = ^xcb_generic_iterator_t;

  xcb_generic_iterator_t = record
    Data: pointer;
    rem: longint;
    index: longint;
  end;

  Pxcb_generic_reply_t = ^xcb_generic_reply_t;

  xcb_generic_reply_t = record
    response_type: uint8_t;
    pad0: uint8_t;
    sequence: uint16_t;
    length: uint32_t;
  end;

  Pxcb_generic_event_t = ^xcb_generic_event_t;

  xcb_generic_event_t = record
    response_type: uint8_t;
    pad0: uint8_t;
    sequence: uint16_t;
    pad: array[0..6] of uint32_t;
    full_sequence: uint32_t;
  end;

  Pxcb_raw_generic_event_t = ^xcb_raw_generic_event_t;

  xcb_raw_generic_event_t = record
    response_type: uint8_t;
    pad0: uint8_t;
    sequence: uint16_t;
    pad: array[0..6] of uint32_t;
  end;

  {*
   * @brief GE event
   *
   * An event as sent by the XGE extension. The length field specifies the
   * number of 4-byte blocks trailing the struct.
   *
   * @deprecated Since some fields in this struct have unfortunate names, it is
   * recommended to use xcb_ge_generic_event_t instead.
    }

  Pxcb_ge_event_t = ^xcb_ge_event_t;

  xcb_ge_event_t = record
    response_type: uint8_t;
    pad0: uint8_t;
    sequence: uint16_t;
    length: uint32_t;
    event_type: uint16_t;
    pad1: uint16_t;
    pad: array[0..4] of uint32_t;
    full_sequence: uint32_t;
  end;

  Pxcb_generic_error_t = ^xcb_generic_error_t;

  xcb_generic_error_t = record
    response_type: uint8_t;
    error_code: uint8_t;
    sequence: uint16_t;
    resource_id: uint32_t;
    minor_code: uint16_t;
    major_code: uint8_t;
    pad0: uint8_t;
    pad: array[0..4] of uint32_t;
    full_sequence: uint32_t;
  end;

  Pxcb_void_cookie_t = ^xcb_void_cookie_t;

  xcb_void_cookie_t = record
    sequence: dword;
  end;

  Pxcb_auth_info_t = ^xcb_auth_info_t;

  xcb_auth_info_t = record
    namelen: longint;
    Name: PChar;
    datalen: longint;
    Data: PChar;
  end;

{ xcb_out.c  }
  {*
   * @brief Forces any buffered output to be written to the server.
   * @param c The connection to the X server.
   * @return > @c 0 on success, <= @c 0 otherwise.
   *
   * Forces any buffered output to be written to the server. Blocks
   * until the write is complete.
    }

function xcb_flush(var c: xcb_connection_t): longint;
  cdecl; external External_library Name 'xcb_flush';

  {*
   * @brief Returns the maximum request length that this server accepts.
   * @param c The connection to the X server.
   * @return The maximum request length field.
   *
   * In the absence of the BIG-REQUESTS extension, returns the
   * maximum request length field from the connection setup data, which
   * may be as much as 65535. If the server supports BIG-REQUESTS, then
   * the maximum request length field from the reply to the
   * BigRequestsEnable request will be returned instead.
   *
   * Note that this length is measured in four-byte units, making the
   * theoretical maximum lengths roughly 256kB without BIG-REQUESTS and
   * 16GB with.
    }
function xcb_get_maximum_request_length(var c: xcb_connection_t): uint32_t;
  cdecl; external External_library Name 'xcb_get_maximum_request_length';

  {*
   * @brief Prefetch the maximum request length without blocking.
   * @param c The connection to the X server.
   *
   * Without blocking, does as much work as possible toward computing
   * the maximum request length accepted by the X server.
   *
   * Invoking this function may cause a call to xcb_big_requests_enable,
   * but will not block waiting for the reply.
   * xcb_get_maximum_request_length will return the prefetched data
   * after possibly blocking while the reply is retrieved.
   *
   * Note that in order for this function to be fully non-blocking, the
   * application must previously have called
   * xcb_prefetch_extension_data(c, &xcb_big_requests_id) and the reply
   * must have already arrived.
    }
procedure xcb_prefetch_maximum_request_length(var c: xcb_connection_t);
  cdecl; external External_library Name 'xcb_prefetch_maximum_request_length';

{ xcb_in.c  }
  {*
   * @brief Returns the next event or error from the server.
   * @param c The connection to the X server.
   * @return The next event from the server.
   *
   * Returns the next event or error from the server, or returns null in
   * the event of an I/O error. Blocks until either an event or error
   * arrive, or an I/O error occurs.
    }
function xcb_wait_for_event(var c: xcb_connection_t): Pxcb_generic_event_t;
  cdecl; external External_library Name 'xcb_wait_for_event';

  {*
   * @brief Returns the next event or error from the server.
   * @param c The connection to the X server.
   * @return The next event from the server.
   *
   * Returns the next event or error from the server, if one is
   * available, or returns @c NULL otherwise. If no event is available, that
   * might be because an I/O error like connection close occurred while
   * attempting to read the next event, in which case the connection is
   * shut down when this function returns.
    }
function xcb_poll_for_event(var c: xcb_connection_t): Pxcb_generic_event_t;
  cdecl; external External_library Name 'xcb_poll_for_event';

  {*
   * @brief Returns the next event without reading from the connection.
   * @param c The connection to the X server.
   * @return The next already queued event from the server.
   *
   * This is a version of xcb_poll_for_event that only examines the
   * event queue for new events. The function doesn't try to read new
   * events from the connection if no queued events are found.
   *
   * This function is useful for callers that know in advance that all
   * interesting events have already been read from the connection. For
   * example, callers might use xcb_wait_for_reply and be interested
   * only of events that preceded a specific reply.
    }
function xcb_poll_for_queued_event(var c: xcb_connection_t): Pxcb_generic_event_t;
  cdecl; external External_library Name 'xcb_poll_for_queued_event';


type
  Pxcb_special_event_t = ^xcb_special_event_t;
  xcb_special_event_t = record
  end;

  {*
   * @brief Returns the next event from a special queue
    }

function xcb_poll_for_special_event(var c: xcb_connection_t;
  var se: xcb_special_event_t): Pxcb_generic_event_t; cdecl;
  external External_library Name 'xcb_poll_for_special_event';

  {*
   * @brief Returns the next event from a special queue, blocking until one arrives
    }
function xcb_wait_for_special_event(var c: xcb_connection_t;
  var se: xcb_special_event_t): Pxcb_generic_event_t; cdecl;
  external External_library Name 'xcb_wait_for_special_event';

  {*
   * @typedef typedef struct xcb_extension_t xcb_extension_t
    }

type
  Pxcb_get_extension_data_t = ^xcb_get_extension_data_t;
  xcb_get_extension_data_t = record
  end;

  Pxcb_extension_t = ^xcb_extension_t;
  xcb_extension_t = record
  end;

  {*
   * @brief Listen for a special event
    }

function xcb_register_for_special_xge(var c: xcb_connection_t;
  var ext: xcb_extension_t; eid: uint32_t; var stamp: uint32_t): Pxcb_special_event_t;
  cdecl; external External_library Name 'xcb_register_for_special_xge';

  {*
   * @brief Stop listening for a special event
    }
procedure xcb_unregister_for_special_event(var c: xcb_connection_t;
  var se: xcb_special_event_t); cdecl;
  external External_library Name 'xcb_unregister_for_special_event';

  {*
   * @brief Return the error for a request, or NULL if none can ever arrive.
   * @param c The connection to the X server.
   * @param cookie The request cookie.
   * @return The error for the request, or NULL if none can ever arrive.
   *
   * The xcb_void_cookie_t cookie supplied to this function must have resulted
   * from a call to xcb_[request_name]_checked().  This function will block
   * until one of two conditions happens.  If an error is received, it will be
   * returned.  If a reply to a subsequent request has already arrived, no error
   * can arrive for this request, so this function will return NULL.
   *
   * Note that this function will perform a sync if needed to ensure that the
   * sequence number will advance beyond that provided in cookie; this is a
   * convenience to avoid races in determining whether the sync is needed.
    }
function xcb_request_check(var c: xcb_connection_t;
  cookie: xcb_void_cookie_t): Pxcb_generic_error_t; cdecl;
  external External_library Name 'xcb_request_check';

  {*
   * @brief Discards the reply for a request.
   * @param c The connection to the X server.
   * @param sequence The request sequence number from a cookie.
   *
   * Discards the reply for a request. Additionally, any error generated
   * by the request is also discarded (unless it was an _unchecked request
   * and the error has already arrived).
   *
   * This function will not block even if the reply is not yet available.
   *
   * Note that the sequence really does have to come from an xcb cookie;
   * this function is not designed to operate on socket-handoff replies.
    }
procedure xcb_discard_reply(var c: xcb_connection_t; sequence: dword);
  cdecl; external External_library Name 'xcb_discard_reply';

  {*
   * @brief Discards the reply for a request, given by a 64bit sequence number
   * @param c The connection to the X server.
   * @param sequence 64-bit sequence number as returned by xcb_send_request64().
   *
   * Discards the reply for a request. Additionally, any error generated
   * by the request is also discarded (unless it was an _unchecked request
   * and the error has already arrived).
   *
   * This function will not block even if the reply is not yet available.
   *
   * Note that the sequence really does have to come from xcb_send_request64();
   * the cookie sequence number is defined as "unsigned" int and therefore
   * not 64-bit on all platforms.
   * This function is not designed to operate on socket-handoff replies.
   *
   * Unlike its xcb_discard_reply() counterpart, the given sequence number is not
   * automatically "widened" to 64-bit.
    }
procedure xcb_discard_reply64(var c: xcb_connection_t; sequence: uint64_t);
  cdecl; external External_library Name 'xcb_discard_reply64';

{ xcb_ext.c  }
  {*
   * @brief Caches reply information from QueryExtension requests.
   * @param c The connection.
   * @param ext The extension data.
   * @return A pointer to the xcb_query_extension_reply_t for the extension.
   *
   * This function is the primary interface to the "extension cache",
   * which caches reply information from QueryExtension
   * requests. Invoking this function may cause a call to
   * xcb_query_extension to retrieve extension information from the
   * server, and may block until extension data is received from the
   * server.
   *
   * The result must not be freed. This storage is managed by the cache
   * itself.
    }
(* Const before type ignored *)
function xcb_get_extension_data(var c: xcb_connection_t;
  var ext: xcb_extension_t): Pxcb_query_extension_reply_t; cdecl;
  external External_library Name 'xcb_get_extension_data';

  {*
   * @brief Prefetch of extension data into the extension cache
   * @param c The connection.
   * @param ext The extension data.
   *
   * This function allows a "prefetch" of extension data into the
   * extension cache. Invoking the function may cause a call to
   * xcb_query_extension, but will not block waiting for the
   * reply. xcb_get_extension_data will return the prefetched data after
   * possibly blocking while it is retrieved.
    }
procedure xcb_prefetch_extension_data(var c: xcb_connection_t;
  var ext: xcb_extension_t); cdecl;
  external External_library Name 'xcb_prefetch_extension_data';

{ xcb_conn.c  }
  {*
   * @brief Access the data returned by the server.
   * @param c The connection.
   * @return A pointer to an xcb_setup_t structure.
   *
   * Accessor for the data returned by the server when the xcb_connection_t
   * was initialized. This data includes
   * - the server's required format for images,
   * - a list of available visuals,
   * - a list of available screens,
   * - the server's maximum request length (in the absence of the
   * BIG-REQUESTS extension),
   * - and other assorted information.
   *
   * See the X protocol specification for more details.
   *
   * The result must not be freed.
    }
(* Const before type ignored *)
function xcb_get_setup(var c: xcb_connection_t): Pxcb_setup_t; cdecl;
  external External_library Name 'xcb_get_setup';

  {*
   * @brief Access the file descriptor of the connection.
   * @param c The connection.
   * @return The file descriptor.
   *
   * Accessor for the file descriptor that was passed to the
   * xcb_connect_to_fd call that returned @p c.
    }
function xcb_get_file_descriptor(var c: xcb_connection_t): longint;
  cdecl; external External_library Name 'xcb_get_file_descriptor';

  {*
   * @brief Test whether the connection has shut down due to a fatal error.
   * @param c The connection.
   * @return > 0 if the connection is in an error state; 0 otherwise.
   *
   * Some errors that occur in the context of an xcb_connection_t
   * are unrecoverable. When such an error occurs, the
   * connection is shut down and further operations on the
   * xcb_connection_t have no effect, but memory will not be freed until
   * xcb_disconnect() is called on the xcb_connection_t.
   *
   * @return XCB_CONN_ERROR, because of socket errors, pipe errors or other stream errors.
   * @return XCB_CONN_CLOSED_EXT_NOTSUPPORTED, when extension not supported.
   * @return XCB_CONN_CLOSED_MEM_INSUFFICIENT, when memory not available.
   * @return XCB_CONN_CLOSED_REQ_LEN_EXCEED, exceeding request length that server accepts.
   * @return XCB_CONN_CLOSED_PARSE_ERR, error during parsing display string.
   * @return XCB_CONN_CLOSED_INVALID_SCREEN, because the server does not have a screen matching the display.
    }
function xcb_connection_has_error(var c: xcb_connection_t): longint;
  cdecl; external External_library Name 'xcb_connection_has_error';

  {*
   * @brief Connects to the X server.
   * @param fd The file descriptor.
   * @param auth_info Authentication data.
   * @return A newly allocated xcb_connection_t structure.
   *
   * Connects to an X server, given the open socket @p fd and the
   * xcb_auth_info_t @p auth_info. The file descriptor @p fd is
   * bidirectionally connected to an X server. If the connection
   * should be unauthenticated, @p auth_info must be @c
   * NULL.
   *
   * Always returns a non-NULL pointer to a xcb_connection_t, even on failure.
   * Callers need to use xcb_connection_has_error() to check for failure.
   * When finished, use xcb_disconnect() to close the connection and free
   * the structure.
    }
function xcb_connect_to_fd(fd: longint;
  var auth_info: xcb_auth_info_t): Pxcb_connection_t; cdecl;
  external External_library Name 'xcb_connect_to_fd';

  {*
   * @brief Closes the connection.
   * @param c The connection.
   *
   * Closes the file descriptor and frees all memory associated with the
   * connection @c c. If @p c is @c NULL, nothing is done.
    }
procedure xcb_disconnect(var c: xcb_connection_t); cdecl;
  external External_library Name 'xcb_disconnect';

{ xcb_util.c  }
  {*
   * @brief Parses a display string name in the form documented by X(7x).
   * @param name The name of the display.
   * @param host A pointer to a malloc'd copy of the hostname.
   * @param display A pointer to the display number.
   * @param screen A pointer to the screen number.
   * @return 0 on failure, non 0 otherwise.
   *
   * Parses the display string name @p display_name in the form
   * documented by X(7x). Has no side effects on failure. If
   * @p displayname is @c NULL or empty, it uses the environment
   * variable DISPLAY. @p hostp is a pointer to a newly allocated string
   * that contain the host name. @p displayp is set to the display
   * number and @p screenp to the preferred screen number. @p screenp
   * can be @c NULL. If @p displayname does not contain a screen number,
   * it is set to @c 0.
    }
(* Const before type ignored *)
function xcb_parse_display(Name: PChar; host: PPchar; var display: longint;
  var screen: longint): longint; cdecl; external External_library Name 'xcb_parse_display';

  {*
   * @brief Connects to the X server.
   * @param displayname The name of the display.
   * @param screenp A pointer to a preferred screen number.
   * @return A newly allocated xcb_connection_t structure.
   *
   * Connects to the X server specified by @p displayname. If @p
   * displayname is @c NULL, uses the value of the DISPLAY environment
   * variable. If a particular screen on that server is preferred, the
   * int pointed to by @p screenp (if not @c NULL) will be set to that
   * screen; otherwise the screen will be set to 0.
   *
   * Always returns a non-NULL pointer to a xcb_connection_t, even on failure.
   * Callers need to use xcb_connection_has_error() to check for failure.
   * When finished, use xcb_disconnect() to close the connection and free
   * the structure.
    }
(* Const before type ignored *)
function xcb_connect(displayname: PChar; var screenp: longint): Pxcb_connection_t;
  cdecl; external External_library Name 'xcb_connect';

  {*
   * @brief Connects to the X server, using an authorization information.
   * @param display The name of the display.
   * @param auth The authorization information.
   * @param screen A pointer to a preferred screen number.
   * @return A newly allocated xcb_connection_t structure.
   *
   * Connects to the X server specified by @p displayname, using the
   * authorization @p auth. If a particular screen on that server is
   * preferred, the int pointed to by @p screenp (if not @c NULL) will
   * be set to that screen; otherwise @p screenp will be set to 0.
   *
   * Always returns a non-NULL pointer to a xcb_connection_t, even on failure.
   * Callers need to use xcb_connection_has_error() to check for failure.
   * When finished, use xcb_disconnect() to close the connection and free
   * the structure.
    }
(* Const before type ignored *)
function xcb_connect_to_display_with_auth_info(display: PChar;
  var auth: xcb_auth_info_t; var screen: longint): Pxcb_connection_t;
  cdecl; external External_library Name 'xcb_connect_to_display_with_auth_info';

{ xcb_xid.c  }
  {*
   * @brief Allocates an XID for a new object.
   * @param c The connection.
   * @return A newly allocated XID.
   *
   * Allocates an XID for a new object. Typically used just prior to
   * various object creation functions, such as xcb_create_window.
    }
function xcb_generate_id(var c: xcb_connection_t): uint32_t; cdecl;
  external External_library Name 'xcb_generate_id';

  {*
   * @brief Obtain number of bytes read from the connection.
   * @param c The connection
   * @return Number of bytes read from the server.
   *
   * Returns cumulative number of bytes received from the connection.
   *
   * This retrieves the total number of bytes read from this connection,
   * to be used for diagnostic/monitoring/informative purposes.
    }
function xcb_total_read(var c: xcb_connection_t): uint64_t; cdecl;
  external External_library Name 'xcb_total_read';

  {*
   *
   * @brief Obtain number of bytes written to the connection.
   * @param c The connection
   * @return Number of bytes written to the server.
   *
   * Returns cumulative number of bytes sent to the connection.
   *
   * This retrieves the total number of bytes written to this connection,
   * to be used for diagnostic/monitoring/informative purposes.
    }
function xcb_total_written(var c: xcb_connection_t): uint64_t; cdecl;
  external External_library Name 'xcb_total_written';

implementation


end.
