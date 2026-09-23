{
Copyright (C) 2006-2026 Matteo Salvi

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

---

Global-hotkey backend for Wayland via the freedesktop GlobalShortcuts
portal (org.freedesktop.portal.GlobalShortcuts) over the session bus.

Used when no X11 display is available or the session is Wayland-native.
Links libdbus like the X11 backend links libX11; without a session bus
or portal everything degrades to False.

Binding model
-------------
The backend keeps a *desired* set (what the application asked for) and
applies it according to TPortalBindStrategy (see TBaseHotkeyManager):

  pbsSpecCompliant - a fresh session for every change, so BindShortcuts is
    called once per session as the interface documentation requires. This
    is the portable behaviour, but on KDE it cannot unregister a shortcut:
    KGlobalAccel::removeAllShortcuts is a no-op for actions that were only
    loaded from a previous session.

  pbsIncremental - a single long-lived session, re-bound on every change.
    Shortcuts removed from the set are dropped because they were registered
    in the current session. The documentation states "an application can
    only attempt to bind shortcuts of a session once"; the
    xdg-desktop-portal frontend does not enforce it and the KDE 6.7 backend
    is written to reconcile a changed set (it computes the difference and
    only prompts for new shortcuts), but a stricter backend could reject the
    later calls. At session start the persisted shortcuts are listed and
    re-bound once so stale entries left by older runs can be dropped too.

  pbsAuto - pbsIncremental on KDE, pbsSpecCompliant elsewhere. The portal
    does not expose which backend serves GlobalShortcuts, so the desktop is
    detected from XDG_CURRENT_DESKTOP / XDG_SESSION_DESKTOP / KDE_FULL_SESSION.

Shortcuts persisted by a previous run cannot be unregistered directly
(KDE only removes actions registered in the current session). At session
start they are therefore listed and re-bound once, which registers them
and lets the following bind drop the ones that are no longer wanted.

The session handle token (AppToken) only namespaces the session, the
request handles and the shortcut ids. It does NOT necessarily select the
KDE component shown in System Settings: the component is the *application
id* the portal derives from the process (e.g. the IDE or terminal it was
started from), or "token_<AppToken>" when no application id could be
determined.

Portal accelerator format (parsed by compositors, e.g. KDE):
  CTRL+ALT+F7   (uppercase modifiers joined by '+', XKB key names)
}

unit Hotkeys.Manager.Portal;

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, LCLType,
  Hotkeys.ShortcutEx
  {$IFDEF UNIX}
  , syncobjs,
  dbus,
  Hotkeys.Manager
  {$ENDIF}
  ;

{ Converts Key + ShiftState to a portal accelerator ('' if unsupported).
  At least one modifier is required, like the THotKey control enforces.
  Pure logic, available on every platform (for tests). }
function PortalAccelerator(Key: Word; Shift: TShiftState): String;

{$IFDEF UNIX}

type
  TPortalShortcutInfo = record
    Id: String;
    Trigger: String;      // accelerator we ask for, or trigger_description we got
    Description: String;  // user-readable text (required by KDE when re-binding)
  end;
  TPortalShortcutArray = array of TPortalShortcutInfo;

  TIterBuilder = procedure(Iter: PDBusMessageIter) of object;

  { TPortalHotkeyEngine }

  { All D-Bus I/O runs on the thread that uses the engine (the application
    main thread), driven by ProcessPending. There is deliberately no
    background thread: the portal only requires the session bus, the app
    already owns a main loop, and a dispatch thread would need the FPC thread
    driver and can deadlock against send_with_reply_and_block. }
  TPortalHotkeyEngine = class
  private
    FConn: PDBusConnection;
    FManager: TBaseHotkeyManager;
    FSession: String;
    FLock: TCriticalSection;
    FOpLock: TCriticalSection;
    FMatchAdded: Boolean;
    FAppToken: String;
    FStrategy: TPortalBindStrategy;
    FNeedsReconcile: Boolean;
    FDesired: TStringList;   // shortcut id -> TShortcutEx (not owned)
    FTriggers: TStringList;  // shortcut id -> trigger assigned by the portal
    FPending: TStringList;   // request path -> TPortalResponse (owned)
    FPendingActivations: TStringList; // shortcut ids waiting to be delivered
    FTokenSeq: Integer;
    FBuildToken: String;
    FBuildSession: String;
    FBuildName: String;
    FBuildItems: TPortalShortcutArray;
    FLastError: String;
    FVerbose: Boolean;

    procedure SetAppToken(const Value: String);
    function ShortcutIdOf(Shortcut: TShortcutEx): String;
    function Connect: Boolean;
    function NewToken(const Prefix: String): String;
    procedure AppendString(Iter: PDBusMessageIter; const Value: String);
    procedure AppendObjectPath(Iter: PDBusMessageIter; const Value: String);
    procedure AppendVariantString(Iter: PDBusMessageIter; const Key, Value: String);
    procedure BuildCreateSessionArgs(Iter: PDBusMessageIter);
    procedure BuildBindArgs(Iter: PDBusMessageIter);
    procedure BuildListArgs(Iter: PDBusMessageIter);
    procedure BuildOwnerArgs(Iter: PDBusMessageIter);
    function CallPortal(const Path, Member: String; Builder: TIterBuilder;
      out Reply: PDBusMessage; TimeoutMs: Integer = 5000): Boolean;
    function CallDBus(const Member: String; Builder: TIterBuilder;
      out Reply: PDBusMessage; TimeoutMs: Integer = 5000): Boolean;
    function RequestPathFromReply(Reply: PDBusMessage; out ReqPath: String): Boolean;
    procedure HandleResponse(Msg: PDBusMessage);
    procedure HandleActivated(Msg: PDBusMessage);
    procedure HandleShortcutsChanged(Msg: PDBusMessage);
    function TakeResponse(const Path: String; out Code: Cardinal;
      out SessionHandle: String; out Shortcuts: TPortalShortcutArray): Boolean;
    function WaitResponse(const RequestPath: String; out Code: Cardinal;
      out SessionHandle: String; out Shortcuts: TPortalShortcutArray;
      TimeoutMs: Integer): Boolean;
    function EnsureSession: Boolean;
    function BindSet(const Items: TPortalShortcutArray;
      out Shortcuts: TPortalShortcutArray): Boolean;
    function ListPersisted(out Items: TPortalShortcutArray): Boolean;
    function SyncIncremental(out Shortcuts: TPortalShortcutArray): Boolean;
    function Rebuild(out Shortcuts: TPortalShortcutArray): Boolean;
    function SyncBindings(out Shortcuts: TPortalShortcutArray): Boolean;
    procedure CloseSession;
    procedure AddMatch;
    procedure RemoveMatch;
    function NameHasOwner(const Name: String): Boolean;
    procedure Log(const Msg: String);
    function FindShortcut(const Id: String): TShortcutEx;
    function SnapshotDesired: TPortalShortcutArray;
    procedure SetTriggers(const Shortcuts: TPortalShortcutArray);
    procedure ClearTriggers;
    function TriggerOf(const Id: String): String;
    procedure RollbackDesired(const Id: String);
    procedure QueueActivation(const Id: String);
    procedure DrainActivations;
    procedure FireActivation(const Id: String);
    function HandleMessage(Msg: PDBusMessage): Boolean;
  public
    constructor Create(AManager: TBaseHotkeyManager);
    destructor Destroy; override;

    function HasPortal: Boolean;
    function RegisterShortcut(Shortcut: TShortcutEx; out Trigger: String): Boolean;
    function UnregisterShortcut(Shortcut: TShortcutEx): Boolean;
    { Dispatches any pending D-Bus message and delivers queued activations.
      Must be called periodically from the thread that owns the engine (the
      application main thread, e.g. from an idle handler or a timer). }
    procedure ProcessPending;
    { Drops every desired shortcut and closes the session. }
    procedure Reset;

    property SessionHandle: String read FSession;
    property LastError: String read FLastError;
    property Verbose: Boolean read FVerbose write FVerbose;
    { Namespace for the session handle, request handles and shortcut ids.
      Set it before the first registration. }
    property AppToken: String read FAppToken write SetAppToken;
    { Binding strategy; pbsAuto is resolved at bind time (see
      TPortalBindStrategy). Set it before the first registration. }
    property BindStrategy: TPortalBindStrategy read FStrategy write FStrategy;
    { The strategy that will actually be used, with pbsAuto resolved. }
    function ResolvedBindStrategy: TPortalBindStrategy;
  end;

{ True when the session bus answers and the portal owns its name }
function PortalAvailable: Boolean;

{$ENDIF}

implementation

{ PortalAccelerator }

function PortalAccelerator(Key: Word; Shift: TShiftState): String;
var
  Mods, KeyName: String;
begin
  Mods := '';
  if ssCtrl in Shift then Mods := Mods + 'CTRL+';
  if ssShift in Shift then Mods := Mods + 'SHIFT+';
  if ssAlt in Shift then Mods := Mods + 'ALT+';
  if (ssMeta in Shift) or (ssSuper in Shift) then Mods := Mods + 'LOGO+';

  case Key of
    VK_A..VK_Z: KeyName := Chr(Key);
    VK_0..VK_9: KeyName := Chr(Key);
    VK_F1..VK_F24: KeyName := 'F' + IntToStr(Key - VK_F1 + 1);
    VK_NUMPAD0..VK_NUMPAD9: KeyName := 'KP_' + Chr(Ord('0') + (Key - VK_NUMPAD0));
    VK_SPACE: KeyName := 'space';
    VK_RETURN: KeyName := 'Return';
    VK_ESCAPE: KeyName := 'Escape';
    VK_TAB: KeyName := 'Tab';
    VK_BACK: KeyName := 'BackSpace';
    VK_DELETE: KeyName := 'Delete';
    VK_INSERT: KeyName := 'Insert';
    VK_HOME: KeyName := 'Home';
    VK_END: KeyName := 'End';
    VK_PRIOR: KeyName := 'Prior';
    VK_NEXT: KeyName := 'Next';
    VK_LEFT: KeyName := 'Left';
    VK_UP: KeyName := 'Up';
    VK_RIGHT: KeyName := 'Right';
    VK_DOWN: KeyName := 'Down';
    VK_PAUSE: KeyName := 'Pause';
    VK_SNAPSHOT: KeyName := 'Print';
    VK_CLEAR: KeyName := 'Clear';
    VK_MENU: KeyName := 'Menu';
    VK_SELECT: KeyName := 'Select';
    VK_EXECUTE: KeyName := 'Execute';
    VK_HELP: KeyName := 'Help';
    VK_MULTIPLY: KeyName := 'KP_Multiply';
    VK_ADD: KeyName := 'KP_Add';
    VK_SEPARATOR: KeyName := 'KP_Separator';
    VK_SUBTRACT: KeyName := 'KP_Subtract';
    VK_DECIMAL: KeyName := 'KP_Decimal';
    VK_DIVIDE: KeyName := 'KP_Divide';
    VK_LCL_EQUAL: KeyName := 'equal';
    VK_LCL_COMMA: KeyName := 'comma';
    VK_LCL_POINT: KeyName := 'period';
    VK_LCL_SLASH: KeyName := 'slash';
    VK_LCL_SEMI_COMMA: KeyName := 'semicolon';
    VK_LCL_MINUS: KeyName := 'minus';
    VK_LCL_OPEN_BRACKET: KeyName := 'bracketleft';
    VK_LCL_CLOSE_BRACKET: KeyName := 'bracketright';
    VK_LCL_BACKSLASH: KeyName := 'backslash';
    VK_LCL_TILDE: KeyName := 'grave';
    VK_LCL_QUOTE: KeyName := 'apostrophe';
  else
    KeyName := '';
  end;

  if (Mods = '') or (KeyName = '') then
    Exit('');
  Result := Mods + KeyName;
end;

{$IFDEF UNIX}

const
  PORTAL_BUS     = 'org.freedesktop.portal.Desktop';
  PORTAL_PATH    = '/org/freedesktop/portal/desktop';
  PORTAL_IFACE   = 'org.freedesktop.portal.GlobalShortcuts';
  REQUEST_IFACE  = 'org.freedesktop.portal.Request';
  SESSION_IFACE  = 'org.freedesktop.portal.Session';
  DBUS_BUS_NAME  = 'org.freedesktop.DBus';
  DBUS_BUS_PATH  = '/org/freedesktop/DBus';
  DBUS_BUS_IFACE = 'org.freedesktop.DBus';
  MATCH_RULE     = 'type=''signal'',interface=''' + PORTAL_IFACE + '''';

type
  { Response of a portal Request, cached by request path. }
  TPortalResponse = class
    Code: Cardinal;
    SessionHandle: String;
    Shortcuts: TPortalShortcutArray;
  end;

function Ok(B: dbus_bool_t): Boolean; inline;
begin
  Result := B <> 0;
end;

function PortalFilter(Conn: PDBusConnection; Msg: PDBusMessage; Data: Pointer): LongInt; cdecl; forward;

// Object path elements (and therefore session/handle tokens) may only use
// [A-Za-z0-9_]; shortcut ids are free-form but we keep the same alphabet.
function SanitizeToken(const Value: String): String;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    C := Value[I];
    if ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'))
      or ((C >= '0') and (C <= '9')) or (C = '_') then
      Result := Result + C
    else
      Result := Result + '_';
  end;
  if Result = '' then
    Result := 'hotkey';
end;

// Heuristic: the portal does not expose which backend serves GlobalShortcuts,
// so pbsAuto falls back to the desktop name.
function KdeDesktopDetected: Boolean;
var
  S: String;
begin
  S := UpperCase(GetEnvironmentVariable('XDG_CURRENT_DESKTOP'));
  if Pos('KDE', S) > 0 then
    Exit(True);
  S := UpperCase(GetEnvironmentVariable('XDG_SESSION_DESKTOP'));
  if Pos('KDE', S) > 0 then
    Exit(True);
  Result := GetEnvironmentVariable('KDE_FULL_SESSION') <> '';
end;

function IndexOfShortcut(const Shortcuts: TPortalShortcutArray; const Id: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Shortcuts) do
    if Shortcuts[I].Id = Id then
      Exit(I);
end;

function PortalAvailable: Boolean;
var
  Engine: TPortalHotkeyEngine;
begin
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Result := Engine.HasPortal;
  finally
    Engine.Free;
  end;
end;

function ReadBasicString(Iter: PDBusMessageIter; WantType: Integer;
  out Value: String): Boolean;
var
  P: PChar;
begin
  Result := dbus_message_iter_get_arg_type(Iter) = WantType;
  if not Result then
    Exit;
  P := nil;
  dbus_message_iter_get_basic(Iter, @P);
  if P <> nil then
    Value := String(P)
  else
    Value := '';
end;

function ReadVariantString(Iter: PDBusMessageIter; out Value: String): Boolean;
var
  Sub: DBusMessageIter;
begin
  Result := False;
  Value := '';
  if dbus_message_iter_get_arg_type(Iter) <> DBUS_TYPE_VARIANT then
    Exit;
  dbus_message_iter_recurse(Iter, @Sub);
  Result := ReadBasicString(@Sub, DBUS_TYPE_STRING, Value);
end;

// Reads the a(sa{sv}) shortcut list returned by BindShortcuts / ListShortcuts
// and by the ShortcutsChanged signal.
function ReadShortcutList(Iter: PDBusMessageIter; out Shortcuts: TPortalShortcutArray): Boolean;
var
  Items, Item, Opts, Opt: DBusMessageIter;
  Id, Trigger, Desc, S: String;
begin
  Result := False;
  SetLength(Shortcuts, 0);
  if dbus_message_iter_get_arg_type(Iter) <> DBUS_TYPE_ARRAY then
    Exit;
  dbus_message_iter_recurse(Iter, @Items);
  repeat
    if dbus_message_iter_get_arg_type(@Items) <> DBUS_TYPE_STRUCT then
      Break;
    dbus_message_iter_recurse(@Items, @Item);
    if not ReadBasicString(@Item, DBUS_TYPE_STRING, Id) then
      Break;
    Trigger := '';
    Desc := '';
    if Ok(dbus_message_iter_next(@Item))
      and (dbus_message_iter_get_arg_type(@Item) = DBUS_TYPE_ARRAY) then
    begin
      dbus_message_iter_recurse(@Item, @Opts);
      repeat
        if dbus_message_iter_get_arg_type(@Opts) <> DBUS_TYPE_DICT_ENTRY then
          Break;
        dbus_message_iter_recurse(@Opts, @Opt);
        if not ReadBasicString(@Opt, DBUS_TYPE_STRING, S) then
          Break;
        if Ok(dbus_message_iter_next(@Opt)) then
        begin
          if S = 'trigger_description' then
            ReadVariantString(@Opt, Trigger)
          else if S = 'description' then
            ReadVariantString(@Opt, Desc);
        end;
      until not Ok(dbus_message_iter_next(@Opts));
    end;
    SetLength(Shortcuts, Length(Shortcuts) + 1);
    Shortcuts[High(Shortcuts)].Id := Id;
    Shortcuts[High(Shortcuts)].Trigger := Trigger;
    Shortcuts[High(Shortcuts)].Description := Desc;
  until not Ok(dbus_message_iter_next(@Items));
  Result := True;
end;

function ReadResultsDict(Iter: PDBusMessageIter; out SessionHandle: String;
  out Shortcuts: TPortalShortcutArray): Boolean;
var
  Arr, Entry, VarIter: DBusMessageIter;
  Key: String;
begin
  Result := False;
  SessionHandle := '';
  SetLength(Shortcuts, 0);
  if dbus_message_iter_get_arg_type(Iter) <> DBUS_TYPE_ARRAY then
    Exit;
  dbus_message_iter_recurse(Iter, @Arr);
  repeat
    if dbus_message_iter_get_arg_type(@Arr) <> DBUS_TYPE_DICT_ENTRY then
      Break;
    dbus_message_iter_recurse(@Arr, @Entry);
    if not ReadBasicString(@Entry, DBUS_TYPE_STRING, Key) then
      Break;
    if not Ok(dbus_message_iter_next(@Entry)) then
      Break;
    if Key = 'session_handle' then
      ReadVariantString(@Entry, SessionHandle)
    else if (Key = 'shortcuts')
      and (dbus_message_iter_get_arg_type(@Entry) = DBUS_TYPE_VARIANT) then
    begin
      dbus_message_iter_recurse(@Entry, @VarIter);
      ReadShortcutList(@VarIter, Shortcuts);
    end;
  until not Ok(dbus_message_iter_next(@Arr));
  Result := True;
end;

function PortalFilter(Conn: PDBusConnection; Msg: PDBusMessage; Data: Pointer): LongInt; cdecl;
var
  Engine: TPortalHotkeyEngine absolute Data;
begin
  if Engine.HandleMessage(Msg) then
    Result := 0 // DBUS_HANDLER_RESULT_HANDLED
  else
    Result := 1; // DBUS_HANDLER_RESULT_NOT_YET_HANDLED
end;

{ TPortalHotkeyEngine }

constructor TPortalHotkeyEngine.Create(AManager: TBaseHotkeyManager);
begin
  inherited Create;
  FManager := AManager;
  FLock := TCriticalSection.Create;
  FOpLock := TCriticalSection.Create;
  FDesired := TStringList.Create;
  FDesired.Sorted := True;
  FDesired.Duplicates := dupIgnore;
  FTriggers := TStringList.Create;
  FTriggers.Sorted := True;
  FTriggers.Duplicates := dupIgnore;
  FPending := TStringList.Create;
  FPending.OwnsObjects := True; // owns the TPortalResponse cache entries
  FPendingActivations := TStringList.Create;
  FPendingActivations.Sorted := True;
  FPendingActivations.Duplicates := dupIgnore;
  FAppToken := 'hotkey';
  FStrategy := pbsAuto;
  dbus_threads_init_default();
end;

function TPortalHotkeyEngine.ResolvedBindStrategy: TPortalBindStrategy;
begin
  Result := FStrategy;
  if Result = pbsAuto then
  begin
    if KdeDesktopDetected then
      Result := pbsIncremental
    else
      Result := pbsSpecCompliant;
  end;
end;

procedure TPortalHotkeyEngine.SetAppToken(const Value: String);
begin
  FAppToken := SanitizeToken(Value);
end;

function TPortalHotkeyEngine.ShortcutIdOf(Shortcut: TShortcutEx): String;
begin
  Result := FAppToken + IntToStr(Shortcut.SimpleShortcut);
end;

destructor TPortalHotkeyEngine.Destroy;
begin
  if FConn <> nil then
    dbus_connection_remove_filter(FConn, @PortalFilter, Self);
  CloseSession;
  RemoveMatch;
  if FConn <> nil then
  begin
    dbus_connection_close(FConn);
    dbus_connection_unref(FConn);
    FConn := nil;
  end;
  FreeAndNil(FPendingActivations);
  FreeAndNil(FPending);
  FreeAndNil(FTriggers);
  FreeAndNil(FDesired);
  FreeAndNil(FOpLock);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TPortalHotkeyEngine.Connect: Boolean;
var
  Err: DBusError;
begin
  if FConn <> nil then
    Exit(True);
  dbus_error_init(@Err);
  FConn := dbus_bus_get_private(DBUS_BUS_SESSION, @Err);
  if Ok(dbus_error_is_set(@Err)) then
    dbus_error_free(@Err);
  Result := FConn <> nil;
  if Result then
  begin
    dbus_connection_set_exit_on_disconnect(FConn, 0);
    // Filter first: Response signals must be cached from the very first call.
    dbus_connection_add_filter(FConn, @PortalFilter, Self, nil);
  end;
end;

function TPortalHotkeyEngine.NewToken(const Prefix: String): String;
begin
  Inc(FTokenSeq);
  Result := Prefix + IntToHex(GetTickCount64 and $FFFFFFFF, 8) + '_' + IntToStr(FTokenSeq);
end;

procedure TPortalHotkeyEngine.AppendString(Iter: PDBusMessageIter; const Value: String);
var
  S: UTF8String;
  P: PChar;
begin
  S := UTF8String(Value);
  P := PChar(S);
  dbus_message_iter_append_basic(Iter, DBUS_TYPE_STRING, @P);
end;

procedure TPortalHotkeyEngine.AppendObjectPath(Iter: PDBusMessageIter; const Value: String);
var
  S: UTF8String;
  P: PChar;
begin
  S := UTF8String(Value);
  P := PChar(S);
  dbus_message_iter_append_basic(Iter, DBUS_TYPE_OBJECT_PATH, @P);
end;

procedure TPortalHotkeyEngine.AppendVariantString(Iter: PDBusMessageIter;
  const Key, Value: String);
var
  Entry, VarIter: DBusMessageIter;
begin
  dbus_message_iter_open_container(Iter, DBUS_TYPE_DICT_ENTRY, nil, @Entry);
  try
    AppendString(@Entry, Key);
    dbus_message_iter_open_container(@Entry, DBUS_TYPE_VARIANT, 's', @VarIter);
    try
      AppendString(@VarIter, Value);
    finally
      dbus_message_iter_close_container(@Entry, @VarIter);
    end;
  finally
    dbus_message_iter_close_container(Iter, @Entry);
  end;
end;

procedure TPortalHotkeyEngine.BuildCreateSessionArgs(Iter: PDBusMessageIter);
var
  Arr: DBusMessageIter;
begin
  dbus_message_iter_open_container(Iter, DBUS_TYPE_ARRAY, '{sv}', @Arr);
  try
    AppendVariantString(@Arr, 'session_handle_token', FAppToken);
    AppendVariantString(@Arr, 'handle_token', FBuildToken);
  finally
    dbus_message_iter_close_container(Iter, @Arr);
  end;
end;

procedure TPortalHotkeyEngine.BuildBindArgs(Iter: PDBusMessageIter);
var
  Arr, Item, Opts, Tail: DBusMessageIter;
  I: Integer;
  Acc, Desc: String;
begin
  // o session_handle
  AppendObjectPath(Iter, FBuildSession);
  // a(sa{sv}) shortcuts
  dbus_message_iter_open_container(Iter, DBUS_TYPE_ARRAY, '(sa{sv})', @Arr);
  try
    for I := 0 to High(FBuildItems) do
    begin
      Acc := FBuildItems[I].Trigger;
      // Persisted entries carry their original description; new ones get one
      // built from the token (KDE ignores it for shortcuts it already knows).
      Desc := FBuildItems[I].Description;
      if Desc = '' then
        Desc := FAppToken + ' hotkey ' + Acc;
      dbus_message_iter_open_container(@Arr, DBUS_TYPE_STRUCT, nil, @Item);
      try
        AppendString(@Item, FBuildItems[I].Id);
        dbus_message_iter_open_container(@Item, DBUS_TYPE_ARRAY, '{sv}', @Opts);
        try
          AppendVariantString(@Opts, 'description', Desc);
          AppendVariantString(@Opts, 'preferred_trigger', Acc);
        finally
          dbus_message_iter_close_container(@Item, @Opts);
        end;
      finally
        dbus_message_iter_close_container(@Arr, @Item);
      end;
    end;
  finally
    dbus_message_iter_close_container(Iter, @Arr);
  end;
  // s parent_window
  AppendString(Iter, '');
  // a{sv} options
  dbus_message_iter_open_container(Iter, DBUS_TYPE_ARRAY, '{sv}', @Tail);
  try
    AppendVariantString(@Tail, 'handle_token', FBuildToken);
  finally
    dbus_message_iter_close_container(Iter, @Tail);
  end;
end;

procedure TPortalHotkeyEngine.BuildListArgs(Iter: PDBusMessageIter);
var
  Opts: DBusMessageIter;
begin
  // o session_handle
  AppendObjectPath(Iter, FBuildSession);
  // a{sv} options
  dbus_message_iter_open_container(Iter, DBUS_TYPE_ARRAY, '{sv}', @Opts);
  try
    AppendVariantString(@Opts, 'handle_token', FBuildToken);
  finally
    dbus_message_iter_close_container(Iter, @Opts);
  end;
end;

procedure TPortalHotkeyEngine.BuildOwnerArgs(Iter: PDBusMessageIter);
begin
  AppendString(Iter, FBuildName);
end;

function TPortalHotkeyEngine.CallPortal(const Path, Member: String;
  Builder: TIterBuilder; out Reply: PDBusMessage; TimeoutMs: Integer): Boolean;
var
  Msg: PDBusMessage;
  Iter: DBusMessageIter;
  Err: DBusError;
begin
  Result := False;
  Reply := nil;
  if not Connect then
    Exit;
  Msg := dbus_message_new_method_call(PORTAL_BUS, PChar(UTF8String(Path)),
    PORTAL_IFACE, PChar(UTF8String(Member)));
  if Msg = nil then
    Exit;
  try
    if Assigned(Builder) then
    begin
      dbus_message_iter_init_append(Msg, @Iter);
      Builder(@Iter);
    end;
    dbus_error_init(@Err);
    Reply := dbus_connection_send_with_reply_and_block(FConn, Msg, TimeoutMs, @Err);
    if Ok(dbus_error_is_set(@Err)) then
    begin
      if Err.name <> nil then
        FLastError := String(Err.name);
      if Err.message <> nil then
        FLastError := FLastError + ': ' + String(Err.message);
      dbus_error_free(@Err);
    end;
    Result := Reply <> nil;
  finally
    dbus_message_unref(Msg);
  end;
end;

function TPortalHotkeyEngine.CallDBus(const Member: String;
  Builder: TIterBuilder; out Reply: PDBusMessage; TimeoutMs: Integer): Boolean;
var
  Msg: PDBusMessage;
  Iter: DBusMessageIter;
  Err: DBusError;
begin
  Result := False;
  Reply := nil;
  if not Connect then
    Exit;
  Msg := dbus_message_new_method_call(DBUS_BUS_NAME, DBUS_BUS_PATH,
    DBUS_BUS_IFACE, PChar(UTF8String(Member)));
  if Msg = nil then
    Exit;
  try
    if Assigned(Builder) then
    begin
      dbus_message_iter_init_append(Msg, @Iter);
      Builder(@Iter);
    end;
    dbus_error_init(@Err);
    Reply := dbus_connection_send_with_reply_and_block(FConn, Msg, TimeoutMs, @Err);
    if Ok(dbus_error_is_set(@Err)) then
    begin
      if Err.name <> nil then
        FLastError := String(Err.name);
      if Err.message <> nil then
        FLastError := FLastError + ': ' + String(Err.message);
      dbus_error_free(@Err);
    end;
    Result := Reply <> nil;
  finally
    dbus_message_unref(Msg);
  end;
end;

function TPortalHotkeyEngine.RequestPathFromReply(Reply: PDBusMessage;
  out ReqPath: String): Boolean;
var
  Iter: DBusMessageIter;
  P: PChar;
begin
  Result := False;
  ReqPath := '';
  if Reply = nil then
    Exit;
  try
    if not Ok(dbus_message_iter_init(Reply, @Iter))
      or (dbus_message_iter_get_arg_type(@Iter) <> DBUS_TYPE_OBJECT_PATH) then
      Exit;
    P := nil;
    dbus_message_iter_get_basic(@Iter, @P);
    if P = nil then
      Exit;
    ReqPath := String(P);
    Result := ReqPath <> '';
  finally
    dbus_message_unref(Reply);
  end;
end;

procedure TPortalHotkeyEngine.Log(const Msg: String);
begin
  if FVerbose then
    WriteLn('[portal] ', Msg);
end;

procedure TPortalHotkeyEngine.HandleResponse(Msg: PDBusMessage);
var
  Iter: DBusMessageIter;
  Path, Session: String;
  Code: Cardinal;
  Shortcuts: TPortalShortcutArray;
  Resp: TPortalResponse;
begin
  Path := String(dbus_message_get_path(Msg));
  Code := 2;
  Session := '';
  SetLength(Shortcuts, 0);
  if Ok(dbus_message_iter_init(Msg, @Iter))
    and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_UINT32) then
  begin
    dbus_message_iter_get_basic(@Iter, @Code);
    if Ok(dbus_message_iter_next(@Iter)) then
      ReadResultsDict(@Iter, Session, Shortcuts);
  end;

  // Cache unconditionally: the signal may arrive before the caller starts
  // waiting (e.g. while send_with_reply_and_block dispatched the reply).
  Resp := TPortalResponse.Create;
  Resp.Code := Code;
  Resp.SessionHandle := Session;
  Resp.Shortcuts := Shortcuts;
  FLock.Acquire;
  try
    if FPending.IndexOf(Path) < 0 then
    begin
      // Bound the cache: a response nobody waits for (e.g. an aborted
      // request) must not accumulate forever.
      while FPending.Count >= 32 do
        FPending.Delete(0);
      FPending.AddObject(Path, Resp);
    end
    else
      Resp.Free;
  finally
    FLock.Release;
  end;
end;

function TPortalHotkeyEngine.TakeResponse(const Path: String; out Code: Cardinal;
  out SessionHandle: String; out Shortcuts: TPortalShortcutArray): Boolean;
var
  I: Integer;
  Resp: TPortalResponse;
begin
  Result := False;
  Code := 2;
  SessionHandle := '';
  SetLength(Shortcuts, 0);
  FLock.Acquire;
  try
    I := FPending.IndexOf(Path);
    if I >= 0 then
    begin
      Resp := TPortalResponse(FPending.Objects[I]);
      Code := Resp.Code;
      SessionHandle := Resp.SessionHandle;
      Shortcuts := Resp.Shortcuts;
      FPending.Delete(I); // OwnsObjects frees Resp
      Result := True;
    end;
  finally
    FLock.Release;
  end;
end;

function TPortalHotkeyEngine.WaitResponse(const RequestPath: String;
  out Code: Cardinal; out SessionHandle: String;
  out Shortcuts: TPortalShortcutArray; TimeoutMs: Integer): Boolean;
var
  Deadline: QWord;
begin
  Code := 2;
  SessionHandle := '';
  SetLength(Shortcuts, 0);
  Deadline := GetTickCount64 + QWord(TimeoutMs);
  repeat
    if TakeResponse(RequestPath, Code, SessionHandle, Shortcuts) then
      Exit(True);
    if FConn <> nil then
      dbus_connection_read_write_dispatch(FConn, 50);
  until GetTickCount64 >= Deadline;
  Result := TakeResponse(RequestPath, Code, SessionHandle, Shortcuts);
end;

function TPortalHotkeyEngine.EnsureSession: Boolean;
var
  Reply: PDBusMessage;
  ReqPath, Handle: String;
  Code: Cardinal;
  Shortcuts: TPortalShortcutArray;
begin
  if FSession <> '' then
    Exit(True);
  if not Connect then
    Exit(False);
  AddMatch;
  FBuildToken := NewToken(FAppToken);
  if not CallPortal(PORTAL_PATH, 'CreateSession', BuildCreateSessionArgs, Reply) then
  begin
    Log('CreateSession failed: ' + FLastError);
    Exit(False);
  end;
  if not RequestPathFromReply(Reply, ReqPath) then
  begin
    Log('CreateSession reply without request path');
    Exit(False);
  end;
  if not WaitResponse(ReqPath, Code, Handle, Shortcuts, 15000) or (Code <> 0) then
  begin
    Log('session wait failed, code=' + IntToStr(Code));
    Exit(False);
  end;
  if Handle = '' then
  begin
    Log('session response without handle');
    Exit(False);
  end;
  FSession := Handle;
  FNeedsReconcile := True; // persisted shortcuts are loaded by CreateSession
  Log('session=' + Handle);
  Result := True;
end;

function TPortalHotkeyEngine.ListPersisted(out Items: TPortalShortcutArray): Boolean;
var
  Reply: PDBusMessage;
  ReqPath, Dummy: String;
  Code: Cardinal;
begin
  Result := False;
  SetLength(Items, 0);
  if FSession = '' then
    Exit;
  FBuildToken := NewToken(FAppToken);
  FBuildSession := FSession;
  if not CallPortal(PORTAL_PATH, 'ListShortcuts', BuildListArgs, Reply) then
  begin
    Log('ListShortcuts call failed: ' + FLastError);
    Exit;
  end;
  if not RequestPathFromReply(Reply, ReqPath) then
    Exit;
  if not WaitResponse(ReqPath, Code, Dummy, Items, 15000) or (Code <> 0) then
  begin
    Log('ListShortcuts response failed, code=' + IntToStr(Code));
    SetLength(Items, 0);
    Exit;
  end;
  Result := True;
end;

function TPortalHotkeyEngine.BindSet(const Items: TPortalShortcutArray;
  out Shortcuts: TPortalShortcutArray): Boolean;
var
  Reply: PDBusMessage;
  ReqPath, Dummy: String;
  Code: Cardinal;
begin
  Result := False;
  SetLength(Shortcuts, 0);
  if not EnsureSession then
    Exit;
  FBuildToken := NewToken(FAppToken);
  FBuildSession := FSession;
  FBuildItems := Items;
  if not CallPortal(PORTAL_PATH, 'BindShortcuts', BuildBindArgs, Reply) then
  begin
    Log('BindShortcuts call failed: ' + FLastError);
    Exit;
  end;
  if not RequestPathFromReply(Reply, ReqPath) then
  begin
    Log('BindShortcuts reply without request path');
    Exit;
  end;
  if not WaitResponse(ReqPath, Code, Dummy, Shortcuts, 45000) then
  begin
    Log('BindShortcuts response timeout');
    Exit;
  end;
  Log('bind response code=' + IntToStr(Code) + ' n=' + IntToStr(Length(Shortcuts)));
  Result := Code = 0;
end;

function TPortalHotkeyEngine.SyncIncremental(out Shortcuts: TPortalShortcutArray): Boolean;
var
  Items, Persisted, Dummy: TPortalShortcutArray;
  I: Integer;
  Dirty: Boolean;
begin
  // Re-bind the whole desired set on the live session. Shortcuts removed
  // from the set are dropped by the portal because they were registered in
  // this very session; a fresh session would only load them and be unable
  // to unregister them (KGlobalAccel::removeAllShortcuts is a no-op then).
  SetLength(Shortcuts, 0);
  if not EnsureSession then
    Exit(False);

  if FNeedsReconcile then
  begin
    FNeedsReconcile := False;
    Items := SnapshotDesired;
    if ListPersisted(Persisted) then
    begin
      // CreateSession loaded the shortcuts persisted by previous runs but
      // did not register them, so they cannot be unregistered directly.
      // Re-binding them first registers them, which lets the following bind
      // drop the ones we no longer want (e.g. leftovers of older versions).
      Dirty := False;
      for I := 0 to High(Persisted) do
        if IndexOfShortcut(Items, Persisted[I].Id) < 0 then
        begin
          Dirty := True;
          Break;
        end;
      if Dirty then
      begin
        Log('reconcile: ' + IntToStr(Length(Persisted)) + ' persisted shortcut(s)');
        if not BindSet(Persisted, Dummy) then
          Log('reconcile: re-bind failed: ' + FLastError);
      end;
    end;
  end;

  Items := SnapshotDesired;
  Result := BindSet(Items, Shortcuts);
  if Result then
    SetTriggers(Shortcuts);
end;

function TPortalHotkeyEngine.Rebuild(out Shortcuts: TPortalShortcutArray): Boolean;
var
  Items: TPortalShortcutArray;
  Attempt: Integer;
begin
  // Spec-compliant strategy: a fresh session for every change, so that
  // BindShortcuts is only called once per session.
  Result := False;
  SetLength(Shortcuts, 0);
  Items := SnapshotDesired;
  for Attempt := 1 to 2 do
  begin
    CloseSession;
    ClearTriggers;
    if BindSet(Items, Shortcuts) then
    begin
      SetTriggers(Shortcuts);
      if Length(Items) = 0 then
        CloseSession; // nothing left, no session to keep alive
      Exit(True);
    end;
    Log('rebind attempt ' + IntToStr(Attempt) + ' failed: ' + FLastError);
  end;
end;

function TPortalHotkeyEngine.SyncBindings(out Shortcuts: TPortalShortcutArray): Boolean;
begin
  if ResolvedBindStrategy = pbsSpecCompliant then
    Result := Rebuild(Shortcuts)
  else
    Result := SyncIncremental(Shortcuts);
end;

procedure TPortalHotkeyEngine.CloseSession;
var
  Msg, Reply: PDBusMessage;
  Err: DBusError;
begin
  if (FSession = '') or (FConn = nil) then
  begin
    FSession := '';
    Exit;
  end;
  Msg := dbus_message_new_method_call(PORTAL_BUS, PChar(UTF8String(FSession)),
    SESSION_IFACE, 'Close');
  if Msg = nil then
  begin
    FSession := '';
    Exit;
  end;
  try
    dbus_error_init(@Err);
    Reply := dbus_connection_send_with_reply_and_block(FConn, Msg, 2000, @Err);
    if Ok(dbus_error_is_set(@Err)) then
      dbus_error_free(@Err);
    if Reply <> nil then
      dbus_message_unref(Reply);
  finally
    dbus_message_unref(Msg);
  end;
  FSession := '';
end;

procedure TPortalHotkeyEngine.AddMatch;
var
  Err: DBusError;
begin
  if FMatchAdded or (FConn = nil) then
    Exit;
  dbus_error_init(@Err);
  dbus_bus_add_match(FConn, PChar(MATCH_RULE), @Err);
  if Ok(dbus_error_is_set(@Err)) then
  begin
    Log('AddMatch failed');
    dbus_error_free(@Err);
  end
  else
    FMatchAdded := True;
end;

procedure TPortalHotkeyEngine.RemoveMatch;
var
  Err: DBusError;
begin
  if not FMatchAdded or (FConn = nil) then
    Exit;
  dbus_error_init(@Err);
  dbus_bus_remove_match(FConn, PChar(MATCH_RULE), @Err);
  if Ok(dbus_error_is_set(@Err)) then
    dbus_error_free(@Err);
  FMatchAdded := False;
end;

function TPortalHotkeyEngine.NameHasOwner(const Name: String): Boolean;
var
  Reply: PDBusMessage;
  Iter: DBusMessageIter;
  B: LongBool;
begin
  Result := False;
  if not Connect then
    Exit;
  FBuildName := Name;
  if not CallDBus('NameHasOwner', BuildOwnerArgs, Reply) then
    Exit;
  try
    if Ok(dbus_message_iter_init(Reply, @Iter))
      and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_BOOLEAN) then
    begin
      B := False;
      dbus_message_iter_get_basic(@Iter, @B);
      Result := B <> False;
    end;
  finally
    dbus_message_unref(Reply);
  end;
end;

function TPortalHotkeyEngine.HasPortal: Boolean;
begin
  Result := Connect and NameHasOwner(PORTAL_BUS);
end;

function TPortalHotkeyEngine.SnapshotDesired: TPortalShortcutArray;
var
  I: Integer;
  H: TShortcutEx;
begin
  Result := nil;
  FLock.Acquire;
  try
    SetLength(Result, FDesired.Count);
    for I := 0 to FDesired.Count - 1 do
    begin
      H := TShortcutEx(FDesired.Objects[I]);
      Result[I].Id := FDesired[I];
      Result[I].Trigger := PortalAccelerator(H.Key, H.ShiftState);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.SetTriggers(const Shortcuts: TPortalShortcutArray);
var
  I: Integer;
begin
  FLock.Acquire;
  try
    FTriggers.Clear;
    for I := 0 to High(Shortcuts) do
      FTriggers.Values[Shortcuts[I].Id] := Shortcuts[I].Trigger;
  finally
    FLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.ClearTriggers;
begin
  FLock.Acquire;
  try
    FTriggers.Clear;
  finally
    FLock.Release;
  end;
end;

function TPortalHotkeyEngine.TriggerOf(const Id: String): String;
begin
  FLock.Acquire;
  try
    Result := FTriggers.Values[Id];
  finally
    FLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.RollbackDesired(const Id: String);
var
  I: Integer;
begin
  FLock.Acquire;
  try
    I := FDesired.IndexOf(Id);
    if I >= 0 then
      FDesired.Delete(I);
  finally
    FLock.Release;
  end;
end;

function TPortalHotkeyEngine.FindShortcut(const Id: String): TShortcutEx;
var
  I: Integer;
begin
  Result := nil;
  if Id = '' then
    Exit;
  FLock.Acquire;
  try
    I := FDesired.IndexOf(Id);
    if I > -1 then
      Result := TShortcutEx(FDesired.Objects[I]);
  finally
    FLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.FireActivation(const Id: String);
var
  H: TShortcutEx;
begin
  H := FindShortcut(Id);
  if (H <> nil) and Assigned(H.Notify) then
    H.Notify(FManager, H);
end;

procedure TPortalHotkeyEngine.QueueActivation(const Id: String);
begin
  // Never call back into user code from inside dbus_connection_dispatch:
  // libdbus forbids re-entrant dispatch and the callback may unregister the
  // very shortcut being delivered. Queue it for the next ProcessPending.
  FLock.Acquire;
  try
    if FPendingActivations.IndexOf(Id) < 0 then
      FPendingActivations.Add(Id);
  finally
    FLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.DrainActivations;
var
  Ids: TStringList;
  I: Integer;
begin
  // Take a snapshot under the lock, then invoke user callbacks outside it.
  FLock.Acquire;
  try
    if FPendingActivations.Count = 0 then
      Exit;
    Ids := TStringList.Create;
    try
      Ids.Assign(FPendingActivations);
      FPendingActivations.Clear;
    except
      Ids.Free;
      raise;
    end;
  finally
    FLock.Release;
  end;
  try
    for I := 0 to Ids.Count - 1 do
      FireActivation(Ids[I]);
  finally
    Ids.Free;
  end;
end;

procedure TPortalHotkeyEngine.ProcessPending;
begin
  if FConn = nil then
    Exit;
  // Non-blocking: returns immediately after dispatching what is already
  // queued, so it is safe to call from a main-loop timer/idle handler.
  dbus_connection_read_write_dispatch(FConn, 0);
  DrainActivations;
end;

procedure TPortalHotkeyEngine.HandleActivated(Msg: PDBusMessage);
var
  Iter: DBusMessageIter;
  Session, Id: String;
  P: PChar;
  Known: Boolean;
begin
  // Activated(o session_handle, s shortcut_id, t timestamp, a{sv} options)
  Session := '';
  Id := '';
  if not Ok(dbus_message_iter_init(Msg, @Iter)) then
    Exit;
  if dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_OBJECT_PATH then
  begin
    P := nil;
    dbus_message_iter_get_basic(@Iter, @P);
    if P <> nil then
      Session := String(P);
  end;
  if not Ok(dbus_message_iter_next(@Iter)) then
    Exit;
  if dbus_message_iter_get_arg_type(@Iter) <> DBUS_TYPE_STRING then
    Exit;
  P := nil;
  dbus_message_iter_get_basic(@Iter, @P);
  if P <> nil then
    Id := String(P);
  if Id = '' then
    Exit;

  // A general match may deliver events from another session; only our own
  // active session may trigger a local shortcut.
  Known := False;
  FLock.Acquire;
  try
    if (FSession <> '') and (Session = FSession) and (FDesired.IndexOf(Id) >= 0) then
      Known := True;
  finally
    FLock.Release;
  end;
  if not Known then
    Exit;

  Log('activated id=' + Id);
  QueueActivation(Id);
end;

procedure TPortalHotkeyEngine.HandleShortcutsChanged(Msg: PDBusMessage);
var
  Iter: DBusMessageIter;
  Session: String;
  P: PChar;
  Shortcuts: TPortalShortcutArray;
  Own: Boolean;
begin
  // ShortcutsChanged(o session_handle, a(sa{sv}) shortcuts)
  Session := '';
  Own := False;
  if Ok(dbus_message_iter_init(Msg, @Iter))
    and (dbus_message_iter_get_arg_type(@Iter) = DBUS_TYPE_OBJECT_PATH) then
  begin
    P := nil;
    dbus_message_iter_get_basic(@Iter, @P);
    if P <> nil then
      Session := String(P);
    FLock.Acquire;
    try
      Own := (FSession <> '') and (Session = FSession);
    finally
      FLock.Release;
    end;
  end;
  if not Own then
    Exit;
  if not Ok(dbus_message_iter_next(@Iter)) then
    Exit;
  SetLength(Shortcuts, 0);
  ReadShortcutList(@Iter, Shortcuts);
  SetTriggers(Shortcuts);
  Log('ShortcutsChanged: ' + IntToStr(Length(Shortcuts)) + ' shortcut(s)');
end;

function TPortalHotkeyEngine.HandleMessage(Msg: PDBusMessage): Boolean;
begin
  Result := False;
  if Ok(dbus_message_is_signal(Msg, REQUEST_IFACE, 'Response')) then
  begin
    HandleResponse(Msg);
    Result := True;
  end
  else if Ok(dbus_message_is_signal(Msg, PORTAL_IFACE, 'Activated')) then
  begin
    HandleActivated(Msg);
    Result := True;
  end
  else if Ok(dbus_message_is_signal(Msg, PORTAL_IFACE, 'ShortcutsChanged')) then
  begin
    HandleShortcutsChanged(Msg);
    Result := True;
  end;
end;

function TPortalHotkeyEngine.RegisterShortcut(Shortcut: TShortcutEx;
  out Trigger: String): Boolean;
var
  Id, Acc: String;
  Shortcuts: TPortalShortcutArray;
  I: Integer;
  NewEntry: Boolean;
begin
  Result := False;
  Trigger := '';
  if Shortcut = nil then
    Exit;
  Acc := PortalAccelerator(Shortcut.Key, Shortcut.ShiftState);
  if Acc = '' then
    Exit; // unsupported key: the portal cannot express it

  Id := ShortcutIdOf(Shortcut);
  FOpLock.Acquire;
  try
    FLock.Acquire;
    try
      I := FDesired.IndexOf(Id);
      NewEntry := I < 0;
      if NewEntry then
        FDesired.AddObject(Id, Shortcut)
      else
        FDesired.Objects[I] := Shortcut;
    finally
      FLock.Release;
    end;

    if not NewEntry then
    begin
      Trigger := TriggerOf(Id);
      Exit(True); // already part of the active set
    end;

    if not SyncBindings(Shortcuts) then
    begin
      RollbackDesired(Id);
      Exit;
    end;

    I := IndexOfShortcut(Shortcuts, Id);
    if (I < 0) or (Shortcuts[I].Trigger = '') then
    begin
      // The portal refused it or the user declined: forget it so the base
      // manager can free the object and a retry starts from a clean set.
      RollbackDesired(Id);
      Exit;
    end;

    Trigger := Shortcuts[I].Trigger;
    Result := True;
  finally
    FOpLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.Reset;
var
  Dummy: TPortalShortcutArray;
  Attempt: Integer;
begin
  FOpLock.Acquire;
  try
    FLock.Acquire;
    try
      FDesired.Clear;
    finally
      FLock.Release;
    end;
    // Binding the empty set unregisters the shortcuts that are live in this
    // session; closing alone would leave them persisted in KGlobalAccel.
    // Retry once and only close once the portal confirmed the empty bind,
    // otherwise the session would be lost with its shortcuts still grabbed.
    if FSession <> '' then
      for Attempt := 1 to 2 do
      begin
        if SyncBindings(Dummy) then
          Break;
        Log('Reset: unbind attempt ' + IntToStr(Attempt) + ' failed: ' + FLastError);
        CloseSession; // start from a clean session on the retry
      end;
    CloseSession;
    ClearTriggers;
  finally
    FOpLock.Release;
  end;
end;

function TPortalHotkeyEngine.UnregisterShortcut(Shortcut: TShortcutEx): Boolean;
var
  Id: String;
  I: Integer;
  Shortcuts: TPortalShortcutArray;
begin
  Result := False;
  if Shortcut = nil then
    Exit;
  Id := ShortcutIdOf(Shortcut);
  FOpLock.Acquire;
  try
    FLock.Acquire;
    try
      I := FDesired.IndexOf(Id);
      if I < 0 then
        Exit(True); // not part of the desired set
      FDesired.Delete(I);
    finally
      FLock.Release;
    end;

    // Bind the reduced set on the live session: the removed shortcut was
    // registered here, so KGlobalAccel really drops it. When nothing is left
    // the empty bind unregisters everything and the session is closed.
    if not SyncBindings(Shortcuts) then
    begin
      // Keep tracking the shortcut: the portal may still hold it and the
      // base manager must not free its object.
      FLock.Acquire;
      try
        FDesired.AddObject(Id, Shortcut);
      finally
        FLock.Release;
      end;
      Exit;
    end;
    if FDesired.Count = 0 then
      CloseSession;
    Result := True;
  finally
    FOpLock.Release;
  end;
end;

{$ENDIF}

end.
