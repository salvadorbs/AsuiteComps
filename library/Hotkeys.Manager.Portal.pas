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
The backend keeps a *desired* set (what the application asked for) and applies
it with a fresh session for every change, so BindShortcuts is called once per
session as the interface documentation requires. A compositor may only remove
an action while it is registered in the current session, so a shortcut
persisted by an older run can survive; that limit is documented rather than
worked around with a desktop-specific branch.

Reading the portal
------------------
QueryById / QueryByActionKey / QueryByTrigger / VerifyRegistered perform a
read-only ListShortcuts and never bind or unbind. They answer about the
shortcuts the portal exposes to this application/session, not about every
global shortcut of the desktop. The host supplies a stable ActionId for each
action (see TShortcutEx.ActionId) so the same binding survives a shortcut
change and a restart; Tag stays as the compatibility fallback.

The session handle token (AppToken) only namespaces the session, the
request handles and the shortcut ids. It does NOT necessarily select the
component shown in the desktop's shortcut settings: the component is the
*application id* the portal derives from the process (e.g. the IDE or terminal
it was started from), or "token_<AppToken>" when no application id could be
determined.

Portal accelerator format (parsed by compositors):
  CTRL+ALT+F7   (uppercase modifiers joined by '+', XKB key names)
}

unit Hotkeys.Manager.Portal;

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, LCLType,
  Hotkeys.ShortcutEx,
  Hotkeys.Manager
  {$IFDEF UNIX}
  , syncobjs,
  dbus
  {$ENDIF}
  ;

{ Converts Key + ShiftState to a portal accelerator ('' if unsupported).
  At least one modifier is required, like the THotKey control enforces.
  Pure logic, available on every platform (for tests). }
function PortalAccelerator(Key: Word; Shift: TShiftState): String;

{ Canonical, comparable form of a portal accelerator or of a trigger
  description returned by the portal: modifiers uppercased and put in a fixed
  order (CTRL, SHIFT, ALT, ALTGR, LOGO), key name uppercased. The portal only
  returns a human-readable trigger_description, not the raw accelerator, so
  comparisons must be best-effort; matching by action id stays authoritative.
  Returns '' when the input is not comparable (no key, or no recognised
  modifier, e.g. a localised description). Pure logic, available on every
  platform (for tests). }
function NormalizePortalAccelerator(const Acc: String): String;

{ Tri-state comparison of two accelerators/trigger descriptions.
    ptmUnknown - at least one side is not a comparable accelerator;
    ptmNo      - both are comparable and differ;
    ptmYes     - both are comparable and equal. }
function ComparePortalTriggers(const A, B: String): THotkeyTriggerMatch;

{ True when the string can be interpreted as a comparable accelerator. }
function PortalTriggerParseable(const Acc: String): Boolean;

type
  TPortalShortcutInfo = record
    Id: String;
    ActionKey: String;    // host-level key decoded from Id ('' if not ours)
    Trigger: String;      // accelerator we ask for, or trigger_description we got
    Description: String;  // user-readable text (required by some backends when re-binding)
  end;
  TPortalShortcutArray = array of TPortalShortcutInfo;

{ Every entry whose trigger matches, compared in canonical form. Pure logic,
  available on every platform (for tests). }
function PortalShortcutsByTrigger(const Shortcuts: TPortalShortcutArray;
  const Trigger: String): TPortalShortcutArray;

{$IFDEF UNIX}

type
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
    FUpdateLevel: Integer;
    FUpdateDirty: Boolean;
    FUpdateSnapshot: TStringList; // id -> TShortcutEx at the outermost BeginUpdate
    FDesired: TStringList;   // shortcut id -> TShortcutEx (not owned)
    FTriggers: TStringList;  // shortcut id -> trigger assigned by the portal
    FPending: TStringList;   // request path -> TPortalResponse (owned)
    FPendingActivations: TStringList; // shortcut ids waiting to be delivered
    FPendingTriggers: TStringList; // Trigger in Strings[], Tag in Objects[]
    FPendingTriggerActions: TStringList; // ActionId, parallel to FPendingTriggers
    FTokenSeq: Integer;
    FBuildToken: String;
    FBuildSession: String;
    FBuildName: String;
    FBuildItems: TPortalShortcutArray;
    FLastError: String;
    FVerbose: Boolean;

    procedure SetAppToken(const Value: String);
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
    function Rebuild(out Shortcuts: TPortalShortcutArray): Boolean;
    procedure CloseSession;
    procedure AddMatch;
    procedure RemoveMatch;
    function NameHasOwner(const Name: String): Boolean;
    procedure Log(const Msg: String);
    function FindShortcut(const Id: String): TShortcutEx;
    function SnapshotDesired: TPortalShortcutArray;
    { Reads the shortcuts currently exposed by the portal. Creates a session
      when none is active and closes it again if it was opened only for the
      query and no binding is active. Returns False (=> hqsUnknown) when the
      portal cannot answer. }
    function FetchShortcuts(out Items: TPortalShortcutArray): Boolean;
    procedure SetTriggers(const Shortcuts: TPortalShortcutArray);
    procedure ClearTriggers;
    procedure DrainTriggers;
    function TriggerOf(const Id: String): String;
    procedure RollbackDesired(const Id: String);
    procedure RollbackUpdate;
    procedure QueueActivation(const Id: String);
    procedure DrainActivations;
    procedure FireActivation(const Id: String);
    function HandleMessage(Msg: PDBusMessage): Boolean;
  public
    constructor Create(AManager: TBaseHotkeyManager);
    destructor Destroy; override;

    function HasPortal: Boolean;
    { Binds a shortcut through the portal. On success Trigger holds the
      accelerator assigned by the portal. The TShortcutEx instance is only
      referenced (not owned): the caller must keep it alive until it is
      unregistered. }
    function RegisterShortcut(Shortcut: TShortcutEx; out Trigger: String): Boolean;
    function UnregisterShortcut(Shortcut: TShortcutEx): Boolean;
    { Defers the actual BindShortcuts until the matching EndUpdate, so a
      burst of changes is applied with a single bind. While an update is open
      RegisterShortcut/UnregisterShortcut only update the desired set. }
    procedure BeginUpdate;
    function EndUpdate: Boolean;
    { Dispatches any pending D-Bus message and delivers queued activations.
      Must be called periodically from the thread that owns the engine (the
      application main thread, e.g. from an idle handler or a timer). }
    procedure ProcessPending;
    { Drops every desired shortcut and unbinds it from the live session, then
      closes the session. This is the *intentional* removal of the whole set:
      it may delete the user's persistent preferences. }
    procedure Reset;
    { Stops serving shortcuts: drops the desired set and closes the session
      WITHOUT unbinding anything, so a persistent desktop preference survives
      the application exit. Used on normal teardown. }
    procedure Shutdown;

    property SessionHandle: String read FSession;
    property LastError: String read FLastError;
    property Verbose: Boolean read FVerbose write FVerbose;
    { Namespace for the session handle, request handles and shortcut ids.
      Set it before the first registration. }
    property AppToken: String read FAppToken write SetAppToken;
    { The stable portal id of a shortcut: derived from its ActionId (the
      action) when present, otherwise from its Tag, otherwise from the key, so
      changing the combination keeps the same binding. The id is injective in
      the host key (see PortalIdFor). }
    function ShortcutIdOf(Shortcut: TShortcutEx): String;

    { Maps a host-level action key (an ActionId string, or IntToStr(Tag)) to
      the portal id used on the bus, applying the same namespacing as
      ShortcutIdOf. The encoding is reversible and collision-free, so distinct
      keys never map to the same id. Returns '' for an empty key. }
    function PortalIdFor(const ActionKey: String): String;
    { Inverse of PortalIdFor: the host-level key encoded in a portal id, or
      the id itself when it is not namespaced by this engine. }
    function HostKeyOf(const PortalId: String): String;

    { Read-only queries against the portal's persistent shortcut store.

      QueryById/QueryByActionKey ask about one action and return its reported
      trigger. QueryByTrigger asks which actions use a key combination and
      returns every match (the portal does not guarantee uniqueness).
      VerifyRegistered combines both: it reports whether the action is present
      and whether its trigger matches the requested one, so a shortcut that was
      reassigned by the user is not mistaken for a missing one. TriggerMatch is
      ptmUnknown when the portal returned a trigger description that cannot be
      compared.

      All three return hqsUnknown when the portal cannot be queried, hqsAbsent
      when it answered and there is no match, hqsPresent otherwise. They never
      bind or unbind anything, and they only answer about the shortcuts the
      portal exposes to this application/session. }
    function QueryById(const Id: String;
      out Info: TPortalShortcutInfo): THotkeyQueryStatus;
    function QueryByActionKey(const ActionKey: String;
      out Info: TPortalShortcutInfo): THotkeyQueryStatus;
    function QueryByTrigger(const Trigger: String;
      out Matches: TPortalShortcutArray): THotkeyQueryStatus;
    function VerifyRegistered(const Id, RequestedTrigger: String;
      out Info: TPortalShortcutInfo;
      out TriggerMatch: THotkeyTriggerMatch): THotkeyQueryStatus;
  end;

{ True when the session bus answers and the portal owns its name }
function PortalAvailable: Boolean;

{ Low-level parsers for the portal shortcut list and the response results
  dictionary. Exposed so the malformed-reply handling can be tested
  deterministically against real D-Bus messages. Both return False when the
  structure is malformed; a partial read is never reported as success. }
function ReadShortcutList(Iter: PDBusMessageIter;
  out Shortcuts: TPortalShortcutArray): Boolean;
function ReadResultsDict(Iter: PDBusMessageIter; out SessionHandle: String;
  out Shortcuts: TPortalShortcutArray): Boolean;

{$ENDIF}

implementation

{ PortalAccelerator }

function PortalAccelerator(Key: Word; Shift: TShiftState): String;
var
  Mods, KeyName: String;
begin
  // The portal accelerator format used here does not cover AltGr/Hyper. Rather
  // than emit a combination that the compositor would interpret differently,
  // report the shortcut as unsupported.
  if (ssAltGr in Shift) or (ssHyper in Shift) then
    Exit('');

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

function NormalizePortalAccelerator(const Acc: String): String;
var
  P, Start: Integer;
  Tok, Key, Mods: String;
  HasCtrl, HasShift, HasAlt, HasAltGr, HasLogo: Boolean;
begin
  Result := '';
  if Acc = '' then
    Exit;

  Key := '';
  HasCtrl := False;
  HasShift := False;
  HasAlt := False;
  HasAltGr := False;
  HasLogo := False;

  P := 1;
  while P <= Length(Acc) do
  begin
    Start := P;
    while (P <= Length(Acc)) and (Acc[P] <> '+') do
      Inc(P);
    Tok := UpperCase(Trim(Copy(Acc, Start, P - Start)));
    Inc(P); // skip the '+'
    if Tok = '' then
      Continue;
    if (Tok = 'CTRL') or (Tok = 'CONTROL') then
      HasCtrl := True
    else if Tok = 'SHIFT' then
      HasShift := True
    else if Tok = 'ALT' then
      HasAlt := True
    else if (Tok = 'ALTGR') or (Tok = 'ISO_LEVEL3_SHIFT') or (Tok = 'MODE_SWITCH') then
      HasAltGr := True
    else if (Tok = 'LOGO') or (Tok = 'SUPER') or (Tok = 'META') or (Tok = 'HYPER') then
      HasLogo := True
    else
      Key := Tok; // last non-modifier token wins
  end;

  if Key = '' then
    Exit; // modifiers only: not a usable accelerator
  if not (HasCtrl or HasShift or HasAlt or HasAltGr or HasLogo) then
    Exit; // no recognised modifier: cannot be compared (e.g. localised text)

  Mods := '';
  if HasCtrl then Mods := Mods + 'CTRL+';
  if HasShift then Mods := Mods + 'SHIFT+';
  if HasAlt then Mods := Mods + 'ALT+';
  if HasAltGr then Mods := Mods + 'ALTGR+';
  if HasLogo then Mods := Mods + 'LOGO+';
  Result := Mods + Key;
end;

function ComparePortalTriggers(const A, B: String): THotkeyTriggerMatch;
var
  NA, NB: String;
begin
  NA := NormalizePortalAccelerator(A);
  NB := NormalizePortalAccelerator(B);
  if (NA = '') or (NB = '') then
    Exit(ptmUnknown);
  if NA = NB then
    Result := ptmYes
  else
    Result := ptmNo;
end;

function PortalTriggerParseable(const Acc: String): Boolean;
begin
  Result := NormalizePortalAccelerator(Acc) <> '';
end;

function PortalShortcutsByTrigger(const Shortcuts: TPortalShortcutArray;
  const Trigger: String): TPortalShortcutArray;
var
  I: Integer;
begin
  Result := nil;
  if Trigger = '' then
    Exit;
  for I := 0 to High(Shortcuts) do
    if ComparePortalTriggers(Shortcuts[I].Trigger, Trigger) = ptmYes then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Shortcuts[I];
    end;
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

// Reversible, collision-free encoding of a host-level action key into an
// object-path-safe string: [A-Za-z0-9] stay as-is, every other byte (including
// '_', which is the escape introducer) becomes '_' + two uppercase hex digits.
// Distinct keys therefore never map to the same id, and the key can be decoded
// back (see DecodeActionKey). Unlike SanitizeToken this is NOT lossy.
function EncodeActionKey(const Value: String): String;
var
  U: UTF8String;
  I: Integer;
  C: Byte;
begin
  Result := '';
  U := UTF8String(Value);
  for I := 1 to Length(U) do
  begin
    C := Byte(U[I]);
    if ((C >= Ord('a')) and (C <= Ord('z')))
      or ((C >= Ord('A')) and (C <= Ord('Z')))
      or ((C >= Ord('0')) and (C <= Ord('9'))) then
      Result := Result + Char(C)
    else
      Result := Result + '_' + IntToHex(C, 2);
  end;
end;

function DecodeActionKey(const Value: String): String;
var
  I, B: Integer;
  U: UTF8String;
begin
  U := '';
  I := 1;
  while I <= Length(Value) do
  begin
    if (Value[I] = '_') and (I + 2 <= Length(Value)) then
    begin
      B := StrToIntDef('$' + Copy(Value, I + 1, 2), -1);
      if B >= 0 then
      begin
        U := U + AnsiChar(B);
        Inc(I, 3);
        Continue;
      end;
    end;
    U := U + AnsiChar(Ord(Value[I]));
    Inc(I);
  end;
  Result := String(U); // UTF-8 -> UnicodeString
end;

// The desktop name is never used to change behaviour: the backend applies one
// explicit, spec-compliant policy for every desktop.
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
// and by the ShortcutsChanged signal. Returns False when the structure is
// malformed (a partial read must not be mistaken for an empty/absent list).
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
  while dbus_message_iter_get_arg_type(@Items) <> DBUS_TYPE_INVALID do
  begin
    if dbus_message_iter_get_arg_type(@Items) <> DBUS_TYPE_STRUCT then
      Exit; // malformed
    dbus_message_iter_recurse(@Items, @Item);
    if not ReadBasicString(@Item, DBUS_TYPE_STRING, Id) then
      Exit; // malformed: no shortcut id
    Trigger := '';
    Desc := '';
    if not Ok(dbus_message_iter_next(@Item)) then
      Exit; // malformed: no options dict
    if dbus_message_iter_get_arg_type(@Item) = DBUS_TYPE_ARRAY then
    begin
      dbus_message_iter_recurse(@Item, @Opts);
      while dbus_message_iter_get_arg_type(@Opts) <> DBUS_TYPE_INVALID do
      begin
        if dbus_message_iter_get_arg_type(@Opts) <> DBUS_TYPE_DICT_ENTRY then
          Exit; // malformed options dict
        dbus_message_iter_recurse(@Opts, @Opt);
        if not ReadBasicString(@Opt, DBUS_TYPE_STRING, S) then
          Exit; // malformed option key
        if not Ok(dbus_message_iter_next(@Opt)) then
          Exit; // malformed option value
        if S = 'trigger_description' then
          ReadVariantString(@Opt, Trigger)
        else if S = 'description' then
          ReadVariantString(@Opt, Desc);
        if not Ok(dbus_message_iter_next(@Opts)) then
          Break;
      end;
    end;
    SetLength(Shortcuts, Length(Shortcuts) + 1);
    Shortcuts[High(Shortcuts)].Id := Id;
    Shortcuts[High(Shortcuts)].Trigger := Trigger;
    Shortcuts[High(Shortcuts)].Description := Desc;
    if not Ok(dbus_message_iter_next(@Items)) then
      Break;
  end;
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
  while dbus_message_iter_get_arg_type(@Arr) <> DBUS_TYPE_INVALID do
  begin
    if dbus_message_iter_get_arg_type(@Arr) <> DBUS_TYPE_DICT_ENTRY then
      Exit; // malformed results dict
    dbus_message_iter_recurse(@Arr, @Entry);
    if not ReadBasicString(@Entry, DBUS_TYPE_STRING, Key) then
      Exit; // malformed key
    if not Ok(dbus_message_iter_next(@Entry)) then
      Exit; // malformed value
    if Key = 'session_handle' then
      ReadVariantString(@Entry, SessionHandle)
    else if (Key = 'shortcuts')
      and (dbus_message_iter_get_arg_type(@Entry) = DBUS_TYPE_VARIANT) then
    begin
      dbus_message_iter_recurse(@Entry, @VarIter);
      if not ReadShortcutList(@VarIter, Shortcuts) then
        Exit; // malformed shortcut list
    end;
    if not Ok(dbus_message_iter_next(@Arr)) then
      Break;
  end;
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
  FPendingTriggers := TStringList.Create;
  FPendingTriggerActions := TStringList.Create;
  FUpdateSnapshot := TStringList.Create;
  FAppToken := 'hotkey';
  FUpdateLevel := 0;
  dbus_threads_init_default();
end;

procedure TPortalHotkeyEngine.SetAppToken(const Value: String);
begin
  // The token namespaces the session handle, the request handles and the
  // shortcut ids. Changing it after a registration would desynchronize the
  // engine from the portal, so it is frozen once anything was registered.
  if (FSession <> '') or (FDesired.Count > 0) then
  begin
    Log('AppToken change ignored: engine already in use');
    Exit;
  end;
  FAppToken := SanitizeToken(Value);
end;

function TPortalHotkeyEngine.PortalIdFor(const ActionKey: String): String;
begin
  if ActionKey = '' then
    Exit('');
  Result := FAppToken + '_' + EncodeActionKey(ActionKey);
end;

function TPortalHotkeyEngine.HostKeyOf(const PortalId: String): String;
var
  Prefix: String;
begin
  Prefix := FAppToken + '_';
  if Copy(PortalId, 1, Length(Prefix)) = Prefix then
    Result := DecodeActionKey(Copy(PortalId, Length(Prefix) + 1, MaxInt))
  else
    Result := PortalId; // not namespaced by this engine
end;

function TPortalHotkeyEngine.ShortcutIdOf(Shortcut: TShortcutEx): String;
begin
  // The portal binding is persistent, so the id must identify the *action*
  // and not the key combination: changing the shortcut must keep the same id
  // or the desktop would treat it as a new action and lose the user's
  // configuration. A host-supplied ActionId is the most stable identity; Tag
  // (a runtime callback value) stays as the compatibility fallback and
  // anonymous registrations fall back to the key. All three go through the
  // injective PortalIdFor, so distinct actions never collide.
  if Shortcut.ActionId <> '' then
    Result := PortalIdFor(Shortcut.ActionId)
  else if Shortcut.Tag <> -1 then
    Result := PortalIdFor(IntToStr(Shortcut.Tag))
  else
    Result := PortalIdFor('key:' + IntToStr(Shortcut.SimpleShortcut));
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
  FreeAndNil(FUpdateSnapshot);
  FreeAndNil(FPendingTriggerActions);
  FreeAndNil(FPendingTriggers);
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
      // built from the token (a backend ignores it for shortcuts it already knows).
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
  FLastError := '';
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
  FLastError := '';
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
    begin
      if not ReadResultsDict(@Iter, Session, Shortcuts) then
      begin
        // A malformed results dict is an error, not a valid empty reply.
        Code := 2;
        Session := '';
        SetLength(Shortcuts, 0);
      end;
    end;
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

function TPortalHotkeyEngine.FetchShortcuts(out Items: TPortalShortcutArray): Boolean;
var
  OwnSession: Boolean;
begin
  SetLength(Items, 0);
  OwnSession := FSession = '';
  if not EnsureSession then
    Exit(False);
  try
    Result := ListPersisted(Items);
  finally
    // A session opened only to answer a query must not outlive it when no
    // binding is active; a session that already serves bindings is kept.
    if OwnSession and (FDesired.Count = 0) then
      CloseSession;
  end;
end;

function TPortalHotkeyEngine.QueryById(const Id: String;
  out Info: TPortalShortcutInfo): THotkeyQueryStatus;
var
  Items: TPortalShortcutArray;
  I: Integer;
begin
  Info.Id := '';
  Info.ActionKey := '';
  Info.Trigger := '';
  Info.Description := '';
  if Id = '' then
    Exit(hqsUnknown);
  if not FetchShortcuts(Items) then
    Exit(hqsUnknown);
  I := IndexOfShortcut(Items, Id);
  if I < 0 then
    Exit(hqsAbsent);
  Info := Items[I];
  Info.ActionKey := HostKeyOf(Info.Id);
  Result := hqsPresent;
end;

function TPortalHotkeyEngine.QueryByActionKey(const ActionKey: String;
  out Info: TPortalShortcutInfo): THotkeyQueryStatus;
begin
  Result := QueryById(PortalIdFor(ActionKey), Info);
end;

function TPortalHotkeyEngine.QueryByTrigger(const Trigger: String;
  out Matches: TPortalShortcutArray): THotkeyQueryStatus;
var
  Items: TPortalShortcutArray;
  I: Integer;
  Unparseable: Boolean;
begin
  SetLength(Matches, 0);
  if Trigger = '' then
    Exit(hqsUnknown);
  if not PortalTriggerParseable(Trigger) then
    Exit(hqsUnknown); // the requested combination itself is not comparable
  if not FetchShortcuts(Items) then
    Exit(hqsUnknown);
  Matches := PortalShortcutsByTrigger(Items, Trigger);
  if Length(Matches) > 0 then
  begin
    for I := 0 to High(Matches) do
      Matches[I].ActionKey := HostKeyOf(Matches[I].Id);
    Exit(hqsPresent);
  end;
  // No match, but if some entry's trigger could not be interpreted the
  // combination may still be registered under an unreadable description.
  Unparseable := False;
  for I := 0 to High(Items) do
    if not PortalTriggerParseable(Items[I].Trigger) then
    begin
      Unparseable := True;
      Break;
    end;
  if Unparseable then
    Result := hqsUnknown
  else
    Result := hqsAbsent;
end;

function TPortalHotkeyEngine.VerifyRegistered(const Id, RequestedTrigger: String;
  out Info: TPortalShortcutInfo;
  out TriggerMatch: THotkeyTriggerMatch): THotkeyQueryStatus;
begin
  TriggerMatch := ptmUnknown;
  Result := QueryById(Id, Info);
  if Result <> hqsPresent then
    Exit;
  // Present but possibly reassigned by the user: report the comparison
  // separately instead of collapsing a different/unreadable trigger into
  // "not registered".
  TriggerMatch := ComparePortalTriggers(Info.Trigger, RequestedTrigger);
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

function TPortalHotkeyEngine.Rebuild(out Shortcuts: TPortalShortcutArray): Boolean;
var
  Items: TPortalShortcutArray;
  Attempt: Integer;
begin
  // A fresh session for every change, so BindShortcuts is called once per
  // session as the portal documents. The persisted shortcuts loaded by
  // CreateSession are not re-bound; a shortcut persisted by an older run that
  // is no longer desired cannot be removed on compositors that only remove
  // in-session (documented limitation).
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
  I, J: Integer;
  Id, Trigger: String;
  H: TShortcutEx;
begin
  FLock.Acquire;
  try
    // Collect the shortcuts whose trigger changed so the manager can be told
    // about the ones the desktop assigned differently from our preference.
    for I := 0 to High(Shortcuts) do
    begin
      Id := Shortcuts[I].Id;
      Trigger := Shortcuts[I].Trigger;
      J := FTriggers.IndexOf(Id);
      if (J < 0) or (FTriggers.ValueFromIndex[J] <> Trigger) then
      begin
        J := FDesired.IndexOf(Id);
        if J >= 0 then
        begin
          H := TShortcutEx(FDesired.Objects[J]);
          FPendingTriggers.AddObject(Trigger, TObject(PtrInt(H.Tag)));
          FPendingTriggerActions.Add(H.ActionId);
        end;
      end;
    end;
    FTriggers.Clear;
    for I := 0 to High(Shortcuts) do
      FTriggers.Values[Shortcuts[I].Id] := Shortcuts[I].Trigger;
  finally
    FLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.DrainTriggers;
var
  Changes, Actions: TStringList;
  I: Integer;
begin
  // Deliver outside the D-Bus dispatch and outside the lock: the callback
  // runs application code.
  FLock.Acquire;
  try
    if FPendingTriggers.Count = 0 then
      Exit;
    Changes := TStringList.Create;
    Actions := TStringList.Create;
    try
      Changes.Assign(FPendingTriggers);
      FPendingTriggers.Clear;
      Actions.Assign(FPendingTriggerActions);
      FPendingTriggerActions.Clear;
    except
      Actions.Free;
      Changes.Free;
      raise;
    end;
  finally
    FLock.Release;
  end;
  try
    if FManager <> nil then
      for I := 0 to Changes.Count - 1 do
        FManager.NotifyTriggerAssigned(Actions[I], PtrInt(Changes.Objects[I]),
          Changes[I]);
  finally
    Actions.Free;
    Changes.Free;
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
  DrainTriggers;
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

    // While a bulk update is open only the desired set changes; the bind is
    // done once in EndUpdate, so a whole list load/refresh is a single
    // BindShortcuts instead of one per item.
    if FUpdateLevel > 0 then
    begin
      FUpdateDirty := True;
      Trigger := Acc;
      Exit(True);
    end;

    if not NewEntry then
    begin
      Trigger := TriggerOf(Id);
      Exit(True); // already part of the active set
    end;

    if not Rebuild(Shortcuts) then
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

procedure TPortalHotkeyEngine.BeginUpdate;
begin
  FOpLock.Acquire;
  try
    if FUpdateLevel = 0 then
    begin
      // Snapshot the desired set so a failed batch can be rolled back.
      FLock.Acquire;
      try
        FUpdateSnapshot.Assign(FDesired);
      finally
        FLock.Release;
      end;
    end;
    Inc(FUpdateLevel);
  finally
    FOpLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.RollbackUpdate;
begin
  // Restore the desired set captured at BeginUpdate and drop the session so
  // the next operation starts from a state that matches the manager.
  FLock.Acquire;
  try
    FDesired.Assign(FUpdateSnapshot);
  finally
    FLock.Release;
  end;
  CloseSession;
end;

function TPortalHotkeyEngine.EndUpdate: Boolean;
var
  Shortcuts, Items: TPortalShortcutArray;
  I, J: Integer;
begin
  FOpLock.Acquire;
  try
    if FUpdateLevel > 0 then
      Dec(FUpdateLevel);
    Result := True;
    if (FUpdateLevel = 0) and FUpdateDirty then
    begin
      FUpdateDirty := False;
      Result := Rebuild(Shortcuts);
      if Result then
      begin
        // A desired shortcut with no trigger was refused or declined by the
        // user; report the whole update as failed so the caller can react.
        Items := SnapshotDesired;
        for I := 0 to High(Items) do
        begin
          J := IndexOfShortcut(Shortcuts, Items[I].Id);
          if (J < 0) or (Shortcuts[J].Trigger = '') then
          begin
            Result := False;
            Break;
          end;
        end;
      end;
      if not Result then
        RollbackUpdate; // keep engine and manager consistent for a retry
    end;
  finally
    FOpLock.Release;
  end;
end;

procedure TPortalHotkeyEngine.Shutdown;
begin
  // Normal teardown: stop serving shortcuts and close the session without
  // unbinding anything, so the desktop keeps the user's preferences.
  FOpLock.Acquire;
  try
    FLock.Acquire;
    try
      FDesired.Clear;
    finally
      FLock.Release;
    end;
    CloseSession;
    ClearTriggers;
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
    // session; closing alone would leave them persisted by the compositor.
    // Retry once and only close once the portal confirmed the empty bind,
    // otherwise the session would be lost with its shortcuts still grabbed.
    if FSession <> '' then
      for Attempt := 1 to 2 do
      begin
        if Rebuild(Dummy) then
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

    // Deferred while a bulk update is open (see RegisterShortcut).
    if FUpdateLevel > 0 then
    begin
      FUpdateDirty := True;
      Exit(True);
    end;

    // Rebuild from the reduced set: the session is recreated and the shortcut
    // is not part of the bind, so the compositor drops it. When nothing is left
    // the empty bind unregisters everything and the session is closed.
    if not Rebuild(Shortcuts) then
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
