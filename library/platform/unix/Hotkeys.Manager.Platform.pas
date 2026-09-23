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

---

With some original code by Codebot (Cross Pascal Library) - https://github.com/sysrpl/Cross.Codebot/

}

unit Hotkeys.Manager.Platform;

{$I ASuiteComps.inc}

{$IFDEF LCLGTK3}
  {$LINKLIB libgdk-3.so.0}
{$ENDIF}

interface

uses
  X, XLib, KeySym, ctypes, Hotkeys.Manager, Hotkeys.ShortcutEx, Hotkeys.Manager.Portal,
  LCLType, Menus, LCLProc, Classes, sysutils, ExtCtrls

  {$IFDEF LCLGTK2}
  , Gdk2, Gdk2x, Gtk2Proc
  {$ENDIF}   

  {$IFDEF LCLGTK3}
  , LazGdk3, LazGLib2
  {$ENDIF}

  {$IFDEF LCLQT5}
  , qt5
  {$ENDIF}

  {$IFDEF LCLQT6}
  , qt6
  {$ENDIF}

  {$IFDEF QT}
  , qtint
  {$ENDIF};

type

  { Backend actually used to grab global hotkeys. Chosen once at
    construction so registration and unregistration always agree, even if
    the environment changes at runtime. }
  TUnixHotkeyBackend = (uhbNone, uhbX11, uhbPortal);

  { TUnixHotkeyManager }

  TUnixHotkeyManager = class(TBaseHotkeyManager)
  private
    FDisplay: PDisplay;
    FPortal: TPortalHotkeyEngine;
    FPortalTimer: TTimer;
    FBackend: TUnixHotkeyBackend;
    FLastTrigger: String;

    {$IFDEF GTK}
    FRoot: PGdkWindow;
    {$ENDIF}

    {$IFDEF QT}
    FQNativeEventFilter: QNativeEventFilter_hookH;

    function FilterKeys(handle: QNativeEventFilter_hookH; eventType: QByteArrayH; message: long): boolean; cdecl;
    {$ENDIF}

    function ShiftToMod(ShiftState: TShiftState): Integer;
    function KeyToSym(Key: Word): TKeySym;
    function SymToKey(Sym: TKeySym): Word;
    function ModToShift(Modifiers: Integer): TShiftState;
    procedure CaptureKey(Display: PDisplay; KeyCode: LongWord; Modifier: LongWord; Window: TWindow);
    procedure ReleaseKey(Display: PDisplay; KeyCode: LongWord; Modifier: LongWord; Window: TWindow);
    procedure AddEventFilter;
    procedure RemoveEventFilter;
    function InternalRegisterShortcut(AShortcut: TShortCutEx; ARegister: Boolean): Boolean;
    function IsWaylandSession: Boolean;
    function SelectBackend: TUnixHotkeyBackend;
    function PortalRegister(AShortcut: TShortCutEx): Boolean;
    function PortalUnregister(AShortcut: TShortCutEx): Boolean;
    procedure PortalTimerTick(Sender: TObject);
  protected
    function DoRegister(Shortcut: TShortCutEx): Boolean; override;
    function DoUnregister(Shortcut: TShortCutEx): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RefreshNotify(Shortcut: TShortCut); override;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; override;

    { Backend selected for this session (mostly for tests/diagnostics) }
    property Backend: TUnixHotkeyBackend read FBackend;
    { Trigger assigned by the portal on the last successful registration }
    property LastPortalTrigger: String read FLastTrigger;
  end;

{$IFDEF GTK}
function FilterKeys(AnyEvent: PXAnyEvent; Event: PGdkEvent; Data: Pointer): TGdkFilterReturn; cdecl;
{$ENDIF}

{$IFDEF LCLGTK3}
function gdk_x11_window_get_xid(AX11Window: PGdkWindow): guint32; cdecl; external;
function gdk_x11_display_get_xdisplay(AX11Display: PGdkDisplay): PDisplay; cdecl; external;
{$ENDIF}

{ Returns the global hotkey manager instance }
function HotkeyManager: TBaseHotkeyManager;

{
  X Key Modifiers:

  Mask        | Value | Key
  ------------+-------+------------
  ShiftMask   |     1 | Shift
  LockMask    |     2 | Caps Lock
  ControlMask |     4 | Ctrl
  Mod1Mask    |     8 | Alt
  Mod2Mask    |    16 | Num Lock
  Mod3Mask    |    32 | Scroll Lock
  Mod4Mask    |    64 | Windows
}

const
  AltMask = Mod1Mask;
  SuperMask = Mod4Mask;
  ModifiersMask = ShiftMask or AltMask or ControlMask or SuperMask;
  CapLock = LockMask;
  NumLock = Mod2Mask;
  NotLock = Integer(not (CapLock or NumLock));

  { Minimal XCB key-press layout, read straight from the native Qt event
    message (xcb_key_press_event_t). Only the fields needed to identify a
    key press are used, so the xcb binding unit is not required. }
  XCB_KEY_PRESS = 2;
  XCB_KEY_DETAIL_OFS = 1;  // keycode
  XCB_KEY_STATE_OFS = 28;  // modifier state (uint16)
  XCB_EVENT_TYPE_MASK = $7F;

implementation

var
  { Set by HookXErrorHandler while a temporary X11 probe is running. }
  HookXError: Boolean = False;

function HookXErrorHandler(para1: PDisplay; para2: PXErrorEvent): cint; cdecl;
begin
  HookXError := True;
  Result := 0;
end;

function InternalFilterKeys(Self: TUnixHotkeyManager; KeyCode: Cardinal; KeyState: Cardinal): Boolean;
var
  I: Integer;
  H: TShortcutEx;
  Sym: TKeySym;
begin
  Sym := XKeycodeToKeysym(Self.FDisplay, KeyCode, 0);
  I := Self.FindHotkey(Self.SymToKey(Sym), Self.ModToShift(KeyState));

  Result := I > -1;
  if Result then
  begin
    H := Self[I];
    if Assigned(H.Notify) then
      H.Notify(Self, H);
  end;
end;

function HotkeyManager: TBaseHotkeyManager;
begin
  if InternalManager = nil then
    InternalManager := TUnixHotkeyManager.Create;

  Result := TBaseHotkeyManager(InternalManager);
end;

function TUnixHotkeyManager.ShiftToMod(ShiftState: TShiftState): Integer;
begin
  Result := 0;
  if ssShift in ShiftState then
    Result := Result or ShiftMask;
  if ssAlt in ShiftState then
    Result := Result or AltMask;
  if ssCtrl in ShiftState then
    Result := Result or ControlMask;
  if (ssSuper in ShiftState) or (ssMeta in ShiftState) then
    Result := Result or SuperMask;
end;

function TUnixHotkeyManager.ModToShift(Modifiers: Integer): TShiftState;
begin
  Result := [];
  if ShiftMask and Modifiers > 0 then
    Include(Result, ssShift);
  if AltMask and Modifiers > 0 then
    Include(Result, ssAlt);
  if ControlMask and Modifiers > 0 then
    Include(Result, ssCtrl);
  if (SuperMask and Modifiers > 0) then
    Include(Result, ssMeta);
end;

procedure TUnixHotkeyManager.CaptureKey(Display: PDisplay; KeyCode: LongWord;
  Modifier: LongWord; Window: TWindow);
begin
  { Capture keys without cap or num lock }
  XGrabKey(Display, KeyCode, Modifier and NotLock, Window, 1, GrabModeAsync, GrabModeAsync);
  { Capture keys with cap lock }
  XGrabKey(Display, KeyCode, Modifier or CapLock, Window, 1, GrabModeAsync, GrabModeAsync);
  { Capture keys with num lock }
  XGrabKey(Display, KeyCode, Modifier or NumLock, Window, 1, GrabModeAsync, GrabModeAsync);
  { Capture keys with cap or num lock }
  XGrabKey(Display, KeyCode, Modifier or CapLock or NumLock, Window, 1, GrabModeAsync, GrabModeAsync);
end;

procedure TUnixHotkeyManager.ReleaseKey(Display: PDisplay; KeyCode: LongWord;
  Modifier: LongWord; Window: TWindow);
begin
  { See comments in CaptureKey }
  XUngrabKey(Display, KeyCode, Modifier and NotLock, Window);
  XUngrabKey(Display, KeyCode, Modifier or CapLock, Window);
  XUngrabKey(Display, KeyCode, Modifier or NumLock, Window);
  XUngrabKey(Display, KeyCode, Modifier or CapLock or NumLock, Window);
end;

procedure TUnixHotkeyManager.AddEventFilter;
begin
  if Count = 0 then
  begin
    {$IFDEF GTK}
    gdk_window_add_filter(FRoot, @FilterKeys, Self);
    {$ENDIF}

    {$IFDEF QT}
    QNativeEventFilter_hook_installfilter(FQNativeEventFilter, FilterKeys);
    {$ENDIF}
  end;
end;

procedure TUnixHotkeyManager.RemoveEventFilter;
begin
  if Count = 1 then
  begin
    {$IFDEF GTK}
    gdk_window_remove_filter(FRoot, @FilterKeys, Self);
    {$ENDIF}

    {$IFDEF QT}
    QNativeEventFilter_hook_removefilter(FQNativeEventFilter);
    {$ENDIF}
  end;
end;

function TUnixHotkeyManager.InternalRegisterShortcut(AShortcut: TShortCutEx;
  ARegister: Boolean): Boolean;
var
  Modifier: LongWord;
  KeySym, ShiftSym: TKeySym;
  KeyCode: LongWord;
  Window: TWindow;
  Key: Word;
  ShiftState: TShiftState;
begin
  //Global hotkeys require X11 (XGrabKey). On sessions without X11
  //(e.g. pure Wayland) FDisplay is nil and registration is not possible.
  if FDisplay = nil then
    Exit(False);

  ShortCutToKey(AShortcut.SimpleShortcut, Key, ShiftState);

  Result := Key <> 0;
  if (Result) then
  begin
    Modifier := ShiftToMod(ShiftState);
    KeySym := KeyToSym(Key);
    if KeySym = 0 then
      Exit(False); // key the X11 backend cannot express
    KeyCode := XKeysymToKeycode(FDisplay, KeySym);
    if KeyCode = 0 then
      Exit(False); // key not present in the current keyboard mapping

    {$IFDEF LCLGTK2}
    Window := gdk_x11_drawable_get_xid(FRoot);
    {$ENDIF}

    {$IFDEF LCLGTK3}
    Window := gdk_x11_window_get_xid(FRoot);
    {$ENDIF}

    {$IFDEF QT}
    Window := DefaultRootWindow(FDisplay);
    {$ENDIF}

    if ARegister then
      CaptureKey(FDisplay, KeyCode, Modifier, Window)
    else
      ReleaseKey(FDisplay, KeyCode, Modifier, Window);

    ShiftSym := XKeycodeToKeysym(FDisplay, KeyCode, 1);

    if (ShiftSym <> 0) and (KeySym <> ShiftSym) then
    begin
      KeyCode := XKeysymToKeycode(FDisplay, ShiftSym);
      if KeyCode <> 0 then
      begin
        if ARegister then
          CaptureKey(FDisplay, KeyCode, Modifier, Window)
        else
          ReleaseKey(FDisplay, KeyCode, Modifier, Window);
      end;
    end;

    if ARegister then
      AddEventFilter
    else
      RemoveEventFilter;
  end;
end;

function TUnixHotkeyManager.KeyToSym(Key: Word): TKeySym;
begin
  case Key of
    VK_TAB: Result := XK_TAB;
    VK_CLEAR: Result := XK_CLEAR;
    VK_RETURN: Result := XK_RETURN;
    VK_MENU: Result := XK_MENU;
    VK_ESCAPE: Result := XK_ESCAPE;
    VK_PAUSE: Result := XK_PAUSE;
    VK_SPACE: Result := XK_SPACE;
    VK_PRIOR: Result := XK_PRIOR;
    VK_NEXT: Result := XK_NEXT;
    VK_END: Result := XK_END;
    VK_HOME: Result := XK_HOME;
    VK_LEFT: Result := XK_LEFT;
    VK_UP: Result := XK_UP;
    VK_RIGHT: Result := XK_RIGHT;
    VK_DOWN: Result := XK_DOWN;
    VK_SELECT: Result := XK_SELECT;
    VK_EXECUTE: Result := XK_EXECUTE;
    VK_SNAPSHOT: Result := XK_PRINT;
    VK_INSERT: Result := XK_INSERT;
    VK_DELETE: Result := XK_DELETE;
    VK_HELP: Result := XK_HELP;
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;
    VK_A: Result := XK_A;
    VK_B: Result := XK_B;
    VK_C: Result := XK_C;
    VK_D: Result := XK_D;
    VK_E: Result := XK_E;
    VK_F: Result := XK_F;
    VK_G: Result := XK_G;
    VK_H: Result := XK_H;
    VK_I: Result := XK_I;
    VK_J: Result := XK_J;
    VK_K: Result := XK_K;
    VK_L: Result := XK_L;
    VK_M: Result := XK_M;
    VK_N: Result := XK_N;
    VK_O: Result := XK_O;
    VK_P: Result := XK_P;
    VK_Q: Result := XK_Q;
    VK_R: Result := XK_R;
    VK_S: Result := XK_S;
    VK_T: Result := XK_T;
    VK_U: Result := XK_U;
    VK_V: Result := XK_V;
    VK_W: Result := XK_W;
    VK_X: Result := XK_X;
    VK_Y: Result := XK_Y;
    VK_Z: Result := XK_Z;
    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_MULTIPLY;
    VK_ADD: Result := XK_KP_ADD;
    VK_SEPARATOR: Result := XK_KP_SEPARATOR;
    VK_SUBTRACT: Result := XK_KP_SUBTRACT;
    VK_DECIMAL: Result := XK_KP_DECIMAL;
    VK_DIVIDE: Result := XK_KP_DIVIDE;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_F13: Result := XK_F13;
    VK_F14: Result := XK_F14;
    VK_F15: Result := XK_F15;
    VK_F16: Result := XK_F16;
    VK_F17: Result := XK_F17;
    VK_F18: Result := XK_F18;
    VK_F19: Result := XK_F19;
    VK_F20: Result := XK_F20;
    VK_F21: Result := XK_F21;
    VK_F22: Result := XK_F22;
    VK_F23: Result := XK_F23;
    VK_F24: Result := XK_F24;
    VK_LCL_EQUAL: Result := XK_EQUAL;
    VK_LCL_COMMA: Result := XK_COMMA;
    VK_LCL_POINT: Result := XK_PERIOD;
    VK_LCL_SLASH: Result := XK_SLASH;
    VK_LCL_SEMI_COMMA: Result := XK_SEMICOLON;
    VK_LCL_MINUS: Result := XK_MINUS;
    VK_LCL_OPEN_BRACKET: Result := XK_BRACKETLEFT;
    VK_LCL_CLOSE_BRACKET: Result := XK_BRACKETRIGHT;
    VK_LCL_BACKSLASH: Result := XK_BACKSLASH;
    VK_LCL_TILDE: Result := XK_GRAVE;
    VK_LCL_QUOTE: Result := XK_SINGLELOWQUOTEMARK;
  else
    Result := 0;
  end;
end;

function TUnixHotkeyManager.SymToKey(Sym: TKeySym): Word;
begin
  case Sym of
    XK_TAB: Result := VK_TAB;
    XK_CLEAR: Result := VK_CLEAR;
    XK_RETURN: Result := VK_RETURN;
    XK_MENU: Result := VK_MENU;
    XK_ESCAPE: Result := VK_ESCAPE;
    XK_PAUSE: Result := VK_PAUSE;
    XK_SPACE: Result := VK_SPACE;
    XK_PRIOR: Result := VK_PRIOR;
    XK_NEXT: Result := VK_NEXT;
    XK_END: Result := VK_END;
    XK_HOME: Result := VK_HOME;
    XK_LEFT: Result := VK_LEFT;
    XK_UP: Result := VK_UP;
    XK_RIGHT: Result := VK_RIGHT;
    XK_DOWN: Result := VK_DOWN;
    XK_SELECT: Result := VK_SELECT;
    XK_EXECUTE: Result := VK_EXECUTE;
    XK_PRINT: Result := VK_SNAPSHOT;
    XK_INSERT: Result := VK_INSERT;
    XK_DELETE: Result := VK_DELETE;
    XK_HELP: Result := VK_HELP;
    XK_0: Result := VK_0;
    XK_1: Result := VK_1;
    XK_2: Result := VK_2;
    XK_3: Result := VK_3;
    XK_4: Result := VK_4;
    XK_5: Result := VK_5;
    XK_6: Result := VK_6;
    XK_7: Result := VK_7;
    XK_8: Result := VK_8;
    XK_9: Result := VK_9;
    XK_A: Result := VK_A;
    XK_B: Result := VK_B;
    XK_C: Result := VK_C;
    XK_D: Result := VK_D;
    XK_E: Result := VK_E;
    XK_F: Result := VK_F;
    XK_G: Result := VK_G;
    XK_H: Result := VK_H;
    XK_I: Result := VK_I;
    XK_J: Result := VK_J;
    XK_K: Result := VK_K;
    XK_L: Result := VK_L;
    XK_M: Result := VK_M;
    XK_N: Result := VK_N;
    XK_O: Result := VK_O;
    XK_P: Result := VK_P;
    XK_Q: Result := VK_Q;
    XK_R: Result := VK_R;
    XK_S: Result := VK_S;
    XK_T: Result := VK_T;
    XK_U: Result := VK_U;
    XK_V: Result := VK_V;
    XK_W: Result := VK_W;
    XK_X: Result := VK_X;
    XK_Y: Result := VK_Y;
    XK_Z: Result := VK_Z;
    XK_KP_0: Result := VK_NUMPAD0;
    XK_KP_1: Result := VK_NUMPAD1;
    XK_KP_2: Result := VK_NUMPAD2;
    XK_KP_3: Result := VK_NUMPAD3;
    XK_KP_4: Result := VK_NUMPAD4;
    XK_KP_5: Result := VK_NUMPAD5;
    XK_KP_6: Result := VK_NUMPAD6;
    XK_KP_7: Result := VK_NUMPAD7;
    XK_KP_8: Result := VK_NUMPAD8;
    XK_KP_9: Result := VK_NUMPAD9;
    XK_KP_MULTIPLY: Result := VK_MULTIPLY;
    XK_KP_ADD: Result := VK_ADD;
    XK_KP_SEPARATOR: Result := VK_SEPARATOR;
    XK_KP_SUBTRACT: Result := VK_SUBTRACT;
    XK_KP_DECIMAL: Result := VK_DECIMAL;
    XK_KP_DIVIDE: Result := VK_DIVIDE;
    XK_F1: Result := VK_F1;
    XK_F2: Result := VK_F2;
    XK_F3: Result := VK_F3;
    XK_F4: Result := VK_F4;
    XK_F5: Result := VK_F5;
    XK_F6: Result := VK_F6;
    XK_F7: Result := VK_F7;
    XK_F8: Result := VK_F8;
    XK_F9: Result := VK_F9;
    XK_F10: Result := VK_F10;
    XK_F11: Result := VK_F11;
    XK_F12: Result := VK_F12;
    XK_F13: Result := VK_F13;
    XK_F14: Result := VK_F14;
    XK_F15: Result := VK_F15;
    XK_F16: Result := VK_F16;
    XK_F17: Result := VK_F17;
    XK_F18: Result := VK_F18;
    XK_F19: Result := VK_F19;
    XK_F20: Result := VK_F20;
    XK_F21: Result := VK_F21;
    XK_F22: Result := VK_F22;
    XK_F23: Result := VK_F23;
    XK_F24: Result := VK_F24;
    XK_EQUAL: Result := VK_LCL_EQUAL;
    XK_COMMA: Result := VK_LCL_COMMA;
    XK_PERIOD: Result := VK_LCL_POINT;
    XK_SLASH: Result := VK_LCL_SLASH;
    XK_SEMICOLON: Result := VK_LCL_SEMI_COMMA;
    XK_MINUS: Result := VK_LCL_MINUS;
    XK_BRACKETLEFT: Result := VK_LCL_OPEN_BRACKET;
    XK_BRACKETRIGHT: Result := VK_LCL_CLOSE_BRACKET;
    XK_BACKSLASH: Result := VK_LCL_BACKSLASH;
    XK_GRAVE: Result := VK_LCL_TILDE;
    XK_SINGLELOWQUOTEMARK: Result := VK_LCL_QUOTE;
  else
    Result := 0;
  end;
end;

{$IFDEF GTK}
function FilterKeys(AnyEvent: PXAnyEvent; Event: PGdkEvent; Data: Pointer): TGdkFilterReturn; cdecl;
var
  Self: TUnixHotkeyManager absolute Data;
  KeyEvent: PXKeyEvent absolute AnyEvent;

  Sym: TKeySym;
  H: TShortcutEx;
  I: Integer;
begin
  if AnyEvent._type <> KeyPress then
    Exit(GDK_FILTER_CONTINUE);

  if InternalFilterKeys(Self, KeyEvent.keycode, KeyEvent.state) then
    Result := GDK_FILTER_REMOVE
  else
    Result := GDK_FILTER_CONTINUE;
end;  
{$ENDIF}

{$IFDEF QT}
function TUnixHotkeyManager.FilterKeys(handle: QNativeEventFilter_hookH;
  eventType: QByteArrayH; message: long): boolean; cdecl;
var
  Event: PByte;
begin
  Result := False;
  if (message = 0) or (QByteArray_data(eventType) = nil) then
    Exit;
  // Qt/X11 delivers an xcb_generic_event_t in `message`; its first byte is
  // the response type, a key press carries the keycode at +1 and the
  // modifier state (uint16) at +28 (see the XCB_KEY_* offsets above).
  if QByteArray_data(eventType) <> 'xcb_generic_event_t' then
    Exit;
  Event := PByte(PtrInt(message));
  if (Event^ and XCB_EVENT_TYPE_MASK) <> XCB_KEY_PRESS then
    Exit;
  Result := InternalFilterKeys(Self, Event[XCB_KEY_DETAIL_OFS],
    PWord(Event + XCB_KEY_STATE_OFS)^);
end;
{$ENDIF}

function TUnixHotkeyManager.DoRegister(Shortcut: TShortCutEx): Boolean;
begin
  case FBackend of
    uhbX11: Result := InternalRegisterShortcut(Shortcut, True);
    uhbPortal: Result := PortalRegister(Shortcut);
  else
    Result := False; // no usable backend (e.g. Wayland without a portal)
  end;
end;

function TUnixHotkeyManager.DoUnregister(Shortcut: TShortCutEx): Boolean;
begin
  case FBackend of
    uhbX11: Result := InternalRegisterShortcut(Shortcut, False);
    uhbPortal:
      if FPortal <> nil then
        Result := PortalUnregister(Shortcut)
      else
        Result := False;
  else
    Result := False;
  end;
end;

procedure TUnixHotkeyManager.RefreshNotify(Shortcut: TShortCut);
begin
  // The portal backend looks the callback up live in its desired set, so a
  // refresh must not trigger a session rebuild (that would spawn a dialog
  // per registered shortcut, e.g. from THotkeyItemsList.RefreshRegs).
  if FBackend = uhbPortal then
    Exit;
  inherited RefreshNotify(Shortcut);
end;

function TUnixHotkeyManager.IsWaylandSession: Boolean;
begin
  Result := (LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE')) = 'wayland')
    or (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '');
end;

function TUnixHotkeyManager.SelectBackend: TUnixHotkeyBackend;
begin
  // XGrabKey only works on X11. On Wayland sessions (even with XWayland
  // running) global grabs are ineffective: use the portal when possible.
  if (FDisplay <> nil) and not IsWaylandSession then
    Exit(uhbX11);
  if PortalAvailable then
    Exit(uhbPortal);
  // Wayland without a portal: no backend can provide global shortcuts.
  Result := uhbNone;
end;

procedure TUnixHotkeyManager.PortalTimerTick(Sender: TObject);
begin
  if FPortal <> nil then
    FPortal.ProcessPending;
end;

function TUnixHotkeyManager.PortalRegister(AShortcut: TShortCutEx): Boolean;
begin
  if FPortal = nil then
  begin
    FPortal := TPortalHotkeyEngine.Create(Self);
    // Read before the first registration.
    FPortal.AppToken := AppToken;
    FPortal.BindStrategy := PortalBindStrategy;
  end;
  FLastTrigger := '';
  Result := FPortal.RegisterShortcut(AShortcut, FLastTrigger);
  // Only poll once a shortcut is actually bound: the portal delivers
  // Activated signals asynchronously, and the timer drains them from the
  // main loop without needing a background thread.
  if Result and (FPortalTimer = nil) then
  begin
    FPortalTimer := TTimer.Create(nil);
    FPortalTimer.Interval := 25;
    FPortalTimer.OnTimer := PortalTimerTick;
    FPortalTimer.Enabled := True;
  end;
end;

function TUnixHotkeyManager.PortalUnregister(AShortcut: TShortCutEx): Boolean;
begin
  FLastTrigger := '';
  if FPortal = nil then
    Exit(False);
  Result := FPortal.UnregisterShortcut(AShortcut);
end;

constructor TUnixHotkeyManager.Create;
begin
  inherited Create;

  //Do NOT use widgetset displays (GDK_WINDOW_XDISPLAY, QX11Info_display,
  //QtWidgetSet.x11Display): they AV when the app runs on Wayland without X11
  //(QX11Application native interface is nil). Probe X11 ourselves; global
  //hotkeys need X11 anyway (XGrabKey), so without it the manager stays dormant.
  FDisplay := XOpenDisplay(nil);

  {$IFDEF GTK}
  if FDisplay <> nil then
    FRoot := gdk_get_default_root_window;
  {$ENDIF}

  {$IFDEF QT}
  if FDisplay <> nil then
    FQNativeEventFilter := QNativeEventFilter_hook_Create(QCoreApplication_instance());
  {$ENDIF}

  // Probe the portal only when X11 cannot be used, to avoid a needless
  // session-bus connection on a plain X11 desktop.
  FBackend := SelectBackend;
end;

destructor TUnixHotkeyManager.Destroy;
begin
  // Stop polling before tearing the portal engine down.
  if FPortalTimer <> nil then
  begin
    FPortalTimer.Enabled := False;
    FreeAndNil(FPortalTimer);
  end;
  // A per-shortcut unregistration would rebuild the whole portal session
  // once per hotkey (with a possible dialog each time): release it once.
  if (FBackend = uhbPortal) and (FPortal <> nil) then
    FPortal.Reset;
  // inherited unregisters everything (needs FPortal alive), then free it
  inherited Destroy;
  FreeAndNil(FPortal);
  {$IFDEF QT}
  // The native event filter is owned by LCL's Qt binding, not by the
  // widgetset: destroy it explicitly (it also uninstalls itself).
  if FQNativeEventFilter <> nil then
  begin
    QNativeEventFilter_Destroy(FQNativeEventFilter);
    FQNativeEventFilter := nil;
  end;
  {$ENDIF}
  if FDisplay <> nil then
  begin
    XCloseDisplay(FDisplay);
    FDisplay := nil;
  end;
end;

function TUnixHotkeyManager.IsHotkeyAvailable(Shortcut: TShortCut): Boolean;
var
  Key, Modifier: Word;
  ShiftState: TShiftState;
  KeySym: TKeySym;
  KeyCode: LongWord;
  Window: TWindow;
  OldHandler: TXErrorHandler;
begin
  Result := False;

  if Shortcut = 0 then
    Exit;

  // Already registered by this manager: probing would grab then release the
  // very binding we own, so just report it as available.
  if FindHotkey(Shortcut) >= 0 then
    Exit(True);

  case FBackend of
    uhbX11:
      begin
        //Probe with a temporary grab on the same window the real registration
        //would use: grabbing a key already taken by another client raises
        //BadAccess asynchronously, so a local X error handler is installed.
        if FDisplay = nil then
          Exit;

        ShortCutToKey(Shortcut, Key, ShiftState);
        if Key = 0 then
          Exit;

        KeySym := KeyToSym(Key);
        if KeySym = 0 then
          Exit;

        KeyCode := XKeysymToKeycode(FDisplay, KeySym);
        if KeyCode = 0 then
          Exit;

        Modifier := ShiftToMod(ShiftState);

        Window := DefaultRootWindow(FDisplay);
        {$IFDEF LCLGTK2}
        Window := gdk_x11_drawable_get_xid(FRoot);
        {$ENDIF}
        {$IFDEF LCLGTK3}
        Window := gdk_x11_window_get_xid(FRoot);
        {$ENDIF}

        OldHandler := XSetErrorHandler(@HookXErrorHandler);
        try
          HookXError := False;
          XGrabKey(FDisplay, KeyCode, Modifier and NotLock, Window, 1,
            GrabModeAsync, GrabModeAsync);
          XSync(FDisplay, False);
          Result := not HookXError;
          if Result then
            XUngrabKey(FDisplay, KeyCode, Modifier and NotLock, Window);
        finally
          XSetErrorHandler(OldHandler);
        end;
      end;
    uhbPortal:
      //The portal only reports which shortcuts it actually bound at bind
      //time; a dry-run probe would open a session (and possibly a dialog)
      //without being able to unregister it. Optimistically report available
      //and let RegisterShortcut reject what the portal refused.
      Result := True;
  else
    //No usable backend (e.g. Wayland without a portal): nothing can be
    //registered, so no shortcut is available.
    Result := False;
  end;
end;

end.
