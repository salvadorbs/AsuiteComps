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
  Hotkeys.Manager.X11, LCLType, Menus, LCLProc, Classes, sysutils, ExtCtrls

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
    FX11: TX11KeyGrabber;
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

    procedure AddEventFilter;
    procedure RemoveEventFilter;
    function InternalRegisterShortcut(AShortcut: TShortCutEx; ARegister: Boolean): Boolean;
    function X11Window: TWindow;
    function IsWaylandSession: Boolean;
    function SelectBackend: TUnixHotkeyBackend;
    function EnsurePortal: TPortalHotkeyEngine;
    function PortalRegister(AShortcut: TShortCutEx): Boolean;
    function PortalUnregister(AShortcut: TShortCutEx): Boolean;
    procedure PortalTimerTick(Sender: TObject);
  protected
    function DoRegister(Shortcut: TShortCutEx): Boolean; override;
    function DoUnregister(Shortcut: TShortCutEx): Boolean; override;
    procedure DoBeginUpdate; override;
    function DoEndUpdate: Boolean; override;
    { On the portal, teardown must NOT unbind: the desktop keeps the user's
      preferences. On X11 the grabs are released. }
    procedure DoShutdown; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RefreshNotify(Shortcut: TShortCut); override;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; override;

    { Portal-backed read-only queries. On X11 or with no backend they return
      hqsUnknown: those backends have no persistent, enumerable store, so
      "not registered" cannot be distinguished from "cannot be checked". }
    function QueryRegisteredById(const ActionId: String;
      out AssignedTrigger: String): THotkeyQueryStatus; override;
    function QueryRegisteredByShortcut(Shortcut: TShortCut;
      var ActionIds: TStringList): THotkeyQueryStatus; override;

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

const
  { Minimal XCB key-press layout, read straight from the native Qt event
    message (xcb_key_press_event_t). Only the fields needed to identify a
    key press are used, so the xcb binding unit is not required. }
  XCB_KEY_PRESS = 2;
  XCB_KEY_DETAIL_OFS = 1;  // keycode
  XCB_KEY_STATE_OFS = 28;  // modifier state (uint16)
  XCB_EVENT_TYPE_MASK = $7F;

implementation

function InternalFilterKeys(Self: TUnixHotkeyManager; KeyCode: Cardinal; KeyState: Cardinal): Boolean;
var
  I: Integer;
  H: TShortcutEx;
  Sym: TKeySym;
begin
  Sym := XKeycodeToKeysym(Self.FDisplay, KeyCode, 0);
  I := Self.FindHotkey(X11SymToKey(Sym), X11ModToShift(KeyState));

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

function TUnixHotkeyManager.X11Window: TWindow;
begin
  // Grabs go on the widgetset's root window so the widgetset filter receives
  // the key press; on Qt the plain X root window is used.
  {$IFDEF LCLGTK2}
  Result := gdk_x11_drawable_get_xid(FRoot);
  {$ENDIF}
  {$IFDEF LCLGTK3}
  Result := gdk_x11_window_get_xid(FRoot);
  {$ENDIF}
  {$IFDEF QT}
  Result := DefaultRootWindow(FDisplay);
  {$ENDIF}
end;

function TUnixHotkeyManager.InternalRegisterShortcut(AShortcut: TShortCutEx;
  ARegister: Boolean): Boolean;
var
  Window: TWindow;
begin
  //Global hotkeys require X11 (XGrabKey). On sessions without X11
  //(e.g. pure Wayland) FDisplay is nil and registration is not possible.
  if (FDisplay = nil) or (FX11 = nil) then
    Exit(False);

  Window := X11Window;
  if ARegister then
    Result := FX11.Grab(AShortcut.SimpleShortcut, Window)
  else
    Result := FX11.Ungrab(AShortcut.SimpleShortcut, Window);

  if Result then
  begin
    // The widgetset filter only matters while at least one grab is active.
    if ARegister then
      AddEventFilter
    else
      RemoveEventFilter;
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

function TUnixHotkeyManager.EnsurePortal: TPortalHotkeyEngine;
begin
  if FPortal = nil then
  begin
    FPortal := TPortalHotkeyEngine.Create(Self);
    // Read before the first registration.
    FPortal.AppToken := AppToken;
  end;
  Result := FPortal;
end;

procedure TUnixHotkeyManager.DoBeginUpdate;
begin
  // Create the engine now so the update scope is known even before the first
  // registration (e.g. while the hotkey list is being loaded).
  if FBackend = uhbPortal then
    EnsurePortal.BeginUpdate;
end;

function TUnixHotkeyManager.DoEndUpdate: Boolean;
begin
  if (FBackend = uhbPortal) and (FPortal <> nil) then
    Result := FPortal.EndUpdate
  else
    Result := True;
end;

function TUnixHotkeyManager.PortalRegister(AShortcut: TShortCutEx): Boolean;
begin
  FLastTrigger := '';
  Result := EnsurePortal.RegisterShortcut(AShortcut, FLastTrigger);
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
  FX11 := TX11KeyGrabber.Create(FDisplay);

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
  // inherited Destroy calls DoShutdown, which for the portal only drops the
  // local tracking (no unbind) and for X11 releases the grabs.
  inherited Destroy;
  // Now that no shortcut object is referenced, close the session without
  // touching the persisted preferences.
  if FPortal <> nil then
    FPortal.Shutdown;
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
  // The grabber borrows the display; drop it before closing the connection.
  FreeAndNil(FX11);
  if FDisplay <> nil then
  begin
    XCloseDisplay(FDisplay);
    FDisplay := nil;
  end;
end;

procedure TUnixHotkeyManager.DoShutdown;
begin
  // Wayland portal bindings survive the application exit by design: do not
  // simulate Unregister on teardown or the user's desktop preferences would be
  // destroyed. X11 (and the no-backend case) release their grabs as usual.
  if FBackend = uhbPortal then
    ForgetAllHotkeys
  else
    inherited DoShutdown;
end;

function TUnixHotkeyManager.IsHotkeyAvailable(Shortcut: TShortCut): Boolean;
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
      // Probe with a temporary grab on the same window the real registration
      // would use; TX11KeyGrabber handles the asynchronous BadAccess.
      Result := (FX11 <> nil) and FX11.IsAvailable(Shortcut, X11Window);
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

function TUnixHotkeyManager.QueryRegisteredById(const ActionId: String;
  out AssignedTrigger: String): THotkeyQueryStatus;
var
  Info: TPortalShortcutInfo;
begin
  AssignedTrigger := '';
  if (FBackend <> uhbPortal) or (ActionId = '') then
    Exit(hqsUnknown);
  Info.Id := '';
  Info.ActionKey := '';
  Info.Trigger := '';
  Info.Description := '';
  Result := EnsurePortal.QueryByActionKey(ActionId, Info);
  if Result = hqsPresent then
    AssignedTrigger := Info.Trigger;
end;

function TUnixHotkeyManager.QueryRegisteredByShortcut(Shortcut: TShortCut;
  var ActionIds: TStringList): THotkeyQueryStatus;
var
  Key: Word;
  Shift: TShiftState;
  Acc: String;
  Matches: TPortalShortcutArray;
  I: Integer;
begin
  if ActionIds = nil then
    Exit(hqsUnknown); // no place to report matches
  ActionIds.Clear;
  if (FBackend <> uhbPortal) or (Shortcut = 0) then
    Exit(hqsUnknown);
  ShortCutToKey(Shortcut, Key, Shift);
  Acc := PortalAccelerator(Key, Shift);
  if Acc = '' then
    Exit(hqsUnknown);
  Result := EnsurePortal.QueryByTrigger(Acc, Matches);
  if Result = hqsPresent then
    for I := 0 to High(Matches) do
      ActionIds.Add(Matches[I].ActionKey); // host-level keys, not portal ids
end;

end.
