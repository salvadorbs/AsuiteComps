unit Tests.Platform;

{$mode objfpc}{$H+}

{ Smoke tests for the platform hotkey manager (Windows / Unix).
  They verify construction, the singleton and the display-independent
  public API. Real global-hotkey grabs are NOT tested here: they depend
  on the machine state (X11/Wayland, other apps holding the shortcut). }

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Menus, LCLType,
  Hotkeys.Manager, Hotkeys.ShortcutEx,
  Hotkeys.Manager.Platform
  {$IFDEF UNIX}
  , Hotkeys.Manager.X11, X, XLib, KeySym
  {$ENDIF}
  ;

type

  { TTestPlatformHotkeys }

  TTestPlatformHotkeys = class(TTestCase)
  published
    procedure TestSingletonNotNil;
    procedure TestSingletonSameInstance;
    procedure TestRegisterZeroRejected;
    procedure TestUnregisterZeroRejected;
    procedure TestUnregisterMissingFails;
    procedure TestFindOnEmpty;
    procedure TestDirectInstanceCreateFree;
    procedure TestIsHotkeyAvailableCallable;
    procedure TestBackendStable;
    procedure TestRefreshNotifyCallable;
    procedure TestQueryRegisteredCallable;
    procedure TestX11Conversions;
    procedure TestX11GrabberUnsupportedKey;
  end;

implementation

procedure TTestPlatformHotkeys.TestSingletonNotNil;
begin
  AssertNotNull('HotkeyManager', HotkeyManager);
end;

procedure TTestPlatformHotkeys.TestSingletonSameInstance;
begin
  AssertTrue('Same instance', HotkeyManager = HotkeyManager);
end;

procedure TTestPlatformHotkeys.TestRegisterZeroRejected;
begin
  AssertFalse('Zero rejected', HotkeyManager.RegisterNotify(0, nil));
end;

procedure TTestPlatformHotkeys.TestUnregisterZeroRejected;
begin
  AssertFalse('Zero rejected', HotkeyManager.UnregisterNotify(0));
end;

procedure TTestPlatformHotkeys.TestUnregisterMissingFails;
begin
  AssertFalse('Missing', HotkeyManager.UnregisterNotify(ShortCut(VK_F24, [ssCtrl, ssAlt, ssShift])));
end;

procedure TTestPlatformHotkeys.TestFindOnEmpty;
begin
  HotkeyManager.ClearAllHotkeys;
  AssertEquals('Not found', -1, HotkeyManager.FindHotkey(ShortCut(VK_A, [ssCtrl])));
end;

procedure TTestPlatformHotkeys.TestDirectInstanceCreateFree;
var
  M: TBaseHotkeyManager;
begin
  { Covers the constructor path (e.g. the Unix Wayland/nil-display guard):
    construction must never raise, even without a display server. }
  {$IFDEF MSWINDOWS}
  M := TWin32HotkeyManager.Create;
  {$ELSE}
  M := TUnixHotkeyManager.Create;
  {$ENDIF}
  try
    AssertNotNull('Direct instance', M);
  finally
    M.Free;
  end;
end;

procedure TTestPlatformHotkeys.TestIsHotkeyAvailableCallable;
begin
  { Only checks the call does not raise; the result depends on the machine. }
  try
    HotkeyManager.IsHotkeyAvailable(ShortCut(VK_F24, [ssCtrl, ssAlt, ssShift]));
  except
    on E: Exception do
      Fail('IsHotkeyAvailable raised: ' + E.Message);
  end;
  AssertFalse('Zero shortcut unavailable', HotkeyManager.IsHotkeyAvailable(0));
  AssertTrue('Called without error', True);
end;

procedure TTestPlatformHotkeys.TestBackendStable;
{$IFDEF MSWINDOWS}
begin
  AssertTrue('Windows uses the native manager', True);
end;
{$ELSE}
var
  First: TUnixHotkeyBackend;
begin
  { The backend is chosen once at construction; reading it twice must not
    change (the environment may differ from the session at startup). }
  First := TUnixHotkeyManager(HotkeyManager).Backend;
  AssertEquals('Backend stable', Ord(First), Ord(TUnixHotkeyManager(HotkeyManager).Backend));
end;
{$ENDIF}

procedure TTestPlatformHotkeys.TestRefreshNotifyCallable;
begin
  { Refreshing an unknown shortcut is a no-op on every backend and must
    never raise or start a registration. }
  try
    HotkeyManager.RefreshNotify(ShortCut(VK_F24, [ssCtrl, ssAlt, ssShift]));
  except
    on E: Exception do
      Fail('RefreshNotify raised: ' + E.Message);
  end;
  AssertTrue('Called without error', True);
end;

procedure TTestPlatformHotkeys.TestX11Conversions;
{$IFDEF UNIX}
var
  Shift: TShiftState;
{$ENDIF}
begin
{$IFDEF UNIX}
  { Pure conversions extracted into Hotkeys.Manager.X11. }
  AssertEquals('Ctrl+Shift mask', Integer(ControlMask or ShiftMask),
    X11ShiftToMod([ssCtrl, ssShift]));
  AssertEquals('Alt+Super mask', Integer(Mod1Mask or Mod4Mask),
    X11ShiftToMod([ssAlt, ssMeta]));

  Shift := X11ModToShift(ControlMask or ShiftMask);
  AssertTrue('Round-trip ctrl', ssCtrl in Shift);
  AssertTrue('Round-trip shift', ssShift in Shift);
  AssertFalse('No spurious alt', ssAlt in Shift);

  AssertEquals('F7 -> XK_F7', Integer(XK_F7), Integer(X11KeyToSym(VK_F7)));
  AssertEquals('XK_F7 -> F7', Integer(VK_F7), Integer(X11SymToKey(XK_F7)));
  AssertEquals('A -> XK_A', Integer(XK_A), Integer(X11KeyToSym(VK_A)));
  AssertEquals('Unmapped key -> 0', 0, Integer(X11KeyToSym(VK_LWIN)));
{$ELSE}
  AssertTrue('X11 is unix-only', True);
{$ENDIF}
end;

procedure TTestPlatformHotkeys.TestQueryRegisteredCallable;
var
  Trigger: String;
  Ids: TStringList;
  Status: THotkeyQueryStatus;
begin
  { Only checks the queries never raise and return a valid tri-state. The
    result depends on the machine: on the portal it may be present/absent, on
    X11 or Windows it is always unknown. }
  Ids := TStringList.Create;
  try
    try
      Trigger := '';
      Status := HotkeyManager.QueryRegisteredById('asuitecompstest_nope', Trigger);
      AssertTrue('ById status valid',
        Status in [hqsUnknown, hqsAbsent, hqsPresent]);

      Ids.Add('leftover');
      Status := HotkeyManager.QueryRegisteredByShortcut(
        ShortCut(VK_F24, [ssCtrl, ssAlt, ssShift]), Ids);
      AssertTrue('ByShortcut status valid',
        Status in [hqsUnknown, hqsAbsent, hqsPresent]);
      AssertEquals('Matches cleared first', 0, Ids.Count);
    except
      on E: Exception do
        Fail('Query raised: ' + E.Message);
    end;
  finally
    Ids.Free;
  end;
end;

procedure TTestPlatformHotkeys.TestX11GrabberUnsupportedKey;
{$IFDEF UNIX}
var
  Disp: PDisplay;
  G: TX11KeyGrabber;
  W: TWindow;
{$ENDIF}
begin
{$IFDEF UNIX}
  { A key the backend cannot express must be refused without touching the X
    server (no grab, no probe). Uses the private display so it never depends on
    the widgetset. }
  Disp := XOpenDisplay(nil);
  if Disp = nil then
    Exit; // no X11 available (pure Wayland): nothing to exercise
  G := TX11KeyGrabber.Create(Disp);
  try
    W := DefaultRootWindow(Disp);
    AssertFalse('Unsupported key not grabbable',
      G.Grab(ShortCut(VK_LWIN, [ssCtrl]), W));
    AssertEquals('No error recorded for unsupported key', 0, G.LastError);
    AssertFalse('Unsupported key not available',
      G.IsAvailable(ShortCut(VK_LWIN, [ssCtrl]), W));
  finally
    G.Free;
    XCloseDisplay(Disp);
  end;
{$ELSE}
  AssertTrue('X11 is unix-only', True);
{$ENDIF}
end;

initialization
  RegisterTest(TTestPlatformHotkeys);

end.
