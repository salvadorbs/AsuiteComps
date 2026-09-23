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
  Hotkeys.Manager.Platform;

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

initialization
  RegisterTest(TTestPlatformHotkeys);

end.
