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
var
  Ok: Boolean;
begin
  { Only checks the call does not raise; the result depends on the machine. }
  try
    Ok := HotkeyManager.IsHotkeyAvailable(ShortCut(VK_F24, [ssCtrl, ssAlt, ssShift]));
  except
    on E: Exception do
      Fail('IsHotkeyAvailable raised: ' + E.Message);
  end;
  AssertTrue('Called without error', True);
end;

initialization
  RegisterTest(TTestPlatformHotkeys);

end.
