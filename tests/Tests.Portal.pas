unit Tests.Portal;

{$mode objfpc}{$H+}

{ Tests for the Wayland GlobalShortcuts portal backend.
  Pure-logic tests always run. The live portal test only runs when the
  ASUITECOMPS_TEST_PORTAL environment variable is set to '1', so CI and
  headless runs never block on permission dialogs. }

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Menus, LCLType,
  Hotkeys.ShortcutEx, Hotkeys.Manager,
  Hotkeys.Manager.Portal;

type

  { TTestPortal }

  TTestPortal = class(TTestCase)
  private
    procedure OnActNotify(Sender: TObject; ShortcutEx: TShortcutEx);
  published
    procedure TestAcceleratorCtrlAlt;
    procedure TestAcceleratorFunctionKeys;
    procedure TestAcceleratorSpecialKeys;
    procedure TestAcceleratorNumpad;
    procedure TestAcceleratorPunctuation;
    procedure TestAcceleratorPrint;
    procedure TestAcceleratorNeedsModifier;
    procedure TestAcceleratorUnsupportedKey;
    procedure TestAcceleratorMetaBecomesLogo;
    procedure TestAvailableRunsWithoutCrash;
    procedure TestEngineCreateFree;
    procedure TestAppTokenSanitized;
    procedure TestBindStrategyResolution;
    procedure TestProcessPendingWithoutSession;
    procedure TestLivePortalRoundTrip;
    procedure TestLiveActivation;
  end;

implementation

{$IFDEF UNIX}
uses
  dbus;

{ Best-effort check against KDE's KGlobalAccel: is any component holding an
  action with this id? Returns False when KGlobalAccel is not reachable, so
  the live test stays usable on non-KDE desktops. }
function KdeShortcutExists(const ActionId: String): Boolean;
var
  Conn: PDBusConnection;
  Msg, Reply: PDBusMessage;
  Iter, Arr, StructIter: DBusMessageIter;
  P: PChar;
  Paths: TStringList;
  I: Integer;
  Name: String;
begin
  Result := False;
  if ActionId = '' then
    Exit;
  Conn := dbus_bus_get_private(DBUS_BUS_SESSION, nil);
  if Conn = nil then
    Exit;
  Paths := TStringList.Create;
  try
    dbus_connection_set_exit_on_disconnect(Conn, 0);
    Msg := dbus_message_new_method_call('org.kde.kglobalaccel', '/kglobalaccel',
      'org.kde.KGlobalAccel', 'allComponents');
    Reply := dbus_connection_send_with_reply_and_block(Conn, Msg, 2000, nil);
    dbus_message_unref(Msg);
    if Reply = nil then
      Exit;
    if dbus_message_iter_init(Reply, @Iter) <> 0 then
    begin
      dbus_message_iter_recurse(@Iter, @Arr);
      repeat
        if dbus_message_iter_get_arg_type(@Arr) = DBUS_TYPE_OBJECT_PATH then
        begin
          P := nil;
          dbus_message_iter_get_basic(@Arr, @P);
          if P <> nil then
            Paths.Add(String(P));
        end;
      until dbus_message_iter_next(@Arr) = 0;
    end;
    dbus_message_unref(Reply);

    for I := 0 to Paths.Count - 1 do
    begin
      Msg := dbus_message_new_method_call('org.kde.kglobalaccel',
        PChar(Paths[I]), 'org.kde.kglobalaccel.Component', 'allShortcutInfos');
      Reply := dbus_connection_send_with_reply_and_block(Conn, Msg, 2000, nil);
      dbus_message_unref(Msg);
      if Reply = nil then
        Continue;
      if dbus_message_iter_init(Reply, @Iter) <> 0 then
      begin
        dbus_message_iter_recurse(@Iter, @Arr);
        repeat
          if dbus_message_iter_get_arg_type(@Arr) = DBUS_TYPE_STRUCT then
          begin
            dbus_message_iter_recurse(@Arr, @StructIter);
            Name := '';
            if dbus_message_iter_get_arg_type(@StructIter) = DBUS_TYPE_STRING then
            begin
              P := nil;
              dbus_message_iter_get_basic(@StructIter, @P);
              if P <> nil then
                Name := String(P);
            end;
            if Name = ActionId then
              Result := True;
          end;
        until Result or (dbus_message_iter_next(@Arr) = 0);
      end;
      dbus_message_unref(Reply);
      if Result then
        Break;
    end;
  finally
    Paths.Free;
    dbus_connection_close(Conn);
    dbus_connection_unref(Conn);
  end;
end;
{$ENDIF}

function LivePortalAllowed: Boolean;
begin
  Result := GetEnvironmentVariable('ASUITECOMPS_TEST_PORTAL') = '1';
end;

procedure TTestPortal.TestAcceleratorCtrlAlt;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl, ssAlt]));
  try
    AssertEquals('CTRL+ALT+A', 'CTRL+ALT+A', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorFunctionKeys;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_F7, [ssCtrl, ssAlt]));
  try
    AssertEquals('CTRL+ALT+F7', 'CTRL+ALT+F7', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
  S := TShortcutEx.Create(ShortCut(VK_F12, [ssShift]));
  try
    AssertEquals('SHIFT+F12', 'SHIFT+F12', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorSpecialKeys;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_SPACE, [ssCtrl]));
  try
    AssertEquals('CTRL+space', 'CTRL+space', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
  S := TShortcutEx.Create(ShortCut(VK_DELETE, [ssAlt]));
  try
    AssertEquals('ALT+Delete', 'ALT+Delete', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorNumpad;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_NUMPAD5, [ssCtrl]));
  try
    AssertEquals('CTRL+KP_5', 'CTRL+KP_5', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
  S := TShortcutEx.Create(ShortCut(VK_ADD, [ssAlt]));
  try
    AssertEquals('ALT+KP_Add', 'ALT+KP_Add', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorPunctuation;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_LCL_COMMA, [ssCtrl]));
  try
    AssertEquals('CTRL+comma', 'CTRL+comma', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
  S := TShortcutEx.Create(ShortCut(VK_LCL_QUOTE, [ssCtrl, ssShift]));
  try
    AssertEquals('CTRL+SHIFT+apostrophe', 'CTRL+SHIFT+apostrophe',
      PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorPrint;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_SNAPSHOT, [ssCtrl]));
  try
    AssertEquals('CTRL+Print', 'CTRL+Print', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorNeedsModifier;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_A, []));
  try
    AssertEquals('No modifier rejected', '', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorUnsupportedKey;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_LWIN, [ssCtrl]));
  try
    AssertEquals('Unsupported key rejected', '', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAcceleratorMetaBecomesLogo;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_T, [ssMeta]));
  try
    AssertEquals('LOGO+T', 'LOGO+T', PortalAccelerator(S.Key, S.ShiftState));
  finally
    S.Free;
  end;
end;

procedure TTestPortal.TestAvailableRunsWithoutCrash;
{$IFDEF UNIX}
var
  Av: Boolean;
{$ENDIF}
begin
{$IFDEF UNIX}
  try
    Av := PortalAvailable;
  except
    on E: Exception do
      Fail('PortalAvailable raised: ' + E.Message);
  end;
  AssertTrue('Called without error', True);
  if Av then
    WriteLn('Portal is available on this machine');
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestEngineCreateFree;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
{$ENDIF}
begin
{$IFDEF UNIX}
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    AssertNotNull('Engine', Engine);
  finally
    Engine.Free;
  end;
  AssertTrue('Freed without error', True);
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestAppTokenSanitized;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
{$ENDIF}
begin
{$IFDEF UNIX}
  { The portal token ends up in an object path element, so it must be
    reduced to [A-Za-z0-9_]. }
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Engine.AppToken := 'my-token.v2 beta';
    AssertEquals('Sanitized', 'my_token_v2_beta', Engine.AppToken);
    Engine.AppToken := '...';
    AssertEquals('All invalid falls back', '___', Engine.AppToken);
    Engine.AppToken := '';
    AssertEquals('Empty falls back', 'hotkey', Engine.AppToken);
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestBindStrategyResolution;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
{$ENDIF}
begin
{$IFDEF UNIX}
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    AssertTrue('Default is pbsAuto', Engine.BindStrategy = pbsAuto);
    { pbsAuto must always resolve to a concrete strategy, whichever the
      desktop is. }
    AssertTrue('Auto resolves away from pbsAuto',
      Engine.ResolvedBindStrategy in [pbsSpecCompliant, pbsIncremental]);

    Engine.BindStrategy := pbsSpecCompliant;
    AssertTrue('Explicit spec-compliant kept',
      Engine.ResolvedBindStrategy = pbsSpecCompliant);
    Engine.BindStrategy := pbsIncremental;
    AssertTrue('Explicit incremental kept',
      Engine.ResolvedBindStrategy = pbsIncremental);
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestProcessPendingWithoutSession;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
{$ENDIF}
begin
{$IFDEF UNIX}
  { No thread is used: polling must be a harmless no-op before a session
    exists and must never raise. }
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Engine.ProcessPending;
    Engine.ProcessPending;
    AssertTrue('ProcessPending without session', True);
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestLivePortalRoundTrip;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
  S: TShortcutEx;
  Trigger: String;
{$ENDIF}
begin
{$IFDEF UNIX}
  if not LivePortalAllowed then
    Exit; // needs a real portal + user approval; run manually
  if not PortalAvailable then
    Fail('Live test requested but no portal available');
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Engine.AppToken := 'asuitecompstest';
    S := TShortcutEx.Create(ShortCut(VK_F7, [ssCtrl, ssAlt]));
    try
      AssertTrue('Register via portal', Engine.RegisterShortcut(S, Trigger));
      AssertTrue('Trigger assigned', Trigger <> '');
      AssertTrue('KDE lists the shortcut while registered',
        KdeShortcutExists('asuitecompstest' + IntToStr(S.SimpleShortcut)));
      AssertTrue('Unregister via portal', Engine.UnregisterShortcut(S));
      AssertFalse('KDE no longer lists the shortcut after unregister',
        KdeShortcutExists('asuitecompstest' + IntToStr(S.SimpleShortcut)));
    finally
      S.Free;
    end;
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

var
  ActFired: Boolean = False;

procedure TTestPortal.OnActNotify(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  ActFired := True;
end;

procedure TTestPortal.TestLiveActivation;
var
  Engine: TPortalHotkeyEngine;
  S: TShortcutEx;
  Trigger: String;
  Deadline: QWord;
begin
  if GetEnvironmentVariable('ASUITECOMPS_TEST_PORTAL') <> '2' then
    Exit; // interactive: needs the user to press the hotkey
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    S := TShortcutEx.Create(ShortCut(VK_F8, [ssCtrl, ssAlt]));
    try
      S.Notify := @OnActNotify;
      AssertTrue('Register via portal', Engine.RegisterShortcut(S, Trigger));
      WriteLn('PRESS Ctrl+Alt+F8 NOW (waiting 60s)...');
      Deadline := GetTickCount64 + 60000;
      while not ActFired and (GetTickCount64 < Deadline) do
      begin
        Engine.ProcessPending;
        Sleep(50);
      end;
      AssertTrue('Activation received', ActFired);
    finally
      Engine.UnregisterShortcut(S);
      S.Free;
    end;
  finally
    Engine.Free;
  end;
end;

initialization
  RegisterTest(TTestPortal);

end.
