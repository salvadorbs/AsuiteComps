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
{$IFDEF UNIX}
    procedure OnActNotify(Sender: TObject; ShortcutEx: TShortcutEx);
{$ENDIF}
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
    procedure TestStableShortcutId;
    procedure TestStableShortcutIdActionId;
    procedure TestProcessPendingWithoutSession;
    procedure TestNormalizeAccelerator;
    procedure TestCompareTriggers;
    procedure TestShortcutsByTrigger;
    procedure TestQueryEmptyInputsUnknown;
    procedure TestPortalIdInjective;
    procedure TestAcceleratorRejectsAltGrHyper;
    procedure TestReadShortcutListParsing;
    procedure TestReadResultsDictMalformed;
    procedure TestLivePortalRoundTrip;
    procedure TestLiveQueryReadOnly;
    procedure TestLivePersistenceAcrossInstances;
    procedure TestLiveActivation;
  end;

implementation

{$IFDEF UNIX}
uses
  dbus;
{$ENDIF}

function LivePortalAllowed: Boolean;
begin
  Result := GetEnvironmentVariable('ASUITECOMPS_TEST_PORTAL') = '1';
end;

{ Read-only live checks (list/query, no bind, no permission dialog) can run
  with a separate switch so they never trigger the interactive bind test. }
function LivePortalQueryAllowed: Boolean;
begin
  Result := LivePortalAllowed
    or (GetEnvironmentVariable('ASUITECOMPS_TEST_PORTAL_QUERY') = '1');
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

procedure TTestPortal.TestStableShortcutId;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
  A, B: TShortcutEx;
  IdA, IdB: String;
{$ENDIF}
begin
{$IFDEF UNIX}
  Engine := TPortalHotkeyEngine.Create(nil);
  A := nil;
  B := nil;
  try
    Engine.AppToken := 'tok';

    // Same action (Tag) with a different key combination must keep the same
    // id, otherwise the desktop would treat the change as a new action.
    A := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
    A.Tag := 42;
    IdA := Engine.ShortcutIdOf(A);
    B := TShortcutEx.Create(ShortCut(VK_B, [ssAlt]));
    B.Tag := 42;
    IdB := Engine.ShortcutIdOf(B);
    AssertEquals('Same action, same id', IdA, IdB);
    AssertEquals('Token + tag', 'tok_42', IdA);

    B.Tag := 43;
    AssertTrue('Different action, different id', IdA <> Engine.ShortcutIdOf(B));

    // Anonymous registration (no Tag) falls back to the key.
    A.Tag := -1;
    AssertTrue('Anonymous falls back to key',
      Copy(Engine.ShortcutIdOf(A), 1, Length('tok_key_')) = 'tok_key_');
  finally
    A.Free;
    B.Free;
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestStableShortcutIdActionId;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
  A: TShortcutEx;
{$ENDIF}
begin
{$IFDEF UNIX}
  Engine := TPortalHotkeyEngine.Create(nil);
  A := nil;
  try
    Engine.AppToken := 'tok';
    { A host-supplied ActionId is the most stable identity: it wins over Tag
      and survives a shortcut change. }
    A := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
    A.Tag := 42;
    A.ActionId := 'open-home';
    AssertEquals('ActionId wins', 'tok_open_2Dhome', Engine.ShortcutIdOf(A));
    AssertEquals('Host key round-trips', 'open-home',
      Engine.HostKeyOf(Engine.ShortcutIdOf(A)));
    A.SimpleShortcut := ShortCut(VK_B, [ssAlt]);
    AssertEquals('Stable across shortcut change', 'tok_open_2Dhome',
      Engine.ShortcutIdOf(A));
    A.ActionId := '';
    AssertEquals('Falls back to Tag', 'tok_42', Engine.ShortcutIdOf(A));
    AssertEquals('Tag host key round-trips', '42',
      Engine.HostKeyOf(Engine.ShortcutIdOf(A)));
  finally
    A.Free;
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestNormalizeAccelerator;
begin
  AssertEquals('Lowercase modifiers and key', 'CTRL+ALT+A',
    NormalizePortalAccelerator('ctrl+alt+a'));
  AssertEquals('Reordered modifiers', 'CTRL+ALT+F7',
    NormalizePortalAccelerator('Alt+Ctrl+F7'));
  AssertEquals('Meta folds to LOGO', 'LOGO+T',
    NormalizePortalAccelerator('Meta+t'));
  AssertEquals('Super folds to LOGO', 'CTRL+LOGO+Q',
    NormalizePortalAccelerator('Super+ctrl+q'));
  AssertEquals('No key', '', NormalizePortalAccelerator('Ctrl+Alt'));
  AssertEquals('Empty', '', NormalizePortalAccelerator(''));
  AssertEquals('Whitespace tolerated', 'CTRL+ALT+F7',
    NormalizePortalAccelerator(' Ctrl + Alt + F7 '));
  { A description with no recognised modifier cannot be compared (e.g. a
    localised string), so it normalises to ''. }
  AssertEquals('No recognised modifier', '', NormalizePortalAccelerator('Strg+F7'));
end;

procedure TTestPortal.TestCompareTriggers;
begin
  AssertTrue('Case insensitive',
    ComparePortalTriggers('Ctrl+Alt+F7', 'CTRL+ALT+F7') = ptmYes);
  AssertTrue('Order insensitive',
    ComparePortalTriggers('Alt+Ctrl+F7', 'CTRL+ALT+F7') = ptmYes);
  AssertTrue('Different key',
    ComparePortalTriggers('Ctrl+Alt+F8', 'CTRL+ALT+F7') = ptmNo);
  AssertTrue('Empty vs value',
    ComparePortalTriggers('', 'CTRL+ALT+F7') = ptmUnknown);
  AssertTrue('Unparseable side',
    ComparePortalTriggers('Strg+F7', 'CTRL+ALT+F7') = ptmUnknown);
  AssertTrue('Parseable', PortalTriggerParseable('Ctrl+Alt+F7'));
  AssertFalse('Not parseable', PortalTriggerParseable('Strg+F7'));
end;

procedure TTestPortal.TestShortcutsByTrigger;
var
  Items, Matches: TPortalShortcutArray;
begin
  SetLength(Items, 2);
  Items[0].Id := 'a';
  Items[0].Trigger := 'Ctrl+Alt+F7';
  Items[1].Id := 'b';
  Items[1].Trigger := 'Ctrl+Alt+F8';

  Matches := PortalShortcutsByTrigger(Items, 'CTRL+ALT+F7');
  AssertEquals('One match', 1, Length(Matches));
  AssertEquals('Matched id', 'a', Matches[0].Id);

  Matches := PortalShortcutsByTrigger(Items, 'CTRL+ALT+F9');
  AssertEquals('No match', 0, Length(Matches));

  Matches := PortalShortcutsByTrigger(nil, 'CTRL+ALT+F7');
  AssertEquals('Empty input', 0, Length(Matches));
end;

procedure TTestPortal.TestQueryEmptyInputsUnknown;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
  Info: TPortalShortcutInfo;
  Matches: TPortalShortcutArray;
{$ENDIF}
begin
{$IFDEF UNIX}
  { Empty criteria must not touch the bus: they are simply unknown. }
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Info.Id := 'x';
    AssertTrue('Empty id unknown', Engine.QueryById('', Info) = hqsUnknown);
    AssertEquals('Info cleared', '', Info.Id);

    SetLength(Matches, 0);
    AssertTrue('Empty trigger unknown',
      Engine.QueryByTrigger('', Matches) = hqsUnknown);
    AssertEquals('Matches cleared', 0, Length(Matches));
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestPortalIdInjective;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
{$ENDIF}
begin
{$IFDEF UNIX}
  { The encoding must be collision-free and reversible: distinct host keys
    never share a portal id, and HostKeyOf recovers the original. }
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Engine.AppToken := 'tok';
    AssertTrue('Dash vs underscore differ',
      Engine.PortalIdFor('open-home') <> Engine.PortalIdFor('open_home'));
    AssertEquals('Round-trip dash', 'open-home',
      Engine.HostKeyOf(Engine.PortalIdFor('open-home')));
    AssertEquals('Round-trip underscore', 'open_home',
      Engine.HostKeyOf(Engine.PortalIdFor('open_home')));
    AssertEquals('Round-trip empty-safe', 'a b/c:d',
      Engine.HostKeyOf(Engine.PortalIdFor('a b/c:d')));
    AssertEquals('Round-trip unicode', 'caffè-è',
      Engine.HostKeyOf(Engine.PortalIdFor('caffè-è')));
    AssertEquals('Empty key -> empty id', '', Engine.PortalIdFor(''));
    AssertEquals('Foreign id returned as-is', 'other_thing',
      Engine.HostKeyOf('other_thing'));
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestAcceleratorRejectsAltGrHyper;
begin
  { AltGr/Hyper are not part of the accelerator format this backend emits:
    reporting the shortcut as unsupported is safer than sending a different
    combination. }
  AssertEquals('AltGr rejected', '', PortalAccelerator(VK_A, [ssCtrl, ssAltGr]));
  AssertEquals('Hyper rejected', '', PortalAccelerator(VK_A, [ssCtrl, ssHyper]));
  AssertEquals('Plain still accepted', 'CTRL+A', PortalAccelerator(VK_A, [ssCtrl]));
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

{$IFDEF UNIX}
function BuildShortcutListMessage(ANonArray: Boolean): PDBusMessage;
var
  Iter, Arr, St, Opts: DBusMessageIter;
  S: PChar;
begin
  Result := dbus_message_new(DBUS_MESSAGE_TYPE_METHOD_RETURN);
  dbus_message_iter_init_append(Result, @Iter);
  if ANonArray then
  begin
    S := 'not-an-array';
    dbus_message_iter_append_basic(@Iter, DBUS_TYPE_STRING, @S);
    Exit;
  end;
  dbus_message_iter_open_container(@Iter, DBUS_TYPE_ARRAY, '(sa{sv})', @Arr);
  dbus_message_iter_open_container(@Arr, DBUS_TYPE_STRUCT, nil, @St);
  S := 'tok_1';
  dbus_message_iter_append_basic(@St, DBUS_TYPE_STRING, @S);
  dbus_message_iter_open_container(@St, DBUS_TYPE_ARRAY, '{sv}', @Opts);
  dbus_message_iter_close_container(@St, @Opts);
  dbus_message_iter_close_container(@Arr, @St);
  dbus_message_iter_close_container(@Iter, @Arr);
end;

function BuildResultsDictMessage(ABadShortcuts: Boolean): PDBusMessage;
var
  Iter, Arr, Entry, VarIter, Inner: DBusMessageIter;
  S: PChar;
begin
  Result := dbus_message_new(DBUS_MESSAGE_TYPE_METHOD_RETURN);
  dbus_message_iter_init_append(Result, @Iter);
  dbus_message_iter_open_container(@Iter, DBUS_TYPE_ARRAY, '{sv}', @Arr);
  dbus_message_iter_open_container(@Arr, DBUS_TYPE_DICT_ENTRY, nil, @Entry);
  S := 'shortcuts';
  dbus_message_iter_append_basic(@Entry, DBUS_TYPE_STRING, @S);
  if ABadShortcuts then
  begin
    // A variant carrying a string where an a(sa{sv}) is expected.
    dbus_message_iter_open_container(@Entry, DBUS_TYPE_VARIANT, 's', @VarIter);
    S := 'oops';
    dbus_message_iter_append_basic(@VarIter, DBUS_TYPE_STRING, @S);
    dbus_message_iter_close_container(@Entry, @VarIter);
  end
  else
  begin
    dbus_message_iter_open_container(@Entry, DBUS_TYPE_VARIANT, 'a(sa{sv})', @VarIter);
    dbus_message_iter_open_container(@VarIter, DBUS_TYPE_ARRAY, '(sa{sv})', @Inner);
    dbus_message_iter_close_container(@VarIter, @Inner);
    dbus_message_iter_close_container(@Entry, @VarIter);
  end;
  dbus_message_iter_close_container(@Arr, @Entry);
  dbus_message_iter_close_container(@Iter, @Arr);
end;
{$ENDIF}

procedure TTestPortal.TestReadShortcutListParsing;
{$IFDEF UNIX}
var
  Msg: PDBusMessage;
  Iter: DBusMessageIter;
  Shortcuts: TPortalShortcutArray;
{$ENDIF}
begin
{$IFDEF UNIX}
  Msg := BuildShortcutListMessage(False);
  try
    AssertTrue('iter init', dbus_message_iter_init(Msg, @Iter) <> 0);
    AssertTrue('valid list parsed', ReadShortcutList(@Iter, Shortcuts));
    AssertEquals('one entry', 1, Length(Shortcuts));
    AssertEquals('id', 'tok_1', Shortcuts[0].Id);
  finally
    dbus_message_unref(Msg);
  end;

  Msg := BuildShortcutListMessage(True);
  try
    AssertTrue('iter init', dbus_message_iter_init(Msg, @Iter) <> 0);
    AssertFalse('non-array is malformed', ReadShortcutList(@Iter, Shortcuts));
    AssertEquals('cleared on failure', 0, Length(Shortcuts));
  finally
    dbus_message_unref(Msg);
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestReadResultsDictMalformed;
{$IFDEF UNIX}
var
  Msg: PDBusMessage;
  Iter: DBusMessageIter;
  Session: String;
  Shortcuts: TPortalShortcutArray;
{$ENDIF}
begin
{$IFDEF UNIX}
  Msg := BuildResultsDictMessage(False);
  try
    AssertTrue('iter init', dbus_message_iter_init(Msg, @Iter) <> 0);
    AssertTrue('valid dict parsed', ReadResultsDict(@Iter, Session, Shortcuts));
    AssertEquals('empty shortcut list', 0, Length(Shortcuts));
  finally
    dbus_message_unref(Msg);
  end;

  Msg := BuildResultsDictMessage(True);
  try
    AssertTrue('iter init', dbus_message_iter_init(Msg, @Iter) <> 0);
    AssertFalse('wrong shortcut type is malformed',
      ReadResultsDict(@Iter, Session, Shortcuts));
  finally
    dbus_message_unref(Msg);
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
  Trigger, Id: String;
  Info: TPortalShortcutInfo;
  Matches: TPortalShortcutArray;
  TriggerMatch: THotkeyTriggerMatch;
  I: Integer;
  Found: Boolean;
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
      // A stable action id: the portal id must survive a shortcut change and
      // identify the action, not the key combination.
      S.ActionId := 'livequery';
      Id := Engine.ShortcutIdOf(S);

      AssertTrue('Register via portal', Engine.RegisterShortcut(S, Trigger));
      AssertTrue('Trigger assigned', Trigger <> '');

      // Read-only verification right after the bind (portal-based, no
      // desktop-specific API): the action is present and the trigger matches.
      AssertTrue('VerifyRegistered present',
        Engine.VerifyRegistered(Id, Trigger, Info, TriggerMatch) = hqsPresent);
      AssertTrue('VerifyRegistered trigger matches', TriggerMatch = ptmYes);
      AssertTrue('QueryByActionKey present',
        Engine.QueryByActionKey('livequery', Info) = hqsPresent);
      AssertEquals('Host key decoded', 'livequery', Info.ActionKey);

      Matches := nil;
      Engine.QueryByTrigger(Trigger, Matches);
      Found := False;
      for I := 0 to High(Matches) do
        if Matches[I].Id = Id then
          Found := True;
      AssertTrue('QueryByTrigger finds the action', Found);

      AssertTrue('Unregister via portal', Engine.UnregisterShortcut(S));
      AssertTrue('QueryByActionKey absent after unregister',
        Engine.QueryByActionKey('livequery', Info) = hqsAbsent);
      AssertFalse('VerifyRegistered no longer present after unregister',
        Engine.VerifyRegistered(Id, Trigger, Info, TriggerMatch) = hqsPresent);
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

procedure TTestPortal.TestLivePersistenceAcrossInstances;
{$IFDEF UNIX}
var
  Engine1, Engine2: TPortalHotkeyEngine;
  S: TShortcutEx;
  Trigger: String;
  Info: TPortalShortcutInfo;
{$ENDIF}
begin
{$IFDEF UNIX}
  if not LivePortalAllowed then
    Exit; // needs a real portal + user approval; run manually
  if not PortalAvailable then
    Fail('Live test requested but no portal available');
  { A normal engine teardown must NOT unbind: the shortcut must survive in the
    desktop and be visible to a new engine with the same namespace. }
  Engine1 := TPortalHotkeyEngine.Create(nil);
  try
    Engine1.AppToken := 'asuitecompspersist';
    S := TShortcutEx.Create(ShortCut(VK_F8, [ssCtrl, ssAlt]));
    try
      S.ActionId := 'persist1';
      AssertTrue('Register via portal', Engine1.RegisterShortcut(S, Trigger));
    finally
      S.Free;
    end;
  finally
    Engine1.Free; // Shutdown on destroy: close session, keep preference
  end;

  Engine2 := TPortalHotkeyEngine.Create(nil);
  try
    Engine2.AppToken := 'asuitecompspersist';
    AssertTrue('Persisted shortcut visible to a new engine',
      Engine2.QueryByActionKey('persist1', Info) = hqsPresent);
    // Clean up intentionally: Reset unbinds the whole set.
    Engine2.Reset;
    AssertTrue('Removed after intentional Reset',
      Engine2.QueryByActionKey('persist1', Info) in [hqsAbsent, hqsUnknown]);
  finally
    Engine2.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

procedure TTestPortal.TestLiveQueryReadOnly;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
  Info: TPortalShortcutInfo;
  Matches: TPortalShortcutArray;
{$ENDIF}
begin
{$IFDEF UNIX}
  if not LivePortalQueryAllowed then
    Exit;
  if not PortalAvailable then
    Fail('Live test requested but no portal available');
  { Read-only: creates a session and lists the shortcuts, never binds. Safe to
    run without a permission dialog or a global grab. }
  Engine := TPortalHotkeyEngine.Create(nil);
  try
    Engine.AppToken := 'asuitecompstest';
    AssertTrue('Unknown action is absent, not unknown',
      Engine.QueryByActionKey('definitely_not_registered_xyz', Info) = hqsAbsent);
    AssertTrue('Unused combination is absent or unknown',
      Engine.QueryByTrigger('CTRL+ALT+F24', Matches) in [hqsAbsent, hqsUnknown]);
  finally
    Engine.Free;
  end;
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

{$IFDEF UNIX}
var
  ActFired: Boolean = False;

procedure TTestPortal.OnActNotify(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  ActFired := True;
end;
{$ENDIF}

procedure TTestPortal.TestLiveActivation;
{$IFDEF UNIX}
var
  Engine: TPortalHotkeyEngine;
  S: TShortcutEx;
  Trigger: String;
  Deadline: QWord;
{$ENDIF}
begin
{$IFDEF UNIX}
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
{$ELSE}
  AssertTrue('Portal is unix-only', True);
{$ENDIF}
end;

initialization
  RegisterTest(TTestPortal);

end.
