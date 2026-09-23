unit Tests.HotkeysManager;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Menus, LCLType,
  Hotkeys.Manager, Hotkeys.ShortcutEx;

type

  { Fake manager: exercises TBaseHotkeyManager logic without touching the OS. }

  TTestHotkeyManager = class(TBaseHotkeyManager)
  private
    FRegisterCalls: Integer;
    FUnregisterCalls: Integer;
    FNextIndex: Integer;
    FRegisterResult: Boolean;
    FUnregisterResult: Boolean;
    FAvailable: Boolean;
  protected
    function DoRegister(Shortcut: TShortcutEx): Boolean; override;
    function DoUnregister(Shortcut: TShortcutEx): Boolean; override;
  public
    constructor Create; override;
    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; override;

    function PubCount: Integer;
    function PubHotkey(Index: Integer): TShortcutEx;

    property RegisterCalls: Integer read FRegisterCalls;
    property UnregisterCalls: Integer read FUnregisterCalls;
    property RegisterResult: Boolean read FRegisterResult write FRegisterResult;
    property UnregisterResult: Boolean read FUnregisterResult write FUnregisterResult;
  end;

  { TTestHotkeysManager }

  TTestHotkeysManager = class(TTestCase)
  private
    FMgr: TTestHotkeyManager;
    FNotifyCount: Integer;
    FNotifyTag: Integer;
    procedure OnTestNotify(Sender: TObject; ShortcutEx: TShortcutEx);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegisterOk;
    procedure TestRegisterDuplicateRejected;
    procedure TestRegisterZeroRejected;
    procedure TestRegisterDistinctModifiers;
    procedure TestRegisterTagStored;
    procedure TestUnregisterOk;
    procedure TestUnregisterMissingFails;
    procedure TestUnregisterZeroFails;
    procedure TestFindByShortcut;
    procedure TestFindByKeyAndShift;
    procedure TestFindMissingReturnsMinusOne;
    procedure TestFindByIndex;
    procedure TestFindByIndexMissing;
    procedure TestRefreshKeepsItem;
    procedure TestRefreshUnregisterFailureKeepsItem;
    procedure TestRefreshReregisterFailureDropsItem;
    procedure TestClearAll;
    procedure TestNotifyCallback;
    procedure TestRegisterFailureNotTracked;
    procedure TestRegisterRetryAfterFailure;
    procedure TestUnregisterFailureKeepsItem;
    procedure TestCompareFunction;
    procedure TestDestructorWithItems;
    procedure TestDefaultHotkeyToken;
    procedure TestAppTokenProperty;
    procedure TestPortalBindStrategyProperty;
  end;

implementation

{ TTestHotkeyManager }

constructor TTestHotkeyManager.Create;
begin
  inherited Create;
  FRegisterResult := True;
  FUnregisterResult := True;
  FAvailable := True;
end;

function TTestHotkeyManager.DoRegister(Shortcut: TShortcutEx): Boolean;
begin
  Inc(FRegisterCalls);
  Result := FRegisterResult;
  if Result then
  begin
    Inc(FNextIndex);
    Shortcut.Index := FNextIndex;
  end;
end;

function TTestHotkeyManager.DoUnregister(Shortcut: TShortcutEx): Boolean;
begin
  Inc(FUnregisterCalls);
  Result := FUnregisterResult;
end;

function TTestHotkeyManager.IsHotkeyAvailable(Shortcut: TShortCut): Boolean;
begin
  Result := FAvailable;
end;

function TTestHotkeyManager.PubCount: Integer;
begin
  Result := Count;
end;

function TTestHotkeyManager.PubHotkey(Index: Integer): TShortcutEx;
begin
  Result := Hotkeys[Index];
end;

{ TTestHotkeysManager }

procedure TTestHotkeysManager.OnTestNotify(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  Inc(FNotifyCount);
  FNotifyTag := ShortcutEx.Tag;
end;

procedure TTestHotkeysManager.SetUp;
begin
  inherited SetUp;
  FMgr := TTestHotkeyManager.Create;
  FNotifyCount := 0;
  FNotifyTag := -1;
end;

procedure TTestHotkeysManager.TearDown;
begin
  FMgr.Free;
  FMgr := nil;
  inherited TearDown;
end;

procedure TTestHotkeysManager.TestRegisterOk;
begin
  AssertTrue('Register', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertEquals('Count', 1, FMgr.PubCount);
  AssertEquals('DoRegister called once', 1, FMgr.RegisterCalls);
end;

procedure TTestHotkeysManager.TestRegisterDuplicateRejected;
begin
  AssertTrue('First register', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertFalse('Duplicate rejected', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertEquals('Count', 1, FMgr.PubCount);
end;

procedure TTestHotkeysManager.TestRegisterZeroRejected;
begin
  AssertFalse('Zero rejected', FMgr.RegisterNotify(0, nil));
  AssertEquals('Count', 0, FMgr.PubCount);
  AssertEquals('DoRegister not called', 0, FMgr.RegisterCalls);
end;

procedure TTestHotkeysManager.TestRegisterDistinctModifiers;
begin
  AssertTrue('Ctrl+A', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertTrue('Alt+A', FMgr.RegisterNotify(ShortCut(VK_A, [ssAlt]), nil));
  AssertTrue('Shift+A', FMgr.RegisterNotify(ShortCut(VK_A, [ssShift]), nil));
  AssertEquals('Count', 3, FMgr.PubCount);
end;

procedure TTestHotkeysManager.TestRegisterTagStored;
begin
  AssertTrue('Register', FMgr.RegisterNotify(ShortCut(VK_F5, []), nil, 42));
  AssertEquals('Tag', 42, FMgr.PubHotkey(0).Tag);
end;

procedure TTestHotkeysManager.TestUnregisterOk;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  AssertTrue('Unregister', FMgr.UnregisterNotify(ShortCut(VK_A, [ssCtrl])));
  AssertEquals('Count', 0, FMgr.PubCount);
  AssertEquals('DoUnregister called once', 1, FMgr.UnregisterCalls);
end;

procedure TTestHotkeysManager.TestUnregisterMissingFails;
begin
  AssertFalse('Missing', FMgr.UnregisterNotify(ShortCut(VK_A, [ssCtrl])));
  AssertEquals('DoUnregister not called', 0, FMgr.UnregisterCalls);
end;

procedure TTestHotkeysManager.TestUnregisterZeroFails;
begin
  AssertFalse('Zero', FMgr.UnregisterNotify(0));
end;

procedure TTestHotkeysManager.TestFindByShortcut;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
  AssertEquals('Find A', 0, FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])));
  AssertEquals('Find B', 1, FMgr.FindHotkey(ShortCut(VK_B, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestFindByKeyAndShift;
begin
  FMgr.RegisterNotify(ShortCut(VK_F9, [ssAlt]), nil);
  AssertEquals('Find by key+shift', 0, FMgr.FindHotkey(VK_F9, [ssAlt]));
  AssertEquals('Wrong shift not found', -1, FMgr.FindHotkey(VK_F9, [ssCtrl]));
end;

procedure TTestHotkeysManager.TestFindMissingReturnsMinusOne;
begin
  AssertEquals('Empty list', -1, FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])));
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  AssertEquals('Missing', -1, FMgr.FindHotkey(ShortCut(VK_Z, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestFindByIndex;
var
  Idx: Integer;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil, 10);
  FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil, 20);
  Idx := FMgr.PubHotkey(1).Index;
  AssertEquals('Find by Index', 1, FMgr.FindHotkeyByIndex(Idx));
end;

procedure TTestHotkeysManager.TestFindByIndexMissing;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  AssertEquals('Missing Index', -1, FMgr.FindHotkeyByIndex(MaxInt));
end;

procedure TTestHotkeysManager.TestRefreshKeepsItem;
var
  CallsBefore: Integer;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  CallsBefore := FMgr.UnregisterCalls;
  FMgr.RefreshNotify(ShortCut(VK_A, [ssCtrl]));
  AssertEquals('Count kept', 1, FMgr.PubCount);
  AssertEquals('Unregistered once', CallsBefore + 1, FMgr.UnregisterCalls);
  AssertEquals('Re-registered', 2, FMgr.RegisterCalls);
  { Refresh of an unknown shortcut must be a no-op }
  FMgr.RefreshNotify(ShortCut(VK_Z, [ssCtrl]));
  AssertEquals('Count still 1', 1, FMgr.PubCount);
end;

procedure TTestHotkeysManager.TestRefreshUnregisterFailureKeepsItem;
begin
  { If the platform does not release the old binding, the shortcut is still
    live: keep tracking it and do not attempt a re-registration. }
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.UnregisterResult := False;
  FMgr.RefreshNotify(ShortCut(VK_A, [ssCtrl]));
  AssertEquals('Still tracked', 1, FMgr.PubCount);
  AssertEquals('Re-register not attempted', 1, FMgr.RegisterCalls);
end;

procedure TTestHotkeysManager.TestRefreshReregisterFailureDropsItem;
begin
  { Unregister succeeds but re-register fails: the shortcut is no longer
    active, so the manager must stop reporting it. }
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.UnregisterResult := True;
  FMgr.RegisterResult := False;
  FMgr.RefreshNotify(ShortCut(VK_A, [ssCtrl]));
  AssertEquals('Item dropped', 0, FMgr.PubCount);
  AssertEquals('No longer found', -1, FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestClearAll;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
  FMgr.RegisterNotify(ShortCut(VK_C, [ssCtrl]), nil);
  FMgr.ClearAllHotkeys;
  AssertEquals('Count', 0, FMgr.PubCount);
  AssertEquals('All unregistered', 3, FMgr.UnregisterCalls);
  AssertEquals('Unknown still missing', -1, FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestNotifyCallback;
var
  H: TShortcutEx;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), @OnTestNotify, 99);
  H := FMgr.PubHotkey(0);
  AssertTrue('Notify assigned', Assigned(H.Notify));
  H.Notify(FMgr, H);
  AssertEquals('Callback fired once', 1, FNotifyCount);
  AssertEquals('Tag visible in callback', 99, FNotifyTag);
end;

procedure TTestHotkeysManager.TestRegisterFailureNotTracked;
begin
  { A failed platform registration must not leave a stale item behind: the
    base manager drops it so the failure has no permanent effect. }
  FMgr.RegisterResult := False;
  AssertFalse('Register fails', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertEquals('Count', 0, FMgr.PubCount);
  AssertEquals('Not tracked', -1, FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestRegisterRetryAfterFailure;
begin
  FMgr.RegisterResult := False;
  AssertFalse('First attempt fails', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  FMgr.RegisterResult := True;
  AssertTrue('Retry succeeds', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertEquals('Count', 1, FMgr.PubCount);
  AssertEquals('DoRegister called twice', 2, FMgr.RegisterCalls);
end;

procedure TTestHotkeysManager.TestUnregisterFailureKeepsItem;
begin
  { When the platform refuses to release a shortcut, it must stay tracked so
    the failure can be retried and the object is not freed while still live. }
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.UnregisterResult := False;
  AssertFalse('Unregister fails', FMgr.UnregisterNotify(ShortCut(VK_A, [ssCtrl])));
  AssertEquals('Still tracked', 1, FMgr.PubCount);
  FMgr.UnregisterResult := True;
  AssertTrue('Retry unregister succeeds', FMgr.UnregisterNotify(ShortCut(VK_A, [ssCtrl])));
  AssertEquals('Count', 0, FMgr.PubCount);
end;

procedure TTestHotkeysManager.TestCompareFunction;
var
  A, B: TShortcutEx;
begin
  A := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
  B := TShortcutEx.Create(ShortCut(VK_B, [ssCtrl]));
  try
    AssertTrue('A < B', HotkeyCompare(A, B) < 0);
    AssertTrue('B > A', HotkeyCompare(B, A) > 0);
    AssertEquals('A = A', 0, HotkeyCompare(A, A));
  finally
    A.Free;
    B.Free;
  end;
  A := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
  B := TShortcutEx.Create(ShortCut(VK_A, [ssAlt]));
  try
    AssertTrue('Same key, different shift', HotkeyCompare(A, B) <> 0);
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TTestHotkeysManager.TestDefaultHotkeyToken;
var
  Token: String;
  I: Integer;
  C: Char;
begin
  Token := DefaultHotkeyToken;
  AssertTrue('Non empty', Token <> '');
  for I := 1 to Length(Token) do
  begin
    C := Token[I];
    AssertTrue('Object-path-safe characters only',
      ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'))
      or ((C >= '0') and (C <= '9')) or (C = '_'));
  end;
end;

procedure TTestHotkeysManager.TestAppTokenProperty;
var
  Local: TTestHotkeyManager;
begin
  { The base class stores the token as-is; the portal backend sanitizes it. }
  FMgr.AppToken := 'my_token';
  AssertEquals('Stored', 'my_token', FMgr.AppToken);

  Local := TTestHotkeyManager.Create;
  try
    AssertTrue('Not empty by default', Local.AppToken <> '');
  finally
    Local.Free;
  end;
end;

procedure TTestHotkeysManager.TestPortalBindStrategyProperty;
begin
  AssertTrue('Default is pbsAuto', FMgr.PortalBindStrategy = pbsAuto);
  FMgr.PortalBindStrategy := pbsIncremental;
  AssertTrue('Stored incremental', FMgr.PortalBindStrategy = pbsIncremental);
  FMgr.PortalBindStrategy := pbsSpecCompliant;
  AssertTrue('Stored spec-compliant', FMgr.PortalBindStrategy = pbsSpecCompliant);
end;

procedure TTestHotkeysManager.TestDestructorWithItems;
var
  Local: TTestHotkeyManager;
begin
  Local := TTestHotkeyManager.Create;
  try
    Local.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
    Local.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
  finally
    Local.Free;
  end;
  AssertTrue('Freed without error', True);
end;

initialization
  RegisterTest(TTestHotkeysManager);

end.
