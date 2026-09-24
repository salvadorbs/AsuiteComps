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
    FBeginCalls: Integer;
    FEndCalls: Integer;
    FEndResult: Boolean;
  protected
    function DoRegister(Shortcut: TShortcutEx): Boolean; override;
    function DoUnregister(Shortcut: TShortcutEx): Boolean; override;
    procedure DoBeginUpdate; override;
    function DoEndUpdate: Boolean; override;
  public
    constructor Create; override;
    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; override;

    function PubCount: Integer;
    function PubHotkey(Index: Integer): TShortcutEx;
    procedure PubShutdown;
    procedure PubForget;

    property RegisterCalls: Integer read FRegisterCalls;
    property UnregisterCalls: Integer read FUnregisterCalls;
    property RegisterResult: Boolean read FRegisterResult write FRegisterResult;
    property UnregisterResult: Boolean read FUnregisterResult write FUnregisterResult;
    property BeginCalls: Integer read FBeginCalls;
    property EndCalls: Integer read FEndCalls;
    property EndResult: Boolean read FEndResult write FEndResult;
  end;

  { A backend whose bindings are persistent (like the Wayland portal): its
    teardown must not unbind. }
  TTestPortalLikeManager = class(TTestHotkeyManager)
  protected
    procedure DoShutdown; override;
  end;

  { TTestHotkeysManager }

  TTestHotkeysManager = class(TTestCase)
  private
    FMgr: TTestHotkeyManager;
    FNotifyCount: Integer;
    FNotifyTag: Integer;
    FTriggerTag: Integer;
    FTriggerShortcut: TShortCut;
    FTriggerAction: String;
    FTriggerExTag: Integer;
    FTriggerExShortcut: TShortCut;
    procedure OnTestNotify(Sender: TObject; ShortcutEx: TShortcutEx);
    procedure OnTestTrigger(Sender: TObject; Tag: Integer; Trigger: TShortCut);
    procedure OnTestTriggerEx(Sender: TObject; const ActionId: String;
      Tag: Integer; Trigger: TShortCut);
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
    procedure TestBeginEndHotkeyUpdate;
    procedure TestEndHotkeyUpdateWithoutBegin;
    procedure TestAssignedTrigger;
    procedure TestBaseQueryUnknown;
    procedure TestRegisterNotifyExActionId;
    procedure TestShutdownDefaultUnregisters;
    procedure TestShutdownPortalLikeKeepsBackend;
    procedure TestForgetAllHotkeys;
    procedure TestBatchRollbackOnFailure;
    procedure TestBatchSuccessKeepsChanges;
    procedure TestBatchRollbackAllowsRetry;
    procedure TestNilActionIds;
    procedure TestTriggerChangedEx;
  end;

implementation

{ TTestHotkeyManager }

constructor TTestHotkeyManager.Create;
begin
  inherited Create;
  FRegisterResult := True;
  FUnregisterResult := True;
  FAvailable := True;
  FEndResult := True;
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

procedure TTestHotkeyManager.DoBeginUpdate;
begin
  Inc(FBeginCalls);
end;

function TTestHotkeyManager.DoEndUpdate: Boolean;
begin
  Inc(FEndCalls);
  Result := FEndResult;
end;

procedure TTestHotkeyManager.PubShutdown;
begin
  DoShutdown;
end;

procedure TTestHotkeyManager.PubForget;
begin
  ForgetAllHotkeys;
end;

procedure TTestPortalLikeManager.DoShutdown;
begin
  // Persistent backend: keep the preferences, only drop the local tracking.
  ForgetAllHotkeys;
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

procedure TTestHotkeysManager.OnTestTrigger(Sender: TObject; Tag: Integer;
  Trigger: TShortCut);
begin
  FTriggerTag := Tag;
  FTriggerShortcut := Trigger;
end;

procedure TTestHotkeysManager.OnTestTriggerEx(Sender: TObject;
  const ActionId: String; Tag: Integer; Trigger: TShortCut);
begin
  FTriggerAction := ActionId;
  FTriggerExTag := Tag;
  FTriggerExShortcut := Trigger;
end;

procedure TTestHotkeysManager.SetUp;
begin
  inherited SetUp;
  FMgr := TTestHotkeyManager.Create;
  FMgr.OnTriggerChanged := @OnTestTrigger;
  FNotifyCount := 0;
  FNotifyTag := -1;
  FTriggerTag := -1;
  FTriggerShortcut := 0;
  FTriggerAction := '';
  FTriggerExTag := -1;
  FTriggerExShortcut := 0;
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

procedure TTestHotkeysManager.TestBeginEndHotkeyUpdate;
begin
  { The update hooks fire once for the outermost pair, and registrations made
    inside the scope still reach the backend. }
  FMgr.BeginHotkeyUpdate;
  FMgr.BeginHotkeyUpdate; // nested
  AssertEquals('DoBeginUpdate once', 1, FMgr.BeginCalls);
  AssertTrue('Register inside update', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertEquals('Registered immediately on this backend', 1, FMgr.PubCount);

  AssertTrue('End nested', FMgr.EndHotkeyUpdate);
  AssertEquals('DoEndUpdate not called yet', 0, FMgr.EndCalls);
  AssertTrue('End outer', FMgr.EndHotkeyUpdate);
  AssertEquals('DoEndUpdate once', 1, FMgr.EndCalls);
end;

procedure TTestHotkeysManager.TestEndHotkeyUpdateWithoutBegin;
begin
  { Unbalanced End must be harmless and must not call the hook. }
  AssertTrue('No pending update', FMgr.EndHotkeyUpdate);
  AssertEquals('DoEndUpdate not called', 0, FMgr.EndCalls);
end;

procedure TTestHotkeysManager.TestAssignedTrigger;
begin
  AssertEquals('Unknown trigger', '', FMgr.TriggerOf(7));

  FMgr.NotifyTriggerAssigned(7, 'Ctrl+Alt+L');
  AssertEquals('Stored', 'Ctrl+Alt+L', FMgr.TriggerOf(7));
  AssertEquals('Callback tag', 7, FTriggerTag);
  AssertEquals('Callback trigger', Integer(ShortCut(VK_L, [ssCtrl, ssAlt])),
    Integer(FTriggerShortcut));

  FMgr.NotifyTriggerAssigned(7, 'Ctrl+Alt+K');
  AssertEquals('Updated', 'Ctrl+Alt+K', FMgr.TriggerOf(7));
  AssertEquals('Other tag untouched', '', FMgr.TriggerOf(8));
end;

procedure TTestHotkeysManager.TestBaseQueryUnknown;
var
  Trigger: String;
  Ids: TStringList;
begin
  { A backend without a queryable persistent store must answer hqsUnknown
    instead of a misleading absent/present, and must clear its outputs. }
  Trigger := 'sentinel';
  AssertTrue('ById unknown',
    FMgr.QueryRegisteredById('whatever', Trigger) = hqsUnknown);
  AssertEquals('Trigger cleared', '', Trigger);

  Ids := TStringList.Create;
  try
    Ids.Add('leftover');
    AssertTrue('ByShortcut unknown',
      FMgr.QueryRegisteredByShortcut(ShortCut(VK_A, [ssCtrl]), Ids) = hqsUnknown);
    AssertEquals('Ids cleared', 0, Ids.Count);
  finally
    Ids.Free;
  end;
end;

procedure TTestHotkeysManager.TestNilActionIds;
var
  NilIds: TStringList;
begin
  { Passing nil must be harmless and reported as unknown, not crash. }
  NilIds := nil;
  AssertTrue('Nil output is unknown',
    FMgr.QueryRegisteredByShortcut(ShortCut(VK_A, [ssCtrl]), NilIds) = hqsUnknown);
end;

procedure TTestHotkeysManager.TestTriggerChangedEx;
var
  Action: String;
  Tag: Integer;
  Trigger: TShortCut;
begin
  FMgr.OnTriggerChangedEx := @OnTestTriggerEx;
  FMgr.NotifyTriggerAssigned('open-home', 5, 'Ctrl+Alt+L');
  AssertEquals('ActionId delivered', 'open-home', FTriggerAction);
  AssertEquals('Tag delivered', 5, FTriggerExTag);
  AssertEquals('Trigger delivered', Integer(ShortCut(VK_L, [ssCtrl, ssAlt])),
    Integer(FTriggerExShortcut));
  AssertEquals('Stored by action', 'Ctrl+Alt+L', FMgr.TriggerOfAction('open-home'));
  AssertEquals('Stored by tag', 'Ctrl+Alt+L', FMgr.TriggerOf(5));
end;

procedure TTestHotkeysManager.TestShutdownDefaultUnregisters;
begin
  { The default teardown releases every binding. }
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
  FMgr.PubShutdown;
  AssertEquals('Count', 0, FMgr.PubCount);
  AssertEquals('Both unregistered', 2, FMgr.UnregisterCalls);
end;

procedure TTestHotkeysManager.TestShutdownPortalLikeKeepsBackend;
var
  Local: TTestPortalLikeManager;
begin
  { A persistent backend must not unbind on teardown. }
  Local := TTestPortalLikeManager.Create;
  try
    Local.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
    Local.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
    Local.PubShutdown;
    AssertEquals('Local tracking dropped', 0, Local.PubCount);
    AssertEquals('Backend untouched', 0, Local.UnregisterCalls);
  finally
    Local.Free;
  end;
end;

procedure TTestHotkeysManager.TestForgetAllHotkeys;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.PubForget;
  AssertEquals('Count', 0, FMgr.PubCount);
  AssertEquals('No unregister', 0, FMgr.UnregisterCalls);
end;

procedure TTestHotkeysManager.TestBatchSuccessKeepsChanges;
begin
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.BeginHotkeyUpdate;
  FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
  FMgr.UnregisterNotify(ShortCut(VK_A, [ssCtrl]));
  AssertTrue('Batch succeeds', FMgr.EndHotkeyUpdate);
  AssertEquals('Count', 1, FMgr.PubCount);
  AssertTrue('B present', FMgr.FindHotkey(ShortCut(VK_B, [ssCtrl])) >= 0);
  AssertEquals('A gone', -1, FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestBatchRollbackOnFailure;
begin
  { A failed batch must leave the manager exactly as it was before it opened. }
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  FMgr.EndResult := False;
  FMgr.BeginHotkeyUpdate;
  FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil);
  FMgr.UnregisterNotify(ShortCut(VK_A, [ssCtrl]));
  AssertFalse('Batch fails', FMgr.EndHotkeyUpdate);
  AssertEquals('Count restored', 1, FMgr.PubCount);
  AssertTrue('A restored', FMgr.FindHotkey(ShortCut(VK_A, [ssCtrl])) >= 0);
  AssertEquals('B dropped', -1, FMgr.FindHotkey(ShortCut(VK_B, [ssCtrl])));
end;

procedure TTestHotkeysManager.TestBatchRollbackAllowsRetry;
begin
  FMgr.EndResult := False;
  FMgr.BeginHotkeyUpdate;
  FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil);
  AssertFalse('Batch fails', FMgr.EndHotkeyUpdate);
  { The rolled-back addition must be retryable, not a stale duplicate. }
  FMgr.EndResult := True;
  AssertTrue('Retry after rollback', FMgr.RegisterNotify(ShortCut(VK_A, [ssCtrl]), nil));
  AssertEquals('Count', 1, FMgr.PubCount);
end;

procedure TTestHotkeysManager.TestRegisterNotifyExActionId;
begin
  AssertTrue('RegisterEx',
    FMgr.RegisterNotifyEx(ShortCut(VK_A, [ssCtrl]), nil, 5, 'open-home'));
  AssertEquals('Count', 1, FMgr.PubCount);
  AssertEquals('Tag stored', 5, FMgr.PubHotkey(0).Tag);
  AssertEquals('ActionId stored', 'open-home', FMgr.PubHotkey(0).ActionId);

  { RegisterNotify keeps an empty ActionId. }
  AssertTrue('Register plain',
    FMgr.RegisterNotify(ShortCut(VK_B, [ssCtrl]), nil, 6));
  AssertEquals('Empty ActionId', '', FMgr.PubHotkey(1).ActionId);
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
