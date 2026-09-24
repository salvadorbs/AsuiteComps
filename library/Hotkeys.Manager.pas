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

unit Hotkeys.Manager;

{$I ASuiteComps.inc}

interface

uses
  SysUtils, Classes, LCLType, LCLProc, Menus, Hotkeys.ShortcutEx,
  Generics.Collections, Generics.Defaults;

type
  { Fired when the backend learns which trigger a shortcut actually has.
    On the portal this may differ from the requested one because the user
    changed it in the desktop settings; Tag identifies the action.
    The trigger is a TShortCut (0 when it cannot be represented) so the event
    crosses the AnsiString/UnicodeString boundary between the package and the
    application without type clashes. }
  TTriggerChangedEvent = procedure(Sender: TObject; Tag: Integer;
    Trigger: TShortCut) of object;

  { Same as TTriggerChangedEvent but carries the stable host-level ActionId
    ('' when the caller did not provide one). Prefer this event when several
    actions may share the same Tag or use Tag = -1. }
  TTriggerChangedExEvent = procedure(Sender: TObject; const ActionId: String;
    Tag: Integer; Trigger: TShortCut) of object;

  { Result of asking a backend whether an action or a key combination is
    currently known to its *persistent* store (e.g. the Wayland
    GlobalShortcuts portal, where bindings survive the application exit).

    The tri-state matters: a plain Boolean cannot tell "not registered" apart
    from "cannot be checked", and a backend without a queryable store must not
    pretend to know.

      hqsUnknown - the backend cannot answer (unsupported, no service, error,
                   malformed reply or timeout);
      hqsAbsent  - the backend answered and the action/combination is not
                   registered there;
      hqsPresent - the backend answered and the action/combination is
                   registered there.

    Querying is a read-only operation and must never create or remove a
    binding. The portal answers about the shortcuts it exposes to this
    application/session, not about every global shortcut of the desktop. }
  THotkeyQueryStatus = (hqsUnknown, hqsAbsent, hqsPresent);

  { Outcome of comparing a requested key combination with the one a backend
    reports. A backend may expose a human-readable trigger that cannot be
    interpreted (e.g. a localised description), so "different" and "not
    comparable" must stay distinct.

      ptmUnknown - at least one side is not a comparable accelerator;
      ptmNo      - both are comparable and differ;
      ptmYes     - both are comparable and equal. }
  THotkeyTriggerMatch = (ptmUnknown, ptmNo, ptmYes);

  THotkeyList = TObjectList<TShortcutEx>;

  THotkeysComparer = TComparer<TShortcutEx>;

  { TBaseHotkeyManager }

  TBaseHotkeyManager = class
  private
    FList: THotkeyList;
    FAppToken: String;
    FUpdateLevel: Integer;
    FUpdateAdded: THotkeyList;   // added during the current batch (not owned)
    FUpdateRemoved: THotkeyList; // removed during the current batch (owned)
    FOnTriggerChanged: TTriggerChangedEvent;
    FOnTriggerChangedEx: TTriggerChangedExEvent;
    FAssignedTriggers: TStringList; // Tag -> trigger assigned by the backend
    FAssignedByAction: TStringList; // ActionId -> trigger assigned
    function GetHotkey(Index: Integer): TShortcutEx;
    function GetCount: Integer;
    procedure RollbackUpdate;
  protected
    function DoRegister(Shortcut: TShortCutEx): Boolean; virtual; abstract;
    function DoUnregister(Shortcut: TShortCutEx): Boolean; virtual; abstract;

    { Bulk-update hooks: called once when the outermost BeginHotkeyUpdate /
      EndHotkeyUpdate pair opens/closes. The default implementation does
      nothing (backends that apply every change immediately need no batching);
      the portal uses them to bind the whole set only once. }
    procedure DoBeginUpdate; virtual;
    function DoEndUpdate: Boolean; virtual;

    { Teardown hook called from the destructor. The default releases every
      binding like ClearAllHotkeys; the portal overrides it to keep the
      desktop preferences intact (a Wayland binding survives the app exit).
      Use ForgetAllHotkeys to drop the local tracking without touching the
      backend. }
    procedure DoShutdown; virtual;
    procedure ForgetAllHotkeys;

    property Hotkeys[Index: Integer]: TShortcutEx read GetHotkey; default;
    property Count: Integer read GetCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function RegisterNotify(Shortcut: TShortCut; Notify: TKeyNotifyEvent; Tag: Integer = -1): Boolean;
    { Same as RegisterNotify but also sets the stable ActionId the host uses to
      identify the action. On backends with a persistent store the ActionId
      keeps the binding across shortcut changes and restarts; Tag remains a
      runtime callback value. }
    function RegisterNotifyEx(Shortcut: TShortCut; Notify: TKeyNotifyEvent;
      Tag: Integer; const ActionId: String): Boolean;
    function UnregisterNotify(Shortcut: TShortCut): Boolean;
    { Re-applies the platform registration of an already known shortcut.
      Only the callback/tag can change, so backends whose bindings are looked
      up live may override this with a no-op. }
    procedure RefreshNotify(Shortcut: TShortCut); virtual;

    function FindHotkey(Key: Word; ShiftState: TShiftState): Integer; overload;
    function FindHotkey(Shortcut: TShortCut): Integer; overload;
    function FindHotkeyByIndex(Index: Integer): Integer;
    procedure ClearAllHotkeys;

    { Groups many registrations/removals into a single backend update. The
      portal binds the resulting set only once, when the outermost update
      closes; other backends keep applying each change immediately. Nested
      calls are allowed. }
    procedure BeginHotkeyUpdate;
    function EndHotkeyUpdate: Boolean;

    { Called by a backend when it learns the trigger actually assigned to a
      shortcut (e.g. the portal reported it). ActionId is the host-level
      identity ('' when unknown). }
    procedure NotifyTriggerAssigned(const ActionId: String; Tag: Integer;
      const Trigger: String); overload;
    procedure NotifyTriggerAssigned(Tag: Integer;
      const Trigger: String); overload;
    { The trigger the backend last reported for Tag, or '' if unknown. On the
      portal this can differ from the requested one when the user changed it
      in the desktop settings. }
    function TriggerOf(Tag: Integer): String;
    { The trigger the backend last reported for a host-level ActionId. }
    function TriggerOfAction(const ActionId: String): String;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; virtual; abstract;

    { Asks the backend whether the action identified by ActionId is currently
      registered in its persistent store, and which trigger it reports.
      ActionId is the host-level identity (the same string passed to
      RegisterNotifyEx, or IntToStr(Tag)); the backend maps it to its own id.
      Backends without a queryable store return hqsUnknown and set
      AssignedTrigger to ''. }
    function QueryRegisteredById(const ActionId: String;
      out AssignedTrigger: String): THotkeyQueryStatus; virtual;
    { Asks the backend which actions currently use the given key combination.
      ActionIds is cleared first and then filled with the host-level action
      keys (the same strings accepted by QueryRegisteredById; possibly more
      than one, or none). Passing nil is allowed and treated as hqsUnknown.
      Backends without a queryable store return hqsUnknown. }
    function QueryRegisteredByShortcut(Shortcut: TShortCut;
      var ActionIds: TStringList): THotkeyQueryStatus; virtual;

    { Keyword used by backends that must namespace themselves (the Wayland
      portal uses it for the session handle, the request handle and the
      shortcut ids). Defaults to the executable name; set it before the
      first registration to override it. The portal backend freezes the
      token at the first registration, because changing it later would
      desynchronize the engine from the portal.

      Note: the portal may still group the shortcuts under the application
      id it derives from the process (e.g. the IDE or terminal that started
      the app), so this does not always change the name shown by the
      desktop environment. }
    property AppToken: String read FAppToken write FAppToken;

    { The trigger actually assigned by the backend (see TTriggerChangedEvent). }
    property OnTriggerChanged: TTriggerChangedEvent
      read FOnTriggerChanged write FOnTriggerChanged;

    { Like OnTriggerChanged but also reports the stable host-level ActionId. }
    property OnTriggerChangedEx: TTriggerChangedExEvent
      read FOnTriggerChangedEx write FOnTriggerChangedEx;
  end;

{ Used by THotkeyList }
function HotkeyCompare(constref A, B: TShortcutEx): Integer;

{ Sanitized default for TBaseHotkeyManager.AppToken (executable basename,
  reduced to object-path-safe characters). }
function DefaultHotkeyToken: String;

var
  InternalManager: TBaseHotkeyManager;

implementation

function DefaultHotkeyToken: String;
var
  S: String;
  I: Integer;
begin
  S := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Result := '';
  for I := 1 to Length(S) do
    if ((S[I] >= 'a') and (S[I] <= 'z'))
      or ((S[I] >= 'A') and (S[I] <= 'Z'))
      or ((S[I] >= '0') and (S[I] <= '9'))
      or (S[I] = '_') then
      Result := Result + S[I]
    else
      Result := Result + '_';
  if Result = '' then
    Result := 'hotkey';
end;

constructor TBaseHotkeyManager.Create;
begin
  inherited Create;

  FAppToken := DefaultHotkeyToken;
  FAssignedTriggers := TStringList.Create;
  FAssignedByAction := TStringList.Create;
  FList := THotkeyList.Create(THotkeysComparer.Construct(HotkeyCompare), True);
  FUpdateAdded := THotkeyList.Create(False);
  FUpdateRemoved := THotkeyList.Create(True);
end;

destructor TBaseHotkeyManager.Destroy;
begin
  DoShutdown;
  FAssignedByAction.Free;
  FAssignedTriggers.Free;
  FUpdateRemoved.Free;
  FUpdateAdded.Free;
  FList.Free;

  inherited Destroy;
end;

function TBaseHotkeyManager.GetHotkey(Index: Integer): TShortcutEx;
begin
  Result := FList[Index];
end;

function TBaseHotkeyManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function HotkeyCompare(constref A, B: TShortcutEx): Integer;
begin
  Result := A.Key - B.Key;
  if Result <> 0 then
    Exit;
  Result := LongInt(A.ShiftState) - LongInt(B.ShiftState);
end;

function TBaseHotkeyManager.FindHotkey(Key: Word; ShiftState: TShiftState): Integer;
var
  Shortcut: TShortcut;
begin
  Shortcut := KeyToShortCut(Key, ShiftState);

  Result := FindHotkey(Shortcut);
end;

function TBaseHotkeyManager.FindHotkey(Shortcut: TShortCut): Integer;
var
  Item: TShortcutEx;
begin
  Result := -1;

  Item := TShortcutEx.Create(Shortcut);
  try
    Result := FList.IndexOf(Item);
  finally
    Item.Free;
  end;
end;

function TBaseHotkeyManager.RegisterNotify(Shortcut: TShortCut; Notify: TKeyNotifyEvent; Tag: Integer = -1): Boolean;
begin
  Result := RegisterNotifyEx(Shortcut, Notify, Tag, '');
end;

function TBaseHotkeyManager.RegisterNotifyEx(Shortcut: TShortCut;
  Notify: TKeyNotifyEvent; Tag: Integer; const ActionId: String): Boolean;
var
  H: TShortcutEx;
  I: Integer;
begin
  Result := False;

  if Shortcut = 0 then
    Exit(False);

  I := FindHotkey(Shortcut);
  if I >= 0 then
    Exit(False); // already registered (or a previous attempt is still tracked)

  // Register with the platform first, and keep the item only when it
  // succeeded. This way a failed DoRegister leaves no stale entry behind
  // and the caller can simply retry (see the transactional portal backend).
  H := TShortcutEx.Create(Shortcut);
  H.Notify := Notify;
  H.Tag := Tag;
  H.ActionId := ActionId;

  if DoRegister(H) then
  begin
    FList.Add(H);
    if FUpdateLevel > 0 then
      FUpdateAdded.Add(H); // tracked so a failed batch can roll it back
    Result := True;
  end
  else
    H.Free;
end;

function TBaseHotkeyManager.UnregisterNotify(Shortcut: TShortCut): Boolean;
var
  I: Integer;
  H: TShortcutEx;
begin
  Result := False;

  if Shortcut = 0 then
    Exit(False);

  I := FindHotkey(Shortcut);
  if I > -1 then
  begin
    // Only drop the item when the platform released it, so a failure keeps
    // the state consistent and can be retried.
    H := FList[I];
    if DoUnregister(H) then
    begin
      if FUpdateLevel > 0 then
      begin
        // Keep the object alive until the batch outcome is known, so a failed
        // batch can restore it.
        FList.OwnsObjects := False;
        try
          FList.Delete(I);
        finally
          FList.OwnsObjects := True;
        end;
        FUpdateRemoved.Add(H);
      end
      else
        FList.Delete(I);
      Result := True;
    end;
  end;
end;

procedure TBaseHotkeyManager.RefreshNotify(Shortcut: TShortCut);
var
  I: Integer;
  ShortCutEx: TShortcutEx;
begin
  I := FindHotkey(Shortcut);
  if I < 0 then
    Exit;

  ShortCutEx := FList[I];

  // Nothing to refresh if the platform kept the old binding: the shortcut is
  // still live and re-registering it would fail anyway.
  if not DoUnregister(ShortCutEx) then
    Exit;

  // The platform may refuse the re-registration (e.g. the shortcut was taken
  // in the meantime). Drop the now-unregistered item so the manager does not
  // report a shortcut that is no longer active; the caller can register it
  // again and will get a fresh callback.
  if not DoRegister(ShortCutEx) then
    FList.Delete(I);
end;

function TBaseHotkeyManager.FindHotkeyByIndex(Index: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to (FList.Count - 1) do
  begin
    if FList[I].Index = Index then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure TBaseHotkeyManager.ClearAllHotkeys;
var
  H: TShortcutEx;
begin
  // Teardown must always terminate, even when the platform refuses to
  // release a shortcut: force the removal instead of looping forever. A
  // failed DoUnregister may leave the shortcut live in the OS, but there is
  // nothing else to try once the manager is going away.
  while Count > 0 do
  begin
    H := Hotkeys[Count - 1];
    DoUnregister(H);
    FList.Delete(Count - 1);
  end;
end;

procedure TBaseHotkeyManager.DoBeginUpdate;
begin
  // Immediate backends need no batching.
end;

function TBaseHotkeyManager.DoEndUpdate: Boolean;
begin
  Result := True;
end;

procedure TBaseHotkeyManager.DoShutdown;
begin
  ClearAllHotkeys;
end;

procedure TBaseHotkeyManager.ForgetAllHotkeys;
begin
  // Drop the local tracking without touching the backend: used on teardown by
  // backends whose bindings are persistent (the Wayland portal).
  FList.Clear;
end;

procedure TBaseHotkeyManager.RollbackUpdate;
var
  I, J: Integer;
  H: TShortcutEx;
begin
  // The backend did not confirm the batch: drop the additions and restore the
  // removals so the manager matches what is actually registered.
  for I := 0 to FUpdateAdded.Count - 1 do
  begin
    H := FUpdateAdded[I];
    J := FList.IndexOf(H);
    if J >= 0 then
    begin
      FList.OwnsObjects := False;
      try
        FList.Delete(J);
      finally
        FList.OwnsObjects := True;
      end;
      H.Free;
    end;
  end;
  FUpdateAdded.Clear;

  for I := 0 to FUpdateRemoved.Count - 1 do
    FList.Add(FUpdateRemoved[I]); // ownership moves back to FList
  FUpdateRemoved.OwnsObjects := False;
  FUpdateRemoved.Clear;
  FUpdateRemoved.OwnsObjects := True;
end;

function TBaseHotkeyManager.QueryRegisteredById(const ActionId: String;
  out AssignedTrigger: String): THotkeyQueryStatus;
begin
  // Backends without a persistent, queryable store cannot answer. Reporting
  // hqsUnknown (instead of a misleading hqsAbsent) lets the caller tell
  // "not registered" apart from "cannot be checked".
  AssignedTrigger := '';
  Result := hqsUnknown;
end;

function TBaseHotkeyManager.QueryRegisteredByShortcut(Shortcut: TShortCut;
  var ActionIds: TStringList): THotkeyQueryStatus;
begin
  if ActionIds <> nil then
    ActionIds.Clear;
  Result := hqsUnknown;
end;

procedure TBaseHotkeyManager.BeginHotkeyUpdate;
begin
  Inc(FUpdateLevel);
  if FUpdateLevel = 1 then
  begin
    FUpdateAdded.Clear;
    FUpdateRemoved.Clear;
    DoBeginUpdate;
  end;
end;

function TBaseHotkeyManager.EndHotkeyUpdate: Boolean;
begin
  Result := True;
  if FUpdateLevel > 0 then
  begin
    Dec(FUpdateLevel);
    if FUpdateLevel = 0 then
    begin
      Result := DoEndUpdate;
      if Result then
      begin
        FUpdateAdded.Clear;
        FUpdateRemoved.Clear; // frees the confirmed removals
      end
      else
        RollbackUpdate;
    end;
  end;
end;

procedure TBaseHotkeyManager.NotifyTriggerAssigned(const ActionId: String;
  Tag: Integer; const Trigger: String);
begin
  FAssignedTriggers.Values[IntToStr(Tag)] := Trigger;
  if ActionId <> '' then
    FAssignedByAction.Values[ActionId] := Trigger;
  if Assigned(FOnTriggerChanged) then
    FOnTriggerChanged(Self, Tag, TextToShortCut(Trigger));
  if Assigned(FOnTriggerChangedEx) then
    FOnTriggerChangedEx(Self, ActionId, Tag, TextToShortCut(Trigger));
end;

procedure TBaseHotkeyManager.NotifyTriggerAssigned(Tag: Integer;
  const Trigger: String);
begin
  NotifyTriggerAssigned('', Tag, Trigger);
end;

function TBaseHotkeyManager.TriggerOf(Tag: Integer): String;
begin
  Result := FAssignedTriggers.Values[IntToStr(Tag)];
end;

function TBaseHotkeyManager.TriggerOfAction(const ActionId: String): String;
begin
  Result := FAssignedByAction.Values[ActionId];
end;

initialization
  InternalManager := nil;

finalization
  InternalManager.Free;

end.

