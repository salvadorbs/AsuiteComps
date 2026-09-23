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
  SysUtils, Classes, LCLType, Menus, Hotkeys.ShortcutEx, Generics.Collections,
  Generics.Defaults;

type
  { How the Wayland portal backend applies a change of the shortcut set.

    pbsSpecCompliant - a fresh session for every change, honouring the
      portal rule that BindShortcuts is called once per session;
    pbsIncremental   - one long-lived session, re-bound on every change;
    pbsAuto          - pbsIncremental on KDE (where a shortcut can only be
      removed while it is registered in the current session), pbsSpecCompliant
      elsewhere. See Hotkeys.Manager.Portal for the details. }
  TPortalBindStrategy = (pbsAuto, pbsSpecCompliant, pbsIncremental);

  THotkeyList = TObjectList<TShortcutEx>;

  THotkeysComparer = TComparer<TShortcutEx>;

  { TBaseHotkeyManager }

  TBaseHotkeyManager = class
  private
    FList: THotkeyList;
    FAppToken: String;
    FPortalBindStrategy: TPortalBindStrategy;
    function GetHotkey(Index: Integer): TShortcutEx;
    function GetCount: Integer;
  protected
    function DoRegister(Shortcut: TShortCutEx): Boolean; virtual; abstract;
    function DoUnregister(Shortcut: TShortCutEx): Boolean; virtual; abstract;

    property Hotkeys[Index: Integer]: TShortcutEx read GetHotkey; default;
    property Count: Integer read GetCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function RegisterNotify(Shortcut: TShortCut; Notify: TKeyNotifyEvent; Tag: Integer = -1): Boolean;
    function UnregisterNotify(Shortcut: TShortCut): Boolean;
    { Re-applies the platform registration of an already known shortcut.
      Only the callback/tag can change, so backends whose bindings are looked
      up live may override this with a no-op. }
    procedure RefreshNotify(Shortcut: TShortCut); virtual;

    function FindHotkey(Key: Word; ShiftState: TShiftState): Integer; overload;
    function FindHotkey(Shortcut: TShortCut): Integer; overload;
    function FindHotkeyByIndex(Index: Integer): Integer;
    procedure ClearAllHotkeys;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; virtual; abstract;

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

    { Binding strategy used by the Wayland portal backend. Set it before the
      first registration; pbsAuto is the recommended default. }
    property PortalBindStrategy: TPortalBindStrategy
      read FPortalBindStrategy write FPortalBindStrategy;
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
  FPortalBindStrategy := pbsAuto;
  FList := THotkeyList.Create(THotkeysComparer.Construct(HotkeyCompare), True);
end;

destructor TBaseHotkeyManager.Destroy;
begin
  ClearAllHotkeys;
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

  if DoRegister(H) then
  begin
    FList.Add(H);
    Result := True;
  end
  else
    H.Free;
end;

function TBaseHotkeyManager.UnregisterNotify(Shortcut: TShortCut): Boolean;
var
  I: Integer;
begin
  Result := False;

  if Shortcut = 0 then
    Exit(False);

  I := FindHotkey(Shortcut);
  if I > -1 then
  begin
    // Only drop the item when the platform released it, so a failure keeps
    // the state consistent and can be retried.
    if DoUnregister(FList[I]) then
    begin
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

initialization
  InternalManager := nil;

finalization
  InternalManager.Free;

end.

