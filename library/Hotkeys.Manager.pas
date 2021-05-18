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
  THotkeyList = TObjectList<TShortcutEx>;

  THotkeysComparer = TComparer<TShortcutEx>;

  { TBaseHotkeyManager }

  TBaseHotkeyManager = class
  private
    FList: THotkeyList;
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
    procedure RefreshNotify(Shortcut: TShortCut);

    function FindHotkey(Key: Word; ShiftState: TShiftState): Integer; overload;
    function FindHotkey(Shortcut: TShortCut): Integer; overload;
    function FindHotkeyByIndex(Index: Integer): Integer;
    procedure ClearAllHotkeys;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; virtual; abstract;
  end;

{ Used by THotkeyList }
function HotkeyCompare(constref A, B: TShortcutEx): Integer;

var
  InternalManager: TBaseHotkeyManager;

implementation

constructor TBaseHotkeyManager.Create;
begin
  inherited Create;

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
  if Shortcut = 0 then
    Exit(False);

  I := FindHotkey(Shortcut);

  Result := I < 0;
  if Result then
  begin
    H := TShortcutEx.Create(Shortcut);
    try
      H.Notify := Notify;
      H.Tag := Tag;

      Result := DoRegister(H);
    finally
      FList.Add(H);
    end;
  end;
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
    Result := DoUnregister(FList[I]);
    FList.Delete(I);
  end;
end;

procedure TBaseHotkeyManager.RefreshNotify(Shortcut: TShortCut);
var
  I: Integer;
  ShortCutEx: TShortcutEx;
begin
  I := FindHotkey(Shortcut);
  if I > -1 then
  begin
    ShortCutEx := Self[I];

    DoUnregister(ShortCutEx);

    DoRegister(ShortCutEx);
  end;
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
  while Count > 0 do
  begin
    H := Hotkeys[Count - 1];
    UnregisterNotify(H.SimpleShortcut);
  end;
end;

initialization
  InternalManager := nil;

finalization
  InternalManager.Free;

end.

