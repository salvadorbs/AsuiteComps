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

unit Hotkeys.Manager.Platform;

{$MODE DelphiUnicode}

interface

uses
  windows, Dialogs, LCLProc, Hotkeys.Manager, Hotkeys.ShortcutEx, Classes, SysUtils;

type

  { TWin32HotkeyManager }

  TWin32HotkeyManager = class(TBaseHotkeyManager)
  private
    FWindow: HWND;
    FWindowClassInfo: WNDCLASSEXW;
    FWindowClassRegistered: Boolean;

    function CreateAppWindow: Boolean;
    function RegisterWindowClass: Boolean;
    procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
  protected
    function DoRegister(Shortcut: TShortCutEx): Boolean; override;
    function DoUnregister(Shortcut: TShortCutEx): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsHotkeyAvailable(Shortcut: TShortCut): Boolean; override;

    { Message-only window that receives WM_HOTKEY (0 when creation failed).
      Mostly for diagnostics. }
    property Window: HWND read FWindow;
  end;

{ Returns the global hotkey manager instance }
function HotkeyManager: TBaseHotkeyManager;

const
  WinClassName: string = 'TWin32HotkeyApp';
  HotKeyAtomPrefix: string = 'TWin32Hotkey';

var
  { The window class is process-wide: keep it registered as long as at least
    one manager is alive (the tests, for instance, create a direct instance
    next to the singleton). }
  WindowClassRefCount: Integer = 0;

implementation

function HotkeyManager: TBaseHotkeyManager;
begin
  if InternalManager = nil then
    InternalManager := TWin32HotkeyManager.Create;

  Result := TBaseHotkeyManager(InternalManager);
end;

function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
var
  Capture: TWin32HotkeyManager;
  I: Integer;
  H: TShortcutEx;
begin
  Result := 0;
  case uMsg of
    WM_HOTKEY:
      begin
        // The manager is stored on the window itself: two managers can
        // coexist without sharing a global handle.
        Capture := TWin32HotkeyManager(GetWindowLongPtr(hw, GWL_USERDATA));
        if Capture <> nil then
        begin
          I := Capture.FindHotkeyByIndex(Longint(wp));

          if I > -1 then
          begin
            H := Capture[I];
            if Assigned(H.Notify) then
              H.Notify(Capture, H);
          end;
        end;
      end
  else
    Result := DefWindowProcW(hw, uMsg, wp, lp);
  end;
end;

{ TWin32HotkeyManager }

procedure TWin32HotkeyManager.SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
// Separate key and modifiers, so they can be used with RegisterHotKey
const
  VK2_META    =  16;
  VK2_SHIFT   =  32;
  VK2_CONTROL =  64;
  VK2_ALT     = 128;
  VK2_WIN     = 256;
var
  Virtuals: Integer;
  V: Word;
  x: Word;
begin
  Key := Byte(HotKey);
  x := HotKey shr 8;
  Virtuals := x;
  V := 0;
  if (Virtuals and VK2_META) <> 0 then
    Inc(V, MOD_WIN);
  if (Virtuals and VK2_WIN) <> 0 then
    Inc(V, MOD_WIN);
  if (Virtuals and VK2_ALT) <> 0 then
    Inc(V, MOD_ALT);
  if (Virtuals and VK2_CONTROL) <> 0 then
    Inc(V, MOD_CONTROL);
  if (Virtuals and VK2_SHIFT) <> 0 then
    Inc(V, MOD_SHIFT);
  Modifiers := V;
end;

function TWin32HotkeyManager.RegisterWindowClass: Boolean;
begin
  FillChar(FWindowClassInfo, SizeOf(FWindowClassInfo), 0);
  FWindowClassInfo.cbSize := SizeOf(FWindowClassInfo);
  FWindowClassInfo.Style := 0;
  FWindowClassInfo.lpfnWndProc := @WinProc;
  FWindowClassInfo.cbClsExtra := 0;
  FWindowClassInfo.cbWndExtra := 0;
  FWindowClassInfo.hInstance := hInstance;
  FWindowClassInfo.hIcon := 0;
  FWindowClassInfo.hCursor := 0;
  FWindowClassInfo.hbrBackground := 0;
  FWindowClassInfo.lpszMenuName := nil;
  FWindowClassInfo.lpszClassName := PChar(WinClassName);
  FWindowClassInfo.hIconSm := 0;
  Result := RegisterClassExW(FWindowClassInfo) <> 0;
  if not Result and (GetLastError = ERROR_CLASS_ALREADY_EXISTS) then
    Result := True; // already registered by another instance in this process
  if Result then
  begin
    FWindowClassRegistered := True;
    Inc(WindowClassRefCount);
  end;
end;

function TWin32HotkeyManager.CreateAppWindow: Boolean;
begin
  Result := False;

  if FWindow <> 0 then
    Exit(True);

  if not RegisterWindowClass then
    Exit;

  FWindow := CreateWindowExW(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
    PChar(WinClassName), PChar(WinClassName), WS_POPUP or WS_CLIPSIBLINGS,
    0, 0, 0, 0, 0, 0, hInstance, nil);

  if FWindow <> 0 then
  begin
    ShowWindow(FWindow, SW_HIDE);
    SetWindowLongPtr(FWindow, GWL_USERDATA, PtrInt(Self));
    UpdateWindow(FWindow);
    Result := True;
  end;
end;

function TWin32HotkeyManager.DoRegister(Shortcut: TShortCutEx): Boolean;
var
  Key, Modifiers: Word;
  AtomId: ATOM;
begin
  Result := False;

  if (Shortcut = nil) or (FWindow = 0) then
    Exit;

  SeparateHotKey(Shortcut.SimpleShortcut, Modifiers, Key);
  if Key = 0 then
    Exit;

  AtomId := GlobalAddAtomW(PChar(HotKeyAtomPrefix + IntToStr(Shortcut.SimpleShortcut)));
  if AtomId = 0 then
    Exit;

  Result := RegisterHotKey(FWindow, Longint(AtomId), Modifiers, Key);
  if Result then
    Shortcut.Index := AtomId
  else
    // Registration failed: do not leak the atom.
    GlobalDeleteAtom(AtomId);
end;

function TWin32HotkeyManager.DoUnregister(Shortcut: TShortCutEx): Boolean;
var
  AtomId: ATOM;
begin
  Result := False;

  if (Shortcut = nil) or (FWindow = 0) then
    Exit;

  AtomId := ATOM(Shortcut.Index);
  if AtomId = 0 then
    Exit(True); // nothing was registered for this item

  Result := UnRegisterHotkey(FWindow, Longint(AtomId));
  GlobalDeleteAtom(AtomId);
  Shortcut.Index := 0;
end;

constructor TWin32HotkeyManager.Create;
begin
  inherited Create;

  CreateAppWindow;
end;

destructor TWin32HotkeyManager.Destroy;
begin
  // Unregister the hotkeys while the message window is still alive, then
  // destroy the window and release the window class.
  inherited Destroy;

  if FWindow <> 0 then
  begin
    DestroyWindow(FWindow);
    FWindow := 0;
  end;

  if FWindowClassRegistered then
  begin
    FWindowClassRegistered := False;
    Dec(WindowClassRefCount);
    if WindowClassRefCount <= 0 then
    begin
      WindowClassRefCount := 0;
      UnregisterClassW(PChar(WinClassName), hInstance);
    end;
  end;
end;

function TWin32HotkeyManager.IsHotkeyAvailable(Shortcut: TShortCut): Boolean;
var
  Modifiers, Key: Word;
  AtomId: ATOM;
begin
  Result := False;

  if (Shortcut = 0) or (FWindow = 0) then
    Exit;

  SeparateHotKey(Shortcut, Modifiers, Key);
  if Key = 0 then
    Exit;

  AtomId := GlobalAddAtomW(PChar(HotKeyAtomPrefix + IntToStr(Shortcut)));
  if AtomId = 0 then
    Exit;

  try
    Result := RegisterHotKey(FWindow, Longint(AtomId), Modifiers, Key);
    if Result then
      UnRegisterHotkey(FWindow, Longint(AtomId));
  finally
    GlobalDeleteAtom(AtomId);
  end;
end;

end.
