{
Copyright (C) 2006-2026 Matteo Salvi

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

X11 global-hotkey primitives, kept separate from the platform manager so the
grab logic and the LCL<->X11 conversions can be read, tested and reasoned about
on their own.

XGrabKey is asynchronous: a shortcut already taken by another client is
reported through the X error handler (BadAccess), not through the return value.
TX11KeyGrabber therefore installs a temporary error handler around every batch
of grabs, calls XSync to force the error through, and rolls back the variants
already grabbed when one of them fails. The X error handler is process-global,
so all batches are serialized through a critical section and the previous
handler is always restored.
}

unit Hotkeys.Manager.X11;

{$I ASuiteComps.inc}

interface

{$IFDEF UNIX}

uses
  X, XLib, KeySym, ctypes, LCLType, Menus, Classes, SyncObjs, Hotkeys.ShortcutEx;

{
  X Key Modifiers:

  Mask        | Value | Key
  ------------+-------+------------
  ShiftMask   |     1 | Shift
  LockMask    |     2 | Caps Lock
  ControlMask |     4 | Ctrl
  Mod1Mask    |     8 | Alt
  Mod2Mask    |    16 | Num Lock
  Mod3Mask    |    32 | Scroll Lock
  Mod4Mask    |    64 | Windows
}
const
  X11AltMask   = Mod1Mask;
  X11SuperMask = Mod4Mask;
  X11CapLock   = LockMask;
  X11NumLock   = Mod2Mask;
  X11NotLock   = Integer(not (LockMask or Mod2Mask));

{ LCL <-> X11 conversions. Pure logic, no display needed. }
function X11ShiftToMod(ShiftState: TShiftState): Integer;
function X11ModToShift(Modifiers: Integer): TShiftState;
function X11KeyToSym(Key: Word): TKeySym;
function X11SymToKey(Sym: TKeySym): Word;

type
  { TX11KeyGrabber }

  { Owns no display: it operates on the display opened by the caller (the
    platform manager), which keeps the lifetime rules in one place. }
  TX11KeyGrabber = class
  private
    FDisplay: PDisplay;
    FLastError: Integer;
    function ResolveKeyCodes(Shortcut: TShortCut; out Code1, Code2: LongWord): Integer;
    function GrabKeyCode(KeyCode: LongWord; Modifier: LongWord;
      Window: TWindow): Boolean;
    function GrabVariants(KeyCode: LongWord; Modifier: LongWord;
      Window: TWindow): Boolean;
    procedure ReleaseVariants(KeyCode: LongWord; Modifier: LongWord;
      Window: TWindow);
  public
    constructor Create(ADisplay: PDisplay);
    function Valid: Boolean;

    { Grabs every CapsLock/NumLock variant of the shortcut. On any failure
      rolls back the variants already grabbed and returns False. }
    function Grab(Shortcut: TShortCut; Window: TWindow): Boolean;
    { Releases the same variants. Returns False if the X server reported an
      error while doing so. }
    function Ungrab(Shortcut: TShortCut; Window: TWindow): Boolean;
    { Grabs then immediately releases, to test whether the combination is
      free. Never touches any registry. }
    function IsAvailable(Shortcut: TShortCut; Window: TWindow): Boolean;

    { Error code reported by the last failing operation (0 when none). }
    property LastError: Integer read FLastError;
  end;

{$ENDIF}

implementation

{$IFDEF UNIX}

const
  { X protocol request codes for the operations we hook. }
  X_GrabKey_Request = 33;
  X_UngrabKey_Request = 34;

var
  XErrorLock: TCriticalSection;
  HookedXError: Integer;
  { Handler that was installed when our temporary one took over. Errors not
    caused by XGrabKey/XUngrabKey are passed through to it instead of being
    swallowed. }
  PreviousXErrorHandler: TXErrorHandler;

function X11ErrorHandler(para1: PDisplay; para2: PXErrorEvent): cint; cdecl;
begin
  if (para2^.request_code = X_GrabKey_Request)
    or (para2^.request_code = X_UngrabKey_Request) then
  begin
    HookedXError := para2^.error_code;
    Result := 0;
  end
  else if Assigned(PreviousXErrorHandler) then
    Result := PreviousXErrorHandler(para1, para2)
  else
    Result := 0;
end;

function X11ShiftToMod(ShiftState: TShiftState): Integer;
begin
  Result := 0;
  if ssShift in ShiftState then
    Result := Result or ShiftMask;
  if ssAlt in ShiftState then
    Result := Result or X11AltMask;
  if ssCtrl in ShiftState then
    Result := Result or ControlMask;
  if (ssSuper in ShiftState) or (ssMeta in ShiftState) then
    Result := Result or X11SuperMask;
end;

function X11ModToShift(Modifiers: Integer): TShiftState;
begin
  Result := [];
  if ShiftMask and Modifiers > 0 then
    Include(Result, ssShift);
  if X11AltMask and Modifiers > 0 then
    Include(Result, ssAlt);
  if ControlMask and Modifiers > 0 then
    Include(Result, ssCtrl);
  if X11SuperMask and Modifiers > 0 then
    Include(Result, ssMeta);
end;

function X11KeyToSym(Key: Word): TKeySym;
begin
  case Key of
    VK_TAB: Result := XK_TAB;
    VK_CLEAR: Result := XK_CLEAR;
    VK_RETURN: Result := XK_RETURN;
    VK_MENU: Result := XK_MENU;
    VK_ESCAPE: Result := XK_ESCAPE;
    VK_PAUSE: Result := XK_PAUSE;
    VK_SPACE: Result := XK_SPACE;
    VK_PRIOR: Result := XK_PRIOR;
    VK_NEXT: Result := XK_NEXT;
    VK_END: Result := XK_END;
    VK_HOME: Result := XK_HOME;
    VK_LEFT: Result := XK_LEFT;
    VK_UP: Result := XK_UP;
    VK_RIGHT: Result := XK_RIGHT;
    VK_DOWN: Result := XK_DOWN;
    VK_SELECT: Result := XK_SELECT;
    VK_EXECUTE: Result := XK_EXECUTE;
    VK_SNAPSHOT: Result := XK_PRINT;
    VK_INSERT: Result := XK_INSERT;
    VK_DELETE: Result := XK_DELETE;
    VK_HELP: Result := XK_HELP;
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;
    VK_A: Result := XK_A;
    VK_B: Result := XK_B;
    VK_C: Result := XK_C;
    VK_D: Result := XK_D;
    VK_E: Result := XK_E;
    VK_F: Result := XK_F;
    VK_G: Result := XK_G;
    VK_H: Result := XK_H;
    VK_I: Result := XK_I;
    VK_J: Result := XK_J;
    VK_K: Result := XK_K;
    VK_L: Result := XK_L;
    VK_M: Result := XK_M;
    VK_N: Result := XK_N;
    VK_O: Result := XK_O;
    VK_P: Result := XK_P;
    VK_Q: Result := XK_Q;
    VK_R: Result := XK_R;
    VK_S: Result := XK_S;
    VK_T: Result := XK_T;
    VK_U: Result := XK_U;
    VK_V: Result := XK_V;
    VK_W: Result := XK_W;
    VK_X: Result := XK_X;
    VK_Y: Result := XK_Y;
    VK_Z: Result := XK_Z;
    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_MULTIPLY;
    VK_ADD: Result := XK_KP_ADD;
    VK_SEPARATOR: Result := XK_KP_SEPARATOR;
    VK_SUBTRACT: Result := XK_KP_SUBTRACT;
    VK_DECIMAL: Result := XK_KP_DECIMAL;
    VK_DIVIDE: Result := XK_KP_DIVIDE;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_F13: Result := XK_F13;
    VK_F14: Result := XK_F14;
    VK_F15: Result := XK_F15;
    VK_F16: Result := XK_F16;
    VK_F17: Result := XK_F17;
    VK_F18: Result := XK_F18;
    VK_F19: Result := XK_F19;
    VK_F20: Result := XK_F20;
    VK_F21: Result := XK_F21;
    VK_F22: Result := XK_F22;
    VK_F23: Result := XK_F23;
    VK_F24: Result := XK_F24;
    VK_LCL_EQUAL: Result := XK_EQUAL;
    VK_LCL_COMMA: Result := XK_COMMA;
    VK_LCL_POINT: Result := XK_PERIOD;
    VK_LCL_SLASH: Result := XK_SLASH;
    VK_LCL_SEMI_COMMA: Result := XK_SEMICOLON;
    VK_LCL_MINUS: Result := XK_MINUS;
    VK_LCL_OPEN_BRACKET: Result := XK_BRACKETLEFT;
    VK_LCL_CLOSE_BRACKET: Result := XK_BRACKETRIGHT;
    VK_LCL_BACKSLASH: Result := XK_BACKSLASH;
    VK_LCL_TILDE: Result := XK_GRAVE;
    VK_LCL_QUOTE: Result := XK_SINGLELOWQUOTEMARK;
  else
    Result := 0;
  end;
end;

function X11SymToKey(Sym: TKeySym): Word;
begin
  case Sym of
    XK_TAB: Result := VK_TAB;
    XK_CLEAR: Result := VK_CLEAR;
    XK_RETURN: Result := VK_RETURN;
    XK_MENU: Result := VK_MENU;
    XK_ESCAPE: Result := VK_ESCAPE;
    XK_PAUSE: Result := VK_PAUSE;
    XK_SPACE: Result := VK_SPACE;
    XK_PRIOR: Result := VK_PRIOR;
    XK_NEXT: Result := VK_NEXT;
    XK_END: Result := VK_END;
    XK_HOME: Result := VK_HOME;
    XK_LEFT: Result := VK_LEFT;
    XK_UP: Result := VK_UP;
    XK_RIGHT: Result := VK_RIGHT;
    XK_DOWN: Result := VK_DOWN;
    XK_SELECT: Result := VK_SELECT;
    XK_EXECUTE: Result := VK_EXECUTE;
    XK_PRINT: Result := VK_SNAPSHOT;
    XK_INSERT: Result := VK_INSERT;
    XK_DELETE: Result := VK_DELETE;
    XK_HELP: Result := VK_HELP;
    XK_0: Result := VK_0;
    XK_1: Result := VK_1;
    XK_2: Result := VK_2;
    XK_3: Result := VK_3;
    XK_4: Result := VK_4;
    XK_5: Result := VK_5;
    XK_6: Result := VK_6;
    XK_7: Result := VK_7;
    XK_8: Result := VK_8;
    XK_9: Result := VK_9;
    XK_A: Result := VK_A;
    XK_B: Result := VK_B;
    XK_C: Result := VK_C;
    XK_D: Result := VK_D;
    XK_E: Result := VK_E;
    XK_F: Result := VK_F;
    XK_G: Result := VK_G;
    XK_H: Result := VK_H;
    XK_I: Result := VK_I;
    XK_J: Result := VK_J;
    XK_K: Result := VK_K;
    XK_L: Result := VK_L;
    XK_M: Result := VK_M;
    XK_N: Result := VK_N;
    XK_O: Result := VK_O;
    XK_P: Result := VK_P;
    XK_Q: Result := VK_Q;
    XK_R: Result := VK_R;
    XK_S: Result := VK_S;
    XK_T: Result := VK_T;
    XK_U: Result := VK_U;
    XK_V: Result := VK_V;
    XK_W: Result := VK_W;
    XK_X: Result := VK_X;
    XK_Y: Result := VK_Y;
    XK_Z: Result := VK_Z;
    XK_KP_0: Result := VK_NUMPAD0;
    XK_KP_1: Result := VK_NUMPAD1;
    XK_KP_2: Result := VK_NUMPAD2;
    XK_KP_3: Result := VK_NUMPAD3;
    XK_KP_4: Result := VK_NUMPAD4;
    XK_KP_5: Result := VK_NUMPAD5;
    XK_KP_6: Result := VK_NUMPAD6;
    XK_KP_7: Result := VK_NUMPAD7;
    XK_KP_8: Result := VK_NUMPAD8;
    XK_KP_9: Result := VK_NUMPAD9;
    XK_KP_MULTIPLY: Result := VK_MULTIPLY;
    XK_KP_ADD: Result := VK_ADD;
    XK_KP_SEPARATOR: Result := VK_SEPARATOR;
    XK_KP_SUBTRACT: Result := VK_SUBTRACT;
    XK_KP_DECIMAL: Result := VK_DECIMAL;
    XK_KP_DIVIDE: Result := VK_DIVIDE;
    XK_F1: Result := VK_F1;
    XK_F2: Result := VK_F2;
    XK_F3: Result := VK_F3;
    XK_F4: Result := VK_F4;
    XK_F5: Result := VK_F5;
    XK_F6: Result := VK_F6;
    XK_F7: Result := VK_F7;
    XK_F8: Result := VK_F8;
    XK_F9: Result := VK_F9;
    XK_F10: Result := VK_F10;
    XK_F11: Result := VK_F11;
    XK_F12: Result := VK_F12;
    XK_F13: Result := VK_F13;
    XK_F14: Result := VK_F14;
    XK_F15: Result := VK_F15;
    XK_F16: Result := VK_F16;
    XK_F17: Result := VK_F17;
    XK_F18: Result := VK_F18;
    XK_F19: Result := VK_F19;
    XK_F20: Result := VK_F20;
    XK_F21: Result := VK_F21;
    XK_F22: Result := VK_F22;
    XK_F23: Result := VK_F23;
    XK_F24: Result := VK_F24;
    XK_EQUAL: Result := VK_LCL_EQUAL;
    XK_COMMA: Result := VK_LCL_COMMA;
    XK_PERIOD: Result := VK_LCL_POINT;
    XK_SLASH: Result := VK_LCL_SLASH;
    XK_SEMICOLON: Result := VK_LCL_SEMI_COMMA;
    XK_MINUS: Result := VK_LCL_MINUS;
    XK_BRACKETLEFT: Result := VK_LCL_OPEN_BRACKET;
    XK_BRACKETRIGHT: Result := VK_LCL_CLOSE_BRACKET;
    XK_BACKSLASH: Result := VK_LCL_BACKSLASH;
    XK_GRAVE: Result := VK_LCL_TILDE;
    XK_SINGLELOWQUOTEMARK: Result := VK_LCL_QUOTE;
  else
    Result := 0;
  end;
end;

{ TX11KeyGrabber }

constructor TX11KeyGrabber.Create(ADisplay: PDisplay);
begin
  inherited Create;
  FDisplay := ADisplay;
  FLastError := 0;
end;

function TX11KeyGrabber.Valid: Boolean;
begin
  Result := FDisplay <> nil;
end;

function TX11KeyGrabber.ResolveKeyCodes(Shortcut: TShortCut;
  out Code1, Code2: LongWord): Integer;
var
  Key: Word;
  ShiftState: TShiftState;
  KeySym, ShiftSym: TKeySym;
begin
  Code1 := 0;
  Code2 := 0;
  Result := 0;
  if FDisplay = nil then
    Exit;

  ShortCutToKey(Shortcut, Key, ShiftState);
  if Key = 0 then
    Exit;

  KeySym := X11KeyToSym(Key);
  if KeySym = 0 then
    Exit;

  Code1 := XKeysymToKeycode(FDisplay, KeySym);
  if Code1 = 0 then
    Exit; // key not present in the current keyboard mapping
  Result := 1;

  // A shifted layout maps the same physical key to a second KeySym; grab it
  // too so the shortcut also fires with Shift held.
  ShiftSym := XKeycodeToKeysym(FDisplay, Code1, 1);
  if (ShiftSym <> 0) and (KeySym <> ShiftSym) then
  begin
    Code2 := XKeysymToKeycode(FDisplay, ShiftSym);
    if Code2 <> 0 then
      Result := 2;
  end;
end;

function TX11KeyGrabber.GrabKeyCode(KeyCode: LongWord; Modifier: LongWord;
  Window: TWindow): Boolean;
begin
  HookedXError := 0;
  XGrabKey(FDisplay, KeyCode, Modifier, Window, 1, GrabModeAsync, GrabModeAsync);
  XSync(FDisplay, False);
  if HookedXError <> 0 then
  begin
    // Keep the first error of the whole operation: a later variant may
    // succeed, but the operation as a whole still failed.
    if FLastError = 0 then
      FLastError := HookedXError;
    Exit(False);
  end;
  Result := True;
end;

function TX11KeyGrabber.GrabVariants(KeyCode: LongWord; Modifier: LongWord;
  Window: TWindow): Boolean;
begin
  // Evaluate every variant (no short-circuit) so a partial success can be
  // rolled back completely by the caller.
  Result := True;
  Result := GrabKeyCode(KeyCode, Modifier and X11NotLock, Window) and Result;
  Result := GrabKeyCode(KeyCode, Modifier or X11CapLock, Window) and Result;
  Result := GrabKeyCode(KeyCode, Modifier or X11NumLock, Window) and Result;
  Result := GrabKeyCode(KeyCode, Modifier or X11CapLock or X11NumLock, Window) and Result;
end;

procedure TX11KeyGrabber.ReleaseVariants(KeyCode: LongWord;
  Modifier: LongWord; Window: TWindow);
begin
  // Best-effort release; the caller ignores errors during rollback.
  XUngrabKey(FDisplay, KeyCode, Modifier and X11NotLock, Window);
  XUngrabKey(FDisplay, KeyCode, Modifier or X11CapLock, Window);
  XUngrabKey(FDisplay, KeyCode, Modifier or X11NumLock, Window);
  XUngrabKey(FDisplay, KeyCode, Modifier or X11CapLock or X11NumLock, Window);
end;

function TX11KeyGrabber.Grab(Shortcut: TShortCut; Window: TWindow): Boolean;
var
  Code1, Code2: LongWord;
  Modifier: LongWord;
  Key: Word;
  ShiftState: TShiftState;
  Count: Integer;
  OldHandler: TXErrorHandler;
begin
  Result := False;
  if FDisplay = nil then
    Exit;

  Count := ResolveKeyCodes(Shortcut, Code1, Code2);
  if Count = 0 then
    Exit;

  ShortCutToKey(Shortcut, Key, ShiftState);
  Modifier := X11ShiftToMod(ShiftState);

  XErrorLock.Acquire;
  try
    HookedXError := 0;
    FLastError := 0;
    OldHandler := XSetErrorHandler(@X11ErrorHandler);
    PreviousXErrorHandler := OldHandler;
    try
      Result := GrabVariants(Code1, Modifier, Window);
      if Result and (Count = 2) then
        Result := GrabVariants(Code2, Modifier, Window);
      if not Result then
      begin
        // Roll back everything: a grab that failed halfway must not leave a
        // partial (e.g. CapsLock-only) binding behind.
        if Count = 2 then
          ReleaseVariants(Code2, Modifier, Window);
        ReleaseVariants(Code1, Modifier, Window);
        XSync(FDisplay, False);
      end;
    finally
      XSetErrorHandler(OldHandler);
      PreviousXErrorHandler := nil;
    end;
  finally
    XErrorLock.Release;
  end;
end;

function TX11KeyGrabber.Ungrab(Shortcut: TShortCut; Window: TWindow): Boolean;
var
  Code1, Code2: LongWord;
  Modifier: LongWord;
  Key: Word;
  ShiftState: TShiftState;
  Count: Integer;
  OldHandler: TXErrorHandler;
begin
  Result := False;
  if FDisplay = nil then
    Exit;

  Count := ResolveKeyCodes(Shortcut, Code1, Code2);
  if Count = 0 then
    Exit;

  ShortCutToKey(Shortcut, Key, ShiftState);
  Modifier := X11ShiftToMod(ShiftState);

  XErrorLock.Acquire;
  try
    HookedXError := 0;
    FLastError := 0;
    OldHandler := XSetErrorHandler(@X11ErrorHandler);
    PreviousXErrorHandler := OldHandler;
    try
      ReleaseVariants(Code1, Modifier, Window);
      if Count = 2 then
        ReleaseVariants(Code2, Modifier, Window);
      XSync(FDisplay, False);
      if HookedXError <> 0 then
      begin
        FLastError := HookedXError;
        Exit(False);
      end;
      Result := True;
    finally
      XSetErrorHandler(OldHandler);
      PreviousXErrorHandler := nil;
    end;
  finally
    XErrorLock.Release;
  end;
end;

function TX11KeyGrabber.IsAvailable(Shortcut: TShortCut; Window: TWindow): Boolean;
begin
  // A probe grab is the only way to know: grabbing a key already taken by
  // another client raises BadAccess asynchronously. The probe is only
  // "available" when both the grab and its release succeeded, so a failed
  // release never leaves a stray grab behind while reporting success.
  Result := Grab(Shortcut, Window);
  if Result then
    Result := Ungrab(Shortcut, Window);
end;

initialization
  XErrorLock := TCriticalSection.Create;

finalization
  XErrorLock.Free;

{$ENDIF}

end.
