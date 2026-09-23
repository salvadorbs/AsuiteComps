unit Tests.HotkeyControl;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Forms, Menus, LCLType,
  HotKey;

type

  { Exposes protected behavior for testing }

  THotKeyCracker = class(THotKey)
  public
    procedure CallDoEnter;
    procedure CallKeyUp(var Key: Word; Shift: TShiftState);
  end;

  { TTestHotkeyControl }

  TTestHotkeyControl = class(TTestCase)
  private
    FChanged: Integer;
    procedure OnChanged(Sender: TObject);
  protected
    procedure SetUp; override;
  published
    procedure TestDefaults;
    procedure TestHotkeyRoundTrip;
    procedure TestPlainKeyGetsCtrl;
    procedure TestNoModifierKeepsPlainKey;
    procedure TestShiftKept;
    procedure TestBackspaceClears;
    procedure TestDeleteClears;
    procedure TestOnChangeFired;
    procedure TestOnChangeNotFiredWithoutInput;
    procedure TestHotkeySetterNotifies;
    procedure TestHotkeySetterUnchanged;
    { TShortcutCapture (non-visual engine) }
    procedure TestCapturePlainKeyGetsCtrl;
    procedure TestCaptureNoModifierKeepsPlainKey;
    procedure TestCaptureClearKey;
    procedure TestCaptureShiftKept;
  end;

implementation

var
  LCLInitialized: Boolean = False;

{ THotKeyCracker }

procedure THotKeyCracker.CallDoEnter;
begin
  inherited DoEnter;
end;

procedure THotKeyCracker.CallKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;

{ TTestHotkeyControl }

procedure TTestHotkeyControl.OnChanged(Sender: TObject);
begin
  Inc(FChanged);
end;

procedure TTestHotkeyControl.SetUp;
begin
  inherited SetUp;
  if not LCLInitialized then
  begin
    Application.Initialize;
    LCLInitialized := True;
  end;
  FChanged := 0;
end;

procedure TTestHotkeyControl.TestDefaults;
var
  H: THotKeyCracker;
begin
  H := THotKeyCracker.Create(nil);
  try
    AssertEquals('Default Hotkey', 0, Integer(H.Hotkey));
    AssertFalse('Default NoModifier', H.NoModifier);
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestHotkeyRoundTrip;
var
  H: THotKeyCracker;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Hotkey', Integer(ShortCut(VK_F5, [ssCtrl])), Integer(H.Hotkey));
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestPlainKeyGetsCtrl;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.CallDoEnter;
    Key := VK_A;
    H.CallKeyUp(Key, []);
    AssertEquals('Ctrl added', Integer(ShortCut(VK_A, [ssCtrl])), Integer(H.Hotkey));
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestNoModifierKeepsPlainKey;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.NoModifier := True;
    H.CallDoEnter;
    Key := VK_F5;
    H.CallKeyUp(Key, []);
    AssertEquals('Plain key kept', Integer(ShortCut(VK_F5, [])), Integer(H.Hotkey));
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestShiftKept;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.CallDoEnter;
    Key := VK_F5;
    H.CallKeyUp(Key, [ssShift]);
    AssertEquals('Shift kept', Integer(ShortCut(VK_F5, [ssShift])), Integer(H.Hotkey));
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestBackspaceClears;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.Hotkey := ShortCut(VK_A, [ssCtrl]);
    H.CallDoEnter;
    Key := VK_BACK;
    H.CallKeyUp(Key, []);
    AssertEquals('Cleared', 0, Integer(H.Hotkey));
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestDeleteClears;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.Hotkey := ShortCut(VK_A, [ssCtrl]);
    H.CallDoEnter;
    Key := VK_DELETE;
    H.CallKeyUp(Key, []);
    AssertEquals('Cleared', 0, Integer(H.Hotkey));
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestOnChangeFired;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.OnChange := @OnChanged;
    H.CallDoEnter;
    Key := VK_A;
    H.CallKeyUp(Key, []);
    AssertEquals('OnChange fired once', 1, FChanged);
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestHotkeySetterNotifies;
var
  H: THotKeyCracker;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.OnChange := @OnChanged;
    H.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Hotkey set', Integer(ShortCut(VK_F5, [ssCtrl])), Integer(H.Hotkey));
    AssertEquals('OnChange fired', 1, FChanged);
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestHotkeySetterUnchanged;
var
  H: THotKeyCracker;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    H.OnChange := @OnChanged;
    H.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('OnChange not fired for same value', 0, FChanged);
  finally
    H.Free;
  end;
end;

procedure TTestHotkeyControl.TestCapturePlainKeyGetsCtrl;
var
  C: TShortcutCapture;
begin
  C := TShortcutCapture.Create;
  try
    C.Capture(VK_A, []);
    AssertEquals('Ctrl added', Integer(ShortCut(VK_A, [ssCtrl])), Integer(C.Hotkey));
  finally
    C.Free;
  end;
end;

procedure TTestHotkeyControl.TestCaptureNoModifierKeepsPlainKey;
var
  C: TShortcutCapture;
begin
  C := TShortcutCapture.Create;
  try
    C.NoModifier := True;
    C.Capture(VK_F5, []);
    AssertEquals('Plain key kept', Integer(ShortCut(VK_F5, [])), Integer(C.Hotkey));
  finally
    C.Free;
  end;
end;

procedure TTestHotkeyControl.TestCaptureClearKey;
var
  C: TShortcutCapture;
begin
  C := TShortcutCapture.Create;
  try
    C.Hotkey := ShortCut(VK_A, [ssCtrl]);
    C.Capture(VK_BACK, []);
    AssertEquals('Cleared', 0, Integer(C.Hotkey));
  finally
    C.Free;
  end;
end;

procedure TTestHotkeyControl.TestCaptureShiftKept;
var
  C: TShortcutCapture;
begin
  C := TShortcutCapture.Create;
  try
    C.Capture(VK_F5, [ssShift]);
    AssertEquals('Shift kept', Integer(ShortCut(VK_F5, [ssShift])), Integer(C.Hotkey));
  finally
    C.Free;
  end;
end;

procedure TTestHotkeyControl.TestOnChangeNotFiredWithoutInput;
var
  H: THotKeyCracker;
  Key: Word;
begin
  H := THotKeyCracker.Create(nil);
  try
    H.OnChange := @OnChanged;
    { No DoEnter -> control does not accept input -> KeyUp ignored }
    Key := VK_A;
    H.CallKeyUp(Key, []);
    AssertEquals('Hotkey untouched', 0, Integer(H.Hotkey));
    AssertEquals('OnChange not fired', 0, FChanged);
  finally
    H.Free;
  end;
end;

initialization
  RegisterTest(TTestHotkeyControl);

end.
