unit Tests.ShortcutGrabber;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Forms, Graphics, Menus, LCLType,
  LResources, ShortcutGrabber, HotKey;

type

  { TTestShortcutGrabber }

  TTestShortcutGrabber = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestImagesEmptyByDefault;
    procedure TestImagesAssignAndClear;
    procedure TestDefaultsMessages;
    procedure TestFormLoadsEmbeddedImages;
    procedure TestFormHotkeySetsKeyControl;
    procedure TestFormModifierButtons;
    procedure TestFormClearsModifiers;
    procedure TestFormImageFromPicture;
    procedure TestFormImageFromDefaults;
    procedure TestFormMessages;
    procedure TestFormTargetHotKeyInit;
  end;

implementation

var
  LCLInitialized: Boolean = False;

procedure TTestShortcutGrabber.SetUp;
begin
  inherited SetUp;
  if not LCLInitialized then
  begin
    Application.Initialize;
    LCLInitialized := True;
  end;

  { Isolate tests from process-wide defaults }
  ShortcutGrabberDefaults.Images.Clear;
end;

procedure TTestShortcutGrabber.TestImagesEmptyByDefault;
var
  Img: TShortcutGrabberImages;
begin
  Img := TShortcutGrabberImages.Create;
  try
    AssertTrue('IsEmpty', Img.IsEmpty);
    AssertTrue('Ctrl empty', (Img.Ctrl.Graphic = nil) or Img.Ctrl.Graphic.Empty);
    AssertTrue('Alt empty', (Img.Alt.Graphic = nil) or Img.Alt.Graphic.Empty);
    AssertTrue('Shift empty', (Img.Shift.Graphic = nil) or Img.Shift.Graphic.Empty);
    AssertTrue('WinKey empty', (Img.WinKey.Graphic = nil) or Img.WinKey.Graphic.Empty);
  finally
    Img.Free;
  end;
end;

procedure TTestShortcutGrabber.TestImagesAssignAndClear;
var
  A, B: TShortcutGrabberImages;
  Bmp: TBitmap;
begin
  A := TShortcutGrabberImages.Create;
  B := TShortcutGrabberImages.Create;
  try
    Bmp := TBitmap.Create;
    try
      Bmp.SetSize(4, 4);
      Bmp.Canvas.Brush.Color := clRed;
      Bmp.Canvas.FillRect(0, 0, 4, 4);
      A.Ctrl.Assign(Bmp);
    finally
      Bmp.Free;
    end;

    AssertFalse('A not empty', A.IsEmpty);
    B.Assign(A);
    AssertFalse('B not empty', B.IsEmpty);
    AssertFalse('B.Ctrl not empty',
      (B.Ctrl.Graphic = nil) or B.Ctrl.Graphic.Empty);

    B.Clear;
    AssertTrue('Empty after Clear', B.IsEmpty);
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TTestShortcutGrabber.TestDefaultsMessages;
begin
  AssertTrue('Defaults created', ShortcutGrabberDefaults <> nil);
  AssertTrue('MessageNoKey', ShortcutGrabberDefaults.MessageNoKey <> '');
  AssertTrue('MessageNoModifier', ShortcutGrabberDefaults.MessageNoModifier <> '');
  AssertTrue('MessageNotAvailable', ShortcutGrabberDefaults.MessageNotAvailable <> '');
end;

procedure TTestShortcutGrabber.TestFormLoadsEmbeddedImages;
var
  Form: TfrmShortcutGrabber;
begin
  Form := TfrmShortcutGrabber.Create(nil);
  try
    AssertTrue('Ctrl bitmap', Form.btnCtrl.BitmapOptions.Bitmap <> nil);
    AssertTrue('Alt bitmap', Form.btnAlt.BitmapOptions.Bitmap <> nil);
    AssertTrue('Shift bitmap', Form.btnShift.BitmapOptions.Bitmap <> nil);
    AssertTrue('WinKey bitmap', Form.btnWinKey.BitmapOptions.Bitmap <> nil);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormHotkeySetsKeyControl;
var
  Form: TfrmShortcutGrabber;
  Key: Word;
  Mods: TShiftState;
begin
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Form.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    ShortCutToKey(Form.hkKeys.Hotkey, Key, Mods);
    AssertEquals('Key', Integer(VK_F5), Integer(Key));
    AssertTrue('Ctrl pressed', Form.btnCtrl.Pressed);
    AssertFalse('Alt not pressed', Form.btnAlt.Pressed);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormModifierButtons;
var
  Form: TfrmShortcutGrabber;
begin
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Form.Hotkey := ShortCut(VK_F9, [ssCtrl, ssAlt, ssShift, ssMeta]);
    AssertTrue('Ctrl', Form.btnCtrl.Pressed);
    AssertTrue('Alt', Form.btnAlt.Pressed);
    AssertTrue('Shift', Form.btnShift.Pressed);
    AssertTrue('Meta', Form.btnWinKey.Pressed);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormClearsModifiers;
var
  Form: TfrmShortcutGrabber;
begin
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Form.Hotkey := ShortCut(VK_F9, [ssCtrl]);
    AssertTrue('Ctrl pressed', Form.btnCtrl.Pressed);

    Form.Hotkey := ShortCut(VK_F9, []);
    AssertFalse('Ctrl cleared', Form.btnCtrl.Pressed);
    AssertFalse('Alt cleared', Form.btnAlt.Pressed);
    AssertFalse('Shift cleared', Form.btnShift.Pressed);
    AssertFalse('Meta cleared', Form.btnWinKey.Pressed);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormImageFromPicture;
var
  Form: TfrmShortcutGrabber;
  Bmp: TBitmap;
begin
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Bmp := TBitmap.Create;
    try
      Bmp.SetSize(8, 8);
      Bmp.Canvas.Brush.Color := clRed;
      Bmp.Canvas.FillRect(0, 0, 8, 8);
      Form.Images.Ctrl.Assign(Bmp);
    finally
      Bmp.Free;
    end;

    Form.LoadImages;
    AssertTrue('Ctrl bitmap from picture', Form.btnCtrl.BitmapOptions.Bitmap <> nil);
    AssertTrue('Custom picture used', Form.Images.IsEmpty = False);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormImageFromDefaults;
var
  Form: TfrmShortcutGrabber;
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(8, 8);
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(0, 0, 8, 8);
    ShortcutGrabberDefaults.Images.Ctrl.Assign(Bmp);
  finally
    Bmp.Free;
  end;

  Form := TfrmShortcutGrabber.Create(nil);
  try
    AssertTrue('Ctrl bitmap from defaults', Form.btnCtrl.BitmapOptions.Bitmap <> nil);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormMessages;
var
  Form: TfrmShortcutGrabber;
begin
  Form := TfrmShortcutGrabber.Create(nil);
  try
    AssertEquals('Fallback to defaults',
      ShortcutGrabberDefaults.MessageNoKey, Form.MessageNoKey);

    Form.MessageNoKey := 'custom no key';
    AssertEquals('Override', 'custom no key', Form.MessageNoKey);
  finally
    Form.Free;
  end;
end;

procedure TTestShortcutGrabber.TestFormTargetHotKeyInit;
var
  Form: TfrmShortcutGrabber;
  Control: THotKey;
begin
  Control := THotKey.Create(nil);
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Control.Hotkey := ShortCut(VK_F7, [ssCtrl, ssAlt]);
    Form.TargetHotKey := Control;

    AssertEquals('Hotkey from control',
      Integer(Control.Hotkey), Integer(Form.Hotkey));
    AssertTrue('Ctrl pressed', Form.btnCtrl.Pressed);
    AssertTrue('Alt pressed', Form.btnAlt.Pressed);
  finally
    Form.Free;
    Control.Free;
  end;
end;

initialization
  RegisterTest(TTestShortcutGrabber);

end.
