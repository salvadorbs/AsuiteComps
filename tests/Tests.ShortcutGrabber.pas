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
    procedure TestDefaultsCaptions;
    procedure TestFormLoadsEmbeddedImages;
    procedure TestFormHotkeySetsKeyControl;
    procedure TestFormModifierButtons;
    procedure TestFormClearsModifiers;
    procedure TestFormImageFromPicture;
    procedure TestFormImageFromDefaults;
    procedure TestFormMessages;
    procedure TestFormNoModifierAllowsBareKey;
    procedure TestFormTargetHotKeyInit;
    procedure TestTargetHotKeyConfirmUpdates;
    procedure TestTargetHotKeyCancelKeeps;
    procedure TestTargetHotKeyClearedOnDestroy;
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

procedure TTestShortcutGrabber.TestDefaultsCaptions;
var
  Form: TfrmShortcutGrabber;
  OldCaption, OldInfo, OldOk, OldCancel: string;
begin
  AssertTrue('Caption', ShortcutGrabberDefaults.Caption <> '');
  AssertTrue('InfoText', ShortcutGrabberDefaults.InfoText <> '');
  AssertTrue('OkCaption', ShortcutGrabberDefaults.OkCaption <> '');
  AssertTrue('CancelCaption', ShortcutGrabberDefaults.CancelCaption <> '');

  OldCaption := ShortcutGrabberDefaults.Caption;
  OldInfo := ShortcutGrabberDefaults.InfoText;
  OldOk := ShortcutGrabberDefaults.OkCaption;
  OldCancel := ShortcutGrabberDefaults.CancelCaption;
  try
    { The form applies the defaults at creation, so an application only has to
      set ShortcutGrabberDefaults once. }
    ShortcutGrabberDefaults.Caption := 'Pick';
    ShortcutGrabberDefaults.InfoText := 'Info';
    ShortcutGrabberDefaults.OkCaption := 'Yes';
    ShortcutGrabberDefaults.CancelCaption := 'No';
    Form := TfrmShortcutGrabber.Create(nil);
    try
      AssertEquals('Form caption', 'Pick', Form.Caption);
      AssertEquals('Info label', 'Info', Form.lblInfo.Caption);
      AssertEquals('Ok button', 'Yes', Form.btnOk.Caption);
      AssertEquals('Cancel button', 'No', Form.btnCancel.Caption);

      { Per-instance overrides win over the defaults. }
      Form.InfoText := 'Local info';
      AssertEquals('Per-instance override', 'Local info', Form.InfoText);
    finally
      Form.Free;
    end;
  finally
    ShortcutGrabberDefaults.Caption := OldCaption;
    ShortcutGrabberDefaults.InfoText := OldInfo;
    ShortcutGrabberDefaults.OkCaption := OldOk;
    ShortcutGrabberDefaults.CancelCaption := OldCancel;
  end;
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

procedure TTestShortcutGrabber.TestFormNoModifierAllowsBareKey;
var
  Form: TfrmShortcutGrabber;
begin
  { With NoModifier a bare key is accepted instead of showing the "no modifier"
    warning (which would block on MessageDlg). }
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Form.NoModifier := True;
    Form.hkKeys.Hotkey := ShortCut(VK_F5, []);
    Form.btnOkClick(Form);
    AssertEquals('Bare key accepted', Integer(ShortCut(VK_F5, [])),
      Integer(Form.Hotkey));
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

procedure TTestShortcutGrabber.TestTargetHotKeyConfirmUpdates;
var
  Form: TfrmShortcutGrabber;
  Control: THotKey;
begin
  Control := THotKey.Create(nil);
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Control.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    Form.TargetHotKey := Control;

    //Simulate choosing Shift+F9 in the dialog and confirming
    Form.hkKeys.Hotkey := ShortCut(VK_F9, [ssShift]);
    Form.btnOkClick(nil);

    AssertEquals('Target updated',
      Integer(ShortCut(VK_F9, [ssShift])), Integer(Control.Hotkey));
  finally
    Form.Free;
    Control.Free;
  end;
end;

procedure TTestShortcutGrabber.TestTargetHotKeyCancelKeeps;
var
  Form: TfrmShortcutGrabber;
  Control: THotKey;
begin
  Control := THotKey.Create(nil);
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Control.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    Form.TargetHotKey := Control;

    Form.hkKeys.Hotkey := ShortCut(VK_F9, [ssShift]);
    Form.btnCancelClick(nil);

    AssertEquals('Target unchanged',
      Integer(ShortCut(VK_F5, [ssCtrl])), Integer(Control.Hotkey));
  finally
    Form.Free;
    Control.Free;
  end;
end;

procedure TTestShortcutGrabber.TestTargetHotKeyClearedOnDestroy;
var
  Form: TfrmShortcutGrabber;
  Control: THotKey;
begin
  Control := THotKey.Create(nil);
  Form := TfrmShortcutGrabber.Create(nil);
  try
    Form.TargetHotKey := Control;
    Control.Free;
    Control := nil;

    AssertTrue('Target cleared', Form.TargetHotKey = nil);
  finally
    Form.Free;
    Control.Free;
  end;
end;

initialization
  RegisterTest(TTestShortcutGrabber);

end.
