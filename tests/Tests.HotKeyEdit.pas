unit Tests.HotKeyEdit;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Forms, Controls, StdCtrls, Menus,
  LCLProc, LCLType, ImgList, HotKeyEdit;

type

  { TTestHotKeyEdit }

  TTestHotKeyEdit = class(TTestCase)
  private
    FChanged: Integer;
    procedure OnHotkeyChanged(Sender: TObject);
    function FindInnerEdit(AE: THotKeyEdit): TEdit;
    function RejectCtrlQ(AShortcut: TShortCut): Boolean;
  protected
    procedure SetUp; override;
  published
    procedure TestReadOnlyByDefault;
    procedure TestHotkeyRoundTrip;
    procedure TestTextParsesHotkey;
    procedure TestClearHotkey;
    procedure TestOnHotkeyChangeFired;
    procedure TestNoChangeNoEvent;
    procedure TestButtonAlwaysVisible;
    procedure TestButtonOnlyWithHotkey;
    procedure TestDefaultIcons;
    procedure TestButtonImageIndex;
    procedure TestButtonImageIndexReset;
    procedure TestNoModifier;
    procedure TestUseDefaultImages;
    procedure TestInlineCapture;
    procedure TestInlineCaptureKeepsProgrammaticHotkey;
    procedure TestInlineCaptureValidator;
    procedure TestCustomImagesDeriveWidth;
    procedure TestExplicitImagesWidthPreserved;
  end;

implementation

var
  LCLInitialized: Boolean = False;

{ TTestHotKeyEdit }

procedure TTestHotKeyEdit.OnHotkeyChanged(Sender: TObject);
begin
  Inc(FChanged);
end;

function TTestHotKeyEdit.FindInnerEdit(AE: THotKeyEdit): TEdit;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AE.ControlCount - 1 do
    if AE.Controls[I] is TEdit then
      Exit(TEdit(AE.Controls[I]));
end;

function TTestHotKeyEdit.RejectCtrlQ(AShortcut: TShortCut): Boolean;
begin
  Result := AShortcut <> ShortCut(VK_Q, [ssCtrl]);
end;

procedure TTestHotKeyEdit.SetUp;
begin
  inherited SetUp;
  if not LCLInitialized then
  begin
    Application.Initialize;
    LCLInitialized := True;
  end;
  FChanged := 0;
end;

procedure TTestHotKeyEdit.TestReadOnlyByDefault;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    AssertTrue('ReadOnly', E.ReadOnly);
    AssertEquals('Empty hotkey', 0, Integer(E.Hotkey));
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestHotkeyRoundTrip;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Hotkey', Integer(ShortCut(VK_F5, [ssCtrl])), Integer(E.Hotkey));
    AssertEquals('Text', ShortCutToText(ShortCut(VK_F5, [ssCtrl])), E.Text);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestTextParsesHotkey;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.Text := 'Ctrl+F5';
    AssertEquals('Hotkey from text', Integer(ShortCut(VK_F5, [ssCtrl])), Integer(E.Hotkey));
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestClearHotkey;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    E.ClearHotkey;
    AssertEquals('Cleared', 0, Integer(E.Hotkey));
    AssertEquals('Text cleared', '', E.Text);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestOnHotkeyChangeFired;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.OnHotkeyChange := @OnHotkeyChanged;
    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Fired once', 1, FChanged);

    E.ClearHotkey;
    AssertEquals('Fired again', 2, FChanged);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestNoChangeNoEvent;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    E.OnHotkeyChange := @OnHotkeyChanged;
    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('No event for same value', 0, FChanged);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestButtonAlwaysVisible;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    AssertFalse('Default ButtonVisibleOnlyWithHotkey', E.ButtonVisibleOnlyWithHotkey);
    AssertTrue('Visible when empty', E.RightButton.Visible);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestButtonOnlyWithHotkey;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.ButtonVisibleOnlyWithHotkey := True;
    AssertFalse('Hidden when empty', E.RightButton.Visible);

    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertTrue('Visible when set', E.RightButton.Visible);

    E.ClearHotkey;
    AssertFalse('Hidden again', E.RightButton.Visible);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestDefaultIcons;
var
  E: THotKeyEdit;
begin
  { Out of the box the right button has the embedded default icons, so a host
    does not have to provide an image list. }
  E := THotKeyEdit.Create(nil);
  try
    AssertNotNull('Default images assigned', E.RightButton.Images);
    AssertEquals('Two default images', 2, E.RightButton.Images.Count);
    AssertEquals('Choose default index', HOTKEYEDIT_CHOOSE_INDEX, E.RightButton.ImageIndex);

    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Clear default index', HOTKEYEDIT_CLEAR_INDEX, E.RightButton.ImageIndex);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestButtonImageIndex;
var
  E: THotKeyEdit;
  List: TImageList;
begin
  E := THotKeyEdit.Create(nil);
  List := TImageList.Create(nil);
  try
    List.Width := 16;
    List.Height := 16;
    { A host image list takes precedence over the defaults. }
    E.RightButton.Images := List;
    E.ClearImageIndex := 3;
    E.ChooseImageIndex := 7;

    AssertEquals('Choose icon when empty', 7, E.RightButton.ImageIndex);

    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Clear icon when set', 3, E.RightButton.ImageIndex);

    E.ClearHotkey;
    AssertEquals('Choose icon again', 7, E.RightButton.ImageIndex);
  finally
    E.Free;
    List.Free;
  end;
end;

procedure TTestHotKeyEdit.TestButtonImageIndexReset;
var
  E: THotKeyEdit;
  List: TImageList;
begin
  { Resetting a per-state index to -1 must clear the glyph, not keep the
    previous one. }
  E := THotKeyEdit.Create(nil);
  List := TImageList.Create(nil);
  try
    List.Width := 16;
    List.Height := 16;
    E.RightButton.Images := List;
    E.ClearImageIndex := 3;
    E.ChooseImageIndex := 7;

    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Clear icon when set', 3, E.RightButton.ImageIndex);

    E.ClearImageIndex := -1;
    AssertEquals('Clear reset to none', -1, E.RightButton.ImageIndex);

    E.Hotkey := 0;
    AssertEquals('Choose icon when empty', 7, E.RightButton.ImageIndex);

    E.ChooseImageIndex := -1;
    AssertEquals('Choose reset to none', -1, E.RightButton.ImageIndex);
  finally
    E.Free;
    List.Free;
  end;
end;

procedure TTestHotKeyEdit.TestNoModifier;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    AssertFalse('Default NoModifier', E.NoModifier);
    E.NoModifier := True;
    AssertTrue('NoModifier set', E.NoModifier);
    E.NoModifier := False;
    AssertFalse('NoModifier reset', E.NoModifier);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestUseDefaultImages;
var
  E: THotKeyEdit;
begin
  { UseDefaultImages = False keeps the host in control of RightButton.Images. }
  E := THotKeyEdit.Create(nil);
  try
    AssertTrue('Default UseDefaultImages', E.UseDefaultImages);
    E.UseDefaultImages := False;
    E.RightButton.Images := nil;
    E.ClearImageIndex := 0; // triggers UpdateButton
    AssertNull('Default images not re-assigned', E.RightButton.Images);
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestInlineCapture;
var
  E: THotKeyEdit;
  Ed: TEdit;
  Key: Word;
begin
  { ShowGrabberOnClick = False: typing the combo in the focused edit captures it. }
  E := THotKeyEdit.Create(nil);
  try
    E.ShowGrabberOnClick := False;
    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);
    Ed.OnEnter(Ed); // arms the inline capture
    Key := VK_F5;
    Ed.OnKeyUp(Ed, Key, [ssCtrl]);
    AssertEquals('Captured inline', Integer(ShortCut(VK_F5, [ssCtrl])), Integer(E.Hotkey));
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestInlineCaptureKeepsProgrammaticHotkey;
var
  E: THotKeyEdit;
  Ed: TEdit;
  Key: Word;
begin
  { Regression: the capture engine used to be out of sync with a hotkey set
    programmatically, so a non-capturable key would reset it to 0. }
  E := THotKeyEdit.Create(nil);
  try
    E.ShowGrabberOnClick := False;
    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);
    Ed.OnEnter(Ed);
    Key := VK_SHIFT; // not representable as a shortcut
    Ed.OnKeyUp(Ed, Key, [ssShift]);
    AssertEquals('Programmatic hotkey kept',
      Integer(ShortCut(VK_F5, [ssCtrl])), Integer(E.Hotkey));
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestInlineCaptureValidator;
var
  E: THotKeyEdit;
  Ed: TEdit;
  Key: Word;
begin
  { The host validator must also apply to inline capture. }
  E := THotKeyEdit.Create(nil);
  try
    E.ShowGrabberOnClick := False;
    E.OnValidateHotkey := @RejectCtrlQ;
    Ed := FindInnerEdit(E);
    Ed.OnEnter(Ed);
    Key := VK_Q;
    Ed.OnKeyUp(Ed, Key, [ssCtrl]);
    AssertEquals('Rejected -> no hotkey', 0, Integer(E.Hotkey));
  finally
    E.Free;
  end;
end;

procedure TTestHotKeyEdit.TestCustomImagesDeriveWidth;
var
  E: THotKeyEdit;
  List: TImageList;
begin
  { A host image list narrower/wider than the embedded default must update the
    button width, even though the default assignment already set it. }
  E := THotKeyEdit.Create(nil);
  List := TImageList.Create(nil);
  try
    List.Width := 24;
    List.Height := 24;
    E.UseDefaultImages := False;
    E.RightButton.Images := List;
    E.ChooseImageIndex := 0; // triggers UpdateButton
    AssertEquals('Derived width from host list', 24, E.RightButton.ImagesWidth);
  finally
    E.Free;
    List.Free;
  end;
end;

procedure TTestHotKeyEdit.TestExplicitImagesWidthPreserved;
var
  E: THotKeyEdit;
  List: TImageList;
begin
  { Regression: an explicit ImagesWidth equal to the embedded default (16) was
    mistaken for "still the default" and overridden with the host list width
    (e.g. 32 from ilIcons), so the glyph looked resized/clipped. }
  E := THotKeyEdit.Create(nil);
  List := TImageList.Create(nil);
  try
    List.Width := 24;
    List.Height := 24;
    E.UseDefaultImages := False;
    E.RightButton.Images := List;
    E.RightButton.ImagesWidth := 16; // explicit, same as the embedded default
    E.ChooseImageIndex := 0;         // triggers UpdateButton
    AssertEquals('Explicit width preserved', 16, E.RightButton.ImagesWidth);
  finally
    E.Free;
    List.Free;
  end;
end;

initialization
  RegisterTest(TTestHotKeyEdit);

end.
