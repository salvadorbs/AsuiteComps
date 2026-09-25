unit Tests.ButtonedEdit;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Types, LCLType, Controls, Forms,
  StdCtrls, Graphics, Menus, ButtonedEdit;

type

  { TTestButtonedEdit }

  TTestButtonedEdit = class(TTestCase)
  private
    FChangeCount: Integer;
    FClickCount: Integer;
    FLeftCount: Integer;
    FRightCount: Integer;
    FKeyPressCount: Integer;
    FKeyPressChar: Char;
    FMouseEnterCount: Integer;
    FMouseLeaveCount: Integer;
    FEnterCount: Integer;
    FExitCount: Integer;
    FDblClickCount: Integer;
    FKeyDownCount: Integer;
    FMouseDownCount: Integer;
    FContextPopupCount: Integer;
    FUtf8Count: Integer;
    procedure OnChange(Sender: TObject);
    procedure OnClick(Sender: TObject);
    procedure OnLeft(Sender: TObject);
    procedure OnRight(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnMouseEnter(Sender: TObject);
    procedure OnMouseLeave(Sender: TObject);
    procedure OnEnter(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnEditDblClick(Sender: TObject);
    procedure OnEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnEditContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure OnEditUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    function FindInnerEdit(AE: TButtonedEdit): TEdit;
  protected
    procedure SetUp; override;
  published
    procedure TestTextProperties;
    procedure TestOnChangeFired;
    procedure TestOnClickWiring;
    procedure TestButtonsExistAndHidden;
    procedure TestLeftButtonClickWiring;
    procedure TestRightButtonClickWiring;
    procedure TestOnKeyPressWiring;
    procedure TestButtonOptionsAssign;
    procedure TestButtonOptionsRoundTrip;
    procedure TestButtonImageStates;
    procedure TestDropDownMenuFreeNotification;
    procedure TestNotFocusedHeadless;
    procedure TestFontAssignment;
    procedure TestMaxLengthPasswordAlignment;
    procedure TestHoverAndFocusForwarding;
    procedure TestHoverAggregationFromButtons;
    procedure TestForwardedEditEvents;
    procedure TestFocusOnButtonClick;
    procedure TestNativeBorder;
    procedure TestSizing;
    procedure TestTabStopProxy;
    procedure TestFontPropagatesToInnerEdit;
    procedure TestButtonOptionsClearWithNil;
  end;

implementation

var
  LCLInitialized: Boolean = False;

procedure TTestButtonedEdit.OnChange(Sender: TObject);
begin
  Inc(FChangeCount);
end;

procedure TTestButtonedEdit.OnClick(Sender: TObject);
begin
  Inc(FClickCount);
end;

procedure TTestButtonedEdit.OnLeft(Sender: TObject);
begin
  Inc(FLeftCount);
end;

procedure TTestButtonedEdit.OnRight(Sender: TObject);
begin
  Inc(FRightCount);
end;

procedure TTestButtonedEdit.OnKeyPress(Sender: TObject; var Key: Char);
begin
  Inc(FKeyPressCount);
  FKeyPressChar := Key;
end;

procedure TTestButtonedEdit.OnMouseEnter(Sender: TObject);
begin
  Inc(FMouseEnterCount);
end;

procedure TTestButtonedEdit.OnMouseLeave(Sender: TObject);
begin
  Inc(FMouseLeaveCount);
end;

procedure TTestButtonedEdit.OnEnter(Sender: TObject);
begin
  Inc(FEnterCount);
end;

procedure TTestButtonedEdit.OnExit(Sender: TObject);
begin
  Inc(FExitCount);
end;

procedure TTestButtonedEdit.OnEditDblClick(Sender: TObject);
begin
  Inc(FDblClickCount);
end;

procedure TTestButtonedEdit.OnEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Inc(FKeyDownCount);
end;

procedure TTestButtonedEdit.OnEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Inc(FMouseDownCount);
end;

procedure TTestButtonedEdit.OnEditContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Inc(FContextPopupCount);
end;

procedure TTestButtonedEdit.OnEditUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  Inc(FUtf8Count);
end;

function TTestButtonedEdit.FindInnerEdit(AE: TButtonedEdit): TEdit;
var
  I: Integer;
begin
  { LCL's Controls[] also lists TGraphicControl children (the glyph buttons),
    so pick the inner TEdit explicitly. }
  Result := nil;
  for I := 0 to AE.ControlCount - 1 do
    if AE.Controls[I] is TEdit then
      Exit(TEdit(AE.Controls[I]));
end;

procedure TTestButtonedEdit.SetUp;
begin
  inherited SetUp;
  if not LCLInitialized then
  begin
    Application.Initialize;
    LCLInitialized := True;
  end;
  FChangeCount := 0;
  FClickCount := 0;
  FLeftCount := 0;
  FRightCount := 0;
  FKeyPressCount := 0;
  FKeyPressChar := #0;
  FMouseEnterCount := 0;
  FMouseLeaveCount := 0;
  FEnterCount := 0;
  FExitCount := 0;
  FDblClickCount := 0;
  FKeyDownCount := 0;
  FMouseDownCount := 0;
  FContextPopupCount := 0;
  FUtf8Count := 0;
end;

procedure TTestButtonedEdit.TestTextProperties;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    //Text
    E.Text := 'hello';
    AssertEquals('Text', 'hello', E.Text);
    E.Text := '';
    AssertEquals('Text cleared', '', E.Text);
    //TextHint
    E.TextHint := 'Search...';
    AssertEquals('TextHint', 'Search...', E.TextHint);
    //ReadOnly
    AssertFalse('Default ReadOnly', E.ReadOnly);
    E.ReadOnly := True;
    AssertTrue('ReadOnly', E.ReadOnly);
    E.ReadOnly := False;
    AssertFalse('ReadOnly back', E.ReadOnly);
    //CharCase
    AssertTrue('Default CharCase', E.CharCase = ecNormal);
    E.CharCase := ecUpperCase;
    AssertTrue('CharCase', E.CharCase = ecUpperCase);
    //ParentFont
    AssertTrue('Default ParentFont', E.ParentFont);
    E.ParentFont := False;
    AssertFalse('ParentFont', E.ParentFont);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestOnChangeFired;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    E.OnChange := @OnChange;
    E.Text := 'hello';
    AssertEquals('OnChange fired once', 1, FChangeCount);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestOnClickWiring;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    E.OnClick := @OnClick;
    E.OnClick(E);
    AssertEquals('OnClick wired', 1, FClickCount);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestButtonsExistAndHidden;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    AssertNotNull('LeftButton', E.LeftButton);
    AssertNotNull('RightButton', E.RightButton);
    AssertFalse('Left hidden by default', E.LeftButton.Visible);
    AssertFalse('Right hidden by default', E.RightButton.Visible);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestLeftButtonClickWiring;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    E.OnLeftButtonClick := @OnLeft;
    E.OnLeftButtonClick(E);
    AssertEquals('Left click wired', 1, FLeftCount);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestRightButtonClickWiring;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    E.OnRightButtonClick := @OnRight;
    E.OnRightButtonClick(E);
    AssertEquals('Right click wired', 1, FRightCount);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestOnKeyPressWiring;
var
  E: TButtonedEdit;
  C: Char;
begin
  E := TButtonedEdit.Create(nil);
  try
    E.OnKeyPress := @OnKeyPress;
    C := 'x';
    E.OnKeyPress(E, C);
    AssertEquals('KeyPress wired', 1, FKeyPressCount);
    AssertEquals('Key passed', 'x', FKeyPressChar);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestButtonOptionsAssign;
var
  E1, E2: TButtonedEdit;
begin
  E1 := TButtonedEdit.Create(nil);
  E2 := TButtonedEdit.Create(nil);
  try
    E1.RightButton.Visible := True;
    E1.RightButton.ImagesWidth := 16;
    E2.RightButton := E1.RightButton;
    AssertTrue('Visible copied', E2.RightButton.Visible);
    AssertEquals('ImagesWidth copied', 16, E2.RightButton.ImagesWidth);
  finally
    E1.Free;
    E2.Free;
  end;
end;

procedure TTestButtonedEdit.TestButtonOptionsRoundTrip;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    AssertEquals('Default ImagesWidth', 0, E.RightButton.ImagesWidth);
    E.RightButton.ImagesWidth := 24;
    AssertEquals('ImagesWidth', 24, E.RightButton.ImagesWidth);

    AssertEquals('Default ImageIndex', -1, E.RightButton.ImageIndex);
    E.RightButton.ImageIndex := 3;
    AssertEquals('ImageIndex', 3, E.RightButton.ImageIndex);

    E.RightButton.Visible := True;
    AssertTrue('Right visible', E.RightButton.Visible);
    E.LeftButton.Visible := True;
    AssertTrue('Left visible', E.LeftButton.Visible);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestDropDownMenuFreeNotification;
var
  E: TButtonedEdit;
  M: TPopupMenu;
begin
  { Regression: the glyph button did not register for free notification, so a
    freed popup menu left FDropDownMenu dangling (AV when the button was
    clicked). The setter now registers/removes the notification. }
  E := TButtonedEdit.Create(nil);
  M := TPopupMenu.Create(nil);
  try
    E.LeftButton.DropDownMenu := M;
    AssertSame('Menu assigned', M, E.LeftButton.DropDownMenu);

    FreeAndNil(M);
    AssertNull('Menu cleared after Free', E.LeftButton.DropDownMenu);
  finally
    M.Free;
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestButtonImageStates;
var
  E: TButtonedEdit;
begin
  { Image states inspired by VCL's TEditButton (normal/hot/pressed/disabled). }
  E := TButtonedEdit.Create(nil);
  try
    AssertEquals('Default HotImageIndex', -1, E.LeftButton.HotImageIndex);
    AssertEquals('Default PressedImageIndex', -1, E.LeftButton.PressedImageIndex);
    AssertEquals('Default DisabledImageIndex', -1, E.LeftButton.DisabledImageIndex);

    E.LeftButton.HotImageIndex := 1;
    E.LeftButton.PressedImageIndex := 2;
    E.LeftButton.DisabledImageIndex := 3;
    AssertEquals('HotImageIndex', 1, E.LeftButton.HotImageIndex);
    AssertEquals('PressedImageIndex', 2, E.LeftButton.PressedImageIndex);
    AssertEquals('DisabledImageIndex', 3, E.LeftButton.DisabledImageIndex);

    E.RightButton := E.LeftButton;
    AssertEquals('HotImageIndex copied', 1, E.RightButton.HotImageIndex);
    AssertEquals('PressedImageIndex copied', 2, E.RightButton.PressedImageIndex);
    AssertEquals('DisabledImageIndex copied', 3, E.RightButton.DisabledImageIndex);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestNotFocusedHeadless;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    AssertFalse('Not focused', E.Focused);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestFontAssignment;
var
  E: TButtonedEdit;
  F: TFont;
begin
  { Regression: SetFont used to assign only when the fonts were already
    equal, so a real assignment was silently ignored. }
  E := TButtonedEdit.Create(nil);
  try
    F := TFont.Create;
    try
      F.Size := 17;
      F.Style := [fsBold];
      E.Font := F;
      AssertEquals('Size applied', 17, E.Font.Size);
      AssertTrue('Style applied', fsBold in E.Font.Style);

      { Re-assigning the same font must not corrupt the value. }
      E.Font := F;
      AssertEquals('Size kept', 17, E.Font.Size);
    finally
      F.Free;
    end;
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestMaxLengthPasswordAlignment;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    AssertEquals('Default MaxLength', 0, E.MaxLength);
    E.MaxLength := 12;
    AssertEquals('MaxLength', 12, E.MaxLength);

    AssertTrue('Default PasswordChar', E.PasswordChar = #0);
    E.PasswordChar := '*';
    AssertTrue('PasswordChar', E.PasswordChar = '*');

    AssertTrue('Default Alignment', E.Alignment = taLeftJustify);
    E.Alignment := taRightJustify;
    AssertTrue('Alignment', E.Alignment = taRightJustify);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestHoverAndFocusForwarding;
var
  E: TButtonedEdit;
  Ed: TEdit;
begin
  { Hover (MouseEnter/MouseLeave) and focus (Enter/Exit) of the inner edit
    must be forwarded to the composite control and exposed as its events. }
  E := TButtonedEdit.Create(nil);
  try
    E.OnMouseEnter := @OnMouseEnter;
    E.OnMouseLeave := @OnMouseLeave;
    E.OnEnter := @OnEnter;
    E.OnExit := @OnExit;

    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);

    AssertTrue('Inner edit wires MouseEnter', Assigned(Ed.OnMouseEnter));
    AssertTrue('Inner edit wires MouseLeave', Assigned(Ed.OnMouseLeave));
    AssertTrue('Inner edit wires Enter', Assigned(Ed.OnEnter));
    AssertTrue('Inner edit wires Exit', Assigned(Ed.OnExit));

    Ed.OnMouseEnter(Ed);
    Ed.OnMouseLeave(Ed);
    Ed.OnEnter(Ed);
    Ed.OnExit(Ed);

    AssertEquals('MouseEnter forwarded', 1, FMouseEnterCount);
    AssertEquals('MouseLeave forwarded', 1, FMouseLeaveCount);
    AssertEquals('Enter forwarded', 1, FEnterCount);
    AssertEquals('Exit forwarded', 1, FExitCount);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestHoverAggregationFromButtons;
var
  E: TButtonedEdit;
  I, Count: Integer;
  Buttons: array[0..1] of TControl;
begin
  { Entering a glyph button counts as entering the whole composite, and the
    outside -> inside transition fires the composite event only once. The
    CM_MOUSEENTER/CM_MOUSELEAVE messages are the same ones LCL sends on every
    widgetset, so the test stays widgetset-agnostic. }
  E := TButtonedEdit.Create(nil);
  try
    E.OnMouseEnter := @OnMouseEnter;
    E.OnMouseLeave := @OnMouseLeave;

    Count := 0;
    for I := 0 to E.ControlCount - 1 do
      if E.Controls[I] is TCustomGlyphButton then
      begin
        if Count < 2 then
          Buttons[Count] := E.Controls[I];
        Inc(Count);
      end;
    AssertEquals('Two glyph buttons', 2, Count);

    Buttons[0].Perform(CM_MOUSEENTER, 0, 0);
    AssertEquals('Enter on composite', 1, FMouseEnterCount);
    Buttons[1].Perform(CM_MOUSEENTER, 0, 0);
    AssertEquals('Enter not repeated across children', 1, FMouseEnterCount);

    Buttons[1].Perform(CM_MOUSELEAVE, 0, 0);
    AssertEquals('Leave on composite', 1, FMouseLeaveCount);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestForwardedEditEvents;
var
  E: TButtonedEdit;
  Ed: TEdit;
  Key: Word;
  Handled: Boolean;
  Utf8Key: TUTF8Char;
  MousePos: TPoint;
begin
  { The composite republishes the usual edit events, so they must be forwarded
    from the inner TEdit to the host. }
  E := TButtonedEdit.Create(nil);
  try
    E.OnDblClick := @OnEditDblClick;
    E.OnKeyDown := @OnEditKeyDown;
    E.OnMouseDown := @OnEditMouseDown;
    E.OnContextPopup := @OnEditContextPopup;
    E.OnUTF8KeyPress := @OnEditUtf8KeyPress;

    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);

    Ed.OnDblClick(Ed);

    Key := 65;
    Ed.OnKeyDown(Ed, Key, []);

    Ed.OnMouseDown(Ed, mbLeft, [], 1, 2);

    Handled := False;
    MousePos := Point(0, 0);
    Ed.OnContextPopup(Ed, MousePos, Handled);

    Utf8Key := 'a';
    Ed.OnUTF8KeyPress(Ed, Utf8Key);

    AssertEquals('DblClick forwarded', 1, FDblClickCount);
    AssertEquals('KeyDown forwarded', 1, FKeyDownCount);
    AssertEquals('MouseDown forwarded', 1, FMouseDownCount);
    AssertEquals('ContextPopup forwarded', 1, FContextPopupCount);
    AssertEquals('UTF8KeyPress forwarded', 1, FUtf8Count);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestFocusOnButtonClick;
var
  E: TButtonedEdit;
  Btn: TCustomGlyphButton;
  I: Integer;
begin
  E := TButtonedEdit.Create(nil);
  try
    AssertFalse('Default FocusOnButtonClick', E.FocusOnButtonClick);

    Btn := nil;
    for I := 0 to E.ControlCount - 1 do
      if (E.Controls[I] is TCustomGlyphButton) and
         (E.Controls[I].Name = 'LeftButton') then
        Btn := TCustomGlyphButton(E.Controls[I]);
    AssertNotNull('Left button found', Btn);

    { With the default (False) the click is still forwarded to the host. }
    E.OnLeftButtonClick := @OnLeft;
    Btn.Click;
    AssertEquals('Left button event forwarded', 1, FLeftCount);

    E.FocusOnButtonClick := True;
    AssertTrue('FocusOnButtonClick set', E.FocusOnButtonClick);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestSizing;
var
  E: TButtonedEdit;
begin
  { Height is anchored to the inner edit (like LCL's grouped edit / TEditButton). }
  E := TButtonedEdit.Create(nil);
  try
    AssertTrue('AutoSize default', E.AutoSize);
    AssertTrue('AutoSizeHeightIsEditHeight default', E.AutoSizeHeightIsEditHeight);

    E.AutoSizeHeightIsEditHeight := False;
    AssertFalse('AutoSizeHeightIsEditHeight set', E.AutoSizeHeightIsEditHeight);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestNativeBorder;
var
  E: TButtonedEdit;
  Ed: TEdit;
begin
  { The composite is borderless and draws the simulated frame itself; the inner
    TEdit's native border is hidden (bsNone) but is still used to compute the
    height (so it matches a TEdit). }
  E := TButtonedEdit.Create(nil);
  try
    AssertTrue('Composite has no border', E.BorderStyle = bsNone);
    AssertEquals('Default BorderColor', clWindowFrame, E.BorderColor);
    AssertEquals('Default FocusColor', clHighlight, E.FocusColor);
    AssertEquals('Default HoverColor', clHighlight, E.HoverColor);

    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);
    AssertTrue('Inner edit native border hidden', Ed.BorderStyle = bsNone);
  finally
    E.Free;
  end;
end;


procedure TTestButtonedEdit.TestTabStopProxy;
var
  E: TButtonedEdit;
  Ed: TEdit;
begin
  { TabStop of the composite must control the inner edit, not the (never
    focused) outer control. }
  E := TButtonedEdit.Create(nil);
  try
    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);

    AssertTrue('Default TabStop', E.TabStop);

    E.TabStop := False;
    AssertFalse('Proxy reports false', E.TabStop);
    AssertFalse('Inner edit TabStop false', Ed.TabStop);

    E.TabStop := True;
    AssertTrue('Proxy reports true', E.TabStop);
    AssertTrue('Inner edit TabStop true', Ed.TabStop);
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestFontPropagatesToInnerEdit;
var
  E: TButtonedEdit;
  Ed: TEdit;
  F: TFont;
begin
  { The composite reuses the inherited TControl.Font; the inner edit follows it
    (ParentFont), so the two must stay in sync. }
  E := TButtonedEdit.Create(nil);
  try
    Ed := FindInnerEdit(E);
    AssertNotNull('Inner edit found', Ed);

    F := TFont.Create;
    try
      F.Size := 19;
      F.Style := [fsBold];
      E.Font := F;

      AssertEquals('Control font size', 19, E.Font.Size);
      AssertEquals('Inner edit font size', 19, Ed.Font.Size);
      AssertTrue('Inner edit bold', fsBold in Ed.Font.Style);
    finally
      F.Free;
    end;
  finally
    E.Free;
  end;
end;

procedure TTestButtonedEdit.TestButtonOptionsClearWithNil;
var
  E: TButtonedEdit;
begin
  E := TButtonedEdit.Create(nil);
  try
    E.LeftButton.ImagesWidth := 16;
    E.LeftButton.Visible := True;
    AssertTrue('Precondition visible', E.LeftButton.Visible);

    E.LeftButton := nil;

    AssertFalse('Visible cleared', E.LeftButton.Visible);
    AssertEquals('ImagesWidth cleared', 0, E.LeftButton.ImagesWidth);
    AssertEquals('ImageIndex cleared', -1, E.LeftButton.ImageIndex);
    AssertNull('DropDownMenu cleared', E.LeftButton.DropDownMenu);
  finally
    E.Free;
  end;
end;

initialization
  RegisterTest(TTestButtonedEdit);

end.
