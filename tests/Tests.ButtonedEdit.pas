unit Tests.ButtonedEdit;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Forms, StdCtrls,
  ButtonedEdit;

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
    procedure OnChange(Sender: TObject);
    procedure OnClick(Sender: TObject);
    procedure OnLeft(Sender: TObject);
    procedure OnRight(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
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
    procedure TestNotFocusedHeadless;
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
    AssertFalse('Default ParentFont', E.ParentFont);
    E.ParentFont := True;
    AssertTrue('ParentFont', E.ParentFont);
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

initialization
  RegisterTest(TTestButtonedEdit);

end.
