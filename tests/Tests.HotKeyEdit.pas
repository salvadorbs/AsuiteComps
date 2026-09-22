unit Tests.HotKeyEdit;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Forms, Menus, LCLProc, LCLType,
  HotKeyEdit;

type

  { TTestHotKeyEdit }

  TTestHotKeyEdit = class(TTestCase)
  private
    FChanged: Integer;
    procedure OnHotkeyChanged(Sender: TObject);
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
    procedure TestButtonImageIndex;
  end;

implementation

var
  LCLInitialized: Boolean = False;

{ TTestHotKeyEdit }

procedure TTestHotKeyEdit.OnHotkeyChanged(Sender: TObject);
begin
  Inc(FChanged);
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

procedure TTestHotKeyEdit.TestButtonImageIndex;
var
  E: THotKeyEdit;
begin
  E := THotKeyEdit.Create(nil);
  try
    E.ClearImageIndex := 3;
    E.ChooseImageIndex := 7;

    AssertEquals('Choose icon when empty', 7, E.RightButton.ImageIndex);

    E.Hotkey := ShortCut(VK_F5, [ssCtrl]);
    AssertEquals('Clear icon when set', 3, E.RightButton.ImageIndex);

    E.ClearHotkey;
    AssertEquals('Choose icon again', 7, E.RightButton.ImageIndex);
  finally
    E.Free;
  end;
end;

initialization
  RegisterTest(TTestHotKeyEdit);

end.
