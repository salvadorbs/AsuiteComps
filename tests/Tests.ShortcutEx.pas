unit Tests.ShortcutEx;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Menus, LCLProc, LCLType,
  Hotkeys.ShortcutEx;

type

  { TTestShortcutEx }

  TTestShortcutEx = class(TTestCase)
  published
    procedure TestCreateVariants;
    procedure TestSimpleShortcut;
    procedure TestIndexAndTag;
    procedure TestNotifyDefaultNil;
    procedure TestToText;
  end;

implementation

procedure TTestShortcutEx.TestCreateVariants;
var
  S: TShortcutEx;
begin
  //Ctrl
  S := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
  try
    AssertEquals('Key', Integer(VK_A), Integer(S.Key));
    AssertTrue('Shift is [ssCtrl]', S.ShiftState = TShiftState([ssCtrl]));
    AssertEquals('SimpleShortcut', Integer(ShortCut(VK_A, [ssCtrl])), Integer(S.SimpleShortcut));
  finally
    S.Free;
  end;

  //No modifiers
  S := TShortcutEx.Create(ShortCut(VK_F5, []));
  try
    AssertEquals('Key', Integer(VK_F5), Integer(S.Key));
    AssertTrue('Empty shift', S.ShiftState = []);
  finally
    S.Free;
  end;

  //Ctrl+Alt+Shift
  S := TShortcutEx.Create(ShortCut(VK_DELETE, [ssCtrl, ssAlt, ssShift]));
  try
    AssertEquals('Key', Integer(VK_DELETE), Integer(S.Key));
    AssertTrue('Shift', S.ShiftState = TShiftState([ssCtrl, ssAlt, ssShift]));
  finally
    S.Free;
  end;

  //Zero
  S := TShortcutEx.Create(0);
  try
    AssertEquals('Key', 0, Integer(S.Key));
    AssertTrue('Empty shift', S.ShiftState = []);
    AssertEquals('SimpleShortcut', 0, Integer(S.SimpleShortcut));
  finally
    S.Free;
  end;
end;

procedure TTestShortcutEx.TestSimpleShortcut;
var
  S: TShortcutEx;
  Sc: TShortCut;
begin
  Sc := ShortCut(VK_F9, [ssAlt]);
  S := TShortcutEx.Create(Sc);
  try
    AssertEquals('RoundTrip', Integer(Sc), Integer(S.SimpleShortcut));

    S.SimpleShortcut := ShortCut(VK_B, [ssAlt, ssShift]);
    AssertEquals('Key', Integer(VK_B), Integer(S.Key));
    AssertTrue('Shift', S.ShiftState = TShiftState([ssAlt, ssShift]));
    AssertEquals('SimpleShortcut', Integer(ShortCut(VK_B, [ssAlt, ssShift])), Integer(S.SimpleShortcut));
  finally
    S.Free;
  end;
end;

procedure TTestShortcutEx.TestIndexAndTag;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
  try
    AssertEquals('Default Index', -1, S.Index);
    AssertEquals('Default Tag', 0, S.Tag);

    S.Index := 42;
    S.Tag := 7;
    AssertEquals('Index', 42, S.Index);
    AssertEquals('Tag', 7, S.Tag);
  finally
    S.Free;
  end;
end;

procedure TTestShortcutEx.TestNotifyDefaultNil;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
  try
    AssertTrue('Default Notify is nil', TMethod(S.Notify).Code = nil);
  finally
    S.Free;
  end;
end;

procedure TTestShortcutEx.TestToText;
var
  S: TShortcutEx;
begin
  S := TShortcutEx.Create(ShortCut(VK_A, [ssCtrl]));
  try
    AssertEquals('ToText', ShortCutToText(ShortCut(VK_A, [ssCtrl])), S.ToText);
    AssertTrue('ToText not empty', S.ToText <> '');
  finally
    S.Free;
  end;
end;

initialization
  RegisterTest(TTestShortcutEx);

end.
