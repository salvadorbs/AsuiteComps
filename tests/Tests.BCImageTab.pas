unit Tests.BCImageTab;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Classes, SysUtils, Forms,
  BCImageTab;

type

  { Exposes protected Click for testing }

  TBCImageTabCracker = class(TBCImageTab)
  public
    procedure DoClick;
  end;

  { TTestBCImageTab }

  TTestBCImageTab = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestDefaults;
    procedure TestGroupIndexRoundTrip;
    procedure TestPressedGatedByToggleAndGroup;
    procedure TestClickTogglesInGroup;
    procedure TestGroupExclusivity;
    procedure TestGroupZeroPlainToggle;
    procedure TestDifferentGroupsIndependent;
  end;

implementation

var
  LCLInitialized: Boolean = False;

{ TBCImageTabCracker }

procedure TBCImageTabCracker.DoClick;
begin
  inherited Click;
end;

procedure TTestBCImageTab.SetUp;
begin
  inherited SetUp;
  if not LCLInitialized then
  begin
    Application.Initialize;
    LCLInitialized := True;
  end;
end;

procedure TTestBCImageTab.TestDefaults;
var
  T: TBCImageTabCracker;
begin
  T := TBCImageTabCracker.Create(nil);
  try
    AssertEquals('Default GroupIndex', 0, T.GroupIndex);
    AssertFalse('Default Pressed', T.Pressed);
    AssertFalse('Default Toggle', T.Toggle);
  finally
    T.Free;
  end;
end;

procedure TTestBCImageTab.TestGroupIndexRoundTrip;
var
  T: TBCImageTabCracker;
begin
  T := TBCImageTabCracker.Create(nil);
  try
    T.GroupIndex := 5;
    AssertEquals('GroupIndex', 5, T.GroupIndex);
  finally
    T.Free;
  end;
end;

procedure TTestBCImageTab.TestPressedGatedByToggleAndGroup;
var
  T: TBCImageTabCracker;
begin
  { Without Toggle the tab never latches Pressed (see TBCCustomImageTab.SetFPressed) }
  T := TBCImageTabCracker.Create(nil);
  try
    T.GroupIndex := 1;
    T.Pressed := True;
    AssertFalse('Pressed without Toggle', T.Pressed);
  finally
    T.Free;
  end;
end;

procedure TTestBCImageTab.TestClickTogglesInGroup;
var
  F: TForm;
  T: TBCImageTabCracker;
begin
  F := TForm.CreateNew(nil);
  try
    T := TBCImageTabCracker.Create(F);
    T.Parent := F;
    T.SetBounds(0, 0, 120, 32);
    T.AlphaTest := False;
    T.Toggle := True;
    T.GroupIndex := 1;
    T.DoClick;
    AssertTrue('Pressed after click', T.Pressed);
  finally
    F.Free;
  end;
end;

procedure TTestBCImageTab.TestGroupExclusivity;
var
  F: TForm;
  T1, T2: TBCImageTabCracker;
begin
  F := TForm.CreateNew(nil);
  try
    T1 := TBCImageTabCracker.Create(F);
    T1.Parent := F;
    T1.SetBounds(0, 0, 120, 32);
    T1.AlphaTest := False;
    T1.Toggle := True;
    T1.GroupIndex := 1;

    T2 := TBCImageTabCracker.Create(F);
    T2.Parent := F;
    T2.SetBounds(0, 32, 120, 32);
    T2.AlphaTest := False;
    T2.Toggle := True;
    T2.GroupIndex := 1;

    T1.DoClick;
    AssertTrue('T1 pressed', T1.Pressed);
    T2.DoClick;
    AssertTrue('T2 pressed', T2.Pressed);
    AssertFalse('T1 released by exclusivity', T1.Pressed);
  finally
    F.Free;
  end;
end;

procedure TTestBCImageTab.TestGroupZeroPlainToggle;
var
  F: TForm;
  T: TBCImageTabCracker;
begin
  { GroupIndex = 0 disables exclusivity management: plain base-class toggle }
  F := TForm.CreateNew(nil);
  try
    T := TBCImageTabCracker.Create(F);
    T.Parent := F;
    T.SetBounds(0, 0, 120, 32);
    T.AlphaTest := False;
    T.Toggle := True;
    T.GroupIndex := 0;
    T.DoClick;
    AssertTrue('Plain toggle with GroupIndex 0', T.Pressed);
  finally
    F.Free;
  end;
end;

procedure TTestBCImageTab.TestDifferentGroupsIndependent;
var
  F: TForm;
  T1, T2: TBCImageTabCracker;
begin
  F := TForm.CreateNew(nil);
  try
    T1 := TBCImageTabCracker.Create(F);
    T1.Parent := F;
    T1.SetBounds(0, 0, 120, 32);
    T1.AlphaTest := False;
    T1.Toggle := True;
    T1.GroupIndex := 1;

    T2 := TBCImageTabCracker.Create(F);
    T2.Parent := F;
    T2.SetBounds(0, 32, 120, 32);
    T2.AlphaTest := False;
    T2.Toggle := True;
    T2.GroupIndex := 2;

    T1.DoClick;
    T2.DoClick;
    AssertTrue('T1 still pressed', T1.Pressed);
    AssertTrue('T2 pressed', T2.Pressed);
  finally
    F.Free;
  end;
end;

initialization
  RegisterTest(TTestBCImageTab);

end.
