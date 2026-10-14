unit Tests.SystemIconsExt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  SystemIcons.Ext;

type
  TTestSystemIconsExt = class(TTestCase)
  published
    procedure TestNormalizeExt;
    procedure TestNormalizeExtKey;
  end;

implementation

procedure TTestSystemIconsExt.TestNormalizeExt;
begin
  AssertEquals('.txt', NormalizeExt('.TXT'));
  AssertEquals('.txt', NormalizeExt('txt'));
  AssertEquals('.txt', NormalizeExt('*.txt'));
  AssertEquals('', NormalizeExt('*'));
  AssertEquals('', NormalizeExt('*.*'));
  AssertEquals('', NormalizeExt(''));
  AssertEquals('.pdf', NormalizeExt('  .Pdf  '));
end;

procedure TTestSystemIconsExt.TestNormalizeExtKey;
begin
  AssertEquals('*.txt', NormalizeExtKey('.txt'));
  AssertEquals('*.txt', NormalizeExtKey('txt'));
  AssertEquals('*.txt', NormalizeExtKey('*.txt'));
  AssertEquals('*.txt', NormalizeExtKey('*.TXT'));
  AssertEquals('', NormalizeExtKey(''));
end;

initialization
  RegisterTest(TTestSystemIconsExt);

end.
