{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ASuiteComps;

{$warn 5023 off : no warning about unused units}
interface

uses
  BCImageTab, ButtonedEdit, HotKey, Hotkeys.Manager, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('BCImageTab', @BCImageTab.Register);
  RegisterUnit('ButtonedEdit', @ButtonedEdit.Register);
  RegisterUnit('HotKey', @HotKey.Register);
end;

initialization
  RegisterPackage('ASuiteComps', @Register);
end.
