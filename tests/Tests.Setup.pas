unit Tests.Setup;

{$mode objfpc}{$H+}

{ Shared test setup helpers. EnsureLCLInitialized calls Application.Initialize
  exactly once per process: calling it from several test units leaks LCL
  one-time allocations (e.g. gtk keyboard tables) on some widgetsets. }

interface

procedure EnsureLCLInitialized;

implementation

uses
  Forms;

var
  Initialized: Boolean = False;

procedure EnsureLCLInitialized;
begin
  if not Initialized then
  begin
    Application.Initialize;
    Initialized := True;
  end;
end;

end.
