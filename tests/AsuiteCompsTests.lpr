{ ASuiteComps automated tests (FPCUnit console runner).

  Build: lazbuild --widgetset=<gtk2|gtk3|qt5|qt6|win32> AsuiteCompsTests.lpi
  Run:   ./AsuiteCompsTests --all --format=plain   (exit code <> 0 on failure)

  GUI control tests need a display server on Linux: use xvfb-run
  (see run_tests.sh, which handles this automatically). }

program AsuiteCompsTests;

{$mode objfpc}{$H+}

uses
  Interfaces,
  consoletestrunner,
  Tests.ShortcutEx,
  Tests.HotkeysManager,
  Tests.HotkeyControl,
  Tests.ButtonedEdit,
  Tests.BCImageTab,
  Tests.Platform;

var
  Application: TTestRunner;
begin
  Application := TTestRunner.Create(nil);
  try
    Application.Run;
  finally
    Application.Free;
  end;
end.
