# ASuiteComps automated tests

FPCUnit test suite for the ASuiteComps package: all visual components
(`THotKey`, `THotKeyEdit`, `TfrmShortcutGrabber`, `TButtonedEdit`,
`TBCImageTab`), the `Hotkeys.Manager` / `Hotkeys.ShortcutEx` logic and the
platform hotkey managers.

## Layout

| File | Covers |
|---|---|
| `AsuiteCompsTests.lpr` / `.lpi` | Console runner (exit code <> 0 on failure) |
| `Tests.ShortcutEx.pas` | `Hotkeys.ShortcutEx` |
| `Tests.HotkeysManager.pas` | `TBaseHotkeyManager` via a fake `DoRegister`/`DoUnregister` manager (no OS calls) |
| `Tests.HotkeyControl.pas` | `THotKey` (via a cracker class) and the `TShortcutCapture` engine |
| `Tests.ShortcutGrabber.pas` | `TfrmShortcutGrabber` images, hotkey/button round-trip and `ShortcutGrabberDefaults` |
| `Tests.HotKeyEdit.pas` | `THotKeyEdit` value/text sync, clear, events and right-button state |
| `Tests.ButtonedEdit.pas` | `TButtonedEdit` properties, events and button options |
| `Tests.BCImageTab.pas` | `TBCImageTab` toggle/group/exclusivity behavior |
| `Tests.Platform.pas` | Platform manager smoke tests (singleton, constructor, display-independent API) |
| `run_tests.sh` | Build + run helper used locally and by CI |

## Run locally

```bash
./run_tests.sh --widgetset=gtk3
```

Options: `--lazbuild <path>` (default: `lazbuild` from PATH),
`--widgetset <gtk2|gtk2|gtk3|qt5|qt6|win32>` (default: `gtk2`).

GUI control tests need a display server on Linux. If `DISPLAY` is not set
and `xvfb-run` is available, the script re-executes itself under
`xvfb-run` automatically.

## Run in CI

`.github/workflows/main.yml` and `main_release.yml` run this suite for
every matrix entry (Windows + Linux GTK2/GTK3/Qt5/Qt6) before building
ASuite. Any test failure fails the job.

## Notes

- Real global-hotkey grabs are intentionally NOT tested: they depend on
  machine state (X11 vs Wayland, shortcuts already taken by other apps).
  `Tests.Platform` only covers construction and display-independent API,
  including the constructor path used when no X11 display exists.
- `TBaseHotkeyManager.RegisterNotify` keeps the item in its list even if
  the platform `DoRegister` fails (its return value is still propagated).
  This behavior is pinned by `TestRegisterFailureStillTracked`.
- `TCustomButtonedEdit.SetFont` currently assigns only when the fonts are
  already equal (inverted guard); it is deliberately not covered by tests
  so a future fix is not blocked.
- `TBCImageTab` click tests set `AlphaTest := False` and explicit bounds:
  an unrendered `BCImageButton` with `AlphaTest = True` touches a nil
  internal bitmap (upstream BGRAControls behavior, unrelated to this package).
