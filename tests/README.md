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
| `Tests.HotkeyControl.pas` | `THotKey` (via a cracker class) and the `TShortcutCapture` engine (including the `ssSuper` → `ssMeta` folding) |
| `Tests.ShortcutGrabber.pas` | `TfrmShortcutGrabber` images, hotkey/button round-trip and `ShortcutGrabberDefaults` |
| `Tests.HotKeyEdit.pas` | `THotKeyEdit` value/text sync, clear, events and right-button state |
| `Tests.ButtonedEdit.pas` | `TButtonedEdit` properties, events and button options |
| `Tests.BCImageTab.pas` | `TBCImageTab` toggle/group/exclusivity behavior |
| `Tests.Platform.pas` | Platform manager smoke tests (singleton, constructor, backend selection, display-independent API) |
| `Tests.Portal.pas` | Wayland `GlobalShortcuts` portal backend: accelerator conversion (always) and live round-trip/activation (opt-in via `ASUITECOMPS_TEST_PORTAL`). Compiled on Unix only; on Windows it asserts the backend is unix-only |
| `run_tests.sh` | Build + run helper used locally and by CI |

## Run locally

```bash
./run_tests.sh --widgetset=gtk3
```

Options: `--lazbuild <path>` (default: `lazbuild` from PATH),
`--widgetset <gtk2|gtk2|gtk3|qt5|qt6|win32>` (default: `gtk2`).
Set `ASUITECOMPS_MIN_TESTS` to change the minimum number of tests the
script requires (default 100); it fails the run if the suite shrinks.

GUI control tests need a display server on Linux. If `DISPLAY` is not set
and `xvfb-run` is available, the script re-executes itself under
`xvfb-run` automatically.

## Run in CI

In this repository `.github/workflows/tests.yml` runs the suite on every push
and pull request for each matrix entry (Windows + Linux GTK2/GTK3/Qt5/Qt6).
Keep this suite green before updating any consumer that pins this package
through a submodule.

## Notes

- Real global-hotkey grabs are intentionally NOT tested: they depend on
  machine state (X11 vs Wayland, shortcuts already taken by other apps).
  `Tests.Platform` only covers construction and display-independent API,
  including the constructor path used when no X11 display exists.
- `TBaseHotkeyManager.RegisterNotify` keeps an item only when the platform
  `DoRegister` succeeds, and `UnregisterNotify` drops it only when
  `DoUnregister` succeeds. A failure therefore leaves no stale entry and can
  be retried; pinned by `TestRegisterFailureNotTracked`,
  `TestRegisterRetryAfterFailure` and `TestUnregisterFailureKeepsItem`.
- `TBaseHotkeyManager.RefreshNotify` is transactional: it returns early when
  the platform keeps the old binding, and drops the item when the
  re-registration fails; pinned by `TestRefreshUnregisterFailureKeepsItem`
  and `TestRefreshReregisterFailureDropsItem`.
- The portal backend re-binds the desired set on a single long-lived session
  instead of recreating one per change: KDE only unregisters a shortcut while
  it is registered in the current session. The platform manager overrides
  `RefreshNotify` so `THotkeyItemsList.RefreshRegs` does not rebind per
  shortcut.
- The Qt backend uses the native event filter exposed by LCL's Qt binding
  (`QNativeEventFilter_hook`): the package ships no Qt helper library and no
  local `xcb` binding, and reads the few needed `xcb_key_press_event` fields
  directly.
- `Tests.Portal.TestLivePortalRoundTrip` (opt-in, `ASUITECOMPS_TEST_PORTAL=1`)
  also checks KDE's KGlobalAccel to confirm the shortcut is listed while
  registered and gone after `UnregisterShortcut`; the check is skipped when
  KGlobalAccel is not reachable.
- `TCustomButtonedEdit.SetFont` now assigns only when the fonts differ (the
  old inverted guard ignored every real assignment); covered by
  `TestFontAssignment`. `MaxLength`, `PasswordChar` and `Alignment` are
  forwarded to the inner edit and covered by `TestMaxLengthPasswordAlignment`.
- `TBCImageTab` click tests set `AlphaTest := False` and explicit bounds:
  an unrendered `BCImageButton` with `AlphaTest = True` touches a nil
  internal bitmap (upstream BGRAControls behavior, unrelated to this package).
