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
| `Tests.Platform.pas` | Platform manager smoke tests (singleton, constructor, backend selection, display-independent API) and the X11 conversions extracted into `Hotkeys.Manager.X11` |
| `Tests.Portal.pas` | Wayland `GlobalShortcuts` portal backend: accelerator conversion and query helpers (always) and live round-trip/activation (opt-in via `ASUITECOMPS_TEST_PORTAL`). Compiled on Unix only; on Windows it asserts the backend is unix-only |
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
  including the constructor path used when no X11 display exists. The X11
  grabber is exercised only on a key the backend cannot express
  (`TestX11GrabberUnsupportedKey`), which never touches the X server.
- `TBaseHotkeyManager.RegisterNotify` keeps an item only when the platform
  `DoRegister` succeeds, and `UnregisterNotify` drops it only when
  `DoUnregister` succeeds. A failure therefore leaves no stale entry and can
  be retried; pinned by `TestRegisterFailureNotTracked`,
  `TestRegisterRetryAfterFailure` and `TestUnregisterFailureKeepsItem`.
- `TBaseHotkeyManager.RefreshNotify` is transactional: it returns early when
  the platform keeps the old binding, and drops the item when the
  re-registration fails; pinned by `TestRefreshUnregisterFailureKeepsItem`
  and `TestRefreshReregisterFailureDropsItem`.
- The portal backend applies the spec-compliant policy for every desktop
  without inspecting any desktop-environment variable: a fresh session for
  every change, so `BindShortcuts` is called once per session. The platform
  manager overrides `RefreshNotify` so `THotkeyItemsList.RefreshRegs` does not
  rebind per shortcut.
- Portal teardown keeps the desktop preferences: `DoShutdown` only drops the
  local tracking (pinned by `TestShutdownPortalLikeKeepsBackend`) while the
  default backend releases its bindings (`TestShutdownDefaultUnregisters`).
  `Reset` is the explicit remove-everything operation.
- A failed batch is rolled back (additions dropped, removals restored) and the
  rolled-back addition can be retried; pinned by `TestBatchRollbackOnFailure`,
  `TestBatchSuccessKeepsChanges` and `TestBatchRollbackAllowsRetry`.
- Portal queries (`QueryRegisteredById`/`QueryRegisteredByShortcut`, backed by
  `ListShortcuts`) are read-only and tri-state (`hqsUnknown`/`hqsAbsent`/
  `hqsPresent`); `QueryRegisteredByShortcut` returns host-level action keys and
  passing `nil` is reported as `hqsUnknown`. Trigger comparison is tri-state
  (`ComparePortalTriggers` -> `THotkeyTriggerMatch`): an unreadable description
  is `ptmUnknown`, never `ptmNo`. The portal id encoding is injective and
  reversible (`TestPortalIdInjective`). Backends without a queryable store
  return `hqsUnknown`.
- The D-Bus parsers (`ReadShortcutList`, `ReadResultsDict`) return False on a
  malformed structure instead of treating a partial read as an empty result;
  covered by `TestReadShortcutListParsing` and `TestReadResultsDictMalformed`
  against real libdbus messages.
- X11 grab/ungrab/probe live in `Hotkeys.Manager.X11` (`TX11KeyGrabber`): a
  temporary X error handler plus `XSync` turns the asynchronous `BadAccess`
  into a return value, the first error of a batch is preserved, foreign X
  errors are chained to the previous handler, and a failed batch is rolled
  back. `IsAvailable` reports success only when the probe grab was released
  again. The LCL<->X11 conversions are pure and unit-tested
  (`TestX11Conversions`); the unsupported-key path is covered by
  `TestX11GrabberUnsupportedKey`.
- The Qt backend uses the native event filter exposed by LCL's Qt binding
  (`QNativeEventFilter_hook`): the package ships no Qt helper library and no
  local `xcb` binding, and reads the few needed `xcb_key_press_event` fields
  directly.
- `Tests.Portal.TestLivePortalRoundTrip` (opt-in, `ASUITECOMPS_TEST_PORTAL=1`)
  verifies the binding through the portal itself (`VerifyRegistered`,
  `QueryByActionKey`, `QueryByTrigger`): present right after the bind, gone
  after `UnregisterShortcut`. No desktop-specific API is used.
- `Tests.Portal.TestLivePersistenceAcrossInstances` (opt-in,
  `ASUITECOMPS_TEST_PORTAL=1`) registers with a stable `ActionId`, tears the
  engine down normally, and checks with a new engine that the shortcut is still
  listed — proving that teardown keeps the desktop preference. It cleans up
  with an intentional `Reset`.
- `Tests.Portal.TestLiveQueryReadOnly` (opt-in,
  `ASUITECOMPS_TEST_PORTAL_QUERY=1`) exercises the read-only query path only:
  it creates a portal session and lists the shortcuts without binding, so it
  is safe to run without a permission dialog or a global grab.
- `TCustomButtonedEdit.SetFont` now assigns only when the fonts differ (the
  old inverted guard ignored every real assignment); covered by
  `TestFontAssignment`. `MaxLength`, `PasswordChar` and `Alignment` are
  forwarded to the inner edit and covered by `TestMaxLengthPasswordAlignment`.
- `TBCImageTab` click tests set `AlphaTest := False` and explicit bounds:
  an unrendered `BCImageButton` with `AlphaTest = True` touches a nil
  internal bitmap (upstream BGRAControls behavior, unrelated to this package).
