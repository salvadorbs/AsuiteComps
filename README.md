# ASuiteComps

FreePascal/Lazarus cross-platform components and libraries.

## Contents

Visual components (palette `ASuite Components`):

| Component | Unit | Description |
|---|---|---|
| `TBCImageTab` | `visual/BCImageTab.pas` | Toggle image button with group exclusivity (tab behavior), based on BGRAControls |
| `TButtonedEdit` | `visual/buttonededit.pas` | Edit with optional left/right glyph buttons (`MaxLength`, `PasswordChar`, `Alignment`, `OnEnter`/`OnExit`) |
| `THotKey` | `visual/HotKey.pas` | Low-level shortcut capture control (direct keyboard capture) |
| `THotKeyEdit` | `visual/HotKeyEdit.pas` | Read-only buttoned edit for a shortcut; opens the grabber, right button clears/chooses |
| `TfrmShortcutGrabber` | `visual/ShortcutGrabber.pas` | Modal "choose hotkey" dialog with Ctrl/Alt/Shift/Win toggle buttons |

Libraries:

| Unit | Description |
|---|---|
| `library/Hotkeys.ShortcutEx.pas` | `TShortcutEx`: shortcut value object (key, modifiers, tag, stable `ActionId`, notify callback) |
| `library/Hotkeys.Manager.pas` | `TBaseHotkeyManager`: OS-independent global-hotkey list management (`AppToken`, registration queries) |
| `library/platform/win/Hotkeys.Manager.Platform.pas` | Windows implementation (`RegisterHotKey`, one message window per manager) |
| `library/platform/unix/Hotkeys.Manager.Platform.pas` | Unix implementation: backend selection, widgetset filter, portal delegation. Uses `Hotkeys.Manager.X11` |
| `library/platform/unix/Hotkeys.Manager.X11.pas` | X11 primitives: LCL<->X11 conversions and `TX11KeyGrabber` (grab/ungrab/probe with asynchronous-error handling and rollback) |
| `library/Hotkeys.Manager.Portal.pas` | Wayland `GlobalShortcuts` portal backend (freedesktop portal over `libdbus`), selected when X11 is unavailable |

Qt uses the native event filter provided by LCL's Qt binding, so no external
helper library and no local `xcb` binding are required.

## Shortcut grabber

`TfrmShortcutGrabber` and `THotKey` live in the same package and can be used
together or independently.

### `THotKey` (direct capture)

`THotKey` is the low-level capture control: the shortcut is typed directly
into it (`NoModifier` allows a bare key). It is also the capture engine used
inside `TfrmShortcutGrabber`. Setting `Hotkey` programmatically fires
`OnChange` (and does nothing when the value is unchanged).

The capture rules live in the non-visual `TShortcutCapture` class
(`visual/HotKey.pas`), so they can be reused and tested without a control.

```pascal
HotKey1.OnChange := @HotKey1Change;
HotKey1.NoModifier := False; // require at least one modifier
```

To pick a shortcut through the dialog, use `THotKeyEdit` (below) or open the
dialog explicitly.

### Standalone dialog

`TryExecute` distinguishes "confirmed" from "cancelled" and returns the chosen
value through an `out` parameter:

```pascal
var
  Shortcut: TShortCut;
begin
  if TfrmShortcutGrabber.TryExecute(Self, TextToShortCut(Edit1.Text), Shortcut) then
    Edit1.Text := ShortCutToText(Shortcut);
end;
```

`Execute` is the historical shortcut: it returns the chosen shortcut, or `0`
when the dialog is cancelled.

A string-based overload is available too:

```pascal
Edit1.Text := TfrmShortcutGrabber.Execute(Self, Edit1.Text);
```

### Direct binding to a `THotKey`

`TfrmShortcutGrabber.TargetHotKey` binds the dialog to a control: it starts
from its `Hotkey` and writes the confirmed value back (firing its `OnChange`).

```pascal
Form := TfrmShortcutGrabber.Create(Self);
try
  Form.TargetHotKey := HotKey1;
  if Form.ShowModal = mrOk then
    // HotKey1.Hotkey is already updated
    Caption := ShortCutToText(HotKey1.Hotkey);
finally
  Form.Free;
end;
```

### `THotKeyEdit` (buttoned edit)

`THotKeyEdit` is a `TButtonedEdit` descendant that shows a shortcut and opens
the grabber on click. It reuses `TfrmShortcutGrabber`, so no wiring is needed:

```pascal
HotKeyEdit1.Hotkey := ShortCut(VK_F5, [ssCtrl]);   // read/write, Text in sync
HotKeyEdit1.OnHotkeyChange := @HotKeyEdit1Change;  // fired when it changes
```

- clicking the edit opens the grabber;
- the right button clears the shortcut, or opens the grabber when empty;
- `ClearImageIndex` / `ChooseImageIndex` pick the right-button glyphs
  (`RightButton.Images` supplies the image list);
- `ButtonVisibleOnlyWithHotkey` hides the button when no shortcut is set;
- `OnValidateHotkey` is forwarded to the grabber (otherwise
  `ShortcutGrabberDefaults.OnValidateHotkey` applies).

### Button images

The four modifier buttons (`Ctrl`, `Alt`, `Shift`, `Win`) are fully
customizable through `TPicture` properties:

- `TfrmShortcutGrabber.Images` — per-instance images
  (`Images.Ctrl`, `Images.Alt`, `Images.Shift`, `Images.WinKey`);
- `ShortcutGrabberDefaults.Images` — process-wide images, used when the
  per-instance picture is empty;
- embedded defaults — Lazarus resources (`asuite_ctrl`, `asuite_alt`,
  `asuite_shift`, `asuite_winkey`) shipped with the package's default theme.

The application decides which images to use: the component never reads a
theme folder by itself. Assign them however you prefer (`TPicture` handles
file/stream loading and accepts any `TGraphic`):

```pascal
Grabber.Images.Ctrl.LoadFromFile('ctrl.png');
Grabber.Images.Alt.Assign(MyPng);
ShortcutGrabberDefaults.Images.WinKey.Clear; // fall back to the default
```

Changing a picture reloads the corresponding button automatically
(`TShortcutGrabberImages.OnChange`). For convenience the form also exposes
`CtrlImage`, `AltImage`, `ShiftImage` and `WinKeyImage` aliases.

The lookup order is: per-instance `TPicture` → `ShortcutGrabberDefaults.Images`
→ embedded Lazarus resource. Missing images never raise an error.

At runtime only the compiled resource `visual/ShortcutGrabber.lrs` is used.
`visual/buttons/` keeps the four source PNGs (byte-identical to the compiled
resources) so the `.lrs` can be regenerated from this repository alone
with `lazres`:

```bash
cd visual
lazres ShortcutGrabber.lrs buttons/asuite_ctrl.png buttons/asuite_alt.png \
  buttons/asuite_shift.png buttons/asuite_winkey.png
```

The four PNGs are the button images of the package's default theme and are
distributed under the same GPL v3+ license as the rest of the package.

### Process-wide defaults

`ShortcutGrabberDefaults` (a `TShortcutGrabberDefaults` instance) configures
the dialog for the whole application:

```pascal
var
  Manager: TBaseHotkeyManager;
begin
  ShortcutGrabberDefaults.MessageNoKey        := '...';
  ShortcutGrabberDefaults.MessageNoModifier   := '...';
  ShortcutGrabberDefaults.MessageNotAvailable := '...';
  ShortcutGrabberDefaults.Images.Ctrl.LoadFromFile('/path/to/theme/ctrl.png');

  Manager := HotkeyManager; // from Hotkeys.Manager.Platform
  ShortcutGrabberDefaults.OnValidateHotkey := Manager.IsHotkeyAvailable;
end;
```

When `OnValidateHotkey` is assigned, the dialog refuses a shortcut already
taken (returning `False` keeps the dialog open and shows the "not available"
message). When it is `nil`, no check is performed, so the component stays
independent from the global hotkey manager.

## Global hotkeys

`HotkeyManager` (from `Hotkeys.Manager.Platform`) returns the platform
manager that binds shortcuts system-wide:

- Windows — `RegisterHotKey` on a message window owned by the manager;
- Unix — X11 `XGrabKey` on GTK2, GTK3, Qt5 and Qt6 (the key events are
  filtered through the widgetset: a GDK filter for GTK, the native event
  filter of LCL's Qt binding for Qt);
- Wayland without X11 — the freedesktop `GlobalShortcuts` portal over
  `libdbus` (`Hotkeys.Manager.Portal`). The application must pump
  `ProcessPending` from its main loop; the manager already installs a timer.

`TBaseHotkeyManager.AppToken` (namespacing, frozen after the first
registration on the portal) namespaces the portal session, request handles and
shortcut ids. The portal backend always applies the spec-compliant policy: a
fresh session for every change, so `BindShortcuts` is called once per session.
No desktop is inspected.

On teardown the portal backend closes the session **without** unbinding: a
Wayland binding is persistent and belongs to the user's desktop settings.
`TBaseHotkeyManager.ClearAllHotkeys` / `UnregisterNotify` remove the action
from the active set while the manager runs; the portal engine's `Reset` is the
explicit "remove everything" operation.

### Stable action identity

A portal binding is persistent: it must be tied to the *action*, not to the key
combination, or changing the shortcut would create a new desktop entry. Pass a
stable id with `RegisterNotifyEx(Shortcut, Notify, Tag, ActionId)`;
`TShortcutEx.ActionId` is the public host-level identity used by the portal
backend to derive its shortcut id. `Tag` is a runtime callback value kept as
the compatibility fallback for existing callers; anonymous shortcuts fall back
to the key. The portal id is an injective encoding of the host key, so distinct
ActionIds never collide (`open-home` and `open_home` are different actions).

### Querying the portal

The portal can be asked, read-only, whether an action or a key combination is
currently registered:

```pascal
var Status: THotkeyQueryStatus; Trigger: String; Match: THotkeyTriggerMatch;
Status := HotkeyManager.QueryRegisteredById('open-home', Trigger);
```

`QueryRegisteredById` / `QueryRegisteredByShortcut` return a tri-state
(`hqsUnknown` / `hqsAbsent` / `hqsPresent`): a plain Boolean could not tell
"not registered" apart from "cannot be checked". `QueryRegisteredByShortcut`
reports host-level action keys (the same strings accepted by
`QueryRegisteredById`). They answer about the shortcuts the portal exposes to
this application/session, not about every global shortcut of the desktop, and
never bind or unbind anything. Backends without a queryable store (X11,
Windows) return `hqsUnknown`.

The portal only returns a human-readable `trigger_description`, so trigger
comparison is tri-state too: `THotkeyTriggerMatch` (`ptmUnknown` / `ptmNo` /
`ptmYes`). A description that cannot be interpreted is `ptmUnknown`, never
mistaken for a different trigger.

A registration is only confirmed once the portal accepted the bind: outside a
bulk update `RegisterNotify` reflects the bind response; inside
`BeginHotkeyUpdate`/`EndHotkeyUpdate` the authoritative verdict is the one
returned by `EndHotkeyUpdate`. When a batch fails, the manager rolls it back
(additions dropped, removals restored) so the state stays consistent and a
retry is possible.

`OnTriggerChanged` reports the trigger actually assigned by the desktop;
`OnTriggerChangedEx` also carries the stable `ActionId`, which is the reliable
identity when several actions share a `Tag` or use `Tag = -1`.

## Requirements

- Lazarus + FPC (see the Lazarus
  [installer](https://www.lazarus-ide.org/index.php?page=downloads) and the
  [Free Pascal documentation](https://www.freepascal.org/docs.html))
- [BGRABitmap](https://github.com/bgrabitmap/bgrabitmap) (`BGRABitmapPack`)
- [BGRAControls](https://github.com/bgrabitmap/bgracontrols)
- Linux: `libdbus-1` development files (`libdbus-1-dev` / `dbus-devel`),
  linked by the Wayland portal backend

## Install in the IDE

1. Open `ASuiteComps.lpk` in Lazarus.
2. `Package` → `Install`, then rebuild the IDE.

## Automated tests

The `tests/` directory holds an FPCUnit suite (144 tests) covering every
component, the hotkey manager logic and the platform managers:

```bash
./tests/run_tests.sh --widgetset=gtk3   # gtk2 | gtk3 | qt5 | qt6 | win32
```

Options: `--lazbuild <path>` (default: `lazbuild` from `PATH`).
On Linux without a display server the script re-executes itself under
`xvfb-run` automatically. See `tests/README.md` for details.

GitHub Actions (`.github/workflows/tests.yml`) runs the suite on
Windows and Linux (GTK2/GTK3/Qt5/Qt6) at every push and pull request.

## Samples

- `samples/TButtonedEdit/` — demo project for `TButtonedEdit`
- `samples/THotkeyManager/` — demo project for global hotkeys + `THotKey`
  (direct capture, plus an explicit "Choose..." button opening the grabber)

## License

GNU General Public License v3.0 or later.
