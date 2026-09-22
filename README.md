# ASuiteComps

FreePascal/Lazarus cross-platform components and libraries for
[ASuite](https://github.com/salvadorbs/Asuite).

## Contents

Visual components (palette `ASuite Components`):

| Component | Unit | Description |
|---|---|---|
| `TBCImageTab` | `visual/BCImageTab.pas` | Toggle image button with group exclusivity (tab behavior), based on BGRAControls |
| `TButtonedEdit` | `visual/buttonededit.pas` | Edit with optional left/right glyph buttons |
| `THotKey` | `visual/HotKey.pas` | Hotkey capture control |

Libraries:

| Unit | Description |
|---|---|
| `library/Hotkeys.ShortcutEx.pas` | `TShortcutEx`: shortcut value object (key, modifiers, tag, notify callback) |
| `library/Hotkeys.Manager.pas` | `TBaseHotkeyManager`: OS-independent global-hotkey list management |
| `library/platform/win/Hotkeys.Manager.Platform.pas` | Windows implementation (`RegisterHotKey`) |
| `library/platform/unix/Hotkeys.Manager.Platform.pas` | Unix implementation (X11 `XGrabKey`; GTK2/GTK3/Qt5/Qt6). Safely dormant when no X11 display exists (e.g. pure Wayland) |

## Requirements

- Lazarus + FPC (see [ASuite BUILD.md](https://github.com/salvadorbs/Asuite/blob/develop/BUILD.md))
- [BGRABitmap](https://github.com/bgrabitmap/bgrabitmap) (`BGRABitmapPack`)
- [BGRAControls](https://github.com/bgrabitmap/bgracontrols)

## Install in the IDE

1. Open `ASuiteComps.lpk` in Lazarus.
2. `Package` → `Install`, then rebuild the IDE.

## Automated tests

The `tests/` directory holds an FPCUnit suite (72 tests) covering every
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

## License

GNU General Public License v3.0 or later, like ASuite.
