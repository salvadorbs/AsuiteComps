unit QGHotkeyHookPas;

{$mode objfpc}{$H+}

interface

uses Types, qt5;

{$MINENUMSIZE 4}

const
  ApplicationFlags = QT_VERSION or $1000000;

{DEFINE QT_5}

{$IFDEF MSWINDOWS}
  QGHotkeyPasLib = 'libQGHotkeyPas.dll';
{$ELSE}
  {$IFDEF DARWIN}
    QGHotkeyPasLib = '';
    {$LINKFRAMEWORK Qt5Pas}
  {$ELSE}
    QGHotkeyPasLib = 'QGHotkeyHookPas.so.1';
    {$IF DEFINED(LINUX) or DEFINED(FREEBSD) or DEFINED(NETBSD)}
      {$DEFINE BINUX}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


type
  PLong = ^Long;
{$ifdef CPU64 and not WIN64}
   Long = Int64;
{$else}
   Long = LongInt;
{$endif}

  QHookH = TMethod;

QGHotkey_hookH = class(TObject) end;

type
  QGHotkeyEvent = function (handle: QGHotkey_hookH; KeyCode: Cardinal; KeyState: Cardinal):boolean of object cdecl;

  function QGHotkey_hook_Create(handle : QCoreApplicationH) : QGHotkey_hookH; cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_Create';
  procedure QGHotkey_hook_Destroy(handle : QGHotkey_hookH ); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_Destroy';
  procedure QGHotkey_hook_hook_events(handle : QGHotkey_hookH; hook : QGHotkeyEvent); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_hook_events';
  procedure QGHotkey_hook_hook_destroyed(handle : QGHotkey_hookH; hook : QObject_destroyed_Event); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_hook_destroyed';

implementation

end.



