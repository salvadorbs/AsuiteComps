{
Copyright (C) 2006-2021 Matteo Salvi

Website: http://www.salvadorsoftware.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit QGHotkeyHookPas;

{$mode objfpc}{$H+}

interface

uses
  Types

  {$IFDEF LCLQT5}
  , qt5
  {$ENDIF}

  {$IFDEF LCLQT6}
  , qt6
  {$ENDIF};

{$MINENUMSIZE 4}

const
  ApplicationFlags = QT_VERSION or $1000000;

{$IFNDEF MSWINDOWS or DARWIN}
  QGHotkeyPasLib = 'QGHotkeyHookPas.so.1';
  {$IF DEFINED(LINUX) or DEFINED(FREEBSD) or DEFINED(NETBSD)}
    {$DEFINE BINUX}
  {$ENDIF}
{$ENDIF}


type
  QHookH = TMethod;

  QGHotkey_hookH = class(TObject) end;

type
  QGHotkeyEvent = function (handle: QGHotkey_hookH; eventType: QByteArrayH; message: Pointer):boolean of object cdecl;

  function QGHotkey_hook_Create(handle : QCoreApplicationH) : QGHotkey_hookH; cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_Create';
  procedure QGHotkey_hook_Destroy(handle : QGHotkey_hookH ); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_Destroy';
  procedure QGHotkey_hook_hook_installfilter(handle : QGHotkey_hookH; hook : QGHotkeyEvent); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_hook_installfilter';
  procedure QGHotkey_hook_hook_destroyed(handle : QGHotkey_hookH; hook : QObject_destroyed_Event); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_hook_destroyed';
  procedure QGHotkey_hook_hook_removefilter(handle : QGHotkey_hookH); cdecl; external QGHotkeyPasLib name 'Q_GHotkey_hook_hook_removefilter';

  {$IFDEF LCLQT6}
  function QX11Info_display(): PDisplay; cdecl; external QGHotkeyPasLib name 'QX11Info_display';    
  {$ENDIF}

implementation

end.
