//******************************************************************************
//  Copyright (C) 2006-2015 Matteo Salvi
//  
//  Website: http://www.salvadorsoftware.com/
//  
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//******************************************************************************

#ifndef Q_GHOTKEY_HOOK_C_H
#define Q_GHOTKEY_HOOK_C_H

#include "qghotkey_hook.h"
#include "pascalbind.h"

C_EXPORT Q_GHotkey_hookH Q_GHotkey_hook_Create(QCoreApplicationH handle);
C_EXPORT void Q_GHotkey_hook_Destroy(Q_GHotkey_hookH handle);
C_EXPORT void Q_GHotkey_hook_hook_installfilter(Q_GHotkey_hookH handle, QHookH hook);
C_EXPORT void Q_GHotkey_hook_hook_destroyed(Q_GHotkey_hookH handle, QHookH hook);
C_EXPORT void Q_GHotkey_hook_hook_removefilter(Q_GHotkey_hookH handle);

#endif
