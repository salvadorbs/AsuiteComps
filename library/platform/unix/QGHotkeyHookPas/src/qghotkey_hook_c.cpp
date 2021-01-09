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

#include "qghotkey_hook_c.h"

Q_GHotkey_hookH Q_GHotkey_hook_Create(QCoreApplicationH handle)
{
    return (Q_GHotkey_hookH) new Q_GHotkey_hook((QCoreApplication*)handle);
}

void Q_GHotkey_hook_Destroy(Q_GHotkey_hookH handle)
{
    delete (Q_GHotkey_hook *)handle;
}

void Q_GHotkey_hook_hook_installfilter(Q_GHotkey_hookH handle, QHookH hook)
{
    ((Q_GHotkey_hook *)handle)->hook_installfilter(hook);
}

void Q_GHotkey_hook_hook_destroyed(Q_GHotkey_hookH handle, QHookH hook)
{
    ((Q_GHotkey_hook *)handle)->hook_destroyed(hook);
}

void Q_GHotkey_hook_hook_removefilter(Q_GHotkey_hookH handle)
{
    ((Q_GHotkey_hook *)handle)->hook_removefilter();
}
