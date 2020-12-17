//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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

void Q_GHotkey_hook_hook_events(Q_GHotkey_hookH handle, QHookH hook)
{
    ((Q_GHotkey_hook *)handle)->hook_events(hook);
}

void Q_GHotkey_hook_hook_destroyed(Q_GHotkey_hookH handle, QHookH hook)
{
    ((Q_GHotkey_hook *)handle)->hook_destroyed(hook);
}
