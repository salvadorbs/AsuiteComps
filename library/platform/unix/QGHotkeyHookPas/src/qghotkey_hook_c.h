//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef Q_GHOTKEY_HOOK_C_H
#define Q_GHOTKEY_HOOK_C_H

#include "qghotkey_hook.h"
#include "pascalbind.h"

C_EXPORT Q_GHotkey_hookH Q_GHotkey_hook_Create(QCoreApplicationH handle);
C_EXPORT void Q_GHotkey_hook_Destroy(Q_GHotkey_hookH handle);
C_EXPORT void Q_GHotkey_hook_hook_events(Q_GHotkey_hookH handle, QHookH hook);
C_EXPORT void Q_GHotkey_hook_hook_destroyed(Q_GHotkey_hookH handle, QHookH hook);

#endif
