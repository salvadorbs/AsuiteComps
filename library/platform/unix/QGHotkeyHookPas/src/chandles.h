//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************



#ifndef CHANDLES_H
#define CHANDLES_H

#if defined _LP64
typedef long long int PTRINT;
typedef unsigned long long int PTRUINT;
#else
typedef int PTRINT;
typedef unsigned int PTRUINT;
#endif

typedef struct Q_GHotkey_hook__ { PTRINT dummy; } *Q_GHotkey_hookH;
typedef struct Q_GHotkey__ { PTRINT dummy; } *Q_GHotkeyH;
typedef struct QCoreApplication__ { PTRINT dummy; } *QCoreApplicationH;
#endif
