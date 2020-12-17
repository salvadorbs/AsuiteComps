//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************



#ifndef PASCALBIND_H
#define PASCALBIND_H

#include <QtCore>
#include <QtGui>

#include "chandles.h"


#if defined(__WIN32__)
#define C_EXPORT extern "C" __declspec( dllexport )
#else
#define C_EXPORT extern "C"
#endif

#ifdef __WIN32__
#include <windows.h>
#endif

typedef bool (*NativeEventFilter)(const QByteArray &eventType, void *message, long *result);

#if defined _LP64
typedef long long int PTRINT;
typedef unsigned long long int PTRUINT;
#else
typedef int PTRINT;
typedef unsigned int PTRUINT;
#endif

typedef struct {
  void *func;
  void *data;
} QHook;
typedef QHook QHookH;
typedef QHook QOverrideHook;
typedef QOverrideHook QOverrideHookH;

#endif
