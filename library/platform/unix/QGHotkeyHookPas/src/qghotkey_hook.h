//******************************************************************************
//  Copyright (C) 2006-2021 Matteo Salvi
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

#ifndef Q_GHOTKEY_HOOK_H
#define Q_GHOTKEY_HOOK_H

#include <qcoreapplication.h>
#include <qabstractnativeeventfilter.h>
#include "pascalbind.h"

#include <xcb/xcb.h>

#if QT_VERSION >= QT_VERSION_CHECK(6, 2, 0)
	#define _NATIVE_EVENT_RESULT qintptr
#else
	#define _NATIVE_EVENT_RESULT long
#endif


class Q_GHotkey_hook : public QAbstractNativeEventFilter {
  //Q_OBJECT

  public:  
	bool nativeEventFilter(const QByteArray &eventType, void *message, _NATIVE_EVENT_RESULT *result) override;

  Q_GHotkey_hook(QCoreApplication *handle) : QAbstractNativeEventFilter() {
    this->handle = handle;
    this->events.func = NULL;
    this->destroyed_event.func = NULL;
    //connect(handle, SIGNAL(destroyed()), this, SLOT(destroyed_hook()));
  }

  virtual ~Q_GHotkey_hook() {
    if (handle) {
      handle->removeNativeEventFilter(this);
      handle = NULL;
    }
  }

  void hook_installfilter(QHook &hook) {
    if (handle) {
      if (!events.func) {
        handle->installNativeEventFilter(this);
        events = hook;
      }
      if (!hook.func)
        handle->removeNativeEventFilter(this);
      events = hook;
    }
  }
  void hook_removefilter() {
    if (handle) {
      handle->removeNativeEventFilter(this);   
      events.func = NULL;
    }
  }

  void hook_destroyed(QHook &hook) {
    destroyed_event = hook;
  }

  protected:

    QCoreApplication *handle;

  private slots:

    void destroyed_hook() {
      if ( destroyed_event.func ) {
        typedef void (*func_type)(void *data);
        (*(func_type)destroyed_event.func)(destroyed_event.data);
      }
      handle = NULL;
    }

  private:
    QHook events;
    QHook destroyed_event;
};


bool Q_GHotkey_hook::nativeEventFilter(const QByteArray &eventType, void *message, _NATIVE_EVENT_RESULT *result) {
  if (events.func) {
    Q_GHotkey_hook* sender = this;
    typedef bool (*func_type)(void *data, Q_GHotkey_hook* sender, const QByteArray &eventType, void *message);
    return (*(func_type)events.func)(events.data, sender, eventType, message);
  }
  return false;
}

#endif
