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

#include <QX11Info>
#include <X11/Xlib.h>
#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>


class Q_GHotkey_hook : public QAbstractNativeEventFilter {
  //Q_OBJECT

  public:

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
      if (!hook.func) handle->removeNativeEventFilter(this);
      events = hook;
    }
  }
  void hook_removefilter() {
    if (handle) {
      handle->removeNativeEventFilter(this);
    }
  }

  void hook_destroyed(QHook &hook) {
    destroyed_event = hook;
  }

  protected:

    QCoreApplication *handle;

    virtual bool nativeEventFilter(const QByteArray &eventType, void *message, long *result) {
      if (events.func) {
        auto *genericEvent = static_cast<xcb_generic_event_t *>(message);

        //Pass to FPC only XCB_KEY_PRESS
        if (genericEvent->response_type == XCB_KEY_PRESS) {
          Q_GHotkey_hook* sender = this;

          // Pass keyCode and Modifiers to FPC
          xcb_key_press_event_t *keyEvent = static_cast<xcb_key_press_event_t *>(message);
          uint8_t keyCode = (uint8_t) keyEvent->detail;
          uint16_t keyState = (uint8_t) keyEvent->state;

          typedef bool (*func_type)(void *data, Q_GHotkey_hook* sender, uint8_t keyCode, uint16_t keyState);
          return (*(func_type)events.func)(events.data, sender, keyCode, keyState);
        }
      }
      return false;
    }

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

#endif
