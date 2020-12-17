VERSION = 1.0.0

QT += gui x11extras
TARGET = QGHotkeyHookPas
TEMPLATE = lib
VPATH = src
MOC_DIR = tmp
OBJECTS_DIR = tmp
QMAKE_CXXFLAGS += -Wfatal-errors

# Match Intel x86_64 i686 i586 i386 x86 ...
is86 = $$find(QMAKE_HOST.arch, ".*86")
# Match 64 bit
is64 = $$find(QMAKE_HOST.arch, ".*64")

target.path = $$[QT_INSTALL_LIBS]

unix:!embedded:!mac:!haiku:PLATFORM = BINUX

CONFIG -= debug_and_release
CONFIG -= debug_and_release_target
CONFIG -= debug
CONFIG -= warn_on
CONFIG -= create_prl
CONFIG -= link_prl

CONFIG -= release
CONFIG += debug
CONFIG += dll
CONFIG += warn_off

DEFINES += $$PLATFORM

INSTALLS += target

HEADERS += \
    src/chandles.h \
    src/pascalbind.h \
    src/qghotkey_hook.h \
    src/qghotkey_hook_c.h
SOURCES += \
    src/qghotkey_hook_c.cpp
