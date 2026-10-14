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

unit SystemIcons.Platform;

{$I ASuiteComps.inc}

interface

uses
  SysUtils, Classes, Graphics, BGRABitmap, SystemIcons.Mime;

type

  { Unix system icon extraction (freedesktop theme + MIME database).

    AMime providers are owned by the caller, which is also responsible for
    loading them (see TMimeIconProvider.LoadFromFiles); a nil or empty
    provider simply yields the generic fallback icons. }

  TSystemIconHelper = class
  private
    class function ResolveExtension(const AFileName: string): string;
    class function IconByDesktopFile(const AFileName: string): string;
    class function ExtractBitmap(const AIconName: string; ASize: Integer): TBitmap;
    {$IFDEF GTK}
    class function PixBufToBitmap(APixbuf: Pointer): TBitmap;
    {$ENDIF}
  public
    { Maps AFileName to a theme icon name without querying the theme
      (first MIME candidate wins). Deterministic and display-independent. }
    class function MapFileToIconName(const AFileName: string; AMime: TMimeIconProvider): string;
    { Checks a theme icon name (or absolute icon path) against the running
      theme. Needs a display server on some widgetsets. }
    class function ThemeIconExists(const AIconName: string): Boolean;
    { Extracts the system icon, picking the first theme icon that really
      exists. Never returns nil. Needs a display server on some widgetsets. }
    class function GetSystemIcon(const AFileName: string; ASize: Integer;
      AMime: TMimeIconProvider): TBGRABitmap;
  end;

implementation

uses
  IniFiles, BaseUnix, LazFileUtils, LCLIntf, LCLType
  {$IFDEF LCLGTK2}
  , gdk2pixbuf, gtk2, glib2, gdk2, GraphType, IntfGraphics
  {$ENDIF}
  {$IFDEF LCLGTK3}
  , lazgdkpixbuf2, lazgtk3, LazGLib2, LazGdk3, LazGObject2, GraphType, IntfGraphics
  {$ENDIF}
  {$IFDEF LCLQT5}
  , qt5, qtobjects, Types
  {$ENDIF}
  {$IFDEF LCLQT6}
  , qt6, qtobjects, Types
  {$ENDIF}
  {$IFDEF LCLQT}
  , qtobjects, Types
  {$ENDIF}
  ;

const
  DESKTOP_GROUP = 'Desktop Entry';
  DESKTOP_KEY_ICON = 'Icon';

{ TSystemIconHelper }

class function TSystemIconHelper.ResolveExtension(const AFileName: string): string;
begin
  // It is a link? Ok, get target file icon
  if FpReadLink(AFileName) <> '' then
    Result := '*' + ExtractFileExt(FpReadLink(AFileName))
  else
    Result := '*' + ExtractFileExt(AFileName);

  Result := LowerCase(Result);
end;

class function TSystemIconHelper.IconByDesktopFile(const AFileName: string): string;
var
  iniDesktop: TIniFile = nil;
begin
  Result := '';

  try
    iniDesktop := TIniFile.Create(AFileName);
    try
      Result := iniDesktop.ReadString(DESKTOP_GROUP, DESKTOP_KEY_ICON, '');
    finally
      FreeAndNil(iniDesktop);
    end;
  except
    Exit;
  end;
end;

class function TSystemIconHelper.MapFileToIconName(const AFileName: string;
  AMime: TMimeIconProvider): string;
var
  Extension: string;
  names: TStringList;
begin
  Result := '';
  Extension := ResolveExtension(AFileName);

  //TODO: Special folders icon https://gitlab.gnome.org/GNOME/glib/-/commit/129eb074823101102611690f053ffa246bb7784d#3549e1301fc4c17bf0dd809eca0a36fb87aac264_1582_1582

  if DirectoryExists(AFileName) then
  begin
    if FileExists(AppendPathDelim(AFileName) + '.directory') then
      Result := IconByDesktopFile(AppendPathDelim(AFileName) + '.directory')
    else
      Result := 'folder';
  end
  else if Extension = '*.desktop' then
    Result := IconByDesktopFile(AFileName)
  else if Extension = '*.ico' then
    Result := AFileName
  else if Extension <> '*' then
  begin
    if Assigned(AMime) then
      names := AMime.GetIconNames(Extension)
    else
      names := nil;
    if Assigned(names) and (names.Count > 0) then
      Result := names[0];
  end
  else if FileIsExecutable(AFileName) then
    Result := 'application-x-executable';

  // Not found icon? No problem. Use generic icon
  if Result = '' then
  begin
    if FileIsText(AFileName) then
      Result := 'text-x-generic'
    else
      Result := 'unknown';
  end;
end;

class function TSystemIconHelper.ThemeIconExists(const AIconName: string): Boolean;
{$IFDEF QT}
var
  sName: WideString;
{$ENDIF}
begin
  Result := False;
  if AIconName = '' then
    Exit;
  {$IFDEF QT}
  sName := WideString(AIconName);
  Result := QIcon_hasThemeIcon(@sName) or FileExists(AIconName);
  {$ELSE}
  {$IFDEF GTK}
  if gtk_icon_theme_has_icon(gtk_icon_theme_get_default,
    PAnsiChar(UTF8Encode(AIconName))) then
    Result := True
  else
    Result := FileExists(AIconName);
  {$ELSE}
  Result := FileExists(AIconName);
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF GTK}
class function TSystemIconHelper.PixBufToBitmap(APixbuf: Pointer): TBitmap;
var
  Pixbuf: PGdkPixbuf;
  width, height, rowstride, n_channels, i, j: Integer;
  pixels: PByte;
  pSrc: PByte;
  pDst: PLongWord;
  BmpData: TLazIntfImage;
  hasAlphaChannel: Boolean;
  QueryFlags: TRawImageQueryFlags = [riqfRGB];
  Description: TRawImageDescription;
  ARawImage: TRawImage;
begin
  Result := nil;
  Pixbuf := PGdkPixbuf(APixbuf);

  n_channels:= gdk_pixbuf_get_n_channels(Pixbuf);

  if ((n_channels <> 3) and (n_channels <> 4)) or  // RGB or RGBA
     (gdk_pixbuf_get_colorspace(pixbuf) <> GDK_COLORSPACE_RGB) or
     (gdk_pixbuf_get_bits_per_sample(pixbuf) <> 8) then Exit;

  width:= gdk_pixbuf_get_width(Pixbuf);
  height:= gdk_pixbuf_get_height(Pixbuf);
  rowstride:= gdk_pixbuf_get_rowstride(Pixbuf);
  pixels:= PByte(gdk_pixbuf_get_pixels(Pixbuf));
  hasAlphaChannel:= gdk_pixbuf_get_has_alpha(Pixbuf);

  if hasAlphaChannel then
    Include(QueryFlags, riqfAlpha);

  BmpData := TLazIntfImage.Create(width, height, QueryFlags);
  try
    BmpData.CreateData;
    Description := BmpData.DataDescription;

    pDst := PLongWord(BmpData.PixelData);
    for j:= 0 to Height - 1 do
    begin
      pSrc := PByte(pixels) + j * rowstride;
      for i:= 0 to Width - 1 do
      begin
        pDst^ := pSrc[0] shl Description.RedShift +
                 pSrc[1] shl Description.GreenShift +
                 pSrc[2] shl Description.BlueShift;

        if hasAlphaChannel then
          pDst^ := pDst^ + pSrc[3] shl Description.AlphaShift;

        Inc(pSrc, n_channels);
        Inc(pDst);
      end;
    end;

    Result := TBitmap.Create;

    BmpData.GetRawImage(ARawImage, True);
    // Simply change raw image owner without data copy
    Result.LoadFromRawImage(ARawImage, True);

    if not hasAlphaChannel then
      Result.Transparent := True;

  finally
    BmpData.Free;
  end;
end;
{$ENDIF}

class function TSystemIconHelper.ExtractBitmap(const AIconName: string; ASize: Integer): TBitmap;
{$IFDEF GTK}
var
  pbPicture: PGdkPixbuf = nil;
{$ENDIF}
{$IFDEF QT}
var
  QIcon: QIconH;
  APixmap: QPixmapH;
  AImage: QImageH;
  Size: Types.TSize;
  sName: WideString;
{$ENDIF}
begin
  Result := TBitmap.Create;

  {$IFDEF GTK}
  if FileExists(AIconName) then
    pbPicture := gdk_pixbuf_new_from_file_at_size(PAnsiChar(UTF8Encode(AIconName)), ASize, ASize, nil)
  else
    begin
      {$IFDEF LCLGTK2}
      pbPicture := gtk_icon_theme_load_icon(gtk_icon_theme_get_for_screen(gdk_screen_get_default),
        Pgchar(PAnsiChar(UTF8Encode(AIconName))), ASize, GTK_ICON_LOOKUP_USE_BUILTIN, nil);
      {$ELSE}
      pbPicture := gtk_icon_theme_load_icon(gtk_icon_theme_get_for_screen(gdk_screen_get_default),
        Pgchar(PAnsiChar(UTF8Encode(AIconName))), ASize, [GTK_ICON_LOOKUP_USE_BUILTIN], nil);
      {$ENDIF}
    end;
  try
    if Assigned(pbPicture) then
    begin
      Result.Free;
      Result := PixBufToBitmap(pbPicture);
      if Result = nil then
        Result := TBitmap.Create;
    end;
  finally
    if Assigned(pbPicture) then
      g_object_unref(pbPicture);
  end;
  {$ENDIF}

  {$IFDEF QT}
  Size := Types.Size(ASize, ASize);
  sName := WideString(AIconName);
  QIcon := QIcon_Create();
  try
    QIcon_fromTheme(QIcon, @sName);
    APixmap := QPixmap_create();
    try
      QIcon_pixmap(QIcon, APixmap, Types.PSize(@Size));
      AImage := QImage_create();
      try
        QPixmap_toImage(APixmap, AImage);
        Result.Handle := HBitmap(TQtImage.Create(AImage));
      finally
        // AImage ownership moves to TQtImage, mirroring previous behaviour
      end;
    finally
      QPixmap_destroy(APixmap);
    end;
  finally
    QIcon_destroy(QIcon);
  end;
  {$ENDIF}

  {$IFNDEF GTK}
  {$IFNDEF QT}
  // No theme backend for this widgetset: leave the empty bitmap
  {$ENDIF}
  {$ENDIF}
end;

class function TSystemIconHelper.GetSystemIcon(const AFileName: string; ASize: Integer;
  AMime: TMimeIconProvider): TBGRABitmap;
var
  Extension, iconName: string;
  names: TStringList;
  I: Integer;
  bmp: TBitmap;
begin
  Result := nil;
  Extension := ResolveExtension(AFileName);

  if DirectoryExists(AFileName) or (Extension = '*.desktop') or (Extension = '*.ico') or
    (Extension = '*') or not Assigned(AMime) then
    iconName := MapFileToIconName(AFileName, AMime)
  else
  begin
    // First valid theme icon wins, like the desktop shell does
    iconName := '';
    names := AMime.GetIconNames(Extension);
    if Assigned(names) then
      for I := 0 to names.Count - 1 do
      begin
        iconName := names[I];
        if ThemeIconExists(iconName) then
          Break;
      end;
    if (iconName = '') or not ThemeIconExists(iconName) then
      iconName := MapFileToIconName(AFileName, AMime);
  end;

  bmp := ExtractBitmap(iconName, ASize);
  try
    if Assigned(bmp) then
      Result := TBGRABitmap.Create(bmp)
    else
      Result := TBGRABitmap.Create;
  finally
    bmp.Free;
  end;
end;

end.
