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
  SysUtils, Classes, BGRABitmap, Windows, SystemIcons.Mime;

type

  { Windows system icon extraction (shell image list + .ico files).
    Paths are UnicodeString so SHGetFileInfoW keeps working regardless of
    the caller string mode. AMime is accepted for API symmetry with the
    unix helper and intentionally ignored. }

  TSystemIconHelper = class
  private
    class function BGRABitmapCreateFromHICON(AHIcon: Windows.HICON): TBGRABitmap;
    class function ExtractIconFromSysImageList(const APathFile: UnicodeString;
      AWantLargeIcon: Boolean): TBGRABitmap;
    class function LoadFromFileIcon(const APathFile: UnicodeString;
      AWantLargeIcon: Boolean): TBGRABitmap;
  public
    { Extracts the system icon for AFileName. ASize selects the small
      (<= 16) or large (> 16) shell icon. Never returns nil: on failure
      an empty bitmap is returned so callers can fall back. }
    class function GetSystemIcon(const AFileName: UnicodeString; ASize: Integer;
      {%H-}AMime: TMimeIconProvider): TBGRABitmap;
  end;

implementation

uses
  Windows, ShellApi, BGRAIconCursor, BGRABitmapTypes, LazFileUtils;

const
  ICON_SIZE_SMALL = 16;
  ICON_SIZE_LARGE = 32;
  ICO_BITDEPTH = 32;

{ TSystemIconHelper }

class function TSystemIconHelper.BGRABitmapCreateFromHICON(AHIcon: Windows.HICON): TBGRABitmap;
var
  IconInfo: TIconInfo;
begin
  IconInfo := Default(TIconInfo);

  Result := TBGRABitmap.Create;

  if Windows.GetIconInfo(AHIcon, IconInfo) = False then
  begin
    // Signal failure with nil so callers can fall back instead of caching blanks
    FreeAndNil(Result);
    Exit;
  end;

  try
    Result.Bitmap.LoadFromBitmapHandles(IconInfo.hbmColor, IconInfo.hbmMask);
  finally
    Windows.DeleteObject(IconInfo.hbmMask);
    Windows.DeleteObject(IconInfo.hbmColor);
  end;
end;

class function TSystemIconHelper.ExtractIconFromSysImageList(const APathFile: UnicodeString;
  AWantLargeIcon: Boolean): TBGRABitmap;
var
  FileInfo: TSHFileInfoW;
  Flags: Integer;
begin
  Result := nil;

  if AWantLargeIcon then
    Flags := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES
  else
    Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES;

  FillChar(FileInfo, SizeOf(TSHFileInfo), 0);
  if SHGetFileInfoW(PWideChar(APathFile), 0, FileInfo, SizeOf(TSHFileInfo), Flags) <> 0 then
  try
    Result := BGRABitmapCreateFromHICON(FileInfo.hIcon);
  finally
    DestroyIcon(FileInfo.hIcon);
  end;
end;

class function TSystemIconHelper.LoadFromFileIcon(const APathFile: UnicodeString;
  AWantLargeIcon: Boolean): TBGRABitmap;
var
  Icon: TBGRAIconCursor;
begin
  Result := nil;

  if not FileExists(APathFile) or (LowerCase(ExtractFileExt(APathFile)) <> '.ico') then
    Exit;

  Icon := TBGRAIconCursor.Create(ifIco);
  try
    try
      Icon.LoadFromFile(APathFile);
    except
      Exit;
    end;

    if AWantLargeIcon then
      Result := (Icon.GetBestFitBitmap(ICON_SIZE_LARGE, ICON_SIZE_LARGE) as TBGRABitmap)
    else
      Result := (Icon.GetBestFitBitmap(ICON_SIZE_SMALL, ICON_SIZE_SMALL) as TBGRABitmap);
  finally
    Icon.Free;
  end;
end;

class function TSystemIconHelper.GetSystemIcon(const AFileName: UnicodeString; ASize: Integer;
  {%H-}AMime: TMimeIconProvider): TBGRABitmap;
var
  AWantLargeIcon: Boolean;
begin
  Result := nil;
  AWantLargeIcon := ASize > ICON_SIZE_SMALL;

  if LowerCase(ExtractFileExt(AFileName)) = '.ico' then
    Result := LoadFromFileIcon(AFileName, AWantLargeIcon)
  else
    Result := ExtractIconFromSysImageList(AFileName, AWantLargeIcon);

  if Result = nil then
    Result := TBGRABitmap.Create;
end;

end.
