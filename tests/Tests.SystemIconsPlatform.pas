unit Tests.SystemIconsPlatform;

{$mode objfpc}{$H+}

{ Tests for SystemIcons.Platform mapping. Only the display-independent
  MapFileToIconName is covered here: theme queries and bitmap extraction
  need a display server and stay in the application smoke tests. }

interface

uses
  fpcunit, testregistry, Classes, SysUtils,
  SystemIcons.Mime
  {$IFDEF UNIX}
  , SystemIcons.Platform
  {$ENDIF}
  ;

{$IFDEF UNIX}
type
  TTestSystemIconsPlatform = class(TTestCase)
  private
    FProvider: TMimeIconProvider;
    FTempDir: string;
    function TempFile(const AName, AContent: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMapExtension;
    procedure TestMapMissingExt;
    procedure TestMapTextFallback;
    procedure TestMapDesktopFile;
    procedure TestMapDirectory;
    procedure TestThemeEmptyIsFalse;
    procedure TestLoadMissingFilesIsEmpty;
    procedure TestLoadSystemTable;
  end;
{$ENDIF}

implementation

{$IFDEF UNIX}

procedure TTestSystemIconsPlatform.SetUp;
var
  globs: TStringList;
begin
  FProvider := TMimeIconProvider.Create;
  globs := TStringList.Create;
  try
    globs.Add('text/plain:*.txt');
    FProvider.LoadFromStrings(globs, nil);
  finally
    globs.Free;
  end;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'sysiconstest';
  ForceDirectories(FTempDir);
end;

procedure TTestSystemIconsPlatform.TearDown;
var
  sr: TSearchRec;
begin
  if FindFirst(FTempDir + PathDelim + '*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') then
        DeleteFile(FTempDir + PathDelim + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  RemoveDir(FTempDir);
  FProvider.Free;
end;

function TTestSystemIconsPlatform.TempFile(const AName, AContent: string): string;
var
  f: TextFile;
begin
  Result := FTempDir + PathDelim + AName;
  AssignFile(f, Result);
  Rewrite(f);
  Write(f, AContent);
  CloseFile(f);
end;

procedure TTestSystemIconsPlatform.TestMapExtension;
begin
  // Deterministic: first MIME candidate wins, no theme query involved
  AssertEquals('text-plain',
    TSystemIconHelper.MapFileToIconName(FTempDir + PathDelim + 'doc.txt', FProvider));
end;

procedure TTestSystemIconsPlatform.TestMapMissingExt;
begin
  // No provider hit and missing file is not text -> generic unknown
  AssertEquals('unknown',
    TSystemIconHelper.MapFileToIconName(FTempDir + PathDelim + 'doc.nodata', FProvider));
end;

procedure TTestSystemIconsPlatform.TestMapTextFallback;
var
  path: string;
begin
  path := TempFile('readme.nodata', 'plain text content');
  AssertEquals('text-x-generic',
    TSystemIconHelper.MapFileToIconName(path, FProvider));
end;

procedure TTestSystemIconsPlatform.TestMapDesktopFile;
var
  path: string;
begin
  path := TempFile('app.desktop',
    '[Desktop Entry]' + LineEnding + 'Icon=my-app-icon' + LineEnding);
  AssertEquals('my-app-icon',
    TSystemIconHelper.MapFileToIconName(path, FProvider));
end;

procedure TTestSystemIconsPlatform.TestMapDirectory;
begin
  AssertEquals('folder',
    TSystemIconHelper.MapFileToIconName(FTempDir, FProvider));
end;

procedure TTestSystemIconsPlatform.TestThemeEmptyIsFalse;
begin
  AssertFalse(TSystemIconHelper.ThemeIconExists(''));
end;

procedure TTestSystemIconsPlatform.TestLoadMissingFilesIsEmpty;
var
  P: TMimeIconProvider;
begin
  P := TMimeIconProvider.Create;
  try
    P.LoadFromFiles('/nonexistent-globs', '');
    AssertEquals(0, P.Count);
  finally
    P.Free;
  end;
end;

procedure TTestSystemIconsPlatform.TestLoadSystemTable;
var
  P: TMimeIconProvider;
begin
  // Only meaningful where shared-mime-info is installed; skip elsewhere
  if not FileExists('/usr/share/mime/globs') then
    Exit;
  P := TMimeIconProvider.Create;
  try
    P.LoadSystemTable;
    AssertTrue(P.Count > 0);
  finally
    P.Free;
  end;
end;

initialization
  RegisterTest(TTestSystemIconsPlatform);
{$ENDIF}

end.
