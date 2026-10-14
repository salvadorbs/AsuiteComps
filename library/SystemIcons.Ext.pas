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

unit SystemIcons.Ext;

{$I ASuiteComps.inc}

interface

{ Normalizes user-supplied extensions ('.TXT', 'txt', '*.txt') to a lowercase
  dotted form ('.txt'). Empty, '*' and '*.*' normalize to ''. }
function NormalizeExt(const AExt: string): string;

{ Normalizes to the dictionary key form used for icon lookups ('*.txt').
  Returns '' when there is no usable extension. }
function NormalizeExtKey(const AExt: string): string;

implementation

uses
  SysUtils;

function NormalizeExt(const AExt: string): string;
var
  s: string;
begin
  s := Trim(AExt);
  if s = '' then
    Exit('');
  // Accept '*.txt', '*.', '*', 'txt', '.txt' forms
  if (Length(s) >= 2) and (s[1] = '*') and (s[2] = '.') then
    Delete(s, 1, 1)
  else if (s = '*') or (s = '*.*') then
    Exit('');
  if (s <> '') and (s[1] <> '.') then
    s := '.' + s;
  if s = '.*' then
    Exit('');
  Result := LowerCase(s);
end;

function NormalizeExtKey(const AExt: string): string;
var
  sNorm: string;
begin
  sNorm := NormalizeExt(AExt);
  if sNorm = '' then
    Result := LowerCase(Trim(AExt))
  else
    Result := '*' + sNorm;
end;

end.
