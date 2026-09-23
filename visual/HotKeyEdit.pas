{
Copyright (C) 2006-2026 Matteo Salvi

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

unit HotKeyEdit;

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ImgList, Menus, StdCtrls, LCLType,
  LCLProc, ButtonedEdit, ShortcutGrabber;

type
  { THotKeyEdit }

  { Read-only "buttoned edit" showing a shortcut. Clicking the edit (or the
    right button, when no shortcut is set) opens TfrmShortcutGrabber; the right
    button clears the current shortcut. It reuses the grabber and THotKey, so
    an application does not need to wire the dialog by hand. }
  THotKeyEdit = class(TCustomButtonedEdit)
  private
    FHotkey: TShortCut;
    FOnHotkeyChange: TNotifyEvent;
    FOnEditClick: TNotifyEvent;
    FOnValidateHotkey: TShortcutValidateEvent;
    FClearImageIndex: TImageIndex;
    FChooseImageIndex: TImageIndex;
    FButtonVisibleOnlyWithHotkey: Boolean;
    FShowGrabberOnClick: Boolean;

    function GetHotkey: TShortCut;
    procedure SetHotkey(AValue: TShortCut);
    function GetHotkeyText: TCaption;
    procedure SetHotkeyText(const AValue: TCaption);
    procedure SetClearImageIndex(AValue: TImageIndex);
    procedure SetChooseImageIndex(AValue: TImageIndex);
    procedure SetButtonVisibleOnlyWithHotkey(AValue: Boolean);
    procedure DoEditClick(Sender: TObject);
    procedure DoRightButtonClick(Sender: TObject);
    procedure UpdateButton;
  public
    constructor Create(AOwner: TComponent); override;

    { Opens TfrmShortcutGrabber and applies the selected shortcut. }
    procedure OpenGrabber;
    { Clears the shortcut (Hotkey := 0). }
    procedure ClearHotkey;

    { Shortcut value; 0 means "none". Text is kept in sync. }
    property Hotkey: TShortCut read GetHotkey write SetHotkey;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsSingle;
    property CharCase;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property LeftButton;
    property ParentBiDiMode;
    property ParentFont;
    property PopupMenu;
    property ReadOnly;
    property RightButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text: TCaption read GetHotkeyText write SetHotkeyText;
    property TextHint;
    property Visible;

    //Events
    property OnHotkeyChange: TNotifyEvent read FOnHotkeyChange write FOnHotkeyChange;
    property OnEditClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnValidateHotkey: TShortcutValidateEvent read FOnValidateHotkey write FOnValidateHotkey;

    //Right button appearance
    property ClearImageIndex: TImageIndex read FClearImageIndex write SetClearImageIndex default -1;
    property ChooseImageIndex: TImageIndex read FChooseImageIndex write SetChooseImageIndex default -1;
    property ButtonVisibleOnlyWithHotkey: Boolean read FButtonVisibleOnlyWithHotkey
      write SetButtonVisibleOnlyWithHotkey default False;
    property ShowGrabberOnClick: Boolean read FShowGrabberOnClick
      write FShowGrabberOnClick default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ASuite Components', [THotKeyEdit]);
end;

{ THotKeyEdit }

constructor THotKeyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FClearImageIndex := -1;
  FChooseImageIndex := -1;
  FShowGrabberOnClick := True;

  //The shortcut is chosen through the grabber, never typed.
  ReadOnly := True;

  //Hook the events of the inner edit / right button.
  OnClick := DoEditClick;
  OnRightButtonClick := DoRightButtonClick;

  UpdateButton;
end;

function THotKeyEdit.GetHotkey: TShortCut;
begin
  Result := FHotkey;
end;

procedure THotKeyEdit.SetHotkey(AValue: TShortCut);
begin
  if FHotkey = AValue then
    Exit;

  FHotkey := AValue;

  if FHotkey = 0 then
    TCustomButtonedEdit(Self).Text := ''
  else
    TCustomButtonedEdit(Self).Text := ShortCutToText(FHotkey);

  UpdateButton;

  if Assigned(FOnHotkeyChange) then
    FOnHotkeyChange(Self);
end;

function THotKeyEdit.GetHotkeyText: TCaption;
begin
  Result := TCustomButtonedEdit(Self).Text;
end;

procedure THotKeyEdit.SetHotkeyText(const AValue: TCaption);
begin
  SetHotkey(TextToShortCut(AValue));
end;

procedure THotKeyEdit.SetClearImageIndex(AValue: TImageIndex);
begin
  if FClearImageIndex = AValue then
    Exit;
  FClearImageIndex := AValue;
  UpdateButton;
end;

procedure THotKeyEdit.SetChooseImageIndex(AValue: TImageIndex);
begin
  if FChooseImageIndex = AValue then
    Exit;
  FChooseImageIndex := AValue;
  UpdateButton;
end;

procedure THotKeyEdit.SetButtonVisibleOnlyWithHotkey(AValue: Boolean);
begin
  if FButtonVisibleOnlyWithHotkey = AValue then
    Exit;
  FButtonVisibleOnlyWithHotkey := AValue;
  UpdateButton;
end;

procedure THotKeyEdit.UpdateButton;
begin
  if FHotkey <> 0 then
  begin
    RightButton.Visible := True;
    RightButton.ImageIndex := FClearImageIndex;
  end
  else
  begin
    RightButton.Visible := not FButtonVisibleOnlyWithHotkey;
    RightButton.ImageIndex := FChooseImageIndex;
  end;

  RightButton.UpdateSize;
end;

procedure THotKeyEdit.DoEditClick(Sender: TObject);
begin
  if Assigned(FOnEditClick) then
    FOnEditClick(Self);

  if FShowGrabberOnClick and not (csDesigning in ComponentState) then
    OpenGrabber;
end;

procedure THotKeyEdit.DoRightButtonClick(Sender: TObject);
begin
  if FHotkey <> 0 then
    ClearHotkey
  else if FShowGrabberOnClick and not (csDesigning in ComponentState) then
    OpenGrabber;
end;

procedure THotKeyEdit.OpenGrabber;
var
  NewHotkey: TShortCut;
begin
  NewHotkey := TfrmShortcutGrabber.Execute(Self, FHotkey, FOnValidateHotkey);
  if NewHotkey <> 0 then
    SetHotkey(NewHotkey);
end;

procedure THotKeyEdit.ClearHotkey;
begin
  SetHotkey(0);
end;

end.
