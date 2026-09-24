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

const
  { Embedded default button images (see ShortcutGrabber.lrs), used when the
    host does not provide its own RightButton.Images. }
  HOTKEYEDIT_CHOOSE_RES   = 'asuite_hotkey_add';
  HOTKEYEDIT_CLEAR_RES    = 'asuite_hotkey_delete';
  HOTKEYEDIT_CHOOSE_INDEX = 0;
  HOTKEYEDIT_CLEAR_INDEX  = 1;

type
  { THotKeyEdit }

  { Read-only "buttoned edit" showing a shortcut. Clicking the edit (or the
    right button, when no shortcut is set) opens TfrmShortcutGrabber; the right
    button clears the current shortcut. It reuses the grabber and THotKey, so
    an application does not need to wire the dialog by hand.

    The right button has default icons (the same ones shipped by ASuite). A
    host that wants different icons assigns RightButton.Images plus
    ClearImageIndex/ChooseImageIndex, which then take precedence. }
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
  published
    { Shortcut value; 0 means "none". Text is kept in sync. }
    property Hotkey: TShortCut read GetHotkey write SetHotkey;

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

var
  { Shared image list with the default choose/clear icons. Created lazily and
    referenced (not owned) by every THotKeyEdit, so it is freed once. }
  DefaultHotKeyEditImages: TImageList = nil;

function GetDefaultHotKeyEditImages: TImageList;
begin
  if DefaultHotKeyEditImages = nil then
  begin
    DefaultHotKeyEditImages := TImageList.Create(nil);
    DefaultHotKeyEditImages.Width := 16;
    DefaultHotKeyEditImages.Height := 16;
    DefaultHotKeyEditImages.AddLazarusResource(HOTKEYEDIT_CHOOSE_RES);
    DefaultHotKeyEditImages.AddLazarusResource(HOTKEYEDIT_CLEAR_RES);
  end;
  Result := DefaultHotKeyEditImages;
end;

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
var
  ClearIdx, ChooseIdx: TImageIndex;
begin
  if (RightButton.Images <> nil) and (RightButton.Images <> DefaultHotKeyEditImages) then
  begin
    // Host-provided icons take precedence (ASuite sets them from its theme).
    ClearIdx := FClearImageIndex;
    ChooseIdx := FChooseImageIndex;
  end
  else
  begin
    // No host icons: fall back to the embedded defaults.
    RightButton.Images := GetDefaultHotKeyEditImages;
    RightButton.ImagesWidth := GetDefaultHotKeyEditImages.Width;
    ClearIdx := HOTKEYEDIT_CLEAR_INDEX;
    ChooseIdx := HOTKEYEDIT_CHOOSE_INDEX;
  end;

  if FHotkey <> 0 then
  begin
    RightButton.Visible := True;
    RightButton.ImageIndex := ClearIdx;
  end
  else
  begin
    RightButton.Visible := not FButtonVisibleOnlyWithHotkey;
    RightButton.ImageIndex := ChooseIdx;
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

finalization
  FreeAndNil(DefaultHotKeyEditImages);

end.
