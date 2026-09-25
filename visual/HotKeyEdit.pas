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
  LCLProc, ButtonedEdit, HotKey, ShortcutGrabber;

const
  { Embedded default button images (see ShortcutGrabber.lrs), used when the
    host does not provide its own RightButton.Images. }
  HOTKEYEDIT_CHOOSE_RES   = 'asuite_hotkey_add';
  HOTKEYEDIT_CLEAR_RES    = 'asuite_hotkey_delete';
  HOTKEYEDIT_CHOOSE_INDEX = 0;
  HOTKEYEDIT_CLEAR_INDEX  = 1;

type
  { THotKeyEdit }

  { "Buttoned edit" showing a shortcut. The control is always read-only.
    - By default clicking the edit (or the right button when no shortcut is set)
      opens TfrmShortcutGrabber; the right button clears the shortcut.
    - With ShowGrabberOnClick = False the shortcut is typed directly in the
      edit (like THotKey): Backspace/Delete clears, a bare key gets Ctrl unless
      NoModifier is set.
    - The right button has embedded default icons. A host can assign its own
      RightButton.Images plus ChooseImageIndex/ClearImageIndex (the width is
      derived from Images when ImagesWidth is 0); set UseDefaultImages = False
      to keep the embedded icons disabled. }
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
    FNoModifier: Boolean;
    FCapture: TShortcutCapture;
    FInlineCapture: Boolean;
    FUseDefaultImages: Boolean;
    FDefaultsWidth: Integer;

    function GetHotkey: TShortCut;
    function GetNoModifier: Boolean;
    function HotkeyAllowed(AValue: TShortCut): Boolean;
    procedure SetHotkey(AValue: TShortCut);
    function GetHotkeyText: TCaption;
    procedure SetHotkeyText(const AValue: TCaption);
    procedure SetClearImageIndex(AValue: TImageIndex);
    procedure SetChooseImageIndex(AValue: TImageIndex);
    procedure SetButtonVisibleOnlyWithHotkey(AValue: Boolean);
    procedure SetNoModifier(AValue: Boolean);
    procedure SetUseDefaultImages(AValue: Boolean);
    procedure UpdateText;
    procedure DoEditClick(Sender: TObject);
    procedure DoRightButtonClick(Sender: TObject);
    procedure UpdateButton;
  protected
    { Inline capture: when ShowGrabberOnClick is False the shortcut is typed
      directly in the edit (like THotKey). }
    procedure DoEditTextEnter(Sender: TObject); override;
    procedure DoEditTextExit(Sender: TObject); override;
    procedure DoEditTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Opens TfrmShortcutGrabber and applies the selected shortcut. }
    procedure OpenGrabber;
    { Clears the shortcut (Hotkey := 0). }
    procedure ClearHotkey;
  published
    { Shortcut value; 0 means "none". Text is kept in sync. }
    property Hotkey: TShortCut read GetHotkey write SetHotkey;
    { When True a bare key (no modifier) does not get an implicit Ctrl. }
    property NoModifier: Boolean read GetNoModifier write SetNoModifier default False;

    property Align;
    property Anchors;
    property AutoSize;
    property AutoSizeHeightIsEditHeight;
    property BiDiMode;
    property BorderColor;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property Enabled;
    property FocusColor;
    property Font;
    property HoverColor;
    property LeftButton;
    property ParentBiDiMode;
    property ParentFont;
    property PopupMenu;
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
    property UseDefaultImages: Boolean read FUseDefaultImages write SetUseDefaultImages default True;
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
  FNoModifier := False;
  FInlineCapture := False;
  FUseDefaultImages := True;
  FDefaultsWidth := 0;

  FCapture := TShortcutCapture.Create;
  FCapture.NoModifier := FNoModifier;

  //The shortcut is chosen through the grabber or typed inline, never freely.
  ReadOnly := True;

  //Hook the events of the inner edit / right button.
  OnClick := DoEditClick;
  OnRightButtonClick := DoRightButtonClick;

  UpdateButton;
end;

destructor THotKeyEdit.Destroy;
begin
  FCapture.Free;
  inherited Destroy;
end;

function THotKeyEdit.GetNoModifier: Boolean;
begin
  Result := FNoModifier;
end;

function THotKeyEdit.HotkeyAllowed(AValue: TShortCut): Boolean;
begin
  // Clearing (0) is always allowed; otherwise the host validator decides.
  Result := (AValue = 0) or (not Assigned(FOnValidateHotkey)) or
    FOnValidateHotkey(AValue);
end;

procedure THotKeyEdit.SetNoModifier(AValue: Boolean);
begin
  if FNoModifier = AValue then
    Exit;
  FNoModifier := AValue;
  if FCapture <> nil then
    FCapture.NoModifier := AValue;
end;

procedure THotKeyEdit.DoEditTextEnter(Sender: TObject);
begin
  //Type the shortcut inline only when the grabber is disabled and the edit is
  //read-only (otherwise the user types freely and we parse on editing done).
  FInlineCapture := (not FShowGrabberOnClick) and ReadOnly and
    (not (csDesigning in ComponentState));
  inherited DoEditTextEnter(Sender);
end;

procedure THotKeyEdit.DoEditTextExit(Sender: TObject);
begin
  FInlineCapture := False;
  inherited DoEditTextExit(Sender);
end;

procedure THotKeyEdit.DoEditTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FInlineCapture then
  begin
    FCapture.Capture(Key, Shift);
    if HotkeyAllowed(FCapture.Hotkey) then
      SetHotkey(FCapture.Hotkey)
    else
      //Rejected: keep the engine on the accepted value.
      FCapture.Hotkey := FHotkey;
  end;
  inherited DoEditTextKeyUp(Sender, Key, Shift);
end;

function THotKeyEdit.GetHotkey: TShortCut;
begin
  Result := FHotkey;
end;

procedure THotKeyEdit.SetHotkey(AValue: TShortCut);
begin
  //Keep the capture engine in sync: it is the starting point for inline typing,
  //so a stale value would corrupt the next capture.
  if FCapture <> nil then
    FCapture.Hotkey := AValue;

  if FHotkey = AValue then
    Exit;

  FHotkey := AValue;

  UpdateText;

  UpdateButton;

  if Assigned(FOnHotkeyChange) then
    FOnHotkeyChange(Self);
end;

procedure THotKeyEdit.UpdateText;
begin
  //ShortCutToText(0) is "Unknown", so handle the empty shortcut explicitly.
  if FHotkey = 0 then
    TCustomButtonedEdit(Self).Text := ''
  else
    TCustomButtonedEdit(Self).Text := ShortCutToText(FHotkey);
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

procedure THotKeyEdit.SetUseDefaultImages(AValue: Boolean);
begin
  if FUseDefaultImages = AValue then
    Exit;
  FUseDefaultImages := AValue;
  UpdateButton;
end;

procedure THotKeyEdit.UpdateButton;
var
  ClearIdx, ChooseIdx: TImageIndex;
begin
  if FUseDefaultImages and
     ((RightButton.Images = nil) or (RightButton.Images = DefaultHotKeyEditImages)) then
  begin
    // No host icons: use the embedded defaults.
    RightButton.Images := GetDefaultHotKeyEditImages;
    RightButton.ImagesWidth := GetDefaultHotKeyEditImages.Width;
    FDefaultsWidth := RightButton.ImagesWidth;
    ClearIdx := HOTKEYEDIT_CLEAR_INDEX;
    ChooseIdx := HOTKEYEDIT_CHOOSE_INDEX;
  end
  else
  if RightButton.Images <> nil then
  begin
    // Host-provided icons take precedence; derive the width from the list when
    // the current value is still the one used for the embedded defaults.
    if (RightButton.ImagesWidth = 0) or (RightButton.ImagesWidth = FDefaultsWidth) then
      RightButton.ImagesWidth := RightButton.Images.Width;
    ClearIdx := FClearImageIndex;
    ChooseIdx := FChooseImageIndex;
  end
  else
  begin
    // No images at all: the button has no glyph.
    ClearIdx := -1;
    ChooseIdx := -1;
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
