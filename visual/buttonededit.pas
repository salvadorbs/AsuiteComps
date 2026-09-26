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

unit ButtonedEdit;

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, StdCtrls, Buttons, Controls, ImgList, Graphics, Menus,
  LCLType, Types;

type
  TButtonPosition = (bpLeft, bpRight);

  TCustomButtonedEdit = class;

  { TCustomGlyphButton }

  TCustomGlyphButton = class(TCustomSpeedButton)
  private
    FDropDownMenu: TPopupMenu;

    procedure SetDropDownMenu(AValue: TPopupMenu);
  public
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;

    procedure Click; override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintBackground(var PaintRect: TRect); override;
  end;

  { TCustomGlyphButtonOptions }

  TCustomGlyphButtonOptions = class(TPersistent)
  private
    FButton: TCustomGlyphButton;
    FParentControl: TCustomButtonedEdit;
    { True when the host explicitly set ImagesWidth; deriving the width from a
      newly assigned image list must not override such a value. }
    FImageWidthExplicit: Boolean;

    function GetDisabledImageIndex: TImageIndex;
    function GetDropDownMenu: TPopupMenu;
    function GetHotImageIndex: TImageIndex;
    function GetImageIndex: TImageIndex;
    function GetImages: TCustomImageList;
    function GetImagesWidth: Integer;
    function GetOnClick: TNotifyEvent;
    function GetPressedImageIndex: TImageIndex;
    function GetVisible: Boolean;
    procedure SetDisabledImageIndex(AValue: TImageIndex);
    procedure SetDropDownMenu(AValue: TPopupMenu);
    procedure SetHotImageIndex(AValue: TImageIndex);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImagesWidth(AValue: Integer);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetPressedImageIndex(AValue: TImageIndex);
    procedure SetVisible(AValue: Boolean);
  public                                       
    property DisabledImageIndex: TImageIndex read GetDisabledImageIndex write SetDisabledImageIndex default -1;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property HotImageIndex: TImageIndex read GetHotImageIndex write SetHotImageIndex default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImagesWidth: Integer read GetImagesWidth write SetImagesWidth default 0;
    { Read-only: True once the host assigned ImagesWidth. Assigning a new image
      list clears it, so the width can be derived from the list again. }
    property ImageWidthExplicit: Boolean read FImageWidthExplicit;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex default -1;  
    property PressedImageIndex: TImageIndex read GetPressedImageIndex write SetPressedImageIndex default -1;
    property Visible: Boolean read GetVisible write SetVisible default False;

    //Events
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;

    constructor Create(AOwner: TCustomButtonedEdit; APosition: TButtonPosition);

    procedure UpdateSize;
    procedure SetOuterSpacing(ASize: Integer);
    { Keeps the glyph at its native size and centers the button vertically in
      an inner area of AParentInnerHeight pixels. The button height is pinned
      to the glyph height so a taller composite never stretches the icon. }
    procedure CenterVertically(AParentInnerHeight: Integer);
    procedure Invalidate;
    procedure Clear;
    procedure Assign(ASource: TPersistent); override;
  end;

  { TGlyphButtonOptions }

  TGlyphButtonOptions = class(TCustomGlyphButtonOptions)
  published
    property DisabledImageIndex;
    property DropDownMenu;
    property HotImageIndex;
    property Images;
    property ImagesWidth;
    property ImageIndex;
    property PressedImageIndex;
    property Visible;
  end;

  { TCustomButtonedEdit }

  TCustomButtonedEdit = class(TCustomControl)
  private
    FLeftButton: TGlyphButtonOptions;
    FEditText: TEdit;
    FBorderColor: TColor;
    FFocusColor: TColor;
    FHoverColor: TColor;
    FAutoSizeHeightIsEditHeight: Boolean;
    FFocusOnButtonClick: Boolean;
    FOnEditContextPopup: TContextPopupEvent;
    FOnEditDblClick: TNotifyEvent;
    FOnEditEditingDone: TNotifyEvent;
    FOnEditKeyDown: TKeyEvent;
    FOnEditKeyUp: TKeyEvent;
    FOnEditMouseDown: TMouseEvent;
    FOnEditMouseMove: TMouseMoveEvent;
    FOnEditMouseUp: TMouseEvent;
    FOnEditTextChange: TNotifyEvent;
    FOnEditTextClick: TNotifyEvent;
    FOnEditTextKeyPress: TKeyPressEvent;
    FOnEditTextMouseEnter: TNotifyEvent;
    FOnEditTextMouseLeave: TNotifyEvent;
    FOnEditUtf8KeyPress: TUTF8KeyPressEvent;
    FOnLeftButtonClick: TNotifyEvent;
    FOnRightButtonClick: TNotifyEvent;
    FMouseInControl: Boolean;
    FRightButton: TGlyphButtonOptions;
    FNativeEditHeight: Integer;
    FUpdatingLayout: Boolean;

    procedure DoChildMouseEnter(Sender: TObject);
    procedure DoChildMouseLeave(Sender: TObject);
    procedure DoEditTextChange(Sender: TObject);
    procedure DoEditTextClick(Sender: TObject);
    procedure DoEditTextContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DoEditTextDblClick(Sender: TObject);
    procedure DoEditTextKeyPress(Sender: TObject; var Key: Char);
    procedure DoEditTextMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEditTextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoEditTextMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEditTextUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure DoLeftButtonClick(Sender: TObject);
    procedure DoRightButtonClick(Sender: TObject);
    procedure DrawHighlightFrame(AColor: TColor);
    function CurrentRingSize: Integer;
    function CurrentGapSize: Integer;
    function NativeEditHeight: Integer;
    procedure UpdateSpacing;
    procedure UpdateVerticalLayout;
    procedure UpdateEditBounds;
    function InnerHeight: Integer;
    function MouseIsOverComposite: Boolean;
    procedure SetHovered(AValue: Boolean);
    procedure UpdateHoverState;
    function GetAlignment: TAlignment;
    function GetCharCase: TEditCharCase;
    function GetMaxLength: Integer;
    function GetPasswordChar: Char;
    function GetReadOnly: Boolean;
    function GetTabStop: Boolean;
    function GetText: TCaption;
    function GetTextHint: TTranslateString;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAutoSizeHeightIsEditHeight(AValue: Boolean);
    procedure SetBorderColor(AValue: TColor);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetFocusColor(AValue: TColor);
    procedure SetHoverColor(AValue: TColor);
    procedure SetLeftButton(AValue: TGlyphButtonOptions);
    procedure SetMaxLength(AValue: Integer);
    procedure SetPasswordChar(AValue: Char);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetRightButton(AValue: TGlyphButtonOptions);
    procedure SetTabStop(AValue: Boolean);
    procedure SetText(AValue: TCaption);
    procedure SetTextHint(AValue: TTranslateString);

    procedure UpdateSize;
  protected
    procedure AdjustClientRect(var ARect: TRect); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
      WithThemeSpace: Boolean); override;
    procedure ChangeScale(Multiplier, Divider: Integer); override;
    procedure DoEditTextEnter(Sender: TObject); virtual;
    procedure DoEditTextExit(Sender: TObject); virtual;
    procedure DoEditTextEditingDone(Sender: TObject); virtual;
    procedure DoEditTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure FontChanged(Sender: TObject); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Loaded; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetAutoSize(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetFocus; override;    
    function Focused: Boolean; override;

    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property AutoSizeHeightIsEditHeight: Boolean read FAutoSizeHeightIsEditHeight write SetAutoSizeHeightIsEditHeight default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clWindowFrame;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clHighlight;
    property FocusOnButtonClick: Boolean read FFocusOnButtonClick write FFocusOnButtonClick default False;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clHighlight;
    property LeftButton: TGlyphButtonOptions read FLeftButton write SetLeftButton;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property RightButton: TGlyphButtonOptions read FRightButton write SetRightButton;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
    property Text: TCaption read GetText write SetText;
    property TextHint: TTranslateString read GetTextHint write SetTextHint;

    //Events
    property OnChange: TNotifyEvent read FOnEditTextChange write FOnEditTextChange;
    property OnClick: TNotifyEvent read FOnEditTextClick write FOnEditTextClick;
    property OnContextPopup: TContextPopupEvent read FOnEditContextPopup write FOnEditContextPopup;
    property OnDblClick: TNotifyEvent read FOnEditDblClick write FOnEditDblClick;
    property OnEditingDone: TNotifyEvent read FOnEditEditingDone write FOnEditEditingDone;
    property OnKeyDown: TKeyEvent read FOnEditKeyDown write FOnEditKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnEditTextKeyPress write FOnEditTextKeyPress;
    property OnKeyUp: TKeyEvent read FOnEditKeyUp write FOnEditKeyUp;
    property OnLeftButtonClick: TNotifyEvent read FOnLeftButtonClick write FOnLeftButtonClick;
    property OnMouseDown: TMouseEvent read FOnEditMouseDown write FOnEditMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnEditTextMouseEnter write FOnEditTextMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnEditTextMouseLeave write FOnEditTextMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnEditMouseMove write FOnEditMouseMove;
    property OnMouseUp: TMouseEvent read FOnEditMouseUp write FOnEditMouseUp;
    property OnRightButtonClick: TNotifyEvent read FOnRightButtonClick write FOnRightButtonClick;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read FOnEditUtf8KeyPress write FOnEditUtf8KeyPress;
  published
  end;

  { TButtonedEdit }

  TButtonedEdit = class(TCustomButtonedEdit)      
  public
    { Public declarations }
  published
    { Published declarations }
    property Align;
    property Alignment;
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
    property FocusOnButtonClick;
    property Font;
    property HoverColor;
    property LeftButton;
    property MaxLength;
    property ParentBiDiMode;
    property ParentFont;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property RightButton;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;

    //Events
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLeftButtonClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnRightButtonClick;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ASuite Components',[TButtonedEdit]);
end;

{ TCustomGlyphButton }

procedure TCustomGlyphButton.Click;
var
  P1, P2: TPoint;
begin
  inherited Click;

  if Assigned(FDropDownMenu) then
  begin
    P1.X:= 0;
    P1.Y:= Self.Height;
    P2:= Self.ClientToScreen(P1);
    FDropDownMenu.PopUp(P2.X, P2.Y);
  end;
end;

procedure TCustomGlyphButton.PaintBackground(var PaintRect: TRect);
begin
  //Draw only the glyph: no themed button background/border on hover/press.
  //The composite draws the simulated frame itself.
end;

procedure TCustomGlyphButton.SetDropDownMenu(AValue: TPopupMenu);
begin
  if FDropDownMenu = AValue then
    Exit;
  if FDropDownMenu <> nil then
    FDropDownMenu.RemoveFreeNotification(Self);
  FDropDownMenu := AValue;
  if FDropDownMenu <> nil then
    FDropDownMenu.FreeNotification(Self);
end;

procedure TCustomGlyphButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
end;

{ TCustomGlyphButtonOptions }

procedure TCustomGlyphButtonOptions.SetVisible(AValue: Boolean);
begin
  if AValue <> (FButton.Visible) then
  begin
    FButton.Visible := AValue;
    UpdateSize;
  end;
end;

procedure TCustomGlyphButtonOptions.SetImages(AValue: TCustomImageList);
begin
  if FButton.Images <> AValue then
  begin
    FButton.Images := AValue;
    // A new list: the width may be derived from it again.
    FImageWidthExplicit := False;
    UpdateSize;
    Invalidate;
  end;
end;

procedure TCustomGlyphButtonOptions.SetImagesWidth(AValue: Integer);
begin
  // Remember the host set the width, even when it equals the current value
  // (e.g. it matches the embedded default). Deriving from Images must not
  // silently override it.
  FImageWidthExplicit := True;
  if FButton.ImageWidth <> AValue then
  begin
    FButton.ImageWidth := AValue;
    UpdateSize;
    Invalidate;
  end;
end;

procedure TCustomGlyphButtonOptions.SetDisabledImageIndex(AValue: TImageIndex);
begin
  if FButton.DisabledImageIndex <> AValue then
  begin
    FButton.DisabledImageIndex := AValue;
    Invalidate;
  end;
end;

procedure TCustomGlyphButtonOptions.SetHotImageIndex(AValue: TImageIndex);
begin
  if FButton.HotImageIndex <> AValue then
  begin
    FButton.HotImageIndex := AValue;
    Invalidate;
  end;
end;

procedure TCustomGlyphButtonOptions.SetPressedImageIndex(AValue: TImageIndex);
begin
  if FButton.PressedImageIndex <> AValue then
  begin
    FButton.PressedImageIndex := AValue;
    Invalidate;
  end;
end;

procedure TCustomGlyphButtonOptions.SetOnClick(AValue: TNotifyEvent);
begin
  FButton.OnClick := AValue;
end;

function TCustomGlyphButtonOptions.GetVisible: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TCustomGlyphButtonOptions.SetDropDownMenu(AValue: TPopupMenu);
begin
  if FButton.DropDownMenu <> AValue then
    FButton.DropDownMenu := AValue;
end;

function TCustomGlyphButtonOptions.GetDisabledImageIndex: TImageIndex;
begin
  Result := FButton.DisabledImageIndex;
end;

function TCustomGlyphButtonOptions.GetHotImageIndex: TImageIndex;
begin
  Result := FButton.HotImageIndex;
end;

function TCustomGlyphButtonOptions.GetPressedImageIndex: TImageIndex;
begin
  Result := FButton.PressedImageIndex;
end;

function TCustomGlyphButtonOptions.GetImageIndex: TImageIndex;
begin
  Result := FButton.ImageIndex;
end;

function TCustomGlyphButtonOptions.GetDropDownMenu: TPopupMenu;
begin
  Result := FButton.DropDownMenu;
end;

function TCustomGlyphButtonOptions.GetImages: TCustomImageList;
begin
  Result := FButton.Images;
end;

function TCustomGlyphButtonOptions.GetImagesWidth: Integer;
begin
  Result := FButton.ImageWidth;
end;

function TCustomGlyphButtonOptions.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

procedure TCustomGlyphButtonOptions.SetImageIndex(AValue: TImageIndex);
begin
  if FButton.ImageIndex <> AValue then
  begin
    FButton.ImageIndex := AValue;
    UpdateSize;
    Invalidate;
  end;
end;

constructor TCustomGlyphButtonOptions.Create(AOwner: TCustomButtonedEdit;
  APosition: TButtonPosition);
begin                       
  FButton := TCustomGlyphButton.Create(AOwner);
  FParentControl := AOwner;
  FImageWidthExplicit := False;

  case APosition of
    bpLeft:
    begin
      FButton.Align := alLeft;
      FButton.Name := 'LeftButton';
    end;
    bpRight:
    begin
      FButton.Align := alRight;
      FButton.Name:= 'RightButton';
    end;
  end;
  FButton.AutoSize := False;
  FButton.Flat := True;
  FButton.Parent := TWinControl(FParentControl);
  FButton.Visible := False;

  //Let the composite track hovering across the edit and both buttons.
  FButton.OnMouseEnter := FParentControl.DoChildMouseEnter;
  FButton.OnMouseLeave := FParentControl.DoChildMouseLeave;

  UpdateSize;
end;

procedure TCustomGlyphButtonOptions.UpdateSize;
begin
  if (ImageIndex >= 0) and (Images <> nil) then
    //Fixed width; the height is pinned by CenterVertically to the glyph size.
    FButton.Width := FButton.ImageWidth
  else
  begin
    FButton.Constraints.MinHeight := 0;
    FButton.Constraints.MaxHeight := 0;
    FButton.Width := 0;
    FButton.Height := 0;
  end;

  //Re-center the button now that its glyph size may have changed.
  if FParentControl <> nil then
    FParentControl.UpdateVerticalLayout;
end;

procedure TCustomGlyphButtonOptions.SetOuterSpacing(ASize: Integer);
begin
  if ASize < 0 then
    ASize := 0;
  //Space between the simulated border and the button (outer side only).
  //Top/Bottom are left to CenterVertically.
  if FButton.Align = alLeft then
    FButton.BorderSpacing.Left := ASize
  else if FButton.Align = alRight then
    FButton.BorderSpacing.Right := ASize;
end;

procedure TCustomGlyphButtonOptions.CenterVertically(AParentInnerHeight: Integer);
var
  GlyphHeight, Extra: Integer;
begin
  if (ImageIndex >= 0) and (Images <> nil) then
    GlyphHeight := Images.Height
  else
    GlyphHeight := 0;

  //The button never grows beyond the glyph: the icon keeps its size even if
  //the composite is taller. MinHeight stays 0 so a short composite clips
  //instead of overflowing.
  FButton.Constraints.MinHeight := 0;
  FButton.Constraints.MaxHeight := GlyphHeight;

  Extra := AParentInnerHeight - GlyphHeight;
  if Extra < 0 then
    Extra := 0;
  FButton.BorderSpacing.Top := Extra div 2;
  FButton.BorderSpacing.Bottom := Extra - (Extra div 2);
end;

procedure TCustomGlyphButtonOptions.Invalidate;
begin
  FButton.Invalidate;
end;

procedure TCustomGlyphButtonOptions.Clear;
begin
  Images := nil;
  ImagesWidth := 0;
  ImageIndex := -1;
  HotImageIndex := -1;
  PressedImageIndex := -1;
  DisabledImageIndex := -1;
  Visible := False;
  DropDownMenu := nil;
end;

procedure TCustomGlyphButtonOptions.Assign(ASource: TPersistent);
begin
  if ASource is TCustomGlyphButtonOptions then
  begin
    Images := TCustomGlyphButtonOptions(ASource).Images;
    ImagesWidth := TCustomGlyphButtonOptions(ASource).ImagesWidth;
    ImageIndex := TCustomGlyphButtonOptions(ASource).ImageIndex;
    HotImageIndex := TCustomGlyphButtonOptions(ASource).HotImageIndex;
    PressedImageIndex := TCustomGlyphButtonOptions(ASource).PressedImageIndex;
    DisabledImageIndex := TCustomGlyphButtonOptions(ASource).DisabledImageIndex;
    Visible := TCustomGlyphButtonOptions(ASource).Visible;
    DropDownMenu := TCustomGlyphButtonOptions(ASource).DropDownMenu;
  end
  else
    inherited Assign(ASource);
end;

{ TCustomButtonedEdit }

function TCustomButtonedEdit.GetReadOnly: Boolean;
begin
  Result := FEditText.ReadOnly;
end;

function TCustomButtonedEdit.GetTabStop: Boolean;
begin
  Result := FEditText.TabStop;
end;

function TCustomButtonedEdit.GetText: TCaption;
begin
  Result := FEditText.Text;
end;

function TCustomButtonedEdit.GetTextHint: TTranslateString;
begin
  Result := FEditText.TextHint;
end;

procedure TCustomButtonedEdit.SetAlignment(AValue: TAlignment);
begin
  if FEditText.Alignment <> AValue then
    FEditText.Alignment := AValue;
end;

procedure TCustomButtonedEdit.SetAutoSizeHeightIsEditHeight(AValue: Boolean);
begin
  if FAutoSizeHeightIsEditHeight <> AValue then
  begin
    FAutoSizeHeightIsEditHeight := AValue;
    if AutoSize then
      AdjustSize;
  end;
end;

procedure TCustomButtonedEdit.SetBorderColor(AValue: TColor);
begin
  if FBorderColor <> AValue then
  begin
    FBorderColor := AValue;
    Invalidate;
  end;
end;

procedure TCustomButtonedEdit.SetFocusColor(AValue: TColor);
begin
  if FFocusColor <> AValue then
  begin
    FFocusColor := AValue;
    Invalidate;
  end;
end;

procedure TCustomButtonedEdit.SetHoverColor(AValue: TColor);
begin
  if FHoverColor <> AValue then
  begin
    FHoverColor := AValue;
    Invalidate;
  end;
end;

procedure TCustomButtonedEdit.SetCharCase(AValue: TEditCharCase);
begin
  if FEditText.CharCase <> AValue then
    FEditText.CharCase := AValue;
end;

procedure TCustomButtonedEdit.SetMaxLength(AValue: Integer);
begin
  if FEditText.MaxLength <> AValue then
    FEditText.MaxLength := AValue;
end;

procedure TCustomButtonedEdit.SetPasswordChar(AValue: Char);
begin
  if FEditText.PasswordChar <> AValue then
    FEditText.PasswordChar := AValue;
end;

procedure TCustomButtonedEdit.DoEditTextChange(Sender: TObject);
begin
  if Assigned(FOnEditTextChange) then
    FOnEditTextChange(Self);
end;

procedure TCustomButtonedEdit.DoEditTextClick(Sender: TObject);
begin
  if Assigned(FOnEditTextClick) then
    FOnEditTextClick(Self);
end;

procedure TCustomButtonedEdit.DoEditTextKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnEditTextKeyPress) then
    FOnEditTextKeyPress(Self, Key);
end;

procedure TCustomButtonedEdit.DoEditTextEnter(Sender: TObject);
begin
  Invalidate;
  if Assigned(OnEnter) then
    OnEnter(Self);
end;

procedure TCustomButtonedEdit.DoEditTextExit(Sender: TObject);
begin
  Invalidate;
  if Assigned(OnExit) then
    OnExit(Self);
end;

procedure TCustomButtonedEdit.DoEditTextContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOnEditContextPopup) then
    FOnEditContextPopup(Self, MousePos, Handled);
end;

procedure TCustomButtonedEdit.DoEditTextDblClick(Sender: TObject);
begin
  if Assigned(FOnEditDblClick) then
    FOnEditDblClick(Self);
end;

procedure TCustomButtonedEdit.DoEditTextEditingDone(Sender: TObject);
begin
  if Assigned(FOnEditEditingDone) then
    FOnEditEditingDone(Self);
end;

procedure TCustomButtonedEdit.DoEditTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnEditKeyDown) then
    FOnEditKeyDown(Self, Key, Shift);
end;

procedure TCustomButtonedEdit.DoEditTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnEditKeyUp) then
    FOnEditKeyUp(Self, Key, Shift);
end;

procedure TCustomButtonedEdit.DoEditTextMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseDown) then
    FOnEditMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomButtonedEdit.DoEditTextMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseMove) then
    FOnEditMouseMove(Self, Shift, X, Y);
end;

procedure TCustomButtonedEdit.DoEditTextMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseUp) then
    FOnEditMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCustomButtonedEdit.DoEditTextUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if Assigned(FOnEditUtf8KeyPress) then
    FOnEditUtf8KeyPress(Self, UTF8Key);
end;

procedure TCustomButtonedEdit.DoLeftButtonClick(Sender: TObject);
begin
  if FFocusOnButtonClick then
    SetFocus;
  if Assigned(FOnLeftButtonClick) then
    FOnLeftButtonClick(Self);
end;

procedure TCustomButtonedEdit.DoRightButtonClick(Sender: TObject);
begin
  if FFocusOnButtonClick then
    SetFocus;
  if Assigned(FOnRightButtonClick) then
    FOnRightButtonClick(Self);
end;

procedure TCustomButtonedEdit.DoChildMouseEnter(Sender: TObject);
begin
  //Entering any child (edit or glyph button) means the mouse is on the composite.
  SetHovered(True);
end;

procedure TCustomButtonedEdit.DoChildMouseLeave(Sender: TObject);
begin
  //The pointer may just have moved to a sibling (edit <-> buttons) or to the
  //border, so only report the leave once it is really outside the composite.
  UpdateHoverState;
end;

function TCustomButtonedEdit.MouseIsOverComposite: Boolean;
var
  R: TRect;
begin
  //Without a handle there is no real hover; treat as outside. This also keeps
  //the logic usable in headless tests.
  if not HandleAllocated then
    Exit(False);

  R := Rect(0, 0, Width, Height);
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  Result := PtInRect(R, Mouse.CursorPos);
end;

procedure TCustomButtonedEdit.SetHovered(AValue: Boolean);
begin
  if AValue = FMouseInControl then
    Exit;

  FMouseInControl := AValue;
  Invalidate;
  if AValue then
  begin
    if Assigned(FOnEditTextMouseEnter) then
      FOnEditTextMouseEnter(Self);
  end
  else
  if Assigned(FOnEditTextMouseLeave) then
    FOnEditTextMouseLeave(Self);
end;

procedure TCustomButtonedEdit.UpdateHoverState;
begin
  SetHovered(MouseIsOverComposite);
end;

function TCustomButtonedEdit.GetAlignment: TAlignment;
begin
  Result := FEditText.Alignment;
end;

function TCustomButtonedEdit.GetCharCase: TEditCharCase;
begin
  Result := FEditText.CharCase;
end;

function TCustomButtonedEdit.GetMaxLength: Integer;
begin
  Result := FEditText.MaxLength;
end;

function TCustomButtonedEdit.GetPasswordChar: Char;
begin
  Result := FEditText.PasswordChar;
end;

procedure TCustomButtonedEdit.SetLeftButton(AValue: TGlyphButtonOptions);
begin
  if AValue = nil then
    FLeftButton.Clear
  else if AValue <> FLeftButton then
    FLeftButton.Assign(AValue);
end;

procedure TCustomButtonedEdit.SetReadOnly(AValue: Boolean);
begin
  if FEditText.ReadOnly <> AValue then
    FEditText.ReadOnly := AValue;
end;

procedure TCustomButtonedEdit.SetTabStop(AValue: Boolean);
begin
  if FEditText.TabStop <> AValue then
    FEditText.TabStop := AValue;
end;

procedure TCustomButtonedEdit.SetRightButton(AValue: TGlyphButtonOptions);
begin
  if AValue = nil then
    FRightButton.Clear
  else if AValue <> FRightButton then
    FRightButton.Assign(AValue);
end;

procedure TCustomButtonedEdit.SetText(AValue: TCaption);
begin
  FEditText.Text := AValue;
end;

procedure TCustomButtonedEdit.SetTextHint(AValue: TTranslateString);
begin
  if FEditText.TextHint <> AValue then
    FEditText.TextHint := AValue;
end;

procedure TCustomButtonedEdit.UpdateSize;
begin
  FLeftButton.UpdateSize;
  FRightButton.UpdateSize;
  UpdateVerticalLayout;
end;

function TCustomButtonedEdit.CurrentRingSize: Integer;
var
  PPI: Integer;
begin
  PPI := Font.PixelsPerInch;
  if PPI <= 0 then
    PPI := 96;
  //Round(PPI / 96), minimum 1px.
  Result := (PPI + 48) div 96;
  if Result < 1 then
    Result := 1;
end;

function TCustomButtonedEdit.CurrentGapSize: Integer;
var
  PPI: Integer;
begin
  PPI := Font.PixelsPerInch;
  if PPI <= 0 then
    PPI := 96;
  //~3px at 96 DPI, scaled.
  Result := (3 * PPI + 48) div 96;
  if Result < 1 then
    Result := 1;
end;

function TCustomButtonedEdit.NativeEditHeight: Integer;
var
  W, H: Integer;
  OldBorder: TBorderStyle;
begin
  //The inner edit is borderless so it does not match a themed TEdit by itself.
  //Measure it once with the native border and cache the result. Toggling
  //BorderStyle on every CalculatePreferredSize would call RecreateWnd on
  //Win32 (TWin32WSWinControl.SetBorderStyle), recreating the handle on each
  //layout pass, so the measurement (which may allocate the handle) is done
  //exactly once per instance. The cache is dropped when the scale or the font
  //changes, because the native height depends on the font metrics.
  if FNativeEditHeight > 0 then
    Exit(FNativeEditHeight);

  if FEditText = nil then
    Exit(0);

  OldBorder := FEditText.BorderStyle;
  try
    FEditText.BorderStyle := bsSingle;
    FEditText.InvalidatePreferredSize;
    W := 0;
    H := 0;
    FEditText.GetPreferredSize(W, H, False, True);
  finally
    FEditText.BorderStyle := OldBorder;
  end;
  FNativeEditHeight := H;
  Result := FNativeEditHeight;
end;

function TCustomButtonedEdit.InnerHeight: Integer;
begin
  Result := Height - 2 * CurrentRingSize;
  if Result < 0 then
    Result := 0;
end;

procedure TCustomButtonedEdit.UpdateEditBounds;
var
  Extra: Integer;
begin
  if FEditText = nil then
    Exit;

  //A small gap between the edit and the frame/buttons on both sides.
  FEditText.BorderSpacing.Left := CurrentGapSize;
  FEditText.BorderSpacing.Right := CurrentGapSize;

  //Center the native edit vertically inside the inner area.
  Extra := InnerHeight - NativeEditHeight;
  if Extra < 0 then
    Extra := 0;
  FEditText.BorderSpacing.Top := Extra div 2;
  FEditText.BorderSpacing.Bottom := Extra - (Extra div 2);
end;

procedure TCustomButtonedEdit.UpdateVerticalLayout;
var
  InnerH: Integer;
begin
  //Reentrancy guard: setting BorderSpacing/Constraints can trigger a layout.
  if FUpdatingLayout then
    Exit;
  FUpdatingLayout := True;
  try
    InnerH := InnerHeight;
    if FLeftButton <> nil then
      FLeftButton.CenterVertically(InnerH);
    if FRightButton <> nil then
      FRightButton.CenterVertically(InnerH);
    UpdateEditBounds;
  finally
    FUpdatingLayout := False;
  end;
end;

procedure TCustomButtonedEdit.UpdateSpacing;
begin
  if FLeftButton <> nil then
    FLeftButton.SetOuterSpacing(CurrentGapSize);
  if FRightButton <> nil then
    FRightButton.SetOuterSpacing(CurrentGapSize);
  UpdateVerticalLayout;
  InvalidateClientRectCache(True);
  RequestAlign;
  Invalidate;
end;

procedure TCustomButtonedEdit.ChangeScale(Multiplier, Divider: Integer);
begin
  //Font metrics change with the scale: drop the cached native height.
  FNativeEditHeight := 0;
  inherited ChangeScale(Multiplier, Divider);
  UpdateSpacing;
end;

procedure TCustomButtonedEdit.FontChanged(Sender: TObject);
begin
  //The native height depends on the font metrics: recompute it.
  FNativeEditHeight := 0;
  inherited FontChanged(Sender);
end;

procedure TCustomButtonedEdit.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);
  InflateRect(ARect, -CurrentRingSize, -CurrentRingSize);
end;

procedure TCustomButtonedEdit.MouseEnter;
begin
  inherited MouseEnter;
  SetHovered(True);
end;

procedure TCustomButtonedEdit.MouseLeave;
begin
  inherited MouseLeave;
  UpdateHoverState;
end;

procedure TCustomButtonedEdit.Resize;
begin
  inherited Resize;
  //Keep the edit and the buttons centered when the height changes.
  UpdateVerticalLayout;
end;

procedure TCustomButtonedEdit.Paint;
begin
  inherited Paint;

  //Simulated frame drawn by the composite; the inner edit's native border is hidden.
  if (FEditText <> nil) and FEditText.Focused and (FFocusColor <> clNone) then
    DrawHighlightFrame(FFocusColor)
  else if FMouseInControl and (FHoverColor <> clNone) then
    DrawHighlightFrame(FHoverColor)
  else
    DrawHighlightFrame(FBorderColor);
end;

procedure TCustomButtonedEdit.DrawHighlightFrame(AColor: TColor);
begin
  if AColor = clNone then
    Exit;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := AColor;
  Canvas.FrameRect(ClientRect);
end;

procedure TCustomButtonedEdit.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if FAutoSizeHeightIsEditHeight then
    //Native TEdit height + the ring reserved for the simulated frame.
    PreferredHeight := NativeEditHeight + 2 * CurrentRingSize;
  //Width is user-defined, not auto-sized (like LCL's grouped edit).
  PreferredWidth := 0;
end;

procedure TCustomButtonedEdit.Loaded;
begin
  inherited Loaded;
  //LCL does not reapply AutoSize after loading, so the LFM height would stick.
  if AutoSize then
    AdjustSize;
end;

class function TCustomButtonedEdit.GetControlClassDefaultSize: TSize;
begin
  //Like LCL's grouped edit: as TCustomEdit + one button.
  Result.CX := 80 + 23;
  Result.CY := 23;
end;

procedure TCustomButtonedEdit.SetAutoSize(AValue: Boolean);
begin
  if AutoSize = AValue then
    Exit;
  inherited SetAutoSize(AValue);
  if FEditText <> nil then
    FEditText.AutoSize := AValue;
end;

constructor TCustomButtonedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoSizeHeightIsEditHeight := True;
  FUpdatingLayout := False;
  FBorderColor := clWindowFrame;
  FFocusColor := clHighlight;
  FHoverColor := clHighlight;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);                
  ParentColor := False;

  BorderStyle := bsNone;

  //Buttons
  FLeftButton := TGlyphButtonOptions.Create(Self, bpLeft);
  FRightButton := TGlyphButtonOptions.Create(Self, bpRight);
  FLeftButton.OnClick := DoLeftButtonClick;
  FRightButton.OnClick := DoRightButtonClick;

  //EditBox
  FEditText:= TEdit.Create(Self);
  with FEditText do
  begin
    Align := alClient;
    //Native border hidden: the composite draws the simulated frame.
    BorderStyle := bsNone;
    Parent := Self;
    ParentColor := True;
    ParentFont := True;
    OnChange := DoEditTextChange;
    OnClick := DoEditTextClick;
    OnContextPopup := DoEditTextContextPopup;
    OnDblClick := DoEditTextDblClick;
    OnEditingDone := DoEditTextEditingDone;
    OnKeyDown := DoEditTextKeyDown;
    OnKeyPress := DoEditTextKeyPress;
    OnKeyUp := DoEditTextKeyUp;
    OnMouseDown := DoEditTextMouseDown;
    OnMouseEnter := DoChildMouseEnter;
    OnMouseLeave := DoChildMouseLeave;
    OnMouseMove := DoEditTextMouseMove;
    OnMouseUp := DoEditTextMouseUp;
    OnUTF8KeyPress := DoEditTextUtf8KeyPress;
    OnEnter := DoEditTextEnter;
    OnExit := DoEditTextExit;
  end;

  UpdateSpacing;
  UpdateSize;

  //Anchor the height to the edit's height (like LCL's grouped edit / TEditButton).
  AutoSize := True;
end;

destructor TCustomButtonedEdit.Destroy;
begin
  FreeAndNil(FLeftButton);
  FreeAndNil(FEditText);
  FreeAndNil(FRightButton);

  inherited Destroy;
end;

procedure TCustomButtonedEdit.SetFocus;
begin
  FEditText.SetFocus;
end;

function TCustomButtonedEdit.Focused: Boolean;
begin
  Result:= FEditText.Focused;
end;

end.

