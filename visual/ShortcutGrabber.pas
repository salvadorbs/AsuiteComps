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

unit ShortcutGrabber;

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs,
  Menus, LCLType, LCLProc, LResources, BGRABitmap, BCImageButton, HotKey;

const
  { File names used to look up button images inside a theme folder. }
  SHORTCUT_CTRL_FILE   = 'ctrl.png';
  SHORTCUT_ALT_FILE    = 'alt.png';
  SHORTCUT_SHIFT_FILE  = 'shift.png';
  SHORTCUT_WINKEY_FILE = 'winkey.png';

  { Embedded fallback resources (see ShortcutGrabber.lrs). }
  SHORTCUT_CTRL_RES    = 'asuite_ctrl';
  SHORTCUT_ALT_RES     = 'asuite_alt';
  SHORTCUT_SHIFT_RES   = 'asuite_shift';
  SHORTCUT_WINKEY_RES  = 'asuite_winkey';

type
  { Optional validator used by the dialog to reject a shortcut already taken
    by another application or by the system. Returning False keeps the dialog
    open and shows the "not available" message.
    The signature matches TBaseHotkeyManager.IsHotkeyAvailable, so a manager
    method can be assigned directly. }
  TShortcutValidateEvent = function(AShortcut: TShortCut): Boolean of object;

  { Indexes for TShortcutGrabberImages.GetPicture }
  TShortcutImageKind = (sikCtrl, sikAlt, sikShift, sikWinKey);

  { TShortcutGrabberImages }

  { Customizable images for the four modifier buttons. Each property is a
    TPicture, so it can be loaded from a file/stream or assigned any graphic
    (TPicture.LoadFromFile, TPicture.Assign, ...).

    When a picture is empty the component falls back to the embedded default
    images (Lazarus resources, the ones shipped with the default ASuite theme).
    Supplying different images (e.g. from a theme) is up to the application:
    assign them here (per dialog) or in ShortcutGrabberDefaults.Images. }
  TShortcutGrabberImages = class(TPersistent)
  private
    FCtrl: TPicture;
    FAlt: TPicture;
    FShift: TPicture;
    FWinKey: TPicture;
    FOnChange: TNotifyEvent;

    procedure PictureChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function IsEmpty: Boolean;
    function GetPicture(AKind: TShortcutImageKind): TPicture;

    { Fired whenever one of the four pictures changes. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Ctrl: TPicture read FCtrl;
    property Alt: TPicture read FAlt;
    property Shift: TPicture read FShift;
    property WinKey: TPicture read FWinKey;
  end;

  { TShortcutGrabberDefaults }

  { Process-wide defaults used by TfrmShortcutGrabber.
    An application (e.g. ASuite) configures these once: localized messages,
    the optional validator and the button images it wants to use (leaving them
    empty keeps the embedded defaults). }
  TShortcutGrabberDefaults = class(TPersistent)
  private
    FMessageNoKey: string;
    FMessageNoModifier: string;
    FMessageNotAvailable: string;
    FImages: TShortcutGrabberImages;
    FOnValidateHotkey: TShortcutValidateEvent;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MessageNoKey: string read FMessageNoKey write FMessageNoKey;
    property MessageNoModifier: string read FMessageNoModifier write FMessageNoModifier;
    property MessageNotAvailable: string read FMessageNotAvailable write FMessageNotAvailable;
    property Images: TShortcutGrabberImages read FImages;
    property OnValidateHotkey: TShortcutValidateEvent read FOnValidateHotkey write FOnValidateHotkey;
  end;

  { TfrmShortcutGrabber }

  TfrmShortcutGrabber = class(TForm)
    btnAlt: TBCImageButton;
    btnCtrl: TBCImageButton;
    btnShift: TBCImageButton;
    btnWinKey: TBCImageButton;
    hkKeys: THotKey;
    lblInfo: TLabel;
    pnlDialogPage: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure hkKeysChange(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  private
    FHotkey: TShortCut;
    FCanClose: Boolean;
    FTargetHotKey: THotKey;
    FImages: TShortcutGrabberImages;
    FUpdatingKeys: Boolean;
    FOnValidateHotkey: TShortcutValidateEvent;
    FMessageNoKey: string;
    FMessageNoModifier: string;
    FMessageNotAvailable: string;

    function GetMessageNoKey: string;
    function GetMessageNoModifier: string;
    function GetMessageNotAvailable: string;
    function GetValidateHotkey: TShortcutValidateEvent;

    function GetModifierFromGUI: TShiftState;
    function GetKeyFromGUI: Word;

    procedure SetGUIKeyFromKey(AKey: Word);
    procedure SetGUIModifierFromShiftState(AMod: TShiftState);
    procedure SetHotkey(AValue: TShortCut);
    procedure SetTargetHotKey(AValue: THotKey);
    procedure ApplyToTarget;
    procedure ImagesChanged(Sender: TObject);

    function GetCtrlImage: TPicture;
    function GetAltImage: TPicture;
    function GetShiftImage: TPicture;
    function GetWinKeyImage: TPicture;

    function EffectivePicture(AKind: TShortcutImageKind): TPicture;
    procedure ApplyImageToButton(AButton: TBCImageButton; APicture: TPicture;
      const AResourceName: string);
    procedure ReplaceButtonBitmap(AButton: TBCImageButton; ABitmap: TBGRABitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Current (confirmed) shortcut. Reading it after mrOk returns the new value. }
    property Hotkey: TShortCut read FHotkey write SetHotkey;

    { Optional direct binding to a THotKey control: the dialog starts from its
      Hotkey and writes the confirmed value back (firing its OnChange). }
    property TargetHotKey: THotKey read FTargetHotKey write SetTargetHotKey;

    { Per-instance images. Empty pictures fall back to ShortcutGrabberDefaults
      and then to the embedded defaults. Changing a picture reloads the button
      automatically. }
    property Images: TShortcutGrabberImages read FImages;
    property CtrlImage: TPicture read GetCtrlImage;
    property AltImage: TPicture read GetAltImage;
    property ShiftImage: TPicture read GetShiftImage;
    property WinKeyImage: TPicture read GetWinKeyImage;

    property OnValidateHotkey: TShortcutValidateEvent read GetValidateHotkey write FOnValidateHotkey;
    property MessageNoKey: string read GetMessageNoKey write FMessageNoKey;
    property MessageNoModifier: string read GetMessageNoModifier write FMessageNoModifier;
    property MessageNotAvailable: string read GetMessageNotAvailable write FMessageNotAvailable;

    { Updates the interface (modifier buttons + key control) from a shortcut. }
    procedure SetGuiFromHotkey(AHotkey: TShortCut);

    { (Re)loads the four button images using the current configuration. }
    procedure LoadImages;

    { Shows the dialog starting from AInitialHotkey. Returns True when the user
      confirms; AHotkey is then the chosen shortcut. Returns False when the
      dialog is cancelled (AHotkey is left as AInitialHotkey). }
    class function TryExecute(AOwner: TComponent; AInitialHotkey: TShortCut;
      out AHotkey: TShortCut;
      AValidate: TShortcutValidateEvent = nil): Boolean; overload;

    { Standalone entry point: shows the dialog starting from AHotkey and returns
      the chosen shortcut (0 when cancelled). }
    class function Execute(AOwner: TComponent; AHotkey: TShortCut;
      AValidate: TShortcutValidateEvent = nil): TShortCut; overload;

    { String-based overload kept for backward compatibility with ASuite. }
    class function Execute(AOwner: TComponent; const AHotkey: string;
      AValidate: TShortcutValidateEvent = nil): string; overload;
  end;

var
  { Process-wide defaults, created at unit initialization. }
  ShortcutGrabberDefaults: TShortcutGrabberDefaults;

implementation

{$R *.lfm}

{ Returns True when APicture holds an actual image. }
function PictureHasImage(APicture: TPicture): Boolean;
begin
  Result := (APicture <> nil) and (APicture.Graphic <> nil) and
    (not APicture.Graphic.Empty);
end;

{ TShortcutGrabberImages }

constructor TShortcutGrabberImages.Create;
begin
  inherited Create;
  FCtrl   := TPicture.Create;
  FAlt    := TPicture.Create;
  FShift  := TPicture.Create;
  FWinKey := TPicture.Create;

  FCtrl.OnChange   := PictureChanged;
  FAlt.OnChange    := PictureChanged;
  FShift.OnChange  := PictureChanged;
  FWinKey.OnChange := PictureChanged;
end;

procedure TShortcutGrabberImages.PictureChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

destructor TShortcutGrabberImages.Destroy;
begin
  FCtrl.Free;
  FAlt.Free;
  FShift.Free;
  FWinKey.Free;
  inherited Destroy;
end;

procedure TShortcutGrabberImages.Assign(Source: TPersistent);
begin
  if Source is TShortcutGrabberImages then
  begin
    FCtrl.Assign(TShortcutGrabberImages(Source).Ctrl);
    FAlt.Assign(TShortcutGrabberImages(Source).Alt);
    FShift.Assign(TShortcutGrabberImages(Source).Shift);
    FWinKey.Assign(TShortcutGrabberImages(Source).WinKey);
  end
  else
    inherited Assign(Source);
end;

procedure TShortcutGrabberImages.Clear;
begin
  FCtrl.Clear;
  FAlt.Clear;
  FShift.Clear;
  FWinKey.Clear;
end;

function TShortcutGrabberImages.IsEmpty: Boolean;
begin
  Result := (not PictureHasImage(FCtrl)) and (not PictureHasImage(FAlt)) and
    (not PictureHasImage(FShift)) and (not PictureHasImage(FWinKey));
end;

function TShortcutGrabberImages.GetPicture(AKind: TShortcutImageKind): TPicture;
begin
  case AKind of
    sikCtrl:   Result := FCtrl;
    sikAlt:    Result := FAlt;
    sikShift:  Result := FShift;
    sikWinKey: Result := FWinKey;
  else
    Result := nil;
  end;
end;

{ TShortcutGrabberDefaults }

constructor TShortcutGrabberDefaults.Create;
begin
  inherited Create;
  FImages := TShortcutGrabberImages.Create;

  FMessageNoKey        := 'You haven''t selected any key!';
  FMessageNoModifier   := 'You haven''t selected any modifier keys!';
  FMessageNotAvailable := 'This hotkey is being used already by another ' +
    'software or by the system itself. Please choose another one.';
end;

destructor TShortcutGrabberDefaults.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

{ TfrmShortcutGrabber }

constructor TfrmShortcutGrabber.Create(AOwner: TComponent);
begin
  FImages := TShortcutGrabberImages.Create;
  FImages.OnChange := ImagesChanged;
  inherited Create(AOwner);
end;

destructor TfrmShortcutGrabber.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

procedure TfrmShortcutGrabber.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FHotkey := 0;

  LoadImages;
end;

procedure TfrmShortcutGrabber.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;
end;

procedure TfrmShortcutGrabber.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_RETURN then
    btnOkClick(Sender)
  else if Ord(Key) = VK_ESCAPE then
    btnCancelClick(Sender);
end;

procedure TfrmShortcutGrabber.btnCancelClick(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TfrmShortcutGrabber.btnOkClick(Sender: TObject);
var
  Key: Word;
  Modifiers: TShiftState;
  NewHotkey: TShortCut;
  Validate: TShortcutValidateEvent;
begin
  FCanClose := False;

  Key := GetKeyFromGUI;
  Modifiers := GetModifierFromGUI;

  if Key = 0 then
  begin
    MessageDlg(MessageNoKey, mtWarning, [mbOK], 0);
    hkKeys.SetFocus;
    Exit;
  end;

  if Modifiers = [] then
  begin
    MessageDlg(MessageNoModifier, mtWarning, [mbOK], 0);
    hkKeys.SetFocus;
    Exit;
  end;

  NewHotkey := KeyToShortCut(Key, Modifiers);

  //Unchanged value: nothing to validate, just confirm.
  if NewHotkey = FHotkey then
  begin
    FCanClose := True;
    Exit;
  end;

  Validate := GetValidateHotkey;
  if Assigned(Validate) and (not Validate(NewHotkey)) then
  begin
    MessageDlg(MessageNotAvailable, mtWarning, [mbOK], 0);
    hkKeys.SetFocus;
    Exit;
  end;

  FHotkey := NewHotkey;
  ApplyToTarget;
  FCanClose := True;
end;

procedure TfrmShortcutGrabber.SetTargetHotKey(AValue: THotKey);
begin
  if FTargetHotKey = AValue then
    Exit;

  if FTargetHotKey <> nil then
    FTargetHotKey.RemoveFreeNotification(Self);

  FTargetHotKey := AValue;

  if FTargetHotKey <> nil then
  begin
    FTargetHotKey.FreeNotification(Self);
    Hotkey := FTargetHotKey.Hotkey;
  end;
end;

procedure TfrmShortcutGrabber.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FTargetHotKey) then
    FTargetHotKey := nil;
end;

procedure TfrmShortcutGrabber.ApplyToTarget;
begin
  if Assigned(FTargetHotKey) then
    FTargetHotKey.Hotkey := FHotkey;
end;

procedure TfrmShortcutGrabber.ImagesChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) and
    not (csDestroying in ComponentState) then
    LoadImages;
end;

function TfrmShortcutGrabber.GetCtrlImage: TPicture;
begin
  Result := FImages.Ctrl;
end;

function TfrmShortcutGrabber.GetAltImage: TPicture;
begin
  Result := FImages.Alt;
end;

function TfrmShortcutGrabber.GetShiftImage: TPicture;
begin
  Result := FImages.Shift;
end;

function TfrmShortcutGrabber.GetWinKeyImage: TPicture;
begin
  Result := FImages.WinKey;
end;

function TfrmShortcutGrabber.GetMessageNoKey: string;
begin
  if FMessageNoKey <> '' then
    Result := FMessageNoKey
  else
    Result := ShortcutGrabberDefaults.MessageNoKey;
end;

function TfrmShortcutGrabber.GetMessageNoModifier: string;
begin
  if FMessageNoModifier <> '' then
    Result := FMessageNoModifier
  else
    Result := ShortcutGrabberDefaults.MessageNoModifier;
end;

function TfrmShortcutGrabber.GetMessageNotAvailable: string;
begin
  if FMessageNotAvailable <> '' then
    Result := FMessageNotAvailable
  else
    Result := ShortcutGrabberDefaults.MessageNotAvailable;
end;

function TfrmShortcutGrabber.GetValidateHotkey: TShortcutValidateEvent;
begin
  if Assigned(FOnValidateHotkey) then
    Result := FOnValidateHotkey
  else
    Result := ShortcutGrabberDefaults.OnValidateHotkey;
end;

function TfrmShortcutGrabber.GetKeyFromGUI: Word;
var
  Key: Word;
  Modifier: TShiftState;
begin
  ShortCutToKey(hkKeys.Hotkey, Key, Modifier);
  Result := Key;
end;

function TfrmShortcutGrabber.GetModifierFromGUI: TShiftState;
begin
  Result := [];
  if btnCtrl.Pressed then
    Result := Result + [ssCtrl];
  if btnShift.Pressed then
    Result := Result + [ssShift];
  if btnAlt.Pressed then
    Result := Result + [ssAlt];
  if btnWinKey.Pressed then
    Result := Result + [ssMeta];
end;

procedure TfrmShortcutGrabber.SetGUIKeyFromKey(AKey: Word);
begin
  hkKeys.Hotkey := ShortCut(AKey, []);
end;

procedure TfrmShortcutGrabber.SetGUIModifierFromShiftState(AMod: TShiftState);
begin
  btnCtrl.Pressed   := ssCtrl in AMod;
  btnShift.Pressed  := ssShift in AMod;
  btnAlt.Pressed    := ssAlt in AMod;
  btnWinKey.Pressed := ssMeta in AMod;
end;

procedure TfrmShortcutGrabber.SetHotkey(AValue: TShortCut);
begin
  FHotkey := AValue;
  SetGuiFromHotkey(AValue);
end;

procedure TfrmShortcutGrabber.SetGuiFromHotkey(AHotkey: TShortCut);
var
  Key: Word;
  Modi: TShiftState;
begin
  ShortCutToKey(AHotkey, Key, Modi);
  SetGUIKeyFromKey(Key);
  SetGUIModifierFromShiftState(Modi);
end;

procedure TfrmShortcutGrabber.hkKeysChange(Sender: TObject);
var
  Key: Word;
  Modi: TShiftState;
begin
  //SetGUIKeyFromKey re-assigns hkKeys.Hotkey and would fire this handler again
  if FUpdatingKeys then
    Exit;

  FUpdatingKeys := True;
  try
    //Separate key and modifiers coming from THotKey and reflect them on the GUI
    ShortCutToKey(hkKeys.Hotkey, Key, Modi);
    SetGUIModifierFromShiftState(Modi);
    SetGUIKeyFromKey(Key);
  finally
    FUpdatingKeys := False;
  end;
end;

function TfrmShortcutGrabber.EffectivePicture(AKind: TShortcutImageKind): TPicture;
begin
  Result := FImages.GetPicture(AKind);
  if not PictureHasImage(Result) then
    Result := ShortcutGrabberDefaults.Images.GetPicture(AKind);
end;

procedure TfrmShortcutGrabber.ReplaceButtonBitmap(AButton: TBCImageButton;
  ABitmap: TBGRABitmap);
var
  Old: TBGRABitmap;
begin
  Old := AButton.BitmapOptions.Bitmap;
  AButton.BitmapOptions.Bitmap := ABitmap;
  if (Old <> nil) and (Old <> ABitmap) then
    Old.Free;
end;

procedure TfrmShortcutGrabber.ApplyImageToButton(AButton: TBCImageButton;
  APicture: TPicture; const AResourceName: string);
var
  Stream: TStream;
  Res: TLazarusResourceStream;
begin
  Stream := nil;
  Res := nil;
  try
    //1) Picture assigned from the outside (per-instance or global defaults)
    if PictureHasImage(APicture) then
    begin
      Stream := TMemoryStream.Create;
      APicture.SaveToStream(Stream);
      Stream.Position := 0;
    end
    else
    begin
      //2) Embedded default image (Lazarus resource, default ASuite theme)
      try
        Res := TLazarusResourceStream.Create(AResourceName, 'PNG');
      except
        Res := nil;
      end;
      if Res <> nil then
      begin
        Stream := TMemoryStream.Create;
        Stream.CopyFrom(Res, 0);
        Stream.Position := 0;
      end;
    end;

    if Stream <> nil then
      ReplaceButtonBitmap(AButton, TBGRABitmap.Create(Stream));
  finally
    Res.Free;
    Stream.Free;
  end;
end;

procedure TfrmShortcutGrabber.LoadImages;
begin
  ApplyImageToButton(btnCtrl, EffectivePicture(sikCtrl), SHORTCUT_CTRL_RES);
  ApplyImageToButton(btnAlt, EffectivePicture(sikAlt), SHORTCUT_ALT_RES);
  ApplyImageToButton(btnShift, EffectivePicture(sikShift), SHORTCUT_SHIFT_RES);
  ApplyImageToButton(btnWinKey, EffectivePicture(sikWinKey), SHORTCUT_WINKEY_RES);
end;

class function TfrmShortcutGrabber.TryExecute(AOwner: TComponent;
  AInitialHotkey: TShortCut; out AHotkey: TShortCut;
  AValidate: TShortcutValidateEvent): Boolean;
var
  Form: TfrmShortcutGrabber;
begin
  AHotkey := AInitialHotkey;
  Result := False;

  Form := TfrmShortcutGrabber.Create(AOwner);
  try
    if Assigned(AValidate) then
      Form.OnValidateHotkey := AValidate;

    Form.Hotkey := AInitialHotkey;
    Form.LoadImages;

    if Form.ShowModal = mrOk then
    begin
      AHotkey := Form.Hotkey;
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;

class function TfrmShortcutGrabber.Execute(AOwner: TComponent; AHotkey: TShortCut;
  AValidate: TShortcutValidateEvent): TShortCut;
var
  Chosen: TShortCut;
begin
  if TfrmShortcutGrabber.TryExecute(AOwner, AHotkey, Chosen, AValidate) then
    Result := Chosen
  else
    Result := 0;
end;

class function TfrmShortcutGrabber.Execute(AOwner: TComponent;
  const AHotkey: string; AValidate: TShortcutValidateEvent): string;
var
  Shortcut: TShortCut;
begin
  Shortcut := TextToShortCut(AHotkey);
  Shortcut := TfrmShortcutGrabber.Execute(AOwner, Shortcut, AValidate);

  if Shortcut = 0 then
    Result := ''
  else
    Result := UpperCase(ShortCutToText(Shortcut));
end;

initialization
  ShortcutGrabberDefaults := TShortcutGrabberDefaults.Create;
  {$I ShortcutGrabber.lrs}

finalization
  FreeAndNil(ShortcutGrabberDefaults);

end.
