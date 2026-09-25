unit Unit1;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ImgList,
  Menus, LCLType, LCLProc, Hotkeys.Manager.Platform, HotKeyEdit,
  Hotkeys.ShortcutEx;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    edtGrabber: THotKeyEdit;
    edtInline: THotKeyEdit;
    edtInlineNoMod: THotKeyEdit;
    edtOnlyWithHotkey: THotKeyEdit;
    edtCustomIcons: THotKeyEdit;
    lblValue: TLabel;
    btnSet: TButton;
    btnClear: TButton;
    btnRegister: TButton;
    btnUnregister: TButton;
    FRegisteredHotkey: TShortCut;

    procedure HotkeyChanged(Sender: TObject);
    procedure SetClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure RegisterClick(Sender: TObject);
    procedure UnregisterClick(Sender: TObject);
    procedure NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
    function ValidateHotkey(AShortcut: TShortCut): Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ Builds a description label + a THotKeyEdit in a column. }
function AddRow(AParent: TWinControl; var AY: Integer;
  const ACaption: string): THotKeyEdit;
var
  Lbl: TLabel;
begin
  Lbl := TLabel.Create(AParent);
  Lbl.Parent := AParent;
  Lbl.Left := 16;
  Lbl.Top := AY;
  Lbl.Caption := ACaption;
  Inc(AY, 20);

  Result := THotKeyEdit.Create(AParent);
  Result.Parent := AParent;
  Result.Left := 16;
  Result.Top := AY;
  Result.Width := 220;
  Inc(AY, 46);
end;

{ Two simple 16x16 colors, used to show custom Choose/Clear icons. }
function MakeDemoImages(AOwner: TComponent): TImageList;
var
  Bmp: TBitmap;
begin
  Result := TImageList.Create(AOwner);
  Result.Width := 16;
  Result.Height := 16;
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(16, 16);
    Bmp.Canvas.Brush.Color := clGreen;
    Bmp.Canvas.FillRect(0, 0, 16, 16);
    Result.Add(Bmp, nil);
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(0, 0, 16, 16);
    Result.Add(Bmp, nil);
  finally
    Bmp.Free;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Y: Integer;
  ImgList: TImageList;
begin
  Caption := 'THotKeyEdit demo';
  ClientWidth := 560;
  ClientHeight := 430;

  Y := 12;

  edtGrabber := AddRow(Self, Y, '1. Grabber on click (default)');
  edtGrabber.Hotkey := ShortCut(VK_F5, [ssCtrl]);
  edtGrabber.OnValidateHotkey := ValidateHotkey;
  edtGrabber.OnHotkeyChange := HotkeyChanged;

  edtInline := AddRow(Self, Y, '2. Inline capture (ShowGrabberOnClick = False)');
  edtInline.ShowGrabberOnClick := False;
  edtInline.OnHotkeyChange := HotkeyChanged;

  edtInlineNoMod := AddRow(Self, Y, '3. Inline + NoModifier (bare key allowed)');
  edtInlineNoMod.ShowGrabberOnClick := False;
  edtInlineNoMod.NoModifier := True;
  edtInlineNoMod.OnHotkeyChange := HotkeyChanged;

  edtOnlyWithHotkey := AddRow(Self, Y, '4. ButtonVisibleOnlyWithHotkey = True');
  edtOnlyWithHotkey.ButtonVisibleOnlyWithHotkey := True;
  edtOnlyWithHotkey.OnHotkeyChange := HotkeyChanged;

  edtCustomIcons := AddRow(Self, Y, '5. Custom icons (Choose/Clear + ImagesWidth)');
  ImgList := MakeDemoImages(Self);
  edtCustomIcons.UseDefaultImages := False;
  edtCustomIcons.RightButton.Images := ImgList;
  edtCustomIcons.RightButton.ImagesWidth := ImgList.Width;
  edtCustomIcons.ChooseImageIndex := 0;
  edtCustomIcons.ClearImageIndex := 1;
  edtCustomIcons.OnHotkeyChange := HotkeyChanged;

  lblValue := TLabel.Create(Self);
  lblValue.Parent := Self;
  lblValue.Left := 16;
  lblValue.Top := Y + 4;
  lblValue.Caption := 'Last change: (none)';

  btnSet := TButton.Create(Self);
  btnSet.Parent := Self;
  btnSet.SetBounds(16, Y + 30, 90, 25);
  btnSet.Caption := 'Set Alt+F6';
  btnSet.OnClick := SetClick;

  btnClear := TButton.Create(Self);
  btnClear.Parent := Self;
  btnClear.SetBounds(112, Y + 30, 90, 25);
  btnClear.Caption := 'Clear';
  btnClear.OnClick := ClearClick;

  btnRegister := TButton.Create(Self);
  btnRegister.Parent := Self;
  btnRegister.SetBounds(220, Y + 30, 90, 25);
  btnRegister.Caption := 'Register';
  btnRegister.OnClick := RegisterClick;

  btnUnregister := TButton.Create(Self);
  btnUnregister.Parent := Self;
  btnUnregister.SetBounds(316, Y + 30, 90, 25);
  btnUnregister.Caption := 'Unregister';
  btnUnregister.Enabled := False;
  btnUnregister.OnClick := UnregisterClick;
end;

procedure TForm1.HotkeyChanged(Sender: TObject);
begin
  lblValue.Caption := 'Last change: ' + ShortCutToText(THotKeyEdit(Sender).Hotkey);
end;

procedure TForm1.SetClick(Sender: TObject);
begin
  edtGrabber.Hotkey := ShortCut(VK_F6, [ssAlt]);
end;

procedure TForm1.ClearClick(Sender: TObject);
begin
  edtGrabber.ClearHotkey;
end;

procedure TForm1.RegisterClick(Sender: TObject);
begin
  if edtGrabber.Hotkey = 0 then
  begin
    ShowMessage('No hotkey selected.');
    Exit;
  end;

  if HotkeyManager.RegisterNotify(edtGrabber.Hotkey, NotifyEvent) then
  begin
    FRegisteredHotkey := edtGrabber.Hotkey;
    ShowMessage('Hotkey registered: ' + ShortCutToText(FRegisteredHotkey));
    btnRegister.Enabled := False;
    btnUnregister.Enabled := True;
    edtGrabber.Enabled := False;
    btnSet.Enabled := False;
    btnClear.Enabled := False;
  end
  else
    ShowMessage('Cannot register hotkey (needs X11 or a Wayland portal with approval, plus a free shortcut).');
end;

procedure TForm1.UnregisterClick(Sender: TObject);
begin
  if HotkeyManager.UnregisterNotify(FRegisteredHotkey) then
  begin
    ShowMessage('Hotkey unregistered: ' + ShortCutToText(FRegisteredHotkey));
    FRegisteredHotkey := 0;
    btnRegister.Enabled := True;
    btnUnregister.Enabled := False;
    edtGrabber.Enabled := True;
    btnSet.Enabled := True;
    btnClear.Enabled := True;
  end
  else
    ShowMessage('Cannot unregister hotkey.');
end;

procedure TForm1.NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  ShowMessage('You typed the shortcut ' + ShortcutEx.totext);
end;

function TForm1.ValidateHotkey(AShortcut: TShortCut): Boolean;
begin
  { Demo: reject Ctrl+Q. }
  Result := AShortcut <> ShortCut(VK_Q, [ssCtrl]);
  if not Result then
    ShowMessage('Ctrl+Q is not allowed by the demo validator.');
end;

end.
