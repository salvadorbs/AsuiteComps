unit Unit1;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ImgList,
  Menus, LCLType, LCLProc, Hotkeys.Manager.Platform, HotKeyEdit,
  Hotkeys.ShortcutEx;

type

  { One demo row: a THotKeyEdit plus its own register/unregister/clear buttons. }
  TRow = class
    Edit: THotKeyEdit;
    BtnRegister: TButton;
    BtnUnregister: TButton;
    BtnClear: TButton;
    Registered: TShortCut;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FRows: array of TRow;
    lblValue: TLabel;

    function AddRow(const ACaption: string; AY: Integer): TRow;
    procedure EditChanged(Sender: TObject);
    procedure RegisterClick(Sender: TObject);
    procedure UnregisterClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
    function ValidateHotkey(AShortcut: TShortCut): Boolean;
    procedure UpdateRowButtons(ARowIndex: Integer);
  public
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

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

destructor TForm1.Destroy;
var
  Row: TRow;
begin
  for Row in FRows do
    Row.Free;
  inherited Destroy;
end;

function TForm1.AddRow(const ACaption: string; AY: Integer): TRow;
var
  Lbl: TLabel;
  Row: TRow;
begin
  Lbl := TLabel.Create(Self);
  Lbl.Parent := Self;
  Lbl.Left := 16;
  Lbl.Top := AY;
  Lbl.Caption := ACaption;

  Row := TRow.Create;
  Row.Edit := THotKeyEdit.Create(Self);
  Row.Edit.Parent := Self;
  Row.Edit.SetBounds(16, AY + 20, 200, 24);
  Row.Edit.OnHotkeyChange := EditChanged;

  Row.BtnRegister := TButton.Create(Self);
  Row.BtnRegister.Parent := Self;
  Row.BtnRegister.SetBounds(232, AY + 20, 84, 24);
  Row.BtnRegister.Caption := 'Register';
  Row.BtnRegister.Tag := Length(FRows);
  Row.BtnRegister.OnClick := RegisterClick;

  Row.BtnUnregister := TButton.Create(Self);
  Row.BtnUnregister.Parent := Self;
  Row.BtnUnregister.SetBounds(322, AY + 20, 84, 24);
  Row.BtnUnregister.Caption := 'Unregister';
  Row.BtnUnregister.Tag := Length(FRows);
  Row.BtnUnregister.Enabled := False;
  Row.BtnUnregister.OnClick := UnregisterClick;

  Row.BtnClear := TButton.Create(Self);
  Row.BtnClear.Parent := Self;
  Row.BtnClear.SetBounds(412, AY + 20, 64, 24);
  Row.BtnClear.Caption := 'Clear';
  Row.BtnClear.Tag := Length(FRows);
  Row.BtnClear.OnClick := ClearClick;

  SetLength(FRows, Length(FRows) + 1);
  FRows[High(FRows)] := Row;
  Result := Row;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Row: TRow;
  ImgList: TImageList;
  Y: Integer;
begin
  Caption := 'THotKeyEdit demo';
  ClientWidth := 620;
  ClientHeight := 430;

  Y := 12;

  Row := AddRow('1. Grabber on click (default)', Y);
  Row.Edit.Hotkey := ShortCut(VK_F5, [ssCtrl]);
  Row.Edit.OnValidateHotkey := ValidateHotkey;
  Inc(Y, 54);

  Row := AddRow('2. Inline capture (ShowGrabberOnClick = False)', Y);
  Row.Edit.ShowGrabberOnClick := False;
  Inc(Y, 54);

  Row := AddRow('3. Inline + NoModifier (bare key allowed)', Y);
  Row.Edit.ShowGrabberOnClick := False;
  Row.Edit.NoModifier := True;
  Inc(Y, 54);

  Row := AddRow('4. Grabber + NoModifier (bare key allowed)', Y);
  Row.Edit.NoModifier := True;
  Inc(Y, 54);

  Row := AddRow('5. ButtonVisibleOnlyWithHotkey = True', Y);
  Row.Edit.ButtonVisibleOnlyWithHotkey := True;
  Inc(Y, 54);

  Row := AddRow('6. Custom icons (Choose/Clear + ImagesWidth)', Y);
  ImgList := MakeDemoImages(Self);
  Row.Edit.UseDefaultImages := False;
  Row.Edit.RightButton.Images := ImgList;
  Row.Edit.RightButton.ImagesWidth := ImgList.Width;
  Row.Edit.ChooseImageIndex := 0;
  Row.Edit.ClearImageIndex := 1;
  Inc(Y, 54);

  lblValue := TLabel.Create(Self);
  lblValue.Parent := Self;
  lblValue.SetBounds(16, Y + 4, 560, 20);
  lblValue.Caption := 'Last change: (none)';
end;

procedure TForm1.EditChanged(Sender: TObject);
begin
  // The initial Hotkey values are set in FormCreate before lblValue exists, so
  // the change event can fire while lblValue is still nil.
  if Assigned(lblValue) then
    lblValue.Caption := 'Last change: ' + ShortCutToText(THotKeyEdit(Sender).Hotkey);
end;

procedure TForm1.UpdateRowButtons(ARowIndex: Integer);
var
  Registered: Boolean;
begin
  Registered := FRows[ARowIndex].Registered <> 0;
  FRows[ARowIndex].BtnRegister.Enabled := not Registered;
  FRows[ARowIndex].BtnUnregister.Enabled := Registered;
  FRows[ARowIndex].BtnClear.Enabled := not Registered;
  FRows[ARowIndex].Edit.Enabled := not Registered;
end;

procedure TForm1.RegisterClick(Sender: TObject);
var
  Row: TRow;
  Index: Integer;
begin
  Index := TButton(Sender).Tag;
  Row := FRows[Index];

  if Row.Edit.Hotkey = 0 then
  begin
    ShowMessage('No hotkey selected.');
    Exit;
  end;

  if HotkeyManager.RegisterNotify(Row.Edit.Hotkey, NotifyEvent) then
  begin
    Row.Registered := Row.Edit.Hotkey;
    ShowMessage('Hotkey registered: ' + ShortCutToText(Row.Registered));
    UpdateRowButtons(Index);
  end
  else
    ShowMessage('Cannot register hotkey (needs X11 or a Wayland portal with approval, plus a free shortcut).');
end;

procedure TForm1.UnregisterClick(Sender: TObject);
var
  Row: TRow;
  Index: Integer;
begin
  Index := TButton(Sender).Tag;
  Row := FRows[Index];

  if HotkeyManager.UnregisterNotify(Row.Registered) then
  begin
    ShowMessage('Hotkey unregistered: ' + ShortCutToText(Row.Registered));
    Row.Registered := 0;
    UpdateRowButtons(Index);
  end
  else
    ShowMessage('Cannot unregister hotkey.');
end;

procedure TForm1.ClearClick(Sender: TObject);
begin
  FRows[TButton(Sender).Tag].Edit.ClearHotkey;
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
