unit Unit1;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLProc, Hotkeys.Manager.Platform, HotKey, ShortcutGrabber, Hotkeys.ShortcutEx;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    HotKey1: THotKey;
    Label1: TLabel;
    btnChoose: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnChooseClick(Sender: TObject);
  private
    procedure NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if HotKey1.Hotkey = 0 then
  begin
    ShowMessage('No hotkey selected.');
    Exit;
  end;

  if HotkeyManager.RegisterNotify(HotKey1.Hotkey, NotifyEvent) then
  begin
    ShowMessage('Hotkey registered: ' + ShortCutToText(HotKey1.Hotkey));
    Button1.Enabled := False;
    Button2.Enabled := True;
    HotKey1.Enabled := False;
  end
  else
    ShowMessage('Cannot register hotkey (needs X11 or a Wayland portal with approval, plus a free shortcut).');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if HotkeyManager.UnregisterNotify(HotKey1.Hotkey) then
  begin
    ShowMessage('Hotkey unregistered.');
    Button1.Enabled := True;
    Button2.Enabled := False;
    HotKey1.Enabled := True;
  end
  else
    ShowMessage('Cannot unregister hotkey.');
end;

procedure TForm1.btnChooseClick(Sender: TObject);
var
  NewHotkey: TShortCut;
begin
  //THotKey captures directly; the dialog is opened explicitly here (in a real
  //app you would rather use THotKeyEdit, which opens it on click).
  if TfrmShortcutGrabber.TryExecute(Self, HotKey1.Hotkey, NewHotkey) then
  begin
    HotKey1.Hotkey := NewHotkey;
    ShowMessage('Selected: ' + ShortCutToText(HotKey1.Hotkey));
  end;
end;

procedure TForm1.NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  ShowMessage('You typed the shorcut ' + ShortcutEx.totext);
end;

end.
