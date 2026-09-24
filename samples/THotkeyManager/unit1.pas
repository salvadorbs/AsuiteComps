unit Unit1;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  LCLProc, Hotkeys.Manager.Platform, HotKeyEdit, Hotkeys.ShortcutEx;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtHotkey: THotKeyEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  if edtHotkey.Hotkey = 0 then
  begin
    ShowMessage('No hotkey selected.');
    Exit;
  end;

  if HotkeyManager.RegisterNotify(edtHotkey.Hotkey, NotifyEvent) then
  begin
    ShowMessage('Hotkey registered: ' + ShortCutToText(edtHotkey.Hotkey));
    Button1.Enabled := False;
    Button2.Enabled := True;
    edtHotkey.Enabled := False;
  end
  else
    ShowMessage('Cannot register hotkey (needs X11 or a Wayland portal with approval, plus a free shortcut).');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if HotkeyManager.UnregisterNotify(edtHotkey.Hotkey) then
  begin
    ShowMessage('Hotkey unregistered.');
    Button1.Enabled := True;
    Button2.Enabled := False;
    edtHotkey.Enabled := True;
  end
  else
    ShowMessage('Cannot unregister hotkey.');
end;

procedure TForm1.NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  ShowMessage('You typed the shorcut ' + ShortcutEx.totext);
end;

end.
