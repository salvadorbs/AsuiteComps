unit Unit1;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Hotkeys.Manager, HotKey, Hotkeys.ShortcutEx;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    HotKey1: THotKey;
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
  if HotKey1.Hotkey <> 0 then
    HotkeyManager.RegisterNotify(HotKey1.Hotkey, NotifyEvent);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  HotkeyManager.UnregisterNotify(HotKey1.Hotkey)
end;

procedure TForm1.NotifyEvent(Sender: TObject; ShortcutEx: TShortcutEx);
begin
  ShowMessage('You typed the shorcut ' + ShortcutEx.totext);
end;

end.

