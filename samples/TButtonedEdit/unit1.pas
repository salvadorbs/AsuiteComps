unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Menus, ButtonedEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonedEdit1: TButtonedEdit;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ButtonedEdit1LeftButtonClick(Sender: TObject);
    procedure ButtonedEdit1RightButtonClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonedEdit1LeftButtonClick(Sender: TObject);
begin
  ShowMessage('ButtonedEdit1LeftButtonClick!');
end;

procedure TForm1.ButtonedEdit1RightButtonClick(Sender: TObject);
begin
  ShowMessage('ButtonedEdit1RightButtonClick!');
end;

end.

