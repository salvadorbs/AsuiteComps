unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Menus, ImgList, ButtonedEdit;

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
    procedure FormCreate(Sender: TObject);
    procedure ButtonedEdit1LeftButtonClick(Sender: TObject);
    procedure ButtonedEdit1RightButtonClick(Sender: TObject);
  private
    FSearchEdit: TButtonedEdit;
    procedure BuildSampleImages(AList: TImageList);
    procedure SearchClearClick(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  ICON_SIZE = 16;

{ Draws a few simple, high-contrast 16x16 icons so the demo is meaningful
  out of the box: 0 = menu, 1 = clear, 2 = search. }
procedure TForm1.BuildSampleImages(AList: TImageList);
var
  Bmp: Graphics.TBitmap;

  procedure BeginIcon(AColor: TColor);
  begin
    Bmp := Graphics.TBitmap.Create;
    Bmp.SetSize(ICON_SIZE, ICON_SIZE);
    Bmp.Canvas.Brush.Color := AColor;
    Bmp.Canvas.FillRect(0, 0, ICON_SIZE, ICON_SIZE);
    Bmp.Canvas.Pen.Color := clWhite;
    Bmp.Canvas.Pen.Width := 1;
  end;

begin
  AList.Clear;
  AList.Width := ICON_SIZE;
  AList.Height := ICON_SIZE;

  // 0 - menu (three lines)
  BeginIcon($00B05000);
  try
    Bmp.Canvas.Pen.Width := 2;
    Bmp.Canvas.Line(3, 4, 12, 4);
    Bmp.Canvas.Line(3, 8, 12, 8);
    Bmp.Canvas.Line(3, 12, 12, 12);
  finally
    AList.Add(Bmp, nil);
    Bmp.Free;
  end;

  // 1 - clear (a cross)
  BeginIcon($00C02020);
  try
    Bmp.Canvas.Pen.Width := 2;
    Bmp.Canvas.Line(4, 4, 11, 11);
    Bmp.Canvas.Line(11, 4, 4, 11);
  finally
    AList.Add(Bmp, nil);
    Bmp.Free;
  end;

  // 2 - search (a magnifier)
  BeginIcon($002060C0);
  try
    Bmp.Canvas.Pen.Width := 2;
    Bmp.Canvas.Ellipse(3, 3, 10, 10);
    Bmp.Canvas.Line(10, 10, 13, 13);
  finally
    AList.Add(Bmp, nil);
    Bmp.Free;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Lbl: TLabel;
begin
  Caption := 'TButtonedEdit demo';
  ClientWidth := 480;
  ClientHeight := 280;

  BuildSampleImages(ImageList1);

  // Design-time edit: left = menu (with drop-down), right = clear.
  ButtonedEdit1.LeftButton.Images := ImageList1;
  ButtonedEdit1.LeftButton.ImagesWidth := ICON_SIZE;
  ButtonedEdit1.LeftButton.ImageIndex := 0;
  ButtonedEdit1.LeftButton.Visible := True;
  ButtonedEdit1.RightButton.Images := ImageList1;
  ButtonedEdit1.RightButton.ImagesWidth := ICON_SIZE;
  ButtonedEdit1.RightButton.ImageIndex := 1;
  ButtonedEdit1.RightButton.Visible := True;
  ButtonedEdit1.TextHint := 'Left = menu, Right = clear';

  // A second, code-created edit showing a search style.
  Lbl := TLabel.Create(Self);
  Lbl.Parent := Self;
  Lbl.SetBounds(48, 144, 320, 20);
  Lbl.Caption := 'Search style: left = search, right = clear';

  FSearchEdit := TButtonedEdit.Create(Self);
  FSearchEdit.Parent := Self;
  FSearchEdit.SetBounds(48, 168, 200, 24);
  FSearchEdit.LeftButton.Images := ImageList1;
  FSearchEdit.LeftButton.ImagesWidth := ICON_SIZE;
  FSearchEdit.LeftButton.ImageIndex := 2;
  FSearchEdit.LeftButton.Visible := True;
  FSearchEdit.RightButton.Images := ImageList1;
  FSearchEdit.RightButton.ImagesWidth := ICON_SIZE;
  FSearchEdit.RightButton.ImageIndex := 1;
  FSearchEdit.RightButton.Visible := True;
  FSearchEdit.TextHint := 'Type to search...';
  FSearchEdit.OnRightButtonClick := @SearchClearClick;
end;

procedure TForm1.ButtonedEdit1LeftButtonClick(Sender: TObject);
begin
  ShowMessage('Left button clicked!');
end;

procedure TForm1.ButtonedEdit1RightButtonClick(Sender: TObject);
begin
  ButtonedEdit1.Text := '';
  ShowMessage('Right button clicked: text cleared!');
end;

procedure TForm1.SearchClearClick(Sender: TObject);
begin
  FSearchEdit.Text := '';
end;

end.
