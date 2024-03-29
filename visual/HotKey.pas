{
a) override the methods DoEnter and DoExit. These are called when the control
gets or looses the focus. Always call the inherited method (first or last,
your choice, this fires the OnEnter and OnExit events, respectively). In
DoEnter you create, place, and show the caret (see CreateCaret, ShowCaret,
SetCaretPos in win32.hlp). In DoExit you hide the caret and destroy it
(HideCaret, DestroyCaret).

b) add a handler for the WM_GETDLGCODE message, reply with a message result of
   DLGC_WANTALLKEYS.

c) Override the KeyDown and KeyPress methods to process keyboard input.
}
unit HotKey;

{$mode DelphiUnicode}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus, {$IFDEF Windows}Windows,{$ENDIF} LMessages, LCLIntf, LCLType, LCLProc, LazLogger;

type
  THKModifier = (
    hkShift,
    hkCtrl,
    hkAlt,
    hkExt
  );

  THKModifiers = set of THKModifier;

  THotKey = class(TCustomControl)
  private
    FHotkey: TShortCut;
    FModifiers: THKModifiers;
    FLastPressed: TShortCut;
    FNoModifier: Boolean;
    FOnChange: TNotifyEvent;

    FBackgroundColor: TColor;
    FBorderColor: TColor;  
    FTextColor: TColor;
    FAcceptsInput: Boolean;

    function GetCharFromVirtualKey(Key: Word): String;
  protected
{    procedure CreateParams(var Params: TCreateParams); override;}
    procedure DoEnter; override;
    procedure DoExit; override;
{    class function GetControlClassDefaultSize: TSize; override;}
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    property LastPressed: TShortcut read FLastPressed write FLastPressed;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure EditingDone; override;
  published        
    property Align;
    property Anchors;
    property Font;
    property BorderStyle stored false; //Has no effect
    property Cursor stored false; //Has no effect
    property Left;
    property Top;
    property Width default 80;
    property Height default 27;
    property TabOrder;
    property TabStop stored false default true;
    property AutoSize;
    property Hotkey: TShortcut read FHotkey write FHotkey;
    property Modifiers: THKModifiers read FModifiers write FModifiers;
    property NoModifier: Boolean read FNoModifier write FNoModifier;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ASuite Components',[THotKey]);
end;

constructor THotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csRequiresKeyboardInput];
  BorderStyle := bsNone;
  FCursor := crIBeam;
  FNoModifier := False;
  FBackgroundColor := clWindow;
  FTextColor := clCaptionText;
  FBorderColor := clBtnShadow;
  FAcceptsInput := False;
  {FAutoSelect := True;
  FAutoSelected := False;
  FTextChangedByRealSetText := False;
  FTextChangedLock := False;
  AutoSize := True;
  // Accessibility
  AccessibleRole := larTextEditorSingleline;
  FTextHint := '';}
end;

{procedure THotKey.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;}

procedure THotKey.DoEnter;
var
  Point: TPoint;
  CharIndex: Integer;
  Pt: Integer;
  res: LResult;
begin
  inherited;
  DebugLn('THotKey.DoEnter');
  //CreateCaret(Handle, 0, 1, 16);
  //GetCaretPos(Point);
  //DebugLn(', X: ' + IntToStr(Point.x));
  //SetCaretPos(1, Point.y);
  //ShowCaret(Handle);

  FBackgroundColor := clWindow;
  FBorderColor := clHighlight;

  FTextColor := clHighlight;
  FAcceptsInput := True;
  Invalidate;
end;

procedure THotKey.DoExit;
begin
  DebugLn('THotKey.DoExit');
  //HideCaret(Handle);
  {DestroyCaret(Handle);}
  FBackgroundColor := clWindow;
  FBorderColor := clBtnShadow;  
  FTextColor := clCaptionText;
  FAcceptsInput := False;
  inherited;
end;

procedure THotKey.MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
begin
  inherited;
  
  if Focused then
    begin           
      FTextColor := clHighlight;
      FAcceptsInput := True;
      Refresh;
    end;

  if CanSetFocus then begin
    SetFocus;
  end;
end;

procedure THotKey.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  DebugLn('THotKey.KeyDown ' + IntToStr(Key));
end;

procedure THotKey.KeyUp(var Key: Word; Shift: TShiftState);
var
  filteredShiftState: TShiftState;
  newShortCut: TShortCut;
begin
  inherited;
  DebugLn('THotKey.KeyUp ' + IntToStr(Key));

  if FAcceptsInput then
    begin
      filteredShiftState := Shift * [ssShift, ssAlt, ssCtrl, ssMeta];
      if ((Key = 8) or (Key = 46)) and (filteredShiftState = []) then
      begin
        Hotkey := 0;
      end else
      begin
        if not(FNoModifier) and (filteredShiftState = []) then
          filteredShiftState := [ssCtrl];

        newShortCut := ShortCut(Key, filteredShiftState);
        if (ShortCutToText(newShortCut) <> '') then
          Hotkey := newShortCut;

        if Assigned(FOnChange) then
          FOnChange(Self);
      end;
                                
      FTextColor := clCaptionText;
      FAcceptsInput := False;
    end;

  Invalidate;
end;

procedure THotKey.Paint;
const
  BorderWidth = 1;
  LeftRightSpacing = 5;
var
  txt: string;
  textBox: TRect;
begin
  Inherited;

  //Draw Background
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.Font.Assign(Self.Font);
  Canvas.Pen.Color := clRed;

  //Draw Border Rectangle
  Canvas.Pen.Color := FBorderColor;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(ClientRect);

  //Canvas.Brush.Assign(Self.Brush);  No Default Brush!
  if Hotkey <> 0 then txt := ShortCutToText(Hotkey) else txt := 'None';

  //Draw Text
  Canvas.Font.Color := FTextColor;
  textBox := TRect.Create(BorderWidth+LeftRightSpacing,BorderWidth,ClientRect.Width-(BorderWidth+LeftRightSpacing)*2, ClientRect.Height-2*BorderWidth);
  Canvas.TextRect(textBox, textBox.Left, ((textBox.Height) div 2 - Canvas.TextHeight(txt) div 2)+1, txt);
end;

procedure THotKey.EditingDone;
begin
  DebugLn('THotKey.EditingDone');
  Invalidate;
  inherited;
end;

function THotKey.GetCharFromVirtualKey(Key: Word): String;
var
  keyboardState: TKeyboardState;
  asciiResult: Integer;
  Shortcut: TShortCut;
begin
  Result := '';
  if Key <> 0 then
  begin
    Shortcut := KeyToShortCut(Key, []);
    Result := ShortCutToText(Shortcut);
  end;
end;

end.