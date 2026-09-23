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

{$I ASuiteComps.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus, {$IFDEF Windows}Windows,{$ENDIF} LMessages, LCLIntf, LCLType, LCLProc
  {$IFDEF INDEBUG}, LazLogger{$ENDIF};

resourcestring
  { Shown by THotKey when no shortcut is set. }
  SHotKeyNoneText = 'None';

type
  { Non-visual shortcut capture engine. It turns a key + shift state into a
    TShortCut using the same rules as THotKey:
      - Backspace/Delete without modifiers clear the shortcut;
      - a bare key gets Ctrl unless NoModifier is set.
    Keeping this logic out of the control makes it reusable and testable. }
  TShortcutCapture = class
  private
    FHotkey: TShortCut;
    FNoModifier: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetHotkey(AValue: TShortCut);
  public
    constructor Create;

    { True when the key clears the shortcut (Backspace/Delete, no modifiers). }
    function IsClearKey(AKey: Word; AShift: TShiftState): Boolean;
    { Builds the shortcut for a key press, applying NoModifier. }
    function MakeShortcut(AKey: Word; AShift: TShiftState): TShortCut;
    { Captures a key press, updating Hotkey and firing OnChange. }
    procedure Capture(AKey: Word; AShift: TShiftState);

    property Hotkey: TShortCut read FHotkey write SetHotkey;
    property NoModifier: Boolean read FNoModifier write FNoModifier;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { Low-level shortcut capture control: the shortcut is always typed directly
    into the control. To pick a shortcut through the modal dialog use
    THotKeyEdit, which wraps TfrmShortcutGrabber. }
  THotKey = class(TCustomControl)
  private
    FCapture: TShortcutCapture;

    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FTextColor: TColor;
    FAcceptsInput: Boolean;
    FOnChange: TNotifyEvent;

    function GetHotkey: TShortCut;
    procedure SetHotkey(AValue: TShortCut);
    function GetNoModifier: Boolean;
    procedure SetNoModifier(AValue: Boolean);
    procedure DoShortcutChange(Sender: TObject);
  protected
{    procedure CreateParams(var Params: TCreateParams); override;}
    procedure DoEnter; override;
    procedure DoExit; override;
{    class function GetControlClassDefaultSize: TSize; override;}
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property Hotkey: TShortcut read GetHotkey write SetHotkey;
    property NoModifier: Boolean read GetNoModifier write SetNoModifier;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ASuite Components',[THotKey]);
end;

{ TShortcutCapture }

constructor TShortcutCapture.Create;
begin
  inherited Create;
  FHotkey := 0;
  FNoModifier := False;
end;

procedure TShortcutCapture.SetHotkey(AValue: TShortCut);
begin
  if FHotkey = AValue then
    Exit;

  FHotkey := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TShortcutCapture.IsClearKey(AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := ((AKey = VK_BACK) or (AKey = VK_DELETE)) and
    ((AShift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper]) = []);
end;

function TShortcutCapture.MakeShortcut(AKey: Word; AShift: TShiftState): TShortCut;
var
  Filtered: TShiftState;
begin
  Filtered := AShift * [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper];
  // LCL TShortCut cannot represent ssSuper (the "Super"/"Windows" key is
  // encoded as ssMeta), so fold it to avoid losing the modifier.
  if ssSuper in Filtered then
  begin
    Exclude(Filtered, ssSuper);
    Include(Filtered, ssMeta);
  end;
  if (not FNoModifier) and (Filtered = []) then
    Filtered := [ssCtrl];

  Result := ShortCut(AKey, Filtered);
end;

procedure TShortcutCapture.Capture(AKey: Word; AShift: TShiftState);
var
  NewShortcut: TShortCut;
begin
  if IsClearKey(AKey, AShift) then
    Hotkey := 0
  else
  begin
    NewShortcut := MakeShortcut(AKey, AShift);
    if ShortCutToText(NewShortcut) <> '' then
      Hotkey := NewShortcut;
  end;
end;

{ THotKey }

constructor THotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csRequiresKeyboardInput];
  BorderStyle := bsNone;
  FCursor := crIBeam;
  FBackgroundColor := clWindow;
  FTextColor := clCaptionText;
  FBorderColor := clBtnShadow;
  FAcceptsInput := False;
  // A hotkey is edited by typing, so expose it as a single-line text editor.
  AccessibleRole := larTextEditorSingleline;

  FCapture := TShortcutCapture.Create;
  FCapture.OnChange := DoShortcutChange;

  {FAutoSelect := True;
  FAutoSelected := False;
  FTextChangedByRealSetText := False;
  FTextChangedLock := False;
  AutoSize := True;
  // Accessibility
  AccessibleRole := larTextEditorSingleline;
  FTextHint := '';}
end;

destructor THotKey.Destroy;
begin
  FCapture.Free;
  inherited Destroy;
end;

function THotKey.GetHotkey: TShortCut;
begin
  Result := FCapture.Hotkey;
end;

procedure THotKey.SetHotkey(AValue: TShortCut);
begin
  FCapture.Hotkey := AValue;
end;

function THotKey.GetNoModifier: Boolean;
begin
  Result := FCapture.NoModifier;
end;

procedure THotKey.SetNoModifier(AValue: Boolean);
begin
  FCapture.NoModifier := AValue;
end;

procedure THotKey.DoShortcutChange(Sender: TObject);
begin
  Invalidate;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{procedure THotKey.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;}

procedure THotKey.DoEnter;
begin
  inherited;
  {$IFDEF INDEBUG}
  DebugLn('THotKey.DoEnter');
  {$ENDIF}
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
  {$IFDEF INDEBUG}
  DebugLn('THotKey.DoExit');
  {$ENDIF}
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
  {$IFDEF INDEBUG}
  DebugLn('THotKey.KeyDown ' + IntToStr(Key));
  {$ENDIF}
end;

procedure THotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  {$IFDEF INDEBUG}
  DebugLn('THotKey.KeyUp ' + IntToStr(Key));
  {$ENDIF}

  if FAcceptsInput then
    begin
      FCapture.Capture(Key, Shift);

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

  //Draw Border Rectangle (also fills the client area with the background)
  Canvas.Pen.Color := FBorderColor;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(ClientRect);

  //Canvas.Brush.Assign(Self.Brush);  No Default Brush!
  if Hotkey <> 0 then txt := ShortCutToText(Hotkey) else txt := SHotKeyNoneText;

  //Draw Text
  Canvas.Font.Color := FTextColor;
  textBox := TRect.Create(BorderWidth+LeftRightSpacing,BorderWidth,ClientRect.Width-(BorderWidth+LeftRightSpacing)*2, ClientRect.Height-2*BorderWidth);
  Canvas.TextRect(textBox, textBox.Left, ((textBox.Height) div 2 - Canvas.TextHeight(txt) div 2)+1, txt);
end;

procedure THotKey.EditingDone;
begin
  {$IFDEF INDEBUG}
  DebugLn('THotKey.EditingDone');
  {$ENDIF}
  Invalidate;
  inherited;
end;

end.
