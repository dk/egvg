{$DEFINE GRAPHICS}
{$DEFINE RUSSIAN}
{$Q-}

unit Gadgets;

interface

uses Dos, Objects, Views, App, Dialogs, Drivers, GDI, Bitmaps, EgInline;

const
  cmUpdateHeapView     = $890;
  cmUpdateClockView    = $891;
  cmUpdateKBDView      = $892;
  cmRadioButtonPressed = $893;

type
  PHeapView = ^THeapView;
  THeapView = object(TTimedView)
    OldMem : LongInt;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure Update; virtual;
  end;

  PClockView = ^TClockView;
  TClockView = object(TTimedView)
    Refresh: Byte;
    LastTime: DateTime;
    TimeStr: string[10];
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function FormatTimeStr(H, M, S: Word): String; virtual;
    procedure Update; virtual;
  end;

{$IFDEF RUSSIAN}
  PKeyboardView = ^TKeyboardView;
  TKeyboardView = object(TTimedView)
    OldMode : Boolean;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure Update; virtual;
  end;
{$ENDIF}

  PComboBox = ^TComboBox;
  TComboBox = Object(TCluster)
    Down : Boolean;
    Constructor Init(var Bounds: TRect; AStrings: PSItem);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Press(Item : Integer); Virtual;
    Procedure   Draw; Virtual;
    Function    GetPalette : PPalette; Virtual;
  End;

{$IFDEF GRAPHICS}
  function PlaceInMenuBar(View: PView; RightIndent: Integer): PView;
  function PlaceInStatusLine(View: PView; RightIndent: Integer): PView;
{$ENDIF}

implementation


{------ Heap Window object ----------}

constructor THeapView.Init(var Bounds: TRect);
begin
  inherited Init(Bounds, cmUpdateHeapView, 9); { ~ раз в полсекунды }
  OldMem := 0;
end;

procedure THeapView.Draw;
var
  S: String;
  {$IFNDEF GRAPHICS}
  B: TDrawBuffer;
  {$ELSE}
  CShadow: word;
  CBack: byte;
  H: Byte;
  {$ENDIF}
  C: Byte;
begin
  C := GetColor(2);
  {$IFDEF GRAPHICS}
  CShadow:= GetColor($0C0D);
  CBack := GetColor($03);
  SetFont(Font);
  H:= (Size.Y - FontHeight) div 2;
  Bar{Pattern}(0, 1, Size.X, Pred(Size.Y), CBack);
  HLine(0, 0, Size.X, Hi(CShadow));
  HLine(0, Pred(Size.Y), Size.X, Lo(CShadow));
{  Rectangle(0, 2, Size.X, Size.Y - 2, 1, Attr2Back(CShadow), Attr2Fore(CShadow));}
  Str(OldMem:(Size.X - 4) div CharWidth, S);
  WrStr(0, H, S, C);
  RestoreFont;
  {$ELSE}
  Str(OldMem:Size.X, S);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, S, C);
  WriteLine(0, 0, Size.X, 1, B);
  {$ENDIF}
end;

procedure THeapView.Update;
begin
  if (OldMem <> MemAvail) then
  begin
    OldMem:= MemAvail;
    DrawView;
  end;
end;

{-------- ClockView Object --------}

function LeadingZero(w: Word): String;
var s: String;
begin
  Str(w:0, s);
  LeadingZero := Copy('00', 1, 2 - Length(s)) + s;
end;

constructor TClockView.Init(var Bounds: TRect);
begin
  inherited Init(Bounds, cmUpdateClockView, 8);
  FillChar(LastTime, SizeOf(LastTime), #$00{FF});
  TimeStr := '';
  Refresh := 1;
end;

procedure TClockView.Draw;
var
  {$IFNDEF GRAPHICS}
  B: TDrawBuffer;
  {$ELSE}
  CShadow: word;
  H: Byte;
  CBack: byte;
  {$ENDIF}
  C: Byte;
begin
  C := GetColor(2);
  {$IFDEF GRAPHICS}
  CShadow:= GetColor($0C0D);
  CBack := GetColor($03);
  SetFont(Font);
  H:= (Size.Y - FontHeight) div 2;
  Bar{Pattern}(0, 1, Size.X, Pred(Size.Y), CBack);
  HLine(0, 0, Size.X, Hi(CShadow));
  HLine(0, Pred(Size.Y), Size.X, Lo(CShadow));
  WrStr(0, H, TimeStr, C);
  RestoreFont;
  {$ELSE}
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, TimeStr, C);
  WriteLine(0, 0, Size.X, 1, B);
  {$ENDIF}
end;

procedure TClockView.Update;
var
  h,m,s,hund: word;
begin
  GetTime(h,m,s,hund);
  if Abs(s - LastTime.sec) >= Refresh then
  begin
    with LastTime do
    begin
      Hour := h;
      Min := m;
      Sec := s;
    end;
    TimeStr := FormatTimeStr(h, m, s);
    DrawView;
  end;
end;

function TClockView.FormatTimeStr(H, M, S: Word): String;
begin
  FormatTimeStr := LeadingZero(h)+ ':'+ LeadingZero(m) +
    ':' + LeadingZero(s);
end;


{------ Keyboard State View object ----------}
{$IFDEF RUSSIAN}
constructor TKeyboardView.Init(var Bounds: TRect);
begin
  inherited Init(Bounds, cmUpdateKbdView, 4); { ~ 4 раза в секунду }
  OldMode:= AltKbdMode;
end;

procedure TKeyboardView.Draw;
var
  S: String;
  {$IFNDEF GRAPHICS}
  B: TDrawBuffer;
  {$ELSE}
  CShadow: Word;
  CBack: byte;
  H: Byte;
  {$ENDIF}
  C: Byte;
begin
  OldMode:= AltKbdMode;
  C := GetColor(2);
  if AltKbdMode then S:= ' �����' else S:= 'England';
  {$IFDEF GRAPHICS}
  CShadow:= GetColor($0C0D);
  CBack := GetColor($03);
  SetFont(Font);
  H:= (Size.Y - FontHeight) div 2;
  Bar{Pattern}(0, 1, Size.X, Pred(Size.Y), CBack);
  HLine(0, 0, Size.X, Hi(CShadow));
  HLine(0, Pred(Size.Y), Size.X, Lo(CShadow));
  WrStr(0, H, S, C);
  RestoreFont;
  {$ELSE}
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, S, C);
  WriteLine(0, 0, Size.X, 1, B);
  {$ENDIF}
end;

procedure TKeyboardView.Update;
begin
  if OldMode <> AltKbdMode then
    DrawView;
end;
{$ENDIF}

{$IFDEF GRAPHICS}
function PlaceInMenuBar(View: PView; RightIndent: Integer): PView;
begin
  PlaceInMenuBar:= View;
  if (MenuBar = Nil) or (View = Nil) then Exit;
  with View^ do
  begin
    GrowMode := gfGrowHiX or gfGrowLoX;
    Origin.X:= MenuBar^.Origin.X + MenuBar^.Size.X - Size.X - RightIndent;
    Origin.Y:= MenuBar^.Origin.Y+1; Size.Y:= MenuBar^.Size.Y-2;
                             {GIO^}                      {GIO^}
  end;
end;

function PlaceInStatusLine(View: PView; RightIndent: Integer): PView;
begin
  PlaceInStatusLine:= View;
  if (StatusLine = Nil) or (View = Nil) then Exit;
  with View^ do
  begin
    GrowMode := gfGrowAll;
    Origin.X:= StatusLine^.Origin.X + StatusLine^.Size.X - Size.X - RightIndent;
    Origin.Y:= StatusLine^.Origin.Y{ + (StatusLine^.Size.Y - Size.Y) div 2};
    Size.Y:= StatusLine^.Size.Y;
  end;
end;
{$ENDIF}

Type
  PXListBox = ^TXListBox;
  TXListBox = Object(TListBox)
    Mask : LongInt;
    Constructor Init(var R : TRect; AHScrollBar : PScrollBar; AMask : LongInt);
    Procedure HandleEvent(var Event : TEvent); Virtual;
    Function  IsSelected(Item : Integer) : Boolean; Virtual;
    function  GetPalette: PPalette; Virtual;
  End;

Constructor TXListBox.Init;
Begin
  Inherited Init(R, 1, AHScrollBar);
  Mask := AMask;
End;

Procedure TXListBox.HandleEvent(var Event : TEvent);
Begin
  if ((Event.What = evMouseDown) and (Event.Double))
  or ((Event.What = evKeyDown) and ((Event.KeyCode = kbEnter) or
  (Event.CharCode = ' '))) then begin
    if not IsSelected(Focused) then begin
      Event.What := evCommand;
      Event.Command := cmOK;
      PutEvent(Event);
    end;
    ClearEvent(Event);
  end else Inherited HandleEvent(Event);
End;


Function  TXListBox.IsSelected(Item : Integer) : Boolean;
Begin
  IsSelected := (Mask and (1 shl Item)) = 0;
End;

function TXListBox.GetPalette: PPalette;
const
  P: String[Length(CListViewer)] = CListViewer;
begin
  P[3] := #21;
  GetPalette := @P;
end;


Constructor TComboBox.Init;
Begin
  Inherited Init(Bounds, AStrings);
  SetCursor(0, 0);
  Down := False;
End;

Procedure TComboBox.HandleEvent(var Event : TEvent);
Var
  ClickRect : TRect;
  Mouse: TPoint;
Begin
  TView.HandleEvent(Event);
  if (Options and ofSelectable) = 0 then Exit;
  if (State and sfDisabled) <> 0 then Exit;
  case Event.What of
  evMouseDown : begin
    GetExtent(ClickRect);
    Inc(ClickRect.A.X);
    Dec(ClickRect.B.Y);
    Down := False;
    repeat
      MakeLocal(Event.Where, Mouse);
      if Down <> ClickRect.Contains(Mouse) then
      begin
        Down := not Down;
        DrawView;
      end;
    until not MouseEvent(Event, evMouseMove);
    if Down then begin
      Press(0);
      DrawView;
    end;
    ClearEvent(Event);
  end;
  evKeyDown : if State and sfFocused <> 0 then
    if (Event.KeyCode = kbDown) or (Event.CharCode = ' ') then begin
      ClearEvent(Event);
      Press(0);
      DrawView;
    end;
  else end;
End;

Procedure   TComboBox.Draw;
Var
  LC, BW: Word;
  P : PImage;
Begin
  if (State and sfDisabled) = 0 then LC := GetColor($0109)
    else LC := GetColor($0809);
  if Down then P := GetImage(11) else P := GetImage(10);
  BW := Size.X - BitmapWidth(P);
  Bar(0, 0, Size.X, Size.Y, Lo(LC));
  if Down then begin
    Bar(1, 1, BW - 1, Size.Y - 1, GetColor($02));
    with PaintInfo do begin
      LineStyle := lsLinePattern;
      LinePattern := psDot;
      Operation := CopyPut;
      LineWidth := 1;
      Back      := ColorIndex^[GetColor($02)];
    end;
    FrameStyle(1, 1, BW - 2, Size.Y - 2);
    DefaultPaint(PaintInfo);
  end;
  if (Strings.Count > 0) and (Value <= Strings.Count) then
    WrStr(2, 2, String(Strings.At(Value)^), Hi(LC));
  if (State and sfDisabled) = 0 then PutBMP(P, BW, 0);
End;

function TComboBox.GetPalette: PPalette;
const
  P: String[Length(CListViewer+CHistory)] = CListViewer+CHistory;
begin
  GetPalette := @P;
end;

Procedure TComboBox.Press;
Type
  TListBoxRec = record
    List: PCollection;
    Selection: Word;
  end;
Var
  P : PDialog;
  C : PView;
  R : TRect;
  T : TListBoxRec;
  H : PScrollBar;
  O : TPoint;
  X : TRect;
  D : Word;
  CW, CH : Integer;
Begin
  if Strings.Count = 0 then begin
    Down := false;
    DrawView;
    Exit;
  end;

  CW := CharWidth;
  CH := FontHeight;
  Owner^.MakeGlobal(Origin, R.A);
  R.B.X := R.A.X + Size.X;
  R.B.Y := R.A.Y + 6 * CH;
  Owner^.GetExtent(X);
  Owner^.MakeGlobal(X.A, X.A);
  Owner^.MakeGlobal(X.B, X.B);
  D := R.B.Y - R.A.Y - 2;
  R.Intersect(X);
  R.B.Y := R.A.Y + MaxInteger(1, MinInteger(8, Strings.Count)) * CH + 4;
  D := R.B.Y - R.A.Y - 2;
  if R.B.Y > ScreenHeight - CH then R.Move(0, -D-2*CH);
  New(P, Init(R, ''));
  P^.Flags := wfClose;
  R.Assign(Size.X - BitmapWidth(@ScrollDownDef) - 2, 1, Size.X - 2, D);
  New(H, Init(R));
  P^.Insert(H);
  R.Assign(1, 1, Size.X - BitmapWidth(@ScrollDownDef) - 2, D);
  P^.Insert(New(PXListBox, Init(R, H, EnableMask)));
  T.List      := @Strings;
  if (Value < 0) or (Value > Strings.Count) then T.Selection := 0 else
  T.Selection := Value;
  O := MinWinSize;
  MinWinSize.X := 5 * CW;
  D := Application^.ExecuteDialog(P, @T);
  MinWinSize := O;
  if D = cmOK then begin
    Value := T.Selection;
    Message(Owner, evBroadCast, cmRadioButtonPressed, @Self);
  end;
  Down := False;
  DrawView;
End;


end.
