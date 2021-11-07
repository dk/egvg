Unit EGCursor;
{******************************************************************
 *                                                                *
 *       ЭМУЛЯЦИЯ ТЕКСТОВОГО КУРСОРА В ГРАФИЧЕСКОМ РЕЖИМЕ         *
 *                                                                *
 *  Имя файла:                 EGCursor.PAS                       *
 *  Автор:                     Tony Berezin                       *
 *  Дата начала разработки:    05-JAN-1994                        *
 *  Дата окончания разработки: 05-JAN-1994                        *
 *                                                                *
 ******************************************************************}

interface uses Objects,  GDI;  { Стандартные объекты Borland Pascal }

const
  MaxTextCursorWidth = 16;
  MaxTextCursorLines = 16;

type
  TextCursorWidth = 1..MaxTextCursorWidth;
  TextCursorLines = 1..MaxTextCursorLines;

type
  TTextCursor = object(TObject)
   public
    constructor Init;
    procedure SetWidth(w: TextCursorWidth);
    procedure SetHeight(h: byte);  { для переназначения координат }
    procedure SetStdIns(l: TextCursorLines);
    procedure SetStdOvr(l: TextCursorLines);
    procedure SetNonStd(l: TextCursorLines);
    procedure SetBlinkRate(VisibleTicks, HiddenTicks: byte);
    procedure SetColor(c: Integer);
    procedure Hide;
    procedure Show;
    function  IsShown: boolean;
    procedure GotoXY(x, y: Integer);
    function  GetY: integer;
    function  GetX: integer;
    procedure GetCursorRect(var R: TRect);
    procedure NonStdShape;
    procedure Shape(Ins: boolean);
    procedure Blink;
   private
    function GetTime: longint;
   private
    LastTime: longint;
    Width: TextCursorWidth;
    Height: byte;
    InsLines: TextCursorLines;
    OvrLines: TextCursorLines;
    NonStdLines: TextCursorLines;
    Mode: byte;
    Clr: integer;
    Shown: boolean;
    Blinked: boolean;
    RateVis, RateHid: byte;
    xxx, yyy, px, py: integer;
    Image: array[1..word(MaxTextCursorWidth)*word(MaxTextCursorLines)+8] of byte;
  end;

var
  TextCursor: TTextCursor;

implementation


function TTextCursor.GetTime: longint; assembler;
asm
  xor    ah, ah
  int    1Ah
  mov    ax, dx
  mov    dx, cx
end;

constructor TTextCursor.Init;
begin
  inherited Init;
  SetWidth(8);
  SetHeight(16);
  SetStdIns(2); SetStdOvr(16{8}); SetNonStd(1);
  SetBlinkRate(2,2);
  SetColor(15);
  Shape(True);
end;

function TTextCursor.IsShown: boolean;
begin
  IsShown := Shown;
end;

procedure TTextCursor.SetWidth(w: TextCursorWidth);
begin
  if w = Width then Exit;
  if w>MaxTextCursorWidth then w := MaxTextCursorWidth;
  if Shown then begin
    Hide;
    Width := w;
    Show;
  end else Width := w;
end;

procedure TTextCursor.SetHeight(h: byte);  { для переназначения координат }
begin
  if h = Height then Exit;
  if Shown then begin
    Hide;
    Height := h;
    Show;
  end else Height := h;
end;

procedure TTextCursor.SetStdIns(l: TextCursorLines);
begin
  if l>MaxTextCursorLines then l := MaxTextCursorLines;
  if Shown then begin
    Hide;
    InsLines := l;
    Show;
  end else InsLines := l;
end;

procedure TTextCursor.SetStdOvr(l: TextCursorLines);
begin
  if l>MaxTextCursorLines then l := MaxTextCursorLines;
  if Shown then begin
    Hide;
    OvrLines := l;
    Show;
  end else OvrLines := l;
end;

procedure TTextCursor.SetNonStd(l: TextCursorLines);
begin
  if l>MaxTextCursorLines then l := MaxTextCursorLines;
  if Shown then begin
    Hide;
    NonStdLines := l;
    Show;
  end else NonStdLines := l;
end;

procedure TTextCursor.SetBlinkRate(VisibleTicks, HiddenTicks: byte);
begin
  RateVis := VisibleTicks;
  RateHid := HiddenTicks;
end;

procedure TTextCursor.SetColor(c: integer);
begin
  if Shown then begin
    Hide;
    Clr := c;
    Show;
  end else Clr := c;
end;

procedure TTextCursor.Hide;
var
  yy: integer;
begin
  if Shown then begin
    Shown := False;
    if Mode=0 then yy:=InsLines
    else if Mode=1 then yy:=OvrLines else yy:=NonStdLines;
    DirectPutImage(PCoordImage(@Image), px, py);
  end;
end;

procedure TTextCursor.GetCursorRect(var R: TRect);
var
  yy: integer;
begin
  if Mode=0 then yy:=InsLines
  else if Mode=1 then yy:=OvrLines else yy:=NonStdLines;
  R.Assign(px, py, px+Width, py+yy);
end;

procedure TTextCursor.Show;
var
  yy: integer;
begin
  Blinked := False;
  if Mode=0 then yy:=InsLines
  else if Mode=1 then yy:=OvrLines else yy:=NonStdLines;
  Hide;
  px := xxx;
  py := yyy+Height-yy;
  if (px<0) or (py<0) or (px+Width-1>640) or (py+yy-1>480) then begin
    Exit;
  end;
  DirectGetImage(px, py, px+Width-1, py+yy-1, Image);
  while yy>0 do begin
    DirectHLine(px, py+yy-1, px+Width-1, ColorIndex^[Clr]);
    dec(yy);
  end;
  LastTime := GetTime;
  Shown := True;
end;

function TTextCursor.GetY: integer; begin GetY := yyy end;
function TTextCursor.GetX: integer; begin GetX := xxx end;

procedure TTextCursor.GotoXY(x, y: integer);
var
  yy: integer;
begin
  if (xxx=x) and (yyy=y) then Exit;
  xxx := x; yyy := y;
  if Mode=0 then yy:=InsLines
  else if Mode=1 then yy:=OvrLines else yy:=NonStdLines;
  if Shown then begin
    Hide;
    px := xxx;
    py := yyy+Height-yy;
    Show;
  end else begin
    px := xxx;
    py := yyy+Height-yy;
  end;
end;

procedure TTextCursor.NonStdShape;
var
  yy: integer;
begin
  Mode:=2;
  if Shown then begin
    Hide;
    GotoXY(xxx, yyy);
    Show;
  end else GotoXY(xxx, yyy);
end;

procedure TTextCursor.Shape(Ins: boolean);
var
  yy: integer;
begin
  if Ins then Mode:=0 else Mode:=1;
  if Shown then begin
    Hide;
    GotoXY(xxx, yyy);
    Show;
  end else GotoXY(xxx, yyy);
end;

procedure TTextCursor.Blink;
var
  yy: integer;
  Tim: longint;
begin
  if not Shown then Exit;
  Tim:=GetTime;
  if Mode=0 then yy:=InsLines
  else if Mode=1 then yy:=OvrLines else yy:=NonStdLines;
  if Blinked then begin
    if Tim-LastTime < longint(RateHid) then Exit;
    while yy>0 do begin
      DirectHLine(px, py+yy-1, px+Width-1, ColorIndex^[Clr]);
      dec(yy);
    end;
    LastTime := Tim;
    Blinked := False;
  end else begin
    if Tim-LastTime < longint(RateVis) then Exit;
    DirectPutImage(PCoordImage(@Image), px, py);
    LastTime := Tim;
    Blinked := True;
  end;
end;

begin
  TextCursor.Init;
end.
