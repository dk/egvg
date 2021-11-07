{-cd}
{$S-}
Uses Mono, egvg, svga256, svga32k, svga64k, svga16m, GDI,
     Drivers, Objects, Bitmaps, Palettes, EGFont;

Procedure RunLines;
Var
  O, S, OP, SP : TPoint;
  I : Word;
  T : TPaintInfo;
Begin
  SP.X := 639; SP.Y := 479; OP := SP;
  DefaultPaint(T);
  for i := 0 to 499 do begin
    T.Fore := ColorIndex^[Random(256)];
    O.X := Random(OP.X);  O.Y := Random(OP.Y);
    S.X := Random(SP.X);  S.Y := Random(SP.Y);
    Line(O.X,O.Y,S.X,S.Y, T);
  end;
End;

Var
  PMG : PImage;
  LP  : TLogPalette;

Procedure RunBars;
Var
  O, S, OP, SP : TPoint;
  I : Word;
  T : TPaintInfo;
Begin
  SP.X := 639; SP.Y := 479; OP := SP;
  DefaultPaint(T);
  for i := 0 to 99 do begin
    T.Fore := ColorIndex^[Random(256)];
    O.X := Random(OP.X);  O.Y := Random(OP.Y);
    S.X := Random(SP.X);  S.Y := Random(SP.Y);
    Bar(O.X,O.Y,S.X,S.Y,T.Fore);
  end;
End;


Procedure RunCircs;
Var
  O, S, OP, SP : TPoint;
  I : Word;
  T : TPaintInfo;
Begin
  SP.X := 799; SP.Y := 599; OP := SP;
  DefaultPaint(T);
  T.LineWidth := 20;
  for i := 0 to 99 do begin
    T.Fore := ColorIndex^[Random(256)];
    T.Back := ColorIndex^[Random(256)];
    O.X := Random(OP.X);  O.Y := Random(OP.Y);
    S.X := Random(100);
    FillCircle(O.X,O.Y,S.X, T);
  end;
End;

{$L _SCROLLS.OBJ}
procedure ScrollDownDef; external;

Procedure RunBMPs;
Var
  I,x,y : Word;
Begin
  for i := 0 to 999 do begin
    x := Random(639);
    y := Random(479);
    PutBMPPart(PImage(@ScrollDownDef), x, y, 0, 0, 30, 30);
  end;
End;

Procedure RunDBMPs;
Var
  I,x,y : Word;
Begin
  for i := 0 to 999 do begin
    x := Random(639);
    y := Random(479);
    PutBMPPartOp(PImage(@ScrollDownDef), x, y, 0, 0, 30, 30, Random(20), nil);
  end;
End;


Procedure RunSBMPs;
Var
  I,x,y,k,l : Word;
  T : TRect;
Begin
  T.Assign(0, 0, 639, 479);
  for i := 0 to 99 do begin
    x := Random(639);
    y := Random(479);
    k := Random(300)+1;
    l := Random(300)+1;
    StretchDIBitmap(PImage(@ScrollDownDef), nil, x, y, 0, 0, 30, 30, k,l,Nil, T);
  end;
End;

Procedure RunTexts;
Var
  I,x,y : Word;
  T     : TPaintInfo;
Begin
  DefaultPaint(T);
  T.ClipRect.Assign(0, 0, 639, 479);
  for i := 0 to 99 do begin
    T.Fore := ColorIndex^[Random(256)];
    x := Random(639);
    y := Random(479);
    WrStr(x,y,'The quick brown fox jumped over big lazy dog then sat on tack',T);
  end;
End;

Var
  T1 : longInt absolute $40:$6C;
  T2,dT : Longint;
  S     : String;
  I   : Word;
  Lines : LongInt;


function LeadingZero(w : Word) : String;
var
  s : String;
begin
  Str(w:0,s);
  if Length(s) = 1 then  s := '0' + s;
  LeadingZero := s;
end;


Procedure InitTime;
Begin
  T2 := T1;
End;

Function  RecTime : LongInt;
Begin
  RecTime := (T1 - T2) * 55 div 10;
End;

Function GetStr(dT : LongInt) : String;
Var
  Hour, Minute, Second, Hund : Word;
Begin
  Hund := dT  mod 100;
  Second := (dT div 100) mod 60;
  Minute := (dT div 6000) mod 60;
  Hour := 0;
  GetStr:=LeadingZero(Hour) +':'+LeadingZero(Minute)+':'+LeadingZero(Second)+'.'+LeadingZero(Hund);
End;

Procedure CD;
Begin
  bar(0, 0, 639, 479, 0);
End;


Const
  Drvs : array[0..5] of LongInt = (
   2, 16, 256, 32768, 65536, 16777216
  );

Var
  dBMP, dCirc, dLine, dSBM, DBBM, DText, DBars : LongInt;
  P : PSimpleFont;
  j:Word;
Begin
  TryBufferedStrategy := False;
  New(p, Init(@DefaultSystemFont^, ''));
  RegisterFont(p, 1);
  SelectFont(1);

  for I := 0 to 5 do begin
    SelectDriver(GetAppropriateDriver(640, 480, Drvs[I], False));
    if ScreenDriver^.NumberOfColors <> Drvs[I] then Continue;
    InitVideo;
    InitTime; RunBMPs;  dBMP := Rectime;  CD;
    InitTime; RunDBMPs; dBBM := Rectime;  CD;
    InitTime; RunSBMPs; dSBM := Rectime;  CD;
    InitTime; RunBars;  dBars:= Rectime;  CD;
    InitTime; RunCircs; dCirc := Rectime; CD;
    InitTime; RunLines; dLine := Rectime; CD;
    InitTime; RunTexts; dText := Rectime; CD;

    DoneVideo;
    WriteLn('Metrix for 640x480x', Drvs[I], ':');
    WriteLn('1000 BMPs           : ', GetStr(dBMP));
    WriteLn('1000 BitBlt BMPs    : ', GetStr(dBBM));
    WriteLn('100  Stretch BMPs   : ', GetStr(dSBM));
    WriteLn('100  Bars           : ', GetStr(dBars));
    WriteLn('100  Circles        : ', GetStr(dCirc));
    WriteLn('500  Lines          : ', GetStr(dLine));
    WriteLn('100  Texts          : ', GetStr(dText));
    asm
      xor ax, ax
      int 16h
      mov j, ax
    end;
    if j = kbEsc then Break;
  end;
End.