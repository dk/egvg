{-cp -v}
{$F+}
Library TxtGdi;

Uses Txt, Gdi, EgFont, Truetype, Drivers, FntLib;

Function InitTGF(Width : Byte) : Boolean; export;
var
  P : PScreenDriver;
Begin
  FadeTxt;
  case Width of
  4  : P := GetAppropriateDriver(32, 1024, 2, True);
  8  : P := GetAppropriateDriver(64, 512, 2, True);
  16 : P := GetAppropriateDriver(128, 256, 2, True);
  32 : P := GetAppropriateDriver(256, 128, 2, True);
  64 : P := GetAppropriateDriver(512, 64, 2, True);
  128 : P := GetAppropriateDriver(1024, 32, 2, True);
  else end;
  SelectDriver(P);
  InitTGF := InitGDI;
  DoneEvents;
End;

Procedure DoneTGF; export;
Begin
  DoneGDI;
End;

Procedure FadeScreen; export;
Begin
  FadeTxt;
End;

Function LoadFont(S : String) : Integer; export;
Var
  I : Integer;
  F, F1 : PAbstractFont;
Begin
  F1 := TrueType.LoadFont(S);
  if F1 = Nil then begin
    LoadFont := -1;
    Exit;
  end;
  F := GetFontByName(F1^.GetName, True);
  if F <> Nil then DisposeFont(GetFontID(F));
  I := GetFreeFontID;
  RegisterFont(F1, I);
  LoadFont := I;
End;


Function  GetFontName(I : Integer) : String; export;
Begin
  if I = 0 then I := 1;
  GetFontName := GetFont(I)^.GetName;
End;

Function  GetFontFlags(I : Integer) : Word; export;
Begin
  if I = 0 then I := 1;
  GetFontFlags := GetFont(I)^.Flags;
End;

Function  GetFontCaps(I : Integer) : Word; export;
Begin
  if I = 0 then I := 1;
  GetFontCaps := GetFont(I)^.CapsNumber;
End;

Function  GetFontCapHeight(I, CapNo : Integer) : Word; export;
Begin
  if I = 0 then I := 1;
  GetFontCapHeight:= GetFont(I)^.CapsHeight(CapNo);
End;

Function FontExists( I : Integer) : Boolean; export;
Begin
  if I = 0 then I := 1;
  FontExists := (I = 1) or (GetFont(I) <> GetFont(1));
End;

Procedure DisposeFont(I : Integer); export;
Begin
  if I = 0 then I := 1;
  egFont.DisposeFont(I);
End;

Procedure SelectFont(var F : TFont); export;
begin
  SelectFontCaps(F);
End;

procedure TextOut(S : String; X, Y : Integer); export;
var
  t:TPaintInfo;
begin
  DefaultPaint(t);
  t.fore := 0;
  BarStyle(0,0,maximalx,maximaly,T);
  t.fore := 1;
  WrStr(x,y,S,t);
  PutBufferPart(0,0,0,0);
end;


Exports
  InitTGF,
  DoneTGF,
  LoadFont,
  GetFontName,
  GetFontFlags,
  GetFontCaps,
  GetFontCapHeight,
  FontExists,
  FadeScreen,
  SelectFont,
  TextOut,
  DisposeFont;

Begin
  RegisterFont(New(PScaledSimpleFont, Init((@DefaultSystemFont)^, 'System')), 1);
  RegisterFont(New(PScaledSimpleFont, Init((@Courier)^, 'Courier')), 2);
  RegisterFont(New(PScaledSimpleFont, Init((@Small)^, 'Small')), 3);
  RegisterFont(New(PScaledSimpleFont, Init((@Thick)^, 'Thick')), 4);
  RegisterFont(New(PScaledSimpleFont, Init((@Large)^, 'Large')), 5);
End.