{$G+,S-,F+}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : VGA palettes handling                                █
  █ Description : Virtual Graphics 256-color virtual palettes kernel   █
  █ Author      : Dmitry Karasik                                       █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 20-JAN-1996                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█

}
unit Palettes;

Interface

Uses Objects, GDI;

Procedure RedrawPalette;

{local palette encodings}
Function  MergePalettes(First, Second : PVGAPalette; FirstColors, SecondColors : Word) : PVGAPalette;
Function  SqueezePalette(Pal : PVGAPalette; Colors : Word) : PVGAPalette;
Procedure RemapPalette(Pal : PVGAPalette; LP : PLogPalette);

{registering palettes operations}
function RegisterPalette(LP : PLogPalette; At : Integer) : Boolean;
function RecordPalette(LP : PLogPalette) : Integer;
function RetrievePalette(Index : Integer) : PLogPalette;
Procedure FreePalette(Index : Integer);

{primary palette fx}
Procedure FadeVGAPalette(Brightness : Byte; var Pal : TVGAPalette);
Procedure MajorVGAColor(var Pal : TVGAPalette; Tone : TVGARegister);
Procedure MinorVGAColor(var Pal : TVGAPalette; Tone : TVGARegister);
Procedure MaskVGAColor(var Pal : TVGAPalette; Mask : TVGARegister);
Procedure OptionalFade(var VPalette : TVGAPalette; DelayRate, Option : Byte);
Procedure UpgradePalette(Count : Byte; var P : array of Byte; Effect : Boolean);

{Optional fade constants}
Const
  fpBlack    = 0;
  fpNormUp   = 1;
  fpWhite    = 2;
  fpNormDown = 3;
  fpOff      = 4;
  fpNormal   = 5;
  fpPreserve = 6;

Const
  StdLogPalette : TLogPalette = (
    Mode     : 0;
    Colors   : 256;
    Palette  : @StdVGAPalette; {здесь 768 байт стандарт вга палитра}
    ColorRef : @StdColorRefMap {здесь 256 байт 0-255}
  );

Var
  PaletteUsers : TCollection;


Implementation

Uses Drivers, Views, App, Memory, EGInline;

Var
  PalBufferSize : Word;
  PalBuffer : PByteArray;
  ColorCnt  : Word;

Procedure RemoveDups(Toler : Byte);
Var
  I, J, K : Word;
  R       : TVGARegister;
  PV      : PVGARegister;
Begin
  {for I := 0 to (ColorCnt - 1) * 3 do if P^[I] > $3f then P^[I] := $3f;}
  for I := 0 to ColorCnt - 1 do begin
    R  := PVGARegister(@PalBuffer^[I shl 1 + I])^;
    if I < ColorCnt - 1 then begin
      K := I shl 1 + I + 3;
      PV := PVGARegister(@PalBuffer^[K]);
      for J := I + 1 to ColorCnt - 1 do begin
        if (J >= SystemColors) and
           ((Abs(PV^[1] - R[1]) + Abs(PV^[2] - R[2]) +
             Abs(PV^[3] - R[3])) <= Toler) then begin
          if ColorCnt = MaxColors then Exit;
          if J >= ColorCnt then Break;
          PalBuffer^[K] := PalBuffer^[ColorCnt shl 1 + ColorCnt - 3];
          PalBuffer^[K+1] := PalBuffer^[ColorCnt shl 1 + ColorCnt - 2];
          PalBuffer^[K+2] := PalBuffer^[ColorCnt shl 1 + ColorCnt - 1];
          Dec(ColorCnt);
        end;
        Inc(LongInt(PV), 3);
        Inc(K, 3);
      end;
    end;
  end;
End;


Procedure RedrawPalette;

Procedure CalcBuf(View : PView); Far;
Begin
  with View^.LogPalette do begin
    if (Mode and (pmUseRGB + pmOptimize) = (pmUseRGB + pmOptimize)) and
       ((Mode and pmEnable) <> 0) then
    Inc(PalBufferSize, Colors);
  end;
End;

Procedure _RegisterPalette(View : PView); Far;
Begin
  with View^.LogPalette do begin
    if (Mode and (pmUseRGB + pmOptimize) = (pmUseRGB + pmOptimize)) and
       ((Mode and pmEnable) <> 0) then begin

      if (Mode and pmMonopoly) <> 0 then ColorCnt := SystemColors;
      Move(Palette^, PalBuffer^[ColorCnt * 3], Colors * 3);
      Inc(ColorCnt, Colors);
    end;
  end;
End;

Procedure FillColorRefs(View : PView); Far;
Var
  I : Word;
  P : ^TVGARegister;
Begin
  with View^.LogPalette do
  if (Mode and (pmUseRGB + pmOptimize) = (pmUseRGB + pmOptimize)) and
     ((Mode and pmEnable) <> 0) and (ColorRef <> Nil) then begin
    Pointer(P) := Palette;
    for I := 0 to Colors  - 1 do begin
      ColorRef^[I] := RGB(P^[1], P^[2], P^[3]);
      Inc(LongInt(P), 3);
    end;
  end;
End;

Procedure UpdateColorUsers(View : PView); Far;
Var
  Is : Boolean;
Begin
  with View^.LogPalette do
  if ((Mode and pmUseRGB) <> 0) and
     ((Mode and pmEnable) <> 0) then begin
    Is := View^.UpdateColors;
    if (Mode and pmDrawFirst) <> 0 then
        Mode := Mode and not(pmDrawFirst)
    else
      if Is then View^.DrawView;
  end;
End;

Var
  Dup : Word;
Begin
  if MaxColors <> 256 then Exit;

  PalBufferSize := SystemColors;
  PaletteUsers.ForEach(@CalcBuf);
  PalBufferSize := PalBufferSize;
  if PalBufferSize > SystemColors then begin
    PalBuffer := MemAlloc(PalBufferSize * 3);
    if PalBuffer = nil then Exit; {т.е. если не будет памяти, то не будет отрисовок!}
    ColorCnt  := SystemColors;
    Move(PMainPalette^, PalBuffer^, SystemColors * 3);
    PaletteUsers.ForEach(@_RegisterPalette);
    Dup := ColorTolerance + ColorTolerance;
    if ColorCnt > 0 then Repeat
      RemoveDups(Dup);
      Inc(Dup, ColorTolerance);
    Until ColorCnt <= MaxColors;
    Move(PalBuffer^, PMainPalette^, MinWord(SizeOf(MainPalette), PalBufferSize * 3));
    PaletteUsers.ForEach(@FillColorRefs);
    SetVGAPalette(0, 256, PalBuffer);
    FreeMem(PalBuffer, PalBufferSize * 3);
  end;
  PaletteUsers.ForEach(@UpdateColorUsers);
End;

Function  MergePalettes(First, Second : PVGAPalette; FirstColors, SecondColors : Word) : PVGAPalette;
Var
  R : PVGAPalette;
Begin
  R := MemAlloc((FirstColors + SecondColors) * 3);
  MergePalettes := R;
  if R = Nil then Exit;
  Move(First^, R^, FirstColors * 3);
  Inc(LongInt(R), FirstColors * 3);
  Move(Second^, R^, SecondColors * 3);
End;

Function  SqueezePalette(Pal : PVGAPalette; Colors : Word) : PVGAPalette;
Var
  R   : PVGAPalette;
  Dup : Word;
  OSC : Byte;

Begin
  R := MemAlloc(768);
  SqueezePalette := R;
  if R = Nil then Exit;
  if Colors <= 256 then begin
    Move(Pal^, R^, Colors * 3);
    Exit;
  end;
  PalBuffer := MemAlloc(Colors * 3);
  if PalBuffer = nil then begin
    FreeMem(R, 768);
    SqueezePalette := Nil;
    Exit;
  end;
  Move(Pal^, PalBuffer^, Colors * 3);
  OSC := SystemColors;
  SystemColors := 0;
  ColorCnt := Colors;
  Dup := ColorTolerance + ColorTolerance;
  if ColorCnt > 0 then Repeat
    RemoveDups(Dup);
    Inc(Dup, ColorTolerance);
  Until ColorCnt <= MaxColors;
  SystemColors := OSC;
  Move(PalBuffer^, R^, MinWord(768, Colors * 3));
  FreeMem(PalBuffer, Colors * 3);
End;

Procedure RemapPalette(Pal : PVGAPalette; LP : PLogPalette);
Var
  I : Byte;
  P : PVGARegister;
Begin
  if LP = Nil then Exit;
  P := @LP^.Palette^[0];
  for I := 0 to LP^.Colors - 1 do begin
    LP^.ColorRef^[I] := RGBIndirect(P^[1], P^[2], P^[3], 0, 255, Pal);
    Inc(LongInt(P), 3);
  end;
End;

Procedure FadeVGAPalette(Brightness : Byte; var Pal : TVGAPalette);
{brightness = 0..255}
Var
  X, Y : Word;
  I    : Integer;
Begin
  if Brightness <= 63 then begin
    Brightness := 63 - Brightness;
    for X := 0 to 255 do
      for Y := 1 to 3 do
        if Pal[X, Y] < Brightness then Pal[X, Y] := 0
          else Dec(Pal[X, Y], Brightness);
  end else begin
    Brightness := Brightness - 63;
    for X := 0 to 255 do
      for Y := 1 to 3 do begin
        if Pal[X, Y] < 63 - Brightness then
          Inc(Pal[X, Y], Brightness)
          else Pal[X, Y] := 63;
      end;
  end;
End;

Procedure MajorVGAColor(var Pal : TVGAPalette; Tone : TVGARegister);
Var
  X, Y : Word;
Begin
  for X := 0 to 255 do
    for Y := 1 to 3 do
     if Pal[X, Y] + Tone[Y] <= 63 then Inc(Pal[X, Y], Tone[Y])
       else Pal[X, Y] := 63;
End;

Procedure MaskVGAColor(var Pal : TVGAPalette; Mask : TVGARegister);
Var
  X, Y : Word;
Begin
  for X := 0 to 255 do
    for Y := 1 to 3 do
      Pal[X, Y] := Pal[X, Y] and Mask[Y];
End;

Procedure MinorVGAColor(var Pal : TVGAPalette; Tone : TVGARegister);
Var
  X, Y : Word;
Begin
  for X := 0 to 255 do
    for Y := 1 to 3 do
      if Pal[X, Y] - Tone[Y] >= 0 then Dec(Pal[X, Y], Tone[Y])
        else Pal[X, Y] := 0;
End;

Procedure OptionalFade(var VPalette : TVGAPalette; DelayRate, Option : Byte);
Var
  VPalette2       : TVGAPalette;
  MidHue, MaxHue  : Byte;
  W               : Word;

Procedure FadeProc(Where : Byte);
Begin
  Move(VPalette, VPalette2, SizeOf(VPalette2));
  FadeVGAPalette(Where, VPalette2);
  SetVGAPalette(0, 256, @VPalette2);
  Delay(DelayRate);
End;

Begin
  Move(VPalette, VPalette2, SizeOf(VPalette2));
  MidHue := 63; MaxHue := 128;
  case Option of
  fpBlack    : for W := MidHue downto 0 do FadeProc(W);
  fpNormUp   : for W := 0 to MidHue do FadeProc(W);
  fpWhite    : for W := MidHue + 1 to MaxHue do FadeProc(W);
  fpNormDown : for W := MaxHue downto MidHue + 1 do FadeProc(W);
  fpOff      : FadeProc(0);
  fpNormal   : FadeProc(MidHue);
  fpPreserve : GetVGAPalette(0, 256, @VPalette);
  else end;
End;

Procedure UpgradePalette(Count : Byte; var P : array of Byte; Effect : Boolean);
Var
  I, J  : Byte;
Begin
  if Count = 0 then Exit;
  for I := 0 to Count - 1 do
    for J := 0 to 2 do
      if Effect then P[I * 3 + J] := P[I * 3 + J] shr 2
                else P[I * 3 + J] := P[I * 3 + J] shl 2;
End;

type
  PPaletteRec = ^TPaletteRec;
  TPaletteRec = Record
    ID   : Integer;
    LP   : TLogPalette;
    Next : PPaletteRec;
  End;

const
  RegisteredPalettes: PPaletteRec = Nil;

function RegisterPalette(LP : PLogPalette; At : Integer) : Boolean;
Var
  P, R : PPaletteRec;
Begin
  RegisterPalette := False;
  New(P);
  if P = Nil then Exit;
  P^.LP := LP^;
  LP^.Colors := 0;
  LP^.Mode   := 0;
  LP^.ColorRef := Nil;
  LP^.Palette  := Nil;
  P^.Next      := Nil;
  P^.ID        := At;
  if RegisteredPalettes = Nil then RegisteredPalettes := P else begin
    R := RegisteredPalettes;
    if R^.ID = At then begin
      DisposePalette(R^.LP);
      R^.LP := P^.LP;
      Dispose(P);
      Exit;
    end;
    While R^.Next <> Nil do begin
      R := R^.Next;
      if R^.ID = At then begin
        DisposePalette(R^.LP);
        R^.LP := P^.LP;
        Dispose(P);
        Exit;
      end;
    end;
    R^.Next := P;
  end;
  RegisterPalette := True;
End;


function RecordPalette(LP : PLogPalette) : Integer;
Var
  I : Integer;
Begin
  RecordPalette := 0;
  for I := 1 to 32767 do if RetrievePalette(I) = @StdLogPalette then Break;
  if I = 32767 then Exit;
  if not RegisterPalette(LP, I) then Exit;
  RecordPalette := I;
End;

function RetrievePalette(Index : Integer) : PLogPalette;
Var
  R : PPaletteRec;
Begin
  RetrievePalette := @StdLogPalette;
  R := RegisteredPalettes;
  if R = Nil then Exit;
  if R^.ID = Index then begin
    RetrievePalette := @R^.LP;
    Exit;
  end;
  while R <> Nil do begin
    if R^.ID = Index then begin
      RetrievePalette := @R^.LP;
      Exit;
    end;
    R := R^.Next;
  end;
End;

Procedure FreePalette(Index : Integer);
var
  P, R : PPaletteRec;
Begin
  R := RegisteredPalettes;
  while R <> Nil do begin
    if R^.ID = Index then Break;
    R := R^.Next;
  end;
  if R = Nil then Exit;
  P := RegisteredPalettes;
  if P <> R then begin
    while P^.Next <> R do P := P^.Next;
    P^.Next := R^.Next;
  end else RegisteredPalettes := R^.Next;
  DisposePalette(R^.LP);
  Dispose(R);
End;


var
  SaveExit: Pointer;

procedure ExitPalettes; far;
begin
  ExitProc := SaveExit;
  PaletteUsers.DeleteAll;
  PaletteUsers.Done;
end;

Begin
  SaveExit := ExitProc;
  ExitProc := @ExitPalettes;
  PaletteUsers.Init(3, 1);
End.
