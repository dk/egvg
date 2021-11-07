{$F+}
unit TTMyf;

{ TrueType support for MyFonts Unit,
  Copr. 1994,95 Matthias K”ppe
}

interface

uses MFonts;

type
  PTransMatrix = ^TTransMatrix;
  TTransMatrix = record
    a11, a12,
    a21, a22: Real
  end;

function LoadTTFont(Filename: string; ScaleX, ScaleY: Integer): PFontRec;

function LoadTTFontExt(Filename: string;
  ScaleX, ScaleY: Integer;
  TransMatrix: PTransMatrix;
  Attr: Word): PFontRec;

Procedure RescaleTTF(Handle, Width, Height, Escapement, Italic, Style : Word;
                     TransMatrix : PTransMatrix); {DK}

{ Matrix functions
}
function Identity(TransMatrix: PTransMatrix): PTransMatrix;
function Rotate(Angle: Real; TransMatrix: PTransMatrix): PTransMatrix;
function Italicize(Angle: Real; TransMatrix: PTransMatrix): PTransMatrix;
function Scale(FactorX, FactorY: Real; TransMatrix: PTransMatrix): PTransMatrix;
function Combine(First, Second: PTransMatrix): PTransMatrix;



implementation

uses Objects, TTF, WinRes;

procedure createwinMetrics(ttfont: pttfont; var metrics: TTextMetric);
var
  os2: tos2;
  FontBounds: TRectInt;


        function ScX(eulav: Word): Integer;
        begin
          ScX := (ttf.Scale(SwapWord(eulav), ttfont^.Scaling.x) + $8000) shr 16;
        end;

        function ScY(eulav: Word): Integer;
        begin
          ScY := (ttf.Scale(SwapWord(eulav), ttfont^.Scaling.y) + $8000) shr 16;
        end;


Begin
  ttfont^.GetOs2(os2);
  ttfont^.GetFontBounds(FontBounds);
  FillChar(metrics, SizeOf(metrics), 0);
  with metrics do begin
    tmAscent := ScY(os2.usWinAscent);
    tmDescent := ScY(os2.usWinDescent);
    tmHeight := tmAscent + tmDescent;
    tmAveCharWidth := ScX(os2.xAvgCharWidth);
    tmMaxCharWidth := FontBounds.B.x - FontBounds.A.x;
    tmLastChar := 255;
    tmDefaultChar := 32;
  end
End;


Procedure AnsiFrom866(P : PString);
Var
  I : Byte;
Begin
  if P^[0] > #0 then for I := 1 to Byte(P^[0]) do begin
    case P^[I] of
    '€'..'Ÿ',' '..'¯'   : Inc(Byte(P^[I]), 64);
    'à'..'ï'            : Inc(Byte(P^[I]), 16);
    else end;
  end;
End;

Function Locale866CMap(C : Char) : Integer;
Begin
  if C < 'à' then Locale866CMap := Byte(C) - 94
    else if C < #255 then Locale866CMap := Byte(C) - 42
      else Locale866CMap := 181;
End;

procedure ConvertTType(FontRec: PFontRec; var s: string); far;
var
  Index: Integer;
  Bitmap: pointer;
  Hmtx: THMtx;
  GlyphBounds: TRectInt;
  Width, Height: TRectInt;
  ch: Char;
  i: Byte;
  BugFix : Boolean; {DK bugfix}
  {$DEFINE DANSI}
Begin
  {$IFDEF RUSSIAN}
  if FontRec^.CodePage = $FFFF then AnsiFrom866(@s);
  {$ELSE}
  if FontRec^.CodePage = $FFFF then AnsiFrom437(@s);
  {$ENDIF}

  with pttfont(SharedList^[FontRec^.srcHandle].p)^ do
    For i := 1 to Length(s) do begin
      ch := s[i];
      BugFix := ch = #255;
      if BugFix then begin
        if Locale then Index := Locale866CMap(Ch)
          else Index := getIndexOf(Ch);
        ch   := Chr(zremap);
        S[I] := Chr(zremap);
      end;
      with PABCLongArray(SharedList^[FontRec^.FontHandle].p)^[Ch] do
      if Bits = nil then begin
        if not Locale then begin
          if not BugFix then Index := getIndexOf(Ch);
        end else begin
          if Ch <> Chr(zremap) then
            if Ch < #127 then Index := getIndexOf(Ch) else
              Index := Locale866CMap(Ch);
        end;
        If Index <= 0 then Continue;
        Size := GetBitmap(Index, Bitmap);
        GetHMtx(Index, HMtx);
        GetGlyphBounds(GlyphBounds);
        GetGlyphExtent(Width, Height);
        Bits := Bitmap;
        A := HMtx.lsb.x - Width.A.x + GlyphBounds.A.x;
        D := - (HMtx.lsb.y - Width.A.y);
        B := GlyphBounds.B.x - GlyphBounds.A.x;

{       C := HMtx.Advancewidth.x - (A + B);
        H := - HMtx.Advancewidth.y - D}

        C := Width.A.x - HMtx.lsb.x + HMtx.AdvanceWidth.x - GlyphBounds.B.x;
        H := - (Width.A.y - HMtx.lsb.y + HMtx.AdvanceWidth.y)
      end
    end;
End;

function LoadTTFontExt;
var
  s: PbufStream;
  FontBounds: trectint;
  textmetric: ttextmetric;
  ttfont: pttfont;
  matrix: tmat2;
  font: PFontRec;
Begin
  LoadTTFontExt := Nil;
  s := new(PBufStream, Init(Filename, stOpenRead, 2048));
  if (s = nil) or (s^.status <> 0) then exit;
  if (s^.buffer = nil) then begin
    dispose(s, done);
    exit;
  end;
  ttfont := new(pttfont, init(s));
  if ttfont = nil then Exit;
  with ttFont^ do
  begin
    if TransMatrix <> nil
    then begin
      with TransMatrix^, Matrix do
      begin
        LongInt(eM11) := Round(a11 * 65536);
        LongInt(eM12) := Round(a12 * 65536);
        LongInt(eM21) := Round(a21 * 65536);
        LongInt(eM22) := Round(a22 * 65536);
      end;
      ttfont^.SetMat2(Matrix)
    end;
    ttfont^.SetPointSize(ScaleX, ScaleY);
    ttfont^.SetResolution(96, 96);
    ttfont^.GetFontBounds(FontBounds)
  end;
  CreateWinMetrics(ttfont, TextMetric);
  Font := LoadHugeFont(ttfont, ConvertTType, TextMetric,
    FontBounds.B.y - FontBounds.A.y + 1);
  Font^.CodePage := ttfont^.CodePage;
  Font^.DiffAttr := Attr;
  LoadTTFontExt := Font
End;

Procedure RescaleTTF; {DK}
Var
  FontBounds: trectint;
  textmetric: ttextmetric;
  ttfont: pttfont;
  matrix: tmat2;
  font: PFontRec;
  I : Char;

function ScX(eulav: Word): Integer;
begin
  ScX := (ttf.Scale(SwapWord(eulav), ttfont^.Scaling.x) + $8000) shr 16;
end;

function ScY(eulav: Word): Integer;
begin
  ScY := (ttf.Scale(SwapWord(eulav), ttfont^.Scaling.y) + $8000) shr 16;
end;

Begin
  Font   := GetFontRec(Handle);
  ttfont := pttfont(SharedList^[Font^.srcHandle].p);
  if ttfont = nil then Exit;
  TransMatrix := Rotate(Escapement, Italicize(Italic, Identity(TransMatrix)));
  with ttFont^ do begin
    if TransMatrix <> nil then begin
      with TransMatrix^, Matrix do begin
        LongInt(eM11) := Round(a11 * 65536);
        LongInt(eM12) := Round(a12 * 65536);
        LongInt(eM21) := Round(a21 * 65536);
        LongInt(eM22) := Round(a22 * 65536);
      end;
      ttfont^.SetMat2(Matrix)
    end;
    ttfont^.SetPointSize(Width, Height);
    ttfont^.GetFontBounds(FontBounds)
  end;
  with Font^.TextMetric do begin
    tmAscent := ScY(ttfont^.os2usWinAscent);
    tmDescent := ScY(ttfont^.os2usWinDescent);
    tmHeight := tmAscent + tmDescent;
    tmAveCharWidth := ScX(ttfont^.os2xAvgCharWidth);
    tmMaxCharWidth := FontBounds.B.x - FontBounds.A.x;
  end;
  Font^.FontLength := FontBounds.B.y - FontBounds.A.y + 1;

  for I := #0 to #255 do
    with PABCLongArray(SharedList^[Font^.FontHandle].p)^[I] do
      if Bits <> Nil then FreeMem(Bits, Size);
  FillChar(SharedList^[Font^.FontHandle].p^, SizeOf(TABCLongArray), 0);
  Font^.CodePage := ttfont^.CodePage;
  Font^.DiffAttr := Style;
End;


function LoadTTFont(Filename: string; ScaleX, ScaleY: Integer): PFontRec;
begin
  LoadTTFont := LoadTTFontExt(Filename, ScaleX, ScaleY, nil, ftNormal)
end;

function Identity(TransMatrix: PTransMatrix): PTransMatrix;
begin
  Identity := TransMatrix;
  with TransMatrix^ do begin
    a11 := 1.0; a21 := 0.0;
    a12 := 0.0; a22 := 1.0
  end
end;

function Rotate(Angle: Real; TransMatrix: PTransMatrix): PTransMatrix;
var
  Matrix: TTransMatrix;
  Sine, Cosine: Real;
begin
  Angle := Angle * (pi / 180.0);
  Sine := sin(Angle);
  Cosine := cos(Angle);
  with Matrix do
  begin
    a11 := Cosine; a12 := - Sine;
    a21 := Sine;   a22 := Cosine
  end;
  Rotate := Combine(TransMatrix, @Matrix)
end;

function Italicize(Angle: Real; TransMatrix: PTransMatrix): PTransMatrix;
var
  Matrix: TTransMatrix;
begin
  Italicize := TransMatrix;
  with Matrix do
  begin
    a11 := 1.0;    a12 := sin(Angle * (pi / 180.0));
    a21 := 0.0;    a22 := 1.0
  end;
  Italicize := Combine(TransMatrix, @Matrix)
end;

function Scale(FactorX, FactorY: Real; TransMatrix: PTransMatrix): PTransMatrix;
begin
  with TransMatrix^ do
  begin
    a11 := FactorX * a11;   a12 := FactorX * a12;
    a21 := FactorY * a21;   a22 := FactorY * a22
  end;
  Scale := TransMatrix
end;

function Combine(First, Second: PTransMatrix): PTransMatrix;
var
  a: TTransMatrix;
begin
  Combine := First;
  a := First^;
  with First^ do
  begin
    a11 := a.a11 * Second^.a11 + a.a12 * Second^.a21;
    a12 := a.a11 * Second^.a12 + a.a12 * Second^.a22;
    a21 := a.a21 * Second^.a11 + a.a22 * Second^.a21;
    a22 := a.a21 * Second^.a12 + a.a22 * Second^.a22
  end
end;


begin
  MFonts.LoadTTFontproc := LoadTTFont;
end.
