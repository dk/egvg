{$F+,I-}

Uses EgString, GDI;

Type
  PBitMapInfoHeader = ^BitMapInfoHeader;
  BitMapInfoHeader = record
    Size          : longint;
    Width         : longint;
    Height        : longint;
    Planes        : word;
    BitCount      : word;
    Compression   : longint;
    SizeImage     : longint;
    XPelsPerMeter : longint;
    YPelsPerMeter : longint;
    ClrUsed       : longint;
    ClrImportant  : longint;
  end;

  PBitMapFileHeader = ^BitMapFileHeader;
  BitMapFileHeader = Record
    bfType       : Word;
    bfSize       : LongInt;
    Res_1, Res_2 : Word;
    bfOffBits    : LongInt;
  End;

Var
  F : File;
  B : Byte;

  FH : BitMapFileHeader;
  IH : BitMapInfoHeader;
  I, J : LongInt;
  Colors : LongInt;
  BitLine: LongInt;
  OutLine: LongInt;
  STP    : LongInt;
  BufLine: array[0..32767] of Byte;
  BN     : String;

  HexPal : array[0..15, 1..4] of Byte;
  LP     : TLogPalette;


Begin
  if ParamCount < 1 then begin
    WriteLn('DK Inc. 1996 BMP to text convertor for EG/SVG vision');
    WriteLn('(!) Format : BMP2TXT FILENAME.BMP [BMP_Name]');
    Halt;
  end;
  Assign(F, ParamStr(1));
  Reset(F, 1);
  if IOResult <> 0 then begin
    WriteLn('(!) Cannot open ' , ParamStr(1));
    Halt;
  end;
  BlockRead(F, FH, SizeOf(FH));
  if FH.bfType <> $4d42 then begin
    WriteLn('(!) Not a bitmap file ' , ParamStr(1));
    Close(F);
    Halt;
  end;
  BlockRead(F, IH, SizeOf(IH));

  BN := ParamStr(2);
  if BN = '' then BN := 'NewBMP';

  WriteLn('public ', BN);
  WriteLn(#13#10, BN, ':');
  Write('DB 070h,019h,');
  Write('0', HexB(Lo(Word(IH.Height))), 'h,');
  Write('0', HexB(Hi(Word(IH.Height))), 'h,');
  case IH.BitCount of
  1 : Write('001h,000h,');
  4 : Write('011h,000h,');
  8 : Write('000h,001h,');
 24 : Write('000h,002h,');
  else end;
  Write('0', HexB(Lo(Word(IH.Width))), 'h,');
  WriteLn('0', HexB(Hi(Word(IH.Width))), 'h');

  case IH.BitCount of
  1: Colors := 2;
  4: if (IH.ClrUsed = 0) or (IH.ClrUsed > 16) then Colors := 16 else Colors := IH.ClrUsed;
  8: if (IH.ClrUsed = 0) or (IH.ClrUsed > 256) then Colors := 256 else Colors := IH.ClrUsed;
 24: Colors := 0;
  else end;

  case IH.BitCount of
  1     : OutLine := (IH.Width shr 3) + Byte((IH.Width and 7) <> 0);
  4     : OutLine := (IH.Width shr 1) + Byte((IH.Width and 1) <> 0);
  8, 24 : OutLine := IH.Width;
  else end;
  BitLine := OutLine;
  if (BitLine and 2) <> 0 then BitLine := (BitLine and $FFFE) + 2;

  if IH.BitCount = 4 then begin
    BlockRead(F, HexPal, Colors * 4);
    CreatePalette(@HexPal, LP, cbwWindows + cbwInit + cbwCreate16Map, Colors)
  end else begin
    Seek(F, FilePos(F) + Colors * 4);
    LP.ColorRef := Nil;
  end;
  Stp := FilePos(F);

  for I := 0 to IH.Height - 1 do begin
    Write('DB ');
    Seek(F, Stp + BitLine * (IH.Height - I - 1));
    BlockRead(F, BufLine, BitLine);
    if LP.ColorRef <> Nil then for J := 0 to BitLine - 1 do begin
      BufLine[J] := LP.ColorRef^[BufLine[J] and $0F] or
                    LP.ColorRef^[BufLine[J] and $F0 shr 4] shl 4;
    end;
    for J := 0 to OutLine - 2 do Write('0', HexB(BufLine[J]), 'h,');
    WriteLn('0', HexB(BufLine[BitLine - 1]), 'h');
  end;

  Close(F);
End.