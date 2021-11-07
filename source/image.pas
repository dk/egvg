{DK Inc. 1996 for SVG vision

  Модуль для загрузки файлов форматов BMP, PCX, GIF
  и сохранения изображений в BMP формате.

}
{$G+,I-,F+}
Unit Image;

Interface

Uses Objects, SFormat, GDI, DIB;

Type
  TPCXHeader = Record
    Manufacturer : Byte;
    Version      : Byte;
    Encoding     : Byte;
    BitPixel     : Byte;
    A, B, Res    : TPoint;
    ColorMap     : array[0..47] of Byte;
    Reserved     : Byte;
    BitPlane     : Byte;
    PrimalSize   : Word;
    PalState     : Word;
    Scanner      : TPoint;
    Filler       : array[0..53] of Byte;
  End;

Function LoadRasterFile(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;

Function LoadBMP(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;
Function LoadPCX(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;
Function LoadGIF(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;

Function SaveBMP(FileName: FNameStr; Image : PImage; LP : PLogPalette) : Boolean;

Implementation

Uses Memory, Bitmaps, Streams;


Function LoadBMP;
var
  S: TDOSStream;
  Size : LongInt;
  FB    : BitmapFileHeader;
  FP    : PackedBitmapFileHeader absolute FB;
  P : PImage;
  PNS : PStream;
  Packs : Boolean;
label NoWay;
begin
  LoadBMP := False;
  Packs := False;
  S.Init(FileName, stOpenRead);
  if S.Status <> 0 then Goto NoWay;
  S.Read(FP, SizeOf(FP));
  PNS := @S;
  { obsolete but works
    if FP.bfType = $5042 then begin
    Packs := True;
    if not AllocateTmpStream(PNS, FP.bfSize, Byte(Size)) then begin
      S.Done;
      Exit;
    end;
    FP.bfType := $4D42;
    PNS^.Seek(0);
    PNS^.Write(FP.bfType, 2);

    if not(SetPackBuffer) or (StreamExplode(@S, PNS) <> 0) then begin
      Dispose(PNS, Done);
      S.Done;
      Exit;
    end;
    FreePackBuffer;
    S.Done;
    PNS^.Reset;
    PNS^.Seek(0);
  end else }
  S.Seek(0);
  PNS^.Read(FB, SizeOf(FB));
  if FB.bfType <> $4D42 then Goto NoWay;
  {if (FB.bfSize <> PNS^.GetSize) and (FB.bfSize <> 0) then Goto NoWay;}
  P := LoadImageFile(PNS^, PNS^.GetSize - SizeOf(FB), LP, ColorType);
  RegisterImageInHeap(AID, P);
  LoadBMP := P <> Nil;
NoWay:
  if Packs then Dispose(PNS, Done) else S.Done;
end;

Function SaveBMP(FileName: FNameStr; Image : PImage; LP : PLogPalette) : Boolean;
Type
  TRGBQuad = array[1..4] of Byte;
  TWindozePal = array[0..0] of TRGBQuad;
Label
  NoWay;
Var
  S  : TDOSStream;
  FH : BitmapFileHeader;
  IH : BitmapInfoHeader;
  BPLin, OurBPLin : LongInt;
  TP    : TLogPalette;
  PalW  : ^TWindozePal;
  B     : PByteArray;
  I, J, OBMark  : LongInt;
  HighMap : Pointer;

Begin
  SaveBMP := False;
  S.Init(FileName, stCreate);
  if S.Status <> 0 then Exit;

  FillChar(FH, SizeOf(FH), 0);
  FH.bfType := $4d42; {BM}
  S.Write(FH, SizeOf(FH)); {?}
  if S.Status <> 0 then Goto NoWay;

  FillChar(IH, SizeOf(IH), 0);
  IH.Size          := 40;
  IH.Width         := PSImage(Image)^.X;
  IH.Height        := PSImage(Image)^.Y;
  IH.Planes        := 1;
  case PSImage(Image)^.NBP and imColor of
  imHiColor,imTC : IH.BitCount := 24;
  im256     : IH.BitCount := 8;
  im16      : IH.BitCount := 4;
  imMono    : IH.BitCount := 1;
  else Goto NoWay; end;

  BPLin := IH.Width * IH.BitCount div 8;
  if (BPLin and 3) <> 0 then BPLin := (BPLin and $FFFC) + 4
    else BPLin := (BPLin and $FFF8) + 8;
{  if PSImage(Image)^.NBP and imColor = im16   then Inc(BPLin, IH.Width and 1);
  if PSImage(Image)^.NBP and imColor = imMono then Inc(BPLin, Byte((IH.Width and 7) <> 0));}
  {IH.SizeImage     := BPLin * IH.Height;}
  S.Write(IH, SizeOf(IH)); {?}

  if LP = Nil then begin
    if (PSImage(Image)^.NBP and imColor) < imHiColor then begin
      CreatePalette(@StdVGAPalette, TP, cbwInit, Word(1) shl IH.BitCount);
      if PSImage(Image)^.NBP and imColor = imMono then FillChar(TP.Palette^[1], 3, 63);
    end;
    LP := @TP;
  end;
  LP^.Mode := 0;

  if (PSImage(Image)^.NBP and imColor) <= im256 then begin
    PalW := MemAlloc(4 * LP^.Colors);
    for I := 0 to LP^.Colors - 1 do begin
      for J := 1 to 3 do PalW^[I, 4 - J] := LP^.Palette^[I, J] shl 2;
      PalW^[I, 4] := 0;
    end;
    S.Write(PalW^, 4 * LP^.Colors);
    FreeMem(PalW, 4 * LP^.Colors);
    if LP = @TP then DisposePalette(TP);
  end;

  case PSImage(Image)^.NBP and imColor of
  imMono    : OurBPLin := (IH.Width shr 3) + Byte((IH.Width and 7) <> 0);
  im16      : OurBPLin := (IH.Width shr 1) + Byte((IH.Width and 1) <> 0);
  im256     : OurBPLin := IH.Width;
  imHiColor,imTC : OurBPLin := IH.Width shl 2;
  else end;


  OBMark := S.GetPos;
 { if BPLin <> OurBPLin then S.Seek(S.GetPos + OurBPLin - BPLin);}

  if IsImageStreamed(Image) then begin
    PSImage(Image)^.PS^.Reset;
    PSImage(Image)^.PS^.Seek(0);
    B := MemAlloc(BPLin);
    for I := 0 to IH.Height - 1 do begin
      PSImage(Image)^.PS^.Seek(LongInt(OurBPLin) * (IH.Height - I - 1));
      PSImage(Image)^.PS^.Read(B^, OurBPLin);
      if (PSImage(Image)^.NBP and imColor) = imHiColor then
        ExpandHiTrue(B, B, 0, IH.Width);
      S.Write(B^, BPLin);
    end;
    FreeMem(B, BPLin);
  end else begin
    if (PSImage(Image)^.NBP and imColor) = imHiColor then begin
      GetMem(HighMap, IH.Width shl 2);
      for I := 0 to IH.Height - 1 do begin
        if BPLin <> OurBPLin then S.Seek(S.GetPos - OurBPLin + BPLin);
        ExpandHiTrue(@PByteArray(Image)^[8 + OurBPLin * (IH.Height - I - 1)], HighMap, IH.Width, 0);
        S.Write(HighMap^, OurBPLin);
      end;
      FreeMem(HighMap, IH.Width shl 2);
    end else for I := 0 to IH.Height - 1 do begin
        if BPLin <> OurBPLin then S.Seek(S.GetPos - OurBPLin + BPLin);
        S.Write(PByteArray(Image)^[8 + OurBPLin * (IH.Height - I - 1)], OurBPLin);
      end;
  end;

  S.Reset;
  S.Seek(0);
  FH.bfSize     := S.GetSize;
  FH.bfOffBits  := OBMark;
  S.Write(FH, SizeOf(FH));
  SaveBMP := True;

NoWay:
  S.Done;
  Exit;
End;


Function LoadPCX(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;
Var
  S               : TBufStream;
  Buf             : PByteArray;

Function ReadCh(var Data, Repper : Byte) : Boolean;
Var
  I : Byte;
Begin
  Repper := 1;
  S.Read(I, 1);
  if S.Status <> 0 then begin
    ReadCh := False;
    Exit;
  end;

  if (I and $C0) = $C0 then begin
    Repper := I and $3F;
    S.Read(I, 1);
    if S.Status <> 0 then begin
      ReadCh := False;
      Exit;
    end;
  end;

  Data := I;
  ReadCh := True;
End;

Var
  BCnt       : Word;
  TotalBytes : Word;
  H          : TPCXHeader;
  P          : PImage;
  XSize, NSize : Word;
  A : TPoint;
  BltType : Byte;
  LineCount : Byte;
  CID       : Word;

Procedure SendLine;
Begin
  case BltType of
  4 : begin
    ExpandPlanedTo17(Buf, PSimage(P)^.Buffer, A.X, 0);
    PSImage(P)^.PS^.Write(PSimage(P)^.Buffer^, XSize);
  end;
  24 : begin
    ConvertPlaned24(Buf, PSimage(P)^.Buffer, A.X, 0);
    case ColorType of
    imTC      : ExpandTrue32(PSimage(P)^.Buffer, PSimage(P)^.Buffer, A.X, 0);
    imHiColor : ImpactTrueHi(PSimage(P)^.Buffer, PSimage(P)^.Buffer, A.X, 0);
    else ImpactTrueColor(PSimage(P)^.Buffer, PSimage(P)^.Buffer, A.X, 0, LineCount); end;
    PSImage(P)^.PS^.Write(PSimage(P)^.Buffer^, XSize);
    Inc(LineCount);
  end;
  else PSImage(P)^.PS^.Write(Buf^, XSize); end;
  BCnt := 0;
End;

Var
  Data, Repper, I : Byte;
  TLP             : TLogPalette;
  BufSize         : LongInt;
  Mde             : Word;
  Pal             : Pointer;

Label NoWay;
Begin
  LoadPCX := False;
  S.Init(FileName, stOpenRead, 512);
  if S.Status <> stOk then Goto NoWay;
  S.Read(H, SizeOf(H));
  if H.Manufacturer <> 10 then Goto NoWay;
  TotalBytes := H.PrimalSize * H.BitPlane;
  A := H.B;
  Dec(A.X, H.A.X);
  Dec(A.Y, H.A.Y);


  BltType := H.BitPlane * H.BitPixel;
  NSize := A.X + 1;
  case BltType of
  1  : XSize := (A.X div 8) + Byte((A.X mod 8) <> 0);
 2,4 : begin
    XSize := (A.X div 2) + Byte((A.X mod 2) <> 0);
    PrepareExpandPlaned(A.X, True);
  end;
  8  : XSize := A.X;
 24  : begin
   case ColorType of
   im256     : XSize := A.X;
   imHiColor : XSize := A.X * 2;
   imTC      : XSize := A.X * 4;
   else end;
   NSize := (XSize + 1) * 4;
 end;
 else Goto NoWay; end;

  Buf := MemAlloc(NSize);
  if Buf = Nil then Goto NoWay;

  P := MemAlloc(SizeOf(TSImage));
  PSimage(P)^.Buffer := MemAlloc(A.X * 4);
  if PSimage(P)^.Buffer = Nil then begin
    FreeMem(P, SizeOf(TSImage));
    Goto NoWay;
  end;
  BufSize := LongInt(XSize) * A.Y;
  AllocateTmpStream(PSImage(P)^.PS, BufSize, I);
  if PSImage(P)^.PS = Nil then begin
    FreeMem(PSimage(P)^.Buffer, A.X * 4);
    FreeMem(P, SizeOf(TSImage));
    Goto NoWay;
  end;
  BCnt := 0;
  PSImage(P)^.PS^.Seek(0);

  LineCount := 0;
  While ReadCh(Data, Repper) do
    for I := 1 to Repper do begin
      if BCnt = TotalBytes then SendLine;
      Buf^[BCnt] := Data;
      Inc(BCnt);
    end;
  PSImage(P)^.PS^.Reset;
  PSImage(P)^.PS^.Seek(0);
  S.Reset;

  if (ColorType = im16) or (LP = Nil) then LP := @TLP;
  LP^.Mode := 0;
  Mde := cbwInit or cbwMonoFix;
  if LP = @TLP then Inc(Mde, cbwCreate16Map);


  case BltType of
  4 : CreatePalette(@H.ColorMap, LP^, Mde, 16);
  8 : begin
    S.Seek(S.GetSize - 769);
    S.Read(I, 1);
    GetMem(Pal, 768);
    S.Read(Pal^, 768);
    if I = 10 then Inc(Mde, cbwTreatLoColor);
    CreatePalette(Pal, LP^, Mde or cbwDelete, 256);
  end;
  24 : if ColorType = im256 then begin
    Mde := cbwInit + cbwCreate256Map;
    if ColorType = im16 then Mde := Mde + cbwCreate16Map;
    CreatePalette(Buf, LP^, Mde, 256);
  end;
  else end;

  PSimage(P)^.Check := imCheck;
  PSimage(P)^.Y     := A.Y;
  PSimage(P)^.X     := A.X;

  case H.BitPlane * H.BitPixel of
  1 : PSimage(P)^.NBP   := imStreamed + imMono;
  4 : PSimage(P)^.NBP   := imStreamed + im16;
  8 : PSimage(P)^.NBP   := imStreamed + im256;
 24 : PSimage(P)^.NBP   := imStreamed + ColorType;
  else end;

  if ColorType = im16 then RemapImage(P, LP^.ColorRef);
  if LP = @TLP then DisposePalette(TLP);

  RegisterImageInHeap(AID, P);

  LoadPCX := True;
  FreeMem(Buf, NSize);
NoWay:
  S.Done;
End;


Function LoadGIF(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;

Label NoWay, Disposal;
Var
  SFile    : PsFormat;
  XLen, I  : Word;
  TLP      : TLogPalette;
  Buf, B16 : PByteArray;
  P        : PImage;
  CType, PMode : Word;
  XSize        : Word;
  MaxC, RemapC : Word;
Begin
  LoadGIF := False;
  SFile := New(PsGif,Open(FileName));
  if (SFile = Nil) or (SFile^.Status <> 0) then Goto NoWay;

  with PsGif(SFile)^ do begin
    ReadNewLocImage;
    SetPlanesFlag(fSGraphPlanes);
    XLen := PicWidth;
    MaxC := GetMaxColor + 1;
    case MaxC of
     2 : begin
       CType := 1;
       XSize := (XLen shr 3) + Byte((XLen and 7) <> 0);
       RemapC := 2;
     end;
    3..15 : begin
      CType := 17;
      XSize := (XLen shr 1) + (XLen and 1);
      PrepareExpandPlaned(XLen, False);
      RemapC := 16;
    end;
   16 : begin
      CType := 17;
      XSize := (XLen shr 1) + (XLen and 1);
      PrepareExpandPlaned(XLen, True);
      RemapC := 16;
    end;
   17..256 : begin
      CType := 256;
      XSize := XLen;
      RemapC := 256;
    end
    else Goto NoWay; end;

    GetMem(Buf, XSize);
    if Buf = Nil then Goto NoWay;
    if CType = 17 then begin
      GetMem(B16, XSize);
      if B16 = Nil then begin
        FreeMem(Buf, XSize);
        Goto NoWay;
      end;
    end else B16 := Nil;

    P := CreateDImageIndirect(XLen, PicHeight, CType, 0);
    if P = Nil then Goto Disposal;

    for I := 0 to PicHeight - 1 do begin
      ReadLine(Buf^);
      if CType = 17 then begin
        ExpandPlanedTo17(Buf, B16, XLen, 0);
        MapBitLineWrite(P, I, XSize, 0, XSize, B16);
      end else MapBitLineWrite(P, I, XSize, 0, XSize, Buf);
    end;
    EndOfLocImage;

    if (ColorType = im16) or (LP = Nil) then LP := @TLP;
    if MaxC > 0 then begin
      CreatePalette(ColorMap, LP^, cbwInit + cbwMonoFix, RemapC);
      for I := 0 to RemapC - 1 do begin
        LP^.Palette^[I, 1] := ColorMap^.R[I];
        LP^.Palette^[I, 2] := ColorMap^.G[I];
        LP^.Palette^[I, 3] := ColorMap^.B[I];
      end;
      if RemapC = 2 then LP^.Palette^[15] := LP^.Palette^[1]; {monofix}
      if ColorType = im16 then begin
        for I := 0 to LP^.Colors - 1 do
          LP^.ColorRef^[I] := RGB16(LP^.Palette^[I, 1], LP^.Palette^[I, 2], LP^.Palette^[I, 3]);
        RemapImage(P, LP^.ColorRef);
      end;
    end else LP^.Colors := 0;
    if LP = @TLP then DisposePalette(TLP);
  end;
  RegisterImageInHeap(AID, P);
  LoadGIF := True;
Disposal:
  if B16 <> Nil then FreeMem(B16, XSize);
  FreeMem(Buf, XSize);
NoWay:
  if SFile <> nil then Dispose(SFile, Done);
End;

Function LoadRasterFile(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;
Var
  S : TDOSStream;
  I : Byte;
Begin
  S.Init(FileName, stOpenRead);
  S.Read(I, 1);
  S.Done;

  case I of
  10 : LoadRasterFile := LoadPCX(FileName, AID, LP, ColorType);
  71 : LoadRasterFile := LoadGIF(FileName, AID, LP, ColorType);
  else LoadRasterFile := LoadBMP(FileName, AID, LP, ColorType);
  end;
End;

End.