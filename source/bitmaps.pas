{$G+,F+,S-}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : BitMaps                                              █
  █                                                                    █
  █ Description : BitMaps handling for Graphic Turbo Vision            █
  █                                                                    █
  █ Author      : Tony Berezin, Oleg Oleinick                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}
unit BitMaps;

interface
uses Objects, MZResource, GDI, DIB, Strings, Drivers, EGInline, Memory, Streams;


type
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

  PPackedBitMapFileHeader = ^PackedBitMapFileHeader;
  PackedBitMapFileHeader = Record
    bfType       : Word;
    bfSize       : LongInt;
  End;


  PBitMapCoreHeader = ^BitMapCoreHeader;  { OS/2 BMP structure }
  BitMapCoreHeader = record
    Size          : longint;
    Width         : integer;
    Height        : integer;
    Planes        : word;
    BitCount      : word;
  end;


  PBitMap = ^TBitMap;
  TBitMap = object(TObject)
    ImageID: Integer;
    constructor Init(AImageID: Word);
    constructor Load(var S: TStream); {OOA}
    destructor  Done; virtual;
    procedure   Store(var S: TStream); {OOA}
    function    ToDraw: PImage;  { please call TView.PutBMP(ToDraw^, x, y) }
    function    Width: Integer; {OOA}
    function    Height: Integer; {OOA}
   private
    Image: PImage;
  end;

function  LoadRscBitmap(Where: PMZResource; Res: PRes; LP : PLogPalette): PImage;
function  LoadBitmap(Where: PMZResource; BitmapName: PChar; LP : PLogPalette): PImage;

function  LoadRscCursor(Where: PMZResource; Res: PRes; var MouseCursor: TMouseCursor): boolean;
function  LoadCursor(Where: PMZResource; CursorName: PChar; var MouseCursor: TMouseCursor): boolean;
procedure FreeCursor(var MouseCursor: TMouseCursor);
procedure MakeCursorImage(const MouseCursor: TMouseCursor; var BitMapAND, BitMapXOR: PImage);

function  LoadRscIcon(Where: PMZResource; Res: PRes; var BitMapAND, BitMapXOR: PImage): boolean;
function  LoadIcon(Where: PMZResource; IconName: PChar; var BitMapAND, BitMapXOR: PImage): boolean;
function  Icon2Cursor(BitMapAND, BitMapXOR: PImage; X, Y: Integer): PMouseCursor;

function  BitMapWidth(BitMap: PImage): integer;
function  BitMapHeight(BitMap: PImage): integer;
function  BitMapColorType(BitMap: PImage): Word;

function IllegalImage(Image: PImage): boolean;     {TONY}
procedure FreeImage(var Image: PImage);            {OOA}

{ Image Manager. OOA }

procedure RegisterImageInCode(AID: Integer; AImage: PImage);
procedure RegisterImageInHeap(AID: Integer; AImage: PImage);
procedure RegisterImageDiscardable(AID: Integer; AImage: PImage);
procedure RegisterImageInMZ(AID: Integer; const AFileName: String; AResName: PChar);
procedure DisposeImage(AID: Integer);
function GetImageID(AImage: PImage): Integer; {0   if not found}
function GetImage(AID: Integer): PImage;      {@EmptyImage if not found}
function GetFreeID : Integer; {-1 if not found}
procedure LockImage(AID: Integer);            {useful for DLLs}
procedure UnlockImage(AID: Integer);
procedure LoadImage(var AID: Integer; var S: TStream);
procedure StoreImage(AID: Integer; var S: TStream);

function LoadImageFile(var S: TStream; Size : LongInt; LP : PLogPalette; ColorType : Word): PImage; {DK}


const
  WinBMPInfoSize   = $28;
  OtherBMPInfoSize = 64;
  OS2BMPInfoSize   = 12;

implementation

uses
  DOS;

Const
  ImageSign = $1970;


function IllegalImage(Image: PImage): boolean;
begin
  IllegalImage := (not Assigned(Image)) or (Image^.Check <> ImageSign);
end;

{ К сожалению, сам по себе объект BitMap имеет возможность загрузки
  из MZ-файла только ресурса ВИДА BITMAP. Так что пришлось иметь
  отдельные объекты TIcon и TMouseCursor, не содержащие полей
  вида SomeBitMap: PBitMap, а прописывать их полностью }

type
  PImageStruct = ^ImageStruct;
  ImageStruct = record
    Check: word;
    Height: word;
    WIB: word;
    Width: word;
    Image: Byte; {Image begins NO-O-OW }
  end;

const
  bmIllegal   = 0;
  bmInCode    = 1;
  bmInHeap    = 2; { Что с этим будешь делать ? }
  bmInMZFile  = 3;
  bmDiscardable = 4; { в Heap и может быть освобождено }

  EmptyImage: ImageStruct = (
    Check: ImageSign;
    Height: 0;
    WIB: 0;
    Width: 0;
    Image: 0
  );


constructor TBitMap.Init(AImageID: Word);
begin
  inherited Init;
  ImageID:= AImageID;
end;

destructor TBitMap.Done;
begin
  DisposeImage(ImageID);
  inherited Done;
end;

function TBitMap.ToDraw: PImage;
begin
  if not Assigned(Image) then
    Image:= GetImage(ImageID);
  if not Assigned(Image) then
  {$IFDEF Debug}
    RunError(255)
  {$ELSE}
    ToDraw := @EmptyImage
  {$ENDIF}
  else
    ToDraw := Image;
end;

constructor TBitmap.Load(var S: TStream); {OOA}
begin
  TObject.Init;
  LoadImage(ImageID, S);
end;

procedure TBitmap.Store(var S: TStream); {OOA}
begin
  StoreImage(ImageID, S);
end;

function TBitmap.Width: Integer; {OOA}
begin
  if not Assigned(Image) then
    Image:= GetImage(ImageID);
  if IllegalImage(Image) then
  {$IFDEF Debug}
    RunError(255)
  {$ELSE}
    Width:= BitmapWidth(@EmptyImage)
  {$ENDIF}
  else
    Width:= BitmapWidth(Image);
end;

function TBitmap.Height: Integer; {OOA}
begin
  if not Assigned(Image) then
    Image:= GetImage(ImageID);
  if not Assigned(Image) then
  {$IFDEF Debug}
    RunError(255)
  {$ELSE}
    Height:= BitmapHeight(@EmptyImage)
  {$ENDIF}
  else
    Height:= BitmapHeight(Image);
end;

Procedure Bitmap2Image(var _Source, _Dest; Width, Height, BPLin, ID : Word);
Var
  I, OurBPLin : Word;
  Source : PByteArray;
  Dest   : PByteArray;
Begin
  Source := @_Source;
  Dest   := @_Dest;
  Word(Pointer(LongInt(Dest) + 0)^) := $1970;
  Word(Pointer(LongInt(Dest) + 2)^) := Height;
  Word(Pointer(LongInt(Dest) + 4)^) := ID;
  Word(Pointer(LongInt(Dest) + 6)^) := Width;
  Inc(LongInt(Dest), 8);
  case ID and $FFF of
  1  : OurBPLin := (Width shr 3) + Byte((Width and 7) <> 0);
  17 : OurBPLin := (Width shr 1) + Byte((Width and 1) <> 0);
  256: OurBPLin := Width;
  else end;
  for I := 0 to Height - 1 do
    Move(Source^[BPLin * I], Dest^[OurBPLin * (Height - I - 1)], OurBPLin);
End;

Procedure Bitmap2SImage(var S : TStream; _Dest : PImage; Width, Height, BPLin, ID, Bits : Word);
Var
  I, OurBPLin : Word;
  Dest, B  : PByteArray;
  Streamed : Boolean;
Begin
  Dest   := PByteArray(_Dest);
  PSimage(Dest)^.Check := imCheck;
  PSimage(Dest)^.Y   := Height;
  Streamed := (ID and imStreamed) <> 0;
  PSimage(Dest)^.NBP := ID;
  PSimage(Dest)^.X   := Width;
  Inc(LongInt(Dest), 8);

  case ID and imColor of
  imMono     : OurBPLin := (Width shr 3) + Byte((Width and 7) <> 0);
  im16       : OurBPLin := (Width shr 1) + Byte((Width and 1) <> 0);
  im256      : OurBPLin := Width;
  imHiColor  : OurBPLin := Width shl 1;
  imTC       : OurBPLin := Width shl 2;
  else end;

  if Streamed then begin
    PSImage(_Dest)^.PS^.Reset;
    B := MemAlloc(maxInteger(BPLin, OurBPLin));
    for I := 0 to Height - 1 do begin
      S.Read(B^, BPLin);
      if Bits = 24 then begin
       case ID and imColor of
       imTC      : ExpandTrue32(B, B, Width, 0);
       imHiColor : ImpactTrueHi(B, B, Width, 0);
       else ImpactTrueColor(B, B, Width, 0, I);
       end;
      end;
      PSImage(_Dest)^.PS^.Seek(LongInt(OurBPLin) * (Height - I - 1));
      PSImage(_Dest)^.PS^.Write(B^, OurBPLin);
    end;
    FreeMem(B, maxInteger(BPLin, OurBPLin));
  end else begin
    if Bits = 24 then begin
      B := MemAlloc(maxInteger(BPLin, OurBPLin));
      for I := 0 to Height - 1 do begin
        S.Read(B^, BPLin);
        case ID and imColor of
        imTC      : ExpandTrue32(B, B, Width, 0);
        imHiColor : ImpactTrueHi(B, B, Width, 0);
        else ImpactTrueColor(B, B, Width, 0, I);
        end;
        Move(B^, Dest^[OurBPLin * (Height - I - 1)], OurBPLin);
       { if BPLin <> OurBPLin then S.Seek(S.GetPos - OurBPLin + BPLin);}
      end;
      FreeMem(B, maxInteger(BPLin, OurBPLin));
    end else
      for I := 0 to Height - 1 do begin
        S.Read(Dest^[OurBPLin * (Height - I - 1)], OurBPLin);
        if BPLin <> OurBPLin then S.Seek(S.GetPos - OurBPLin + BPLin);
      end;
  end;
End;


Procedure MakeMono16(var _Source, _Dest; Width, Height, BPLin : Word; Color0, Color1 : Byte);
Var
  I, J, OurBPLin, NBP : Word;
  Source : PByteArray;
  Dest   : PByteArray;
  Color  : Byte;
Begin
  Source := @_Source;
  Dest   := @_Dest;
  Word(Pointer(LongInt(Dest) + 0)^) := $1970;
  Word(Pointer(LongInt(Dest) + 2)^) := Height;
  Word(Pointer(LongInt(Dest) + 4)^) := 17;
  Word(Pointer(LongInt(Dest) + 6)^) := Width;
  Inc(LongInt(Dest), 8);
  OurBPLin := (Width shr 1) + Byte((Width and 1) <> 0);
  NBP := BPLin * (Height - 1);
  for I := 0 to Height - 1 do begin
    for J := 0 to Width - 1 do begin
      Color := Source^[NBP + J shr 3] and (1 shl (7 - J and 7));
      if Color = 0 then Color := Color0 else Color := Color1;
      if Boolean(J and 1) then
        Dest^[J shr 1] := Dest^[J shr 1] and $F0 or Color else
        Dest^[J shr 1] := Dest^[J shr 1] and $0F or (Color shl 4);
    end;
    Dec(NBP, BPLin);
    Inc(LongInt(Dest), OurBPLin);
  end;
End;

Procedure MakeMonoS16(var S : TSTream; _Dest : PImage; Width, Height, BPLin : Word; Color0, Color1 : Byte; ID : Word);
Var
  I, J, OurBPLin : Word;
  Dest   : PByteArray;
  Color  : Byte;
  Buf, Buf2 : PByteArray;
  Streamed  : Boolean;
  M         : PSImage absolute _Dest;
Begin
  Dest   := PByteArray(_Dest);
  Word(Pointer(LongInt(Dest) + 0)^) := $1970;
  Word(Pointer(LongInt(Dest) + 2)^) := Height;
  Streamed := (ID and $F000) <> 0;
  Word(Pointer(LongInt(Dest) + 4)^) := ID;
  Word(Pointer(LongInt(Dest) + 6)^) := Width;
  Inc(LongInt(Dest), 8);


  Buf := MemAlloc(BPLin);
  OurBPLin := (Width shr 1) + Byte((Width and 1) <> 0);
  if Streamed then begin
    M^.PS^.Reset;
    Buf2 := MemAlloc(OurBPLin);
    for I := 0 to Height - 1 do begin
      S.Read(Buf^, BPLin);
      M^.PS^.Seek(LongInt(OurBPLin) * (Height - I - 1));
      for J := 0 to Width - 1 do begin
        Color := Buf^[J shr 3] and (1 shl (7 - J and 7));
        if Color = 0 then Color := Color0 else Color := Color1;
        if Boolean(J and 1) then
          Buf2^[J shr 1] := Buf2^[J shr 1] and $F0 or Color else
          Buf2^[J shr 1] := Buf2^[J shr 1] and $0F or (Color shl 4);
      end;
      M^.PS^.Write(Buf2^, OurBPLin);
    end;
    FreeMem(Buf2, OurBPLin);
  end else begin
    Inc(LongInt(Dest), OurBPLin * (Height - 1));
    for I := 0 to Height - 1 do begin
      S.Read(Buf^, BPLin);
      for J := 0 to Width - 1 do begin
        Color := Buf^[J shr 3] and (1 shl (7 - J and 7));
        if Color = 0 then Color := Color0 else Color := Color1;
        if Boolean(J and 1) then
          Dest^[J shr 1] := Dest^[J shr 1] and $F0 or Color else
          Dest^[J shr 1] := Dest^[J shr 1] and $0F or (Color shl 4);
      end;
      Dec(LongInt(Dest), OurBPLin);
    end;
  end;
  FreeMem(Buf, BPLin);
End;


function BitMapWidth(BitMap: PImage): integer;
begin
  if Assigned(BitMap) and (BitMap^.Check = ImageSign) then
    BitMapWidth := PImageStruct(BitMap)^.Width
  else
    BitMapWidth := 0;
end;

function BitMapHeight(BitMap: PImage): integer;
begin
  if Assigned(BitMap) and (BitMap^.Check = ImageSign) then
    BitMapHeight := PImageStruct(BitMap)^.Height
  else
    BitMapHeight := 0;
end;

function  BitMapColorType(BitMap: PImage): Word;
Begin
  if Assigned(BitMap) and (BitMap^.Check = ImageSign) then
    BitMapColorType := PImageStruct(BitMap)^.WIB and $FFF
  else
    BitMapColorType := 0;
End;

function LoadBitmap(Where: PMZResource; BitmapName: PChar; LP : PLogPalette): PImage;
var
  Res: PRes;
begin
  LoadBitMap := Nil;
  {--- Поиск ресурса ---}
  Res := Where^.FindResource(MakeIntResource(mzBitMap), BitmapName);
  if Res = Nil then Exit;
  LoadBitMap := LoadRscBitmap(Where, Res, LP);
end;


function LoadRscBitmap(Where: PMZResource; Res: PRes; LP : PLogPalette): PImage;
Var
  Inp : PStream;
const
  imtccolor=imtc;
Begin
  Inp := Where^.CreateAliasStream(Res);
  if Inp = Nil then LoadRscBitmap := Nil else begin
    LoadRscBitmap := LoadImageFile(Inp^, Res^.GetSize, LP, 0);
    Dispose(Inp, Done);
  end;
End;

procedure MakeCursorImage(const MouseCursor: TMouseCursor; var BitMapAND, BitMapXOR: PImage);
var
  BufSize: word;
  SizeImage: word;
  Image: PChar;
begin
  BitMapAND := Nil;
  BitMapXOR := Nil;
  if MouseCursor.Image = Nil then Exit;
  BufSize := 1 + {GetBitMapBufferSize(32, 32)} GetImageBufferSize(32, 32, 17);
  SizeImage := 128;
  if MaxAvail < (2*BufSize) then Exit;
  BitMapAND := MemAlloc(BufSize);
  BitMapXOR := MemAlloc( BufSize);
  Image := MouseCursor.Image; Inc(Image, 6);
  {MakeBWBuffer(BitMapAND^, Image^, 32, 32, SizeImage div 32, 0, 15, 0);}
  Bitmap2Image(Image^, BitMapAnd^, 32, 32, SizeImage div 32, 1);
  Inc(Image, SizeImage);
  {MakeBWBuffer(BitMapXOR^, Image^, 32, 32, SizeImage div 32, 0, 15, 0);}
  Bitmap2Image(Image^, BitMapXor^, 32, 32, SizeImage div 32, 1);
end;

function  LoadRscCursor(Where: PMZResource; Res: PRes; var MouseCursor: TMouseCursor): boolean;
label
  EndOfLoad;
type
  PWord = ^word;
  PByte = ^byte;
var
  Original: PWord;
  Hdr: PBitMapInfoHeader;
  Image: PChar;
  Dest: PWord;
  DestB: PByte absolute Dest;
  SizeImage: word;
  BufSize: word;
  i: integer;
begin
  LoadRscCursor := False;
  MouseCursor.Image := Nil;

  {--- Загрузка ресурса ---}
  if (Res^.Size > $FFF0) or (MaxAvail < Res^.Size) then
    Exit; { Заведомо недостаточно памяти !!! }
  Original := MemAlloc( Res^.Size);
  if Original = Nil then Exit;
  if not Where^.ReadResource(Res, Original^) then goto EndOfLoad;

  MouseCursor.XHotSpot := Original^;
  Inc(Original);
  MouseCursor.YHotSpot := Original^;
  Inc(Original);
  Hdr := Pointer(Original);
  Dec(Original, 2);

  if (Hdr^.Size <> WinBMPInfoSize) or  { Только WIN }
     (Hdr^.Width <> 32) or             { Только 32x32 !!! }
     (Hdr^.Height <> 64) or
     (Hdr^.Planes <> 1) or
     (Hdr^.BitCount <> 1)
  then goto EndOfLoad;

  Image := Pointer(Hdr);
  Inc(Image, Hdr^.Size);  { Skip header }
  Inc(Image, 4*2); { Skip palette info }
  SizeImage := 128;
  BufSize := 128*2 + 6 {internal buffer info} + 1 {byte for DPMI version};

  MouseCursor.Image:= Nil;
  if MaxAvail < BufSize then goto EndOfLoad;
  MouseCursor.Image := MemAlloc( BufSize);
  Dest := MouseCursor.Image;
  Dest^ := 32;  { YSize } Inc(Dest);
  Dest^ := 4;  { WIB } Inc(Dest);
  Dest^ := 32;  { XSize } Inc(Dest);
  Inc(Image, SizeImage*2);  {На конец}
  for i := 1 to 64 do begin
    Dec(Image, 4);
    Move(Image^, DestB^, 4);
    Inc(DestB, 4);
  end;

  LoadRscCursor := True;
EndOfLoad:
  FreeMem(Original, Res^.Size);
end;

procedure FreeCursor(var MouseCursor: TMouseCursor);
var
  BufSize: word;
begin
  if MouseCursor.Image <> Nil then begin
    BufSize := 128*2 + 6 {internal buffer info} + 1 {byte for DPMI version};
    FreeMem(MouseCursor.Image, BufSize);
    MouseCursor.Image := Nil;
  end;
end;

function  LoadCursor(Where: PMZResource; CursorName: PChar; var MouseCursor: TMouseCursor): boolean;
var
  Res: PRes;
begin
  LoadCursor := False;
  MouseCursor.Image := Nil;
  {--- Поиск ресурса ---}
  Res := Where^.FindResource(MakeIntResource(mzCursor), CursorName);
  if not Assigned(Res) then Exit;
  LoadCursor := LoadRscCursor(Where, Res, MouseCursor);
end;

function  LoadRscIcon(Where: PMZResource; Res: PRes; var BitMapAND, BitMapXOR: PImage): boolean;
label
  EndOfLoad;
type
  PWord = ^word;
  PByte = ^byte;
  PBMPPalette = ^TBMPPalette;
  TBMPPalette = array [0..15] of record
    B, G, R, res: byte;
  end;
var
  Original: PWord;
  Hdr: PBitMapInfoHeader;
  Image: PChar;
  Dest: PWord;
  DestB: PByte absolute Dest;
  SizeImage: word;
  BufSize: word;
  i: integer;
  Pal: PBMPPalette;
  ReMap: array [0..15] of byte;
  PaletteSize: integer;
  LP : TLogPalette;
begin
  LoadRscIcon := False;
  BitMapAND := Nil; BitMapXOR := Nil;

  {--- Загрузка ресурса ---}
  if (Res^.Size > $FFF0) or (MaxAvail < Res^.Size) then
    Exit; { Заведомо недостаточно памяти !!! }
  Original := MemAlloc(Res^.Size);
  if Original = Nil then Exit;
  if not Where^.ReadResource(Res, Original^) then goto EndOfLoad;

  Hdr := Pointer(Original);
  if (Hdr^.Size <> WinBMPInfoSize) or  { Только WIN }
     (Hdr^.Width <> 32) or             { Только 32x32 !!! }
     (Hdr^.Height <> 64) or
     (Hdr^.Planes <> 1) or
     (Hdr^.BitCount > 8)
  then goto EndOfLoad;

  Image := Pointer(Hdr);
  Inc(Image, Hdr^.Size);  { Skip header }

  { Получаем указатель на палитру }
  Pal := pointer(Image);
  PaletteSize := Word(1) shl Hdr^.BitCount;
  CreatePalette(Pal, LP, cbwWindows + cbwInit + cbwCreate16Map, PaletteSize);

  PaletteSize := PaletteSize * 4;
  Inc(Image, PaletteSize);  { Skip palette info }

  SizeImage := 128*Hdr^.BitCount;
  BufSize := 128;

  { Перевод изображения во внутренний формат }
  if MaxAvail < BufSize + SizeImage + 32 then begin
    DisposePalette(LP);
    goto EndOfLoad;
  end;
  BitMapXOR := MemAlloc( 128*4 + 10);
  BitMapAND := MemAlloc( 128 + 10);

  case Hdr^.BitCount of
  1:MakeMono16(Image^, BitMapXOR^, 32, 32, SizeImage div 32,
               LP.ColorRef^[0], LP.ColorRef^[1]);
  4:begin
      Bitmap2Image(Image^, BitMapXOR^, 32, 32, SizeImage div 32, 17);
      ReMapImage(BitmapXOR, LP.ColorRef);
    end;
  8:begin
      Bitmap2Image(Image^, BitMapXOR^, 32, 32, SizeImage div 32, 256);
      ReMapImage(BitmapXOR, LP.ColorRef);
    end;
  else end;
  Inc(Image, SizeImage);

  Bitmap2Image(Image^, BitMapAND^, 32, 32, Integer(SizeImage div 32 div HDR^.BitCount), 1);
  DisposePalette(LP);
  LoadRscIcon := True;
EndOfLoad:
  FreeMem(Original, Res^.Size);
end;

function  LoadIcon(Where: PMZResource; IconName: PChar; var BitMapAND, BitMapXOR: PImage): boolean;
var
  Res: PRes;
begin
  LoadIcon := False;
  BitMapAND := Nil; BitMapXOR := Nil;
  {--- Поиск ресурса ---}
  Res := Where^.FindResource(MakeIntResource(mzIcon), IconName);
  if Res = Nil then Exit;
  LoadIcon := LoadRscIcon(Where, Res, BitMapAND, BitMapXOR);
end;

function Icon2Cursor(BitMapAND, BitMapXOR: PImage; X, Y: Integer): PMouseCursor;
var
  MC: PMouseCursor;
  P, P1, P2, P3, P4: PChar;
  i,j: integer;
  Color : Byte;
begin
  New(MC);
  Icon2Cursor := Nil;
  if MC = Nil then Exit;
  if Assigned(MC) then begin
    if X>31 then X := 31;
    if Y>31 then Y := 31;
    if X<0 then X := 0;
    if Y<0 then Y := 0;
    MC^.XHotSpot := X;
    MC^.YHotSpot := Y;
    MC^.Image := MemAlloc( 128*2 + 7);
    Move(BitMapAND^.Buffer, MC^.Image^, 128+6);

    { Хитрая функция отображения цветной иконы на черно-белый курсор }
    P := MC^.Image;
    Inc(P, 128+6);
    P1 := PChar(BitMapXOR);
    Inc(P1, 8);
    P2 := P1;
    {Inc(P2, 128);
    P3 := P2;
    Inc(P3, 128);
    P4 := P3;
    Inc(P4, 128);
    for i:=0 to 127 do
      P[i] := char( (byte(P3[i]) and byte(P1[i])) or (byte(P4[i]) and byte(P2[i])) );}
    FillChar(P^, 128, 0);
    for I := 0 to 31 do begin
      for J := 0 to 15 do begin
        Color := Byte(P2[j]) and $0F;
        if Boolean(Color and 3) then
          Byte(P[J shr 2]) := Byte(P[J shr 2]) or (1 shl (6 - (j and 3 shl 1)));
        Color := Byte(P2[j]) shr 4;
        if Boolean(Color and 3) then
          Byte(P[J shr 2]) := Byte(P[J shr 2]) or (1 shl (7 - (j and 3 shl 1)));
      end;
      Inc(P2, 16);
      Inc(P, 4);
    end;
  end;
  Icon2Cursor := MC;
end;

procedure FreeImage(var Image: PImage);
var
  BufSize: word;
  ImageStruct: PImageStruct absolute Image;
begin
  if not Assigned(Image) then Exit;
  BufSize := 1 + {GetBitMapBufferSize(ImageStruct^.Width, ImageStruct^.Height);}
    GetImageBufferSize(ImageStruct^.Width, ImageStruct^.Height, ImageStruct^.WIB);
  { Был распределен лишний байт для краевых эффектов }
  if (BufSize <> 0) then begin
    FreeMem(Image, BufSize);
    Image:= Nil;
  end;
end;

{ Image Registering. OOA }

type
  PPImageRec = ^PImageRec;
  PImageRec = ^TImageRec;
  TImageRec = record
    ID: Integer;
    Image: PImage;
    Location: Byte;
    LockLev: Integer;
    FileName: PString;
    ResourceName: PChar;
    Next: PImageRec;
  end;


const
  RegisteredImages  : PImageRec   = Nil;

{$S-}
procedure RegisterImagePrim(AID: Integer; AImage: PImage; ALocation: Byte;
  AFileName: PString; AResourceName: PChar); near;
var
  P: PImageRec;
begin
  if AID = 0 then
  begin
    {$IFDEF DEBUG}
    PrintStr(^J^M'BITMAPS.RegisterImage: Image already registered.'^J^M);
    RunError(255);
    {$ELSE}
    Exit;
    {$ENDIF}
  end;
  if RegisteredImages = Nil then
  begin
    New(RegisteredImages);
    P:= RegisteredImages;
  end else
  begin
    P:= RegisteredImages;
    while (P <> Nil) and (P^.Next <> Nil) do
      if P^.ID <> AID then P:= P^.Next else
      begin
        {$IFDEF DEBUG}
        PrintStr(^J^M'BITMAPS.RegisterImage: Image already registered.'^J^M);
        RunError(255);
        {$ELSE}
        Exit;
        {$ENDIF}
      end;
    New(P^.Next);
    P:= P^.Next;
    if P = Nil then Exit;
  end;
  with P^ do
  begin
    ID:= AID;
    Image:= AImage;
    Location:= ALocation;
    LockLev:= 0;
    FileName:= AFileName;
    ResourceName:= AResourceName;
    Next:= Nil;
  end
end;

function FindPrevPtr(AID: Integer): PPImageRec; near;
var
  P: PImageRec;
begin
  FindPrevPtr:= @RegisteredImages;
  P:= RegisteredImages;
  if (P <> Nil) and (P^.ID <> AID) then
  begin
    while (P^.Next <> Nil) and (P^.Next^.ID <> AID) do
      P:= P^.Next;
    FindPrevPtr:= @P^.Next;
  end;
end;
{$S+}

procedure RegisterImageInCode(AID: Integer; AImage: PImage);
begin
  RegisterImagePrim(AID, AImage, bmInCode, Nil, Nil);
end;

procedure RegisterImageInHeap(AID: Integer; AImage: PImage);
begin
  RegisterImagePrim(AID, AImage, bmInHeap, Nil, Nil);
end;

procedure RegisterImageDiscardable(AID: Integer; AImage: PImage);
begin
  RegisterImagePrim(AID, AImage, bmDiscardable, Nil, Nil);
end;

procedure RegisterImageInMZ(AID: Integer; const AFileName: String; AResName: PChar);
begin
  if Seg(AResName^) <> 0 then AResName := StrNew(AResName);
  if AID <> 0 then {TONY}
    RegisterImagePrim(AID, nil, bmInMZFile, NewStr(AFileName), AResName);
end;

procedure DisposeImage(AID: Integer);
var
  P: PPImageRec;
  F: PImageRec;
begin
  P:= FindPrevPtr(AID);
  F:= P^;
  if (F <> Nil) and (F^.LockLev = 0) and
    (F^.Location in [bmInHeap,bmInMZFile,bmDiscardable]) then
  begin
    P^:= F^.Next;
    FreeDImage(F^.Image);
    DisposeStr(F^.FileName);
    if Seg(F^.ResourceName^) <> 0 then StrDispose(F^.ResourceName);
    Dispose(F);
  end;
end;

function GetImageID(AImage: PImage): Integer; {0   if not found}
var
  P: PImageRec;
begin
  P:= RegisteredImages;
  while (P <> Nil) and (P^.Image <> AImage) do P:= P^.Next;
  if P <> Nil then GetImageID:= P^.ID else GetImageID:= 0;
end;

function GetFreeID : Integer; {-1 if not found}
Var
  I : Integer;
begin
  GetFreeId := -1;
  for I := 1 to 32767 do begin
    if GetImage(I) = @EmptyImage then begin
      GetFreeId := I;
      Exit;
    end;
  end;
end;


function GetImage(AID: Integer): PImage;      { @EmptyImage if not found}
var
  P: PPImageRec;
  MZR: PMZResource;
begin
  GetImage:= @EmptyImage;
  P:= FindPrevPtr(AID);
  if P^ <> Nil then with P^^ do
    case Location of
      bmInCode, bmInHeap, bmDiscardable: GetImage:= Image;
      bmInMZFile: if Image <> Nil then GetImage:= Image else
        if FileName <> Nil then
        begin
          New(MZR, Init);
          if MZR = Nil then Exit;
          MZR^.Read(FindFileInPath(FileName^, GetEnv('PATH')));
          Image := LoadBitmap(MZR, ResourceName, nil);
          if Assigned(Image) then {TONY}
            GetImage := Image;
          Dispose(MZR, Done);
          Exit;
        end;
    end;
end;

procedure LockImage(AID: Integer);             {useful for DLLs}
var
  P: PPImageRec;
begin
  P:= FindPrevPtr(AID);
  if P^ <> Nil then Inc(P^^.LockLev);
end;

procedure UnlockImage(AID: Integer);
var
  P: PPImageRec;
begin
  P:= FindPrevPtr(AID);
  if P^ <> Nil then with P^^ do
  begin
    Dec(LockLev);
    {$IFDEF DEBUG}
{ Не нужно генерить ошибку при лишней анлокировке
    if LockLev < 0 then
    begin
      PrintStr(^J^M'Too many BITMAPS.Unlock''s'^J^M);
      RunError(255);
    end;
}{TONY}
    {$ENDIF}
    if (LockLev <= 0) and (Location = bmInMZFile) then FreeDImage(Image);
  end;
end;

const

  { ResourceNameSign - What is ResourceName ? }

  rnsBadValue = 0;
  rnsResourceName = 1;
  rnsResourceNumber = 2;

procedure LoadImage(var AID: Integer; var S: TStream); {OOA}
var
  ID: Integer;
  Image: Pointer;
  Location: Byte;
  FileName: PString;
  ResourceName: PChar;
  ByteBuffer: Byte;
  WordBuffer: Word;
begin
  Image:= Nil;
  FileName:= Nil;
  ResourceName:= Nil;
  S.Read(ID, SizeOf(Word));
  S.Read(Location, SizeOf(Byte));
  case Location of
    bmInCode:
      if FindPrevPtr(ID)^ = Nil then
      {$IFDEF DEBUG}
      begin
        PrintStr(^M^J'Loading unknown Image ID in BITMAPS.LoadImage'^M^J);
        RunError(255);
      end;
      {$ELSE}
        Exit;
      {$ENDIF}
    bmInMZFile:
      begin
        FileName:= S.ReadStr;
        S.Read(ByteBuffer, SizeOf(Byte));
        case ByteBuffer of
          rnsResourceName: ResourceName:= S.StrRead;
          rnsResourceNumber:
            begin
              S.Read(WordBuffer, SizeOf(Word));
              ResourceName:= Ptr(0, WordBuffer);
            end;
        end;
      end;
    bmDiscardable,bmInHeap: {GIO}
      begin
        S.Read(WordBuffer, SizeOf(Word));
        if (WordBuffer > 0) and (MaxAvail > (WordBuffer + 16)) then
        begin
          {MemAlloc ???} Image := MemAlloc( WordBuffer);
          S.Read(Image^, WordBuffer);
        end;
      end;
  {$IFDEF DEBUG}
  else
      PrintStr(^M^J'Bad LocationFlag in BITMAPS.LoadImage'^M^J);
      RunError(255);
  {$ENDIF}
  end;
  RegisterImagePrim(ID, Image, Location, FileName, ResourceName);
end;

procedure StoreImage(AID: Integer; var S: TStream);    {OOA}
var
  P: PPImageRec;
  ByteBuffer: Byte;
  WordBuffer: Word;
begin
  P:= FindPrevPtr(AID);
  if P^ <> Nil then with P^^ do
  begin
    S.Write(ID, SizeOf(Word));
    S.Write(Location, SizeOf(Byte));
    case Location of
      bmInCode: ;
      bmInMZFile:
        begin
          S.WriteStr(FileName);
          if Seg(ResourceName) = 0 then
          begin
            ByteBuffer:= rnsResourceNumber;
            S.Write(ByteBuffer, SizeOf(Byte));
            WordBuffer:= Ofs(ResourceName);
            S.Write(WordBuffer, SizeOf(Word));
          end else
          begin
            ByteBuffer:= rnsResourceName;
            S.Write(ByteBuffer, SizeOf(Byte));
            S.StrWrite(ResourceName);
          end;
        end;
      bmDiscardable,bmInHeap:  {GIO}
        begin
          if IllegalImage(Image) then
          begin
            WordBuffer:= 0;
            S.Write(WordBuffer, SizeOf(Word));
          end else
          begin
            WordBuffer := 1 +
             GetImageBufferSize(PImageStruct(Image)^.Width,
                PImageStruct(Image)^.Height, PImageStruct(Image)^.WIB);
            S.Write(WordBuffer, SizeOf(Word));
            S.Write(Image^, WordBuffer);
          end;
        end;
    end;
  end;
end;

function LoadImageFile(var S: TStream; Size : LongInt; LP : PLogPalette; ColorType : Word): PImage; {DK}
label
  EndOfLoad;
type
  TMisc = Record
    X, Y, Colors:Word;
    Bits, MPal  :Byte;
  End;
Var
  Original  : Pointer;                  {место для заголовка}
  Image     : PChar;
  Hdr       : PBitMapInfoHeader absolute Original;
  HdrOS2    : PBitMapCoreHeader absolute Original;
  HdrSize   : LongInt;                  {размер заголовка}
  TLP       : TLogPalette;              {буфер палитры}
  Misc      : TMisc;                    {общие данные}
  PMode     : Word;                     {флаги палитры}
  Pal       : Pointer;                  {указатель на палитру файла}
  BPL       : LongInt;                  {байтов на линейку в файле}
  OurBPL    : Word;                     {байтов на линейку у нас}
  BitMapImg : PImage;                   {картинка}
  Streamed  : Boolean;                  {shortcut to IsImageStreamed(BitmapImg)}
  B, Dest   : PByteArray;               {буфера под конвертор}
  PS        : PStream;                  {shortcut to PSImage(BItMapImage)^.PS}
  I         : LongInt;
  _Dest     : PByteArray;
  ExtraPal  : TLogPalette;              {палитра на время конверсии}


Begin
  {init}
  LoadImageFile := Nil;
  if (ColorType = imNone) and (LP = Nil) then ColorType := im16;
  if LP = Nil then LP := @TLP;
  LP^.Mode := 0;
  if (ColorType <> imMono)    and
     (ColorType <> im16)      and
     (ColorType <> im256)     and
     (ColorType <> imHiColor) and
     (ColorType <> imTC)      then ColorType := 0;

  {--- Загрузка ресурса ---}
  S.Read(HDRSize, 4); {header}
  Inc(HDRSize, 1024);
  S.Seek(S.GetPos - 4);
  Original := MemAlloc(HDRSize);
  if Original = Nil then Exit;
  S.Read(Original^, HDRSize - 1024);

  {--- Анализ ресурса ---}
  if Hdr^.Size in [WinBMPInfoSize, OtherBMPInfoSize] then begin
    if Hdr^.Compression <> 0 then goto EndOfLoad; { Упакованные BMP's пока не обрабатываются }
    Misc.X := Hdr^.Width;
    Misc.Y := Hdr^.Height;
    Misc.Bits := Hdr^.BitCount;
    Misc.MPal := 4;
    PMode := cbwWindows + cbwInit;
  end else if Hdr^.Size = OS2BMPInfoSize then begin
    Misc.X := HdrOS2^.Width;
    Misc.Y := HdrOS2^.Height;
    Misc.Bits := HdrOS2^.BitCount;
    Misc.MPal := 3;
    PMode := cbwOS2 + cbwInit;
  end else goto EndOfLoad;  { Неизвестный мне тип BitMap'а }

  { установка типа загрузки при default load }
  if ColorType = imNone then case Misc.Bits of
  1 : ColorType := imMono;
  4 : ColorType := im16;
  8 : ColorType := im256;
 24 : ColorType := imTC;
  else goto EndOfLoad; end;  { Неизвестный мне тип BitMap'а }

  {установка флагов палитры для понижающей конверсии}
  PMode := PMode or cbwMonoFix;
  if ColorType = im16                         then PMode := PMode or cbwCreate16Map;
  if (ColorType = im16)  and (Misc.Bits = 24) then PMode := PMode or cbwCreate256Map;
  if (ColorType = im256) and (Misc.Bits = 24) then PMode := PMode or cbwCreate256Map;


  {--- Анализ цветовой гаммы изображения ---}
  case Misc.Bits of
  1: Misc.Colors := 2;
  4:
    if (Hdr^.ClrUsed = 0) or (Hdr^.ClrUsed > 16) then  Misc.Colors := 16
       else Misc.Colors := Hdr^.ClrUsed;
  8:if (Hdr^.ClrUsed = 0)  or (Hdr^.ClrUsed > 256) then Misc.Colors := 256
      else Misc.Colors := Hdr^.ClrUsed;
  24 : begin
    Misc.Colors := 0;
    PMode := PMode and not (cbwWindows or cbwOS2);
  end;
  else Goto EndOfLoad; end; { Необрабатываемый либо неверный тип изображения }

  S.Read(Pointer(LongInt(Original) + HDRSize - 1024)^, Misc.Colors * Misc.MPal);
  { Получаем указатель на палитру }
  Image := Original;
  Inc(Image, Hdr^.Size);  { Skip header }
  Pal := pointer(Image);

  {подготовка, если надо, палитры для диферинга, который требует палитры
  от исходной картинки}
  FillChar(ExtraPal, SizeOf(TLogPalette), 0);
  case ColorType of
  imMono : begin
    if Misc.Bits in [4,8] then CreatePalette(Pal, ExtraPal, PMode, Word(1) shl Misc.Bits);
    Misc.Colors := 2;
  end;
  im16 : begin
    if (Misc.Bits = 8) or ((Misc.Bits = 4) and (LP = @TLP)) then begin
      CreatePalette(Pal, ExtraPal, PMode and not cbwCreate256Map, Word(1) shl Misc.Bits);
      Misc.Colors := 16;
      Pal := @StdVGAPalette;
      PMode := PMode and not (cbwWindows or cbwOS2);
    end;
    if Misc.Bits = 24 then begin
      CreatePalette(@StdVGAPalette, ExtraPal, PMode or cbwCreate256Map or cbwCreate16Map, 256);
      Misc.Colors := 16;
      Pal := @StdVGAPalette;
      PMode := PMode and not (cbwWindows or cbwOS2);
    end;
  end;
  imHiColor, imTC : begin
      {Для совсем бестолковых, которые загружают в 1/16/256-видеорежимах
      что-то как HiColor/TrueColor, палитра на всякий случай}
      if ColorType = imHiColor then
        CreatePalette(Pal, ExtraPal, PMode or cbwCreateHCMap, Word(1) shl Misc.Bits)
      else
        CreatePalette(Pal, ExtraPal, PMode or cbwCreateTCMap, Word(1) shl Misc.Bits);
      if DIBType in [im16, imMono] then PMode := PMode or cbwCreate16Map;
      if DIBType = im256 then PMode := PMode or cbwCreate256Map;
      if DIBType <= 256 then Misc.Colors := 256;
    end;
  else end;

  {создаем результирующую палитру }
  if Misc.Colors > 0 then begin
    Inc(Image, Misc.Colors * Misc.MPal);  { Skip palette info }
    CreatePalette(Pal, LP^, PMode, Misc.Colors);
  end else CreatePalette(@StdVGAPalette, LP^, PMode, 256);
  {заполняем стандартную моно палитру для рез-тов диферинга}
  if Misc.Colors = 2 then begin
    if (Misc.Bits > 1) then begin
      FillChar(LP^.Palette^[0], 3, 0);
      FillChar(LP^.Palette^[1], 3, 255);
      FillChar(LP^.Palette^[15], 3, 255); {cbwMonoFix}
    end;
    if LP^.ColorRef^[0] > 0 then LP^.ColorRef^[0] := 1;
    if LP^.ColorRef^[1] > 0 then LP^.ColorRef^[1] := 1;
  end;

  {--- Анализ размеров изображения ---}
  if (Hdr^.Size <> WinBMPInfoSize) or (Hdr^.SizeImage = 0) then
    BPL := Size - Hdr^.Size - (Word(1) shl Misc.Bits) * Misc.MPal
  else
    BPL := Hdr^.SizeImage;
  BPL := BPL div Misc.Y;

  {место под картинку}
  BitMapImg := CreateDImageIndirect(Misc.X, Misc.Y, ColorType, 0);
  if BitmapImg = Nil then Goto EndOfLoad;
  Dest := PByteArray(LongInt(BitmapImg) + 8);
  Streamed := IsImageStreamed(BitMapImg);
  OurBPL := BPLine(BitmapImg);
  if Streamed then PS := PStream(PSImage(BitmapImg)^.PS);
  if Streamed then PS^.Reset;
  B := MemAlloc(MaxInteger(BPL, OurBPL));
  _Dest := @ScrollMoveArea;
  case ColorType of
  imMono : if Misc.Bits =  1 then _Dest := B;
  im16   : if Misc.Bits =  4 then _Dest := B;
  im256  : if Misc.Bits =  8 then _Dest := B;
  end;

  for I := 0 to Misc.Y - 1 do begin
    S.Read(B^, BPL);

    {конверсия}
    case Misc.Bits of
    1  : case ColorType of
      im16     : begin
        ExpandMono(B, @ScrollMoveArea, Misc.X, 0, PByteArray(LP^.ColorRef));
        Impact16(@ScrollMoveArea, @ScrollMoveArea, Misc.X, 0, @StdColorRefMap);
      end;
      im256    : ExpandMono  (B, @ScrollMoveArea, Misc.X, 0, @StdColorRefMap);
      imHiColor: ExpandMonoHi(B, @ScrollMoveArea, Misc.X, 0, PLongArray(ExtraPal.ColorRef));
      imTC     : ExpandMonoTC(B, @ScrollMoveArea, Misc.X, 0, PLongArray(ExtraPal.ColorRef));
    end;
    4 : case ColorType of
      imMono   : Impact16Mono(B, @ScrollMoveArea, Misc.X, 0, I and 7, ExtraPal.Palette);
      im16     : if LP = @TLP then Remap16(B, Misc.X, ExtraPal.ColorRef);
      im256    : Expand16  (B, @ScrollMoveArea, Misc.X, 0, @StdColorRefMap);
      imHiColor: Expand16Hi(B, @ScrollMoveArea, Misc.X, 0, PLongArray(ExtraPal.ColorRef));
      imTC     : Expand16TC(B, @ScrollMoveArea, Misc.X, 0, PLongArray(ExtraPal.ColorRef));
    end;
    8  : case ColorType of
      imMono   : Impact256Mono(B, @ScrollMoveArea, Misc.X, 0, I and 7, ExtraPal.Palette);
      im16     : Impact16(B, @ScrollMoveArea, Misc.X, 0, ExtraPal.ColorRef);
      imHiColor: ExpandHi(B, @ScrollMoveArea, Misc.X, 0, PLongArray(ExtraPal.ColorRef));
      imTC     : ExpandTC(B, @ScrollMoveArea, Misc.X, 0, PLongArray(ExtraPal.ColorRef));
    end;
    24 : case ColorType of
      imMono   : begin
        ExpandTrue32(B, @ScrollMoveArea, Misc.X, 0);
        ImpactTCMono(@ScrollMoveArea, @ScrollMoveArea, Misc.X, 0, I and 7);
      end;
      im16     : begin
        ImpactTrueColor(B, @ScrollMoveArea, Misc.X, 0, I and 7);
        Impact16(@ScrollMoveArea, @ScrollMoveArea, Misc.X, 0, ExtraPal.ColorRef);
      end;
      im256    : ImpactTrueColor(B, @ScrollMoveArea, Misc.X, 0, I and 7);
      imHiColor: ImpactTrueHi(B, @ScrollMoveArea, Misc.X, 0);
      imTC     : ExpandTrue32(B, @ScrollMoveArea, Misc.X, 0);
    end;
    end;

    if Streamed then begin
      PS^.Seek(LongInt(OurBPL) * (Misc.Y - I - 1));
      PS^.Write(_Dest^, OurBPL);
    end else Move(_Dest^, Dest^[OurBPL * (Misc.Y - I - 1)], OurBPL);
  end;
  FreeMem(B, MaxInteger(BPL, OurBPL));
  if ExtraPal.Colors > 0 then DisposePalette(ExtraPal);
  LoadImageFile := BitmapImg;
EndOfLoad:
  if LP = @TLP then DisposePalette(TLP);
  FreeMem(Original, HdrSize);
End;


end.
