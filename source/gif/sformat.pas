{*******************************************************}
{                                                       }
{                SFormat  unit                          }
{                                                       }
{     Инструментальный пакет для работы с файлами -     }
{     изображениями в форматах GIF, PCX и TIFF          }
{                                                       }
{     Копирование запрещено (С), 1993, В.А.Кашкаров     }
{     НТЦ "Модуль", Москва, Россия                      }
{                                                       }
{     Copyright (C) 1993, Vitaly.A.Kashkarov            }
{     RC Module, Moscow, Russia                         }
{                                      Апрель 1993 г.   }
{                                                       }
{*******************************************************}

{$F+,G+,I-,O+}
Unit SFormat;

INTERFACE

uses Objects,
     LZW,
     ExManag;

  const

{ Коды ошибок }

    NoErrors        = 0;
    CantOpenFile    = 101;
    ReadFileError   = 102;
    WriteFileError  = 103;
    InvalidFormat   = 104;
    NoMemory        = 105;
    StatusError     = 106;
    OutOfFile       = 107;
    PackLineError   = 108;
    UnpackLineError = 109;
    NotRealized     = 110;
    BadCode         = 111;
    PaletteError    = 112;

{Константа управления памятью }

    BlockSize = 16384;

{ Флаги расположения битовых плоскостей }

    fBGIPlanes    = $00;
    fSGraphPlanes = $01;

{ Константы формата GIF }

    GifSignature = 'GIF87a';
    ImageDescPtr = $2C;
    Terminator   = $3B;

{ Константы формата PCX }

    Manuf         : byte    = 10;
    Hard          : byte    = 5;
    Encod         : byte    = 1;
    RasterDataPtr : longint = 128;

{ Константы формата TIFF }

    MaxDirSize = 1000;

  TYPE

  PsByteBuff = ^TsByteBuff;
  TsByteBuff = array[0..255] of byte;

{ Типы палитр }

    PsVGAMap = ^TsVGAMap;
    TsVGAMap = array[0..255,0..2] of byte;

    PsEGAPal = ^TsEGAPal;
    TsEGAPal = array[0..15] of byte;

    PsPal16     = ^TsPal16;
    TsPal16     = array[0..15,0..2] of byte;

    PsVGAPal = ^TsVGAPal;
    TsVGAPal =
      record
        R,
        G,
        B : TsByteBuff;
      end;


{ Внешние типы формата GIF }

    LineOrderType   = (Seq, Interl);           { Последовательность строк   }

    PsScreenDescriptor = ^TsScreenDescriptor;
    TsScreenDescriptor =                       { Описатель экрана           }
       record
         Width          : word;                { Ширина экрана              }
         Height         : word;                { Высота экрана              }
         GlobColorMap   : boolean;             { Наличие глобальной палитры }
         ColorRes       : byte;                { Кол-во битов на цвет       }
         BitPerPix      : byte;                { Кол-во битов на пиксел     }
         BackGround     : byte                 { Цвет фона                  }
       end;

    PsImageDescriptor = ^TsImageDescriptor;
    TsImageDescriptor =                        { Описатель  изображения     }
       record
         Left           : word;                { Координаты левого          }
         Top            : word;                {            верхнего угла   }
         Width          : word;                { Ширина  изображения        }
         Height         : word;                { Высота  изображения        }
         LocColorMap    : boolean;             { Наличие локальной палитры  }
         LineOrder      : LineOrderType;       { Последовательность строк   }
         BitPerPix      : byte                 { Кол-во битов на пиксел,    }
       end;                                    {   при условии  наличия     }
                                               {   локальной палитры        }
{ Внешние типы формата PCX }

    PspcxHeader = ^TspcxHeader;
    TspcxHeader =
      record
        BitPerPix  : byte;                     { Бит на точку в одной плоск.}
        Left       : word;                     { Размеры картинки(включит.) }
        Top        : word;                     {                            }
        Right      : word;                     {                            }
        Bottom     : word;                     {                            }
        HScrRes    : word;                     { Гориз.разрешение дисплея   }
        VScrRes    : word;                     { Вертик.разрешение дисплея  }
        EGAPlt     : array[0..15,0..2] of byte;{ Палитра                    }
        nPlanes    : byte;                     { Кол-во плоскостей          }
        BytePerLin : word;                     { Байт на строку             }
        PltInfo    : word;                     { Инф.о палитре (1=цв.,2=сер)}
        ScanHRes   : word;                     { Разрешение сканнера        }
        ScanVRes   : word;                     {                            }
      end;

{ Внешние типы формата TIFF }

    OrderType = (Motorolla, Intel);
    CompType  = (NoCompression, ModifiedCCITT);
    ColType   = (WhiteToBlack, BlackToWhite, RGB);

    PsUserData = ^TsUserData;
    TsUserData =
      record
        ByteOrder : OrderType;                 { Послед-ть байтов в слове   }
        Width     : word;                      { Гориз. размер изобр-ния    }
        Height    : word;                      { Вертик. размер изобр-ния   }
        BitPerPix : byte;                      { Кол-во бит на пиксель      }
        ColorRes  : byte;                      { Кол-во битов на цвет       }
        Compress  : CompType;                  { Тип сжатия изображения     }
        ColorType : ColType;                   { Тип палитры                }
      end;


{ Внутренние типы формата GIF }

    PsBuf = ^TsBuf;
    TsBuf = array[0..16383] of byte;

    PsScreen = ^TsScreen;
    TsScreen =
      record
        Width    : word;
        Height   : word;
        InfoByte : byte;
        BkGround : byte;
        ZeroByte : byte
      end;

    PsImage = ^TsImage;
    TsImage =
      record
        Header   : byte;
        Left     : word;
        Top      : word;
        Width    : word;
        Height   : word;
        InfoByte : byte
      end;

    TsGifStatus  = (stRead, stWrite, stNormal);

{ Внутренние типы формата PCX }

    PsPCXBuf = ^TsPCXBuf;
    TsPCXBuf = array[0..BlockSize] of byte;

    TsVGAPalCode = 0..12;
    PsHeader = ^TsHeader;
    TsHeader =
      record
        manuf : byte;
        hard  : byte;
        encod : byte;
        bitpx : byte;
        x1    : word;
        y1    : word;
        x2    : word;
        y2    : word;
        hres  : word;
        vres  : word;
        Plt   : TsPal16;
        vmode : byte;
        nplan : byte;
        bplin : word;
        pinfo : word;
        shres : word;
        svres : word
      end;

{ Внутренние типы формата TIFF}


    PsLongArray = ^TsLongArray;
    TsLongArray = array[1..4096] of longint;

    PsTagsDirPtr = ^TsTagsDirRec;
    TsTagsDirRec =
      record
        SubfileType     : (Full, Reduced, MultiPage);
        Width           : word;
        Height          : word;

        Photometric     : ColType;
        Compression     : CompType;
        FillOrder       : (LeftToRight, RightToLeft);
        PlanarConfig    : (Pixel, Plane);

        SamplesPerPixel : byte;
        BitsPerSample   : byte;
        MinSampleValue  : word;
        MaxSampleValue  : word;

        StripCount      : word;
        RowsPerStrip    : longint;
        StripOffsets    : PsLongArray;
        StripByteCounts : PsLongArray;

        Next            : PsTagsDirPtr;
      end;

    PsTiffHeaderRec = ^TsTiffHeaderRec;
    TsTiffHeaderRec =
      record
        ByteOrder : OrderType;
        FirstDir  : PsTagsDirPtr;
        LastDir   : PsTagsDirPtr;
        Version   : word;
      end;

  TsHeaderRec =
    record
       ByteOrder: array[1..2] of char;         { 'II' - Intel 8086      }
                                               { 'MM' - Motorolla 68000 }
       Version  : word;
       FirstDir : longint;
    end;

  TsTagRec =
    record
       TagType : word;
       DataType: word;
       Length  : longint;
    case Boolean of
       false: (Value1, Value2: word);
       true : (DataOffs: longint);
    end;

  DirArray = array[1..MaxDirSize] of TsTagRec;

  WOffsets = array[1..1024] of word;


{ Общие внутренние типы }

    TsMode       = (mOpen,mCreate);

    PsByte = ^TsByte;
    TsByte = array[0..65500] of byte;

{Основной об'ект }

    PsFormat = ^TsFormat;
    TsFormat =
      object (TBufStream)

        ColorMap   : PsVGAPal;

        procedure  ReadLine( var BitMap); virtual;

        procedure  WriteLine(var BitMap); virtual;

        function   PicWidth  : word; virtual;

        function   PicHeight : word; virtual;

        function   GetMaxColor : byte; virtual;

        procedure  SetColorInPalette(Num,R,G,B : byte); virtual;

        procedure  GetColorFromPalette(Num:byte;var R,G,B : byte); virtual;

        procedure  ConvVGA2EGAPal(BitPPix,BitPCol:byte;S:TsVGAPal;var D:TsEGAPal);

        procedure  ConvEGA2VGAPal(BitPPix,BitPCol:byte;S:TsEGAPal;var D:TsVGAPal);

        procedure  SetPlanesFlag(PlanesConfig : byte);

      private

        PlanesFlag : byte;
        BufP       : PsByte;
        Mode       : TsMode;

        procedure ConvByte4Plane(var SourceBuf,DestBuf;Num : word);
        procedure Conv4PlaneByte(var SourceBuf,DestBuf;Num : word);
        procedure ConvByte3Plane(var SourceBuf,DestBuf;Num : word);
        procedure Conv3PlaneByte(var SourceBuf,DestBuf;Num : word);
        procedure ConvByte2Plane(var SourceBuf,DestBuf;Num : word);
        procedure Conv2PlaneByte(var SourceBuf,DestBuf;Num : word);
        procedure ConvByte1Plane(var SourceBuf,DestBuf;Num : word);
        procedure Conv1PlaneByte(var SourceBuf,DestBuf;Num : word);
        procedure Conv4PlaneSemiByte(var SourceBuf,DestBuf;Num : word);
        procedure ConvSemiByte4Plane(var SourceBuf,DestBuf;Num : word);
        procedure Conv4Plane2Byte(var SourceBuf,DestBuf;Num : word);
        procedure Conv2Byte4Plane(var SourceBuf,DestBuf;Num : word);

      end;

{ Наследники объекта TFormat }

{ Объект для работы с форматом  GIF }

    PsGif = ^TsGif;
    TsGif =
      object (TsFormat)

        ScrDescriptor     : PsScreenDescriptor ;
        ImgDescriptor     : PsImageDescriptor ;

        PltType           : (Glob,Loc,None);

        constructor Open(FName : FNameStr);

        constructor Create(FName          : FNameStr; { Имя файла                  }
                           ScreenWidth    : word;     { Гориз.разрешение дисплея   }
                           ScreenHeight   : word;     { Вертик.разрешение дисплея  }
                           GlobalColorMap : boolean;  { Глобальная палитра         }
                           ScrColorRes    : byte;     { Кол-во битов на цвет       }
                           ScrBitPerPix   : byte;     { Кол-во бит на пиксель      }
                           BGround        : byte   ); { Цвет фона                  }

        procedure CreateNewLocImage(ImageLeft      : word;          { Координаты левого          }
                                    ImageTop       : word;          {            верхнего угла   }
                                    ImageWidth     : word;          { Ширина  изображения        }
                                    ImageHeight    : word;          { Высота  изображения        }
                                    LocalColorMap  : boolean;       { Локальная палитра          }
                                    ImgLineOrder   : LineOrderType; { Порядок строк в изображении}
                                    ImgBitPerPix   : byte       );  { Кол-во бит на пиксель      }
                                                                    { при наличии лок. палитры   }

        procedure  ReadNewLocImage;

        procedure  EndOfLocImage;

        function   SetPtrOnLocImage(Num : byte) : boolean;

        function   GetMaxLocImage : byte;

        procedure  ReadLine( var BitMap); virtual;

        procedure  WriteLine(var BitMap); virtual;

        procedure  SetColorInPalette(Num,R,G,B : byte);            virtual;

        procedure  GetColorFromPalette(Num:byte;var R,G,B : byte); virtual;

        procedure  SetPtrAtTheEndOfGif;

        function   CurrentLogicalLine : word;

        function   GetMaxColor : byte; virtual;

        function   PicWidth  : word; virtual;

        function   PicHeight : word; virtual;

        destructor Done ; virtual;

      private

        P                 : PLZWObject;
        Tail              : word;
        MaxColor          : word;
        PixRes            : word;
        BlockNumber       : Longint;
        TTBuf             : PsByteBuff;
        GifColorMap       : PsVGAMap;
        LL                : array[0..2047] of word;
        CC                : word;
        StartCodLen       : byte;
        GifStatus         : TsGifStatus;
        PScreen           : PsScreen;
        PImage            : PsImage;
        RestorePal        : PsVGAPal;
        PtrOnLocImg       : array[0..63] of longint;
        MaxLocImg         : byte;
        GlobColorMapPtr   : longint;
        LocColorMapPtr    : longint;
        ImgDescPos        : longint;
        RasterDataPtr     : longint;

        procedure  Read(var BitMap; Count : word); virtual;
        procedure Write(var BitMap; Count : word); virtual;
        procedure Seek(Pos :longint); virtual;
        function  GetPos   : Longint; virtual;
        function  GetSize  : Longint; virtual;
        procedure Order(vert : word);
        procedure FindLocImg;

      end;

{ Объект для работы с форматом  PCX }

  PsPCX = ^TsPCX;
  TsPCX =
    object (TsFormat)

      pcxHeader : PspcxHeader;

      constructor Open(FName : FNameStr);

      constructor Create(FName   : FNameStr;   { Имя файла                  }
                         BitpPix : byte;       { Бит на пикс. в одной плоск.}
                         xl      : word;       { Левый край картинки        }
                         yt      : word;       { Верхний край картинки      }
                         xr      : word;       { Правый край картинки       }
                         yb      : word;       { Нижний край картинки       }
                         hres    : word;       { Гориз.разрешение дисплея   }
                         vres    : word;       { Вертик.разрешение дисплея  }
                         npanel  : byte;       { Кол-во плоскостей          }
                         BytePLn : word;       { Байт на строку             }
                         palinfo : word);      { Инф.о палитре (1=цв.,2=сер)}

      procedure ReadLine( var BitMap); virtual;

      procedure WriteLine(var BitMap); virtual;

      procedure SetColorInPalette(Num,R,G,B : byte);            virtual;

      procedure GetColorFromPalette(Num:byte;var R,G,B : byte); virtual;

      function  PicWidth  : word; virtual;

      function  PicHeight : word; virtual;

      function  GetMaxColor : byte; virtual;

      destructor  Done; virtual;

    private

      Head         : PsHeader;
      VGAMapPtr    : PsVGAMap;
      VGAPalCode   : TsVGAPalCode;
      Bff          : PsPCXBuf;
      BffPtr       : word;
      BffCount     : longint;
      MaxColor     : word;
      Rept,Res     : byte;
      LinByteCount : word;

      procedure PCXLinePacker(var SourceBuf,DestBuf;var BpLine,Count :word);
      procedure PCXLineUnPacker(var SourceBuf,DestBuf;var BpLine,Count :word);


    end;

{ Объект для работы с форматом  TIFF }

  PsTIFF = ^TsTIFF;
  TsTIFF =
    object  (TsFormat)

      TiffInfoPtr : PsUserData;

      constructor Open(FName : FNameStr);

      constructor Create(FName     : FNameStr; { Имя файла                  }
                         Order     : OrderType;{ Послед-ть байтов в слове   }
                         ImgWidth  : word;     { Ширина  изображения        }
                         ImgHeight : word;     { Высота  изображения        }
                         PixelSize : byte);    { Кол-во бит на пиксел       }

      procedure ReadLine( var BitMap); virtual;

      procedure WriteLine(var BitMap); virtual;

      procedure SetYCoord(Ypos : word);

      function  PicWidth    : word; virtual;

      function  PicHeight   : word; virtual;

      function  GetMaxColor : byte; virtual;

      destructor Done; virtual;

    private
      Y : word;
      TiffHeader : PsTiffHeaderRec;

      procedure SwapWord(var W: word);
      procedure SwapLong(var L: longint);
      procedure ReadNextDir(var NextDir: longint);
      procedure WriteDir(ImageWidth,
                         ImageHeight  : word;
                         PixelSize    : byte;
                         var
                           FreeAddr   : longint);
    end;

IMPLEMENTATION
{ ********************************************************************* }
{ ********************************************************************* }

{ ********************************************************************* }
procedure  TsFormat.ReadLine( var BitMap);
BEGIN
  Abstract;
END;

{ ********************************************************************* }
procedure  TsFormat.WriteLine(var BitMap);
BEGIN
  Abstract;
END;

{ ********************************************************************* }
procedure  TsFormat.SetColorInPalette(Num,R,G,B : byte);
BEGIN
END;

{ ********************************************************************* }
procedure  TsFormat.GetColorFromPalette(Num:byte;var R,G,B : byte);
BEGIN
END;

{ ********************************************************************* }
function TsFormat.PicWidth : word;
BEGIN
  Abstract;
END;

{ ********************************************************************* }
function TsFormat.PicHeight : word;
BEGIN
  Abstract;
END;

{ ********************************************************************* }
function TsFormat.GetMaxColor : byte;
BEGIN
END;

{ ********************************************************************* }
procedure  TsFormat.SetPlanesFlag(PlanesConfig : byte);
BEGIN
  PlanesFlag := PlanesConfig;
END;

{ ********************************************************************* }
procedure TsFormat.ConvVGA2EGAPal(BitPPix,BitPCol:byte;S:TsVGAPal;var D:TsEGAPal);
const
  MaskR  : array[0..3] of byte = (0,32,4,36);
  MaskG  : array[0..3] of byte = (0,16,2,18);
  MaskB  : array[0..3] of byte = (0,8,1,9);
  STable : array[0..1] of byte = (7,6);
var
  Max   : byte;
  i     : byte;
  shift : byte;

BEGIN
FillChar(D,16,0);
if (BitPCol in [1,2]) and (BitPPix in [1,2,3,4]) then
  begin
    Max   := 1 shl BitPPix;
    shift := STable[pred(BitPCol)];
  end
else
  begin
    Status    := stError;
    ErrorInfo := PaletteError;
    Exit;
  end;
for i := 0 to pred(Max) do
  begin
    D[i] := D[i] or (MaskR[S.R[i] shr shift]);
    D[i] := D[i] or (MaskG[S.G[i] shr shift]);
    D[i] := D[i] or (MaskB[S.B[i] shr shift]);
  end;
END;

{ ********************************************************************* }
procedure TsFormat.ConvEGA2VGAPal(BitPPix,BitPCol:byte;S:TsEGAPal;var D:TsVGAPal);
const
  RGBrgb : array[0..2,0..3] of byte = ((0,32,4,36),(0,16,2,18),(0,8,1,9));
  Elm    : array[0..3] of byte      = ($0,$55,$AA,$FF);
var
  k   : byte;
  i   : byte;
  Max : byte;
BEGIN
FillChar(D,768,0);
if (BitPCol in [1,2]) and (BitPPix in [1,2,3,4]) then
  Max   := 1 shl BitPPix
else
  begin
    Status    := stError;
    ErrorInfo := PaletteError;
    Exit;
  end;
for i := 0 to pred(Max) do
for k := 0 to 3 do
  begin
    if (S[i] and RGBrgb[0,3]) = RGBrgb[0,k] then
      D.R[i] := Elm[k];
    if (S[i] and RGBrgb[1,3]) = RGBrgb[1,k] then
      D.G[i] := Elm[k];
    if (S[i] and RGBrgb[2,3]) = RGBrgb[2,k] then
      D.B[i] := Elm[k];
   end;
END;

{ ********************************************************************* }
procedure TsFormat.ConvSemiByte4Plane(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    AL,  ES:[DI].TsFormat.PlanesFlag
     MOV    flag, AL
     MOV    AX, Num
     TEST   AX, 3
     JNZ    @A0
     SHR    AX, 1
     SHR    AX, 1
     DEC    AX
     JMP    @A1
@A0:
     SHR    AX, 1
     SHR    AX, 1

@A1:
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
@A2:
     MOV    AH, 4
@A3:
     LODSB
     DEC    CX
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BL, 1
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     ADD    AH, AH
     MOV    CL, AH
     RCL    BL, CL
     RCL    BH, CL
     RCL    DL, CL
     RCL    DH, CL
     POP    CX
@C1:
     TEST   flag, 1
     JZ     @C2
     MOV    AL, BL
     STOSB
     PUSH   DI
     ADD    DI, step
     MOV    AL, BH
     STOSB
     ADD    DI, step
     MOV    AL, DL
     STOSB
     ADD    DI, step
     MOV    AL, DH
     STOSB
     POP    DI
     JCXZ   @C3
     JMP    @A2
@C2:
     MOV    AL, DH
     STOSB
     PUSH   DI
     ADD    DI, step
     MOV    AL, DL
     STOSB
     ADD    DI, step
     MOV    AL, BH
     STOSB
     ADD    DI, step
     MOV    AL, BL
     STOSB
     POP    DI
     JCXZ   @C3
     JMP    @A2

@C3:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.ConvByte4Plane(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    Al, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, Al
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
@A2:
     MOV    AH, 8
     XOR    BL, BL
     XOR    BH, BH
     XOR    DL, DL
     XOR    DH, DH
@A3:
     LODSB
     DEC    CX
     RCR    AL, 1
     RCL    BL, 1
     RCR    AL, 1
     RCL    BH, 1
     RCR    AL, 1
     RCL    DL, 1
     RCR    AL, 1
     RCL    DH, 1
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     MOV    CL, AH
     RCL    BL, CL
     RCL    BH, CL
     RCL    DL, CL
     RCL    DH, CL
     POP    CX
@C1:
     TEST   flag, 1
     JZ    @C2
     PUSH   DI
     MOV    [ES: DI], BL
     ADD    DI, step
     MOV    [ES: DI], BH
     ADD    DI, step
     MOV    [ES: DI], DL
     ADD    DI, step
     MOV    [ES: DI], DH
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2
@C2:
     PUSH   DI
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DL
     ADD    DI, step
     MOV    [ES: DI], BH
     ADD    DI, step
     MOV    [ES: DI], BL
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2

@C3:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.ConvByte3Plane(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    Al, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, Al
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
     XOR    DH, DH
@A2:
     MOV    AH, 8
     XOR    BL, BL
     XOR    BH, BH
     XOR    DL, DL
@A3:
     LODSB
     DEC    CX
     RCR    AL, 1
     RCL    BL, 1
     RCR    AL, 1
     RCL    BH, 1
     RCR    AL, 1
     RCL    DL, 1
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     MOV    CL, AH
     RCL    BL, CL
     RCL    BH, CL
     RCL    DL, CL
     POP    CX
@C1:
     TEST   FLAG, 1
     JZ     @C2
     PUSH   DI
     MOV    [ES: DI], BL
     ADD    DI, step
     MOV    [ES: DI], BH
     ADD    DI, step
     MOV    [ES: DI], DL
     ADD    DI, step
     MOV    [ES: DI], DH
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2
@C2:
     PUSH   DI
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DL
     ADD    DI, step
     MOV    [ES: DI], BH
     ADD    DI, step
     MOV    [ES: DI], BL
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2

@C3:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.ConvByte2Plane(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    Al, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, Al
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
     XOR    DH, DH
@A2:
     MOV    AH, 8
     XOR    BX, BX
@A3:
     LODSB
     DEC    CX
     RCR    AL, 1
     RCL    BL, 1
     RCR    AL, 1
     RCL    BH, 1
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     MOV    CL, AH
     RCL    BL, CL
     RCL    BH, CL
     POP    CX
@C1:
     TEST   FLAG, 0
     JZ     @C2
     PUSH   DI
     MOV    [ES: DI], BL
     ADD    DI, step
     MOV    [ES: DI], BH
     ADD    DI, step
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DH
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2
@C2:
     PUSH   DI
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], BH
     ADD    DI, step
     MOV    [ES: DI], BL
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2

@C3:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.ConvByte1Plane(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    Al, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, Al
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
     XOR    DH, DH
@A2:
     MOV    AH, 8
     XOR    BL, BL
@A3:
     LODSB
     DEC    CX
     RCR    AL, 1
     RCL    BL, 1
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     MOV    CL, AH
     RCL    BL, CL
     POP    CX
@C1:
     TEST   FLAG, 1
     JZ     @C2
     PUSH   DI
     MOV    [ES: DI], BL
     ADD    DI, step
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DH
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2
@C2:
     PUSH   DI
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], DH
     ADD    DI, step
     MOV    [ES: DI], BL
     POP    DI
     INC    DI
     JCXZ   @C3
     JMP    @A2

@C3:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv4PlaneSemiByte(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    AL, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, AL
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
@A0:
     TEST   FLAG, 1
     JZ     @AA
     LODSB
     MOV    DH, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    BH, AL
     ADD    SI, step
     LODSB
     MOV    BL, AL
     POP    SI
     JMP    @AB
@AA:
     LODSB
     MOV    BL, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    BH, AL
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    DH, AL
     POP    SI
@AB:
     MOV    AH, 4
@A1:
     xor    al, al
     RCL    BL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     DEC    CX
     JCXZ   @BB
     RCL    BL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     STOSB
     DEC    CX
     JCXZ   @C0
     DEC    AH
     JZ     @A0
     JMP    @A1
@BB:
     STOSB
@C0:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv4PlaneByte(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    AL, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, AL
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
@A0:
     TEST   FLAG, 1
     JZ     @AA
     LODSB
     MOV    DH, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    BH, AL
     ADD    SI, step
     LODSB
     MOV    BL, AL
     POP    SI
     JMP    @AB
@AA:
     LODSB
     MOV    BL, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    BH, AL
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    DH, AL
     POP    SI
@AB:
     MOV    AH, 8
@A1:
     XOR    AL, AL
     RCL    BL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     STOSB
     DEC    CX
     JCXZ   @C0
     DEC    AH
     JZ     @A0
     JMP    @A1
@C0:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv3PlaneByte(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    AL, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, AL
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
@A0:
     TEST   FLAG, 1
     JZ     @AA
     LODSB
     MOV    DH, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    BH, AL
     POP    SI
     JMP    @AB
@AA:
     INC    SI
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    BH, AL
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    DH, AL
     POP    SI
@AB:
     MOV    AH, 8
@A1:
     XOR    AL, AL
     RCL    BH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     STOSB
     DEC    CX
     JCXZ   @C0
     DEC    AH
     JZ     @A0
     JMP    @A1
@C0:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv2PlaneByte(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;

ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    AL, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, AL
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
@A0:
     TEST   FLAG, 1
     JZ     @AA
     LODSB
     MOV    DH, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    DL, AL
     POP    SI
     JMP    @AB
@AA:
     INC    SI
     PUSH   SI
     INC    SI
     ADD    SI, step
     ADD    SI, step
     LODSB
     MOV    DH, AL
     ADD    SI, step
     LODSB
     MOV    DL, AL
     POP    SI

@AB:
     DEC    CX
     MOV    AH, 8
@A1:
     XOR    AL, AL
     RCL    DL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     STOSB
     DEC    CX
     JCXZ   @C0
     DEC    AH
     JZ     @A0
     JMP    @A1
@C0:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv1PlaneByte(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;
  flag : byte;
ASM
     PUSH   DS
     CLD
     LES    DI, Self
     MOV    AL, ES:[DI].TsFormat.PlanesFlag
     MOV    flag, AL
     MOV    AX, Num
     ADD    AX, 7
     MOV    CL, 3
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
@A0:
     TEST   FLAG, 1
     JZ     @AA
     LODSB
     MOV    DH, AL
     JMP    @AB
@AA:
     PUSH   SI
     INC    SI
     INC    SI
     LODSB
     MOV    DH, AL
     POP    SI
     INC    SI
@AB:
     MOV    AH, 8
@A1:
     XOR    AL, AL
     RCL    DL, 1
     RCL    AL, 1
     STOSB
     DEC    CX
     JCXZ   @C0
     DEC    AH
     JZ     @A0
     JMP    @A1
@C0:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv4Plane2Byte(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;

ASM
     PUSH   DS
     CLD
     MOV    AX, Num
     ADD    AX, 3
     MOV    CL, 2
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
     TEST   CX, 1
     JZ     @A0
     INC    CX

@A0:
     LODSB
     MOV    BL, AL
     PUSH   SI
     ADD    SI, step
     LODSB
     MOV    BH, AL
     ADD    SI, step
     LODSB
     MOV    DL, AL
     ADD    SI, step
     LODSB
     MOV    DH, AL
     POP    SI
     JMP    @AB
@AB:
     MOV    AH, 4
@A1:
     RCL    BL, 1
     RCL    AL, 1
     RCL    BL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     STOSB
     DEC    CX
     DEC    CX
     JCXZ   @C0
     DEC    AH
     JZ     @A0
     JMP    @A1
@BB:
     STOSB
@C0:
     POP    DS
END;

{ ********************************************************************* }
procedure TsFormat.Conv2Byte4Plane(var SourceBuf,DestBuf;Num : word); assembler;
var
  step : word;

ASM
     PUSH   DS
     CLD
     MOV    AX, Num
     ADD    AX, 3
     MOV    CL, 2
     SHR    AX, CL
     DEC    AX
     MOV    step, AX
     LDS    SI, SourceBuf
     LES    DI, DestBuf
     MOV    CX, Num
@A2:
     MOV    AH, 4
@A3:
     LODSB
     DEC    CX
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BL, 1
     RCL    AL, 1
     RCL    BL, 1
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     ADD    AH, AH
     MOV    CL, AH
     RCL    BL, CL
     RCL    BH, CL
     RCL    DL, CL
     RCL    DH, CL
     POP    CX
@C1:
     MOV    AL, BL
     STOSB
     PUSH   DI
     ADD    DI, step
     MOV    AL, BH
     STOSB
     ADD    DI, step
     MOV    AL, DL
     STOSB
     ADD    DI, step
     MOV    AL, DH
     STOSB
     POP    DI
     JCXZ   @C2
     JMP    @A2
@C2:
     POP    DS
END;

{ ********************************************************************* }
{ ********************************************************************* }

constructor TsGIF.Open(FName : FNameStr);

var
  st          : string[6];
  str         : byte;
  bool        : boolean;
  i,j         : word;
  position    : longint;
  buflength   : word;
  bufer       : TsByteBuff;
  E           : Exception;

BEGIN
  TBufStream.Init(FName,stOpen,BlockSize);
  if EverythingOk(E) then
    begin

      if Status  <> stOk then Raise(E,3);

      st[0] := #6;
      Mode := mOpen;
      fillchar(PtrOnLocImg[0],256,0);

      TBufStream.Read(st[1],6);
      if Status  <> stOk then Raise(E,1);

      if st <> GifSignature then Raise(E,4);
      if MaxAvail >= SizeOf(TsScreen) then New(PScreen)
      else Raise(E,2);

      TBufStream.Read(PScreen^,SizeOf(TsScreen));
      if Status  <> stOk then Raise(E,1);
      position:= TBufStream.GetPos;

      if (Lo(PScreen^.InfoByte) and 128) <> 0 then bool := true
      else bool := false;

      if MaxAvail >= SizeOf(TsScreenDescriptor) then New(ScrDescriptor)
      else Raise(E,2);

      With ScrDescriptor^ do
        begin
          Width    := PScreen^.Width;
          Height   := PScreen^.Height;

          GlobColorMap := bool;

          ColorRes := (PScreen^.InfoByte and 112) shr 4 + 1;

          BitPerPix := (PScreen^.InfoByte and 7) + 1;

          BackGround := PScreen^.BkGround;

        end;
      Dispose(PScreen);
      PScreen := nil;

      if ScrDescriptor^.GlobColorMap then
        begin
          PltType := Glob;
          GlobColorMapPtr := position;

          if MaxAvail >= SizeOf(TsVGAMap) then
            New(GifColorMap)
          else Raise(E,2);

          MaxColor  := (1 shl ScrDescriptor^.BitPerPix);
          buflength := MaxColor * 3;

          TBufStream.Read(GifColorMap^,buflength);
          if Status <> stOk then Raise(E,1);

          if MaxAvail >= SizeOf(TsVGAPal) then
            New(ColorMap)
          else Raise(E,2);
          for i := 0 to pred(MaxColor) do
            begin
              ColorMap^.R[i] := GifColorMap^[i,0];
              ColorMap^.G[i] := GifColorMap^[i,1];
              ColorMap^.B[i] := GifColorMap^[i,2];
            end;

          Dispose(GifColorMap);
          GifColorMap := nil;
        end
      else
        PltType := None;

      TBufStream.Read(str,1);
      if Status  <> stOk then Raise(E,1);

      if str <> ImageDescPtr then
        begin
          position := TBufStream.GetPos;

          repeat
            TBufStream.Read(bufer,SizeOf(bufer));
            if Status  <> stOk then Raise(E,1);

            i := 0;
            repeat
              str := bufer[i];
              inc(i);
            until ((i = 256) or (str = ImageDescPtr));

            inc(position,i)
          until str = ImageDescPtr;
          dec(position);
          TBufStream.Seek(position);
        end;

      position := TBufStream.GetPos;
      dec(position);
      TBufStream.Seek(position);

      ImgDescPos := position;
      PtrOnLocImg[0] := ImgDescPos;
      FindLocImg;
      if Status = stError then Raise(E,5);
      GifStatus  := stNormal;
    end
  else
    begin
      if Status = stOk then Status := stError;
      case E.Result of

        1 : ErrorInfo := ReadFileError;

        2 : ErrorInfo := NoMemory;

        3 : ErrorInfo := CantOpenFile;

        4 : ErrorInfo := InvalidFormat;

        5 : ErrorInfo := NotRealized;
      end;

    end;
END;

{ ********************************************************************* }
constructor TsGIF.Create(FName           : FNameStr;
                          ScreenWidth    : word;
                          ScreenHeight   : word;
                          GlobalColorMap : boolean;
                          ScrColorRes    : byte;
                          ScrBitPerPix   : byte;
                          BGround        : byte );
var
  st          : string[6];
  str         : char;
  i,j,k       : word;
  position    : longint;
  buflength   : word;
  bufer       : array[0..255] of char;
  E           : Exception;

BEGIN
  TBufStream.Init(FName,stCreate,BlockSize);
  if EverythingOk(E) then
    begin
      if Status <> stOk then Raise(E,3);

      Mode  := mCreate;
      st[0] := #6;
      st    := GifSignature;

      TBufStream.Write(st[1],6);
      if Status <> stOk then Raise(E,1);

      if MaxAvail >= SizeOf(TsScreen) then New(PScreen)
      else Raise(E,2);

      if MaxAvail >= SizeOf(TsScreenDescriptor) then New(ScrDescriptor)
      else Raise(E,2);

      With ScrDescriptor^ do
        begin
          Width    := ScreenWidth;
          Height   := ScreenHeight;

          GlobColorMap := GlobalColorMap ;

          ColorRes  := ScrColorRes;
          BitPerPix := ScrBitPerPix ;

          BackGround := BGround;

          PScreen^.Width  := Width;
          PScreen^.Height := Height;

          if GlobColorMap  then
            PScreen^.InfoByte := 128
          else PScreen^.InfoByte := 0;

          PScreen^.InfoByte := PScreen^.InfoByte or
                               ((ColorRes - 1) shl 4);
          PScreen^.InfoByte  := PScreen^.InfoByte  or
                               (BitPerPix - 1);
          PScreen^.BkGround := BackGround;
          PScreen^.ZeroByte := 0;
        end;

      TBufStream.Write(PScreen^,SizeOf(TsScreen));
      if Status <> stOk then Raise(E,1);

      Dispose(PScreen);
      PScreen := nil;

      if ScrDescriptor^.GlobColorMap then
        begin

          if MaxAvail >= SizeOf(TsVGAPal) then
            New(ColorMap)
          else
            Raise(E,2);

          PltType           := Glob;
          GlobColorMapPtr := TBufStream.GetPos;
          MaxColor          := (1 shl ScrBitPerPix);
          buflength         := MaxColor * 3;
          position          := GlobColorMapPtr ;

          inc(position,buflength);
          TBufStream.Seek(position);
        end
      else
        PltType := None;

      ImgDescPos := TBufStream.GetPos;
      GifStatus  := stNormal;
      MaxLocImg  := 0;
      PtrOnLocImg[MaxLocImg] := ImgDescPos;
    end
  else
    begin
    if Status = stOk then Status := stError;
    case E.Result of

      0 : ErrorInfo := InvalidFormat;

      1 : ErrorInfo := WriteFileError;

      2 : ErrorInfo := NoMemory;

      3 : ErrorInfo := CantOpenFile;

    end;
    end;
END;

{ ********************************************************************* }
procedure TsGIF.CreateNewLocImage(ImageLeft      : word;
                                  ImageTop       : word;
                                  ImageWidth     : word;
                                  ImageHeight    : word;
                                  LocalColorMap    : boolean;
                                  ImgLineOrder   : LineOrderType;
                                  ImgBitPerPix   : byte       );
var
  i,j       : word;
  buflength : word;
  pos       : longint;
  E         : Exception;


BEGIN
  if EverythingOk(E) then
    begin
      if GifStatus <> stNormal then Raise(E,0);

      P := New(PLZWPacker,Init(Self,ImgBitPerPix));
      if P = nil then Raise(E,2);

      GifStatus := stWrite;

      inc(MaxLocImg);
      if Mode = mCreate then
        begin
          PtrOnLocImg[MaxLocImg] := ImgDescPos;
          TBufStream.Seek(ImgDescPos);
        end
      else
        begin
          SetPtrAtTheEndOfGif;
          ImgDescPos := TBufStream.GetPos;
          PtrOnLocImg[MaxLocImg] := ImgDescPos;
        end;
      if MaxAvail >= SizeOf(TsImage) then New(PImage)
      else Raise(E,2);

      if MaxAvail >= SizeOf(TsImageDescriptor) then New(ImgDescriptor)
      else Raise(E,2);

      With ImgDescriptor^ do
        begin
          Left     := ImageLeft;
          Top      := ImageTop;

          Width    := ImageWidth;
          Height   := ImageHeight;

          LocColorMap := LocalColorMap ;

          BitPerPix := ImgBitPerPix ;

          LineOrder  := ImgLineOrder ;

          PImage^.Header := ImageDescPtr;
          PImage^.Left   := Left;
          PImage^.Top    := Top;
          PImage^.Width  := Width;
          PImage^.Height := Height;

          if LocColorMap  then
            PImage^.InfoByte := 128
          else PImage^.InfoByte := 0;

          if LineOrder = Interl then
            PImage^.InfoByte := PImage^.InfoByte or 64;

          PImage^.InfoByte := PImage^.InfoByte or pred(BitPerPix);
        end;

      pos := TBufStream.GetPos;
      TBufStream.Write(PImage^,SizeOf(TsImage));
      if Status <> stOk then Raise(E,1);

      Dispose(PImage);
      PImage := nil;

      if ImgDescriptor^.LocColorMap then
        begin
          PltType := Loc;
          if ColorMap = nil then
            if MaxAvail >= SizeOf(TsVGAPal) then
              New(ColorMap)
            else
              Raise(E,2);

          MaxColor := (1 shl ImgBitPerPix);
          LocColorMapPtr := TBufStream.GetPos;
          pos := LocColorMapPtr;

          buflength := MaxColor * 3;
          inc(pos,buflength );
          TBufStream.Seek(pos);

          if ImgDescriptor^.BitPerPix <> 1 then
            StartCodLen := ImgBitPerPix
          else
            StartCodLen := 2;
        end
      else
        begin
          if GlobColorMapPtr <> 0
             then PltType := Glob
             else PltType := None;
          if ScrDescriptor^.BitPerPix <> 1 then
            StartCodLen := ScrDescriptor^.BitPerPix
          else
            StartCodLen := 2;
        end;

      TBufStream.Write(StartCodLen,1);
      if Status <> stOk then Raise(E,1);

      if ImgDescriptor^.LineOrder = Interl then
        Order(ImgDescriptor^.Height)
      else
        for i := 0 to pred(ImgDescriptor^.Height) do LL[i] := i;


      if ImgDescriptor^.LocColorMap then
        PixRes := ImgBitPerPix
      else
        PixRes := ScrDescriptor^.BitPerPix;

      RasterDataPtr := TBufStream.GetPos;

      CC := 0;

    end
  else
    begin
    if Status = stOk then Status := stError;
      case E.Result of

        0 : ErrorInfo := StatusError;

        1 : ErrorInfo := WriteFileError;

        2 : ErrorInfo := NoMemory;

        3 : ErrorInfo := NotRealized;
      end;
    end;
END;

{ ********************************************************************* }
procedure TsGIF.ReadNewLocImage;
var
   i,j,k      : word;
   bool       : boolean;
   position   : longint;
   buflength  : word;
   E          : Exception;


BEGIN
if EverythingOk(E) then
  begin

    if GifStatus <> stNormal then Raise(E,0);


    GifStatus := stRead;
    RestorePal := nil;

    if MaxAvail >= SizeOf(TsImage) then New(PImage)
    else Raise(E,2);

    position := pred(TBufStream.GetSize);
    if ImgDescPos = position then Raise(E,3);

    TBufStream.Seek(ImgDescPos);
    TBufStream.Read(PImage^,SizeOf(TsImage));

    if Status  <> stOk then Raise(E,1);

    if (PImage^.InfoByte and 128) = 128 then bool := true
    else bool := false;

    if MaxAvail >= SizeOf(TsImageDescriptor) then New(ImgDescriptor)
    else Raise(E,2);

    With ImgDescriptor^ do
      begin

        Left := PImage^.Left;
        Top  := PImage^.Top;

        Width  := PImage^.Width;
        Height := PImage^.Height;

        LocColorMap := bool;

        if (PImage^.InfoByte and 64) = 0 then
             LineOrder := Seq
        else LineOrder := Interl;

        BitPerPix := (PImage^.InfoByte and 7) + 1;
      end;


    Dispose(PImage);
    PImage := nil;

    if ImgDescriptor^.LocColorMap then PixRes := ImgDescriptor^.BitPerPix
    else PixRes := ScrDescriptor^.BitPerPix;


    if ImgDescriptor^.LocColorMap then
      begin


        LocColorMapPtr := TBufStream.GetPos;

        if MaxAvail >= SizeOf(TsVGAMap) then
          New(GifColorMap)
        else Raise(E,2);

        if ColorMap = nil then
          if MaxAvail >= SizeOf(TsVGAPal) then
            New(ColorMap)
          else Raise(E,2);

        PltType   := Loc;
        MaxColor  := (1 shl ImgDescriptor^.BitPerPix);
        buflength := MaxColor * 3;


        TBufStream.Read(GifColorMap^,buflength);
        if Status <> stOk then Raise(E,1);

        for i := 0 to pred(MaxColor) do
          begin
            ColorMap^.R[i] := GifColorMap^[i,0];
            ColorMap^.G[i] := GifColorMap^[i,1];
            ColorMap^.B[i] := GifColorMap^[i,2];
          end;

        Dispose(GifColorMap);
        GifColorMap := nil;


        TBufStream.Read(StartCodLen,1);
        if Status <> stOk then Raise(E,1);

        P := New(PLZWUnPacker,Init(Self,ImgDescriptor^.BitPerPix));
        if P = nil then Raise(E,2);

      end
    else
      begin
        if PltType = Loc then
        if GlobColorMapPtr <> 0 then
          begin
            PltType := Glob;
            position := TBufStream.GetPos;
            TBufStream.Seek(GlobColorMapPtr);
            if MaxAvail >= SizeOf(TsVGAMap) then
              New(GifColorMap)
            else Raise(E,2);

            MaxColor  := (1 shl ScrDescriptor^.BitPerPix);
            buflength := MaxColor * 3;

            TBufStream.Read(GifColorMap^,buflength);
            if Status <> stOk then Raise(E,1);

            if ColorMap = nil then
              if MaxAvail >= SizeOf(TsVGAPal) then
                New(ColorMap)
              else Raise(E,2);

            for i := 0 to pred(MaxColor) do
              begin
                ColorMap^.R[i] := GifColorMap^[i,0];
                ColorMap^.G[i] := GifColorMap^[i,1];
                ColorMap^.B[i] := GifColorMap^[i,2];
              end;
            Dispose(GifColorMap);
            GifColorMap := nil;
            TBufStream.Seek(position);
          end
        else
          PltType := None;

        TBufStream.Read(StartCodLen,1);
        if Status <> stOk then Raise(E,1);

        P := New(PLZWUnPacker,Init(Self,ScrDescriptor^.BitPerPix));
        if P = nil then Raise(E,2);

      end;

    if ImgDescriptor^.LineOrder = Interl then
      Order(ImgDescriptor^.Height)
    else
      for i := 0 to pred(ImgDescriptor^.Height) do LL[i] := i;

    if ImgDescriptor^.LocColorMap then PixRes := ImgDescriptor^.BitPerPix
    else PixRes := ScrDescriptor^.BitPerPix;

    RasterDataPtr := TBufStream.GetPos;
    CC := 0;

  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      0 : ErrorInfo := StatusError;

      1 : ErrorInfo := ReadFileError;

      2 : ErrorInfo := NoMemory;

      3 : ErrorInfo := OutOfFile;

      4 : ErrorInfo := NotRealized;

    end;
  end;
END;

{ ********************************************************************* }
procedure TsGIF.EndOfLocImage;
var
   buflength   : word;
   position    : longint;
   str         : string;
   ch          : byte;
   i           : byte;
   E           : Exception;
BEGIN
if EverythingOk(E) then
  begin
    if GifStatus = stWrite then
      begin
        Dispose(PLZWPacker(P), Done);
        p := nil;

        TTBuf^[0] := 255 - Tail;
        Tail := 256 - Tail;

        TBufStream.Write(TTBuf^,Tail);
        if Status  <> stOk then Raise(E,1);

        TTBuf^[0] := 0;
        TBufStream.Write(TTBuf^,1);
        if Status  <> stOk then Raise(E,1);

        if TTBuf <> nil then
        begin
          Dispose(TTBuf);
          TTBuf := nil;
        end;

        ch  := Terminator;

        TBufStream.Write(ch,1);
        if Status <> stOk then Raise(E,2);

        position := TBufStream.GetPos;
        dec(position);
        TBufStream.Seek(position);
        ImgDescPos := position;

        if ImgDescriptor^.LocColorMap then
          begin

            if MaxAvail >= SizeOf(TsVGAMap) then
              New(GifColorMap)
            else
              Raise(E,2);

            for i := 0 to pred(MaxColor) do
              begin
                GifColorMap^[i,0] := ColorMap^.R[i];
                GifColorMap^[i,1] := ColorMap^.G[i];
                GifColorMap^[i,2] := ColorMap^.B[i];
              end;

            TBufStream.Seek(LocColorMapPtr);

            buflength := MaxColor * 3;
            TBufStream.Write(GifColorMap^,buflength);
            if Status <> stOk then Raise(E,1);

            Dispose(GifColorMap);
            GifColorMap := nil;

          end;
      end
    else
      begin
        Dispose(PLZWUnPacker(P), Done);
        p := nil;
        position := TBufStream.GetPos;
        ImgDescPos := position;
      end;

    Dispose(ImgDescriptor);
    ImgDescriptor := nil;

    GifStatus := stNormal;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := WriteFileError;

      2 : ErrorInfo := ReadFileError;

    end;
  end;
END;

{ ********************************************************************* }
procedure TsGIF.ReadLine(var BitMap);
var
  Res     : word;
  SBuf    : array[0..2047] of byte;
  PBitMap : pointer;
BEGIN

  PBitMap := @BitMap;

  if PLZWUnpacker(P)^.Eof then
  begin
    if Status = stOk then Status := stError;
    ErrorInfo := BadCode;
    exit;
  end;

  if PixRes in [5,6,7,8] then
    PLZWUnpacker(P)^.BlockRead(PBitMap^,ImgDescriptor^.Width,Res)
  else
    PLZWUnpacker(P)^.BlockRead(SBuf,ImgDescriptor^.Width,Res);

  if ErrorCode = 0 then
    begin
      inc(CC);
      case PixRes of

        1 : ConvByte1Plane(SBuf,PBitMap^,ImgDescriptor^.Width);

        2 : ConvByte2Plane(SBuf,PBitMap^,ImgDescriptor^.Width);

        3 : ConvByte3Plane(SBuf,PBitMap^,ImgDescriptor^.Width);

        4 : ConvByte4Plane(SBuf,PBitMap^,ImgDescriptor^.Width);

      end;
    end
  else
    begin
      if Status = stOk then Status := stError;
      ErrorInfo := UnpackLineError;
    end;
END;

{ ********************************************************************* }
procedure TsGIF.WriteLine(var BitMap);
var
  SBuf    : array[0..2047] of byte;
  PBitMap : pointer;

BEGIN
  PBitMap := @BitMap;

  case PixRes of

    1 : Conv1PlaneByte(PBitMap^,SBuf,ImgDescriptor^.Width);

    2 : Conv2PlaneByte(PBitMap^,SBuf,ImgDescriptor^.Width);

    3 : Conv3PlaneByte(PBitMap^,SBuf,ImgDescriptor^.Width);

    4 : Conv4PlaneByte(PBitMap^,SBuf,ImgDescriptor^.Width);

  end;

  if PixRes in [5,6,7,8] then
    PLZWPacker(P)^.BlockWrite(PBitMap^,ImgDescriptor^.Width)
  else
    PLZWPacker(P)^.BlockWrite(SBuf,ImgDescriptor^.Width);

  if ErrorCode = 0 then
    inc(CC)
  else
    begin
      if Status = stOk then Status := stError;
      ErrorInfo := UnpackLineError;
    end;

END;

{ ********************************************************************* }
procedure TsGIF.SetPtrAtTheEndOfGif;
var
  l   : longint;
  a   : word;
  Enf : TsByteBuff;
BEGIN

  l := pred(TBufStream.GetSize) - 256;
  TBufStream.Seek(l);
  TBufStream.Read(Enf,256);

  asm
    PUSH   DS
    LEA    DI, Enf
    PUSH   SS
    POP    ES
    ADD    DI, 256
    MOV    AX, DI
    MOV    a , AX
    STD
    MOV    AL, 03BH
    MOV    CX, 256
    REPNE  SCASB
    SUB    a , DI
    POP    DS
  end;

  inc(l,256);
  dec(l,pred(a));
  ImgDescPos := l;
  TBufStream.Seek(l);
  TBufStream.Read(a,1);

END;

{ ********************************************************************* }
procedure TsGIF.SetColorInPalette(Num,R,G,B : byte);
BEGIN
  if ColorMap <> nil then
    if Num <= pred(MaxColor) then
      begin
        ColorMap^.R[Num] := R;
        ColorMap^.G[Num] := G;
        ColorMap^.B[Num] := B;
      end
    else
      begin
        if Status = stOk then Status := stError;
        ErrorInfo := InvalidFormat;
      end;
END;

{ ********************************************************************* }
procedure TsGIF.GetColorFromPalette(Num:byte;var R,G,B : byte);
BEGIN
  if ColorMap <> nil then
    if Num <= pred(MaxColor) then
      begin
        R := ColorMap^.R[Num];
        G := ColorMap^.G[Num];
        B := ColorMap^.B[Num];
      end
    else
      begin
        R := 0;
        G := 0;
        B := 0;
      end;
END;

{ ********************************************************************* }
function  TsGIF.CurrentLogicalLine : word;
BEGIN
  CurrentLogicalLine := LL[CC];
END;

{ ********************************************************************* }
function  TsGIF.GetMaxColor : byte;
BEGIN
  GetMaxColor := pred(MaxColor);
END;

{ ********************************************************************* }
function  TsGIF.SetPtrOnLocImage(Num : byte) : boolean;
BEGIN
  if (Mode = mOpen) and (GifStatus = stNormal) and (MaxLocImg >= Num) then
    begin
      ImgDescPos := PtrOnLocImg[Num];
      TBufStream.Seek(ImgDescPos);
      SetPtrOnLocImage := true;
    end
  else
    SetPtrOnLocImage := false;
END;

{ ********************************************************************* }
function  TsGIF.GetMaxLocImage : byte;
BEGIN
  GetMaxLocImage := MaxLocImg;
END;

{ *********************************************************************** }
function  TsGIF.PicWidth : word;
BEGIN
  PicWidth := ImgDescriptor^.Width;
END;

{ *********************************************************************** }
function  TsGIF.PicHeight : word;
BEGIN
  PicHeight := ImgDescriptor^.Height;
END;

{ ********************************************************************* }

destructor TsGIF.Done;
var
  i         : byte;
  pos       : longint;
  ch        : char;
  buflength : word;
  E         : Exception;

BEGIN
if EverythingOk(E) then
  begin
    if Mode = mCreate then
      begin
        if ScrDescriptor^.GlobColorMap  then
           begin
             if MaxAvail >= SizeOf(TsVGAMap) then
               New(GifColorMap)
             else
               Raise(E,1);

               for i := 0 to pred(MaxColor) do
                 begin
                   GifColorMap^[i,0] := ColorMap^.R[i];
                   GifColorMap^[i,1] := ColorMap^.G[i];
                   GifColorMap^[i,2] := ColorMap^.B[i];
                 end;

             pos := TBufStream.GetPos;
             TBufStream.Seek(GlobColorMapPtr);

             buflength := MaxColor * 3;
             TBufStream.Write(GifColorMap^,buflength);
             if Status <> stOk then Raise(E,2);

           end;

      end;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := NoMemory;

      2 : ErrorInfo := WriteFileError;

    end;
  end;

  if P             <> nil then begin
                                 if GifStatus = stWrite then
                                   Dispose(PLZWPacker(P),Done);
                                 if GifStatus = stRead then
                                   Dispose(PLZWUnPacker(P),Done);
                                 P := nil;
                               end;
  if TTBuf         <> nil then begin
                                 Dispose(TTBuf);
                                 TTBuf := nil;
                               end;
  if GifColorMap   <> nil then begin
                                 Dispose(GifColorMap);
                                 GifColorMap := nil;
                               end;
  if ColorMap      <> nil then begin
                                 Dispose(ColorMap);
                                 ColorMap := nil;
                               end;
  if RestorePal    <> nil then begin
                                 Dispose(RestorePal);
                                 RestorePal := nil;
                               end;
  if PScreen       <> nil then begin
                                 Dispose(PScreen);
                                 PScreen := nil;
                               end;
  if PImage        <> nil then begin
                                 Dispose(PImage);
                                 PImage := nil;
                               end;
  if ScrDescriptor <> nil then begin
                                 Dispose(ScrDescriptor);
                                 ScrDescriptor := nil;
                               end;
  if ImgDescriptor <> nil then begin
                                 Dispose(ImgDescriptor);
                                 ImgDescriptor := nil;
                               end;
  TBufStream.Done;
END;

{ ********************************************************************* }
procedure TsGIF.Read(var BitMap; Count : word);
  var
    BlockCount : byte;
    i,s,a      : word;
    E          : Exception;

BEGIN

if EverythingOk(E) then
  begin
    BufP := @BitMap;
    s := 0;
    inc(BlockNumber,Count);
    repeat
      if Tail = 0 then
        begin
          TBufStream.Read(BlockCount,1);
          if Status <> stOk then Raise(E,1);

          a := Count - s;
          if BlockCount <> 0 then
            if a > BlockCount then
              begin
                TBufStream.Read(BufP^[s],BlockCount);
                if Status <> stOk then Raise(E,1);
                inc(s,BlockCount);
              end
            else
              begin
                Tail := BlockCount - a;

                TBufStream.Read(BufP^[s],a);
                if Status <> stOk then Raise(E,1);
                inc(s,a);
              end
          else
            begin
              i := Count - s;
              fillchar(BufP^[s],i,0);
              s := Count
            end
        end
      else
        begin
          TBufStream.Read(BufP^[s],Tail);
          if Status <> stOk then Raise(E,1);
          inc(s,Tail);

          Tail := 0;
        end
    until s = Count;
  end
else
  begin
    if Status = stOk then Status := stError;
    ErrorInfo := ReadFileError;
  end;
END;

{ ********************************************************************* }
procedure TsGIF.Write(var BitMap;Count : word);
  const
    TBuf : PsBuf = nil;
  var
    i,s,j      : word;
    E          : Exception;

procedure Write_To_Block(Count_B : word);
  var
    BlockCount : byte;
    a,k,l      : integer;
    E          : Exception;

begin
if EverythingOk(E) then
  begin
    if MaxAvail >= SizeOf(TsBuf) then
      New(TBuf)
    else
      Raise(E,1);
    a := Count_B div 255;
    l := 0;
    for k := 0 to pred(a) do
      begin
        TBuf^[l] := 255;
        inc(l);
        move(BufP^[s],TBuf^[l],255);
        inc(s,255);
        inc(l,255);
      end;

    TBufStream.Write(TBuf^,a shl 8);
    if Status <> stOk then Raise(E,1);

    if TBuf <> nil then
    begin
      Dispose(TBuf);
      TBuf := nil;
    end;

    BlockCount := Count_B mod 255;

    if TTBuf = nil then
      if MaxAvail >= SizeOf(TsByteBuff) then
        New(TTBuf)
      else
        Raise(E,1);

    if BlockCount <> 0 then
      begin
        Tail := 255 - BlockCount;
        move(BufP^[s],TTBuf^[1],BlockCount);
        inc(s,BlockCount);
       end;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of
      1 : ErrorInfo := NoMemory;
    end;
    if TBuf <> nil then
    begin
      Dispose(TBuf);
      TBuf := nil;
    end;

  end;
end;

BEGIN
if EverythingOk(E) then
  begin
    BufP := @BitMap;
    s := 0;
    inc(BlockNumber,Count);
    if Tail = 0 then
      begin
        Write_To_Block(Count);
        if Status <> stOk then Raise(E,2);
      end
    else
      begin
        if Count > Tail then
          begin
            i := 256 - Tail;
            move(BufP^[s],TTBuf^[i],Tail);
            i := Tail;
            inc(s,i);

            TTBuf^[0]:=255;

            TBufStream.Write(TTBuf^,256);
            if Status <> stOk then Raise(E,1);

            Count := Count - Tail;
            Tail := 0;
            Write_To_Block(Count);
            if Status <> stOk then Raise(E,2);
          end
        else
          begin
            i := Tail - Count;
            j := 256 - Tail;
            move(BufP^[s],TTBuf^[j],i);
            inc(s,i);
            Tail := i;
          end;
      end;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

     1 : ErrorInfo := WriteFileError;

     2 : ErrorInfo := NoMemory;

    end;
  end;
END;


{ ********************************************************************* }
procedure TsGIF.Seek(Pos: Longint);

BEGIN

END;

{ ********************************************************************* }
function TsGIF.GetPos : Longint;

BEGIN
  GetPos := BlockNumber;
END;

{ ********************************************************************* }
function TsGIF.GetSize : Longint;

BEGIN
  GetSize := $7fffffff;
END;

{ ********************************************************************* }
procedure TsGIF.Order(vert : word);
var
  i,j,k : word;

BEGIN

  k := 0;
  j := 0;
  repeat
    LL[k] := j;
    inc(j,8);
    inc(k);
  until j > pred(Vert);

  j := 4;
  repeat
    LL[k] := j;
    inc(k);
    inc(j,8);
  until j > pred(Vert);

  j := 2;
  repeat
    LL[k] := j;
    inc(k);
    inc(j,4);
  until j > pred(Vert);

  j := 1;
  repeat
    LL[k] := j;
    inc(k);
    inc(j,2);
  until j > pred(Vert);

END;

{ ********************************************************************* }
procedure TsGIF.FindLocImg;
var
  Num,
  aa,dd    : byte;
  ch       : byte;
  int,int1 : longint;
  i,ii,bb  : word;
  bufer    : TsByteBuff;
  E          : Exception;

BEGIN
if EverythingOk(E) then
  begin
    int  := ImgDescPos;
    Num := 0;

    repeat
      inc(int,9);
      TBufStream.Seek(int);
      TBufStream.Read(aa,1);
      if Status <> stOk then Raise(E,1);

      if (aa and 128) <> 0 then
        begin
          dd := aa and 7 + 1;
          case dd of
            1 : inc(int,8);
            2 : inc(int,14);
            3 : inc(int,26);
            4 : inc(int,50);
            5 : inc(int,98);
            6 : inc(int,194);
            7 : inc(int,386);
            8 : inc(int,770);
          end;
        end
      else
        inc(int,2);

      TBufStream.Seek(int);

      repeat
        TBufStream.Read(aa,1);
        if Status <> stOk then Raise(E,1);
        bb := aa + 1;
        inc(int,bb);
        TBufStream.Seek(int)
      until aa = 0;

      int1 := TBufStream.GetSize;
      if (int1 - int) < SizeOf(bufer) then ii := int1 - int
                                       else ii := SizeOf(bufer);
      repeat
        TBufStream.Read(bufer,ii);
        if Status  <> stOk then Raise(E,1);

        i := 0;
        repeat
          ch := bufer[i];
          inc(i);
        until ((i = ii) or (ch = ImageDescPtr) or (ch = Terminator));

        inc(int,i)
      until ((ch = ImageDescPtr) or (ch = Terminator));

      if ch = ImageDescPtr then
        begin
          inc(Num);
          dec(int);
          PtrOnLocImg[Num] := int;
          TBufStream.Seek(int);
        end;

    until ch = Terminator;

    TBufStream.Seek(ImgDescPos);
    MaxLocImg := Num;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := ReadFileError;

      2 : ErrorInfo := NotRealized;

    end;
  end;
END;

{ *********************************************************************** }
{ *********************************************************************** }
constructor TsPCX.Open(FName : FNameStr);

var
   position  : longint;
   buflength : word;
   k,i       : byte;
   E         : Exception;
BEGIN
TDosStream.Init(FName,stOpen);
  if EverythingOk(E) then
    begin
      if Status  <> stOk then Raise(E,1);

      Mode := mOpen;

      if MaxAvail >= SizeOf(TsHeader) then New(Head)
      else Raise(E,2);

      if MaxAvail >= SizeOf(TspcxHeader) then New(pcxHeader)
      else Raise(E,2);

      TDosStream.Read(Head^,SizeOf(TsHeader));
      if Status  <> stOk then Raise(E,3);

      if Head^.manuf <> 10 then Raise(E,4);
      with pcxHeader^ do
        begin
          Manuf      := Head^.manuf;
          Hard       := Head^.hard;
          Encod      := Head^.encod;
          BitPerPix  := Head^.bitpx;
          Left       := Head^.x1;
          Top        := Head^.y1;
          Right      := Head^.x2;
          Bottom     := Head^.y2;
          HScrRes    := Head^.hres;
          VScrRes    := Head^.vres;
          nPlanes    := Head^.nplan;
          BytePerLin := Head^.bplin;
          PltInfo    := Head^.pinfo;
          ScanHRes   := Head^.shres;
          ScanVRes   := Head^.svres;
        end;

      LinByteCount := pcxHeader^.BytePerLin * pcxHeader^.nPlanes;

      MaxColor  := (1 shl (pcxHeader^.BitPerPix * pcxHeader^.nPlanes));
      buflength := MaxColor * 3;

      if MaxAvail >= SizeOf(TsVGAPal) then New(ColorMap)
      else Raise(E,2);

      if ((pcxHeader^.BitPerPix = 2) and (pcxHeader^.nPlanes = 4)) or
         (pcxHeader^.BitPerPix in [8,7,6,5]) then
        begin
          position := pred(TBufStream.GetSize);
          dec(position,buflength);
          TDosStream.Seek(position);

          TDosStream.Read(k,1);
          if Status  <> stOk then Raise(E,3);

          if (k = 10) or (k = 12) then
            if k = 12 then VGAPalCode := 12
            else VGAPalCode := 10
          else VGAPalCode := 0;

          if MaxAvail >= SizeOf(TsVGAMap)
            then New(VGAMapPtr)
          else Raise(E,2);

          TDosStream.Read(VGAMapPtr^,buflength);
          if Status  <> stOk then Raise(E,3);


          if VGAPalCode = 10 then
            for i := 0 to pred(MaxColor) do
              begin
                ColorMap^.R[i] := VGAMapPtr^[i,0] shl 2;
                ColorMap^.G[i] := VGAMapPtr^[i,1] shl 2;
                ColorMap^.B[i] := VGAMapPtr^[i,2] shl 2;
              end
          else
            for i := 0 to pred(MaxColor) do
              begin
                ColorMap^.R[i] := VGAMapPtr^[i,0];
                ColorMap^.G[i] := VGAMapPtr^[i,1];
                ColorMap^.B[i] := VGAMapPtr^[i,2];
              end;
        end
      else
        begin
          VGAPalCode := 0;

          for i := 0 to pred(MaxColor) do
            begin
              ColorMap^.R[i] := Head^.Plt[i,0];
              ColorMap^.G[i] := Head^.Plt[i,1];
              ColorMap^.B[i] := Head^.Plt[i,2];
            end;
        end;

      dispose(Head);
      Head := nil;

      Rept := 0;
      Res  := 0;

      TDosStream.Seek(RasterDataPtr);

      position := TDosStream.GetSize - RasterDataPtr;

      if MaxAvail >= SizeOf(TsPCXBuf) then New(Bff)
      else Raise(E,2);

      if position < BlockSize then
        begin
          TDosStream.Read(Bff^,position);
          if Status  <> stOk then Raise(E,3);
          BffCount := position;
        end
      else
        begin
          TDosStream.Read(Bff^,BlockSize);
          if Status  <> stOk then Raise(E,3);
          BffCount := BlockSize;
        end;

      BffPtr := 0;

    end
  else
    begin
      if Status = stOk then Status := stError;
      case E.Result of

        1 : ErrorInfo := CantOpenFile;

        2 : ErrorInfo := NoMemory;

        3 : ErrorInfo := ReadFileError;

        4 : ErrorInfo := InvalidFormat;
      end;
    end;
END;

{ *********************************************************************** }
constructor TsPCX.Create(FName   : FNameStr; { Имя файла                  }
                         BitpPix : byte;     { Кол - во бит на пиксел     }
                         xl      : word;     { Левый край картинки        }
                         yt      : word;     { Верхний край картинки      }
                         xr      : word;     { Правый край картинки       }
                         yb      : word;     { Нижний край картинки       }
                         hres    : word;     { Гориз.разрешение дисплея   }
                         vres    : word;     { Вертик.разрешение дисплея  }
                         npanel  : byte;     { Кол-во плоскостей          }
                         BytePLn : word;     { Байт на строку             }
                         palinfo : word);    { Инф.о палитре (1=цв.,2=сер)}

var
  position  : longint;
  buflength : word;
  E          : Exception;

BEGIN
  TDosStream.Init(FName,stCreate);
  if EverythingOk(E) then
    begin

      if Status  <> stOk then Raise(E,1);

      Mode := mCreate;

      if MaxAvail >= SizeOf(TsHeader) then New(Head)
      else Raise(E,2);

      if MaxAvail >= SizeOf(TspcxHeader) then New(pcxHeader)
      else Raise(E,2);

      with pcxHeader^ do
        begin
          BitPerPix   := BitpPix;
          Left        := xl;
          Top         := yt;
          Right       := xr;
          Bottom      := yb;
          HScrRes     := hres;
          VScrRes     := vres;
          nPlanes     := npanel;
          BytePerLin  := BytePLn;
          PltInfo     := palinfo;
          ScanHRes    := 0;
          ScanVRes    := 0;

          Head^.manuf := Manuf;
          Head^.hard  := Hard;
          Head^.encod := Encod;
          Head^.bitpx := BitpPix;
          Head^.x1    := xl;
          Head^.y1    := yt;
          Head^.x2    := xr;
          Head^.y2    := yb;
          Head^.hres  := hres;
          Head^.vres  := vres;
          Head^.nplan := npanel;
          Head^.bplin := BytePerLin;
          Head^.pinfo := palinfo;
          Head^.shres := 0;
          Head^.svres := 0;

        end;
      position := TDosStream.GetPos;
      TDosStream.Write(Head^,SizeOf(TsHeader));
      if Status  <> stOk then Raise(E,3);

      dispose(Head);
      Head  := nil;

      LinByteCount := pcxHeader^.BytePerLin * pcxHeader^.nPlanes;

      if MaxAvail >= SizeOf(TsVGAPal) then New(ColorMap)
      else Raise(E,2);

      MaxColor  := (1 shl (BitpPix * npanel));
      buflength := MaxColor * 3;

      if ((pcxHeader^.BitPerPix = 2) and (pcxHeader^.nPlanes = 4)) or
         (pcxHeader^.BitPerPix in [8,7,6,5]) then
        VGAPalCode := 12
      else
        VGAPalCode := 0;

      TDosStream.Seek(RasterDataPtr);

      if MaxAvail >= SizeOf(TsPCXBuf) then New(Bff)
      else Raise(E,2);

      BffPtr   := 0;
{
      BffCount := 0;
}
    end
  else
    begin
      if Status = stOk then Status := stError;
      case E.Result of

        1 : ErrorInfo := CantOpenFile;

        2 : ErrorInfo := NoMemory;

        3 : ErrorInfo := WriteFileError;

      end;
    end;
END;

{ *********************************************************************** }
procedure TsPCX.ReadLine(var BitMap);

var
  dest       : array[0..639] of byte;

  NCount     : word;

  pos        : longint;
  E          : Exception;
  Size       : longint;

BEGIN
  if EverythingOk(E) then
    begin
      BufP := @BitMap;
      Size := TDosStream.GetSize;

      if (BffCount < BlockSize) and (BffPtr >= BffCount) then
        FillChar(BufP^,LinByteCount,0)
      else
        begin
          if (pcxHeader^.nPlanes = 4) and (pcxHeader^.BitPerPix = 2) then
              begin
                TsPCX.PCXLineUnPacker(Bff^[BffPtr],dest,LinByteCount,NCount);
                inc(BffPtr,NCount);
                Conv4Plane2Byte(Dest,BufP^,LinByteCount);
              end
          else
            if (pcxHeader^.BitPerPix = 4) and (pcxHeader^.nPlanes = 1) then
              begin
                With pcxHeader^ do
                  begin
                    TsPCX.PCXLineUnPacker(Bff^[BffPtr],Dest,LinByteCount,NCount);
                    inc(BffPtr,NCount);

                    if BytePerLin = (Right - Left + 2) shr 1 then
                      ConvSemiByte4Plane(Dest,BufP^,BytePerLin)
                    else
                      ConvByte4Plane(Dest,BufP^,BytePerLin);
                  end;
              end
            else
              begin
                TsPCX.PCXLineUnPacker(Bff^[BffPtr],BufP^,LinByteCount,NCount);
                inc(BffPtr,NCount);
              end;

          if BffCount = BlockSize then
            begin
              pos := BffCount - BffPtr;

              if pos < 640 then
                begin
                  move(Bff^[BffPtr],Bff^[0],pos);
                  BffPtr := Word(pos);

                  pos := Size - TDosStream.GetPos ;

                  if pos < (BlockSize - BffPtr) then
                    begin
                      BffCount := pos + BffPtr;
                      TDosStream.Read(Bff^[BffPtr],pos);
                      if Status  <> stOk then Raise(E,1);
                      FillChar(Bff^[BffCount],SizeOf(TsPCXBuf)-BffCount,0);
                    end
                  else
                    begin
                      TDosStream.Read(Bff^[BffPtr],BlockSize - BffPtr);
                      if Status  <> stOk then Raise(E,1);
                      BffCount := BlockSize;
                    end;

                  BffPtr := 0;
                end;
            end;
        end;
    end
  else
    begin
      if Status = stOk then Status := stError;
      case E.Result of
        1 : ErrorInfo := ReadFileError;
      end;
    end;
END;

{ *********************************************************************** }
procedure TsPCX.WriteLine(var BitMap);
var
  dest     : array[0..2047] of byte;
  NCount   : word;
  pos      : longint;
  E        : Exception;

BEGIN
  if EverythingOk(E) then
    begin
      BufP := nil;
      BufP := @BitMap;

      if (pcxHeader^.nPlanes = 4) and (pcxHeader^.BitPerPix = 2) then
        begin
          Conv2Byte4Plane(BufP^,Dest,LinByteCount);
          TsPCX.PCXLinePacker(Dest,Bff^[BffPtr],LinByteCount,NCount);
          inc(BffPtr,NCount);
        end
      else
        if (pcxHeader^.BitPerPix = 4) and (pcxHeader^.nPlanes = 1) then
          begin
            With pcxHeader^ do
              begin
                if BytePerLin = (Right - Left + 2) shr 1 then
                  Conv4PlaneSemiByte(BufP^,Dest,BytePerLin shl 1)
                else
                  Conv4PlaneByte(BufP^,Dest,BytePerLin);
              end;
            TsPCX.PCXLinePacker(Dest,Bff^[BffPtr],LinByteCount,NCount);
            inc(BffPtr,NCount);
          end
        else
          begin
            TsPCX.PCXLinePacker(BufP^,Bff^[BffPtr],LinByteCount,NCount);
            inc(BffPtr,NCount);
          end;

      pos := BlockSize - BffPtr;
      if pos < 640 then
        begin
          TDosStream.Write(Bff^[0],BffPtr);
          if Status  <> stOk then Raise(E,1);
          BffPtr := 0;
        end;
    end
  else
    begin
      if Status = stOk then Status := stError;
      case E.Result of
        1 : ErrorInfo := WriteFileError;
      end;
    end;
END;

{ *********************************************************************** }
procedure TsPCX.GetColorFromPalette(Num:byte;var R,G,B : byte);
BEGIN
if Num > pred(MaxColor) then
  begin
    if Status = stOk then Status := stError;
    ErrorInfo := PaletteError;
  end
else
  begin
    R := ColorMap^.R[Num];
    G := ColorMap^.G[Num];
    B := ColorMap^.B[Num];
  end;
END;

{ *********************************************************************** }
procedure TsPCX.SetColorInPalette(Num,R,G,B : byte);
BEGIN
if Num > pred(MaxColor) then
  begin
    if Status = stOk then Status := stError;
    ErrorInfo := PaletteError;
  end
else
  begin
    ColorMap^.R[Num] := R;
    ColorMap^.G[Num] := G;
    ColorMap^.B[Num] := B;
  end;
END;

{ *********************************************************************** }
function  TsPCX.PicWidth : word;
BEGIN
  PicWidth := pcxHeader^.Right - pcxHeader^.Left + 1;
END;

{ *********************************************************************** }
function  TsPCX.PicHeight : word;
BEGIN
  PicHeight := pcxHeader^.Bottom - pcxHeader^.Top + 1;
END;

{ ********************************************************************* }
function  TsPCX.GetMaxColor : byte;
BEGIN
  GetMaxColor := pred(MaxColor);
END;

{ *********************************************************************** }
destructor TsPCX.Done;
const
  EGAPlt : PsPal16 = nil;
var
  buflength  : word;
  i          : byte;
  E          : Exception;

BEGIN
if Mode = mCreate then
  if EverythingOk(E) then
    begin
      if BffPtr <> 0 then
        begin
          TDosStream.Write(Bff^[0],BffPtr);
          if Status  <> stOk then Raise(E,1);
        end;
      buflength := MaxColor * 3;
      if VGAPalCode = 0 then
        begin
          if MaxAvail >= SizeOf(TsPal16)
            then New(EGAPlt);

          for i := 0 to pred(MaxColor) do
            begin
              EGAPlt^[i,0] := ColorMap^.R[i];
              EGAPlt^[i,1] := ColorMap^.G[i];
              EGAPlt^[i,2] := ColorMap^.B[i];
            end;

          TDosStream.Seek(16);
          TDosStream.Write(EGAPlt^,buflength);
          if Status  <> stOk then Raise(E,1);

          Dispose(EGAPlt);
          EGAPlt := nil;
        end
      else
        begin
          VGAPalCode := 12;
          TDosStream.Write(VGAPalCode,1);
          if Status  <> stOk then Raise(E,1);

          if MaxAvail >= SizeOf(TsVGAMap)
            then New(VGAMapPtr)
          else Raise(E,2);

          for i := 0 to pred(MaxColor) do
            begin
              VGAMapPtr^[i,0] := ColorMap^.R[i];
              VGAMapPtr^[i,1] := ColorMap^.G[i];
              VGAMapPtr^[i,2] := ColorMap^.B[i];
            end;
          TDosStream.Write(VGAMapPtr^,buflength);
          if Status  <> stOk then Raise(E,1);

        end;
      end
    else
      begin
        if Status = stOk then Status := stError;
        case E.Result of

          1 : ErrorInfo := WriteFileError;

          2 : ErrorInfo := NoMemory;

        end;
      end;

  TDosStream.Done;

  if EGAPlt <> nil then
    begin
      Dispose(EGAPlt);
      EGAPlt := nil;
    end;
  if VGAMapPtr <> nil then
    begin
     Dispose(VGAMapPtr);
      VGAMapPtr := nil;
    end;

  if ColorMap <> nil then
    begin
      Dispose(ColorMap);
      ColorMap := nil;
    end;

  if pcxHeader <> nil then
    begin
      Dispose(pcxHeader);
      pcxHeader := nil;
    end;

  if Bff <> nil then
    begin
      Dispose(Bff);
      Bff := nil;
    end;

END;

{ *********************************************************************** }
procedure TsPCX.PCXLinePacker(var SourceBuf,DestBuf; var BpLine,Count : word);
var
   DestIndex : word;
BEGIN
  asm
        push    ds
        cld
        lds     si, BpLine
        mov     cx, [ds:si]
        lds     si, SourceBuf
        les     di, DestBuf
        mov     DestIndex, di
        mov     dx, 01h
        lodsb
        dec     cx
        mov     bl,al
@L1:
        lodsb
        dec     cx
        cmp     bl, al
        jne     @L2
        cmp     dl, 03Fh
        jae     @L2
        jcxz    @L2
        inc     dl
        jmp     @L1
@L2:
        mov     ah, al
        cmp     bl, 0C0h
        jae     @L3
        cmp     dl, 1
        je      @L4
@L3:
        or      dl, 0C0h
        mov     al, dl
        stosb
@L4:
        mov     al, bl
        stosb
        jcxz    @L5
        mov     bl, ah
        mov     dl, 1
        jmp     @L1
@L5:
        cmp     ah, 0C0h
        jb      @L6
        mov     al, 0C1h
        stosb

@L6:    mov     al, ah
        stosb

        sub     di, DestIndex
        lds     si, Count
        mov     [ds : si], di
        pop     ds
  end;
END;

{ *********************************************************************** }
procedure TsPCX.PCXLineUnPacker(var SourceBuf,DestBuf; var BpLine,Count : word);
var
  SourceIndex : word;
BEGIN
  asm
         push   ds
         cld
         les   di, Self
         mov   cl, es:[di].TsPCX.Rept
         mov   al, es:[di].TsPCX.Res
         mov   es:[di].TsPCX.Rept, 0
         mov   es:[di].TsPCX.Res, 0
         les   di, DestBuf
         lds   si, BpLine
         mov   dx, [ds:si]
         xor   ch, ch
         jcxz  @Prg
         sub   dx, cx
         rep   stosb
@Prg:
         lds    si, SourceBuf
         mov    SourceIndex, si
@Loop:
         lodsb
         mov    bl,al
         and    bl, 0C0h
         cmp    bl, 0C0h
         jne    @Else
         mov    cl, al
         and    cl, 3Fh
         lodsb
         jmp    @EndIf
@Else:
         mov    cl, 1
@EndIf:
         cmp    dx, cx
         jl     @Men
         sub    dx, cx
         rep    stosb
         cmp    dx, 0
         jnz    @Loop
         xor    bx, bx
         jmp    @Norm
@Men:
         mov    bx, cx
         sub    bx, dx
         mov    cx, dx
         rep    stosb
@Norm:
         sub    si, SourceIndex
         les    di, Count
         mov    [es: di], si
         cmp    bl , 0
         je     @End
         les    di, Self
         mov    es:[di].TsPCX.Rept, bl
         mov    es:[di].TsPCX.Res, al
@End:
         pop    ds
  end;
END;

{ ********************************************************************** }
{ ********************************************************************** }

procedure TsTIFF.SwapWord(var W: word);

BEGIN
  W:= Swap(W);
END;

{ ********************************************************************** }
procedure TsTIFF.SwapLong(var L: longint);

var WW : array[1..2] of word absolute L;
    W  : word;

BEGIN
  SwapWord(WW[1]);
  SwapWord(WW[2]);
  W:= WW[1];
  WW[1]:= WW[2];
  WW[2]:= W;
END;

{ ********************************************************************** }
procedure TsTIFF.ReadNextDir(var NextDir: longint);

const
  DirPtr  : ^DirArray    = nil;
  TagsDir : PsTagsDirPtr = nil;
  WOffs   : ^WOffsets    = nil;

var
  DirCount: word;
  DirSize : word;

  OffsSize: word;

  i,j     : word;
  E          : Exception;

begin
if EverythingOk(E) then
  begin
    with TiffHeader^ do
      begin
        FirstDir^.StripOffsets    := nil;
        FirstDir^.StripByteCounts := nil;
        FirstDir := nil;
        TBufStream.Seek(NextDir);
        TBufStream.Read(DirCount, SizeOf(word));
        if Status  <> stOk then Raise(E,1);

        if ByteOrder = Motorolla then
          SwapWord(DirCount);

        DirSize:= DirCount*SizeOf(TsTagRec);
        GetMem(DirPtr, DirSize);

        TBufStream.Read(DirPtr^, DirSize);
        if Status  <> stOk then
          begin
            FreeMem(DirPtr, DirSize);
            DirPtr := nil;
            Raise(E,1);
          end;
        TBufStream.Read(NextDir, SizeOf(NextDir));
        if Status  <> stOk then
          begin
            FreeMem(DirPtr, DirSize);
            DirPtr := nil;
            Raise(E,1);
          end;

        if ByteOrder = Motorolla then
          begin
            SwapLong(NextDir);
            for i:= 1 to DirCount do
            with DirPtr^[i] do
             begin
               SwapWord(TagType);
               SwapWord(DataType);
               SwapLong(Length);
               if (DataType = 4) or (Length > 1) then
                  SwapLong(DataOffs)
               else if DataType = 3 then
               begin
                 SwapWord(Value1);
                 SwapWord(Value2);
               end;
             end;
          end;

        new(TagsDir);
        with TagsDir^ do
          begin { Установить значения полей по умолчанию }
            SubfileType:= Full;
            Width:= 0;
            Height:= 0;

            Photometric:= WhiteToBlack;
            Compression:= NoCompression;
            FillOrder:= LeftToRight;
            PlanarConfig:= Pixel;

            SamplesPerPixel:= 1;
            BitsPerSample:= 0;
            MinSampleValue:= 0;
            MaxSampleValue:= $FFFF;

            StripCount:= 0;
            RowsPerStrip:= $FFFFFFFF;
            StripOffsets:= nil;
            StripByteCounts:= nil;
          end;

        for i:= 1 to DirCount do with TagsDir^, DirPtr^[i] do
        case TagType of

          255: case Value1 of
                 1: SubfileType:= Full;
                 2: SubfileType:= Reduced;
                 3: SubfileType:= MultiPage;
               else
                 Raise(E,2);
               end;

          256: begin
                 Width:= Value1;
                 TiffInfoPtr^.Width := Value1;
               end;

          257: begin
                 Height:= Value1;
                 TiffInfoPtr^.Height := Value1;
               end;

          258: begin
                 BitsPerSample:= Value1;
                 TiffInfoPtr^.ColorRes := Value1;
               end;

          259: case Value1 of
                 1: begin
                      Compression:= NoCompression;
                      TiffInfoPtr^.Compress := NoCompression;
                    end;
                 2: begin
                      Compression:= ModifiedCCITT;
                      TiffInfoPtr^.Compress := ModifiedCCITT;
                    end;
               else
                 Raise(E,2);
               end;

          262: case Value1 of
                 0: begin
                      Photometric:= WhiteToBlack;
                      TiffInfoPtr^.ColorType := WhiteToBlack;
                    end;
                 1: begin
                      Photometric:= BlackToWhite;
                      TiffInfoPtr^.ColorType := BlackToWhite;
                    end;
                 2: begin
                      Photometric:= RGB;
                      TiffInfoPtr^.ColorType := RGB;
                    end;
               else
                 Raise(E,2);
               end;

          266: case Value1 of
                 1: FillOrder:= LeftToRight;
                 2: FillOrder:= RightToLeft;
               else
                 Raise(E,2);
               end;

          273: begin
                 if (StripOffsets <> nil) or (Length = 0) or
                    ((StripCount <> 0) and (StripCount <> Length))
                 then Raise(E,2);

                 StripCount:= Length;

                 if DataType = 3 then
                   begin
                     OffsSize:= StripCount*SizeOf(word);
                     GetMem(WOffs, OffsSize);
                     if StripCount = 1 then
                       WOffs^[1]:= Value1
                     else
                       begin
                         TBufStream.Seek(DataOffs);
                         TBufStream.Read(WOffs^,OffsSize);
                         if Status  <> stOk then
                           begin
                             FreeMem(WOffs, OffsSize);
                             WOffs := nil;
                             Raise(E,1);
                           end;
                       end;

                     GetMem(StripOffsets, StripCount*SizeOf(longint));
                     for j:= 1 to StripCount do
                       begin
                         if ByteOrder = Motorolla then
                           SwapWord(WOffs^[j]);
                         StripOffsets^[j]:= WOffs^[j];
                       end;

                     FreeMem(WOffs, OffsSize);
                   end
                 else
                   begin
                     OffsSize:= StripCount*SizeOf(longint);
                     GetMem(StripOffsets, OffsSize);

                     if StripCount = 1 then
                        StripOffsets^[1]:= DataOffs
                     else
                       begin
                         TBufStream.Seek(DataOffs);
                         TBufStream.Read(StripOffsets^, OffsSize);
                         if Status  <> stOk then
                           begin
                             FreeMem(StripOffsets, OffsSize);
                             StripOffsets := nil;
                             Raise(E,1);
                           end;

                         if ByteOrder = Motorolla then
                           for j:= 1 to StripCount do
                             SwapLong(StripOffsets^[j]);
                        end;
                   end
               end;

          277: begin
                 SamplesPerPixel:= Value1;
                 TiffInfoPtr^.BitPerPix := Value1 * TiffInfoPtr^.ColorRes;
               end;

          278: begin
                 if DataType = 3 then
                   RowsPerStrip:= Value1
                 else
                   RowsPerStrip:= DataOffs;
               end;

          279: begin
                 if (StripByteCounts <> nil) or (Length = 0) or
                    ((StripCount <> 0) and (StripCount <> Length))
                 then Raise(E,2);

                 StripCount:= Length;

                 OffsSize:= StripCount*SizeOf(longint);
                 GetMem(StripByteCounts, OffsSize);
                 if StripCount = 1 then
                   StripByteCounts^[1]:= DataOffs
                 else
                   begin
                     TBufStream.Seek(DataOffs);
                     TBufStream.Read(StripByteCounts^, OffsSize);
                     if Status  <> stOk then
                       begin
                         FreeMem(StripOffsets, OffsSize);
                         StripOffsets := nil;
                         Raise(E,1);
                       end;

                     if ByteOrder = Motorolla then
                     for j:= 1 to StripCount do
                       SwapLong(StripByteCounts^[j]);
                   end;
               end;

          280: MinSampleValue:= Value1;

          281: MaxSampleValue:= Value1;

          284: if Value1 = 1 then
                  PlanarConfig:= Pixel
               else if Value1 = 2 then
                 PlanarConfig:= Plane
               else begin
                 Raise(E,2);
               end;

        end;

        with TagsDir^ do
          begin { Установить значения полей по умолчанию }
            if (Width = 0) or (Height = 0) or
               (BitsPerSample = 0) or
               (StripOffsets = nil)
            then Raise(E,2);

            if RowsPerStrip = $FFFFFFFF then
               RowsPerStrip:= Height;

            if StripCount <> (Height + RowsPerStrip - 1) div RowsPerStrip then
              Raise(E,2);

            if (Photometric = RGB) or (Compression <> NoCompression) or
               (FillOrder <> LeftToRight) or (SamplesPerPixel <> 1) or
               (BitsPerSample > 8) or (BitsPerSample in [2,3]) or
               (MinSampleValue > MaxSampleValue) or (MaxSampleValue > 255)
            then Raise(E,2);

          end;

        if LastDir = nil then
          begin
            FirstDir:= TagsDir;
            LastDir := TagsDir;
          end
        else
          LastDir^.Next:= TagsDir;

        TagsDir^.Next:= nil;

        if DirPtr <> nil then
          begin
            FreeMem(DirPtr, DirSize);
            DirPtr := nil;
          end;
      end;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := ReadFileError;

      2 : ErrorInfo := InvalidFormat;

    end;

    if TagsDir^.StripOffsets <> nil then
      begin
        FreeMem(TagsDir^.StripOffsets, TagsDir^.StripCount*SizeOf(longint));
        TagsDir^.StripOffsets := nil;
      end;

    if TagsDir^.StripByteCounts <> nil then
      begin
        FreeMem(TagsDir^.StripByteCounts, TagsDir^.StripCount*SizeOf(longint));
        TagsDir^.StripByteCounts := nil;
      end;

    if TagsDir <> nil then
      begin
        dispose(TagsDir);
        TagsDir := nil;
      end;

    if DirPtr <> nil then
      begin
        FreeMem(DirPtr, DirSize);
        DirPtr := nil;
      end;

  end;

end;

{ ********************************************************************** }
procedure TsTIFF.WriteDir(ImageWidth, ImageHeight: word;
                      PixelSize: byte;
                      var FreeAddr: longint);

const
  TagsInDir = 11;
  DirPtr  : ^DirArray    = nil;
  TagsDir : PsTagsDirPtr = nil;
  Buff    : PsByte       = nil;

var
  DirCount: word;
  DirSize : word;
  i       : word;
  Offsets : longint;
  RowLen  : word;
  Counts  : longint;
  NextDir : longint;
  E          : Exception;


BEGIN
if EverythingOk(E) then
  begin
    with TiffHeader^ do
      begin
        DirCount:= TagsInDir;
        DirSize:= DirCount*SizeOf(TsTagRec);

        if ByteOrder = Motorolla then
           SwapWord(DirCount);

        inc(FreeAddr, SizeOf(word));

        TBufStream.Write(DirCount, SizeOf(word));
          if Status <> stOk then Raise(E,1);

        GetMem(DirPtr, DirSize);

        inc(FreeAddr, DirSize + SizeOf(longint));

        new(TagsDir);
        with TagsDir^ do
          begin
            SubfileType:= Full;
            Width:= ImageWidth;
            TiffInfoPtr^.Width := ImageWidth;

            Height:= ImageHeight;
            TiffInfoPtr^.Height := ImageHeight;

            Photometric:= BlackToWhite;
            TiffInfoPtr^.ColorType := BlackToWhite;

            Compression:= NoCompression;
            TiffInfoPtr^.Compress := NoCompression;

            FillOrder:= LeftToRight;
            PlanarConfig:= Pixel;

            SamplesPerPixel:= 1;
            BitsPerSample:= PixelSize;
            TiffInfoPtr^.ColorRes := BitsPerSample;
            TiffInfoPtr^.BitPerPix := BitsPerSample * SamplesPerPixel;

            MinSampleValue:= 0;
            MaxSampleValue:= ($0001 shl PixelSize) - 1;

            RowLen:= (Width*BitsPerSample + 7) div 8;

            StripCount:= Height;
            RowsPerStrip:= 1;

            GetMem(StripOffsets, StripCount*SizeOf(longint));
            GetMem(StripByteCounts, StripCount*SizeOf(longint));

            for i:= 1 to StripCount do
              StripByteCounts^[i]:= RowLen;

            if StripCount = 1 then
              begin
                Offsets:= FreeAddr;
                Counts:= StripByteCounts^[1];
              end
            else
              begin
                Offsets:= FreeAddr;
                inc(FreeAddr, StripCount*SizeOf(longint));
                Counts:= FreeAddr;
                inc(FreeAddr, StripCount*SizeOf(longint));
              end;

            Next:= nil;
          end;

        with TagsDir^ do
          begin
            with DirPtr^[1] do
              begin
                TagType:= 255;
                DataType:= 3;
                Length:= 1;
                case SubfileType of
                  Full     : Value1:= 1;
                  Reduced  : Value1:= 2;
                  MultiPage: Value1:= 3;
                end;
                Value2:= 0;
              end;

            with DirPtr^[2] do
              begin
                TagType:= 256;
                DataType:= 3;
                Length:= 1;
                Value1:= Width;
                Value2:= 0;
              end;

            with DirPtr^[3] do
              begin
                TagType:= 257;
                DataType:= 3;
                Length:= 1;
                Value1:= Height;
                Value2:= 0;
              end;

            with DirPtr^[4] do
              begin
                TagType:= 258;
                DataType:= 3;
                Length:= 1;
                Value1:= BitsPerSample;
                Value2:= 0;
              end;

            with DirPtr^[5] do
              begin
                TagType:= 262;
                DataType:= 3;
                Length:= 1;
                case Photometric of
                  WhiteToBlack: Value1:= 0;
                  BlackToWhite: Value1:= 1;
                  RGB         : Value1:= 2;
                end;
                Value2:= 0;
              end;

            with DirPtr^[6] do
              begin
                TagType:= 273;
                DataType:= 4;
                Length:= StripCount;
                DataOffs:= Offsets;
              end;

            with DirPtr^[7] do
              begin
                TagType:= 277;
                DataType:= 3;
                Length:= 1;
                Value1:= SamplesPerPixel;
                Value2:= 0;
              end;

            with DirPtr^[8] do
              begin
                TagType:= 278;
                DataType:= 3;
                Length:= 1;
                Value1:= RowsPerStrip;
                Value2:= 0;
              end;

            with DirPtr^[9] do
              begin
                TagType:= 279;
                DataType:= 4;
                Length:= StripCount;
                DataOffs:= Counts;
              end;

            with DirPtr^[10] do
              begin
                TagType:= 280;
                DataType:= 3;
                Length:= 1;
                Value1:= MinSampleValue;
                Value2:= 0;
              end;

            with DirPtr^[11] do
              begin
                TagType:= 281;
                DataType:= 3;
                Length:= 1;
                Value1:= MaxSampleValue;
                Value2:= 0;
              end;
          end;

        if ByteOrder = Motorolla then
          for i:= 1 to TagsInDir do
          with DirPtr^[i] do
            begin
              SwapWord(TagType);
              if (DataType = 4) or (Length > 1) then
                SwapLong(DataOffs)
              else if DataType = 3 then
                begin
                  SwapWord(Value1);
                  SwapWord(Value2);
                end;
              SwapLong(Length);
              SwapWord(DataType);
            end;

          TBufStream.Write(DirPtr^, DirSize);
          if Status <> stOk then Raise(E,1);

          NextDir:= 0;
          TBufStream.Write(NextDir, SizeOf(NextDir));
          if Status <> stOk then Raise(E,1);

       with TagsDir^ do if StripCount > 1 then
       begin
          for i:= 1 to StripCount do
          begin
             StripOffsets^[i]:= FreeAddr;
             inc(FreeAddr, RowLen);
          end;

          if ByteOrder = Motorolla then
          for i:= 1 to StripCount do
          begin
             SwapLong(StripOffsets^[i]);
             SwapLong(StripByteCounts^[i]);
          end;

          TBufStream.Write(StripOffsets^,StripCount*SizeOf(longint));
            if Status  <> stOk then
              begin
                FreeMem(StripOffsets,StripCount*SizeOf(longint));
                StripOffsets := nil;
                Raise(E,1);
              end;

          TBufStream.Write(StripByteCounts^,StripCount*SizeOf(longint));
            if Status  <> stOk then
              begin
                FreeMem(StripByteCounts,StripCount*SizeOf(longint));
                StripByteCounts := nil;
                Raise(E,1);
              end;

          if ByteOrder = Motorolla then
          for i:= 1 to StripCount do
          begin
             SwapLong(StripOffsets^[i]);
             SwapLong(StripByteCounts^[i]);
          end;
       end;

       GetMem(Buff, RowLen);
       FillChar(Buff^, RowLen, #255);

       for i:= 1 to TagsDir^.StripCount do
         begin
           TBufStream.Write(Buff^, RowLen);
           if Status <> stOk then
             begin
               FreeMem(Buff, RowLen);
               Buff := nil;
               Raise(E,1);
             end;
         end;

       if Buff <> nil then
       begin
         FreeMem(Buff, RowLen);
         Buff := nil;
       end;

       if DirPtr <> nil then
       begin
         FreeMem(DirPtr, DirSize);
         DirPtr := nil;
       end;

       FirstDir:= TagsDir;
       LastDir := TagsDir;

    end;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := WriteFileError;

      2 : ErrorInfo := InvalidFormat;

    end;


    if TagsDir^.StripOffsets <> nil then
      begin
        FreeMem(TagsDir^.StripOffsets, TagsDir^.StripCount*SizeOf(longint));
        TagsDir^.StripOffsets := nil;
      end;

    if TagsDir^.StripByteCounts <> nil then
      begin
        FreeMem(TagsDir^.StripByteCounts, TagsDir^.StripCount*SizeOf(longint));
        TagsDir^.StripByteCounts := nil;
      end;

    if DirPtr <> nil then
      begin
        FreeMem(DirPtr, DirSize);
        DirPtr := nil;
      end;

    if TagsDir <> nil then
      begin
        dispose(TagsDir);
        TagsDir := nil;
      end;
  end;
END;

   {--------------------- Основные процедуры ---------------------}

{ ********************************************************************** }
constructor TsTIFF.Open(FName : FNameStr);

var

  Header  : TsHeaderRec;
  DirOffs : longint;
  E          : Exception;

BEGIN

TBufStream.Init(FName,stOpen,BlockSize);
if EverythingOk(E) then
  begin


    if Status  <> stOk then Raise(E,1);

    if MaxAvail >= SizeOf(TsUserData) then New(TiffInfoPtr)
    else Raise(E,5);

    if MaxAvail >= SizeOf(TsTiffHeaderRec) then New(TiffHeader)
    else Raise(E,5);

    TiffHeader^.FirstDir:= nil;
    TiffHeader^.LastDir := nil;

    with TiffHeader^ do
      begin


        TBufStream.Read(Header, SizeOf(TsHeaderRec));
        if Status <> stOk then Raise(E,2);

        if (UpCase(Header.ByteOrder[1]) <> 'M') and
           (UpCase(Header.ByteOrder[1]) <> 'I') then Raise(E,4);

        if UpCase(Header.ByteOrder[1]) = 'M' then
          begin
            ByteOrder:= Motorolla;
            TiffInfoPtr^.ByteOrder := Motorolla;
            SwapWord(Header.Version);
            SwapLong(Header.FirstDir);
          end
        else
          begin
            ByteOrder:= Intel;
            TiffInfoPtr^.ByteOrder := Intel;
          end;
        Version:= Header.Version;
        DirOffs:= Header.FirstDir;


        while DirOffs <> 0 do
          begin
            ReadNextDir(DirOffs);
            if Status <> stOk then Exit;
          end;
      end;
    Y := 0;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := CantOpenFile;

      2 : ErrorInfo := ReadFileError;

      4 : ErrorInfo := InvalidFormat;

      5 : ErrorInfo := NoMemory;

    end;
  end;
END;

{ ********************************************************************** }
constructor TsTIFF.Create(FName     : FNameStr;
                         Order     : OrderType;
                         ImgWidth  : word;
                         ImgHeight : word;
                         PixelSize : byte);

var
  Header  : TsHeaderRec;
  DirOffs : longint;
  E          : Exception;

BEGIN
TBufStream.Init(FName,stCreate,BlockSize);
if EverythingOk(E) then
  begin

    if Status  <> stOk then Raise(E,1);

    if MaxAvail >= SizeOf(TsUserData) then New(TiffInfoPtr)
    else Raise(E,5);

    if MaxAvail >= SizeOf(TsTiffHeaderRec) then New(TiffHeader)
    else Raise(E,5);

    TiffHeader^.FirstDir:= nil;
    TiffHeader^.LastDir := nil;

    with TiffHeader^ do
      begin
        ByteOrder:= Order;
        TiffInfoPtr^.ByteOrder := Order;
        Version  := 42;
        DirOffs  := SizeOf(Header);

        Header.Version := Version;
        Header.FirstDir:= DirOffs;

        if ByteOrder = Motorolla then
        begin
           Header.ByteOrder:= 'MM';
           SwapWord(Header.Version);
           SwapLong(Header.FirstDir);
        end
        else
           Header.ByteOrder:= 'II';

        TBufStream.Write(Header,SizeOf(TsHeaderRec));
        if Status  <> stOk then Raise(E,3);

        LastDir := nil;
        FirstDir:= nil;

        WriteDir(ImgWidth, ImgHeight, PixelSize, DirOffs);
        if Status <> stOk then Exit;

        Y := 0;
      end;
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      1 : ErrorInfo := CantOpenFile;

      3 : ErrorInfo := WriteFileError;

      4 : ErrorInfo := InvalidFormat;

      5 : ErrorInfo := NoMemory;

    end;
  end
END;

{ ********************************************************************** }
destructor TsTIFF.Done;
var
  TagsDir: PsTagsDirPtr;
BEGIN
if TiffHeader <> nil then
  begin
    with TiffHeader^ do
      begin
        while FirstDir <> nil do
          begin
            TagsDir:= FirstDir^.Next;
            with FirstDir^ do
              begin
                if StripOffsets <> nil then
                  begin
                    FreeMem(StripOffsets, StripCount*SizeOf(longint));
                    StripOffsets := nil;
                  end;
                if StripByteCounts <> nil then
                  begin
                    FreeMem(StripByteCounts, StripCount*SizeOf(longint));
                    StripByteCounts := nil;
                  end;
              end;
           dispose(FirstDir);
            FirstDir:= TagsDir;
          end;
      end;
    dispose(TiffHeader);
    TiffHeader:= nil;
  end;
if TiffInfoPtr <> nil then
  begin
    Dispose(TiffInfoPtr);
    TiffInfoPtr := nil;
  end;
TBufStream.Done;
END;

{ ********************************************************************** }
function TsTIFF.PicWidth : word;
BEGIN
  PicWidth:= TiffHeader^.FirstDir^.Width;
END;

{ ********************************************************************** }
function TsTIFF.PicHeight : word;
BEGIN
  PicHeight:= TiffHeader^.FirstDir^.Height;
END;

{ ********************************************************************** }
procedure TsTIFF.SetYCoord(Ypos : word);
BEGIN
  Y := Ypos;
END;

{ ********************************************************************** }
function  TsTIFF.GetMaxColor : byte;
BEGIN
  GetMaxColor := pred(1 shl TiffInfoPtr^.BitPerPix);
END;
{ ********************************************************************** }
procedure TsTIFF.ReadLine(var BitMap);

const
  DirPtr    : PsTagsDirPtr = nil;
  Buff      : PsByte       = nil;

var

  Strip     : word;
  RowLen    : word;
  RowOffs   : word;
  PBitMap   : PsByte;
  i         : word;
  E          : Exception;

BEGIN
if EverythingOk(E) then
  begin
    DirPtr := TiffHeader^.FirstDir;

    with TiffHeader^, DirPtr^ do
      begin
        if (Y >= Height) then Raise(E,4);

        Strip    := Y div RowsPerStrip + 1;
        RowLen   := (Width*BitsPerSample + 7) shr 3;
        RowOffs  := (Y mod RowsPerStrip)*RowLen;
        PBitMap := @BitMap;

        TBufStream.Seek(StripOffsets^[Strip] + RowOffs);

        if BitsPerSample = 4 then
          begin
            GetMem(Buff,RowLen);
            TBufStream.Read(Buff^, RowLen);
            if Status <> stOk then
              begin
                FreeMem(Buff,RowLen);
                Buff := nil;
                Raise(E,2);
              end;
              ConvSemiByte4Plane(Buff^,PBitMap^,RowLen);
            FreeMem(Buff,RowLen);
            Buff := nil;
          end
        else
          begin
            TBufStream.Read(PBitMap^,RowLen);
            if Status <> stOk then Raise(E,2);
          end;

        if Photometric = WhiteToBlack then
          for i := 0 to pred(RowLen) do PBitMap^[i] := 255 - PBitMap^[i];
      end;
    inc(Y);
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      2 : ErrorInfo := ReadFileError;

      4 : ErrorInfo := InvalidFormat;

    end;

    if DirPtr^.StripOffsets <> nil then
      begin
        FreeMem(DirPtr^.StripOffsets,SizeOf(DirPtr^.StripOffsets^));
        DirPtr^.StripOffsets := nil;
      end;

    if DirPtr^.StripByteCounts <> nil then
      begin
        FreeMem(DirPtr^.StripByteCounts,SizeOf(DirPtr^.StripByteCounts^));
        DirPtr^.StripByteCounts := nil;
      end;
  end;
END;


{ ********************************************************************** }
procedure TsTIFF.WriteLine(var BitMap);

const
  DirPtr: PsTagsDirPtr = nil;
  Buff      : PsByte   = nil;

var

  Strip     : word;
  RowLen    : word;
  RowOffs   : word;
  PBitMap   : PsByte;
  ByteCount : word;

  i         : word;
  E         : Exception;

BEGIN
if EverythingOk(E) then
  begin
    DirPtr := TiffHeader^.FirstDir;
    with TiffHeader^, DirPtr^ do
      begin
        if (Y >= Height) then Raise(E,4);

        Strip     := Y div RowsPerStrip + 1;
        RowLen    := (Width*BitsPerSample + 7) shr 3;
        RowOffs   := (Y mod RowsPerStrip)*RowLen;
        ByteCount := (RowLen + 3) shr 2;
        PBitMap   := @BitMap;

        if Photometric = WhiteToBlack then
          for i := 0 to pred(RowLen) do PBitMap^[i] := 255 - PBitMap^[i];

        TBufStream.Seek(StripOffsets^[Strip] + RowOffs);

        if BitsPerSample = 4 then
          begin
            GetMem(Buff,RowLen);
            Conv4PlaneSemiByte(PBitMap^,Buff^,Width);
            TBufStream.Write(Buff^,RowLen);
            if Status <> stOk then
              begin
                FreeMem(Buff,RowLen);
                Buff := nil;
                Raise(E,2);
              end;
            FreeMem(Buff,RowLen);
            Buff := nil;
          end
        else
          begin
            TBufStream.Write(PBitMap^,RowLen);
            if Status <> stOk then Raise(E,2);
          end;

      end;
    inc(Y);
  end
else
  begin
    if Status = stOk then Status := stError;
    case E.Result of

      3 : ErrorInfo := WriteFileError;

      4 : ErrorInfo := InvalidFormat;

    end;
  end;

END;

END.