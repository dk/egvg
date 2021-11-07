{*******************************************************}
{                                                       }
{      Graphic Vision 2.0 Unit                          }
{      Turbo Pascal Version 6.0&7.0                     }
{      For Real and Protected Mode                      }
{      Copyright (c) 1992-94 ICS Co. Ltd.               }
{                                                       }
{*******************************************************}

unit GrVTypes;

{$F+,O+,X+,V-,R-,I-,S-,L-}
interface

Uses GDI;

type
  WordRec = record
    Lo, Hi: Byte;
  end;

  LongRec = record
    Lo, Hi: Word;
  end;

  PtrRec = record
    Ofs, Seg: Word;
  end;

  PByteArray = ^TByteArray;
  TByteArray = array[0..65534] of Byte;

  PCharArray = ^TCharArray;
  TCharArray = array[0..65534] of Char;

  PWordArray = ^TWordArray;
  TWordArray = array[0..32766] of Word;

  PIntArray = ^TIntArray;
  TIntArray = array[0..32766] of integer;

  PString = ^String;
{$IFNDEF VER70}
  PChar   = PCharArray;
{$ENDIF }
  FNameStr = string[79];

  PCharSet = ^TCharSet;
  TCharSet = set of Char;

  PPoint = ^TPoint;
  TPoint = record
    X, Y: Integer;
  end;

  PPointArray = ^TPointArray;
  TPointArray = Array[1..16382] of TPoint;

  PLineRec = ^TLineRec;
  TLineRec = record
    Y1,Y2 : Integer;
    arrX : PIntArray;
    Next : PLineRec;
  end;

  PMouseMask = ^TMouseMask;
  TMouseMask = record
    SizeX,
    SizeY: Integer;
    HotY,
    HotX: Integer;
    Buf: array[0..255] of Byte;
  end;


const
  MaxViewWidth = 1024;
var
  MaxColors : LongInt absolute GDI.MaxColors;

type
  PaletteType = record
    Size    : Byte;
    Colors  : array[0..15,1..4] of byte;
  end;

{ Направления изменения размера }
const
  gsMove = $00;
  gsLeft = $01;
  gsRight= $02;
  gsUp   = $04;
  gsDown = $08;

const

{ Поддерживаемые видео режимы }
                    { resol  box  resoltn       page addr }
  vmDefault = $00 ;
  vmCGAHi   = $06 ; { 80x25  8x8  640x200 B&W     1  B800 CGA }
  vmHBW80   = $07 ; { 80x25  8x14         mono    1  B000 MDA/Hercules   }
                    { 8       EGA/VGA }
  vmEGALo   = $0D ; { 40x25       320x200 16      8  A000 EGA/VGA }
  vmEGAMed  = $0E ; { 80x25       640x200 16      4  A000 EGA/VGA }
  vmEGAMono = $0F ; { 80x25       640x350 mono    2  A000 EGA/VGA }
  vmEGAHi   = $10 ; { 80x25       640x350 4or16   2  A000 EGA/VGA }
  vmMCGAMono= $11 ; { 80x30       640x480 mono       A000 VGA/MCGA}
  vmVGAColor= $12 ; { 80x30       640x480 16         A000 VGA     }
  vmMCGA    = $13 ; { 40x24       320x200 256        A000 VGA/MCGA}
  vmHGC     = $14 ; { 80x25       720x348 mono       B000 Hercules}
  vmVGA800  = $5b ; { 80x25       720x348 mono       B000 Hercules}
  vmFont8x8 = $0100 ;

const
{ логика рисования }
  dlNormal   =  CopyPut;
  dlOR       =  OrPut;
  dlAND      =  AndPut;
  dlXOR      =  XorPut;
  DrawLogic: byte = dlNormal;

var
  CurrentMode: byte;            { Текущий видео режим      }
  CurrentDisplay: word;         { Текущий дисплей          }
  CurrentPalette: PaletteType;
  HerculesCard: boolean;        { True если Hercules }
  InColorMode: boolean;         { True если используется цветной видео режим }
  CurMouseMask: PMouseMask;     { Текущая маска курсора мыши }
  DrawColor   :word;            {[Lo]->BGColor  [Hi]->FGColor }

const
  iwOk      = $f001;
  iwCancel  = $f002;
  iwNo      = $f003;
  iwHelp    = $f004;
  iwYes     = $f005;
  iwActive  = 1000;
  iwPassive = 2000;
  iwPressed = 3000;
  iwBMP     = 4000;
  iwList    = 5000;

const
{ Стандартные типы заливки }
  fpFillPattern  : TFillPattern = ($55,$aa,$55,$aa,$55,$aa,$55,$aa);
  fpBorlandStyle : TFillPattern = ($ff,$aa,$ff,$aa,$ff,$aa,$ff,$aa);
  fpSamPattern   : TFillPattern = ($88,$54,$22,$45,$88,$15,$22,$51);
  fpCritters     : TFillPattern = (0, 80, 114, 32, 0, 5, 39, 2);
  fpDefpattern   : TFillPattern = ($55,$aa,$55,$aa,$55,$aa,$55,$aa);

const
  { * Константы цветов * }
  Black        : byte =  $00;
  Blue         : byte =  $11;
  Green        : byte =  $22;
  Cyan         : byte =  $33;
  Red          : byte =  $44;
  Magenta      : byte =  $55;
  Brown        : byte =  $66;
  LightGray    : byte =  $77;
  DarkGray     : byte =  $88;
  LightBlue    : byte =  $99;
  LightGreen   : byte =  $aa;
  LightCyan    : byte =  $bb;
  LightRed     : byte =  $cc;
  LightMagenta : byte =  $dd;
  Yellow       : byte =  $ee;
  White        : byte =  $ff;


{ Тип палитры цветов }
type
  PPalette = ^TPalette;
  TPalette = String;

const
{Индексы палитры цветов }
  ciNormalFg = 1;
  ciNormalBg = 2;
  ciSelectFg = 3;
  ciSelectBg = 4;
  ciNormalSc = 5;
  ciSelectSc = 6;
  ciDisabled = 7;
  ciBlockFg  = 5;
  ciBlockBg  = 6;
  ciFocusFg  = 5;
  ciFocusBg  = 6;

{ Палитра цветов программы }
  ColorPalette =
{ 1 DeskTop }
{  1 } #$8f+

{ 2..8  Рамка окна TFrame                  }

{  2 } #$BB+ { Пассивного заголовока текст }
{  3 } #$04+ { Пассивного заголовока фон   }
{  4 } #$FF+ { Активного заголовока  текст }
{  5 } #$7C+ { Активного заголовока  фон   }
{  6 } #$88+ { Рамка пассивная             }
{  7 } #$88+ { Рамка активная              }
{  8 } #$ff+ { Внутренний фон              }

{ 9..15 Меню и строка статуса              }

{  9 } #$00+ { Нормальный текст            }
{ 10 } #$88+ { Нормальный фон              }
{ 11 } #$00+ { Выбранный текст             }
{ 12 } #$66+ { Выбранный фон               }
{ 13 } #$11+ { Нормальный выбор            }
{ 14 } #$11+ { Выбранный выбор             }
{ 15 } #$77+ { Запрещенный текст           }

{ 16..22 Метка, кнопка, кластер, статический текст  }

{ 16 } #$00+ { Нормальный текст            }
{ 17 } #$88+ { Нормальный фон              }
{ 18 } #$ff+ { Выбранный текст             }
{ 19 } #$88+ { Выбранный фон               }
{ 20 } #$11+ { Нормальный выбор            }
{ 21 } #$aa+ { Текст по умолчанию          }
{ 22 } #$77+ { Запрещенный текст           }

{ 23..29 Строка ввода и TMemo              }

{ 23 } #$00+ { Нормальный текст            }
{ 24 } #$ee+ { Нормальный фон              }
{ 25 } #$00+ { Выбранный текст             }
{ 26 } #$FF+ { Выбранный фон               }
{ 27 } #$00+ { Отмеченного блока текст     }
{ 28 } #$88+ { Отмеченного блока фон       }
{ 29 } #$00+ { Запрещенный текст           }

{ 30..35 Редактор и просмотр текста        }

{ 30 } #$00+ { Нормальный текст            }
{ 31 } #$ee+ { Нормальный фон              }
{ 32 } #$00+ { Выбранный текст             }
{ 33 } #$FF+ { Выбранный фон               }
{ 34 } #$00+ { Отмеченного блока текст     }
{ 35 } #$88+ { Отмеченного блока фон       }

{ 36..42 Списoк                            }

{ 36 } #$00+ { Нормальный текст            }
{ 37 } #$88+ { Нормальный фон              }
{ 38 } #$cc+ { Выбранный текст             }
{ 39 } #$88+ { Выбранный фон               }
{ 40 } #$ff+ { Сфокусированный текст       }
{ 41 } #$66+ { Сфокусированный фон         }
{ 42 } #$00+ { Запрещенный текст           }

{ 43..45 Скроллер                          }

{ 43 } #$ee+ { Полоса пассивная            }
{ 44 } #$88+ { Полоса активная             }
{ 45 } #$77+ { Полоса запрещенная          }
{ 46 } #$ff;
const
  MonoPalette =
{ 1 }   #$ff+
{ 2 }   #$0f#$0f#$F0#$f0#$f0#$f0+
{ 8 }   #$f0#$f0#$f0#$0f#$0f#$f0#$0f#$00 +
{16 }   #$0f#$0F#$0f#$0f#$f0#$0f#$f0#$00 +
{24 }   #$f0#$f0#$f0#$0f#$0f#$f0#$0f#$00 +
{32 }   #$f0#$f0#$f0#$0f#$0f#$f0#$f0#$f0 +
{40 }   #$f0#$0f#$0f#$0F#$f0#$f0;

{ Атрибут не найденного цвета}
  ErrorAttr: Byte = $CF;


const
  AppPalette: TPalette = ColorPalette;

{ Константы размеров }
var
  smWScreen: Word absolute GDI.MaximalX;
  smHScreen: Word absolute GDI.MaximalY;

const
  smMinWinSize: TPoint = (X: 64; Y: 26); { Минимальные размеры окна }
  smWHFrame:    Byte = 4;  { Ширина/Высота рамки окна }
  smWHDlgFrame: Byte = 6;  { Ширина/Высота рамки диалогового окна }
  smWHScroller: Byte = 18; { Ширина/Высота полосы скроллера }
  smWSysButton: Byte = 19; { Ширина системной кнопки окна }
  smHSysButton: Byte = 18; { Высота системной кнопки окна }
  smHMenu:      Byte = 17; { Высота меню }
  smXMouseStep: Byte = 2;  { Дискретность перемещения мыши по X }
  smYMouseStep: Byte = 2;  { Дискретность перемещения мыши по Y }
  smTabSize:    Word = $40;{ Размер табуляции в пикселях }
  smIndent :    Byte = 2;  { Размер отступа слева и справа в строке ввода}
  smHCluster :  Byte = 16; { Высота системной кнопки кластера}
  smAspectRatio:Real = 1.0;

  WinPalette : PaletteType =
     (Size:16; Colors:(
       ( $00, $00, $00, 0  ),     {0}
       ( $80, $00, $00, 4  ),     {1}
       ( $00, $80, $00, 2  ),     {2}
       ( $80, $80, $00, 6  ),     {3}
       ( $00, $00, $80, 1  ),     {4}
       ( $80, $00, $80, 5  ),     {5}
       ( $00, $c0, $c0, 3  ),     {6}
       ( $80, $80, $80, 56 ),     {7}
       ( $C0, $C0, $C0, 7  ),     {8}
       ( $FF, $00, $00, 60 ),     {9}
       ( $00, $FF, $00, 58 ),     {10}
       ( $FF, $FF, $00, 62 ),     {11}
       ( $00, $00, $FF, 57 ),     {12}
       ( $FF, $00, $FF, 61 ),     {13}
       ( $C0, $DC, $C0, 59 ),     {14}
       ( $FF, $FF, $FF, 63 )) );  {15}

  DefaultPalette : PaletteType =
     (Size:16; Colors:(
       ( $00, $00, $00, 0  ),     {0}
       ( $00, $00, $80, 1  ),     {1}
       ( $00, $80, $00, 2  ),     {2}
       ( $80, $80, $00, 3  ),     {3}
       ( $80, $00, $00, 4  ),     {4}
       ( $80, $00, $80, 5  ),     {5}
       ( $00, $80, $80, 20 ),     {6}
       ( $C0, $C0, $C0, 7  ),     {7}
       ( $80, $80, $80, 56 ),     {8}
       ( $00, $00, $F0, 57 ),     {9}
       ( $00, $FF, $00, 58 ),     {10}
       ( $FF, $FF, $00, 59 ),     {11}
       ( $FF, $00, $00, 60 ),     {12}
       ( $FF, $00, $FF, 61 ),     {13}
       ( $00, $FF, $FF, 62 ),     {14}
       ( $FF, $FF, $FF, 63 )) );  {15}


function LongMul(X, Y: Integer): Longint;
inline($5A/$58/$F7/$EA);

function LongDiv(X: Longint; Y: Integer): Integer;
inline($59/$58/$5A/$F7/$F9);

{$IFNDEF VER70}
const
  SegA000 = $A000;
  Seg0040 = $0040;
{$ENDIF }

const
  sfDrawAll = $FFFF;
  ofWhiteLeft = 0;

implementation

Begin
  CurrentPalette := WinPalette;
end.