{$G+,S-,F+}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : Graphics Device Interface                            █
  █ Description : Virtual Graphics Common Routines                     █
  █ Author      : Dmitry Karasik                                       █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 26-DEC-1996                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█

}

Unit GDI;

Interface

Uses  Objects, Streams, Memory;

type
  PImage = ^TImage;
  TImage = Record
    Check: word;
    Buffer: byte; { Some buffer (length unknown during compilation) }
  end;
  PSImage=^TSImage;
  TSImage = Record
    Check, Y, NBP, X : Word;
    case Byte of
    0:(Data:array[0..0] of Byte);
    1:(PS : PStream; Buffer : PByteArray);
  End;
  PCoordImage = ^TCoordImage;
  TCoordImage = record
    X, Y: word;
    Buffer: array[0..0] of byte; { Some buffer (length unknown during compilation) }
  end;
  TGraphColor = Record
    R, G, B: Byte;
  end;
  PGraphPalette = ^TGraphPalette;
  TGraphPalette = array [0..15] of TGraphColor;
  PColorRef = ^TColorRef;
  TColorRef = array [0..255] of Byte;
  PVGARegister = ^TVGARegister;
  PVGAPalette  = ^TVGAPalette;
  TFillPattern = array[0..7] of Byte;
  TVGARegister = array[1..3] of Byte;
  TVGAPalette  = array[0..255] of TVGARegister;
  TEnterGraphics = function : boolean;
  TLeaveGraphics = procedure;

  {main paint structure}
  PPaintInfo = ^TPaintInfo;
  TPaintInfo = Record
    Fore, Back : LongInt;                   {fore/backing colors}
    Operation  : Byte;                      {drawing logic mode, 0..21}
    LinePattern: Word;                      {16x1 pattern for lines - psXXXX}
    LineStyle  : Byte;                      {figure filling style   - lsXXXX}
    LineWidth  : Byte;                      {for FillEllipse/Line   - lwXXXX}
    BitMap     : PImage;                    {bitmap pattern, dib colors only}
    BitMapOrg  : TPoint;                    {for lsBitmapOrg mode}
    ColorRef   : PColorRef;                 {for lsBitmapXXX modes}
    Pattern    : TFillPattern;              {8x8 pattern for bars}
    ClipRect   : TRect;                     {clipping rectangle}
    Device     : PImage;                    {if to screen should be nil else image}
  End;

  {}
  PFont = ^TFont;
  TFont = Record
    Font       : Byte;                      {font number          }
    Width      : Word;                      {average symbol width }
    Height     : Word;                      {average symbol height}
    GapLength  : Word;                      {space between symbols}
    Escapement : Word;                      {direction in degrees }
    Italic     : Byte;                      {skewing factor       }
    Style      : Word;                      {mixed styles         }
  End;

  PLogPalette = ^TLogPalette;
  TLogPalette = Record
    Mode       : Word;                      {misc. flags}
    Colors     : Word;                      {active colors in palette}
    Palette    : PVGAPalette;               {palette}
    ColorRef   : PColorRef;                 {color reference table}
  End;

  TLinedRect = Object(TRect)
    Function Allowed(var R : TRect) : Boolean;
    Function ContainPel(X, Y : Word) : Boolean;
  End;

  PDriverMethods = ^TDriverMethods;
  TDriverMethods = Record
    _SetPixel          : Procedure (X, Y : Word; Color : LongInt);
    _SetPixelOp        : Procedure (X, Y : Word; Color : LongInt; Operation: Byte; Device : PImage); {*}
    _GetPixel          : Function (X, Y : Word) : LongInt;
    _GetPixelBM        : Function (X, Y : Word; Device : PImage) : LongInt;
    _HLine             : Procedure (X, Y, X2 : Word; Color : LongInt);
    _VLine             : Procedure (X, Y, Y2 : Word; Color : LongInt);
    _DisplayXxY        : Procedure (x, y: integer; BitMap: pointer; Width, Height : Word; Clr : LongInt);
    _DisplayXxYClip    : Procedure (x, y: integer; BitMap: pointer; Msk : Byte; Width, Height : Word; Clr : LongInt);
    _ReadScanLine      : Procedure (X, Y, XLen : Word; Buffer : Pointer);
    _WriteScanLine     : Procedure (X, Y, XLen : Word; Buffer : Pointer);
    _WriteScanLineOp   : Procedure (X, Y, XLen : Word; Buffer : Pointer; BitBlt : Byte);
    _PutBMPPart        : Procedure (Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
    _PutBMPPartOp      : Procedure (Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;Operation: Byte;
                                  ColorRef : PColorRef);
    _HLineStyleOp      : Procedure (X1, Y, X2 : Integer; var T : TPaintInfo);
    _DisplayXx8Op      : Procedure (x, y: integer; BitMap: pointer; Msk : Byte; Height : Word; var T : TPaintInfo);
    _QuickSave         : Procedure (xxx, yyy, xLen, yLen: integer);
    _QuickRestore      : Procedure (xxx, yyy, xLen, yLen: integer);
    _DirectMousePut    : Procedure (var Buf; x, y, xFrom, yFrom, Width, Height: integer);
    _VLineStyleT       : Procedure (x, y1, y2, Width: integer; Clr : LongInt; Pattern: word);
    _DirectGetImage    : Procedure (x1, y1, x2, y2 : integer; var BitMap);
    _DirectPutImage    : Procedure (Image: PCoordImage; x, y : integer);
    _PutBufferPart     : Procedure (x1, y1, x2, y2: integer);
    _MapBMPLineRead    : Function  (P : PImage; Y : Word; ColorRef : PColorRef) : PByteArray;
    _Init              : Function : Boolean;
    _Done              : Procedure;
    _StretchDIBitmap   : Procedure (Image, Dest : PImage; xxx, yyy, xFrom, yFrom, xLen, yLen, xDest, yDest : integer;
                                  ColorRef:PColorRef;ClipRect : TRect);
    _ImplantDIBitmap   : Procedure (Image,Dest:PImage;X,Y,xFrom,yFrom,xLen,yLen:Integer;Operation:Byte;ColorRef:PColorRef);
    _SetOutPut         : Procedure (OnBuffer : Boolean);
    _PrepareDrawing    : Procedure;
    _SetUserBitBltProc : Procedure (Action : Pointer);
    _SetColorBitBlt    : Procedure (Color : LongInt; Transparent : Boolean);
    _EMSAdjSelect      : Procedure;
    _DIBAdjSelect      : Procedure;
  End;


  PScreenDriver = ^TScreenDriver;
  TScreenDriver = record
    VirtualScreenStyle : Byte;   { See vssXXXX flags }
    ScreenWidth        : Word;
    ScreenHeight       : Word;
    MaximalX           : Word;
    MaximalY           : Word;
    NumberOfColors     : LongInt;
    ColorShift         : Byte;   { Theoretical limit is 32, for 16 color modes }
    NumPlanes          : Byte;
    GraphPalette       : PGraphPalette;
    EnterGraphics      : TEnterGraphics;
    LeaveGraphics      : TLeaveGraphics;
    Methods            : PDriverMethods;
    ID                 : Word;
    { Following fields are filled by info after initializing graphics }
    BitPlaneSize       : Word;
    HeapPlanes         : Byte;
    Granularity        : LongInt;
    MaxBanks           : Word;
    EMSBanks           : Word;
    NextDriver         : PScreenDriver;
  end;

Const
  _cs_SetPixel          = 0;
  _cs_SetPixelOp        = 1;
  _cs_GetPixel          = 2;
  _cs_GetPixelBM        = 3;
  _cs_HLine             = 4;
  _cs_VLine             = 5;
  _cs_DisplayXxY        = 6;
  _cs_DisplayXxYClip    = 7;
  _cs_ReadScanLine      = 8;
  _cs_WriteScanLine     = 9;
  _cs_WriteScanLineOp   = 10;
  _cs_PutBMPPart        = 11;
  _cs_PutBMPPartOp      = 12;
  _cs_HLineStyleOp      = 13;
  _cs_DisplayXx8Op      = 14;
  _cs_QuickSave         = 15;
  _cs_QuickRestore      = 16;
  _cs_DirectMousePut    = 17;
  _cs_VLineStyleT       = 18;
  _cs_DirectGetImage    = 19;
  _cs_DirectPutImage    = 20;
  _cs_PutBufferPart     = 21;
  _cs_MapBMPLineRead    = 22;
  _cs_Init              = 23;
  _cs_Done              = 24;
  _cs_StretchDIBitmap   = 25;
  _cs_ImplantDIBitmap   = 26;
  _cs_SetOutPut         = 27;
  _cs_PrepareDrawing    = 28;
  _cs_SetUserBitBltProc = 29;
  _cs_SetColorBitBlt    = 30;
  _cs_EMSAdjSelect      = 31;
  _cs_DIBAdjSelect      = 32;


  MaxColors  : LongInt = 256;
  BufGranula = 16384;
  DIBType    : Word    = 256;
  ColorTolerance : Byte = 5; {if less, colors are smarter but
                              time to calculate it greater and v/v}
  SystemColors   : Byte = 16; {you may change it up to 32 and get extra
                               16 colors in grayscale palette}
  ColorIndex     : PLongArray = Nil;

  DIBMethods : Set of Byte = [
    _cs_SetPixelOp,
    _cs_GetPixelBM,
    _cs_ReadScanLine,
    _cs_WriteScanLine,
    _cs_WriteScanLineOp,
    _cs_HLineStyleOp,
    _cs_DisplayXx8Op,
    _cs_MapBMPLineRead,
    _cs_StretchDIBitmap,
    _cs_ImplantDIBitmap,
    _cs_SetUserBitBltProc,
    _cs_SetColorBitBlt,
    _cs_DIBAdjSelect
  ];


{$IFNDEF ShimonColors}

  DefaultGraphPalette : TGraphPalette = (
  (R:0; G:0; B:0), (R:0; G:0; B:120), (R:0; G:128; B:0), (R:140; G:188; B:180),
  (R:128; G:0; B:0), (R:176; G:180; B:184), (R:136; G:8; B:136), (R:192; G:192; B:192),
  (R:128; G:128; B:128), (R:0; G:0; B:252), (R:0; G:252; B:0), (R:0; G:252; B:252),
  (R:252; G:0; B:0), (R:252; G:0; B:252), (R:252; G:252; B:0), (R:252; G:252; B:252));

{$ELSE}
  SepiaGraphPalette : TGraphPalette = (
    (R: 0; G: 0; B: 0),  {black}
    (R: 0; G: 0; B:120),  {blue}
    (R: 0; G:120; B: 0),  {green}
    (R:168; G:148; B:140),  {C1}        {cyan  - modified 1 -------}
    (R:120; G: 0; B: 0),  {red}
    (R:188; G:164; B:160),  {C2}        {magenta - modified 2 -------}
    (R:208; G:184; B:176),  {C3}        {brown - modified 2 --------}
    (R:208; G:188; B:184),  {light gray}
    (R:128; G:116; B:112),  {dark gray}
    (R: 0; G: 0; B:240),  {light blue}
    (R:80; G:160; B:40),  {light green}
    (R:224; G:200; B:192),  {C4}        {light cyan - modified 1 ------}
    (R:240; G: 0; B: 0),  {light red}
    (R:88; G:32;  B:0),   {C5}        {light magenta - modified 2 ----}
    (R:240; G:240; B: 0),  {yellow}
    (R:252; G:232; B:228)   {white}
  );

  CyanGraphPalette : TGraphPalette = (
    (R: 0; G: 0; B: 0),  {black}
    (R: 0; G: 0; B:120),  {blue}
    (R: 0; G:120; B: 0),  {green}
    (R:136; G:164; B:156),  {C1}        {cyan  - modified 1 -------}
    (R:120; G: 0; B: 0),  {red}
    (R:152; G:184; B:176),  {C2}        {magenta - modified 2 -------}
    (R:172; G:204; B:196),  {C3}        {brown - modified 2 --------}
    (R:192; G:192; B:192),  {light gray}
    (R:120; G:120; B:120),  {dark gray}
    (R: 0; G: 0; B:240),  {light blue}
    (R: 20; G:160; B:10),  {light green}
    (R:184; G:220; B:212),  {C4}        {light cyan - modified 1 ------}
    (R:240; G: 0; B: 0),  {light red}
    (R:88; G:32; B: 0),  {C5}        {light magenta - modified 2 ----}
    (R:240; G:240; B: 0),  {yellow}
    (R:252; G:252; B:252)   {white}
  );

  SeaGraphPalette : TGraphPalette = (
    (R: 0; G: 0; B: 0),  {black}
    (R: 0; G: 0; B:120),  {blue}
    (R: 0; G:120; B: 0),  {green}
    (R:140; G:152; B:164),  {C1}        {cyan  - modified 1 -------}
    (R:120; G: 0; B: 0),  {red}
    (R:156; G:172; B:184),  {C2}        {magenta - modified 2 -------}
    (R:172; G:188; B:208),  {C3}        {brown - modified 2 --------}
    (R:184; G:184; B:204),  {light gray}
    (R:112; G:112; B:132),  {dark gray}
    (R: 0; G: 0; B:240),  {light blue}
    (R: 80; G:160; B: 40),  {light green}
    (R:188; G:204; B:224),  {C4}        {light cyan - modified 1 ------}
    (R:240; G: 0; B: 0),  {light red}
    (R:88; G:32; B:0),  {C5}        {light magenta - modified 2 ----}
    (R:240; G:240; B: 0),  {yellow}
    (R:228; G:228; B:244)   {white}
  );
var
  DefaultGraphPalette : TGraphPalette absolute CyanGraphPalette;
{$ENDIF}

var
  MainPalette   : TVGAPalette;
  DriverMethods : TDriverMethods;
  DefaultDriver : PScreenDriver;
  ScreenDrivers : PScreenDriver;
const
  PMainPalette  : PVGAPalette = @MainPalette;


{TPaintInfo specific constants}
Const
  {colors 0-15}
  Black        = 0;
  Blue         = 1;
  Green        = 2;
  Cyan         = 3;
  Red          = 4;
  Gray         = 5;
  Brown        = 6;
  LightGray    = 7;
  DarkGray     = 8;
  LightBlue    = 9;
  LightGreen   = 10;
  LightCyan    = 11;
  LightRed     = 12;
  LightMagenta = 13;
  Yellow       = 14;
  White        = 15;

  {drawing modes - TPaintInfo.Operation}
  CopyPut      = 0;      {dest  = src}
  XorPut       = 1;      {dest ^= src}
  AndPut       = 2;      {dest &= src}
  OrPut        = 3;      {dest |= src}
  NotPut       = 4;      {dest = !src}
  NotBlack     = 5;      {dest = (src <> 0) ? src}
  NotDestXor   = 6;      {dest = (!dest) ^ src}
  NotDestAnd   = 7;      {dest = (!dest) & src}
  NotDestOr    = 8;      {dest = (!dest) | src}
  NotSrcXor    = 9;      {dest ^= !src}
  NotSrcAnd    = 10;     {dest &= !src}
  NotSrcOr     = 11;     {dest |= !src}
  NotXor       = 12;     {dest = !(src ^ dest)}
  NotAnd       = 13;     {dest = !(src & dest)}
  NotOr        = 14;     {dest = !(src | dest)}
  NotBlackXor  = 15;     {dest ^= (src <> 0) ? src}
  NotBlackAnd  = 16;     {dest &= (src <> 0) ? src}
  NotBlackOr   = 17;     {dest |= (src <> 0) ? src}
  NoOper       = 18;     {}
  Blackness    = 19;     {dest = 0}
  Whiteness    = 20;     {dest = 15}
  UserBitBlt   = 21;     {user defined operation for SetUserBitBltProc}

  {line fill styles - TPaintInfo.LineStyle}
  lsLinePattern  = 0;   {use TPaintInfo.LinePattern}
  lsPattern      = 1;   {use TPaintInfo.Pattern}
  lsBitMap8x8    = 2;   {use TPaintInfo.BitMap as 8x8 matrix}
  lsBitMap       = 3;   {use TPaintInfo.BitMap as full wallpaper}
  lsBitMapOrg    = 4;   {use TPaintInfo.BitMap with org = 0,0}

  {line patterns - TPaintInfo.LinePattern}
  psNull      = $0000;      {         }
  psSolid     = $FFFF;      {─────────}
  psDash      = $F0F0;      {- - - - -}
  psDot       = $5555;      {.........}
  psDashDot   = $FAFA;      {-.-.-.-.-}
  psDashDotDot= $EAEA;      {-..-..-..}

  {line widths - TPaintInfo.LineWidth}
  lwHollow     =  0;
  lwThin       =  1;
  lwExtraLight =  2;
  lwLight      =  3;
  lwNormal     =  4;
  lwMedium     =  5;
  lwSemiBold   =  6;
  lwBold       =  7;
  lwExtraBold  =  8;
  lwHeavy      =  9;
  lwUltraHeavy = 10;

  {standard font angles - TPaintInfo.Escapement}
  faHorizontal = 0;
  faVertical   = 270;
  {standard font italics - TPaintInfo.Italic}
  fiNormal     = 0;
  fiItalic     = 15;
  fiBlown      = 30;
  {font drawing styles for stroked fonts - TPaintInfo.Style}
  ftNormal          = 0;
  ftBold            = 1;
  ftThin            = 2;
  ftItalic          = 4;
  ftUnderlined      = 8;
  ftStruckOut       = 16;
  ftVerticalMoves   = 64;
  ftNoVerticalMoves = 0;
  ftNoDescent       = 128;
  ftFill            = 1024;
  ftTransparent     = 0;
  ftOpaque          = 2048;

  {driver memory models}
  vssFlat        = $00;
  vssSegmented   = $01;   { not vssSegmented = vssFlat }
  vssMultiPlane  = $02;   { not vssMultiPlane = vssNoPlanes }
  vssGranulated  = $03;   { superVGA multibank mode}

  {for CreateDIBitmap/CreatePalette functions}
  cbwInit         = $0001;    {make new bitmap/palette}
  cbwDelete       = $0002;    {delete old when done}
  cbwClear        = $0004;    {if cbwInit, clear new bitmap or setup palette}
  {for CreatePalette function}
  cbwQuad         = $0008;    {if palette has Windows RGBQuad structure}
  cbwTreatLoColor = $0010;   {if palette has VGA sandard structure}
  cbwReverse      = $0020;   {if palete has BGR structure}
  cbwCreate16Map  = $0040;   {if Colors <= 16, fills ColorRef from DefaultGraphPalette}
  cbwWindows      = cbwQuad + cbwReverse;
  cbwOS2          = cbwReverse;
  cbwCreate256Map = $0080;  {fills palette with enhanced color refinery map}
  cbwCreateHCMap  = $0100;  {creates large model colormap for HiColor model}
  cbwCreateTCMap  = $0200;  {creates large model colormap for TrueColor model}
  cbwMonoFix      = $0400;  {creates full 16-color palette for mono palette}
  {for CreateDIBitmapIndirect}
  cbwSetAlloc     = $0800;  {sets specified model for bitmap}
  cbwAllocFlat    = $1000;  {forces bitmap to be flat}


  {image model types}
  imNone     = 0;     {for image loading, none}
  imMono     = 1;     {monochrome DIB}
  imCursor   = 4;     {planed 4-color}
  imIcon     = 8;     {planed 8-color}
  imPlaned   = 16;    {planed 16-color}
  im16       = 17;    {16 color DIB}
  imPlaned16 = 21;    {planed 16-color, the same as imPlane}
  im256      = 256;   {256 color DIB}
  imHiColor  = 655;   {15/16-bit hi-color}
  imTC       = 1677;  {TrueColor 32-bit DIB}
  imStreamed = $8000; {bit flag, set if image in PStream derivate}
  imColor    = $0FFF; {bits, reserved for color types}
  imModel    = $F000; {bits, reserved for memory models}
  imCheck    = $1970; {image check constant}


  fsEmpty      =    0;  { Uses background color }
  fsSolid      =    1;  { Uses draw color fill  }
  fsLine       =    2;  { ---                   }
  fsLtSlash    =    3;  { ///                   }
  fsSlash      =    4;  { /// thick             }
  fsBkSlash    =    5;  { \\\ thick             }
  fsLtBkSlash  =    6;  { \\\                   }
  fsHatch      =    7;  { Light hatch           }
  fsXHatch     =    8;  { Heavy cross hatch     }
  fsInterleave =    9;  { Interleaving line     }
  fsWideDot    =   10;  { Widely spaced dot     }
  fsCloseDot   =   11;  { Closely spaced dot    }
  fsSimpleDots =   12;  { . . . . . . . . . .   }
  fsBorland    =   13;  { ####################  }
  fsParquet    =   14;  { \/\/\/\/\/\/\/\/\/\/\ }
  fsCritters   =   15;  {  }
  fsUser       =   16;  {can be defined with any value}
  fsNoUse      =  255;  {reserved fot TView.SetPaint() usage}

  {TPaintInfo.Pattern}
  FillPatterns : array[fsEmpty..fsUser] of TFillPattern = (
  ($00, $00, $00, $00, $00, $00, $00, $00),
  ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF),
  ($FF, $FF, $00, $00, $FF, $FF, $00, $00),
  ($01, $02, $04, $08, $10, $20, $40, $80),
  ($E0, $C1, $83, $07, $0E, $1C, $38, $70),
  ($F0, $78, $3C, $1E, $0F, $87, $C3, $E1),
  ($A5, $D2, $69, $B4, $5A, $2D, $96, $4B),
  ($FF, $88, $88, $88, $FF, $88, $88, $88),
  ($81, $42, $24, $18, $81, $42, $24, $18),
  ($CC, $33, $CC, $33, $CC, $33, $CC, $33),
  ($80, $00, $08, $00, $80, $00, $08, $00),
  ($88, $00, $22, $00, $88, $00, $22, $00),
  ($55, $aa, $55, $aa, $55, $aa, $55, $aa),
  ($ff, $aa, $ff, $aa, $ff, $aa, $ff, $aa),
  ($88, $54, $22, $45, $88, $15, $22, $51),
  ($00, $50, $72, $20, $00, $05, $27, $02),
  ($00, $00, $00, $00, $00, $00, $00, $00)
);

Function  InitGDI : Boolean;
Procedure DoneGDI;
Procedure SetOutput(OnBuffer : Boolean); {swaps output for buffered functions}
Procedure PrepareDrawing;

{buffered output supporting routines}
Procedure SetPixel(X, Y : Word; Color: LongInt);
Procedure SetPixelOp(X, Y : Word; Color : LongInt; Operation: Byte; Device : PImage); {*}
Function  GetPixel(X, Y : Word) : LongInt;
Function  GetPixelBM(X, Y : Word; Device : PImage) : LongInt;                  {*}
Procedure HLine(X, Y, X2 : Word; Color : LongInt);
Procedure VLine(X, Y, Y2 : Word; Color : LongInt);
Procedure Bar(X, Y, X2, Y2 : Word; Color : LongInt);
Procedure DisplayXxY(x, y: integer; BitMap: pointer; Width, Height : Word; Clr : LongInt);
Procedure DisplayXxYClip(x, y: integer; BitMap: pointer; Msk : Byte; Width, Height : Word; Clr : LongInt);
Procedure ReadScanLine(X, Y, XLen : Word; Buffer : Pointer);
Procedure WriteScanLine(X, Y, XLen : Word; Buffer : Pointer);
Procedure WriteScanLineOp(X, Y, XLen : Word; Buffer : Pointer; BitBlt : Byte);
Procedure PutBMPPart(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
Procedure PutBMPPartOp(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;Operation: Byte; ColorRef : PColorRef);
Procedure HLineStyleOp(X1, Y, X2 : Integer; var T : TPaintInfo);
Procedure DisplayXx8Op(x, y: integer; BitMap: pointer; Msk : Byte; Height : Word; var T : TPaintInfo);


{routines which doesn't support buffer output}
procedure QuickSave(xxx, yyy, xLen, yLen: integer);
procedure QuickRestore(xxx, yyy, xLen, yLen: integer);
procedure DirectMousePut(var Buf; x, y, xFrom, yFrom, Width, Height: integer);
procedure VLineStyleT(x, y1, y2, Width: integer; Clr : LongInt; Pattern: word);
Procedure DirectGetImage(x1, y1, x2, y2 : integer; var BitMap);
procedure DirectPutImage(Image: PCoordImage; x, y : integer);
procedure PutBufferPart(x1, y1, x2, y2: integer);
Function  MapBMPLineRead(P : PImage; Y : Word; ColorRef : PColorRef) : PByteArray;
Function  MapBitLineRead(P : PImage; Y, BPL : Word) : PByteArray;
Procedure MapBitLineWrite(P : PImage; Y, BPL, XFrom, XLen : Word; Data : Pointer);
Procedure MapBitLineFlush(P : PImage; Y, BPL : Word);

{indirect GDI methods}
Procedure BarStyle(X, Y, X2, Y2 : Integer; var T : TPaintInfo);
Procedure Line(RAX, RAY, RBX, RBY : Integer; var T : TPaintInfo);
procedure FillEllipse(X, Y, Rx, Ry: Integer; var T : TPaintInfo);
procedure Ellipse(X, Y, Rx, Ry: Integer; var T : TPaintInfo);
procedure FillPoly(NumPoints : Word; var Polygon; At : TPoint; var T : TPaintInfo);{ICS}
Procedure FloodFillArea(x, y : Integer; var T : TPaintInfo);
Procedure FloodFill(x, y : Integer; BorderColor : LongInt; var T : TPaintInfo);
Procedure DefaultPaint(var T : TPaintInfo);
Procedure DefaultFont(var F : TFont);
Procedure Rectangle(x1, y1, x2, y2 : Integer; var T : TPaintInfo);
Procedure DrawPoly(NumPoints: Word; var PolyPoints; At : TPoint; var T : TPaintInfo);
Procedure Circle(x1, y1, R : Integer; var T : TPaintInfo);
Procedure FillCircle(x1, y1, R : Integer; var T : TPaintInfo);
Procedure RoundBar(X1, Y1, X2, Y2, RX, RY : Integer; var T : TPaintInfo);
Procedure RoundRect(X1, Y1, X2, Y2, RX, RY : Integer; var T : TPaintInfo);
Procedure PutPixel(X, Y : Integer; var T : TPaintInfo);
Function  ReadPixel(X, Y : Integer; var T : TPaintInfo) : LongInt;
Procedure PutImage(Image : PImage; X, Y, XFrom, YFrom, XLen, YLen : Integer; ColorRef: PColorRef; var T : TPaintInfo);
procedure ScrollDraw(x1, y1, x2, y2, atX, atY: integer);

{bitmaps handling routines}
Function  CreateDIBitmap(Source : PImage; Usage : Word) : PImage;
Function  CreateDImage(Width, Height : Word) : PImage;
Function  CreateDImageIndirect(Width, Height, NBP, Options : Word) : PImage;
Procedure FreeDImage(Image : PImage);
Function  GetImageSize(Image : PImage) : LongInt;
Function  GetImageBufferSize(X, Y, ID : Word) : LongInt;
Procedure RemapImage(Image : PImage; ColorRef : PColorRef);
Procedure StretchDIBitmap(Image, Dest : PImage; xxx, yyy, xFrom, yFrom, xLen, yLen, xDest, yDest : integer;
          ColorRef : PColorRef; ClipRect : TRect);
Procedure ImplantDIBitmap(Image,Dest:PImage;X,Y,xFrom,yFrom,xLen,yLen:Integer;Operation:Byte;ColorRef:PColorRef);
Procedure CopyDIBits(Source, Dest : PImage; ColorIfNil : LongInt);
Procedure GetDIBitmap(Image : PImage; X, Y : Integer);
Procedure SetUserBitBltProc(Action : Pointer);
Procedure SetColorBitBlt(Color : LongInt; Transparent : Boolean);
Procedure EMSAdjSelect;

{micro dib actions}
Function  ReadPixelBM(X, Y : Integer; Image : PImage) : LongInt;

{palette handling routines}
Procedure CreatePalette(Palette : Pointer; var T : TLogPalette; Options, Colors : Word);
Procedure DisposePalette(var T : TLogPalette);
Procedure MakeHiPalette(var T : TLogPalette; ForTrueColor : Boolean);
Function  MapColor(Color : LongInt; var T : TLogPalette) : LongInt;

{obsoletes, but still supporting}
procedure ScrollY(x1, y1, x2, y2, How: integer);
procedure HLineStyleT(x1, y, x2: integer; Clr : LongInt;  Pattern: byte);
procedure HLineStyle(x1, y, x2: integer; Pattern : Byte; Clr0, Clr1: LongInt);
Procedure DirectLine(ST_X, ST_Y, END_X, END_Y, Color : Integer);
Procedure DirectHLine(X, Y, X2 : Word; Color : LongInt);
Procedure DirectDisplayXxY(x, y: integer; BitMap: pointer; Width, Height : Word; Fore, Back: LongInt; Erase : Boolean);
procedure DirectPutPartOfImage(Image: PImage; xxx, yyy,xFrom, yFrom, xLen, yLen: integer);

{dithering & colormapping}
Procedure MakePercentilePattern(var Pattern; Perc : Byte);
Procedure MakeDither(R, G, B : Byte; Colors : Word; var T : TPaintInfo; P : PVGAPalette);
Procedure MonoDither(R, G, B : Byte; var T : TPaintInfo);
Procedure MakeHiDither(R, G, B : Byte; var T : TPaintInfo);
Procedure RGBToHLS(R, G, B : Word; var H, L, S : integer);
Procedure HLSToRGB(H, L, S : Word; var R, G, B : Integer);
Function  HiColor     (R, G, B : Byte) : Word;
Function  TrueColor   (R, G, B : Byte) : LongInt;
Function  RGB         (R, G, B : Byte) : Byte;
Function  RGB16       (R, G, B : Byte) : Byte;
Function  RGBGrayScale(R, G, B : Byte) : Byte;
Function  RGBIndirect (R, G, B, LeftMargin, RightMargin : Byte; Where : PVGAPalette) : Byte;

{miscellaneous}
Procedure GetDeviceExtension(Device : Pointer; var Ext : TRect);
Procedure DrawXORFrame(x1, y1, x2, y2, Width: integer);
Function  IsImageStreamed(Image : PImage) : Boolean;
Function  BPLine(Image : PImage) : Word;
Function  DIBColors(Image : PImage) : LongInt;
Function  RecalcPixels(SizeInPoints : LongInt; PPI : Word) : LongInt;
Function  RecalcPoints(SizeInPixels : LongInt; PPI : Word) : LongInt;

{colormapped palettes}
Procedure SetVGAPalette(First, Count : Word; PBuf : Pointer);
Procedure GetVGAPalette(First, Count : Word; PBuf : Pointer);
Function  GetVGARegister(Num : Word) : LongInt; {as R, G, B : Byte}
Procedure SetVGARegister(Num : Word; Reg : TVGARegister);
Procedure SetEGARegister(Num, Reg : Byte);
Procedure WaitForRetrace;
Procedure WaitForHRetrace;
Procedure WaitForVRetrace;

Procedure StdVGAPalette;        {map}
Procedure StdColorRefMap;       {map}

{driver selections}
Procedure RegisterDriver(Driver : PScreenDriver);
Procedure SelectDriver(Driver : PScreenDriver);
Function  GetAppropriateDriver(Width, Height, Colors : LongInt; Precise : Boolean) : PScreenDriver;
Function  SetDIBDriver(ADIBType : Word) : Boolean;
Procedure RestoreDIBDriver;
Function  DriverInstalled(ADIBType : Word) : Boolean;


{Startup section}
const
  TryBufferedStrategy: boolean = True;    {try to allocate buffers}
  TryBuffersInPage   : boolean = False;   {if true, uses page buffering - it slower but requests no memory}

  BufferedStrategy   : boolean = False;   {r/o. shows is whether buffering goes thru buffers}
  BuffersInPage      : boolean = False;   {r/o. shows is whether buffering goes thru video mem}
  {$IFNDEF DPMI}
  BuffersInEms       : Boolean = False;   {Read only. If true then system uses EMS buffering}
  {$ENDIF}
  ScreenDriver       : PScreenDriver = Nil;
  ActivePage         : Byte = 0;
  HiColor16Bit       : Byte = 6; {6 for 64K HiColor, 5 for 32K HiColor }

Var
  StandardGraphPalette : PGraphPalette;
  MaximalX, MaximalY   : Word;                        {internal}
  LineLength           : Integer;                     {internal}
  QuickSaveArea        : array[0..1023] of LongInt;   {for mouse}
  ScrollMoveArea       : array[0..2047] of LongInt;   {for scrollDraw}
  ColorTransparent     : LongInt;                     {internal}
  SaveDIBDriver        : PScreenDriver;               {internal}

Const
  LongReverse : array[0..31] of Byte = (    {internal}
    24, 25, 26, 27, 28, 29, 30, 31, 16, 17, 18, 19, 20, 21, 22, 23,
    8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7 );

  EGARegisters : array [0..15] of byte = (     {EGA std 16-color palette}
    0, 1, 2, 3, 4, 5, 20, 7, 56, 57, 58, 59, 60, 61, 62, 63
  );

Implementation

Uses EGInline;

Procedure Bar(X, Y, X2, Y2 : Word; Color : LongInt);
Var
  I : Word;
Begin
  {$IFDEF PLOTRANGECHECK}
  if Y2 > ScreenWidth - 1 then Y2 := ScreenWidth - 1;
  if Y  > ScreenWidth - 1 then Exit;
  {$ENDIF}
  if X > X2 then asm
    mov ax, x
    mov bx, x2
    mov x, bx
    mov x2, ax
  end;
  if Y > Y2 then asm
    mov ax, y
    mov bx, y2
    mov y, bx
    mov y2, ax
  end;
  for I := Y to Y2 do DriverMethods._HLine(X, I, X2, Color);
End;


Function  ReadPixelBM(X, Y : Integer; Image : PImage) : LongInt;
Var
  P : PSImage absolute Image;
  D : Pointer;
  A1 : Byte;
  A2 : Word;
  A4 : LongInt;
Begin
  if (Y < 0) or (Y >= P^.Y) then Y := 0;
  if (X < 0) or (X >= P^.X) then X := 0;
  D := MapBitLineRead(Image, Y, BPLine(Image));
  case P^.NBP and imColor of
  imMono : ReadPixelBM :=
    LongInt((PByteArray(D)^[X shr 3] and (1 shl (7 - X and 7))) <> 0);
  im16   : begin
    A1 := PByteArray(D)^[X shr 1];
    if (X and 1) = 0 then ReadPixelBM := A1 shr 4 else ReadPixelBM := A1 and $F0;
  end;
  im256     : ReadPixelBM := PByteArray(D)^[X];
  imHiColor : ReadPixelBM := PWordArray(D)^[X];
  imTC      : ReadPixelBM := PLongArray(D)^[X];
  else end;
End;

Procedure CreatePalette(Palette : Pointer; var T : TLogPalette; Options, Colors : Word);
Var
  I : Word;
  S : Byte;
  C : Word;
  R, G, B : Byte;
  WasFix : Boolean;
Begin
  WasFix := ((Options and cbwMonoFix) <> 0) and (Colors = 2);
  if WasFix then Colors := 16;
  T.Colors := Colors;
  C := Colors * 3;
  if (Options and (cbwCreateHCMap + cbwCreateTCMap)) <> 0 then
    T.ColorRef  := MemAlloc(Colors * 4)
    else T.ColorRef  := MemAlloc(Colors);
  if T.ColorRef = Nil then Exit;
  if (Options and cbwInit) <> 0 then begin
    T.Palette   := MemAlloc(C);
    if T.Palette = Nil then Exit;
    T.Mode      := T.Mode or $10; {<- Views.pmInHeap}
  end else begin
    T.Palette := Palette;
    {очень осторожно - если не установлен cbwInit и проводится
    операция над палитрой, то изменяется _оригинальная_ палитра}
   end;
  if WasFix then Colors := 2;
  C := Colors * 3;
  if (Options and cbwClear) <> 0 then Move(PMainPalette^, T.Palette^, C);
  if (Options and cbwQuad) = 0 then begin
    if ((Options and cbwInit) <> 0) and (Palette <> Nil) then
      Move(Palette^, T.Palette^, C);
  end else  for I := 0 to Colors - 1 do begin
      T.Palette^[I, 1] := TVGARegister(Pointer(LongInt(Palette) + I * 4)^)[1];
      T.Palette^[I, 2] := TVGARegister(Pointer(LongInt(Palette) + I * 4)^)[2];
      T.Palette^[I, 3] := TVGARegister(Pointer(LongInt(Palette) + I * 4)^)[3];
    end;
  if (Options and cbwReverse) <> 0 then
    for I := 0 to Colors - 1 do begin
      S := T.Palette^[I, 1];
      T.Palette^[I, 1] := T.Palette^[I, 3];
      T.Palette^[I, 3] := S;
    end;
  if (Options and cbwTreatLoColor) <> 0 then
    for I := 0 to Colors * 3 - 1 do
      PByteArray(T.Palette)^[I] := PByteArray(T.Palette)^[I] shl 2;
  if (Options and cbwDelete) <> 0 then
    if (Options and cbwQuad) = 0 then FreeMem(Palette, C)
      else FreeMem(Palette, Colors * 4);
  if (Options and cbwCreate256Map) <> 0 then
    for B := 0 to 5 do for G := 0 to 5 do for R := 0 to 5 do begin
      I := Round(R + G * 6 + B * 36);
      if I >= Colors then Continue;
      T.Palette^[I, 1] := Trunc(R * 12.6) * 4;
      T.Palette^[I, 2] := Trunc(G * 12.6) * 4;
      T.Palette^[I, 3] := Trunc(B * 12.6) * 4;
    end;
  if (Options and cbwCreate16Map) <> 0 then for I := 0 to Colors - 1 do
    T.ColorRef^[I] := RGB16(T.Palette^[I, 1], T.Palette^[I, 2], T.Palette^[I, 3]);
  if (Options and cbwCreateHCMap) <> 0 then begin
    for I := 0 to Colors - 1 do
      PLongArray(T.ColorRef)^[I] := HiColor(T.Palette^[I, 1], T.Palette^[I, 2], T.Palette^[I, 3]);
    T.Mode      := T.Mode or $100; {<- Views.pmHiColor}
  end else if (Options and cbwCreateTCMap) <> 0 then begin
    for I := 0 to Colors - 1 do
      PLongArray(T.ColorRef)^[I] := TrueColor(T.Palette^[I, 1], T.Palette^[I, 2], T.Palette^[I, 3]);
    T.Mode      := T.Mode or $100; {<- Views.pmHiColor}
  end else T.Mode := T.Mode and not $100;
  if WasFix then begin
    {this trick is required for correct reproducing of mono bitmaps without
    any palette, based on default palette}
    if (Options and (cbwCreateHCMap + cbwCreateTCMap)) <> 0 then
      PlongArray(T.ColorRef)^[15] := PLongArray(T.ColorRef)^[1]
    else T.ColorRef^[15] := T.ColorRef^[1];
    T.Palette^[15]  := T.Palette^[1];
  end;
End;

Procedure DisposePalette(var T : TLogPalette);
Begin
  if T.Colors = 0 then Exit;
  if T.ColorRef <> Nil then begin
    if (T.Mode and $100) <> 0 then
      FreeMem(T.ColorRef, T.Colors * 4) else
      FreeMem(T.ColorRef, T.Colors);
  end;
  T.ColorRef := Nil;
  if (T.Palette <> Nil) and ((T.Mode and $10) <> 0) then
    FreeMem(T.Palette, T.Colors * 3);
  T.Palette := Nil;
  T.Colors := 0;
  T.Mode := T.Mode and not ($110);
End;

Procedure MakeHiPalette;
Var
  I : Word;
  P : PLongArray;
  X : PByteArray;
Begin
  if (T.Mode and $100) <> 0 then Exit;
  if T.Palette = Nil then Exit;
  if (T.Colors < 16) then begin
    FreeMem(T.ColorRef, T.Colors);
    if (T.Mode and $10) <> 0 then begin
      X := MemAlloc(16 * 3);
      Move(PMainPalette^, X^, 48);
      Move(T.Palette^, X^, 3);
      Move(T.Palette^[1], X^[15], 3);
      FreeMem(T.Palette, T.Colors * 3);
      Pointer(T.Palette) := X;
      T.Mode := T.Mode and not $10;
    end;
    T.ColorRef := MemAlloc(16);
    T.Colors := 16;
  end;
  P := MemAlloc(T.Colors * 4);
  if ForTrueColor then
  for I := 0 to T.Colors - 1 do
    P^[I] := TrueColor(T.Palette^[I, 1], T.Palette^[I, 2], T.Palette^[I, 3])
  else for I := 0 to T.Colors - 1 do
    P^[I] := HiColor(T.Palette^[I, 1], T.Palette^[I, 2], T.Palette^[I, 3]);
  T.Mode      := T.Mode or $100; {<- Views.pmHiColor}
  if T.ColorRef <> Nil then FreeMem(T.ColorRef, T.Colors);
  Pointer(T.ColorRef) := P;
End;

Function  MapColor;
Var
  C : LongInt;
Begin
  if T.ColorRef = Nil then begin
    if MaxColors <= 256 then C := Color else
      C := ColorIndex^[Color];
  end else
  if MaxColors = 2 then else
  if MaxColors <= 256 then C := T.ColorRef^[Color] else
  if MaxColors >= 32768 then begin
    if (T.Mode and $100) = 0 then begin
      if MaxColors > 65536 then
        C := TrueColor(T.Palette^[Color, 1], T.Palette^[Color, 2], T.Palette^[Color, 3])
      else
        C := HiColor(T.Palette^[Color, 1], T.Palette^[Color, 2], T.Palette^[Color, 3]);
    end else begin
      if (MaxColors = 32768) or (MaxColors = 65536) then
        C := PWordArray(T.ColorRef)^[Color] else
      if MaxColors > 65536 then MapColor := PLongArray(T.ColorRef)^[Color];
    end;
  end;
  MapColor := C;
End;


procedure ScrollY(x1, y1, x2, y2, How: integer);
Var
  _yFrom, _yTo : Integer;
Begin
  if How > 0 then begin
    _yFrom := y1 + How;
    _yTo   := Y1;
  end else begin
    _yFrom := y1;
    _yTo   := Y1 - How;
    Inc(y2, How-1);
  end;
  ScrollDraw(x1, _yFrom, x2, y2, x1, _yTo);
End;

Procedure DirectHLine(X, Y, X2 :Word; Color : LongInt);
Begin
  SetOutput(False);
  HLine(X, Y, X2, Color);
  SetOutput(True);
End;

Procedure DirectDisplayXxY(x, y: integer; BitMap: pointer; Width, Height : Word; Fore, Back: LongInt; Erase : Boolean);
Begin
  SetOutput(False);
  if Erase then Bar(x, y, x+Width, y+Height, Back);
  DisplayXxY(x, y, BitMap, Width, Height, Fore);
  SetOutput(True);
End;

procedure HLineStyleT(x1, y, x2: integer; Clr:LongInt; Pattern: byte);
Var
  T : TPaintInfo;
Begin
  T.LinePattern:= Word(Pattern) shl 8 + Pattern;
  T.Fore       := Clr;
  T.Back       := 0;
  T.Operation  := XORPut;
  T.LineStyle  := lsLinePattern;
  T.LineWidth  := 1;
  T.Device     := Nil;
  SetOutput(False);
  HLineStyleOp(x1, y, x2, T);
  SetOutput(True);
End;

procedure HLineStyle(x1, y, x2: integer; Pattern : Byte; Clr0, Clr1: LongInt);
Var
  T : TPaintInfo;
Begin
  T.LinePattern:= Word(Pattern) shl 8 + Pattern;
  T.Fore       := Clr0;
  T.Back       := Clr1;
  T.Operation  := CopyPut;
  T.LineWidth  := 1;
  T.Device     := Nil;
  T.LineStyle  := lsLinePattern;
  HLineStyleOp(x1, y, x2, T);
End;

Procedure DirectLine(ST_X, ST_Y, END_X, END_Y, Color : Integer);
Var
  T : TPaintInfo;
Begin
  SetOutput(False);
  T.ClipRect.Assign(MinInteger(ST_X,END_X), MinInteger(ST_Y,END_Y),
                    MaxInteger(ST_X,END_X), MaxInteger(ST_Y,END_Y));
  T.Fore        := Color;
  T.Operation   := CopyPut;
  T.LinePattern := psSolid;
  T.LineStyle   := lsLinePattern;
  T.LineWidth   := 1;
  T.Device      := Nil;
  Line(ST_X, ST_Y, END_X, END_Y, T);
  SetOutput(True);
End;


procedure DirectPutPartOfImage(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
Begin
  SetOutPut(False);
  PutBMPPart(Image, xxx, yyy, xFrom, yFrom, xLen, yLen);
  SetOutPut(True);
End;


Procedure MakePercentilePattern(var Pattern; Perc : Byte);
Var
  P : PByteArray;
  PC, I, J : Byte;
Const
  DT : array[0..3, 0..3] of Byte = (
    (00, 03, 09, 11),
    (16, 19, 29, 31),
    (04, 07, 13, 15),
    (20, 22, 25, 27)
  );
  DC : array[0..7, 0..3] of Byte = (
    ($01, $11, $15, $55),
    ($02, $22, $2A, $AA),
    ($04, $44, $45, $55),
    ($08, $88, $8A, $AA),
    ($10, $11, $51, $55),
    ($20, $22, $A2, $AA),
    ($40, $44, $54, $55),
    ($80, $88, $A8, $AA)
  );
Begin
  P := @Pattern;
  FillChar(P^, 8, 0);
  if Perc < 32 then PC := Perc else PC := 64 - Perc;
  for I := 0 to 3 do begin
    if PC > DT[I, 0] then if PC < DT[I, 1] then P^[I] := DC[I, 0]
      else if PC < DT[I, 2] then P^[I] := DC[I, 1] else
        if PC < DT[I, 3] then P^[I] := DC[I, 2] else P^[I] := DC[I, 3];
  end;
  for J := 0 to 3 do begin
    I := J + 4;
    if PC > DT[J, 0] + 1 then if PC < DT[J, 1] + 1 then P^[I] := DC[I, 0]
      else if PC < DT[J, 2] + 1 then P^[I] := DC[I, 1] else
        if PC < DT[J, 3] + 1 then P^[I] := DC[I, 2] else P^[I] := DC[I, 3];
  end;
  if Perc > 32 then for PC := 0 to 7 do P^[PC] := not P^[PC];
End;


Function HiColor(R, G, B : Byte) : Word; Assembler;
Asm
  mov al, R
  shr al, 3
  shl ax, 5
  mov dl, G
  shr dl, 3
  add al, dl
  mov cl, HiColor16Bit
  shl ax, cl
  mov dl, B
  shr dl, 3
  add al, dl
End;

Function TrueColor(R, G, B : Byte) : LongInt; Assembler;
Asm
  mov al, B
  mov ah, G
  mov dl, R
  xor dh, dh
End;

Function RGB(R, G, B : Byte) : Byte; Assembler;
Var
  NumDiff, CDiff, Diff: Word;
asm
    xor ax, ax
    mov NumDiff, ax
    mov CDiff,   ax
    dec ax
    mov Diff, ax
    cld
    shr B, 1
    shr R, 1
    shr G, 1
    mov si, offset MainPalette
      xor cx, cx
      dec cl
@@1:
      lodsb
      shr al, 1
      xor bh, bh
      mov bl, R
      sub ax, bx
      cbw
      xor al, ah
      sub al, ah
      xor ah, ah
      mov dx, ax
      lodsb
      shr al, 1
      mov bl, G
      sub ax, bx
      cbw
      xor al, ah
      sub al, ah
      xor ah, ah
      add dx, ax
      lodsb
      shr al, 1
      mov bl, B
      sub ax, bx
      cbw
      xor al, ah
      sub al, ah
      xor ah, ah
      add dx, ax
      mov CDiff, dx
      cmp dx, Diff
      jnb @@2
      xor ax, ax
      dec ax
      sub ax, cx
      mov NumDiff, ax
      mov Diff, dx
      mov al, ColorTolerance
      cbw
      cmp dx, ax
      jg @@2
      mov ax, NumDiff
      jmp @@3
 @@2:
      loop @@1
      mov ax, NumDiff
 @@3:
End;

Function RGB16(R, G, B : Byte) : Byte;
Begin
  RGB16 := RGBIndirect(R, G, B, 0, 15, @DefaultGraphPalette);
End;

{this useful when system colors = 32 only}
Function RGBGrayScale(R, G, B : Byte) : Byte;
Begin
  RGBGrayScale := RGBIndirect(R, G, B, 16, 31, PMainPalette);
End;

Function  RGBIndirect(R, G, B, LeftMargin, RightMargin : Byte; Where : PVGAPalette) : Byte; Assembler;
{Label Exit;
Var
  I : Byte;
  NumDiff, Diff, CDiff : Word;
Begin
  if Where = Nil then Where := PMainPalette;
  Diff := Word(-1); NumDiff := 0; CDiff := 0;
  for I := LeftMargin to RightMargin do begin
    CDiff :=  Abs(R - Where^[I,1]) +
              Abs(G - Where^[I,2]) +
              Abs(B - Where^[I,3]);
    if CDiff < Diff then begin
      NumDiff := I;
      Diff := CDiff;
      if CDiff = 0 then Goto Exit;
    end;
  end;
 Exit:
   RGBIndirect := NumDiff;
End;}
Var
  NumDiff, CDiff, Diff: Word;
asm
    mov ax, word ptr [Where]
    or  ax, word ptr [Where+2]
    jne @@0
    mov ax, word ptr [PMainPalette+2]
    mov word ptr [Where+2], ax
    mov ax, word ptr [PMainPalette]
    mov word ptr [Where], ax
@@0:
    push ds
    xor ax, ax
    mov NumDiff, ax
    mov CDiff,   ax
    dec ax
    mov Diff, ax
    shr B, 1
    shr R, 1
    shr G, 1
    cld
    mov al, ColorTolerance
    xor ah, ah
    mov di, ax

    lds si, Where
    mov al, LeftMargin
    add si, ax
    add si, ax
    add si, ax
    xor cx, cx
    mov cl, RightMargin
    sub cl, al
    inc cx
@@1:
      lodsb
      shr al, 1
      xor bh, bh
      mov bl, R
      sub ax, bx
      cbw
      xor al, ah
      sub al, ah
      xor ah, ah
      mov dx, ax
      lodsb
      shr al, 1
      mov bl, G
      sub ax, bx
      cbw
      xor al, ah
      sub al, ah
      xor ah, ah
      add dx, ax
      lodsb
      shr al, 1
      mov bl, B
      sub ax, bx
      cbw
      xor al, ah
      sub al, ah
      xor ah, ah
      add dx, ax
      mov CDiff, dx
      cmp dx, Diff
      jnb @@2
      xor ah, ah
      mov al, RightMargin
      sub ax, cx
      inc ax
      mov NumDiff, ax
      mov Diff, dx
      cmp dx, di
      jg @@2
      mov ax, NumDiff
      jmp @@3
 @@2:
      loop @@1
      mov ax, NumDiff
 @@3:
   pop ds
End;


Procedure MakeDither(R, G, B : Byte; Colors : Word; var T : TPaintInfo; P : PVGAPalette);

Function  RGBLeft : Byte;
Var
  NumDiff, I, LastAbs : Word;
  Last : TVGARegister;
Begin
  NumDiff := 0; LastAbs := $FFFF;
  for I := 0 to Colors - 1 do begin
    if (P^[I,1] <= R) and (P^[I,2] <= G) and (P^[I,3] <= B) then begin
      if Abs(R - P^[I,1]) + Abs(G - P^[I,2]) + Abs(B - P^[I,3]) < LastAbs then begin
        Last := P^[I];
        LastAbs := Abs(R - Last[1]) + Abs(G - Last[2]) + Abs(B - Last[3]);
        NumDiff := I;
      end;
    end;
  end;
  RGBLeft := NumDiff;
End;

Function  RGBRight : Byte;
{Var
  A0, A1 : TVGARegister;
  A2 : array[1..3] of Integer;
  i, j : Integer;
  jei  : Real;
  CDiff, NDiff : Word;
Begin
  A1 := P^[T.Fore];
  A0[1] := R;
  A0[2] := G;
  A0[3] := B;
  NDiff := 0;
  for j := 1 to 3 do begin
    Dec(A0[j], A1[j]);
    Inc(NDiff, A0[j]);
  end;
  if NDiff = 0 then begin
    RGBRight := T.Fore;
    Exit;
  end;
  j := maxInteger(maxInteger(A0[1], A0[2]), A0[3]);
  if j <> 0 then jei := 255 / j else jei := 1;
  for j := 1 to 3 do A0[j] := Round(A0[j] * jei);
  CDiff := $FFFF;
  for i := 0 to Colors - 1 do begin
    for j := 1 to 3 do A2[j] := P^[i, j] - A1[j];
    j := maxInteger(maxInteger(A2[1], A2[2]), A2[3]);
    if j <> 0 then jei := 255 / j else jei := 1;
    for j := 1 to 3 do A2[j] := Round(A2[j] * jei);
    NDiff := Abs(A2[1] - A0[1]) +
             Abs(A2[2] - A0[2]) +
             Abs(A2[3] - A0[3]);
    if NDiff < CDiff then begin
      CDiff := NDiff;
      RGBRight := i;
      if CDiff < 12 then Exit;
    end;
  end;
End;}
Var
  NumDiff, I, LastAbs : Word;
  Last : TVGARegister;
Begin
  NumDiff := 0; LastAbs := $FFFF;
  for I := 0 to Colors - 1 do begin
    if (P^[I,1] >= R) and (P^[I,2] >= G) and (P^[I,3] >= B) then begin
      if Abs(R - P^[I,1]) + Abs(G - P^[I,2]) + Abs(B - P^[I,3]) < LastAbs then begin
        Last := P^[I];
        LastAbs := Abs(R - Last[1]) +
                   Abs(G - Last[2]) +
                   Abs(B - Last[3]);
        NumDiff := I;
      end;
    end;
  end;
  RGBRight := NumDiff;
End;

Var
  B0, B1, B2 : Integer;
  Perc       : Integer;

Begin
  if P = Nil then P := PMainPalette;
  if R > 252 then R := 252;
  if G > 252 then G := 252;
  if B > 252 then B := 252;
  T.Fore := RGBLeft;
  T.Back := RGBRight;
  T.LineStyle := lsPattern;
  B0 := R + G + B;
  B1 := P^[T.Fore, 1] + P^[T.Fore, 2] + P^[T.Fore, 3];
  B2 := P^[T.Back, 1] + P^[T.Back, 2] + P^[T.Back, 3];
  if (B1 = B2) or (B0 = B1) or (B0 = B2) then begin
    if (B0 = B1) or (B1 = B2) then Perc := 64 else Perc := 0;
  end else Perc := Abs(Word(64) * (B0 - B2) div (B1 - B2));
  if (Perc > 64) or (Perc < 0) then Perc := 64;
  MakePercentilePattern(T.Pattern, Perc);
End;

Procedure MonoDither(R, G, B : Byte; var T : TPaintInfo);
Var
  B0, B1, B2 : Integer;
  Perc       : Integer;
Begin
  if R > 252 then R := 252;
  if G > 252 then G := 252;
  if B > 252 then B := 252;
  T.Fore := 1;
  T.Back := 0;
  T.LineStyle := lsPattern;
  B0 := R + G + B;
  B1 := 252 * 3;
  B2 := 0;
  if (B1 = B2) or (B0 = B1) or (B0 = B2) then begin
    if (B0 = B1) or (B1 = B2) then Perc := 64 else Perc := 0;
  end else Perc := Abs(Word(64) * (B0 - B2) div (B1 - B2));
  if (Perc > 64) or (Perc < 0) then Perc := 64;
  MakePercentilePattern(T.Pattern, Perc);
End;

Procedure MakeHiDither(R, G, B : Byte; var T : TPaintInfo);
Var
  R1, G1, B1 : Word;
Begin
  R1 := R and $F8;
  G1 := G and $F8;
  B1 := B and $F8;
  T.Back := HiColor(R1, G1, B1);
  T.LineStyle := lsPattern;
  R1 := R1 + (R and 7) shl 2;
  G1 := G1 + (G and 7) shl 2;
  B1 := B1 + (B and 7) shl 2;
  if R1 > 255 then R1 := 255;
  if G1 > 255 then G1 := 255;
  if B1 > 255 then B1 := 255;
  T.Fore := HiColor(R1, G1, B1);
  MakePercentilePattern(T.Pattern, 16);
End;

{Hue, Luminocity, Saturation}
Procedure RGBToHLS(R, G, B : Word; var H, L, S : integer);
Var
  cr,cg,cb,m1,m2,ir,ig,ib,ih,il,is:real;
Begin
  m1 := MaxWord(MaxWord(r, g), b) / 63;
  m2 := MinWord(MinWord(r, g), b) / 63;
  ir := r / 63;
  ig := g / 63;
  ib := b / 63;
  il := (m1 + m2) / 2;
  if m1 = m2 then begin
    is := 0;
    ih := 0;
  end else begin
    if il <= 0.5 then is := (m1 - m2) / (m1 + m2) else
      is := (m1 - m2) / (2 - m1 - m2);
    cr := (m1 - ir) / (m1 - m2);
    cg := (m1 - ig) / (m1 - m2);
    cb := (m1 - ib) / (m1 - m2);
    if ir = m1 then ih := cb - cg;
    if ig = m1 then ih := 2 + cr - cb;
    if ib = m1 then ih := 4 + cg - cr;
  end;
  h := Round(60 * ih);
  if h < 0 then h := h + 360;
  l := Round(il * 100);
  s := Round(is * 100);
End;

Procedure HLSToRGB(H, L, S : Word; var R, G, B : Integer);

Function XRGB(HH, mm1, mm2 : Real) : Real;
Begin
  if hh < 0 then hh := hh + 360;
  if hh > 360 then hh := hh - 360;
  if hh < 60 then xrgb := mm1 + (mm2 - mm1) * hh / 60 else
    if hh < 180 then xrgb := mm2 else
      if hh < 240 then xrgb := mm1 + (mm2 - mm1) * (240 - hh) / 60 else
        xrgb := mm1;
End;

Var
  cr,cg,cb,m1,m2,ir,ig,ib,ih,il,is : Real;
Begin
  il := l / 100;
  ih := h;
  is := s / 100;
  if il <= 0.5 then m2 := il * (1 + is) else m2 := il + is - il * is;
  m1 :=2 * il - m2;
  if s = 0 then begin
    ir := il;
    ig := il;
    ib := il
  end else begin
    ir := XRGB(ih + 120, m1, m2);
    ig := XRGB(ih , m1, m2);
    ib := XRGB(ih - 120, m1, m2);
  end;
  r := Round(ir * 63);
  g := Round(ig * 63);
  b := Round(ib * 63);
End;



Procedure GetDeviceExtension(Device : Pointer; var Ext : TRect);
Begin
  Ext.A.X := 0;
  Ext.A.Y := 0;
  if Device = Nil then begin
    Ext.B.X := ScreenDriver^.MaximalX;
    Ext.B.Y := ScreenDriver^.MaximalY;
  end else begin
    Ext.B.X := PSImage(Device)^.X - 1;
    Ext.B.Y := PSImage(Device)^.Y - 1;
  end;
End;

Function IsImageStreamed(Image : PImage) : Boolean;
Begin
  IsImageStreamed := (PSImage(Image)^.NBP and $F000) <> 0;
End;


Function  BPLine(Image : PImage) : Word;
Var
  NX : Word;
Begin
  NX := PSImage(Image)^.X;
  case PSImage(Image)^.NBP and imColor of
  imMono:     BPLine := NX shr 3 + Byte((NX and 7) <> 0);
  imIcon, imCursor, imPlaned, imPlaned16 :
    BPLine := ((NX + 7) shr 3) shl 2;
  im16:       BPLine := (NX shr 1) + NX and 1;
  imHiColor:  BPLine := NX shl 1;
  imTC:       BPLine := NX shl 2;
  else        BPLine := NX;
  end;
End;


Function  DIBColors;
Begin
  case PSImage(Image)^.NBP and imColor of
  imMono:     DIBColors := 2;
  imIcon, imCursor, imPlaned, imPlaned16, im16 : DIBColors := 16;
  im256   :  DIBColors := 256;
  imHiColor: DIBColors := 65536;
  imTC:      DIBColors := 16777216;
  else DIBColors := 0;
  end;
End;

Function  RecalcPixels(SizeInPoints : LongInt; PPI : Word) : LongInt;
Begin
  RecalcPixels := SizeInPoints * PPI div 72;
End;

Function  RecalcPoints(SizeInPixels : LongInt; PPI : Word) : LongInt;
Begin
  RecalcPoints := SizeInPixels * 72 div PPI;
End;

Function TLinedRect.Allowed(var R : TRect) : Boolean;
Begin
  if (A.X > R.B.X) or (B.X < R.A.X) or
     (A.Y > R.B.Y) or (A.Y < R.A.Y) then Allowed := False
  else begin
    A.X := MaxInteger(A.X, R.A.X);
    A.Y := MaxInteger(A.Y, R.A.Y);
    B.X := MinInteger(B.X, R.B.X);
    Allowed := True;
  end;
End;

Function TLinedRect.ContainPel(X, Y : Word) : Boolean;
Begin
  ContainPel := (X >= A.X) and (X <= B.X) and (Y >= A.Y) and (Y <= B.Y);
End;

Procedure BarStyle(X, Y, X2, Y2 : Integer; var T : TPaintInfo);
Var
  I : Word;
  R : TRect;
Begin
  {$IFDEF PLOTRANGECHECK}
  if Y2 > ScreenWidth - 1 then Y2 := ScreenWidth - 1;
  if Y  > ScreenWidth - 1 then Exit;
  {$ENDIF}
  if X > X2 then asm
    mov ax, x
    mov bx, x2
    mov x, bx
    mov x2, ax
  end;
  if Y > Y2 then asm
    mov ax, y
    mov bx, y2
    mov y, bx
    mov y2, ax
  end;
  R.Assign(X, Y, Succ(X2), Succ(Y2));
  R.Intersect(T.ClipRect);
  if R.Empty then Exit;

  for I := R.A.Y to Pred(R.B.Y) do HLineStyleOp(R.A.X, I, Pred(R.B.X), T);
End;

Procedure Line(RAX, RAY, RBX, RBY : Integer; var T : TPaintInfo);
Var
  DI_Y_IN, DI_X_IN, SH_DI, ST_X_IN, ST_Y_IN, ST_CO, DI_CO : Word;
  Pattern : Word;
  I : Byte;
  Ends : array[0..4] of TPoint;
  X, Y : Integer;
  SetPixelProc : Pointer;
  IgnoLine : Boolean;
Begin
  IgnoLine := (T.LineStyle <> lsLinePattern) and (T.LineWidth > 1);
  if IgnoLine then begin
    Ends[0].X := RAX;
    Ends[0].Y := RAY;
    Ends[1].X := RBX;
    Ends[1].Y := RBY;
    Ends[3] := Ends[0];
    Ends[2] := Ends[1];
  end;
asm
    les di, T
    mov ax, es:[di].TPaintInfo.LinePattern
    mov Pattern, ax
    mov cx, 1
    mov dx, 1
    mov di, RBY
    sub di, RAY
    jge @@KEEP_Y
    neg dx
    neg di
  @@KEEP_Y:mov DI_Y_IN, dx
    mov si, RBX
    sub si, RAX
    jge @@KEEP_X
    neg cx
    neg si
  @@KEEP_X:mov DI_X_IN, cx

    cmp si, di
    jge @@HORS
    xor cx, cx
    xchg si, di
    jmp @@SAVS
@@HORS:xor dx, dx
{?}    inc si
@@SAVS:mov SH_DI, di
    mov ST_X_IN, cx
    mov ST_Y_IN, dx
    mov ax, SH_DI
    shl ax, 1
    mov st_co, ax
    sub ax, si
    mov bx, ax
    sub ax, si
    mov DI_CO, ax
    inc si
{}
    xor ax, ax
    mov X, ax
    mov Y, ax
    les di, T
    mov al, es:[di].TPaintInfo.LineWidth
    mov I, al
    cmp al, 1 {если толщина = 1 центрирования не требуется}
    jbe @@10
    mov cl, al
    shr cl, 1
    xor ch, ch
    mov ax, ST_X_IN
    imul cl
    sub RAY, ax
    mov y, ax
    mov ax, ST_Y_IN
    imul cl
    sub RAX, ax
    mov x, ax
  @@10:
  end;
  if IgnoLine then begin
    Ends[4].X := X;
    Ends[4].Y := Y;
    X := ST_Y_IN * (T.LineWidth - 1);
    Y := ST_X_IN * (T.LineWidth - 1);

    Dec(Ends[2].X, X);
    Dec(Ends[2].Y, Y);
    Dec(Ends[3].X, X);
    Dec(Ends[3].Y, Y);

    FillPoly(4, Ends, Ends[4], T);
  end else asm
    mov cx, RAX
    mov dx, RAY
  @@MLOOP:
     dec si
     jz @@FINIS
     ror Pattern, 1
     push cx
     push dx
     les di, T
     mov al, es:[di].TPaintInfo.LineWidth
     mov I, al
  @@Width:
{}   dec I

     les di, T
     cmp cx, es:[di].TPaintInfo.ClipRect.A.X
     jl @@8
     cmp cx, es:[di].TPaintInfo.ClipRect.B.X
     jge  @@8
     cmp dx, es:[di].TPaintInfo.ClipRect.A.Y
     jl @@8
     cmp dx, es:[di].TPaintInfo.ClipRect.B.Y
     jge  @@8
     mov ax, Pattern
     and ax, 1
     db $66; mov ax, word ptr es:[di].TPaintInfo.Fore
     jne @@9
     db $66; mov ax, word ptr es:[di].TPaintInfo.Back
@@9:
     push cx
     push dx
     push bx
     push si
     push di
     cmp es:[di].TPaintInfo.LineStyle, lsLinePattern
     jne  @@Abnormal
     {normal line, any line width}
     push cx  {x}
     push dx  {y}
     db $66; push ax  {<- color}
     mov  al, es:[di].TPaintInfo.Operation
     push ax
     mov  ax, word ptr es:[di].TPaintInfo.Device+2
     push ax
     mov  ax, word ptr es:[di].TPaintInfo.Device
     push ax
     call setPixelOp
     jmp @@Pops
 @@Abnormal:
    {для линии с шириной 1 и Style <> lsLinePattern}
     push cx  {x}
     push dx  {y}
     push cx  {x}
     push es
     push di
     call HLineStyleOp
 @@Pops:
     pop  di
     pop  si
     pop  bx
     pop  dx
     pop  cx

  @@8:
{}   add  cx, ST_Y_IN
     add  dx, ST_X_IN
     cmp  I, 0
     jne @@Width
     pop dx
{}   pop cx

    cmp bx, 0
    jge @@DILIN
    add cx, ST_X_IN
    add DX, ST_Y_IN
    add BX, ST_CO
    jmp  @@MLOOP
@@DILIN:add cx, DI_X_IN
    add dx, DI_Y_IN
    ADD BX, DI_CO
    jmp @@MLOOP
  @@FINIS:
end;
End;

{следующие две процедуры задерты из киевского Vision'a
(те в свою очередь из Р.Уилтона). Переделки _были_.}
procedure FillEllipse(X, Y, Rx, Ry : Integer; var T : TPaintInfo);
var
  Xc, Yc : Integer;
  Asquared, TwoAsquared,  Bsquared,  TwoBsquared, d, dx, dy: LongInt;
  YR : TLinedRect;
  CR : TLinedRect;
  Inner, Outer : PIntegerArray;
  Ry1, C, E, W, Rx_ : Integer;
  ADC : Byte;

begin
  CR.Copy(T.ClipRect);
  W := T.LineWidth-0;
  if (W >= Rx) or (W >= Ry) or (W = 0) then W := -1;
  if W = 1 then ADC := 0 else ADC := 1;
  Xc := X;  Yc := Y;  x := 0;  y := Ry; Ry1 := Ry - W;
  Outer := MemAlloc(SizeOf(Integer) * (Ry + 1));
  if Outer = Nil then Exit;
  if W >= 0 then begin
    Inner := MemAlloc(SizeOf(Integer) * (Ry + 1));
    if Inner = Nil then begin
      FreeMem(Outer, SizeOf(Integer) * (Ry+1));
      Exit;
    end;
  end;

  Asquared := Rx * Rx;
  TwoAsquared := Asquared shl 1;
  Bsquared := Ry * Ry;
  TwoBsquared := Bsquared shl 1;
  d := Bsquared - Asquared * Ry + Asquared shr 2;
  dx := 0;
  dy := TwoAsquared * Ry;

  while (dx<dy) do begin
    Outer^[Y] := X;
    if (d > 0) then begin
      Dec(Y);
      Dec(DY, TwoAsquared);
      Dec(D, DY);
    end;
    Inc(x);
    Inc(DX, TwoBsquared);
    Inc(D, Bsquared + dx);
  end;

  d := (Asquared - Bsquared + (Asquared - Bsquared) div 2 - dx - dy) div 2;

  while (y >= 0) do begin
    Outer^[Y] := X;
    if (d < 0) then begin
      Inc(X);
      Inc(DX, TwoBsquared);
      Inc(d, DX);
    end;
    Dec(y);
    Dec(DY, TwoAsquared);
    Inc(D, Asquared - dy);
  end;

  if W >= 0 then begin
    Dec(Rx, W);
     x := 0;  y := Ry1;
    Asquared := Rx * Rx;
    TwoAsquared := Asquared shl 1;
    Bsquared := Ry1 * Ry1;
    TwoBsquared := Bsquared shl 1;
    d := Bsquared - Asquared * Ry1 + Asquared shr 2;
    dx := 0;
    dy := TwoAsquared * Ry1;

    while (dx<dy) do begin
      Inner^[Y] := X;
      if (d > 0) then begin
        Dec(Y);
        Dec(DY, TwoAsquared);
        Dec(D, DY);
      end;
      Inc(x);
      Inc(DX, TwoBsquared);
      Inc(D, Bsquared + dx);
    end;

    d := (Asquared - Bsquared + (Asquared - Bsquared) div 2 - dx - dy) div 2;

    while (y >= 0) do begin
      Inner^[Y] := X;
      if (d < 0) then begin
        Inc(X);
        Inc(DX, TwoBsquared);
        Inc(d, DX);
      end;
      Dec(y);
      Dec(DY, TwoAsquared);
      Inc(D, Asquared - dy);
    end;
  end;

  Rx_ := Xc - Rx - 1;
  if W > 0 then begin
    C := Outer^[0]; E := Inner^[0] + ADC;
    with YR do begin
      Assign(Xc - C, Yc, Xc - E, 0);
      if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X, T);
      Assign(Xc + E, Yc, Xc + C, 0);
      if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X, T);
    end;

    YR.A.Y := Yc;
    for Y := 1 to Ry - (ADC xor 1) do begin
      C := Outer^[Y]; E := Inner^[Y] + ADC;
      YR.A.X := Xc - C; YR.B.X := Xc + C;
      Dec(YR.A.Y);
      if Y > Ry1 - (ADC xor 1) then with YR do begin
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X, T);
      end else with YR do begin
        YR.A.X := Xc - C; YR.B.X := Xc - E;
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X,T);
        YR.A.X := Xc + E; YR.B.X := Xc + C;
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X,T);
      end;
    end;

    YR.A.Y := Yc;
    for Y := 1 to Ry - (ADC xor 1) do begin
      C := Outer^[Y]; E := Inner^[Y] + ADC;
      Inc(YR.A.Y);
      YR.A.X := Xc - C; YR.B.X := Xc + C;
      if Y > Ry1 - (ADC xor 1) then with YR do begin
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X,T);
      end else with YR do begin
        YR.A.X := Xc - C; YR.B.X := Xc - E;
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X,T);
        YR.A.X := Xc + E; YR.B.X := Xc + C;
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X, T);
      end;
    end;
  end
  else begin
    C := Outer^[0];
    with YR do begin
      Assign(Xc - C, Yc, Xc + C, 0);
      if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X, T);
    end;
    YR.Assign(Xc - C, Yc - 1, Xc + C, 0);
    for Y := 1 to Ry do begin
      C := Outer^[Y];
      with YR do begin
        A.X := Xc - C; B.X := Xc + C;
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X,T);
        Dec(A.Y);
      end
    end;
    YR.A.Y := YC + 1;
    for Y := 1 to Ry do begin
      C := Outer^[Y];
      with YR do begin
        A.X := Xc - C; B.X := Xc + C;
        if Allowed(CR) then HLineStyleOp(A.X, A.Y, B.X,T);
        Inc(A.Y);
      end
    end;
  end;

  FreeMem(Outer, SizeOf(Integer) * (Ry + 1));
  if W >= 0 then FreeMem(Inner, SizeOf(Integer) * (Ry + 1));
end;

procedure Ellipse(X, Y, Rx, Ry: Integer; var T : TPaintInfo);
var
  Xc, Yc : Integer;
  Asquared, TwoAsquared,  Bsquared,  TwoBsquared, d, dx, dy: LongInt;
  YR : TLinedRect;
  CR : TLinedRect;
  Color : LongInt;
  OLS : Byte;
  Pattern : Word;
  OldBM, Data   : Pointer;
  PP : TPaintInfo;

begin
  if (T.LineWidth > 1) then begin
    OLS := T.LineStyle;
    if T.LineStyle = lsLinePattern then begin
      OldBM := T.BitMap;
      T.Bitmap := CreateDImage(8, 16);
      Pattern := T.LinePattern;
      DefaultPaint(PP);
      PP.Device := T.Bitmap;
      for Yc := 0 to 15 do begin
        asm ror Pattern, 1 end;
        if (Pattern and 1) <> 0 then PP.Fore := T.Fore else PP.Fore := T.Back;
        HLineStyleOp(0, 7, Yc, PP);
      end;
    end;
    FillEllipse(X, Y, Rx, Ry, T);
    T.LineStyle := OLS;
    if T.LineStyle = lsLinePattern then begin
      FreeDImage(T.Bitmap);
      T.BitMap := OldBM;
    end;
    Exit;
  end;

  CR.Copy(T.ClipRect);
  Pattern := T.LinePattern;
  Xc := X;  Yc := Y;  x := 0;  y := Ry;
  Asquared := LongInt(Rx) * Rx;
  TwoAsquared := Asquared shl 1;
  Bsquared := LongInt(Ry) * Ry;
  TwoBsquared := Bsquared shl 1;
  d := Bsquared - Asquared * Ry + Asquared shr 2;
  dx := 0;
  dy := TwoAsquared * Ry;

  while (dx<dy) do begin
    with YR do begin
      Assign(Xc - X, Yc - Y, Xc + X, Yc + Y);
      asm ror Pattern, 1 end;
      if (Pattern and 1) <> 0 then Color := T.Fore else Color := T.Back;
      if CR.ContainPel(A.X, A.Y) then SetPixelOp(A.X, A.Y, Color, T.Operation, T.Device);
      if CR.ContainPel(B.X, A.Y) then SetPixelOp(B.X, A.Y, Color, T.Operation, T.Device);
      if CR.ContainPel(A.X, B.Y) then SetPixelOp(A.X, B.Y, Color, T.Operation, T.Device);
      if CR.ContainPel(B.X, B.Y) then SetPixelOp(B.X, B.Y, Color, T.Operation, T.Device);
    end;

    if (d > 0) then begin
      Dec(Y);
      Dec(DY, TwoAsquared);
      Dec(D, DY);
    end;
    Inc(x);
    Inc(DX, TwoBsquared);
    Inc(D, Bsquared + dx);
  end;

  d := (Asquared - Bsquared + (Asquared - Bsquared) div 2 - dx - dy) div 2;

  while (y >= 0) do begin
    with YR do begin
      Assign(Xc - X, Yc - Y, Xc + X, Yc + Y);
      asm ror Pattern, 1 end;
      if (Pattern and 1) <> 0 then Color := T.Fore else Color := T.Back;
      if CR.ContainPel(A.X, A.Y) then SetPixelOp(A.X, A.Y, Color, T.Operation, T.Device);
      if CR.ContainPel(B.X, A.Y) then SetPixelOp(B.X, A.Y, Color, T.Operation, T.Device);
      if CR.ContainPel(A.X, B.Y) then SetPixelOp(A.X, B.Y, Color, T.Operation, T.Device);
      if CR.ContainPel(B.X, B.Y) then SetPixelOp(B.X, B.Y, Color, T.Operation, T.Device);
    end;

    if (d < 0) then begin
      Inc(X);
      Inc(DX, TwoBsquared);
      Inc(d, DX);
    end;
    Dec(y);
    Dec(DY, TwoAsquared);
    Inc(D, Asquared - dy);
  end;
end;

{procedure trap216; assembler; asm lldt bx end;}

{Эта зараза задерта из московского SuperVision'a}
procedure FillPoly(NumPoints: Word;var Polygon; At : TPoint;var T : TPaintInfo);{Володин В.А.}
type
      TPolygon = array[1..16380] of TPoint;
      PPolygon = ^TPolygon;

  Type
  PLTR = ^TLTR;
  TLTR = array[0..2047] of
                    record
                      Min, Max: Integer;
                    end;

  Var
    PlineTable : PLTR;

      var
        Step   : Integer;
        StPoint: Word;
        EnPoint: Word;
        PolyP  : PPolygon;
        Start  : TPoint;

   procedure A_Line(x1, y1, x2, y2: integer; XYOffs : Word);
   var w, x, y, dx, dy, d, Incr1, Incr2, YStep: integer;
       PLT : PLTR;
   begin
      PLT := PLineTable;
      if x1 > x2 then begin
         w:= x1; x1:= x2; x2:= w;
         w:= y1; y1:= y2; y2:= w;
      end;
      dx:= x2-x1;
      dy:= abs(y2-y1);
      if y1 > y2 then YStep:= -1 else YStep:= 1;
      x := x1; y := y1;
      if dy <= dx then begin
         incr1:= dy shl 1;
         d:= incr1 - dx;
         incr2:= d - dx;
         while x <> x2 do begin
            asm
               mov    ax, x
               mov    si, y
               les    di, PLT
               shl    si, 1
               shl    si, 1
               add    si, di
               add    si, XYOffs
               mov    [es: si], ax
               inc    x
            end;

            if d < 0 then inc(d, Incr1) else begin
               inc(d, Incr2);
               inc(y, YStep);
            end;
         end;
      end else { dy > dx } begin
         incr1:= dx shl 1;
         d:= incr1 - dy;
         incr2:= d - dy;

         while y <> y2 do begin asm
               mov    ax, x
               mov    si, y
               les    di, PLT
               shl    si, 1
               shl    si, 1
               add    si, di
               add    si, XYOffs
               mov    [es: si], ax
            end;
            inc(y, YStep);

            if d < 0 then inc(d, Incr1) else begin
               inc(d, Incr2);
               inc(x);
            end;
         end;
      end;

      asm
         mov    ax, x
         mov    si, y
         les    di, PLT
         shl    si, 1
         shl    si, 1
         add    si, di
         add    si, XYOffs
         mov    [es: si], ax
      end;
   end;

      procedure CheckInvert;
         var
            S: LongInt;
            k: Word;
            X0, Y0, X1, Y1, X2, Y2: Integer;
      begin
         S:= 0;
         for k := 1 to NumPoints do begin
           Inc(PolyP^[k].X, At.X);
           Inc(PolyP^[k].Y, At.Y);
         end;
         Start := PolyP^[1];
         for k := 3 to NumPoints do begin
           Start.Y := MinInteger(Start.Y, PolyP^[k].Y);
           Start.X := MinInteger(Start.X, PolyP^[k].X);
         end;

         X0:= PolyP^[1].X; X1:= X0;
         Y0:= PolyP^[1].Y; Y1:= Y0;
         for k:= 2 to NumPoints do
         begin
            X2:= PolyP^[k].X;
            Y2:= PolyP^[k].Y;

            S:= S + Longint(Y2 + Y1)*(X2 - X1);

            X1:= X2;
            Y1:= Y2;
         end;
         S:= S + Longint(Y0 + Y1)*(X0 - X1);

         if S > 0 then begin
           Step:= -1;
           StPoint:= NumPoints + 1;
           EnPoint:= 1;
         end else begin
           Step:= 1;
           StPoint:= 1;
           EnPoint:= NumPoints + 1;
         end
      end;

      function RightTriangle(I1, I2, I3: Word): Boolean;
         var
            S: LongInt;
            X1, Y1, X2, Y2, X3, Y3: Integer;
      begin
         S:= 0;
         X1 := PolyP^[I1].X; Y1 := PolyP^[I1].Y;
         X2 := PolyP^[I2].X; Y2 := PolyP^[I2].Y;
         X3 := PolyP^[I3].X; Y3 := PolyP^[I3].Y;
         S:= S + Longint(Y2 + Y1)*(X2 - X1);
         S:= S + Longint(Y3 + Y2)*(X3 - X2);
         S:= S + Longint(Y1 + Y3)*(X1 - X3);
         RightTriangle:= (S < 0);
      end;

      function NormalTriangle(I1, I2, I3: Word): Boolean;
         var
            Res: Boolean;
            I  : Word;
      begin
         Res:= RightTriangle(I1, I2, I3);
         I:= I3 + Step;
         While Res and (I <> EnPoint) do
         begin
            Res:= Res and not
             (RightTriangle(I1, I2, I) and
             RightTriangle(I2, I3, I) and
             RightTriangle(I3, I1, I));
            I:= I + Step;
         end;
         NormalTriangle:= Res;
      end;

      procedure MakeTriangle(I1, I2, I3: Word);
         var
            X1, Y1, X2, Y2, X3, Y3, MnY, MxY, MxX, MnX: Integer;
           i, PJ: Word;
           P: PPolygon;
           PLT : PLTR;
      begin
         P:= PolyP;
         PLT := PLineTable;
         asm
           push  ds

           lds   si, P
           mov   bx, si
           sub   bx, 4
           mov   cl, 2
           cld

           mov   si, I1
           shl   si, cl
           add   si, bx
           lodsw
           mov   X1, ax
           lodsw
           mov   Y1, ax

           mov   si, I2
           shl   si, cl
           add   si, bx
           lodsw
           mov   X2, ax
           lodsw
           mov   Y2, ax

           mov   si, I3
           shl   si, cl
           add   si, bx
           lodsw
           mov   X3, ax
           lodsw
           mov   Y3, ax

           pop   ds
         end;

         MxY:= Y1;
         if Y2 > MxY then MxY:= Y2;
         if Y3 > MxY then MxY:= Y3;
         if MxY > MaximalY then MxY:= MaximalY;
         if MxY > T.ClipRect.B.Y then MxY := T.ClipRect.B.Y - 0;

         MnY:= Y1;
         if Y2 < MnY then MnY:= Y2;
         if Y3 < MnY then MnY:= Y3;
         if MnY < 0 then MnY:= 0;
         if MnY < T.ClipRect.A.Y then MnY := T.ClipRect.A.Y;

         MxX := T.ClipRect.B.X-0;

         if (MnY > T.ClipRect.B.Y) or (MxY < T.ClipRect.A.Y) then  exit;

         asm
           mov   cx, MxY
           sub   cx, MnY
           inc   cx

           les   di, PLT
           mov   ax, MnY
           shl   ax, 1
           shl   ax, 1
           add   di, ax

           cld

           mov   bx, MxX
           @Loop:
             mov   ax, bx
             stosw
             xor   ax, ax
             stosw
           loop   @Loop
         end;

         if Y2 < Y1 then A_Line(X1, Y1, X2, Y2, 0)
         else if Y2 > Y1 then A_Line(X1, Y1, X2, Y2, 2);

         if Y3 < Y2 then A_Line(X2, Y2, X3, Y3, 0)
         else if Y3 > Y2 then  A_Line(X2, Y2, X3, Y3, 2);

         if Y1 < Y3 then A_Line(X3, Y3, X1, Y1, 0)
         else if Y1 > Y3 then  A_Line(X3, Y3, X1, Y1, 2);

         if MnY = Start.Y then PJ := 0 else PJ := MnY - Start.Y;
         for i:= MnY to MxY do begin
            MnX := MaxInteger(PLineTable^[i].Min, T.ClipRect.A.X);
            MxX := MinInteger(PLineTable^[i].Max, T.ClipRect.B.X-0);
            Inc(PJ);
            if (MnX > T.ClipRect.B.X) or (MxX < T.ClipRect.A.X) then Continue;
            HLineStyleOp(MnX, i, MxX,T);
         end;
      end;

      function MakePoly(I1: Word; var I2: Word): Boolean;
         var I3: Word;
      begin
         MakePoly:= False;
         I3:= I2 + Step;
         while I3 <> EnPoint do
         begin
            if NormalTriangle(I1, I2, I3) then
            begin
               MakeTriangle(I1, I2, I3);
               I2:= I3;
               I3:= I2 + Step;
               MakePoly:= True;
            end
            else if not MakePoly(I2, I3) then
              exit;
         end;
      end;

      var
         i: Word;
   begin
      if NumPoints < 3 then Exit;

      PLineTable := MemAlloc(4096);
      if PLineTable = Nil then Exit;

      PolyP:= @Polygon;
      if (PolyP^[NumPoints].X = PolyP^[1].X) and
          (PolyP^[NumPoints].Y = PolyP^[1].Y)
      then
         Dec(NumPoints);

      CheckInvert;

      i:= StPoint + Step;
      if MakePoly(1, i) then;
      for i := 1 to NumPoints do begin
        Dec(PolyP^[i].X, At.X);
        Dec(PolyP^[i].Y, At.Y);
      end;
      FreeMem(PlineTable, 4096);
   end;

{Wilton grafix book, line adjacent fill}
Procedure GeneralFill(X, Y : Integer; UseBorder : Boolean; BorderColor : LongInt; var T : TPaintInfo);

Const
  MaxPoints = 1024;

Var
  ImmediateExit : Boolean;
  PrimColor     : LongInt;

  UseHLine, UseHOp : Boolean;
  HLineColor       : LongInt;
  TrapFill         : Boolean;

Type
  PPArray = ^TPArray;
  TPArray = array[0..MaxPoints] of Record
    X, Y, X2 : Word;
  End;
  PParmsArray = ^TParmsArray;
  TParmsArray = array[0..MaxPoints] of Record
    SeedX, SeedY, PrevXL, PrevXR : Integer;
    D                            : ShortInt;
  End;

Var
  R  : TRect;
  P  : PPArray;
  PC : Word;
  S  : PParmsArray;
  SC : Word;
  V  : Integer;

Procedure NewParm(SeedX, SeedY, D, PrevXL, PrevXR : Integer);
Begin
  if Sc >= MaxPoints then Exit;
  S^[SC].SeedX  := SeedX;
  S^[SC].SeedY  := SeedY;
  S^[SC].PrevXL := PrevXL;
  S^[SC].PrevXR := PrevXR;
  S^[SC].D      := D;
  Inc(SC);
End;

(*Function ColorexEvalue(Prim, Fill, Color : LongInt) : Boolean;
Begin
  ColorexEvalue := Color <> Prim;
End;

Function BorderEvalue(Prim, Fill, Color : LongInt) : Boolean;
Begin
  BorderEvalue := (Color = Prim) or (Color = Fill);
End;*)

Function  LineAdjFill : Integer; Near;
Var
  x,xr : Integer;
Begin
  xr := S^[SC-1].SeedX;
  LineAdjFill := xr;
  if SPtr <= 124 then ImmediateExit := True;
  if ImmediateExit then Exit;
  if not UseBorder then begin
    Repeat
      Dec(S^[SC-1].SeedX);
    Until (S^[SC-1].SeedX < R.A.X) or (ReadPixel(S^[SC-1].SeedX, S^[SC-1].SeedY, T) <> PrimColor);
    Inc(S^[SC-1].SeedX);
    Repeat
      Inc(XR);
    Until (XR > R.B.X) or (ReadPixel(xr, S^[SC-1].SeedY, T) <> PrimColor);
  end else begin
    Repeat
      Dec(S^[SC-1].SeedX);
      x := ReadPixel(S^[SC-1].SeedX, S^[SC-1].SeedY, T);
    Until (S^[SC-1].SeedX < R.A.X) or (x = PrimColor) or (x = T.Fore) or (x = T.Back);
    Inc(S^[SC-1].SeedX);
    Repeat
      Inc(XR);
      x := ReadPixel(xr, S^[SC-1].SeedY, T);
    Until (XR > R.B.X) or (x = PrimColor) or (x = T.Fore) or (x = T.Back);
  end;
  Dec(XR);

  if UseHLine then HLine(S^[SC-1].SeedX, S^[SC-1].SeedY, xr, HLineColor);
  if UseHOp   then HLineStyleOp(S^[SC-1].SeedX, S^[SC-1].SeedY, xr, T);
  if TrapFill then begin
    if PC >= MaxPoints * 2 then begin
      ImmediateExit := True;
      Exit;
    end;
    P^[PC].X  := S^[SC-1].SeedX;
    P^[PC].X2 := XR;
    P^[PC].Y  := S^[SC-1].SeedY;
    Inc(PC);
  end;

  if UseBorder then begin
    if (S^[SC-1].SeedY + S^[SC-1].D >= R.A.Y) and (S^[SC-1].SeedY + S^[SC-1].D < R.B.Y) then begin
      x := S^[SC-1].SeedX;
      while x <= xr do begin
        v := ReadPixel(x, S^[SC-1].SeedY+S^[SC-1].D, T);
        if (v <> PrimColor) and (v <> T.Fore) and (v <> T.Back) then begin
          NewParm(x, S^[SC-1].SeedY+S^[SC-1].D, S^[SC-1].D, S^[SC-1].SeedX, xr);
          x := LineAdjFill;
          if ImmediateExit then Exit;
          Dec(SC);
        end;
        inc(x);
      end;
    end;

    if (S^[SC-1].SeedY - S^[SC-1].D >= R.A.Y) and (S^[SC-1].SeedY - S^[SC-1].D < R.B.Y) then begin
      x := S^[SC-1].SeedX;
      while x < S^[SC-1].PrevXL do begin
        v := ReadPixel(x, S^[SC-1].SeedY-S^[SC-1].D, T);
        if (v <> PrimColor) and (v <> T.Fore) and (v <> T.Back) then begin
          NewParm(x, S^[SC-1].SeedY-S^[SC-1].D, -S^[SC-1].D, S^[SC-1].SeedX, xr);
          x := LineAdjFill;
          if ImmediateExit then Exit;
          Dec(SC);
        end;
        inc(x);
      end;

      x := S^[SC-1].PrevXR;
      while x < xr do begin
        v := ReadPixel(x, S^[SC-1].SeedY-S^[SC-1].D, T);
        if (v <> PrimColor) and (v <> T.Fore) and (v <> T.Back) then begin
          NewParm(x, S^[SC-1].SeedY-S^[SC-1].D, -S^[SC-1].D, S^[SC-1].SeedX, xr);
          x := LineAdjFill;
          if ImmediateExit then Exit;
          Dec(SC);
        end;
        inc(x);
      end;
    end;
  end else begin
    if (S^[SC-1].SeedY + S^[SC-1].D >= R.A.Y) and (S^[SC-1].SeedY + S^[SC-1].D < R.B.Y) then begin
      x := S^[SC-1].SeedX;
      while x <= xr do begin
        if ReadPixel(x, S^[SC-1].SeedY+S^[SC-1].D, T) = PrimColor then begin
          NewParm(x, S^[SC-1].SeedY+S^[SC-1].D, S^[SC-1].D, S^[SC-1].SeedX, xr);
          x := LineAdjFill;
          if ImmediateExit then Exit;
          Dec(SC);
        end;
        inc(x);
      end;
    end;

    if (S^[SC-1].SeedY - S^[SC-1].D >= R.A.Y) and (S^[SC-1].SeedY - S^[SC-1].D < R.B.Y) then begin
      x := S^[SC-1].SeedX;
      while x < S^[SC-1].PrevXL do begin
        if ReadPixel(x, S^[SC-1].SeedY-S^[SC-1].D, T) = PrimColor then begin
          NewParm(x, S^[SC-1].SeedY-S^[SC-1].D, -S^[SC-1].D, S^[SC-1].SeedX, xr);
          x := LineAdjFill;
          if ImmediateExit then Exit;
          Dec(SC);
        end;
        inc(x);
      end;

      x := S^[SC-1].PrevXR;
      while x < xr do begin
        if ReadPixel(x, S^[SC-1].SeedY-S^[SC-1].D, T) = PrimColor then begin
          NewParm(x, S^[SC-1].SeedY-S^[SC-1].D, -S^[SC-1].D, S^[SC-1].SeedX, xr);
          x := LineAdjFill;
          if ImmediateExit then Exit;
          Dec(SC);
        end;
        inc(x);
      end;
    end;
  end;

  LineAdjFill := xr;
end;

Procedure FlushFill;
Var
  I : Word;
Begin
  if PC > 0 then begin
    if UseHLine then for I := 0 to PC - 1 do HLine(P^[I].X, P^[I].Y, P^[I].X2, HLineColor)
                else for I := 0 to PC - 1 do HLineStyleOp(P^[I].X, P^[I].Y, P^[I].X2, T);
  end;
End;

Function GetPatternalia : Word;
Var
  I, J : Word;
Begin
  case T.LineStyle of
  lsLinePattern : GetPatternalia := T.LinePattern;
  lsPattern     : begin
    J := 0;
    for I := 0 to 7 do Inc(J, T.Pattern[I]);
    if J = $FF * 8 then J := $FFFF;
    GetPatternalia := J;
  end;
  else GetPatternalia := 1; end;
End;

Label _1;

var
  PD : TPoint;
  VX : Word;
  OP : Boolean;
  TX : TPaintInfo;
  OrgColor : LongInt;

Begin
  GetDeviceExtension(T.Device, R);
  R.Intersect(T.ClipRect);
  PD.X := X; PD.Y := Y;
  if not R.Contains(PD) then Exit;

  VX := GetPatternalia;
  OP := T.Operation in [CopyPut, NotPut, BlackNess, WhiteNess, NoOper];

  PrimColor := ReadPixel(X, Y, T);
  OrgColor  := PrimColor;
  if UseBorder then begin
    if BorderColor = PrimColor then Exit;
    PrimColor := BorderColor;
  end;

  PC := 0; SC := 0;
  ImmediateExit := False;

  S := MemAlloc(MaxPoints * (SizeOf(Integer) * 4 + SizeOf(ShortInt)));
  if S = Nil then Exit;

  if OP and ((VX = 0) or (VX = $FFFF)) and (T.Device = Nil) and (T.LineStyle <= lsPattern) then begin
    UseHLine := True;
    UseHOp   := False;
    TrapFill := False;
    if VX = 0 then HLineColor := T.Back else HLineColor := T.Fore;
    case T.Operation of
    NotPut    : HLineColor := not HLineColor;
    BlackNess : HLineColor := ColorIndex^[Black];
    WhiteNess : HLineColor := ColorIndex^[White];
    NoOper    : Goto _1;
    else end;
    if not UseBorder and (HLineColor = PrimColor) then Goto _1;
    TX := T;
    T.Fore := HLineColor;
    T.Back := HLineColor;
    NewParm(x, y, 1, x, x);
    LineAdjFill;  {quickest 1-pass fill}
    T := TX;
_1: FreeMem(S, MaxPoints * (SizeOf(Integer) * 4 + SizeOf(ShortInt)));
    {if ImmediateExit then GeneralFill(x, y, UseBorder, BorderColor, T);}
    Exit;
  end;

  if OP and (T.LineStyle <= lsPattern) and
   (((T.Fore <> PrimColor) and (T.Back <> PrimColor)) or
    ((T.Fore <> PrimColor) and (VX = 0)) or
    ((T.Back <> PrimColor) and (VX = $FFFF))) then begin
    UseHLine := False;
    UseHOp   := True;
    TrapFill := False;
    NewParm(x, y, 1, x, x);
    LineAdjFill;  {less qucker 1-pass fill}
    FreeMem(S, MaxPoints * (SizeOf(Integer) * 4 + SizeOf(ShortInt)));
    {if ImmediateExit then GeneralFill(x, y, UseBorder, BorderColor, T);}
    Exit;
  end;

  TX := T;
  if T.Device = Nil then begin
    UseHLine := True;
    if PrimColor = 0 then HLineColor := 8 else HLineColor := 0;
    T.Fore := HLineColor;
    T.Back := HLineColor;
    UseHOp   := False;
  end else begin
    UseHOp := True;
    T.Operation := CopyPut;
    T.LineStyle := lsLinePattern;
    T.LinePattern := $FFFF;
    UseHLine := False;
    if PrimColor = 0 then T.Fore := 8 else T.Fore := 0;
  end;
  TrapFill := True;
  P := MemAlloc(MaxPoints * 2 * SizeOf(Integer) * 3);
  if P = Nil then begin
    FreeMem(S, MaxPoints * (SizeOf(Integer) * 4 + SizeOf(ShortInt)));
    Exit;
  end;
  NewParm(x, y, 1, x, x);
  LineAdjFill;      {prefill for count}
  FreeMem(S, MaxPoints * (SizeOf(Integer) * 4 + SizeOf(ShortInt)));

  if OP then begin
    T := TX;                {2-nd pass for non-operationals}
    UseHLine := False;
    FlushFill;
    FreeMem(P, MaxPoints * 2 * SizeOf(Integer) * 3);
    Exit;
  end;

  {3rd pass for other types}
  T.Fore     := OrgColor;
  HLineColor := OrgColor;
  UseHLine := T.Device = Nil;
  FlushFill;
  T := TX;
  UseHLine := False;
  FlushFill;
  FreeMem(P, MaxPoints * 2 * SizeOf(Integer) * 3);
  {if ImmediateExit then GeneralFill(x, y, UseBorder, BorderColor, T);}
End;

Procedure FloodFillArea(x, y : Integer; var T : TPaintinfo);
Begin
  GeneralFill(x, y, False, 0, T);
End;

Procedure FloodFill(x, y : Integer; BorderColor : LongInt; var T : TPaintinfo);
Begin
  GeneralFill(x, y, True, BorderColor, T);
End;

Procedure DefaultPaint(var T : TPaintInfo);
Begin
  with T do begin
    FillChar(T, SizeOf(T), 0);
    Fore := $FFFFFFFF;
    Operation   := CopyPut;
    LineStyle   := lsLinePattern;
    LinePattern := psSolid;
    FillChar(Pattern, 8, $FF);
    LineWidth   := 1;
    Bitmap      := Nil;
    ColorRef    := Nil;
    if ScreenDriver = Nil then ClipRect.Assign(0, 0, 639, 479)
      else ClipRect.Assign(0, 0, ScreenDriver^.MaximalX, ScreenDriver^.MaximalY);
    BitmapOrg.Y := 0;
    BitmapOrg.X := 0;
    Device     := Nil;
  end;
End;

Procedure DefaultFont(var F : TFont);
Begin
  with F do begin
    FillChar(F, SizeOf(F), 0);
    Font   := 1;
    Width  := 8;
    Height := 16;
  end;
End;


Procedure Rectangle(x1, y1, x2, y2 : Integer; var T : TPaintInfo);
Var
  Divs, Vert, Hors : Byte;
Begin
  if X1 > X2 then asm
    mov ax, x1
    mov bx, x2
    mov x1, bx
    mov x2, ax
  end;
  if Y1 > Y2 then asm
    mov ax, y1
    mov bx, y2
    mov y1, bx
    mov y2, ax
  end;
  Divs := T.LineWidth shr 1;
  Vert := T.LineWidth and 1;
  Hors := Byte(not Boolean(Vert));
  Line(X1-Divs, Y1, X2+Divs-Hors, Y1, T);
  Line(X1, Y1+Divs+Vert, X1, Y2-Divs, T);
  Line(X1-Divs, Y2, X2+Divs-Hors, Y2, T);
  Line(X2, Y1+Divs+Vert, X2, Y2-Divs, T);
End;

Procedure DrawPoly(NumPoints: Word; var PolyPoints; At : TPoint; var T : TPaintInfo);
Var
  I, J : Word;
  P    : PIntegerArray;
Begin
  if NumPoints < 2 then Exit;
  J := 0; P := @PolyPoints;
  for I := 0 to NumPoints - 2 do begin
    Line(P^[J]+At.X, P^[J+1]+At.Y, P^[J+2]+At.X, P^[J+3]+At.Y, T);
    Inc(J, 2);
  end;
  Line(P^[J]+At.X, P^[J+1]+At.Y, P^[0]+At.X, P^[1]+At.Y, T);
End;

Procedure Circle(x1, y1, R : Integer; var T : TPaintInfo);
Begin
  Ellipse(x1, y1, R, R, T);
End;

Procedure FillCircle(x1, y1, R : Integer; var T : TPaintInfo);
Var
  tlw : Word;
Begin
  tlw := T.LineWidth;
  T.LineWidth := 0;
  FillEllipse(x1, y1, R, R, T);
  T.LineWidth := tlw;
End;

Procedure RoundBar(X1, Y1, X2, Y2, RX, RY : Integer; var T : TPaintInfo);
Var
  T2    : TPaintInfo;
  L     : TRect;
  DR    : Byte;
Begin
  if X1 > X2 then asm
    mov ax, x1
    mov bx, x2
    mov x1, bx
    mov x2, ax
  end;
  if Y1 > Y2 then asm
    mov ax, y1
    mov bx, y2
    mov y1, bx
    mov y2, ax
  end;
  DR := T.LineWidth shr 1;
  Inc(X1, DR);
  Inc(Y1, DR);
  Dec(X2, DR);
  Dec(Y2, DR);
  T2 := T;
  RY := MinInteger(RY, (Y2 - Y1) div 2);
  RX := MinInteger(RX, (X2 - X1) div 2);
  BarStyle(X1, Y1 + RY+1, X2, Y2 - RY-1, T);
  BarStyle(X1 + RX+1, Y1, X2 - RX-1, Y1 + RY, T);
  BarStyle(X1 + RX+1, Y2 - RY, X2 - RX-1, Y2, T);
  T2.LineWidth := 0;

  L.Assign(X1, Y1, X1 + RX, Y1 + RY);
  T2.ClipRect.Intersect(L);
  FillEllipse(X1 + RX, Y1 + RY, RX, RY, T2);

  L.Move(X2 - X1 - RX, 0);
  T2.ClipRect := T.ClipRect;
  T2.ClipRect.Intersect(L);
  FillEllipse(X2 - RX, Y1 + RY, RX, RY, T2);

  L.Move(0, Y2 - Y1 - RY);
  T2.ClipRect := T.ClipRect;
  T2.ClipRect.Intersect(L);
  FillEllipse(X2 - RX, Y2 - RY, RX, RY, T2);

  L.Move(X1 - X2 + RX, 0);
  T2.ClipRect := T.ClipRect;
  T2.ClipRect.Intersect(L);
  FillEllipse(X1 + RX, Y2 - RY, RX, RY, T2);
End;

Procedure RoundRect(X1, Y1, X2, Y2, RX, RY : Integer; var T : TPaintInfo);
Var
  T2    : TPaintInfo;
  L     : TRect;
  DR, A : Byte;
Begin
  if X1 > X2 then asm
    mov ax, x1
    mov bx, x2
    mov x1, bx
    mov x2, ax
  end;
  if Y1 > Y2 then asm
    mov ax, y1
    mov bx, y2
    mov y1, bx
    mov y2, ax
  end;
  T2 := T;
  RY := MinInteger(RY, (Y2 - Y1) div 2);
  RX := MinInteger(RX, (X2 - X1) div 2);
  DR := T.LineWidth shr 1;
  A  := T.LineWidth and 1;
  if A = 0 then A := 1 else A := 0;
  Inc(X1, DR);
  Inc(Y1, DR);
  Dec(X2, DR);
  Dec(Y2, DR);

  if T.LineWidth <> 1 then begin
    BarStyle(X1 + RX+1, Y1 - DR, X2 - RX-1, Y1 + DR-A, T);
    BarStyle(X1 + RX+1, Y2 - DR+A, X2 - RX-1, Y2 + DR, T);
    BarStyle(X1 - DR, Y1 + RY+1, X1 + DR-A, Y2 - RY-1, T);
    BarStyle(X2 - DR+A, Y1 + RY+1, X2 + DR, Y2 - RY-1, T);
  end else begin
    Line(X1+RX+1, Y1, X2-RX-1, Y1, T);
    Line(X1 + RX+1, Y2, X2 - RX-1, Y2, T);
    Line(X1, Y1+RY+0, X1, Y2-RY-0, T);
    Line(X2, Y1+RY+0, X2, Y2-RY-0, T);
  end;
  L.Assign(X1 - DR, Y1 - DR, X1 + RX, Y1 + RY);
  T2.ClipRect.Intersect(L);
  if T2.LineWidth = 1 then Ellipse(X1 + RX, Y1 + RY, RX + DR, RY + DR, T2)
    else FillEllipse(X1 + RX, Y1 + RY, RX + DR, RY + DR, T2);

  L.Move(X2 - X1 - RX + DR, 0);
  T2.ClipRect := T.ClipRect;
  T2.ClipRect.Intersect(L);
  if T2.LineWidth = 1 then
  Ellipse(X2 - RX, Y1 + RY, RX + DR, RY + DR, T2) else
  FillEllipse(X2 - RX, Y1 + RY, RX + DR, RY + DR, T2);

  L.Move(0, Y2 - Y1 - RY + DR);
  T2.ClipRect := T.ClipRect;
  T2.ClipRect.Intersect(L);
  if T2.LineWidth = 1 then
  Ellipse(X2 - RX, Y2 - RY, RX + DR, RY + DR, T2) else
  FillEllipse(X2 - RX, Y2 - RY, RX + DR, RY + DR, T2);

  L.Move(X1 - X2 + RX - DR, 0);
  T2.ClipRect := T.ClipRect;
  T2.ClipRect.Intersect(L);
  if T2.LineWidth = 1 then
  Ellipse(X1 + RX, Y2 - RY, RX + DR, RY + DR, T2) else
  FillEllipse(X1 + RX, Y2 - RY, RX + DR, RY + DR, T2);
End;

Procedure PutPixel(X, Y : Integer; var T : TPaintInfo);
Begin
  HLineStyleOp(X, Y, X, T);
End;

Function  ReadPixel(X, Y : Integer; var T : TPaintInfo) : LongInt;
Begin
  if T.Device = Nil then ReadPixel := GetPixel(X, Y) else
                         ReadPixel := GetPixelBM(X, Y, T.Device);
End;

Procedure PutImage(Image : PImage; X, Y, XFrom, YFrom, XLen, YLen : Integer; ColorRef: PColorRef; var T : TPaintInfo);
Var
  DC : PImage;
Begin
  if Image = Nil then Exit;
  if T.Device = Nil then begin
    if (T.Operation = CopyPut) and (ColorRef = Nil) then
          PutBMPPart(Image, X, Y, XFrom, YFrom, XLen, YLen)
     else PutBMPPartOp(Image, X, Y, XFrom, YFrom, XLen, YLen, T.Operation, ColorRef);
  end else begin
    {if (PSImage(Image)^.NBP and imColor) <> DIBType then begin
      DC := CreateDIBitmap(Image, 0);
      if DC = Nil then Exit;
    end else DC := Image;
    ImplantDIBitmap(DC, T.Device, X, Y, XFrom, YFrom, XLen, YLen, T.Operation, ColorRef);
    if DC <> Image then FreeDImage(DC);}
    ImplantDIBitmap(Image, T.Device, X, Y, XFrom, YFrom, XLen, YLen, T.Operation, ColorRef);
  end;
End;


Function  CreateDIBitmap(Source : PImage; Usage : Word) : PImage;
Var
  Dest          : PSImage;
  Data, Out     : PByteArray;
  NX, NY, NBP   : Word;
  Streamed      : Boolean;
  K : Byte;
  Size : LongInt;

Begin
  CreateDIBitmap := Nil;
  if Source = Nil then Exit;
  NY  := PSimage(Source)^.Y;
  NBP := PSimage(Source)^.NBP;
  NX  := PSimage(Source)^.X;
  Streamed := False;
  Size := GetImageBufferSize(NX, NY, DIBType) - 8;
  if (Size > 64200) or (MaxAvail < 64200) or
  (MemAvail < LongInt(MaxMemCanLeave) * 1024 + Size) then begin
    Dest := MemAlloc(SizeOf(TSImage));
    Dest^.Buffer := MemAlloc(NX * 4);
    if Dest^.Buffer = Nil then begin
      FreeMem(Dest, SizeOf(TSImage));
      Exit;
    end;
    if not AllocateTmpStream(PSImage(Dest)^.PS, Size, K) then begin
      FreeMem(Dest^.Buffer, NX * 4);
      FreeMem(Dest, SizeOf(TSImage));
      Exit;
    end;
    Streamed := True;
  end else Dest := MemAlloc(Size + 8 + 2);
  if Dest = Nil then Exit;
  Data := Pointer(LongInt(Source) + 8);
  CreateDIBitmap := PImage(Dest);
  Out  := PByteArray(Dest);
  Word(Pointer(Out)^) := imCheck;Inc(LongInt(Out), 2);
  Word(Pointer(Out)^) := NY;   Inc(LongInt(Out), 2);
  if Streamed then Word(Pointer(Out)^) := imStreamed + DIBType else
    Word(Pointer(Out)^) := DIBType;
  Inc(LongInt(Out), 2);
  Word(Pointer(Out)^) := NX;   Inc(LongInt(Out), 2);
  if (Usage and cbwInit) <> 0 then begin
    if (Usage and cbwClear)  <> 0 then CopyDIBits(nil, PImage(Dest), 0);
    if (Usage and cbwDelete) <> 0 then FreeDImage(Source);
    Exit;
  end;

  CopyDIBits(Source, PImage(Dest), 0);
  if (Usage and cbwDelete) <> 0 then FreeDImage(Source);
End;

Function  CreateDImage(Width, Height : Word) : PImage;
Var
  IMG : array[0..3] of Word;
Begin
  if (Width = 0) or (Height = 0) then begin
    CreateDImage := Nil;
    Exit;
  end;
  IMG[0] := imCheck; IMG[1] := Height; IMG[2] := DIBType; IMG[3] := Width;
  CreateDImage := CreateDIBitmap(@IMG, cbwInit + cbwClear);
End;

Function  CreateDImageIndirect(Width, Height, NBP, Options : Word) : PImage;
Var
  P : Pimage;
  BufSize : LongInt;
  XSize   : Word;
  I       : LongInt;
  streamed:Boolean;
Begin
  CreateDImageIndirect := Nil;
  if (Width = 0) or (Height = 0) then Exit;

  NBP := NBP and imColor;
  case NBP of
  imMono : XSize := (Width shr 3) + Byte((Width and 7) <> 0);
  imIcon,imCursor,im16,imPlaned,ImPlaned16 : XSize := (Width shr 1) + (Width and 1);
  imHiColor : XSize := Width + Width;
  imTC  : XSize := Width shl 2;
  else XSize := Width; end;
  Streamed := False;

  BufSize := LongInt(XSize) * Height + 8 + 2;
  if (BufSize > 64200) or (MaxAvail < 64200) or (MemAvail < (LongInt(MaxMemCanLeave) * 1024 + BufSize))
  or (((Options and cbwSetAlloc) <> 0) and ((Options and cbwAllocFlat) = 0))
  then begin
    if (((Options and cbwSetAlloc) <> 0) and ((Options and cbwAllocFlat) <> 0)) then Exit;
    Streamed := True;
    P := MemAlloc(SizeOf(TSImage));
    PSimage(P)^.Buffer := MemAlloc(Width * 4);
    if PSimage(P)^.Buffer = Nil then begin
      FreeMem(P, SizeOf(TSImage));
      Exit;
    end;
    BufSize := LongInt(XSize) * Height;
    AllocateTmpStream(PSImage(P)^.PS, BufSize, Byte(I));
    if PSImage(P)^.PS = Nil then begin
      FreeMem(PSimage(P)^.Buffer, Width * 4);
      FreeMem(P, SizeOf(TSImage));
      Exit;
    end;
    if Byte(I) = sttEMS then begin
      {фикс лажи EMS}
      PSImage(P)^.PS^.Seek(0);
      with PSImage(P)^.PS^ do for I := 0 to Height - 1 do
         Write(PSImage(P)^.Buffer^, XSize);
      PSImage(P)^.PS^.Reset;
      PSImage(P)^.PS^.Seek(0);
    end;
    NBP := NBP or imStreamed;
  end else P := MemAlloc(BufSize);
  if P = Nil then Exit;
  PSImage(P)^.X := Width;
  PSImage(P)^.Y := Height;
  if Streamed then NBP := NBP or imStreamed;
  PSImage(P)^.NBP := NBP;
  PSImage(P)^.Check := imCheck;
  CreateDImageIndirect := P;
End;

Procedure FreeDImage(Image : PImage);
Var
  P : PStream;
Begin
  if Assigned(Image) then begin
    if (PSImage(Image)^.X = 0) or (PSImage(Image)^.Y = 0) then Exit;
    if Word(Pointer(LongInt(Image) + 4)^) and imStreamed <> 0 then begin
      if PSImage(Image)^.PS <> Nil then begin
        Dispose(PSImage(Image)^.PS, Done);
        FreeMem(PSimage(Image)^.Buffer, PSimage(Image)^.X * 4);
        FreeMem(Image, SizeOf(TSImage));
      end;
    end else FreeMem(Image, GetImageSize(Image));
  end;
End;

Function  GetImageSize(Image : PImage) : LongInt;
Begin
  GetImageSize := GetImageBufferSize(
    PSimage(Image)^.X, PSimage(Image)^.Y, PSimage(Image)^.NBP) + 2;
End;

Function  GetImageBufferSize(X, Y, ID : Word) : LongInt;
Var
  BPLin : Word;
Begin
  case ID and imColor of
  imMono    : BPLin := X shr 3 + Byte((X and 7) <> 0);
  im256     : BPLin := X;
  imHiColor : BPLin := X + X;
  imTC      : BPLin := X shl 2;
  else BPLin := (X shr 1) + Byte((X and 1) <> 0);
  end;
  GetImageBufferSize := LongInt(BPLin) * Y + 8;
End;

Procedure RemapImage(Image : PImage; ColorRef : PColorRef);
Var
  I, J, NY, NX, K : Word;
  Data : PByteArray;
  C : Byte;

Begin
  if Word(Pointer(LongInt(Image))^) <> imCheck then Exit;
  Data := Pointer(LongInt(Image) + 8);
  NY  := Word(Pointer(LongInt(Image) + 2)^);
  NX  := Word(Pointer(LongInt(Image) + 6)^);
  if (NY = 0) or (NX = 0) then Exit;
  I := GetImageSize(Image) - 8;
  if ColorRef = Nil then ColorRef := @StdColorRefMap;
  case Word(Pointer(LongInt(Image) + 4)^) and $FFF of
  im16:begin
        NX := NX shr 1 + NX and 1;
        for I := 0 to NY - 1 do begin
          Data := MapBitLineRead(Image, I, NX);
          for J := 0 to NX - 1 do begin
            C := Data^[J];
            Data^[J] := ColorRef^[C shr 4] shl 4 + ColorRef^[C and 15];
          end;
          MapBitLineFlush(Image, I, NX);
        end;
      end;
  im256:
      for I := 0 to NY - 1 do begin
        Data := MapBitLineRead(Image, I, NX);
        for J := 0 to NX - 1 do Data^[J] := ColorRef^[Data^[J]];
        MapBitLineFlush(Image, I, NX);
      end;
  else end;
End;


Procedure GetDIBitmap(Image : PImage; X, Y : Integer);
Var
  NX, NY, NBP, NLX, I, J   : Word;
Begin
  NY  := PSimage(Image)^.Y;
  NBP := PSimage(Image)^.NBP;
  NX  := PSimage(Image)^.X;
  if (NY = 0) or (NX = 0) then Exit;
  if (NBP and imColor) <> DIBType then Exit;
  NLX := BPLine(Image);
  for I := 0 to NY - 1 do begin
    ReadScanLine(X, Y + I, NX, @ScrollMoveArea);
    MapBitLineWrite(Image, I, NLX, 0, NLX, @ScrollMoveArea);
  end;
End;

Procedure CopyDIBits(Source, Dest : PImage; ColorIfNil : LongInt);
Var
  NX, NY, NBP, BPLin, I, J, ADX : Word;
  Color : LongInt;
  PlanesPreCalc : array[0..3] of Word;
  K      : Byte;
  Data, Out     : PByteArray;
  XL, PrimSize : LongInt;
Begin
  NY  := PSimage(Dest)^.Y;
  if Source <> Nil then NBP := PSimage(Dest)^.NBP
    else NBP := PSimage(Dest)^.NBP;
  NX  := PSimage(Dest)^.X;
  if (NY = 0) or (NX = 0) then Exit;
  Out := PByteArray(Dest);
  Inc(LongInt(Out), 8);
  Data := PByteArray(Source);
  Inc(LongInt(Data), 8);
  if Source <> Nil then BPLin := BPLine(Source)
                   else BPLin := BPLine(Dest);

  if Source = Nil then begin
    if (NBP and imStreamed) <> 0 then with PSImage(Dest)^.PS^ do begin
      PrimSize := Longint(BPLin) * NY;
      Reset;
      Seek(0);
      FillChar(ScrollMoveArea, 1024, ColorIfNil);
      if PrimSize div 1024 > 0 then
      for XL := 0 to PrimSize div 1024 - 1 do Write(ScrollMoveArea, 1024);
      if PrimSize mod 1024 > 0 then Write(ScrollMoveArea, PrimSize mod 1024);
    end else FillChar(Out^, GetImageSize(Dest) - 9, ColorIfNil);
    Exit;
  end;

  BPLin := BPLine(Dest);
  for I := 0 to NY - 1 do begin
    Data := MapBMPLineRead(Source, I, Nil);
    MapBitLineWrite(Dest, I, BPLin, 0, BPLin, Data);
  end;
End;

procedure ScrollDraw(x1, y1, x2, y2, atX, atY: integer);
Var
  XLen, I, _I, J : Word;
  Increment   : ShortInt;
  P           : PByteArray;
Begin
  {$IFDEF PLOTRANGECHECK}
  if atX < 0 then begin  dec(x1, atX);  atX := 0;  end;
  if aty < 0 then begin  dec(y1, atY);  atY := 0;  end;
  if (x1 >= LineLength - 1) or (y1 >= ScreenWidth - 1)
  or (atX >= LineLength - 1) or (atY >= ScreenWidth - 1) then Exit;
  if X2 > ScreenWidth - 1 then X2 := ScreenWidth - 1;
  if Y2 > ScreenWidth - 1 then Y2 := ScreenWidth - 1;
  if (atX + x2 - x1) > ScreenWidth - 1 then  x2 := ScreenWidth - 1 - atX + x1;
  if (atY + y2 - y1) > ScreenWidth - 1 then  y2 := ScreenWidth - 1 - atY + y1;
  {$ENDIF}
  XLen := x2 - x1 + 1;

  if (LongInt(y1) * LineLength + x1) > (LongInt(aty) * LineLength + atx) then begin
    _I := atY;
    Increment := 1;
    I := Y1;
  end else begin
    _I := atY + Y2 - Y1;
    Increment := -1;
    I := Y2;
  end;

  for J := Y1 to Y2 do begin
    ReadScanLine (X1,   I, XLen, @ScrollMoveArea);
    WriteScanLine(atX, _I, XLen, @ScrollMoveArea);
    Inc(_I, Increment);
    Inc(I, Increment);
  end;
End;

procedure DrawXORFrame(x1, y1, x2, y2, Width: integer);{TONY}
var
  x, y : integer;
  up, dn: array [boolean] of byte;
  upw: word absolute up;
  dnw: word absolute dn;
  vert: word;
  switcher : boolean;
  WV1, WV2: integer;
begin
  WV1 := Width; WV2 := Width;
  {RMWbits := (3 SHL 3);  { XOR put }
  if odd(y1) then begin
    upw := $55AA;
    if odd(Width) then vert := $AA55 else vert := $55AA;
  end else begin
    upw := $AA55;
    if odd(Width) then vert := $55AA else vert := $AA55;
  end;
  if odd(y2) then dnw := $55AA else dnw := $AA55;
  switcher := false;
  if x1<0 then begin
    WV1 := x1+Width;
    x1 := 0;
  end;
  if x2 > MaximalX then begin
    WV2 := Width + (MaximalX - x2);
    x2 := Pred(MaximalX);
  end;
  for y:=0 to Width-1 do begin
    if (y1+y)>=0 then
      HLineStyleT(x1, y1+y, x2, 255, up[switcher]);
    if (y2-y)<ScreenDriver^.ScreenHeight then
    HLineStyleT(x1, y2-y, x2, 255, dn[switcher]);
    switcher := not switcher;
  end;
  if y1 + Width < 0 then y1 := -Width;
  if y2 - Width >= ScreenDriver^.ScreenHeight then
    y2 := ScreenDriver^.ScreenHeight + Width - 1;
  if WV1>0 then
    VLineStyleT(x1, y1+Width, y2-Width, WV1, 255, vert);
  if WV2>0 then begin
    if WV2 > x2 then WV2 := x2; {DK}
    VLineStyleT(x2-WV2+1, y1+Width, y2-Width, WV2, 255, vert);
  end;
  {RMWbits := 0;}
end;


Function GetVGARegister(Num : Word) : LongInt; {as R, G, B : Byte} Assembler;
Asm
    mov al, 15h
    mov ah, 10h
    mov bx, Num
    int 10h
    mov al, dh
    mov ah, ch
    mov dl, cl
    xor dh, dh
    mov cl, 2
    shl al, cl
    shl ah, cl
    shl dl, cl
End;

Procedure SetVGARegister(Num : Word; Reg : TVGARegister); Assembler;
Asm
    mov al, 10h
    mov ah, 10h
    mov bx, Num
    les di, Reg
    mov dh, es:[di]
    mov ch, es:[di+1]
    mov cl, es:[di+2]
    shr dh, 2
    shr ch, 2
    shr cl, 2
    int 10h
End;

Procedure SetEGARegister(Num, Reg : Byte); Assembler; Asm
  mov ah, 10h
  mov al, 0
  mov bl, Num
  mov bh, Reg
  int 10h
End;

Procedure SetVGAPalette(First, Count : Word; PBuf : Pointer); Assembler;
Asm
  call WaitForHRetrace
  mov dx, 03c8h
  les di, PBuf
  mov al, byte ptr First
  mov ah, al
  mov cx, Count
@@1:
  out dx, al
  inc dx
  mov al, es:[di]
  inc di
  shr al, 2
  out dx, al
  mov al, es:[di]
  inc di
  shr al, 2
  out dx, al
  mov al, es:[di]
  inc di
  shr al, 2
  out dx, al
  inc ah
  mov al, ah
  dec dx
  loop @@1
End;

Procedure GetVGAPalette(First, Count : Word; PBuf : Pointer); Assembler;
Asm
    mov ah, 10h
    mov al, 17h
    mov bx, First
    mov cx, Count
    les dx, PBuf
    int 10h

    les di, PBuf
    add di, First
    add di, First
    add di, First
    mov cx, Count
    sub cx, First
    mov ax, cx
    add cx, cx
    add cx, ax
    cld
 @@1:
    mov ax, cx
    mov cl, 2
    shl es:[di], cl
    inc di
    mov cx, ax
    loop @@1
End;

procedure WaitForRetrace; assembler;{TONY}
asm
    mov    dx, 03DAh
    mov    cl, 8
@@1:
    in     al, dx
    and    al, cl
    jnz    @@1
@@2:
    in     al, dx
    and    al, cl
    jz     @@2
end;

procedure WaitForHRetrace; assembler;{TONY}
asm
    mov    dx, 03DAh
    mov    cl, 1
@@1:
    in     al, dx
    and    al, cl
    jnz    @@1
@@2:
    in     al, dx
    and    al, cl
    jz     @@2
end;

Procedure WaitForVRetrace; Assembler; Asm
    mov     dx, 3DAh
  @@1:
    in      al, dx
    rcr     al, 1
    jnb     @@1
End;


{$L STDCRM.OBJ}
Procedure StdVGAPalette; External;
procedure StdColorRefMap; External;


Function InitGDI;
Begin
  if ScreenDriver = Nil then SelectDriver(DefaultDriver);
  InitGDI := DriverMethods._Init;
End;

Procedure DoneGDI;
Begin
  DriverMethods._Done;
  BufferedStrategy   := False;
  BuffersInPage      := False;
  ScreenDriver       := Nil;
  {$IFNDEF DPMI}
  BuffersInEms       := False;
  {$ENDIF}
End;


Procedure FailVideo; Far;
Begin
  asm
    mov ax, 3
    int 10h
  end;
  {$IFDEF RUSSIAN}
  WriteLn('(!) ������ࠩ��� �� �� ���樠����஢��');
  {$ELSE}
  WriteLn('(!) Videodriver has been not initialized');
  {$ENDIF}
  Halt(5);
End;

Function MapBitLineRead(P : PImage; Y, BPL : Word) : PByteArray;
Begin
  if (PSImage(P)^.NBP and imStreamed) <> 0 then with PSImage(P)^.PS^ do begin
    Seek(LongInt(Y) * BPL);
    Read(PSimage(P)^.Buffer^, BPL);
    {$IFNDEF DPMI} if BuffersInEms then EMSAdjSelect; {$ENDIF}
    MapBitLineRead := PSimage(P)^.Buffer;
  end else MapBitLineRead := PByteArray(LongInt(P) + 8 + Y * BPL);
End;

Procedure MapBitLineWrite(P : PImage; Y, BPL, XFrom, XLen : Word; Data : Pointer);
Begin
  if (PSImage(P)^.NBP and imStreamed) <> 0 then with PSImage(P)^.PS^ do begin
    Seek(LongInt(Y) * BPL + XFrom);
    Write(Data^, XLen);
  end else Move(Data^, Pointer(LongInt(P) + 8 + Y * BPL + XFrom)^, XLen);
End;

Procedure MapBitLineFlush(P : PImage; Y, BPL : Word);
Begin
  if (PSImage(P)^.NBP and imStreamed) <> 0 then with PSImage(P)^.PS^ do begin
    Seek(LongInt(Y) * BPL);
    Write(PSimage(P)^.Buffer^, BPL);
  end;
End;


{$L GDI.OBJ}

Procedure SetPixel; External;
Procedure SetPixelOp; External;
Function  GetPixel; External;
Function  GetPixelBM; External;
Procedure HLine; External;
Procedure VLine; External;
Procedure DisplayXxY; External;
Procedure DisplayXxYClip; External;
Procedure ReadScanLine; External;
Procedure WriteScanLine; External;
Procedure WriteScanLineOp; External;
Procedure PutBMPPart; External;
Procedure PutBMPPartOp; External;
Procedure HLineStyleOp; External;
Procedure DisplayXx8Op; External;
procedure QuickSave; External;
procedure QuickRestore; External;
procedure DirectMousePut; External;
procedure VLineStyleT; External;
Procedure DirectGetImage; External;
procedure DirectPutImage; External;
procedure PutBufferPart; External;
Function  MapBMPLineRead; External;
Procedure InitDriver; External;
Procedure DoneDriver; External;
Procedure StretchDIBitmap; External;
Procedure ImplantDIBitmap; External;
Procedure SetOutput; External;
Procedure PrepareDrawing; External;
Procedure SetUserBitBltProc; External;
Procedure SetColorBitBlt; External;
Procedure EMSAdjSelect; External;
Procedure DIBAdjSelect; External;

Function SetDIBDriver(ADIBType : Word) : Boolean;
Var
  P : PScreenDriver;
  A : LongInt;
  PD, PS : PlongArray;
Begin
  SetDIBDriver := False;
  case ADIBType and imColor of
  imMono : A := 2;
  im16, imCursor, imIcon, imPlaned, imPlaned16 : A := 16;
  im256 : A := 256;
  imHiColor : A := 65536;
  imTC  : A := 16777216;
  else Exit; end;
  P := GetAppropriateDriver(0, 0, A, False);
  if (ADIBType = imHiColor) and (P^.NumberOfColors <> 65536) then
    P := GetAppropriateDriver(0, 0, 32768, False);
  if P^.NumberOfColors <> A then Exit;
  SaveDIBDriver := ScreenDriver;
  SelectDriver(P);
  if SaveDIBDriver <> Nil then begin
    PS := Pointer(SaveDIBDriver^.Methods);
    PD := @DriverMethods;
    for A := 0 to SizeOf(TDriverMethods) div SizeOf(Pointer) - 1 do
      if not (A in DIBMethods) then PD^[A] := PS^[A];
  end;
  DIBAdjSelect;
  SetDIBDriver  := True;
End;

Function  DriverInstalled(ADIBType : Word) : Boolean;
Var
  P : PScreenDriver;
  A : LongInt;
Begin
  DriverInstalled := False;
  case ADIBType and imColor of
  imMono : A := 2;
  im16, imCursor, imIcon, imPlaned, imPlaned16 : A := 16;
  im256 : A := 256;
  imHiColor : A := 65536;
  imTC  : A := 16777216;
  else Exit; end;
  P := GetAppropriateDriver(0, 0, A, False);
  if (ADIBType = imHiColor) and (P^.NumberOfColors <> 65536) then
    P := GetAppropriateDriver(0, 0, 32768, False);
  if P^.NumberOfColors <> A then Exit;
  DriverInstalled  := True;
End;

Procedure RestoreDIBDriver;
Begin
  SelectDriver(SaveDIBDriver);
  if ScreenDriver <> nil then DIBAdjSelect;
End;


Procedure RegisterDriver(Driver : PScreenDriver);
Var
  P : PScreenDriver;
Begin
  if ScreenDrivers = Nil then ScreenDrivers := Driver else begin
    P := ScreenDrivers;
    while P^.NextDriver <> Nil do P := P^.NextDriver;
    P^.NextDriver := Driver;
  end;
  if DefaultDriver = Nil then DefaultDriver := Driver;
End;

Procedure SelectDriver(Driver : PScreenDriver);
Begin
  if Driver = Nil then Exit;
  ScreenDriver := Driver;
  Move(ScreenDriver^.Methods^, DriverMethods, SizeOf(DriverMethods));
End;

Function  GetAppropriateDriver;
Var
  P : PScreenDriver;
Begin
  if ScreenDrivers = Nil then begin
    GetAppropriateDriver := Nil;
    Exit;
  end;
  if (Colors = 0) and (Width = 0) and (Height = 0) and not Precise then begin
    GetAppropriateDriver := DefaultDriver;
    Exit;
  end;
  if Colors = 0 then Colors := MaxColors;
  P := ScreenDrivers;
  while P <> Nil do begin
    if P^.NumberOfColors = Colors then begin
      if  ((P^.ScreenWidth  div 10) = (Width  div 10))
      and ((P^.ScreenHeight div 10) = (Height div 10)) then begin
        GetAppropriateDriver := P;
        Exit;
      end;
    end;
    P := P^.NextDriver;
  end;
  if Precise then begin
    GetAppropriateDriver := Nil;
    Exit;
  end;
  P := ScreenDrivers;
  while P <> Nil do begin
    if P^.NumberOfColors = Colors then begin
      GetAppropriateDriver := P;
      Exit;
    end;
    P := P^.NextDriver;
  end;
  GetAppropriateDriver := DefaultDriver;
End;

Procedure Null; Far; Assembler; Asm End;
Procedure NullMethods;
Var
  I : Byte;
  L : array[1..100] of Pointer absolute DriverMethods;
Begin
  for I := 1 to SizeOf(DriverMethods) div SizeOf(Pointer) do L[I] := @FailVideo;
  DriverMethods._EMSAdjSelect := Null;
End;

begin
  NullMethods;
  ScreenDrivers := Nil;
  DefaultDriver := Nil;
  ColorIndex := MemAlloc(1024);
end.

