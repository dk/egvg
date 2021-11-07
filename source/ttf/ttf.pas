unit ttf;

{ TrueType Font File Parser and Scan Converter,
  Copr. 1994,95 Matthias K”ppe

  ttf.inf
}

{$X+,G+,F+,S-}

interface

uses
  Objects;

{ Basic types
}
type
  Fixed = LongInt;
  FWord = Integer;
  uFWord = Word;
  F2Dot14 = Integer;
  longDateTime = array[0..3] of Word;
  PWord = ^Word;

  PFWordArray = ^TFWordArray;
  TFWordArray = array[0..0] of FWord;

{ Table directory
}

type
  PTableHeader = ^TTableHeader;
  TTableHeader = record
    version: Fixed;
    numTables: Word;
    searchRange: Word;
    entrySelector: Word;
    rangeShift: Word;
  end;

  PTableEntry = ^TTableEntry;
  TTableEntry = record
    tag: LongInt;
    checkSum: LongInt;
    offset: LongInt;
    length: LongInt
  end;


const
  _cmap = $70616d63; { forward (Motorola) }
  _loca = $61636f6c;
  _maxp = $7078616d;
  _head = $64616568;
  _glyf = $66796c67;
  _hhea = $61656868;
  _hmtx = $78746d68;
  _OS2  = $322f534f;
  _name = $656d616e;

{ "head" - Font Header
}

const
{ Header flags
}
  hfBaselineZero = 1;
  hfLeftSideZero = 2;
  hfInstrDepSize = 4;
  hfForcePpemInt = 8;
  hfNonLAdvWidth = 16;

{ Font data hints
}
  fdhMixed = 0;
  fdhStronglyRight = 1;
  fdhRight = 2;
  fdhStronglyLeft = -1;
  fdhLeft = -2;

{ Index to Location Format
}
  ilfShort = 0;
  ilfLong = 1;

type
  PFontHeader = ^TFontHeader;
  TFontHeader = record
    version: Fixed;
    fontRevision: Fixed;
    checkSumAdjustment: LongInt;
    magicNumber: LongInt;
    flags: Word;
    unitsPerEm: Word;
    created: longDateTime;
    modified: longDateTime;
    xMin: FWord;
    yMin: FWord;
    xMax: FWord;
    yMax: FWord;
    macStyle: Word;
    lowestRecPPEM: Word;
    fontDirectionHint: Integer;
    indexToLocFormat: Integer;
    glyphDataFormat: Integer
  end;

{ "maxp" - Maximum Profile
}
type
  Pmaxp = ^Tmaxp;
  Tmaxp = record
    version: Fixed;
    numGlyphs: Word;
    maxPoints: Word;
    maxContours: Word;
    maxCompositePoints: Word;
    maxCompositeContours: Word;
    maxZones: Word;
    maxTwilightPoints: Word;
    maxStorage: Word;
    maxFunctionDefs: Word;
    maxInstructionDefs: Word;
    maxStackElements: Word;
    maxSizeOfInstructions: Word;
    maxComponentElements: Word;
    maxComponentDepth: Word
  end;

{ "cmap" - Character to Glyph Index Mapping Table
}
type
  PcmapHeader = ^TcmapHeader;
  TcmapHeader = record
    version: Word;
    numTables: Word
  end;

  PcmapEntry = ^TcmapEntry;
  TcmapEntry = record
    Platform: Word;
    Encoding: Word;
    Offset: LongInt;    { from beginning of TcmapHeader structure }
  end;

  PcmapFormatAny = ^TcmapFormatAny;
  TcmapFormatAny = record
    format: Word;
    length: Word;
    version: Word;
    data: record end;
  end;

  PcmapFormat4 = ^TcmapFormat4;
  TcmapFormat4 = record
    segCountX2: Word;
    searchRange: Word;
    entrySelector: Word;
    rangeShift: Word;
    {
    endCount: array of Word;
    reservedPad: Word;
    startCount: array of Word;
    idDelta: array of Word;
    idRangeOffset: array of Word;
    glyphIdArray: array of Word
    }
 end;

type
  Pcmap = ^Tcmap;
  Tcmap = array[0..255] of Word;

{ "glyf" - Glyph Data
}

  PGlyphDataHeader = ^TGlyphDataHeader;
  TGlyphDataHeader = record
    numberOfContours: Integer;
    xMin: FWord;
    yMin: FWord;
    xMax: FWord;
    yMax: FWord
  end;

  PSimpleGlyph = ^TSimpleGlyph;
  TSimpleGlyph = record
{   endPtsOfContours: array of Word;
    instructionLength: Word;
    instructions: array of Byte;
    flags: array of Byte;
    xCoordinates: array of Byte/Integer;
    yCoordinates: array of Byte/Integer
} end;

const
  ptOnCurve   = $01;
  ptXshort    = $02;
  ptYshort    = $04;
  ptRepeat    = $08;
  ptXsame     = $10;
  ptXshortpos = $10;
  ptYsame     = $20;
  ptYshortpos = $20;

{ "loca" - Index to Location mapping
}
type
  Ploca = pointer;
  PlocaLong = ^TlocaLong;
  TlocaLong = array[0..0] of LongInt;
  PlocaWord = ^TlocaWord;
  TlocaWord = array[0..0] of Word;

{ "hhea" - Horizontal Header
}

type
  Phhea = ^Thhea;
  Thhea = record
    TableVersion: Fixed;
    Ascender: FWord;
    Descender: FWord;
    LineGap: FWord;
    advanceWidthMax: uFWord;
    minLeftSideBearing: FWord;
    minRightSideBearing: FWord;
    xMaxExtent: FWord;
    caretSlopeRise: Integer;
    caretSlopeRun: Integer;
    Reserved: array[0..4] of Integer;
    metricDataFormat: Integer;
    numberOfHMetrics: Word
  end;

  PlongHorMetric = ^TlongHorMetric;
  TlongHorMetric = record
    advanceWidth: uFWord;
    lsb: FWord
  end;

  PLongHorMetricArray = ^TLongHorMetricArray;
  TLongHorMetricArray = array[0..0] of TLongHorMetric;

{ "OS/2": Windows Metrics
}
type
  TPanose = record
    bFamilyType: Byte;
    bSerifStyle: Byte;
    bWeight: Byte;
    bProportion: Byte;
    bContrast: Byte;
    bStrokeVariation: Byte;
    bArmStyle: Byte;
    bLetterform: Byte;
    bMidline: Byte;
    bXHeight: Byte;
  end;

type
  TOS2 = record
    version:            Word;
    xAvgCharWidth:      Integer;
    usWeightClass:      Word;
    usWidthClass:       Word;
    fsType:             Integer;
    ySubscriptXSize:    Integer;
    ySubscriptYSize:    Integer;
    ySubscriptXOffset:  Integer;
    ySubscriptYOffset:  Integer;
    ySuperscriptXSize:  Integer;
    ySuperscriptYSize:  Integer;
    ySuperscriptXOffset:Integer;
    ySuperscriptYOffset:Integer;
    yStrikeoutSize:     Integer;
    yStrikeoutPosition: Integer;
    sFamilyClass:       Integer;
    panose:             TPanose;
    ulCharRange:        array[0..3] of LongInt;
    achVendID:          array[0..3] of Char;
    fsSelection:        Word;
    usFirstCharIndex:   Word;
    usLastCharIndex:    Word;
    sTypoAscender:      Word;
    sTypoDescender:     Word;
    sTypoLineGap:       Word;
    usWinAscent:        Word;
    usWinDescent:       Word
  end;

{name section is implemented by DK Inc. 1996}
{name }
  Tnamingtable = Record
    format : Word;
    count  : Word;
    stringOffset : Word;
  End;

  Tnamerecord = record
    platformID : Word;
    specificID : Word;
    languageID : Word;
    nameID     : Word;
    length     : Word;
    offset     : Word;
  end;

{ ************************************************************************ }

type
  TPointInt = record
    x: Integer;
    y: Integer
  end;

  TRectInt = record
    A: TPointInt;
    B: TPointInt
  end;

  TPointLong = record
    x: LongInt;
    y: LongInt
  end;

  TScaled = record
    Flag: Byte;
    Point: TPointLong
  end;

  PScaledArray = ^TScaledArray;
  TScaledArray = array[0..0] of TScaled;

  PFixed = ^TFixed;
  TFixed = record
    fract: Word;
    value: Integer;
  end;

  PMat2 = ^TMat2;
  TMat2 = record
    eM11: TFixed;
    eM12: TFixed;
    eM21: TFixed;
    eM22: TFixed;
  end;

  TScaling = record
    x: Fixed;
    xy: Fixed;
    yx: Fixed;
    y: Fixed
  end;

  TMat2Dot14 = record
    b11, b12,
    b21, b22: F2Dot14
  end;

  PGlyphMetrics = ^TGlyphMetrics;
  TGlyphMetrics = record
    gmBlackBoxX: Word;
    gmBlackBoxY: Word;
    gmptGlyphOrigin: TPointInt;
    gmCellIncX: Integer;
    gmCellIncY: Integer;
  end;

  THMtx = record
    advanceWidth: TPointInt;
    lsb: TPointInt
  end;

  PPointFX = ^TPointFX;
  TPointFX = record
    x: TFixed;
    y: TFixed;
  end;

  PTTPolyCurve = ^TTTPolyCurve;
  TTTPolyCurve = record
    wType: Word;
    cpfx: Word;
    apfx: array[0..0] of TPointFX;
  end;

  PPolygonHeader = ^TPolygonHeader;
  TPolygonHeader = record
    cb: Longint;
    dwType: Longint;
    pfxStart: TPointFX;
  end;

const
  tt_Polygon_Type = 24;
  tt_Prim_Line = 1;
  tt_Prim_QSpline = 2;

{ *********************************************************************************** }

type
  PTTFont = ^TTTFont;
  TTTFont = object(TObject)
    stream: PStream;
    head: PFontHeader;
    maxp: Pmaxp;
    hhea: Phhea;
    sizept: TPointInt;
    resdpi: TPointInt;
    mat2: Tmat2;
    postscaling: integer;
    codepage: word;
    locale: boolean; {DK}
    zremap: byte; {DK}
    name:Pstring; {DK}
    os2usWinAscent, os2usWinDescent : Word; {DK}
    os2xAvgCharWidth : Integer; {DK}
{ protected }
    scaled: PScaledArray;       { current scaled point set }
    cscaled: integer;
    sscaled: word;
    scaling: TScaling;
  private
    cmap: Pcmap;
    loca: PLoca;
    sloca: LongInt;                     { loca buffer size }
    eloca: Word;                        { loca entry size }
    hmetrics: PlongHorMetricArray;      { hmtx.hmetrics }
    shmetrics: LongInt;
    leftSideBearing: PFWordArray;       { hmtx.lsb }
    sleftsidebearing: LongInt;
    iglyf: LongInt;                     { index to glyph }
    instr: pointer;
    sinstr: word;
    ip: Word;
    glyphbounds: TRectInt;
    glyphwidth: TRectInt;
    glyphheight: TRectInt;
  public
    constructor Init(S: PStream);
    destructor Done; virtual;
    function GetBitmap(Index: Integer; var Bitmap: pointer): LongInt;
    procedure GetFontBounds(var Bounds: TRectInt);
    procedure GetGlyphBounds(var Bounds: TRectInt);
    procedure GetGlyphExtent(var Width, Height: TRectInt);
    procedure GetHMtx(Index: Integer; var Mtx: THMtx);
    function GetIndexOf(Ch: Char): Integer;
    procedure GetOS2(var OS2: TOS2);
    function GetOutline(Index: Integer; var Outline: pointer): LongInt;
    procedure SetMat2(var amat2: Tmat2);
    procedure SetPointSize(X, Y: Integer);
    procedure SetPostScaling(Factor: Integer);
    procedure SetResolution(X, Y: Integer);
{ protected }
    function CreateOutline(var Outline: pointer): LongInt; { scaled -> outline }
    procedure FreeScaled;
    procedure ProcessGlyph(Index: Integer);
  private
    procedure CalcScaling;
    procedure FreeInstr;
    function GetMaxIndex: Word;
    procedure Gridfit;
    function MoveToGlyphOf(Index: Integer): LongInt;
    procedure NewInstr(Source: pointer; size: LongInt);
    function PostScale: pointer;
    procedure Readcmap;
    procedure Readhead;
    procedure Readhhea;
    procedure Readhmtx;
    procedure Readloca;
    procedure Readname;
    procedure Readmaxp;
    procedure RunInstr;                                 { single instruction }
    procedure ScaleRect(var Rect: TRectInt; x1, y1, x2, y2: FWord);
    function SkipToTTTable(ATag: LongInt): LongInt;
    procedure TransPoint(var Point: TPointLong; sx, sy: FWord);
    procedure TransPointInt(var Point: TPointInt; sx, sy: FWord);
  end;

{ ************************************************************************ }

{ Following macros produce intel-style numbers out of motorola-style
  numbers.
}
function SwapWord(w: Word): Word;
inline(
  $58/          {pop ax}
  $86/$C4);     {xchg ah, al}

function SwapLong(l: LongInt): LongInt;
inline(
  $5a/          {pop dx}
  $58/          {pop ax}
  $86/$C4/      {xchg ah, al}
  $86/$D6);     {xchg dh, dl}

function Scale(AnFWord: Word; Scaling: LongInt): LongInt;

implementation

{ Disabled debugging interfaces
}
procedure PutPixel; near; assembler; asm end;
procedure Line; near; assembler; asm end;

{$L ttf.obj (ttf.asm)           Fixed-point operations
}
procedure diviif; near; external;
procedure mulfif; near; external;
procedure mulfff; near; external;
procedure mattrans; near; external;
procedure matmul(var Sc: TScaling; var Mat2Dot14: TMat2Dot14); near; external;

{$L ttype.obj (ttype.asm)       Parsing glyphs
}
procedure OutPolygon(p: pointer; Size: Word; Dense: Word;
  ScanBuf: pointer); near; external;

var
  SplineDeltaU: LongInt;
  SplineDense: Word;

{$L castel.obj (castel.asm)     de Casteljau algorithm
}

{$L scan.obj (scan.asm)         Scan-converting
}
procedure scanSetupP(Buffer: pointer;
  TopClip, BottomClip, LeftAdd, TopBearing: Integer;
  Bitmap: pointer; DoLine: Word); near; external;
procedure scanConvBufP(Buffer: pointer); near; external;

{$L writefnt.obj (writefnt.asm) Writing to font-style bitmaps
}
type
  PWriteFntBuf = ^TWriteFntBuf;
  TWriteFntBuf = record
    ExtentX: Word;
    ExtentY: Word;
    Buffer: pointer
  end;

procedure writefntLine(ABitmap: PWriteFntBuf; x1, x2, y: Integer); near; external;

function NewWriteFntBuf(AnExtentX, AnExtentY: Word): PWriteFntBuf;
var
  Buf: PWriteFntBuf;
  size: Word;
Begin
  NewWriteFntBuf := Nil;
  New(Buf);
  if Buf = Nil then Exit;
  with Buf^ do Begin
    ExtentX := AnExtentX;
    ExtentY := AnExtentY;
    size := (ExtentX + 7) div 8 * ExtentY;
    GetMem(Buffer, size);
    if Buffer = nil then begin
      Dispose(Buf);
      Exit;
    end;
    FillChar(Buffer^, size, 0)
  End;
  NewWriteFntBuf := Buf
End;

{ ****
}

const
  ScanBufSize = $8000;
  ScanDense = 10;

function Scale(AnFWord: Word; Scaling: LongInt): LongInt; assembler;
{ Scale }
asm
       mov      cx, AnFWord
       mov      ax, WORD PTR Scaling[0]
       mov      dx, WORD PTR Scaling[2]
       call     mulfif
end;

{ TTTFont object ***********************************************************
}

constructor TTTFont.Init(S: PStream);
Var
  os2:TOS2;
Begin
  inherited Init;
  if S^.Status <> 0 then Fail;
  stream := S;
  ReadHead;
  Readmaxp;
  Readcmap;
  Readloca;
  Readname;
  Readhhea;
  Readhmtx;
  GetOS2(os2);
  os2usWinAscent   := os2.usWinAscent;
  os2usWinDescent  := os2.usWinDescent;
  os2xAvgCharWidth := os2.xAvgCharWidth;
  SkipToTTTable(_glyf);
  iglyf := Stream^.GetPos;
  with resdpi do begin
    x := 96;
    y := 96
  end;
  with sizept do begin
    x := 12;
    y := 12
  end;
  with mat2 do begin
    LongInt(eM11) := $10000;  LongInt(eM12) :=      0;
    LongInt(eM12) :=      0;  LongInt(eM22) := $10000
  end;
  PostScaling := 1;
  CalcScaling
End;

destructor TTTFont.Done;
Begin
  if Head <> Nil then Dispose(Head);
  if Cmap <> Nil then Dispose(Cmap);
  if MaxP <> Nil then Dispose(MaxP);
  if Loca <> Nil then FreeMem(Loca, sLoca);
  if Name <> Nil then DisposeStr(name);
  if leftsidebearing <> Nil then FreeMem(leftsidebearing, sleftsidebearing);
  if hmetrics <> Nil then FreeMem(hmetrics, shmetrics);
  FreeScaled;
  if Stream <> Nil then Dispose(stream, Done);
  inherited Done
End;

procedure TTTFont.CalcScaling; assembler;
asm
        les     di, Self
        les     di, es:[di].TTTFont.head
        mov     ax, es:[di].TFontHeader.unitsperem
        xchg    al, ah
        mov     cx, 72
        imul    cx
        mov     bx, dx
        mov     cx, ax
        les     di, Self

        push    bx
        push    cx
        mov     ax, es:[di].TTTFont.sizept.x
        imul    es:[di].TTTFont.resdpi.x
        call    diviif
        push    dx
        push    ax
        mov     cx, WORD PTR es:[di].TTTFont.mat2.eM11[0]
        mov     bx, WORD PTR es:[di].TTTFont.mat2.eM11[2]
        call    mulfff
        mov     WORD PTR es:[di].TTTFont.Scaling.x[0], ax
        mov     WORD PTR es:[di].TTTFont.Scaling.x[2], dx
        pop     ax
        pop     dx
        mov     cx, WORD PTR es:[di].TTTFont.mat2.eM12[0]
        mov     bx, WORD PTR es:[di].TTTFont.mat2.eM12[2]
        call    mulfff
        mov     WORD PTR es:[di].TTTFont.Scaling.xy[0], ax
        mov     WORD PTR es:[di].TTTFont.Scaling.xy[2], dx

        pop     cx
        pop     bx
        mov     ax, es:[di].TTTFont.sizept.y
        imul    es:[di].TTTFont.resdpi.y
        call    diviif

        push    dx
        push    ax
        mov     cx, WORD PTR es:[di].TTTFont.mat2.eM21[0]
        mov     bx, WORD PTR es:[di].TTTFont.mat2.eM21[2]
        call    mulfff
        mov     WORD PTR es:[di].TTTFont.Scaling.yx[0], ax
        mov     WORD PTR es:[di].TTTFont.Scaling.yx[2], dx
        pop     ax
        pop     dx
        mov     cx, WORD PTR es:[di].TTTFont.mat2.eM22[0]
        mov     bx, WORD PTR es:[di].TTTFont.mat2.eM22[2]
        call    mulfff
        mov     WORD PTR es:[di].TTTFont.Scaling.y[0], ax
        mov     WORD PTR es:[di].TTTFont.Scaling.y[2], dx
end;

function TTTFont.CreateOutline(var Outline: pointer): LongInt;
var
  PolySize: Integer;
  Poly: pointer;
  theScaled: pointer;
  countScaled: Integer;

 procedure CalcPolySize; assembler;
 asm
        push    ds                      { + }
        push    bp                      {  + }
        mov     bp, [bp]
        mov     cx, countScaled
        lds     si, theScaled
        xor     di, di
        cld
        lodsb                           { get first flag }
        jmp     @@1
@@3:
        add     di, 8
@@1:
        add     di, 16
        add     si, 8
        dec     cx
        jz      @@0
        lodsb
@@2:
        test    al, ptOnCurve
        jz      @@off

        add     di, 4
@@on:
        add     si, 8
        add     di, 8
        dec     cx
        jz      @@9

        lodsb
        test    al, 80H
        jnz     @@1

        test    al, ptOnCurve
        jnz     @@on

@@off:
        add     di, 12
        add     si, 8
        dec     cx
        jz      @@8
@@off1:
        lodsb
        test    al, 80H
        jnz     @@3

        add     si, 8
        add     di, 8
        dec     cx
        jz      @@7
        test    al, ptOnCurve
        jz      @@off1

        lodsb
        test    al, 80H
        jnz     @@1
        jmp     @@2
@@7:
        test    al, ptOnCurve
        jnz     @@9
@@8:
        add     di, 8
@@9:
@@0:
        mov     PolySize, di
        pop     bp                      {  - }
        pop     ds                      { - }
 end;

 procedure CreatePoly; assembler;
 asm
        push    ds                      { + }
        push    bp                      {  + }
        mov     bp, [bp]
        les     di, Poly
        lds     si, theScaled
        mov     bp, countScaled
        cld
        lodsb                           { get first flag }
        jmp     @@head1
@@3:                            { ** spline was last curve in poly:
                                     add poly's first point as on-curve }
        push    si
        mov     si, bx                  { PolygonHeader }
        add     si, 8                   { first point fx }
        mov     cx, 4
        rep     seges movsw
        pop     si
@@1:                            { ** complete line/spline header }
        mov     ax, di
        sub     ax, dx
        shr     ax, 3                   { byte count --> point count }
        xchg    dx, di
        mov     es:[di+2], ax
        xchg    dx, di
@@head:                         { ** new polygon: complete polygon header }
        mov     cx, di
        sub     cx, bx
        mov     es:[bx], cx             { store countbytes }
        mov     WORD PTR es:[bx+2], 0
@@head1:                        { ** create new TPolygonHeader }
        mov     bx, di                  { store @PolygonHeader }
        add     di, 4
        mov     ax, tt_Polygon_Type
        stosw
        xor     ax, ax
        stosw
        mov     cx, 4
        rep     movsw
        dec     bp
        jz      @@0                     { out of points }
        lodsb
@@2:
        mov     dx, di                  { store @PolyCurve }
        test    al, ptOnCurve
        jz      @@off

        mov     WORD PTR es:[di], tt_Prim_Line
        add     di, 4
@@on:                           { ** process on-curve points }
        mov     cx, 4
        rep     movsw
        dec     bp
        jz      @@9                     { out of points }

        lodsb
        test    al, 80H
        jnz     @@1                     { end of polygon }

        test    al, ptOnCurve
        jnz     @@on                    { next on-curve point }

                                { ** complete line header
                                  ** (next is off-curve)  }
        mov     ax, di
        sub     ax, dx
        shr     ax, 3                   { byte count --> point count }
        xchg    dx, di
        mov     es:[di+2], ax
        xchg    dx, di
        mov     dx, di
@@off:                          { ** make new spline header }
        mov     WORD PTR es:[di], tt_Prim_QSpline
        add     di, 4
        mov     cx, 4
        rep     movsw
        dec     bp
        jz      @@8                     { out of points, add 1st point }
@@off1:                         { ** process off-curve points }
        lodsb
        test    al, 80H
        jnz     @@3                     { new polygon starts }

        mov     cx, 4
        rep     movsw
        dec     bp
        jz      @@7                     { out of points: check if on }

        test    al, ptOnCurve
        jz      @@off1
                                { ** on-curve: complete spline header }
        mov     ax, di
        sub     ax, dx
        shr     ax, 3                   { byte count --> point count }
        xchg    dx, di
        mov     es:[di+2], ax
        xchg    dx, di
        lodsb
        test    al, 80H
        jnz     @@head                  { new polygon starts }
        jmp     @@2                     { new line or spline }

@@7:                            { ** out of points; if last one was off-curve,
                                     add poly's first point as on-curve }
        test    al, ptOnCurve
        jnz     @@9

@@8:                            { ** out of points;
                                     spline was last curve in poly:
                                     add poly's first point as on-curve }
        push    si
        mov     si, bx                  { PolygonHeader }
        add     si, 8                   { first point }
        mov     cx, 4
        rep     seges movsw
        pop     si
@@9:                            { ** complete polygon header }
        mov     ax, di
        sub     ax, dx
        shr     ax, 3                   { byte count --> point count }
        xchg    dx, di
        mov     es:[di+2], ax
        xchg    dx, di
@@0:
        mov     cx, di
        sub     cx, bx
        mov     es:[bx], cx             { store countbytes }
        mov     WORD PTR es:[bx+2], 0
        pop     bp                      {  - }
        pop     ds                      { - }
 end;

Begin
  countScaled := cScaled;
  theScaled := PostScale;
  CalcPolySize;
  GetMem(Poly, PolySize);
  if Poly = Nil then begin
    CreateOutline := 0;
    Exit;
  end;
  CreatePoly;
  If theScaled <> Scaled
  then FreeMem(theScaled, cScaled * SizeOf(TScaled));
  Outline := Poly;
  CreateOutline := PolySize
End;

procedure TTTFont.FreeInstr;
Begin
  If instr <> nil then Begin
    FreeMem(instr, sinstr);
    instr := nil
  End
End;

procedure TTTFont.FreeScaled;
Begin
  if scaled <> nil then
  Begin
    FreeMem(scaled, sscaled);
    scaled := nil
  End
End;

function TTTFont.GetBitmap(Index: Integer; var Bitmap: pointer): LongInt;
var
  OutLine: pointer;
  sOutLine: LongInt;
  FontBounds: TRectInt;
  ScanBuf: pointer;
{  Mtx: TLongHorMetric;
  thelsb: Integer;}
  bmpbuf: PWritefntBuf;

 { NOTE: lsb is handled pixelwise. Instead a FWord-based solution would be
         better...
 }

Begin
  sOutLine := GetOutLine(Index, OutLine);

  { Calc effective bounds
  }

  GetFontBounds(FontBounds);
{  GetHmtx(Index, Mtx);
  thelsb := Mtx.lsb;
}
  { Scan buffer
  }

  bmpbuf := NewWriteFntBuf(
    GlyphBounds.B.x - GlyphBounds.A.x {+ thelsb} + 1,
    FontBounds.B.y - FontBounds.A.y + 1);
  with bmpbuf^ do begin
    GetBitmap := (ExtentX + 7) div 8 * ExtentY;
    Bitmap := Buffer
  end;

  If Outline <> nil
  then Begin
    GetMem(ScanBuf, ScanBufSize);
    if ScanBuf = Nil then begin
      GetBitmap := 0;
      Exit;
    end;
    scanSetupP(ScanBuf,
      GlyphBounds.B.y {topclip},
      GlyphBounds.A.y {bottomclip},
      -GlyphBounds.A.x {+ thelsb {left add},
      FontBounds.B.y - GlyphBounds.B.y {top bearing},
      BmpBuf, Ofs(writefntLine));
    OutPolygon(Outline, sOutline, ScanDense, ScanBuf);
    FreeMem(OutLine, sOutLine);
    scanConvBufP(ScanBuf);
    FreeMem(scanBuf, scanBufSize);
  end;
  Dispose(BmpBuf);                      { but don't free buffer... }
End;

procedure TTTFont.GetHMtx(Index: Integer; var Mtx: THMtx);
var
  M: TLongHorMetric;
  P: TPointLong;
Begin
  If Index < SwapWord(hhea^.numberOfHMetrics)
  then M := HMetrics^[Index]
  else Begin
    M.advanceWidth := HMetrics^[hhea^.NumberOfHMetrics].advancewidth;
    M.lsb := leftsidebearing^[Index - hhea^.NumberOfHMetrics]
  End;
  with Mtx do Begin
    TransPointInt(advanceWidth, SwapWord(M.advanceWidth), 0);
    TransPointInt(lsb, SwapWord(M.lsb), 0)
  End
End;

function TTTFont.GetMaxIndex: Word; assembler;
asm
        les     di, Self
        les     si, es:[di].TTTFont.cmap
        mov     ax, 0
        mov     cx, 100H
@@1:    mov     dx, es:[si]
        add     si, 2
        cmp     dx, ax
        jbe     @@2
        mov     ax, dx
@@2:    loop    @@1
end;

function TTTFont.GetIndexOf(Ch: Char): Integer;
Begin
  GetIndexOf := cmap^[ord(ch)]
End;

procedure TTTFont.GetOS2(var OS2: TOS2);
Begin
  if SkipToTTTable(_os2) = 0 then Exit;
  Stream^.Read(OS2, SizeOf(TOS2))
End;

function TTTFont.GetOutline(Index: Integer; var Outline: pointer): LongInt;
Begin
  ProcessGlyph(Index);
  If Scaled <> nil
  then GetOutline := CreateOutline(Outline)
  else Begin
    GetOutline := 0;
    outline := nil
  End;
  FreeScaled
End;

procedure TTTFont.GetFontBounds(var Bounds: TRectInt);
Begin
  with head^ do
  ScaleRect(Bounds, SwapWord(xmin), SwapWord(ymin), SwapWord(xmax), SwapWord(ymax))
End;

procedure TTTFont.GetGlyphBounds(var Bounds: TRectInt);
Begin
  Bounds := GlyphBounds
End;

procedure TTTFont.GetGlyphExtent(var Width, Height: TRectInt);
begin
  Width := GlyphWidth;
  Height := GlyphHeight
end;

procedure TTTFont.GridFit;
Begin
End;

function TTTFont.MoveToGlyphOf(Index: Integer): LongInt;
{ MoveToGlyphOf moves the stream to the glyph of the character indiced
  Index, returning the size.
}
var
  pos1, pos2: LongInt;
Begin
  MoveToGlyphOf := 0;
  If LongMul(Index+1, eloca) >= sloca then Exit;
  If eloca = 2
  then Begin
    pos1 := LongInt(SwapWord(PlocaWord(loca)^[Index])) * 2;
    pos2 := LongInt(SwapWord(PlocaWord(loca)^[Index+1])) * 2
  End
  else Begin
    pos1 := SwapLong(PlocaLong(loca)^[Index]);
    pos2 := SwapLong(PlocaLong(loca)^[Index+1])
  End;
  MoveToGlyphOf := pos2 - pos1;
  Stream^.Seek(iglyf + pos1)
End;

procedure TTTFont.NewInstr(Source: pointer; size: LongInt);
Begin
  GetMem(Instr, size);
  if Instr = Nil then Exit;
  sInstr := size;
  Move(Source^, Instr^, size)
End;

function TTTFont.PostScale: pointer;
var
  Copy: pointer;

 procedure Mult(S, D: pointer; Count, Factor: Integer); assembler;
 asm
        push    ds
        lds     si, S
        les     di, D
        mov     cx, Count
        mov     bx, Factor
        cld
@@1:
        movsb                   { copy flag }
        lodsw
        imul    bx
        stosw
        lodsw
        imul    bx
        stosw
        lodsw
        imul    bx
        stosw
        lodsw
        imul    bx
        stosw
        loop    @@1

        pop     ds
 end;

Begin
  If PostScaling = 1
  then PostScale := Scaled
  else Begin
    GetMem(Copy, cScaled * SizeOf(TScaled));
    if Copy = Nil then begin
      Exit;
      PostScale := Nil;
    end;
    Mult(Scaled, Copy, cScaled, PostScaling);
    PostScale := Copy
  End
End;

procedure TTTFont.ProcessGlyph(Index: Integer);
var
  Buffer: pointer;
  Size: LongInt;
  NewScaled: PScaledArray;
  SizeScaled: Word;
  Count: Word;
  nContours: Word;
  InstrLen: Word;
  COfs, IOfs, FOfs, XOfs, YOfs: Word;
  Last: Word;
  Sc: TScaling;
  XValue, YValue: Word;
  CompositeData: PWordArray;
  CompFlags, OrSum: Word;
  AnIndex: Integer;
  Arg1, Arg2: Word;
  Delta, Point: TPointLong;
  param: Word;
  Mat2Dot14: TMat2Dot14;

const
  ARG_1_AND_2_ARE_WORDS         = $0001;
  ARGS_ARE_XY_VALUES            = $0002;
  ROUND_XY_TO_GRID              = $0004;
  WE_HAVE_A_SCALE               = $0008;
  MORE_COMPONENTS               = $0020;
  WE_HAVE_AN_X_AND_Y_SCALE      = $0040;
  WE_HAVE_A_TWO_BY_TWO          = $0080;
  WE_HAVE_INSTRUCTIONS          = $0100;
  USE_MY_METRICS                = $0200;

 procedure CountCoords; assembler;
 { Determines the nContours, COfs, IOfs, FOfs, XOfs, and YOfs values by counting
   the bytes used by the coordinates according to flags.
 }
 asm
        push    bp                      { + }
        mov     bp, [bp]                { BP ProcessGlyph stack frame }
        les     si, Buffer
        mov     ax, es:[si].TGlyphDataHeader.numberOfContours
        xchg    al, ah
        mov     nContours, ax
        add     si, TYPE TGlyphDataHeader
        mov     COfs, si
        shl     ax, 1
        add     si, ax                  { skip endptsofcontours[] }
        mov     ax, es:[si-2]           { get point count - 1}
        xchg    al, ah
        inc     ax
        mov     Count, ax
        mov     di, ax
        mov     ax, es:[si]
        xchg    al, ah
        mov     InstrLen, ax
        add     si, 2                   { skip instructionlength }
        mov     IOfs, si
        add     si, ax                  { skip instructions }
        mov     FOfs, si
        xor     dx, dx                  { x byte count }
        xor     bx, bx                  { y byte count }
@@1:
        mov     al, es:[si]             { get flag }
        inc     si

        { count up X bytes }

        test    al, ptXShort
        mov     cl, 1
        jnz     @@2
        test    al, ptXSame
        mov     cl, 0
        jnz     @@2
        mov     cl, 2
@@2:
        { count up Y bytes }

        test    al, ptYShort
        mov     ch, 1
        jnz     @@3
        test    al, ptYSame
        mov     ch, 0
        jnz     @@3
        mov     ch, 2
@@3:
        { process repeat bit }

        test    al, ptRepeat
        pushf                           {  + }
        xor     ah, ah
        mov     al, cl
        add     dx, ax                  { add x }
        mov     al, ch
        add     bx, ax                  { add y }
        popf                            {  - }
        jz      @@4

        mov     al, cl                  { x }
        mov     cl, es:[si]             { repeat factor }
        inc     si
        mul     cl
        add     dx, ax                  { add x }
        mov     al, ch                  { y }
        mul     cl
        add     bx, ax                  { add y }
        xor     ch, ch
        sub     di, cx
        jbe     @@0
@@4:
        dec     di
        jnz     @@1                     { process next flag }

@@0:    mov     XOfs, si
        add     si, dx
        mov     YOfs, si
        pop     bp                      { -     restore stack frame }
 end;

 procedure ScaleCoords; assembler;
 asm
        push    ds                      { + }
        push    bp                      {  + }
        mov     bp, [bp]
        lds     si, Buffer
        mov     Last, -1
        les     di, NewScaled           { Buffer for scaled }
        mov     XValue, 0               { start at 0 }
        mov     YValue, 0               { start at 0 }
        cld
@@0:
        mov     si, COfs
        mov     cx, [si]
        xchg    cl, ch
        add     si, 2
        mov     COfs, si
        mov     ax, cx
        sub     cx, Last                { this contour's point count }
        mov     Last, ax
        mov     si, FOfs
        lodsb                           { get first flag }
        or      al, 80h                 { started... }
@@1:
        mov     dx, cx
        push    cx                      {   + }
        mov     cx, 1
        test    al, ptRepeat
        jz      @@3

        add     cl, [si]
        adc     ch, 0
        inc     si
        cmp     cx, dx
        jbe     @@3
                                        { repeat count > point count }
        push    dx                      {    + }
        sub     cx, dx
        dec     cx
        mov     [si-1], cl
        sub     si, 2
        pop     cx                      {    - }

@@3:
        push    cx                      {    + }
@@2:
        push    ax                      {     + }

        { process X flag bits }

        mov     bx, XOfs
        xor     dx, dx

        test    al, ptXShort
        jz      @@x1

        mov     dl, [bx]                { short x }
        inc     bx
        test    al, ptXShortPos
        jnz     @@x0
        neg     dx
        jmp     @@x0
@@x1:                                   { long x }
        test    al, ptXSame
        jnz     @@x0

        mov     dx, [bx]
        xchg    dl, dh
        add     bx, 2
@@x0:
        mov     XOfs, bx
        add     XValue, dx

        { process Y flag bits }

        mov     bx, YOfs
        xor     dx, dx

        test    al, ptYShort
        jz      @@y1

        mov     dl, [bx]                { short x }
        inc     bx
        test    al, ptYShortPos
        jnz     @@y0
        neg     dx
        jmp     @@y0
@@y1:                                   { long x }
        test    al, ptYSame
        jnz     @@y0

        mov     dx, [bx]
        xchg    dl, dh
        add     bx, 2
@@y0:
        mov     YOfs, bx
        add     YValue, dx

        { store point }

        and     al, 81H
        stosb                           { store flags }

        push    cx                      {      + }
        push    0
        push    YValue
        push    0
        push    XValue
        lea     bx, Sc
        call    matTrans
        pop     WORD PTR es:[di]
        pop     WORD PTR es:[di+2]
        pop     WORD PTR es:[di+4]
        pop     WORD PTR es:[di+6]
        add     di, 8
        pop     cx                      {      - }

        { loop ends }

        pop     ax                      {     - }
        and     al, 7Fh                 { mask first coord bit }
        loop    @@2                     { repeat loop }

        pop     dx                      {    - }
        pop     cx                      {   - }
        lodsb
        and     al, 7Fh                 { get next flag }
        sub     cx, dx
        ja      @@1                     { point loop }

        dec     si
        mov     FOfs, si

        dec     nContours
        jnz     @@0                     { contour loop }

        pop     bp                      {  - }
        pop     ds                      { - }
 end;

 procedure ReadCompositeData; assembler;
 asm
        push    bp
        mov     bp, [bp]
        les     si, CompositeData
        cld
        seges lodsw
        xchg    al, ah
        mov     CompFlags, ax
        mov     cx, ax
        seges lodsw
        xchg    al, ah
        mov     AnIndex, ax
        test    cx, ARG_1_AND_2_ARE_WORDS
        jz      @@1
        seges lodsw
        xchg    al, ah
        mov     Arg1, ax
        seges lodsw
        xchg    al, ah
        mov     Arg2, ax
        jmp     @@0
@@1:    seges lodsb
        cbw
        mov     Arg1, ax
        seges lodsb                             { sequence due to Big Endian }
        cbw
        mov     Arg2, ax
@@0:    mov     WORD PTR CompositeData, si
        pop     bp
 end;

 function ScaleRC(AnFWord: Word; Scaling: LongInt): LongInt; assembler;
 { Scale, round conditionally }
 asm
        mov     cx, AnFWord
        mov     ax, WORD PTR Scaling[0]
        mov     dx, WORD PTR Scaling[2]
        call    mulfif
        push    bp
        mov     bp, [bp]
        test    compflags, ROUND_XY_TO_GRID
        jz      @@1
        or      ax, ax
        jns     @@1
        inc     dx                      { round up }
 @@1:
        xor     ax, ax
        pop     bp
 end;

 procedure MoveScaled(Scaled: pointer; Count: Word; var Delta: TPointLong); assembler;
 asm
        les     di, Delta
        mov     ax, WORD PTR es:[di].TPointLong.x[0]
        mov     dx, WORD PTR es:[di].TPointLong.x[2]
        mov     bx, WORD PTR es:[di].TPointLong.y[0]
        mov     si, WORD PTR es:[di].TPointLong.y[2]
        les     di, Scaled
        mov     cx, Count
@@1:
        add     es:[di+1], ax
        adc     es:[di+3], dx
        add     es:[di+5], bx
        adc     es:[di+7], si
        add     di, 9
        loop    @@1
 end;

Begin
  sc := Scaling;
  Size := MoveToGlyphOf(Index);
  If Size = 0 then Begin
    Scaled := nil;
    sScaled := 0;
    cScaled := 0;
    FillChar(GlyphBounds, 3 * SizeOf(TRectInt), 0);
    Exit
  End;
  with Stream^ do Begin
    GetMem(Buffer, Size);
    if Buffer = Nil then Exit;
    Read(Buffer^, Size);
    with PGlyphDataHeader(Buffer)^ do Begin

      If Integer(SwapWord(numberOfContours)) > 0 then Begin

        { Simple Glyph }

        CountCoords;

        SizeScaled := Count * SizeOf(TScaled);
        GetMem(NewScaled, SizeScaled);
        if NewScaled = Nil then begin
          FreeMem(Buffer, Size);
          Exit;
        end;
        ScaleCoords;
        Scaled := NewScaled;
        sScaled := SizeScaled;
        cScaled := Count;

        NewInstr(Ptr(Seg(Buffer^), IOfs), InstrLen);
        GridFit;
        FreeInstr

      End
      else Begin

        { Composite Glyph }

        SizeScaled := SwapWord(maxp^.maxCompositePoints) * SizeOf(TScaled);
        GetMem(NewScaled, SizeScaled);
        if NewScaled = Nil then begin
          FreeMem(Buffer, Size);
          Exit;
        end;
        FillChar(NewScaled^, SizeScaled, 0);
        Count := 0;

        PChar(CompositeData) := PChar(Buffer) + SizeOf(TGlyphDataHeader);
        OrSum := 0;

        Repeat

          ReadCompositeData;                    { incs CompositeData }

          OrSum := OrSum or CompFlags;

          If CompFlags and We_Have_A_Scale <> 0
          then begin
            mat2Dot14.b11 := SwapWord(PWordArray(CompositeData)^[0]);
            mat2Dot14.b12 := 0;
            mat2Dot14.b21 := 0;
            mat2Dot14.b22 := SwapWord(PWordArray(CompositeData)^[0]);
            param := 1
          end else
          if CompFlags and We_Have_An_X_And_Y_Scale <> 0
          then begin
            mat2Dot14.b11 := SwapWord(PWordArray(CompositeData)^[0]);
            mat2Dot14.b12 := 0;
            mat2Dot14.b21 := 0;
            mat2Dot14.b22 := SwapWord(PWordArray(CompositeData)^[1]);
            param := 2
          end else
          if CompFlags and We_Have_A_Two_By_Two <> 0
          then begin
            mat2Dot14.b11 := SwapWord(PWordArray(CompositeData)^[0]);
            mat2Dot14.b21 := SwapWord(PWordArray(CompositeData)^[1]);
            mat2Dot14.b12 := SwapWord(PWordArray(CompositeData)^[2]);
            mat2Dot14.b22 := SwapWord(PWordArray(CompositeData)^[3]);
            param := 4
          end else
          param := 0;

          If Param <> 0
          then begin
            matmul(Scaling, mat2Dot14);
            Inc(PtrRec(CompositeData).Ofs, param * SizeOf(Word))
          end;

          {rec} ProcessGlyph(AnIndex);          { returns in object variables }

          Scaling := Sc;

          If CompFlags and Args_Are_XY_Values <> 0
          then Begin
            TransPoint(Delta, Arg1, Arg2)

            { & wo bleibt da "RC"? }

{           Delta.x := ScaleRC(Arg1, sc.x);
            Delta.y := ScaleRC(Arg2, sc.y)
}
          End
          else Begin
            Point := PScaledArray(Scaled)^[Arg2].Point;
            Delta := PScaledArray(NewScaled)^[Arg1].Point;
            Dec(Delta.x, Point.x);
            Dec(Delta.y, Point.y)
          End;

          If Scaled <> nil then Begin
            MoveScaled(Scaled, cScaled, Delta);
            Move(Scaled^, (PChar(NewScaled) + SizeOf(TScaled)*Count)^, SizeOf(TScaled)*cScaled);
            Inc(count, cScaled);
          End;

          FreeScaled;

          if CompFlags and Use_My_Metrics <> 0
          then ;                                                                { *********** }

        Until CompFlags and More_Components = 0;

        Scaled := NewScaled;
        sScaled := SizeScaled;
        cScaled := Count;

        If OrSum and We_Have_Instructions <> 0 then Begin
          NewInstr((PChar(CompositeData) + 2), SwapWord(PWord(CompositeData)^));
          Gridfit;
          FreeInstr
        End
      End;
      ScaleRect(GlyphBounds, SwapWord(xmin), SwapWord(ymin), SwapWord(xmax), SwapWord(ymax));
      TransPointInt(GlyphWidth.A, SwapWord(xmin), 0);
      TransPointInt(GlyphWidth.B, SwapWord(xmax), 0);
      TransPointInt(GlyphHeight.A, SwapWord(ymin), 0);
      TransPointInt(GlyphHeight.B, SwapWord(ymax), 0)
    End;
    FreeMem(Buffer, Size)
  End
End;

{ Readcmap reads the character-mapping table from the TrueType file and
  creates a linear character-to-glyph table.
}

procedure TTTFont.ReadCMap;
var
  Header: TcmapHeader;
  Entry: TcmapEntry;
  FormatAny: TcmapFormatAny;
  FormatData: pointer;
  P: LongInt;
  List: pcmap;
  i: Integer;
  size: Word;
  mask: Word;

 procedure HandleFormat4; assembler;
 asm
        cld
        push    ds
        push    bp
        mov     bp, [bp]
        les     bx, List
        lds     si, FormatData
        mov     dx, [si].TcmapFormat4.segCountX2
        xchg    dl, dh
        add     si, 8
        mov     di, 0
@@5:
        mov     cx, [si]                { * endCount }
        xchg    cl, ch
        cmp     cx, -1
        jz      @@0
        and     cx, mask
        cmp     ch, 0
        jz      @@9
        mov     cx, 0ffh
@@9:
        add     si, 2
        push    si                      { }
        add     si, dx
        mov     ax, [si]                { * startCount }
        xchg    al, ah

        and     ax, Mask

        sub     cx, ax
        jb      @@10
        inc     cx                      { code count }
        shl     ax, 1
@@4:
        cmp     di, ax                  { fill up with zero }
        jae     @@3
        mov     WORD PTR es:[bx][di], 0
        add     di, 2
        jmp     @@4
@@3:
        mov     di, ax
        add     si, dx
        mov     ax, [si]                { * idDelta }
        xchg    al, ah
        add     si, dx
        cmp     WORD PTR [si], 0        { * idRangeOffset }
        jz      @@1

        push    dx                         { }
        mov     dx, [si]
        xchg    dl, dh
        add     si, dx                  { entry in glyphIdArray }
        mov     dx, ax
@@6:
        lodsw
        or      ax, ax
        jz      @@7
        xchg    al, ah
        add     ax, dx
@@7:
        mov     es:[bx][di], ax
        add     di, 2
        loop    @@6
        pop     dx                         { }
        pop     si                      { }
        jmp     @@5
@@1:
        shr     di, 1
        add     ax, di                  { first glyph index }
        shl     di, 1
@@2:
        mov     es:[bx][di], ax
        add     di, 2
        inc     ax
        loop    @@2
        pop     si                      { }
        jmp     @@5
@@10:
        pop     si
@@0:
        cmp     di, 0200H
        jae     @@8
        mov     WORD PTR es:[bx][di], 0
        add     di, 2
        jmp     @@0
@@8:
        pop     bp
        pop     ds
 end;


Begin
  If SkipToTTTable(_cmap) = 0 then Exit;
  Locale := False;
  with Stream^ do Begin
    P := GetPos;
    Read(Header, SizeOf(Header));
    For i := 1 to SwapWord(Header.numTables) do Begin
      Read(Entry, SizeOf(Entry));
      If (SwapWord(Entry.Platform) = 3) then Begin
        If SwapWord(Entry.Encoding) = 1 then begin
          mask := $FFFF;
          CodePage := $FFFF
        end else begin
          mask := $FF;
          CodePage := 0
        end;
        Seek(P + SwapLong(Entry.Offset));
        Read(FormatAny, SizeOf(FormatAny));
        size := SwapWord(FormatAny.length) - SizeOf(FormatAny);
        GetMem(FormatData, size);
        if FormatData = Nil then Exit;
        Read(FormatData^, size);
        New(List);
        if List = nil then begin
          FreeMem(FormatData, size);
          Exit;
        end;
        case SwapWord(FormatAny.Format) of
          4: HandleFormat4;
          else Begin
            Dispose(List);
            List := nil
          End;
        end;
        FreeMem(FormatData, size);
      End
    End;
  End;
  cmap := List;

  {$IFDEF RUSSIAN} {DK}
  asm
    cld
    push ds
    lds si, list
    mov cx, 256
    xor dx, dx
  @@1:
    lodsw
    or ax, ax
    jz @@2
    inc dx
  @@2:
    loop @@1
    pop ds
    mov size, dx
  end;
  Locale := Size < 127;
  for Size := 1 to 255 do begin
    if cmap^[Size] = 0 then begin
      ZRemap := Size;
      Break;
    end;
  end;
  cmap^[ZRemap] := cmap^[255];
  {$ENDIF}
End;

procedure TTTFont.Readhead;
Begin
  If SkipToTTTable(_head) = 0 then Exit;
  New(head);
  if head = nil then Exit;
  Stream^.Read(head^, SizeOf(TFontHeader));
End;

procedure TTTFont.Readhhea;
Begin
  If SkipToTTTable(_hhea) = 0 then Exit;
  New(hhea);
  if hhea = nil then Exit;
  Stream^.Read(hhea^, SizeOf(Thhea))
End;

procedure TTTFont.Readhmtx;
Begin
  If SkipToTTTable(_hmtx) = 0 then Exit;
  shmetrics := SwapWord(hhea^.numberofhmetrics) * SizeOf(TLongHorMetric);
  GetMem(hmetrics, shmetrics);
  if hmetrics = Nil then Exit;
  Stream^.Read(hmetrics^, shmetrics);
  sleftsidebearing := SwapWord(maxp^.numglyphs) - SwapWord(hhea^.numberofhmetrics);
  If sleftsidebearing > 0
  then begin
    GetMem(leftsidebearing, sleftsidebearing);
    if leftsidebearing = Nil then begin
      FreeMem(hmetrics, shmetrics);
      Exit;
    end;
    Stream^.Read(leftsidebearing^, sleftsidebearing)
  end
End;

procedure TTTFont.ReadLoca;
Begin
  If SkipToTTTable(_loca) = 0 then Exit;
  If head^.indexToLocFormat = ilfShort then eloca := 2 else eloca := 4;
  sloca := eloca * (GetMaxIndex+2 {<- DK bugfix});
  GetMem(loca, sloca);
  if loca = Nil then Exit;
  Stream^.Read(loca^, sloca);
End;

procedure TTTFont.ReadName;
Var
  S : String;
  NamingTable : Tnamingtable;
  NameRecord  : Tnamerecord;
  numNames    : Byte;
  curseek,Skip : LongInt;
Begin
  S := '';
  if SkipToTTTable(_name) = 0 then Exit;
  Skip := Stream^.GetPos;
  Stream^.Read(NamingTable, sizeof (NamingTable));
  numNames := SwapWord(NamingTable.count);
  while numNames > 0 do begin
    Stream^.Read(NameRecord, sizeof (NameRecord));
    CurSeek := Stream^.GetPos;
    if ((SwapWord(NameRecord.platformID) = 1) and
      (SwapWord(NameRecord.nameID) = 4)) then begin
      Stream^.Seek(SwapWord (NameRecord.offset) +
                   SwapWord(NamingTable.stringOffset) + Skip);
      Stream^.Read(S[1], SwapWord(NameRecord.length));
      S[0] := Char(SwapWord(NameRecord.length));
      Stream^.Seek(CurSeek);
    end;
    Dec(numNames);
  end;
  Name := NewStr(S);
End;


procedure TTTFont.Readmaxp;
Begin
  If SkipToTTTable(_maxp) = 0 then Exit;
  New(maxp);
  if maxp = Nil then Exit;
  Stream^.Read(maxp^, SizeOf(Tmaxp))
End;

procedure TTTFont.RunInstr;
Begin
End;

procedure TTTFont.ScaleRect(var Rect: TRectInt; x1, y1, x2, y2: Integer);
var
  Sc: TScaling;

        procedure ConsiderPoint(x, y: Word);
        var
          xx, yy: Integer;
        begin
          asm
                push    bp
                mov     bp, [bp]
                lea     bx, Sc
                pop     bp
                push    0
                push    y
                push    0
                push    x
                call    mattrans
                pop     ax
                pop     xx
                pop     ax
                pop     yy
          end;
          If xx < Rect.A.x then Rect.A.x := xx;
          If xx >= Rect.B.x then Rect.B.x := xx + 1;
          If yy < Rect.A.y then Rect.A.y := yy;
          If yy >= Rect.B.y then Rect.B.y := yy + 1
        end;

Begin
  Sc := Scaling;
  with Rect do begin
    A.x := MaxInt;
    A.y := MaxInt;
    B.x := - MaxInt;
    B.y := - MaxInt
  end;
  ConsiderPoint(x1, y1);
  ConsiderPoint(x1, y2);
  ConsiderPoint(x2, y1);
  ConsiderPoint(x2, y2)
End;

procedure TTTFont.SetMat2(var amat2: Tmat2);
Begin
  mat2 := amat2
End;

procedure TTTFont.SetPointSize(X, Y: Integer);
Begin
  sizept.x := x;
  sizept.y := y;
  CalcScaling
End;

procedure TTTFont.SetPostScaling(Factor: Integer);
Begin
  PostScaling := Factor
End;

procedure TTTFont.SetResolution(X, Y: Integer);
Begin
  resdpi.x := x;
  resdpi.y := y;
  CalcScaling
End;

function TTTFont.SkipToTTTable(ATag: LongInt): LongInt;
{ SkipToTTTable skips to the table specified by ATag and returns its size,
  or zero, if the table does not exist. }
var
  Header: TTableHeader;
  Entry: TTableEntry;
  i: Integer;
Begin
  { Linear search. Since the table entries are sorted by tags, a binary
    search would be more efficient.
  }
  with Stream^ do Begin
    Seek(0);
    Read(Header, SizeOf(Header));
    For i := 1 to SwapWord(Header.numTables) do Begin
      Read(Entry, SizeOf(Entry));
      If Entry.Tag = ATag then Begin
        Seek(SwapLong(Entry.offset));
        SkipToTTTable := SwapLong(Entry.length);
        Exit
      End
    End;
  End;
  SkipToTTTable := 0
End;

procedure TTTFont.TransPoint(var Point: TPointLong; sx, sy: FWord);
{ TransPoint scales and transforms the FWord point (sx,sy) to a
  fixed-coordinate point. }
var
  Sc: TScaling;
begin
  Sc := Scaling;
  asm
        push    0
        push    sy
        push    0
        push    sx
        lea     bx, Sc
        call    mattrans
        les     di, Point
        pop     WORD PTR es:[di].TPointLong.x
        pop     WORD PTR es:[di].TPointLong.x.2
        pop     WORD PTR es:[di].TPointLong.y
        pop     WORD PTR es:[di].TPointLong.y.2
  end
end;

procedure TTTFont.TransPointInt(var Point: TPointInt; sx, sy: FWord);
var
  Long: TPointLong;
begin
  TransPoint(Long, sx, sy);
  Point.x := (Long.x + $8000) shr 16;
  Point.y := (Long.y + $8000) shr 16
end;

{ No main
}

end.
