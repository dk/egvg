unit WinRes;

{ unit WinRes, Version 1.20.004 Copyright (c) 1993 Matthias K봯pe.

  winres.scr winres.skr winres.txt winres.doc
}

{$A+,B-,F-,G+,O-,R-,S-,X+,T-}

interface

uses Objects;

type
  MakeIntResource = PChar;

{ Predefined Resource Types
}
const
  rt_Cursor       = MakeIntResource(1);
  rt_Bitmap       = MakeIntResource(2);
  rt_Icon         = MakeIntResource(3);
  rt_Menu         = MakeIntResource(4);
  rt_Dialog       = MakeIntResource(5);
  rt_String       = MakeIntResource(6);
  rt_FontDir      = MakeIntResource(7);
  rt_Font         = MakeIntResource(8);
  rt_Accelerator  = MakeIntResource(9);
  rt_RCData       = MakeIntResource(10);
  rt_Group_Cursor = MakeIntResource(12);
  rt_Group_Icon   = MakeIntResource(14);
  rt_TTF_Path     = MakeIntResource(204);

type

{ Old-style EXE header
}
  TExeHeader = record
    Signature: Word;
    LastCount: Word;
    PageCount: Word;
    ReloCount: Word;
    eHdrSize:   Word;
    eMinAbove:  Word;
    eMaxAbove:  Word;
    eInitSS:    Word;
    eInitSP:    Word;
    eCheckSum:  Word;
    eInitPC:    Word;
    eInitCS:    Word;
    eRelocOfs:  Word;
    eOvlyNum:   Word;
    eRelocTab:  Word;
    eSpace:     array[1..30] of Byte;
    eNewHeader: Word;
  end;

{ New-Style EXE header
}
  TWinHeader = record
    Signature: Word;
    LinkerVer: Word;
    EntryOffs: Word;
    EntrySize: Word;
    ReservedA: array[0..3] of Byte;
    LinkerFlags: Word;
    nDataSeg: Word;
    LocalHeapSize: Word;
    StackSize: Word;
    CSIP: pointer;
    SSSP: pointer;
    nSegEntries: Word;
    nModRefEntries: Word;
    nNonResNameBytes: Word;
    SegTbl: Word;                       { offsets from Win header }
    ResourceTbl: Word;
    ResNameTbl: Word;
    ModRefTbl: Word;
    ImpNameTbl: Word;
    NonResNameTbl: LongInt;             { offset from top }
    nMovableEntryPoints: Word;
    ShiftCount: Word;
    nResourceSegs: Word;
    OS: Byte;
    Extra: Byte;
    ReservedB: array[0..7] of Byte;
  End;

{ Resource information
}
  TNameInfo = record
    rnOffset: Word;                     { in alignment units from top }
    rnLength: Word;                     { in bytes }
    rnFlags: Word;
    rnID: Word;         { offset from resource table or int value + 8000H }
    rnHandle: Word;
    rnUsage: Word;
  End;

{ Resource type information
}
  TTypeInfo = record
    rtTypeID: Word;     { offset from resource table or int value + 8000H }
    rtResourceCount: Word;
    rtReserved: array[0..3] of Byte;
{   rtNameInfo: array of TNameInfo;
}  End;

{ Resource table
}
  TResTable = record
    rscAlignShift: Word;
{   rscTypes: array of TTypeInfo;
    rscEndTypes: Word = 0;
    rscResourceNames: array of Char;
    rscEndNames: Byte = 0;
} End;

{ Data structures for font resources ***************************************
}

{ Table entry for raster fonts 2.x
}
  TRasterInfo = record                  { Win 2 fonts }
    dcWidth: Word;
    dcOffset: Word;
  End;

{ Table entry for raster fonts 3.x
}
  TNewRasterInfo = record
    dcWidth: Word;
    dcOffset: LongInt;                  { Win 3 fonts }
  End;

{ Table entry for monospaced vector fonts
}
  TFixedVectorInfo = record
    dcOffset: Word;
  End;

{ Table entry for proportionally spaced vector fonts
}
  TPropVectorInfo = record
    dcOffset: Word;
    dcWidth: Word;
  End;

{ Font resource information
}
  TFontInfo = record
{ Font dir only
}
    dfResID: Word;

{ Always present
}
    dfVersion: Word;
    dfSize: LongInt;
    dfCopyright: array[0..59] of Char;
    dfType: Word;                       { Lowest Bit = 1 -> Vector Font }
    dfPoints: Word;
    dfVertRes: Word;
    dfHorizRes: Word;
    dfAscent: Word;
    dfInternalLeading: Word;
    dfExternalLeading: Word;
    dfItalic: Byte;
    dfUnderline: Byte;
    dfStrikeOut: Byte;
    dfWeight: Word;
    dfCharset: Byte;
    dfPixWidth: Word;                   { 0 if variable-width }
    dfPixHeight: Word;
    dfPitchAndFamily: Byte;             { Lowest Bit = 1 -> variable pitch }
    dfAvgWidth: Word;
    dfMaxWidth: Word;
    dfFirstChar: Byte;
    dfLastChar: Byte;
    dfDefaultChar: Byte;
    dfBreakChar: Byte;
    dfWidthBytes: Word;
    dfDevice: Word;
    dfReserved2: Word;
    dfFace: Word;
    dfReserved3: Word;
    dfBitsPointer: pointer;
    dfBitsOffset: LongInt;
    dfReserved: Byte;

{ Version 3.x only
}
    dfFlags: Word;
    dfAspace: Word;
    dfBspace: Word;
    dfCspace: Word;
    dfColorPointer: LongInt;
    dfReserved1: Word;

{ Font dir only
}
{   devicename: array of Char;
    facename: array of Char;

{ Font resource only
}
{   dfCharTable: array of record
      case Integer of
        Raster:      (dfRasterTbl:      TRasterInfo);
        NewRaster:   (dfNewRasterTbl:   TNewRasterInfo);
        FixedVector: (dfFixedVectorTbl: TFixedVectorInfo);
        PropVector:  (dfPropVectorTbl:  TPropVectorInfo)
    end;
    bitmaps: array of Byte;
    facename: array of Char;
    devicename: array of Char;
}
  End;

const
  FontInfoExtraSize = 14;
  FontInfoBaseSize = SizeOf(TFontInfo) - FontInfoExtraSize;

type

{ Font directory
}
  TFontDir = record
    fdCount: Word;
{   fdDir: array of TFontInfo;
} End;


{ Data structures for menu resources ***************************************
}

const
  mo_TurboVision        = 0;
  mo_GraphicsVision     = 1;

const
  mf_Grayed             = 1;
  mf_Disabled           = 2;
  mf_Checked            = 8;
  mf_PopUp              = 16;
  mf_MenuBarBreak       = 32;
  mf_MenuBreak          = 64;
  mf_End                = 128;

type
  TMenuHeader = record
    wVersion: Word;
    wReserved: Word
  End;

  TPopUpMenuItem = record
    fItemFlags: Word;
{   szItemText: array of Char
} End;

  TNormalMenuItem = record
    fItemFlags: Word;
    wMenuID: Word;
{   szItemText: array of Char
} End;

{ Data structures for bitmap resources *************************************
}

type
  TPaletteEntry = record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;

  PLogPalette = ^TLogPalette;
  TLogPalette = record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[0..0] of TPaletteEntry;
  end;

{ Bitmap header definition }

type
  PBitmap = ^TBitmap;
  TBitmap = record
    bmType: Integer;
    bmWidth: Integer;
    bmHeight: Integer;
    bmWidthBytes: Integer;
    bmPlanes: Byte;
    bmBitsPixel: Byte;
    bmBits: Pointer;
    bmPalette: Pointer;
    bmFast: Pointer;
  end;

type
  TRGBTriple = record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

type
  TRGBQuad = record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;

{ Structures for defining DIBs }

type
  PBitmapCoreHeader = ^TBitmapCoreHeader;
  TBitmapCoreHeader = record
    bcSize: Longint;              { used to get to color table }
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;

type
  PBitmapInfoHeader = ^TBitmapInfoHeader;
  TBitmapInfoHeader = record
    biSize: Longint;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: Longint;
    biSizeImage: Longint;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: Longint;
    biClrImportant: Longint;
  end;

{ Constants for the biCompression field }

const
  bi_RGB  = 0;
  bi_RLE8 = 1;
  bi_RLE4 = 2;

type
  PBitmapInfo = ^TBitmapInfo;
  TBitmapInfo = record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..0] of TRGBQuad;
  end;

type
  PBitmapCoreInfo = ^TBitmapCoreInfo;
  TBitmapCoreInfo = record
    bmciHeader: TBitmapCoreHeader;
    bmciColors: array[0..0] of TRGBTriple;
  end;

type
  PBitmapFileHeader = ^TBitmapFileHeader;
  TBitmapFileHeader = record
    bfType: Word;
    bfSize: Longint;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Longint;
  end;

type

{ Extended Image (device-compatible bitmap) Definition
}
  PImage = ^TImage;
  TImage = record
    imSizeXm1: Integer;
    imSizeYm1: Integer;
    case Integer of
    -1: (
      imSize: TPoint;
      imBmpPtr: pointer;
      imFast: pointer);
    0: (
      imBitmap: array[0..0] of Byte)
  end;

{ Bitmap installable services
}
type
  TBitmapLoadProc = function(var BitmapInfoHeader: TBitmapInfoHeader;
    var S: TStream; Size: LongInt; Palette: pointer;
    CreateImage: Boolean): pointer;
  TBitmapFreeProc = procedure(Bitmap: PBitmap);
  TBitmapStoreProc = procedure(var S: TStream; Bitmap: PBitmap);
  TBitmapAllocProc = function(Bitmap: PBitmap; var Bits: pointer): pointer;

var
  BitmapLoadProc: TBitmapLoadProc;
  BitmapStoreProc: TBitmapStoreProc;
  BitmapFreeProc: TBitmapFreeProc;
  BitmapAllocProc: TBitmapAllocProc;

{ Data structures for string resources *************************************
}
type
  PStringBlock = ^TStringBlock;
  TStringBlock = record
    sbIndex: Word;
    sbSize: Word;
    sbNext: PStringBlock;
    sbData: record end
  End;

{ Data structures for cursor resources *************************************
}

type
  PCursorDirEntry = ^TCursorDirEntry;
  TCursorDirEntry = record
    wWidth: Word;
    wHeight: Word;
    wPlanes: Word;
    wBitCount: Word;
    lBytesInRes: LongInt;
    wImageIndex: Word
  end;

  PCursorRes = ^TCursorRes;
  TCursorRes = record
    crHotSpotX: Word;
    crHotSpotY: Word;
    crHeader: TBitmapInfoHeader;
{   crColors: array of TRGBQuad;
    crXOR: array of Byte;
    crAND: array of Byte
} end;

type
  PMyCursor = ^TMyCursor;
  TMyCursor = record
    mcHeight: Word;
    mcWidth: Word;
    mcHotSpotX: Word;
    mcHotSpotY: Word;
    mcAND: array[0..127] of Byte;
    mcXOR: array[0..127] of Byte
  end;

{ Data structures for dialog template resources ***************************
}

{ Window Styles }

const
  ws_Overlapped   = $00000000;
  ws_Popup        = $80000000;
  ws_Child        = $40000000;
  ws_Minimize     = $20000000;
  ws_Visible      = $10000000;
  ws_Disabled     = $08000000;
  ws_ClipSiblings = $04000000;
  ws_ClipChildren = $02000000;
  ws_Maximize     = $01000000;
  ws_Caption      = $00C00000;    { ws_Border + ws_DlgFrame }
  ws_Border       = $00800000;
  ws_DlgFrame     = $00400000;
  ws_VScroll      = $00200000;
  ws_HScroll      = $00100000;
  ws_SysMenu      = $00080000;
  ws_ThickFrame   = $00040000;
  ws_Group        = $00020000;
  ws_TabStop      = $00010000;

const
  ws_MinimizeBox = $00020000;
  ws_MaximizeBox = $00010000;

const
  ws_Tiled   = ws_Overlapped;
  ws_Iconic  = ws_Minimize;
  ws_SizeBox = ws_ThickFrame;

{ Common Window Styles }

const
  ws_OverlappedWindow = ws_Overlapped + ws_Caption + ws_SysMenu +
                        ws_ThickFrame + ws_MinimizeBox + ws_MaximizeBox;
  ws_PopupWindow      = ws_Popup + ws_Border + ws_SysMenu;
  ws_ChildWindow      = ws_Child;
  ws_TiledWindow      = ws_OverlappedWindow;

{ Extended Window Styles }

const
  ws_ex_DlgModalFrame  = $00000001;
  ws_ex_NoParentNotify = $00000004;

{ Dialog Box Command IDs }

const
  id_Ok     = 1;
  id_Cancel = 2;
  id_Abort  = 3;
  id_Retry  = 4;
  id_Ignore = 5;
  id_Yes    = 6;
  id_No     = 7;

{ Edit Control Styles }

const
  es_Left        = $0000;
  es_Center      = $0001;
  es_Right       = $0002;
  es_MultiLine   = $0004;
  es_UpperCase   = $0008;
  es_LowerCase   = $0010;
  es_Password    = $0020;
  es_AutoVScroll = $0040;
  es_AutoHScroll = $0080;
  es_NoHideSel   = $0100;
  es_OEMConvert  = $0400;

{ Button Control Styles }

const
  bs_PushButton      = $00;
  bs_DefPushButton   = $01;
  bs_CheckBox        = $02;
  bs_AutoCheckBox    = $03;
  bs_RadioButton     = $04;
  bs_3State          = $05;
  bs_Auto3State      = $06;
  bs_GroupBox        = $07;
  bs_UserButton      = $08;
  bs_AutoRadioButton = $09;
  bs_PushBox         = $0A;
  bs_OwnerDraw       = $0B;
  bs_LeftText        = $20;

{ Static Control Constants }

const
  ss_Left           = $00;
  ss_Center         = $01;
  ss_Right          = $02;
  ss_Icon           = $03;
  ss_BlackRect      = $04;
  ss_GrayRect       = $05;
  ss_WhiteRect      = $06;
  ss_BlackFrame     = $07;
  ss_GrayFrame      = $08;
  ss_WhiteFrame     = $09;
  ss_UserItem       = $0A;
  ss_Simple         = $0B;
  ss_LeftNoWordWrap = $0C;
  ss_NoPrefix       = $80;   { Don't do "&" character translation }

{ Dialog Styles }

const
  ds_AbsAlign   = $01;
  ds_SysModal   = $02;
  ds_LocalEdit  = $20;   { Edit items get Local storage }
  ds_SetFont    = $40;   { User specified font for Dlg controls }
  ds_ModalFrame = $80;   { Can be combined with ws_Caption }
  ds_NoIdleMsg  = $100;  { wm_EnterIdle message will not be sent }

{ Listbox Styles }

const
  lbs_Notify            = $0001;
  lbs_Sort              = $0002;
  lbs_NoRedraw          = $0004;
  lbs_MultipleSel       = $0008;
  lbs_OwnerDrawFixed    = $0010;
  lbs_OwnerDrawVariable = $0020;
  lbs_HasStrings        = $0040;
  lbs_UseTabStops       = $0080;
  lbs_NoIntegralHeight  = $0100;
  lbs_MultiColumn       = $0200;
  lbs_WantKeyboardInput = $0400;
  lbs_ExtendedSel       = $0800;
  lbs_Standard          = lbs_Notify + lbs_Sort + ws_VScroll + ws_Border;

{ Combo Box styles }

const
  cbs_Simple            = $0001;
  cbs_DropDown          = $0002;
  cbs_DropDownList      = $0003;
  cbs_OwnerDrawFixed    = $0010;
  cbs_OwnerDrawVariable = $0020;
  cbs_AutoHScroll       = $0040;
  cbs_OEMConvert        = $0080;
  cbs_Sort              = $0100;
  cbs_HasStrings        = $0200;
  cbs_NoIntegralHeight  = $0400;

{ Scroll Bar Styles }

const
  sbs_Horz                    = $0000;
  sbs_Vert                    = $0001;
  sbs_TopAlign                = $0002;
  sbs_LeftAlign               = $0002;
  sbs_BottomAlign             = $0004;
  sbs_RightAlign              = $0004;
  sbs_SizeBoxTopLeftAlign     = $0002;
  sbs_SizeBoxBottomRightAlign = $0004;
  sbs_SizeBox                 = $0008;

{ Dialog box template resource }

type
  PDialogBoxHeader = ^TDialogBoxHeader;
  TDialogBoxHeader = record
    lStyle: LongInt;
    bNumberOfItems: Byte;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    szMenuName: PChar;
    szClassName: PChar;
    szCaption: PChar;
    wPointSize: Word;
    szFaceName: PChar
  end;

  PControlData = ^TControlData;
  TControlData = record
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    wID: Word;
    lStyle: LongInt;
    szClass: PChar;
    szText: PChar;
    bExtraSize: Byte;
    bExtra: array[0..255] of Byte
  end;

{ Dialog/control class definition }

type
  MakeIntClass = PChar;

const
  cl_Button    = MakeIntClass($80);
  cl_Edit      = MakeIntClass($81);
  cl_Static    = MakeIntClass($82);
  cl_ListBox   = MakeIntClass($83);
  cl_ScrollBar = MakeIntClass($84);
  cl_ComboBox  = MakeIntClass($85);

type
  PLinkRec = ^TLinkRec;
  TLinkRec = record
    Next: PLinkRec;
    proc: pointer; { TLinkProc }
    control: pointer
  end;

  PDialogInfo = ^TDialogInfo;
  TDialogInfo = record
    Base: record x, y: Real end;
    Move: TPoint;
    Grow: TPoint;
    Font: Word;
    Links: PLinkRec;
    Group: Boolean;
    Dialog: pointer;
    Wake: pointer;  { TWakeProc }
  end;

  PClassRec = ^TClassRec;
  TClassRec = record
    Class: PChar;
    Init: pointer;  { TInitProc }
  end;

  TInitProc = procedure(Data: pointer; Info: PDialogInfo);
  TLinkProc = procedure(link, control: pointer);
  TWakeProc = procedure(Info: PDialogInfo);

{ Data structures for BGI and CPI files **********************************
}

{ BGI stroked font information
}
type
  TBgiFontInfo = record
    biRes1: Byte;
    biCharCount: Byte;
    biRes2: Byte;
    biRes3: Byte;
    biFirstChar: Byte;
    biVectorOffset: Word;
    biRes4: Byte;
    biPixHeight: Byte;
    biRes5: array[0..6] of Byte;
  { biOffsets: array of Word;
    biWidths: array of Byte }
  End;

{ CPI file header
}
type
  TCpiFileHeader = record
    cfhSign1: LongInt;
    cfhSign2: LongInt;
    cfhReserved: array[0..16] of Byte
  end;

{ CPI device/codepage header
}
type
  TCpiDevCpHeader = record
    cdchRes1: Word;
    cdchNext: LongInt;
    cdchRes2: Word;
    cdchDevice: array[0..7] of Char;
    cdchCodePage: Word;
    cdchRes3: array[0..11] of Byte;
    cdchCount: Word;
    cdchRes4: Word
  end;

{ CPI font header
}
type
  TCpiFontHeader = record
    cfhHeight: Byte;
    cfhWidth: Byte;
    cfhRes1: Word;
    cfhRes2: Word
  end;

{ Object types *************************************************************
}

{ Resource name collection, allowing integer and string names
}
type
  PNameCollection = ^TNameCollection;
  TNameCollection = object(TStringCollection)
    function Compare(Key1, Key2: pointer): Integer; virtual;
    procedure FreeItem(Item: pointer); virtual;
  end;

{ Procedures and functions *************************************************
}

function SkipStub(var S: TStream): Boolean;
function SkipToResTbl(var S: TStream): Boolean;
function SkipToResource(var S: TStream; Name, ResType: PChar): Boolean;
function SkipToResourceS(var S: TStream; Name, ResType: PChar): LongInt;

function ListResourceNames(var S: TStream; ResType: PChar): PNameCollection;

procedure ReadFontInfo(var S: TStream; var FontInfo: TFontInfo;
  FontDir: Boolean);
procedure ReadFontInfoExt(var S: TStream; var FontInfo: TFontInfo;
  FontDir: Boolean; DeviceName, FaceName: PString);
procedure ReadStrZ(var S: TStream; str: PString);

function LoadCursor(var S: TStream; CursorName: PChar): PMyCursor;

function LoadBitmap(var S: TStream; BitmapName: PChar): PBitmap;
function LoadBitmapImg(var S: TStream; BitmapName: PChar): pointer;
function LoadBitmapFile(var S: TStream): PBitmap;
function LoadBitmapFileImg(var S: TStream): pointer;
procedure DeleteBitmap(Bitmap: PBitmap);
procedure StoreBitmapFile(var S: TStream; Bitmap: PBitmap);

function LoadStringBlock(var S: TStream; Index: Word): PStringBlock;
procedure FreeStringBlock(Block: PStringBlock);
function GetStringFromBlock(Block: PStringBlock; Index: Word): PString;
function LoadString(var S: TStream; Index: Word): string;

procedure AnsiFrom437(Buf: PChar);
procedure AnsiFrom437Str(Str: PString);
procedure AnsiTo437(Buf: PChar);
procedure AnsiTo437Str(Str: PString);

{$IFNDEF VER60  Following routines are not defined for version 6.0.
}

function LoadMenu(var S: TStream; MenuName: PChar; Options: Word): pointer;

function LoadDialog(var S: TStream; DialogName: PChar): pointer;
procedure InitClasses;
procedure RegisterClass(var ClassRec: TClassRec);
procedure DoneClasses;
procedure InsertLink(Info: PDialogInfo; AProc: pointer; AControl: pointer);
procedure CreateLinks(Info: PDialogInfo; Link: pointer);

function ConvertMarkers(Buf: PChar; Max: Word): Boolean;

{$ENDIF}

procedure FreeMemAlign(var P: Pointer; Size: Word; PtrUp, SizeUp: Boolean);

implementation

uses Memory

{$IFNDEF VER60}
, Strings;

{$ELSE}
;

{ From Strings.pas, Borland Pascal 7.0 Runtime Library,
  Copyright (c) Borland International Inc. 1992
}
function StrIComp(Str1, Str2: PChar): Integer; assembler;
asm
        PUSH    DS
        CLD
        LES     DI,Str2
        MOV     SI,DI
        MOV     CX,0FFFFH
        XOR     AX,AX
        CWD
        REPNE   SCASB
        NOT     CX
        MOV     DI,SI
        LDS     SI,Str1
@@1:    REPE    CMPSB
        JE      @@4
        MOV     AL,DS:[SI-1]
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    MOV     DL,ES:[DI-1]
        CMP     DL,'a'
        JB      @@3
        CMP     DL,'z'
        JA      @@3
        SUB     DL,20H
@@3:    SUB     AX,DX
        JE      @@1
@@4:    POP     DS
end;

{$ENDIF}

procedure FreeMemAlign(var P: Pointer; Size: Word; PtrUp, SizeUp: Boolean);
  {$IFDEF MSDOS}
var
  n: Word;
Begin
  with PtrRec(P) do Begin
    If PtrUp
    then n := (Ofs + 7) and $FFF8
    else n := Ofs and $FFF8;
    Inc(Size, Ofs - n);
    If not SizeUp then Size := Size and $FFF8;
    Ofs := n;
    Inc(Seg, Ofs shr 4);
    Ofs := Ofs and $8
  End;
  {$ELSE}
Begin
  {$ENDIF}
  FreeMem(P, Size);
End;

{ Internal structure
}
type
  TResInt = record
    riAlignShift: Word;
    riTablePos: LongInt;
    riStream: PStream;
  end;

function SkipStub(var S: TStream): Boolean;
var
  ExeHeader: TExeHeader;
begin
  SkipStub := false;
  If S.GetPos > S.GetSize - SizeOf(TExeHeader) then Exit;
  S.Read(ExeHeader, SizeOf(TExeHeader));
  If (ExeHeader.Signature <> $5A4D) or (ExeHeader.eRelocOfs < $40) then Exit;
  S.Seek(ExeHeader.eNewHeader);
  SkipStub := S.Status = 0
End;

function SkipToResTbl(var S: TStream): Boolean;
var
  WinHeader: TWinHeader;
  HeaderPos: LongInt;
begin
  SkipToResTbl := false;
  HeaderPos := S.GetPos;
  If HeaderPos > S.GetSize - SizeOf(TWinHeader) then Exit;
  S.Read(WinHeader, SizeOf(TWinHeader));
  If WinHeader.Signature <> $454E then Exit;
  S.Seek(HeaderPos + WinHeader.ResourceTbl);
  SkipToResTbl := S.Status = 0
End;

function CompareStrings(Key: PChar; Offset: Word; var ResInt: TResInt): Boolean;
var
  P: LongInt;
  c: array[0..255] of Char;
  l: Byte;
Begin
  CompareStrings := false;
  with ResInt.riStream^ do Begin
    P := GetPos;
    Seek(ResInt.riTablePos + Offset);
    Read(l, 1);
    Read(c, l);
    Seek(P);
  End;
  c[l] := #0;
  CompareStrings := StrIComp(Key, @c) = 0;
End;

function Compare(Key: PChar; ID: Word; var ResInt: TResInt): Boolean;
Begin
  If LongRec(Key).Hi = 0
    then if ID < $8000
      then Compare := false
      else Compare := LongRec(Key).Lo = ID and $7FFF
    else if ID < $8000
      then Compare := CompareStrings(Key, ID, ResInt)
      else Compare := false
End;

function ReadNameStr(Offset: Word; var ResInt: TResInt): PString;
var
  P: LongInt;
  l: Byte;
  t: PString;
Begin
  with ResInt.riStream^ do Begin
    P := GetPos;
    Seek(ResInt.riTablePos + Offset);
    Read(l, 1);
    GetMem(t, l + 1);
    t^[0] := Chr(l);
    Read(t^[1], l);
    Seek(P)
  End;
  ReadNameStr := t
End;

{ This procedure fills the ResInt structure. }

function SkipToResType(var S: TStream; ResType: PChar;
  var ResInt: TResInt): Word;
var
  ResTable: TResTable;
  TypeInfo: TTypeInfo;
Begin
  SkipToResType := 0;
  ResInt.riTablePos := S.GetPos;
  S.Read(ResInt.riAlignShift, SizeOf(TResTable));
  ResInt.riStream := @S;
  Repeat
    S.Read(TypeInfo, SizeOf(TTypeInfo));
    If TypeInfo.rtTypeID = 0 then Exit else
    if Compare(ResType, TypeInfo.rtTypeID, ResInt) then Begin
      SkipToResType := TypeInfo.rtResourceCount;
      Exit
    End else
      S.Seek(S.GetPos + TypeInfo.rtResourceCount * SizeOf(TNameInfo))
  Until false
End;

function SkipToResourceS(var S: TStream; Name, ResType: PChar): LongInt;
var
  Count: Word;
  NameInfo: TNameInfo;
  ResInt: TResInt;
  i: Word;
begin
  SkipToResourceS := 0;
  For i := 1 to SkipToResType(S, ResType, ResInt) do Begin
    S.Read(NameInfo, SizeOf(TNameInfo));
    If (Name = nil) or Compare(Name, NameInfo.rnID, ResInt) then Begin
      S.Seek(LongInt(NameInfo.rnOffset) shl ResInt.riAlignShift);
      { Size is given in alignment units, and not in bytes, as the
        documentation erroneously states. }
      SkipToResourceS := NameInfo.rnLength shl ResInt.riAlignShift;
      Exit
    End;
  End;
End;

function SkipToResource(var S: TStream; Name, ResType: PChar): Boolean;
Begin
  SkipToResource := SkipToResourceS(S, Name, ResType) <> 0
End;

function ListResourceNames(var S: TStream; ResType: PChar): PNameCollection;
var
  coll: PNameCollection;
  NameInfo: TNameInfo;
  ResInt: TResInt;
  i, count: Word;
  n: LongInt;
Begin
  count := SkipToResType(S, ResType, ResInt);
  If count = 0 then Begin
    ListResourceNames := nil;
    Exit
  End;
  coll := New(PNameCollection, Init(count, 8));
  For i := 1 to count do Begin
    S.Read(NameInfo, SizeOf(TNameInfo));
    If NameInfo.rnID < $8000
    then coll^.Insert(ReadNameStr(NameInfo.rnID, ResInt))
    else Begin
      n := NameInfo.rnID and $7FFF;
      coll^.Insert(pointer(n))
    End
  End;
  ListResourceNames := coll
End;

{ TNameCollection object 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
}

function TNameCollection.Compare(Key1, Key2: pointer): Integer;
Begin
  If (Seg(Key1^) = 0) or (Seg(Key2^) = 0) then
    if LongInt(Key1) > LongInt(Key2) then Compare := +1 else
    if LongInt(Key1) < LongInt(Key2) then Compare := -1 else Compare := 0
  else Compare := TStringCollection.Compare(Key1, Key2)
End;

procedure TNameCollection.FreeItem(Item: pointer);
Begin
  If Seg(Item^) <> 0 then TStringCollection.FreeItem(Item)
End;

{ Font routines 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
}

procedure ReadFontInfo(var S: TStream; var FontInfo: TFontInfo;
  FontDir: Boolean);
begin
  ReadFontInfoExt(S, FontInfo, FontDir, nil, nil)
end;

procedure ReadStrZ(var S: TStream; str: PString);
{ bwah }
var
  C: Char;
begin
  if str <> nil then str^ := '';
  Repeat
    S.Read(C, 1);
    If (C <> #0) and (str <> nil)
    then str^ := str^ + C;
  until C = #0;
end;

procedure ReadFontInfoExt(var S: TStream; var FontInfo: TFontInfo;
  FontDir: Boolean; DeviceName, FaceName: PString);
Begin
  FillChar(FontInfo, SizeOf(TFontInfo), 0);
  If FontDir
    then S.Read(FontInfo, FontInfoBaseSize - 5)
    else Begin
      FontInfo.dfResID:=0;
      S.Read(FontInfo.dfVersion, FontInfoBaseSize - 3);
      case FontInfo.dfVersion of
        $200:
          S.Read(FontInfo.dfReserved, 1);
        $300:
        Begin
          S.Read(FontInfo.dfReserved, 1);
          S.Read(FontInfo.dfFlags, FontInfoExtraSize);
          S.Seek(S.GetPos + 16)
        End;
      End;
    End;
  If FontDir then Begin
    ReadStrZ(S, DeviceName);
    ReadStrZ(S, FaceName);
  End
End;

{ Cursor routines 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
}

procedure ConvertMask(var Source; var Dest); assembler;
asm
        push    ds
        lds     si, Source
        les     di, Dest
        cld
        mov     cx, 32
        add     di, 64
@@1:    sub     di, 2
        lodsw
        xchg    ah, al
        mov     es:[di], ax
        lodsw
        xchg    ah, al
        mov     es:[di].64, ax
        loop    @@1
        pop     ds
end;

function LoadCursor(var S: TStream; CursorName: PChar): PMyCursor;
var
  p: LongInt;
  c: PMyCursor;
  CursorRes: TCursorRes;
  Mask: array[0..127] of Byte;
Begin
  p := S.GetPos;
  c := nil;
  If SkipToResource(S, CursorName, rt_Cursor) then Begin
    S.Read(CursorRes, SizeOf(TCursorRes));
    New(c);
    with c^ do
    with CursorRes do Begin
      mcHotSpotX := crHotSpotX;
      mcHotSpotY := crHotSpotY;
      with crHeader do Begin
        mcHeight := 32 {biHeight div 2};
        mcWidth := 4 {biHeight div 8};
        S.Seek(S.GetPos + 1 shl biBitCount * SizeOf(TRGBQuad))
      End;
      S.Read(Mask, 128);
      ConvertMask(mask, mcXOR);
      S.Read(Mask, 128);
      ConvertMask(mask, mcAND)
    End
  End;
  S.Seek(p);
  LoadCursor := c
End;

{ Bitmap routines 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
}

{ Load Bitmap
}

procedure TripleToQuad(var Triple, Quad; Count: Word); near; external;

{$L wrespal.obj (wrespal.asm) }

function PaletteLoad(var BitmapInfoHeader: TBitmapInfoHeader;
  var S: TStream): pointer; near;
var
  palsize, tempsize: Word;
  pal, temp: pointer;
Begin
  with BitmapInfoHeader do Begin
    palsize := SizeOf(TRGBQuad) shl biBitCount;
    GetMem(pal, palsize);
    If biSize = SizeOf(TBitmapCoreHeader)
    then begin
      tempsize := SizeOf(TRGBTriple) shl biBitCount;
      GetMem(temp, tempsize);
      S.Read(temp^, tempsize);
      TripleToQuad(temp^, pal^, 1 shl biBitCount);
      FreeMem(temp, tempsize)
    end
    else
      S.Read(pal^, palsize)
  End;
  PaletteLoad := pal
End;

function StdBitmapAlloc(Bitmap: PBitmap; var Bits: pointer): pointer; far;
begin
  with Bitmap^ do
    GetMem(Bits, abs(bmHeight) * bmWidthBytes * bmPlanes);
  StdBitmapAlloc := nil
end;

function StdBitmapLoad(var BitmapInfoHeader: TBitmapInfoHeader;
  var S: TStream; Size: LongInt; Palette: pointer;
  CreateImage: Boolean): pointer; far;
var
  Bitmap: PBitmap;
Begin
  with BitmapInfoHeader do
  If not CreateImage and (biCompression = bi_RGB) then Begin
    New(Bitmap);
    FillChar(Bitmap^, SizeOf(TBitmap), 0);
    with Bitmap^ do Begin
      bmWidth := biWidth;
      bmHeight := - biHeight;
      bmPlanes := biPlanes;
      bmBitsPixel := biBitCount div biPlanes;
      bmWidthBytes := ((bmWidth * bmBitsPixel + 31) shr 3) and $FFFC;
      bmPalette := Palette;
      bmFast := StdBitmapAlloc(Bitmap, bmBits);
      If bmBits <> nil
      then S.Read(bmBits^, abs(bmHeight) * bmWidthBytes * bmPlanes);
    End;
    StdBitmapLoad := Bitmap;
  End
  else StdBitmapLoad := nil;
End;

function DoLoadBitmap(var S: TStream; Size: LongInt;
  CreateImage: Boolean): pointer; near;
var
  Bitmap: PBitmap;
  BitmapInfoHeader: TBitmapInfoHeader;
  pal: pointer;
Begin
  DoLoadBitmap := nil;
  Inc(Size, S.GetPos);
  with BitmapInfoHeader do Begin
    S.Read(biSize, 4);
    If biSize > SizeOf(TBitmapInfoHeader) then Exit;
    S.Read(biWidth, biSize - 4);
    If biSize = SizeOf(TBitmapCoreHeader) then Begin
      biCompression := bi_RGB;
      FillChar(biSizeImage, 20, 0)
    End;
  End;
  If S.Status <> stOk then Exit;
  pal := PaletteLoad(BitmapInfoHeader, S);
  Dec(Size, S.GetPos);
  DoLoadBitmap := BitmapLoadProc(BitmapInfoHeader, S, Size, pal, CreateImage)
End;

function DoLoadBitmapRes(var S: TStream; BitmapName: PChar;
  CreateImage: Boolean): pointer; near;
var
  p, Size: LongInt;
Begin
  p := S.GetPos;
  Size := SkipToResourceS(S, BitmapName, rt_Bitmap);
  If Size <> 0
  then DoLoadBitmapRes := DoLoadBitmap(S, Size, CreateImage)
  else DoLoadBitmapRes := nil;
  S.Seek(P);
End;

function DoLoadBitmapFile(var S: TStream; CreateImage: Boolean): PBitmap; near;
var
  BitmapFileHeader: TBitmapFileHeader;
Begin
  S.Read(BitMapFileHeader, SizeOf(TBitmapFileHeader));
  with BitMapFileHeader do
    If bfType = $4D42
    then DoLoadBitmapFile := DoLoadBitmap(S, bfSize, CreateImage)
    else DoLoadBitmapFile := nil
End;

function LoadBitmap(var S: TStream; BitmapName: PChar): PBitmap; {public}
Begin
  LoadBitmap := DoLoadBitmapRes(S, BitmapName, false)
End;

function LoadBitmapImg(var S: TStream; BitmapName: PChar): pointer; {public}
Begin
  LoadBitmapImg := DoLoadBitmapRes(S, BitmapName, true)
End;

function LoadBitmapFile(var S: TStream): PBitmap; {public}
Begin
  LoadBitmapFile := DoLoadBitmapFile(S, false)
End;

function LoadBitmapFileImg(var S: TStream): pointer; {public}
Begin
  LoadBitmapFileImg := DoLoadBitmapFile(S, true)
End;

{ Free Bitmap
}

procedure StdBitmapFree(Bitmap: PBitmap); far;
Begin
  with Bitmap^ do
    FreeMemAlign(bmBits, abs(bmHeight) * bmWidthBytes * bmPlanes, false, true);
End;

procedure DeleteBitmap(Bitmap: PBitmap); {public}
Begin
  If Bitmap <> nil then Begin
    with Bitmap^ do Begin
      If bmBits <> nil then
        BitmapFreeProc(Bitmap);
      If bmPalette <> nil then
        FreeMem(bmPalette, SizeOf(TRGBQuad) shl (bmBitsPixel * bmPlanes));
    End;
    Dispose(Bitmap)
  End
End;

{ Store Bitmap
}

procedure StdBitmapStore(var S: TStream; Bitmap: PBitmap); far;
Begin
  with Bitmap^ do
    S.Write(bmBits^, bmWidthBytes*abs(bmHeight)*bmPlanes)
End;

procedure StoreBitmapFile(var S: TStream; Bitmap: PBitmap); {public}
var
  FileHeader: TBitmapFileHeader;
  BitmapInfoheader: TBitmapInfoHeader;
  size: Word;
  zero: pointer;
  posa, posb, posc: LongInt;
Begin
  posa := S.GetPos;
  with Bitmap^ do Begin
    FillChar(FileHeader, SizeOf(FileHeader), 0);
    with FileHeader do
      bfType := $4D42;
    S.Write(FileHeader, SizeOf(TBitmapFileHeader));
    with BitmapInfoHeader do Begin
      biSize := SizeOf(TBitmapInfoHeader);
      biWidth := bmWidth;
      biHeight := abs(bmHeight);
      biPlanes := 1 {bmPlanes;     !!!!!!!!! };
      biBitCount := bmPlanes * bmBitsPixel;
      biCompression := bi_RGB;
      biSizeImage := 0;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0
    End;
    S.Write(BitmapInfoHeader, SizeOf(TBitmapInfoHeader));
    If bmPalette = nil then Begin
      size := SizeOf(TRGBQuad) shl BitmapinfoHeader.biBitCount;
      GetMem(zero, size);
      FillChar(zero, size, 0);
      S.Write(zero^, size);
      FreeMem(zero, size)
    End
    else
      S.Write(bmPalette^, SizeOf(TRGBQuad) shl (bmPlanes * bmBitsPixel));
    posb := S.GetPos;
    BitmapStoreProc(S, Bitmap);
  End;
  posc := S.GetPos;
  S.Seek(posa);
  with FileHeader do
  begin
    bfSize := posc - posa;
    bfOffBits := posb - posa
  end;
  S.Write(FileHeader, SizeOf(TBitmapFileHeader));
  S.Seek(posc);
End;

{ Code page routines 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
}

{$L WResChr.obj  (WResChr.asm)          WinRes: Character translation }
{$L 10070437.obj (10070437.bin) }
{$L 04371007.obj (04371007.bin) }

procedure AnsiFrom437(Buf: PChar); external;
procedure AnsiFrom437Str(Str: PString); external;

procedure AnsiTo437(Buf: PChar); external;
procedure AnsiTo437Str(Str: PString); external;

procedure AnsiTo437Buf(var Buf); near; external;

{ String resource routines 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
}

function LoadStringBlock(var S: TStream; Index: Word): PStringBlock;
var
  P, Q, Size: LongInt;
  Block: PStringBlock;
Begin
  LoadStringBlock := nil;
  P := S.GetPos;
  size := SkipToResourceS(S, MakeIntResource(Index shr 4 + 1), rt_String);
  If size <> 0 then Begin
    Q := S.GetPos;
    GetMem(Block, SizeOf(TStringBlock) + size);
    LoadStringBlock := Block;
    with Block^ do Begin
      sbIndex := Index shr 4 + 1;
      sbSize := size;
      sbNext := nil;
      S.Read(sbData, size);
      AnsiTo437Buf(sbData)
    End
  End;
  S.Seek(P)
End;

procedure FreeStringBlock(Block: PStringBlock);
Begin
  If Block <> nil then
  FreeMem(Block, SizeOf(TStringBlock) + Block^.sbSize)
End;

function GetStringFromBlock(Block: PStringBlock; Index: Word): PString; external;

{$L WResStr.obj  (WResStr.asm)          WinRes: strings }

function LoadString(var S: TStream; Index: Word): string;
var
  P, Cur: LongInt;
  i: Integer;
  d: Byte;
  str: PString;
Begin
  P := S.GetPos;
  If SkipToResource(S, MakeIntResource(Index shr 4 + 1), rt_String)
  then Begin
    Cur := S.GetPos;
    For i := 1 to Index and 15 do Begin
      S.Read(d, 1);
      Inc(Cur, d + 1);
      S.Seek(Cur)
    End;
    asm
        les     di, @Result
        mov     WORD PTR str, di
        mov     WORD PTR str.2, es
    end;
    S.Read(str^[0], 1);
    S.Read(str^[1], Length(str^));
    AnsiTo437Str(str)
  End
  else LoadString := '';
  S.Seek(P)
End;

{$IFNDEF VER60 *************************************************************}

{ String zero routines
}

procedure ReadSZ(var S: TStream; Buf: PChar); near;
var
  C: Char;
  cnt: Word;
Begin
  cnt := 0;
  Repeat
    S.Read(C, 1);
    Buf[0] := C;
    If cnt < 256 then Inc(Buf);
    Inc(cnt)
  Until C = #0;
End;

function ReadNewSZ(var S: TStream; Buf: PChar): PChar; near;
Begin
  ReadSZ(S, Buf);
  ReadNewSZ := StrNew(Buf)
End;

procedure DisposeSZ(Buf: PChar); near;
Begin
  If Seg(Buf^) <> 0 then StrDispose(Buf)
End;

{ Marker routines
}

function ConvertMarkers(Buf: PChar; Max: Word): Boolean;
var
  M: PChar;
Begin
  ConvertMarkers := false;
  M := Buf + Max;
  Repeat
    Buf := StrScan(Buf, '&');
    If Buf <> nil then
    if Buf[1] = '&'
    then Begin                                  { &&  & }
      StrMove(Buf + 1, Buf, StrLen(Buf));
      Inc(Buf, 2)
    End
    else Begin                                  { &a  ~a~ }
      ConvertMarkers := true;
      Buf[0] := '~';
      If StrEnd(Buf) = M - 1
      then StrMove(Buf + 3, Buf + 2, StrLen(Buf+3))
      else StrMove(Buf + 3, Buf + 2, StrLen(Buf+1));
      Buf[2] := '~';
      Inc(Buf, 3)
    End;
  Until (Buf >= M) or (Buf = nil)
End;

{ Menu routines
}
const
  dfDisabled   = $01;
  dfMenuCheck  = $02;
  dfRadio      = $04;
  dfCheckState = $08;
  dfBitmap     = $10;

type
  PMenu = ^TMenu;

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    Next: PMenuItem;
    Name: PString;
    Command: Word;
    Disabled: Byte;
    KeyCode: Word;
    HelpCtx: Word;
    case Integer of
      0: (Param: PString);
      1: (SubMenu: PMenu);
  end;

  TMenu = record
    Items: PMenuItem;
    Default: PMenuItem;
  end;

function LoadMenu(var S: TStream; MenuName: PChar; Options: Word): pointer;
var
  P: LongInt;
  MenuHeader: TMenuHeader;

 function DoLoadMenu: PMenu;
 var
   Menu: PMenu;
   Res, Item: PMenuItem;
   Flags: Word;
   Prefix: string[2];
   Buf: array[0..255] of Char;
   P: PChar;
 Begin
   New(Menu); FillChar(Menu^, SizeOf(Menu^), 0);
   DoLoadMenu := Menu;
   Res := nil;
   Repeat
     S.Read(Flags, SizeOf(Word));
     New(Item);
     If Res = nil then Begin
       Menu^.Items := Item;
       Menu^.Default := Item
     End else Res^.Next := Item;
     Res := Item;
     with Item^ do Begin
       Next := nil;
       Boolean(Disabled) := Flags and (mf_Grayed + mf_Disabled) <> 0;
       KeyCode := 0;
       HelpCtx := 0;
       If Flags and mf_PopUp = 0 then Begin
         S.Read(Command, SizeOf(Word));
         ReadSZ(S, Buf);
         AnsiTo437(Buf);
         P := StrScan(Buf, #9);
         If P <> nil then Begin
           P[0] := #0;
           Param := NewStr(StrPas(P+1))
         End
         else Param := nil;
         Prefix := '';
         If Flags and mf_Checked <> 0 then
         if Options = mo_GraphicsVision
           then Disabled := Disabled or (dfMenuCheck + dfCheckState)
           else Prefix := ' ';
         ConvertMarkers(Buf, 256);
         Name := NewStr(Prefix + StrPas(Buf))
       End
       else Begin
         Command := 0;
         ReadSZ(S, Buf);
         AnsiTo437(Buf);
         ConvertMarkers(Buf, 256);
         Name := NewStr(StrPas(Buf));
         SubMenu := DoLoadMenu
       End
     End
   Until Flags and mf_End <> 0;
 End;

Begin
  P := S.GetPos;
  If SkipToResource(S, MenuName, rt_Menu) then Begin
    S.Read(MenuHeader, SizeOf(TMenuHeader));
    LoadMenu := DoLoadMenu
  End else LoadMenu := nil;
  S.Seek(P);
End;

{ Dialog template routines
}

type
  PClassCollection = ^TClassCollection;
  TClassCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
  End;

function TClassCollection.Compare;
var
  C1, C2: PChar;
Begin
  C1 := PClassRec(Key1)^.Class;
  C2 := PClassRec(Key2)^.Class;
  If (Seg(C1^) = 0) or (Seg(C2^) = 0) then
    if LongInt(C1) > LongInt(C2) then Compare := +1 else
    if LongInt(C1) < LongInt(C2) then Compare := -1 else Compare := 0
  else Compare := StrIComp(C1, C2)
End;

procedure TClassCollection.FreeItem;
Begin
  { don't free }
End;

const
  Classes: PSortedCollection = nil;

procedure InitClasses;
Begin
  Classes := New(PClassCollection, Init(8, 8))
End;

procedure DoneClasses;
Begin
  Dispose(Classes, Done);
  Classes := nil
End;

procedure RegisterClass(var ClassRec: TClassRec);
var
  Index: Integer;
Begin
  If Classes = nil then Begin
    InitClasses;
    If Classes = nil then Exit
  End;
  with Classes^ do Begin
    If Search(@ClassRec, Index) then AtDelete(Index);
    AtInsert(Index, @ClassRec)
  End
End;

function GetClass(ClassName: PChar): PClassRec; near;
var
  Index: Integer;
  Rec: TClassRec;
Begin
  Rec.Class := ClassName;
  If Classes^.Search(@Rec, Index)
  then GetClass := Classes^.At(Index)
  else GetClass := nil
End;

function LoadDialog(var S: TStream; DialogName: PChar): pointer;
var
  Header: TDialogBoxHeader;
  Control: TControlData;
  Info: TDialogInfo;
  Buf: array[0..255] of Char;
  DlgClass, Class: PClassRec;
  Pos: LongInt;
  i: Byte;
Begin
  Pos := S.GetPos;
  LoadDialog := nil;
  If SkipToResource(S, DialogName, rt_Dialog) then Begin
    S.Read(Header, 13);
    with Header do Begin
      FillChar(szMenuName, SizeOf(TDialogBoxHeader) - 13, 0);
      S.Read(Buf, 1);
      If Buf[0] = #255 then S.Read(szMenuName, 2) else
      if Buf[0] <> #0 then Begin
        ReadSZ(S, Buf+1);
        szMenuName := StrNew(Buf)
      End;
      szClassname := ReadNewSZ(S, Buf);
      szCaption := ReadNewSZ(S, Buf);
      If szCaption <> nil then AnsiTo437(szCaption);
      If lStyle and ds_SetFont <> 0 then Begin
        S.Read(wPointSize, 2);
        szFaceName := ReadNewSZ(S, Buf)
      End;
      If Classes = nil
        then DlgClass := nil
        else DlgClass := GetClass(szClassname);
      If DlgClass = nil
        then Info.Dialog := nil
        else TInitProc(DlgClass^.Init)(@Header, @Info);
      DisposeSZ(szMenuName);
      DisposeSZ(szClassname);
      DisposeSZ(szCaption);
      DisposeSZ(szFaceName);
      If Info.Dialog <> nil then Begin
        For i := 1 to bNumberOfItems do Begin
          S.Read(Control, 14);
          with Control do Begin
            S.Read(Buf, 1);
            If Buf[0] >= #$80 then LongInt(szClass) := Byte(Buf[0]) else
            if Buf[0] = #0 then szClass := nil else Begin
              ReadSZ(S, Buf+1);
              szClass := StrNew(Buf)
            End;
            szText := ReadNewSZ(S, Buf);
            If szText <> nil then AnsiTo437(szText);
            S.Read(bExtraSize, 1);
            If bExtraSize <> 0 then
            S.Read(bExtra, bExtraSize);
            Class := GetClass(szClass);
            If Class <> nil then
              TInitProc(Class^.Init)(@Control, @Info);
            DisposeSZ(szClass);
            DisposeSZ(szText)
          End
        End;
        If Info.Wake <> nil then
        TWakeProc(Info.Wake)(@Info)
      End
    End;
    LoadDialog := Info.Dialog
  End;
  S.Seek(Pos)
End;

procedure CreateLinks(Info: PDialogInfo; Link: pointer);
var
  N, P: PLinkRec;
Begin
  P := Info^.Links;
  Info^.Links := nil;
  while P <> nil do
    with P^ do Begin
      TLinkProc(proc)(link, control);
      N := Next;
      Dispose(P);
      P := N
    End
End;

procedure InsertLink(Info: PDialogInfo; AProc: pointer; AControl: pointer);
var
  Link: PLinkRec;
Begin
  New(Link);
  with Link^ do Begin
    Next := Info^.Links;
    Proc := AProc;
    Control := AControl
  End;
  Info^.Links := Link
End;

{$ENDIF}

Begin
  BitmapLoadProc := StdBitmapLoad;
  BitmapFreeProc := StdBitmapFree;
  BitmapStoreProc := StdBitmapStore;
  BitmapAllocProc := StdBitmapAlloc
End.
