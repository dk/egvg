unit MFonts;

{ unit MyFonts, Version 1.60.006, Copyright 1993,95 Matthias K”ppe.

}

{$A+,B-,F-,G+,O-,R-,S-,X+}

interface

uses Objects, Dos, Gr, GDI, EGInline
{$DEFINE RUSSIAN}
{$IFDEF DPMI}
,EGDPMI
{$ENDIF}
;

{$DEFINE MEMPATCH   I do not now why, but cache work is non-stabile. DK}

{ BGI-like justification constants
}

const
  LeftText      = 0;
  CenterText    = 1;
  RightText     = 2;

  BottomText    = 0;
{ CenterText    = 1; }
  TopText       = 2;
  BaseLine      = 3;
  LeadLine      = 4;

{ FontMode constants
}
const
  ftUnresolved  = 0;
  ftBGI         = 1;
  ftBIOS        = 2;
  ftCPI         = 4;
  ftWin         = 8;
  ftWinVector   = 16;
  ftWinHuge     = 32;           { collection of single glyphs }

{ Font attributes
}
const
  ftNormal          = 0;
  ftBold            = 1;
  ftThin            = 2;
  ftItalic          = 4;
  ftUnderlined      = 8;
  ftStruckOut       = 16;
  ftVerticalMoves   = 64;
  ftNoVerticalMoves = 0;
  ftNoDescent       = 128;
  ftOpaque          = 2048;

{ Font Families
}
const
  ff_DontCare   = 0 shl 4;  { Don't care or don't know. }
  ff_Roman      = 1 shl 4;  { Variable stroke width, serifed. }
                            { Times Roman, Century Schoolbook, etc. }
  ff_Swiss      = 2 shl 4;  { Variable stroke width, sans-serifed. }
                            { Helvetica, Swiss, etc. }
  ff_Modern     = 3 shl 4;  { Constant stroke width, serifed or sans-serifed. }
                            { Pica, Elite, Courier, etc. }
  ff_Script     = 4 shl 4;  { Cursive, etc. }
  ff_Decorative = 5 shl 4;  { Old English, etc. }

{ Font weights
}
  fw_DontCare   = 0;
  fw_Thin       = 100;
  fw_ExtraLight = 200;
  fw_Light      = 300;
  fw_Normal     = 400;
  fw_Medium     = 500;
  fw_SemiBold   = 600;
  fw_Bold       = 700;
  fw_ExtraBold  = 800;
  fw_Heavy      = 900;

  fw_UltraLight = fw_ExtraLight;
  fw_Regular    = fw_Normal;
  fw_DemiBold   = fw_SemiBold;
  fw_UltraBold  = fw_ExtraBold;
  fw_Black      = fw_Heavy;

{ Windows-style char sets
}
const
  Ansi_CharSet     = 0;
  Symbol_CharSet   = 2;
  ShiftJIS_CharSet = 128;
  OEM_CharSet      = 255;

{ Windows-style text metric
}
type
  PTextMetric = ^TTextMetric;
  TTextMetric = record
    tmHeight: Integer;
    tmAscent: Integer;
    tmDescent: Integer;
    tmInternalLeading: Integer;
    tmExternalLeading: Integer;
    tmAveCharWidth: Integer;
    tmMaxCharWidth: Integer;
    tmWeight: Integer;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmFirstChar: Byte;
    tmLastChar: Byte;
    tmDefaultChar: Byte;
    tmBreakChar: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
    tmOverhang: Integer;
    tmDigitizedAspectX: Integer;
    tmDigitizedAspectY: Integer;
  end;

{ Shared pointer handling
}
const
  sh_Fixed = 0;
  sh_Buf = 1;
  sh_Str = 2;
  sh_Obj = 3;
  sh_ABC = 4;
  sh__Save = 7;
  sh_NoNull = 8;
  sh__Mem = 16;

type
  hShared = Integer;
  PShared = ^TShared;
  TShared = record
    p: pointer;
    Used: Word;
    Options: Word
  end;

  PSharedList = ^TSharedList;
  TSharedList = array[0..255] of TShared;

const
  SharedList: PSharedList = nil;
  SharedCount: Word = 0;
  SharedMax: Word = 0;
  SharedDelta: Word = 16;

{ ABC array
}

type
  PABCLong = ^TABCLong;
  TABCLong = record             { size = 16 }
    Bits: pointer;
    Size: Word;
    A, B, C, D, H: Integer
  end;

  PABCLongArray = ^TABCLongArray;
  TABCLongArray = array[Char] of TABCLong;

  PFontRec = ^TFontRec;
  TConvertProc = procedure(FontRec: PFontRec; var s: string);

{ Font entry
}
  TFontRec = record
    CodePage: Word;
    DiffAttr: Word;
    FontLength: Integer;
    VertAdd: Integer;
    MultX: Word;
    DivX: Word;
    MultY: Word;
    DivY: Word;
    FontHandle: hShared;                { handle of font buffer }
    NameHandle: hShared;                { handle of file name buffer }
    SrcHandle: hShared;                 { handle of source buffer }
    TempMemSize: Word;
    TextMetric: TTextMetric;
    case FontMode: Byte of
      ftUnresolved: (
        Alias: Word);
      ftBGI: (
        BgiSize: Word);
      ftBIOS: ();
      ftCPI: (
        CpiFilePos: LongInt);
      ftWin, ftWinVector, ftWinHuge: (
        WinCharTbl: LongInt;
        WinFilePos: LongInt;
        FontSize: Word;
        EntrySize: Byte;
        BitMapDelta: Word;
        proc: TConvertProc);
  End;

{ Font collection object type
}

{ Current params
}
var
  sHoriz, sVert, sFont, sColor:Word;
  sCharSpace: Integer;
  sMarker, potMarker: Char;
  sAttr: Word;
  LocMaximalY, LocMaximalX : Word;

{ High-level font handling ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
}
procedure OutTextXY(x, y: Integer; s: string);
procedure SetTextJustify(Horiz, Vert: Word);
procedure SetTextParams(Font: Word; CharSpace: Integer; Color: Word; UseMarker: Boolean);
procedure SetExtTextParams(Font: Word; Charspace: Integer; Color: Word; UseMarker: Boolean; Attr: Word);
procedure SetMarker(Marker: Char);
function TextWidth(s: string): Integer;
function TextHeight(s: string): Integer;
procedure GetTextMetrics(var Metrics: TTextMetric);

{ Font management ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
}
function DefFont(Font: PFontRec): Word;
procedure ReplaceFont(Font: Word; FontRec: PFontRec);
procedure FreeFont(Font: Word);
function  GetFontRec(Handle : Word) : PFontRec;


{ Font registration ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
}
function FullAliasOf(Font: Word): PFontRec;
function AliasOf(Font: Word; CharSpace, AVertAdd, AHeight: Integer;
  Attr: Word): PFontRec;
function LoadCpiFont(FName: PathStr; ReqCP, ReqLength: Integer;
  CharSpace, AVertAdd, AHeight: Integer): PFontRec;
function LoadBgiFileFont(FName: PathStr; ReqHeight, AMultX, ADivX, AMultY,
  ADivY: Integer; CharSpace, AVertAdd, AHeight: Integer;
  Attr: Word): PFontRec;
function LoadBiosFont(ReqLength: Word;
  CharSpace, AVertAdd, AHeight: Integer): PFontRec;
function LoadWinFont(FName: PathStr; reqNr, reqPt, reqLength: Integer;
  CharSpace, AVertAdd, AHeight: Integer; Attr: Word; var FaceName : String): PFontRec;
procedure ReloadWinFont(Item: PFontRec);

function FixedAddr(p: Pointer): PathStr;
procedure GetBIOSPtr;

{ TrueType link ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
}
type
  TLoadTTFontProc = function(Filename: string; ScaleX, ScaleY: Integer): PFontRec;

const
  LoadTTFontProc: TLoadTTFontProc = nil;

function LoadHugeFont(aSource: pointer; aProc: TConvertProc;
  aMetric: TTextMetric; aLength: Integer): PFontRec;

{ BIOS fonts ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
}

var
  BIOS8, BIOS14, BIOS16: pointer;

implementation

uses Memory, WinRes;

type
  PFontCollection = ^TFontCollection;
  TFontCollection = object(TCollection)
    procedure FreeItem(Item: pointer); virtual;
    procedure Insert(Item: pointer); virtual;
  End;

{ Font collection
}
var
  FontCollection: PFontCollection;


const
  MyFontsTempMem: Integer = 0;

var
  MemoryDetected: Boolean;

procedure Copyright; assembler;
Asm
        RET
        DB      "MyFonts (C) Matthias K”ppe"
End;


{$IFDEF MEMPATCH}
Procedure DisposeCache(var P : Pointer);
Begin
  Freemem(P, Word(P^) + 2);
  P := Nil;
End;
{$ENDIF}

(************************* Resolve Reference proc **************************)

function ConvertAttr(var TextMetric: TTextMetric): Word; { not used }
var
  Res: Word;
Begin
  with TextMetric do Begin
    If tmWeight < fw_Bold
      then Res := 0
      else Res := ftBold;
    If tmItalic <> 0
      then Res := Res or ftItalic;
    If tmUnderlined <> 0
      then Res := Res or ftUnderlined;
    If tmStruckOut <> 0
      then Res := Res or ftStruckOut
  End;
  ConvertAttr := Res
End;

function DoAddAttr(var DiffAttr: Word; Delta: Word): Word; near;
Begin
  DiffAttr := DiffAttr or Delta;
  If DiffAttr and (ftBold + ftThin) = ftBold + ftThin
    then DiffAttr := DiffAttr and not (ftBold + ftThin);
  DoAddAttr := DiffAttr
End;

procedure AddAttr(var TextMetric: TTextMetric;
  var DiffAttr: Word; Delta: Word);
Begin
  with TextMetric do Begin
    If Delta and ftBold <> 0 then Inc(tmWeight, 300);
    If Delta and ftThin <> 0 then Dec(tmWeight, 300);
    If tmWeight < fw_Thin then tmWeight := fw_Thin;
    If tmWeight > fw_Bold then tmWeight := fw_Bold;
    If Delta and ftItalic <> 0 then tmItalic := 1;
    If Delta and ftUnderlined <> 0 then tmUnderlined := 1;
    If Delta and ftStruckOut <> 0 then tmStruckOut := 1
  End;
  DoAddAttr(DiffAttr, Delta)
End;

procedure IncreaseSharedCount(Item: PFontRec);
Begin
  with Item^ do Begin
    If FontHandle <> -1 then Inc(SharedList^[FontHandle].Used);
    If NameHandle <> -1 then Inc(SharedList^[NameHandle].Used)
  End
End;

procedure ResolveReference(Item: PFontRec);
var
  Temp: TTextMetric;
  Source: PFontRec;
  Attr: Word;
  VAdd: Integer;
Begin
  Source := FontCollection^.At(Item^.Alias);
  Move(Source^.TextMetric, Temp, SizeOf(TTextMetric));
  Attr := Item^.DiffAttr;
  VAdd := Item^.VertAdd;
  with Item^.TextMetric do Begin
    If tmHeight <> 0
      then Temp.tmHeight := tmHeight;
    Inc(Temp.tmOverhang, tmOverhang);
    Inc(Temp.tmAveCharWidth, tmOverhang);
    Inc(Temp.tmMaxCharWidth, tmOverhang);
  End;
  Move(Source^, Item^, SizeOf(TFontRec));
  IncreaseSharedCount(Item);
  Move(Temp, Item^.TextMetric, SizeOf(TTextMetric));
  AddAttr(Item^.TextMetric, Attr, Item^.DiffAttr);
  Item^.DiffAttr := Attr;
  Inc(Item^.VertAdd, VAdd);
End;

(************************ TFontCollection object ***************************)

procedure ReleaseHandle(Handle: hShared); near;
var
  c: Char;
Begin
  If (Handle <> -1) and (Handle < SharedMax) then
  with SharedList^[Handle] do Begin
    Dec(Used);
    If (Used = 0) and (p <> nil) then
      if Options >= sh__Mem
      then
        FreeMem(p, Options)
      else
        case Options of
          sh_Fixed:
            ;
          sh_Buf:
            DisposeCache(p);
          sh_Str:
            DisposeStr(p);
          sh_Obj:
            Dispose(PObject(p), Done);
          sh_ABC:
            begin
              For c := #0 to #255 do
                with PABCLongArray(p)^[c] do
                  If Bits <> nil
                  then FreeMem(Bits, Size);
              Dispose(PABCLongArray(p))
            end;
        end;
  End
End;

procedure TFontCollection.FreeItem;
Begin
  If Item = nil then Exit;
  with PFontRec(Item)^ do Begin
    ReleaseHandle(NameHandle);
    ReleaseHandle(FontHandle);
    ReleaseHandle(SrcHandle)
  End;
  Dispose(PFontRec(Item))
End;

procedure TFontCollection.Insert;

 procedure DoResolve(Item: PFontRec); far;
 Begin
   If Item <> nil then
   with Item^ do
   If (FontMode = ftUnresolved) and
      (Alias = Count - 1) then
     ResolveReference(Item)
 End;

 procedure InsertAtNil(Item: pointer);
 var
   i: Integer;
 Begin
   For i := 0 to Count - 1 do
     If Items^[i] = nil
     then Begin
       AtPut(i, Item);
       Exit
     End;
   TCollection.Insert(Item)
 End;

Begin
  InsertAtNil(Item);
  with PFontRec(Item)^ do
  If (Item <> Nil) and (FontMode = ftUnresolved)
     and (Alias < Count) then ResolveReference(Item);
  ForEach(@DoResolve)
End;

(********************* Shared pointer handling routines ********************)

function NewShared(Value: pointer; Opt: Word): hShared;
var
  p: PSharedList;
Begin
  If (Opt and sh_NoNull <> 0) and (Value = nil) then Begin
    NewShared := -1;
    Exit
  End;
  NewShared := SharedCount;
  Inc(SharedCount);
  If SharedCount > SharedMax then Begin
    Inc(SharedMax, SharedDelta);
    GetMem(p, SharedMax * SizeOf(TShared));
    if P = Nil then begin
      Dec(SharedCount);
      Exit;
    end;
    FillChar(p^, SharedMax * SizeOf(TShared), 0);
    If SharedCount > 1 then Begin
      Move(SharedList^, p^, (SharedCount - 1) * SizeOf(TShared));
      FreeMem(SharedList, (SharedCount - 1) * SizeOf(TShared));
    End;
    SharedList := p;
  End;
  with SharedList^[SharedCount - 1] do Begin
    p := Value;
    Options := Opt and not sh_NoNull;
    Used := 1
  End
End;

function FixedAddr; assembler;
Asm
        LES     DI, @Result
        CLD
        MOV     AX, ":" * 256 + 5
        STOSW
        MOV     AX, WORD PTR p
        STOSW
        MOV     AX, WORD PTR p + 2
        STOSW
End;

function GetPointer(s: PathStr): pointer; near; assembler;
Asm
        LES     DI, s
        MOV     AX, ":" * 256 + 5
        CLD
        SCASW
        MOV     AX, 0
        MOV     DX, AX
        JNZ     @@1
        MOV     AX, [ES:DI]
        MOV     DX, [ES:DI+2]
@@1:
End;

procedure ForceNewCache(var P: Pointer; Size: Word);
Begin
  {$IFNDEF MEMPATCH - original}
  NewCache(P, Size);
  If P = nil then Begin
    DoneMemory;
    NewCache(P, Size)
  End;
  {$ELSE}
  P := MemAllocSeg(Size + 2);
  if P <> Nil then Word(P^) := Size;
  {$ENDIF}
End;

(************************* Font Stream routines ****************************)

procedure Skip(var S: TStream; d: LongInt);
Begin
  S.Seek(S.GetPos + d)
End;

function NewFontStream(FName: PathStr): PStream; near;
var
  F : pbufStream;
Begin
  f := New(PBufStream, Init(FName, stOpenRead, 2048));
  if (F <> Nil) and (F^.Status <> 0) and (F^.Buffer = Nil) then
    Dispose(F, Done);
  newFontStream := F;
End;

(*********************** Generic Bitmap Font Handling **********************)

{ $L cc.obj (cc.asm) }
{Procedure SetupCache;
Procedure GotoPos;
Procedure SetRightPos;}

{procedure Setup_16; near; external;}

{ $L ccach16.obj (ccach16.asm) }

(*procedure Flush_256o; near; external;
procedure Flush_256t; near; external;
procedure Setup_256; near; external;

{ $L ccach256.obj (ccach256.asm) } *)

{var
  FlushOpaque: Word;
  FlushTrans: Word;
  SetupProc: Word;
  FlushProc: Word;}

procedure WriteBmpText(CreateProc: Word; Item: PFontRec;
  x, y, CharSpace: Integer; Attr: Byte; Color: Word;
  s: string; Marker: Char); far; forward;

{ $L myfobmp.obj (myfobmp.asm) }

(**************************** CPI Font Handling ****************************)

{procedure CpiCreateList; near; external;}

function CPIWidth(s: String; CharSpace: Integer;
  Marker: Char): Integer; near; assembler;
Asm
        LES     DI, s
        MOV     CL, [ES:DI]
        XOR     CH, CH
        MOV     DX, CX
        INC     DI
        MOV     AL, Marker
@@2:    REPNE SCASB
        JNE     @@1
        DEC     DX
        JMP     @@2
@@1:    MOV     AL, BYTE PTR CharSpace
        ADD     AL, 8
        IMUL    DL
        SUB     AX, CharSpace
End;

function CpiAscent(PixHeight: Integer): Integer; near;
Begin
  case PixHeight of
     8: CpiAscent := 7;
    14: CpiAscent := 11;
    16: CpiAscent := 12;
  else  CpiAscent := (PixHeight * 3) div 4;
  end;
End;

function CpiIntLead(PixHeight: Integer): Integer; near;
Begin
  case PixHeight of
     8: CpiIntLead := 0;
    14: CpiIntLead := 2;
    16: CpiIntLead := 2;
  else  CpiIntLead := (PixHeight * 3) div 4;
  end;
End;

function LoadCpiFont(FName: PathStr; ReqCP, ReqLength: Integer;
  CharSpace, AVertAdd, AHeight: Integer): PFontRec;
var
  CpiFileHeader: TCpiFileHeader;
  CpiDevCpHeader: TCpiDevCpHeader;
  CpiFontHeader: TCpiFontHeader;
  Fits: Boolean;
  Item: PFontRec;
  F: PStream;
  p: pointer;
  fPos: LongInt;
  Options: Word;

 procedure FindEntry;
 var
   i: Integer;
 Begin
   For i := 1 to CpiDevCpHeader.cdchCount do Begin
     F^.Read(CpiFontHeader, SizeOf(TCpiFontHeader));
     with CpiFontHeader do Begin
       fits := cfhHeight = ReqLength;
       If fits then Exit;
       Skip(F^, 256 * cfhHeight * ((cfhWidth - 1) div 8 + 1));
     End
   End;
   fits := false
 End;

Begin
  LoadCPIFont := nil;
  p := GetPointer(FName);
  If p = nil then Begin
    F := NewFontStream(FName);
    if f = Nil then Exit;
    F^.Read(CpiFileHeader, SizeOf(TCpiFileHeader));
    with CpiFileHeader do
    If (F^.Status <> 0) or (cfhSign1 <> $4E4F46FF) or (cfhSign2 <> $20202054)
    then Begin
      Dispose(F, Done);
      Exit
    End;
    with CpiDevCpHeader do
    Repeat
      F^.Read(CpiDevCpHeader, SizeOf(TCpiDevCpHeader));
      fits := (ReqCP = 0) or (ReqCP = cdchCodePage);
      If not fits and (cdchNext <> -1) then
        F^.Seek(cdchNext)
    Until fits or (cdchNext = -1);
    If fits then FindEntry;
    fPos := F^.GetPos;
    Dispose(F, Done);
    Options := sh_Buf
  End
  else Begin
    fPos := -1;
    FName := '';
    CpiDevCpHeader.cdchCodePage := ReqCP;
    Options := sh_Fixed
  End;
  New(Item);
  if Item = Nil then Exit;
  with Item^ do Begin
    CodePage    := CpiDevCpHeader.cdchCodePage;
    DiffAttr    := 0;
    FontMode    := ftCpi;
    FontLength  := ReqLength;
    VertAdd     := AVertAdd;
    FontHandle  := NewShared(p, Options);
    NameHandle  := NewShared(NewStr(FName), sh_Str + sh_NoNull);
    SrcHandle   := -1;
    CpiFilePos  := fPos;
    TempMemSize := 4096;
    FillChar(TextMetric, SizeOf(TTextMetric), 0);
    with TextMetric do Begin
      If AHeight = 0
      then tmHeight     := ReqLength
      else tmHeight     := AHeight;
      tmAscent          := CpiAscent(ReqLength);
      tmDescent         := ReqLength - tmAscent;
      tmInternalLeading := CpiIntLead(ReqLength);
      tmAveCharWidth    := 8 + CharSpace;
      tmMaxCharWidth    := 8 + CharSpace;
      tmWeight          := fw_Normal;
      tmLastChar        := 255;
      tmDefaultChar     := 32;
      tmBreakChar       := 32;
      tmPitchAndFamily  := ff_Modern;
      tmCharSet         := OEM_CharSet;
      tmOverhang        := CharSpace;
      tmDigitizedAspectX := 100;
      tmDigitizedAspectY := 100
    End
  End;
  LoadCPIFont := Item;
End;

procedure ReloadCPIFont(Item: PFontRec); near;
var
  F: PStream;
Begin
  with Item^ do Begin
    F := NewFontStream(PString(SharedList^[NameHandle].p)^);
    if F = Nil then Exit;
    ForceNewCache(SharedList^[FontHandle].p, FontLength*256);
    F^.Seek(CPIFilePos);
    F^.Read(SharedList^[FontHandle].p^, FontLength*256);
    Dispose(F, Done);
  End;
End;

{$IFDEF DPMI}
function CallInt: pointer; near;
Var
  R : DPMIRegisters;
  C : Word;
Begin
  asm mov c, bx end;
  R.AX := $1130;
  R.BX := C;
  SimulateRealModeInt($10, R);
  GetSelectorForRealMem(Ptr(R.ES,0), $FFFF, C);
  CallInt := Ptr(C, R.BP);
End;
{$ELSE}
function CallInt: pointer; near; assembler;
asm
        PUSH    BP
        MOV     AX, 1130H
        INT     10H
        MOV     DX, ES
        MOV     AX, BP
        POP     BP
end;
{$ENDIF}

procedure GetBIOSPtr; assembler;
Asm
        MOV     AX, 40H
        MOV     ES, AX
        MOV     AL, [ES:49H]
        CMP     AL, 3
        JBE     @@1
        CMP     AL, 7
        JNZ     @@2
@@1:    MOV     BH, 3
        CALL    CallInt
        MOV     WORD PTR BIOS8, AX
        MOV     WORD PTR BIOS8+2, DX
        MOV     BH, 2
        CALL    CallInt
        MOV     WORD PTR BIOS14, AX
        MOV     WORD PTR BIOS14+2, DX
        MOV     BH, 6
        CALL    CallInt
        MOV     WORD PTR BIOS16, AX
        MOV     WORD PTR BIOS16+2, DX
@@2:
End;

function LoadBiosFont(ReqLength: Word;
  CharSpace, AVertAdd, AHeight: Integer): PFontRec;
var
  Font: pointer;
Begin
  If Bios8 = nil then GetBiosPtr;
  LoadBiosFont := nil;
  case ReqLength of
     8: Font := Bios8;
    14: Font := Bios14;
    16: Font := Bios16;
    else Exit
  End;
  If Font = nil then Exit;
  LoadBiosFont := LoadCpiFont(FixedAddr(SelOfs(Font)),
    {$IFDEF RUSSIAN}
    866
    {$ELSE}
    437
    {$ENDIF}
    ,ReqLength, CharSpace, AVertAdd, AHeight)
End;

(**************************** Win Bmp Handling ****************************)

{procedure WinCreateList; near; external;}

function WinWidth(Item: PFontRec; s: string; CharSpace: Integer;
  Marker: Char): Integer; near; assembler;
Asm
        LES     DI, Item
        MOV     CH, [ES:DI+TFontRec.EntrySize]
        MOV     DI, [ES:DI+TFontRec.FontHandle]
        SHL     DI, 3
        LES     BX, SharedList
        LES     DI, [ES:BX][DI]
        PUSH    DS
        LDS     SI, s
        XOR     DX, DX
        MOV     CL, [SI]
        OR      CL, CL
        JZ      @@3
        INC     SI
@@2:    MOV     BL, [SI]
        INC     SI
        CMP     BL, Marker
        JZ      @@1
        XOR     BH, BH
        MOV     AL, [ES:DI][BX]         { Tabellentranslation }
        MUL     CH
        MOV     BX, AX
        ADD     DX, ES:[DI+256][BX]
        ADD     DX, CharSpace
@@1:    DEC     CL
        JNZ     @@2
        SUB     DX, CharSpace
@@3:    MOV     AX, DX
        POP     DS
End;

(************************** Win Vector Handling ****************************)

procedure SetColor(Color: Byte);
Begin {GRT.Fore := Color;} End;

procedure SetThickness(Thickness: Word);
Begin {GRT.LineWidth := Thickness;} End;

procedure WriteWinVectorText(Item: PFontRec; x, y, CharSpace: Integer;
  Attr: Byte; Color: Word; s: string; Marker: Char;
  MultX, DivX, MultY, DivY: Integer); near; assembler;
var
  EntrySize: Byte;
  Delta: Word;
  Width: Word;
  Thickness: Word;
  xc, yc: Integer;
  xr, yr: Integer;
  SavedDS: Word;
  SavedBX: Word;
Asm
        MOV     AX, 1
        PUSH    gcnStopUpd
        CALL    ClipNotifyProc
        MOV     AX, 3
        PUSH    gcpLineStyle + gcpGetSize
        PUSH    0
        PUSH    0
        CALL    ChParamsProc
        SUB     SP, AX
        MOV     DX, SP
        MOV     AX, 3
        PUSH    gcpLineStyle + gcpGetParams
        PUSH    SS
        PUSH    DX
        CALL    ChParamsProc
        SUB     SP, 2
        MOV     DX, SP
        MOV     AX, 3
        PUSH    gcpColor + gcpGetParams
        PUSH    SS
        PUSH    DX
        CALL    ChParamsProc
        MOV     SavedDS, DS
        LDS     SI, Item
        MOV     AL, [SI][TFontRec.EntrySize]
        MOV     EntrySize, AL
        MOV     AX, [SI][TFontRec.BitmapDelta]
        MOV     Delta, AX
        TEST    [SI][TFontRec.DiffAttr], ftBold
        MOV     AX, 1
        JZ      @@7
        MOV     AX, 3
@@7:    PUSH    AX
        MOV     AX, [SI][TFontRec.TextMetric.tmAveCharWidth]
        MOV     Width, AX
        MOV     SI, [SI][TFontRec.FontHandle]
        SHL     SI, 3
        MOV     DS, SavedDS
        PUSH    Color
        CALL    SetColor
        CALL    SetThickness
        LDS     BX, SharedList
        LDS     BX, [BX][SI]
        MOV     SavedBX, BX
        ADD     Delta, BX
        LES     SI, s
        MOV     CL, [ES:SI]
        OR      CL, CL
        JZ      @@13
        XOR     CH, CH
        INC     SI

@@5:    MOV     BX, SavedBX
        MOV     AX, x
        MOV     xc, AX
        MOV     AX, y
        MOV     yc, AX
        SEGES LODSB
        CMP     AL, Marker
        JNZ     @@8
        ROL     Color, 8
        PUSH    DS
        MOV     DS, SavedDS
        PUSH    Color
        CALL    SetColor
        POP     DS
        LOOP    @@5
        JMP     @@13
@@8:    PUSH    SI
        PUSH    CX
        XLAT
        MUL     EntrySize
        MOV     SI, AX
        MOV     DI, [BX+258][SI]        { Width or Next }
        CMP     EntrySize, 2            { Fixed or variable pitch }
        JZ      @@1
        MOV     Width, DI
        MOV     DI, [BX+260][SI]        { Next }
@@1:    MOV     SI, [BX+256][SI]        { Offset }
        ADD     DI, Delta
        ADD     SI, Delta
        XOR     AX, AX
        MOV     xr, AX
        MOV     yr, AX
@@3:    CMP     SI, DI
        JAE     @@4
        LODSB
        CMP     AL, 80H                 { Move To }
        MOV     CL, AL
        JZ      @@6

        PUSH    DS
        PUSH    ES
        PUSH    DI
        PUSH    SI

        PUSH    xc
        PUSH    yc
        JMP     @@2
@@6:    LODSB
@@2:    CBW
        IMUL    MultX
        MOV     BX, DivX                { DivX > 0 }
        IDIV    BX
        INC     BX
        SAR     BX, 1
        ADD     xc, AX
        ADD     xr, DX
        MOV     AX, xr
        JS      @@10
        CMP     AX, BX
        JS      @@9
        SUB     AX, DivX
        MOV     xr, AX
        INC     xc
        JMP     @@9
@@10:   ADD     BX, AX
        JNS     @@9
        ADD     AX, DivX
        MOV     xr, AX
        DEC     xc
@@9:    LODSB
        CBW
        IMUL    MultY
        MOV     BX, DivY                { DivY > 0 }
        IDIV    BX
        INC     BX
        SAR     BX, 1
        ADD     yc, AX
        ADD     yr, DX
        MOV     AX, yr
        JS      @@11
        CMP     AX, BX
        JS      @@12
        SUB     AX, DivY
        MOV     yr, AX
        INC     yc
        JMP     @@12
@@11:   ADD     BX, AX
        JNS     @@12
        ADD     AX, DivY
        MOV     yr, AX
        DEC     yc
@@12:   CMP     CL, 80H
        JZ      @@3
        PUSH    xc
        PUSH    yc
        MOV     AX, 4                   { Param Count }
        MOV     DS, SavedDS
        CALL    LineProc
        POP     SI
        INC     SI
        POP     DI
        POP     ES
        POP     DS
        JMP     @@3
@@4:    MOV     AX, Width
        IMUL    MultX
        MOV     BX, DivX
        INC     BX
        SAR     BX, 1
        ADD     AX, BX
        IDIV    DivX
        ADD     AX, CharSpace           { ist schon korrigiert }
        ADD     x, AX
        POP     CX
        POP     SI
        DEC     CX
        JNZ     @@5
@@13:   MOV     DS, SavedDS
        MOV     AX, 1
        PUSH    gcnContUpd
        CALL    ClipNotifyProc
        MOV     DX, SP
        PUSH    gcpColor + gcpSetParams
        PUSH    SS
        PUSH    DX
        MOV     AX, 3
        CALL    ChParamsProc
        ADD     SP, AX
        MOV     DX, SP
        PUSH    gcpLineStyle + gcpSetParams
        PUSH    SS
        PUSH    DX
        MOV     AX, 3
        CALL    ChParamsProc
        ADD     SP, AX
End;

function WinVectorWidth(Item: PFontRec; s: string; CharSpace: Integer;
  Marker: Char; MultX, DivX: Integer): Integer; assembler;
var
  Width: Integer;
  Count: Integer;
  Help: Word;
Asm
        MOV     AX, DivX
        SAR     AX, 1
        MOV     Help, AX
        LES     DI, Item
        MOV     CH, [ES:DI+TFontRec.EntrySize]
        CMP     CH, 2
        JZ      @@4
        MOV     DI, [ES:DI+TFontRec.FontHandle]
        SHL     DI, 3
        LES     BX, SharedList
        LES     DI, [ES:BX][DI]
        PUSH    DS
        LDS     SI, s
        XOR     DX, DX
        XOR     AX, AX
        MOV     Count, -1
        MOV     CL, [SI]
        OR      CL, CL
        JZ      @@3
        INC     SI
@@2:    MOV     BL, [SI]
        INC     SI
        CMP     BL, Marker
        JZ      @@1
        XOR     BH, BH
        MOV     AL, [ES:DI][BX]         { Tabellentranslation }
        MUL     CH
        MOV     BX, AX
        CMP     CH, 2
        JZ      @@1
        ADD     DX, ES:[DI+258][BX]
        INC     Count
@@1:    DEC     CL
        JNZ     @@2
        MOV     AX, DX
        IMUL    MultX
        ADD     AX, Help
        IDIV    DivX
        MOV     BX, AX
        MOV     AX, CharSpace
        IMUL    Count
        ADD     AX, BX
        JMP     @@3

@@4:    MOV     AX, [ES:DI+TFontRec.TextMetric.tmAveCharWidth]
        IMUL    MultX
        ADD     AX, Help
        IDIV    DivX
        ADD     AX, CharSpace
        MOV     Width, AX
        PUSH    DS
        LDS     SI, s
        XOR     DX, DX
        XOR     AX, AX
        MOV     CL, [SI]
        OR      CL, CL
        JZ      @@3
        XOR     CH, CH
        INC     SI
        MOV     AH, Marker
@@6:    LODSB
        CMP     AL, AH
        JZ      @@5
        INC     DX
@@5:    LOOP    @@6
        MOV     AX, Width
        IMUL    DX
        SUB     AX, CharSpace
@@3:    POP     DS
End;

(************************* Win Huge Font Handling **************************)

procedure WinHugeCreateList; near; forward;
procedure WinCreateList; near; forward;
procedure CPICreateList; near; forward;

function NewABCArray: PABCLongArray; near;
var
  p: PABCLongArray;
Begin
  New(p);
  if p <> Nil then FillChar(p^, SizeOf(TABCLongArray), 0);
  NewABCArray := p
End;

function LoadHugeFont(aSource: pointer; aProc: TConvertProc;
  aMetric: TTextMetric; aLength: Integer): PFontRec;
var
  fontrec: pfontRec;
  ch: Char;
Begin
  LoadHugeFont := Nil;
  New(FontRec);
  if FontRec = Nil then Exit;
  FillChar(FontRec^, SizeOf(TFontRec), 0);
  with FontRec^ do Begin
    CodePage := $FFFF;
    DiffAttr := 0;
    TextMetric := aMetric;
    FontLength := aLength;
    FontHandle := NewShared(NewABCArray, sh_ABC);
    FontMode := ftWinHuge;
    TempMemSize := 4096;
    srchandle := NewShared(aSource, sh_Obj);
    proc := aProc;
  End;
  LoadHugeFont := FontRec
End;

function WinHugeWidth(FontRec: PFontRec; s: string): Integer;
var
  i: Byte;
  ch: Char;
  Width: Integer;
  abc: PABCLongArray;
Begin
  FontRec^.Proc(FontRec, s);
  abc := SharedList^[FontRec^.FontHandle].p;
  Width := 0;
  For i := 1 to Length(s) do Begin
    ch := s[i];
    If ch <> sMarker then
    with abc^[ch] do
      Inc(Width, a + b + c + sCharSpace)
  End;
  WinHugeWidth := Width - sCharSpace
End;

(************************ Win Font Handling ********************************)

procedure CopyWinMetrics(var FontInfo: TFontInfo;
  var TextMetric: TTextMetric; MultX, DivX, MultY, DivY: Integer); assembler;
var
  Help: Word;
Asm
        PUSH    DS
        LDS     SI, FontInfo
        LES     DI, TextMetric
        MOV     BX, MultY
        MOV     CX, DivY
        MOV     AX, CX
        SAR     AX, 1
        MOV     Help, AX
        MOV     AX, [SI][TFontInfo.dfPixHeight]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmHeight }
        MOV     AX, [SI][TFontInfo.dfAscent]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmAscent }
        MOV     AX, [SI][TFontInfo.dfPixHeight]
        SUB     AX, [SI][TFontInfo.dfAscent]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmDescent }
        MOV     AX, [SI][TFontInfo.dfInternalLeading]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmInternalLeading }
        MOV     AX, [SI][TFontInfo.dfExternalLeading]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmExternalLeading }
        MOV     BX, MultX
        MOV     CX, DivX
        MOV     AX, CX
        SAR     AX, 1
        MOV     Help, AX
        MOV     AX, [SI][TFontInfo.dfAvgWidth]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmAveCharWidth }
        MOV     AX, [SI][TFontInfo.dfMaxWidth]
        IMUL    BX
        ADD     AX, Help
        IDIV    CX
        STOSW   { tmMaxCharWidth }
        MOV     AX, [SI][TFontInfo.dfWeight]
        STOSW   { tmWeight }
        MOV     AX, WORD PTR [SI][TFontInfo.dfItalic]
        STOSW   { tmItalic, tmUnderlined }
        MOV     AL, [SI][TFontInfo.dfStrikeOut]
        STOSB   { tmStruckOut }
        MOV     AX, WORD PTR [SI][TFontInfo.dfFirstChar]
        STOSW   { tmFirstChar, tmLastChar }
        MOV     AX, WORD PTR [SI][TFontInfo.dfDefaultChar]
        ADD     AH, [SI][TFontInfo.dfFirstChar]
        ADD     AL, [SI][TFontInfo.dfFirstChar]
        STOSW   { tmDefaultChar, tmBreakChar }
        MOV     AL, [SI][TFontInfo.dfPitchAndFamily]
        STOSB   { tmPitchAndFamily }
        MOV     AL, [SI][TFontInfo.dfCharSet]
        STOSB   { tmCharSet }
        MOV     AX, 0
        STOSW   { tmOverhang }
        MOV     AX, [SI][TFontInfo.dfHorizRes]
        STOSW   { tmDigitizedAspectX }
        MOV     AX, [SI][TFontInfo.dfVertRes]
        STOSW   { tmDigitizedAspectY }
        POP     DS
End;

function LoadWinTables(F: PStream; Item: PFontRec;
  Handle: hShared): pointer; near;
var
  TblSize: Word;
  p: pointer;

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


 procedure FillTable(p: pointer);
 var
   i: Byte;
   s: string[1];
 Begin
   with Item^ do
   begin
     FillChar(p^, 256, 0);
     with TextMetric do
     If CodePage = $FFFF
     then begin
       s[0] := #1;
       For i := tmFirstChar to tmLastChar do
       begin
         s[1] := chr(i);
         {$IFDEF RUSSIAN}
         AnsiFrom866(@s);
         {$ELSE}
         AnsiFrom437Str(@s);
         {$ENDIF}
         PByteArray(p)^[i] := ord(s[1]) - tmFirstChar
       end
     end
     else
       For i := tmFirstChar to tmLastChar do
         PByteArray(p)^[i] := i - tmFirstChar
   end
 End;

Begin
  with Item^ do Begin
    with TextMetric do
      TblSize := (tmLastChar - tmFirstChar + 1) * EntrySize;
    ForceNewCache(SharedList^[Handle].p,
      256 + TblSize + FontSize);
    p := SharedList^[Handle].p;
    If p = nil then Exit;
    FillTable(p);
    Inc(PtrRec(p).Ofs, 256);
    F^.Seek(WinCharTbl);
    F^.Read(p^, TblSize);
    Inc(PtrRec(p).Ofs, TblSize);
    If FontSize <> 0
    then Begin
      F^.Seek(WinFilePos);
      F^.Read(p^, FontSize)
    End;
  End;
  LoadWinTables := p
End;

procedure ReloadWinHuge(Item: PFontRec); near;
var
  F: PStream;
Begin
  with Item^ do
    If NameHandle <> -1
    then Begin
      F := NewFontStream(PString(SharedList^[NameHandle].p)^);
      if F = Nil then Exit;
      LoadWinTables(F, Item, SrcHandle);
      Dispose(F, Done)
    End
End;

procedure ConvWin3(FontRec: PFontRec; var s: string); far;
type
  PWin3Buf = ^TWin3Buf;
  TWin3Buf = record
    Trans: array[Char] of Char;
    Table: array[Char] of TNewRasterInfo
  end;
var
  p: PWin3Buf;
  posA: LongInt;
  Size: LongInt;
  abc: PABCLongArray;
  F: PStream;
  ch, mappedch: char;
  i: Byte;
Begin
  with FontRec^ do Begin
    F := NewFontStream(PString(SharedList^[NameHandle].p)^);
    if F = Nil then Exit;
    If SharedList^[SrcHandle].p = nil then ReloadWinHuge(FontRec);
    p := SharedList^[SrcHandle].p;
    abc := SharedList^[FontHandle].p;
    with p^ do
      For i := 1 to Length(s) do
      begin
        ch := s[i];
        If abc^[ch].Bits = nil
        then Begin
          mappedch := Trans[ch];
          posA := Table[mappedch].dcOffset;
          Size := Table[Succ(mappedch)].dcOffset - Table[mappedch].dcOffset;
          F^.Seek(WinFilePos + posA + Integer(BitmapDelta));
          with abc^[ch] do Begin
            ForceNewCache(Bits, Size);
            F^.Read(Bits^, Size);
            a := 0;
            b := Table[mappedch].dcWidth;
            c := 0
          End
        End;
      end
  End;
  Dispose(F, Done)
End;

function LoadWinFont(FName: PathStr; reqNr, reqPt, reqLength: Integer;
  CharSpace, AVertAdd, AHeight: Integer; Attr: Word; var FaceName : String): PFontRec;
var
  ResID: Word;
  P, ResTbl: LongInt;
  Item: PFontRec;
  F: PStream;
  FontDir: TFontDir;
  FontInfo: TFontInfo;
  FSize: LongInt;
  TblSize: Word;
  Code: Word;

type
  GenGl = array[0..2] of word;

 function GetOffset(Gl: GenGl): LongInt;
 Begin
   case Code of
     1: GetOffset := Gl[0];
     2: GetOffset := Gl[1];
     3: GetOffset := Gl[2] shl 16 + Gl[1];
   End
 End;

 function GetFontSize: LongInt;
 var
   P: LongInt;
   Gl: GenGl;
   min, max: LongInt;
 Begin
   P := F^.GetPos;
   with FontInfo do
   with Item^ do Begin
     F^.Read(Gl, EntrySize);
     min := GetOffset(Gl);
     F^.Seek(P + (dfLastChar - dfFirstChar + 1) * EntrySize);
     F^.Read(Gl, EntrySize);
     max := GetOffset(Gl)
   End;
   GetFontSize := max - min;
 End;

 function FindEntry: Word;
 var
   i: Integer;

 Begin
   For i := 1 to FontDir.fdCount do Begin
     {ReadFontInfo(F^, FontInfo, true);}
     ReadFontInfoExt(F^, FontInfo, true, nil, @FaceName);
     with FontInfo do
       If (dfPoints = ReqPt) or
          (dfPixHeight = ReqLength) or
          (i = ReqNr) or
          ((ReqNr = 0) and (FontInfo.dfType and 1 <> 0))
       then Begin
         FindEntry := FontInfo.dfResID;
         Exit
       End
   End;
   FindEntry := 0
 End;

 procedure HandleWin3;
 var
   ch: Char;
 Begin
   with Item^ do Begin
     FontSize := 0;
     SrcHandle := NewShared(nil, sh_Buf);
     FontHandle := NewShared(NewABCArray, SizeOf(TABCLongArray));
     FontMode := ftWinHuge;
     proc := ConvWin3
   End
 End;

Begin
  LoadWinFont := nil;
  F := NewFontStream(FName);
  if F = Nil then Exit;
  If not (SkipStub(F^) and SkipToResTbl(F^))
    then Begin Dispose(F, Done); Exit End;

  ResTbl := F^.GetPos;
  If not SkipToResource(F^, nil, rt_FontDir)
    then Begin Dispose(F, Done); Exit End;

  F^.Read(FontDir, SizeOf(TFontDir));
  ResID := FindEntry;
  If ResID = 0
    then Begin Dispose(F, Done); Exit End;

  F^.Seek(ResTbl);
  If not SkipToResource(F^, MakeIntResource(ResID), rt_Font)
    then Begin Dispose(F, Done); Exit End;

  P := F^.GetPos;
  ReadFontInfo(F^, FontInfo, false);
  New(Item);
  if Item = Nil then begin
    Dispose(F, Done);
    Exit;
  end;
  with Item^ do Begin
    TempMemSize := 4096;
    with FontInfo do Begin
      If dfType and 1 = 0
      then Begin
        FontMode  := ftWin;
        EntrySize := 2 * Hi(dfVersion);
        Code    := Hi(dfVersion);
        MultX   := 1;
        DivX    := 1;
      End
      else Begin
        FontMode  := ftWinVector;
        If dfPitchAndFamily and 1 = 0
        then EntrySize := 2
        else EntrySize := 4;
        Code    := 1;
        If ReqPt <> 0 then Begin
          MultX := ReqPt;
          DivX  := dfPoints
        End else
        if ReqLength <> 0 then Begin
          MultX := ReqLength;
          DivX  := dfPixHeight
        End else Begin
          MultX := 1;
          DivX  := 1
        End;
      End;
      MultY     := MultX;
      DivY      := DivX;
      CopyWinMetrics(FontInfo, TextMetric, MultX, DivX, MultY, DivY);
      FontLength  := dfPixHeight;
      VertAdd     := AVertAdd;
      WinCharTbl  := F^.GetPos;
      WinFilePos  := P + dfBitsOffset;
      TblSize     := (dfLastChar - dfFirstChar + 1) * EntrySize;
      case Code of
        1: BitMapDelta := 256 + TblSize;
        2: BitMapDelta := 256 + TblSize - dfBitsOffset;
        3: BitmapDelta := - dfBitsOffset
      end;
      FontHandle  := NewShared(nil, sh_Buf);
      NameHandle  := NewShared(NewStr(FName), sh_Str + sh_NoNull);
      SrcHandle := -1;
    End;
    If (TextMetric.tmCharSet = OEM_CharSet) and (FontInfo.dfVersion > $100)
      then CodePage := 437
      else CodePage := $FFFF;

    If Code = 3
    then HandleWin3
    else Begin
      FSize := GetFontSize;
      If FSize + 256 + TblSize > 65528
        then FontSize := 65272 - TblSize
        else FontSize := FSize
    End;
    with TextMetric do Begin
      tmOverhang  := CharSpace;
      Inc(tmAveCharWidth, tmOverhang);
      Inc(tmMaxCharWidth, tmOverhang);
      If AHeight <> 0
      then tmHeight := AHeight;
      DiffAttr := Attr
    End
  End;
  LoadWinFont := Item;
  Dispose(F, Done);
End;

procedure ReloadWinFont(Item: PFontRec); {near;}
var
  F: PStream;
Begin
  with Item^ do Begin
    F := NewFontStream(PString(SharedList^[NameHandle].p)^);
    if F = Nil then Exit;
    LoadWinTables(F, Item, FontHandle);
    Dispose(F, Done)
  End
End;

(**************************** BGI Font Handling ****************************)

procedure WriteBGIText(Item: PFontRec; x, y, CharSpace: Integer; Color: Word;
  s: string; Marker: Char; MultX, DivX, MultY, DivY: Integer); assembler;
var
  Width: Word;
  xc, yc: Integer;
  xn, yn: Integer;
  SavedDS: Word;
  SavedBX: Word;
  DefChar: Byte;
  lHelpX, hHelpX, lHelpY, hHelpY: Word;
Asm
        MOV     AX, 1
        PUSH    gcnStopUpd
        CALL    ClipNotifyProc
        MOV     AX, 3
        PUSH    gcpLineStyle + gcpGetSize
        PUSH    0
        PUSH    0
        CALL    ChParamsProc
        SUB     SP, AX
        MOV     DX, SP
        MOV     AX, 3
        PUSH    gcpLineStyle + gcpGetParams
        PUSH    SS
        PUSH    DX
        CALL    ChParamsProc
        SUB     SP, 2
        MOV     DX, SP
        MOV     AX, 3
        PUSH    gcpColor + gcpGetParams
        PUSH    SS
        PUSH    DX
        CALL    ChParamsProc
        MOV     SavedDS, DS
        LDS     SI, Item
        TEST    [SI][TFontRec.DiffAttr], ftBold
        MOV     AX, 1
        JZ      @@7
        MOV     AX, 3
@@7:    PUSH    AX
        MOV     AX, [SI][TFontRec.TextMetric.tmAscent]
        ADD     y, AX
        MOV     AL, [SI][TFontRec.TextMetric.tmDefaultChar]
        SUB     AL, [SI][TFontRec.TextMetric.tmFirstChar]
        MOV     DefChar, AL
        MOV     SI, [SI][TFontRec.FontHandle]
        SHL     SI, 3
        MOV     DS, SavedDS
        PUSH    Color
        CALL    SetColor
        CALL    SetThickness
        LDS     BX, SharedList
        LDS     BX, [BX][SI]
        MOV     SavedBX, BX
        LES     SI, s
        MOV     CL, [ES:SI]
        OR      CL, CL
        JZ      @@13
        XOR     CH, CH
        INC     SI

        MOV     BX, MultX
        MOV     AX, DivX
        OR      BX, BX
        JNS     @@20
        inc     ax
        NEG     AX
@@20:   SAR     AX, 1
        CWD
        MOV     lHelpX, AX
        MOV     hHelpX, DX

        MOV     BX, MultY
        MOV     AX, DivY
        OR      BX, BX
        JNS     @@21
        inc     ax
        NEG     AX
@@21:   SAR     AX, 1
        CWD
        MOV     lHelpY, AX
        MOV     hHelpY, DX

@@5:    MOV     BX, SavedBX
        MOV     AX, x
        MOV     xc, AX
        MOV     AX, y
        MOV     yc, AX
        SEGES LODSB
        CMP     AL, Marker
        JNZ     @@8
        ROL     Color, 8
        PUSH    DS
        MOV     DS, SavedDS
        PUSH    Color
        CALL    SetColor
        POP     DS
        LOOP    @@5
        JMP     @@13
@@8:    PUSH    SI
        PUSH    CX
        SUB     AL, [BX+4]              { erstes Zeichen }
        JB      @@18
        CMP     AL, [BX+1]
        JB      @@17
@@18:   MOV     AL, DefChar
@@17:   MOV     AH, 0
        MOV     DI, AX
        SHL     DI, 1
        MOV     SI, AX
        MOV     AL, [BX+1]              { Anzahl Zeichen }
        XOR     AH, AH
        SHL     AX, 1
        ADD     SI, AX
        MOV     AL, [BX+16][SI]         { Width }
        XOR     AH, AH
        MOV     Width, AX
        MOV     SI, [BX+16][DI]         { Offset }
        MOV     DI, [BX+18][DI]         { Next }

        MOV     AX, [BX+5]              { Delta zum Bitmap-Anfang }
        ADD     AX, BX
        ADD     DI, AX
        ADD     SI, AX

        XOR     AX, AX

@@3:    CMP     SI, DI
        JAE     @@4
        LODSB
        AND     AL, 7FH
        TEST    AL, 40H
        JZ      @@1
        OR      AL, 80H
@@1:    CBW
        IMUL    MultX
        ADD     AX, lHelpX
        ADC     DX, hHelpX
        IDIV    DivX
        ADD     AX, x
        MOV     xn, AX

        LODSB
        MOV     CL, AL
        AND     AL, 7FH                 { negativ }
        TEST    AL, 40H
        JZ      @@2
        OR      AL, 80H
@@2:    CBW
        NEG     AX
        IMUL    MultY
        ADD     AX, lHelpY
        ADC     DX, hHelpY
        IDIV    DivY
        ADD     AX, y
        MOV     yn, AX

        TEST    CL, 80H
        JZ      @@14

        PUSH    DS
        PUSH    ES
        PUSH    DI
        PUSH    SI
        PUSH    xc
        PUSH    yc
        PUSH    xn
        PUSH    yn
        MOV     AX, 4                   { Param Count }
        MOV     DS, SavedDS
        CALL    LineProc
        POP     SI
        POP     DI
        POP     ES
        POP     DS
@@14:   MOV     AX, xn
        MOV     xc, AX
        MOV     AX, yn
        MOV     yc, AX
        JMP     @@3

@@4:    MOV     AX, Width
        IMUL    MultX
        ADD     AX, lHelpX
        ADC     DX, hHelpX
        IDIV    DivX
        ADD     AX, CharSpace           { ist schon korrigiert }
        ADD     x, AX
        POP     CX
        POP     SI
        DEC     CX
        JNZ     @@5
@@13:   MOV     DS, SavedDS
        MOV     AX, 1
        PUSH    gcnContUpd
        CALL    ClipNotifyProc
        MOV     DX, SP
        PUSH    gcpColor + gcpSetParams
        PUSH    SS
        PUSH    DX
        MOV     AX, 3
        CALL    ChParamsProc
        ADD     SP, AX
        MOV     DX, SP
        PUSH    gcpLineStyle + gcpSetParams
        PUSH    SS
        PUSH    DX
        MOV     AX, 3
        CALL    ChParamsProc
        ADD     SP, AX
End;

function FindBgiDescent(Item: PFontRec): Integer; near; assembler;
asm
        LES     DI, Item
        MOV     CX, ES:[DI].TFontRec.BgiSize
        MOV     SI, ES:[DI].TFontRec.FontHandle
        SHL     SI, 3
        LES     BX, SharedList
        LES     SI, ES:[BX][SI]
        ADD     CX, SI
        ADD     SI, ES:[SI+5]
        SUB     CX, SI
        SHR     CX, 1
        XOR     DX, DX
        INC     SI
@@1:    MOV     AL, [ES:SI]
        ADD     SI, 2
        AND     AL, 7FH
        TEST    AL, 40H
        JZ      @@2
        OR      AL, 80H
@@2:    CBW
        NEG     AX
        CMP     AX, DX
        JNG     @@3
        MOV     DX, AX
@@3:    LOOP    @@1
        MOV     AX, DX
end;

function BGIWidth(Item: PFontRec; s: string; CharSpace: Integer; Marker: Char;
  MultX, DivX: Integer): Integer; assembler;
var
  Count: Integer;
  lHelp, hHelp: Word;
  DefChar, CharCount: Byte;
Asm
        MOV     BX, MultX
        MOV     AX, DivX
        OR      BX, BX
        JNS     @@20
        inc     ax
        NEG     AX
@@20:   SAR     AX, 1
        CWD
        MOV     lHelp, AX
        MOV     hHelp, DX
        LES     DI, Item
        MOV     AL, [ES:DI+TFontRec.TextMetric.tmDefaultChar]
        SUB     AL, [ES:DI+TFontRec.TextMetric.tmFirstChar]
        MOV     DefChar, AL
        MOV     DI, [ES:DI+TFontRec.FontHandle]
        SHL     DI, 3
        LES     BX, SharedList
        LES     DI, [ES:BX][DI]
        PUSH    DS
        LDS     SI, s
        XOR     DX, DX
        XOR     AX, AX
        MOV     Count, -1
        MOV     CL, [SI]
        OR      CL, CL
        JZ      @@3
        INC     SI
        MOV     CH, ES:[DI+4]
        MOV     AL, ES:[DI+1]
        MOV     CharCount, AL
        XOR     AH, AH
        SHL     AX, 1
        ADD     DI, AX
@@2:    MOV     BL, [SI]
        INC     SI
        CMP     BL, Marker
        JZ      @@1
        SUB     BL, CH
        JB      @@18
        CMP     BL, CharCount
        JB      @@17
@@18:   MOV     BL, DefChar
@@17:   XOR     BH, BH
        ADD     DL, ES:[DI+16][BX]
        ADC     DH, 0
        INC     Count
@@1:    DEC     CL
        JNZ     @@2
        MOV     AX, DX
        IMUL    MultX
        ADD     AX, lHelp
        ADC     DX, hHelp
        IDIV    DivX
        MOV     BX, AX
        MOV     AX, CharSpace
        IMUL    Count
        ADD     AX, BX
@@3:    POP     DS
End;

procedure CopyBgiMetrics(var BgiFontInfo: TBgiFontInfo;
  var TextMetric: TTextMetric; MultX, DivX, MultY, DivY: Integer;
  Attr: Word; Descent, CharSpace, AVertAdd, AHeight: Integer); assembler;
var
  lHelp, hHelp: Word;
Asm
        PUSH    DS
        LDS     SI, BgiFontInfo
        LES     DI, TextMetric
        MOV     BX, MultY
        MOV     CX, DivY
        MOV     AX, CX
        OR      BX, BX
        JNS     @@20
        inc     ax
        NEG     AX
@@20:   SAR     AX, 1
        CWD
        MOV     lHelp, AX
        MOV     hHelp, DX

        MOV     AX, Descent
        IMUL    BX
        ADD     AX, lHelp
        ADC     DX, hHelp
        IDIV    CX
        PUSH    AX

        MOV     AL, [SI][TBgiFontInfo.biPixHeight]
        MOV     AH, 0
        IMUL    BX
        ADD     AX, lHelp
        ADC     DX, hHelp
        IDIV    CX
        POP     DX
        PUSH    AX
        ADD     AX, DX
        CMP     AHeight, 0
        JZ      @@2
        MOV     AX, AHeight
@@2:    STOSW   { tmHeight }
        POP     AX
        STOSW   { tmAscent }
        MOV     AX, DX
        STOSW   { tmDescent }
        XOR     AX, AX
        STOSW   { tmInternalLeading }
        STOSW   { tmExternalLeading }
        STOSW   { tmAveCharWidth }
        STOSW   { tmMaxCharWidth }
        TEST    Attr, ftBold
        MOV     AX, fw_Normal
        JZ      @@1
        MOV     AX, fw_Bold
@@1:    STOSW   { tmWeight }
        MOV     AX, 0
        STOSW   { tmItalic / tmUnderlined }
        STOSB   { tmStruckOut }
        MOV     AL, [SI][TBgiFontInfo.biFirstChar]
        STOSB   { tmFirstChar }
        ADD     AL, [SI][TBgiFontInfo.biCharCount]
        DEC     AL
        STOSB   { tmLastChar }
        MOV     AX, 2020H
        STOSW   { tmDefaultChar / tmBreakChar }
        MOV     AL, ff_DontCare
        MOV     AH, OEM_Charset
        STOSW   { tmPitchAndFamily / tmCharset }
        MOV     AX, CharSpace
        STOSW   { tmOverhang }
        MOV     AX, 100
        STOSW   { tmDigitizedAspectX }
        STOSW   { tmDigitizedAspectY }
        POP     DS
End;

procedure CalcBgiWidths(Item: PFontRec); near;
var
  ch: Char;
  Sum: LongInt;
  width: Integer;
begin
  Sum := 0;
  with Item^ do
  begin
    For ch := Chr(TextMetric.tmFirstChar) to Chr(TextMetric.tmLastChar) do
    begin
      width := BgiWidth(Item, ch, 0, #0, MultX, DivX);
      If width > TextMetric.tmMaxCharWidth
      then TextMetric.tmMaxCharWidth := width;
      Inc(Sum, width)
    end;
    with TextMetric do
      tmAveCharWidth := Sum div (tmLastChar - tmFirstChar + 1);
  end
end;

procedure ReloadBgiFont(Item: PFontRec); near;
var
  F: PStream;
Begin
  with Item^ do Begin
    F := NewFontStream(PString(SharedList^[NameHandle].p)^);
    if F = Nil then Exit;
    F^.Seek($80);
    ForceNewCache(SharedList^[FontHandle].p, BgiSize);
    F^.Read(SharedList^[FontHandle].p^, BgiSize);
    Dispose(F, Done);
  End
End;

function LoadBgiFileFont(FName: PathStr; ReqHeight, AMultX, ADivX, AMultY,
  ADivY: Integer; CharSpace, AVertAdd, AHeight: Integer;
  Attr: Word): PFontRec;
var
  Item: PFontRec;
  F: PStream;
  BgiFontInfo: TBgiFontInfo;
  Signature: LongInt;
  p: pointer;
  fSize: LongInt;
  Options: Word;
  Descent: Integer;
Begin
  LoadBgiFileFont := nil;
  p := GetPointer(FName);
  If p = nil then Begin
    F := NewFontStream(FName);
    if F = Nil then Exit;
    F^.Read(Signature, 4);
    If Signature <> $08084B50 then Begin
      Dispose(F, Done);
      Exit
    End;
    F^.Seek($80);
    F^.Read(BgiFontInfo, SizeOf(TBgiFontInfo));
    If F^.Status <> 0 then Begin
      Dispose(F, Done);
      Exit
    End;
    fSize := F^.GetSize - $80;
    Dispose(F, Done);
    Options := sh_Buf
  End
  else Begin
    PtrRec(p).Ofs := (PtrRec(p).Ofs + $80) and $FFF0;
    Move(p^, BgiFontInfo, SizeOf(TBgiFontInfo));
    FName := '';
    fSize := 0;
    Options := sh_Fixed
  End;
  New(Item);
  if Item = Nil then Exit;
  with Item^ do Begin
    FontMode    := ftBGI;
    CodePage    := 437;
    VertAdd     := AVertAdd;
    BgiSize     := fSize;
    FontHandle  := NewShared(p, Options);
    NameHandle  := NewShared(NewStr(FName), sh_Str + sh_NoNull);
    TempMemSize := 4096;
    If Attr and ftNoDescent = 0
    then Begin
      ReloadBgiFont(Item);
      Descent := FindBgiDescent(Item)
    End
    else Descent := 0;
    with BgiFontInfo do
    If ReqHeight <> 0 then
      If (AMultX = 0) and (AMultY = 0) then Begin
        MultX   := ReqHeight;
        MultY   := ReqHeight;
        DivX    := biPixHeight + Descent;
        DivY    := biPixHeight + Descent
      End
      else Begin
        MultX   := ReqHeight * AMultX * ADivY;
        MultY   := ReqHeight;
        DivX    := (biPixHeight + Descent) * AMultY * ADivX;
        DivY    := biPixHeight + Descent
      End
    else Begin
      MultX     := AMultX;
      MultY     := AMultY;
      DivX      := ADivX;
      DivY      := ADivY
    End;
    CopyBgiMetrics(BgiFontInfo, TextMetric, MultX, DivX, MultY, DivY,
      Attr, Descent, CharSpace, AVertAdd, AHeight);
    CalcBgiWidths(Item);
    DiffAttr    := Attr;
  End;
  LoadBgiFileFont := Item
End;

(**************************** High-Level Routines **************************)

{function DetectMemoryUnit: Boolean;
begin
  DetectMemoryUnit := Seg(HeapError^) = Seg(@InitMemory^)
end;}

procedure InitMyFonts;
Begin
  {MemoryDetected := DetectMemoryUnit;
  If not MemoryDetected then InitMemory;} {DK}
  {Sure, that's really great, but TProgram will not asks if there was
  InitMemory and simply calls it. To use MFonts without turbo vision
  you SHOULD call InitMemory first.}
  {but modifications in Memory unit can solve this rather simply ->}
  InitMemory; 

  FontCollection := New(PFontCollection, Init(8, 4));
  sHoriz := 0; sVert := 0; sFont := 0; sColor := 15; sCharSpace := 0;
  sMarker := #255; potMarker := '~'; sAttr := 0;
  If BIOS8 = nil then GetBIOSPtr;
End;

procedure DoneMyFonts;
Begin
  If FontCollection <> nil then Begin
    Dispose(FontCollection, Done);
    FontCollection := nil
  End;
  {If (not MemoryDetected) and DetectMemoryUnit then DoneMemory}{DK}
  DoneMemory;
End;

procedure GetTextMetrics;
var
  Font: PFontRec;
Begin
  Font := FontCollection^.At(sFont);
  Metrics := Font^.TextMetric
End;

procedure OutTextXY;
var
  Font: PFontRec;
  ChSp: Integer;
  AttrDiff: Word;
Begin
  If (FontCollection = nil) or (s = '') then Exit;
  Font := FontCollection^.At(sFont);
  If Font = nil then Exit;
  with Font^ do
  with TextMetric do Begin
    case sHoriz of
      CenterText: Dec(x, TextWidth(s) div 2);
      RightText:  Dec(x, TextWidth(s))
    End;
    case sVert of
      BottomText: Dec(y, tmHeight);
      CenterText: Dec(y, tmHeight div 2);
      TopText:    ;
      BaseLine:   Dec(y, tmAscent);
      LeadLine:   Dec(y, tmInternalLeading);
    End;
    ChSp := sCharSpace + TextMetric.tmOverhang;
    Inc(y, VertAdd);
    AttrDiff := DiffAttr;
    DoAddAttr(AttrDiff, sAttr);
    case FontMode of
      ftCPI:
      Begin
        If SharedList^[FontHandle].p = nil then ReloadCPIFont(Font);
        WriteBmpText(Ofs(CpiCreateList), Font,
          x, y,
          ChSp, attrdiff, sColor, s, sMarker)
      End;
      ftWin:
      Begin
        If SharedList^[FontHandle].p = nil then ReloadWinFont(Font);
        WriteBmpText(Ofs(WinCreateList), Font,
          x, y,
          ChSp, attrdiff, sColor, s, sMarker)
      End;
      ftWinVector:
      Begin
        If SharedList^[FontHandle].p = nil then ReloadWinFont(Font);
        WriteWinVectorText(Font,
          x, y,
          ChSp, attrdiff, sColor, s, sMarker,
          MultX, DivX, MultY, DivY)
      End;
      ftWinHuge:
      Begin
        If SharedList^[SrcHandle].p = nil then ReloadWinFont(Font);
        Proc(Font, s);
        WriteBmpText(Ofs(WinHugeCreateList), Font,
          x, y, ChSp, attrdiff, sColor, s, sMarker);
      End;
      ftBGI:
      Begin
        If SharedList^[FontHandle].p = nil then ReloadBgiFont(Font);
        WriteBGIText(Font, x, y, ChSp, sColor, s, sMarker,
          MultX, DivX, MultY, DivY)
      End;
    End
  End
End;

procedure SetTextJustify;
Begin
  sHoriz := Horiz;
  sVert := Vert
End;

procedure SetTextParams;
Var
  P : PFontRec;
Begin
  sFont := Font;
  sCharSpace := CharSpace;
  sColor := Color;
  If UseMarker
    then sMarker := potMarker
    else sMarker := #255;
  P := PFontRec(FontCollection^.At(sFont));
  if P <> Nil then GetTempMem(MyFontsTempMem, P^.TempMemSize);
End;

procedure SetExtTextParams;
Begin
  SetTextParams(Font, Charspace, Color, UseMarker);
  sAttr := Attr
End;

procedure SetMarker;
Begin
  potMarker := Marker
End;

function TextWidth;
var
  Item: PFontRec;
Begin
  TextWidth := 0;
  If (FontCollection = nil) or (s='') then Exit;
  Item := FontCollection^.At(sFont);
  If Item = nil then Exit;
  with Item^ do
    case FontMode of
      ftCPI:
        Begin
          If SharedList^[FontHandle].p = nil then ReloadCPIFont(Item);
          TextWidth := CPIWidth(s,
            TextMetric.tmOverhang + sCharSpace, sMarker)
        End;
      ftWin:
        Begin
          If SharedList^[FontHandle].p = nil then ReloadWinFont(Item);
          TextWidth := WinWidth(Item, s,
            TextMetric.tmOverhang + sCharSpace, sMarker)
        End;
      ftWinVector:
        Begin
          If SharedList^[FontHandle].p = nil then ReloadWinFont(Item);
          TextWidth := WinVectorWidth(Item, s,
            TextMetric.tmOverhang + sCharSpace, sMarker, MultX, DivX)
        End;
      ftWinHuge:
        Begin
          If SharedList^[SrcHandle].p = nil then ReloadWinFont(Item);
          TextWidth := WinHugeWidth(Item, s)
        End;
      ftBGI:
        Begin
          If SharedList^[FontHandle].p = nil then ReloadBgiFont(Item);
          TextWidth := BGIWidth(Item, s,
            TextMetric.tmOverhang + sCharSpace, sMarker, MultX, DivX);
        End;
    End
End;

function TextHeight;
var
  Item: PFontRec;
Begin
  TextHeight := 0;
  If FontCollection = nil then Exit;
  Item := FontCollection^.At(sFont);
  If Item = nil then Exit;
  TextHeight := Item^.TextMetric.tmHeight;
End;

function AliasOf;
var
  Item: PFontRec;
Begin
  Item := FullAliasOf(Font);
  with Item^ do Begin
    with TextMetric do Begin
      tmHeight          := AHeight;
      tmOverhang        := CharSpace
    End;
    DiffAttr := Attr;
    VertAdd := AVertAdd;
  End;
  AliasOf := Item
End;

function FullAliasOf;
var
  Item: PFontRec;
Begin
  New(Item);
  FullAliasOf := Item;
  if Item = Nil then Exit;
  with Item^ do Begin
    FillChar(Item^, SizeOf(TFontRec), 0);
    FontMode := ftUnresolved;
    Alias := Font
  End;
End;

function DefFont(Font: PFontRec): Word;
Begin
  if FontCollection <> nil then
  with FontCollection^ do Begin
    Insert(Font);
    DefFont := IndexOf(Font)
  End
End;

procedure ReplaceFont(Font: Word; FontRec: PFontRec);
var
  Item: pointer;
Begin
  if FontCollection <> nil then
  with FontCollection^ do Begin
    Item := At(Font);
    FreeItem(Item);
    AtPut(Font, FontRec)
  End
End;

procedure FreeFont(Font: Word);
Begin
  if FontCollection <> nil then
  with FontCollection^ do
  Begin
    FreeItem(Items^[Font]);
    Items^[Font] := nil
  End
End;

function  GetFontRec(Handle : Word) : PFontRec;
Begin
  GetFontRec := FontCollection^.At(Handle);
End;


{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ Graphic Notification ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}

var
  NextNotify: TNotifyProc;

(*procedure mfBkMode; near;
Begin
  {If BkMode = Opaque
  then FlushProc := FlushOpaque
  else FlushProc := FlushTrans}
End;*)

Var
  AtDrawX, AtDrawY : Word;

Procedure Setup_V; Near; Assembler;
Asm
  push ax
  and ax, $fff8
  mov AtDrawX, ax
  mov AtDrawY, dx
  pop  ax
  mov  cx, ax
  sar  cx, 3
  mov  es:[bx+6],   cx
  mov  es:[bx+0eh], cx
  push cx
  push ax
  mov  cx, ax
  and  cl, 7
  mov al, $ff
  shr al, cl
  mov es:[bx+12h], al
  mov al, 0feh
  mov es:[bx+13h], al
  mov al, 79
  mov es:[bx+10h], al
  pop  ax
  pop  cx
End;


Procedure Flush_Vt; Near;
Label NoWay;
Var
  X, Y, S, H, C, W, ADC, _X : Word;
  BM               : PByteArray;
  Msk              : Byte;
Begin
  asm
    push es
    pusha
    mov ax, seg @data
    mov ds, ax
    mov s, si
    mov ax, es:[bx+2]
    mov H, ax
    mov ax, es:[bx+$17]
    mov C, ax
    mov word ptr [BM], di
    mov word ptr [BM+2], es
  end;

  X := AtDrawX;
  Y := AtDrawY;
  {x := s mod 80 * 8;
  Y := S div 80;}
  if (X > GRT.ClipRect.B.X) or (X + 8 <= GRT.ClipRect.A.X) or
     (Y > GRT.ClipRect.B.Y) or (Y + H <= GRT.ClipRect.A.Y) then Goto NoWay;

  if Y < GRT.ClipRect.A.Y then begin
    Inc(LongInt(BM), GRT.ClipRect.A.Y - Y);
    Dec(H, GRT.ClipRect.A.Y - Y);
    Y := GRT.ClipRect.A.Y;
  end;
  W := 8;
  if X < GRT.ClipRect.A.X then begin
    Dec(W, GRT.ClipRect.A.X - X);
    Msk := Pred(Word(1) shl W);
    _X := GRT.ClipRect.A.X;
  end else begin
    Msk := $FF;
    _X := X;
  end;
  H := MinInteger(H, GRT.ClipRect.B.Y - Y);
  ADC := MinInteger(W, GRT.ClipRect.B.X - _X);
  S := GRT.ClipRect.B.X - MaxInteger(GRT.ClipRect.A.X, X);
  if S < W then
    Msk := Msk and not Pred(1 shl (W - S));
  if ((GRF.Style and ftOpaque) <> 0) and (ADC > 0) then Bar(_X, Y, _X+ADC-1, Y+H-1, GRT.Back);
    {if (X and 8) <> 0 then Msk := Msk and Lo(GRT.LinePattern)
                      else Msk := Msk and Hi(GRT.LinePattern);}
  DisplayXx8Op(X, Y, @BM^[{6}0], Msk, H{ - 7}, GRT);
  NoWay:
  Inc(AtDrawX, 8);
  asm
    popa
    pop es
    inc si
  end;
End;



(*procedure mfInitGraphics; near;
Begin
  {FlushOpaque := Ofs(Flush_Vo);
  FlushTrans := Ofs(Flush_Vt);
  SetupProc := Ofs(Setup_V);}
  mfBkMode
End;


procedure mfCloseGraphics; near;
Begin
End;*)

function NotifyMyFonts(Notice: Word; Info: LongInt): LongInt; far;
Begin
  (*case Notice of
    gnpInitGraphics,
      {mfInitGraphics;}
    gnpCloseGraphics,
      {mfCloseGraphics;}
    gnpBkMode:
      {mfBkMode;}
  end;*)
  NotifyMyFonts := DefaultNotify(Notice, Info, NotifyMyFonts, NextNotify, 20);
End;

(***************************************************************************)
var
  SaveExit: Pointer;

procedure ExitMFonts; far;
begin
  ExitProc := SaveExit;
  DoneMyFonts;
end;

Const
  FlushProcOffs = $15;
  Backing       = $19;
  FakeBPLin     = $1B;
  XOffs         = $0A;

Procedure SetupCache; Near; Assembler; Asm
   mov    es:[bx+02],cx
   call   Setup_V
   mov    ax,bx
   add    ax,001Dh
   mov    es:[bx+04],ax
   mov    es:[bx+17h],di
   mov    di,ax
   xchg   si,ax
   imul   es:word ptr [bx+02]
   mov    cx,ax
   add    ax,si
   mov    es:[bx+08],ax
   mov    ax, 80
   mov    es:[bx+FakeBPLin],ax
   mov    ax, 0
   mov    es:[bx+Backing],ax
   mov    es:byte ptr [bx+14h],0FFh
   {mov    ax,[FlushProc]
   mov    es:[bx+FlushProcOffs],ax}
   cld
   mov    ax,0000
   shr    cx,1
   rep stosw
   jnb    @@1
   stosb
  @@1:
End;

Procedure UpToDate; Near; Assembler; Asm
  mov    cx,es:[bx+02]
  xor    ax,ax
  cld
  shr    cx,1
  rep stosw
  jnb    @@1
  stosb
  @@1:
End;


Procedure GotoPos; Near; Assembler; Asm
   mov    cl,al
   and    cl,07
   sar    ax,03
   mov    di,es:[bx+04]
   cmp    ax,es:[bx+06]
   jle    @@1
   push   cx
   mov    cx,ax
   sub    cx,es:[bx+06]
   xchg   es:[bx+06],ax
   push   ds
   {lds    si,es:[bx+XOffs]}
@@3:push   cx
   push   ax
   {call   word ptr es:[bx+FlushProcOffs]}
   call   Flush_Vt
   call   UpToDate
   mov    ax,di
   sub    ax,es:[bx+08]
   jb     @@2
   add    ax,001Dh
   add    ax,bx
   mov    di,ax
@@2:pop    ax
   pop    cx
   inc    ax
   loop   @@3
   pop    ds
   pop    cx
   mov    es:[bx+XOffs],si
   mov    es:[bx+04],di
@@1:
End;

Procedure SetRightPos; Near; Assembler; Asm
   push   ax
   call   GotoPos
   pop    cx
   and    cl,07
   mov    ax,0FF00h
   shr    ax,cl
   mov    es:[bx+14h],al
   push   ax
   push   ds
   {lds    si,es:[bx+XOffs]}
   mov    ax,es:[bx+06]
   {call   word ptr es:[bx+FlushProcOffs]}
   call   Flush_Vt
   pop    ds
   mov    es:byte ptr [bx+14h],0FFh
   pop    ax
   mov    cx,es:[bx+06]
   cmp    cx,es:[bx+0Eh]
   jl     @@1
   je     @@2
   mov    es:byte ptr [bx+12h],0FFh
@@2:not    al
   and    es:[bx+12h],al
   mov    es:[bx+0Eh],cx
   @@1:
End;

Procedure _23A4; Near; Assembler; Asm
  mov    bx,ss
  mov    es,bx
  mov    bx,ds
  lds    si,[bp+08]
  lea    di,[bp-0102h]
  cld
  lodsb
  mov    cl,al
  not    cl
  mov    ch,00
  add    di,cx
  mov    [bp-02],cx
  stosb
  mov    cl,al
  mov    dx,cx
  rep movsb
  mov    ds,bx
{  test   word ptr [GR.METASTATE],0002}
End;

Procedure _23CE; Near; Assembler; Asm
  mov    si,es:[si+10h]
  shl    si,03
  push   bx
  push   ax
  les    bx,[SharedList]
  les    si,es:[bx+si]
  pop    ax
  pop    bx
End;

Procedure _23FD; Near; Assembler; Asm
    mov    cx, [LocMaximalY]
    mov    dx, 0
    sub    cx,dx
    mov    ax,[bp+12h]
    mov    word ptr [bp-0106h],0000
    sub    dx,ax
    jle    @@1
    mov    [bp-0106h],dx
    sub    [bp-0108h],dx
    jbe    @@2
    mov    ax, 0
@@1:mov    dx, [LocMaximalY]
    sub    dx,[bp+12h]
    cmp    cx,dx
    jle    @@3
    mov    cx,dx
@@3:or     cx,cx
    jg     @@4
@@2:stc
    ret
@@4:cmp    cx,[bp-0108h]
    jg     @@5
    mov    [bp-0108h],cx
@@5:clc
End;

{$W+}
Procedure WriteBMPText; Assembler;
Var
  ParamBuf : array[0..286] of Byte;
Asm
    call   _23A4
(*  je     @@1
    nop
    nop
    nop
    push   0120h
    push   0016h
    add    dx,0003
    push   dx
    mov    ax,0003
    call   far [GR.USER00]
    test   word ptr [GR.METASTATE],0002
    jne    @@1
    nop
    nop
    nop
    mov    ax, word ptr [GR.METAORIGIN]
    add    [bp+14h],ax
    mov    ax, [0454h]
    add    [bp+12h],ax
@@1:test   word ptr [GR.METASTATE],0001
    jne    @@2
    jmp    @@3
@@2:call   GR.SAVEREGS*)
    les    si,[bp+16h]
    mov    al,es:[si+42h]
    mov    [bp-010Ch],al
    mov    ax,es:[si+43h]
    mov    [bp-011Ch],ax
    mov    al,[bp+0Eh]
    xor    ah,ah
    mov    [bp-011Eh],ax
    mov    ax,es:[si+04]
    mov    [bp-010Ah],ax
    mov    [bp-0108h],ax
    call   _23FD
    jnb    @@4
    nop
    nop
    nop
    test   byte ptr [bp+0Eh],40
    jne    @@4
    jmp    @@5
@@4:push   ax
    call   _23CE
    mov    [bp-0114h],si
    mov    [bp-0112h],es
    les    di, [TempMem]
    call   word ptr [CreateProc]
    and    word ptr [bp-011Eh],7CFFh
    mov    bx,di
    mov    [bp-0110h],bx
    mov    [bp-010Eh],es
    pop    dx
    mov    ax,[bp-0118h]
    sub    [bp+14h],ax
    add    ax,[bp+14h]
@@K:push   ax
    push   dx
    mov    cx,[bp-0108h]
    mov    si,0003
    push   es:word ptr [bx]
    mov    di,[bp+0Ch]
    call   SetupCache
    mov    word ptr [bp-0120h],0FFFFh
    push   ds
    lds    si,[bp-0114h]
@@A:add    es:word ptr [bx],0008
    mov    bx,es:[bx]
    mov    ax,es:[bx+02]
    inc    ax
    jne    @@6
    jmp    @@7
@@6:inc    ax
    je     @@8
    mov    ax,es:[bx+04]
    and    ax,7FFFh
    cmp    ax,[bp-011Eh]
    je     @@9
    nop
    nop
    nop
    cmp    word ptr [bp-0120h],0FFFFh
    jne    @@8
    mov    [bp-0120h],ax
@@8:mov    bx,[bp-0110h]
    jmp    @@A
@@9:mov    ax,0FFFEh
    xchg   es:[bx+02],ax
    or     ax,ax
    je     @@B
    mov    ds,ax
    push   si
    mov    si,es:[bx]
    add    si,[bp-0106h]
    push   si
    jmp    @@C
@@B:mov    ax,es:[bx]
    push   si
    add    ax,si
    add    ax,[bp-0106h]
    push   ax
@@C:mov    ax,es:[bx+06]
    add    ax,[bp+14h]
    mov    bx,[bp-0110h]
    call   GotoPos
    pop    si
    push   bx
    push   di
    add    di,es:[bx+02]
    mov    ax,di
    sub    ax,es:[bx+08]
    jb     @@D
    add    ax,001Dh
    add    ax,bx
    mov    di,ax
@@D:pop    bx
    mov    dx,[bp-0108h]
    test   byte ptr [bp+0Eh],01
    je     @@E
    nop
    nop
    nop
    push   bp
    mov    bp,di
    add    bp,es:[bx+02]
    mov    ax,bp
    sub    ax,es:[bx+08]
    jb     @@F
    add    ax,001Dh
    add    ax,bx
    mov    bp,ax
@@F:lodsb
    mov    ah,al
    xor    al,al
    shr    ax,cl
    push   dx
    mov    dx,ax
    shr    dx,1
    jnb    @@G
    or     es:byte ptr [bp],80h
@@G:or     ax,dx
    pop    dx
    or     es:[di],al
    or     es:[bx],ah
    inc    di
    inc    bx
    dec    dx
    jne    @@F
    pop    bp
    jmp    @@H
@@E:lodsb
    mov    ah,al
    xor    al,al
    shr    ax,cl
    or     es:[di],al
    or     es:[bx],ah
    inc    di
    inc    bx
    dec    dx
    jne    @@E
@@H:pop    bx
    pop    si
    jmp    @@A
@@7:pop    ds
    push   bx
    push   es:word ptr [bx+04]
    push   es:word ptr [bx+0Eh]
    mov    ax,es:[bx]
    add    ax,[bp+14h]
    mov    bx,[bp-0110h]
    call   SetRightPos
    mov    ax,[bp-0120h]
    cmp    ax,0FFFFh
    je     @@I
    mov    dx,[bp-011Eh]
    xor    dx,ax
    test   dx,0100h
    je     @@J
    rol    word ptr [bp+0Ch],08
@@J:mov    [bp-011Eh],ax
    les    bx,[bp-0110h]
    add    sp,0006
    pop    es:word ptr [bx]
    pop    dx
    pop    ax
    jmp    @@K
@@I:pop    ax
    pop    cx
    or     cx,cx
    je     @@5
    pop    di
    les    bx,[bp-0110h]
    add    sp,0002
    mov    es:[bx],di
    pop    dx
    pop    di
    push   ax
    add    [bp+12h],cx
    mov    ax,[bp-010Ah]
    mov    [bp-0108h],ax
    call   _23FD
    mov    dx,ax
    pop    ax
    jb     @@5
    add    ax,[bp+14h]
    jmp    @@K
@@5:{call   GR.RESTOREREGS
    mov    dx,03CEh
    mov    al,05
    mov    ah,[0459h]
    out    dx,ax}
@@3:
End;

Procedure _2694; Near; Assembler; Asm
  cld
  xor    ax,ax
  stosw
  stosw
  stosw
  mov    ax,8000h
  mov    [bp-011Ah],ax
  stosw
  dec    ax
  mov    [bp-0118h],ax
End;

Procedure _2746; Near; Assembler; Asm
  cmp    al,[bp+06]
  stc
  jne    @@1
  xor    word ptr [bp-011Eh],0300h
@@1:
End;

Procedure _26A8; Near; Assembler; Asm
    push   di
    push   ax
    push   cx
    push   si
    push   di
    xor    cx,cx
    mov    ax,[bp-0116h]
    cmp    ax,[bp-0118h]
    jnl    @@3
    mov    [bp-0118h],ax
@@3:cmp    ax,es:[di-02]
    jnl    @@4
    add    cx,4
    sub    di,8
    jmp    @@3
@@4:pop    si
    sub    si,2
    mov    di,si
    add    di,8
    std
    seges rep movsw
    stosw
    mov    ax,[bp-011Eh]
    stosw
    mov    ax,dx
    stosw
    pop    si
    pop    cx
    pop    ax
    stosw
    cld
    pop    di
    add    di,8
 End;

 Procedure _26EB; Near; Assembler; Asm
    cmp    es:word ptr [di-06],0FFFFh
    jne    @@5
    add    es:[di-04],dx
    jne    @@6
    sub    di,8
@@6:ret

@@5:cld
    mov    ax,[bp-011Ah]
    stosw
    mov    ax,0FFFFh
    stosw
    mov    ax,dx
    stosw
    mov    ax,8000h
    stosw
End;

Procedure _270E; Near; Assembler; Asm
    mov    ax,0FFFFh
    cmp    ax,es:[di-06]
    jne    @@1
    sub    di,8
@@1:cld
    mov    ax,dx
    stosw
    mov    ax,0FFFFh
    stosw
    xor    ax,ax
    stosw
    mov    ax,8000h
    stosw
    mov    ax, word ptr [TempMem]
    mov    es:[di],ax
    xchg   di,ax
    stosw
    mov    dx,es:[di+0Ch]
    mov    bx,es:[di+0Ah]
    mov    [bp-011Eh],bx
    mov    es:[di+04],dx
    mov    di,ax
    mov    ax,dx
End;



Procedure WinHugeCreateList; Assembler; Asm
      call   _2694
      lea    bx,[bp-0102h]
      add    bx,[bp-02]
      mov    cl,ss:[bx]
      inc    bx
      mov    word ptr [bp-0116h],0000
      push   ds
      lds    si,[bp-0114h]
  @@B:push   bx
      mov    al,ss:[bx]
      or     word ptr [bp-011Eh],8000h
      call   _2746
      jb     @@1
      jmp    @@2
  @@1:mov    bl,al
      xor    bh,bh
      shl    bx,04
      add    bx,si
      push   si
      mov    dx,[bx+0Ch]
      or     dx,dx
      je     @@3
      call   _26EB
  @@3:mov    ax,[bx+06]
      add    [bp-0116h],ax
      mov    ax,[bx]
      mov    dx,[bx+02]
      mov    ch,[bx+08]
  @@5:call   _26A8
      and    word ptr [bp-011Eh],7FFFh
      sub    ch,08
      jbe    @@4
      add    word ptr [bp-0116h],0008
      add    ax,[bp-010Ah]
      jmp    @@5
  @@4:add    ch,08
      mov    al,ch
      xor    ah,ah
      add    ax,[bp-0116h]
      cmp    ax,[bp-011Ah]
      jle    @@6
      mov    [bp-011Ah],ax
  @@6:add    ax,[bx+0Ah]
      cmp    ax,[bp-011Ah]
      jle    @@7
      mov    [bp-011Ah],ax
  @@7:add    ax,[bp+10h]
      cmp    ax,[bp-011Ah]
      jle    @@8
      mov    [bp-011Ah],ax
  @@8:mov    [bp-0116h],ax
      test   byte ptr [bp+0Eh],40h
      je     @@9
      mov    dx,[bx+0Eh]
      or     dx,dx
      je     @@9
      call   _26EB
  @@9:pop    si
  @@2:pop    bx
      inc    bx
      dec    cl
      je     @@A
      jmp    @@B
  @@A:pop    ds
      mov    dx,[bp-011Ah]
      call   _270E
End;

Procedure CPICreateList; Assembler; Asm
    call   _2694
    lea    bx,[bp-0102h]
    add    bx,[bp-02]
    mov    cl,ss:[bx]
    inc    bx
    xor    ch,ch
    mov    dl,[bp-010Ah]
    mov    dh,[bp+10h]
    add    dh,08
    mov    word ptr [bp-0116h],0000
    or     word ptr [bp-011Eh],8000h
@@2:mov    al,ss:[bx]
    inc    bx
    call   _2746
    jnb    @@1
    mul    dl
    push   dx
    xor    dx,dx
    call   _26A8
    pop    dx
    mov    al,dh
    cbw
    add    [bp-0116h],ax
@@1:loop   @@2
    or     dh,dh
    mov    dx,0008
    js     @@3
    mov    dx,[bp-0116h]
@@3:call   _270E
End;

Procedure WinCreateList; Assembler; Asm
    call   _2694
    lea    bx,[bp-0102h]
    add    bx,[bp-02]
    mov    cl,ss:[bx]
    inc    bx
    mov    word ptr [bp-0116h],0000
    mov    dx,[bp-010Ah]
    push   ds
    lds    si,[bp-0114h]
@@6:push   bx
    mov    al,ss:[bx]
    or     word ptr [bp-011Eh],8000h
    call   _2746
    jnb    @@1
    mov    bl,al
    xor    bh,bh
    mov    al,[bx+si]
    mul    byte ptr [bp-010Ch]
    mov    bx,ax
    add    bx,si
    push   si
    mov    si,[bx+0102h]
    add    si,[bp-011Ch]
    mov    ch,[bx+0100h]
@@3:mov    ax,si
    push   dx
    xor    dx,dx
    call   _26A8
    pop    dx
    and    word ptr [bp-011Eh],7FFFh
    sub    ch,08
    jbe    @@2
    add    word ptr [bp-0116h],8
    add    si,dx
    jmp    @@3
@@2:add    ch,8
    mov    al,ch
    xor    ah,ah
    add    ax,[bp-0116h]
    cmp    ax,[bp-011Ah]
    jle    @@4
    mov    [bp-011Ah],ax
@@4:add    ax,[bp+10h]
    cmp    ax,[bp-011Ah]
    jle    @@5
    mov    [bp-011Ah],ax
@@5:mov    [bp-0116h],ax
    pop    si
@@1:pop    bx
    inc    bx
    dec    cl
    jne    @@6
    pop    ds
    mov    dx,[bp-011Ah]
    call   _270E
End;



Begin
  SaveExit := ExitProc;
  ExitProc := @ExitMFonts;
  InitMyFonts;
  CopyRight;
  GetBIOSPtr;
  InstallNotifyProc(GrNotifyProc, NotifyMyFonts);
  {mfInitGraphics;}
End.
