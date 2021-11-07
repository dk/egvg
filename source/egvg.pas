{$F+,I-,S-,T-}
{
  ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
  Û Unit        : Express Graphics Interface Library                   Û
  Û Description : Virtual Graphics Basic Unit                          Û
  Û Author      : Tony Berezin                                         Û
  Û Version     : X01.00 (internal)                                    Û
  Û Release     : 01.05                                                Û
  Û Last update : 2-JUN-1994                                           Û
  ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÛ
}

unit EGVG;


Interface

uses Objects, Dos, GDI, DIB;


function EGVGInitialize: boolean;
procedure EGVGFinitialize;

Procedure SetPixel(X, Y : Word; Color: LongInt);
Procedure SetPixelOp(X, Y : Word; Color : LongInt; Operation: Byte; Device : PImage); {*}
Function  GetPixel(X, Y : Word) : LongInt;
Function  GetPixelBM(X, Y : Word; Device : PImage) : LongInt;                  {*}
Procedure HLine(X, Y, X2 : Word; Color : LongInt);
Procedure VLine(X, Y, Y2 : Word; Color : LongInt);
Procedure DisplayXxY(x, y: integer; BitMap: pointer; Width, Height : Word; Clr : LongInt);
Procedure DisplayXxYClip(x, y: integer; BitMap: pointer; Msk : Byte; Width, Height : Word; Clr : LongInt);
Procedure ReadScanLine(X, Y, XLen : Word; Buffer : Pointer);
Procedure WriteScanLine(X, Y, XLen : Word; Buffer : Pointer);
Procedure WriteScanLineOp(X, Y, XLen : Word; Buffer : Pointer; BitBlt : Byte);
Procedure PutBMPPart(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
Procedure PutBMPPartOp(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;Operation: Byte);
Procedure HLineStyleOp(X1, Y, X2 : Integer; var T : TPaintInfo);
Procedure DisplayXx8Op(x, y: integer; BitMap: pointer; Msk : Byte; Height : Word; var T : TPaintInfo);


{ Main proc for direct write to screen }
procedure QuickSave(xxx, yyy, xLen, yLen: integer);
procedure QuickRestore(xxx, yyy, xLen, yLen: integer);
procedure DirectMousePut(var Buf; x, y, xFrom, yFrom, Width, Height: integer);
procedure PutBufferPart(x1, y1, x2, y2: integer);
procedure VLineStyleT(x, y1, y2, Width: integer; Clr : LongInt; Pattern: word);
Procedure DirectGetImage(x1, y1, x2, y2 : integer; var BitMap);
procedure DirectPutImage(Image: PCoordImage; x, y : integer);
procedure PrepareDrawing;
Procedure EMSAdjSelect;
Procedure SetOutput(OnBuffer : Boolean);


Function  MapBMPLineRead(P : PImage; Y : Word; ColorRef : PColorRef) : PByteArray;
Procedure StretchDIBitmap(Image, Dest : PImage; xxx, yyy, xFrom, yFrom, xLen, yLen, xDest, yDest : integer;
          ColorRef:PColorRef;ClipRect : TRect);
Procedure ImplantDIBitmap(Image,Dest:PImage;X,Y,xFrom,yFrom,xLen,yLen:Integer;Operation:Byte;ColorRef:PColorRef);

{sets user-defined bitBlt action. Action must be far proc.
  es:[di] - byte to be modified, dh - color to put}
Procedure SetUserBitBltProc(Action : Pointer);
Procedure SetColorBitBlt(Color : LongInt; Transparent : Boolean);

const
  RMWbits : byte = 0;



{$IFDEF DPMI}
Var
  CodeAlias  : Word;
{$ENDIF}

Implementation

Uses EGInLine, Memory, VESA;

function VGAEnterGraphics: boolean; far; assembler;
asm
  mov    ax, 0012h
  int    10h
  mov    al, 1
end;

function EGAHiEnterGraphics: boolean; far; assembler;
asm
  mov    ax, 0010h
  int    10h
  mov    al, 1
end;

function EGAEnterGraphics: boolean; far; assembler;
asm
  mov    ax, 000Eh
  int    10h
  mov    al, 1
end;

function EGALoEnterGraphics: boolean; far; assembler;
asm
  mov    ax, 000Dh
  int    10h
  mov    al, 1
end;


function Vesa16_800x600EnterGraphics: boolean; far;
begin Vesa16_800x600EnterGraphics := VesaEnterGraphics($102); end;


procedure StandardLeaveGraphics; assembler;
asm
  mov    ax, 0003h
  int    10h
end;

Procedure DIBAdjSelect;
Var
  I : Word;
Begin
  DIBType := imPlaned;
  MaxColors := 16;
  for I := 0 to 15 do ColorIndex^[I] := I;
End;

Const
  OutBuffer : Boolean = True;

Procedure SetOutput;
Begin
  OutBuffer := OnBuffer;
End;

Procedure HLineStyle(X1, Y, X2 : Integer; Pattern:Word; Clr0, Clr1 : LongInt); external;
Procedure HLineStyleT(X1, Y, X2 : Integer; Clr : LongInt; Pattern:Word); external;
Procedure DirectHLine(X1, Y1, X2 : Integer; Color : LongInt); external;
Procedure DirectDisplayXxY(x, y: integer; BitMap: pointer; Width, Height : Word; Fore, Back: LongInt; Erase : Boolean);
external;
Procedure Make16Buffer(Dest, Source : Pointer; WidthBuf, Height, WIB : Integer; Invert : Boolean); External;

Procedure HLineStyleOp;
Begin
  if T.Device = nil then begin
    if OutBuffer then begin
      if T.Operation = CopyPut then begin
        if T.LineStyle = lsPattern then begin
          HLineStyle(X1, Y, X2, T.Pattern[Y and 7], T.Back, T.Fore)
        end;
      end;
    end else if T.LineStyle = lsLinePattern then begin
      case T.Operation of
      CopyPut :
        case T.LinePattern of
        0 : DirectHLine(X1, Y, X2, T.Back);
        $FFFF : DirectHLine(X1, Y, X2, T.Fore);
        else end;
      XorPut : if T.Back = 0 then begin
        RMWbits := 3 SHL 3;
        HLineStyleT(X1, Y, X2, T.Fore, T.LinePattern);
        RMWbits := 0;
      end;
      else end;
    end;
  end;
End;

Procedure DisplayXx8Op;
Var
  C : LongInt;
Begin
  if T.Device = nil then begin
    if OutBuffer then begin
      if T.Operation = CopyPut then begin
        if T.LineStyle = 0 then
          if T.LinePattern = 0 then C := T.Back else C := T.Fore
        else C := T.Fore;
        if Msk = $FF then
          DisplayXxY(x, y, BitMap, 8, Height, C)
        else
          DisplayXxYClip(x, y, BitMap, Msk, 8, Height, C);
      end;
    end;
  end;
End;


Procedure VLineStyleT_Fake(x, y1, y2, Width: integer; Clr : LongInt; Pattern: word);far;
Begin
  RMWBits := 3 shl 3;
  VLineStyleT(x, y1, y2, Width, Clr, pattern);
  RMWBits := 0;
End;


Procedure  PutBMPPartOp_Fake(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;Operation: Byte; ColorRef : PColorRef);
Var
  Data : Pointer;
  Buf  : PImage;
  NX, NY, MaxX, MaxY, I, NB : Integer;

Begin
  if ((PSImage(Image)^.NBP and imStreamed) = 0) and
     ((PSImage(Image)^.NBP and imColor) in [imMono, imIcon, imCursor, imPlaned]) and
     (Operation in [CopyPut, AndPut, OrPut, XorPut, NotPut])
       then PutBMPPartOp(Image, xxx, yyy, xFrom, yFrom, xLen, yLen, Operation)
     else begin
     NY  := Word(Pointer(LongInt(Image) + 2)^);
     NX  := Word(Pointer(LongInt(Image) + 6)^);
     Data := Pointer(LongInt(Image) + 8);
     if xFrom > NX then Exit;
     if yFrom > NY then Exit;
     MaxX := MinWord(xLen, NX - xFrom);
     MaxY := MinWord(yLen, NY - yFrom);
     if (MaxX = 0) or (MaxY = 0) or (NX = 0) or (NY = 0) then Exit;
     NB := BPline(Image);
     Buf := MemAllocSeg(NB * 2 + 18);
     PSImage(Buf)^.NBP := imPlaned;
     PSImage(Buf)^.Y := 1;
     PSImage(Buf)^.X := NX;
     for I := 0 to MaxY - 1 do begin
       Data := MapBitLineRead(Image, yFrom + I, NB);
       case PSimage(Image)^.NBP and imColor of
       im16  : Impact17Planed(Data, @PSimage(Buf)^.Data, NX);
{       im256 : Impact256Planed(Data, @PSimage(Buf)^.Data, NX, nil);}
       else end;
       PutBMPPartOp(Buf, xxx, yyy + I, xFrom, 0, xLen, 1, Operation);
     end;
     FreeMem(Buf, NB * 2 + 18);
   end;
End;

Procedure  PutBMPPart_Fake(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
Begin
  PutBMPPartOp_Fake(Image, xxx, yyy, xFrom, yFrom, xLen, yLen, CopyPut, @StdColorRefMap);
End;


Procedure HLine_Fake(X, Y, X2 : Word; Color : LongInt);
Begin
  if OutBuffer then HLine(X, Y, X2, Color)
  else DirectHLine(X, Y, X2, Color);
End;

Procedure DisplayXxY_Fake(x, y: integer; BitMap: pointer; Width, Height : Word; Clr : LongInt);
Begin
  if OutBuffer then DisplayXxY(x, y, Bitmap, Width, Height, Clr)
    else DirectDisplayXxY(x, y, Bitmap, Width, Height, Clr, 0, False);
End;


Const
  DriverMethods : TDriverMethods = (
    _SetPixel        : SetPixel;
    _SetPixelOp      : SetPixelOp;
    _GetPixel        : GetPixel;
    _GetPixelBM      : GetPixelBM;
    _HLine           : HLine_Fake;
    _VLine           : VLine;
    _DisplayXxY      : DisplayXxY_Fake;
    _DisplayXxYClip  : DisplayXxYClip;
    _ReadScanLine    : ReadScanLine;
    _WriteScanLine   : WriteScanLine;
    _WriteScanLineOp : WriteScanLineOp;
    _PutBMPPart      : PutBMPPart_Fake;
    _PutBMPPartOp    : PutBMPPartOp_Fake;
    _HLineStyleOp    : HLineStyleOp;
    _DisplayXx8Op    : DisplayXx8Op;
    _QuickSave       : QuickSave;
    _QuickRestore    : QuickRestore;
    _DirectMousePut  : DirectMousePut;
    _VLineStyleT     : VLineStyleT_Fake;
    _DirectGetImage  : DirectGetImage;
    _DirectPutImage  : DirectPutImage;
    _PutBufferPart   : PutBufferPart;
    _MapBMPLineRead  : MapBMPLineRead;
    _Init            : EGVGInitialize;
    _Done            : EGVGFInitialize;
    _StretchDIBitmap : StretchDIBitmap;
    _ImplantDIBitmap : ImplantDIBitmap;
    _SetOutput       : SetOutput;
    _PrepareDrawing  : PrepareDrawing;
    _SetUserBitBltProc:SetUserBitBltProc;
    _SetColorBitBlt  : SetColorBitBlt;
    _EMSAdjSelect    : EMSAdjSelect;
    _DIBAdjSelect    : DIBAdjSelect
  );
  StandardVGAScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssMultiPlane;
    ScreenWidth        : 640;
    ScreenHeight       : 480;
    MaximalX           : 639;
    MaximalY           : 479;
    NumberOfColors     : 16;
    ColorShift         : 4;
    NumPlanes          : 4;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : VGAEnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $12
  );
  Vesa16_800x600ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssMultiPlane;
    ScreenWidth        : 800;
    ScreenHeight       : 600;
    MaximalX           : 799;
    MaximalY           : 599;
    NumberOfColors     : 16;
    ColorShift         : 4;
    NumPlanes          : 4;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa16_800x600EnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $102
  );
  StandardEGAHiScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssMultiPlane;
    ScreenWidth        : 640;
    ScreenHeight       : 350;
    MaximalX           : 639;
    MaximalY           : 349;
    NumberOfColors     : 16;
    ColorShift         : 4;
    NumPlanes          : 4;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : EGAHiEnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $10
  );
  StandardEGAScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssMultiPlane;
    ScreenWidth        : 640;
    ScreenHeight       : 200;
    MaximalX           : 639;
    MaximalY           : 199;
    NumberOfColors     : 16;
    ColorShift         : 4;
    NumPlanes          : 4;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : EGAEnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $0E
  );
  StandardEGALoScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssMultiPlane;
    ScreenWidth        : 320;
    ScreenHeight       : 200;
    MaximalX           : 319;
    MaximalY           : 199;
    NumberOfColors     : 16;
    ColorShift         : 4;
    NumPlanes          : 4;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : EGALoEnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $0D
  );

const
  MaxPlanesNum = 32;
  BitPlaneHandle: Word = 0;

var
  BitPlaneSizeW : Word;
  BitPlaneSizeD : Word;
  BitPlanes : Array [0..MaxPlanesNum-1] of Pointer;


{$L EGVG.OBJ}
procedure PutBufferPart; external;
procedure HLine; external;
procedure VLine; external;
procedure DisplayXxY; external;
procedure DisplayXxYClip; external;
procedure SetPixel; external;
Procedure DirectGetImage; external;
procedure DirectPutImage; external;
procedure DirectMousePut; external;
procedure VLineStyleT; external;
procedure PutBMPPart; external;
procedure PutBMPPartOp; external;
procedure QuickSave; external;
procedure QuickRestore; external;



Const
  FirstNormalPlane: Integer = 0;
{$IFNDEF Dpmi}
  FirstUMBPlane: Integer = 0;
  FirstQEMMPlane: Integer = 0;
{$ENDIF}

{$IFNDEF DPMI}
Const
  PageFrame: word = 0;

function EMSCheck(PgNum: Integer): boolean; near; assembler;
const
  EmsDeviceLen = 8;
  EmsDeviceStr: array[1..EmsDeviceLen] of Char = 'EMMXXXX0';
asm
    cmp    PgNum, 0     { ‘«¨èª®¬ ¬ «® ¯à®áïâ }
    jle    @@Fail
    cmp    PgNum, 4     { ‘«¨èª®¬ ¬­®£® ¯à®áïâ }
    jg     @@Fail
    mov    ax, 3567h
    int    21h          { Get vector 67h -> es:bx }
    mov    di, 0Ah
    mov    si, offset EmsDeviceStr
    mov    cx, EmsDeviceLen
    cld
    rep    cmpsb
    jne    @@Fail
    mov    ah, 41h
    int    67h          { Get page frame segment }
    mov    PageFrame, bx
    mov    ah, 42h
    int    67h          { EMS memory size/available size }
    cmp    bx, PgNum
    jb     @@Fail
    mov    ax, 4300h
    mov    bx, PgNum
    int    67h          { Get pool for 1st bit plane }
    or     ah, ah       { Error allocating EMS }
    jnz    @@Fail
    mov    BitPlaneHandle, dx
@@Success:
    mov    al, 1
    jmp    @@Exit
@@Fail:
    xor    al, al
@@Exit:
end;

procedure FreeEMSPool(Pool: Word); near; assembler;
asm
    mov    ax, PageFrame
    or     ax, ax
    jz     @@Exit
    mov    dx, Pool
    or     dx, dx
    jz     @@Exit
    mov    ax, 4500h
    int    67h
@@Exit:
end;

procedure SwitchPlane(PgNum: Integer); far; assembler;
asm
    mov    bx, offset BitPlaneHandle
    mov    dx, [bx]
    mov    ax, 4400h
    xor    bx, bx
@@Loop:
    cmp    bx, PgNum
    jae    @@Exit
    int    67h
    inc    bx
    mov    ah, 44h
    mov    al, bl
    jmp    @@Loop
@@Exit:
end;
{$ENDIF}

procedure PrepareDrawing;
begin
{$IFNDEF DPMI}
  SwitchPlane(ScreenDriver^.EMSBanks);
{$ENDIF}
end;

{$IFNDEF Dpmi}

{---------------- UMB-Oriented functions ---------------------}

function GetStrategy: word; near; assembler;
asm
    mov    ax, 5800h
    int    21h
    jnc    @@Exit
    mov    ax, -1
@@Exit:
end;

function SetStrategy(Strategy: Word): boolean; near; assembler;
asm
    mov    ax, 5801h
    mov    bx, Strategy
    xor    bh, bh
    int    21h
    mov    al, 0
    jc     @@Exit
    inc    al
@@Exit:
end;

function GetUMBLinkState: boolean; near; assembler;
asm
    mov    ax, 5802h
    int    21h
    jnc    @@Exit
    xor    al, al
@@Exit:
end;

function SetUMBLinkState(Linked: boolean): boolean; near; assembler;
asm
    mov    ax, 5803h
    mov    bl, Linked
    xor    bh, bh
    int    21h
    mov    al, 0
    jc     @@Exit
    inc    al
@@Exit:
end;

function GetFreeMem: Word; near; assembler;
asm
    mov    bx, 0FFFFh
    mov    ah, 48h
    int    21h
    mov    ax, bx
end;

function GetDosMem(var Alloc: Word; Size: Word): boolean; near; assembler;
asm
    les    di, Alloc
    xor    ax, ax
    mov    es:[di], ax
    mov    bx, Size
    mov    ah, 48h
    int    21h
    mov    bx, ax
    mov    al, 0
    jc     @@Exit
    inc    al
    mov    word ptr es:[di], bx
@@Exit:
end;

function FreeDosMem(Alloc: Word): boolean; near; assembler;
asm
    mov    es, Alloc
    mov    ah, 49h
    int    21h
    mov    al, 0
    jc     @@Exit
    inc    al
@@Exit:
end;

var
  OriginalStrategy: Word;
  UMBinDOS: Boolean;
  UMBSegment: Word;
  UMBPlanes, QEMMPlanes: Word;

procedure GetUMBPlanes;
var
  MinimalNeed: Word;
  AllNeed: Word;
  IsFree: Word;
  Segm: Word;
  i: Integer;
label
  SetOldStrategy;
begin
  UMBSegment := 0;
  FirstUMBPlane := FirstNormalPlane;
  if FirstNormalPlane >= 4 then Exit;
  MinimalNeed := (ScreenDriver^.BitPlaneSize + 15) SHR 4;
  AllNeed := MinimalNeed * (4 - FirstNormalPlane);
  OriginalStrategy := GetStrategy;
  UMBinDOS := GetUMBLinkState;
  SetUMBLinkState(True);
  SetStrategy($40);
  IsFree := GetFreeMem;
  if IsFree < MinimalNeed then goto SetOldStrategy;
  while AllNeed > IsFree do Dec(AllNeed, MinimalNeed);
  if not GetDosMem(UMBSegment, AllNeed) then goto SetOldStrategy;
  Segm := UMBSegment;
  Inc(FirstNormalPlane, AllNeed div MinimalNeed);
  Inc(UMBPlanes, AllNeed div MinimalNeed);
  for i := FirstUMBPlane to FirstNormalPlane-1 do begin
    BitPlanes[i] := Ptr(Segm, 0);
    Inc(Segm, MinimalNeed);
  end;
SetOldStrategy:
  SetUMBLinkState(UMBinDOS);
  SetStrategy(OriginalStrategy);
end;

procedure FreeUMBs;
begin
  if UMBSegment = 0 then Exit;
  if FirstUMBPlane >= FirstNormalPlane then Exit;
  if FirstUMBPlane >= 4 then Exit;
  OriginalStrategy := GetStrategy;
  UMBinDOS := GetUMBLinkState;
  SetUMBLinkState(True);
  SetStrategy($40);
  FreeDosMem(UMBSegment); UMBSegment := 0;
  SetUMBLinkState(UMBinDOS);
  SetStrategy(OriginalStrategy);
end;

{---------------- Quarterdeck-Oriented functions ----------------}

Const
  QRPCIn: Byte = 0;

function CheckRPCI: boolean; near; assembler;
asm
    mov    QRPCIn, 0
    mov    ax, 0D200h
@@Repeat:
    mov    bx, 5144h
    mov    cx, 4D45h
    mov    dx, 4D30h
    int    2Fh            { Qaurterdeck RPCI installation check }
    cmp    al, 0FFh
    jne    @@CheckNext
    cmp    bx, 4D45h
    jne    @@CheckNext
    cmp    cx, 4D44h
    jne    @@CheckNext
    cmp    dx, 5652h
    jne    @@CheckNext
    mov    QRPCIn, ah
    mov    al, 1
    jmp    @@Exit
@@CheckNext:
    cmp    ah, 0D1h
    je     @@Fail
    inc    ah
    jnz    @@Repeat
    mov    ah, 0C0h
    jmp    @@Repeat
@@Fail:
    xor    al, al
    mov    QRPCIn, al
@@Exit:
end;

function GetHiRamChain: Word; near; assembler;
asm
    mov    ah, QRPCIn
    or     ah, ah
    jz     @@Fail
    mov    al, 1
    mov    bx, 4849h
    mov    cx, 5241h
    mov    dx, 4D30h
    int    2Fh
    cmp    bx, 4F4Bh
    jne    @@Fail
    mov    ax, cx
    jmp    @@Exit
@@Fail:
    xor    ax, ax
@@Exit:
end;

type
  PMCB = ^TMCB;
  TMCB = record
    IsLast: Char;
    Owner: Word;
    Size: Word;
    Resrv: array [0..2] of byte;
    Name: array [0..7] of char;
  end;

function GetQEMMsUMB(MinSize: Word): Word;
var
  MCB: PMCB;
  Free, FreeSize, All, AllSize: Word;
begin
  GetQEMMsUMB := 0;
  MCB := Ptr(GetHiRamChain, 0);
  if MCB = Nil then Exit;
  while true do begin
    if (MCB^.Owner = 0) and (MCB^.Size >= MinSize) then begin
      GetQEMMsUMB := Seg(MCB^);
      Exit;
    end;
    if MCB^.IsLast = 'Z' then Break;
    MCB := Ptr(Seg(MCB^) + MCB^.Size + 1, 0);
  end;
end;

var
  QEMMMCBS: array [0..MaxPlanesNum-1] of word;

procedure GetQEMMPlanes;
var
  MinimalNeed, AllNeed, PortionNeed: Word;
  i, j: Integer;
  UMB: Word;
  MCB: PMCB;
label
  OnceMore;
begin
  for i:=0 to MaxPlanesNum-1 do QEMMMCBS[i] := 0;
  FirstQEMMPlane := FirstNormalPlane;
  if FirstNormalPlane >= ScreenDriver^.NumPlanes then Exit;
  MinimalNeed := (ScreenDriver^.BitPlaneSize + 15) SHR 4;
  AllNeed := MinimalNeed * (ScreenDriver^.NumPlanes - FirstNormalPlane);
  PortionNeed := AllNeed; j := 0;

OnceMore:
  UMB := GetQEMMsUMB(MinimalNeed);
  if UMB = 0 then Exit;
  MCB := Ptr(UMB, 0);
  while PortionNeed > MCB^.Size do Dec(PortionNeed, MinimalNeed);
  MCB^.Owner := PrefixSeg;
  QEMMMCBS[j] := UMB; Inc(j); Inc(UMB); {!!!}
  Inc(FirstNormalPlane, PortionNeed div MinimalNeed);
  Inc(QEMMPlanes, PortionNeed div MinimalNeed);
  for i := FirstQEMMPlane to FirstNormalPlane-1 do begin
    BitPlanes[i] := Ptr(UMB, 0);
    Inc(UMB, MinimalNeed);
  end;
  PortionNeed := AllNeed - PortionNeed;
  if PortionNeed > 0 then goto OnceMore;
end;

procedure FreeQEMMs;
var
  i: Integer;
  MCB: PMCB;
begin
  if FirstQEMMPlane >= FirstNormalPlane then Exit;
  if FirstQEMMPlane >= ScreenDriver^.NumPlanes then Exit;
  for i:=0 to MaxPlanesNum-1 do if QEMMMCBS[i] <> 0 then begin
    MCB := Ptr(QEMMMCBS[i], 0);
    MCB^.Owner := 0;
    QEMMMCBS[i] := 0;
  end;
end;
{$ENDIF}


function EGVGInitialize: boolean;
var
  i: integer;
begin
with ScreenDriver^ do begin
  EGVGInitialize := False;
  if ((VirtualScreenStyle and (vssSegmented or vssMultiPlane)) <> (vssSegmented or vssMultiPlane)) then Exit;
  BitPlaneSize  := (ScreenWidth div 8) * ScreenHeight;
  LineLength    := ScreenWidth div 8;
  BitPlaneSizeW := BitPlaneSize Div 2;
  BitPlaneSizeD := BitPlaneSizeW Div 2;
  HeapPlanes := 0;
{$IFNDEF DPMI}
  EMSPages := 0;
  EMSBanks := 0;
  UMBPlanes := 0;
  QEMMPlanes := 0;

  FirstNormalPlane := 0;
  if EMSCheck(BitPlaneSize div 16384 + 1) then begin
    BitPlanes[0] := Ptr(PageFrame, 0);
    FirstNormalPlane := 1;
    EMSBanks := (BitPlaneSize div 16384) + 1;
  end;
  GetUMBPlanes;
  GetQEMMPlanes;
{$ENDIF}
  for i := FirstNormalPlane to NumPlanes-1 do begin
    if MaxAvail < BitPlaneSize then Exit;
    BitPlanes[i] := MemAllocSeg(BitPlaneSize);
    Inc(HeapPlanes);
  end;
  for i := 0 to NumPlanes-1 do
    FillChar(BitPlanes[i]^, BitPlaneSize, $81);
  if not EnterGraphics then Exit;
  if VideoCard >= VGA then begin
    for i:=0 to 15 do SetVGARegister(i, TVGARegister(ScreenDriver^.GraphPalette^[I]));
  end else begin
    for i:=0 to 15 do SetEGARegister(I, EGARegisters[I]);
  end;
  EGVGInitialize := True;
  PrepareDrawing;
  if VideoCard >= VGA then GetVGAPalette(0, 16, PMainPalette)
    else Move(Pointer(@StdVGAPalette)^, PMainPalette^, 48);
  DIBAdjSelect;
  BufferedStrategy := True;
end;
MaximalX := ScreenDriver^.MaximalX;
MaximalY := ScreenDriver^.MaximalY;
end;

var
  SaveExit: Pointer;

procedure EmergencyFree; far;
var
  i: integer;
begin
  ExitProc := SaveExit;
{$IFNDEF Dpmi}
  FreeQEMMs;
  FreeUMBs;
{$ENDIF}
{$IFDEF EMSScreen}
  FreeEMSPool(BitPlaneHandle);
  BitPlaneHandle := 0;
{$ENDIF}
end;

procedure EGVGFinitialize;
var
  i: integer;
begin
{$IFNDEF Dpmi}
  FreeQEMMs;
  FreeUMBs;
{$ENDIF}
{$IFDEF EMSScreen}
  FreeEMSPool(BitPlaneHandle);
  BitPlaneHandle := 0;
{$ENDIF}
  for i := FirstNormalPlane to ScreenDriver^.NumPlanes-1 do
    FreeMem(BitPlanes[i], ScreenDriver^.BitPlaneSize);
  ScreenDriver^.LeaveGraphics;
end;


Procedure SetPixelOp;
Begin
  SetPixel(X, Y, Color);
End;

Function GetPixel; Begin End;
Procedure ReadScanLine; Begin End;
Procedure WriteScanLine; Begin End;
Procedure WriteScanLineOp; Begin End;
Procedure EMSAdjSelect; Begin End;
Function  MapBMPlineRead; Begin End;
Procedure StretchDIBitmap; Begin End;
Procedure ImplantDIBitmap; Begin End;
Procedure SetUserBitBltProc; Begin End;
Procedure SetColorBitBlt; Begin End;


Function GetPixelBM;
Var
  I : Byte;
  DX : Integer;
Begin
  DX := PSimage(Device)^.X;
  I := MapBitLineRead(Device, Y, (DX shr 1) + DX and 1)^[X shr 1];
  if (X and 1) = 0 then GetPixelBM := I and $F0 else GetPixelBM := I shr 4;
End;

begin
{$IFNDEF DPMI}
  CheckRPCI;
{$ELSE}
  {íâ® ¤«ï â®£® çâ®¡ë ¯à®¯¨áë¢ âì ª®¤ ­¥ ¢ë§ë¢ ï exception 13.}
  asm
    mov bx, cs
    mov ax, 0Ah
    int 31h
    mov CodeAlias, ax
  end;
{$ENDIF}
  RegisterDriver(@StandardVGAScreenDriver); {1st as default}
  RegisterDriver(@Vesa16_800x600ScreenDriver);
  RegisterDriver(@StandardEGAHiScreenDriver);
  RegisterDriver(@StandardEGAScreenDriver);
  RegisterDriver(@StandardEGALoScreenDriver);
end.
