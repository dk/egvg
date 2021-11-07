{$G+,S-,F+}
{$IFDEF DPMI}
{$C FIXED PRELOAD PERMANENT}
{$ENDIF}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : SVGA Graphics Library                                █
  █ Description : Virtual Graphics SuperVGA 64K-colors driver          █
  █ Author      : Dmitry Karasik                                       █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 26-AUG-1996                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█

}
{$DEFINE BEWARENOTSVGA}
Unit SVGA64K;

Interface

Uses Objects, Memory, EGInline, GDI, DIB;


Function  SVGA64KInitialize : Boolean;
Procedure SVGA64KFInitialize;

Procedure SetOutput(OnBuffer : Boolean); {swaps output for buffered functions}
Procedure PrepareDrawing;

{routines which may be buffered/altered some kinda}
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
Procedure PutBMPPartOp(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;Operation: Byte; ColorRef : PColorRef);
Procedure HLineStyleOp(X1, Y, X2 : Integer; var T : TPaintInfo);
Procedure DisplayXx8Op(x, y: integer; BitMap: pointer; Msk : Byte; Height : Word; var T : TPaintInfo);

{these ones cannot be buffered anyway hey}
procedure QuickSave(xxx, yyy, xLen, yLen: integer);
procedure QuickRestore(xxx, yyy, xLen, yLen: integer);
procedure DirectMousePut(var Buf; x, y, xFrom, yFrom, Width, Height: integer);
procedure VLineStyleT(x, y1, y2, Width: integer; Clr : LongInt; Pattern: word);
Procedure DirectGetImage(x1, y1, x2, y2 : integer; var BitMap);
procedure DirectPutImage(Image: PCoordImage; x, y : integer);
procedure PutBufferPart(x1, y1, x2, y2: integer);
Procedure EMSAdjSelect;

Function  MapBMPLineRead(P : PImage; Y : Word; ColorRef : PColorRef) : PByteArray;
Procedure StretchDIBitmap(Image, Dest : PImage; xxx, yyy, xFrom, yFrom, xLen, yLen, xDest, yDest : integer;
          ColorRef:PColorRef;ClipRect : TRect);
Procedure ImplantDIBitmap(Image,Dest:PImage;X,Y,xFrom,yFrom,xLen,yLen:Integer;Operation:Byte;ColorRef:PColorRef);
{sets user-defined bitBlt action. Action must be far proc.
  es:[di] - byte to be modified, dx - color to put}
Procedure SetUserBitBltProc(Action : Pointer);
Procedure SetColorBitBlt(Color : LongInt; Transparent : Boolean);

Implementation

Uses VESA;

function Vesa64K_320x200EnterGraphics: boolean; far;
begin Vesa64K_320x200EnterGraphics := VesaEnterGraphics($10E); end;
function Vesa64K_640x480EnterGraphics: boolean; far;
begin Vesa64K_640x480EnterGraphics := VesaEnterGraphics($111); end;
function Vesa64K_800x600EnterGraphics: boolean; far;
begin Vesa64K_800x600EnterGraphics := VesaEnterGraphics($114); end;
function Vesa64K_1024x768EnterGraphics: boolean; far;
begin Vesa64K_1024x768EnterGraphics := VesaEnterGraphics($117); end;
function Vesa64K_1280x1024EnterGraphics: boolean; far;
begin Vesa64K_1280x1024EnterGraphics := VesaEnterGraphics($11A); end;

{$IFDEF DPMI}
Var
  CodeAlias  : Word;
{$ENDIF}

Procedure DIBAdjSelect;
Var
  I : Word;
Begin
  HiColor16Bit := 6;
  for I := 0 to 15 do with ScreenDriver^ do ColorIndex^[I] :=
    HiColor(GraphPalette^[I].R, GraphPalette^[I].G, GraphPalette^[I].B);
  for I := 16 to 255 do ColorIndex^[I] :=
    HiColor(PMainPalette^[I, 1], PMainPalette^[I, 2], PMainPalette^[I, 3]);
  DIBType := imHiColor;
  MaxColors := 65536;
End;

Const
  DriverMethods : TDriverMethods = (
    _SetPixel        : SetPixel;
    _SetPixelOp      : SetPixelOp;
    _GetPixel        : GetPixel;
    _GetPixelBM      : GetPixelBM;
    _HLine           : HLine;
    _VLine           : VLine;
    _DisplayXxY      : DisplayXxY;
    _DisplayXxYClip  : DisplayXxYClip;
    _ReadScanLine    : ReadScanLine;
    _WriteScanLine   : WriteScanLine;
    _WriteScanLineOp : WriteScanLineOp;
    _PutBMPPart      : PutBMPPart;
    _PutBMPPartOp    : PutBMPPartOp;
    _HLineStyleOp    : HLineStyleOp;
    _DisplayXx8Op    : DisplayXx8Op;
    _QuickSave       : QuickSave;
    _QuickRestore    : QuickRestore;
    _DirectMousePut  : DirectMousePut;
    _VLineStyleT     : VLineStyleT;
    _DirectGetImage  : DirectGetImage;
    _DirectPutImage  : DirectPutImage;
    _PutBufferPart   : PutBufferPart;
    _MapBMPLineRead  : MapBMPLineRead;
    _Init            : SVGA64KInitialize;
    _Done            : SVGA64KFInitialize;
    _StretchDIBitmap : StretchDIBitmap;
    _ImplantDIBitmap : ImplantDIBitmap;
    _SetOutput       : VESA.SetOutput;
    _PrepareDrawing  : VESA.PrepareDrawing;
    _SetUserBitBltProc:SetUserBitBltProc;
    _SetColorBitBlt  : SetColorBitBlt;
    _EMSAdjSelect    : VESA.EMSAdjSelect;
    _DIBAdjSelect    : DIBAdjSelect
  );
  Vesa64K_320x200ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 320;
    ScreenHeight       : 200;
    MaximalX           : 319;
    MaximalY           : 199;
    NumberOfColors     : 65536;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa64K_320x200EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $10E
  );
  Vesa64K_640x480ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 640;
    ScreenHeight       : 480;
    MaximalX           : 639;
    MaximalY           : 479;
    NumberOfColors     : 65536;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa64K_640x480EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $111
  );
  Vesa64K_800x600ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 800;
    ScreenHeight       : 600;
    MaximalX           : 799;
    MaximalY           : 599;
    NumberOfColors     : 65536;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa64K_800x600EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $114
  );
  Vesa64K_1024x768ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 1024;
    ScreenHeight       : 768;
    MaximalX           : 1023;
    MaximalY           : 767;
    NumberOfColors     : 65536;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa64K_1024x768EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $117
  );
  Vesa64K_1280x1024ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 1280;
    ScreenHeight       : 1024;
    MaximalX           : 1279;
    MaximalY           : 1023;
    NumberOfColors     : 65536;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa64K_1280x1024EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $11A
  );

{fills PixelInfo:
  Bank : bank number
  Offs : offset into bank
  Next : Granularity - offset

  es = SegA000
  di = Offs
}
Procedure GetPixelInfo(X, Y : Word); Far; Assembler; Asm
  push cx
  mov bx, Y
  mov ax, LineLength
  mul bx
  mov bx, X
  add ax, bx
  adc dx, 0
{}shl ax, 1
{}rcl dx, 1
  mov si, word ptr svgaQLD
  mov di, word ptr svgaQLD + 2
  mov bx, si
  and bx, ax
  mov PixelInfo.Offs, bx
  mov al, ah
  mov ah, dl
  mov cl, svgaQGD
  shr ax, cl
  mov dx, ax
  mov di, PixelInfo.Offs
  mov ax, SegA000
  mov es, ax

  mov ax, dx
  mov PixelInfo.Bank, ax
  mov si, word ptr svgaQLD
  sub si, bx
  shr si, 1
  mov PixelInfo.Next, si
  cmp ax, CurrentBank
  jne @@3
  pop cx
  leave
  ret 4
@@3:
  mov CurrentBank, ax
  mov dx, ax
  xor bx, bx
  push dx
  call SwitchCallBack
  pop  dx
  mov bx, 1
  call SwitchCallBack
  { $ENDIF}
  pop cx
End;

Procedure GetPixelInfoBuf(X, Y : Word); Assembler; Asm
  cli
  push cx
  mov bx, Y
  mov ax, LineLength
  mul bx
  mov bx, X
  add ax, bx
  adc dx, 0
{}shl ax, 1
{}rcl dx, 1
  mov si, word ptr emsdQLD     {emm page size}
  mov di, word ptr emsdQLD+2   {as long - 1}
  mov bx, si
  and bx, ax
  mov PixelInfo.Offs, bx
  mov al, ah
  mov ah, dl
  mov cl, emsdQGD     {for emm granularity = 16Kb}
  shr ax, cl
  mov dx, ax

  mov di, PixelInfo.Offs
  mov ax, BufferSeg
  mov es, ax
  mov ax, dx
  mov PixelInfo.Bank, ax
  mov si, word ptr emsdQLD
  sub si, bx
  shr si, 1
  mov PixelInfo.Next, si
  cmp ax, CurrentBankBuf
  sti
  jne @@3
  {$IFNDEF DPMI}
  mov ch, BuffersInEms
  or  ch, ch
  jne  @@4
  pop cx
  leave
  ret 4
@@4:
  mov cx, EMSCurHandle
  cmp cx, EMSHandler
  jne @@3
  {$ENDIF}
  pop cx
  leave
  ret 4
@@3:
  mov CurrentBankBuf, ax
  cmp BuffersInPage, 0
  je @@5
  add ax, BanksInPage
  push ax
  call SelectBank
  pop cx
  leave
  ret 4
@@5:
  push ax
  call SwitchPlane
  pop cx
End;

Procedure SetPixel(X, Y : Word; Color : LongInt);Assembler; Asm
  push x
  push y
  call dword ptr [PGetPixelInfoBuf]
  mov  ax, word ptr Color
  mov  es:[di], ax
End;

Function GetPixel(X, Y : Word) : LongInt; Assembler; Asm
  push x
  push y
  call dword ptr [PGetPixelInfoBuf]
  mov  ax, es:[di]
End;

Function GetPixelBM;
Begin
  GetPixelBM := PWordArray(MapBitLineRead(Device, Y, PSimage(Device)^.X shl 1))^[X];
End;


Procedure HLine; Assembler; Asm
  cld
  push x
  push y
  call dword ptr [PGetPixelInfoBuf]
  mov ax, x2
  inc ax
  sub ax, x
  mov dx, ax
  mov si, ax
  dec dx
  cmp dx, PixelInfo.Next
  ja @@1
  mov cx, ax
  mov ax, word ptr Color
  rep stosw
  leave
  retf 0008
@@1:
  mov cx, PixelInfo.Next
  inc cx
  push cx
  mov ax, word ptr Color
  rep stosw
  mov bx, [PCurrentBankBuf]
  mov ax, [bx]
  inc  ax
  push ax
  call dword ptr [PSelectBankBuf]
  pop  cx
  sub si, cx
  mov cx, si
  mov ax, word ptr Color
  xor di, di
  rep stosw
@@2:
End;

Procedure VLine; Assembler; Asm
  mov ax, y
  cmp ax, y2
  jbe @@0
  mov bx, y2
  mov y, bx
  mov y2, ax
@@0:
  push x
  push y
  call dword ptr [PGetPixelInfoBuf]
  mov  ax, PixelInfo.Next
  xor  dx, dx
  mov cx, y2
  sub cx, y
  inc cx
  mov bx, word ptr Color
@@1:
  mov es:[di], bx
  sub ax, LineLength
  sbb dx, 0
  add di, LineLength
  add di, LineLength
  cmp dx, 0
  jl  @@3
  jg  @@2
  cmp ax, 0
  ja  @@2
@@3:
  mov ax, y2
  inc ax
  inc ax
  sub ax, cx
  push x
  push ax
  call dword ptr [PGetPixelInfoBuf]
  xor dx, dx
  mov ax, PixelInfo.Next
  mov bx, word ptr Color
@@2:
  loop @@1
End;


Procedure DisplayXxY(x, y: integer; BitMap: pointer; Width, Height : Word; Clr : LongInt);
Assembler;
Var
  I : Word;
Asm
  {for i:= y to y + height - 1 do}
  mov ax, y
  mov I, ax
@@1:
  push x
  push I
  call dword ptr [PGetPixelInfoBuf]
  les  si, dword ptr [BitMap]
  mov  bl, byte ptr es:[si]

  mov cx, Width
@@3:
  {if Boolean(byte(BitMap^) and (1 shl (7 - (Width - cx)))) then}
  mov dx, Width
  sub dx, cx
  push cx
  mov cl, 7
  sub cl, dl
  xor ch, ch
  inc ch
  shl ch, cl
  test bl, ch
  je  @@4
  mov  si, dx
  shl  si, 1
  add  si, di
  {set pixel}
  mov dx, word ptr Clr
  mov ax, BufferSeg
  mov es, ax
  mov es:[si], dx
@@4:
  {if Width - cx = PixelInfo.Next then SelectBank(CurrentBank + 1)}
  pop cx
  mov dx, Width
  sub dx, cx
  cmp dx, PixelInfo.Next
  jne @@2

  mov bx, [PCurrentBankBuf]
  mov ax, [bx]
  inc ax
  push ax
  call dword ptr [PSelectBankBuf]
  mov dx, Width
  sub dx, cx
  xor di, di
  sub di, 2
  sub di, dx
  sub di, dx
  les  si, dword ptr [BitMap]
  mov  bl, byte ptr es:[si]

@@2:loop @@3

  add word ptr [BitMap], 1
  adc word ptr [BitMap + 2], 0
  inc I
  mov ax, I
  sub ax, y
  cmp ax, Height
  jl  @@1
End;


Procedure DisplayXxYClip(x, y: integer; BitMap: pointer; Msk : Byte; Width, Height : Word; Clr : LongInt);
Assembler;
Var
  I : Word;
Asm
  mov ax, y
  mov I, ax
@@1:
  push x
  push I
  call dword ptr [PGetPixelInfoBuf]
  les  si, dword ptr [BitMap]
  mov  bl, byte ptr es:[si]

  mov cx, Width
@@3:
  mov dx, Width
  sub dx, cx
  push cx
  mov cl, 7
  sub cl, dl
  xor ch, ch
  inc ch
  shl ch, cl
  mov ah, Msk
  and ah, ch
  test bl, ah
  je  @@4
  mov  si, dx
  shl  si, 1
  add  si, di
  mov dx, word ptr Clr
  mov ax, BufferSeg
  mov es, ax
  mov es:[si], dx
@@4:
  pop  cx
  xor  dh, dh
  cmp  dx, PixelInfo.Next
  jne @@2

  mov bx, [PCurrentBankBuf]
  mov ax, [bx]
  inc ax
  push ax
  call dword ptr [PSelectBankBuf]
  mov dx, Width
  sub dx, cx
  xor di, di
  sub di, 2
  sub di, dx
  sub di, dx
  les  si, dword ptr [BitMap]
  mov  bl, byte ptr es:[si]

@@2:loop @@3

  add word ptr [BitMap], 1
  adc word ptr [BitMap + 2], 0
  inc I
  mov ax, I
  sub ax, y
  cmp ax, Height
  jl  @@1
End;


{эти 2 процс - намордник для моуса, который вызывается извне
и может вызваться во время рисования и все погадить}
Procedure SaveSVGAState; Near; Assembler;
Asm
  mov ax, ds
  mov es, ax
  mov si, offset PixelInfo
  mov di, offset SavePixelInfo
  mov cx, 6
  rep movsb
  mov ax, CurrentBank
  mov SaveBank, ax
  mov CurrentBank, 0FFFFh
End;

Procedure RestoreSVGAState; Near; Assembler;
Asm
  mov ax, ds
  mov es, ax
  mov si, offset SavePixelInfo
  mov di, offset PixelInfo
  mov cx, 6
  rep movsb
  mov ax, SaveBank
  cmp CurrentBank, ax
  jne @@1
  ret
@@1:
  push ax
  call SelectBank
End;

{Quick Save/Restore тянут максимум квадрат 32x32}
procedure QuickRestore(xxx, yyy, xLen, yLen: integer); Assembler;
Var
  Y, I : Word;
Asm
  cli
  call SaveSVGAState
  and  Y, 0
  mov  ax, yyy
  mov  I, ax
@@5:
  push xxx
  push I
  call GetPixelInfo
  mov ax, xLen
  mov cx, ax
  inc ax
  cld
  cmp cx, PixelInfo.Next
  jbe @@1
  mov cx, PixelInfo.Next
  inc cx

  mov  si, offset QuickSaveArea
  add  si, y
  rep movsw

  mov dx, CurrentBank
  inc dx
  push dx
  call SelectBank

  mov cx, xLen
  sub cx, PixelInfo.Next
  dec cx
  xor di, di
  mov  si, offset QuickSaveArea
  add  si, y
  add  si, PixelInfo.Next
  add  si, PixelInfo.Next
  inc  si
  inc  si
  rep movsw

  jmp @@2
@@1:
  mov  si, offset QuickSaveArea
  add  si, y
  rep movsw
@@2:
  mov ax, xLen
  add y, ax
  add y, ax
  inc I
  mov ax, I
  sub ax, yyy
  cmp ax, yLen
  jl  @@5
  call RestoreSVGAState
  sti
End;


procedure QuickSave(xxx, yyy, xLen, yLen: integer); Assembler;
Var
  Y, I : Word;
Asm
  cli
  call SaveSVGAState
  and  Y, 0
  mov  ax, yyy
  mov  I, ax
@@5:
  push xxx
  push I
  call GetPixelInfo
  mov ax, xLen
  mov cx, ax
  inc ax
  cld
  cmp cx, PixelInfo.Next
  jbe @@1
  mov cx, PixelInfo.Next
  inc cx
  push ds
  mov  si, PixelInfo.Offs
  mov  ax, ds
  mov  es, ax
  mov  ax, SegA000
  mov  ds, ax
  mov  di, offset QuickSaveArea
  add  di, y
  rep movsw
  pop ds

  mov dx, CurrentBank
  inc dx
  push dx
  call SelectBank

  mov cx, xLen
  sub cx, PixelInfo.Next
  dec cx
  mov  di, offset QuickSaveArea
  add  di, y
  add  di, PixelInfo.Next
  add  di, PixelInfo.Next
  inc  di
  inc  di
  push ds
  mov  si, 0
  mov  ax, ds
  mov  es, ax
  mov  ax, SegA000
  mov  ds, ax
  rep movsw
  pop ds

  jmp @@2
@@1:
  push ds
  mov  si, PixelInfo.Offs
  mov  ax, ds
  mov  es, ax
  mov  ax, SegA000
  mov  ds, ax
  mov  di, offset QuickSaveArea
  add  di, y
  rep movsw
  pop ds
@@2:
  mov ax, xLen
  add y, ax
  add y, ax
  inc I
  mov ax, I
  sub ax, yyy
  cmp ax, yLen
  jl  @@5
  call RestoreSVGAState
  sti
End;

{специфичен для стандарного вида курсоров как mono and/xor масок}
procedure DirectMousePut(var Buf; x, y, xFrom, yFrom, Width, Height: integer);
Var
  I, J, K : Word;
  _A, _X : Word;
  Ands, Xors : PLongArray;
  Z : LongInt;

Begin
  {$IFDEF PLOTRANGECHECK}
  if (Y > ScreenWidth - 1) or (Height = 0)then Exit;
  if Y + Height > ScreenWidth - 1 then Height := ScreenWidth - Y;
  if (X > ScreenWidth - 1) or (Width = 0) then Exit;
  if X + Width > ScreenWidth - 1 then Width := ScreenWidth - X;
  {$ENDIF}
  Ands := Pointer(LongInt(@Buf) + 6);
  Xors := Pointer(LongInt(Ands) + 128);
  Dec(Width,  xFrom);
  Dec(Height, yFrom);


  for I := Y to Y + Height - 1 do begin
    GetPixelInfo(X, I);
    if LongInt(PixelInfo.Next) + 1 < Width then begin
      for J := 0 to PixelInfo.Next do begin
        K := 31 - LongReverse[J + xFrom];
        if ((Ands^[I - Y + yFrom]) and (LongInt(1) shl K)) <> 0 then _A := $ffff else _A := 0;
        if ((Xors^[I - Y + yFrom]) and (LongInt(1) shl K)) <> 0 then _X := $ffff else _X := 0;
        MemW[SegA000: PixelInfo.Offs + J shl 1] := MemW[SegA000: PixelInfo.Offs + J shl 1] and _A xor _X;
      end;
      SelectBank(CurrentBank + 1);
      for J := 0 to LongInt(Width) - PixelInfo.Next - 2 do begin
        K := 31 - LongReverse[J + xFrom + PixelInfo.Next + 1];
        if ((Ands^[I - Y + yFrom]) and (LongInt(1) shl K)) <> 0  then _A := $ffff else _A := 0;
        if ((Xors^[I - Y + yFrom]) and (LongInt(1) shl K)) <> 0 then _X := $ffff else _X :=  0;
        MemW[SegA000: J shl 1] := MemW[SegA000: J shl 1] and _A xor _X;
      end;
    end else begin
      for J := 0 to Width - 1 do begin
        Z :=  LongInt(1) shl (31 - LongReverse[J + xFrom]);
        K := I - Y + yFrom;
        if ((Ands^[K]) and Z) <> 0 then _A := $ffff else _A := 0;
        if ((Xors^[K]) and Z) <> 0 then _X := $ffff else _X := 0;
        {Mem[SegA000: PixelInfo.Offs + J] := Mem[SegA000: PixelInfo.Offs + J] and _A xor _X;}
        asm
          mov di, PixelInfo.Offs
          add di, J
          add di, J
          mov ax, SegA000
          mov es, ax
          mov ax, es:[di]
          and ax, _A
          xor ax, _X
          mov es:[di], ax
        end;
      end;
    end;
  end;
  RestoreSVGAState;
End;

Const
  _CodeBraks : array[0..21, 0..6] of Byte = (
    ($26, $89, $15, $90, $90, $90, $90),       {mov es:[di], dx}
    ($26, $31, $15, $90, $90, $90, $90),       {xor es:[di], dx}
    ($26, $21, $15, $90, $90, $90, $90),       {and es:[di], dx}
    ($26, $09, $15, $90, $90, $90, $90),       {or  es:[di], dx}
    ($f7, $d2, $26, $89, $15, $90, $90),       {not dx         }
                                               {mov es:[di], dx}
    ($0b, $d2, $74, $03, $26, $89, $15),       {or dx, dx}
                                               {je $+5}
                                               {mov es:[di], dx}
    ($26, $F7, $15, $26, $31, $15, $90),       {not es:[di]}
                                               {xor es:[di], dx}
    ($26, $F7, $15, $26, $21, $15, $90),       {not es:[di]}
                                               {and es:[di], dx}
    ($26, $F7, $15, $26, $09, $15, $90),       {not es:[di]}
                                               {or  es:[di], dx}
    ($f7, $d2, $26, $31, $15, $90, $90),       {not dx         }
                                               {xor es:[di], dx}
    ($f7, $d2, $26, $21, $15, $90, $90),       {not dx         }
                                               {and es:[di], dx}
    ($f7, $d2, $26, $09, $15, $90, $90),       {not dx         }
                                               {or  es:[di], dx}
    ($26, $31, $15, $26, $F7, $15, $90),       {xor es:[di], dx}
                                               {not es:[di]}
    ($26, $21, $15, $26, $F7, $15, $90),       {and es:[di], dx}
                                               {not es:[di]}
    ($26, $09, $15, $26, $F7, $15, $90),       {or  es:[di], dx}
                                               {not es:[di]}
    ($0b, $d2, $74, $03, $26, $31, $15),       {or dx, dx}
                                               {je $+5}
                                               {xor es:[di], dx}
    ($0b, $d2, $74, $03, $26, $21, $15),       {or dx, dx}
                                               {je $+5}
                                               {and es:[di], dx}
    ($0b, $d2, $74, $03, $26, $09, $15),       {or dx, dx}
                                               {je $+5}
                                               {or  es:[di], dx}
    ($90, $90, $90, $90, $90, $90, $90),       {no operation}
    ($33, $d2, $26, $89, $15, $90, $90),       {xor dx, dx}
                                               {mov es:[di], dx}
    ($BA, $FF, $FF, $26, $89, $15, $90),       {mov dx, $FFFF}
                                               {mov es:[di], dx}
    ($90, $90, $90, $90, $90, $90, $90)        {no operation, for user}
  );

Procedure __Before(Operation : Byte);
                     Inline($BE/_CodeBraks/       {mov si, offset _CodeBraks}
                            $31/$C0/              {xor ax, ax   }
                            $B0/$07/              {mov al, 7    }
                            $5B/                  {pop bx}
                            $F6/$E3/              {mul bl}
                            $03/$F0);             {add si, ax   }

                           {cannot be inline}     {mov ax, offset cs:label}
Procedure __After;   Inline({$IFDEF DPMI}
                            $8B/$3E/CodeAlias/    {mov di, CodeAlias}
                            {$ELSE}
                            $8C/$CF/              {mov di, cs   }
                            {$ENDIF}
                            $8E/$C7/              {mov es, di   }
                            $89/$C7/              {mov di, ax   }
                            $B9/$07/$00/          {mov cx, 7    }
                            $FC/                  {cld}
                            $F3/$A4);             {rep movsb    }
Procedure __NullDef; Inline($90/$90/$90/$90/$90/$90/$90);

Procedure MoveOp(var Source, Dest; dwCount : Word; Operation : Byte);
Label __Code, __Rep;
Begin
  __Before(Operation);
  asm mov ax, offset cs:__Code end;
  __After;
  asm
    mov al, $47
    mov dx, word ptr Source
    mov si, word ptr Dest
    cmp dx, si
    jae @@1
    mov al, $4F
    jmp @@1
  @@1:
      stosb
      stosb

      push ds
      lds  si, Source
      les  di, Dest
      mov  cx, dwCount
      cmp  si, di
      jae  __Rep
      add  si, cx
      add  di, cx
      add  si, cx
      add  di, cx
      dec  si
      dec  di
      dec  si
      dec  di
      std
    __Rep:lodsw
      mov dx, ax
    end;
    __Code:__NullDef;
    asm
      inc di
      inc di
      loop __Rep
      pop ds
      cld
    end;
End;


Procedure SetPixelOp(X, Y : Word; Color : LongInt; Operation : Byte; Device : PImage);
Label __Code1, __Code2;
Var
  Data : PbyteArray;
Begin
  if Device <> Nil then begin
    if (X >= PSimage(Device)^.X) or (Y >= PSimage(Device)^.Y) then Exit;
    Data := MapBitLineRead(Device, Y, PSimage(Device)^.X shl 1);
    __Before(Operation);
    asm mov ax, offset cs:__Code1 end;
    __After;
    asm
      mov dx, word ptr Color
      les di, Data
      add di, X
      add di, X
    end;
    __Code1:__NullDef;
    MapBitLineFlush(Device, Y, PSimage(Device)^.X shl 1);
  end else begin
    __Before(Operation);
    asm mov ax, offset cs:__Code2 end;
    __After;
    PGetPixelInfoBuf(X, Y);
    asm mov dx, word ptr Color end;
    __Code2:__NullDef;
  end;
End;

{центровая процедура отрисовки фигур}
procedure HLineStyleOp;
Label
  __Code, __Code1, __Code2, __BMPCode, __BMPCode1, __BMPCode2, __MetaCode1, __MetaCode2;
Var
  Fore, Back            : Word;
  Pattern, ANDer, D, I  : Word;
  Blocked               : Boolean;
  Color                 : Word;

  NBP, NX, NY, ADX, ADY : Word;
  Data            : PWordArray;
  DevCp           : PByteArray;
  DecrX                 : Integer;

Begin
  if X1 > X2 then asm
    mov ax, x1
    mov bx, x2
    mov x2, ax
    mov x1, bx
  end;
  D := LongInt(X2) - X1 + 1;
  case T.LineStyle of
  lsLinePattern : begin
    Blocked := False;
    Pattern := T.LinePattern;
    ANDer   := 15;
    Fore := T.Fore; Back := T.Back;
    asm
      mov ax, x1
      and ax, 7
      mov cl, al
      ror Pattern, cl
    end;
  end;
  lsPattern : begin
    Blocked := True;
    Back := T.Pattern[Y and 7];
    Pattern := Word(Back) shl 8 + Back;
    ANDer   := 7;
    Fore := T.Fore; Back := T.Back;
    asm
      mov ax, x1
      and ax, 7
      mov cl, al
      ror Pattern, cl
    end;
  end;
  lsBitMap8x8 : begin
    if T.Bitmap = Nil then Exit;
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    While (T.BitmapOrg.X >= NX) do Dec(T.BitmapOrg.X, NX);
    ADX := 8;
    ADY := 8;
    I := (Word(Y) mod ADY) + T.BitMapOrg.Y;
    While (I >= NY) do Dec(I, NY);
    Data  := PWordArray(MapBMPLineRead(T.BitMap, I, T.ColorRef));
    DecrX := X1 + T.BitMapOrg.X;
  end;
  lsBitmap : begin
    if T.Bitmap = Nil then Exit;
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    ADX := NX;
    ADY := NY;
    Data  := PWordArray(MapBMPLineRead(T.BitMap, Word(Y) mod ADY, T.ColorRef));
    DecrX := X1;
  end;
  lsBitmapOrg : begin
    if T.Bitmap = Nil then Exit;
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    While (T.BitmapOrg.Y > 0) do Dec(T.BitmapOrg.Y, NY);
    While (T.BitmapOrg.X > 0) do Dec(T.BitmapOrg.X, NX);
    Data  := PWordArray(MapBMPLineRead(T.BitMap, Word(Y - T.BitMapOrg.Y) mod NY, T.ColorRef));
    ADY := $FFFF;
    ADX := NX;
    DecrX := X1 - T.BitMapOrg.X;
  end;
  end;

  if T.Device <> Nil then begin
    if (X1 >= PSimage(T.Device)^.X) or (Y >= PSimage(T.Device)^.Y) then Exit;
    X2 := MinInteger(X2, PSimage(T.Device)^.X - 1);
    DevCp := MapBitLineRead(T.Device, Y, PSimage(T.Device)^.X shl 1);
    if T.LineStyle <= lsPattern then begin
      __Before(T.Operation); asm mov ax, offset cs:__MetaCode1 end; __After;
      for I := 0 to X2-X1 do begin
        if WordBool(Pattern and (Word(1) shl (I and ANDer))) then
          Color := Fore else Color := Back;
        asm
          les di, DevCp
          add di, I
          add di, I
          add di, X1
          add di, X1
          mov dx, Color
        end;
        __MetaCode1:__NullDef;
      end;
    end else begin
      __Before(T.Operation); asm mov ax, offset cs:__MetaCode2 end; __After;
      for I := 0 to X2-X1 do begin
        Color := Data^[Word(DecrX + I) mod ADX];
        asm
          les di, DevCp
          add di, I
          add di, I
          add di, X1
          add di, X1
          mov dx, Color
        end;
      __MetaCode2:__NullDef;
      end;
    end;
    MapBitLineFlush(T.Device, Y, PSimage(T.Device)^.X shl 1);
    Exit;
  end;
  PGetPixelInfoBuf(X1, Y);
  if T.LineStyle <= lsPattern then begin
      if D > LongInt(PixelInfo.Next) + 1 then begin
      __Before(T.Operation); asm mov ax, offset cs:__Code1 end; __After;
      __Before(T.Operation); asm mov ax, offset cs:__Code2 end; __After;
      for I := 0 to PixelInfo.Next do begin
        if WordBool(Pattern and (Word(1) shl (I and ANDer))) then
          Color := Fore else Color := Back;
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, PixelInfo.Offs
          add di, I
          add di, I
          mov dx, Color
        end;
        __Code1:__NullDef;
      end;
      PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
      for I := 0 to LongInt(X2) - X1 - PixelInfo.Next - 1 do begin
        if WordBool(Pattern and (Word(1) shl ((I + PixelInfo.Next + 1) and ANDer))) then
        Color := Fore else Color := Back;
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, I
          add di, I
          mov dx, Color
        end;
        __Code2:__NullDef;
      end;
    end else begin
      __Before(T.Operation); asm mov ax, offset cs:__Code end; __After;
      for I := 0 to LongInt(X2) - X1 do
        begin asm
            mov cx, I
            mov bx, cx
            and cx, ANDer
            xor dx, dx
            inc dx
            shl dx, cl
            test Pattern, dx
            mov dx, Fore
            jne @@1
            mov dx, Back
         @@1:
            mov di, PixelInfo.Offs
            add di, bx
            add di, bx
            mov si, BufferSeg
            mov es, si
        end;
        __Code: __NullDef; end;
    end;
  end else begin
    if D > LongInt(PixelInfo.Next) + 1 then begin
      __Before(T.Operation); asm mov ax, offset cs:__BMPCode1 end; __After;
      __Before(T.Operation); asm mov ax, offset cs:__BMPCode2 end; __After;
      for I := 0 to PixelInfo.Next do begin
        Color := Data^[Word(DecrX + I) mod ADX];
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, PixelInfo.Offs
          add di, I
          add di, I
          mov dx, Color
        end;
        __BMPCode1:__NullDef;
      end;
      PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
      for I := 0 to LongInt(X2) - X1 - PixelInfo.Next - 1 do begin
        Color := Data^[Word(DecrX + I + PixelInfo.Next + 1) mod ADX];
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, I
          add di, I
          mov dx, Color
        end;
        __BMPCode2:__NullDef;
      end;
    end else begin
      __Before(T.Operation); asm mov ax, offset cs:__BMPCode end; __After;
      for I := 0 to {LongInt(X2) - X1}Pred(D) do begin
        {Color := Data^[Word(DecrX + I) mod ADX];}
        asm
          xor dx, dx
          mov ax, DecrX
          add ax, I
          mov si, ADX
          idiv si
          les si, Data
          add si, dx
          add si, dx
          mov dx, es:[si]

          mov ax, BufferSeg
          mov es, ax
          mov di, PixelInfo.Offs
          add di, I
          add di, I
        end;
        __BMPCode: __NullDef;
      end;
    end;
  end;
End;

Procedure DisplayXx8Op(x, y: integer; BitMap: pointer; Msk : Byte;
          Height : Word; var T : TPaintInfo);
Label
  __Code, __Code1, __Code2, __BMPCode, __BMPCode1, __BMPCode2, _Cont, _Cont2, __MetaCode1, __MetaCode2;
Var
  Fore, Back, Color     : Word;
  Pattern, ANDer, D, I  : Word;
  Blocked               : Boolean;
  NN, AMsk              : Byte;

  NBP, NX, NY, ADX, ADY : Word;
  Data            : PWordArray;
  DevCp           : PByteArray;
  DecrX                 : Integer;
  Width                 : Integer;

Begin
  Width := 8;
  if (T.LineStyle in [lsBitmap8x8..lsBitmapOrg]) and (T.Bitmap <> Nil) then begin
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    if T.Device = Nil then begin
      __Before(T.Operation); asm mov ax, offset cs:__BMPCode1 end; __After;
      __Before(T.Operation); asm mov ax, offset cs:__BMPCode2 end; __After;
      __Before(T.Operation); asm mov ax, offset cs:__BMPCode  end; __After;
    end else
      __Before(T.Operation); asm mov ax, offset cs:__MetaCode2 end; __After;
  end;
  if (T.LineStyle in [lsLinePattern, lsPattern]) then begin
    if T.Device = Nil then begin
      __Before(T.Operation); asm mov ax, offset cs:__Code1 end; __After;
      __Before(T.Operation); asm mov ax, offset cs:__Code2 end; __After;
      __Before(T.Operation); asm mov ax, offset cs:__Code end; __After;
    end else
      __Before(T.Operation); asm mov ax, offset cs:__MetaCode1 end; __After;
  end;
  case T.LineStyle of
  lsLinePattern : begin
    Blocked := False;
    Pattern := T.LinePattern;
    ANDer   := 15;
  end;
  lsPattern : begin
    Blocked := True;
    Pattern := T.LinePattern;
    ANDer   := 7;
  end;
  lsBitMap8x8 : begin
    ADX := 8;
    ADY := 8;
    DecrX := X;
  end;
  lsBitmap: begin
    ADX := NX;
    ADY := NY;
    DecrX := X;
  end;
  lsBitmapOrg : begin
    While (T.BitmapOrg.Y > 0) do Dec(T.BitmapOrg.Y, NY);
    While (T.BitmapOrg.X > 0) do Dec(T.BitmapOrg.X, NX);
    ADY := $FFFF;
    ADX := NX;
    DecrX := X - T.BitMapOrg.X;
  end;
  end;
  Fore := T.Fore; Back := T.Back;

  if T.Device <> Nil then begin
    if (X >= PSimage(T.Device)^.X) or (Y >= PSimage(T.Device)^.Y) then Exit;
    if X + Width > PSimage(T.Device)^.X then Width := PSimage(T.Device)^.X - X;
    if (Width = 0) or (Height = 0) then Exit;
    for D := Y to Y + Height - 1 do begin
      AMsk := Msk and PByteArray(Bitmap)^[D-Y];
      if AMsk = 0 then Continue;
      DevCp := MapBitLineRead(T.Device, D, PSimage(T.Device)^.X shl 1);
      if Blocked then begin
        NN := T.Pattern[D and 7];
        Pattern := Word(NN) shl 8 + NN;
      end;
      asm
        mov ax, x
        and ax, 7
        mov cl, al
        ror Pattern, cl
      end;
      if T.LineStyle <= lsPattern then begin
        __Before(T.Operation); asm mov ax, offset cs:__MetaCode1 end; __After;
        for I := 0 to Pred(Width) do begin
          if (AMsk and (Word(1) shl (7 - I and 7))) = 0 then Continue;
          if WordBool(Pattern and (Word(1) shl (I and ANDer))) then
            Color := Fore else Color := Back;
          asm
            les di, DevCp
            add di, I
            add di, I
            add di, x
            add di, x
            mov dx, Color
          end;
          __MetaCode1:__NullDef;
        end;
      end else begin
        case T.LineStyle of
        lsBitMap8x8, lsBitmap :
          Data := PWordArray(MapBMPLineRead(T.BitMap, Word(D) mod ADY, T.ColorRef));
        lsBitmapOrg :
          Data := PWordArray(MapBMPLineRead(T.BitMap, Word(D - T.BitMapOrg.Y) mod NY, T.ColorRef));
        end;
        __Before(T.Operation); asm mov ax, offset cs:__MetaCode2 end; __After;
        for I := 0 to Pred(Width) do begin
          if (AMsk and (Word(1) shl (7 - I and 7))) = 0 then Continue;
          Color := Data^[Word(DecrX + I) mod ADX];
          asm
            les di, DevCp
            add di, I
            add di, I
            add di, x
            add di, x
            mov dx, Color
          end;
        __MetaCode2:__NullDef;
        end;
      end;
      MapBitLineFlush(T.Device, D, PSimage(T.Device)^.X shl 1);
    end;
    Exit;
  end;

if Height > 0 then for D := Y to Y + Height - 1 do begin
  AMsk := Msk and PByteArray(Bitmap)^[D-Y];
  if AMsk = 0 then Continue;
  PGetPixelInfoBuf(X, D);
  if T.LineStyle <= lsPattern then begin
    if Blocked then begin
      NN := T.Pattern[D and 7];
      Pattern := Word(NN) shl 8 + NN;
    end;
    asm
      mov ax, x
      and ax, 7
      mov cl, al
      ror Pattern, cl
    end;
    if Width > LongInt(PixelInfo.Next) + 1 then begin
      for I := 0 to PixelInfo.Next do begin
        if (AMsk and (Word(1) shl (7 - I and 7))) = 0 then Continue;
        if WordBool(Pattern and (Word(1) shl (I and ANDer))) then
          Color := Fore else Color := Back;
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, PixelInfo.Offs
          add di, I
          add di, I
          mov dx, Color
        end;
        __Code1:__NullDef;
      end;
      PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
      for I := 0 to LongInt(Width) - PixelInfo.Next - 2 do begin
        if (AMsk and (Word(1) shl (7 - ((I + PixelInfo.Next + 1) and 7)))) = 0 then Continue;
        if WordBool(Pattern and (Word(1) shl ((I + PixelInfo.Next + 1) and ANDer))) then
        Color := Fore else Color := Back;
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, I
          add di, I
          mov dx, Color
        end;
        __Code2:__NullDef;
      end;
    end else begin
      for I := 0 to Width-1 do  begin
          {if (AMsk and (Word(1) shl (7 - I and 7))) = 0 then Continue;}
          asm
            mov ax, I
            mov cx, 7
            and ax, cx
            sub cx, ax
            xor ax, ax
            inc ax
            shl ax, cl
            test AMsk, al
            jz  _Cont2

            mov cx, I
            mov bx, cx
            and cx, ANDer
            xor dx, dx
            inc dx
            shl dx, cl
            test Pattern, dx
            mov dx, Fore
            jne @@1
            mov dx, Back
         @@1:
            mov di, PixelInfo.Offs
            add di, bx
            add di, bx
            mov si, BufferSeg
            mov es, si
        end;
        __Code: __NullDef;
        _Cont2:
      end;
    end;
  end else if T.Bitmap <> Nil then begin
    case T.LineStyle of
    lsBitMap8x8, lsBitmap :
      Data := PWordArray(MapBMPLineRead(T.BitMap, Word(D) mod ADY, T.ColorRef));
    lsBitmapOrg :
      Data := PWordArray(MapBMPLineRead(T.BitMap, Word(D - T.BitMapOrg.Y) mod NY, T.ColorRef));
    end;
    if Width > LongInt(PixelInfo.Next) + 1 then begin
      for I := 0 to PixelInfo.Next do begin
        if (AMsk and (Word(1) shl (7 - I and 7))) = 0 then Continue;
        Color := Data^[Word(DecrX + I) mod ADX];
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, PixelInfo.Offs
          add di, I
          add di, I
          mov dx, Color
        end;
        __BMPCode1:__NullDef;
      end;
      PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
      for I := 0 to LongInt(Width) - PixelInfo.Next - 2 do begin
        Color := Data^[Word(DecrX + I + PixelInfo.Next + 1) mod ADX];
        if (AMsk and (Word(1) shl ((7 - I + PixelInfo.Next + 1) and 7))) = 0 then Continue;
        asm
          mov dx, BufferSeg
          mov es, dx
          mov di, I
          add di, I
          mov dx, Color
        end;
        __BMPCode2:__NullDef;
      end;
    end else begin
      for I := 0 to Width - 1 do begin
        {if (AMsk and (Word(1) shl (7 - I and 7))) = 0 then Continue;
         Color := Data^[(DecX + I) mod ADX];}
        asm
          mov ax, I
          mov cx, 7
          and ax, cx
          sub cx, ax
          xor ax, ax
          inc ax
          shl ax, cl
          test AMsk, al
          jz  _Cont

          mov ax, DecrX
          add ax, I
          xor dx, dx
          mov si, ADX
          idiv si
          les bx, Data
          add bx, dx
          add bx, dx
          mov dx, es:[bx]

          mov ax, BufferSeg
          mov es, ax
          mov di, PixelInfo.Offs
          add di, I
          add di, I
        end;
        __BMPCode: __NullDef;
        _Cont:
      end;
    end;
  end;
end;
End;

procedure VLineStyleT(x, y1, y2, Width: integer; Clr : LongInt; Pattern: word);
Var
  I, J  : Word;
  O     : LongInt;
Begin
  {$IFDEF PLOTRANGECHECK}
  if Y1 >  ScreenWidth - 1 then Exit;
  if Y2 >  ScreenWidth - 1 then Y2 := ScreenWidth - 1;
  if X >  ScreenWidth - 1 then Exit;
  {$ENDIF}
  if (X and 1) = 0 then asm not Pattern end;
  GetPixelInfo(X, Y1);
  O := PixelInfo.Next;
  for I := Y1 to Y2 do begin
    if O >= Width then
      for J := 0 to Width - 1 do asm
          mov cx, J
          mov bx, cx
          and cx, 7
          xor dx, dx
          inc dx
          shl dx, cl
          test Pattern, dx
          jne @@1
          mov di, PixelInfo.Offs
          add di, bx
          add di, bx
          mov ax, SegA000
          mov es, ax
          mov dx, word ptr Clr
          xor es:[di], dx
        @@1:
        {if Boolean(Pattern and (1 shl (J and 7))) then
        Mem[SegA000 : PixelInfo.Offs + J] := Mem[SegA000 : PixelInfo.Offs + J] xor Clr;}
    end else
      for J := 0 to Width - 1 do  asm
            mov cx, J
            mov bx, cx
            and cx, 7
            xor dx, dx
            inc dx
            shl dx, cl
            test Pattern, dx
            jne @@1
            mov ax, bx
            add ax, X
            push ax
            push i
            call GetPixelInfo
            mov dx, word ptr Clr
            xor es:[di], dx
          @@1:
            xor ax, ax
            mov word ptr o, ax
            mov word ptr o + 2, ax
          {if Boolean(Pattern and (1 shl (J and 7))) then
          GetPixelInfo(J + X, I);
          Mem[SegA000 : PixelInfo.Offs] := Mem[SegA000 : PixelInfo.Offs] xor Clr;}
          {O := 0;}
    end;
    Dec(O, LineLength);
    Inc(PixelInfo.Offs,  LineLength shl 1);
    if O <= 0 then begin
      GetPixelInfo(X, I + 1);
      O := PixelInfo.Next;
    end;
    asm not Pattern end;
  end;
End;

Procedure MoveSW(var Source, Dest; Count : Word); Assembler; Asm
   mov dx, ds
   lds si, Source
   les di, Dest
   mov cx, Count
   cld
   cmp si, di
   jae @@1
   add si, cx
   add di, cx
   add si, cx
   add di, cx
   dec si
   dec si
   dec di
   dec di
   std
@@1: rep movsw
   cld
   mov ds, dx
End;

Procedure FillSW(var Source; Count, Value : Word); Assembler; Asm
  les di, Source
  mov cx, Count
  mov ax, Value
  cld
  rep stosw
End;

Procedure DirectGetImage(x1, y1, x2, y2 : integer; var BitMap);
var
  I, Y : Word;
  xLen : Word;
Begin
  Y := 0;
  TCoordImage(Bitmap).X := x2 - x1 + 1;
  TCoordImage(Bitmap).Y := y2 - y1 + 1;
  xLen := x2 - x1 + 1;
  for I := y1 to y2 do begin
    GetPixelInfo(X1, I);
    if xLen > LongInt(PixelInfo.Next) + 1 then begin
      MoveSW(Mem[SegA000 : PixelInfo.Offs], TCoordImage(Bitmap).Buffer[Y], PixelInfo.Next + 1);
      SelectBank(CurrentBank + 1);
      MoveSW(Mem[SegA000 : 0], TCoordImage(Bitmap).Buffer[Y + PixelInfo.Next shl 1 + 2], xLen - PixelInfo.Next - 1);
    end else MoveSW(Mem[SegA000 : PixelInfo.Offs], TCoordImage(Bitmap).Buffer[Y], xLen);
    Inc(Y, xLen + xLen);
  end;
End;

procedure DirectPutImage(Image: PCoordImage; x, y : integer);
var
  I, YY, XX : Word;
Begin
  YY := 0;
  XX := Image^.X;
  for I := y to y + Image^.Y - 1 do begin
    GetPixelInfo(X, I);
    if xx > LongInt(PixelInfo.Next) + 1 then begin
      MoveSW(Image^.Buffer[YY], Mem[SegA000 : PixelInfo.Offs], PixelInfo.Next + 1);
      SelectBank(CurrentBank + 1);
      MoveSW(Image^.Buffer[YY + PixelInfo.Next shl 1 + 2], Mem[SegA000 : 0], xx - PixelInfo.Next - 1);
    end else MoveSW(Image^.Buffer[YY], Mem[SegA000 : PixelInfo.Offs], xx);
    Inc(YY, xx + xx);
  end;
End;

Procedure WriteScanLine(X, Y, XLen : Word; Buffer : Pointer);
Var
  P : PByteArray absolute Buffer;
  Z : Word;
Begin
  PGetPixelInfoBuf(X, Y);
  Z := PixelInfo.Next;
  if XLen > (LongInt(Z) + 1) then begin
    MoveSW(P^, Mem[BufferSeg:PixelInfo.Offs], Z + 1);
    PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
    MoveSW(P^[Z shl 1 + 2], Mem[BufferSeg:0], XLen - Z - 1);
  end else {MoveSW(P^, Mem[BufferSeg:PixelInfo.Offs], XLen);}asm
    cld
    push ds
    lds  si, buffer
    mov  cx, XLen
    rep  movsw
    pop  ds
  end;
End;

Procedure WriteScanLineOp(X, Y, XLen : Word; Buffer : Pointer; BitBlt : Byte);
Var
  P : PByteArray absolute Buffer;
Begin
  PGetPixelInfoBuf(X, Y);
  if XLen > (LongInt(PixelInfo.Next) + 1) then begin
    MoveOp(P^, Mem[BufferSeg:PixelInfo.Offs], PixelInfo.Next + 1, BitBlt);
    PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
    MoveOp(P^[(PixelInfo.Next shl 1) + 2], Mem[BufferSeg:0], XLen - PixelInfo.Next - 1, BitBlt);
  end else MoveOp(P^, Mem[BufferSeg:PixelInfo.Offs], XLen, BitBlt);
End;

Procedure ReadScanLine(X, Y, XLen : Word; Buffer : Pointer);
Var
  P : PByteArray absolute Buffer;
Begin
  PGetPixelInfoBuf(X, Y);
  if XLen > (LongInt(PixelInfo.Next) + 1) then begin
    MoveSW(Mem[BufferSeg:PixelInfo.Offs], P^, PixelInfo.Next + 1);
    PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
    MoveSW(Mem[BufferSeg:0], P^[PixelInfo.Next shl 1 + 2], XLen - PixelInfo.Next - 1);
  end else {MoveSW(Mem[BufferSeg:PixelInfo.Offs], P^, XLen);}asm
    cld
    push ds
    mov  ax, es
    mov  ds, ax
    mov  si, di
    les  di, buffer
    mov  cx, XLen
    rep  movsw
    pop  ds
  end;
End;

{в отличие от BGI и GDI здесь разумно разнесены функции нормального
и операционного вывода бмпов}
{выводит битмапы с переводом по ColorRef}
Procedure PutBMPPartOp(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;
                       Operation: Byte; ColorRef : PColorRef);
Label
  _MonoCode, _PlanedCode, _EGACode, _VGACode1, _VGACode2, _VGACode;
Var
  Data : PByteArray;
  NX, NY, NBP, BPLin : Word;
  I, J, L, MaxX, MaxY, ADX : Word;
  K : Byte;
  Color : Word;
  PlanesPreCalc : array[0..3] of Word;
  High : Boolean;


Procedure __MovDHColor; Inline($8B/$56/<Color);   {mov dx, Color}

Begin
  NY  := Word(Pointer(LongInt(Image) + 2)^);
  NBP := Word(Pointer(LongInt(Image) + 4)^);
  NX  := Word(Pointer(LongInt(Image) + 6)^);
  Data := Pointer(LongInt(Image) + 8);
  if xFrom > NX then Exit;
  if yFrom > NY then Exit;
  MaxX := MinWord(xLen, NX - xFrom);
  MaxY := MinWord(yLen, NY - yFrom);
  if (MaxX = 0) or (MaxY = 0) or (NX = 0) or (NY = 0) then Exit;
  if Operation > 21 then Exit;
  if ColorRef = Nil then Pointer(ColorRef) := ColorIndex;
  BPLin := BPLine(Image);
  case NBP and imColor of
  im256, imHiColor : begin
      __Before(Operation); asm mov ax, offset cs:_VGACode1 end; __After;
      __Before(Operation); asm mov ax, offset cs:_VGACode2 end; __After;
      __Before(Operation); asm mov ax, offset cs:_VGACode end; __After;
      High := NBP and imColor = imHiColor;
      if High then begin
        XFrom := XFrom shl 1;
        NX := NX shl 1;
      end;
      for I := 0 to MaxY - 1 do begin
        PGetPixelInfoBuf(xxx, yyy + I);
        Data := PByteArray(LongInt(MapBitLineRead(Image, yFrom + I, NX)) + XFrom);
        if MaxX > (LongInt(PixelInfo.Next) + 1) then begin
          for J := 0 to PixelInfo.Next do begin
            if High then Color := PWordArray(Data)^[J] else
              Color := PLongArray(ColorRef)^[Data^[J]];
            asm
              mov dx, BufferSeg
              mov es, dx
              mov di, PixelInfo.Offs
              add di, J
              add di, J
              mov dx, Color
            end;
          _VGACode1:__NullDef;
          end;
          PSelectBankBuf(MemW[DSeg:PCurrentBankBuf] + 1);
          for J := 0 to MaxX - PixelInfo.Next - 2 do begin
            if High then Color := PWordArray(Data)^[J + PixelInfo.Next + 1] else
              Color := PLongArray(ColorRef)^[Data^[J + PixelInfo.Next + 1]];
            asm
              mov dx, BufferSeg
              mov es, dx
              mov di, J
              shl di, 1
              mov dx, Color
            end;
            _VGACode2:__NullDef;
          end;
        end else begin
          asm
            mov di, PixelInfo.Offs
            dec di
            dec di
            mov cx, BufferSeg
          end;
          for J := 0 to MaxX - 1 do begin
            {if High then Color := PWordArray(Data)^[J] else
              Color := PLongArray(ColorRef)^[Data^[J]];}
            asm
              les si, Data
              mov ax, J
              add si, ax
              xor dh, dh
              cmp dh, &High
              jne @@1
              mov dl, es:[si]
              shl dx, 2
              mov bx, dx
              les si, ColorRef
              mov dx, es:[si+bx]
              jmp @@2
          @@1:
              add si, ax
              mov dx, es:[si]
          @@2:
              inc di
              inc di
              mov es, cx
            end;
            _VGACode:__NullDef;
          end;
        end;
        Inc(NBP, BPLin);
      end;
    end;
  (*im16:begin
      __Before(Operation); asm mov ax, offset cs:_EGACode end; __After;
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        for J := 0 to MaxX - 1 do begin
          if Boolean((xFrom + J) and 1) then
            Color := Data^[(xFrom + J) shr 1] and $F
          else Color := Data^[(xFrom + J) shr 1] shr 4;
          if (Operation = AndPut) and (Color > 0) then Color := Color or $F0;
          Color := PLongArray(ColorRef)^[Color];
          PGetPixelInfoBuf(xxx + J, yyy + I);
          __MovDHColor;
          _EGACode:__NullDef;
        end;
        Inc(NBP, BPLin);
      end;
    end;
  imMono:begin
      __Before(Operation); asm mov ax, offset cs:_MonoCode end; __After;
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        for J := 0 to MaxX - 1 do begin
          Color := Data^[(xFrom + J + 0) shr 3];
          Color := Color and (1 shl (7 - (xFrom + J) and 7));
          if Color <> 0 then Color := 15;
          Color := PLongArray(ColorRef)^[Color];
          PGetPixelInfoBuf(xxx + J, yyy + I);
          __MovDHColor;
          _MonoCode:__NullDef;
        end;
        Inc(NBP, BPLin);
      end;
    end;*)
   imMono:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        ExpandMonoHi(Data, @ScrollMoveArea, MaxX, XFrom, ColorIndex);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
    end;
    im16:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Expand16Hi(Data, @ScrollMoveArea, MaxX, XFrom, ColorIndex);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
    end;
    imTC:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Impact32Hi(Data, @ScrollMoveArea, MaxX, XFrom);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
    end;
  imIcon, imCursor, imPlaned, imPlaned16 : begin
      if IsImageStreamed(Image) then Exit;
      {рудиментарная отработка 4-х плановых изображений}
      __Before(Operation); asm mov ax, offset cs:_PlanedCode end; __After;
      NBP := yFrom * BPLin;
      for I := 0 to 3 do PlanesPreCalc[I] := (NY * BPLin) * I;
      for I := 0 to MaxY - 1 do begin
        for J := 0 to MaxX - 1 do begin
          Color:= 0;  ADX := NBP + (xFrom + J) shr 3;
          K := (1 shl (7 - (xFrom + J) and 7));
          if (Data^[PlanesPreCalc[0] + ADX] and K) <> 0 then Color := Color or 8;
          if (Data^[PlanesPreCalc[1] + ADX] and K) <> 0 then Color := Color or 4;
          if (Data^[PlanesPreCalc[2] + ADX] and K) <> 0 then Color := Color or 2;
          if (Data^[PlanesPreCalc[3] + ADX] and K) <> 0 then Color := Color or 1;
          Color := PLongArray(ColorRef)^[Color];
          {if (Operation = AndPut) and (Color > 0) then Color := Color or $F0;}
          PGetPixelInfoBuf(xxx + J, yyy + I);
          __MovDHColor;
          _PlanedCode:__NullDef;
        end;
        Inc(NBP, BPLin);
      end;
    end;
  else end;
End;

Procedure PutBMPPart(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
Var
  Data : PByteArray;
  NX, NY, NBP, BPLin{, ADX} : Word;
  I, J, K, MaxX, MaxY : Word;

Begin
  NY  := Word(Pointer(LongInt(Image) + 2)^);
  NBP := Word(Pointer(LongInt(Image) + 4)^);
  NX  := Word(Pointer(LongInt(Image) + 6)^);
  Data := Pointer(LongInt(Image) + 8);
  if xFrom > NX then Exit;
  if yFrom > NY then Exit;
  MaxX := MinWord(xLen, NX - xFrom);
  MaxY := MinWord(yLen, NY - yFrom);
  if (MaxX = 0) or (MaxY = 0) or (NX = 0) or (NY = 0) then Exit;
  BPLin := BPLine(Image);
  case NBP and imColor of
   imHiColor:begin
      for I := 0 to MaxY - 1 do begin
        Data := PByteArray(LongInt(MapBitLineRead(Image, yFrom + I, BPLin)) + XFrom shl 1);
        WriteScanLine(xxx, yyy + I, MaxX, Data);
      end;
    end;
   im256:begin
      for I := 0 to MaxY - 1 do begin
        Data := PByteArray(LongInt(MapBitLineRead(Image, yFrom + I, NX)));
        ExpandHi(Data, @ScrollMoveArea, MaxX, XFrom, ColorIndex);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
    end;
   imMono:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        ExpandMonoHi(Data, @ScrollMoveArea, MaxX, XFrom, ColorIndex);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
    end;
    im16:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Expand16Hi(Data, @ScrollMoveArea, MaxX, XFrom, ColorIndex);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
    end;
    imTC:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Impact32Hi(Data, @ScrollMoveArea, MaxX, XFrom);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
    end;
    imIcon, imCursor, imPlaned, imPlaned16:begin
      {if IsImageStreamed(Image) then Exit;}
      {рудиментарная отработка 4-х плановых изображений}
      PrepareExpandPlaned(NX, False);
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        ExpandPlaned(Data, @ScrollMoveArea, MaxX, XFrom);
        ExpandHi(@ScrollMoveArea, @ScrollMoveArea, MaxX, 0, ColorIndex);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
    end;
  else end;
End;

procedure PutBufferPart(x1, y1, x2, y2: integer);
Var
  J, Y, X : Word;
  MODD : Word;
  P    : PByteArray;
  Last : Word;

Procedure Switch(Bank : Word);
Inline($33/$DB/ {xor bx, bx}
       $5A/     {pop dx}
       $FF/$1E/SwitchCallBack  {call SwitchCallBack});

Begin
  if not BufferedStrategy then Exit;
  if X2 > MaximalX then X2 := MaximalX;
  if Y2 > MaximalY then Y2 := MaximalY;
  if BuffersInPage then begin
    X := X2 - X1 + 1;
    if VESAAcceleratorPresent then begin
      for Y := Y1 to Y2 do begin
        GetPixelInfoBuf(X1, Y);
        if Last <> CurrentBankBuf then asm
          mov  dx, CurrentBankBuf
          mov  Last, dx
          xor  bx, bx
          push dx
          call SwitchCallBack
          mov  bx, 1
          pop  dx
          add  dx, BanksInPage
          call SwitchCallBack
        end;
        if x > LongInt(PixelInfo.Next) + 1 then begin
          MoveSW(Mem[SegA000 : PixelInfo.Offs], Mem[SegA000 : PixelInfo.Offs], PixelInfo.Next + 1);
          asm
            inc  CurrentBankBuf
            mov  dx, CurrentBankBuf
            mov  Last, dx
            xor  bx, bx
            push dx
            call SwitchCallBack
            mov  bx, 1
            pop  dx
            add  dx, BanksInPage
            call SwitchCallBack
          end;
          MoveSW(Mem[SegA000 : 0], Mem[SegA000 : 0], x - PixelInfo.Next - 1);
        end else {Move(Mem[SegA000:PixelInfo.Offs], Mem[SegA000:PixelInfo.Offs], x);} asm
          push ds
          cld
          mov  si, PixelInfo.Offs
          mov  ax, SegA000
          mov  es, ax
          mov  ds, ax
          mov  di, si
          mov  cx, x
          rep  movsw
          pop  ds
        end;
      end;
    end else begin
      for Y := Y1 to Y2 do begin
        GetPixelInfoBuf(X1, Y);
        Switch(CurrentBank);
        if x > LongInt(PixelInfo.Next) + 1 then begin
          MoveSW(Mem[SegA000 : PixelInfo.Offs], ScrollMoveArea, PixelInfo.Next + 1);
          Switch(CurrentBankBuf);
          MoveSW(ScrollMoveArea, Mem[SegA000 : PixelInfo.Offs], PixelInfo.Next + 1);
          Switch(CurrentBank+1);
          MoveSW(Mem[SegA000 : 0], ScrollMoveArea, x - PixelInfo.Next - 1);
          Switch(CurrentBankBuf+1);
          MoveSW(ScrollMoveArea, Mem[SegA000 : 0], x - PixelInfo.Next - 1);
          Inc(CurrentBankBuf);
          Inc(CurrentBank);
        end else{ Move(Mem[SegA000:PixelInfo.Offs], ScrollMoveArea, x);}
          asm
            cld
            mov  di, offset ScrollMoveArea
            mov  si, PixelInfo.Offs
            mov  ax, SegA000
            mov  bx, ds
            mov  ds, ax
            mov  es, bx

            mov  cx, x
            rep  movsw

            mov  bx, es
            mov  ds, bx
            mov  ax, SegA000
            mov  es, ax

            xor bx, bx
            mov dx, CurrentBankBuf
            call SwitchCallBack
          {Move(ScrollMoveArea, Mem[SegA000:PixelInfo.Offs], x);}
            mov  si, offset ScrollMoveArea

            mov  di, PixelInfo.Offs
            mov  cx, x
            rep  movsw
        end;
      end;
    end;
    CurrentBankBuf := $FFFF;
    CurrentBank    := $FFFF;
  end else begin
    SelectBankBuf(0);
    SelectBank(0);
    with ScreenDriver^ do if EMSBanks >= MaxBanks then begin
      J := Granularity div BufGranula;
      X := X2 - X1 + 1;
      for Y := Y1 to Y2 do begin
        GetPixelInfo(X1, Y);
        GetPixelInfoBuf(X1, Y);
        Modd := (CurrentBankBuf mod J) * BufGranula;
        if x > LongInt(PixelInfo.Next) + 1 then begin
          MoveSW(Mem[BufferSeg : PixelInfo.Offs], Mem[SegA000 : PixelInfo.Offs + Modd] , PixelInfo.Next + 1);
          SelectBankBuf(CurrentBankBuf + 1);
          Inc(Modd, BufGranula);
          if (Modd and svgaQLD) = 0 then begin
            SelectBank(CurrentBank + 1);
            Modd := 0;
          end;
          MoveSW(Mem[BufferSeg : 0], Mem[SegA000 : Modd], x - PixelInfo.Next - 1);
        end else
          {MoveSW(Mem[BufferSeg:PixelInfo.Offs], Mem[SegA000 : PixelInfo.Offs + Modd shl 1], x);}
          asm
            push ds
            cld
            mov  si, PixelInfo.Offs
            mov  ax, SegA000
            mov  es, ax
            mov  ax, BufferSeg
            mov  ds, ax
            mov  di, si
            add  di, Modd
            mov  cx, x
            rep  movsw
            pop  ds
          end;
      end;
    end else begin
    {не проверено для карточек с банками по 8К и ниже - как у AT wonder/Paradise
    /старых Cirrusов
    предполагается что меньше чем по 4К карточек не существует.}
      J := BufGranula div Granularity;
      X := X2 - X1 + 1;
      for Y := Y1 to Y2 do begin
        GetPixelInfoBuf(X1, Y);
        GetPixelInfo(X1, Y);
        Modd := (CurrentBank mod J) * Granularity;
        if x > LongInt(PixelInfo.Next) + 1 then begin
          MoveSW(Mem[BufferSeg : PixelInfo.Offs + Modd], Mem[SegA000 : PixelInfo.Offs] , PixelInfo.Next + 1);
          Inc(Modd, Granularity);
          if (Modd and emsdQLD) = 0 then begin
            Modd := 0;
            SelectBankBuf(CurrentBankBuf + 1);
          end;
          SelectBank(CurrentBank + 1);
          MoveSW(Mem[BufferSeg : Modd], Mem[SegA000 : 0], x - PixelInfo.Next - 1);
        end else MoveSW(Mem[BufferSeg:PixelInfo.Offs + Modd], Mem[SegA000 : PixelInfo.Offs], x);
      end;
    end;
    SelectBankBuf(0);
    SelectBank(0);
  end;
End;

Function MapBMPLineRead;
Var
  NX, BP, I, J, K: Word;
  Data     : PByteArray;
  Color    : Word;
  PlanesPreCalc:array[0..3] of Word;
  CRef     : PLongArray absolute ColorRef;

Begin
  NX := PSImage(P)^.X;
  if ColorRef = nil then CRef := ColorIndex;
  case PSImage(P)^.NBP of
  imHiColor {native core bmp}:
     MapBMPLineRead := PByteArray(LongInt(P) + 8 + Y * NX shl 1);
  imHiColor + imStreamed : {native streamed bmp}  with PSImage(P)^.PS^ do begin
    Seek(LongInt(Y) * NX shl 1);
    Read(PSimage(P)^.Buffer^, NX shl 1);
    {$IFNDEF DPMI}
    if BuffersInEms and (EMSCurHandle <> EMSHandler) then PSelectBankBuf(CurrentBankBuf);
    {$ENDIF}
    MapBMPLineRead := PSimage(P)^.Buffer;
  end;
  im256 : {native 256 core bmp}begin
    Data := PByteArray(LongInt(P) + 8 + Y * NX);
    ExpandHi(Data, @ScrollMoveArea, NX, 0, CRef);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  im256 + imStreamed{native 256 streamed bmp}: with PSImage(P)^.PS^ do begin
    Seek(LongInt(Y) * NX);
    Read(PSimage(P)^.Buffer^, NX);
    {$IFNDEF DPMI}
    if BuffersInEms and (EMSCurHandle <> EMSHandler) then PSelectBankBuf(CurrentBankBuf);
    {$ENDIF}
    Data := PSImage(P)^.Buffer;
    ExpandHi(Data, Data, NX, 0, CRef);
    MapBMPLineRead := PSimage(P)^.Buffer;
  end;
  im16 {native 16-color core non-planar bmp}: begin
    BP   := (NX shr 1) + (NX and 1);
    Data := PByteArray(LongInt(P) + 8 + Y * BP);
    Expand16Hi(Data, @ScrollMoveArea, NX, 0, CRef);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  im16 + imStreamed{native 16-color streamed}: with PSImage(P)^.PS^ do begin
    BP   := (NX shr 1) + (NX and 1);
    Seek(LongInt(Y) * BP);
    Read(PSimage(P)^.Buffer^, BP);
    {$IFNDEF DPMI}
    if BuffersInEms and (EMSCurHandle <> EMSHandler) then PSelectBankBuf(CurrentBankBuf);
    {$ENDIF}
    Data := PSImage(P)^.Buffer;
    Expand16Hi(Data, Data, NX, 0, CRef);
    MapBMPLineRead := Data;
  end;
  imMono {native core monochrome}: begin
    BP   := (NX shr 3) + Byte((NX and 7) <> 0);
    Data := PByteArray(LongInt(P) + 8 + Y * BP);
    ExpandMonoHi(Data, @ScrollMoveArea, NX, 0, CRef);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  imMono + imStreamed{innative streamed monochrome}: with PSImage(P)^.PS^ do begin
    BP   := (NX shr 3) + Byte((NX and 7) <> 0);
    Seek(LongInt(Y) * BP);
    Read(PSimage(P)^.Buffer^, BP);
    {$IFNDEF DPMI}
    if BuffersInEms and (EMSCurHandle <> EMSHandler) then PSelectBankBuf(CurrentBankBuf);
    {$ENDIF}
    Data := PSImage(P)^.Buffer;
    ExpandMonoHi(Data, @ScrollMoveArea, NX, 0, CRef);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  imTC {true color core bmp}: begin
    Data := PByteArray(LongInt(P) + 8 + Y * NX shl 2);
    Impact32Hi(Data, @ScrollMoveArea, NX, 0);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  imTC + imStreamed : {true color  streamed bmp}  with PSImage(P)^.PS^ do begin
    Seek(LongInt(Y) * NX shl 2);
    Data := PSimage(P)^.Buffer;
    Read(Data^, NX shl 2);
    {$IFNDEF DPMI}
    if BuffersInEms and (EMSCurHandle <> EMSHandler) then PSelectBankBuf(CurrentBankBuf);
    {$ENDIF}
    Impact32Hi(Data, @ScrollMoveArea, NX, 0);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  imIcon, imCursor, imPlaned, imPlaned16 : {innative rudimental 4-planar 16-color} begin
    BP   := (NX shr 3) + Byte((NX and 7) <> 0);
    Data := PByteArray(LongInt(P) + 8 + Y * BP);
    for J := 0 to 3 do PlanesPreCalc[J] := BP * J;
    for J := 0 to NX - 1 do begin
      Color := 0;
      NX := J shr 3;
      K :=  1 shl (7 - J and 7);
      if (Data^[PlanesPreCalc[0] + NX] and K) <> 0 then Color := Color or 8;
      if (Data^[PlanesPreCalc[1] + NX] and K) <> 0 then Color := Color or 4;
      if (Data^[PlanesPreCalc[2] + NX] and K) <> 0 then Color := Color or 2;
      if (Data^[PlanesPreCalc[3] + NX] and K) <> 0 then Color := Color or 1;
      PWordArray(@ScrollMoveArea)^[J] := ColorIndex^[Color];
    end;
    MapBMPLineRead := @ScrollMoveArea;
  end;
  else MapBMPLineRead := Nil; end;
End;

{not tested for metafile}
Procedure StretchDIBitmap(Image, Dest : PImage; xxx, yyy, xFrom, yFrom,
          xLen, yLen, xDest, yDest : integer; ColorRef : PColorRef; ClipRect : TRect);
Var
  Data, DestData : PWordArray;
  NX, NY, NBP, I, J, MaxX, MaxY, xaDest, yaDest, NX2, NY2 : Word;
  LastX, LastY : Integer;
  StepXL, StepYL, CounterXL, CounterYL : LongInt;
  xiDest, yiDest : ShortInt;

  CXLW  : LongRec absolute CounterXL;
  CYLW  : LongRec absolute CounterYL;

  UseScreen : Boolean;
  MapBuffer : array[0..2047] of Word;
  Z : TRect;
  rxDest, rsDest, rzDest : Integer;
  PMap : PWordArray;


Procedure ImplodeX;
Var
  J, JM : Word;
Begin
  CounterXL := $10000;
  LastX    := 0;
  if xDest > 0 then JM := 0 else JM := xaDest - 1;
  for J := 0 to MaxX - 1 do begin
    if CXLW.Hi > LastX then begin
      MapBuffer[JM] := Data^[J];
      Inc(JM, xiDest);
      LastX := CXLW.Hi;
    end;
    Inc(CounterXL, StepXL);
  end;
  MapBuffer[JM] := Data^[J];
End;

Procedure ExplodeX;
Var
  Pixel : Word;
  J, JM : Integer;
Begin
  CounterXL := 0;
  LastX    := 0;
  if xDest > 0 then JM := 0 else JM := xFrom + xLen - 1;
  Pixel := Data^[0];
  for J := 0 to xaDest do begin
    if CXLW.Hi <= J then begin
      Pixel := Data^[JM];
      Inc(JM, xiDest);
      Inc(CounterXL, StepXL);
    end;
    MapBuffer[J] := Pixel;
  end;
  MapBuffer[J] := Pixel;
End;


Begin
  NY  := Word(Pointer(LongInt(Image) + 2)^);
  NBP := Word(Pointer(LongInt(Image) + 4)^);
  NX  := Word(Pointer(LongInt(Image) + 6)^);
  if (NY = 0) or (NX = 0) then Exit;
  if Dest <> Nil then begin
    NX2 := Word(Pointer(LongInt(Dest) + 6)^);
    NY2 := Word(Pointer(LongInt(Dest) + 2)^);
  end;
  Data := Pointer(LongInt(Image) + 8);
  if xFrom > NX then Exit;
  if yFrom > NY then Exit;
  MaxX := MinWord(xLen, NX - xFrom);
  MaxY := MinWord(yLen, NY - yFrom);
  xaDest := Abs(xDest); yaDest := Abs(yDest);
  if xDest < 0 then xiDest := -1 else xiDest := 1;
  if yDest < 0 then yiDest := -1 else yiDest := 1;

  if (MaxX = 0) or (MaxY = 0) then Exit;
  UseScreen := Dest = Nil;
  if UseScreen then Z.Assign(0, 0, ScreenDriver^.ScreenWidth, ScreenDriver^.ScreenHeight)
               else Z.Assign(0, 0, NX2, NY2);
  ClipRect.Intersect(Z);
  if ClipRect.Empty then Exit;
  Z.Assign(xxx, yyy, xxx + Abs(xDest), yyy + Abs(yDest));
  ClipRect.Intersect(Z);
  if ClipRect.Empty then Exit;
  rsDest := MaxInteger(xxx, ClipRect.A.X);
  rxDest := (MinInteger(ClipRect.B.X, xxx + Abs(xDest)) - rsDest);
  rzDest := rxDest * 2;
  if ClipRect.A.X > xxx then PMap := Pointer(LongInt(@MapBuffer) + (ClipRect.A.X - xxx) shl 1)
   else PMap := @MapBuffer;

  LastY    := 0;
  CounterYL := 0;
  if MaxX = 1 then StepXL := $10000 else
  StepXL := LongInt(Trunc(xaDest / (MaxX - 1)) * $10000) + Trunc(Frac(xaDest / (MaxX - 1)) * $10000);
  if MaxY = 1 then StepYL := $10000 else
  StepYL := LongInt(Trunc(yaDest / (MaxY - 1)) * $10000) + Trunc(Frac(yaDest / (MaxY - 1)) * $10000);

  if LongRec(StepYL).Hi = 0 then begin        { IMPLODE Y }
    Data := PWordArray(LongInt(MapBMPLineRead(Image, YFrom + 0, ColorRef)) + XFrom shl 1);
    if yDest > 0 then J := 0 else J := yaDest - 1;
    for I := 0 to MaxY - 1{?} do begin
      if CYLW.Hi > LastY then begin
        if ((yyy + J) >= ClipRect.A.Y) and ((yyy + J) < ClipRect.B.Y) then begin
          if LongRec(StepXL).Hi = 0 then ImplodeX else ExplodeX;
          if UseScreen then WriteScanLine(rsDest, yyy + J, rxDest, PMap) else
            MapBitLineWrite(Dest, yyy + J, NX2 shl 1, rsDest, rzDest, PMap);
        end else
          if ((yyy + j) >= ClipRect.B.Y) and (yiDest = 1)
           or((yyy + j) <  ClipRect.A.Y) and (yiDest = -1) then Break;
        Inc(J, yiDest);
        LastY := CYLW.Hi;
      end;
      Inc(CounterYL, StepYL);
      Data := PWordArray(LongInt(MapBMPLineRead(Image, YFrom + I, ColorRef)) + XFrom shl 1);
    end;
  end else begin                { EXPLODE Y }
    if yDest > 0 then J := 0 else J := yFrom + yLen - 1;
    Data := PWordArray(LongInt(MapBMPLineRead(Image, YFrom + J, ColorRef)) + XFrom shl 1);
    if LongRec(StepXL).Hi = 0 then ImplodeX else ExplodeX;
    for I := 0 to yaDest - 0{?} do begin
      if CYLW.Hi < LastY then begin
        if ((yyy + i + LongRec(StepYL).Hi) >= ClipRect.A.Y) and ((yyy + i - LongRec(StepYL).Hi) < ClipRect.B.Y) then begin
          Data := PWordArray(LongInt(MapBMPLineRead(Image, YFrom + J, ColorRef)) + XFrom shl 1);
          if (LongRec(StepXL).Hi = 0) then ImplodeX else ExplodeX;
        end;
        Inc(CounterYL, StepYL);
        Inc(J, yiDest);
      end;
      Inc(LastY);
      if ((yyy + i) >= ClipRect.A.Y) and ((yyy + i) < ClipRect.B.Y) then begin
        if UseScreen then WriteScanLine(rsDest, yyy + I, rxDest, PMap) else
          MapBitLineWrite(Dest, yyy + I, NX2 shl 1, rsDest, rzDest, PMap);
      end else
        if (yyy + i) >= ClipRect.B.Y then Break;
    end;
    if ((yyy + i) >= ClipRect.A.Y) and ((yyy + i) < ClipRect.B.Y) then begin
      if UseScreen then WriteScanLine(rsDest, yyy + I, rxDest, PMap) else
        MapBitLineWrite(Dest, yyy + I, NX2 shl 1, rsDest, rzDest, PMap);
      end;
  end;
End;

Procedure ImplantDIBitmap(Image,Dest:PImage;X,Y,xFrom,yFrom,xLen,yLen:Integer;Operation:Byte;ColorRef:PColorRef);
Label __Code;
Var
  NX, NY, NBP, NX2, NY2, I, J, K, L : Integer;
  Data, PMapBuffer : PWordArray;
  MapBuffer : array[0..2047] of Word;
Begin
  if Image = Nil then Exit;
  if (PSimage(Dest)^.NBP and imColor) <> DIBType then Exit;
  NY  := PSimage(Image)^.Y;
  NX  := PSimage(Image)^.X;
  NY2 := PSimage(Dest)^.Y;
  NX2 := PSimage(Dest)^.X;
  if (NY = 0) or (NX = 0) then Exit;
  if (NY2 = 0) or (NX2 = 0) then Exit;
  if (XFrom >= NX) or (YFrom >= NY){ or (Y + YFrom >= NY2) or (X + XFrom >= NX2)}then Exit;
  XLen := MinInteger(XLen, NX - XFrom);
  YLen := MinInteger(YLen, NY - YFrom);
  XFrom := MaxInteger(0, MinInteger(XFrom, NX - 1));
  YFrom := MaxInteger(0, MinInteger(YFrom, NY - 1));
  if (XLen = 0) or (YLen = 0) then Exit;
  if ColorRef = Nil then Pointer(ColorRef) := ColorIndex;
  PMapBuffer := @MapBuffer;

  __Before(Operation);asm mov ax, offset cs:__Code end;__After;
  K := Y;
  for I := yFrom to yFrom + yLen - 1 do begin
    if (K < 0) or (K >= NY2) then begin
      Inc(K);
      Continue;
    end;
    Data := PWordArray(MapBMPLineRead(Image, I, ColorRef));
    Move(Data^, MapBuffer, NX shl 1);
    Data := PWordArray(MapBitLineRead(Dest, K, NX2 shl 1));
    L := X;
    for J := xFrom to xFrom + xLen - 1 do begin
      if (L < 0) or (L >= NX2) or (J < 0) then begin
        Inc(L);
        Continue;
      end;
      asm
        les di, PMapBuffer
        add di, J
        add di, J
        mov dx, es:[di]
        les di, Data
        add di, L
        add di, L
      end;
      __Code: __NullDef;
      Inc(L);
    end;
    MapBitLineFlush(Dest, K, NX2 shl 1);
    Inc(K);
  end;
End;

Function SVGA64KInitialize : Boolean;
Begin
  @PDriverGPInfo    := @GetPixelInfo;
  @PDriverGPInfoBuf := @GetPixelInfoBuf;
  SVGA64KInitialize := False;
  if not VESAInitialize then Exit;
  SVGA64KInitialize := True;
  GetVGAPalette(0, 256, PMainPalette);
  DIBAdjSelect;
End;

Procedure SVGA64KFInitialize;
Begin
  VESAFInitialize;
End;

Procedure SetOutput(OnBuffer : Boolean);
Begin
  VESA.SetOutPut(OnBuffer);
End;

Procedure PrepareDrawing;
Begin
  VESA.PrepareDrawing;
End;

Procedure EMSAdjSelect;
Begin
  VESA.EMSAdjSelect;
End;


Procedure SetUserBitBltProc(Action : Pointer);
Var
  Code : Record
    Call : Byte;
    Rec  : Pointer;
    NOP  : Word;
  End;
Begin
  if Action <> Nil then begin
    Code.Call := $9A; {call far}
    Code.Rec  := Action;
    Code.NOP  := $9090;
    Move(Code, _CodeBraks[UserBitBlt], 7);
  end else FillChar(_CodeBraks[UserBitBlt], 7, $90);
End;

Var
  ColorTransparent : Word;

Procedure ColorTransparentBitBlt; Far; Assembler; Asm
  cmp dx, ColorTransparent
  je  @@1
  mov es:[di], dx
@@1:
End;

Procedure ColorOpaqueBitBlt; Far; Assembler; Asm
  cmp dx, ColorTransparent
  jne  @@1
  mov es:[di], dx
@@1:
End;


Procedure SetColorBitBlt(Color : LongInt; Transparent : Boolean);
Begin
  ColorTransparent := Color;
  if Transparent then
    SetUserBitBltProc(@ColorTransparentBitBlt)
  else
    SetUserBitBltProc(@ColorOpaqueBitBlt);
End;

Procedure CopyRight; Assembler;Asm
  ret
  db "DK Inc. 1996  SVGA HiColor low-level library"
End;


begin
  asm mov di, offset CopyRight end;
  {$IFDEF DPMI}
  {это для того чтобы прописывать код не вызывая exception 13.}
  asm
    mov bx, cs
    mov ax, 0Ah
    int 31h
    mov CodeAlias, ax
  end;
  {$ENDIF}
  RegisterDriver(@Vesa64K_640x480ScreenDriver); {1st as default}
  RegisterDriver(@Vesa64K_320x200ScreenDriver);
  RegisterDriver(@Vesa64K_800x600ScreenDriver);
  RegisterDriver(@Vesa64K_1024x768ScreenDriver);
  RegisterDriver(@Vesa64K_1280x1024ScreenDriver);
end.
