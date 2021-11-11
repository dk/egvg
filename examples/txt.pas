{$F+,I-,S-,R-,G+}
Unit Txt;

Interface

Uses Objects, Memory, EGInline, GDI, DIB, Mono, VESA;


Function  TxtInitialize : Boolean;
Procedure TxtFInitialize;

Procedure PutBufferPart(x1, y1, x2, y2:Integer);
Procedure SetTextRect(x, y, width, height : Byte);
Procedure SetPrimaryWidth(NewWidth : Byte);
Procedure FadeTxt;

var
  TextRect : TRect;

Implementation


Var
  MemBuffer : PByteArray;

Function  MemInit : Boolean;
Begin
  MemBuffer := MemAllocSeg(LongInt(ScreenDriver^.ScreenWidth) * ScreenDriver^.ScreenHeight div 8);
  MemInit := MemBuffer <> Nil;
End;

Procedure MemDone;
Begin
  if MemBuffer <> Nil then FreeMem(MemBuffer, LongInt(ScreenDriver^.ScreenWidth) * ScreenDriver^.ScreenHeight div 8);
End;


Procedure SetOutput(OnBuffer : Boolean);
Begin
  BufferSeg := Seg(MemBuffer^);
End;


var
  ScanLines, TxtX, TxtY : Byte;


Function TxtInitialize : Boolean;
Begin
  TxtInitialize := False;
  if VideoCard < EGA then Exit;
  if not ScreenDriver^.EnterGraphics then Exit;
  TxtInitialize := True;
  Move(@StdVGAPalette^, PMainPalette^, 768);
  DIBAdjSelect;
  MaximalX := ScreenDriver^.MaximalX;
  MaximalY := ScreenDriver^.MaximalY;
  LineLength := (ScreenDriver^.MaximalX + 1) div 8;
  MemBuffer := Nil;
  MemInit;
  BufferedStrategy := True;
  SetOutput(True);
  TxtX :=  ScreenDriver^.MaximalX div 8 + 1;
  TxtY :=  ScreenDriver^.MaximalY div 16 + 1;
  TextRect.Assign(0, 0, TxtX, TxtY);
  FillChar(MemBuffer^, LongInt(ScreenDriver^.ScreenWidth) * ScreenDriver^.ScreenHeight div 8, 0);
  VideoSeg := BufferSeg;
End;

Procedure TxtFInitialize;
Begin
  MemDone;
  BufferedStrategy := False;
  ScreenDriver^.LeaveGraphics;
End;


Procedure SetAdapter; Assembler; Asm
        MOV     dx,     03C4h
        out     dx,al
        MOV     al,     ah
        INC     dx
        out     dx,al
End;

Procedure SetGraphController; Assembler; Asm
        MOV     dx,     03CEh
        out     dx,al
        MOV     al,     ah
        INC     dx
        out     dx,al
End;


Procedure OpenEgaOutput; Assembler; Asm
        MOV     ax,     0402H
        CALL    SetAdapter
        MOV     ax,     0704H
        CALL    SetAdapter
        MOV     ax,     0204H
        CALL    SetGraphController
        MOV     ax,     1005H
        cmp     VideoCard, EGA
        jnz     @@1
        MOV     ax,     0005H
@@1:    CALL    SetGraphController
        MOV     ax,     0006H
        CALL    SetGraphController
End;

Procedure CloseEgaOutput; Assembler; Asm
        MOV     ax,     0302H
        CALL    SetAdapter
        MOV     ax,     0304H
        CALL    SetAdapter
        MOV     ax,     1005H
        CALL    SetGraphController
        MOV     ax,     0E06H
        CALL    SetGraphController
        MOV     ax,     0004H
        CALL    SetGraphController
End;


Procedure FadeTxt; Assembler; Asm
  cld
  mov ax, SegB800
  mov es, ax
  mov cx, 5000
  xor di, di
@@1:
  and word ptr es:[di], 1111011111111111b
  inc di
  inc di
  loop @@1
End;



Function  SetTxtVGA : Boolean; far; Assembler; Asm
  mov ax, 1103h
  mov bl, 32
  int 10h
  mov al, 1
  mov ScanLines, 16
  cmp VideoCard, EGA
  jnz @@1
  mov ScanLines, 14
@@1:
  mov ax, Seg0040
  mov es, ax
  mov al, es:[84h]
  cmp al, 24
  jle @@2
  mov ScanLines, 8
@@2:
  mov ax, 1003h
  mov bl, 1
  int 10h
End;


Procedure FreeTxt; far; Assembler; Asm
  mov ax, 1003h
  mov bl, 0
  int 10h
  MOV   ax,     0003H
  CALL  SetAdapter
End;


Procedure CopySymbols; Assembler; Asm
  call OpenEGAOutput
  push ds
  mov ax, SegA000
  mov es, ax
  mov di, 2000h
  mov bx, LineLength
  mov dl, ScanLines
  xor dh, dh
  dec bx
  lds si, memBuffer
  cld
  mov cx, 256

@@1:
  push cx
  push si
  mov cx, dx

@@2:
  lodsb
  stosb
  add si, bx
  loop @@2

  pop si
  pop cx

  inc si
  add di, 32
  sub di, dx

  mov ax, seg @data
  mov ds, ax
  mov ax, cx
  div TxtX
  dec ah
  jnz @@3
  mov ax, bx
  inc ax
  mul dl
  sub ax, bx
  dec ax
  add si, ax
@@3:
  mov ax, word ptr [MemBuffer+2]
  mov ds, ax

  loop @@1
  call CloseEGAOutput
  pop ds
End;


Procedure SetTextRect(x, y, width, height : Byte);
var
  I, J : Integer;
Begin
  x := MinInteger(80, x);
  y := MinInteger(25, y);
  width := MinInteger(128, width);
  height :=  MinInteger(128, height);
  TextRect.Assign(x, y, x + width, y + height);
  for J := y to y + Height - 1 do
    for I := x to x + Width - 1 do
      MemW[SegB800:(J shl 1) * 80 + (I shl 1)] :=
        (I - x) + (j - y) * Width + $0f00;
End;


Procedure SetPrimaryWidth(NewWidth : Byte);
Begin
  TxtX := newWidth;
  TxtY := 256 div TxtX;
  LineLength := newWidth;
  MaximalX := txtx * 8 - 1;
  MaximalY := txtx * ScanLines - 1;
  ScreenDriver^.Maximalx := maximalx;
  ScreenDriver^.Maximaly := maximaly;
End;
Function GetPixelInfo(X, Y : Word) : Word; Near; Assembler; Asm
  mov     ax, BufferSeg
  mov     es,ax           {ES:BX = byte address of pixel}
  push    dx
  mov     ax, y
  mov     bx, x
  mul     LineLength
  pop     dx              {AX = y*BytesPerLine}
  mov     dl,bl           {DL = low-order byte of x}
  shr     bx,3            {BX = x/8}
  add     bx,ax           {BX = y*BytesPerLine + x/8}
  and     dl,7            {CL = x & 7}
  xor     dl,7            {DL = number of bits to shift left}
  mov     di,bx
  mov     ax,bx
End;


procedure QuickRestore(xxx, yyy, xLen, yLen: integer);
Var
  L, LL, I, O : Word;
Begin
  O  := GetPixelInfo(xxx, yyy);
  LL := (xLen shr 3) + 1;
  L := 0;
  for I := 1 to yLen do begin
    Move(QuickSaveArea[L], Mem[BufferSeg:O], LL);
    Inc(O, LineLength);
    Inc(L, LL);
  end;
  CopySymbols;
End;

procedure QuickSave(xxx, yyy, xLen, yLen: integer);
Var
  L, LL, I, O : Word;
Begin
  O  := GetPixelInfo(xxx, yyy);
  LL := (xLen shr 3) + 1;
  L := 0;
  for I := 1 to yLen do begin
    Move(Mem[BufferSeg:O], QuickSaveArea[L], LL);
    Inc(O, LineLength);
    Inc(L, LL);
  end;
End;

procedure DirectMousePut(var Buf; x, y, xFrom, yFrom, Width, Height: integer);
Var
  I, J, K : Word;
  Ands, Xors : PLongArray;
  Z : LongInt;

Begin
  Ands := Pointer(LongInt(@Buf) + 6);
  Xors := Pointer(LongInt(Ands) + 128);
  Dec(Width,  xFrom);
  Dec(Height, yFrom);
  for I := Y to Y + Height - 1 do begin
    for J := 0 to Width - 1 do begin
      Z := LongInt(1) shl (31 - LongReverse[J + xFrom]);
      K := I - Y + yFrom;
      case Byte(((Ands^[K]) and Z) <> 0) shl 1 + Byte(((Xors^[K]) and Z) <> 0) of
      0 : SetPixel(x + J, I, 0);
      1 : SetPixel(x + J, I, 1);
      3 : SetPixel(x + J, I, not GetPixel(x + J, I));
      else end;
    end;
  end;
  CopySymbols;
End;


Procedure PutBufferPart;
Begin
{  WriteLn(txtx);
  WriteLn(txty);}
  CopySymbols;
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
    _Init            : txtInitialize;
    _Done            : txtFInitialize;
    _StretchDIBitmap : StretchDIBitmap;
    _ImplantDIBitmap : ImplantDIBitmap;
    _SetOutput       : SetOutput;
    _PrepareDrawing  : PrepareDrawing;
    _SetUserBitBltProc:SetUserBitBltProc;
    _SetColorBitBlt  : SetColorBitBlt;
    _EMSAdjSelect    : EMSAdjSelect;
    _DIBAdjSelect    : DIBAdjSelect
  );
  TxtScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 256;
    ScreenHeight       : 128;
    MaximalX           : 255;
    MaximalY           : 127;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : SetTxtVGA;
    LeaveGraphics      : FreeTxt;
    Methods            : @DriverMethods;
    ID                 : $3
  );
  Txt64ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 512;
    ScreenHeight       : 64;
    MaximalX           : 511;
    MaximalY           : 63;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : SetTxtVGA;
    LeaveGraphics      : FreeTxt;
    Methods            : @DriverMethods;
    ID                 : $301
  );



Begin
  RegisterDriver(@TxtScreenDriver); {1st as default}
  RegisterDriver(@Txt64ScreenDriver);
End.