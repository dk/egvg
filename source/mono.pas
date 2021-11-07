{$G+,S-,F+}
{$IFDEF DPMI}
{$C FIXED PRELOAD PERMANENT}
{$ENDIF}
Unit Mono;

Interface

Uses Objects, Memory, EGInline, GDI, DIB;


Function  MonoInitialize : Boolean;
Procedure MonoFInitialize;

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
  ax - word to be modified, dx - masked bitstroke, cx - destination mask}
Procedure SetUserBitBltProc(Action : Pointer);
Procedure SetColorBitBlt(Color : LongInt; Transparent : Boolean);

Procedure DIBAdjSelect;

Const
  VideoSeg      : Word = $A000;


Implementation

Uses VESA;

Var
  MemBuffer     : Pointer;


Function MonoPrimEnterGraphics(Mode, BPS : Word) : Boolean;
Label Ex;
Begin
  MonoPrimEnterGraphics := False;
  if (Mode <= $10) and (VideoCard < EGA) then Exit;
  if (Mode = $11)  and (VideoCard < VGA) then Exit;
  asm
    mov    ax, Mode
    int    10h
    mov    ah, 0fh
    int    10h
    cmp    byte ptr Mode, al
    jne    Ex
  end;
  MonoPrimEnterGraphics := True;
  with ScreenDriver^ do begin
    Granularity := $10000;
    MaxBanks := 1;
    EMSBanks := 1;
    BitPlaneSize := BPS;
  end;
  VideoSeg := SegA000;
Ex:
End;

function Mono_640x480EnterGraphics: boolean; far;
  begin  Mono_640x480EnterGraphics := MonoPrimEnterGraphics($0011, $9600); end;
function Mono_800x600EnterGraphics: boolean; far;
  begin
    Mono_800x600EnterGraphics := VesaEnterGraphics($102);
    VideoSeg := SegA000;
    ScreenDriver^.BitPlaneSize := $EA60
  end;
function Mono_640x350EnterGraphics: boolean; far;
  begin  Mono_640x350EnterGraphics := MonoPrimEnterGraphics($0010, $6D60); end;
function Mono_640x200EnterGraphics: boolean; far;
  begin  Mono_640x200EnterGraphics := MonoPrimEnterGraphics($000e, $3E80); end;
function Mono_320x200EnterGraphics: boolean; far;
  begin  Mono_320x200EnterGraphics := MonoPrimEnterGraphics($000d, $1F40); end;

procedure StandardLeaveGraphics; assembler;
asm
  mov    ax, 0003h
  int    10h
end;

{$IFDEF DPMI}
Var
  CodeAlias  : Word;
{$ENDIF}


Procedure DIBAdjSelect;
Var
  I, J : Word;
Begin
  for I := 0 to 255 do begin
    J := PMainPalette^[I, 1];
    Inc(J, PMainPalette^[I, 2]);
    Inc(J, PMainPalette^[I, 3]);
    if J > 280 then J := 1 else J := 0;
    ColorIndex^[I] := J;
  end;
  DIBType := imMono;
  MaxColors := 2;
{  FillChar(MainPalette[0], 3, $00);
  FillChar(MainPalette[1], 3, $FF);}
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
    _Init            : MonoInitialize;
    _Done            : MonoFInitialize;
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
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 640;
    ScreenHeight       : 480;
    MaximalX           : 639;
    MaximalY           : 479;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Mono_640x480EnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $12
  );
  Mono_800x600ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 800;
    ScreenHeight       : 600;
    MaximalX           : 799;
    MaximalY           : 599;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Mono_800x600EnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $10
  );

  Mono_640x350ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 640;
    ScreenHeight       : 350;
    MaximalX           : 639;
    MaximalY           : 349;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Mono_640x350EnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $10
  );
  Mono_320x200ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 320;
    ScreenHeight       : 200;
    MaximalX           : 319;
    MaximalY           : 199;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Mono_320x200EnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $0d
  );
  StandardCGAScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssFlat;
    ScreenWidth        : 640;
    ScreenHeight       : 200;
    MaximalX           : 639;
    MaximalY           : 199;
    NumberOfColors     : 2;
    ColorShift         : 1;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Mono_640x200EnterGraphics;
    LeaveGraphics      : StandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $0e
  );


{
  ax = y
  bx = x
}
Function GetPixelAddr : Word; Near; Assembler; Asm
  mov     di, ax
  mov     dx, BufferSeg
  mov     es, dx           {ES:BX = byte address of pixel}
  mul     LineLength
  mov     dl,bl           {DL = low-order byte of x}
  shr     bx,3            {BX = x/8}
  add     bx,ax           {BX = y*BytesPerLine + x/8}
  and     dl,7            {CL = x & 7}
  xor     dl,7            {DL = number of bits to shift left}
  mov     di,bx
  mov     ax,bx
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

Function GetPixelWidth(Width : Word) : Word; Assembler;
Asm
  mov ax, Width
  mov bx, ax
  shr ax, 3
  and bx, 7
  jz @@1
  inc ax
@@1:
End;


Procedure SetPixel(X, Y : Word; Color : LongInt);Assembler; Asm
   mov     ax, Y
   mov     bx, X
   call    GetPixelAddr
   mov     cl, dl
   mov     ah, 1
   shl     ah, cl
   mov     si, bx
   mov     al, ah
   not     al
   test    byte ptr [Color], 1
   jnz     @@Set
@@Clear:
   and     es:[di], al
   jmp     @@1
@@Set:
   or      es:[di], ah
@@1:
End;

Function GetPixel(X, Y : Word) : LongInt; Assembler; Asm
  mov     ax, Y
  mov     bx, X
  call    GetPixelAddr
  mov     al, es:[di]
  mov     ah, 1
  mov     cl, dl
  shl     ah, cl
  and     al, ah
  shr     al, cl
  xor     ah, ah
  xor     dx, dx
End;

Function GetPixelBM;
Var
  I : Byte;
  DX : Word;
Begin
  DX := PSimage(Device)^.X;
  I := MapBitLineRead(Device, Y, (DX shr 3) + Byte((DX and 7) <> 0))^[X shr 3];
  GetPixelBM := I shr (X and 7) and 1;
End;


Procedure HLine(X, Y, X2 : Word; Color : LongInt);Assembler; Asm
  cld
  mov     ax, BufferSeg
  mov     es, ax
  mov     ax, Y
  mov     bx, X
  mov     cl, bl
  and     cl, 7
  xor     cl, 7
  call    GetPixelAddr
  mov     ah, 1

  mov     dh, ah
  not     dh
  mov     dl, 0FFh
  shl     dh, cl
  not     dh
  mov     cx, X2
  and     cl, 7
  xor     cl, 7
  shl     dl, cl
  mov     ax, X2
  mov     bx, X
  shr     ax, 3
  shr     bx, 3
  mov     cx, ax
  sub     cx, bx
  xor     bx, bx
  xor     ah, ah
  mov     al, 1
  test    byte ptr [Color], al
  mov     al, ah
  jz      @@1
  not     al
@@1:
  or      dh, dh
  js      @@43
  or      cx, cx
  jnz     @@42
  and     dl, dh
  jmp     @@44
@@42:
   mov    ah, al
   and    ah, dh
   not    dh
   and    es:[di], dh
   or     es:[di], ah
   inc    di
   dec    cx
@@43:
   shr     cx, 1
   pushf
   jz      @@OneByte
   mov     ah, al
   rep     stosw
@@OneByte:
   popf
   jnc     @@NoOneByte
   stosb
@@NoOneByte:
@@44:
   and  al, dl
   not  dl
   and  es:[di], dl
   or   es:[di], al
End;

Procedure VLine(X, Y, Y2 : Word; Color : LongInt); Assembler; Asm
   cld
   mov  ax, BufferSeg
   mov  es, ax
   mov  ax, Y
   mov  bx, X
   mov  cl, bl
   and  cl, 7
   xor  cl, 7
   call GetPixelAddr
   mov  ah, 1
   shl  ah, cl
   mov  al, ah
   not  al
   mov  cx, Y2
   sub  cx, Y
   inc  cx
   mov  bx, LineLength
   test byte ptr [Color], 1
   jnz  @@Set
@@Clear:
   and  es:[di], al
   add  di, bx
   loop @@Clear
   jmp        @@Exit
@@Set:
   or   es:[di], ah
   add  di, bx
   loop @@Set
@@Exit:
End;

Procedure DisplayXxYClip(x, y: integer; BitMap: pointer; Msk : Byte; Width, Height : Word; Clr : LongInt);
Assembler;
Var
  Local : Word;
Asm
   cld
   mov  ax, BufferSeg
   mov  es, ax
   dec  Width
   mov  ax, LineLength
   mov  Local, ax
   mov  ax, Y
   mov  bx, X
   mul  Local
   mov  dl, bl
   shr  bx, 3
   add  bx, ax
   mov  di, bx
   and  dl, 7
   jz   @@ByteAligned

@@NotByteAligned:
   mov  cl, byte ptr [Width]
   xor  cl, 7
   mov  bx, 0FFh
   shl  bl, cl
   mov  cl, dl
   xor  cl, 7
   inc  cl
   shl  bx, cl
   mov  dx, bx
   not  dx
   push ds
   lds  si, BitMap
   test byte ptr [Clr], 1
   jnz  @@NBASet
@@NBAClr:
   lodsb
{*}and  al, Msk
   xor  ah, ah
   not  al
   shl  ax, cl
   or   ax, dx
   xchg al, ah
   and  es:[di], ax
   add  di, Local
   dec  Height
   jnz  @@NBAClr
   jmp  @@3
@@NBASet:
   lodsb
{*}and  al, Msk
   shl  ax, cl
   and  ax, bx
   xchg al, ah
   or   es:[di], ax
   add  di, Local
   dec  Height
   jnz  @@NBASet
   jmp @@3

@@ByteAligned:
   mov  cl, byte ptr [Width]
   xor  cl, 7
   mov  bl, 0FFh
   shl  bl, cl
   mov  bh, bl
   not  bh
   mov  cx, Height
   push ds
   lds  si, BitMap
   test byte ptr [Clr], 1
   jnz  @@BASet

@@BAClr:
   lodsb
{*}and   al, Msk
   not   al
   or    al, bh
   and   [es:di], al
   add   di, Local
   loop  @@BAClr
   jmp   @@3
@@BASet:
   lodsb
{*}and   al, Msk
   and   al, bl
   or    es:[di], al
   add   di, Local
   loop  @@BASet
@@3:
   pop   ds
End;

Procedure DisplayXxY(x, y: integer; BitMap: pointer;Width, Height : Word; Clr : LongInt);
Begin
  DisplayXxYClip(x, y, Bitmap, $FF, Width, Height, Clr);
End;

procedure QuickRestore(xxx, yyy, xLen, yLen: integer);
Var
  L, LL, I, O : Word;
Begin
  O  := GetPixelInfo(xxx, yyy);
  LL := (xLen shr 3) + 1;
  L := 0;
  for I := 1 to yLen do begin
    Move(QuickSaveArea[L], Mem[VideoSeg:O], LL);
    Inc(O, LineLength);
    Inc(L, LL);
  end;
End;

procedure QuickSave(xxx, yyy, xLen, yLen: integer);
Var
  L, LL, I, O : Word;
Begin
  O  := GetPixelInfo(xxx, yyy);
  LL := (xLen shr 3) + 1;
  L := 0;
  for I := 1 to yLen do begin
    Move(Mem[VideoSeg:O], QuickSaveArea[L], LL);
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
  SetOutput(False);
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
  SetOutput(True);
End;


Const
  _CodeEdges : array[0..21, 0..7] of Byte = (
    ($23, $C1, $0B, $D0, $90, $90, $90, $90),  {and ax, cx}
                                               {or  dx, ax}
    ($33, $D0, $90, $90, $90, $90, $90, $90),  {xor dx, ax}
    ($0B, $D1, $23, $D0, $90, $90, $90, $90),  {or  dx, cx}
                                               {and dx, ax}
    ($0B, $D0, $90, $90, $90, $90, $90, $90),  {or  dx, ax}
    ($0B, $D1, $F7, $D2, $23, $C1, $0B, $D0),  {or  dx, cx}
                                               {not dx    }
                                               {and ax, cx}
                                               {or  dx, ax}
    ($0B, $D0, $90, $90, $90, $90, $90, $90),  {or  dx, ax}
    ($F7, $D1, $33, $C1, $33, $D0, $90, $90),  {not cx}
                                               {xor ax, cx}
                                               {xor dx, ax}
    ($0B, $D1, $F7, $D1, $33, $C1, $23, $D0),  {or  dx, cx}
                                               {not cx}
                                               {xor ax, cx}
                                               {and dx, ax}
    ($F7, $D1, $33, $C1, $0B, $D0, $90, $90),  {not cx}
                                               {xor ax, cx}
                                               {or  dx, ax}
    ($0B, $D1, $F7, $D2, $33, $D0, $90, $90),  {or  dx, cx}
                                               {not dx}
                                               {xor dx, ax}
    ($0B, $D1, $F7, $D2, $0B, $D1, $23, $D0),  {or  dx, cx}
                                               {not dx}
                                               {and dx, ax}
    ($0B, $D1, $F7, $D2, $0B, $D0, $90, $90),  {or  dx, cx}
                                               {not dx}
                                               {or  dx, ax}
    ($33, $D0, $F7, $D1, $33, $D1, $90, $90),  {xor dx, ax}
                                               {not cx}
                                               {xor dx, cx}
    ($0B, $D1, $23, $D0, $F7, $D1, $33, $D1),  {or  dx, cx}
                                               {and dx, ax}
                                               {not cx}
                                               {xor dx, cx}
    ($0B, $D0, $F7, $D0, $F7, $D1, $33, $D1),  {or  dx, ax}
                                               {not cx}
                                               {xor dx, cx}
    ($33, $D0, $90, $90, $90, $90, $90, $90),  {xor dx, ax}
    ($90, $90, $90, $90, $90, $90, $90, $90),  {no operation}
    ($0B, $D0, $90, $90, $90, $90, $90, $90),  {or  dx, ax}
    ($90, $90, $90, $90, $90, $90, $90, $90),  {no operation}
    ($23, $C1, $8B, $D0, $90, $90, $90, $90),  {and ax, cx}
                                               {mov dx, ax}
    ($F7, $D1, $0B, $C1, $8B, $D0, $90, $90),  {not cx}
                                               {or ax, cx}
                                               {mov dx, ax}
    ($90, $90, $90, $90, $90, $90, $90, $90)   {no operation, for user}

  );

  Masks : array[0..16] of Word =
     ($0000,
     $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
     $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF);


Procedure __BeforeEdge(Operation : Byte);
                     Inline($BE/_CodeEdges/       {mov si, offset _CodeBraks}
                            $31/$C0/              {xor ax, ax   }
                            $B0/$08/              {mov al, 8    }
                            $5B/                  {pop bx}
                            $F6/$E3/              {mul bl}
                            $03/$F0);             {add si, ax   }
                           {cannot be inline}     {mov ax, offset cs:label}
Procedure __AfterEdge;   Inline({$IFDEF DPMI}
                            $8B/$3E/CodeAlias/    {mov di, CodeAlias}
                            {$ELSE}
                            $8C/$CF/              {mov di, cs   }
                            {$ENDIF}
                            $8E/$C7/              {mov es, di   }
                            $89/$C7/              {mov di, ax   }
                            $B9/$08/$00/          {mov cx, 8    }
                            $FC/                  {cld}
                            $F3/$A4);             {rep movsb    }

Procedure __EdgeDef; Inline($90/$90/$90/$90/$90/$90/$90/$90);
Procedure __EdgeGet; Inline($26/$8B/$05/          {mov ax, es:[di]}
                            $23/$D1/              {and dx, cx}
                            $F7/$D1/              {not cx}
                            $86/$F2/              {xchg dl, dh}
                            $86/$E9);             {xchg cl, ch}


Procedure __EdgeSet; Inline($26/$89/$15);                 {mov es:[di], dx}


Function GetWordStroke(Buffer : Pointer; From, Count : Word) : Word; Assembler;
Var
  LD : Byte;
Asm
  mov bx, From
@@0:
  cmp bx, Count
  jl @@Start
  sub bx, Count
  jmp @@0
@@Start:
  mov From, bx
  mov dx, From
  mov cx, dx
  shr dx, 3
  and cx, 7
  les di, Buffer
  mov si, di
  add di, dx
  mov ax, es:[di]
  rol ax, cl
  add bx, 8
  cmp bx, Count
  jbe @@Exit1
  mov dl, $FF
  shl dl, cl
  and al, dl
  mov ch, 8
  sub ch, cl
  xchg cl, ch
  mov bl, es:[si]
  shr bl, cl
  xchg cl, ch
  or al, bl
@@Exit1:
  mov LD, al
  add From, 8
  mov bx, From
  cmp bx, Count
  jl @@1
  sub bx, Count
@@1:
  mov dx, bx
  mov cx, dx
  shr dx, 3
  and cx, 7
  mov di, word ptr [Buffer]
  mov si, di
  add di, dx
  mov ax, es:[di]
  rol ax, cl
  add bx, 8
  cmp bx, Count
  jbe @@Exit2
  mov dl, $FF
  shl dl, cl
  and al, dl
  mov ch, 8
  sub ch, cl
  xchg cl, ch
  mov bl, es:[si]
  shr bl, cl
  or al, bl
@@Exit2:
  mov ah, LD
End;


Procedure SetPixelOp(X, Y : Word; Color : LongInt; Operation : Byte; Device : PImage);
Label __Code;
Var
  Data : PbyteArray;
  BPL  : Word;
Begin
  __BeforeEdge(Operation);
  asm mov ax, offset cs:__Code end;
  __AfterEdge;
  if Device <> Nil then begin
    if (X >= PSimage(Device)^.X) or (Y >= PSimage(Device)^.Y) then Exit;
    BPL := BPline(Device);
    Data := MapBitLineRead(Device, Y, BPL);
    asm
      les di, Data
      mov ax, x
      shr ax, 3
      add di, ax
    end;
  end else GetPixelInfo(X, Y);
  asm
    mov cx, x
    and cx, 7
    inc cx
    ror dx, cl
    mov bx, 1
    ror bx, cl
    mov cx, bx
    mov dx, word ptr Color
  end;
  __EdgeGet;
  __Code : __EdgeDef;
  __EdgeSet;
  if Device <> Nil then MapBitLineFlush(Device, Y, BPL);
End;


procedure HLineStyleOp;
Label
  __Code, __EdgeLeft, __EdgeRight;
Var
  Fore, Back            : Byte;
  Pattern, ANDer, D, I: Word;
  Blocked, Mapped       : Boolean;
  Color                 : Byte;

  NBP, NX, NY, ADX, ADY : Word;
  Data, DevCp, WPtr     : PByteArray;
  WData                 : PWordArray;
  DecrX, BPL            : Integer;
  W1, W2, Mask          : Word;

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
    Mapped := False;
    Fore := T.Fore; Back := T.Back;
  end;
  lsPattern : begin
    Blocked := True;
    Back := T.Pattern[Y and 7];
    Pattern := Word(Back) shl 8 + Back;
    ANDer   := 7;
    Fore := T.Fore; Back := T.Back;
    Mapped := False;
  end;
  lsBitMap8x8 : begin
    if T.Bitmap = Nil then Exit;
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    ADX := 8;
    ADY := 8;
    Data  := MapBMPLineRead(T.BitMap, Word(Y) mod ADY, T.ColorRef);
    DecrX := T.BitMapOrg.X;
    Mapped := True;
  end;
  lsBitmap : begin
    if T.Bitmap = Nil then Exit;
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    ADX := NX;
    ADY := NY;
    Data  := MapBMPLineRead(T.BitMap, Word(Y) mod ADY, T.ColorRef);
    DecrX := 0;
    Mapped := True;
  end;
  lsBitmapOrg : begin
    if T.Bitmap = Nil then Exit;
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
    While (T.BitmapOrg.Y > 0) do Dec(T.BitmapOrg.Y, NY);
    While (T.BitmapOrg.X > 0) do Dec(T.BitmapOrg.X, NX);
    Data  := MapBMPLineRead(T.BitMap, Word(Y - T.BitMapOrg.Y) mod NY, T.ColorRef);
    ADY := $FFFF;
    ADX := NX;
    DecrX := - T.BitMapOrg.X;
    Mapped := True;
  end;
  end;

  if T.Device <> Nil then begin
    if (X1 >= PSimage(T.Device)^.X) or (Y >= PSimage(T.Device)^.Y) then Exit;
    X2 := MinInteger(X2, PSimage(T.Device)^.X - 1);
    BPL := BPLine(T.Device);
    DevCp := MapBitLineRead(T.Device, Y, BPL);
    WPtr := DevCp;
  end else WPtr := Ptr(BufferSeg, GetPixelInfo(0, Y));
  {decrypt pattern because Fore & Back are 1-bit significant here}
  if not Mapped then asm
    mov dx, Pattern
    mov cx, 16
    mov al, byte Ptr [Fore]
    mov ah, byte Ptr [Back]
  @@1:
    rcl dx, 1
    jnc @@2
    mov bl, al
    jmp @@3
  @@2:
    mov bl, ah
  @@3:
    rcr bl, 1
    rcl si, 1
    loop @@1
    mov Pattern, si
  end;
  W1   := X1 shr 4;
  W2   := X2 shr 4;

  {left edge}
  I := X1 and 15;
  D := (X2 + 1) and 15;
  if D = 0 then D := 16;
  if (I <> 0) or ((I = 0) and (W1 = W2)) then begin
    __BeforeEdge(T.Operation); asm mov ax, offset cs:__EdgeLeft end; __AfterEdge;
    Mask := Masks[16 - I];
    if W1 = W2 then Mask := Mask and not Masks[16 - D];
    if Mapped then Pattern := GetWordStroke(Data, W1 shl 4 + DecrX, ADX);
    asm
      les di, WPtr
      mov cx, Mask
      mov ax, W1
      shl ax, 1
      add di, ax
      mov dx, Pattern
    end;
    __EdgeGet;
    __EdgeLeft : __EdgeDef;
    __EdgeSet;
  end;
  {right edge}
  if (D <> 0) and (W1 <> W2) then begin
    __BeforeEdge(T.Operation); asm mov ax, offset cs:__EdgeRight end; __AfterEdge;
    Mask := not Masks[16 - D];
    if Mapped then Pattern := Swap(GetWordStroke(Data, W2 shl 4 + DecrX, ADX));
    asm
      les di, WPtr
      mov cx, Mask
      mov ax, W2
      shl ax, 1
      add di, ax
      mov dx, Pattern
      xchg dl, dh
    end;
    __EdgeGet;
    __EdgeRight : __EdgeDef;
    __EdgeSet;
  end;
  {center}
  if I <> 0 then Inc(W1);
  if W2 > W1 then begin
    if D = 0 then Inc(W2);
    __BeforeEdge(T.Operation); asm mov ax, offset cs:__Code end; __AfterEdge;
    for Mask := W1 to W2 - 1 do begin
      if Mapped then Pattern := GetWordStroke(Data, Mask shl 4 + DecrX, ADX);
      asm
        les di, WPtr
        mov ax, Mask
        shl ax, 1
        add di, ax
        mov dx, Pattern
        xor cx, cx
        dec cx
      end;
      __EdgeGet;
      __Code:__EdgeDef;
      __EdgeSet;
    end;
  end;
  if T.Device <> Nil then MapBitLineFlush(T.Device, Y, BPL);
End;

Procedure DisplayXx8Op(x, y: integer; BitMap: pointer; Msk : Byte; Height : Word; var T : TPaintInfo);
Label
   __Edge;
Var
  Fore, Back            : Byte;
  Pattern, ANDer, I, J  : Word;
  Blocked, Mapped       : Boolean;
  Color, NN             : Byte;

  NBP, NX, NY, ADX, ADY : Word;
  Data, DevCp, WPtr     : PByteArray;
  DecrX                 : Integer;
  W, Mask,  AMsk, BPL   : Word;

Begin
  if (T.LineStyle in [lsBitmap8x8..lsBitmapOrg]) and (T.Bitmap <> Nil) then begin
    NX := Word(Pointer(LongInt(T.BitMap) + 6)^);
    NY := Word(Pointer(LongInt(T.BitMap) + 2)^);
    if (NX = 0) or (NY = 0) then Exit;
  end;
  case T.LineStyle of
  lsLinePattern : begin
    Blocked := False;
    Pattern := T.LinePattern;
    asm
      mov cl, byte ptr x
      and cl, 15
      ror Pattern, cl
    end;
    ANDer   := 15;
    Mapped  := False;
  end;
  lsPattern : begin
    Blocked := True;
    Pattern := T.LinePattern;
    ANDer   := 7;
    Mapped  := False;
  end;
  lsBitMap8x8 : begin
    ADX := 8;
    ADY := 8;
    DecrX := X + T.BitMapOrg.X;
    Mapped  := True;
  end;
  lsBitmap: begin
    ADX := NX;
    ADY := NY;
    DecrX := X;
    Mapped  := True;
  end;
  lsBitmapOrg : begin
    While (T.BitmapOrg.Y > 0) do Dec(T.BitmapOrg.Y, NY);
    While (T.BitmapOrg.X > 0) do Dec(T.BitmapOrg.X, NX);
    ADY := $FFFF;
    ADX := NX;
    DecrX := X - T.BitMapOrg.X;
    Mapped  := True;
  end;
  end;
  Fore := T.Fore; Back := T.Back;

  if T.Device <> Nil then begin
    W := 8;
    if (X >= PSimage(T.Device)^.X) or (Y >= PSimage(T.Device)^.Y) then Exit;
    if X + W > PSimage(T.Device)^.X then W := PSimage(T.Device)^.X - X;
    if (Height = 0) or (W = 0) then Exit;
    BPL := BPline(T.Device);
  end;

  if Height > 0 then for J := Y to Y + Height - 1 do begin
    AMsk := Msk and PByteArray(Bitmap)^[J - Y];
    if AMsk = 0 then Continue;
    if T.Device <> Nil then begin
      DevCp := MapBitLineRead(T.Device, J, BPL);
      WPtr  := DevCp;
    end else WPtr := Ptr(BufferSeg, GetPixelInfo(0, J));

    {decrypt pattern because Fore & Back are 1-bit significant here}
    if Blocked then begin
      NN := T.Pattern[J and 7];
      Pattern := Word(NN) shl 8 + NN;
    end else Pattern := T.LinePattern;
    if Mapped then begin
      case T.LineStyle of
      lsBitMap8x8, lsBitmap :
        Data := MapBMPLineRead(T.BitMap, Word(J) mod ADY, T.ColorRef);
      lsBitmapOrg :
        Data := MapBMPLineRead(T.BitMap, Word(J - T.BitMapOrg.Y) mod NY, T.ColorRef);
      else end;
      Pattern := GetWordStroke(Data, DecrX, ADX);
    end;
    asm
      mov dx, Pattern
      mov cx, 16
      mov al, byte Ptr [Fore]
      mov ah, byte Ptr [Back]
    @@1:
      rcl dx, 1
      jnc @@2
      mov bl, al
      jmp @@3
    @@2:
      mov bl, ah
    @@3:
      rcr bl, 1
      rcl si, 1
      loop @@1
      mov Pattern, si
      mov cx, x
      and cl, 7
      mov ax, AMsk
      xchg al, ah
      ror ax, cl {?}
      mov AMsk, ax
    end;
    W := X shr 3;
    I := X and 7;
    __BeforeEdge(T.Operation); asm mov ax, offset cs:__Edge end; __AfterEdge;
    Mask := Masks[16 - I] and not Masks[8 - I] and AMsk;
    asm
      les di, WPtr
      mov cx, Mask
      add di, W
      mov dx, Pattern
    end;
    __EdgeGet;
    __Edge : __EdgeDef;
    __EdgeSet;
    if T.Device <> Nil then MapBitLineFlush(T.Device, J, BPL);
  end;
End;

procedure VLineStyleT(x, y1, y2, Width: integer; Clr: LongInt; Pattern: word);
Var
  I, J : Word;
Begin
  SetOutput(False);
  if (X and 1) = 0 then asm not Pattern end;
  for I := Y1 to Y2 do begin
    for J := 0 to Width - 1 do if ((Word(1) shl J) and Pattern) <> 0 then
      SetPixel(x + J, I, GetPixel (x + J, I) xor Clr);
    asm not Pattern end;
  end;
  SetOutput(True);
End;

Procedure  DirectGetImage(x1, y1, x2, y2 : integer; var BitMap);
var
  I, Y : Word;
  xLen : Word;
Begin
  Y := 0;
  TCoordImage(Bitmap).X := x2 - x1 + 1;
  TCoordImage(Bitmap).Y := y2 - y1 + 1;
  xLen := GetPixelWidth(x2 - x1 + 1);
  for I := y1 to y2 do begin
    Move(Mem[VideoSeg : GetPixelInfo(X1, I)], TCoordImage(Bitmap).Buffer[Y], xLen);
    Inc(Y, xLen);
  end;
End;

procedure DirectPutImage(Image: PCoordImage; x, y : integer);
var
  I, YY, XX : Word;
Begin
  YY := 0;
  XX := GetPixelWidth(Image^.X);
  for I := y to y + Image^.Y - 1 do begin
    Move(Image^.Buffer[YY], Mem[VideoSeg : GetPixelInfo(X, I)], xx);
    Inc(YY, xx);
  end;
End;

Procedure WriteScanLine(X, Y, XLen : Word; Buffer : Pointer);
Var
  I, D, W1, W2, YShl, Mask : Word;
  ShlDef : Byte;
  IsRight : Boolean;
Begin
  W1   := X shr 3;
  W2   := (XLen + X) shr 3;
  YShl := GetPixelInfo(0, Y);
  I := X and 7;
  D := (XLen + X) and 7;
  {left margin}
  if (I <> 0) or ((I = 0) and (W1 = W2)) then begin
    Mask := Masks[8 - I];
    if W1 = W2 then Mask := Mask and not Masks[8 - D];
    asm
      mov ax, BufferSeg
      mov es, ax
      push ds
      lds si, Buffer
      mov cx, I
      mov di, W1
      add di, YShl
      mov dx, ds:[si]
      ror dx, cl
      xchg dh, dl
      mov cx, Mask
      xchg ch, cl
      pop ds
    end;
    __EdgeGet;
    asm
      and ax, cx
      or  dx, ax
    end;
    __EdgeSet;
  end;

  IsRight := (D <> 0) and (W1 <> W2);
  ShlDef := 0;
  if I <> 0 then begin
    Inc(W1);
    ShlDef := 8 - (x and 7);
  end;

  {right margin}
  if IsRight then begin
    Mask := not Masks[8 - D];
    asm
      mov ax, BufferSeg
      mov es, ax
      push ds
      lds si, Buffer
      mov ax, W2
      sub ax, W1
      add si, ax
      mov cx, Mask
      xchg ch, cl
      and cx, $FF00
      mov di, W2
      add di, YShl
      mov dx, ds:[si]
      mov bl, shlDef
      xchg bx, cx
      rol dx, cl
      xchg dh, dl
      xchg bx, cx
      pop ds
    end;
    __EdgeGet;
    asm
      and ax, cx
      or  dx, ax
    end;
    __EdgeSet;
  end;

  {center}
  D := 0;
  if W2 > W1 then asm
    push ds
    mov ax, BufferSeg
    mov es, ax
    mov di, YShl
    add di, W1
    lds si, Buffer
    mov cx, W2
    sub cx, W1
    mov bl, ShlDef
  @@1:
    xchg bx, cx
    lodsw
    dec si
    rol ax, cl
    xchg bx, cx
    stosb
    loop @@1
    pop ds
  end;
End;

Procedure WriteScanLineOpPtr(X, Y, XLen : Word; Buffer, Dest : Pointer; BitBlt : Byte);
Label __EdgeLeft, __EdgeRight, __EdgeCode, __1;
Var
  I, D, W1, W2, Mask : Word;
  ShlDef : Byte;
  IsRight : Boolean;
Begin
  W1   := X shr 3;
  W2   := (XLen + X) shr 3;
  {YShl := GetPixelInfo(0, Y);}
  I := X and 7;
  D := (XLen + X) and 7;
  {left margin}
  if (I <> 0) or ((I = 0) and (W1 = W2)) then begin
    __BeforeEdge(BitBlt);
    asm mov ax, offset cs:__EdgeLeft end;
    __AfterEdge;
    Mask := Masks[8 - I];
    if W1 = W2 then Mask := Mask and not Masks[8 - D];
    asm
      {mov ax, BufferSeg
      mov es, ax}
      les di, Dest
      push ds
      lds si, Buffer
      mov cx, I
     { mdd di, YShl}
      add di, W1
      mov dx, ds:[si]
      ror dx, cl
      xchg dh, dl
      mov cx, Mask
      xchg ch, cl
      pop ds
    end;
    __EdgeGet;
    __EdgeLeft : __EdgeDef;
    __EdgeSet;
  end;

  IsRight := (D <> 0) and (W1 <> W2);
  ShlDef := 0;
  if I <> 0 then begin
    Inc(W1);
    ShlDef := 8 - (x and 7);
  end;

  {right margin}
  if IsRight then begin
    __BeforeEdge(BitBlt);
    asm mov ax, offset cs:__EdgeRight end;
    __AfterEdge;
    Mask := not Masks[8 - D];
    asm
     { mov ax, BufferSeg
      mov es, ax}
      les di, Dest
      push ds
      lds si, Buffer
      mov ax, W2
      sub ax, W1
      add si, ax
      mov cx, Mask
      xchg ch, cl
      and cx, $FF00
      {mov di, YShl}
      add di, W2
      mov dx, ds:[si]
      mov bl, shlDef
      xchg bx, cx
      rol dx, cl
      xchg dh, dl
      xchg bx, cx
      pop ds
    end;
    __EdgeGet;
    __EdgeRight : __EdgeDef;
    __EdgeSet;
  end;

  {center}
  D := 0;
  if W2 > W1 then begin
    __BeforeEdge(BitBlt); asm mov ax, offset cs:__EdgeCode end; __AfterEdge;
    asm
      push ds
      les di, Dest
      {mov ax, BufferSeg
      mov es, ax}
      add di, W1
      {add di, YShl}
      lds si, Buffer
      mov cx, W2
      sub cx, W1
      mov bl, ShlDef
    __1:
      xchg bx, cx
      lodsw
      dec si
      mov dx, ax
      rol dx, cl
      push cx
      mov cx, $FF00
      xchg dh, dl
    end;
    __EdgeGet;
    __EdgeCode:__EdgeDef;
    __EdgeSet;
    asm
      pop cx
      xchg bx, cx
      inc di
      loop __1
      pop ds
    end;
  end;
End;

Procedure WriteScanLineOp(X, Y, XLen : Word; Buffer : Pointer; BitBlt : Byte);
Begin
  WriteScanLineOpPtr(X, Y, XLen, Buffer, Ptr(BufferSeg, GetPixelInfo(0, Y)), BitBlt);
End;

Procedure ReadScanLine(X, Y, XLen : Word; Buffer : Pointer); Assembler;
Asm
    mov ax, y
    mov bx, x
    call GetPixelAddr
    push ds
    mov ax, es
    mov ds, ax
    mov si, di
    les di, Buffer
    mov cx, XLen
    mov bx, cx
    shr cx, 3
    and bx, 7
    jz @@1
    inc cx
  @@1:
    cld
    test x, 7
    jnz @@NotAligned

    rep movsb
    jmp @@Exit
@@NotAligned:
    xor dh, dh
    xor dl, 7
@@2:
    xchg dx, cx
    lodsw
    dec si
    xchg ah, al
    shl  ax, cl
    mov  al, ah
    stosb
    xchg dx, cx
    loop @@2
@@Exit:
    pop ds
End;


Procedure PutBMPPartOp(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer;
                       Operation: Byte; ColorRef : PColorRef);
Var
  Data : PByteArray;
  NX, NY, NBP, BPLin : Word;
  I, MaxX, MaxY : Word;
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
   imMono:
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        RightMonoAdj(Data, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
    im16 :
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Impact16Mono(Data, @ScrollMoveArea, NX, 0, (yyy + I) and 7, Nil);
        RightMonoAdj(@ScrollMoveArea, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
   im256 :
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Impact256Mono(Data, @ScrollMoveArea, NX, 0, (yyy + I) and 7, Nil);
        RightMonoAdj(@ScrollMoveArea, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
    imTC :
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        ImpactTCMono(Data, @ScrollMoveArea, NX, 0, (yyy + I) and 7);
        RightMonoAdj(@ScrollMoveArea, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLineOp(xxx, yyy + I, MaxX, @ScrollMoveArea, Operation);
      end;
  else end;
End;

Procedure PutBMPPart(Image: PImage; xxx, yyy, xFrom, yFrom, xLen, yLen: integer);
Var
  Data : PByteArray;
  NX, NY, NBP, BPLin : Word;
  I, MaxX, MaxY : Word;
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
   imMono:begin
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        RightMonoAdj(Data, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
    end;
   im16 :
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Impact16Mono(Data, @ScrollMoveArea, NX, 0, (yyy + I) and 7, Nil);
        RightMonoAdj(@ScrollMoveArea, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
   im256 :
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        Impact256Mono(Data, @ScrollMoveArea, NX, 0, (yyy + I) and 7, Nil);
        RightMonoAdj(@ScrollMoveArea, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
   imTC :
      for I := 0 to MaxY - 1 do begin
        Data := MapBitLineRead(Image, yFrom + I, BPLin);
        ImpactTCMono(Data, @ScrollMoveArea, NX, 0, (yyy + I) and 7);
        RightMonoAdj(@ScrollMoveArea, @ScrollMoveArea, xFrom, MaxX);
        WriteScanLine(xxx, yyy + I, MaxX, @ScrollMoveArea);
      end;
  else end;
End;

procedure PutBufferPart(x1, y1, x2, y2: integer);
Var
  Y, W1, W2, CD : Integer;
  WPtr          : Pointer;
Begin
  if not BufferedStrategy then Exit;
  if X2 > MaximalX then X2 := MaximalX;
  if Y2 > MaximalY then Y2 := MaximalY;
  W1 := x1 shr 3;
  W2 := x2 shr 3;
  CD := W2 - W1 + 1;
  for Y := y1 to y2 do asm
    mov ax, x1
    push ax
    mov ax, y
    push ax
    call GetPixelInfo
    push ds
    cld
    mov cx, CD
    mov ax, VideoSeg
    mov bx, es
    mov ds, bx
    mov es, ax
    mov ax, es
    mov si, di
    rep movsb
    pop ds
  end;
End;

Function  MapBMPLineRead(P : PImage; Y : Word; ColorRef : PColorRef) : PByteArray;
Var
  NX   : Word;
  Data : Pointer;
Begin
  NX  := PSImage(P)^.X;
  case PSImage(P)^.NBP and imColor of
  imMono :
    MapBMPLineRead := MapBitLineRead(P, Y, NX shr 3 + Byte((NX and 7) <> 0));
  im16 : begin
    Data := MapBitLineRead(P, Y, (NX shr 1) + (NX and 1));
    Impact16Mono(Data, @ScrollMoveArea, NX, 0, Y and 7, Nil);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  im256 : begin
    Data := MapBitLineRead(P, Y, NX);
    Impact256Mono(Data, @ScrollMoveArea, NX, 0, Y and 7, Nil);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  imPlaned, imPlaned16, imIcon, imCursor : begin
    Data := MapBitLineRead(P, Y, (NX shr 1) + (NX and 1));
    ExpandPlanedTo17(Data, @ScrollMoveArea, NX, 0);
    Impact16Mono(@ScrollMoveArea, @ScrollMoveArea, NX, 0, Y and 7, Nil);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  imTC : begin
    Data := MapBitLineRead(P, Y, NX shl 2);
    ImpactTCMono(Data, @ScrollMoveArea, NX, 0, Y and 7);
    MapBMPLineRead := @ScrollMoveArea;
  end;
  end;
End;


Procedure StretchDIBitmap(Image, Dest : PImage; xxx, yyy, xFrom, yFrom,
          xLen, yLen, xDest, yDest : integer; ColorRef : PColorRef; ClipRect : TRect);
{страшно медленный и глюкавый - а все равно не нужен никому :(}
Var
  Data, DestData : PByteArray;
  NX, NY, NBP, I, J, MaxX, MaxY, xaDest, yaDest, NX2, NY2, BPL2 : Word;
  LastX, LastY : Integer;
  StepXL, StepYL, CounterXL, CounterYL : LongInt;
  xiDest, yiDest : ShortInt;

  CXLW  : LongRec absolute CounterXL;
  CYLW  : LongRec absolute CounterYL;

  UseScreen : Boolean;
  MapBuffer : array[0..2047] of Byte;
  Z : TRect;
  rxDest, rsDest : Integer;
  PMap : PByteArray;
  WPtr : Pointer;


Procedure ImplodeX;
Var
  Pixel, XC, XD : Byte;
  J, JM : Word;
  LData : Pointer;
Begin
  CounterXL := $10000;
  LastX    := 0;
  if xDest > 0 then begin
    JM := 0;
    XC := Data^[0];
  end else begin
    JM := xaDest - 1;
    XC := Data^[JM shr 3];
  end;
  for J := 0 to MaxX - 1 do begin
    if (Succ(J) and 7) = 0 then XC := Data^[Succ(J) shr 3];
    if CXLW.Hi > LastX then begin
      if xiDest > 0 then begin
        asm
          rcl XC, 1
          rcl XD, 1
        end;
        if (Succ(JM) and 7) = 0 then MapBuffer[JM shr 3] := XD;
      end else begin
        asm
          rcl XC, 1
          rcr XD, 1
        end;
        if (JM and 7) = 0 then MapBuffer[JM shr 3] := XD;
      end;
      Inc(JM, xiDest);
      LastX := CXLW.Hi;
    end else asm shl XC, 1 end;
    Inc(CounterXL, StepXL);
  end;
  if (Succ(J) and 7) = 0 then XC := Data^[Succ(J) shr 3];
  asm
    rcl XC, 1
    rcl XD, 1
    mov cl, 8
    mov ch, byte ptr JM
    and ch, 7
    sub cl, 8
    shl XD, 1
  end;
  MapBuffer[JM shr 3] := XD;
  if ClipRect.A.X > xxx then RightMonoAdj(@MapBuffer, @MapBuffer, ClipRect.A.X - xxx, rxDest);
End;

Procedure ExplodeX;
Var
  Pixel, XC, XD, SDef : Byte;
  J, JM, JX : Word;
  LData : Pointer;
Begin
  CounterXL := 0;
  LData := Data;
  if xDest > 0 then begin
    JM := 0;
    XC := Data^[0];
    asm
      mov al, XC
      rcl al, 1
      rcl Pixel, 1
    end
  end else begin
    JM := xFrom + xLen - 1;
    JX := 0;
    SDef := JM and 7;
    asm
      mov ax, jm
      shr ax, 3
      dec ax
      dec ax
      les di, LData
      add di, ax
      mov ax, es:[di]
      mov cl, SDef
      xchg al, ah
      rol ax, cl
      mov XC, ah
    end;
    asm
      rcr XC, 1
      rcl Pixel, 1
    end;
  end;
  for J := 0 to xaDest do begin
    if CXLW.Hi <= J then begin
      if xiDest > 0 then begin
        if (Succ(JM) and 7) = 0 then XC := Data^[Succ(JM) shr 3];
        asm
          rcl XC, 1
          rcl Pixel, 1
        end
      end else begin
        if (Succ(JX) and 7) = 0 then {XC := Data^[Pred(JM) shr 3];} asm
          les di, LData
          mov ax, JM
          shr ax, 3
          dec ax
          dec ax
          add di, ax
          mov ax, es:[di]
          mov cl, SDef
          xchg al, ah
          rol ax, cl
          mov XC, ah
        end;
        asm
          rcr XC, 1
          rcl Pixel, 1
        end;
        Inc(JX);
      end;
      Inc(JM, xiDest);
      Inc(CounterXL, StepXL);
    end;
    asm
      mov al, Pixel
      rcr al, 1
      rcl XD, 1
    end;
    if (Succ(J) and 7) = 0 then MapBuffer[J shr 3] := XD;
  end;
  if xiDest > 0 then asm
    mov cl, 8
    mov al, Pixel
    rcr al, 1
    rcl XD, 1
    mov ch, byte ptr J
    and ch, 7
    sub cl, ch
    shl XD, cl
  end else asm
    mov cl, 8
    mov al, Pixel
    rcr al, 1
    rcl XD, 1
    mov ch, byte ptr J
    and ch, 7
    sub cl, ch
    shl XD, cl
  end;
  MapBuffer[J shr 3] := XD;
  if ClipRect.A.X > xxx then RightMonoAdj(@MapBuffer, @MapBuffer, ClipRect.A.X - xxx, rxDest);
End;


Begin
  NY  := Word(Pointer(LongInt(Image) + 2)^);
  NBP := Word(Pointer(LongInt(Image) + 4)^);
  NX  := Word(Pointer(LongInt(Image) + 6)^);
  if (NY = 0) or (NX = 0) then Exit;
  if Dest <> Nil then begin
    NX2 := Word(Pointer(LongInt(Dest) + 6)^);
    NY2 := Word(Pointer(LongInt(Dest) + 2)^);
    BPL2 := BPline(Dest);
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
  rxDest := MinInteger(ClipRect.B.X, xxx + Abs(xDest)) - rsDest;
  PMap := @MapBuffer;

  LastY    := 0;
  CounterYL := 0;
  if MaxX = 1 then StepXL := $10000 else
  StepXL := LongInt(Trunc(xaDest / (MaxX - 1)) * $10000) + Trunc(Frac(xaDest / (MaxX - 1)) * $10000);
  if MaxY = 1 then StepYL := $10000 else
  StepYL := LongInt(Trunc(yaDest / (MaxY - 1)) * $10000) + Trunc(Frac(yaDest / (MaxY - 1)) * $10000);

  if LongRec(StepYL).Hi = 0 then begin        { IMPLODE Y }
    Data := MapBMPLineRead(Image, YFrom, ColorRef);
    RightMonoAdj(Data, @ScrollMoveArea, xFrom, NX);
    Data := @ScrollMoveArea;
    if yDest > 0 then J := 0 else J := yaDest - 1;
    for I := 0 to MaxY - 1 do begin
      if CYLW.Hi > LastY then begin
        if ((yyy + J) >= ClipRect.A.Y) and ((yyy + J) < ClipRect.B.Y) then begin
          if LongRec(StepXL).Hi = 0 then ImplodeX else ExplodeX;
          if UseScreen then WriteScanLine(rsDest, yyy + J, rxDest, PMap) else begin
            WPtr := MapBitLineRead(Dest, yyy + J, BPL2);
            WriteScanLineOpPtr(rsDest, yyy + J, rxDest, PMap, WPtr, CopyPut);
            MapBitLineFlush(Dest, yyy + J, BPL2);
          end;
        end else
          if ((yyy + j) >= ClipRect.B.Y) and (yiDest = 1)
           or((yyy + j) <  ClipRect.A.Y) and (yiDest = -1) then Break;
        Inc(J, yiDest);
        LastY := CYLW.Hi;
      end;
      Inc(CounterYL, StepYL);
      Data := MapBMPLineRead(Image, YFrom + I, ColorRef);
      RightMonoAdj(Data, @ScrollMoveArea, xFrom, NX);
      Data := @ScrollMoveArea;
    end;
  end else begin                { EXPLODE Y }
    if yDest > 0 then J := 0 else J := yFrom + yLen - 1;
    Data := MapBMPLineRead(Image, YFrom + J, ColorRef);
    RightMonoAdj(Data, @ScrollMoveArea, xFrom, NX);
    Data := @ScrollMoveArea;
    if LongRec(StepXL).Hi = 0 then ImplodeX else ExplodeX;
    for I := 0 to yaDest - 0{?} do begin
      if CYLW.Hi < LastY then begin
        if ((yyy + i + LongRec(StepYL).Hi) >= ClipRect.A.Y) and ((yyy + i - LongRec(StepYL).Hi) < ClipRect.B.Y) then begin
          Data := MapBMPLineRead(Image, YFrom + J, ColorRef);
          RightMonoAdj(Data, @ScrollMoveArea, xFrom, NX);
          Data := @ScrollMoveArea;
          if (LongRec(StepXL).Hi = 0) then ImplodeX else ExplodeX;
        end;
        Inc(CounterYL, StepYL);
        Inc(J, yiDest);
      end;
      Inc(LastY);
      if ((yyy + i) >= ClipRect.A.Y) and ((yyy + i) < ClipRect.B.Y) then begin
        if UseScreen then WriteScanLine(rsDest, yyy + I, rxDest, PMap) else begin
          WPtr := MapBitLineRead(Dest, yyy + I, BPL2);
          WriteScanLineOpPtr(rsDest, yyy + I, rxDest, PMap, WPtr, CopyPut);
          MapBitLineFlush(Dest, yyy + I, BPL2);
        end;
      end else
        if (yyy + i) >= ClipRect.B.Y then Break;
    end;
    if ((yyy + i) >= ClipRect.A.Y) and ((yyy + i) < ClipRect.B.Y) then begin
      if UseScreen then WriteScanLine(rsDest, yyy + I, rxDest, PMap) else begin
        WPtr := MapBitLineRead(Dest, yyy + I, BPL2);
        WriteScanLineOpPtr(rsDest, yyy + I, rxDest, PMap, WPtr, CopyPut);
        MapBitLineFlush(Dest, yyy + I, BPL2);
      end;
    end;
  end;
End;


Procedure ImplantDIBitmap(Image,Dest:PImage;X,Y,xFrom,yFrom,xLen,yLen:Integer;Operation:Byte;ColorRef:PColorRef);
Var
  NX, NY, NBP, NX2, NY2, I, J, K : Integer;
  Data : PByteArray;
  MapBuffer : array[0..2047] of Byte;
Begin
  if Image = Nil then Exit;
  if (Word(Pointer(LongInt(Dest) + 4)^) and imColor) <> imMono then Exit;
  NY  := Word(Pointer(LongInt(Image) + 2)^);
  NX  := Word(Pointer(LongInt(Image) + 6)^);
  NY2 := Word(Pointer(LongInt(Dest) + 2)^);
  NX2 := Word(Pointer(LongInt(Dest) + 6)^);
  if (NY = 0) or (NX = 0) then Exit;
  if (NY2 = 0) or (NX2 = 0) then Exit;
  if (XFrom >= NX) or (YFrom >= NY) then Exit;
  XLen := MinInteger(XLen, NX - XFrom);
  YLen := MinInteger(YLen, NY - YFrom);
  if (XLen = 0) or (YLen = 0) then Exit;
  if ColorRef = Nil then ColorRef := @StdColorRefMap;
  NX2 := BPLine(Dest);

  K := Y;
  for I := yFrom to yFrom + yLen - 1 do begin
    if (K < 0) or (K >= NY2) then begin
      Inc(K);
      Continue;
    end;
    Data := MapBMPLineRead(Image, I, ColorRef);
    Move(Data^, MapBuffer, NX);
    Data := MapBitLineRead(Dest, K, NX2);
    RightMonoAdj(@MapBuffer, @MapBuffer, xFrom, xLen);
    WriteScanLineOpPtr(X, I, xLen, @MapBuffer, Data, Operation);
    MapBitLineFlush(Dest, K, NX2);
    Inc(K);
  end;
End;

Function  MemInit : Boolean;
Begin
  MemBuffer := MemAllocSeg(LongInt(ScreenDriver^.ScreenWidth) * ScreenDriver^.ScreenHeight div 8);
  MemInit := MemBuffer <> Nil;
End;

Procedure MemDone;
Begin
  if MemBuffer <> Nil then FreeMem(MemBuffer, LongInt(ScreenDriver^.ScreenWidth) * ScreenDriver^.ScreenHeight div 8);
End;


Function MonoInitialize : Boolean;
Begin
  MonoInitialize := False;
  if not ScreenDriver^.EnterGraphics then Exit;
  MonoInitialize := True;
  Move(@StdVGAPalette^, PMainPalette^, 768);
  DIBAdjSelect;
  MaximalX := ScreenDriver^.MaximalX;
  MaximalY := ScreenDriver^.MaximalY;
  LineLength := (ScreenDriver^.MaximalX + 1) div 8;
  MemBuffer := Nil;
  if TryBufferedStrategy then BufferedStrategy := MemInit else
    BufferedStrategy := False;
  BufferSeg := VideoSeg;
  SetOutput(BufferedStrategy);
End;

Procedure MonoFInitialize;
Begin
  if BufferedStrategy then begin
    MemDone;
    BufferedStrategy := False;
  end;
  ScreenDriver^.LeaveGraphics;
End;

var
  SaveExit: Pointer;

procedure EmergencyFree; far;
begin
  ExitProc := SaveExit;
  if BufferedStrategy then begin
    MemDone;
    BufferedStrategy := False;
  end;
end;

Procedure SetOutput(OnBuffer : Boolean);
Const
  AtBuffer : Boolean = True;
Begin
  if not(BufferedStrategy) or (OnBuffer = AtBuffer) then Exit;
  if OnBuffer then BufferSeg := Seg(MemBuffer^) else BufferSeg := VideoSeg;
  AtBuffer := OnBuffer;
End;

Procedure PrepareDrawing; {Don't needs here}
Begin
End;

Procedure EMSAdjSelect; {Don't needs here}
Begin
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
    Move(Code, _CodeEdges[UserBitBlt], 7);
  end else FillChar(_CodeEdges[UserBitBlt], 7, $90);
End;

Procedure SetColorBitBlt(Color : LongInt; Transparent : Boolean);
Var
  Op : Byte;
Begin
  if Color = 0 then
    if Transparent then Op := OrPut else Op := AndPut
  else
    if Transparent then Op := AndPut else Op := OrPut;
  Move(_CodeEdges[Op], _CodeEdges[UserBitBlt], 7);
End;

Procedure CopyRight; Assembler;Asm
  ret
  db "DK Inc. 1996  Monochrome low-level library"
End;


Begin
  asm mov di, offset CopyRight end;
  VideoSeg := SegA000;
  {$IFDEF DPMI}
  {это для того чтобы прописывать код не вызывая exception 13.}
  asm
    mov bx, cs
    mov ax, 0Ah
    int 31h
    mov CodeAlias, ax
  end;
  {$ENDIF}
  RegisterDriver(@StandardVGAScreenDriver); {1st as default}
  RegisterDriver(@Mono_800x600ScreenDriver);
  RegisterDriver(@Mono_640x350ScreenDriver);
  RegisterDriver(@Mono_320x200ScreenDriver);
  RegisterDriver(@StandardCGAScreenDriver);
End.