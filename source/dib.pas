{$G+,S-,F+,D+}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : Device Indepenedent Bitmaps                          █
  █ Description : Bitmaps Handling & Bitstroke Conversions             █
  █ Author      : Dmitry Karasik                                       █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 26-DEC-1996                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█

}

Unit DIB;

Interface

Uses Objects, GDI;

{bitstroke conversion}
Procedure ExpandMono(Source, Dest : Pointer; Count, From : Word; ColorRef : PByteArray);   {1->256}
Procedure ExpandMonoHi(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); {1->64}
Procedure ExpandMonoTC(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); {1->32}
Procedure RightMonoAdj(Source, Dest : Pointer; From, Count : Word);
Procedure Impact16Mono(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte; Palette:PVGAPalette); {16->1 dithered}
Procedure Impact17Planed(Source, Dest : PByteArray; Count : Word);                         {16->4}
Procedure Expand16(Source, Dest : Pointer; Count, From : Word; ColorRef : PByteArray);     {16->256}
Procedure Expand16Hi(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray);   {16->64}
Procedure Expand16TC(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray);   {16->32}
Procedure Remap16(Map : PByteArray; Count : Word; ColorRef : PColorRef);
Procedure ImpactMono(Source, Dest : Pointer; Count, From : Word; ColorRef : PColorRef);    {256->1}
Procedure Impact256Mono(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte;Palette:PVGAPalette);{256->1 dithered}
Procedure Impact256Planed(Source, Dest : PByteArray; Count : Word);                        {256->4}
Procedure Impact16(Source, Dest : Pointer; Count, From : Word; ColorRef : PColorRef);      {256->16}
Procedure ExpandHi(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray);     {256->64}
Procedure ExpandTC(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray);     {256->32}
Procedure Remap256(Map : PByteArray; Count : Word; ColorRef : PColorRef);
Procedure ExpandHiTrue(Source, Dest : PByteArray; Count, From : Word);                     {64->24}
Procedure ExpandHi32(Source, Dest : PByteArray; Count, From : Word);                       {64->32}
Procedure PrepareExpandPlaned(XLen : Word; ReversedPlanes : Boolean);
Procedure ExpandPlanedTo17(Source, Dest : PByteArray; Count, From : Word);                 {4->16}
Procedure ExpandPlaned(Source, Dest : Pointer; Count, From : Word);                        {4->256}
Procedure ConvertPlaned24(Source, Dest : Pointer; Count, From : Word);                     {4->24}
Procedure ImpactTrueColor(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte);{24->256 dithered}
Procedure ReverseRGB(Map : Pointer; Count : Word);                                         {24->24}
Procedure ExpandTrue32(Source, Dest : Pointer; Count, From : Word);                        {24->32}
Procedure ImpactTrueHi(Source, Dest : PByteArray; Count, From : Word);                     {24->64}
Procedure ImpactTCMono(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte);   {32->1 dithered}
Procedure ImpactTrue256(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte);{32->256 dithered}
Procedure ImpactTrue32(Source, Dest : PByteArray; Count, From : Word);                     {32->24}
Procedure Impact32Hi(Source, Dest : PByteArray; Count, From : Word);                       {32->64}


Implementation


Var
  PlanesPreCalc        : array[0..3] of Word;         {internal}

Procedure ExpandTrue32(Source, Dest : Pointer; Count, From : Word); Assembler; Asm
  push ds
  les di, Dest
  lds si, Source
  mov ax, Count
  mov cx, From
  add cx, Count
  shl ax, 2
  dec di
  add di, ax
  add si, cx
  add si, cx
  add si, cx
  dec si
  mov cx, Count
  std
@@1:
  lodsb
  stosb
  stosb
  lodsb
  stosb
  lodsb
  stosb
  loop @@1
  cld
  pop ds
End;

Procedure ImpactTrue32(Source, Dest : PByteArray; Count, From : Word); Assembler; Asm
  push ds
  les di, Dest
  lds si, Source
  mov cx, Count
  mov ax, From
  shl ax, 2
  add si, ax
  cld
@@1:
  lodsw
  stosw
  lodsw
  stosb
  loop @@1
  pop ds
End;


Procedure ExpandTC(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); Assembler;
Asm
  push ds
  mov cx, Count
  les di, Source
  add di, From
  mov si, word ptr [Dest]
  mov ax, word ptr [Dest+2]
  db  $8E, $E0   {mov fs, ax}
  lds  bx, ColorRef

  add di, Count
  add si, Count
  add si, Count
  add si, Count
  add si, Count
  sub si, 4
  dec di
@@1:
  mov bx, word ptr [ColorRef]
  xor ah, ah
  mov al, es:[di]
  dec di
  add bx, ax
  add bx, ax
  add bx, ax
  add bx, ax
  db $66; mov ax, [bx]

  db $66,$64,$89,$04 {mov fs:[si], eax}
  sub si, 4
  loop @@1
  pop ds
End;

Procedure Remap256(Map : PByteArray; Count : Word; ColorRef : PColorRef); Assembler; Asm
    mov ax, word ptr [ColorRef]
    or  ax, word ptr [ColorRef+2]
    jne @@0
    mov ax, cs
    mov word ptr [ColorRef+2], ax
    mov word ptr [ColorRef], offset StdColorRefMap
@@0:
    push ds
    les di, Map
    lds bx, ColorRef
    mov cx, Count
    cld
@@1:
    mov al, es:[di]
    xlat
    stosb
    loop @@1
    pop ds
End;

Procedure Expand16TC(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); Assembler;
Asm
      push ds
      mov cx, Count
      les di, Source
      mov si, word ptr [Dest]
      mov ax, word ptr [Dest+2]
      db  $8E, $E0   {mov fs, ax}
      mov ax, word ptr [ColorRef + 2]
      mov ds, ax
    @@1:
      xor ah, ah
      mov  bx, cx
      dec  bx
      mov  dx, bx
      add  bx, From
      push bx
      shr  bx, 1
      mov  al, es:[di + bx]
      mov  bx, dx
      pop  dx
      shl  bx, 2
      test dx, 1
      jz @@2
      and al, $F
      mov dx, bx
      mov bx, ax
      shl bx, 2
      {mov ax, [offset DrawColor + bx]}
      add bx, word ptr [ColorRef]
      db $66; mov ax, [bx]
      mov bx, dx
      db $66, $64,$89,$00 {mov fs:[si + bx], eax}
      loop @@1
      jmp @@3
    @@2:
      shr al, 4
      mov dx, bx
      mov bx, ax
      shl bx, 2
      {mov ax, [offset DrawColor + bx]}
      add bx, word ptr [ColorRef]
      db $66; mov ax, [bx]
      mov bx, dx
      db $66,$64,$89,$00 {mov fs:[si + bx], eax}
      loop @@1
    @@3:
      pop ds
End;

Procedure Remap16(Map : PByteArray; Count : Word; ColorRef : PColorRef); Assembler;
Asm
    mov ax, word ptr [ColorRef]
    or  ax, word ptr [ColorRef+2]
    jne @@0
    mov ax, cs
    mov word ptr [ColorRef+2], ax
    mov word ptr [ColorRef], offset StdColorRefMap
@@0:
    push ds
    les di, Map
    lds bx, ColorRef
    mov ax, Count
    mov cx, ax
    shr cx, 1
    and ax, 1
    add cx, ax
    cld
@@1:
    xor al, al
    mov ah, es:[di]
    rol ax, 4
    xlat
    shl al, 4
    rol ax, 4
    xlat
    shl ah, 4
    or  al, ah
    stosb
    loop @@1
    pop ds
End;

Procedure ExpandHi(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); Assembler;
Asm
  push ds
  mov cx, Count
  les di, Source
  add di, From
  mov si, word ptr [Dest]
  mov ax, word ptr [Dest+2]
  db  $8E, $E0   {mov fs, ax}
  lds  bx, ColorRef

  add di, Count
  add si, Count
  add si, Count
  sub si, 2
  dec di
@@1:
  mov bx, word ptr [ColorRef]
  xor ah, ah
  mov al, es:[di]
  dec di
  add bx, ax
  add bx, ax
  add bx, ax
  add bx, ax
  mov ax, [bx]

  db $64,$89,$04 {mov fs:[si], ax}
  dec si
  dec si
  loop @@1
  pop ds
End;

Procedure Expand16Hi(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); Assembler;
Asm
      push ds
      mov cx, Count
      les di, Source
      mov si, word ptr [Dest]
      mov ax, word ptr [Dest+2]
      db  $8E, $E0   {mov fs, ax}
      mov ax, word ptr [ColorRef + 2]
      mov ds, ax
    @@1:
      xor ah, ah
      mov  bx, cx
      dec  bx
      mov  dx, bx
      add  bx, From
      push bx
      shr  bx, 1
      mov  al, es:[di + bx]
      mov  bx, dx
      pop  dx
      shl  bx, 1
      test dx, 1
      jz @@2
      and al, $F
      mov dx, bx
      mov bx, ax
      shl bx, 2
      {mov ax, [offset DrawColor + bx]}
      add bx, word ptr [ColorRef]
      mov ax, [bx]
      mov bx, dx
      db $64,$89,$00 {mov fs:[si + bx], ax}
      loop @@1
      jmp @@3
    @@2:
      shr al, 4
      mov dx, bx
      mov bx, ax
      shl bx, 2
      {mov ax, [offset DrawColor + bx]}
      add bx, word ptr [ColorRef]
      mov ax, [bx]
      mov bx, dx
      db $64,$89,$00 {mov fs:[si + bx], ax}
      loop @@1
    @@3:
      pop ds
End;


Procedure Expand16(Source, Dest : Pointer; Count, From : Word; ColorRef : PByteArray); Assembler;
Asm
      push ds
      mov cx, Count
      les di, Source
      lds si, Dest
    @@1:
      mov  bx, cx
      dec  bx
      mov  dx, bx
      add  bx, From
      push bx
      shr  bx, 1
      mov  al, es:[di + bx]
      mov  bx, dx
      pop  dx
      test dx, 1
      jz @@2
      and al, $F
      mov [si + bx], al
      loop @@1
      jmp @@3
    @@2:
      shr al, 4
      mov [si + bx], al
      loop @@1
    @@3:
      mov cx, Count
      les di, Dest
      lds si, ColorRef
      xor bh, bh
    @@4:
      mov bl, es:[di]
      mov al, ds:[si+bx]
      stosb
      loop @@4
      pop ds
End;

Procedure ExpandMono(Source, Dest : Pointer; Count, From : Word; ColorRef : PByteArray); Assembler;
Var
  ClrMap  : Word;
asm
  les di, ColorRef
  mov al, es:[di]
  mov byte ptr [ClrMap], al
  mov al, es:[di+15]
  mov byte ptr [ClrMap+1], al
      push ds
      les di, Source
      lds si, Dest
      mov cx, Count
    @@1:
      mov bx, cx
      dec bx
      add bx, From
      mov dx, bx
      shr bx, 3
      mov ah, es:[di + bx]
      mov bx, dx
      mov dx, 7
      and bx, dx
      sub dx, bx
      xor bx, bx
      inc bx
      push cx
      mov cx, dx
      shl bx, cl
      pop  cx
      mov dx, ClrMap
      test ah, bl
      jnz  @@2
      xchg dh, dl
    @@2:
      mov bx, cx
      dec bx
      mov [si+bx], dh
      loop @@1
      pop ds
End;

Procedure ExpandMonoHi(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); Assembler;
Var
  Clr0, Clr1 : Word;
asm
  les di, ColorRef
  mov ax, es:[di]
  mov Clr0, ax
  mov ax, es:[di+60]
  mov Clr1, ax

      push ds
      les di, Source
      lds si, Dest
      mov cx, Count
    @@1:
      mov bx, cx
      dec bx
      add bx, From
      mov dx, bx
      shr bx, 3
      mov ah, es:[di + bx]
      mov bx, dx
      mov dx, 7
      and bx, dx
      sub dx, bx
      xor bx, bx
      inc bx
      push cx
      mov cx, dx
      shl bx, cl
      pop  cx
      mov dx, Clr1
      test ah, bl
      jnz  @@2
      mov dx, Clr0
    @@2:
      mov bx, cx
      add bx, cx
      dec bx
      dec bx
      mov [si+bx], dx
      loop @@1
      pop ds
End;

Procedure ExpandMonoTC(Source, Dest : Pointer; Count, From : Word; ColorRef : PLongArray); Assembler;
Var
  Clr0, Clr1 : LongInt;
asm
  les di, ColorRef
  db $66; mov ax, es:[di]
  db $66; mov word ptr Clr0, ax
  db $66; mov ax, es:[di+60]
  db $66; mov word ptr Clr1, ax

      push ds
      les di, Source
      lds si, Dest
      mov cx, Count
    @@1:
      mov bx, cx
      dec bx
      add bx, From
      mov dx, bx
      shr bx, 3
      mov ah, es:[di + bx]
      mov bx, dx
      mov dx, 7
      and bx, dx
      sub dx, bx
      xor bx, bx
      inc bx
      push cx
      mov cx, dx
      shl bx, cl
      pop  cx
      db $66; mov dx, word ptr Clr1
      test ah, bl
      jnz  @@2
      db $66; mov dx, word ptr Clr0
    @@2:
      mov bx, cx
      shl bx, 2
      sub bx, 4
      db $66; mov [si+bx], dx
      loop @@1
      pop ds
End;


Procedure PrepareExpandPlaned(XLen : Word; ReversedPlanes : Boolean);
Var
  I : Byte;
Begin
  XLen := (XLen shr 3) + Byte((XLen and 7) <> 0);
  if ReversedPlanes then
  for I := 0 to 3 do PlanesPreCalc[3 - I] := XLen * I else
  for I := 0 to 3 do PlanesPreCalc[I] := XLen * I;
End;

Procedure ExpandPlanedTo17;
Var
  CB, Color : Byte;
  ADX, J : Word;
Begin
  CB := 0;
  for J := 0 to Count - 1 do asm
      mov bx, J
      add bx, From
      mov ax, bx
      shr bx, 3
      mov ADX, bx
      mov cl, 7
      and al, cl
      sub cl, al
      xor ch, ch
      mov Color, ch
      inc ch
      shl ch, cl

      les di, Source
      mov si, offset PlanesPreCalc
      xor dl, dl
      mov dh, 8

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@1
      or dl, dh
   @@1:
      shr dh, 1

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@2
      or dl, dh
   @@2:
      shr dh, 1

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@3
      or dl, dh
   @@3:
      shr dh, 1

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@4
      or dl, dh
   @@4:
      shr dh, 1

      mov Color, dl

      mov bx, J
      add bx, From
      test bx, 1
      je @@5
      and CB, $F0
      or  CB, dl
      les di, Dest
      shr bx, 1
      add di, bx
      mov al, CB
      stosb
      jmp @@6
    @@5:
      shl dl, 4
      mov CB, dl
    @@6:
  end;
  if (J and 1) = 0 then Dest^[J shr 1] := Color;
End;

Procedure ExpandPlaned;
Var
  ADX, J   : Word;
Begin
  for J := 0 to Count - 1 do asm
      mov bx, J
      add bx, From
      mov ax, bx
      shr bx, 3
      mov ADX, bx
      mov cl, 7
      and al, cl
      sub cl, al
      xor ch, ch
      inc ch
      shl ch, cl

      les di, Source
      mov si, offset PlanesPreCalc
      xor dl, dl
      mov dh, 8

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@1
      or dl, dh
   @@1:
      shr dh, 1

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@2
      or dl, dh
   @@2:
      shr dh, 1

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@3
      or dl, dh
   @@3:
      shr dh, 1

      mov bx, [si]
      add bx, ADX
      inc si
      inc si
      test ch, es:[di+bx]
      je @@4
      or dl, dh
   @@4:
      shr dh, 1
      les di, Dest
      mov bx, J
      add bx, From
      mov es:[di+bx], dl
  end;
End;

Procedure Impact17Planed(Source, Dest : PByteArray; Count : Word); Assembler;
var
  step : word;
ASM
     PUSH   DS
     CLD
     MOV    AX, Count
     TEST   AX, 3
     JNZ    @A0
     SHR    AX, 2
     DEC    AX
     JMP    @A1
@A0:
     SHR    AX, 2
@A1:
     MOV    step, AX
     LDS    SI, Source
     LES    DI, Dest
     MOV    CX, Count
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

Procedure Impact256Planed(Source, Dest : PByteArray; Count : Word); Assembler;
var
  step : word;
ASM
     PUSH   DS
     CLD
     MOV    AX, Count
     TEST   AX, 3
     JNZ    @A0
     SHR    AX, 2
     DEC    AX
     JMP    @A1
@A0:
     SHR    AX, 2
@A1:
     MOV    step, AX
     LDS    SI, Source
     LES    DI, Dest
     MOV    CX, Count
@A2:
     MOV    AH, 8
@A3:
     LODSB
     DEC    CX
     shl al, 4
     RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BL, 1
   {  RCL    AL, 1
     RCL    DH, 1
     RCL    AL, 1
     RCL    DL, 1
     RCL    AL, 1
     RCL    BH, 1
     RCL    AL, 1
     RCL    BL, 1}
     DEC    AH
     JZ     @C1
     JCXZ   @C0
     JMP    @A3
@C0:
     CMP    AH, 0
     JZ     @C1
     PUSH   CX
     {ADD    AH, AH}
     MOV    CL, AH
     RCL    BL, CL
     RCL    BH, CL
     RCL    DL, CL
     RCL    DH, CL
     POP    CX
@C1:
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


Procedure Impact16(Source, Dest : Pointer; Count, From : Word; ColorRef : PColorRef); Assembler;
Asm
    mov ax, word ptr [ColorRef]
    or  ax, word ptr [ColorRef+2]
    jne @@0
    mov ax, cs
    mov word ptr [ColorRef+2], ax
    mov word ptr [ColorRef], offset StdColorRefMap
@@0:
      push ds
      cld
      mov cx, Count
{!!}  sub cx, From
      mov ax, cx
      shr cx, 1
      and ax, 1
      add cx, ax
      les di, Dest
      lds si, Source
      add si, From
      mov ax, word ptr [ColorRef + 2]
      db $8E, $E0 {mov fs, ax}
      mov bx, word ptr [ColorRef]
      push bp
    @@1:
      mov dx, bx
      xor ah, ah
      lodsb
      add bx, ax
      db $64, $8A, $07 {mov al, fs:[bx]}
      shl ax, 4
      mov bp, ax
      mov bx, dx
      lodsb
      add bx, ax
      db $64, $8A, $07 {mov al, fs:[bx]}
      and ax, $F
      or ax, bp
      stosb
      mov bx, dx
      loop @@1
      pop bp

      pop ds
End;


Procedure ImpactMono(Source, Dest : Pointer; Count, From : Word; ColorRef : PColorRef); Assembler;
Asm
    mov ax, word ptr [ColorRef]
    or  ax, word ptr [ColorRef+2]
    jne @@0
    mov ax, cs
    mov word ptr [ColorRef+2], ax
    mov word ptr [ColorRef], offset StdColorRefMap
@@0:
      push ds
      cld
      lds si, Source
      les di, Dest
      mov ax, word ptr [ColorRef + 2]
      db $8E, $E0 {mov fs, ax}
      mov bx, word ptr [ColorRef]
      mov cx, Count
      shr cx, 3
      mov ax, Count
      and ax, 7
      or ax, ax
      jz @@1
      inc cx
@@1:
      push cx
      mov cx, 8
@@2:
      lodsb
      xor ah, ah
      push bx
      add bx, ax
      db $64, $8A, $07 {mov al, fs:[bx]}
      shr al, 1
      rcl dx, 1
      pop bx
      loop @@2
      pop cx
      mov ax, dx
      stosb
      loop @@1
      pop ds
End;

Const
  aHalftone8x8 : Array[0..63] of Byte =
  ( 0, 38,  9, 47,  2, 40, 11, 50,
   25, 12, 35, 22, 27, 15, 37, 24,
    6, 44,  3, 41,  8, 47,  5, 43,
   31, 19, 28, 15, 34, 21, 31, 18,
    1, 39, 11, 49,  0, 39, 10, 48,
   27, 14, 36, 23, 26, 13, 35, 23,
    7, 46,  4, 43,  7, 45,  3, 42,
   33, 20, 30, 17, 32, 19, 29, 16);


Procedure ImpactTrueColor(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte); Assembler;
Asm
    push ds
    lds si, Source
    les di, Dest
    cld
    mov dl, LineSeqNo
    xor dh, dh
    and dl, 7
    shl dl, 3
    mov cx, From
    add si, cx
    add si, cx
    add si, cx
    xor cx, cx
    mov ax, seg aHalfTone8x8
    db $8E, $E0      {mov fs, ax}
  @@1:
    mov bl, cl
    and bl, 7
    add bl, dl
    xor bh, bh
    add bx, offset aHalfTone8x8
    db $64, $8A, $37 {mov dh, fs:[bx]}
    xor bl, bl
    mov bh, 51

    xor ah, ah
    lodsb
    div bh
    cmp ah, dh
    jbe @Blue
    inc al
  @Blue:
    shl al, 2
    mov ah, al
    shl al, 3
    add al, ah
    mov bl, al

    xor ah, ah
    lodsb
    div bh
    cmp ah, dh
    jbe @Green
    inc al
  @Green:
    shl al, 1
    mov ah, al
    shl al, 1
    add al, ah
    add bl, al

    xor ah, ah
    lodsb
    div bh
    cmp ah, dh
    jbe @Red
    inc al
  @Red:
    add al, bl

    stosb
    inc cx
    cmp cx, Count
    jb  @@1

    pop ds
End;

Procedure ImpactTrue256(Source, Dest : PByteArray; Count, From : Word; LineSeqNo : Byte); Assembler; Asm
    push ds
    lds si, Source
    les di, Dest
    cld
    mov dl, LineSeqNo
    xor dh, dh
    and dl, 7
    shl dl, 3
    mov cx, From
    add si, cx
    add si, cx
    add si, cx
    add si, cx
    xor cx, cx
    mov ax, seg aHalfTone8x8
    db $8E, $E0      {mov fs, ax}
  @@1:
    mov bl, cl
    and bl, 7
    add bl, dl
    xor bh, bh
    add bx, offset aHalfTone8x8
    db $64, $8A, $37 {mov dh, fs:[bx]}
    xor bl, bl
    mov bh, 51

    xor ah, ah
    lodsb
    div bh
    cmp ah, dh
    jbe @Blue
    inc al
  @Blue:
    shl al, 2
    mov ah, al
    shl al, 3
    add al, ah
    mov bl, al

    xor ah, ah
    lodsb
    div bh
    cmp ah, dh
    jbe @Green
    inc al
  @Green:
    shl al, 1
    mov ah, al
    shl al, 1
    add al, ah
    add bl, al

    xor ah, ah
    lodsb
    inc si
    div bh
    cmp ah, dh
    jbe @Red
    inc al
  @Red:
    add al, bl

    stosb
    inc cx
    cmp cx, Count
    jb  @@1

    pop ds
End;


Procedure Impact256Mono; Assembler;
Var
  DC : Byte;
Asm
    mov ax, word ptr [Palette]
    or  ax, word ptr [Palette+2]
    jne @@0
    mov ax, word ptr [PMainPalette+2]
    mov word ptr [Palette+2], ax
    mov ax, word ptr [PMainPalette]
    mov word ptr [Palette], ax
 @@0:
    push ds
    lds si, Source
    les di, Dest
    cld
    mov dl, LineSeqNo
    xor dh, dh
    and dl, 7
    shl dl, 3
    mov cx, From
    add si, cx
    xor cx, cx
    mov ax, seg aHalfTone8x8
    db $8E, $E0      {mov fs, ax}
    mov ax, word ptr [Palette+2]
    db $8E, $E8      {mov gs, ax}
  @@1:
    mov bl, cl
    and bl, 7
    add bl, dl
    xor bh, bh
    add bx, offset aHalfTone8x8
    db $64, $8A, $37 {mov dh, fs:[bx]}
    xor ah, ah
    lodsb

    push di
    mov di, ax
    add di, ax
    add di, ax
    add di, word ptr [Palette]

    xor bh, bh
    db $65, $8A, $05      {mov al, gs:[di]}
    db $65, $8A, $5D, $01 {mov bl, gs:[di+1]}
    add ax, bx
    db $65, $8A, $5D, $02 {mov bl, gs:[di+2]}
    add ax, bx
    pop di

    mov bh, 51
    div bh
    cmp ah, dh
    jbe @Next
    inc al
  @Next:
    xor ah, ah

    cmp al, 7
    ja @@2
    xor al, al
    jmp @@3
  @@2:
    mov al, 1
  @@3:
    rcr al, 1
    rcl DC, 1
    inc cx
    test cx, 7
    jnz @@4
    mov al, DC
    stosb
  @@4:
    cmp cx, Count
    jb  @@1
    and cx, 7
    jz @@5
    mov ch, 8
    sub ch, cl
    xchg ch, cl
    mov al, DC
    rol al, cl
    stosb
  @@5:
    pop ds
End;

Procedure ImpactTCMono; Assembler;
Var
  DC : Byte;
Asm
    push ds
    lds si, Source
    les di, Dest
    cld
    mov dl, LineSeqNo
    xor dh, dh
    and dl, 7
    shl dl, 3
    mov cx, From
    add si, cx
    xor cx, cx
    mov ax, seg aHalfTone8x8
    db $8E, $E0      {mov fs, ax}
  @@1:
    mov bl, cl
    and bl, 7
    add bl, dl
    xor bh, bh
    add bx, offset aHalfTone8x8
    db $64, $8A, $37 {mov dh, fs:[bx]}

    xor ah, ah
    lodsb
    mov bx, ax
    lodsb
    add bx, ax
    lodsb
    add bx, ax
    lodsb
    xchg bx, ax

    mov bh, 51
    div bh
    cmp ah, dh
    jbe @Next
    inc al
  @Next:
    xor ah, ah

    cmp al, 7
    ja @@2
    xor al, al
    jmp @@3
  @@2:
    mov al, 1
  @@3:
    rcr al, 1
    rcl DC, 1
    inc cx
    test cx, 7
    jnz @@4
    mov al, DC
    stosb
  @@4:
    cmp cx, Count
    jb  @@1
    and cx, 7
    jz @@5
    mov ch, 8
    sub ch, cl
    xchg ch, cl
    mov al, DC
    rol al, cl
    stosb
  @@5:
    pop ds
End;



Procedure Impact16Mono; Assembler;
Var
  DC : Byte;
Asm
    mov ax, word ptr [Palette]
    or  ax, word ptr [Palette+2]
    jne @@0
    mov ax, word ptr [PMainPalette+2]
    mov word ptr [Palette+2], ax
    mov ax, word ptr [PMainPalette]
    mov word ptr [Palette], ax
 @@0:
    push ds
    lds si, Source
    les di, Dest
    cld
    mov dl, LineSeqNo
    xor dh, dh
    and dl, 7
    shl dl, 3
    mov cx, From
    add si, cx
    mov ax, seg aHalfTone8x8
    db $8E, $E0      {mov fs, ax}
    mov ax, word ptr [Palette+2]
    db $8E, $E8      {mov gs, ax}
  @@1:
    mov bl, cl
    and bl, 7
    add bl, dl
    xor bh, bh
    add bx, offset aHalfTone8x8
    db $64, $8A, $37 {mov dh, fs:[bx]}
    xor bl, bl
    mov bh, 51
    xor ah, ah
    lodsb
    dec si
    test cx, 1
    jz @@6
    shl al, 4
    inc si
  @@6:
    shr al, 4
    xor ah, ah
    push di

    mov di, ax
    add di, ax
    add di, ax
    add di, word ptr [Palette]

    xor bh, bh
    db $65, $8A, $05      {mov al, gs:[di]}
    db $65, $8A, $5D, $01 {mov bl, gs:[di+1]}
    add ax, bx
    db $65, $8A, $5D, $02 {mov bl, gs:[di+2]}
    add ax, bx
    pop di

    mov bh, 51
    div bh
    cmp ah, dh
    jbe @Next
    inc al
  @Next:
    xor ah, ah

    cmp al, 7
    ja @@2
    xor al, al
    jmp @@3
  @@2:
    mov al, 1
  @@3:
    rcr al, 1
    rcl DC, 1
    inc cx
    test cx, 7
    jnz @@4
    mov al, DC
    stosb
  @@4:
    cmp cx, Count
    jb  @@1
    and cx, 7
    jz @@5
    mov ch, 8
    sub ch, cl
    xchg ch, cl
    mov al, DC
    rol al, cl
    stosb
  @@5:
    pop ds
End;


Procedure ExpandHiTrue(Source, Dest : PByteArray; Count, From : Word); Assembler;
Asm
  push ds
  lds si, Source
  les di, Dest
  mov cx, Count
  mov ax, cx
  dec ax
  shl ax, 1
  add si, ax
  add si, From
  add si, From
  add di, ax
  add di, cx
  inc di
  std
@@1:
  lodsw
  mov dx, ax
  shl al, 3
  stosb
  mov ax, dx
  shr ax, 6
  shl al, 3
  stosb
  mov al, dh
  and al, 11111000b
  stosb
  loop @@1
  cld
  pop  ds
End;

Procedure ExpandHi32(Source, Dest : PByteArray; Count, From : Word); Assembler;
Asm
  push ds
  lds si, Source
  les di, Dest
  mov cx, Count
  mov ax, cx
  dec ax
  shl ax, 1
  add si, ax
  add si, From
  add si, From
  add di, ax
  add di, ax
  inc di
  std
@@1:
  stosb
  lodsw
  mov dx, ax
  shl al, 3
  stosb
  mov ax, dx
  shr ax, 6
  shl al, 3
  stosb
  mov al, dh
  and al, 11111000b
  stosb
  loop @@1
  cld
  pop  ds
End;


Procedure ImpactTrueHi(Source, Dest : PByteArray; Count, From : Word); Assembler;
Asm
  push ds
  mov dh, HiColor16Bit
  lds si, Source
  les di, Dest
  cld
  mov cx, Count
  add si, From
  add si, From
  add si, From
@@1:
  lodsb
  mov bh, al
  lodsb
  mov dl, al
  and dl, 11111000b
  xchg cl, dh
  shr  bx, cl
  xchg cl, dh
  or  bh, dl
  lodsb
  mov dl, al
  and dl, 11111000b
  shr bx, 5
  or  bh, dl
  mov ax, bx
  mov bx, cx
  mov cl, dh
  and cl, 1
  shr ax, cl
  mov cx, bx
  stosw
  loop @@1
  pop  ds
End;

Procedure Impact32Hi(Source, Dest : PByteArray; Count, From : Word); Assembler;
Asm
  push ds
  mov dh, HiColor16Bit
  lds si, Source
  les di, Dest
  cld
  mov cx, Count
  add si, From
  add si, From
  add si, From
  add si, From
@@1:
  lodsb
  mov bh, al
  lodsb
  mov dl, al
  and dl, 11111000b
  xchg cl, dh
  shr  bx, cl
  xchg cl, dh
  or  bh, dl
  lodsw
  mov dl, al
  and dl, 11111000b
  shr bx, 5
  or  bh, dl
  mov ax, bx
  mov bx, cx
  mov cl, dh
  and cl, 1
  shr ax, cl
  mov cx, bx
  stosw
  loop @@1
  pop  ds
End;


Procedure ConvertPlaned24(Source, Dest : Pointer; Count, From : Word); Assembler;
Var
  Second : Word;
Asm
  push ds
  lds  bx, Source
  les  di, Dest
  mov  cx, Count
  mov  ax, From
  add  ax, cx
  inc ax
  mov Second, ax
  add ax, From
  add ax, cx
  inc ax
  mov dx, ax
  cld
@@1:
  mov  si, bx
  add  si, dx
  mov  al, [si]
  stosb
  mov  si, bx
  add  si, Second
  mov  al, [si]
  stosb
  mov  al, [bx]
  stosb
  inc  bx
  loop @@1
  pop  ds
End;

Procedure RightMonoAdj(Source, Dest : Pointer; From, Count : Word); Assembler; Asm
  mov ax, From
  mov dx, ax
  sar dx, 3
  and ax, 7
  mov bl, 8
  sub bl, al
  mov cx, Count
  sar cx, 3
  inc cx
  cld
  push ds
  lds si, Source
  les di, Dest
  add si, dx
@@1:
  lodsw
  dec si
  xchg cx, bx
  ror ax, cl
  xchg cx, bx
  xchg ah, al
  stosb
  loop @@1
  pop ds
End;



Procedure ReverseRGB(Map : Pointer; Count : Word); Assembler;
Asm
  push ds
  lds si, map
  les di, map
  cld
  mov cx, Count
@@1:
  lodsw
  xchg bl, al
  lodsb
  stosw
  xchg bl, al
  stosb
  loop @@1
  pop ds
End;


End.