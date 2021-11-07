{$G+,F+,S-}
{$IFDEF DPMI}
{$C FIXED PRELOAD PERMANENT}
{$ENDIF}
unit Prot386;

{ Protects 386 calculations from nasty ISRs that trash the 386 registers.
  Just "use prot386" and interrupt handlers will be installed to protect
  calculations. }
{Dk Inc. 1996 patch. Old version is not usable for protected mode,
so I made a reconstruction for it}

interface

implementation

uses
  Dos;

const
  Op32 = $66;

Type
  PIntRecArray = ^TIntRecArray;
  {$IFDEF DPMI}
  TIntRecArray = array[0..15] of Pointer;
  {$ELSE}
  regs = (reax,rebx,recx,redx);
  TIntRec = record   { This record must be exactly 16 bytes long!!! }
    oldisr : pointer;
    junk : array[1..12] of byte;
  end;
  TIntRecArray = array[0..15] of TIntRec;
  {$ENDIF}

{$IFNDEF DPMI}
{ Put the oldisr pointers in the code segment to make the ISR simple. }

procedure InterruptRecs; assembler;
{ We need 256 bytes here.  Most of it is unused, but it'd make the ISR
  too complicated if I got rid of the Junk field.}
asm
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7,8
  dd 1,2,3,4,5,6,7
  db 1,2,3
end;  { RET is the last byte }

const
  PtrOfs = 15*sizeof(TIntRec);

procedure FixupISR; assembler;
{ This ISR saves and restores the high word of EAX,EBX,ECX,EDX.
  Use it to fix up a bad handler.  Uses 14 bytes of stack space. }
asm
  push bp
  mov bp,sp
  db Op32; push ax
  pop ax
  db Op32; push bx
  pop bx
  db Op32; push cx
  pop cx
  db Op32; push dx
  pop dx
  push word ptr [bp+6]      { This pushes the old flags again }
  mov bp,[bp]                { Restore BP for the old interrupt }
  call dword ptr cs:InterruptRecs[PtrOfs]
  push ax
  pushf
  pop ax                     { Now flags are in AX }

  push bp                    { Save the ISR's BP }
  mov bp,sp                  { Set up our frame again }
  add bp,12
  mov word ptr [bp+6],ax      { This way flags on our return will be
                                   as the old ISR returned them. }
  pop ax
  mov word ptr [bp],ax        { as will BP }
  pop ax

  push dx
  db Op32; pop dx
  push cx
  db Op32; pop cx
  push bx
  db Op32; pop bx
  push ax
  db Op32; pop ax
  pop bp
  iret
end;
{$ENDIF}

{$IFDEF DPMI}
Var
  CodeAlias   : Word;

procedure Isr00; assembler;
{ This ISR saves and restores the high word of EAX,EBX,ECX,EDX.
  Use it to fix up a bad handler.  Uses 14 bytes of stack space. }
asm
  push bp
  mov bp,sp
  db Op32; push ax
  pop ax
  db Op32; push bx
  pop bx
  db Op32; push cx
  pop cx
  db Op32; push dx
  pop dx
  push word ptr [bp+6]      { This pushes the old flags again }
  mov bp,[bp]                { Restore BP for the old interrupt }
  db 9ah                  {< - call far}
  dd 0
  push ax
  pushf
  pop ax                     { Now flags are in AX }

  push bp                    { Save the ISR's BP }
  mov bp,sp                  { Set up our frame again }
  add bp,12
  mov word ptr [bp+6],ax      { This way flags on our return will be
                                   as the old ISR returned them. }
  pop ax
  mov word ptr [bp],ax        { as will BP }
  pop ax

  push dx
  db Op32; pop dx
  push cx
  db Op32; pop cx
  push bx
  db Op32; pop bx
  push ax
  db Op32; pop ax
  pop bp
  iret
end;

{These IsrXX are the same as Isr00, but in code}
Procedure Isr01; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr02; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr03; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr04; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr05; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr06; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr07; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr08; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;


Procedure Isr09; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr10; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr11; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr12; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr13; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr14; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;

Procedure Isr15; Assembler; Asm
  db $55, $89, $e5, $66, $50, $58, $66, $53, $5B, $66, $51, $59, $66, $52, $5a, $ff
  db $76, $06, $8b, $6e, $00, $9a, $00, $00, $00, $00, $50, $9c, $58, $55, $89, $e5
  db $83, $c5, $0c, $89, $46, $06, $58, $89, $46, $00, $58, $52, $66, $5a, $51, $66
  db $59, $53, $66, $5b, $50, $66, $58, $5D, $cf
End;
{$ENDIF}

Procedure SetIsrs(var Isrs : TIntRecArray);
Begin
  {$IFDEF DPMI}
  isrs[00] := @Isr00;
  isrs[01] := @Isr01;
  isrs[02] := @Isr02;
  isrs[03] := @Isr03;
  isrs[04] := @Isr04;
  isrs[05] := @Isr05;
  isrs[06] := @Isr06;
  isrs[07] := @Isr07;
  isrs[08] := @Isr08;
  isrs[09] := @Isr09;
  isrs[10] := @Isr10;
  isrs[11] := @Isr11;
  isrs[12] := @Isr12;
  isrs[13] := @Isr13;
  isrs[14] := @Isr14;
  isrs[15] := @Isr15;
  {$ENDIF}
End;

procedure Install;
var
  int,irq : byte;
  isrs    : TIntRecArray;
  IntRecs : PIntRecArray;

  procedure InstallHandler;
  var
    addr : Pointer;
    segmod : byte;
  begin
    {$IFDEF DPMI}
    addr := Ptr(CodeAlias, Ofs(Isrs[irq]^) + 22);
    GetIntVec(int, Pointer(addr^));
    SetIntVec(int, Isrs[irq]);
    {$ELSE}
    GetIntVec(int, IntRecs^[irq].OldIsr);
    segmod := 15-irq;
    Addr := Ptr(Seg(FixupISR)-segmod, Ofs(FixupISR)+16*segmod);
    SetIntVec(int,Addr);
    {$ENDIF}
  end;

begin
  {$IFDEF DPMI}
  SetIsrs(isrs);
  asm
    mov ax, 0ah
    mov bx, cs
    int 31h
    mov CodeAlias, ax
  end;
  {$ELSE}
  IntRecs := @InterruptRecs;
  {$ENDIF}
  for int := 8 to $F do
  begin
    irq := int-8;
    installhandler;
  end;
  for int := $70 to $77 do
  begin
    irq := int-$70+8;
    installhandler;
  end;
end;

procedure UnInstall;
var
  int,irq : byte;
  isrs    : TIntRecArray;
  IntRecs : PIntRecArray;

  procedure UnInstallHandler;
  var
    Addr : Pointer;
  begin
    {$IFDEF DPMI}
    addr := Ptr(CodeAlias, Ofs(Isrs[irq]^) + 22);
    SetIntVec(int, Pointer(addr^));
    {$ELSE}
    SetIntVec(int,IntRecs^[irq].OldIsr);
    {$ENDIF}
  end;

begin
  {$IFDEF DPMI}
  SetIsrs(isrs);
  {$ELSE}
  IntRecs := @InterruptRecs;
  {$ENDIF}
  for int := 8 to $F do
  begin
    irq := int-8;
    uninstallhandler;
  end;
  for int := $70 to $77 do
  begin
    irq := int-$70+8;
    uninstallhandler;
  end;
  {$IFDEF DPMI}
  asm
    mov ax, 1
    mov bx, CodeAlias
    int 31h
  end;
  {$ENDIF}
end;

var
  OldExitProc : pointer;

procedure MyExitProc; far;
begin
  ExitProc := OldExitProc;
  Uninstall;
end;

begin
  if test8086 >= 2 then
  begin
    Install;
    OldExitProc := ExitProc;
    ExitProc := @MyExitProc;
  end;
end.
