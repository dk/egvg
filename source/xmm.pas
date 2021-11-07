{$F+,G+,S-}
{
  DK Inc. 1994
  Extended memory management unit.
}
Unit XMM;

Interface

Uses Objects;

Const
  XMMError   : Byte    = 0;

  xeNoError        = $00;
  xeNotFunc        = $80;
  xeIsVDisk        = $81;
  xeA20Error       = $82;
  xeGeneralError   = $8E;
  xeFatalError     = $8F;
  xeNoHMA          = $90;
  xeNoAccessHMA    = $91;
  xeDXUnderFlow    = $92;
  xeNoAllocHMA     = $93;
  xeA20StillOn     = $94;
  xeNoMoreXMS      = $A0;
  xeNoMoreHandles  = $A1;
  xeInvalidHandle  = $A2;
  xeInvalidSourHan = $A3;
  xeInvalidSourOfs = $A4;
  xeInvalidDestHan = $A5;
  xeInvalidDestOfs = $A6;
  xeInvalidLength  = $A7;
  xeInvalidOverlap = $A8;
  xeParityError    = $A9;
  xeNotLocked      = $AA;
  xeLocked         = $AB;
  xeLockOverflow   = $AC;
  xeLockFailed     = $AD;
  xeOnlyLessUMBs   = $B0;
  xeNoUMBsAvail    = $B1;
  xeInvalidUMBSeg  = $B2;

Type
  TXMMMoveRec = Record
    Length       : LongInt;
    SourceHandle : Word;
    SourceOffset : LongInt;
    DestHandle   : Word;
    DestOffset   : LongInt;
  End;

  TXMSStream = Object(TStream)
    Handle  : Word;
    Size    : LongInt;
    Current : LongInt;

    Constructor Init(aSize : LongInt);
    Destructor  Done; Virtual;
    Function    GetPos  : LongInt; Virtual;
    Function    GetSize : LongInt; Virtual;
    Procedure   Read (var Buf; Count : Word); Virtual;
    Procedure   Write(var Buf; Count : Word); Virtual;
    Procedure   Seek(Pos : LongInt); Virtual;
    Procedure   Truncate; Virtual;
  End;
  PXMSStream = ^TXMSStream;

Function XMMInstalled : Boolean;
{Checks HIMEM.SYS, stores the @director in XMMControl}
{!if not XMMInstalled any call of other function will hangup the system!}

Function XMMVersion : LongInt;                     {*}

Function XMMRequestHMA(Space : Word{KBytes}) : LongInt;    {*}

Function XMMReleaseHMA : LongInt;                  {*}

Function XMMGlobalEnableA20 : LongInt;             {*}

Function XMMGlobalDisableA20 : LongInt;            {*}

Function XMMEnableA20 : LongInt;                   {*}

Function XMMDisableA20 : LongInt;                  {*}

Function XMMQueryA20 : LongInt;                    {*}

Function XMMQueryLargestFree : LongInt;            {*}

Function XMMQueryTotalFree : LongInt;              {*}

Function XMMAllocateExtended(Space : Word{KBytes}) : LongInt;
{if Ok, returns Handle}
Function XMMFreeExtended(Handle : Word) : LongInt;

Function XMMMoveExtended(var MR : TXMMMoveRec) : LongInt;

Function XMMLockExtended(Handle : Word) : LongInt;

Function XMMUnLockExtended(Handle : Word) : LongInt;

Function XMMGetHandleLength(Handle : Word) : LongInt;

Function XMMGetHandleInfo(Handle : Word) : LongInt;

Function XMMReallocateExtended(Handle, NewSize : Word) : LongInt;

Function XMMRequestUMB(Space : Word) : LongInt;

Function XMMReleaseUMB(Segment : Word) : LongInt;

Function MoveToXMM(Handle : Word; DestOffset, Size : LongInt; Where : Pointer) : LongInt;

Function MoveFromXMM(Handle : Word; SourceOffset, Size : LongInt; Where : Pointer) : LongInt;

Function XMMErrorStr(Code : Byte) : String;

Implementation

Const
  XMMControl : Pointer = Nil;

Constructor TXMSStream.Init(aSize : LongInt);
Var
  X : LongInt;
  S : LongInt;
Begin
  Inherited Init;
  if not(XMMInstalled) then begin
    Fail;
    Exit;
  end;

  S := aSize div 1024;
  if aSize mod 1024 > 0 then Inc(S);
  X := XMMAllocateExtended(S);
  if X < 0 then begin
    Status := -11;
    Exit;
  end;

  Size    := aSize;
  Handle  := X;
  Current := 0;
  Status  := 0;
End;

Destructor TXMSStream.Done;
Begin
  XMMFreeExtended(Handle);
  Inherited Done;
End;

Function TXMSStream.GetPos  : LongInt;
Begin
  GetPos := Current;
End;

Function TXMSStream.GetSize : LongInt;
Begin
  GetSize := Size;
End;

Procedure TXMSStream.Seek(Pos : LongInt);
Begin
  if (Pos >= 0) and (Pos <= Size) then Current := Pos;
End;

Procedure TXMSStream.Read(var Buf; Count : Word);
Var
  Tail : Word;
  Piss : Word;
  PBuf : ^Byte;
Begin
  if Status <> 0 then Exit;

  Piss := Count;
  if Odd(Count) then Dec(Piss);
  if Piss > 0 then MoveFromXMM(Handle, Current, Piss, @Buf);
  Inc(Current, Piss);

  if Odd(Count) then begin
    MoveFromXMM(Handle, Current, 2, @Tail);
    PBuf := @Buf;
    Inc(Current);
    Inc(LongInt(PBuf), Piss);
    PBuf^ := Byte(Tail);
  end;

  if Current > Size then begin
    Current := Size;
    Status  := stReadError;
  end;
End;

Procedure TXMSStream.Write(var Buf; Count : Word);
Var
  Tail : Word;
  Piss : Word;
  PBuf : ^Byte;
  L    : LongInt;
Begin
  if Status <> 0 then Exit;
  if Count + Current > Size then begin
    Tail := (Count + Current + 1) div 1024 + Byte(((Count + Current + 1) mod 1024) > 0);
    Piss := Size div 1024 + Byte((Size mod 1024) > 0);
    if Tail <= Piss then L := 0 else L := XMMReallocateExtended(Handle, Tail);
    if L = 0 then Size := Count + Current;
  end;

  Piss := Count;
  if Odd(Count) then Dec(Piss);
  if Piss > 0 then MoveToXMM(Handle, Current, Piss, @Buf);
  Inc(Current, Piss);

  if Odd(Count) then begin
    MoveFromXMM(Handle, Current, 2, @Tail);
    PBuf := @Buf;
    Inc(LongInt(PBuf), Piss);
    Byte(Tail) := PBuf^;
    MoveToXMM(Handle, Current, 2, @Tail);
    Inc(Current);
  end;

  if Current > Size then begin
    Current := Size;
    Status  := stWriteError;
  end;
End;

Procedure TXMSStream.Truncate;
Begin
  if XMMReallocateExtended(Handle, (Current div 1024) + Byte((Current mod 1024) > 0)) = 0 then Size := Current;
End;

Function XMMErrorStr(Code : Byte) : String;
Begin
  case Code of
  $00 : XMMErrorStr := 'No error';
  $80 : XMMErrorStr := 'Function not implemented';
  $81 : XMMErrorStr := 'VDISK was detected';
  $82 : XMMErrorStr := 'An A20 error occurred';
  $8E : XMMErrorStr := 'A general driver error';
  $8F : XMMErrorStr := 'Unrecoverable driver error';
  $90 : XMMErrorStr := 'HMA does not exist';
  $91 : XMMErrorStr := 'HMA is already in use';
  $92 : XMMErrorStr := 'DX is less than the /HMAMIN= parameter';
  $93 : XMMErrorStr := 'HMA is not allocated';
  $94 : XMMErrorStr := 'A20 line still enabled';
  $A0 : XMMErrorStr := 'All extended memory is allocated';
  $A1 : XMMErrorStr := 'All available extended memory handles are allocated';
  $A2 : XMMErrorStr := 'Invalid handle';
  $A3 : XMMErrorStr := 'Source handle is invalid';
  $A4 : XMMErrorStr := 'Source offset is invalid';
  $A5 : XMMErrorStr := 'Destination handle is invalid';
  $A6 : XMMErrorStr := 'Destination offset is invalid';
  $A7 : XMMErrorStr := 'Length is invalid';
  $A8 : XMMErrorStr := 'Move has an invalid overlap';
  $A9 : XMMErrorStr := 'Parity error occurred';
  $AA : XMMErrorStr := 'Block is not locked';
  $AB : XMMErrorStr := 'Block is locked';
  $AC : XMMErrorStr := 'Block lock count overflowed';
  $AD : XMMErrorStr := 'Block failed';
  $B0 : XMMErrorStr := 'Only a smaller UMB is available';
  $B1 : XMMErrorStr := 'No UMB''s are available';
  $B2 : XMMErrorStr := 'UMB segment number is invalid';
  else XMMErrorStr := 'Unknown error'; end;
End;


Function XMMInstalled : Boolean;
Begin
  asm
        mov ax, 4300h
        int 2fh
        cmp al, 80h
        jne @NotInstalled
        mov ax, 4310h
        int 2fh
        mov word ptr [XMMControl], bx
        mov word ptr [XMMControl+2], es
        mov ax, 1
        jmp @Installed
   @NotInstalled:mov ax, 0
   @Installed:
        mov [bp-1], al
        mov XMMError, 0
  end;
End;

Function XMMVersion : LongInt;
Begin
  asm
        mov XMMError, 0
        xor ah, ah
        call [XMMControl]
        mov dx, bx
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMRequestHMA(Space : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 1
        mov dx, Space
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
        @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end;
End;

Function XMMReleaseHMA : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 2
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
        @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end;
End;

Function XMMGlobalEnableA20 : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 3
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMGlobalDisableA20 : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 4
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMEnableA20 : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 5
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMDisableA20 : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 6
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMQueryA20 : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 7
        call [XMMControl]
        xor dx, dx
        or  ax, ax
        jnz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMQueryLargestFree : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 8
        call [XMMControl]
        xor dx, dx
        or  ax, ax
        jnz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMQueryTotalFree : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 8
        call [XMMControl]
        or  ax, ax
        mov ax, dx
        mov dx, 0
        jnz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMAllocateExtended(Space : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 9
        mov dx, Space
        call [XMMControl]
        or  ax, ax
        mov ax, dx
        mov dx, 0
        jnz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMFreeExtended(Handle : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 0ah
        mov dx, Handle
        call [XMMControl]
        xor dx, dx
        dec ax
        jz  @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMMoveExtended(var MR : TXMMMoveRec) : LongInt;
Var
  P : Pointer;
Begin
  P := @MR;
  asm
        push ds
        mov XMMError, 0
        mov ax, ds
        mov es, ax
        mov ah, 0bh
        mov ds, word ptr [p+2]
        mov si, word ptr p
        call es:[XMMControl]
        pop ds
        xor dx, dx
        dec ax
        jz  @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMLockExtended(Handle : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 0ch
        mov dx, Handle
        call [XMMControl]
        xchg ax, bx
        dec bx
        jz  @Success
        mov dh, al
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMUnLockExtended(Handle : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 0dh
        mov dx, Handle
        call [XMMControl]
        xor dx, dx
        dec ax
        jz  @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMGetHandleLength(Handle : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 0eh
        mov dx, Handle
        call [XMMControl]
        or  ax, ax
        mov ax, dx
        mov dx, 0
        jnz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMGetHandleInfo(Handle : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 0eh
        mov dx, Handle
        call [XMMControl]
        mov dx, bx
        or  ax, ax
        mov ax, dx
        mov dx, 0
        jnz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMReallocateExtended(Handle, NewSize : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 0fh
        mov dx, Handle
        mov bx, NewSize
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMRequestUMB(Space : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 10h
        mov dx, Space
        call [XMMControl]
        xchg bx, ax
        dec  bx
        jz @Success
        xchg ax, dx
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function XMMReleaseUMB(Segment : Word) : LongInt;
Begin
  asm
        mov XMMError, 0
        mov ah, 11h
        mov dx, Segment
        call [XMMControl]
        xor dx, dx
        dec ax
        jz @Success
        mov dh, bl
        mov XMMError, bl
       @Success:
        mov [bp-4], ax
        mov [bp-2], dx
  end
End;

Function MoveToXMM(Handle : Word; DestOffset, Size : LongInt; Where : Pointer) : LongInt;
Var
  X : TXMMMoveRec;
Begin
  X.Length := Size;
  X.SourceHandle := 0;
  X.SourceOffset := LongInt(Where);
  X.DestHandle   := Handle;
  X.DestOffset   := DestOffset;
  MoveToXMM := XMMMoveExtended(X);
End;

Function MoveFromXMM(Handle : Word; SourceOffset, Size : LongInt; Where : Pointer) : LongInt;
Var
  X : TXMMMoveRec;
Begin
  X.Length := Size;
  X.DestHandle := 0;
  X.DestOffset := LongInt(Where);
  X.SourceHandle   := Handle;
  X.SourceOffset   := SourceOffset;
  MoveFromXMM := XMMMoveExtended(X);
End;

End.

** note from Ralf Brown's interrupt list **
----------2F4300-----------------------------
INT 2F - EXTENDED MEMORY SPECIFICATION (XMS) - INSTALLATION CHECK
        AX = 4300h
Return: AL = 80h XMS driver installed
        AL <> 80h no driver
Notes:  XMS gives access to extended memory and noncontiguous/nonEMS memory
          above 640K
        this installation check DOES NOT follow the format used by other
          software
SeeAlso: AX=4310h
----------2F4310-----------------------------
INT 2F - EXTENDED MEMORY SPECIFICATION (XMS) - GET DRIVER ADDRESS
        AX = 4310h
Return: ES:BX -> driver entry point
Note:   HIMEM.SYS v2.77 chains to previous handler if AH is not 00h or 10h
SeeAlso: AX=4300h

Perform a FAR call to the driver entry point with AH set to the function code
        AH      function
        00h  Get XMS version number
             Return: AX = XMS version (in BCD, AH=major, AL=minor)
                     BX = internal revision number
                     DX = 0001h if HMA (1M to 1M + 64K) exists
                          0000h if HMA does not exist
        01h  Request High Memory Area (1M to 1M + 64K)
             DX = memory in bytes (for TSR or device drivers)
                  FFFFh if application program
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,90h,91h,92h) (see below)
        02h  Release High Memory Area
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,90h,93h) (see below)
        03h  Global enable A20, for using the HMA
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,82h) (see below)
        04h  Global disable A20
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,82h,94h) (see below)
        05h  Local enable A20, for direct access to extended memory
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,82h) (see below)
        06h  Local disable A20
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,82h,94h) (see below)
        07h  Query A20 state
             Return: AX = 0001h enabled
                        = 0000h disabled
                     BL = error code (00h,80h,81h) (see below)
        08h  Query free extended memory, not counting HMA
             BL = 00h (some implementations leave BL unchanged on success)
             Return: AX = size of largest extended memory block in K
                     DX = total extended memory in K
                     BL = error code (00h,80h,81h,A0h) (see below)
        09h  Allocate extended memory block
             DX = Kbytes needed
             Return: AX = 0001h success
                           DX = handle for memory block
                        = 0000h failure
                           BL = error code (80h,81h,A0h) (see below)
        0Ah  Free extended memory block
             DX = handle of block to free
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,A2h,ABh) (see below)
        0Bh  Move extended memory block
             DS:SI -> EMM structure (see below)
             Note: if either handle is 0000h, the corresponding offset is
                   considered to be an absolute segment:offset address in
                   directly addressable memory
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h-82h,A3h-A9h) (see below)
        0Ch  Lock extended memory block
             DX = handle of block to lock
             Return: AX = 0001h success
                           DX:BX = 32-bit linear address of locked block
                        = 0000h failure
                           BL = error code (80h,81h,A2h,ACh,ADh) (see below)
        0Dh  Unlock extended memory block
             DX = handle of block to unlock
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,A2h,AAh) (see below)
        0Eh  Get handle information
             DX = handle for which to get info
             Return: AX = 0001h success
                           BH = block's lock count
                           BL = number of free handles left
                           DX = block size in K
                        = 0000h failure
                           BL = error code (80h,81h,A2h) (see below)
        0Fh  Reallocate extended memory block
             DX = handle of block
             BX = new size of block in K
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,81h,A0h-A2h,ABh) (see below)
        10h  Request upper memory block (nonEMS memory above 640K)
             DX = size of block in paragraphs
             Return: AX = 0001h success
                           BX = segment address of UMB
                           DX = actual size of block
                        = 0000h failure
                           BL = error code (80h,B0h,B1h) (see below)
                           DX = largest available block
        11h  Release upper memory block
             DX = segment address of UMB to release
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,B2h) (see below)
        12h  (XMS v3.0) Reallocate upper memory block
             DX = segment address of UMB to resize
             BX = new size of block in paragraphs
             Return: AX = 0001h success
                        = 0000h failure
                           BL = error code (80h,B0h,B2h) (see below)
        34h  (QEMM 5.11 only, undocumented) ???
        44h  (QEMM 5.11 only, undocumented) ???
        88h  (XMS v3.0) Query free extended memory
             Return: EAX = largest block of extended memory, in K
                     BL = status
                         00h success
                         80h not implemented (i.e. on a 286 system)
                         81h VDISK detected
                         A0h all extended memory allocated
                     ECX = physical address of highest byte of memory
                                (valid even on error codes 81h and A0h)
                     EDX = total Kbytes of extended memory (0 if status A0h)
        89h  (XMS v3.0) Allocate any extended memory
             EDX = Kbytes needed
             Return: AX = 0001h success
                            DX = handle for allocated block (free with AH=0Ah)
                        = 0000h failure
                            BL = status (80h,81h,A0h,A1h,A2h) (see below)
        8Eh  (XMS v3.0) Get extended EMB handle information
             DX = handle
             Return: AX = 0001h success
                            BH = block's lock count
                            CX = number of free handles left
                            EDX = block size in K
                        = 0000h failure
                            BL = status (80h,81h,A2h) (see below)
        8Fh  (XMS v3.0) Reallocate any extended memory block
             DX = unlocked handle
             EBX = new size in K
             Return: AX = 0001h success
                        = 0000h failure
                            BL = status (80h,81h,A0h-A2h,ABh) (see below)
Notes:  HIMEM.SYS requires at least 256 bytes free stack space
        the XMS driver need not implement functions 10h through 12h to be
          considered compliant with the standard

Format of EMM structure:
Offset  Size    Description
 00h    DWORD   number of bytes to move (must be even)
 04h    WORD    source handle
 06h    DWORD   offset into source block
 0Ah    WORD    destination handle
 0Ch    DWORD   offset into destination block
Notes:  if source and destination overlap, only forward moves (source base
          less than destination base) are guaranteed to work properly
        if either handle is zero, the corresponding offset is interpreted
          as a real-mode address referring to memory directly addressable
          by the processor

