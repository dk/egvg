{$F+,X+,T-,V-}
{
            **********************************************
            *                                            *
            *              MEMDEBUG.PAS                  *
            *             DK  Inc.  1996                 *
            *        Built-in debugger/profiler          *
            *          for memory allocations            *
            *                                            *
            **********************************************
}
{ $DEFINE WRITEFULLMEMPROFILE}
{ $DEFINE TRACEGETMEM}
{ $DEFINE TRACECACHE}
Unit MemDebug;

Interface

Const
  UseMemDebug         : Boolean = True;
  WriteFullMemProfile : Boolean = False;
  TraceGetMem         : Boolean = False;
  TraceCache          : Boolean = False;
  TraceObjects        : Boolean = False;

Function MemAlloc(Size : Word) : Pointer;
Procedure FreeMem(P : Pointer; Size : Word);
Procedure GetMem(var P; Size : Word);
Procedure NewCache(var P; Size : Word);
Procedure DisposeCache(var P);

Implementation

Uses DOS, Objects, Memory, EgString, EgInline;

Type
  PHeapRec = ^THeapRec;
  THeapRec = Record
    Next : Pointer;
    Offs : Word;
    Segs : Word;
  End;

  PCall = ^TCall;
  TCall = array[0..3] of Byte;

Var
  F  : Text;
  TC : TCollection;
  OrgNew, OrgDis : PCall;
  Old98, Old99   : Pointer;
  DSize : Word;
  DPtr  : Pointer;
  Retc  : Pointer;
  RetInh : Boolean;


Procedure FInit;
Begin
  Append(F);
End;

Procedure FDone;
Begin
  Close(F);
End;



Procedure WriteMemProfile;
Var
  P, R, C : PHeapRec;
  I, W : Word;
  J : Integer;

Function Valid(P : Pointer)  : Boolean;
Begin
  {$IFDEF DPMI}
  Valid := True;
  {$ELSE}
  Valid := (Seg(P^) <= Seg(HeapPtr^)) and (Seg(P^) > Seg(HeapOrg^));
  {$ENDIF}
End;

Begin
{$IFNDEF DPMI}
  if UseMemDebug and WriteFullMemProfile then begin
    P := FreeList; I := 0;
    C := P;
    Repeat
      if LongInt(P^.Segs) * 16 + P^.Offs > 8 then
      WriteLn(F, 'Free ', HexW(Seg(P^)), Ofs(P^), ' of ', HexW(LongInt(P^.Segs) * 16 + P^.Offs));
      if C <> P then Write(F, '!');
      if Valid(P^.Next) then begin
        R := Normalized(Ptr(Seg(P^) + P^.Segs, Ofs(P^) + P^.Offs));
        W := LongInt(Seg(P^.Next^) - Seg(P^) - P^.Segs) * 16 +
                 Ofs(P^.Next^) - Ofs(P^) - P^.Offs;
        Write(F, 'Busy ', HexW(Seg(R^)), Ofs(R^), ' of ', HexW(W));
        C := Normalized(Ptr(Seg(R^), Ofs(R^) + W));
        J := TC.IndexOf(R);
        if J >= 0 then begin
          J := Integer(TC.At(J+1));
          WriteLn(F, ' - respond ', HexW(J));
          if J > W then WriteLn(F, '!!! range exceeding !!!');
        end else WriteLn(F);
      end;
      P := P^.Next;
      Inc(I);
      if I > 256 then begin
        WriteLn(F, '!Abnormal break.');
        Break;
      end;
    Until not Valid(P);
  WriteLn(F);
  end;
{$ENDIF}
End;

Const
  WorkTC : Boolean = False;

Function  GetCaller : Pointer; Inline(
    $8C/$D0/        {mov ax, ss}
    $8E/$C0/        {mov es, ax}
    $8B/$FD/        {mov di, bp}
    $83/$C7/$02/    {add di, 2}
    $26/$8B/$05/    {mov ax, es:[di]}
    $26/$8B/$55/$02 {mov dx, es:[di+2]}
);


Function MemAlloc(Size : Word) : Pointer;
Var
  P : Pointer;
Begin
  if UseMemDebug then begin
    if Size = 0 then begin
      MemAlloc := Nil;
      Exit;
    end;
    P := Memory.MemAllocSeg(Size);
    MemAlloc := P;
    if (TC.Items = Nil) or WorkTC then Exit;
    if not RetInh then Retc := GetCaller;
    WorkTC := True;
    TC.Insert(P);
    TC.Insert(Pointer(Size));
    WorkTC := False;
    if P = Nil then Exit;
    FInit;
    Write(F, 'Allocated ', HexW(HiWord(LongInt(P))), HexW(LoWord(LongInt(P))), ' of ', HexW(Size),
      '(', HexW(HiWord(LongInt(Retc))), ':', HexW(LoWord(LongInt(Retc))), ')');

    WriteLn(F, ' - avail ', HexL(MemAvail), ' (', MemAvail, ')');
    WriteMemProfile;
    FDone;
  end else MemAlloc := memory.MemAlloc(Size);
  {Matter is some day I cannot catch one of that pesky bugs, which, I think,
  has appeared when someone is writing beyond it's memory block.
  Because memory chunks, allocated thru GetMem (and MemAlloc too)
  do not use each own selector (I mean under DPMI), so writing beyond
  does not cause exc 13, but memAllocSeg do, so for debugging it's
  strongly recommended. BTW, the bug was GetMem(x) and FreeMem(x+1).}
  {-> that to force DPMI set selector limit. asm
         mov ax, 8
         mov bx, word ptr p + 2
         mov cx, 0
         mov dx, Size
         int 31h
  end; that is shorted than
  function MemResizeBlock(Selector: Word; Size: Word): Integer; far;
          external 'RTM' index $0007;
  }
End;

Procedure FreeMem(P : Pointer; Size : Word);
Var
  S : LongInt;
Begin
  if UseMemDebug and not WorkTC then begin
    WorkTC := True;
    if TC.IndexOf(P) >= 0 then begin
      S := Word(TC.At(TC.IndexOf(P)+1));
      TC.AtDelete(TC.IndexOf(P)+1);
      TC.Delete(P);
    end else S  := -1;
    WorkTC := False;
    System.FreeMem(P, Size);
    FInit;
    if not RetInh then Retc := GetCaller;
    Write(F, 'Disposed ', HexW(HiWord(LongInt(P))), HexW(LoWord(LongInt(P))), ' of ', HexW(Size),
      '(', HexW(HiWord(LongInt(Retc))), ':', HexW(LoWord(LongInt(Retc))), ')');
    WriteLn(F, ' - avail ', HexL(MemAvail), ' (', MemAvail, ')');
    if (S > 0) and (S <> Size) then WriteLn(F, '!Size mismatching');
    if S = -1 then WriteLn(F, '!Unhooked pointer');
    WriteMemProfile;
    FDone;
  end else system.FreeMem(P, Size);
End;


Procedure GetMem(var P; Size : Word);
Begin
  if not RetInh then begin
    Retc   := GetCaller;
    RetInh := True;
  end;
  if UseMemDebug and TraceGetMem then Pointer(P) := MemAlloc(Size)
    else system.GetMem(Pointer(P), Size);
  RetInh := False;
End;

Type
  PCache = ^TCache;
  TCache = Record
    Size   : Word;
    Master : ^Pointer;
    Data   : record end;
  End;

Procedure NewCache(var P; Size : Word);
Begin
  if not RetInh then begin
    Retc   := GetCaller;
    RetInh := True;
  end;
  if UseMemDebug and TraceCache then begin
    Pointer(P) := MemAlloc(Size);
    PCache(P)^.Size   := 0;
    PCache(P)^.Master := Nil;
  end else memory.NewCache(Pointer(P), Size);
  RetInh := False;
End;

Procedure DisposeCache(var P);
Var
  S : LongInt;
Begin
  if not RetInh then begin
    Retc   := GetCaller;
    RetInh := True;
  end;
  if UseMemDebug and TraceCache then begin
    if TC.IndexOf(Pointer(P)) >= 0 then S := Word(TC.At(TC.IndexOf(Pointer(P))+1)) else S := -1;
    FreeMem(Pointer(P), S);
    Pointer(P) := Nil;
  end else memory.DisposeCache(Pointer(P));
  RetInh := False;
End;

Procedure NewAcc;
Begin
  WorkTC := True;
  TC.Insert(DPtr);
  TC.Insert(Pointer(DSize));
  WorkTC := False;
  FInit;
  Retc := GetCaller;
  Write(F, 'Allocated object ', HexW(HiWord(LongInt(DPtr))), HexW(LoWord(LongInt(DPtr))), ' of ', HexW(DSize),
      '(', HexW(HiWord(LongInt(Retc))), ':', HexW(LoWord(LongInt(Retc))), ')');
  WriteLn(F, ' - avail ', HexL(MemAvail), ' (', MemAvail, ')');
  WriteMemProfile;
  FDone;
End;

Procedure DisAcc;
Var
  S : LongInt;
Begin
  WorkTC := True;
  if TC.IndexOf(DPtr) >= 0 then begin
    S := Word(TC.At(TC.IndexOf(DPtr)+1));
    TC.AtDelete(TC.IndexOf(DPtr)+1);
    TC.Delete(DPtr);
  end else S  := -1;
  WorkTC := False;
  Retc := GetCaller;
  FInit;
  Write(F, 'Disposed object ', HexW(HiWord(LongInt(DPtr))), HexW(LoWord(LongInt(DPtr))), ' of ', HexW(DSize),
      '(', HexW(HiWord(LongInt(Retc))), ':', HexW(LoWord(LongInt(Retc))), ')');
  WriteLn(F, ' - avail ', HexL(MemAvail), ' (', MemAvail, ')');
  if (S > 0) and (S <> DSize) then WriteLn(F, '!Size mismatching');
  WriteMemProfile;
  FDone;
End;

Procedure CheckNew; Far; Assembler; Asm
  mov  si, [bp + 0ah]
  cmp  si, 1
  jb   @@InheritedConstructor
  mov  ax, [si]
  mov  DSize, ax
  call OrgNew
  mov  word ptr DPtr,   ax
  mov  word ptr DPtr+2, dx
  push ax
  push dx
  pushf
  mov al, UseMemDebug
  and al, TraceObjects
  je @@1
  call NewAcc
@@1:
  popf
  pop dx
  pop ax
@@InheritedConstructor:
  iret
End;

Procedure CheckDis; Far; Assembler; Asm
  push ax
  push si
  push bx
  push es
  les bx, [bp+6]
  mov si, es:[bx+di]
  mov ax, [si]
  mov DSize, ax
  mov ax, word ptr [bp+08h]
  mov word ptr DPtr+2, ax
  mov ax, word ptr [bp+06h]
  mov word ptr DPtr, ax
  pop es
  pop bx
  pop si
  pop ax

  cmp word ptr [bp+0ah], 0
  call OrgDis
  mov al, UseMemDebug
  and al, TraceObjects
  je @@1
  call DisAcc
@@1:
  iret
End;


Procedure InitDynamix;
Begin
  OrgNew := PCall(Pointer(LongInt(@TObject.Init) + 6)^);
  OrgDis := PCall(Pointer(LongInt(@TObject.Done) + 6)^);
  OrgNew^[0] := $CD;
  OrgNew^[1] := $98;
  OrgNew^[2] := $90;
  OrgDis^[0] := $CD;
  OrgDis^[1] := $99;
  OrgDis^[2] := $90;
  OrgDis^[3] := $90;
  Inc(LongInt(OrgNew), 3);
  Inc(LongInt(OrgDis), 4);
  GetIntVec($98, Old98);
  GetIntVec($99, Old99);
  SetIntVec($98, @CheckNew);
  SetIntVec($99, @CheckDis);
End;

Var
  ExitSave : Pointer;

Procedure DoneDynamix; Far;
Begin
  ExitProc := ExitSave;
  SetIntVec($98, Old98);
  SetIntVec($99, Old99);
End;


Begin
  Assign(F, 'PROFILE.MEM');
  Rewrite(F);
  Close(F);
  RetInh := False;
  TC.Init(100, 100);
  {$IFNDEF DPMI}
  InitDynamix;
  ExitSave := ExitProc;
  ExitProc := @DoneDynamix;
  {$ENDIF}
  TraceGetMem := True;
  WriteFullmemProfile := True;
End.