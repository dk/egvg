{$O+,F+,I-,R-,S-,C-,V-}
Unit Streams;

Interface

Uses DOS, Objects, Memory, XMM;

Const
  sttNone         = 0;   {Stream allocation type}
  sttConventional = 1;
  sttCMM          = 1;
  sttEMS          = 2;
  sttEMM          = 2;
  sttRAM          = 7;
  sttXMS          = 4;
  sttXMM          = 4;
  sttDisk         = 8;
  sttDMM          = 8;
  sttAll          = $FF;

Type
  TProcedure = Object
    TVarPtr  : Pointer;
    TVarLong : LongInt;
    Constructor Init;
    Destructor  Done;
    Procedure TProc; Virtual;
  End;
  PProcedure = ^TProcedure;

  TDMMStream = Object(TBufStream)
    Constructor Init(MaxSize : LongInt);
    Destructor  Done; Virtual;
  Private
    FileName : String[90];
  End;
  PDMMStream = ^TDMMStream;

Const
  UseDMM          : Boolean = False;
  Percenter       : PProcedure = Nil;
  EMSPriority     : Boolean = True;
  MaxMemCanLeave  : Word    = 64;      {bound in KB for TMemoryStream}

Function AllocateTmpStream(var P : PStream; Size : LongInt; var Code : Byte):Boolean;
Function AllocateExpandableStream(var P : PStream; Size, MaxSize : LongInt; var Code : Byte):Boolean;
Function StreamExplode(SFrom, STo : PStream) : Integer;
Function StreamImplode(SFrom, STo : PStream) : Integer;
Function  SetPackBuffer : Boolean;
Procedure FreePackBuffer;
Function GetFreeSpace(Where : Byte) : LongInt;
Function PercentMessage(Code : LongInt; Info : Pointer) : LongInt;

Implementation

Const
  PackBuffer : Pointer = Nil;

Procedure OffDiskControl(Disk : Byte);
Begin
  asm
        push ax
        push bx
        push cx
        push bp
        push ds
        pushf
        mov ax, 440Fh
        mov bl, Disk
        int 21h
        popf
        pop ds
        pop bp
        pop cx
        pop bx
        pop ax
      end;
End;


Constructor TDMMStream.Init(MaxSize : LongInt);
Var
  P : Pointer;
  I : LongInt;
  S : Integer;
  T, M : String;
  Disk, MaxDisk : Byte;

Function Long2Str(I : Longint) : String;
Var
  S : String;
Begin
  Str(I, S);
  Long2Str := S;
End;

Begin
  if MaxSize < 0 then begin Fail; Exit; end;

  if Percenter <> Nil then with Percenter^ do begin
    TVarPtr := @Self;
    TVarLong := -4; {disk operations}
    TProc;
  end;
  MaxDisk := 0; I := 0;
  for Disk := 3 to 26 do begin
    if DiskFree(Disk) > I then begin
      OffDiskControl(Disk);
      I := DiskFree(Disk);
      MaxDisk := Disk;
    end;
  end;

  if I < MaxSize then
    for Disk := 1 to 2 do begin
      if DiskFree(Disk) > I then begin
        OffDiskControl(Disk);
        I := DiskFree(Disk);
        MaxDisk := Disk;
      end;
    end;

  Disk := MaxDisk;

  if IOResult <> 0 then;
  GetDir(Disk, T);
  if T[Length(T)] <> '\' then T := T + '\';

  Repeat
    FileName := T + 'SVG' + Long2Str(Random(9999)) + '.$$$';
    M := FileName;
    Inherited Init(FileName, stOpen, 5120);
    S := Status;
    if Buffer = Nil then begin Status := -11; Fail; Exit; end;
    Inherited Done;
  Until S <> stOk;  {hey, new file!}

  Inherited Init(M, stCreate, 5120);
  FileName := M;

  OffDiskControl(Disk);
  if DiskFree(Disk) < MaxSize then begin
    Status := stError;
    Exit;
  end;


  P := Ptr(SegA000, 0);
  for I := 1 to MaxSize div 32768 do begin
    Inherited Write(P^, 32768);
    if Percenter <> Nil then with Percenter^ do begin
      TVarPtr := @Self;
      if MaxSize = 0 then TVarLong := 0 else
        TVarLong := Round(I/(MaxSize div 32768)*100);
      TProc;
    end;
  end;
  Inherited Done;
  Inherited Init(M, stOpen, 5120);
  FileName := M;

  if Percenter <> Nil then with Percenter^ do begin
    TVarPtr := @Self;
    TVarLong := -3; {end of disk operations}
    TProc;
  end;
  if Status <> 0 then Exit;
  Seek(0);
End;

Destructor  TDMMStream.Done;
Var
  S : String;
  F : File;
Begin
  S := FileName;
  Inherited Done;
  Assign(F, S);
  Erase(F);
End;


Function AllocateExpandableStream(var P : PStream; Size, MaxSize : LongInt; var Code : Byte):Boolean;
Var
  L : LongInt;
Begin
  AllocateExpandableStream := False;
  Code := 0;
  P := Nil;
  if (Size < 0) or (MaxSize < 0) then Exit;

  {$IFNDEF DPMI}
  if EMSPriority then begin
   P := New(PEMSStream, Init(Size, MaxSize));
   if P <> Nil then
    if P^.Status <> stOk then begin
        Dispose(P, Done);
        P := Nil;
      end else begin
        Code := sttEMS;
        AllocateExpandableStream := True;
        Exit;
      end;


  if XMMInstalled and (GetFreeSpace(sttXMM) >= MaxSize) then begin
    P := New(PXMSStream, Init(Size));
    if P <> Nil then
      if P^.Status <> stOk then begin
          Dispose(P, Done);
          P := Nil;
        end else begin
          Code := sttXMS;
          AllocateExpandableStream := True;
          Exit;
        end;
   end;
  end else begin
   if XMMInstalled and (GetFreeSpace(sttXMM) >= MaxSize)then begin
    P := New(PXMSStream, Init(Size));
    if P <> Nil then
      if P^.Status <> stOk then begin
          Dispose(P, Done);
          P := Nil;
        end else begin
          Code := sttXMS;
          AllocateExpandableStream := True;
          Exit;
        end;
   end;

   P := New(PEMSStream, Init(Size, MaxSize));
   if P <> Nil then
    if P^.Status <> stOk then begin
        Dispose(P, Done);
        P := Nil;
      end else begin
        Code := sttEMS;
        AllocateExpandableStream := True;
        Exit;
      end;
 end;
  {$ENDIF}

  if (MemAvail >= MaxSize) and (((MemAvail - MaxSize) div 1024) > MaxMemCanLeave) then begin
    L := Size;
    if L > 64000 then L := MaxAvail div 2;
    P := New(PMemoryStream, Init(Size, L));
    if P <> Nil then
      if P^.Status <> stOk then begin
        Dispose(P, Done);
        P := Nil;
      end else begin
          Code := sttConventional;
          AllocateExpandableStream := True;
          Exit;
        end;
  end;


  if not(UseDMM) then Exit;
  P := New(PDMMStream, Init(Size));
  if P = Nil then Exit;
  if P^.Status <> stOk then begin
    Dispose(P, Done);
    P := Nil;
   end else begin
      Code := sttDisk;
      AllocateExpandableStream := True;
      Exit;
    end;
End;

Function AllocateTmpStream(var P : PStream; Size : LongInt; var Code : Byte):Boolean;
Begin
  AllocateTmpStream := AllocateExpandableStream(P, Size, Size, Code);
End;

{$L IMPLODE.OBJ}

Const
  CMP_Binary     = 0;
  CMP_ASCII      = 1;
  PackBufferSize = 35256;

Type
  TBuffer = Packed array [1..PackBufferSize] of Char;
  PBuffer = ^TBuffer;

  IntFunc = Function(var Buff : TBuffer; var Size : Word): Word;
   { receives buffer, its size and returns really handled size }

Function Implode(Read      : IntFunc;       { function to get data }
                 Write     : IntFunc;       { function to set data }
                 var Buf   : TBuffer;       { buffer }
                 var cType : Word;          { compare type - Binary/ASCII }
                 var bSize : Word)          { dictionary size (near 4096) }
                 : Integer; External;

Function Explode(Read    : IntFunc;         { function to get data }
                 Write   : IntFunc;         { function to set data }
                 var Buf : TBuffer)         { buffer }
                 : Integer; External;
Var
  xSFrom, xSTo : PStream;

Function xRead(var Buff : TBuffer; var Size : Word): Word; Far;
Var
  Pos       : LongInt;
  SSize     : LongInt;
Begin
   Pos := xSFrom^.GetPos;
   SSize := xSFrom^.GetSize;
   if Pos + Size >= SSize then
     Size := SSize  - Pos;
   xSFrom^.Read(Buff, Size);
   if xSFrom^.Status <> 0 then xSFrom^.Reset;
   xRead := Size;
   if Percenter <> Nil then with Percenter^ do begin
     TVarLong := Round((xSFrom^.GetPos+Size)/xSFrom^.GetSize*100);
     {if TVarLong > 100 then TVarLong := 100;}
     TProc;
   end;
End;

Function xWrite(var Buff : TBuffer; var Size : Word): Word; Far;
Var
  Pos       : LongInt;
Begin
  Pos := xSTo^.GetPos;
  xSTo^.Write(Buff, Size);
  if xSTo^.Status <> 0 then xSTo^.Reset;
  xWrite := xSTo^.GetPos - Pos;
  if Percenter <> Nil then with Percenter^ do begin
     TVarLong := Round((xSto^.GetPos+Size)/xSto^.GetSize*100);
     {if TVarLong > 100 then TVarLong := 100;}
     TProc;
   end;
End;

Function StreamExplode(SFrom, STo : PStream) : Integer;
Begin
  if PackBuffer = Nil then begin
    StreamExplode := 100;
    Exit;
  end;
  xSFrom := SFrom;
  xSTo   := STo;
  if Percenter <> Nil then with Percenter^ do begin
     TVarLong := -1;    {Starting explode}
     TProc;
  end;
  StreamExplode := Explode(xRead, xWrite, TBuffer(PackBuffer^));
  if Percenter <> Nil then with Percenter^ do begin
     TVarLong := -3;    {Ending explode/implode}
     TProc;
  end;
End;

Function StreamImplode(SFrom, STo : PStream) : Integer;
Var
  CMP    : Word;
  Dict   : Word;
Begin
  if PackBuffer = Nil then begin
    StreamImplode := 100;
    Exit;
  end;
  xSFrom := SFrom;
  xSTo   := STo;
  CMP  := CMP_Binary;
  Dict := 4096;
  if Percenter <> Nil then with Percenter^ do begin
     TVarLong := -2;    {Starting implode}
     TProc;
  end;
  StreamImplode := Implode(xRead, xWrite, TBuffer(PackBuffer^), CMP, Dict);
  if Percenter <> Nil then with Percenter^ do begin
     TVarLong := -3;    {Ending explode/implode}
     TProc;
  end;
End;

Function  SetPackBuffer : Boolean;
Begin
  if PackBuffer <> Nil then Exit;
  PackBuffer := MemAlloc(PackBufferSize);
  SetPackBuffer := PackBuffer <> Nil;
End;

Procedure FreePackBuffer;
Begin
  if PackBuffer <> Nil then FreeMem(PackBuffer, PackBufferSize);
  PackBuffer := Nil;
End;

function EmsInstalled : Boolean;
    {-Returns true if the Expanded Memory Manager is installed.}
var
  F : file;
begin
  Assign(F, 'EMMXXXX0');
  Reset(F);
  if IoResult = 0 then begin
    EmsInstalled := True;
    Close(F);
  end
  else
    EmsInstalled := False;
end;

Function GetFreeSpace(Where : Byte) : LongInt;
Var
  C, E, D, X, Ret : LongInt;
  I            : Byte;
Begin
  C   := MemAvail;
  if C < 0 then C := 0;
  E   := 0;
  D   := 0;
  Ret := 0;

  if (Where and sttDMM) <> 0 then for I := 1 to 26 do begin
    OffDiskControl(I);
    X := DiskFree(I);
    if X < 0 then X := 0;
    Inc(D, X);
  end;

  if EmsInstalled then begin
    E := 0;
    asm
      mov ax, 4200h
      int 67h
      mov word ptr e+2, bx
    end;
    E := E * 16384;
  end;

  if XMMInstalled then begin
    X := XMMQueryTotalFree;
    if X < 0 then X := 0 else X := X * 1024;
  end else X := 0;

  if (Where and sttCMM) <> 0 then Inc(Ret, C);
  if (Where and sttEMM) <> 0 then Inc(Ret, E);
  if (Where and sttXMM) <> 0 then Inc(Ret, X);
  if (Where and sttDMM) <> 0 then Inc(Ret, D);
  GetFreeSpace := Ret;
End;

Constructor TProcedure.Init;
Begin
End;

Destructor TProcedure.Done;
Begin
End;

Procedure TProcedure.TProc;
Begin
End;

Function HeapFunc(Size : Word) : Integer; Far;
Begin
  HeapFunc := 1;
End;

Function PercentMessage(Code : LongInt; Info : Pointer) : LongInt;
Begin
  if Percenter <> Nil then with Percenter^ do begin
    TVarLong := Code;
    TVarPtr  := Info;
    TProc;
    PercentMessage := TVarLong;
  end;
End;

Begin
  HeapError := @HeapFunc;
  PackBuffer := Nil;
End.