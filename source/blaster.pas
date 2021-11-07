{$F+,I-,S-,R-,V-,G+}
{
  €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€
  € Unit        : Sound Subsystem                                      €
  € Description : Sound driver for Sound Blaster                       €
  € Author      : Dmitry Karasik portions copyright Mark Feldman       €
  € Version     : X01.00 (internal)                                    €
  € Release     : 01.00                                                €
  € Last update : 26-DEC-1996                                          €
  €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€

}

Unit Blaster;

Interface

Uses Objects, WavePlay;


Const
  {blaster models}
  bm1x     = 1;
  bmPro    = 2;
  bm2      = 3;
  bmPro2   = 4;
  bmProMCV = 5;
  bmAWE32  = 6;

  BlasterPort         : Byte = 2;
  BlasterIRQ          : Byte = 7;
  BlasterDMAChannel8  : Byte = 1;
  BlasterDMAChannel16 : Byte = 7;
  BlasterMIDIAddr     : Word = $300;
  BlasterModel        : Byte = bm1x;
  IntrMasked          : Boolean = False;


Function  BlasterInit : Boolean;
Procedure BlasterDone;
Function  InitWave(S : PStream) : Word;
Procedure SetRate(Rate : Word);
Procedure StartSound;
Procedure StopSound;
Procedure PauseSound;
Procedure ContinueSound;

function ResetDSP(Base : Word; Safe : Boolean) : boolean;

Implementation

Uses DOS;

var      DSP_RESET : word;
     DSP_READ_DATA : word;
    DSP_WRITE_DATA : word;
  DSP_WRITE_STATUS : word;
    DSP_DATA_AVAIL : word;
Const
  DOS_Seg      : word = 0;
  DPMI_Seg     : word = 0;
  IRQAddc      : byte = 0;

Var
  OldIrq, AuxBuffer : Pointer;
  LastRead : Word;

function ResetDSP(Base : Word; Safe : Boolean) : boolean;

Procedure XDelay; Assembler; Asm
    mov cx, 200
@@1:loop @@1
End;

Procedure XDelay2; Assembler; Asm
    mov cx, 20000
@@1:loop @@1
End;


var
  i,j:Byte;


begin
  base := base * $10;
 { Calculate the port addresses }
  DSP_RESET := base + $206;
  DSP_READ_DATA := base + $20A;
  DSP_WRITE_DATA := base + $20C;
  DSP_WRITE_STATUS := base + $20C;
  DSP_DATA_AVAIL := base + $20E;

  { Reset the DSP, and give some nice long delays just to be safe }
  Port[DSP_RESET] := 1;
  for I := 1 to 10 do j := Port[DSP_RESET];
  if not Safe then XDelay else XDelay2;
  Port[DSP_RESET] := 0;
  for I := 1 to 10 do j := Port[DSP_RESET];
  if not Safe then XDelay else XDelay2;
  if (Port[DSP_DATA_AVAIL] And $80 = $80) And
     (Port[DSP_READ_DATA] = $AA) then
    ResetDSP := true
  else
    ResetDSP := false;
end;

procedure WriteDSP(value : byte);
begin
  while Port[DSP_WRITE_STATUS] And $80 <> 0 do;
  Port[DSP_WRITE_DATA] := value;
end;

function ReadDSP : byte;
begin
  while Port[DSP_DATA_AVAIL] and $80 = 0 do;
  ReadDSP := Port[DSP_READ_DATA];
end;

procedure WriteDAC(level : byte);
begin
  WriteDSP($10);
  WriteDSP(level);
end;

function ReadDAC : byte;
begin
  WriteDSP($20);
  ReadDAC := ReadDSP;
end;

function SpeakerOn: byte;
begin
  WriteDSP($D1);
end;

function SpeakerOff: byte;
begin
  WriteDSP($D3);
end;

procedure ContinueSound;
begin
  if not PlayingSound then Exit;
  WriteDSP($D4);
  PlayingSound := True;
  if @SoundNotify <> Nil then SoundNotify;
end;

procedure PauseSound;
begin
  if not PlayingSound then Exit;
  WriteDSP($D0);
  PlayingSound := False;
  if @SoundNotify <> Nil then SoundNotify;
end;

Procedure Refresh(FirstCall : Boolean);
Var
  L, P : LongInt;
  I, page, offset : word;
  time_constant : word;
Begin
  if (SoundStream = nil) or not PlayingSound then Exit;
  SoundBufPos := 0;
  if FirstCall or (not FirstCall and (LastRead = 0)) then begin
    L := SoundHeader.DataSize + 8; {SoundStream^.GetSize;}
    P := SoundStream^.GetPos;
    if (L <= P) or (SoundStream^.Status <> stOk) then begin
      SoundPos := L;
      StopSound;
      Exit;
    end;
    SoundPos := P;
    if @SoundNotify <> Nil then SoundNotify;
    if P + SoundBufSize > L then I := L - P else I := SoundBufSize;
    SoundStream^.Read(SoundBuffer^, I)
  end else begin
    Move(AuxBuffer^, SoundBuffer^, SoundBufSize);
    I := LastRead;
  end;
  {$IFNDEF DPMI}
  offset := Seg(SoundBuffer^) Shl 4 + Ofs(SoundBuffer^);
  page := (Seg(SoundBuffer^) + Ofs(SoundBuffer^) shr 4) shr 12;
  {$ELSE}
  offset := DOS_Seg Shl 4;
  page := DOS_Seg shr 12;
  asm
    push ds
    cld
    mov dx, DPMI_Seg
    mov es, dx
    lds si, SoundBuffer
    xor di, di
    mov cx, i
    rep movsb
    pop ds
  end;
  {$ENDIF}

  asm cli end;

  if BlasterIRQ < 8 then begin
    IntrMasked := ((1 shl BlasterIRQ) and Port[$21]) <> 0;
    Port[$21] := (not(1 shl BlasterIRQ)) and Port[$21];
  end else begin
    IntrMasked := ((1 shl (BlasterIRQ - 8)) and Port[$A1]) <> 0;
    Port[$A1] := not(1 shl (BlasterIRQ - 8)) and Port[$A1];
  end;
  asm sti end;
  ResetDSP(BlasterPort, FirstCall);
  SpeakerOn;

  Port[$0A] := 4 or BlasterDMAChannel8;
  Port[$0C] := 0;
  Port[$0B] := $48 or BlasterDMAChannel8;
  Port[$02] := Lo(offset);
  Port[$02] := Hi(offset);
  Port[$83] := page;
  Port[$03] := Lo(i);
  Port[$03] := Hi(i);
  Port[$0A] := BlasterDMAChannel8;

  { Set the playback frequency }
  time_constant := 256 - 1000000 div ActualFreq;
  WriteDSP($40);
  WriteDSP(time_constant);

  { Set the playback type (8-bit) }
  WriteDSP($14);
  WriteDSP(Lo(i));
  WriteDSP(Hi(i));

  L := SoundHeader.DataSize + 8; {SoundStream^.GetSize;}
  P := SoundStream^.GetPos;
  if P + SoundBufSize > L then I := L - P else I := SoundBufSize;
  if not FirstCall then begin
    SoundPos := SoundStream^.GetPos;
    if @SoundNotify <> Nil then SoundNotify;
  end;
  if I > 0 then SoundStream^.Read(AuxBuffer^, I);
  LastRead := I;
End;



Procedure IRQHandler; Interrupt;
Begin
  if BlasterIRQ < 8 then Port[$20] := $20 else Port[$A0] := $20;
  SoundBufPos := SoundBufSize;
  Refresh(False);
End;


Procedure StopSound;
Begin
  if not PlayingSound then Exit;
  PauseSound;
  PlayingSound := False;
  {$IFDEF DPMI}
  if DPMI_Seg <> 0 then asm
    mov ax, 101h
    mov dx, DPMI_Seg
    int 31h
  end;
  DPMI_Seg := 0;
  {$ELSE}
  if SoundBuffer <> nil then FreeMem(SoundBuffer, SoundBufSize);
  if AuxBuffer <> nil then FreeMem(AuxBuffer, SoundBufSize);
  {$ENDIF}

  SetIntVec(BlasterIRQ + IRQAddc, OldIRQ);
  if IntrMasked then
    if BlasterIRQ < 8 then
      Port[$21] := (1 shl BlasterIRQ) or Port[$21]
    else
      Port[$A1] := (1 shl (BlasterIRQ - 8)) or Port[$A1];
  IntrMasked := False;
  asm sti end;
  if SoundStream <> nil then Dispose(SoundStream, Done);
  if @SoundNotify <> Nil then SoundNotify;
End;


Procedure StartSound;
var
  Size : word;
begin
  if (SoundStream = nil) or PlayingSound then Exit;
  StopSound;
  if SoundBufSize > MaxAvail then Exit;
  GetMem(SoundBuffer, SoundBufSize);
  GetMem(AuxBuffer, SoundBufSize);
  SoundRead:=0;
  SoundStream^.Reset;
  SoundStream^.Seek(SoundStart);
  PlayingSound:=True;
  size := SoundBufSize;
  {$IFDEF DPMI}
  asm
    mov ax, 0100h
    mov bx, Size
    shr bx, 4
    inc bx
    int 31h
    mov DOS_Seg, ax
    mov DPMI_Seg, dx
  end;
  {$ENDIF}
  GetIntVec(BlasterIRQ + IRQAddc, OldIRQ);
  SetIntVec(BlasterIRQ + IRQAddc, @IRQHandler);
  Refresh(True);
end;

Procedure SetRate;
Begin
End;

Function  InitWave;
Begin
  InitWave := bsOk;
End;

Procedure IrqSetup; Far; Assembler; Asm
  push ds
  push ax
  mov ax, seg @data
  mov ds, ax
  mov BlasterIRQ, dl
  mov dl, BlasterPort
  xor dh, dh
  shl dx, 4
  add dx, 20Eh {DX = DSP data available port 2xEh}
  in al, dx   {acknowledge DSP interrupt}
  mov al, 20h
  cmp BlasterIRQ, 7
  ja @@Above7
  out 20h, al
  jmp @@Next
@@Above7:
  out 0A0h, al
@@Next:
  pop ax
  pop ds
  iret
End;

Procedure Irq2;  Far; Assembler; Asm mov dx, 2; jmp IrqSetUp End;
Procedure Irq3;  Far; Assembler; Asm mov dx, 3; jmp IrqSetUp End;
Procedure Irq5;  Far; Assembler; Asm mov dx, 5; jmp IrqSetUp End;
Procedure Irq7;  Far; Assembler; Asm mov dx, 7; jmp IrqSetUp End;
Procedure Irq9;  Far; Assembler; Asm mov dx, 9; jmp IrqSetUp End;
Procedure Irq10; Far; Assembler; Asm mov dx, 10; jmp IrqSetUp End;
Procedure Irq11; Far; Assembler; Asm mov dx, 11; jmp IrqSetUp End;
Procedure Irq12; Far; Assembler; Asm mov dx, 12; jmp IrqSetUp End;



Function  BlasterInit : Boolean;
Label NoWay;
Const
  MaxInt = 8;
  Intmap  : array [1..MaxInt] of Byte = (2, 3, 5, 7, 9, 10, 11, 12);
  IrqChk  : array [1..MaxInt] of Pointer =  (@Irq2, @Irq3, @Irq5, @Irq7, @Irq9, @Irq10, @Irq11, @Irq12);
Var
  IntSave : array [1..MaxInt] of Pointer;
  IntrMasked : array[1..MaxInt] of Boolean;
  I, J:Integer;
  WaitLine : LongInt;

Var
  S : String;
  P : Integer;

Function Parm(What : Char) : Integer;
Var
  I, J : Integer;
  Z : String;
Begin
  Parm := -1;
  I := Pos(What, S);
  if I = 0 then Exit;
  Z := '';
  Inc(I);
  while (I <= Length(S)) and (S[I] <> ' ') do begin
    Z := Z + S[I];
    Inc(I);
  end;
  Val(Z, I, J);
  if J <> 0 then Exit;
  Parm := I;
End;

Begin
  BlasterInit := false;
  S := GetEnv('BLASTER');
  P := Parm('A');
  if P > 0 then BlasterPort := (P - 200) div 10;
  P := Parm('I');
  if P > 0 then BlasterIRQ := P;
  P := Parm('D');
  if P > 0 then BlasterDMAChannel8 := P;
  P := Parm('H');
  if P > 0 then BlasterDMAChannel16 := P;
  P := Parm('P');
  if P > 0 then BlasterMIDIAddr := P;
  P := Parm('T');
  if P > 0 then BlasterModel := P;

  for BlasterPort := 1 to 6 do
    if ResetDsp(BlasterPort, True) then Break;
  if not ResetDsp(BlasterPort, True) then Exit;
  for I := 1 to MaxInt do begin
    if IntMap[I] < 8 then IRQAddc := 8 else IRQAddc := $68;
    GetIntVec(Intmap[I] + IRQAddc, IntSave[I]);
    SetIntVec(Intmap[I] + IRQAddc, IrqChk[I]);
    asm cli end;
    if IntMap[I] < 8 then begin
      IntrMasked[I] := ((1 shl IntMap[I]) and Port[$21]) <> 0;
      Port[$21] := (not(1 shl IntMap[I])) and Port[$21];
    end else begin
      IntrMasked[I] := ((1 shl (IntMap[I] - 8)) and Port[$A1]) <> 0;
      Port[$A1] := not(1 shl (IntMap[I] - 8)) and Port[$A1];
    end;
    asm sti end;
  end;
  BlasterIRQ := 0;
  WaitLine := 0;
  while (Port[DSP_WRITE_STATUS] and $80) <> 0 do begin
    Inc(WaitLine);
    if WaitLine > 2000 then begin
      BlasterInit := False;
      BlasterIRQ := 0;
      Goto NoWay;
    end;
  end;
  Port[DSP_WRITE_DATA] := $F2;
  for I := 1 to 2550 do
    if BlasterIRQ <> 0 then Break;
NoWay:
  for I := 1 to MaxInt do begin
    asm cli end;
    if IntrMasked[I] then
      if IntMap[I] < 8 then
        Port[$21] := 1 shl IntMap[I] or Port[$21]
      else
        Port[$A1] := 1 shl (IntMap[I] - 8) or Port[$A1];
    asm sti end;
    if IntMap[I] < 8 then IRQAddc := 8 else IRQAddc := $68;
    SetIntVec(Intmap[I] + IRQAddc, IntSave[I]);
  end;
  BlasterInit := BlasterIRQ <> 0;
  if BlasterIRQ < 8 then IRQAddc := 8 else IRQAddc := $68;
End;

Procedure BlasterDone;
Begin
  ResetDsp(BlasterPort, True);
End;

Const
  DriverMethods : TSoundDriverMethods = (
    _Init          : BlasterInit;
    _Done          : BlasterDone;
    _InitWave      : InitWave;
    _SetRate       : SetRate;
    _StartSound    : StartSound;
    _StopSound     : StopSound;
    _PauseSound    : PauseSound;
    _ContinueSound : ContinueSound
  );
  BlasterSoundDriver   : TSoundDriver = (
    DeviceType         : sdSoundBlaster;
    DeviceName         : 'Sound Blaster';
    ID                 : 0;
    Methods            : @DriverMethods
  );

Begin
  RegisterSoundDriver(@BlasterSoundDriver);
End.