{$F+,I-,S-,R-}
{
  ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
  Û Unit        : Sound Subsystem                                      Û
  Û Description : Sound drivers for PC Speaker & Covox                 Û
  Û Author      : Dmitry Karasik portions copyright Sasha Peslyak      Û
  Û Version     : X01.00 (internal)                                    Û
  Û Release     : 01.00                                                Û
  Û Last update : 26-DEC-1996                                          Û
  ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÛ

}

Unit Speaker;

Interface

Uses Objects, WavePlay;


Const
  CovoxPort : Byte = 1;

Function  SpeakerInit : Boolean;
Procedure SpeakerDone;
Function  InitWave(S : PStream) : Word;
Procedure SetRate(Rate : Word);
Procedure StartSound;
Procedure StopSound;
Procedure PauseSound;
Procedure ContinueSound;

Implementation

Uses DOS;

const
  LastTimer : Pointer= Nil;
  TimerInt =  8;
Var
  TimerRate,
  TimerDiv,
  TimerIndex,
  TimerDelay   :Word;
const
   SineTable    :Array [0..$FF] of Byte=
  (1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  2,
   2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
   3,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  5,
   5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  6,  6,  6,  6,  6,
   6,  6,  6,  6,  6,  6,  6,  6,  7,  7,  7,  7,  7,  7,  7,
   7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
   8,  8,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 11, 11,
  12, 12, 13, 14, 14, 15, 16, 17, 17, 18, 19, 20, 21, 22, 23,
  24, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 38, 39, 40, 41,
  43, 44, 45, 46, 48, 49, 50, 51, 52, 53, 54, 55, 57, 58, 58,
  59, 60, 61, 62, 63, 64, 64, 65, 66, 66, 67, 67, 67, 67, 67,
  67, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 69,
  69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 70, 70, 70, 70, 70,
  70, 70, 70, 70, 70, 70, 70, 70, 71, 71, 71, 71, 71, 71, 71,
  71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,
  72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74,
  74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 75, 75, 75, 75,
  75, 75);

procedure SetRate;
assembler;
asm
   cli
   mov  cx,Rate
   mov  TimerRate,cx
   cmp  cx,18
   jbe  @@1
   mov  dx,12h
   mov  ax,34DCh
   div  cx
   mov  TimerDiv,ax

   xor  ax,ax
   mov  dx,1
   div  TimerDiv
   mov  TimerDelay,ax
   jmp  @@2
@@1:
   mov  TimerDiv,0
   mov  TimerDelay,1
@@2:
   sti
end;

Function  GetByte : Byte;
Begin
  if SoundBufPos>=SoundRead then begin
    GetByte:=0;
    if SoundCheckDos and DosActive^ then Exit;
    asm
       mov  al,20h
       out  20h,al
    end;
    SoundRead := SoundBufSize;
    if SoundRead > SoundSize - SoundPos then
      SoundRead := SoundSize - SoundPos;
    Inc(SoundPos, SoundRead);
    SoundStream^.Read(SoundBuffer^, SoundRead);
    SoundBufPos := 0;
    if @SoundNotify <> Nil then SoundNotify;

    if (SoundRead = 0) or (SoundStream^.Status <> stOk) then begin
      SoundPos := SoundSize;
      StopSound;
      Exit;
    end;
  end;

  asm
    les  bx,SoundBuffer
    add  bx,SoundBufPos
    mov  al,es:[bx]
    mov  SoundValue,al
    inc  SoundBufPos
  end;
  GetByte := SoundValue;
End;

Procedure InitTimer; Assembler;
Asm
  mov  al,0B0h
  out  43h,al
  mov  al,1
  out  42h,al
  dec  al
  out  42h,al

  in   al,61h
  or   al,3
  out  61h,al

  mov  al,90h
  out  43h,al
End;

Var
  Working : Boolean;


Procedure TimerProcSpeaker; Interrupt;
Begin
   asm
      cmp  Working,0
      jnz  @@8
      mov  Working,1

      cmp  PlayingSound,0
      jnz  @@1
      pushf
      call dword ptr LastTimer
      jmp  @@9
@@1:
      dec  TimerIndex
      jne  @@2
      mov  ax,TimerDelay
      mov  TimerIndex,ax
      pushf
      call dword ptr LastTimer

      call InitTimer
@@2:
      call GetByte

      cmp  SoundVolume,64
      je   @@3
      sub  al,80h
      cbw
      mul  SoundVolume
      mov  cl,6
      shr  ax,cl
      add  al,80h
@@3:
      push ds
      pop  es
      mov  bx,offset SineTable
      xlat
      out  42h,al
@@9:
      mov  Working,0
@@8:
      mov  al,20h
      out  20h,al
   end;
End;

Procedure TimerProcCovox; Interrupt;
Begin
   asm
      cmp  Working,0
      jnz  @@8
      mov  Working,1

      cmp  PlayingSound,0
      jnz  @@1
      pushf
      call dword ptr LastTimer
      jmp  @@9
@@1:
      dec  TimerIndex
      jne  @@2
      mov  ax,TimerDelay
      mov  TimerIndex,ax
      pushf
      call dword ptr LastTimer

      call InitTimer
@@2:
      call GetByte

      cmp  SoundVolume,64
      je   @@3
      sub  al,80h
      cbw
      mul  SoundVolume
      mov  cl,6
      shr  ax,cl
      add  al,80h
@@3:
      cmp  CovoxPort, 1
      jne  @@6
      mov  dx,378h
      jmp  @@7
@@6:
      mov  dx,278h
@@7:
      out  dx,al
@@9:
      mov  Working,0
@@8:
      mov  al,20h
      out  20h,al
   end;
End;


Procedure StartSound;
Begin
  if SoundStream = nil then Exit;
  StopSound;

  if SoundBufSize > MaxAvail then Exit;
  GetMem(SoundBuffer, SoundBufSize);
  SoundRead := 0;

  SoundStream^.Reset;
  SoundStream^.Seek(SoundStart);
  SoundPos := 0;

  GetIntVec(TimerInt, LastTimer);

  TimerIndex := TimerDelay;
  InitTimer;

  PlayingSound:=True;
  ContinueSound;
  Working := False;
End;


procedure StopSound;
begin
  if not PlayingSound then Exit;
  PauseSound;
  PlayingSound:=False;
  if SoundBuffer<>nil then FreeMem(SoundBuffer, SoundBufSize);
  SoundBuffer := nil;
  if SoundStream <> nil then Dispose(SoundStream, Done);
  if @SoundNotify <> Nil then SoundNotify;
end;

Procedure PauseSound;
Begin
  if not PlayingSound then Exit;
  asm
     mov  al,36h
     out  43h,al
     xor  ax,ax
     out  40h,al
     out  40h,al

     in   al,61h
     and  al,0FCh
     out  61h,al
  end;
  SetIntVec(TimerInt, LastTimer);
  if @SoundNotify <> Nil then SoundNotify;
End;

Procedure ContinueSound;
Var
  TProc : Pointer;
Begin
  if not PlayingSound then Exit;
  if SoundDriver^.ID = 0 then TProc := @TimerProcSpeaker
    else TProc := @TimerProcCovox;
  SetIntVec(TimerInt, TProc);
  if TimerDiv <> 0 then asm
     mov  al,36h
     out  43h,al
     mov  ax,TimerDiv
     out  40h,al
     mov  al,ah
     out  40h,al
  end;
  if @SoundNotify <> Nil then SoundNotify;
End;

Function SpeakerInit;
Begin
  SpeakerInit := True;
End;

Procedure SpeakerDone;
Begin
End;

Function InitWave;
Begin
  InitWave := bsOk;
End;


Const
  DriverMethods : TSoundDriverMethods = (
    _Init          : SpeakerInit;
    _Done          : SpeakerDone;
    _InitWave      : InitWave;
    _SetRate       : SetRate;
    _StartSound    : StartSound;
    _StopSound     : StopSound;
    _PauseSound    : PauseSound;
    _ContinueSound : ContinueSound
  );
  PCSpeakerSoundDriver : TSoundDriver = (
    DeviceType         : sdPCSpeaker;
    DeviceName         : 'PC Speaker';
    ID                 : 0;
    Methods            : @DriverMethods
  );
  CovoxSoundDriver     : TSoundDriver = (
    DeviceType         : sdCovox;
    DeviceName         : 'Covox';
    ID                 : 1;
    Methods            : @DriverMethods
  );

Begin
  RegisterSoundDriver(@PCSpeakerSoundDriver); {1st as default}
  RegisterSoundDriver(@CovoxSoundDriver);
End.
