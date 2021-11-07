{$F+,I-,R-,S-,V-}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : Sound subsystem                                      █
  █ Description : Sound Device Interface                               █
  █ Author      : Dmitry Karasik, portions copyright Sasha Peslyak     █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 26-DEC-1996                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█

}
Unit WavePlay;

Interface

Uses Objects;

Const
  sdNone         = 0;
  sdPCSpeaker    = 1;
  sdCovox        = 2;
  sdSoundBlaster = 3;
  sdUltraSound   = 4;

Type
  PSoundDriverMethods = ^TSoundDriverMethods;
  TSoundDriverMethods = Record
    _Init          : Function : Boolean;
    _Done          : Procedure;
    _InitWave      : Function (S : PStream) : Word;
    _SetRate       : Procedure (Rate : Word);
    _StartSound    : Procedure;
    _StopSound     : Procedure;
    _PauseSound    : Procedure;
    _ContinueSound : Procedure;
  End;

  PSoundDriver = ^TSoundDriver;
  TSoundDriver = Record
    DeviceType         : Word;
    DeviceName         : String[40];
    ID                 : Word;
    Methods            : PSoundDriverMethods;
    NextDriver         : PSoundDriver;
  end;


  TSoundHeader = Record
    IdRIFF    : array [1..4] of Char;
    FileSize  : LongInt;
    IdWAVEfmt : array [1..8] of Char;
    sLength   : LongInt;
    Format    : Word;
    Modus     : Word;
    Frequency : LongInt;
    BytePS    : LongInt;
    ByteSam   : Word;
    BitSam    : Word;
    IdData    : array [1..4] of Char;
    DataSize  : LongInt;
  End;

Const
  bsOk        = 0;
  bsInvFormat = 1;
  bsReadError = 2;

  SoundBufSize : Word = 16384;
  SoundVolume  : Word = 64;
  PlayingSound : Boolean = False;
  SoundDriver  : PSoundDriver = Nil;
  SoundNotify  : Procedure = Nil;
  SoundFileId  : Array [1..4] of Char = 'RIFF';


var
  DriverMethods : TSoundDriverMethods;
  DefaultDriver : PSoundDriver;
  SoundDrivers  : PSoundDriver;

  SoundStream   : PStream;
  SoundCheckDos : Boolean;
  SoundBuffer   : Pointer;
  SoundRead,
  SoundBufPos   : Word;
  SoundStart,
  SoundSize,
  SoundPos      : LongInt;
  SoundValue    : Byte;
  DosActive     : ^Boolean;
  SoundHeader   : TSoundHeader;
  ActualFreq    : LongInt;


Procedure PlayWavFile(FileName : String; BackGround : Boolean);

Function  InitSound : Boolean;
Procedure DoneSound;
Function  InitWaveSound(S : PStream) : Word;
Procedure SetRate(Rate : Word);
Procedure StartSound;
Procedure StopSound;
Procedure PauseSound;
Procedure ContinueSound;

Procedure RegisterSoundDriver(Driver : PSoundDriver);
Procedure SelectSoundDriver(Driver : PSoundDriver);
Function  GetSoundDriver(ADeviceType : Byte) : PSoundDriver;

Implementation

Function InitSound : Boolean;
Var
  Ok : Boolean;
Begin
  if SoundDriver = Nil then SelectSoundDriver(DefaultDriver);
  Ok := DriverMethods._Init;
  InitSound := Ok;
  if not Ok then begin
    SelectSoundDriver(GetSoundDriver(sdNone));
    DriverMethods._Init;
  end;
End;

Procedure DoneSound;
Begin
  StopSound;
  DriverMethods._Done;
  SoundDriver       := Nil;
End;

Procedure FailSound; Far;
Begin
  asm
    mov ax, 3
    int 10h
  end;
  {$IFDEF RUSSIAN}
  WriteLn('(!) �㤨��ࠩ��� �� �� ���樠����஢��');
  {$ELSE}
  WriteLn('(!) Audiodriver has been not initialized');
  {$ENDIF}
  Halt(5);
End;

Procedure RegisterSoundDriver(Driver : PSoundDriver);
Var
  P : PSoundDriver;
Begin
  if SoundDrivers = Nil then SoundDrivers := Driver else begin
    P := SoundDrivers;
    while P^.NextDriver <> Nil do P := P^.NextDriver;
    P^.NextDriver := Driver;
  end;
  DefaultDriver := Driver;
End;

Procedure SelectSoundDriver(Driver : PSoundDriver);
Begin
  if Driver = Nil then Exit;
  SoundDriver := Driver;
  Move(SoundDriver^.Methods^, DriverMethods, SizeOf(DriverMethods));
End;

Function  GetSoundDriver;
Var
  P : PSoundDriver;
Begin
  if SoundDrivers = Nil then GetSoundDriver := Nil else begin
    P := SoundDrivers;
    while P^.NextDriver <> Nil do begin
      if P^.DeviceType = ADeviceType then begin
        GetSoundDriver := P;
        Exit;
      end;
      P := P^.NextDriver;
    end;
    if DefaultDriver <> Nil then GetSoundDriver := DefaultDriver;
  end;
End;

Procedure NullMethods;
Var
  I : Byte;
  L : array[1..100] of Pointer absolute DriverMethods;
Begin
  for I := 1 to SizeOf(DriverMethods) div SizeOf(Pointer) do L[I] := @FailSound;
End;

{$L SDI.OBJ}
Procedure Init; External;
Procedure Done; External;
Function  InitWave(S : PStream) : Boolean; External;
Procedure SetRate; External;
Procedure StartSound; External;
Procedure StopSound; External;
Procedure PauseSound; External;
Procedure ContinueSound; External;

Function  InitWaveSound;
Begin
  StopSound;
  SoundStream := Nil;
  if S = Nil then begin
    InitWaveSound := bsReadError;
    Exit;
  end;
  S^.Read(SoundHeader, SizeOf(SoundHeader));
  if LongInt(SoundHeader.IdRIFF) <> LongInt(SoundFileId)
    then InitWaveSound := bsInvFormat else begin
      SoundSize  := SoundHeader.DataSize - 4;
      if SoundHeader.Frequency > SoundHeader.BytePS then
        ActualFreq := SoundHeader.Frequency else
        ActualFreq := SoundHeader.BytePS;
      SetRate(ActualFreq);
      SoundStart := S^.GetPos;
      SoundPos := 0;
      if S^.Status = stOk then begin
        SoundStream := S;
        SoundCheckDos := (TypeOf(SoundStream^)=TypeOf(TBufStream)) or
                         (TypeOf(SoundStream^)=TypeOf(TDosStream));
        InitWaveSound := DriverMethods._InitWave(S);
      end else InitWaveSound := bsReadError;
   end;
end;

Procedure PlayWavFile(FileName : String; BackGround : Boolean);
var
  FS : PBufStream;
Begin
  New(FS, Init(FileName, stOpenRead, 20480));
  if FS = Nil then Exit;
  if FS^.Status <> stOk then begin
    Dispose(FS, Done);
    Exit;
  end;
  FS^.Seek(0);
  if InitWaveSound(FS) <> bsOk then Exit;
  StartSound;
  if not BackGround then repeat until not PlayingSound;
End;

Var
  LastExitProc : Pointer;

procedure BSoundExitProc; far;
begin
  if SoundDriver <> Nil then StopSound;
  ExitProc:=LastExitProc;
end;

Function  NoneInit : Boolean;
Begin
  NoneInit := True;
End;

Procedure NoneDef;
Begin
End;

Function  NoneInitWave(S : PStream) : Word;
Begin
  NoneInitWave := 0;
End;

Procedure NoneSetRate(Rate : Word);
Begin
End;

Const
  NoneDriverMethods : TSoundDriverMethods = (
    _Init          : NoneInit;
    _Done          : NoneDef;
    _InitWave      : NoneInitWave;
    _SetRate       : NoneSetRate;
    _StartSound    : NoneDef;
    _StopSound     : NoneDef;
    _PauseSound    : NoneDef;
    _ContinueSound : NoneDef
  );
  NoneSoundDriver   : TSoundDriver = (
    DeviceType         : sdNone;
    DeviceName         : '';
    ID                 : 0;
    Methods            : @NoneDriverMethods
  );

begin
  NullMethods;
  SoundDrivers := Nil;
  DefaultDriver := Nil;
  asm
     mov  ah,34h
     int  21h
     mov  word ptr DosActive,bx
     mov  word ptr DosActive+2,es
  end;
  LastExitProc := ExitProc;
  ExitProc := @BSoundExitProc;
  RegisterSoundDriver(@NoneSoundDriver);
end.
