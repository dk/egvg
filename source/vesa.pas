{$G+,F+,S-}
{Internal unit for VESA-drivers}
Unit VESA;

Interface

Uses Objects, Memory, GDI;

Function  VESAInitialize : Boolean;
Procedure VESAFinitialize;
Procedure ReInitVideo;
procedure VESAStandardLeaveGraphics;
Procedure PrepareDrawing;
Procedure SetOutput(OnBuffer : Boolean); {swaps output for buffered functions}
Procedure SetActivePage(PageNo : Byte); {sets page output in videobuffer}
Procedure EMSAdjSelect;

procedure SwitchPlane(PgNum: Integer);
Procedure SelectBank(Which:Word); Far;
Procedure SelectBankBuf(Which:Word); Far;

Type
  TSVGAPixelInfo = Record
    Bank       : Word;
    Offs       : Word;
    Next       : Word;
  End;

Const
  vmiaSupported = 1;
  vmiaInfoAvail = 2;
  vmiaBIOSAvail = 4;
  vmiaColor     = 8;
  vmiaGraphics  = 16;

  vmiwExists   = 1;
  vmiwReadable = 2;
  vmiwWritable = 4;

  vmmText      = 0;
  vmmCGA       = 1;
  vmmHGC       = 2;
  vmmEGA       = 3;
  vmmPacked    = 4;
  vmmSequ256   = 5;
  vmmHiColor   = 6;
  vmmYUV       = 7;

  vgc8BitDAC     = 0;
  vgcNonVGACtrl  = 1;
  vgcDACBlankBit = 2;

Type
  TSVGABoardInfo = Record
    Signature  : array[1..4] of Char;
    Version    : Word;
    OEMName    : PChar;
    Capability : LongInt;    {vgcXXX}
    ModeList   : PWordArray;
    VideoMem   : Word;
    {VBE 2.0}
    OEMVersion : Word;
    VendorName : PChar;
    ProductName: PChar;
    Revision   : PChar;
    Res_Fill   : array[1..222] of Byte;
  End;

  TVESAModeInfo = Record
    Attribute    : Word;               {vmiaXXXX}
    Window       : array[0..1] of Byte;{vmiwXXX}
    Granularity  : Word;               {in KB}
    WinSize      : Word;               {in KB}
    Segment      : array[0..1] of Word;
    Callback     : Pointer;
    BPLine       : Word;
    {for VESA modes in v1.0/1.1, needed for OEM modes}
    Width, Height: Word;
    TextWidth, TextHeight    : Word;
    Planes, BPPixel, Banks   : Byte;
    MemoryModel              : Byte;  {vmmXXX}
    BankSize, ImagePages, Res: Byte;
    {VBE v1.2+}
    Colors : array[0..3, 0..1] of Byte; {red mask, red field position, green ...}
    HiColorModeInfo          : Byte;
    Res_2                    : Byte;
    Res_Fill                 : array[1..213] of Byte;
  End;

  TVideoCard = (Herc, CGA, EGA, VGA, SVGA);

Const
  VESAAcceleratorPresent : Boolean = False;


{Read-only system section}
Function  VesaPresent : Boolean;
Procedure GetVesaBoardInfo(var T : TSVGABoardInfo);
Function  VesaEnterGraphics(VesaMode : Word) : Boolean;
Procedure GetVesaModeInfo(Mode : Word; var T : TVESAModeInfo);

Const
  CurrentBank      : Word = 0;        {svga current bank}
  BufferSeg        : Word = $A000;    {if BufferedStrategy, then = emspageframe/ScreenBuffers else SegA000}
  CurrentBankBuf   : Word = 0;
  PCurrentBankBuf  : Word = ofs(CurrentBankBuf);
  PGetPixelInfoBuf : Procedure(X, Y : Word) = Nil;
  PDriverGPInfoBuf : Procedure(X, Y : Word) = Nil;
  PDriverGPInfo    : Procedure(X, Y : Word) = Nil;
  PSelectBankBuf   : Procedure(Which: Word) = Nil;
  SwitchCallBack   : Pointer = Nil;            {instead int 10h}
  CurrentPageBank  : Byte = 0;
  BytesPerPixel    : Byte = 1;


  {EMS privates}
  {в DPMI ясен хрен EMS нет, но все названия пусть с EMSом остаются -
  различать легче. Кроме того, куски памяти в DPMI под буфер видимо тоже
  будут по 16К. Очень желательно использование этих переменных под работу
  с EMS для видеодрайверов - связано с тем, что TEMSStream может копироваться
  в экранный буфер, который также может быть в EMS}
  {$IFNDEF DPMI}
  EMSFrame   : Word = 0;
  EMSPages   : Word = 0;
  EMSHandler : Word = 0;
  {$ENDIF}


Var
  svgaQGD : Byte;        {(y * maxY + X) shr (svgaQGD + 8) = bank number}
  svgaQLD : LongInt;     {(y * maxY + X) and svgaQLD = offset inside bank}
  emsdQGD : Byte;        {(y * maxY + X) shr (emsdQGD + 8) = EMS page number}
  emsdQLD : LongInt;     {(y * maxY + X) and emsdQLD = offset inside page}
  ScreenBuffers            : array[0..239] of Word;
  LastMode                 : Byte;
  VideoBoardMem            : Word;
  BanksInPage, MaxPage     : Word;
  PixelInfo, SavePixelInfo : TSVGAPixelInfo; {internal}
  SaveBank                 : Word;           {internal for mouse}
  VideoCard                : TVideoCard;

Implementation


{$IFDEF DPMI}
Type
  DPMIRegisters = Record
    EDI, ESI, EBP, Reserved, EBX, EDX, ECX, EAX : LongInt;
    FLAGS, _ES, _DS, _FS, _GS, _IP, _CS, _SP, _SS : Word;
  End;

Procedure DefaultDPMIBankSwitch; Assembler;
Asm
  mov ax, 4f05h
  int 10h
End;

Procedure VesaInt(var R : DPMIRegisters; ESDIBuffer : Pointer);
Var
  RealSeg, ProtSeg : Word;  { это сегмент и дескриптор одного
                             и того же куска памяти для обмена
                             с int 10h}
  P : Pointer;
Begin
  asm
    {получаем в DOS pool 256 байт}
    mov ax, 0100h
    mov bx, 16
    int 31h
    mov RealSeg, ax
    mov ProtSeg, dx
  end;
  {инитим регистры - т.к. прерывание будет выполняться в
  виртуальном режиме, то наша адресация ему не подходит -
  суем в ES реальный сегмент}
  R._ES := RealSeg;
  R.EDI := 0;
  P := @R;
  asm
    les di, p
    xor cx, cx
    mov bl, 10h
    mov bh, 0
    mov ax, 300h
    int 31h
  end;
  {а сами качаем инфо по нашенскому защищенному дескриптору}
  Move(Mem[ProtSeg:0], ESDIBuffer^, 256);
  {и освобождаем DOS pool. Жопа, не правда ли?}
  asm
    mov ax, 0101h
    mov dx, ProtSeg
    int 31h
  end;
  {That's all folx! Thanx to Frolov Bros.}
End;
{$ENDIF}

Function  VesaEnterGraphics(VesaMode : Word) : Boolean;
var
  S : String;
  T : TVESAModeInfo absolute S;
  P : ^TVESAModeInfo;
  L : LongInt;
  X : Word;
  Y : Byte;
  MBank : Word;
{$IFDEF DPMI}
  Regs : DPMIRegisters;
{$ENDIF}
begin
  asm
    mov   ax, 4f02h
    mov    bx, VesaMode
    int    10h
    mov    x, ax
  end;
  VesaEnterGraphics := Byte(x) = $4f{True};
  asm
    mov ah, 0fh
    int 10h
    mov y, al
  end;
  if  (Byte(x) <> $4f) or (y <= 3) then begin
    asm
      mov ax, 3
      int 10h
    end;
    VesaEnterGraphics := False;
    Exit;
  end;
  P := @S;
  {$IFDEF DPMI}
  FillChar(Regs, SizeOf(Regs), 0);
  Regs.EBX := 0;
  Regs.EAX := $4F01;
  Regs.ECX := VesaMode;
  VesaInt(Regs, P);
  {$ELSE}
  asm
    mov ax, 4f01h
    mov cx, VesaMode
    les di, p
    int 10h
  end;
  {$ENDIF}
  ScreenDriver^.Granularity := LongInt(P^.Granularity) * 1024;
  {$IFDEF DPMI}
  asm
    mov ax, $4f0a
    mov bx, 1
    int 10h
    mov x, ax
    mov word ptr [l],   di
    mov ax, es
    mov word ptr [l+2], ax
    mov MBank, cx
  end;
  if false and (X = $4F) then begin
    SwitchCallBack := MemAlloc(MBank);
    if SwitchCallBack <> Nil then begin
      Move(Pointer(L)^, SwitchCallBack^, MBank);
      asm
        mov ax, 9
        mov bx, word ptr [SwitchCallback+2]
        mov cx, 9eh {code}
        int 31h
      end;
    end else SwitchCallBack := @DefaultDPMIBankSwitch;
  end else SwitchCallBack := @DefaultDPMIBankSwitch;
  {$ELSE}
  SwitchCallBack := P^.CallBack;
  {$ENDIF}
  asm
    mov ah, 0fh
    int 10h
    mov LastMode, al
  end;

  if (ScreenDriver^.NumberOfColors = 32768) or
    (ScreenDriver^.NumberOfColors = 65536) then BytesPerPixel := 2
      else if (ScreenDriver^.NumberOfColors = 16777216) then BytesPerPixel := 4
        else BytesPerPixel := 1;

  L := ScreenDriver^.Granularity;
  for svgaQGD := 0 to 31 do
    if (L and (LongInt(1) shl svgaQGD)) <> 0 then Break;
  svgaQLD := (LongInt(1) shl svgaQGD) - 1;
  svgaQGD := svgaQGD - 8;
  with ScreenDriver^ do begin
    MaxBanks := (LongInt(ScreenWidth) * ScreenHeight * BytesPerPixel) div Granularity + 1;
    EMSBanks := (LongInt(ScreenWidth) * ScreenHeight * BytesPerPixel) div BufGranula + 1;
    BitPlaneSize := Granularity - 1;
  end;
  MBank := VideoBoardMem div P^.Granularity;
  BanksInPage:= ScreenDriver^.MaxBanks;
  MaxPage    := MBank div ScreenDriver^.MaxBanks;
  if TryBufferedStrategy then
    BuffersInPage := (ScreenDriver^.MaxBanks * BytesPerPixel) <= MBank
  else BuffersInPage := False;
  VESAAcceleratorPresent :=
   (((P^.Window[0] and (vmiwExists + vmiwReadable)) = vmiwExists + vmiwReadable) and
    ((P^.Window[1] and (vmiwExists + vmiwWritable)) = vmiwExists + vmiwWritable)) or
   (((P^.Window[1] and (vmiwExists + vmiwReadable)) = vmiwExists + vmiwReadable) and
    ((P^.Window[0] and (vmiwExists + vmiwWritable)) = vmiwExists + vmiwWritable));
End;

Procedure GetVesaModeInfo(Mode : Word; var T : TVESAModeInfo);
{$IFDEF DPMI}
var
  Regs : DPMIRegisters;
{$ENDIF}

Begin
  {$IFDEF DPMI}
  FillChar(Regs, SizeOf(Regs), 0);
  Regs.EBX := 0;
  Regs.EAX := $4F01;
  Regs.ECX := Mode;
  VesaInt(Regs, @T);
  {$ELSE}
  asm
    mov ax, 4f01h
    mov cx, Mode
    les di, T
    int 10h
  end;
  {$ENDIF}
End;

Function  _VideoCard : TVideoCard;
Var
  Present : Boolean;
  Area    : String;
  PArea   : ^String;
Begin
  _VideoCard := Herc;
  asm
    mov Present, 0
    mov dx, 03BAh
    in  al, dx
    cmp al, 80h
    je  @@1
    mov Present, 1
    @@1 :
  end;
  if Present then _VideoCard := CGA else Exit;

  asm
    mov Present, 0
    mov ax, 1212h
    mov bl, 10h
    int 10h
    cmp bl, 10h
    je  @@1
    mov Present, 1
    @@1 :
  end;
  if Present then _VideoCard := EGA else Exit;

  asm
    mov Present, 0
    mov ax, 1A00h
    int 10h
    cmp al, 1Ah
    jne @@1
    mov Present, 1
    @@1 :
  end;
  if Present then _VideoCard := VGA else Exit;
  if VesaPresent then _VideoCard := SVGA;
End;

function VesaPresent: boolean;
var
  S : String;
  VesaBuffer : TSVGABoardInfo absolute S;
  P  : Pointer;
  {$IFDEF DPMI}
  R  : DPMIRegisters;
  {$ENDIF}
begin
  P := @S;
  {$IFDEF DPMI}
  FillChar(R, SizeOf(R), 0);
  R.EAX := $4F00;
  VesaInt(R, P);
  VesaPresent := Byte(R.EAX) = $4F;
  {$ELSE}
  asm
    les di, dword ptr [p]
    mov ax, 4f00h
    int 10h
    mov byte ptr [s], al
  end;
  VesaPresent := (S[0] = Char($4F));
  {$ENDIF}
  VideoBoardMem := VesaBuffer.VideoMem * 64;
end;

Procedure GetVesaBoardInfo(var T : TSVGABoardInfo);
var
  P  : Pointer;
  {$IFDEF DPMI}
  R  : DPMIRegisters;
  {$ENDIF}
begin
  P := @T;
  {$IFDEF DPMI}
  FillChar(R, SizeOf(R), 0);
  R.EAX := $4F00;
  VesaInt(R, P);
  {$ELSE}
  asm
    les di, dword ptr [p]
    mov ax, 4f00h
    int 10h
  end;
  {$ENDIF}
end;


{Part 2. EMS/DPMI}
{$IFDEF DPMI}
procedure SwitchPlane(PgNum: Integer); assembler;
{$ELSE}
procedure MemSwitchPlane(PgNum: Integer); assembler;
{$ENDIF}
asm
  push si
  mov ax, PgNum
  mov si, offset ScreenBuffers
  shl ax, 1
  add si, ax
  mov ax, [si]
  mov BufferSeg, ax
  mov es, ax
  pop si
end;

Function MemInit : Boolean;
Var
  I : Word;
  P : Pointer;
Begin
  for I := 0 to ScreenDriver^.EMSBanks - 1 do begin
    P := MemAllocSeg(BufGranula);
    ScreenBuffers[I] := Seg(P^);
    if P = Nil then begin
      if I > 0 then for I := I - 1 downto 0 do
        FreeMem(Ptr(ScreenBuffers[I], 0), BufGranula);
      MemInit := False;
      Exit;
    end;
  end;
  MemInit := True;
  {$IFDEF DPMI}
  SwitchPlane(0);
  {$ELSE}
  MemSwitchPlane(0);
  {$ENDIF}
End;

Procedure MemDone;
Var
  P : Pointer;
  I : Word;
Begin
  for I := ScreenDriver^.EMSBanks - 1 downto 0 do begin
    P := Ptr(ScreenBuffers[I], 0);
    if P <> Nil then FreeMem(P, BufGranula)
  end;
End;

{$IFNDEF DPMI}
function EMSCheck(PgNum: Integer): boolean; near; assembler;{TONY}
const
  EmsDeviceLen = 8;
  EmsDeviceStr: array[1..EmsDeviceLen] of Char = 'EMMXXXX0';
asm
    cmp    PgNum, 0     { Слишком мало просят }
    jle    @@Fail
    mov    ax, 3567h
    int    21h          { Get vector 67h -> es:bx }
    mov    di, 0Ah
    mov    si, offset EmsDeviceStr
    mov    cx, EmsDeviceLen
    cld
    rep    cmpsb
    jne    @@Fail
    mov    ah, 41h
    int    67h          { Get page frame segment }
    mov    EMSFrame, bx
    mov    ah, 42h
    int    67h          { EMS memory size/available size }
    cmp    bx, PgNum
    jb     @@Fail
    mov    ax, 4300h
    mov    bx, PgNum
    int    67h          { Get pool for 1st bit plane }
    or     ah, ah       { Error allocating EMS }
    jnz    @@Fail
    mov    EMSHandler, dx
@@Success:
    mov    al, 1
    jmp    @@Exit
@@Fail:
    xor    al, al
@@Exit:
end;

procedure FreeEMSPool(Pool: Word); near; assembler;{TONY}
asm
    mov    ax, EMSFrame
    or     ax, ax
    jz     @@Exit
    mov    dx, Pool
    or     dx, dx
    jz     @@Exit
    mov    ax, 4500h
    int    67h
@@Exit:
end;

procedure EMSSwitchPlane(PgNum: Integer); assembler;{TONY}
asm
    mov    dx, EMSHandler
    mov    ax, 4400h
    mov    bx, PgNum
    int    67h
    mov    EmsCurHandle,dx
    mov    EmsCurPage,  bx
end;

Function EMSInit : Boolean;
Begin
  if not EMSCheck(ScreenDriver^.EMSBanks) then begin
    EMSPages := 0;
    EMSInit  := False;
    Exit;
  end;

  EMSInit := True;
  EMSSwitchPlane(0);
End;

Procedure EMSDone;
Begin
  FreeEMSPool(EMSHandler);
End;

procedure SwitchPlane(PgNum: Integer); assembler;
Asm
  mov  ax, PgNum
  push ax
  mov ah, BuffersInEms
  or ah, ah
  je @@1
  call EMSSwitchPlane
  leave
  ret 2
@@1:
  call MemSwitchPlane
End;
{$ENDIF}

Function InitEMS : Boolean;
Begin
  InitEMS := True;
  if BuffersInPage then Exit;
  {$IFDEF DPMI}
  if MemInit then Exit;
  {$ELSE}
  if (ScreenDriver^.EMSBanks <= 4) and MemInit then Exit;
  if EMSInit then begin
    BuffersInEms := True;
    Exit;
  end;
  {$ENDIF}
  InitEMS := False;
End;

Procedure DoneEMS;
Begin
  if BuffersInPage then Exit;
  {$IFDEF DPMI}
  MemDone;
  {$ELSE}
  if BuffersInEms then FreeEMSPool(EMSHandler) else MemDone;
  {$ENDIF}
End;

{Part 3. SVGA low level}
{Здесь собраны процедуры зависимые от структуры видеопамяти SVGA}

Procedure SelectBank(Which:Word); Assembler;
Asm
  xor bx, bx
  mov dx, Which
  mov CurrentBank, dx
  mov PixelInfo.Bank, dx
  push dx
  call SwitchCallBack
  mov bx, 1
  pop  dx
  call SwitchCallBack
End;


Procedure SelectBankBuf(Which:Word); Assembler;
Asm
  mov dx, Which
  mov CurrentBankBuf, dx
  cmp BuffersInPage, 0
  je @@1 {quick abnormal videomem-buffer}
  add dx, BanksInPage
  push dx
  call SelectBank
  jmp @@2
@@1: {normal memory-dumped buffer}
  mov PixelInfo.Bank, dx
  push dx
  call SwitchPlane
@@2:
End;




function VESAInitialize: boolean;
var
  i: integer;
begin
with ScreenDriver^ do begin
  VESAInitialize := False;
  BitPlaneSize  := ScreenWidth * ScreenHeight;
  LineLength    := ScreenWidth;
  if not EnterGraphics then Exit;
  VESAInitialize := True;

  if not TryBuffersInPage or not TryBufferedStrategy then BuffersInPage := False;

  if TryBufferedStrategy then begin
    BufferedStrategy := InitEMS
  end else BufferedStrategy := False;

  if BufferedStrategy and not (BuffersInPage) then begin
    {$IFNDEF DPMI}
    if BuffersInEms then BufferSeg := EMSFrame else
         BufferSeg := ScreenBuffers[0];
    {$ENDIF}
    PCurrentBankBuf := Ofs(CurrentBankBuf);
    @PSelectBankBuf  := @SelectBankBuf;
    PGetPixelInfoBuf:= PDriverGPInfoBuf;
    for emsdQGD := 0 to 31 do if (BufGranula and (LongInt(1) shl emsdQGD)) <> 0 then Break;
    emsdQLD := (LongInt(1) shl emsdQGD) - 1;
    Dec(emsdQGD, 8);
  end else begin
    BufferSeg := SegA000;
    if BuffersInPage then begin
      PCurrentBankBuf  := Ofs(CurrentBankBuf);
      @PSelectBankBuf  := @SelectBankBuf;
      PGetPixelInfoBuf := PDriverGPInfoBuf;
      SetActivePage(1);
    end else begin
      PSelectBankBuf  := SelectBank;
      PGetPixelInfoBuf:= PDriverGPInfo;
      PCurrentBankBuf := Ofs(CurrentBank);
    end;
    emsdQLD := svgaQLD;
    emsdQGD := svgaQGD;
  end;
  SelectBank(0);
  GetVGAPalette(0, 256, PMainPalette);
end;
MaximalX := ScreenDriver^.MaximalX;
MaximalY := ScreenDriver^.MaximalY;
end;

var
  SaveExit: Pointer;

procedure EmergencyFree; far;
begin
  ExitProc := SaveExit;
  if BufferedStrategy then begin
    DoneEMS;
    BufferedStrategy := False;
  end;
end;

procedure VESAFinitialize;
begin
  if BufferedStrategy then begin
    DoneEMS;
    BufferedStrategy := False;
  end;
  ScreenDriver^.LeaveGraphics;
end;

procedure VESAStandardLeaveGraphics; assembler;
asm
  mov    ax, 0003h
  int    10h
  mov    ax, 1104h
  xor    bx, bx
  int    10h
end;


Procedure ReInitVideo; Assembler;
Asm
  mov ah, 0
  mov al, LastMode
  int 10h
End;

Procedure PrepareDrawing;
Begin
  {$IFNDEF DPMI}
  if BufferedStrategy then SelectBankBuf(0);
  {$ENDIF}
End;

Procedure EMSAdjSelect; Assembler; Asm
{$IFNDEF DPMI}
{if BuffersInEms and (EMSCurHandle <> EMSHandler) then PSelectBankBuf(CurrentBankBuf);}
  mov ax, EmsCurHandle
  cmp ax, EMSHandler
  jne @@1
  retf
@@1:
  mov ax, CurrentBankBuf
  push ax
  call PSelectBankBuf
{$ENDIF}
End;

Procedure SetActivePage(PageNo : Byte);
Begin
  if PageNo > MaxPage then Exit;
  CurrentPageBank := PageNo * BanksInPage;
  ActivePage      := PageNo;
  CurrentBankBuf  := $FFFF;
  CurrentBank     := $FFFF;
End;

{redirects output from buffer to screen and vice versa.
uses to avoid routine duplicates}
Procedure SetOutput(OnBuffer : Boolean);
Const
  AtBuffer : Boolean = True;
  _BufferSeg       : Word = $A000;
  _CurrentBankBuf  : Word = ofs(CurrentBankBuf);
  _SelectBankBuf   : Procedure(Which: Word) = SelectBankBuf;
  _GetPixelInfoBuf : Procedure(X, Y : Word) = Nil;
  _emsdQLD         : LongInt = 0;
  _emsdQGD         : LongInt = 0;

Begin
  if not(BufferedStrategy) or (OnBuffer = AtBuffer) then Exit;
  asm cli end;
  if OnBuffer then begin
    emsdQGD := _emsdQGD;
    emsdQLD := _emsdQLD;
    PSelectBankBuf   := _SelectBankBuf;
    PGetPixelInfoBuf := _GetPixelInfoBuf;
    PCurrentBankBuf  := _CurrentBankBuf;
    BufferSeg        := _BufferSeg;
    FillChar(PixelInfo, SizeOf(PixelInfo), $FF);
  end else begin
    _emsdQGD := emsdQGD;
    _emsdQLD := emsdQLD;
    _SelectBankBuf   := PSelectBankBuf;
    _GetPixelInfoBuf := PGetPixelInfoBuf;
    _CurrentBankBuf  := PCurrentBankBuf;
    _BufferSeg       := BufferSeg;

    emsdQGD := svgaQGD;
    emsdQLD := svgaQLD;
    PSelectBankBuf   := SelectBank;
    PGetPixelInfoBuf := PDriverGPInfo;
    PCurrentBankBuf  := ofs(CurrentBank);
    BufferSeg        := SegA000;
    FillChar(PixelInfo, SizeOf(PixelInfo), $FF);
  end;
  AtBuffer := OnBuffer;
asm sti end;
End;

Begin
  VideoCard := _VideoCard;
End.
