{$F+,I-}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : Printers Device Interface                            █
  █ Description : Virtual Graphics Common Routines                     █
  █ Author      : Dmitry Karasik                                       █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 26-MAR-1997                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}

Unit Printers;

Interface

Uses Objects, GDI;


Type
  TOrientation = (otPortrait, otLandscape);
  TPaperSize   = (psA1, psA2, psA3, psA4, psA5, psA6, psA7, psA8);
  TPrinterCapSelection = (pcsOrient, pcsQuality, pcsSize, pcsResolution, pcsFormFeed);

  TPrinterCaps = Record
    Cap      : TPrinterCapSelection;
    Entries  : Word;
    List     : array [0..31] of Word;
  End;

  PPrinter = ^TPrinter;
  TPrinter = Object(TObject)
  {user variables}
    PaintInfo   : TPaintInfo;      {Карта вывода (аналоги - HDC, TCanvas)}
    Palette     : TLogPalette;     {Палитра для цветного вывода (HPALETTE)}
    Font        : TFont;           {Шрифт}
    Orientation : TOrientation;    {Ориентация страницы}
    PaperSize   : TPaperSize;      {Размер страницы}
    PageNumber  : Integer;         {Номер страницы}
    Name        : String;          {Наименование драйвера}
    FormFeed    : Boolean;         {Добавляет #12 в конце; поддерживается опционально}
  {developer variables}
    PS          : PStream;         {Поток вывода. Зачастую просто prn}
    PM          : PImage;          {Карта вывода, на которую ссылается PaintInfo}
    Size, Res   : TPoint;          {Размер листа в пикселах и разрешение в точках}
    ColorType   : Word;            {Цветность карты (imXXXX)}
    Quality     : Boolean;         {Качество печати}
    BPL         : Word;            {Размер линейки PM}
    NextPrinter : PPrinter;        {Следующий принтер в цепочке, см. EnumPrinters}
  {user functions}
    Constructor Init(OutputTo : PStream; AName : String; AResX, AResY : Integer);
    Destructor  Done; Virtual;
    Function    BeginDoc : Boolean;                     {начало печати}
    Procedure   BeginDocIndirect(Map : PImage);
    Procedure   NewPage;                                {новая страница}
    Procedure   EndDoc;                                 {конец печати}
    Procedure   SetOutput(NewStream : PStream);
    Procedure   SetPaperSize(P : TPaperSize; O : TOrientation);
    Procedure   SetResolution(AResX, AResY : Integer);
    Procedure   SetQuality(AQuality : Boolean);
    Procedure   SetFormFeed(AFormFeed : Boolean);
    Procedure   SetCapsIndirect(P : TPaperSize; O : TOrientation; Resolution : Integer; AQuality : Boolean);
    Procedure   GetDevCaps(var PrinterCaps : TPrinterCaps); Virtual;
  {developer functions}
    Procedure   Flush; Virtual;
    Procedure   Write(var Buf; Count : Word);
    Procedure   WriteStr(S : String);
    Procedure   CheckChange;
  End;

  {Driver for HP B/W laser printers family. Requires Mono-driver}
  PPCLPrinter = ^TPCLPrinter;
  TPCLPrinter = Object(TPrinter)
    Constructor Init(OutputTo : PStream);
    Procedure   Flush; Virtual;
    Procedure   GetDevCaps(var PrinterCaps : TPrinterCaps); Virtual;
  End;

 {Driver for IBM 9-pin printers family. Requires Mono-driver}
  PIBMPrinter = ^TIBMPrinter;
  TIBMPrinter = Object(TPrinter)
    Constructor Init(OutputTo : PStream);
    Procedure   Flush; Virtual;
    Procedure   GetDevCaps(var PrinterCaps : TPrinterCaps); Virtual;
  End;

Function  CreatePRNStream : PStream;
Function  EnumPrinters(dwCallback : Pointer) : PPrinter; {must be Func(Pointer):Boolean far local}
Procedure CheckBinDevice(Handle : Word);

Const
  Printer        : PPrinter = Nil;
  CurrentPrinter : PPrinter = Nil;

Implementation

Function CreatePRNStream : PStream;
Var
  P : PBufStream;
Begin
  New(P, Init('PRN', stCreate, 2048));
  CheckBinDevice(P^.Handle);
  CreatePRNStream := P;
End;

Function EnumPrinters;
Var
  P : PPrinter;
  I : Boolean;
Begin
  EnumPrinters := Nil;
  P := Printer;
  if P <> Nil then while P <> Nil do begin
    asm
      les di, P
      push es
      push di
      push word ptr [bp]
      call dwCallback
      mov I, al
    end;
    if I then begin
      EnumPrinters := P;
      Exit;
    end;
    P := P^.NextPrinter;
  end;
End;

Procedure CheckBinDevice(Handle : Word); Assembler; Asm
  mov ax, 4400h
  mov bx, Handle
  int 21h
  mov bx, dx
  rol dl, 1
  and dl, 1
  mov al, dl
  or  al, al
  je @@1
  mov dx, bx
  xor dh, dh
  or  dx, 20h  {if device, set binary mode}
  mov ax, 4401h
  mov bx, Handle
  int 21h
@@1:
End;


Constructor TPrinter.Init;
Var
  P : PPrinter;
Begin
  Inherited Init;
  PS := OutputTo;
  PM := Nil;
  Size.X := 0;
  Size.Y := 0;
  PageNumber := 0;
  ColorType  := imMono;
  Name := AName;
  SetResolution(AResX, AResY);
  SetPaperSize(psA4, otPortrait);
  if Printer = Nil then Printer := @Self else begin
    P := Printer;
    while P^.NextPrinter <> Nil do P := P^.NextPrinter;
    P^.NextPrinter := @Self;
  end;
  NextPrinter := Nil;
  if CurrentPrinter = Nil then CurrentPrinter := @Self;
  FormFeed := False;
End;

Destructor TPrinter.Done;
Var
  P, Prev : PPrinter;
Begin
  if PS <> Nil then Dispose(PS, Done);
  if PM <> Nil then FreeDImage(PM);
  if Printer = @Self then Printer := NextPrinter else begin
    P := Printer;
    while P^.NextPrinter <> @Self do P := P^.NextPrinter;
    P^.NextPrinter := NextPrinter;
  end;
  if CurrentPrinter = @Self then begin
    if NextPrinter <> Nil then CurrentPrinter := NextPrinter
     else CurrentPrinter := Printer;
  end;
  Inherited Done;
End;

Procedure  TPrinter.SetOutput;
Begin
  PS := NewStream;
End;

Function TPrinter.BeginDoc;
Begin
  BeginDoc := False;
  CheckChange;
  PM := CreateDImageIndirect(Size.X, Size.Y, ColorType, 0);
  if PM = Nil then Exit;
  BeginDocIndirect(PM);
  BarStyle(0, 0, Size.X, Size.Y, PaintInfo);
  BeginDoc := True;
End;

Procedure TPrinter.BeginDocIndirect;
Begin
  PM  := Map;
  BPL := BPLine(PM);
  DefaultPaint(PaintInfo);
  DefaultFont(Font);
  PaintInfo.Device := PM;
  PaintInfo.ClipRect.Assign(0, 0, Size.X, Size.Y);
  PaintInfo.Fore := White;
  SetDIBDriver(ColorType);
  PageNumber := 0;
End;

Procedure TPrinter.EndDoc;
Begin
  if PM = Nil then Exit;
  RestoreDIBDriver;
  Flush;
  if PS <> Nil then PS^.Flush;
  FreeDImage(PM);
  PM := Nil;
End;

Procedure TPrinter.NewPage;
Begin
  if PM = Nil then Exit;
  Flush;
  Inc(PageNumber);
  PaintInfo.Fore := White;
  PaintInfo.LineStyle   := lsLinePattern;
  PaintInfo.LinePattern := psSolid;
  BarStyle(0, 0, Size.X, Size.Y, PaintInfo);
End;

Procedure  TPrinter.Write;
Begin
  if PS <> Nil then PS^.Write(Buf, Count);
End;

Procedure  TPrinter.WriteStr;
Begin
  if PS <> Nil then PS^.Write(S[1], Byte(S[0]));
End;


Procedure   TPrinter.CheckChange;
Begin
  if PM <> Nil then FreeDImage(PM);
End;

function RecalcToDots(MM : longint;Resolution : word) : word;
begin
  RecalcToDots := MM * Resolution div 254;
end;

function RecalcFromDots(D : longint;Resolution : word) : word;
begin
  RecalcFromDots := D * 254 div Resolution;
end;

Procedure  TPrinter.SetPaperSize;
var
  xT, MaxPictureWid, MaxPictureHei : Word;

Procedure DoSetSize;
Begin
  CheckChange;
  case P of
  psA1 : begin
    MaxPictureWid := 5949;
    MaxPictureHei := 8419;
  end;
  psA2 : begin
    MaxPictureWid := 4209;
    MaxPictureHei := 5949;
  end;
  psA3 : begin
    MaxPictureWid := 2979;
    MaxPictureHei := 4209;
  end;
  psA4 : begin
    MaxPictureWid := 2109;
    MaxPictureHei := 2979;
  end;
  psA5 : begin
    MaxPictureWid := 1489;
    MaxPictureHei := 2109;
  end;
  psA6 : begin
    MaxPictureWid := 1059;
    MaxPictureHei := 1489;
  end;
  psA7 : begin
    MaxPictureWid :=  744;
    MaxPictureHei := 1059;
  end;
  psA8 : begin
    MaxPictureWid :=  528;
    MaxPictureHei :=  744;
  end;
  else end;
  PaperSize := P;
End;

Procedure DoSetOr;
Begin
  Orientation := O;
  if O = otLandscape then begin
     xT := MaxPictureWid;
     MaxPictureWid := MaxPictureHei;
     MaxPictureHei := xT;
  end;
End;

Label SizeSet, OrSet;

Var
  T : TPrinterCaps;
  I : Integer;
Begin
  T.Cap := pcsSize;
  GetDevCaps(T);
  if T.Entries > 0 then for I := 0 to T.Entries - 1 do begin
    if T.List[I] = Word(P) then begin
      DoSetSize;
      Goto SizeSet;
    end;
  end;
  P := TPaperSize(T.List[0]);
  DoSetSize;
SizeSet:

  T.Cap := pcsOrient;
  GetDevCaps(T);
  if T.Entries > 0 then for I := 0 to T.Entries - 1 do begin
    if T.List[I] = Word(O) then begin
      DoSetOr;
      Goto OrSet;
    end;
  end;
  O := TOrientation(T.List[0]);
  DoSetOr;
OrSet:
  Size.X := RecalcToDots(MaxPictureWid, Res.X);
  Size.Y := RecalcToDots(MaxPictureHei, Res.Y);
end;

Procedure TPrinter.SetResolution;
Var
  T : TPrinterCaps;
  I : Integer;
Begin
  T.Cap := pcsResolution;
  GetDevCaps(T);
  if T.Entries > 0 then for I := 0 to T.Entries - 1 do begin
    if T.List[I] = AResX then begin
      CheckChange;
      Res.X := AResX;
      Res.Y := AResY;
      Exit;
    end;
  end;
  CheckChange;
  Res.X := T.List[0];
  Res.Y := T.List[0];
End;

Procedure TPrinter.SetQuality;
Var
  T : TPrinterCaps;
  I : Integer;
Begin
  T.Cap := pcsQuality;
  GetDevCaps(T);
  if T.Entries > 0 then for I := 0 to T.Entries - 1 do begin
    if T.List[I] = Word(AQuality) then begin
      Quality := AQuality;
      Exit;
    end;
  end;
End;

Procedure  TPrinter.SetFormFeed;
Var
  T : TPrinterCaps;
  I : Integer;
Begin
  T.Cap := pcsFormFeed;
  GetDevCaps(T);
  if T.Entries > 0 then for I := 0 to T.Entries - 1 do begin
    if T.List[I] = Word(AFormFeed) then begin
      FormFeed := AFormFeed;
      Exit;
    end;
  end;
End;


Procedure  TPrinter.SetCapsIndirect;
Begin
  SetPaperSize(P, O);
  SetResolution(Resolution, Resolution);
  SetQuality(AQuality);
End;

Procedure  TPrinter.Flush;
Begin
  Abstract;
End;

Procedure TPrinter.GetDevCaps;
Begin
  Abstract;
End;

Procedure NotBuffer(Source, Dest : Pointer; Size : Word); Assembler; Asm
  push ds
  mov cx, Size
  les di, Dest
  lds si, Source
  dec cx
  cld
@@1:
  lodsb
  not al
  stosb
  loop @@1
  pop ds
End;

Function IsZero(Buffer : Pointer; Size : Word; CmpByte : Byte) : Boolean; Assembler; Asm
    xor al, al
    les di, Buffer
    mov cx, Size
    mov dl, CmpByte
@@1:cmp es:[di], dl
    jne @@2
    inc di
    loop @@1
    inc al
@@2:
End;


Function MonoFillGap(Buffer : Pointer; Last, SizeX : Word) : Pointer; Assembler;
Asm
  les di, Buffer
  add di, Last
  dec di
  mov ax, SizeX
  and al, 7
  or  al, al
  je  @@1
  mov ah, 8
  sub ah, al
  mov cl, ah
  mov al, 1
  shl al, cl
  dec al
  or es:[di], al
@@1:
  mov dx, es
  mov ax, word ptr [Buffer]
End;


Const
  Esc = #27;
  ZeroString = #13#10;

Constructor TPCLPrinter.Init;
Begin
  Inherited Init(OutputTo, 'PCL Generic printer', 300, 300);
  Quality := True;
End;

Procedure   TPCLPrinter.Flush;
Var
  S        : String;
  I        : Integer;
  Buf, Esp : Pointer;
Begin
  GetMem(Buf, BPL);
  S := '0';
  Inc(S[1], Ord(Orientation));
  WriteStr(Esc + '&l' + S + 'O');{orientation}
  WriteStr(Esc + '*r0F');        {?}
  WriteStr(Esc + #42#112'0'#89); {Y=0}
  WriteStr(Esc + #42#112'0'#88); {X=0}
  Str(Res.X, S);
  WriteStr(Esc + '*t' + s + 'R' + Esc + '*r1A'); {resolution}
  Str(BPL - 1, S);
  S := Esc + '*b' + S + 'W'; {bpl}
  for I := 0 to Size.Y - 1 do begin
    NotBuffer(MonoFillGap(MapBitLineRead(PM, I, BPL), BPL, Size.X), Buf, BPL);
    if not IsZero(Buf, BPL, 0) then begin
      WriteStr(S);
      Write(Buf^, BPL - 1);
    end else WriteStr(ZeroString);
  end;
  if FormFeed then WriteStr(#12) else WriteStr(#13#10);
  FreeMem(Buf, BPL);
End;

Procedure  TPCLPrinter.GetDevCaps;
Begin
  with PrinterCaps do begin
    case Cap of
    pcsOrient : begin
      Entries := 2;
      List[0] := Word(otPortrait);
      List[1] := Word(otLandscape);
    end;
    pcsQuality : begin
      Entries := 1;
      List[0] := Word(WordBool(True));
    end;
    pcsSize : begin
      Entries := 2;
      List[0] := Word(psA4);
      List[1] := Word(psA5);
      List[2] := Word(psA6);
    end;
    pcsResolution : begin
      Entries := 3;
      List[0] := 150;
      List[1] := 300;
      List[2] := 600;
    end;
    pcsFormFeed : begin
      Entries := 2;
      List[0] := 0;
      List[1] := 1;
    end;
    else end;
  end;
End;


Const
  SemiCR = #27#74#1#13;
  FullCR = #27#74#23#13;

Constructor TIBMPrinter.Init;
Begin
  Inherited Init(OutputTo, 'IBM Generic 9-pin printer', 120, 143);
  Quality := True;
End;

Procedure   TIBMPrinter.Flush;
Var
  BPL8   : Word;
  RevBuf : array[0..15] of PByteArray;
  S      : String;
  I, Last: Integer;
  Buf    : Pointer;
  ELine  : PByteArray;

Procedure Flush8Line;
Var
  I : Integer;
  J, K : Byte;
  SBuf    : array[0..7] of Byte;

Procedure SCvt(P : Pointer); Assembler; Asm
      les bx, p
      push bp
      xor ax, ax
      xor cx, cx
      xor dx, dx
      xor bp, bp
      xor di, di
      xor si, si
  @@1:mov al, es:[bx+si]
      xchg ah, cl
      shl ax, 1
      xchg ah, cl
      xchg ah, ch
      shl ax, 1
      xchg ah, ch
      xchg ah, dl
      shl ax, 1
      xchg ah, dl
      xchg ah, dh
      shl ax, 1
      xchg ah, dh
      xchg cx, bp
      xchg dx, di
      xchg ah, cl
      shl ax, 1
      xchg ah, cl
      xchg ah, ch
      shl ax, 1
      xchg ah, ch
      xchg ah, dl
      shl ax, 1
      xchg ah, dl
      xchg ah, dh
      shl ax, 1
      xchg ah, dh
      xchg cx, bp
      xchg dx, di
      cmp si, 7
      je  @@2
      inc si
      jmp @@1
  @@2:mov es:[bx+0], cx
      mov es:[bx+2], dx
      mov es:[bx+4], bp
      mov es:[bx+6], di
      pop bp
      push ds
      pop ds
end;

Procedure FlushEndLine;
Var
  I : Integer;
Begin
  for I := BPL * 8 - 1 downto 0 do if ELine^[I] <> 0 then Break;
  if (I = 0) and (ELine^[0] = 0) then Exit;
  Inc(I);
  WriteStr(#27'L' + Chr(I mod 256) + Chr(I div 256));
  Write(ELine^, I);
End;

Begin
  if BPL = 0 then Exit;
  for I := 0 to BPL - 1 do begin
    for J := 0 to 7 do SBuf[J] := RevBuf[J shl 1]^[I];
    SCvt(@SBuf);
    Move(SBuf, ELine^[I shl 3], 8);
  end;
  FlushEndLine;

  if Quality then begin
    WriteStr(SemiCR);
    for I := 0 to BPL - 1 do begin
      for J := 0 to 7 do SBuf[J] := RevBuf[J shl 1 + 1]^[I];
      SCvt(@SBuf);
      Move(SBuf, ELine^[I shl 3], 8);
    end;
    FlushEndLine;
  end;
  WriteStr(FullCR);
  for I := 0 to 15 do FillChar(RevBuf[I]^, BPL8, 0);
End;


Begin
  WriteStr(#27'J'#1#13);
  S := #27'L' + Chr((BPL * 8) mod 256) + Chr((BPL * 8) div 256);
  BPL8 := (BPL and $FFF8) + 8;
  for Last := Size.Y - 1 downto 0 do begin
    Buf := MapBitLineRead(PM, Last, BPL);
    MonoFillGap(Buf, BPL, Size.X);
    if not IsZero(Buf, BPL, $FF) then Break;
  end;
  GetMem(ELine, BPL8 * 8);
  for I := 0 to 15 do begin
    GetMem(RevBuf[I], BPL8);
    FillChar(RevBuf[I]^, BPL8, 0);
  end;
  if Last > 0 then for I := 0 to Last do begin
    NotBuffer(MonoFillGap(MapBitLineRead(PM, I, BPL), BPL, Size.X), RevBuf[I and 15], BPL);
    if ((I and 15) = 15) then Flush8Line;
  end;
  if (I and 15) <> 15 then Flush8Line;
  for I := 0 to 15 do FreeMem(RevBuf[I], BPL8);
  FreeMem(ELine, BPL8 * 8);
  if FormFeed then WriteStr(#12) else WriteStr(#13#10);
End;


Procedure  TIBMPrinter.GetDevCaps;
Begin
  with PrinterCaps do begin
    case Cap of
    pcsOrient : begin
      Entries := 1;
      List[0] := Word(otPortrait);
    end;
    pcsQuality : begin
      Entries := 2;
      List[0] := Word(WordBool(True));
      List[1] := Word(WordBool(False));
    end;
    pcsSize : begin
      Entries := 3;
      List[0] := Word(psA3);
      List[1] := Word(psA4);
      List[2] := Word(psA5);
      List[3] := Word(psA6);
    end;
    pcsResolution : begin
      Entries := 1;
      List[0] := 120;
    end;
    pcsFormFeed : begin
      Entries := 2;
      List[0] := 0;
      List[1] := 1;
    end;
    else end;
  end;
End;

var
  PrevExitProc: pointer;

procedure MyExitProc; far;
begin
  ExitProc := PrevExitProc;
  while Printer <> Nil do Dispose(Printer, Done);
end;


Begin
  PrevExitProc := ExitProc;
  ExitProc := @MyExitProc;
End.