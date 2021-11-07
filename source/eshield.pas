{$F+,G+,S-,I-,R-}
{$C FIXED PRELOAD PERMANENT}

{
   DK Inc. 1997(2:464/46.36@fidonet)
     Exception handling module
}

{$DEFINE HARDPROTECTION}
{$DEFINE EXCEPTIONS}
{$DEFINE VERBOSE}
{$DEFINE RUNERROR}
{ $DEFINE CASTTDX}

{$IFNDEF DPMI}
  {$UNDEF EXCEPTIONS}
  {$UNDEF HARDPROTECTION}
{$ENDIF}
Unit EShield;

{$IFNDEF VER70}
This unit for Borland Pascal 7.0 only!
{$ENDIF}

Interface

Type
  TJump = Record
    Result  : Word;
    LocData : array[1..9] of Byte;
  end;


Function  Try : Boolean;
Procedure TryFree;
Procedure Raise(ErrorType : Word);
Function  SetJump(var E : TJump) : Boolean;
Procedure Jump(var E : TJump; Result : Word);

{
  EShield:–ú–æ–¥—É–ª—å –¥–ª—è –∑–∞—â–∏—Ç—ã –∏ –æ–±—Ä–∞–±–æ—Ç–∫–∏ –∏—Å–∫–ª—é—á–µ–Ω–∏–π.

  –£—Å–ª–æ–≤–Ω—ã–µ —Ä–µ–∂–∏–º—ã –∫–æ–º–ø–∏–ª—è—Ü–∏–∏:
 * EXCEPTIONS      - –æ—Ç—Ä–∞–±–∞—Ç—ã–≤–∞–µ—Ç –ø—Ä–æ—Ü–µ—Å—Å–æ—Ä–Ω—ã–µ –∏—Å–∫–ª—é—á–µ–Ω–∏—è.
 * HARDPROTECTION  - —É—Å—Ç–∞–Ω–∞–≤–ª–∏–≤–∞–µ—Ç –∫–æ–Ω—Ç—Ä–æ–ª—å–Ω—ã–µ —Ç–æ—á–∫–∏ –≤ –º–µ—Å—Ç–∞—Ö
                     –≤—ã–∑–æ–≤–æ–≤ –ø—Ä–æ—Ü–µ–¥—É—Ä.
   VERBOSE         - –≤–ª–∏—è–µ—Ç –Ω–∞ —Å–æ–æ–±—â–µ–Ω–∏—è HaltTrapProc –∏ DetectTDX.
   RUNERROR        - –æ—Ç—Ä–∞–±–∞—Ç—ã–≤–∞–µ—Ç –æ—à–∏–±–∫–∏ –≤—Ä–µ–º–µ–Ω–∏ –≤—ã–ø–æ–ª–Ω–µ–Ω–∏—è
 * CASTTDX         - –æ—Ç–∫–ª—é—á–∞–µ—Ç —Å–ª–µ–∂–µ–Ω–∏–µ –∑–∞ –∏—Å–∫–ª—é—á–µ–Ω–∏—è–º–∏ –ø–æ–¥ –æ—Ç–ª–∞–¥—á–∏–∫–æ–º TDX
 ----
 * - —Ä–∞–±–æ—Ç–∞–µ—Ç —Ç–æ–ª—å–∫–æ –≤ –∑–∞—â–∏—â–µ–Ω–Ω–æ–º —Ä–µ–∂–∏–º–µ

  –ü—Ä–∏–º–µ—Ä 1:
     move(mem[0:0], mem[0:0], 1);
  –Ω–µ–∑–∞—â–∏—â–µ–Ω–Ω—ã–π –∫–æ–¥ –ø—Ä–∏–≤–µ–¥–µ—Ç –∫ –≤—ã–∑–æ–≤—É TrapProc. –î–ª—è –∑–∞—â–∏—Ç—ã –∏—Å–ø–æ–ª—å–∑—É–µ—Ç—Å—è
  –ø–∞—Ä–∞ Try/TryFree, –∞ –¥–ª—è –≤—ã–∑–æ–≤–∞ –æ—Ç—Ä–∞–±–æ—Ç–∫–∏ –∏—Å–∫–ª—é—á–µ–Ω–∏—è –∏—Å–ø–æ–ª—å–∑—É–µ—Ç—Å—è Raise.

  –ü—Ä–∏–º–µ—Ä 2:
    if Try then                               (* –∑–∞—â–∏—Ç–∞ Try                *)
      move(mem[0:0], mem[0:0], 1);            (* –≤–µ—Ä–æ—è—Ç–Ω–æ–µ –∏—Å–∫–ª—é—á–µ–Ω–∏–µ      *)
      WriteLn('Ok.');
      Raise($218);                            (* —è–≤–Ω–æ–µ –∏—Å–∫–ª—é—á–µ–Ω–∏–µ          *)
      TryFree;                                (* —Å—ä–µ–º –∑–∞—â–∏—Ç—ã Try (cleanup) *)
    end else WriteLn('Exception #', TrapType) (* –æ—Ç—Ä–∞–±–æ—Ç–∫–∞ –∑–∞—â–∏—Ç—ã          *)

   –†–µ–∂–∏–º HARDPROTECTION:
     –ê–Ω–∞–ª–æ–≥ Try —É—Å—Ç–∞–Ω–∞–≤–ª–∏–≤–∞–µ—Ç—Å—è –Ω–∞ –∑–∞–≥–æ–ª–æ–≤–æ–∫
   –≤—Å–µ—Ö –ø—Ä–æ—Ü–µ–¥—É—Ä, –æ—Ç–∫–æ–º–ø–∏–ª–∏—Ä–æ–≤–∞–Ω–Ω—ã—Ö —Å –æ–ø—Ü–∏–µ–π S+, –∏ –ø—Ä–∏ –≤–æ–∑–Ω–∏–∫–Ω–æ–≤–µ–Ω–∏–∏
   –∏—Å–∫–ª—é—á–µ–Ω–∏—è –ø—Ä–æ–∏—Å—Ö–æ–¥–∏—Ç –≤–æ–∑–≤—Ä–∞—Ç –≤ –º–µ—Å—Ç–æ –≤—ã–∑–æ–≤–∞ –ø—Ä–æ—Ü–µ–¥—É—Ä—ã.
     p1:–ö–æ—Ä—Ä–µ–∫—Ç–Ω–∞—è –æ—Ç—Ä–∞–±–æ—Ç–∫–∞ –ø—Ä–æ–∏—Å—Ö–æ–¥–∏—Ç —Ç–æ–ª—å–∫–æ –≤ —Å–ª—É—á–∞–µ far-–≤—ã–∑–æ–≤–æ–≤,
   –∏ –µ—Å–ª–∏ –≤–æ–∑–Ω–∏–∫–∞–µ—Ç –∏—Å–∫–ª—é—á–µ–Ω–∏–µ –∏–ª–∏ –Ω–µ–≤–µ—Ä–Ω—ã–π –≤–æ–∑–≤—Ä–∞—Ç, —Å–ª–µ–¥—É–µ—Ç
   –ø–æ–¥–æ–∑—Ä–∏—Ç–µ–ª—å–Ω—ã–µ near-–ø—Ä–æ—Ü–µ–¥—É—Ä—ã –æ–±—ä—è–≤–∏—Ç—å far –ª–∏–±–æ –ø–µ—Ä–µ–∫–æ–º–ø–∏–ª–∏—Ä–æ–≤–∞—Ç—å
   –≤ —Ä–µ–∂–∏–º–µ S-.
     p2:–≤–Ω—É—Ç—Ä–∏ –ø—Ä–æ—Ü–µ–¥—É—Ä –≤—ã–∑–æ–≤ –æ—á–∏—Å—Ç–∫–∏ TryFree –Ω–µ —Ç—Ä–µ–±—É–µ—Ç—Å—è.

   –î–ª—è TurboVision-–ø–æ–¥–æ–±–Ω—ã—Ö –ø—Ä–æ–≥—Ä–∞–º–º —Ä–µ–∫–æ–º–µ–Ω–¥—É–µ—Ç—Å—è —Ä–µ–∂–∏–º HARDPROTECTION
   –∏ TrapProc = ProtectedTrapProc, –∞ —Ç–∞–∫–∂–µ Shutdown - –ø—Ä–æ—Ü–µ–¥—É—Ä–∞, –≤–æ–∑–º–æ–∂–Ω–æ
   —Å–º–æ–≥—É—â–∞—è –∫–æ—Ä—Ä–µ–∫—Ç–Ω–æ –æ—Ç—Ä–∞–±–æ—Ç–∞—Ç—å –≤—ã—Ö–æ–¥ –∏–∑ –ø—Ä–æ–≥—Ä–∞–º–º—ã:

   –ü—Ä–∏–º–µ—Ä 3:
   procedure TMyApp.HandleEvent(var Event : TEvent);
   begin
      Inherited HandleEvent;
      if (Event.CharCode = 'e') then begin
        ClearEvent(Event);
        asm lldt bx end;
      end;
   end;
   –≤–æ–∑–≤—Ä–∞—Ç –∏–∑ –∏—Å–∫–ª—é—á–µ–Ω–∏—è –ø—Ä–æ–∏–∑–æ–π–¥–µ—Ç –≤ –≤—ã–∑–≤–∞–≤—à–µ–≥–æ HandleEvent.
   ...
    (*$S-*)
    procedure MyAppDone; far;
    begin
      if Application <> Nil then begin
        Message(Application, evCommand, cmQuit, nil);
        Application^.Done;
      end;
    end;
  ...
    ShutDown := MyAppDone;

  –ü—Ä–∏–º–µ—Ä 4:
  var
    J:TJump;
  procedure a0;
    Jump(J);  ->------------------|
  procedure a1;                   |
    Raise(0); ->------|           |
  procedure a2;       |           |
    if Try then a0;   |           |
    if Try then a1; <-|           |
  procedure a3;                   |
    if Try and SetJump then a2; <-|

    –ø–∞—Ä–∞ SetJump/Jump - –∞–Ω–∞–ª–æ–≥ Try/Raise, –Ω–æ –ø—Ä–∏ –≤–ª–æ–∂–µ–Ω–Ω—ã—Ö –≤—ã–∑–æ–≤–∞—Ö
    Raise –≤—Å–µ–≥–¥–∞ –ø–µ—Ä–µ–¥–∞—Å—Ç —É–ø—Ä–∞–≤–ª–µ–Ω–∏–µ –Ω–∞ –ø–æ—Å–ª–µ–¥–Ω–∏–π –≤—ã–∑–≤–∞–Ω–Ω—ã–π Try,
    –∞ SetJump –ø–æ–∑–≤–æ–ª—è–µ—Ç –∫–æ–Ω–∫—Ä–µ—Ç–Ω–æ —É–∫–∞–∑–∞—Ç—å –∞–¥—Ä–µ—Å –ø–µ—Ä–µ—Ö–æ–¥–∞.
}

Type
  TRegisters = Record
    AX, BX, CX, DX, BP, SI, DI, SS, SP, DS, ES, Flags, IP, CS : Word;
    CSLIM, DSLIM, ESLIM, SSLIM : Word;
  End;

Var
  TrapReg   : TRegisters;
  TrapType  : Integer;

Const
  TrapProc  : Procedure = Nil; {user function called when the trap occured, must be $S-,F+}
  Shutdown  : Procedure = Nil; {user function called before shutdown, must be $S-,F+}
  Recovery  : Procedure = Nil; {user function called after exception ignore}

Procedure ProtectedTrapProc;
Procedure HaltTrapProc;
Procedure SwapExceptions;
Function  TrapDescription(ATrapType : Word) : String;



Implementation

Type
  TStoreRec = Record
    _ID, _SS, _SP, _BP : Word;
    case Byte of
    0 : (_CSIP    : Pointer);
    1 : (_IP, _CS : Word);
  End;
  TStoreArray = array[0..1023] of TStoreRec;
  PStoreArray = ^TStoreArray;


Var
  ABuf      : Word;
  OrgStack  : Pointer;
  OrgAdst   : Pointer;
  OrgRTM    : Pointer;
  CallStack : TStoreArray;
  SaveExc   : array[0..15] of Pointer;


Const
  SizeOfStoreRec = SizeOf(TStoreRec);
  CallCount      : Word = 0;
  SwapExc        : Boolean = False;
  DebugMode      : Boolean = False;
  UseHardProt    : Boolean = True;



Var
  OwnStack : array[0..1767] of Byte;
  SSSP     : Pointer;

Procedure Xepb; far;
Begin
  TrapType := $202;
  FillChar(TrapReg, SizeOf(TrapReg), 0);
  HaltTrapProc;
End;

Function GetExceptionVec(ExpNo : Byte) : Pointer; Assembler; Asm
  mov ax, 202h
  mov bl, ExpNo
  int 31h
  mov ax, dx
  mov dx, cx
End;

Procedure SetExceptionVec(ExpNo : Byte; NewInt : Pointer); Assembler; Asm
  mov ax, 203h
  mov bl, ExpNo
  les dx, NewInt
  mov cx, es
  int 31h
End;

function Hex(W : Word) : string;
const
  Digits : array[0..$F] of Char = '0123456789ABCDEF';
  {-Return hex string for word}
begin
  Hex[0] := #4;
  Hex[1] := Digits[hi(W) shr 4];
  Hex[2] := Digits[hi(W) and $F];
  Hex[3] := Digits[lo(W) shr 4];
  Hex[4] := Digits[lo(W) and $F];
end;

function Long2Str(I : LongInt) : String;
var
  S : String;
Begin
  Str(I, S);
  Long2Str := S;
End;


Procedure HaltTrapProc;
Begin
  {$IFDEF VERBOSE}
  {$IFNDEF RUSSIAN}
  WriteLn('The program in this session encountered an error.');
  WriteLn('The system detected Trap #', Hex(TrapType));
  {$ELSE}
  WriteLn('è‡Æ£‡†¨¨† ¢ Ì‚Æ© ·•··®® Ø‡Æ®ß¢•´† ÆË®°™„.');
  WriteLn('ë®·‚•¨† Æ°≠†‡„¶®´† Trap #', Hex(TrapType));
  {$ENDIF}
  WriteLn('AX:', Hex(TrapReg.AX), '       ', 'BX:', Hex(TrapReg.BX), '       ', 'CSLIM:', Hex(TrapReg.CSLIM));
  WriteLn('CX:', Hex(TrapReg.CX), '       ', 'DX:', Hex(TrapReg.DX), '       ', 'DSLIM:', Hex(TrapReg.DSLIM));
  WriteLn('SI:', Hex(TrapReg.SI), '       ', 'DI:', Hex(TrapReg.DI), '       ', 'ESLIM:', Hex(TrapReg.ESLIM));
  WriteLn('DS:', Hex(TrapReg.DS), '       ', 'ES:', Hex(TrapReg.ES), '       ', 'SSLIM:', Hex(TrapReg.SSLIM));
  WriteLn('SS:', Hex(TrapReg.SS), '       ', 'SP:', Hex(TrapReg.SP));
  WriteLn('CS:', Hex(TrapReg.CS), '       ', 'IP:', Hex(TrapReg.IP));
  WriteLn('BP:', Hex(TrapReg.BP), '       ', 'Flags:', Hex(TrapReg.Flags));
  {$ELSE}
  {$IFNDEF RUSSIAN}
  WriteLn('General protection fault.');
  {$ELSE}
  WriteLn('é°È†Ô ÆË®°™† ß†È®‚Î.');
  {$ENDIF}
  {$ENDIF}
  Halt(217);
End;

Var
  ScreenBuffer : Pointer;

Procedure ProtectedTrapProc;
var
  P : Pointer;
  Cursor, Lines : Word;
  Vidmode : Byte;
  F : File;

Type
  iString = String[80];


Procedure TextOut(Text : iString; X, Y, Attr : Word); Assembler; Asm
  cld
  push ds
  mov bl, 80
  xor bh, bh
  les di, ScreenBuffer
  lds si, Text
  lodsb
  or al, al
  jz @@Exit
  mov cl, al
  xor ch, ch
  mov ax, y
  shl ax, 1
  mul bx
  add di, ax
  mov ax, x
  shl ax, 1
  add di, ax
  mov dx, Attr
@@1:
  lodsb
  mov ah, dl
  stosw
  loop @@1
@@Exit:
  pop ds
End;

Function Key : Word; Assembler; Asm xor ax, ax; int 16h end;

Procedure TextOutC(S : iString; Y : Integer);
Begin
  TextOut(S, (80 - Length(S)) div 2, Y, $1f);
End;

Var
  CK : Word;
  OldProc : Procedure;

Const
  kbESC = 283;
  kbF3 = 15616;
  kbF5 = 16128;
  kbF7 = 16640;

Begin
  ScreenBuffer := Ptr(SegB800, 0);
  OldProc := TrapProc;
  TrapProc := HaltTrapProc;
  GetMem(P, 16384);
  if P = Nil then begin
    Assign(F, 'fdmp.$$$');
    Rewrite(F, 1);
    BlockWrite(F, ScreenBuffer^, 16384);
  end else Move(ScreenBuffer^, P^, 16384);
  asm
    mov ah, $f
    int 10h
    mov Vidmode, al
    mov ah, 3
    xor bh, bh
    int 10h
    mov Cursor, dx
    mov Lines, cx
    mov ax, 83h
    int 10h
    cld
    mov ax, 1020h
    mov cx, 8000
    les di, ScreenBuffer
    rep stosw
  end;

  {$IFDEF DPMI}
  TextOut('€€€€ DPMI 16 (' + ParamStr(0) + ')', 0, 0, $1f);
  {$ENDIF}
  {$IFNDEF RUSSIAN}
  TextOutC('The program in this session encountered an error at '+Hex(TrapReg.CS)+':'+Hex(TrapReg.IP), 2);
  TextOutC('The system detected Trap #'+ Hex(TrapType)+' ('+Long2Str(TrapType)+')', 3);
  {$ELSE}
  TextOutC('è‡Æ£‡†¨¨† Ø‡Æ®ß¢•´† ÆË®°™„ ØÆ †§‡•·„ '+Hex(TrapReg.CS)+':'+Hex(TrapReg.IP), 2);
  TextOutC('ë®·‚•¨† Æ°≠†‡„¶®´† Trap #'+ Hex(TrapType)+' ('+Long2Str(TrapType)+')', 3);

  {$ENDIF}
  {$IFDEF VERBOSE}
  TextOutC('"'+TrapDescription(TrapType)+'"', 4);
  {$ENDIF}
  TextOut('AX:'+ Hex(TrapReg.AX)+ '       BX:'+ Hex(TrapReg.BX)+'       CSLIM:'+Hex(TrapReg.CSLIM), 5, 5, $1f);
  TextOut('CX:'+ Hex(TrapReg.CX)+ '       DX:'+ Hex(TrapReg.DX)+'       DSLIM:'+Hex(TrapReg.DSLIM), 5, 6, $1f);
  TextOut('SI:'+ Hex(TrapReg.SI)+ '       DI:'+ Hex(TrapReg.DI)+'       ESLIM:'+Hex(TrapReg.ESLIM), 5, 7, $1f);
  TextOut('DS:'+ Hex(TrapReg.DS)+ '       ES:'+ Hex(TrapReg.ES)+'       SSLIM:'+Hex(TrapReg.SSLIM), 5, 8, $1f);
  TextOut('SS:'+ Hex(TrapReg.SS)+ '       SP:'+ Hex(TrapReg.SP), 5, 8, $1f);
  TextOut('BP:'+ Hex(TrapReg.BP)+ '       Flags:'+ Hex(TrapReg.Flags), 5, 9, $1f);
  {$IFNDEF RUSSIAN}
  TextOutC('Press the following keys to perform user actions or use Ctrl+Alt+Del to reboot', 12);
  TextOut('ESC   - Ignore error and continue', 3, 14, $1f);
  TextOut('F3    - Exit immediately', 3, 15, $1f);
  if @Shutdown <> Nil then TextOut('F5    - Try to perform application shutdown', 3, 16, $1f);
  TextOut('F7    - Debugger step', 3, 17, $1f);
  {$ELSE}
  TextOutC('ÇÎ°•‡®‚• §•©·‚¢®• ®ß ™´†¢®Ë ¢≠®ß„ ®´® ≠†¶¨®‚• Ctrl+Alt+Del §´Ô Ø•‡•ß†£‡„ß™®', 12);
  TextOut('ESC   - à£≠Æ‡®‡Æ¢†‚Ï ÆË®°™„ ® Ø‡Æ§Æ´¶®‚Ï', 3, 14, $1f);
  TextOut('F3    - ç•¨•§´•≠≠Î© ¢ÎÂÆ§', 3, 15, $1f);
  if @Shutdown <> Nil then TextOut('F5    - èÆØÎ‚†‚Ï·Ô ß†™‡Î‚Ï Ø‡®´Æ¶•≠®•', 3, 16, $1f);
  TextOut('F7    - ò†£ Æ‚´†§Á®™†', 3, 17, $1f);
  {$ENDIF}


  Repeat
    CK := Key;
    if (CK = kbESC) or (CK = kbF3) or (CK = kbF7) then Break;
    if (@Shutdown <> Nil) and (CK = kbF5) then Break;
  Until false;

  asm
    mov ah, 0
    mov al, Vidmode
    add al, 80h
    int 10h
    mov ah, 2
    xor bh, bh
    mov dx, Cursor
    int 10h
    mov ah, 1
    mov cx, Lines
    int 10h
  end;

  if P = Nil then begin
    Seek(F, 0);
    BlockRead(F, ScreenBuffer^, 16384);
    Close(F);
    Erase(F);
  end else Move(P^, ScreenBuffer^, 16384);
  if P <> Nil then FreeMem(P, 16384);
  if (CK = kbF5) and (@Shutdown <> Nil) then Shutdown;
  if CK = kbF3 then HaltTrapProc;
  if CK = kbF5 then Halt(218);
  TrapProc := OldProc;
  if CK = kbF7 then asm int 3 end else
    if @Recovery <> Nil then Recovery;
End;


Function GetLimit(Selector : Word) : Word; Near; Assembler;
Var
  _Desc : array[0..7] of Byte;
Asm
  mov ax, ss
  mov es, ax
  mov di, sp
  mov ax, 0bh
  mov bx, Selector
  int 31h
  mov ax, es:[di]
End;

Procedure FillLimits; Near;
Begin
  TrapReg.CSLIM := GetLimit(TrapReg.CS);
  TrapReg.DSLIM := GetLimit(TrapReg.DS);
  TrapReg.ESLIM := GetLimit(TrapReg.ES);
  TrapReg.SSLIM := GetLimit(TrapReg.SS);
End;

Function Try : Boolean; Assembler;
Asm
  cli
  cld
  mov ax, seg @data
  mov ds, ax
  mov es, ax
  mov di, offset CallStack
  mov ax, SizeOfStoreRec
  mul CallCount
  add di, ax
  cmp CallCount, 1023
  je  @@OverFlow
  inc CallCount
  xor  ax, ax
  xchg al, UseHardProt
  shl  al, 1
  or   al, 4
  stosw
  mov ax, ss
  stosw
  mov ax, sp
  add ax, 4
  stosw
  mov ax, bp
  stosw
  pop ax
  stosw
  pop dx
  xchg dx, ax
  stosw
  push ax
  push dx
  mov al, 1
  sti
  retf
@@OverFlow:
  sti
  jmp Xepb
End;

Procedure Return; Assembler;
Asm
  cli
  cld
  push di
  push es
  push ds
  push ax
  mov ax, seg @data
  mov ds, ax
  mov es, ax
  pop ax
  mov di, offset TrapReg
  stosw
  mov ax, bx
  stosw
  mov ax, cx
  stosw
  mov ax, dx
  stosw
  mov ax, bp
  stosw
  mov ax, si
  stosw
  pop ax
  stosw
  mov ax, ss
  stosw
  mov ax, sp
  stosw
  pop ax {ds}
  stosw
  pop ax {es}
  stosw
  lahf
  stosw
  pop ax {di}
  stosw
  mov dx, ax
  pop ax
  stosw
  push ax
  push dx

  dec CallCount
  cmp CallCount, 0ffffh
  je @@Noload
  mov si, offset CallStack
  mov ax, SizeOfStoreRec
  mul CallCount
  add si, ax
  lodsw
  mov bx, ax
  test al, 4
  jz @@NoHardProtAdjust
  shr al, 1
  and al, not 2
  mov UseHardProt, al
@@NoHardProtAdjust:
  and bx, 1
  lodsw
  mov ss, ax
  lodsw
  mov sp, ax
  lodsw
  mov bp, ax
  lodsw
  mov dx, ax
  lodsw
  push ax
  push dx

@@Noload:
  mov word ptr [sssp], sp
  mov word ptr [sssp+2], ss
  mov ax, ds
  mov ss, ax
  mov sp, offset OwnStack + 1766
  {$IFDEF DPMI}
  push bx
  call FillLimits
  pop bx
  {$ENDIF}
  mov ax, word ptr [TrapProc]
  or  ax, word ptr [TrapProc+2]
  jz  @@Further
  or  bx, bx
  jz  @@Further
  call TrapProc
@@Further:
  cmp CallCount, 0ffffh
  jz  @@Overflow
  mov al, 0
  mov sp, word ptr [sssp]
  mov ss, word ptr [sssp+2]
  jmp @@Exit
@@OverFlow:
  sti
  jmp Xepb
@@Exit:
  sti
End;


Procedure TryFree; Assembler; Asm
  mov ax, ds
  mov es, ax
  cmp CallCount, 0
  je @@OverFlow
  mov si, offset CallStack
  mov ax, SizeOfStoreRec
  mul CallCount
  add si, ax
  sub si, SizeOfStoreRec
  lodsw
  test al, 4
  jz @@NoHardProtAdjust
  and al, 2
  shr al, 1
  mov UseHardProt, al
@@NoHardProtAdjust:
  dec CallCount
  jmp @@Exit
@@OverFlow:
  jmp Xepb
@@Exit:
End;

Procedure Raise; Assembler; Asm
  mov ax, ErrorType
  mov TrapType, ax
  mov ax, ds
  mov es, ax
  jmp Return
End;

Function  SetJump; Assembler; Asm
  les di, E
  cld
  xor ax, ax
  stosw
  stosb
  mov ax, bp
  add ax, 10
  stosw
  mov ax, [bp].word
  stosw
  mov ax, [bp+2].word
  stosw
  mov ax, [bp+4].word
  stosw
  mov al, 1
End;

Procedure Jump; Assembler; Asm
  les di, E
  cld
  mov ax, Result
  stosw
  mov al, 1
  stosb
  mov sp, es:[di]
  mov bp, es:[di+2]
  mov ax, es:[di+6]
  push ax
  mov ax, es:[di+4]
  push ax
  xor ax, ax
  retf
End;

Procedure GeneralTrap; far; assembler; asm
  mov ax, seg @data
  mov ds, ax
  mov bh, $10
  mov TrapType, bx
  add sp, 8
  pop ABuf
  push ABuf
  mov ax, 3
  arpl ABuf, ax
  sub sp, 2
  jmp Return
end;

Procedure Trap00; far; assembler; asm  mov bl, 00; jmp GeneralTrap; end;
Procedure Trap01; far; assembler; asm  mov bl, 01; jmp GeneralTrap; end;
Procedure Trap02; far; assembler; asm  mov bl, 02; jmp GeneralTrap; end;
Procedure Trap03; far; assembler; asm  mov bl, 03; jmp GeneralTrap; end;
Procedure Trap04; far; assembler; asm  mov bl, 04; jmp GeneralTrap; end;
Procedure Trap05; far; assembler; asm  mov bl, 05; jmp GeneralTrap; end;
Procedure Trap06; far; assembler; asm  mov bl, 06; jmp GeneralTrap; end;
Procedure Trap07; far; assembler; asm  mov bl, 07; jmp GeneralTrap; end;
Procedure Trap08; far; assembler; asm  mov bl, 08; jmp GeneralTrap; end;
Procedure Trap09; far; assembler; asm  mov bl, 09; jmp GeneralTrap; end;
Procedure Trap10; far; assembler; asm  mov bl, 10; jmp GeneralTrap; end;
Procedure Trap11; far; assembler; asm  mov bl, 11; jmp GeneralTrap; end;
Procedure Trap12; far; assembler; asm  mov bl, 12; jmp GeneralTrap; end;
Procedure Trap13; far; assembler; asm  mov bl, 13; jmp GeneralTrap; end;
Procedure Trap14; far; assembler; asm  mov bl, 14; jmp GeneralTrap; end;
Procedure Trap15; far; assembler; asm  mov bl, 15; jmp GeneralTrap; end;


Var
  JLeave : Pointer;

procedure StdLeave; far; assembler; asm
  push ax
  push dx
  push si
  mov ax, seg @data
  mov ds, ax
  cmp CallCount, 0
  je @@OverFlow
  cld

  dec CallCount
  mov si, offset CallStack
  mov ax, SizeOfStoreRec
  mul CallCount
  add si, ax
  add si, 4 {id, ss}

  lodsw
  sub ax, 6 {pushes before}
  cmp ax, sp
  je @@NoInc
  sub si, 2
@@Again:
  lodsw
  sub ax, 6
  cmp ax, sp
  ja  @@Inc
  dec CallCount
  cmp CallCount, 0
  jl  @@ZeroInc
  sub si, SizeOfStoreRec + 2
  jmp @@Again
@@Inc:
  add si, SizeOfStoreRec
@@ZeroInc:
  inc CallCount
@@NoInc:
  add si, 2 {bp}
  mov ax, [si]
  mov word ptr [JLeave], ax
  mov ax, [si+2]
  mov word ptr [JLeave+2], ax
  pop si
  pop dx
  pop ax
  jmp dword ptr [JLeave]
@@Overflow:
  jmp Xepb
end;


procedure Enter; far; assembler; asm
  cmp UseHardProt, 0
  jz  @@RTClear
  mov bx, bp
  add bx, 4
  cmp ss:[0ch], bx
  jbe @@RTClear             {looks like unit init call}
  mov bx, ss:[bp+4]
  verr bx
  jnz @@RTClear             {looks like far call, readable segment.}
  verw bx
  jz @@RTClear              {but writable, not a code.}
  push ds
  push ax
  push si
  push di                   {writing record}
  push es
  cld
  mov ax, seg @data
  mov ds, ax
  mov es, ax
  mov di, offset CallStack
  mov ax, SizeOfStoreRec
  mul CallCount
  add di, ax
  cmp CallCount, 1023
  je  @@OverFlow
  inc CallCount
  mov ax, 1
  stosw               {id}
  mov ax, ss
  stosw               {ss}
  mov ax, bp
  add ax, 6           {sp}
  stosw
  mov ax, ss:[bp]     {bp}
  stosw
  mov ax, ss:[bp+2]   {ip}
  stosw
  mov ax, ss:[bp+4]   {cs}
  stosw
  mov  ax, offset StdLeave
  mov  ss:[bp+2], ax
  mov  ss:[bp+4], cs
  pop  es
  pop  di
  pop  si
  pop  ax
  pop  ds
  jmp @@RTClear
@@Overflow:
  mov ax, $ffff
  jmp dword ptr [OrgStack]  {simulate stack overflow}
@@RTClear:
  add ax, 200h              {overwritten runtime}
  jnb @@1
  jmp dword ptr [OrgAdst]
@@1:
  jmp dword ptr [OrgStack]  {jmp to runtime}
end;

Procedure RunErrorCover; Assembler; Asm
  mov dx, seg @data
  mov ds, dx
  or  bx, cx
  jz  @@SimpleHalt
  mov TrapType, ax
  jmp Return
@@SimpleHalt:
End;

Procedure SwapExceptions;
Var
  I : Integer;
Begin
  {$IFDEF EXCEPTIONS}
  if SwapExc then begin
    for I := 0 to 15 do SetExceptionVec(I, SaveExc[I]);
  end else begin
    {$IFNDEF RUNERROR} SetExceptionVec(00, @Trap00); {$ENDIF}
    SetExceptionVec(04, @Trap04);
    SetExceptionVec(05, @Trap05);
    SetExceptionVec(06, @Trap06);
    SetExceptionVec(08, @Trap08);
    SetExceptionVec(09, @Trap09);
    SetExceptionVec(10, @Trap10);
    SetExceptionVec(12, @Trap12);
    SetExceptionVec(13, @Trap13);
  end;
  SwapExc := not SwapExc;
  {$ENDIF}
End;

Procedure GetExc;
Var
  I : Integer;
Begin
  for I := 0 to 15 do SaveExc[I] := GetExceptionVec(I);
End;

Function  TrapDescription(ATrapType : Word) : String;
Begin
  case ATrapType of
  {$IFNDEF RUSSIAN}
  1      : TrapDescription := 'Invalid DOS function code';
  2      : TrapDescription := 'File not found';
  3      : TrapDescription := 'Path not found';
  4      : TrapDescription := 'Too many open files';
  5      : TrapDescription := 'File access denied';
  6      : TrapDescription := 'Invalid file handle';
  8      : TrapDescription := 'Not enough memory';
  12     : TrapDescription := 'Invalid file access code';
  15     : TrapDescription := 'Invalid drive number';
  16     : TrapDescription := 'Cannot remove current directory';
  17     : TrapDescription := 'Cannot rename across drives';
  100    : TrapDescription := 'Disk read error';
  101    : TrapDescription := 'Disk write error';
  102    : TrapDescription := 'File not assigned';
  103    : TrapDescription := 'File not open';
  104    : TrapDescription := 'File not open for input';
  105    : TrapDescription := 'File not open for output';
  106    : TrapDescription := 'Invalid numeric format';
  150    : TrapDescription := 'Disk is write-protected';
  152    : TrapDescription := 'Drive not ready'; 
  154    : TrapDescription := 'CRC error in data'; 
  156    : TrapDescription := 'Disk seek error'; 
  158    : TrapDescription := 'Sector not found';
  159    : TrapDescription := 'Printer out of paper';
  160    : TrapDescription := 'Device write fault';
  161    : TrapDescription := 'Device read fault';
  162    : TrapDescription := 'Hardware failure';
  200    : TrapDescription := 'Division by zero';
  201    : TrapDescription := 'Range check error';
  202    : TrapDescription := 'Stack overflow error';
  203    : TrapDescription := 'Heap overflow error';
  204    : TrapDescription := 'Invalid pointer operation';
  205    : TrapDescription := 'Floating point overflow';
  206    : TrapDescription := 'Floating point underflow';
  207    : TrapDescription := 'Invalid floating point operation';
  208    : TrapDescription := 'Overlay manager not installed';
  209    : TrapDescription := 'Overlay file read error';
  210    : TrapDescription := 'Object not initialized';
  211    : TrapDescription := 'Call to abstract method';
  212    : TrapDescription := 'Stream registration error';
  213    : TrapDescription := 'Collection index out of range';
  214    : TrapDescription := 'Collection overflow error';
  215    : TrapDescription := 'Arithmetic overflow';
  216    : TrapDescription := 'General protection fault';
  217, $202 : TrapDescription := 'Exception shield trace stack underflow';
  $1000  : TrapDescription := 'Division on zero, processor';
  $1001  : TrapDescription := 'Single step';
  $1002  : TrapDescription := 'Non-maskable interrupt';
  $1003  : TrapDescription := 'Breakpoint';
  $1004  : TrapDescription := 'Processor overflow';
  $1005  : TrapDescription := 'Array bounds exceeded';
  $1006  : TrapDescription := 'Illegal instruction';
  $1007  : TrapDescription := 'No coprocessor';
  $1008  : TrapDescription := 'Double exception';
  $1009  : TrapDescription := 'Coprocessor access violation';
  $100a  : TrapDescription := 'Invalid task state segment';
  $100b  : TrapDescription := 'Page violation';
  $100c  : TrapDescription := 'Stack violation';
  $100d  : TrapDescription := 'Memory access violation';
  $100e  : TrapDescription := 'Page fault';
  $1010  : TrapDescription := 'Coprocessor exception';
  $1011  : TrapDescription := 'Data type misalignment';
  $2000 :  TrapDescription := 'User generated error';
  else     TrapDescription := 'Runtime error ' + Long2Str(ATrapType);
  {$ELSE}
  1      : TrapDescription := 'ç•¢•‡≠Î© ™Æ§ ‰„≠™Ê®® Ñéë';
  2      : TrapDescription := 'î†©´ ≠• ≠†©§•≠';
  3      : TrapDescription := 'è„‚Ï ≠• ≠†©§•≠';
  4      : TrapDescription := 'ë´®Ë™Æ¨ ¨≠Æ£Æ Æ‚™‡Î‚ÎÂ ‰†©´Æ¢';
  5      : TrapDescription := 'ÑÆ·‚„Ø ß†™‡Î‚';
  6      : TrapDescription := 'ç•¢•‡≠Î© „™†ß†‚•´Ï ‰†©´†';
  8      : TrapDescription := 'ç•§Æ·‚†‚ÆÁ≠Æ Ø†¨Ô‚®';
  12     : TrapDescription := 'ç•¢•‡≠Î© ™Æ§ §Æ·‚„Ø† ™ ‰†©´„';
  15     : TrapDescription := 'ç•¢•‡≠Î© ™Æ§ §®·™†';
  16     : TrapDescription := 'ç•¢Æß¨Æ¶≠Æ „§†´®‚Ï ‚•™„È„Ó §®‡•™‚Æ‡®Ó';
  17     : TrapDescription := 'ç•¢Æß¨Æ¶≠Æ Ø•‡•®¨•≠Æ¢†‚Ï ‰†©´ ≠† §‡„£Æ© §®·™';
  100    : TrapDescription := 'éË®°™† Á‚•≠®Ô §®·™†';
  101    : TrapDescription := 'éË®°™† ß†Ø®·® §®·™†';
  102    : TrapDescription := 'î†©´ ≠• ≠†ß≠†Á•≠';
  103    : TrapDescription := 'î†©´ ≠• Æ‚™‡Î‚';
  104    : TrapDescription := 'î†©´ ≠• Æ‚™‡Î‚ ≠† ¢¢Æ§';
  105    : TrapDescription := 'î†©´ ≠• Æ‚™‡Î‚ ≠† ¢Î¢Æ§';
  106    : TrapDescription := 'ç•¢•‡≠Î© Á®·´Æ¢Æ© ‰Æ‡¨†‚';
  150    : TrapDescription := 'Ñ®·™ ß†È®È•≠ Æ‚ ß†Ø®·®';
  152    : TrapDescription := 'Ñ®·™ ≠• £Æ‚Æ¢'; 
  154    : TrapDescription := 'éË®°™† Ê•´Æ·‚≠Æ·‚® §†≠≠ÎÂ'; 
  156    : TrapDescription := 'éË®°™† ØÆ®·™† §®·™†'; 
  158    : TrapDescription := 'ë•™‚Æ‡ ≠• ≠†©§•≠';
  159    : TrapDescription := 'ç•‚ °„¨†£® ¢ Ø‡®≠‚•‡•';
  160    : TrapDescription := 'éË®°™† ß†Ø®·® „·‚‡Æ©·‚¢†';
  161    : TrapDescription := 'éË®°™† Á‚•≠®Ô „·‚‡Æ©·‚¢†';
  162    : TrapDescription := 'ÄØØ†‡†‚≠Î© ·°Æ©';
  200    : TrapDescription := 'Ñ•´•≠®• ≠† ≠Æ´Ï';
  201    : TrapDescription := 'è‡•¢ÎË•≠®• £‡†≠®Ê ¨†··®¢†';
  202    : TrapDescription := 'è•‡•ØÆ´≠•≠®• ·‚•™†';
  203    : TrapDescription := 'è•‡•ØÆ´≠•≠®• ·¢Æ°Æ§≠Æ© Ø†¨Ô‚®';
  204    : TrapDescription := 'ç•™Æ‡‡•™‚≠†Ô ÆØ•‡†Ê®Ô · „™ß†‚•´•¨';
  205    : TrapDescription := 'è•‡•ØÆ´≠•≠®• ¢ ÆØ•‡†Ê®® · Ø´†¢†ÓÈ•© ‚ÆÁ™Æ©';
  206    : TrapDescription := 'èÆ‚•‡Ô ‚ÆÁ≠Æ·‚® ¢ ÆØ•‡†Ê®® · Ø´†¢†ÓÈ•© ‚ÆÁ™Æ©';
  207    : TrapDescription := 'ç•¢•‡≠†Ô ÆØ•‡†Ê®Ô · Ø´†¢†ÓÈ•© ‚ÆÁ™Æ©';
  208    : TrapDescription := 'ç• „·‚†≠Æ¢´•≠ ¨•≠•§¶•‡ Æ¢•‡´••¢';
  209    : TrapDescription := 'éË®°™† Á‚•≠®Ô Æ¢•‡´•©≠Æ£Æ ‰†©´†';
  210    : TrapDescription := 'é°Í•™‚ ≠• ®≠®Ê®†´®ß®‡Æ¢†≠';
  211    : TrapDescription := 'ÇÎßÆ¢ †°·‚‡†™‚≠Æ£Æ ¨•‚Æ§†';
  212    : TrapDescription := 'éË®°™† ‡•£®·‚‡†Ê®®';
  213    : TrapDescription := 'ÇÎÂÆ§ ß† £‡†≠®Ê„ ™Æ´´•™Ê®®';
  214    : TrapDescription := 'è•‡•ØÆ´•≠®• ™Æ´´•™Ê®®';
  215    : TrapDescription := 'Ä‡®‰¨•‚®Á•·™Æ• Ø•‡•ØÆ´≠•≠®•';
  216    : TrapDescription := 'é°È†Ô ÆË®°™† ß†È®‚Î';
  217, $202 : TrapDescription := 'ÇÎÂÆ§ ß† £‡†≠®Ê„ ·‚•™† ¨Æ§„´Ô ß†È®‚Î';
  $1000  : TrapDescription := 'Ñ•´•≠®• ≠† ≠Æ´Ï, Ø‡ÆÊ•··Æ‡';
  $1001  : TrapDescription := 'ò†£Æ¢Æ• Ø‡•‡Î¢†≠®•';
  $1002  : TrapDescription := 'ç•¨†·™®‡„•¨Æ• Ø‡•‡Î¢†≠®•';
  $1003  : TrapDescription := 'è‡•‡Î¢†≠®• ‚ÆÁ™® Æ·‚†≠Æ¢†';
  $1004  : TrapDescription := 'è•‡•ØÆ´≠•≠®• Ø‡ÆÊ•··Æ‡†';
  $1005  : TrapDescription := 'è‡•¢ÎË•≠®• £‡†≠®Ê ¨†··®¢†';
  $1006  : TrapDescription := 'ç•¢•‡≠†Ô ®≠·‚‡„™Ê®Ô Ø‡ÆÊ•··Æ‡†';
  $1007  : TrapDescription := 'é‚·„‚·‚¢®• ·ÆØ‡ÆÊ•··Æ‡†';
  $1008  : TrapDescription := 'Ñ¢Æ©≠Æ• ®·™´ÓÁ•≠®•';
  $1009  : TrapDescription := 'éË®°™† §Æ·‚„Ø† ¢ ·ÆØ‡ÆÊ•··Æ‡•';
  $100a  : TrapDescription := 'ç•¢•‡≠Î© ‡•£®·‚‡ ·Æ·‚ÆÔ≠®Ô ß†§†Á®';
  $100b  : TrapDescription := 'éË®°™† §Æ·‚„Ø† ·‚‡†≠®ÊÎ';
  $100c  : TrapDescription := 'éË®°™† §Æ·‚„Ø† ·‚•™†';
  $100d  : TrapDescription := 'éË®°™† §Æ·‚„Ø† ™ Ø†¨Ô‚®';
  $100e  : TrapDescription := 'é‚™†ß ·‚‡†≠®ÊÎ';
  $1010  : TrapDescription := 'à·™´ÓÁ•≠®• ·ÆØ‡ÆÊ•··Æ‡†';
  $1011  : TrapDescription := 'ç•¢Î‡†¢≠•≠≠Î© ·•£¨•≠‚ §†≠≠ÎÂ';
  $2000  : TrapDescription := 'éË®°™† ·£•≠•‡®‡Æ¢†≠≠†Ô ØÆ´ÏßÆ¢†‚•´•¨';
  else     TrapDescription := 'éË®°™† ¢‡•¨•≠® ¢ÎØÆ´≠•≠®Ô ' + Long2Str(ATrapType);
  {$ENDIF}
  end;

End;

Function DetectTDX : Boolean;
Begin
  {$IFNDEF CASTTDX}
  DebugMode := Byte(SaveExc[3]^) = $55;
  DetectTDX := DebugMode;
  {$ELSE}
  DetectTDX := False;
  {$ENDIF}
End;

{$S+}procedure Dummy; begin end;{$S-}
procedure DummyRTM; near; begin RunError(0); end;

var
  ExitSave : Pointer;

procedure Ex1; far;
begin
  ExitProc := ExitSave;
  SwapExceptions;
end;


begin
  OrgStack := Pointer(Pointer(LongInt(@Dummy) + 6)^);
  {$IFDEF HARDPROTECTION}
(*  {$IFDEF VERBOSE}
  WriteLn('(!) Warning - EShield HARDPROTECTION = ON');
  {$ENDIF} *)
  OrgAdst  := OrgStack;
  asm
    {$IFDEF DPMI}
    {Create tmp alias selector for stack runtime}
    mov ax, 0ah
    mov bx, word ptr [OrgStack+2]
    int 31h
    {set calling code. only for BP 7.0 runtime.}
    cld
    {$ELSE}
    mov ax, word ptr [OrgStack+2]
    {$ENDIF}
    mov es, ax
    mov di, word ptr [OrgStack]
    mov al, 0eah {jmp far}
    stosb
    mov ax, offset Enter
    stosw
    mov ax, cs
    stosw
    {$IFDEF DPMI}
    {free selector}
    mov ax, 1
    mov bx, es
    int 31h
    {$ENDIF}
    {increment $jmp}
    add word ptr [OrgStack], 5
    add word ptr [OrgAdst],  01eh
  end;
  {$ENDIF}

  {$IFDEF RUNERROR}
  OrgRTM   := Pointer(LongInt(Pointer(LongInt(@DummyRTM) + 6)^) +
  {$IFDEF DPMI}8{$ELSE}11{$ENDIF}
  );
  asm
    {$IFDEF DPMI}
    {Create tmp alias selector for runtime handler}
    mov ax, 0ah
    mov bx, word ptr [OrgRTM+2]
    int 31h
    {set calling code. only for BP 7.0 runtime handler.}
    cld
    {$ELSE}
    mov ax, word ptr [OrgRTM+2]
    {$ENDIF}
    mov es, ax
    mov di, word ptr [OrgRTM]
    mov al, 09ah {call far}
    stosb
    mov ax, offset RunErrorCover
    stosw
    mov ax, cs
    stosw
    {$IFDEF DPMI}
    {free selector}
    mov ax, 1
    mov bx, es
    int 31h
    {$ENDIF}
  end;
  {$ENDIF}


  TrapProc := HaltTrapProc;
  {$IFDEF EXCEPTIONS}
  GetExc;
  if DetectTDX then
    {$IFDEF VERBOSE}
    WriteLn('TDX detected. Exception shield disabled.')
    {$ENDIF}
  else begin
    SwapExceptions;
    ExitSave := ExitProc;
    ExitProc := @Ex1;
  end;
  {$ENDIF}
end.