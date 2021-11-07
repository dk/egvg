
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Drivers;

{$F+,X+,I-,S-,P-,G+}
{$C FIXED PRELOAD PERMANENT}
{$DEFINE Russian}
{.$DEFINE Ukrainian}

{$IFDEF Ukrainian}
  {$DEFINE Russian}
{$ENDIF}

{$DEFINE EXTENDEDSOUND}

interface


uses Objects,
{$IFDEF EXTENDEDSOUND}
  WavePlay,
{$ENDIF}
  GDI;

{ ******** EVENT MANAGER ******** }

const

{ Patch Info }

PatchVersion:

{$IFDEF Ukrainian}
  String[37] = 'TV2SVG 07-28-96 03:56pm. Ukrainian.';
{$ELSE}
  {$IFDEF Russian}
  String[35] = 'TV2SVG 07-28-96 03:56pm. Russian.';
  {$ELSE}
  String[35] = 'TV2SVG 07-28-96 03:56pm. English.';
  {$ENDIF}
{$ENDIF}

{ Event codes }

  evMouseDown = $0001;
  evMouseUp   = $0002;
  evMouseMove = $0004;
  evMouseAuto = $0008;
  evKeyDown   = $0010;
  evCommand   = $0100;
  evBroadcast = $0200;

{ Event masks }

  evNothing   = $0000;
  evMouse     = $000F;
  evKeyboard  = $0010;
  evMessage   = $FF00;

{ Extended key codes }

  kbEsc       = $011B;  kbAltSpace  = $0200;  kbCtrlIns   = $0400;
  kbShiftIns  = $0500;  kbCtrlDel   = $0600;  kbShiftDel  = $0700;
  kbBack      = $0E08;  kbCtrlBack  = $0E7F;  kbShiftTab  = $0F00;
  kbTab       = $0F09;  kbAltQ      = $1000;  kbAltW      = $1100;
  kbAltE      = $1200;  kbAltR      = $1300;  kbAltT      = $1400;
  kbAltY      = $1500;  kbAltU      = $1600;  kbAltI      = $1700;
  kbAltO      = $1800;  kbAltP      = $1900;  kbCtrlEnter = $1C0A;
  kbEnter     = $1C0D;  kbAltA      = $1E00;  kbAltS      = $1F00;
  kbAltD      = $2000;  kbAltF      = $2100;  kbAltG      = $2200;
  kbAltH      = $2300;  kbAltJ      = $2400;  kbAltK      = $2500;
  kbAltL      = $2600;  kbAltZ      = $2C00;  kbAltX      = $2D00;
  kbAltC      = $2E00;  kbAltV      = $2F00;  kbAltB      = $3000;
  kbAltN      = $3100;  kbAltM      = $3200;  kbF1        = $3B00;
  kbF2        = $3C00;  kbF3        = $3D00;  kbF4        = $3E00;
  kbF5        = $3F00;  kbF6        = $4000;  kbF7        = $4100;
  kbF8        = $4200;  kbF9        = $4300;  kbF10       = $4400;
  kbHome      = $4700;  kbUp        = $4800;  kbPgUp      = $4900;
  kbGrayMinus = $4A2D;  kbLeft      = $4B00;  kbRight     = $4D00;
  kbGrayPlus  = $4E2B;  kbEnd       = $4F00;  kbDown      = $5000;
  kbPgDn      = $5100;  kbIns       = $5200;  kbDel       = $5300;
  kbShiftF1   = $5400;  kbShiftF2   = $5500;  kbShiftF3   = $5600;
  kbShiftF4   = $5700;  kbShiftF5   = $5800;  kbShiftF6   = $5900;
  kbShiftF7   = $5A00;  kbShiftF8   = $5B00;  kbShiftF9   = $5C00;
  kbShiftF10  = $5D00;  kbCtrlF1    = $5E00;  kbCtrlF2    = $5F00;
  kbCtrlF3    = $6000;  kbCtrlF4    = $6100;  kbCtrlF5    = $6200;
  kbCtrlF6    = $6300;  kbCtrlF7    = $6400;  kbCtrlF8    = $6500;
  kbCtrlF9    = $6600;  kbCtrlF10   = $6700;  kbAltF1     = $6800;
  kbAltF2     = $6900;  kbAltF3     = $6A00;  kbAltF4     = $6B00;
  kbAltF5     = $6C00;  kbAltF6     = $6D00;  kbAltF7     = $6E00;
  kbAltF8     = $6F00;  kbAltF9     = $7000;  kbAltF10    = $7100;
  kbCtrlPrtSc = $7200;  kbCtrlLeft  = $7300;  kbCtrlRight = $7400;
  kbCtrlEnd   = $7500;  kbCtrlPgDn  = $7600;  kbCtrlHome  = $7700;
  kbAlt1      = $7800;  kbAlt2      = $7900;  kbAlt3      = $7A00;
  kbAlt4      = $7B00;  kbAlt5      = $7C00;  kbAlt6      = $7D00;
  kbAlt7      = $7E00;  kbAlt8      = $7F00;  kbAlt9      = $8000;
  kbAlt0      = $8100;  kbAltMinus  = $8200;  kbAltEqual  = $8300;
  kbCtrlPgUp  = $8400;  kbAltBack   = $0800;  kbNoKey     = $0000;

{ Keyboard state and shift masks }

  kbRightShift  = $0001;
  kbLeftShift   = $0002;
  kbCtrlShift   = $0004;
  kbAltShift    = $0008;
  kbScrollState = $0010;
  kbNumState    = $0020;
  kbCapsState   = $0040;
  kbInsState    = $0080;

  kbSpace       = $3920;

{ Mouse button state masks }

  mbLeftButton  = $01;
  mbRightButton = $02;

type

{ Event record }

  PEvent = ^TEvent;
  TEvent = record
    What: Word;
    case Word of
      evNothing: ();
      evMouse: (
        Buttons: Byte;
        Double: Boolean;
        Where: TPoint);
      evKeyDown: (
        case Integer of
          0: (KeyCode: Word);
          1: (CharCode: Char;
              ScanCode: Byte));
      evMessage: (
        Command: Word;
        case Word of
          0: (InfoPtr: Pointer);
          1: (InfoLong: Longint);
          2: (InfoWord: Word);
          3: (InfoInt: Integer);
          4: (InfoByte: Byte);
          5: (InfoChar: Char));
  end;

const

{ Initialized variables }

  ButtonCount: Byte = 0;
  MouseEvents: Boolean = False;
  MouseReverse: Boolean = False;
  DoubleDelay: Word = 8;
  RepeatDelay: Word = 8;

var

{ Uninitialized variables }

  MouseIntFlag: Byte;
  MouseButtons: Byte;
  MouseWhere: TPoint;


const  {TONY}
  MouseCursorSize:    TPoint = (X: 32; Y: 32);
  MouseTrapRect  : TRect = (A:(X:0; Y:0); B:(X:0; Y:0)); {DK}
  MouseTrapOccurs: Boolean = False; {DK}

{TONY}  { DONT CALL THESE PROCS !!!}
procedure DefaultMouseCursor;
procedure ClockMouseCursor;
procedure Diag1MouseCursor;
procedure Diag2MouseCursor;
procedure HorizontalMouseCursor;
procedure VerticalMouseCursor;
procedure TargetMouseCursor;
procedure CrossMouseCursor;
procedure BoxMouseCursor;
procedure InputMouseCursor;

type   {TONY}
  PMouseCursor = ^TMouseCursor;
  TMouseCursor = record
    XHotSpot, YHotSpot: integer;
    Image: pointer;
  end;

const {TONY}
  CurrentMouseCursor: PMouseCursor = @DefaultMouseCursor;

{ Event manager routines }

procedure InitEvents;
procedure DoneEvents;
procedure ShowMouse;
procedure HideMouse;
procedure UpdateMouse; {TONY}
procedure HoldMouse(Hold: Boolean); {TONY}
procedure SetMouseCursorShape(Cur: PMouseCursor); {TONY}
procedure LockMouseShape; {TONY}
procedure UnLockMouseShape; {TONY}
procedure GetMouseEvent(var Event: TEvent);
procedure GetKeyEvent(var Event: TEvent);
function GetShiftState: Byte;
function GetTicks: Word; {TONY}
function GetKey: Char; inline($B4/$00/$CD/$16); {OOA}
function CheckKey: Char;                        {OOA}
procedure StuffKey(W : Word);                   {DK}
procedure ClearKBDBuffer;                       {DK}

{ ******** SCREEN MANAGER ******** }

const   {TONY}
  MouseLockCounter: word = 0;

var
{ Uninitialized variables }

  ScreenWidth: Word;
  ScreenHeight: Word;
  AllScreen: TRect;

{ Screen manager routines }

procedure InitVideo;
procedure DoneVideo;
procedure ClearScreen;
procedure DefaultSystemFont;   {TONY}
{$IFDEF IncludeFonts}
procedure LoadAltFont(CharHeight: Byte); {OOA}
procedure LoadAppropriateFont;           {OOA}
{$ENDIF}

{ ******** SYSTEM ERROR HANDLER ******** }

type

{ System error handler function type }

  TSysErrorFunc = function(ErrorCode: Integer; Drive: Byte): Integer;

{ Default system error handler routine }

function SystemError(ErrorCode: Integer; Drive: Byte): Integer;

{ System notify handler function type }
type
  TSysNotifyProc = procedure(EventType, EventCommand : Word; EventInfo : Pointer);

{ Default system notify handler routine }

procedure SystemNotify(EventType, EventCommand : Word; EventInfo : Pointer);
procedure DriversSystemNotify(EventType, EventCommand : Word; EventInfo : Pointer);

const

{ Initialized variables }

  SaveInt09: Pointer = nil;
  SysErrorFunc: TSysErrorFunc = SystemError;
  SysColorAttr: Word = $4E4F;
  SysMonoAttr: Word = $7070;
  CtrlBreakHit: Boolean = False;
  SaveCtrlBreak: Boolean = False;
  SysErrActive: Boolean = False;
  FailSysErrors: Boolean = False;
  SysNotifyProc: TSysNotifyProc = DriversSystemNotify;

{ System error handler routines }

procedure InitSysError;
procedure DoneSysError;

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

{$IFDEF Russian}
const
  AltKbdMode: Boolean = False;  {read/write}
  AltKbdSwitch: Word = kbNoKey; {read/write}

procedure InitKbdDriver;          {OOA}
procedure DoneKbdDriver;          {OOA}
function UpCase(Ch: Char): Char;  {OOA}
{$ENDIF}

function GetAltChar(KeyCode: Word): Char;
function GetAltCode(Ch: Char): Word;
function GetCtrlChar(KeyCode: Word): Char;
function GetCtrlCode(Ch: Char): Word;
function CtrlToArrow(KeyCode: Word): Word;

{ String routines }

procedure FormatStr(var Result: String; const Format: String; var Params);
procedure PrintStr(const S: String);
function InputStr(Hide: Boolean): String; {OOA}
procedure ReStr(var P: PString; const S: String); {TONY}
  {--- –°–Ω–∞—á–∞–ª–∞ DisposeStr(P) –∑–∞—Ç–µ–º P:= NewStr(S) }

{ Buffer move routines for text views compatibitity }

procedure MoveBuf(var Dest; var Source; Attr: Byte; Count: Word);
procedure MoveChar(var Dest; C: Char; Attr: Byte; Count: Word);
procedure MoveCStr(var Dest; const Str: String; Attrs: Word);
procedure MoveStr(var Dest; const Str: String; Attr: Byte);

function CStrLen(const S: String): Integer;
function CStr(S : String) : String;

(*** SOUNDS ***)
const
  DefaultBeep  = 0;

  {DK notifications}
  snAlertSound = 0; {sound request}
  snRedraw     = 1; {non-object call for redraw}
  snUser       = 2; {user notify}

  sbNone       = 0;
  sbSysError   = 1;
  sbError      = 2;
  sbWarning    = 3;
  sbInfo       = 4;
  sbConfirm    = 5;
  sbEntry      = 6;
  sbExit       = 7;
  sbDefault    = 8;

  unError        = 0;
  unWarning      = 1;
  unInformation  = 2;
  unConfirmation = 3;
  unPercent      = 4;
  unPercentAux   = 5;
  unMessage      = 6;

{$IFDEF EXTENDEDSOUND}
  WaveFiles : array[sbSysError..sbDefault] of String[80] = ('', '', '', '', '', '', '', '');
{$ENDIF}

var
  OneMS : Word;

procedure SystemBeep(Alert: word);
procedure Sound(Freq: word);  {GIO}
procedure NoSound;
procedure Delay(Tm: word);


implementation


(*** SOUNDS ***)
var
  BIOSDataSele: word;

{$L SNDDELAY.OBJ}
procedure DelayCalibrate; far; external;
procedure Sound(Freq: word); external;
procedure NoSound; external;
procedure Delay(Tm: word); external;

procedure SystemBeep(Alert: word);
begin
  case Alert of
    DefaultBeep : begin
      Sound(800); delay(100); NoSound;
    end;
  else SysNotifyProc(snAlertSound, Alert, nil); end;
end;

{$L STDCUR.OBJ}    {TONY}
procedure DefaultMouseCursor; external;
procedure ClockMouseCursor; external;
procedure Diag1MouseCursor; external;
procedure Diag2MouseCursor; external;
procedure HorizontalMouseCursor; external;
procedure VerticalMouseCursor; external;
procedure TargetMouseCursor; external;
procedure CrossMouseCursor; external;
procedure BoxMouseCursor;   external;
procedure InputMouseCursor; external;

{$L MFONT.OBJ}
procedure DefaultSystemFont; external;   {TONY}


{ ******** EVENT MANAGER ******** }

const

{ Event manager constants }

  EventQSize = 16;

var

{ Event manager variables }

  LastButtons: Byte;
  DownButtons: Byte;
  LastDouble: Boolean;
  LastWhere: TPoint;
  DownWhere: TPoint;
  DownTicks: Word;
  AutoTicks: Word;
  AutoDelay: Word;
  EventCount: Word;
  EventQHead: Word;
  EventQTail: Word;
  EventQueue: array[0..EventQSize - 1] of TEvent;
  EventQLast: record end;

var
  ShiftState: Byte absolute $40:$17;
  Ticks: Word absolute $40:$6C;

{ Detect mouse driver }

procedure DetectMouse; near; assembler;
asm
        MOV     AX,3533H
        INT     21H
        MOV     AX,ES
        OR      AX,BX
        JE      @@1
        XOR     AX,AX
        INT     33H
        OR      AX,AX
        JE      @@1
  { TONY
        PUSH    BX
        MOV     AX,4
        XOR     CX,CX
        XOR     DX,DX
        INT     33H
        POP     AX
  }
        MOV AX, BX {TONY}
@@1:    MOV ButtonCount,AL
end;

{ Store event in GetMouseEvent and GetKeyEvent }

procedure StoreEvent; near; assembler;
asm
        MOV     DI,SP
        LES     DI,SS:[DI+8]
        CLD
        STOSW
        XCHG    AX,BX
        STOSW
        XCHG    AX,CX
        STOSW
        XCHG    AX,DX
        STOSW
end;

{ Get mouse state }
{ Out   BL = Button mask }
{       CX = X coordinate }
{       DX = Y coordinate }
{       DI = Timer ticks }

procedure GetMouseState; near; assembler;
asm
        CLI
        CMP     EventCount,0
        JNE     @@1
        MOV     BL,MouseButtons
        MOV     CX,MouseWhere.Word[0]
        MOV     DX,MouseWhere.Word[2]
        MOV     ES,Seg0040
        MOV     DI,ES:Ticks
        JMP     @@3
@@1:    MOV     SI,EventQHead
        CLD
        LODSW
        XCHG    AX,DI
        LODSW
        XCHG    AX,BX
        LODSW
        XCHG    AX,CX
        LODSW
        XCHG    AX,DX
        CMP     SI,OFFSET EventQLast
        JNE     @@2
        MOV     SI,OFFSET EventQueue
@@2:    MOV     EventQHead,SI
        DEC     EventCount
@@3:    STI
        CMP     MouseReverse,0
        JE      @@4
        MOV     BH,BL
        AND     BH,3
        JE      @@4
        CMP     BH,3
        JE      @@4
        XOR     BL,3
@@4:
end;

var
  saveSS, saveSP : Word;                   {DK}
  MouseStack     : array[1..1024] of Byte; {DK}
  {–±–µ–∑ —ç—Ç–æ–≥–æ –º–∞–ª–æ–ø–æ–Ω—è—Ç–Ω—ã–µ –ª–∞–∂–∏ –ø–æ–¥ OS/2 —Å DPMI - —Ç–∞–∫–æ–µ –≤–ø–µ—á–∞—Ç–ª–µ–Ω–∏–µ
  —á—Ç–æ VMOUSE –¥–ª—è DOS-Box'a –¥–∞–µ—Ç —Å–æ–≤—Å–µ–º –º–∞–ª–æ —Å—Ç–µ–∫–∞ –Ω–∞ —Ö—É–∫ int 33}

procedure MouseInt; far; assembler;
asm
        MOV     SI,SEG @DATA
        MOV     DS,SI
        MOV     SI,CX
  { TONY
        MOV     CL,3
        SHR     SI,CL
        SHR     DX,CL
  }
        MOV     MouseButtons,BL
        MOV     MouseWhere.X,SI
        MOV     MouseWhere.Y,DX
        TEST    AX,11110B
        JE      @@2
        CMP     EventCount,EventQSize
        JE      @@2
        MOV     ES,Seg0040
        MOV     AX,ES:Ticks
        MOV     DI,EventQTail
        PUSH    DS
        POP     ES
        CLD
        STOSW
        XCHG    AX,BX
        STOSW
        XCHG    AX,SI
        STOSW
        XCHG    AX,DX
        STOSW
        CMP     DI,OFFSET EventQLast
        JNE     @@1
        MOV     DI,OFFSET EventQueue
@@1:    MOV     EventQTail,DI
        INC     EventCount
@@2:    MOV     MouseIntFlag,1

  mov  saveSS, ss
  mov  saveSp, sp
  mov  ax, ds
  mov  ss, ax
  mov  sp, offset MouseStack
  add  sp, 1022

  call UpdateMouse  {TONY}

  mov ss, saveSS
  mov sp, saveSP
end;

var  {TONY}
  MouseWhereLast: TPoint;
  MouseCounter: integer;   { >=0 - –≤–∏–¥–∏–º–æ, <0 - –Ω–µ–≤–∏–¥–∏–º–æ }

procedure InitEvents; assembler;
asm
  mov MouseCounter, -32760  {TONY}
        XOR     AX,AX
        CMP     AL,ButtonCount
        JE      @@1
        MOV     DownButtons,AL
        MOV     LastDouble,AL
        MOV     EventCount,AX
        MOV     AX,OFFSET DS:EventQueue
        MOV     EventQHead,AX
        MOV     EventQTail,AX
{TONY - setting Mouse Range manually !}
  les    di, ScreenDriver
  mov    ax, 7
  xor    cx, cx
  mov    dx, es:[di+TScreenDriver.MaximalX]
  push   dx
  int    33h
  mov    ax, 8
  mov    dx, es:[di+TScreenDriver.MaximalY]
  push   dx
  int    33h
{TONY - setting Mouse Position to the Center of the Screen}
  pop    dx
  shr    dx, 1
  pop    cx
  shr    cx, 1
  mov    ax, 4
  int    33h
{--- Query position & buttons ---}
        MOV     AX,3
        INT     33H
        XCHG    AX,CX
  { TONY
        MOV     CL,3
        SHR     AX,CL
        SHR     DX,CL
  }
        MOV     MouseButtons,BL
        MOV     MouseWhere.X,AX
        MOV     MouseWhere.Y,DX
        MOV     MouseWhereLast.X,AX   {TONY}
        MOV     MouseWhereLast.Y,DX   {TONY}
  MOV MouseCounter, -1      {TONY}
        MOV     LastButtons,BL
        MOV     LastWhere.X,AX
        MOV     LastWhere.Y,DX
        MOV     AX,12
        MOV     CX,0FFFFH
        MOV     DX,OFFSET CS:MouseInt
        PUSH    CS
        POP     ES
        INT     33H
  call    ShowMouse  {TONY}
  { TONY
  MOV     AX,1
  INT     33H
  }
        MOV     MouseEvents,1
@@1:
end;

procedure DoneEvents; assembler;
asm
        CMP     ButtonCount,0
        JE      @@1
        CMP     MouseEvents,0
        JE      @@1
        MOV     MouseEvents,0
  {TONY
  MOV     AX,2
  INT     33H
  }
  call    HideMouse  {TONY}
        MOV     AX,12
        XOR     CX,CX
        MOV     DX,CX
        MOV     ES,CX
        INT     33H
@@1:
end;

var     {TONY}
  UnderMouseBuffer : array [1..512+6] of byte;

function Min(A, B: integer): integer;
inline (
    $5B/      {pop  bx (A)}
    $58/      {pop  ax (B)}
    $39/$D8/  {cmp  ax, bx}
    $7C/$02/  {jl   @@1}
    $89/$D8   {mov  ax, bx}
{@@1:}
);

function Max(A, B: integer): integer;
inline (
    $5B/      {pop  bx (A)}
    $58/      {pop  ax (B)}
    $39/$D8/  {cmp  ax, bx}
    $7F/$02/  {jg   @@1}
    $89/$D8   {mov  ax, bx}
{@@1:}
);

procedure HoldMouse(Hold: Boolean); assembler;
asm
    cmp    Hold, True
    jne    @@UnHold
    cli
    inc    MouseLockCounter
    sti
    jmp    @@Exit
@@UnHold:
    cli
    cmp    MouseLockCounter, 0
    je     @@1
    dec    MouseLockCounter
@@1:
    sti
@@Exit:
end;

procedure UpdateMouseShow;   {TONY}
var
  xxx, yyy, xFrom, yFrom, xLen, yLen: integer;
begin
  with CurrentMouseCursor^ do with MouseWhereLast do begin
    if X<16 then begin
       X := X;
    end;
    xxx := Max(X-XHotSpot, 0);
    yyy := Max(Y-YHotSpot, 0);
    xFrom := Max(0, XHotSpot-X);
    yFrom := Max(0, YHotSpot-Y);
    xLen := Max(0, Min(32, ScreenWidth-xxx));
    yLen := Max(0, Min(32, ScreenHeight-yyy));
  end;
  { –ó–∞–ø–æ–º–Ω–∏—Ç—å –ø–æ–¥–ª–æ–∂–∫—É }
  QuickSave(xxx, yyy, xLen, yLen);
  { –ù–∞—Ä–∏—Å–æ–≤–∞—Ç—å –º—ã—à–∫—É }
  DirectMousePut(CurrentMouseCursor^.Image^, xxx, yyy, xFrom, yFrom, xLen, yLen);
end;

procedure ShowMouse;  {TONY}
var
  xxx, yyy, xLen, yLen: integer;
begin
  HoldMouse(True);
  if (MouseCounter >=0) then begin
    { –í–æ—Å—Å—Ç–∞–Ω–æ–≤–∏—Ç—å –ø–æ–¥–ª–æ–∂–∫—É }
    with CurrentMouseCursor^ do with MouseWhereLast do begin
      xxx := Max(X-XHotSpot, 0);
      yyy := Max(Y-YHotSpot, 0);
      xLen := Max(0, Min(32, ScreenWidth-xxx));
      yLen := Max(0, Min(32, ScreenHeight-yyy));
      {QuickRestore(xxx, yyy, xLen, yLen);}
    end;
  end;
  Inc(MouseCounter);
  MouseWhereLast := MouseWhere;
  if MouseCounter >=0 then begin
    UpdateMouseShow;
  end;
  HoldMouse(False);
end;

procedure HideMouse;  {TONY}
var
  xxx, yyy, xLen, yLen: integer;
begin
  HoldMouse(True);
  if MouseCounter >=0 then begin
    { –í–æ—Å—Å—Ç–∞–Ω–æ–≤–∏—Ç—å –ø–æ–¥–ª–æ–∂–∫—É }
    with CurrentMouseCursor^ do with MouseWhereLast do begin
      xxx := Max(X-XHotSpot, 0);
      yyy := Max(Y-YHotSpot, 0);
      xLen := Max(0, Min(32, ScreenWidth-xxx));
      yLen := Max(0, Min(32, ScreenHeight-yyy));
      QuickRestore(xxx, yyy, xLen, yLen);
    end;
  end;
  Dec(MouseCounter);
  MouseWhereLast := MouseWhere;
  if MouseCounter >=0 then begin
    UpdateMouseShow;
  end;
  HoldMouse(False);
end;

const
  MouseLocked: Boolean = False;
  MouseCursorToBeSet: PMouseCursor = @DefaultMouseCursor;

procedure LockMouseShape; {TONY}
begin
  MouseLocked := True;
end;

procedure UnLockMouseShape; {TONY}
begin
  MouseLocked := False;
  SetMouseCursorShape(MouseCursorToBeSet);
end;

procedure SetMouseCursorShape(Cur: PMouseCursor); {TONY}
var
  xxx, yyy, xLen, yLen: integer;
begin
  if MouseLocked then begin
    MouseCursorToBeSet := Cur;
    Exit;
  end;
  if Cur = CurrentMouseCursor then Exit;
  HoldMouse(True);
  if MouseCounter >=0 then begin
    { –í–æ—Å—Å—Ç–∞–Ω–æ–≤–∏—Ç—å –ø–æ–¥–ª–æ–∂–∫—É }
    with CurrentMouseCursor^ do with MouseWhereLast do begin
      xxx := Max(X-XHotSpot, 0);
      yyy := Max(Y-YHotSpot, 0);
      xLen := Max(0, Min(32, ScreenWidth-xxx));
      yLen := Max(0, Min(32, ScreenHeight-yyy));
      QuickRestore(xxx, yyy, xLen, yLen);
    end;
  end;
  CurrentMouseCursor := Cur;
  MouseWhereLast := MouseWhere;
  if MouseCounter >=0 then begin
    UpdateMouseShow;
  end;
  HoldMouse(False);
end;

procedure UpdateMouse; {TONY}
var
  xxx, yyy, xLen, yLen: integer;
  R : TRect;
begin
  if MouseLockCounter <> 0 then Exit;
  if (MouseWhereLast.X = MouseWhere.X) and (MouseWhereLast.Y = MouseWhere.Y) then Exit;
  HoldMouse(True);


  if MouseCounter >=0 then begin
    { –í–æ—Å—Å—Ç–∞–Ω–æ–≤–∏—Ç—å –ø–æ–¥–ª–æ–∂–∫—É }
    with CurrentMouseCursor^ do with MouseWhereLast do begin
      xxx := Max(X-XHotSpot, 0);
      yyy := Max(Y-YHotSpot, 0);
      xLen := Max(0, Min(32, ScreenWidth-xxx));
      yLen := Max(0, Min(32, ScreenHeight-yyy));
      QuickRestore(xxx, yyy, xLen, yLen);
    end;
  end;
  MouseWhereLast := MouseWhere;

  if not(MouseTrapOccurs) and not(MouseTrapRect.Empty) then begin {DK}
    R.A.X := MouseWhere.X - CurrentMouseCursor^.XHotSpot;
    R.A.X := MouseWhere.Y - CurrentMouseCursor^.YHotSpot;
    R.B.X := R.A.X + 32;
    R.B.Y := R.A.Y + 32;
    R.Intersect(MouseTrapRect);
    if not R.Empty then begin
      MouseTrapOccurs := True;
      MouseTrapRect.Assign(0, 0, 0, 0);
      Dec(MouseCounter);
      HoldMouse(False);
      Exit;
    end;
  end;

  if MouseCounter >=0 then begin
    UpdateMouseShow;
  end;
  HoldMouse(False);
end;

{TONY
procedure ShowMouse; assembler;
asm
        CMP     ButtonCount,0
        JE      @@1
        PUSH    AX
        MOV     AX,1
        INT     33H
        POP     AX
@@1:
end;

procedure HideMouse; assembler;
asm
        CMP     ButtonCount,0
        JE      @@1
        PUSH    AX
        MOV     AX,2
        INT     33H
        POP     AX
@@1:
end;
}

procedure GetMouseEvent(var Event: TEvent); assembler;
asm
        CMP     MouseEvents,0
        JE      @@2
        CALL    GetMouseState
        MOV     BH,LastDouble
        MOV     AL,LastButtons
        CMP     AL,BL
        JE      @@1
        OR      AL,AL
        JE      @@3
        OR      BL,BL
        JE      @@5
        MOV     BL,AL
@@1:    CMP     CX,LastWhere.X
        JNE     @@6
        CMP     DX,LastWhere.Y
        JNE     @@6
        OR      BL,BL
        JE      @@2
        MOV     AX,DI
        SUB     AX,AutoTicks
        CMP     AX,AutoDelay
        JAE     @@7
@@2:    XOR     AX,AX
        MOV     BX,AX
        MOV     CX,AX
        MOV     DX,AX
        JMP     @@9
@@3:    MOV     BH,0
        CMP     BL,DownButtons
        JNE     @@4
        CMP     CX,DownWhere.X
        JNE     @@4
        CMP     DX,DownWhere.Y
        JNE     @@4
        MOV     AX,DI
        SUB     AX,DownTicks
        CMP     AX,DoubleDelay
        JAE     @@4
        MOV     BH,1
@@4:    MOV     DownButtons,BL
        MOV     DownWhere.X,CX
        MOV     DownWhere.Y,DX
        MOV     DownTicks,DI
        MOV     AutoTicks,DI
        MOV     AX,RepeatDelay
        MOV     AutoDelay,AX
        MOV     AX,evMouseDown
        JMP     @@8
@@5:    MOV     AX,evMouseUp
        JMP     @@8
@@6:    MOV     AX,evMouseMove
        JMP     @@8
@@7:    MOV     AutoTicks,DI
        MOV     AutoDelay,1
        MOV     AX,evMouseAuto
@@8:    MOV     LastButtons,BL
        MOV     LastDouble,BH
        MOV     LastWhere.X,CX
        MOV     LastWhere.Y,DX
@@9:    CALL    StoreEvent
end;

procedure GetKeyEvent(var Event: TEvent); assembler;
asm
@@0:
        MOV     AH,1
        INT     16H
        MOV     AX,0
        MOV     BX,AX
        JE      @@2
        MOV     AH,0
        INT     16H
        XCHG    AX,BX
  MOV AH,1      {OOA}
  INT 16H       {OOA}
  JZ  @@1       {OOA}
  CMP AX,BX     {OOA}
  JE  @@0       {OOA}
@@1:
        MOV     AX,evKeyDown
@@2:    XOR     CX,CX
        MOV     DX,CX
        CALL    StoreEvent
end;

function GetShiftState: Byte; assembler;
asm
        MOV     ES,Seg0040
        MOV     AL,ES:ShiftState
end;

function GetTicks: Word; assembler; {TONY}
asm
  mov    es, Seg0040
  mov    ax, es:Ticks
end;

function CheckKey: Char; assembler;
asm
  MOV   AH,1
  INT   16H
  MOV   AX,0
  JE    @@1
  INT   16H
@@1:
end;

procedure StuffKey(W : Word);
const
  KbdStart = $1E;
  KbdEnd = $3C;
var
  KbdHead : ^Word {absolute $40 : $1A};
  KbdTail : ^Word {absolute $40 : $1C};
  SaveKbdTail : Word;
begin
  KbdHead := Ptr(Seg0040, $1A);
  KbdTail := Ptr(Seg0040, $1C);
  SaveKbdTail := KbdTail^;
  if KbdTail^ = KbdEnd then KbdTail^ := KbdStart else Inc(KbdTail^, 2);
  if KbdTail^ = KbdHead^ then KbdTail^ := SaveKbdTail else MemW[Seg0040:SaveKbdTail] := W;
end;


procedure ClearKBDBuffer;
Begin
  MemW[Seg0040:$1A] := MemW[Seg0040:$1C];
End;

{ ******** SCREEN MANAGER ******** }

{ Save registers and call video interrupt }

procedure VideoInt; near; assembler;
asm
        PUSH    BP
        PUSH    ES
        INT     10H
        POP     ES
        POP     BP
end;

procedure InitVideo {TONY};
begin
  if not InitGDI then begin
{$IFDEF Russian}
    PrintStr('éË®°™† ®≠®Ê®†´®ß†Ê®® £‡†‰®™®');
{$ELSE}
    PrintStr('Error initializing graphics');
{$ENDIF}
    Halt;
  end;
  ScreenWidth := ScreenDriver^.ScreenWidth;
  ScreenHeight := ScreenDriver^.ScreenHeight;
  AllScreen.Assign(0, 0, ScreenWidth, ScreenHeight);
(*
  if ButtonCount > 0 then
  asm
          MOV   AX,4
          MOV   CX, ScreenWidth
    SHR CX, 1
          MOV   DX, ScreenHeight
    SHR DX, 1
          INT   33H
  end;
*)
  InitSound;
end;

procedure DoneVideo {TONY};
begin
  DoneSound;
  DoneGDI;
end;

procedure ClearScreen; assembler {TONY};
asm
end;

{$IFDEF IncludeFonts} {OOA}

{$L alt8x16.obj}
{$L alt8x14.obj}
{$L alt8x8.obj}
procedure AltFont8x16; external;
procedure AltFont8x14; external;
procedure AltFont8x8; external;

procedure LoadCharacterGenerator(Height: Byte; Start: Char; Count: Byte; BOffs: Pointer); assembler;
{TONY, OOA}
const
  CRTValues : array[1..10] of Word =
  ($0402, $0704, $0005, $0406, $0204, $0302, $0304, $1005, $0E06, $0004);
asm
        LEA     SI, CRTValues
        { –†–µ–≥–∏—Å—Ç—Ä –º–∞—Å–∫–∏ (2); –†–∞–∑—Ä–µ—à–µ–Ω –¥–æ—Å—Ç—É–ø –∫ –ø–ª–æ—Å–∫–æ—Å—Ç–∏ 2 (4)        }
        MOV     DX, 03C4h
        LODSW
        OUT     DX, AX
        { –†–µ–≥–∏—Å—Ç—Ä —Ä–µ–∂–∏–º–∞ –ø–∞–º—è—Ç–∏ (4);                                  }
        { –í—ã–±—Ä–∞–Ω —Ç–µ–∫—Å—Ç–æ–≤—ã–π —Ä–µ–∂–∏–º (1+), –∏—Å–ø–æ–ª—å–∑. —Ä–∞—Å—à. –ø–∞–º—è—Ç—å (2+),    }
        { –ø–æ—Å–ª–µ–¥–æ–≤–∞—Ç–µ–ª—å–Ω—ã–π –¥–æ—Å—Ç—É–ø –∫ –¥–∞–Ω–Ω—ã–º –≤ –±–∏—Ç–æ–≤–æ–π –ø–ª–æ—Å–∫–æ—Å—Ç–∏ (4+=7) }
        LODSW
        OUT     DX, AX
        { –†–µ–≥–∏—Å—Ç—Ä —Ä–µ–∂–∏–º–∞ (5); 0                                       }
        MOV     DX, 03CEh
        LODSW
        OUT     DX, AX
        { –°–º–µ—à–∞–Ω—ã–π —Ä–µ–≥–∏—Å—Ç—Ä (6); –†–∞–∑—Ä–µ—à–µ–Ω–∞ –∞–¥—Ä–µ—Å–∞—Ü–∏—è –∫ –∑–Ω–∞–∫–æ–≥–µ–Ω,       }
        { –∞–¥—Ä–µ—Å A000 –¥–ª—è 64 K (4)                                     }
        LODSW
        OUT     DX, AX
        { –†–µ–≥–∏—Å—Ç—Ä –≤—ã–±–æ—Ä–∞ –∫–∞—Ä—Ç—ã —á—Ç–µ–Ω–∏—è (4); –ü–ª–æ—Å–∫–æ—Å—Ç—å 2 (?)            }
        LODSW
        OUT     DX, AX
        { –°–æ–±—Å—Ç–≤–µ–Ω–Ω–æ –∑–∞–≥—Ä—É–∑–∫–∞ –≤—Ç–æ—Ä–æ–π –ø–ª–æ—Å–∫–æ—Å—Ç–∏ }
        PUSH    SI
        PUSH    DS
        MOV     AL, [Count]
        MOV AH, [Height]
        XOR     BH, BH
        MOV     BL, [Start]
        SHL     BX, 5
        MOV ES, [SegA000]
        LDS     SI, [BOffs]
        OR      AL, AL
@@1:
        JZ      @@2
        MOV     DI, BX
        XOR     CH, CH
        MOV     CL, AH
        REP MOVSB
        ADD     BX, 32
        DEC     AL
        JMP     @@1
@@2:
        POP     DS
        POP     SI

        { –†–∞–∑—Ä–µ—à–µ–Ω –¥–æ—Å—Ç—É–ø –∫ –ø–ª–æ—Å–∫–æ—Å—Ç—è–º 0, 1                           }
        MOV     DX, 03C4h
        LODSW
        OUT     DX, AX
        { –ß–µ—Ç–Ω—ã–µ / –Ω–µ—á–µ—Ç–Ω—ã–µ, –∫–∞–∫ –≤ CGA                                }
        LODSW
        OUT     DX, AX
        MOV     DX, 03CEh
        LODSW
        OUT     DX, AX
        LODSW
        OUT     DX, AX
        LODSW
        OUT     DX, AX
end;

procedure LoadAltFont(CharHeight: Byte); {OOA}
var
  FontPtr: Pointer;
  FontCode : byte;
begin
  case CharHeight of
    8  : begin
           FontPtr:= @AltFont8x8;
           FontCode := $02;
         end;
    14 : begin
           FontPtr:= @AltFont8x14;
           FontCode := $01;
         end;
    16 : begin
           FontPtr:= @AltFont8x16;
           FontCode := $04;
         end
  else
    Exit
  end;
  asm
    XOR   BL, BL
    MOV   AL, [FontCode]
    MOV   AH, 11H
    INT   10H
  end;
  LoadCharacterGenerator(CharHeight, 'Ä', 128, FontPtr);
end;

procedure LoadAppropriateFont; {OOA}
begin
  LoadAltFont(Byte(Ptr(Seg0040, $85)^));
end;

{$ENDIF} {IncludeFonts}

{ ******** SYSTEM ERROR HANDLER ******** }

{$IFDEF DPMI}
{$L SYSINT.OBP}
{$ELSE}
{$L SYSINT.OBJ}
{$ENDIF}

const

{ System error messages }

{$IFDEF Ukrainian}
  SCriticalErrorHeader: string[20] = 'ä‡®‚®Á•·™†Ô ÆË®°™† !';   {TONY}
  SCriticalError:  string[29] = 'ä‡®‚®Á•·™®© ·°Æ© §®·™Æ¢Æ§† %c';
  SWriteProtected: string[25] = 'Ñ®·™ %c ß†È®È•≠ Æ‚ ß†Ø®·®';
  SDiskNotReady:   string[20] = 'Ñ®·™Æ¢Æ§ %c ≠• £Æ‚Æ¢';
  SDataIntegrity:  string[27] = 'éË®°™† ¢ §†≠≠ÎÂ ≠† §®·™• %c';
  SSeekError:      string[32] = 'éË®°™† ØÆß®Ê®Æ≠®‡Æ¢†≠®Ô §®·™† %c';
  SUnknownMedia:   string[35] = 'ç•®ß¢•·‚≠Î© ≠Æ·®‚•´Ï ¢ §®·™Æ¢Æ§• %c';
  SSectorNotFound: string[28] = 'ç• ≠†©§•≠ ·•™‚Æ‡ ≠† §®·™• %c';
  SOutOfPaper:     string[21] = 'ç•‚ °„¨†£® ¢ Ø‡®≠‚•‡•';
  SWriteFault:     string[24] = 'éË®°™† ß†Ø®·® ≠† §®·™ %c';
  SReadFault:      string[22] = 'éË®°™† Á‚•≠®Ô §®·™† %c';
  SGeneralFailure: string[28] = 'ÄØØ†‡†‚≠Î© ·°Æ© §®·™Æ¢Æ§† %c';
  SBadImageOfFAT:  string[31] = 'ç•Ø‡†¢®´Ï≠†Ô ™ÆØ®Ô FAT ¢ Ø†¨Ô‚®';
  SDeviceError:    string[27] = 'éË®°™† §Æ·‚„Ø† ™ „·‚‡Æ©·‚¢„';
  SInsertDisk:     string[27] = 'Ç·‚†¢Ï‚• §®·™ ¢ §®·™Æ¢Æ§ %c';
  SRetryOrCancel:  string[30] = '<Enter> èÆ¢‚Æ‡®‚Ï  <Esc> é‚™†ß';
{$ELSE}
  {$IFDEF Russian}
  SCriticalErrorHeader: string[20] = 'ä‡®‚®Á•·™†Ô ÆË®°™† !';   {TONY}
  SCriticalError:  string[29] = 'ä‡®‚®Á•·™®© ·°Æ© §®·™Æ¢Æ§† %c';
  SWriteProtected: string[25] = 'Ñ®·™ %c ß†È®È•≠ Æ‚ ß†Ø®·®';
  SDiskNotReady:   string[20] = 'Ñ®·™Æ¢Æ§ %c ≠• £Æ‚Æ¢';
  SDataIntegrity:  string[27] = 'éË®°™† ¢ §†≠≠ÎÂ ≠† §®·™• %c';
  SSeekError:      string[32] = 'éË®°™† ØÆß®Ê®Æ≠®‡Æ¢†≠®Ô §®·™† %c';
  SUnknownMedia:   string[35] = 'ç•®ß¢•·‚≠Î© ≠Æ·®‚•´Ï ¢ §®·™Æ¢Æ§• %c';
  SSectorNotFound: string[28] = 'ç• ≠†©§•≠ ·•™‚Æ‡ ≠† §®·™• %c';
  SOutOfPaper:     string[21] = 'ç•‚ °„¨†£® ¢ Ø‡®≠‚•‡•';
  SWriteFault:     string[24] = 'éË®°™† ß†Ø®·® ≠† §®·™ %c';
  SReadFault:      string[22] = 'éË®°™† Á‚•≠®Ô §®·™† %c';
  SGeneralFailure: string[28] = 'ÄØØ†‡†‚≠Î© ·°Æ© §®·™Æ¢Æ§† %c';
  SBadImageOfFAT:  string[31] = 'ç•Ø‡†¢®´Ï≠†Ô ™ÆØ®Ô FAT ¢ Ø†¨Ô‚®';
  SDeviceError:    string[27] = 'éË®°™† §Æ·‚„Ø† ™ „·‚‡Æ©·‚¢„';
  SInsertDisk:     string[27] = 'Ç·‚†¢Ï‚• §®·™ ¢ §®·™Æ¢Æ§ %c';
  SRetryOrCancel:  string[30] = '<Enter> èÆ¢‚Æ‡®‚Ï  <Esc> é‚™†ß';
  {$ELSE}
  SCriticalErrorHeader: string[16] = 'Critical error !';   {TONY}
  SCriticalError:  string[31] = 'Critical disk error on drive %c';
  SWriteProtected: string[35] = 'Disk is write-protected in drive %c';
  SDiskNotReady:   string[29] = 'Disk is not ready in drive %c';
  SDataIntegrity:  string[32] = 'Data integrity error on drive %c';
  SSeekError:      string[22] = 'Seek error on drive %c';
  SUnknownMedia:   string[30] = 'Unknown media type in drive %c';
  SSectorNotFound: string[28] = 'Sector not found on drive %c';
  SOutOfPaper:     string[20] = 'Printer out of paper';
  SWriteFault:     string[23] = 'Write fault on drive %c';
  SReadFault:      string[22] = 'Read fault on drive %c';
  SGeneralFailure: string[28] = 'Hardware failure on drive %c';
  SBadImageOfFAT:  string[32] = 'Bad memory image of FAT detected';
  SDeviceError:    string[19] = 'Device access error';
  SInsertDisk:     string[27] = 'Insert diskette in drive %c';
  SRetryOrCancel:  string[27] = '<Enter> Retry  <Esc> Cancel';
  {$ENDIF}
{$ENDIF}

{ Critical error message translation table }

  ErrorString: array[0..15] of Word = (
    Ofs(SWriteProtected),
    Ofs(SCriticalError),
    Ofs(SDiskNotReady),
    Ofs(SCriticalError),
    Ofs(SDataIntegrity),
    Ofs(SCriticalError),
    Ofs(SSeekError),
    Ofs(SUnknownMedia),
    Ofs(SSectorNotFound),
    Ofs(SOutOfPaper),
    Ofs(SWriteFault),
    Ofs(SReadFault),
    Ofs(SGeneralFailure),
    Ofs(SBadImageOfFAT),
    Ofs(SDeviceError),
    Ofs(SInsertDisk));

{ System error handler routines }

procedure InitSysError; external;
procedure DoneSysError; external;

function SelectKey: Integer; near; assembler;
asm
        MOV     AH,3
        MOV     BH,0
        CALL    VideoInt
        PUSH    CX
        MOV     AH,1
        MOV     CX,2000H
        CALL    VideoInt
@@1:    MOV     AH,1
        INT     16H
        PUSHF
        MOV     AH,0
        INT     16H
        POPF
        JNE     @@1
        XOR     DX,DX
        CMP     AL,13
        JE      @@2
        INC     DX
        CMP     AL,27
        JNE     @@1
@@2:    POP     CX
        PUSH    DX
        MOV     AH,1
        CALL    VideoInt
        POP     AX
end;

{$V-}

function SystemError(ErrorCode: Integer; Drive: Byte): Integer;  {TONY}
const
  CShadow = $00;
  CFrame  = $4F;
  CText   = $FE;
  FontAddr: pointer = @DefaultSystemFont;

type
  PFontStruct = ^TFontStruct;
  TFontStruct = record
    Width: integer;     { Must be 8 ! }
    Height: integer;
    Image: array [0..32767] of byte;
  end;

var
  P: Pointer;
  S: string[63];
  i, XSize, YSize, XBeg, YBeg, XEnd, YEnd, Beg0, Beg1, Beg2: integer;
  Font: PFontStruct absolute FontAddr;

  function Attr2Back(Attr: word): Byte;
  inline(
     $58/        {pop ax}
     $C1/$E8/$04 {shr ax, 4}
  );

  function Attr2Fore(Attr: word): Byte;
  inline(
     $58         {pop ax}
  );

  procedure PutChar(x, y :integer; Ch: char; Color: Byte);
  begin
    DirectDisplayXxY(x, y,
      @(Font^.Image[Integer(Byte(Ch))*Font^.Height]),
      8, Font^.Height, ColorIndex^[Color], 0, False);
  end;

begin
  if FailSysErrors then
  begin
    SystemError := 1;
    Exit;
  end;
  if Font^.Width <> 8 then begin
    RunError(255);  { –ù–µ–≤–µ—Ä–Ω—ã–π —Å–∏—Å—Ç–µ–º–Ω—ã–π —à—Ä–∏—Ñ—Ç }
  end;

  P := Pointer(Drive + Ord('A'));
  FormatStr(S, PString(Ptr(DSeg, ErrorString[ErrorCode]))^, P);
  {SRetryOrCancel}
  {--- Obtain dimensions ---}
  XSize := Length(S);
  if Length(SRetryOrCancel) > XSize then XSize := Length(SRetryOrCancel);
  XSize := XSize * 8 + 20;
  YSize := Font^.Height * 3 + 50;
  XBeg := (ScreenWidth - XSize) div 2;
  XEnd := XBeg + XSize;
  YBeg := (ScreenHeight - YSize) div 3;  { –ü–µ—Ä–≤–∞—è —Ç—Ä–µ—Ç—å —ç–∫—Ä–∞–Ω–∞ }
  YEnd := YBeg + YSize;
  if Length(SRetryOrCancel) < Length(S) then begin
    Beg1 := XBeg;
    Beg2 := XBeg + (Length(S) - Length(SRetryOrCancel)) * 4;
    Beg0 := XBeg + (Length(S) - Length(SCriticalErrorHeader)) * 4;
  end else begin
    Beg2 := XBeg;
    Beg1 := XBeg + (Length(SRetryOrCancel) - Length(S)) * 4;
    Beg0 := XBeg + (Length(SRetryOrCancel) - Length(SCriticalErrorHeader)) * 4;
  end;
  {--- Drawing ---}
  HideMouse;
  for i:=YBeg to Pred(YEnd) do begin
    GDI.DirectHLine(XBeg, i, Pred(XEnd), ColorIndex^[Attr2Back(CFrame)]);
  end;
  GDI.DirectHLine(XBeg, YBeg, Pred(XEnd), ColorIndex^[Attr2Fore(CFrame)]);
  GDI.DirectHLine(XBeg, YBeg + Font^.Height + 6, Pred(XEnd), ColorIndex^[Attr2Fore(CText)]);
  GDI.DirectHLine(XBeg, Pred(YEnd), Pred(XEnd), ColorIndex^[Attr2Fore(CFrame)]);
  GDI.VLineStyleT(XBeg, YBeg, Pred(YEnd), 1, ColorIndex^[Attr2Fore(CFrame)], $FFFF);
  GDI.VLineStyleT(Pred(XEnd), YBeg, Pred(YEnd), 1, ColorIndex^[Attr2Fore(CFrame)], $FFFF);
  {SCriticalErrorHeader}
  for i:=1 to Length(SCriticalErrorHeader) do begin
    PutChar(Beg0 + 10 + (i-1)*8, YBeg + 3, SCriticalErrorHeader[i], Attr2Back(CText));
  end;
  for i:=1 to Length(S) do begin
    PutChar(Beg1 + 10 + (i-1)*8, YBeg + 20 + Font^.Height, S[i], Attr2Fore(CText));
  end;
  for i:=1 to Length(SRetryOrCancel) do begin
    PutChar(Beg2 + 10 + (i-1)*8, YEnd - 16 - Font^.Height, SRetryOrCancel[i], Attr2Back(CText));
  end;
  SystemBeep(sbSysError);
  {--- Selecting an action ---}
  SystemError := SelectKey;
  {--- Restoring ---}
  if BufferedStrategy then PutBufferPart(XBeg, YBeg, XEnd, YEnd)
    else SysNotifyProc(snRedraw, 0, nil);
  ShowMouse;
end;

procedure DriversSystemNotify(EventType, EventCommand : Word; EventInfo : Pointer);
Begin
 {$IFDEF EXTENDEDSOUND}
 if (EventType = snAlertSound) and (EventCommand in [sbSysError..sbDefault]) and
   (WaveFiles[EventCommand] <> '') then
   PlayWavFile(WaveFiles[EventCommand], EventCommand <> sbExit);
   {application must wait the end of exit sound}
 {$ENDIF}
End;

procedure SystemNotify(EventType, EventCommand : Word; EventInfo : Pointer);
Begin
  if @SysNotifyProc <> Nil then SysNotifyProc(EventType, EventCommand, EventInfo);
End;

{$V+}

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

const

{$IFDEF Russian}
  SysAltCode  = 254;
  LastAltCode = $35;
  AltCodes0: array[$02..$0B] of Char = '!":/%,.?()';
{$ELSE}
  SysAltCode  = 240;
  LastAltCode = $32;
{$ENDIF}

{$IFDEF Ukrainian}
  AltCodes1: array[$10..LastAltCode] of Char =
    '©Ê„™•≠£ËÈßÂ¯'#0#0'‰ˆ¢†Ø‡Æ´§¶Ù'#0#0#0'ÔÁ·¨®‚Ï°ÓÚ';
{$ELSE}
  {$IFDEF Russian}
  AltCodes1: array[$10..LastAltCode] of Char =
    '©Ê„™•≠£ËÈßÂÍ'#0#0'‰Î¢†Ø‡Æ´§¶Ì'#0#0#0'ÔÁ·¨®‚Ï°ÓÒ';
  {$ELSE} {English}
  AltCodes1: array[$10..LastAltCode] of Char =
    'QWERTYUIOP'#0#0#0#0'ASDFGHJKL'#0#0#0#0#0'ZXCVBNM';
  {$ENDIF}
{$ENDIF}

  AltCodes2: array[$78..$83] of Char =
    '1234567890-=';

function GetAltChar(KeyCode: Word): Char;
begin
  GetAltChar := #0;
  if Lo(KeyCode) = 0 then
    case Hi(KeyCode) of
      $02: GetAltChar := Char(SysAltCode);
      $10..LastAltCode: GetAltChar := AltCodes1[Hi(KeyCode)];
      $78..$83: GetAltChar := AltCodes2[Hi(KeyCode)];
    end;
end;

function GetAltCode(Ch: Char): Word;
var
  I: Word;
begin
  GetAltCode := 0;
  if Ch = #0 then Exit;
  Ch := UpCase(Ch);
  if Ch = Char(SysAltCode) then
  begin
    GetAltCode := $0200;
    Exit;
  end;
  for I := $10 to LastAltCode do
    {$IFDEF Russian}
    if UpCase(AltCodes1[I]) = Ch then
    {$ELSE}
    if AltCodes1[I] = Ch then
    {$ENDIF}
    begin
      GetAltCode := I shl 8;
      Exit;
    end;
  for I := $78 to $83 do
    if AltCodes2[I] = Ch then
    begin
      GetAltCode := I shl 8;
      Exit;
    end;
end;

function GetCtrlChar(KeyCode: Word): Char;
begin
  GetCtrlChar := #0;
  if (Lo(KeyCode) <> 0) and (Lo(KeyCode) <= Byte('Z') - Byte('A') + 1) then
    GetCtrlChar := Char(Lo(KeyCode) + Byte('A') - 1);
end;

function GetCtrlCode(Ch: Char): Word;
begin
  GetCtrlCode := GetAltCode(Ch) or (Byte(UpCase(Ch)) - Byte('A') + 1);
end;

function CtrlToArrow(KeyCode: Word): Word;
const
  NumCodes = 11;
  CtrlCodes: array[0..NumCodes-1] of Char = ^S^D^E^X^A^F^G^V^R^C^H;
  ArrowCodes: array[0..NumCodes-1] of Word =
    (kbLeft, kbRight, kbUp, kbDown, kbHome, kbEnd, kbDel, kbIns,
     kbPgUp, kbPgDn, kbBack);
var
  I: Integer;
begin
  CtrlToArrow := KeyCode;
  for I := 0 to NumCodes - 1 do
    if WordRec(KeyCode).Lo = Byte(CtrlCodes[I]) then
    begin
      CtrlToArrow := ArrowCodes[I];
      Exit;
    end;
end;


{$IFDEF Russian}

{$IFDEF DPMI}
{$L CHAR.OBP}
{$ELSE}
{$L CHAR.OBJ}
{$ENDIF}

procedure InitKbdDriver; external;
procedure DoneKbdDriver; external;
function UpCase(Ch: Char): Char; external;

{$ENDIF}

{ String formatting routines }

{$L FORMAT.OBJ}

procedure FormatStr(var Result: String; const Format: String; var Params);
external {FORMAT};

procedure PrintStr(const S: String); assembler;
asm
        PUSH    DS
        LDS     SI,S
        CLD
        LODSB
        XOR     AH,AH
        XCHG    AX,CX
        MOV     AH,40H
        MOV     BX,1
        MOV     DX,SI
        INT     21H
        POP     DS
end;

function InputStr(Hide: Boolean): String;
var
  Key: Char;
  S: String;
begin
  S:= '';
  repeat
    Key:= GetKey;
    if (Key >= ' ') and (Length(S) < 40) then
    begin
      S:= S + Upcase(Key);
      if not Hide then PrintStr(S[Length(S)]);
    end else if (Key = ^H) and (Length(S) > 0) then
    begin
      Delete(S, Length(S), 1);
      if not Hide then PrintStr(^H' '^H);
    end;
  until Key = ^M;
  PrintStr(^M^J);
  InputStr:= S;
end;

procedure ReStr(var P: PString; const S: String);
begin
  if (Seg(P^) <> DSeg) and (Seg(P^) <> SSeg) and (P <> Nil) then DisposeStr(P);
  P:= NewStr(S);
end;


{ Buffer move routines }

procedure MoveBuf(var Dest; var Source; Attr: Byte; Count: Word); assembler;
asm
        MOV     CX,Count
        JCXZ    @@5
        MOV     DX,DS
        LES     DI,Dest
        LDS     SI,Source
        MOV     AH,Attr
        CLD
        OR      AH,AH
        JE      @@3
@@1:    LODSB
        STOSW
        LOOP    @@1
        JMP     @@4
@@2:    INC     DI
@@3:    MOVSB
        LOOP    @@2
@@4:    MOV     DS,DX
@@5:
end;

procedure MoveChar(var Dest; C: Char; Attr: Byte; Count: Word); assembler;
asm
        MOV     CX,Count
        JCXZ    @@4
        LES     DI,Dest
        MOV     AL,C
        MOV     AH,Attr
        CLD
        OR      AL,AL
        JE      @@1
        OR      AH,AH
        JE      @@3
        REP     STOSW
        JMP     @@4
@@1:    MOV     AL,AH
@@2:    INC     DI
@@3:    STOSB
        LOOP    @@2
@@4:
end;

procedure MoveCStr(var Dest; const Str: String; Attrs: Word); assembler;
asm
        MOV     DX,DS
        LDS     SI,Str
        CLD
        LODSB
        MOV     CL,AL
        XOR     CH,CH
        JCXZ    @@3
        LES     DI,Dest
        MOV     BX,Attrs
        MOV     AH,BL
@@1:    LODSB
        CMP     AL,'~'
        JE      @@2
        STOSW
        LOOP    @@1
        JMP     @@3
@@2:    XCHG    AH,BH
        LOOP    @@1
@@3:    MOV     DS,DX
end;

procedure MoveStr(var Dest; const Str: String; Attr: Byte); assembler;
asm
        MOV     DX,DS
        LDS     SI,Str
        CLD
        LODSB
        MOV     CL,AL
        XOR     CH,CH
        JCXZ    @@4
        LES     DI,Dest
        MOV     AH,Attr
        OR      AH,AH
        JE      @@3
@@1:    LODSB
        STOSW
        LOOP    @@1
        JMP     @@4
@@2:    INC     DI
@@3:    MOVSB
        LOOP    @@2
@@4:    MOV     DS,DX
end;

function CStrLen(const S: String): Integer; assembler;
asm
        LES     DI,S
        MOV     CL,ES:[DI]
        INC     DI
        XOR     CH,CH
        MOV     BX,CX
        JCXZ    @@2
        MOV     AL,'~'
        CLD
@@1:    REPNE   SCASB
        JNE     @@2
        DEC     BX
        JMP     @@1
@@2:    MOV     AX,BX
end;

function CStr(S : String) : String;
var
  I : Byte;
Begin
  I := 1;
  if Byte(S[0]) > 0 then while I <= Byte(S[0]) do if S[I] = '~' then
    Delete(S, I, 1) else Inc(I);
  CStr := S;
End;

{ Drivers unit initialization and shutdown }

var
  SaveExit: Pointer;

procedure ExitDrivers; far;
begin
  DoneSysError;
  DoneEvents;
{$IFDEF RUSSIAN}
  DoneKbdDriver;
{$ENDIF}
  ExitProc := SaveExit;
end;

begin
  DetectMouse;
  SaveExit := ExitProc;
  ExitProc := @ExitDrivers;
{$IFDEF RUSSIAN}
  InitKbdDriver;
{$ENDIF}
  BIOSDataSele := Seg0040;
  DelayCalibrate;
end.
