unit Gr;

{ Unit Gr, Version 1.25.003, Copyright 1993 by Matthias K”ppe.

}

{$A+,B-,F-,G+,O-,R-,S-,X+}

interface

uses Objects, Memory, GDI;


var
  GRT : TPaintInfo;
  GRF : TFont;


 (* GrMode: Word;
  SizeX:Word{ absolute GDI.MaximalX;};
  SizeY: Word{ absolute GDI.MaximalY;};
  Page0Seg, Free0Seg, Page1Seg, Free1Seg, EndSeg: Word;
  GrFlags: Word;
  ScreenF: Word;
  Granularity: Byte;
  MapFlags: Byte;
  RealBytesPerLine: Word;
  ActivePage: Word;
  ActiveSeg: Word;
  ActivePSeg: Word;
  ActivePOfs: Word;
  BytesPerLine: Word;
  WindowSize: Word;
  OffsetMask: Word;
  MapGranMask: Word;
  GransPerWindow: Word;
  MapGranRight, MapGranLeft: Byte;
  WindowNum: Word;
  WindowAddr: pointer;
  DrawOrigin: TPoint;
  ClipRect: TRect;*)

const
  Transparent = 1;
  Opaque = 2;
  TempCount = 32;
  UserCount = 16;
 { GrActive: Boolean = true;
  BkColor: Word = 0;
  BkMode: Integer = Transparent;}
  TempUsed: Word = 0;

var
  TempHandles: array[1..TempCount] of Word;
  TempMem: pointer;
  TempMemSize: Word;

  User00: pointer;
  User01: pointer;
  User02: pointer;
  User03: pointer;
  User04: pointer;
  User05: pointer;
  User06: pointer;
  User07: pointer;
  User08: pointer;
  User09: pointer;
  User0A: pointer;
  User0B: pointer;
  User0C: pointer;
  User0D: pointer;
  User0E: pointer;
  User0F: pointer;

{ Quality indicating bytes
}
const
  Quality00: Byte = 0;
  Quality01: Byte = 0;
  Quality02: Byte = 0;
  Quality03: Byte = 0;
  Quality04: Byte = 0;
  Quality05: Byte = 0;
  Quality06: Byte = 0;
  Quality07: Byte = 0;
  Quality08: Byte = 0;
  Quality09: Byte = 0;
  Quality0A: Byte = 0;
  Quality0B: Byte = 0;
  Quality0C: Byte = 0;
  Quality0D: Byte = 0;
  Quality0E: Byte = 0;
  Quality0F: Byte = 0;

{ MetaGraph support
}
const
  ms_Draw       = 1;
  ms_Record     = 2;
  ms_BGI        = 4;
  ms_Execute    = 8;

  gcnUpdAll    = 1;
  gcnUpdOrigin = 2;
  gcnStopUpd   = 10;
  gcnContUpd   = 11;
  gcnHaltUpd   = 12;
  gcnStartUpd  = 13;
  gcnUpdOnReq  = 14;

  gcpColor     = 1;
  gcpLineStyle = 2;
  gcpSolidThLn = 3;
  gcpGetSize   = $000;
  gcpGetParams = $100;
  gcpSetParams = $200;

  npInstall    = 0;
  npUninstall  = 1;
  npGetQuality = 2;
  npSetNext    = 3;

  gnpInitGraphics  = 100;
  gnpCloseGraphics = 101;
  gnpBkMode        = 102;


type
  TLineProc = procedure(x1, y1, x2, y2: Integer);
  TClipNotifyProc = procedure(Msg: Word);
  TInitGraphProc = function: Boolean;
  TChParamsProc = function(Cmd: Word; var Buf): Word;
  TNotifyProc = function(Notice: Word; Info: LongInt): LongInt;
  TSetDispStartProc = procedure(Linear: LongInt);

(*const
  MetaState: Word = ms_Draw or ms_BGI;


var
  MetaClipRect: TRect;
  MetaOrigin: TPoint;
  UserRegArea: array[0..6] of Byte;*)

procedure InstallNotifyProc(var Chain: TNotifyProc;
  Proc: TNotifyProc);
procedure UninstallNotifyProc(var Chain: TNotifyProc;
  Proc: TNotifyProc);
function DefaultNotify(Notice: Word; Info: LongInt; ThisProc: TNotifyProc;
  var NextProc: TNotifyProc; Quality: Byte): LongInt;


var
   ExtSave: pointer                     absolute User00;
  qExtSave: Byte                        absolute Quality00;
   LineProc: TLineProc                  absolute User01;
  qLineProc: Byte                       absolute Quality01;
   ClipNotifyProc: TClipNotifyProc      absolute User02;
  qClipNotifyProc: Byte                 absolute Quality02;
   InitGraphProc: TInitGraphProc        absolute User03;
  qInitGraphProc: Byte                  absolute Quality03;
   CloseGraphProc: procedure            absolute User04;
  qCloseGraphProc: Byte                 absolute Quality04;
   ActivePageProc: procedure            absolute User05;
  qActivePageProc: Byte                 absolute Quality05;
   ChParamsProc: TChParamsProc          absolute User06;
  qChParamsProc: Byte                   absolute Quality06;
   GrNotifyProc: TNotifyProc            absolute User07;
  qGrNotifyProc: Byte                   absolute Quality07;
   MapAddrProc: procedure               absolute User08;
  qMapAddrProc: Byte                    absolute Quality08;
   SetDispStartProc: TSetDispStartProc  absolute User09;
  qSetDispStartProc: Byte               absolute Quality09;

procedure GetTempMem(var Handle: Integer; Size: Word);
function SelOfs(ParOfs: pointer): pointer;
function SelOfsZero(ParOfs: pointer): pointer;
procedure UserParams(Count: Word);inline($58 {pop ax});
procedure UserDummy;
procedure FillUserXX;


implementation

{ Background color and mode
}
{procedure SetBkColor(Color: Word);
Begin
  BkColor := Color
End;}

{procedure SetBkMode(Mode: Integer); assembler;
asm
        mov     ax, Mode
        cmp     ax, BkMode
        je      @@0
        mov     BkMode, ax
        mov     ax, 3
        push    gnpBkMode
        sub     sp, 4
        call    GrNotifyProc
@@0:
end;    }

{ VGA register handling *****************************************************
}
(*procedure SaveRegs; assembler;
Asm
        PUSHF
        CLD
        MOV     DI, OFFSET UserRegArea
        MOV     AX, DS
        MOV     ES, AX
        MOV     DX, 03CEH               { GC }
        MOV     AL, 3                   { Data Rotate }
        OUT     DX, AL
        INC     DX
        IN      AL, DX
        DEC     DX
        STOSB
        MOV     AL, 1                   { Enable Set/Reset }
        OUT     DX, AL
        INC     DX
        IN      AL, DX
        DEC     DX
        STOSB
        MOV     AL, 8                   { Bit Mask }
        OUT     DX, AL
        INC     DX
        IN      AL, DX
        DEC     DX
        STOSB
        MOV     AL, 5                   { Graphics Mode }
        OUT     DX, AL
        INC     DX
        IN      AL, DX
        STOSB
        MOV     DX, 03C4H               { SC }
        MOV     AL, 2                   { Map Mask }
        OUT     DX, AL
        INC     DX
        IN      AL, DX
        STOSB
        POPF
End;

procedure RestoreRegs; assembler;
Asm
        PUSHF
        CLD
        MOV     SI, OFFSET UserRegArea
        MOV     DX, 03CEH               { GC }
        MOV     AH, 3                   { Data Rotate }
        LODSB
        XCHG    AH, AL
        OUT     DX, AX
        MOV     AH, 1                   { Enable Set/Reset }
        LODSB
        XCHG    AH, AL
        OUT     DX, AX
        MOV     AH, 8                   { Bit Mask }
        LODSB
        XCHG    AH, AL
        OUT     DX, AX
        INC     SI
        MOV     DX, 03C4H               { SC }
        MOV     AH, 2                   { Map Mask }
        LODSB
        XCHG    AH, AL
        OUT     DX, AX
        POPF
End;*)

function SelOfs(ParOfs: pointer): pointer; assembler;
asm
        MOV     DX, WORD PTR ParOfs + 2
        MOV     AX, WORD PTR ParOfs
end;

function SelOfsZero(ParOfs: pointer): pointer; assembler;
asm
        MOV     DX, WORD PTR ParOfs + 2
        MOV     AX, WORD PTR ParOfs
end;



var
  DummyJumpback: pointer;

procedure UserDummy; assembler;
Asm
        POP     WORD PTR DummyJumpBack
        POP     WORD PTR DummyJumpBack+2
        SHL     AX, 1
        ADD     SP, AX
        XOR     AX, AX                  { Funktionsergebnis }
        MOV     DX, AX
        JMP     DummyJumpback
End;

procedure FillUserXX; assembler;
Asm
        MOV     DI, OFFSET User00
        MOV     SI, OFFSET Quality00
        MOV     CX, UserCount
        MOV     DX, CS
        MOV     BX, OFFSET UserDummy
@@2:    CMP     BYTE PTR [SI], 0
        JNZ     @@1
        MOV     [DI], BX
        MOV     [DI+2], DX
@@1:    INC     SI
        ADD     DI, 4
        LOOP    @@2
End;

{ Notification procedure handling *******************************************
}
procedure InstallNotifyProc;
var
  Res: LongInt;
Begin
  If @Chain = nil then @Chain := @UserDummy;
  UserParams(3);
  Res := Chain(npInstall, LongInt(@Proc));
  If Res = 0 then Begin
    Res := LongInt(@Chain);
    Chain := Proc
  End else
  If Res = LongInt(@Chain)
  then Chain := Proc;
  UserParams(3);
  Proc(npSetNext, Res)
End;

procedure UninstallNotifyProc;
var
  Res: LongInt;
Begin
  UserParams(3);
  Res := Chain(npUninstall, LongInt(@Proc));
  If Res <> 0 then
  Chain := TNotifyProc(Res)
End;

function DefaultNotify;
var
  Res: LongInt;
Begin
  case Notice of
    npInstall:
      If TNotifyProc(Info)(npGetQuality, 0) >= Quality
      then DefaultNotify := LongInt(@ThisProc)
      else Begin
        UserParams(3);
        Res := NextProc(npInstall, Info);
        If Res = 0 then Res := LongInt(@NextProc);
        DefaultNotify := Res;
        If (Res = LongInt(@NextProc)) or (Res = 0) then
          LongInt(@NextProc) := Info
      End;
    npUninstall:
      If LongInt(@ThisProc) = Info
      then DefaultNotify := LongInt(@NextProc)
      else Begin
        UserParams(3);
        Res := NextProc(npUninstall, Info);
        DefaultNotify := LongInt(@ThisProc);
        If Res <> 0 then
        LongInt(@NextProc) := Res
      End;
    npGetQuality:
      DefaultNotify := Quality;
    npSetNext:
      NextProc := TNotifyProc(Info);
  else
    Begin
      UserParams(3);
      NextProc(Notice, Info)
    End;
  end;
End;

{ Gr Temporary Memory *******************************************************
}

procedure GetTempMem(var Handle: Integer; Size: Word);
var
  i: Integer;
Begin
  If Handle = 0
  then Begin
    If TempUsed >= TempCount then Exit;
    Inc(TempUsed);
    Handle := TempUsed
  End;
  TempHandles[Handle] := Size;
  For i := 1 to TempUsed do
    If TempHandles[Handle] > Size
    then Size := TempHandles[Handle];
  If Size <> TempMemSize
  then Begin
    If TempMem <> nil
    then FreeMem(TempMem, TempMemSize);
    GetMem(TempMem, Size);
    TempMemSize := Size
  End
End;

{ MAIN **********************************************************************
}
  {SizeX := 2000; SizeY := 2000;
  Page0Seg := SegA000;
  Free0Seg := SegA000;
  Page1Seg := SegA000;
  Free1Seg := SegA000;
  EndSeg   := SegB000;
  GrFlags  := 0;
  ScreenF  := 10000;
  Granularity := 4;
  MapFlags := 1;
  RealBytesPerLine := 80;

  ActivePage := 0;
  ActiveSeg  := $A000;
  ActivePSeg := SegA000;
  ActivePOfs := 0;
  BytesPerLine:= 80;

  WindowSize:= 0;
  OffsetMask:= $ffff;
  MapGranMask:= $ffff;
  GransPerWindow:=1;
  MapGranRight:= 16;
  MapGranLeft:= 0;
  WindowNum:=0;
  WindowAddr:=Ptr(SegA000, 0);
  DrawOrigin.X := 0; DrawOrigin.Y := 0;
  ClipRect.Assign(0,0,2000,2000);
  MetaClipRect := ClipRect;}

Procedure Line(x1, y1, x2, y2: Integer); Far;
Begin
  GDI.Line(x1, y1, x2, y2, GRT);
End;


Begin
  FillUserXX;
  LineProc := Line;
  qLineProc := 0;
  DefaultPaint(GRT);
  DefaultFont(GRF);
End.
