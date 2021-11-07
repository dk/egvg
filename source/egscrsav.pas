{
  ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
  Û Unit        : EgScrSav.Pas                                         Û
  Û                                                                    Û
  Û Description : “­¨¢¥àá «ì­ ï á®åà ­ï«ª  íªà ­  ¤«ï EG/TV            Û
  Û                                                                    Û
  Û Author      : Tony Berezin                                         Û
  ÛÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÛ
  Û                                                                    Û
  Û                                                                    Û
  ÛÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÛ
}

unit EgScrSaver;

interface uses Objects, Drivers, GDI, EgInline;

Const
  ssfTimer         = $01;  { Activate on time }
  ssfMouseReset    = $02;  { Activate when mouse in upper right corner }
  ssfMousePreserve = $04;  { Don't activate when mouse in upper left }
  ssfAllOptions    = $07;

Type
  PScreenSaver = ^TScreenSaver;
  TScreenSaver = object(TObject)
    constructor Init(AFlags: Byte; ATime: Word);
    destructor  Done; virtual;
    procedure   Check; { Call it from Idle of your application }
    procedure   SaveLoop;
    procedure   SaveAction(FirstCall: Boolean); virtual;
    procedure   Reset; { Call it when Idle is not called (override GetEvent)}
   private
    LastTime: Longint;
    Flags: Byte;
    Time: Longint;
  end;

implementation

var
  LastMouse: TPoint;
  CurrentFlags: Byte;

function GetTime: longint; assembler;
asm
  xor    ah, ah
  int    1Ah
  mov    ax, dx
  mov    dx, cx
end;

constructor TScreenSaver.Init(AFlags: Byte; ATime: Word);
begin
  inherited Init;
  Flags := AFlags;
  Time := LongInt(ATime) * 91;
  Time := Time div 5;
  Reset;
  LastMouse.X := 0;
  LastMouse.Y := 0;
end;

destructor TScreenSaver.Done;
begin
  inherited Done;
end;

procedure TScreenSaver.Reset;
begin
  LastTime := GetTime;
end;

procedure TScreenSaver.Check;
var
  NewTime: Longint;
label Ending;
begin
  if ByteFlagIsSet(Flags, ssfMousePreserve) and
     (not (ButtonCount = 0)) and
     (MouseWhere.X = 0) and (MouseWhere.Y = 0)
  then begin
    Reset;
    goto Ending;
  end;
  if ByteFlagIsSet(Flags, ssfMouseReset) and
    (MouseWhere.X = ScreenWidth-1) and (MouseWhere.Y = 0) and
    ( not ((LastMouse.X = MouseWhere.X) and (LastMouse.Y = MouseWhere.Y)))
  then begin
    SaveLoop;
    goto Ending;
  end;
  if ByteFlagIsSet(Flags, ssfTimer) then begin
    NewTime := GetTime;
    if NewTime - LastTime > 0 then NewTime := NewTime - LastTime else NewTime := LastTime - NewTime;
    if NewTime >= Time then SaveLoop;
  end;
Ending:
  LastMouse := MouseWhere;
end;

function CheckEvent: boolean; near; assembler;
asm
  cmp   ButtonCount, 0
  je    @@KbdCheck
  mov   ax, 3
  int   33h
  or    bx, bx
  jne   @@RetTrue
  cmp   cx, LastMouse.X
  jne   @@RetTrue
  cmp   dx, LastMouse.Y
  jne   @@RetTrue
@@KbdCheck:
  mov   ah, 2
  int   16h
  cmp   CurrentFlags, al
  jne   @@RetTrue
  mov   ah, 1
  int   16h
  jz    @@RetFalse
  xor   ax, ax
  int   16h
@@RetTrue:
  mov   ax, 1
  jmp   @@Exit
@@RetFalse:
  xor   ax, ax
@@Exit:
end;

procedure TScreenSaver.SaveLoop;
var
  y: Integer;
begin
  HideMouse;
  for y := 0 to ScreenHeight - 1 do DirectHLine(0, y, ScreenWidth-1, 0);
  LastMouse := MouseWhere;
  asm
    mov   ah, 2
    int   16h
    mov   CurrentFlags, al
  end;
  SaveAction(True);
  while not CheckEvent do SaveAction(False);
  PutBufferPart(0, 0, ScreenWidth-1, ScreenHeight-1);
  ShowMouse;
  Reset;
end;

procedure TScreenSaver.SaveAction(FirstCall: Boolean); begin end;


end.
