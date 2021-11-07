{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        :                                                      █
  █                                                                    █
  █ Description :                                                      █
  █                                                                    █
  █ Author      :                                                      █
  █════════════════════════════════════════════════════════════════════█
  █                                                                    █
  █                                                                    █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}

unit EgPrint;

interface uses Objects, EgString;

Type
  PPrinter = ^TPrinter;
  TPrinter = object(TObject)
    constructor Init(Name: String);
    destructor  Done; virtual;
    function    WrChar(var Character: char): boolean;
    function    WrStr(const S: string): boolean;
    function    NewLine: boolean;
    function    NewLines(i: integer): boolean;
    function    Ready: boolean;
   private
    IsDevice: Boolean;
    FHandle: Word;
    PreDefined: Boolean;
  end;

implementation

constructor TPrinter.Init(Name: String);
var
  Handle: Word;
  Dev: byte;
  PS: ^String;
begin
  inherited Init;
  Name := StUpCase(Name);
  if Name = 'PRN' then begin
    Handle := 4;
    PreDefined := True;
  end else begin
    Name[byte(Name[0])+1] := #0;
    Handle := $FFFF;
    PS := @Name;
    asm
      { Пытаемся открыть на запись }
      mov    ax, 3D01h
      push   ds
      lds    dx, PS
      inc    dx
      int    21h
      pop    ds
      jc     @@1  { Не удалось }
@@3:
      mov    Handle, ax
      { Позиционируем на конец файла - ДОБАВЛЕНИЕ }
      mov    bx, ax
      mov    ax, 4202h
      xor    cx, cx
      xor    dx, dx
      int    21h    { Удалось, не удалось - не важно }
      jmp    @@2
@@1:
      { Пытаемся создать этот файл }
      mov    ah, 3Ch
      mov    cx, 20h
      push   ds
      lds    dx, PS
      inc    dx
      int    21h
      pop    ds
      jnc    @@3
@@2:
    end;
    if Handle = $FFFF then Fail;
  end;
  Dev := 2;
  asm
    mov    ax, 4400h
    mov    bx, Handle
    int    21h
    jc     @@1
    mov    Dev, 1
    and    dl, 80h
    jnz    @@1
    dec    Dev
@@1:
  end;
  if Dev = 2 then Fail;
  IsDevice := (Dev = 1);
  FHandle := Handle;
end;

destructor TPrinter.Done;
var
  Handle: Word;
begin
  if not PreDefined then begin
    Handle := FHandle;
    asm
      mov    ah, 3Eh
      mov    bx, Handle
      int    21h
    end;
  end;
  inherited Done;
end;

function TPrinter.WrChar(var Character: char): boolean; assembler;
asm
  les    di, Self
  mov    ah, 40h
  mov    cx, 1
  mov    bx, es:[di+FHandle]
  push   ds
  lds    dx, Character
  int    21h
  pop    ds
  mov    al, 0
  jc     @@Error
  inc    al
@@Error:
end;

function TPrinter.WrStr(const S: string): boolean;
var
  i: integer;
  c: char;
begin
  WrStr := False;
  for i:=1 to Length(S) do begin
    c := S[i];
    if not WrChar(c) then
      Exit;
  end;
  WrStr := True;
end;

function TPrinter.NewLine: boolean;
begin
  NewLine := WrStr(#13#10);
end;

function TPrinter.NewLines(i: integer): boolean;
begin
  NewLines := False;
  while (i > 0) do if not NewLine then Exit else Dec(i);
  NewLines := True;
end;

function TPrinter.Ready: boolean; assembler;
asm
  les    di, Self
  mov    al, es:[di+IsDevice]
  or     al, al
  jz     @@OKay
  mov    ax, 4407h
  mov    bx, es:[di+FHandle]
  int    21h
  jnc    @@Exit
  xor    al, al
  jmp    @@Exit
@@Okay:
  inc    al
@@Exit:
end;


end.
