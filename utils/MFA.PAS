Program MFA; { Make Conversion  Font -> Assembler in Code }

uses EgString, Memory;

type
  PByteArray = ^ByteArray;
  ByteArray = array [0..65534] of byte;

var
  F: File;
  OutF: Text;
  P: PByteArray;
  SZ: Longint;
  S: string;
  i, j: word;


begin
  InitMemory;
  WriteLn;
  WriteLn('Font -> Assembler in Code Conversion Utility Version 1.00');
  WriteLn('Copyright (C) 1994 by Tony Berezin'); WriteLn;
  if ParamCount<>2 then begin
    WriteLn('Usage --- MFA FontFileName OutPutFileName'); Halt;
  end;
  {$I-}
  Assign(F, ParamStr(1));
  Reset(F, 1);
  if IOResult<>0 then begin
    WriteLn('File open error --- ', ParamStr(1)); Halt;
  end;
  SZ := FileSize(F);
  if SZ>SizeOf(ByteArray) then begin
    WriteLn('Input file too big --- ', ParamStr(1));
    Close(F);
    Halt;
  end;
  GetMem(P, SZ);
  if P = Nil then begin
    WriteLn('Not enough memory');
    Close(F);
    Halt;
  end;
  BlockRead(F, P^, SZ);
  if IOResult<>0 then begin
    WriteLn('File read error --- ', ParamStr(1));
    Close(F);
    Halt;
  end;
  Close(F);
  Assign(OutF, ParamStr(2));
  Rewrite(OutF);
  if IOResult<>0 then begin
    WriteLn('File create error --- ', ParamStr(2));
    FreeMem(P, SZ);
    Halt;
  end;
  i := 0;
  while i<SZ do begin
    S := '        DB      ';
    j := 0;
    while (j<16) and (longint(i)+longint(j)<SZ) do begin
      S := S + '0' + HexB(P^[i+j]) + 'H, ';
      Inc(j);
    end;
    S[0] := char(byte(S[0])-2);
    WriteLn(OutF, S);
    Inc(i, 16);
  end;
  Close(OutF);
  FreeMem(P, SZ);
  DoneMemory;
  {$I+}
end.