{$I-}
Const
  Os2 = True;
  Win = False;

  HdrSize : array[Boolean] of Byte = ($3d, $39);

  WinHdr : array[0..$3d] of Byte = (
$00, $00, $01, $00, $01, $00, $20, $20, $10, $00, $01, $00, $04, $00, $E8, $02,
$00, $00, $16, $00, $00, $00, $28, $00, $00, $00, $20, $00, $00, $00, $40, $00,
$00, $00, $01, $00, $04, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
  Os2Hdr : array[0..$39] of Byte = (
$43, $49, $1A, $00, $00, $00, $10, $00, $10, $00, $6A, $00, $00, $00, $0C, $00,
$00, $00, $20, $00, $40, $00, $01, $00, $01, $00, $00, $00, $00, $FF, $FF, $FF,
$43, $49, $1A, $00, $00, $00, $10, $00, $10, $00, $6A, $01, $00, $00, $0C, $00,
$00, $00, $20, $00, $20, $00, $01, $00, $04, $00);

  Headers  : array[Boolean] of Pointer = (@WinHdr, @Os2Hdr);
  PalStart : array[Boolean] of Word    = ($3e, $3a);
  PalSize  : array[Boolean] of Word    = (64, 48);
  ImgStart : array[Boolean] of Word    = ($7e, $16A);
  MonStart : array[Boolean] of Word    = ($27e, $ea);
  WholSize : array[Boolean] of Word    = (766, 874);

Var
  I, O : Boolean;
  S : String;
  F : File;
  L : LongInt;
  PalBuf : array[0..63]  of Byte;
  GapBuf : array[0..874] of Byte;
  ImgBuf : array[0..512] of Byte;
  MonBuf : array[0..128] of Byte;

Procedure Error(x : String);
Begin
  WriteLn('(!) Error: ');
  halt(5);
End;

Begin
  WriteLn('DK Inc. 1997 OS2 <-> Windoze 16 color 32x32 icon converter.');
  if ParamCount < 3 then begin
    WriteLn('Format: os2wico source.ico dest.ico conversion_type');
    WriteLn('conversion_type can be OS/2 or WIN');
    WriteLn('(!) Warning - source icon must be exactly 16 color and 32x32.');
    WriteLn(' Os/2 standard icon editor corressponds to that as Independent Color Form (=VGA)');
    Exit;
  end;
  S := ParamStr(3);
  for L := 1 to Byte(S[0]) do S[L] := Upcase(S[L]);
  if S = 'OS/2' then O := Os2 else
  if S = 'WIN' then O := Win else Error('conversion_type must be OS/2 or WIN');
  I := not O;
  Assign(F, ParamStr(1));
  Reset(F, 1);
  if IOResult <> 0 then Error(ParamStr(1) + ' cannot be open');
  Seek(F, PalStart[I]);
  BlockRead(F, PalBuf, PalSize[I]);
  Seek(F, ImgStart[I]);
  BlockRead(F, ImgBuf, 512);
  Seek(F, MonStart[I]);
  BlockRead(F, MonBuf, 128);
  if IOResult <> 0 then Error(ParamStr(1) + ' cannot be read');
  Close(F);

  Assign(F, ParamStr(2));
  Rewrite(F, 1);
  if IOResult <> 0 then Error(ParamStr(2) + ' cannot be open');
  FillChar(GapBuf, WholSize[O], 0);
  BlockWrite(F, GapBuf, WholSize[O]);
  if IOResult <> 0 then Error(ParamStr(2) + ' cannot be written');
  Seek(F, 0);
  BlockWrite(F, Headers[O]^, HdrSize[O]);
  Seek(F, PalStart[O]);
  for L := 0 to 15 do Move(PalBuf[L * (3 + Byte(O))], GapBuf[L * (3 + Byte(I))], 3);
  BlockWrite(F, GapBuf, PalSize[O]);
  Seek(F, ImgStart[O]);
  BlockWrite(F, ImgBuf, 512);
  Seek(F, MonStart[O]);
  BlockWrite(F, MonBuf, 128);
  Close(F);
End.