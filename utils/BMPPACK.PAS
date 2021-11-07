{$M $7000, 100000, 100000);
{$F+,I-}
Uses DOS, Objects, Streams, CRT;

Type
  THDR = array[1..2] of Char;

  TLongTimeHandler = Object(TProcedure)
    Mode  : ShortInt;
    Procedure TProc; Virtual;
  End;
  PLongTimeHandler = ^TLongTimeHandler;

Procedure TLongTimeHandler.TProc;
Var
  S : String[30];
Begin
  if TVarLong < 0 then begin
    Mode := TVarLong;
  end else begin
    case Mode of
    -1 : S := 'Unpack : ';
    -2 : S := 'Pack : ';
    else S := ''; end;
    GotoXY(1, WhereY);
    if TVarLong <= 100 then Write(S, TVarLong, '% ');
  end;
End;

Const
  BM : THDR = 'BM';
  BP : THDR = 'BP';
Var
  I, O : TBufStream;
  HDR  : THDR;
  Pack : Boolean;
  IE, OE : ExtStr;
  D      : DirStr;
  N      : NameStr;
  ResP   : Integer;
  P      : TLongTimeHandler;
  Max, Min : LongInt;

Begin
  WriteLn('DK Inc. 1996  BMP packer for SVG vision.');
  if ParamCount < 1 then begin
    WriteLn('(!) Format : BMPPACK FILENAME.EXT');
    Halt(1);
  end;
  I.Init(ParamStr(1), stOpenRead, 1024);
  if I.Status <> 0 then begin
    WriteLn('(!) Cannot open/read ', ParamStr(1));
    Halt;
  end;
  I.Read(HDR, 2);
  if (HDR <> BM) and (HDR <> BP) then begin
    WriteLn('(!) ', ParamStr(1), ' is not a bitmap file');
    I.Done;
    Halt;
  end;

  Pack := HDR = BM;
  FSplit(ParamStr(1), D, N, IE);
  if Pack then OE := '.PBM' else OE := '.BMP';
  if IE = OE then begin
    I.Done;
    WriteLn('(!) Output file is the same as input');
    Halt;
  end;

  O.Init(D+N+OE, stCreate, 1024);
  if O.Status <> 0 then begin
    I.Done;
    WriteLn('(!) Cannot open ', D+N+OE);
    Halt;
  end;

  if Pack then HDR := BP else HDR := BM;
  O.Write(HDR, 2);
  if Pack then begin
    Min := I.GetSize;
    O.Write(Min, 4);
  end else I.Read(Min, 4);
  if O.Status <> 0 then begin
    I.Done;
    WriteLn('(!) Cannot write ', D+N+OE);
    Halt;
  end;

  SetPackBuffer;
  P.Init;
  Percenter := @P;
  if Pack then ResP := StreamImplode(@I, @O)
    else ResP := StreamExplode(@I, @O);
  Percenter := Nil;
  P.Done;
  if not Pack then begin
    if O.GetSize <> Min then WriteLn('(!) Packed image corrupt.');
  end;
  FreePackBuffer;
  if ResP <> 0 then WriteLn('(!) Packing operation has not been accomplished properly.')
    else begin
    WriteLn(#13#10'Ok.');
    WriteLn('Original size : ', I.GetSize);
    WriteLn('New size      : ', O.GetSize);
    if I.GetSize > O.GetSize then begin
      Min := O.GetSize;
      Max := I.GetSize;
    end else begin
      Min := I.GetSize;
      Max := O.GetSize;
    end;
    WriteLn('Ratio         : ',  Min * 100 / Max:1:1, '%.');
  end;
  O.Done;
  I.Done;
End.