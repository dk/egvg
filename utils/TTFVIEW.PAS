{-cd -m -dRUSSIAN}
{$F+,I-,S-,R-}
Uses DOS, SVGA256, GDI, MFonts, GR, TTMYF, EgString, Bitmaps, Palettes,
     CRT, Streams, TTF, Image;

Var
  A, B : Word;
  F : PFontRec;
  H, I : Word;
  D : DirStr;
  N : NameStr;
  E : ExtStr;
  P : PImage;
  LP : TLogPalette;
  BMPS, LINE : String;
  FontName : String;
  DizF : Text;
  Drv : PScreenDriver;

Const
  WriteDiz : Boolean = False;

Procedure IPErr;
Begin
  WriteLn('(!) Insufficient parameters.');
  Halt(0);
End;

Procedure MakeOut;
Begin
  SetTextJustify(0,2);
  SetTextParams(A, 0, 15, False);
  LocMaximalX := Drv^.MaximalX;
  LocMaximalY := Drv^.MaximalY;
  if Line <> '' then OutTextXY(0, 0, Line) else begin
    OutTextXY(0, 0, 'AaBbCcZz');
    OutTextXY(0, H+30, '123-€ Ÿï');
  end;
  {BarStyle(0, 0, 100, 100, GRT);}
End;

Begin
  WriteLn('DK Inc. 1996  Fonts viewer. (2:464/46.36@fidonet)');
  if ParamCount = 0 then begin
    WriteLn('Format : TTFVIEW  FILENAME.<TTF/FON/CHR>  options');
    WriteLn('/SVGA       - run with 640x480x256');
    WriteLn('/BMP XXXX   - load bmp-file to draw with');
    WriteLn('/LINE XXX   - write this line instead AaBbCc');
    WriteLn('/DIZ  XXX   - write font name into given file');
    WriteLn;
    WriteLn(#13#10'Special thanx to Matthias Koppe.');
    Exit;
  end;


  UseDMM := True;
  H := 50;
  BMPS := ''; LINE := '';
  Drv := GetAppropriateDriver(320, 200, 256, False);

  for I := 2 to ParamCount do begin
    D := StUpCase(ParamStr(I));
    if D = '/SVGA' then begin
      H := 100;
      Drv := GetAppropriateDriver(640, 480, 256, False);
      Continue;
    end;

    if D = '/BMP' then begin
      BMPS := ParamStr(I+1);
      if ParamCount < I+1 then IPErr;
      Inc(I);
      Continue;
    end;

    if D = '/LINE' then begin
      LINE := ParamStr(I+1);
      if ParamCount < I+1 then IPErr;
      Inc(I);
      Continue;
    end;

    if D = '/DIZ' then begin
      BMPS := ParamStr(I+1);
      if ParamCount < I+1 then IPErr;
      WriteDiz := True;
      Inc(I);
      Continue;
    end;


    WriteLn('(!) Unknown parameter - ', ParamStr(I));
    Halt(0);
  end;

  FSPlit(ParamStr(1), D, N, E);
  F := Nil;
  E := StUpCase(E);
  if E = '.TTF' then begin
    F := LoadTTFont(ParamStr(1), H, H+20);
    if PTTFont(SharedList^[F^.SrcHandle].P)^.Name = nil then
      FontName := ParamStr(1) else FontName := PTTFont(SharedList^[F^.SrcHandle].P)^.Name^;
  end else Fontname := ParamStr(1);
  if E = '.CHR' then F := LoadBGIFileFont(ParamStr(1), H, 1,1,1,1,0,0,0,0);
  if E = '.FON' then begin
    F := LoadWinFont(ParamStr(1), 1, 0, 0, 0, 0, 0, 0, FontName);
    if F <> Nil then begin
      B := 1;
      A := DefFont(F);
      Repeat
        FreeFont(A);
        Inc(B);
        F := LoadWinFont(ParamStr(1), B, 0, 0, 0, 0, 0, 0, FontName);
        A := DefFont(F);
      Until (F = Nil);
      F := LoadWinFont(ParamStr(1), B-1, 0, 0, 0, 0, 0, 0, FontName);
      A := DefFont(F);
    end;
  end;
  if F = Nil then begin
    WriteLn('(!) ', ParamStr(1), ' - not a valid font file.');
    Exit;
  end;
  A := DefFont(F);
  TryBufferedStrategy := False;

  if WriteDiz then begin
    Assign(DizF, BMPS);
    Rewrite(DizF);
    Write(DizF, FontName);
    Close(DizF);
    FreeFont(A);
    Exit;
  end;


  SetDIBDriver(im256);
  if BMPS <> '' then begin
    if not LoadBMP(BMPS, 601, @LP, im256) then begin
      WriteLn('(!) Not a bitmap file - ', BMPS);
      Halt(0);
    end;
    P := GetImage(601);
    if (PSImage(P)^.NBP and $FFF) <> 256 then P := CreateDIBitmap(P, cbwDelete);
  end;

  SelectDriver(Drv);

  if not InitGDI then begin
    FreeFont(A);
    Exit;
  end;
  GRT.LineStyle   := lsLinePattern;
  GRT.Operation := CopyPut;
  if BMPS <> '' then begin
    if LP.Colors > 0 then begin
      SetVGAPalette(0, LP.Colors, LP.Palette);
      Move(LP.Palette^, MainPalette, LP.Colors * 3);
      Bar(0, 0, MaximalX, MaximalY, RGB(0, 0, 0));
    end;
    GRT.LineStyle   := lsBitmapOrg;
    GRT.Bitmap      := P;
  end;
  GRT.ClipRect.Assign(0, 0, ScreenDriver^.MaximalX, ScreenDriver^.MaximalY);
  GRT.LinePattern := $FFFF;
  GRT.LineStyle   := lsLinePattern;
  GRT.Fore := RGB(255, 255, 255);
  MakeOut;
  F := LoadBIOSFont(16, 0, 0, 0);
  if F = Nil then F := LoadBIOSFont(14, 0, 0, 0);
  if F = Nil then F := LoadBIOSFont(8, 0, 0, 0);
  B := DefFont(F);
  SetTextParams(B, 0, 15, False);
  SetTextJustify(0,2);
  OutTextXY(H*3, H*3+22, FontName);
  I := 0;
  with GRT do begin
    LineStyle   := lsBitmapOrg;
    BitmapOrg.X := 0; BitmapOrg.Y := 0;
    Repeat
      Dec(BitmapOrg.X);
      Dec(BitmapOrg.Y);
      if BMPS <> '' then MakeOut;
      asm
        mov ax, 3
        int 33h
        xor bh, bh
        mov I, bx
      end;
    Until KeyPressed or (I <> 0);
  end;
  if I = 0 then ReadKey;
  DoneGDI;
  if BMPS <> '' then begin
    FreeDImage(P);
    DisposePalette(LP);
  end;
  FreeFont(A);
  WriteLn('DK Inc. 1996 TrueType fonts viewer.');
End.