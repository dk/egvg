uses objects, gdi, svga256, svga64k;

var
  t:tpaintinfo;
  i,j:byte;

const
  cell = 5;



Begin
  SelectDriver(GetAppropriateDriver(320, 200, 256, True));
  TryBufferedStrategy := False;
  InitGDI;
  DefaultPaint(T);
  T.Fore := 15;
  T.Back := 0;
  T.LineStyle := lsPattern;
  if MaxColors = 256 then begin
    for i := 0 to 63 do begin
      MakeDither(0, 0, i*4, 16, t, @MainPalette);
      BarStyle(i * cell, 0, (i + 1) * cell, 199, t);
    end;
  end else begin
    for i := 0 to 255 do begin
      MakehiDither(0, 0, i, t);
      BarStyle(i, 0, (i + 1), 299, t);
    end;
  end;
  asm
    xor ax, ax
    int 16h
  end;
  DoneGDI;
End.