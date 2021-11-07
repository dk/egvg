{-cd -ue:\bp\eg\256\source}
uses gdi, svga256;

Const
  LightX = 150;
  Lighty = 150;
  Lightz = 50;

Var
  T : TPaintInfo;

procedure DrawSphere(cx, cy: Integer; r: integer);
var
  lx, ly, lz, l, s: real;
  I, J, X, Y, Color: Integer;
  y1, y2: real;
begin
  lx := LightX - cx; ly := LightY - cy; lz := LightZ;
  l := sqrt(lx * lx + ly * ly + lz * lz);
  lx := lx / l; ly := ly / l; lz := lz / l;
  For Y := -R to R do begin
    y1 := r*r-y*y;
    y2 := y*ly;
    For X := -R to R do begin
      s := y1 -x*x;
      if s >=0 then
      begin
        t.fore := Round(64*(x*lx+y2+sqrt(s)*lz)/R);
        if (t.fore >= 0) and (t.fore <= 64) then PutPixel(Y+cy, X+cx, t);
      end;
    end;
  end;
end;

var
  I : Byte;

begin
  TryBufferedStrategy := False;
  InitGDI;
  DefaultPaint(T);
  for I := 1 to 255 do begin
    MainPalette[I, 1] := I + 30;
    MainPalette[I, 2] := I + 60;
    MainPalette[I, 3] := I + 60;
  end;
  SetVGAPalette(0, 256, @MainPalette);
  DrawSphere(200, 200, 100);
  asm
    xor ax, ax
    int 16h
  end;
  DoneGDI;
end.