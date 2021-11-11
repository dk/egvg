uses txt, gdi, egfont, truetype, objects;

var
  T : TPaintInfo;
  F : TFont;
  xf : File;



Begin
{  Assign(xF, 'dump');
  Reset(xf, 1);
  BlockRead(xF, Mem[SegB800:0], 4000);
  Close(xF); }
  FadeTxt;
  SelectDriver(GetAppropriateDriver(256, 128, 2, False));
  SetTextRect(0, 0, 32, 8);
  RegisterFont(LoadFont('academy.ttf'), 1);
  InitGDI;
{  SetPrimaryWidth(64);}
  SetTextRect(0, 0, 32, 8);
  DefaultPaint(T);
  DefaultFont(F);
  F.Width := 58;
  F.height := 92;
  F.Italic := 15;
  SelectFontCaps(F);
  WrStr(10, 10, 'Hello!', T);
  Line(0,0,600,100,T);
  PutBufferPart(0,0,0,0);
  asm
    xor ax, ax
    int 16h
  end;
  DoneGDI;
End.