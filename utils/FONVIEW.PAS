uses SVGA256, GDI, EgFont, TrueType;

var
  T : TPaintInfo;
  F : TFont;

  I, H : Integer;
  PF : PWindowsFont;

begin
  New(PF, Init(ParamStr(1)));
  if PF = Nil then Halt;
  RegisterFont(PF, 1);
  TryBufferedStrategy := false;
  SelectDriver(GetAppropriateDriver(320, 200, 256, True));
  InitGDI;
  DefaultPaint(T);
  DefaultFont(F);
  T.Fore := 15;
  H := 0;
  if PF^.CapsNumber > 0 then for I := 1 to PF^.CapsNumber do begin
    F.Height := PF^.CapsHeight(I);
    SelectFontCaps(F);
    WrStr(0, H, 'AaBbCcÄ†Å°Ç¢', T);
    Inc(H, F.Height);
  end;
  asm
    xor ax, ax
    int 16h
  end;
  DoneGDI;
end.