uses Objects, GDI, Mono, Printers, Image, Bitmaps, EgFont, TrueType;

Var
  P  : PImage;
  LP : TLogPalette;
  PR : PIBMPrinter;

Begin
  WriteLn('  This sample will print some on printer. If you don''t have one, or');
  WriteLn('your printer is not Epson 9-pin compatible, press Ctrl+Break.');
  ReadLn;
  if not LoadBMP('block3.bmp', 666, @LP, imMono) then exit;
  RegisterFont(New(PScaledWindowsFont, Init('vgasys.fon')), 1);
  P := GetImage(666);
  Move(LP.Palette^, MainPalette, LP.Colors * 3);
  New(PR, Init(CreatePRNStream));

  if PR^.BeginDoc then with PR^ do begin
    Font.Font  := 1;
    Font.Width := 16;
    Font.Height := 20;
    Font.Style := ftItalic;
    SelectFontCaps(Font);
    PaintInfo.Fore := Black;
    PutImage(P, 0, 0, 0, 0, 640, 480, nil, PaintInfo);
    WrStr(10, PSImage(P)^.Y + 20, 'DK Inc. Grafix Visi0n!', PaintInfo);
    PR^.EndDoc;
  end;
  DisposeImage(666);
End.