{-cp}
Uses Objects, App, EGFont, FntLib, WStdDlg, Dialogs, svga256, GDI, TrueType,
     Views, Drivers, WallPapers;

Var
  T : TApplication;
  Fnt : PSimpleFont;
  P : TPaintInfo;
  F : TFont;

Begin
  TryBufferedStrategy := False;
  WallPaper := @WallPaperDoom;
  WallPaperPalette := @StdVGAPalette;
  T.Init;
  New(PScaledWindowsFont(fnt), Init('c:\windows\system\vgasys.fon'));
  RegisterFont(fnt, 1);
  New(PScaledSimpleFont(fnt), Init(@Small^, 'Small'));
  RegisterFont(fnt, 2);
  New(PScaledSimpleFont(fnt), Init(@Courier^, 'Courier'));
  RegisterFont(fnt, 3);
  New(PBIOSFont(fnt), Init);
  RegisterFont(fnt, 4);
  New(PTrueTypeFont(fnt), Init('c:\windows\system\timcyr.ttf'));
  RegisterFont(fnt, 7);
  New(PWindowsFont(fnt), Init('sserife.fon'));
  RegisterFont(fnt, 15);
  New(PWindowsFont(fnt), Init('helvecyr.fon'));
  RegisterFont(fnt, 16);
  T.Redraw;
  DefaultPaint(P);
  DefaultFont(F);
  Desktop^.Insert(New(PColorDialog, Init));
  Desktop^.Insert(New(PFontDialog, Init));
  T.Run;
  T.Done;
End.