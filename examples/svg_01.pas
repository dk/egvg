{-cd -m}
{ $DEFINE SVGADEBUG}
{$DEFINE RUSSIAN}
{$DEFINE USESTANDARDBITMAPS}
Uses Objects, App, Msgbox, Menus, Drivers, Views, Dialogs, EGFont,
     svga256, GDI, Wallpapers, BGIFont, FntLib, AfterDrk, Bitmaps, Streams,
     TrueType, Image;
Type
  PFuckingStatic = ^TFuckingStatic;
  TFuckingStatic = Object(TView)
    Constructor Init;
    Procedure   Draw; Virtual;
  End;

  PJopa = ^TJopa;
  TJopa = Object(TView)
    Constructor Init;
    Procedure   Draw; Virtual;
  End;

  PJopa1 = ^TJopa1;
  TJopa1 = Object(TView)
    Constructor Init;
    Procedure   Draw; Virtual;
  End;

  PIdioticBackGround = ^TIdioticBackGround;
  TIdioticBackGround = Object(TBackGround)
    Constructor Init(var Bounds: TRect; Chr : Char);
    Procedure Draw; Virtual;
  End;

  PIdioticDesktop = ^TIdioticDesktop;
  TIdioticDesktop = Object(TDesktop)
    Procedure InitBackGround; Virtual;
  End;

  TApp = Object(TApplication)
    Constructor Init;
    Procedure InitScreenSaver; Virtual;
    Procedure InitDesktop; Virtual;
  End;


Procedure TIdioticDesktop.InitBackGround;
Var
  R : TRect;
Begin
  GetExtent(R);
  BackGround := New(PIdioticBackGround{PBackGround}, Init(R, #176));
End;

Constructor TIdioticBackGround.Init;
Begin
  Inherited Init(Bounds, Chr);
  SetPaint(0, 12, NotBlack, psDot, lwUltraHeavy, lsBitMap, fsNoUse);
  PaintInfo.BitMap := @WallPaperLeather;
End;

Procedure TIdioticBackGround.Draw;
Begin
  Inherited Draw;
  SetTextMetrics(14, 100, 250, faHorizontal, 10, fiNormal, 0);
  WrStr(60, 50, 'Hello!', PaintInfo.Fore);
End;

procedure TApp.InitScreenSaver;
var R: TRect;
begin
  Lock;
  R.Copy(AllScreen);
  ScreenSaver := PView(New(PFlyingBallsScreenSaver, Init(1000)));
end;


Constructor TApp.Init;
Var
  P : PDialog;
  R : TRect;
  Control : PView;
  Fnt : PSimpleFont;
  F: PAbstractFont;
  LP : TLogPalette;
Begin
  {WallPaper := @WallPaperDoom;}
  useDMM := True; systemColors := 32;
  if LoadBMP('ATOLL.BMP', 666, @LP, 0) then begin
    WallPaper := GetImage(666);
    WallPaperPalette := LP.Palette;
    if (PSImage(WallPaper)^.X < Round(ScreenDriver^.ScreenWidth / 1.5)) and
       (PSImage(WallPaper)^.Y < Round(ScreenDriver^.ScreenHeight / 1.5)) then
      WallPaperFlags := wpfStretch + wpfSingle + wpfPicture;
  end;
  Inherited Init;
  New(fnt, Init(@Small^, 'Small'));
  RegisterFont(fnt, 2);
  New(fnt, Init(@Courier^, 'Courier'));
  RegisterFont(fnt, 3);
  New(PScaledWindowsFont(fnt), Init('vgasys.fon'));
  if fnt <> nil then RegisterFont(fnt, 1);
  New(PBIOSFont(fnt), Init);
  RegisterFont(fnt, 4);
  F := New(PScaledWindowsFont, Init('sserife.fon'));
  RegisterFont(F, 14);
  SelectFont(GlobalFont.Font);
  R.Assign(0, 0, 50 * GetCharWidth, 11 * 16);
  P := New(PDialog, Init(R, 'DK Inc. 1996  EG to SVG conversion'));
  P^.Options := P^.Options or ofCentered;
  P^.Frame^.Font.GapLength := 2;
  P^.Frame^.Font.Italic    := 10;


  Inc(P^.Flags, wfGrow + wfZoom);
  R.Assign(0, 0, 10 * GetCharWidth, 2 * GetHeight);
  R.Move(20 * GetCharWidth, 8 * GetHeight);
  Control := MakeOKButton(R, cmQuit, bfDefault);
{  Control^.SetState(sfDisabled, True);
  Control^.DisableCommands([1]);}
  P^.Insert(Control);
  R.Assign(0, 0, 15 * GetCharWidth, 2 * GetHeight);
  R.Move(20 * GetCharWidth, 2 * GetHeight);
  P^.Insert(New(PStaticText, Init(R, 'Hello, I see lot of colors!')));
  P^.Insert(New(PFuckingStatic, Init));
{  T.ExecuteDialog(P, Nil);}
  Desktop^.Insert(P);
  R.Assign(0, 0, 40 * GetCharWidth, 11 * GetHeight);
  R.Move(25 * GetCharWidth, 4 * GetHeight);
  P := New(PDialog, Init(R, 'Мрачные отрисовки'));
  R.Assign(0, 0, 6 * GetCharWidth, 1 * GetHeight);
  R.Move(25 * GetCharWidth, 7 * GetHeight);
  Control := New(PInputLine, Init(R, 5));
  P^.Insert(Control);
  R.Assign(0, 0, 1 * GetCharWidth, 1 * GetHeight);
  R.Move(32 * GetCharWidth, 7 * GetHeight);
  P^.Insert(New(PHistory, Init(R, PInputLine(Control), 1)));

  R.Assign(0, 0, 6 * GetCharWidth, 2 * GetHeight);
  R.Move(5 * GetCharWidth, 8 * GetHeight);
  Control := New(PCheckBoxes, Init(R,
                  NewSItem('Yes',
                  NewSItem('No',
                  nil))));
  P^.Insert(Control);
  R.Assign(0, 0, 6 * GetCharWidth, 2 * GetHeight);
  R.Move(15 * GetCharWidth, 8 * GetHeight);
  Control := New(PRadioButtons, Init(R,
                  NewSItem('Yes',
                  NewSItem('No',
                  nil))));
  P^.Insert(Control);
  P^.Insert(New(PJopa, Init));
  P^.Insert(New(PJopa1, Init));
  {P^.Palette := dpBlueDialog;}
  Desktop^.Insert(P);
  Desktop^.SelectNext(False);
  UnLock;
End;

Procedure TApp.InitDesktop;
var
  R, R1: TRect;
begin
  GetExtent(R);
  StandardMenuBarRect(R1);
  R.A.Y := R1.B.Y;
  StandardStatusRect(R1);
  R.B.Y := R1.A.Y;
  Desktop := New(PIdioticDesktop, Init(R));
End;

Constructor TFuckingStatic.Init;
Var
  R : TRect;
Begin
  R.Assign(8, 28, 136, 156);
  Inherited Init(R);
End;

Procedure TFuckingStatic.Draw;
Var
  I, J : Byte;
Begin
  for I := 0 to 15 do
    for J := 0 to 15 do
      Bar(J * 8, I * 8, (J + 1) * 8, (I + 1) * 8, I * 16 + J);
End;

Constructor TJopa.Init;
Var
  R : TRect;
Begin
  R.Assign(8, 28, 136, 106);
  Inherited Init(R);
End;

Procedure TJopa.Draw;
Var
  I:Byte;
Const
  Triangle: array[1..5] of TPoint = ((X: 100; Y: 30),
                          (X: 85; Y: 60), (X: 35; Y: 75),
                          (X: 160; Y: 50), (X: 120; Y: 40));
Begin
  Bar(0, 0, Size.X, Size.Y, 7);
  SetPaint(12, 10, CopyPut, psDash, lwSemiBold, lsPattern, fsInterleave);
  Line(0, 50, 50, 0);
  Line(10, 0, 50, 50);
  SetPaint(12, 10, CopyPut, psDash, lwThin, lsLinePattern, fsNoUse);
  Line(0, 0, 50, 10);
  SetPaint(13, 0, NotBlack, psDot, lwThin, lsLinePattern, fsNoUse);
  Ellipse(90, 20, 25, 13);
  SetPaint(2, 0, NotBlack, psNull, lwThin, lsPattern, fsSlash);
  BarStyle(Size.X div 4, Size.Y div 4, Size.X div 2, Size.Y div 2);
  SetPaint(12, 4, CopyPut, psNull, lwThin, lsBitMapOrg, fsParquet);
  PaintInfo.BitMap := @ScrollDownDef;
  FillPoly(5, Triangle);
  SetPaint(30, 1, CopyPut, psNull, lwSemiBold, lsPattern, fsBkSlash);
  for I := 0 to 15 do begin
    PaintInfo.Fore := ColorIndex^[16+I];
    PaintInfo.Back := ColorIndex^[31-I];
    FillCircle(50, 50, 18 - i);
  end;
End;

Constructor TJopa1.Init;
Var
  R : TRect;
Begin
  R.Assign(138, 28, 266, 106);
  Inherited Init(R);
  {PaintInfo.BitMap := CreateDIBitmap(@Mark_RedArm, 0);}
End;

Procedure TJopa1.Draw;
Var
  I : Word;
Begin
  Bar(0, 0, Size.X, Size.Y, 9);

  SetPaint(30, 0, XORPut, psSolid, lwNormal, lsLinePattern, fsSolid);
  for I := 1 to 16 do begin
    PaintInfo.Fore := ColorIndex^[32-I];
    FillCircle(58, 50, 4 * i);
  end;

  SetPaint(16, 0, CopyPut, psSolid, lwThin, lsLinePattern, fsNoUse);
  {SetTextMetrics(4, 100, 100, 340, 0, 0, 0);}
  SetTextMetrics(14, 16, 82, 340, 0, 0, 0);
  WrStr(1, 1, 'So what?', 0);
  Font.Font := GlobalFont.Font;

  SetPaint(30, 0, XORPut, psSolid, lwNormal, lsLinePattern, fsSolid);
  for I := 1 to 16 do begin
    PaintInfo.Fore := ColorIndex^[32-I];
    FillCircle(58, 50, 4 * i);
  end;

  SetPaint(30, 0, CopyPut, psSolid, lwHollow, lsBitMap, fsSolid);
  PaintInfo.Bitmap   := WallPaper;
  PaintInfo.ColorRef := Desktop^.BackGround^.LogPalette.ColorRef;
  FillCircle(23, 62, 23);
End;


Var
  T : TApp;
Begin
  SelectDriver(GetAppropriateDriver(640,480,256,True));
  T.Init;
  T.Run;
  T.Done;
End.