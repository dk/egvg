{-cp -m}
{ $DEFINE SVGADEBUG}
{$DEFINE RUSSIAN}
{$DEFINE USESTANDARDBITMAPS}
Uses Objects, App, Msgbox, Menus, Drivers, Views, Dialogs, EGFont,
     svga64k, GDI, Wallpapers, BGIFont, FntLib, AfterDrk, Bitmaps,
     Streams, Image, TrueType;
Type
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


Constructor TIdioticBackGround.Init;
Begin
  Inherited Init(Bounds, Chr);
  LogPalette.Mode := pmUseRGB or pmHiColor;
End;

Procedure TIdioticBackGround.Draw;
Var
  B, Y, Dif, Adc : Integer;
Begin
  Dif := Size.Y div 128;
  Adc := Size.Y mod 128;
  Bar(0, Size.Y - Adc, Size.X, Size.Y, RGB(0, 0, 0));
  B := 255;
  Y := 0;
  while B >= 0 do begin
    SolidBrush(0, 0, B);
    BarStyle(0, Y, Size.X, Y + Dif);
    Dec(B, 2);
    Inc(Y, Dif);
  end;
End;

Procedure TIdioticDesktop.InitBackGround;
Var
  R : TRect;
Begin
  GetExtent(R);
  BackGround := New(PIdioticBackGround, Init(R, #176));
End;


procedure TApp.InitScreenSaver;
var R: TRect;
begin
  Lock;
  R.Copy(AllScreen);
  ScreenSaver := PView(New(PFlameScreenSaver, Init(1000)));
end;


Constructor TApp.Init;
Var
  P : PDialog;
  R : TRect;
  Control : PView;
Begin
  Inherited Init;
  RegisterFont(New(PWindowsFont, Init('sserife.fon')), 1);
  R.Assign(0, 0, 50 * GetCharWidth, 8 * GetHeight);
  P := New(PDialog, Init(R, 'DK Inc. 1996  EG to SVG conversion'));
  P^.Options := P^.Options or ofCentered;

  Inc(P^.Flags, wfGrow + wfZoom);
  R.Assign(0, 0, 10 * GetCharWidth, 2 * GetHeight);
  R.Move(15 * GetCharWidth, 4 * GetHeight);
  Control := MakeOKButton(R, cmQuit, bfDefault);
  P^.Insert(Control);
  R.Assign(0, 0, 30 * GetCharWidth, 2 * GetHeight);
  R.Move(2 * GetCharWidth, 2 * GetHeight);
  P^.Insert(New(PStaticText, Init(R, 'Looks like a Windoze setup.....')));
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


Var
  T : TApp;
Begin
  T.Init;
  T.Run;
  T.Done;
End.