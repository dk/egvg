Unit GV2SVG;

Interface

Uses GrvTypes, Objects, VApp, Views, Dialogs, Menus, EgFont, SVGA256, GDI;

Type
  TGVApplication = Object(TVApplication)
    Constructor Init(xMode : Word);
  End;
  PApplication = ^TGVApplication;
  TApplication = TGVApplication;

  TGVWindow = Object(TWindow)
    Procedure GetExtentWin(var R : TRect);
  End;
  PWindow = ^TGVWindow;
  TWindow = TGVWindow;

  TGVDialog = Object(TDialog)
    Procedure GetExtentWin(var R : TRect);
  End;
  TDialog = TGVDialog;
  PDialog = ^TGVDialog;

  TGVView = Object(TView)
    {$I VIDEF.INC}
  End;
  TView = TGVView;
  PView = ^TGVView;

  TGVScroller = Object(TScroller)
    {$I VIDEF.INC}
  End;
  TScroller = TGVScroller;
  PScroller = ^TGVScroller;

  PStylePane = ^TStylePane;
  TStylePane = Object(TShade)
    Constructor Init(R : TRect; Fill : TFillPattern; AColor:Byte);
  End;

  PStaticText = ^TStaticText;
  TStaticText = Object(Dialogs.TStaticText)
    Constructor Init(R:Trect; S:String; Dummy:Byte);
  End;


Const
  miDisabled = $01;
  miChecked  = $02;
  miBitMap   = $04;
  miVerLine  = $08;
  miHorLine  = $10;

const
{Text Format masks}
  tfCenterX    = $0001;
  tfCenterY    = $0002;
  tfLeft       = $0004;
  tfRight      = $0008;
  tfTop        = $0010;
  tfBottom     = $0020;
  tfBackGround = $0040;



Function NewLine(x : Byte; Next : PMenuItem) : PMenuItem;

Implementation

Const
  IdioticColorMap : array[0..15] of Byte = (
    0, 4, 2, 6, 1, 5, 3, 8, 7, 12, 10, 14, 9, 13, 11, 15
  );




Constructor TGVApplication.Init;
Var
  I : Integer;
Begin
  smHMenu := MenusFont.Height + 8;
  Inherited Init;
  if MaxColors > 16 then for I := 0 to 15 do
    if IdioticColorMap[i] <> 5 then
    SetVGARegister(IdioticColorMap[i], PVGARegister(@CurrentPalette.Colors[I])^);
End;

Procedure TGVWindow.GetExtentWin;
Begin
  GetClientExtent(R);
End;

Procedure TGVDialog.GetExtentWin;
Begin
  GetClientExtent(R);
End;

Constructor TStylePane.Init(R : TRect; Fill : TFillPattern; AColor:Byte);
Var
  F,B : Word;
Begin
  F := IdioticColorMap[Color and $f];
  B := IdioticColorMap[Color and $f0 shr 4];
  Inherited Init(R, (B shl 8) + F, $0708, shBarPattern);
  paintInfo.Pattern := Fill;
End;


Procedure _SetColor(P : PView; Color : Byte);
Begin
  P^.PaintInfo.Fore := IdioticColorMap[Color and $f];
  if (P^.LogPalette.Mode and pmHiColor) = 0 then
    P^.PaintInfo.Fore := ColorIndex^[P^.PaintInfo.Fore];
End;

Function _ChStrLen(P : PFont; S : String; From : Integer) : Integer;
Var
  X, I : Integer;
  F    : PAbstractFont;
Begin
  X := 0;
  F := GetFont(P^.Font);
  if Byte(S[0]) > 0 then for I := 1 to Byte(S[0]) do begin
    if From > 0 then begin
      Dec(From, F^.GetCharWidth(S[I]));
      Inc(X);
    end else Break;
  end;
  _ChStrLen := X;
End;

Constructor TStaticText.Init(R:Trect; S:String; Dummy:Byte);
Begin
  Inherited Init(R, S);
End;

{TView}
Procedure TGVView.SetColor(IdioticColor : Byte); Begin  _SetColor(@Self, IdioticColor); End;
Procedure TGVView.FillRect(x1, y1, x2, y2 : Integer); Begin  Bar(x1, y1, x2, y2, PaintInfo.Fore); End;
Procedure TGVView.WriteStr(x, y : Integer; S : String); Begin  WrStr(x, y, s, PaintInfo.Fore); End;
Procedure TGVView.PlotVLine(x, y1, y2 : Integer); Begin VLine(x, y1, y2, PaintInfo.Fore); End;
Procedure TGVView.PlotLine(x1, x2, y1, y2 : Integer); Begin Line(x1, x2, y1, y2); End;
Function  TGVView._Font : PFont; Begin  _Font := @Font; End;
Function  TGVView.ChStrLen; Begin ChStrLen := _ChStrLen(@Font, S, From) End;
{TScroller}
Procedure TGVScroller.SetColor(IdioticColor : Byte); Begin  _SetColor(@Self, IdioticColor); End;
Procedure TGVScroller.FillRect(x1, y1, x2, y2 : Integer); Begin  Bar(x1, y1, x2, y2, PaintInfo.Fore); End;
Procedure TGVScroller.WriteStr(x, y : Integer; S : String); Begin  WrStr(x, y, s, PaintInfo.Fore); End;
Procedure TGVScroller.PlotVLine(x, y1, y2 : Integer); Begin VLine(x, y1, y2, PaintInfo.Fore); End;
Procedure TGVScroller.PlotLine(x1, x2, y1, y2 : Integer); Begin Line(x1, x2, y1, y2); End;
Function  TGVScroller._Font : PFont; Begin  _Font := @Font; End;
Function  TGVScroller.ChStrLen; Begin ChStrLen := _ChStrLen(@Font, S, From) End;


Function NewLine(x : Byte; Next : PMenuItem) : PMenuItem;
Begin
  NewLine := Menus.NewLine(Next);
End;

End.