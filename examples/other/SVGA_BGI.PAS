{Substitutes the BGI procs}
Unit SVGA_BGI;

Interface

Uses GDI;

Const
  TopOn     =  True;
  TopOff    =  False;
  ClipOn   = True;
  ClipOff  = False;

  Detect        =    0;
  CGA           =    1;
  MCGA          =    2;
  EGA           =    3;
  EGA64         =    4;
  EGAMono       =    5;
  IBM8514       =    6;
  HercMono      =    7;
  ATT400        =    8;
  VGA           =    9;
  PC3270        =   10;

  grOk = 0;
  grError = 1;
  grFileNotFound = 1;

  SolidLn    =0;
  DottedLn   =1;
  CenterLn   =2;
  DashedLn   =3;
  UserBitLn  =4;
  NormWidth  =1;
  ThickWidth =3;

  DefaultFont   =   0;
  TriplexFont   =   1;
  SmallFont     =   2;
  SansSerifFont =   3;
  GothicFont    =   4;
  HorizDir      =   0;
  VertDir       =   270;
  UserCharSize  =   0;

  LeftText   =   0;
  CenterText =   1;
  RightText  =   2;
  BottomText =   0;
  TopText    =   2;

  EmptyFill      =    0;
  SolidFill      =    1;
  LineFill       =    2;
  LtSlashFill    =    3;
  SlashFill      =    4;
  BkSlashFill    =    5;
  LtBkSlashFill  =    6;
  HatchFill      =    7;
  XHatchFill     =    8;
  InterleaveFill =    9;
  WideDotFill    =   10;
  CloseDotFill   =   11;
  UserFill       =   12;

  NormalPut =   0;
  CopyPut   =   0;
  XORPut    =   1;
  OrPut     =   2;
  AndPut    =   3;
  NotPut    =   4;

  LastDriverNum = 0;

Type
  ViewPortType = record
    x1, y1, x2, y2 : integer;
    Clip           : Boolean;
  end;
  LineSettingsType = record
    LineStyle : Word;
    Pattern   : Word;
    Thickness : Word;
  end;
  FillSettingsType = record
    Pattern : Word;
    Color   : Word;
  end;
  TextSettingsType = record
    Font      : Word;
    Direction : Word;
    CharSize  : Word;
    Horiz     : Word;
    Vert      : Word;
  end;
  FillPatternType = array [1..8] of Byte;
  PaletteType = record
    Size    : Byte;
    Colors  : array[0..255] of Shortint;
  end;
  PointType = record
    X, Y : integer;
  end;
  ArcCoordsType = record
    X, Y,
    Xstart, Ystart,
    Xend, Yend : integer;
  end;

procedure InitGraph(var GraphDriver:Integer; GraphMode: Integer; PathToDriver: string);
procedure CloseGraph;
procedure RestoreCrtMode;
procedure SetGraphMode(Mode: Integer);
function InstallUserDriver(Name: string; AutoDetectPtr: pointer): integer;

procedure SetColor(Color: Word);
procedure SetLineStyle(LineStyle: Word; Pattern: Word; Thickness: Word);
procedure SetViewPort(x1, y1, x2, y2: Integer; Clip: Boolean);
procedure SetTextStyle(Font, Direction: Word; CharSize: Word);
procedure SetTextJustify(Horiz, Vert: Word);
procedure SetFillStyle(Pattern: Word; Color: Word);
procedure SetWriteMode(WriteMode: Integer);
function SetAspectRatio(Xasp, Yasp: Word): Word;
procedure SetUserCharSize(MultX, DivX, MultY, DivY: Word);
procedure SetFillPattern(Pattern: FillPatternType; Color: Word);
procedure SetPalette(ColorNum: Word; Color: Shortint);
procedure SetAllPalette(var Palette);

function GetDriverName: string;
function GetGraphMode: Integer;
function GetModeName(ModeNumber: Integer): string;
procedure GetViewSettings(var ViewPort: ViewPortType);
function GraphResult: Integer;
function GraphErrorMsg(ErrorCode: Integer): string;
function GetMaxX: Integer;
function GetMaxY: Integer;
function TextHeight(TextString: string): Word;
function TextWidth(TextString: string): Word;
procedure GetLineSettings(var LineInfo: LineSettingsType);
procedure GetFillSettings(var FillInfo: FillSettingsType);
procedure GetTextSettings(var TextInfo: TextSettingsType);
procedure GetPalette(var Palette: PaletteType);
function GetX: Integer;
function GetY: Integer;
function GetMaxColor: Word;
function GetColor: Word;
procedure GetAspectRatio(var Xasp, Yasp: Word);
procedure GetArcCoords(var ArcCoords: ArcCoordsType);
function ImageSize(x1, y1, x2, y2: Integer): Word;

Procedure ClearDevice;
procedure Rectangle(x1, y1, x2, y2: Integer);
procedure OutTextXY(X,Y: Integer; TextString: string);
procedure OutText(TextString: string);
procedure Bar(x1, y1, x2, y2: Integer);
procedure FillEllipse(X, Y: Integer; XRadius, YRadius: Word);
procedure Sector(x, y: Integer; StAngle,EndAngle, XRadius, YRadius: Word);
procedure Line(x1, y1, x2, y2: Integer);
procedure Circle(X,Y: Integer; Radius: Word);
procedure MoveTo(X, Y: Integer);
procedure LineTo(X, Y: Integer);
procedure MoveRel(Dx, Dy: Integer);
procedure LineRel(Dx, Dy: Integer);
procedure FillPoly(NumPoints: Word; var PolyPoints);
procedure FloodFill(X, Y: Integer; Border: Word);
procedure PieSlice(X, Y: Integer; StAngle, EndAngle, Radius: Word);
procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Word; Top: Boolean);
procedure Arc (X,Y: Integer; StAngle, EndAngle, Radius: Word);
procedure PutPixel(X, Y: Integer; Pixel: Word);
function GetPixel(X,Y: Integer): Word;
procedure Ellipse(X, Y: Integer; StAngle, EndAngle: Word; XRadius, YRadius:Word);
procedure GetImage(x1, y1, x2, y2: Integer; var BitMap);
procedure PutImage(X, Y: Integer; var BitMap; BitBlt: Word);

Implementation

Uses Objects, EGFont, BGIFont;

Var
  T : TPaintInfo;
  F : TFont;
  Font : PAbstractFont;
  J : Record
    H, V : Word;
  End;
  _LineStyle, _Pattern : Word;
  _Palette  : PaletteType;
  _Overscans : longInt;
  _X, _Y : Word;
  _ArcCoords : ArcCoordsType;

procedure InitGraph(var GraphDriver:Integer; GraphMode: Integer; PathToDriver: string);
Begin
  TryBufferedStrategy := False;
  InitGDI;
  T.ClipRect.Assign(0, 0, GetMaxX, GetMaxY);
End;

procedure CloseGraph;
Begin
  DoneGDI;
End;

function GraphResult: Integer;
Begin
  GraphResult := 0;
End;

function GraphErrorMsg(ErrorCode: Integer): string;
Begin
  GraphErrorMsg := '';
End;

function GetMaxX: Integer;
Begin
  GetMaxX := ScreenDriver^.MaximalX;
End;

function GetMaxY: Integer;
Begin
  GetMaxY := ScreenDriver^.MaximalY;
End;

procedure SetColor(Color: Word);
Begin
  T.Fore := Color;
End;

procedure SetLineStyle(LineStyle: Word; Pattern: Word; Thickness: Word);
Begin
  case LineStyle of
  SolidLn: T.LinePattern := psSolid;
  DottedLn: T.LinePattern := psDot;
  CenterLn: T.LinePattern := psDashDot;
  DashedLn: T.LinePattern := psDash;
  UserBitLn:T.LinePattern := Pattern;
  else end;
  _LineStyle := LineStyle;
  T.LineWidth := ThickNess;
End;

procedure GetViewSettings(var ViewPort: ViewPortType);
Begin
  Move(T.ClipRect, ViewPort, 8);
  Dec(ViewPort.X2);
  Dec(ViewPort.Y2);
  ViewPort.Clip := True;
End;

Function xx(var x : Integer) : Integer;
Begin
  inc(x, T.ClipRect.A.X);
  xx := x;
End;

Function yy(var y : Integer) : Integer;
Begin
  inc(y, T.ClipRect.A.y);
  yy := y;
End;

Procedure ClearDevice;
Begin
  GDI.Bar(0, 0, Getmaxx, getmaxy, 0);
  _X :=0; _Y := 0;
  T.Fore := 15;
  SetTextJustify(0, 0);
End;

procedure SetViewPort(x1, y1, x2, y2: Integer; Clip: Boolean);
Begin
  T.ClipRect.Assign(x1, y1, x2-1, y2-1);
  SetTextJustify(0, 0);
End;

Function GetFontName(Font : Byte) : String;
Var
  S : String[12];
Begin
    Dec(Font);
    case Font of
    1 : S := 'TRIP';
    2 : S := 'LITT';
    3 : S := 'SANS';
    4 : S := 'GOTH';
    5 : S := 'SCRI';
    6 : S := 'SIMP';
    7 : S := 'TSCR';
    8 : S := 'LCOM';
    9 : S := 'EURO';
    else S := ''; end;
    S := S + '.CHR';
    GetFontName := S;
End;

procedure SetTextStyle(Font, Direction: Word; CharSize: Word);
Var
  B : PBGIFont;
Begin
  Inc(Font);
  if Font = F.Font then Exit;
  if F.Font <> DefaultFont then RegisterFont(Nil, F.Font);
  if Font <> DefaultFont then begin
    B := New(PBGIFont, Init(GetFontName(Font)));
    if B <> Nil then RegisterFont(B, Font) else Font := 1;
  end;
  SelectFont(Font);
  F.Width  := 8 * CharSize;
  F.Height := 14 * CharSize;
  F.Font := Font;
  F.Escapement := Direction;
End;

procedure Rectangle(x1, y1, x2, y2: Integer);
Begin
  xx(x1); xx(x2); yy(y1); yy(y2);
  GDI.Rectangle(x1, y1, x2, y2, T);
End;

procedure SetTextJustify(Horiz, Vert: Word);
Begin
  J.H := Horiz;
  J.V := Vert;
End;

procedure OutTextXY(X,Y: Integer; TextString: string);
Var
  W, H : Word;
Begin
  {xx(x); yy(y);}
  W := GetWidth(TextString);
  H := GetHeight;
  case J.H of
  CenterText: begin
    x := (T.ClipRect.B.x - T.ClipRect.A.x - w) div 2;
    xx(x);
  end;
  RightText : X := T.ClipRect.B.X - W;
  else end;
  case J.V of
  CenterText: begin
    Y := (T.ClipRect.B.Y - T.ClipRect.A.Y - H) div 2;
    yy(y);
  end;
  BottomText : Y := T.ClipRect.B.Y - Y;
  else end;
  WrStr(x, y, TextString, T);
  Inc(X, W);
  if X > T.ClipRect.B.X then begin
    X := 0;
    Inc(Y, H);
  end;
End;

procedure OutText(TextString: string);
Begin
  OutTextXY(_X, _Y, TextString);
End;

function TextHeight(TextString: string): Word;
Begin
  TextHeight := GetHeight;
End;

function TextWidth(TextString: string): Word;
Begin
  TextWidth := GetWidth(TextString);
End;

procedure SetUserCharSize(MultX, DivX, MultY, DivY: Word);
Begin
  if DivX > 0 then F.Width  := 8 * MultX div DivX;
  if DivY > 0 then F.Height := 14 * Multy div Divy;
End;

procedure SetFillStyle(Pattern: Word; Color: Word);
Begin
  if Pattern >= UserFill then Exit;
  Move(FillPatterns[Pattern], T.Pattern, 8);
  _Pattern := Pattern;
  {T.Fore := Color;}
End;

Procedure Redirect(var x1, y1, x2, y2: Integer);
Begin
  xx(x1); xx(x2); yy(y1); yy(y2);
  if x1 < T.ClipRect.A.X then x1 := T.ClipRect.A.X;
  if y1 < T.ClipRect.A.y then y1 := T.ClipRect.A.y;
  if x2 > T.ClipRect.b.X then x2 := T.ClipRect.b.X;
  if y2 > T.ClipRect.b.y then y2 := T.ClipRect.b.y;
End;

procedure Bar(x1, y1, x2, y2: Integer);
Var
  OL : Word;
Begin
  OL := T.LineStyle;
  T.LineStyle := lsPattern;
  Redirect(x1, y1, x2, y2);
  GDI.BarStyle(x1, y1, x2, y2, T);
  T.LineStyle := OL;
End;

function GetDriverName: string;
Begin
  GetDriverName := 'DK Inc. SVGA driver';
End;

function GetGraphMode: Integer;
Begin
  GetGraphMode := ScreenDriver^.ScreenWidth;
End;

function GetModeName(ModeNumber: Integer): string;
Begin
  case ModeNumber of
  320: getModeName := 'VGA, 320x200 256';
  400: getModeName := 'SVGA, 640x400 256';
  480: getModeName := 'SVGA, 640x480 256';
  600: getModeName := 'SVGA, 800x600 256';
  768: getModeName := 'SVGA, 1024x768 256';
  else getModeName := ''; end;
End;

procedure GetLineSettings(var LineInfo: LineSettingsType);
Begin
  LineInfo.LineStyle := _LineStyle;
  LineInfo.Pattern   := T.LinePattern;
  LineInfo.ThickNess := T.LineWidth;
End;

procedure GetFillSettings(var FillInfo: FillSettingsType);
Begin
  FillInfo.Pattern := _Pattern;
  FillInfo.Color   := T.Fore;
End;

procedure GetTextSettings(var TextInfo: TextSettingsType);
Begin
  TextInfo.Font := F.Font - 1;
  TextInfo.Direction := F.Escapement;
  TextInfo.CharSize  := F.Width shr 3;
  TextInfo.Horiz     := J.H;
  TextInfo.Vert      := J.V;
End;

procedure GetPalette(var Palette: PaletteType);
Begin
  Palette := _Palette;
End;

function GetX: Integer;
Begin
  GetX := _X;
End;

function GetY: Integer;
Begin
  GetY := _Y;
End;

function GetMaxColor: Word;
Begin
  GetMaxColor := 15;
End;

function GetColor: Word;
Begin
  GetColor := T.Fore;
End;

procedure FillEllipse(X, Y: Integer; XRadius, YRadius: Word);
Var
  OL, OP : Word;
Begin
  xx(x); yy(y);
  OL := T.LineStyle;
  OP := T.LineWidth;
  T.LineStyle := lsPattern;
  T.LineWidth := 0;
  GDI.FillEllipse(x, y, xRadius, yRadius, T);
  T.LineStyle := OL;
  T.LineWidth := OP;
End;


procedure Sector(x, y: Integer; StAngle,EndAngle, XRadius, YRadius: Word);
Begin
End;

procedure Line(x1, y1, x2, y2: Integer);
Begin
  xx(x1); yy(y1); xx(x2); yy(y2);
  GDI.Line(x1, y1, x2, y2, T);
End;

procedure SetWriteMode(WriteMode: Integer);
Begin
  if WriteMode < 21 then T.Operation := WriteMode;
End;

procedure Circle(X,Y: Integer; Radius: Word);
Begin
  GDI.Circle(xx(x), yy(y), Radius, T);
End;

procedure GetAspectRatio(var Xasp, Yasp: Word);
Begin
  xasp := 10000;
  yasp := 10000;
End;

function SetAspectRatio(Xasp, Yasp: Word): Word;
Begin End;

procedure MoveTo(X, Y: Integer);
Begin
  _X := xx(X);
  _y := yy(y);
End;

procedure LineTo(X, Y: Integer);
Begin
  GDI.Line(_X, _Y, xx(x), yy(y), T);
  MoveTo(x, y);
End;

procedure MoveRel(Dx, Dy: Integer);
Begin
  Inc(_X, dx);
  Inc(_y, dy);
End;

procedure LineRel(Dx, Dy: Integer);
Begin
  GDI.Line(_X, _Y, _X + dx, _Y + dy, T);
  MoveRel(dx, dy);
End;

procedure SetFillPattern(Pattern: FillPatternType; Color: Word);
Begin
  Move(Pattern, T.Pattern, 8);
  T.Fore := Color;
End;

procedure FillPoly(NumPoints: Word; var PolyPoints);
Var
  P : TPoint;
Begin
  P := T.ClipRect.A;
  GDI.FillPoly(NumPoints, PolyPoints, P, T);
End;

procedure FloodFill(X, Y: Integer; Border: Word);
Begin
  GDI.FloodFill(xx(x),xx(y),Border,T);
End;

procedure PieSlice(X, Y: Integer; StAngle, EndAngle, Radius: Word);
Begin End;
procedure Arc (X,Y: Integer; StAngle, EndAngle, Radius: Word);
Begin End;

procedure GetArcCoords(var ArcCoords: ArcCoordsType);
Begin
  ArcCoords := _ArcCoords;
End;

procedure Bar3D(x1, y1, x2, y2: Integer; Depth: Word; Top: Boolean);
Begin
  Bar(x1, y1, x2, y2);
  Rectangle(x1, y1, x2, y2);
  Line(x1, y1, x1 + Depth, y1 - Depth);
  Line(x2, y1, x2 + Depth, y1 - Depth);
  Line(x1 + Depth, y1 - Depth, x2 + Depth, y1 - Depth);
  Line(x2+Depth, y1-Depth, x2 + Depth, y2 - Depth);
  Line(x2, y2, x2 + Depth, y2 - Depth);
End;

procedure PutPixel(X, Y: Integer; Pixel: Word);
Begin
  SetPixel(xx(x), yy(y), Pixel);
End;

function GetPixel(X,Y: Integer): Word;
Begin
  GetPixel := GDI.GetPixel(x,y);
End;

procedure Ellipse(X, Y: Integer; StAngle, EndAngle: Word; XRadius, YRadius:Word);
Begin
  GDI.Ellipse(xx(x), yy(y), xRadius, yRadius, T);
End;

function ImageSize(x1, y1, x2, y2: Integer): Word;
Begin
  ImageSize := (x2-x1)*(y2-y1)+6;
End;

procedure GetImage(x1, y1, x2, y2: Integer; var BitMap);
Var
  P : PChar;
  I, J : Integer;
Begin
  P := @BitMap;
  Word(Pointer(P)^) := x2 - x1;
  Inc(P, 2);
  Word(Pointer(P)^) := y2 - y1;
  J := x2 - x1;
  Inc(P, 2);
  xx(x1); yy(y1); xx(x2); yy(y2);
  for I := y1 to y2 do begin
    ReadScanLine(x1, y1, J, P);
    Inc(P, J);
  end;
End;

procedure PutImage(X, Y: Integer; var BitMap; BitBlt: Word);
Var
  P : PChar;
  I, J : Integer;
  AX, AY : Integer;
Begin
  P := @BitMap;
  AX := Word(P^);
  Inc(P, 2);
  AY := Word(P^);
  xx(x); yy(y);
  Inc(P, 2);
  if BitBlt = 0 then

  for I := y to y + ay do begin
    WriteScanLine(x, i, ay, P);
    Inc(P, ay);
  end else
  for I := y to y + ay do begin
    WriteScanLineOp(x, i, ay, P, BitBlt);
    Inc(P, ay);
  end;
End;

procedure SetAllPalette(var Palette);
Var
  P : Pointer;
Begin
  P := @Palette;
  asm
    les bx, dword ptr [p]
    mov al, 2
    mov ah, 10h
    int 10h
  end;
End;

procedure SetPalette(ColorNum: Word; Color: Shortint);
Begin
  _Palette.Colors[ColorNum] := Color;
  SetAllPalette(_Palette);
End;

procedure RestoreCrtMode;
Begin
  DoneGDI;
End;

procedure SetGraphMode(Mode: Integer);
Begin
  InitGraph(Mode, 0, '');
End;


function InstallUserDriver(Name: string; AutoDetectPtr: pointer): integer;
Begin
  InstallUserDriver := -1;
End;

{$L MFONT.OBJ}
Procedure DefaultSystemFont; External;

Begin
  New(PSimpleFont(font), Init(@DefaultSystemFont^, ''));
  RegisterFont(font, 1);
  DefaultPaint(T);
  GDI.DefaultFont(F);
  T.Fore := 15;
  _LineStyle := 1;
  _Pattern   := 1;
  _OverScans := 0;
  _X := 0; _Y := 0;
  FillChar(_ArcCoords, SizeOf(_ArcCoords), 0);
End.