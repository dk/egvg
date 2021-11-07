{-cp -m -dUSESTANDARDBITMAPS -dBEWARENOTSVGA -dEXTENDEDSOUND -dRUSSIAN -v}
{$A+,B-,D+,E-,F+,G+,I-,L-,N-,O-,P-,Q-,R-,S-,T-,V+,X+,Y-}
{$M 16384,0,400000}
{$DEFINE USESTANDARDBITMAPS}
program Demo;
uses
  Drivers, Objects, Memory, Views, App, Menus, MsgBox, Dialogs, StdDlg,
  SVGA64K, SVGA256, SVGA16M, SVGA32K, mono, GDI, EGFont, Gadgets, BitMaps,
  EgInline, WallPapers, Palettes, VApp,
  MZResource, FntLib, CDialogs, BGIFont, AfterDrk, Flick, WStdDlg, Calc,
  EGString, Streams, TrueType, EGDPMI, WAVEPlay, {Blaster, }Image, DOS;

const
  cmSwitchKbd       = 100;
  cmSwitchBack      = 101;
  cmGetInfo         = 102;
  cmSwitchMode      = 103;
  cmMovePels        = 104;
  cmAnimation1      = 105;
  cmAnimation2      = 106;
  cmAbout           = 107;
  cmEarth           = 108;
  cmDemoMessageBox  = 109;
  cmDemoTestDialog  = 110;

  cmDemoColorDialog = 111;
  cmDemoGDIDialog   = 112;
  cmMoveFLI         = cmNextFlickFrame;
  cmOpenFLI         = 114;
  cmMoveBMTF        = 115;
  cmDemoBMTF        = 116;
  cmLandscape       = 117;
  cmVideoMode       = 118;
  cmFonts           = 119;
  cmCalc            = 120;

const
  Convert256 : Boolean = False;

type
  MyApp = object(TVApplication)
    Clock: PClockView;
    Heap: PHeapView;
    Keyboard: PKeyboardView;
    constructor Init;
    destructor  Done;        Virtual;
    procedure   InitMenuBar; virtual;
    procedure   InitScreenSaver; virtual;
    procedure   DoTestDialog;
    procedure   DoTestBoxMini;
    procedure   DoGDIDialog;
    procedure   DoColorDialog;
    procedure   DoBMTF;
    procedure   HandleEvent(var Event: TEvent); virtual;
    procedure   GetInfo;
    Procedure   OutOfMemory; Virtual;
    Procedure   OpenFLI(var Event : TEvent); Virtual cmOpenFLI; {DK}
    Procedure   SelectVideoMode;
  end;

  PIconListBox = ^TIconListBox;
  TIconListBox = object(TListBox)
    constructor Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);
    procedure DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt); virtual;
  end;

  PTestDialog= ^TTestDialog;
  TTestDialog = object(TMonolog)
    Box: PIconListBox;
    constructor Init;
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { TFileViewer }

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
  end;

  PFileViewer = ^TFileViewer;
  TFileViewer = object(TScroller)
    Hilite: Boolean;
    FileLines: PLineCollection;
    constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      AFileLines: PLineCollection; AWidth: Integer; AHilite: Boolean);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
  end;

{ TFileWindow }

  PFileWindow = ^TFileWindow;
  TFileWindow = object(TWindow)
    constructor Init(const FileName: String; ANumber: Integer);
    procedure   HandleEvent(var Event: TEvent); virtual;
  end;

  { Animation1 View }
  PPixelsAndSphere = ^TPixelsAndSphere;
  TPixelsAndSphere = object(TView)
    constructor Init(var Bounds: TRect);
    procedure   Draw; virtual;
    procedure   HandleEvent(var Event: TEvent); virtual;
   private
    CurPos: TPoint;
    Sphere: Byte;
  end;

  { Animation 2 View }
  PEarth = ^TEarth;
  TEarth = object(TView)
    constructor Init(var Bounds: TRect);
    procedure   Draw; virtual;
    procedure   HandleEvent(var Event: TEvent); virtual;
   private
    CurID: integer;
  end;

  {GDI view}
  PGDIView = ^TGDIView;
  TGDIView = Object(TView)
    Constructor Init;
    Procedure   Draw; Virtual;
  End;

  {Color view}
  PColorBar = ^TColorBar;
  TColorBar  = Object(TView)
    R, G, B : Byte;
    Constructor Init(var Bounds : TRect);
    Procedure   Draw; Virtual;
    Procedure   SetColor(_R, _G, _B : Byte);
  End;

  PColorDialog = ^TColorDialog;
  TColorDialog = Object(TDialog)
    Ctrl : array[1..3] of Record
             Scroll : PScrollBar;
             Value  : PParamText;
           end;
    ColorBar : PColorBar;
    Constructor Init;
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

  {FLI Dialog}
  PFLIDialog = ^TFLIDialog;
  TFLIDialog = Object(TDialog)
    Constructor Init(FN : String);
  End;

  PTTFBar = ^TTTFBar;
  TTTFBar = Object(Tview)
    PartialDraw : Boolean;
    Constructor Init(var Bounds : TRect);
    Procedure   Draw; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
  End;

Const
  WinIconsAndID   = 10010;
  WinIconsXorID   = 10020;
  SpheresAndID    = 10030;
  SpheresXorID    = 10040;
  EarthID         = 10100;

(****************************************************************)

var
  CurIcon: integer;

{ No Objects }

function Long2Str(N: LongInt): string;
var
  S: string;
begin
  Str(N, S);
  Long2Str := S;
end;

{ TFileViewer }


procedure TLineCollection.FreeItem(Item: Pointer);
begin
  DisposeStr(PString(Item));
end;

constructor TFileViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; AFileLines: PLineCollection;
  AWidth: Integer; AHilite: Boolean);
begin
  inherited Init(Bounds, AHScrollbar, AVScrollBar);
  Step.Y := 16;
  GrowMode := gfGrowHiX + gfGrowHiY;
  if AFileLines <> Nil then
  begin
    FileLines:= AFileLines;
    Limit.X := AWidth;
    Limit.Y := FileLines^.Count;
    Hilite:= AHilite;
  end;
end;

destructor TFileViewer.Done;
begin
  if FileLines <> Nil then Dispose(FileLines, Done);
  TScroller.Done;
end;

procedure TFileViewer.Draw;
var
  W_norm, W_hi: Byte;
  Str: String;

  procedure Upper(P: PString);
  var I: Integer;
  begin
    for I:= 0 to Succ(Byte(P^[0])) do Str[I]:= Upcase(P^[I]);
  end;

  function Find(const S: String): Boolean;
  begin
    Find:= True;
    if S = 'END' then Exit;
    if S = 'BEGIN' then Exit;
    if S = 'VAR' then Exit;
    if S = 'IF' then Exit;
    if S = 'DO' then Exit;
    if S = 'TO' then Exit;
    if S = 'FOR' then Exit;
    if S = 'THEN' then Exit;
    if S = 'ELSE' then Exit;
    if S = 'WHILE' then Exit;
    if S = 'DOWNTO' then Exit;
    if S = 'OF' then Exit;
    if S = 'CASE' then Exit;
    if S = 'TYPE' then Exit;
    if S = 'CONST' then Exit;
    if S = 'RECORD' then Exit;
    if S = 'UNTIL' then Exit;
    if S = 'REPEAT' then Exit;
    if S = 'FUNCTION' then Exit;
    if S = 'PROCEDURE' then Exit;
    if S = 'VIRTUAL' then Exit;
    if S = 'CONSTRUCTOR' then Exit;
    if S = 'DESTRUCTOR' then Exit;
    if S = 'OBJECT' then Exit;
    if S = 'USES' then Exit;
    if S = 'UNIT' then Exit;
    if S = 'PROGRAM' then Exit;
    if S = 'INTERFACE' then Exit;
    if S = 'IMPLEMENTATION' then Exit;
    Find:= False;
  end;

  procedure FindHilite;
  var
    I: Byte;
  begin
    W_hi:= 0; W_norm:= 0;
    while (W_hi = 0) and (W_norm < Byte(Str[0])) do
    begin
      while (W_norm < Byte(Str[0])) and not (Str[1 + W_norm] in ['A'..'Z','_','$']) do
        Inc(W_norm); {╨▓╤Л╨▒╤А╨░╤В╤М ╨┐╤А╨╛╨▒╨╡╨╗╤Л}
      while ((W_norm + W_hi) < Byte(Str[0])) and (Str[1 + W_norm + W_hi] in ['A'..'Z','_','$'])  do
        Inc(W_hi); {╨▓╤Л╨▒╤А╨░╤В╤М ╤Б╨╗╨╛╨▓╨╛}
      if (W_hi > 0) and not Find(Copy(Str, 1 + W_norm, W_hi)) then
      begin
        Inc(W_norm, W_hi);
        W_hi:= 0;
      end;
    end;
  end;

var
  C_norm, C_hi, I: Byte;
  X, Y: Integer;
  P: PString;
  S: String;
  FirstY, LastY: integer;
  R: TRect;
begin
  if (LastDelta.X - Delta.X = 0) and (abs(LastDelta.Y - Delta.Y) = 1) then begin
    ScrollY(0, 0, Size.X, Size.Y, (Delta.Y-LastDelta.Y)*Step.Y);
  end;
  C_norm := 5; C_hi:= 9;
  GetExtent(R);
  GetInvalidExtent(R);
  Bar(R.A.X, R.A.Y, R.B.X, R.B.Y, MonoColor(C_norm, White));
  FirstY := R.A.Y div Step.Y;
  LastY := FirstY + (R.B.Y - R.A.Y + Step.Y - 1) div Step.Y;
  for Y := FirstY to LastY do
    if (Delta.Y + Y < FileLines^.Count) then
    begin
      P:= FileLines^.At(Delta.Y + Y);
      if P <> Nil then
      begin
        if not Hilite then
        begin
          WrStr((- Delta.X) * Step.X, Y * Step.Y, P^, C_norm);
          Continue;
        end;
        Upper(P); { out: Str }
        I:= 1; X:= 0;
        repeat
          FindHilite; { in: Str; out: W_norm, W_hi }
          S:= Copy(P^, I, W_norm);
          WrStr((X - Delta.X) * Step.X, Y * Step.Y, S, 0);
          Inc(X, Length(S));
          if W_hi > 0 then
          begin
            Font.Font:= 1;
            S:= Copy(P^, I + W_norm, W_hi);
            WrStr((X - Delta.X) * Step.X, Y * Step.Y, S, C_hi);
            Inc(X, Length(S));
            Str:= Copy(Str, W_norm + W_hi + 1, 255);
            Inc(I, W_norm + W_hi);
            Font.Font:= 3;
          end;
        until W_hi = 0;
      end;
    end;
end;

procedure TFileViewer.SetState(AState: Word; Enable: Boolean);
begin
  TScroller.SetState(AState, Enable);
  if Enable and (AState and sfExposed <> 0) then
     SetLimit(Limit.X, Limit.Y);
end;

{=== TFileWindow ===}

procedure TFileWindow.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What = evKeyDown) and (Event.KeyCode = kbShiftF1) then begin
    ClearEvent(Event);
    Iconize;
  end;
end;

constructor TFileWindow.Init(const FileName: String; ANumber: Integer);
var
  W: Integer;

  function LoadLines: PLineCollection;
  var
    F: Text;
    L: PLineCollection;
    S: String;
    I: Byte;
  begin
    New(L, Init(100, 10));
    Assign(F, FileName);
    Reset(F);
    if IOResult = 0 then
      while not Eof(F) and (IOResult = 0) and not LowMemory do
      begin
        ReadLn(F, S);
        if IOResult <> 0 then Break;
        I:= Pos(^I, S);
        while I > 0 do
        begin
          System.Delete(S, I, 1);
          System.Insert('    ', S, I);
          I:= Pos(^I, S);
        end;
        if Length(S) > W then W:= Length(S);
        L^.Insert(NewStr(S));
      end;
    System.Close(F);
    LoadLines:= L;
  end;

var
  R: TRect;
begin
  SetFont(IconsFont);
  Desktop^.GetExtent(R);
  Dec(R.B.X, R.B.X div 3);
  Dec(R.B.Y, R.B.Y div 3);
  inherited Init(R, FileName, ANumber);
  Options := Options or ofTileable;
{  Palette:= wpBlueWindow;}
  Frame^.GetClientExtent(R);
  Dec(R.B.X, BitMapWidth(@ScrollUpDef)+2);
  Dec(R.B.Y, BitMapWidth(@ScrollUpDef)+2);
  W:= 0;
  Insert(New(PFileViewer, Init(R,
    StandardScrollBar(sbHorizontal + sbHandleKeyboard),
    StandardScrollBar(sbVertical + sbHandleKeyboard), LoadLines, W,
    (Pos('.PAS', FileName) = (Length(FileName) - 3)) or
    (Pos('.INC', FileName) = (Length(FileName) - 3))
    )));
  RestoreFont;
end;

constructor TIconListBox.Init(var Bounds: TRect; ANumCols: Word; AScrollBar: PScrollBar);
var
  I: Integer;
begin
  inherited Init(Bounds, ANumCols, AScrollBar);
  I:= BitMapWidth(@ScrollLeftDef);
  if I > Step.Y then Step.Y := I;
  Step.Y:= 3 * Step.Y div 2;
end;

procedure TIconListBox.DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt);
begin
  Inc(Bounds.A.X, 2 * Step.X);
  inherited DrawItem(Item, Indent, Bounds, Color);
  Dec(Bounds.A.X, 2 * Step.X);
  Inc(Bounds.A.Y, ((Bounds.B.Y - Bounds.A.Y) - BitMapWidth(@ScrollLeftDef)) div 2);
  if IsSelected(Item) then
    PutBmp((@ScrollRightPress), Bounds.A.X, Bounds.A.Y)
  else
    PutBmp((@ScrollLeftDef), Bounds.A.X, Bounds.A.Y)
end;

constructor TTestDialog.Init;
var
  P : PView;
  R : TRect;
  C : PStringCollection;
  Scr: PScrollBar;
begin
  R.Assign(40, 20, 400, 400);
  inherited Init(R, 'Dialog');

  R.Assign(10, 30, 240, 30 + 16);
  Insert(New(PStaticText, Init(R, ^F#2'Это есть статик '^F#1'текст'^F#2'!')));

  R.Assign(10, 60, 240, 60 + 16);
  P := New(PInputLine, Init(R, 255));
  Insert(P);
  GlobalFont := MenusFont;
  R.Assign(10, 46, 240, 60);
  Insert(New(PLabel, Init(R,  '~С~трока, которую можно ввести!', P)));
  DefaultFont(GlobalFont);

  R.Assign(10, 100, 200, 132);
  P:= New(PRadioButtons, Init(R, NewSItem('~Ф~orward',
                                 NewSItem('~Б~ackward', nil))));
  Insert(P);
  R.Assign(10, 86, 200, 100);
  Insert(New(PLabel, Init(R,  '~Р~адио-кнопка', P)));

  R.Assign(10, 140, 200, 156);
  Insert(New(PCheckBoxes, Init(R, NewSItem('~Ф~irst',
                                  NewSItem('~С~econd', nil)))));

  C:= New(PStringCollection, Init(10,5));
  C^.Insert(NewStr(' 1 One'));
  C^.Insert(NewStr(' 2 Two'));
  C^.Insert(NewStr(' 3 Three'));
  C^.Insert(NewStr(' 4 Four'));
  C^.Insert(NewStr(' 5 Five'));
  C^.Insert(NewStr(' 6 Six'));
  C^.Insert(NewStr(' 7 Seven'));
  C^.Insert(NewStr(' 8 Eight'));
  C^.Insert(NewStr(' 9 Nine'));
  C^.Insert(NewStr('10 Ten'));
  C^.Insert(NewStr('11 Eleven'));
  C^.Insert(NewStr('12 12'));
  C^.Insert(NewStr('13 13'));
  C^.Insert(NewStr('14 14'));
  C^.Insert(NewStr('X And so on...'));
  R.Assign(18, 340, 200, 340+8);
  New(Scr, Init(R));
  Insert(Scr);
  R.Assign(18, 180, 200, 340);

  Box:= New(PIconListBox, Init(R, 2, Scr));
  Box^.NewList(C);
  Insert(Box);

  R.Assign(230, 140, 350, 156);
  Insert(New(PShade, Init(R, $7, $F, 0, 0, shBar)));
  R.Assign(230, 180, 350, 310);
  Insert(New(PShade, Init(R, $7, $F, 0, 0, shFrame)));

  R.Assign(240, 190, 340, 220);
  Insert(New(PButton, Init(R, '~П~алитра', cmOk, bfNormal)));

  R.Assign(240, 230, 340, 260);
  Insert(MakeYesButton(R, cmOK, bfDefault));

  R.Assign(240, 270, 340, 300);
  Insert(MakeCancelButton(R, cmCancel, bfNormal));

  SelectNext(False);
end;

destructor TTestDialog.Done;
begin
  Box^.NewList(Nil);
  inherited Done;
end;

procedure TTestDialog.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What = evCommand) and (Event.Command = cmOk) then
  begin Inc(Palette); if Palette>=dpMagentaDialog+1 then Palette:=dpBlueDialog;
    DrawView;
    ClearEvent(Event);
  end;
end;

constructor MyApp.Init;
var R : TRect; MZR: PMZResource; ImgXor, ImgAnd: PImage; i: integer;
    fnt:PSimpleFont; S1: String;
    F:PAbstractFont;
    LP : TLogPalette;
begin
  SystemColors := 32;
  New(fnt, Init(@Small^, 'Small'));
  RegisterFont(fnt, 2);
  New(PScaledSimpleFont(fnt), Init(@Courier^, 'Courier'));
  RegisterFont(fnt, 3);
  New(PScaledSimpleFont(fnt), Init(@Large^, 'Large Courier'));
  RegisterFont(fnt, 4);
  New(PScaledSimpleFont(fnt), Init(@Thick^, 'Thick'));
  RegisterFont(fnt, 5);
  F := New(BGIFont.PBGIFont, Init('TRIP.CHR'));
  RegisterFont(F, 6);
  F := New(PTrueTypeFont, Init('ACADEMY.TTF'));
  if F = Nil then DisableCommands([cmDemoBMTF])
  else begin
    RegisterFont(F, 7);
    LoadGIF('HELLFIRE.GIF', 667, @LP, 0);
    RegisterPalette(@LP, 1);
  end;


  WaveFiles[sbEntry]    := 'CHAINSAW.WAV';
  WaveFiles[sbExit]     := 'DSPDIEHI.WAV';
  WaveFiles[sbSysError] := 'LAUGH.WAV';
  WaveFiles[sbError]    := 'ALERT.WAV';

  ReleaseTimeSlice := True;
  AssignFont(2, TitleFont);
  AssignFont(3, IconsFont);
  if WallPaper = Nil then begin
    WallPaper  := @WallPaperLeather;
    WallPaperPalette := Nil;
  end;

  S1:=StUpCase(ParamStr(1));
  if Pos('S', S1)<>0 then begin
      SelectDriver(GetAppropriateDriver(800, 600, 0, False));
      AssignFont(5, MenusFont);
      AssignFont(4, StatusFont);
    end else begin
      AssignFont(1, MenusFont);
      AssignFont(3, StatusFont);
    end;
  if Pos('2', S1)<>0 then Convert256 := True;

  inherited Init;
  RegisterFont(New(PWindowsFont, Init('vgasys.fon')), 1);
  RedRaw;
  SetMouseWait;
  InitKbdDriver;
  AltKbdSwitch:= kbF9;
{}
  SetFont(MenusFont);
  R.Assign(0, 0, 8*GetCharWidth, 0);
  Clock := New(PClockView, Init(R));
  Insert(PlaceInMenuBar(Clock, 4));
  RestoreFont;
{}
  SetFont(StatusFont);
  R.Assign(0, 0, 7*GetCharWidth, 0);
  Keyboard := New(PKeyboardView, Init(R));
  Insert(PlaceInStatusLine(Keyboard, 4));

  R.B.X:= 9*GetCharWidth;
  Heap:= New(PHeapView, Init(R));
  Insert(PlaceInStatusLine(Heap, 8 + Keyboard^.Size.X + 4));
  RestoreFont;
{}
  New(MZR, Init);
  if MZR = Nil then Exit;
  if MZR^.Read('DEMO.DLL') then begin
    for i:=1 to 5 do begin
      LoadIcon(MZR, MakeIntResource(i), ImgAnd, ImgXor);
      RegisterImageInHeap(SpheresAndID+i, ImgAnd);
      RegisterImageInHeap(SpheresXorID+i, ImgXor);
    end;
    for i:=0 to 3 do begin
      LoadIcon(MZR, MakeIntResource(7+i*2), ImgAnd, ImgXor);
      RegisterImageInHeap(WinIconsAndID+i, ImgAnd);
      RegisterImageInHeap(WinIconsXorID+i, ImgXor);
    end;
    {$IFDEF Dpmi}
    for i:=1 to 24 do begin
      ImgAnd := LoadBitMap(MZR, MakeIntResource(i), nil);
      if Convert256 then ImgAnd := CreateDIBitmap(ImgAnd, cbwDelete);
      RegisterImageInHeap(EarthID+i, ImgAnd);
    end;
    {$ELSE}
    {for i:=1 to 12 do begin
      ImgAnd := LoadBitMap(MZR, MakeIntResource(i*2));
      if Convert256 then ImgAnd := CreateDIBitmap(ImgAnd, cbwDelete);
      RegisterImageInHeap(EarthID+i*2, ImgAnd);
    end;}
    DisableCommands([cmAnimation2]);
    {$ENDIF}
  end;
  CurIcon := 0;
  Dispose(MZR, Done);
end;

Destructor MyApp.Done;
Begin
  FreeDImage(GetImage(667));
  Inherited Done;
End;

procedure MyApp.InitScreenSaver;
var
  R: TRect;
begin
  {--- Example ---}
  R.Copy(AllScreen);
  {ScreenSaver := New(PScreenSaver, Init(R, ssfAllOptions, 5));}
  ScreenSaver := PView(New(PFlyingBallsScreenSaver, Init(100)));
end;

procedure MyApp.DoTestBoxMini;
begin
  MessageBox(^C'T'^F#2'urbo '^F#1'V'^F#2'ision '^F#1'T'^F#2'utorial '^F#1'A'^F#2'pplication'#13 +
    ^C^F#1'Copyright '^F#3'1992'#13^C'B'^F#2'orland '^F#1'I'^F#2'nternational',
    nil, mfInformation or mfOKButton or mfInsertInApp);
end;

procedure MyApp.DoTestDialog;
begin
  InsertWindow(New(PTestDialog, Init));
end;

Procedure MyApp.DoGDIDialog;
Var
  R : TRect;
  P : PDialog;
Begin
  R.Assign(0, 0, 35 * GetCharWidth, 7 * GetHeight + 3);
  R.Move(25 * GetCharWidth, 4 * GetHeight);
  P := New(PDialog, Init(R, 'Мрачные отрисовки'));
  P^.Insert(New(PGDIView, Init));
  Desktop^.Insert(P);
End;

Procedure MyApp.DoBMTF;
Var
  R : TRect;
  P : PDialog;
Begin
  R.Assign(0, 0, 35 * GetCharWidth, 7 * GetHeight + 3);
  P := New(PDialog, Init(R, 'TrueType'));
  R.Grow(-GetCharWidth, -GetHeight-3);
  R.Move(0, 6);
  P^.Insert(New(PTTFBar, Init(R)));
  Desktop^.Insert(P);
  if BitmapWidth(GetImage(667)) > 0 then TimedMessage(1, P, evBroadcast, cmMoveBMTF, Nil, 3)
End;

Procedure MyApp.DoColorDialog;
Begin
  Desktop^.Insert(New(PColorDialog, Init));
End;

Procedure  MyApp.OutOfMemory;
Begin
  MessageBox('No more fucking memory!', nil, mfError + mfOkButton);
End;

procedure MyApp.GetInfo;
var
  R: TRect;
  Dlg: PDialog;
begin
{$IFNDEF Dpmi}
  R.Assign(0, 0, 400, 180);
{$ELSE}
  R.Assign(0, 0, 350, 100);
{$ENDIF}
  New(Dlg, Init(R, 'Информация'));
  SetFlag(Dlg^.Options, ofCentered);
{$IFNDEF Dpmi}
  R.Assign(10, 26, 390, 42);
{$ELSE}
  R.Assign(10, 26, 340, 42);
{$ENDIF}
  Dlg^.Insert(New(PStaticText, Init(R,
   ^F#3'Разрешение экрана          : '^F#1+Long2Str(ScreenDriver^.ScreenWidth)+'x'+
   Long2Str(ScreenDriver^.ScreenHeight))));
  R.Move(0, 16);
  Dlg^.Insert(New(PStaticText, Init(R,
   ^F#3'Число цветов               : '^F#1+Long2Str(ScreenDriver^.NumberOfColors))));
  R.Move(0, 16);
  Dlg^.Insert(New(PStaticText, Init(R,
   ^F#3'Размер битовой плоскости   : '^F#1+Long2Str(ScreenDriver^.BitPlaneSize)+' байт')));
  R.Move(0, 16);
  Dlg^.Insert(New(PStaticText, Init(R,
   ^F#3'Число битовых плоскостей   : '^F#1+Long2Str(ScreenDriver^.NumPlanes))));
{$IFNDEF Dpmi}
{$ENDIF}
  ExecuteDialog(Dlg, Nil);
end;

{$I VOXEL.PAS}

Procedure MyApp.SelectVideoMode;
Var
  P : PScreenDriver;
  R : TRect;
Begin
  P := Nil;
  if ExecuteDialog(New(PVideoDriverDialog, Init), @P) = cmOk then begin
    EndModal(cmQuit);
    TApplication.Done;
    SelectDriver(P);
    Desktop := Nil;
    StatusLine := Nil;
    MenuBar   := Nil;
    ScreenSaver := Nil;
    TApplication.Init;
    RegisterFont(New(PWindowsFont, Init('vgasys.fon')), 1);
    Redraw;

    SetFont(MenusFont);
    R.Assign(0, 0, 8*GetCharWidth, 0);
    Clock := New(PClockView, Init(R));
    Insert(PlaceInMenuBar(Clock, 4));
    RestoreFont;
  {}
    SetFont(StatusFont);
    R.Assign(0, 0, 7*GetCharWidth, 0);
    Keyboard := New(PKeyboardView, Init(R));
    Insert(PlaceInStatusLine(Keyboard, 4));

    R.B.X:= 9*GetCharWidth;
    Heap:= New(PHeapView, Init(R));
    Insert(PlaceInStatusLine(Heap, 8 + Keyboard^.Size.X + 4));
    RestoreFont;
  end;
End;

procedure MyApp.HandleEvent(var Event: TEvent);
var
  R: TRect;
  S: String;
  PW: PFileWindow;

  procedure Animation1;
  var
    Dlg: PWindow;
    R : TRect;
  begin
    R.Assign(100, 100, 220, 240);
    New(Dlg, Init(R, 'Точки и шарик', 0));
    Dlg^.Options := Dlg^.Options or ofTileable;

    Dlg^.Frame^.GetClientExtent(R);
    Dlg^.Insert(New(PPixelsAndSphere, Init(R)));

    InsertWindow(Dlg);
    TimedMessage(1, Dlg, evBroadcast, cmMovePels, Nil, 2);
  end;

  procedure Animation2;
  var
    Dlg: PDialog;
    R : TRect;
  begin
    R.Assign(200, 200, 200+167+12, 200+146+26);
    New(Dlg, Init(R, 'День и ночь'));

    R.Assign(6, 20, 6+167, 20+146);
    Dlg^.Insert(New(PEarth, Init(R)));

    if InsertWindow(Dlg) <> Nil then TimedMessage(1, Dlg, evBroadcast, cmEarth, Nil, 0);
  end;

  procedure About;
  var
    AOpt : Word;
    LP    : TLogPalette;
    PassBM: array[1..2] of Pointer;
  begin
    SetMouseWait;
    AOpt := mfInformation + mfOKButton;
    if LoadGIF('BLOCK3.GIF', 666, @LP, 0) then begin
      LP.Mode := pmUseRGB + pmOptimize + pmInHeap;
      AOpt := AOpt + mfBitmap;
      PassBM[1] := GetImage(666);
      PassBM[2] := @LP;
    end;
    MessageBox(^M^C^F#3'DK Inc. 1996  EG to SVG conversion'+
    ^M^M^C^F#1'Crazy Chickotilo revenge'+
    ^M^M^C^F#3'Version 1.0', @PassBM, AOpt);
  end;

  procedure NotImplemented;
  begin
    MessageBox(^M^C^F#3'Не реализовано!'^M^M^C^F#3'Это же '^F#1'ДЕМО!',
      nil, mfError or mfOKButton);
  end;

begin
  inherited HandleEvent(Event);
  if Event.What <> evCommand then Exit else
    case Event.Command of
      cmNew,
      cmSave,
      cmSaveAs,
      cmSaveAll:
        NotImplemented;
      cmGetInfo: GetInfo;
      cmChangeDir:ExecuteDialog(New(PChDirDialog, Init(cdNormal, 0)), nil);
      cmOpen:
        begin
          S:= 'DEMO_.PAS';
          if ExecuteDialog(New(PFileDialog, Init('*.*', 'Выбор файла',
            '~И~мя', fdOkButton, 0)), @S) <> cmCancel then begin
              PW := New(PFileWindow, Init(S, 0));
              if InsertWindow(PW) <> Nil then begin
                PW^.SetIcon(WinIconsAndID+CurIcon, WinIconsXorID+CurIcon);
                Inc(CurIcon);
                if CurIcon > 3 then CurIcon := 0;
              end;
          end;
        end;
      cmDemoMessageBox: DoTestBoxMini;
      cmDemoTestDialog: DoTestDialog;
      cmDemoGDIDialog : DoGDIDialog;
      cmDemoColorDialog : DoColorDialog;
      cmDemoBMTF  : DoBMTF;
      cmLandscape : LoadLandscape;
      cmVideoMode : SelectVideoMode;
      cmFonts     : Desktop^.Insert(New(PFontDialog, Init));
      cmCalc      : Desktop^.Insert(New(PCalculator, Init));
      cmAnimation1: Animation1;
      cmAnimation2: Animation2;
      cmAbout: About;
      cmSwitchKbd: AltKbdMode:= not AltKbdMode;
      cmSwitchMode:
        begin
          R.A.X := ScreenDriver^.ScreenHeight;
          HideMouse;
          DoneEvents;
          DoneVideo;
          if R.A.X = 600
          then ScreenDriver := GetAppropriateDriver(640, 480, 0, False)
          else ScreenDriver := GetAppropriateDriver(800, 600, 0, False);
          InitVideo;
          InitEvents;
          R.Assign(0, 0, ScreenWidth, ScreenHeight);
          ChangeBounds(R);
          RedrawPalette;
        end;
      cmSwitchBack:
        begin
          if WallPaper = Nil then Exit;
          if WallPaper = @WallPaperLeather then begin
            WallPaper := @WallPaperDoom;
            WallPaperPalette := @StdVGAPalette;
          end else begin
            WallPaper := @WallPaperLeather;
            WallPaperPalette := Nil;
          end;
          Desktop^.BackGround^.UpdateWallPaper;
        end;
    else
      Exit
    end;
  ClearEvent(Event);
end;

Procedure MyApp.OpenFLI;
Var
  S : String;
  P : PDialog;
  R : TRect;
Begin
  S:= '*.FL*';
  if ExecuteDialog(New(PFileDialog, Init('*.FL*', 'Открыть анимаку',
    '~И~мя', fdOkButton, 0)), @S) <> cmCancel then begin
    P := New(PFLIDialog, Init(S));
    if P = Nil then begin
      P := @S;
      MessageBox('%s: или не флик или мало памяти', @P,
                               mfError + mfOkButton)
    end
    else begin
      Desktop^.Insert(P);
    end;
  end;
End;


procedure MyApp.InitMenuBar;
var
  R: TRect;
begin
  StandardMenuBarRect(R);
  MenuBar := New(PMenuBar, Init(R,
               NewMenu(
                 NewSubMenu('~Ф~айл', hcNoContext,
                   NewMenu(StdFileMenuItems(Nil)),
                 NewSubMenu('~Р~едактор', hcNoContext,
                   NewMenu(StdEditMenuItems(Nil)),
                 NewSubMenu('~О~кно', hcNoContext,
                   NewMenu(StdWindowMenuItems(
                   NewLine(
                   NewItem('~Р~ежим клавиатуры', 'F9', kbNoKey, cmSwitchKbd, hcNoContext,
                   NewItem('~Б~экграунд', 'F8', kbF8, cmSwitchBack, hcNoContext,
                   NewItem('~Э~кран', 'AltF9', kbAltF9, cmSwitchMode, hcNoContext,
                   Nil)))))),
                 NewSubMenu('~П~римеры', hcNoContext, NewMenu(
                   NewItem('MessageBox', '', kbNoKey, cmDemoMessageBox, hcNoContext,
                   NewItem('~Д~иалог', '', kbNoKey, cmDemoTestDialog, hcNoContext,
                   NewItem('Пример ~G~DI', '', kbNoKey, cmDemoGDIDialog, hcNoContext,
                   NewItem('~Ц~вета', '', kbNoKey, cmDemoColorDialog, hcNoContext,
                   NewItem('~Ш~рифты', '', kbNoKey, cmFonts, hcNoContext,
                   NewItem('~К~алькулятор', '', kbNoKey, cmCalc, hcNoContext,
                   NewItem('~T~rueType', '', kbNoKey, cmDemoBMTF, hcNoContext,
                   NewLine(
                   NewItem('Мультик ~1~', '', kbNoKey, cmAnimation1, hcNoContext,
                   NewItem('Мультик ~2~', '', kbNoKey, cmAnimation2, hcNoContext,
                   NewItem('~Ф~лик', '', kbNoKey, cmOpenFLI, hcNoContext,
                   NewItem('~Л~андшафт', '', kbNoKey, cmLandscape, hcNoContext,
                   NewItem('~В~идео драйвер', '', kbNoKey, cmVideoMode, hcNoContext,
                   Nil)))))))))))))),
                 NewSubMenu('~И~нформация', hcNoContext, NewMenu(
                   NewItem('~В~идео', '', kbNoKey, cmGetInfo, hcNoContext,
                   NewLine(
                   NewItem('~О~ программе', '', kbNoKey, cmAbout, hcNoContext,
                   Nil)))),
                 Nil))))))));
end;


{ TPixelsAndSphere }

constructor TPixelsAndSphere.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  CurPos.X := Size.X div 2;
  CurPos.Y := Size.Y div 3;
  SetFlag(EventMask, evBroadcast);
  GrowMode := gfGrowHiX + gfGrowHiY;
  Sphere := 1;
end;

procedure TPixelsAndSphere.Draw;
var
  i, j: integer;
begin
  Bar(0, 0, Size.X, Size.Y, 7);
  for i:=0 to 2 do
    for j:=0 to 2 do
      SetPixel(CurPos.X+i, CurPos.Y+j, 1);
  PutBMPOp(GetImage(SpheresAndID+Sphere), Size.X div 2 - 16, Size.Y div 2 - 16, 2);
  PutBMPOp(GetImage(SpheresXorID+Sphere), Size.X div 2 - 16, Size.Y div 2 - 16, 1);
end;

procedure TPixelsAndSphere.HandleEvent(var Event: TEvent);
var
  R: TRect;
begin
  if (Event.What = evBroadcast) and (Event.Command = cmMovePels) then begin
    ClearEvent(Event);
    TimedMessage(1, Owner, evBroadcast, cmMovePels, Nil, 3);
    R.Assign(CurPos.X, CurPos.Y, CurPos.X+3, CurPos.Y+3);
    CurPos.X := Random(Size.X-3);
    CurPos.Y := Random(Size.Y-3);
    InvalidateRect(R); DrawInvalidated;
    R.Assign(CurPos.X, CurPos.Y, CurPos.X+3, CurPos.Y+3);
    InvalidateRect(R); DrawInvalidated;
    R.Assign(Size.X div 2 - 16, Size.Y div 2 - 16, Size.X div 2 + 16, Size.Y div 2 + 16);
    InvalidateRect(R); DrawInvalidated;
    Inc(Sphere); if Sphere > 5 then Sphere := 1;
  end;
  inherited HandleEvent(Event);
end;


{ TEarth }

constructor TEarth.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  SetFlag(EventMask, evBroadcast);
  CurID := 1;
end;

procedure TEarth.Draw;
begin
  PutBMP(GetImage(EarthID + CurID), 0, 0);
end;

procedure TEarth.HandleEvent(var Event: TEvent);
begin
  if (Event.What = evBroadcast) and (Event.Command = cmEarth) then begin
    ClearEvent(Event);
    TimedMessage(1, Owner, evBroadcast, cmEarth, Nil, 0);
    DrawView;
    Inc(CurID); if CurID > 24 then CurID := 1;
  end;
end;

{GDI View}
Constructor TGDIView.Init;
Var
  R : TRect;
Begin
  R.Assign(12, 25, 266, 103);
  Inherited Init(R);
  LogPalette.Mode := pmUseRGB;
End;

Procedure TGDIView.Draw;
Var
  I:Word;
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
  SetPaint(12, 4, CopyPut, psNull, lwThin, lsBitMap, fsParquet);
  PaintInfo.BitMap := @ScrollDownDef;
  FillPoly(5, Triangle);
  SetPaint(30, 1, CopyPut, psNull, lwSemiBold, lsPattern, fsBkSlash);
  for I := 0 to 15 do begin
    PaintInfo.Fore := ColorIndex^[16+I];
    PaintInfo.Back := ColorIndex^[31-I];
    FillCircle(50, 50, 18 - i);
  end;

  SetPaint(30, 0, XORPut, psSolid, lwNormal, lsLinePattern, fsSolid);
  for I := 1 to 16 do begin
    PaintInfo.Fore := ColorIndex^[32-I];
    FillCircle(138+58, 50, 4 * i);
  end;

  SetPaint(16, 0, CopyPut, psSolid, lwThin, lsLinePattern, fsNoUse);
  SetTextMetrics(6, 16, 32, 340, 0, 0, 0);
  WrStr(138+15, 1, 'So what?', 0);
  Font := GlobalFont;

  SetPaint(30, 0, XORPut, psSolid, lwNormal, lsLinePattern, fsSolid);
  for I := 1 to 16 do begin
    PaintInfo.Fore := ColorIndex^[32-I];
    FillCircle(138+58, 50, 4 * i);
  end;

  SetPaint(30, 0, CopyPut, psSolid, lwHollow, lsBitMap, fsSolid);
  PaintInfo.ColorRef  := Desktop^.BackGround^.LogPalette.ColorRef;
  PaintInfo.Bitmap    := WallPaper;
  FillCircle(138+23, 62, 23);
End;

{ColorView}
Constructor TColorBar.Init;
Begin
  Inherited Init(Bounds);
  LogPalette.Mode := pmUseRGB or pmHiColor;
End;

Procedure TColorBar.Draw;
Begin
  SolidBrush(R, G, B);
  BarStyle(0, 0, Size.X, Size.Y);
End;

Procedure TColorBar.SetColor;
Begin
  R := _R; G := _G; B := _B;
  DrawView;
End;

Constructor TColorDialog.Init;
Var
  R : TRect;
  P : PView;
  I : Byte;
Const
  RGBS : array[1..3] of Char = 'RGB';
Begin
  R.Assign(100, 100, 415, 200);
  Inherited Init(R, 'Цвета');
  R.Assign(20, 30, 220, 46);

  for I := 1 to 3 do begin
    R.Assign(8, 7 + I * 20, 20, 23 + I * 20);
    P := New(PStaticText, Init(R, RGBS[I]));
    Insert(P);
    R.Assign(20, 6 + I * 20, 220, 22 + I * 20);
    P := New(PScrollBar, Init(R));
    PScrollbar(P)^.SetParams(0, 0, 255, 10, 1);
    Insert(P);
    Ctrl[I].Scroll := PScrollBar(P);
    R.Assign(224, 6 + I * 20, 252, 22 + I * 20);
    P := New(PParamText, Init(R, '%d', 1));
    P^.SetData(Ctrl[I].Scroll^.Value);
    Insert(P);
    Ctrl[I].Value := PParamText(P);
  end;
  R.Assign(252, 26, 300, 82);
  New(ColorBar, Init(R));
  Insert(ColorBar);
  SelectNext(False);
End;

Procedure TColorDialog.HandleEvent(var Event : TEvent);
Var
  I : Byte;
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmScrollBarChanged) then begin
    for I := 1 to 3 do Ctrl[I].Value^.DrawView;
    ColorBar^.SetColor(Ctrl[1].Scroll^.Value, Ctrl[2].Scroll^.Value, Ctrl[3].Scroll^.Value);
  end;
End;

Constructor TFLIDialog.Init;
Var
  P : PView;
  R : TRect;
  S : TBufStream;
Begin
  S.Init(FN, stOpenRead, 2048);
  if S.Status <> 0 then Fail;
  P := New(PFLIView, Init(S));
  S.Done;
  if P = Nil then Fail;
  R.Assign(8, 28, 100, 100);
  Inherited Init(R, FN);
  R.Assign(0, 0, P^.Size.X, P^.Size.Y);
  R.Grow(10, 19);
  R.Move(160, 100);
  Locate(R);
  Insert(P);
  TimedMessage(1, P, evBroadcast, cmMoveFLI, Nil, 2);
End;

Constructor TTTFBar.Init;
var
  LP : PlogPalette;
Begin
  Inherited Init(Bounds);
  if BitmapWidth(GetImage(667)) > 0 then begin
    LP := RetrievePalette(1);
    CreatePalette(LP^.Palette, LogPalette, 0, LP^.Colors);
    PaintInfo.Bitmap := GetImage(667);
    PaintInfo.LineStyle := lsBitmapOrg;
    SetBitmapOrg(0, 0);
    Eventmask := EventMask or evBroadcast;
    if MaxColors >= 32768 then MakeHiPalette(LogPalette, MaxColors > 65536);
    PaintInfo.ColorRef := LogPalette.ColorRef;
    if PaintInfo.ColorRef <> Nil then LogPalette.Mode := pmUseRGB + pmOptimize;
  end else begin
    PaintInfo.LineStyle   := lsLinePattern;
    PaintInfo.LinePattern := psSolid;
  end;
  PartialDraw := False;
  Font.Font := 7;
End;

Procedure TTTFBar.Draw;
Begin
  if not PartialDraw then Bar(0, 0, Size.X, Size.Y, 0);
  SetTextMetrics(7, 125, 60, 0, 0, 0, 0);
  WrStr(2, 2, 'DK', 7);
End;

Procedure TTTFBar.HandleEvent;
Var
  R : TRect;
Begin
  if (Event.What = evBroadcast) and (Event.Command = cmMoveBMTF) then begin
    ClearEvent(Event);
    TimedMessage(1, Owner, evBroadcast, cmMoveBMTF, Nil, 3);
    R.Assign(0, 0, Pred(Size.X), Pred(Size.Y));
    PartialDraw := True;
    InvalidateRect(R);
    DrawInvalidated;
    PartialDraw := False;
    Dec(PaintInfo.BitmapOrg.X);
    Dec(PaintInfo.BitmapOrg.Y);
  end;
  Inherited HandleEvent(Event);
End;


var
  Task: MyApp;
begin
  usedmm := true;
  Task.Init;
  Task.Run;
  Task.Done;
end.
