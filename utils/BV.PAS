{-cp -m}
Uses Objects, VApp, Bitmaps, SVGA64k, GDI, Dialogs, Views, Drivers,
     App, StdDlg, MsgBox, Menus, Gadgets, Image, EgInline;


Const
  cmRadioButtonPressed = $8901;
  UseOpt : Boolean = False;
  Num: Word = 1;

Type
  PApp = ^TApp;
  TApp = Object(TVApplication)
    Heap: PHeapView;
    Procedure OpenBitMap(var Event : TEvent); Virtual cmOpen;
    Procedure InitStatusLine; Virtual;
    Procedure OutOfMemory;    Virtual;
  End;

  PFuckingStatic = ^TFuckingStatic;
  TFuckingStatic = Object(TView)
    Constructor Init;
    Procedure   Draw; Virtual;
  End;


  PBMPView = ^TBMPView;
  TBMPView = Object(TScroller)
    BM  : Integer;
    BMP : PImage;
    Constructor Init(FN : String; AHSB, AVSB : PScrollBar);
    Destructor  Done;   Virtual;
    Procedure   Draw;   Virtual;
    Procedure HandleEvent(var Event : TEvent); Virtual;
    Procedure  SetState(AState: Word; Enable: Boolean); Virtual;
  End;

  PBMPDialog = ^TBMPDialog;
  TBMPDialog = Object(TWindow)
    P : PBMPView;
    Constructor Init(FN : String);
    Procedure SizeLimits(var Min, Max: TPoint); Virtual;
    Procedure Zoom; Virtual;
  End;

  PTellingCheckBoxes = ^TTellingCheckBoxes;
  TTellingCheckBoxes = Object(TCheckBoxes)
    procedure Press(Item: Integer); virtual;
    procedure HandleEvent(var Event : TEvent); Virtual;
  End;

Procedure TTellingCheckBoxes.Press(Item : Integer);
Begin
  Inherited Press(Item);
  UseOpt := Boolean(Value);
  LPMessage(1,Application, evBroadCast, cmRadioButtonPressed, Nil);
End;

procedure TTellingCheckBoxes.HandleEvent(var Event : TEvent);
var
  Is : Boolean;
Begin
  Is := (State and sfFocused <> 0) and (Event.What = evKeyDown) and
     (Event.ScanCode in [Hi(kbEnter)]);
  Inherited HandleEvent(Event);
  if Is then Message(Owner, evBroadCast, cmRadioButtonPressed, Nil);
End;

Constructor TBMPView.Init;
Var
  R : TRect;
  X : PImage;
Const
  Use256ColorOptimization = True;
Begin
  R.Assign(8, 28, 100, 100);
  Inherited Init(R, AHSB, AVSB);
  BM := GetFreeID;
  if not LoadRasterFile(FN, BM, @LogPalette, DIBType) then Fail;
  BMP := GetImage(BM);
  if (PSimage(BMP)^.NBP = im16) and (MaxColors = 256) then begin
    X := CreateDIBitmap(BMP, 0);
    DisposeImage(BM);
    BMP := X;
    BM := GetFreeID;
    RegisterImageInHeap(BM, BMP);
  end;
  R.B.X := R.A.X + BitmapWidth(BMP);
  R.B.Y := R.A.Y + BitmapHeight(BMP);
  Locate(R);
  if UseOpt then Options := Options or ofSelectable;
  GrowMode := gfGrowHiX + gfGrowHiY;

  if MaxColors = 256 then begin
    {if not Use256ColorOptimization or (BitMapColorType(BMP) = 17) then begin
      if BitMapColorType(BMP) >= 16 then begin
        LogPalette.Mode := 0;
        CreatePalette(LogPalette.Palette, LogPalette, cbwCreate16Map, LogPalette.Colors);
        ReMapImage(BMP, LogPalette.ColorRef);
        DisposePalette(LogPalette);
      end;
    end else}
    if BitMapColorType(BMP) >= 16 then LogPalette.Mode := pmUseRGB + pmOptimize;
    if (UseOpt and (LogPalette.Mode <> 0)) then LogPalette.Mode :=
       LogPalette.Mode or pmKeepFocus;
  end;
  EventMask := EventMask or evBroadCast;
End;

Procedure TBMPView.Draw;
Var
  R, IR  : TRect;
  DX, DY : Integer;
Begin
  if LongInt(LastDelta) <> LongInt(Delta) then begin
    R.Assign(0, 0, Size.X-1, Size.Y-1);
    DY := LastDelta.Y - Delta.Y;
    DX := LastDelta.X - Delta.X;
    IR.Assign(0, 0, Pred(ScreenWidth), Pred(ScreenHeight));
    MakeLocal(IR.A, IR.A); MakeLocal(IR.B, IR.B);
    R.Intersect(IR);
    if DY < 0 then Dec(R.A.Y, DY) else Dec(R.B.Y, DY);
    if DX < 0 then Dec(R.A.X, DX) else Dec(R.B.X, DX);
    Scroll(R.A.X, R.A.Y, R.B.X, R.B.Y, DX, DY);
  end;
  PutBMPPart(BMP, 0, 0, Delta.X, Delta.Y);
End;

Destructor TBMPView.Done;
Begin
  DisposeImage(GetImageID(BMP));
  Inherited Done;
End;

Procedure TBMPView.HandleEvent(var Event : TEvent);
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmRadioButtonPressed) then
    if not UseOpt then begin
      Options := Options and not ofSelectable;
      LogPalette.Mode := LogPalette.Mode and not(pmKeepFocus or pmMonopoly);
      RealizePalette;
    end else begin
      Options := Options or ofSelectable;
      LogPalette.Mode := LogPalette.Mode or (pmKeepFocus or pmMonopoly);
      RealizePalette;
    end;
End;

Procedure  TBMPView.SetState(AState: Word; Enable: Boolean);
Begin
  Inherited SetState(AState, Enable);
  if (AState and sfDragging) <> 0 then
    SetLimit(MaxInteger(0, PSImage(BMP)^.X - Size.X),
             MaxInteger(0, PSImage(BMP)^.Y - Size.Y));
End;


Constructor TBMPDialog.Init;
Var
  R, R1 : TRect;
  HSB, VSB : PScrollBar;
Begin
  R.Assign(8, 28, 100, 100);
  Inherited Init(R, FN, Num);

  Frame^.GetClientExtent(R);
  HSB := New(PScrollBar, Init(R));
  R.A.X := R.B.X - 17;
  VSB := New(PScrollBar, Init(R));

  Inc(Num);
  P := New(PBMPView, Init(FN, HSB, VSB));
  if P = Nil then Fail;
  R.Assign(0, 0, P^.Size.X, P^.Size.Y);
  Desktop^.GetExtent(R1);
  R.Grow(10, 20);
  Inc(R.B.X, 17);
  Inc(R.A.X, 17);
  Dec(R.B.X, R.A.X); R.A.X := 0;
  Dec(R.B.Y, R.A.Y); R.A.Y := 0;
  R.Intersect(R1);
  ZoomRect.Copy(R);
  R.Move(100, 100);
  Locate(R);
  Frame^.GetClientExtent(R);
  P^.Locate(R);
  Insert(P);

  Frame^.GetClientExtent(R);
  Dec(R.B.X, 17);
  R.A.Y := R.B.Y - 18;
  HSB^.Locate(R);
  Insert(HSB);

  Frame^.GetClientExtent(R);
  R.A.X := R.B.X - 18;
  VSB^.Locate(R);
  Insert(VSB);

  P^.SetLimit(MaxInteger(0, PSImage(P^.BMP)^.X - P^.Size.X),
              MaxInteger(0, PSImage(P^.BMP)^.Y - P^.Size.Y));
End;

Procedure TBMPDialog.SizeLimits(var Min, Max: TPoint);
Var
  Y : TRect;
  R, R1 : TRect;
Begin
  Inherited SizeLimits(Min, Max);
  Frame^.GetClientExtent(Y);
  Max.X := MinInteger(Max.X, PSImage(P^.BMP)^.X) + Size.X - Y.B.X + Y.A.X + 17;
  Max.Y := MinInteger(Max.Y, PSImage(P^.BMP)^.Y) + Size.Y - Y.B.Y + Y.A.Y + 17;
  R.A.X := 0; R.A.Y := 0; R.B := Max;
  Desktop^.GetExtent(R1);
  R.Intersect(R1);
  Max := R.B;
End;

Procedure   TBMPDialog.Zoom;
Begin
  Inherited Zoom;
  SetState(sfDragging, True);
  SetState(sfDragging, false);
End;



Procedure TApp.OpenBitMap(var Event : TEvent);
Var
  S : String;
  P : PWindow;
Begin
  S:= '*.BMP;*.PCX;*.GIF';
  if ExecuteDialog(New(PFileDialog, Init(S, 'Load image',
    '~N~ame', fdOkButton, 0)), @S) <> cmCancel then begin
    P := New(PBMPDialog, Init(S));
    if P = Nil then begin
      P := @S;
      MessageBox('%s: not a BMP/PCX/GIF file or no fucking memory', @P,
                               mfError + mfOkButton)
    end else Desktop^.Insert(P);
  end;
End;

Procedure TApp.OutOfMemory;
Begin
  MessageBox('No more fucking memory!', nil, mfError + mfOkButton);
End;


Procedure TApp.InitStatusLine;
Var
  R : TRect;
Begin
  StandardStatusRect(R);
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      NewStatusKey('~Ctrl-F5~ Move', kbCtrlF5, cmResize,
      NewStatusKey('~Alt-F3~ Close', kbAltF3, cmClose,
      NewStatusKey('~F6~ Next', kbF6, cmNext,
      NewStatusKey('~Shift-F6~ Prev', kbShiftF6, cmPrev,
      NewStatusKey('~F3~ Load', kbF3, cmOpen, nil)))))),
    nil)));
End;

Constructor TFuckingStatic.Init;
Var
  R : TRect;
Begin
  R.Assign(8, 28, 136, 206);
  Inherited Init(R);
End;

Procedure TFuckingStatic.Draw;
Var
  I, J : Byte;
Begin
  Bar(0, 0, 128, 50, 7);
  WrStr(0, 0,  '  DK Inc.  SVG', 0);
  WrStr(0, 12, '   BMP viewer', 0);
  WrStr(4, 34, 'current palette', 0);
  for I := 0 to 15 do
    for J := 0 to 15 do
      Bar(J * 8, 50 + I * 8, (J + 1) * 8, 50 +(I + 1) * 8, I * 16 + J);
End;



Var
  T : TApp;
  I : Byte;
  R : TRect;
  P : PView;

Begin
  WriteLn('DK Inc. 1996  Demo image viewer  2:464/46.36@fidonet');
  SystemColors := 16;
 { TryBufferedStrategy := False;}
  T.Init;
  with PApp(Application)^ do begin
    R.Assign(0, 0, 9*8, 0);
    Heap:= New(PHeapView, Init(R));
    Insert(PlaceInStatusLine(Heap,4));
    RAssign(R, 3, 3, 30, 20);
  end;
  P:=New(PDialog, Init(R, 'Options'));
  Dec(PDialog(P)^.Flags, wfClose);
  Dec(PDialog(P)^.Options, ofSelectable);
  Application^.InsertWindow(PDialog(P));
  with PDialog(P)^ do begin
    Insert(New(PFuckingStatic, Init));
    R.Assign(8, 220, 196, 220+18);
    P:=New(PTellingCheckBoxes, Init(R,NewSItem('Optimizing palette',nil)));
    Insert(P);
  end;
  if ParamCount > 0 then for I := 1 to ParamCount do
    Desktop^.Insert(Application^.ValidView(New(PBMPDialog, Init(ParamStr(I)))));
  T.Run;
  T.Done;
  WriteLn('DK Inc. 1996  Demo image viewer  2:464/46.36@fidonet');
End.