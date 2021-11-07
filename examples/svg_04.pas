{-cp -m}
Uses Objects, svga256, GDI, Drivers, Views, Dialogs, App, EGInline, Bitmaps,
     EgString, EGFont, StdDlg, WStdDlg, MsgBox, Image, FntLib, TrueType;


Type
  PEPanel = ^TEPanel;
  TEPanel = Object(TScroller)
    Pane       : PImage;
    MaxX, MaxY : Word;
    TPI        : TPaintInfo;
    DrawOverride : Boolean;
    Constructor Init(var R : TRect; AMaxX, AMaxY : Word; AHSB, AVSB : PScrollBar);
    Destructor  Done; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
    Procedure   SetDevice(Screen : Boolean);
    Procedure   SetState(AState: Word; Enable: Boolean); virtual;
  End;

  TBRec = Record
    Def, Press : PImage;
  End;

  PETools = ^TETools;
  TETools = Object(TView)
    Mode : Word;
    Buttons : array[0..15] of TBRec;
    Texts   : array[0..2] of TBRec;
    CurrentLine : String;
    Constructor Init(var R : TRect);
    Destructor  Done; Virtual;
    Procedure   Draw; Virtual;
    Procedure   SetState(AState: Word; Enable: Boolean); virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   SelectTool(Tool : Word);
  End;

  PEColor = ^TEColor;
  TEColor = Object(TView)
    Fore, Back : Word;
    AutoorgX   : Integer;
    Constructor Init(var R : TRect);
    Procedure   Draw; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Function    MapColor(Num : Word) : Word;
    Procedure   SetState(AState: Word; Enable: Boolean); virtual;
    Procedure   SetColor(AFore, ABack : Word);
  End;

  PEDialog = ^TEDialog;
  TEDialog = Object(TDialog)
    Panel : PEPanel;
    Tools : PETools;
    Color : PEColor;
    Global: TPaintInfo;
    GFont : TFont;
    Constructor Init;
    Destructor  Done; Virtual;
    Procedure   SizeLimits(var Min, Max: TPoint); virtual;
    Procedure   Zoom; Virtual;
  End;

Const
  mdFreeHand = 0;
  mdLinear   = 1;
  mdRect     = 2;
  mdBar      = 3;
  mdCircle   = 4;
  mdFillCirc = 5;
  mdRoundRect= 6;
  mdRoundBar = 7;

  mdLinePat  = 8;
  mdErase    = 9;
  mdEraseC   = 10;
  mdText     = 11;
  mdTag      = 12;
  mdFlood    = 13;

  cmToolChanged  = $10CA;
  cmColorChanged = $10CB;
  cmPanelChanged = $10CD;
  cmPaintChanged = $10CE;
  cmMoreColors   = $10CF;
  cmWriteLn      = $10D0;

Constructor TEDialog.Init;
Var
  R         : TRect;
  HSB, VSB  : PScrollBar;
  C         : PView;
Begin
  Desktop^.GetExtent(R);
  Inherited  Init(R, '');
  DefaultPaint(Global);
  DefaultFont(Font);

  Frame^.GetClientExtent(R);
  Inc(R.A.X, 64);
  Dec(R.B.X, 16);
  Dec(R.B.Y, 34);
  R.A.Y := R.B.Y - 16;
  HSB := New(PScrollBar, Init(R));
  Insert(HSB);

  Frame^.GetClientExtent(R);
  Dec(R.B.X, 2);
  Dec(R.B.Y, 32);
  R.A.X := R.B.X - 16;
  VSB := New(PScrollBar, Init(R));
  Insert(VSB);

  Frame^.GetClientExtent(R);
  Inc(R.A.X, 64);
  Dec(R.B.X, 18);
  Dec(R.B.Y, 50);
  New(Panel, Init(R, 300, 200, HSB, VSB));
  if Panel = Nil then Fail;
  Insert(Panel);

  Frame^.GetClientExtent(R);
  R.B.X := R.A.X + 64;
  Dec(R.B.Y, 18);
  New(Tools, Init(R));
  Insert(Tools);

  Frame^.GetClientExtent(R);
  R.A.Y := R.B.Y - 32;
  Inc(R.A.X, 64);
  New(Color, Init(R));
  Insert(Color);

  Frame^.GetClientExtent(R);
  R.A.Y := R.B.Y - 16;
  R.A.X := R.B.X - 40;
  C := New(PButton, Init(R, 'More', cmMoreColors, bfNormal+bfBroadCast));
  C^.GrowMode := gfGrowAll;
  Insert(C);

  Flags := Flags or wfGrow or wfZoom;
  SelectNext(False);
End;

Destructor  TEDialog.Done;
Begin
  if Global.Bitmap <> Nil then DisposeImage(GetImageID(Global.Bitmap));
  Inherited Done;
End;

Procedure   TEDialog.SizeLimits(var Min, Max: TPoint);
Begin
  Inherited SizeLimits(Min, Max);
  if Min.X < 200 then Min.X := 200;
  if Min.Y < 200 then Min.Y := 200;
End;

Procedure   TEDialog.Zoom;
Begin
  Inherited Zoom;
  SetState(sfDragging, True);
  SetState(sfDragging, false);
End;

Var
  BufferedSVGA : Boolean;

Constructor TEPanel.Init;
Begin
  Inherited Init(R, AHSB, AVSB);
  MaxX := AMaxX; MaxY := AMaxY;
  Pane := CreateDImage(MaxX, MaxY);
  if Pane = Nil then begin
    case MessageBox('Not enough memory for '+Long2Str(MaxX)+' x '+Long2Str(MaxY)+' image.'+
                     ^M'Try 100x50 ?',
              nil, mfError + mfOkCancel) of
    cmOk : begin
      MaxX := 100;
      MaxY := 50;
      Pane := CreateDImage(MaxX, MaxY);
      if Pane = Nil then begin
        MessageBox('Cannot allocate even 100x50.'^M' Exiting now.', nil, mfError+mfOkButton);
        Fail;
      end;
    end;
    else Fail; end;
  end;
  TPI  := PaintInfo;
  TPI.Device := Pane;
  GrowMode := GrowMode or (gfGrowHiX + gfGrowHiY);
  SetLimit(MaxInteger(0,MaxX - Size.X), MaxInteger(0,MaxY - Size.Y));
  DrawOverride := False;
End;

Destructor TEPanel.Done;
Begin
  FreeDImage(Pane);
  Inherited Done;
End;

Procedure  TEPanel.SetDevice(Screen : Boolean);
Var
  Org : TPoint;
  R   : TRect;
Begin
  if Screen then begin
    Owner^.MakeGlobal(Origin, Org);
    TPI.ClipRect.Assign(Org.X, Org.Y, Org.X + MinWord(Size.X, MaxX-1), Org.Y + MinWord(Size.Y, MaxY-1));
    SetOutput(False);
    TPI.Device := Nil;
    if TPI.LineStyle in [lsBitmap, lsBitmapOrg] then begin
      TPI.LineStyle := lsBitmapOrg;
      Owner^.MakeGlobal(Origin, TPI.BitmapOrg);
    end;
    {BufferedStrategy := False;}
  end else begin
    {BufferedStrategy := BufferedSVGA;}
    TPI.ClipRect.Assign(0, 0, MaxX-1, MaxY-1);
    SetOutput(True);
    TPI.Device := Pane;
    if TPI.LineStyle = lsBitmapOrg then TPI.LineStyle := lsBitmap;
  end;
End;

Procedure  TEPanel.SetState(AState: Word; Enable: Boolean);
Begin
  Inherited SetState(AState, Enable);
  if (AState and sfDragging) <> 0 then SetLimit(MaxInteger(0,MaxX - Size.X), MaxInteger(0,MaxY - Size.Y));
End;

Procedure TEPanel.HandleEvent(var Event : TEvent);
var
  LastMouse : TPoint;
  FirstPress: TPoint;
  R         : TRect;
  LastOp : Byte;

Procedure WaitForMove;
Begin
  Repeat
    GetMouseEvent(Event);
  Until (Event.What in [evMouseUp, evMouseMove, evMouseDown]);
End;

Procedure FreeHand;
Begin
  InvalidateSelf;
  LastMouse := FirstPress;
  Repeat
    HideMouse;
    SetDevice(True);
    gdi.FillCircle(LastMouse.X, LastMouse.Y, TPI.LineWidth div 2, TPI);
    gdi.FillCircle(Event.Where.X, Event.Where.Y, TPI.LineWidth div 2, TPI);
    gdi.Line(LastMouse.X, LastMouse.Y, Event.Where.X, Event.Where.Y, TPI);
    ShowMouse;
    MakeLocal(Event.Where, R.B);
    MakeLocal(LastMouse,   R.A);
    SetDevice(False);
    R.Move(Delta.X, Delta.Y);
    gdi.FillCircle(R.A.X, R.A.Y, TPI.LineWidth div 2, TPI);
    gdi.FillCircle(R.B.X, R.B.Y, TPI.LineWidth div 2, TPI);
    gdi.Line(R.A.X, R.A.Y, R.B.X, R.B.Y, TPI);
    LastMouse := Event.Where;
    WaitForMove;
    if Event.What = evMouseUp then Break;
  Until False;
  if BufferedStrategy then DrawView;
  ClearEvent(Event);
End;

Procedure Figure(Mode : Byte);

Procedure XorFigure(Mode : Byte);
Var
  P : TPoint;
  TP : TPaintInfo;
Begin
  DefaultPaint(TP);
  TP.Operation := XORPut;
  TP.LinePattern := psDot;
  SetDevice(True);
  TP.ClipRect.Copy(TPI.ClipRect);
  HideMouse;
  P.X := LastMouse.X - FirstPress.X;
  P.Y := LastMouse.Y - FirstPress.Y;
  if P.X <> 0 then P.X := P.X div 4;
  if P.Y <> 0 then P.Y := P.Y div 4;
  case Mode of
  mdLinear          : begin
    gdi.FillCircle(FirstPress.X, FirstPress.Y, TP.LineWidth div 2, TP);
    gdi.FillCircle(Event.Where.X, Event.Where.Y, TP.LineWidth div 2, TP);
    gdi.Line(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, TP);
  end;
  mdRect, mdBar     : gdi.Rectangle(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, TP);
  mdCircle, mdFillCirc: gdi.Ellipse(FirstPress.X + P.X, FirstPress.Y + P.Y, Abs(P.X*2), Abs(P.Y*2), TP);
  mdRoundRect, mdRoundBar: gdi.RoundRect(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, Abs(P.X), Abs(P.Y), TP);
  else end;
  ShowMouse;
  SetDevice(False);
End;

Procedure PutFigure(Mode : Byte);
Var
  P : TPoint;
  LW  : Word;
Begin
  TPI.Operation := LastOp;
  LW := TPI.LineWidth;

  SetDevice(True);
  HideMouse;
  P.X := LastMouse.X - FirstPress.X;
  P.Y := LastMouse.Y - FirstPress.Y;
  if P.X <> 0 then P.X := P.X div 4;
  if P.Y <> 0 then P.Y := P.Y div 4;
  if Mode in [mdRoundBar, mdFillCirc] then TPI.LineWidth := 0;
  case Mode of
  mdLinear   : begin
    gdi.Line(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, TPI);
    gdi.FillCircle(FirstPress.X, FirstPress.Y, TPI.LineWidth div 2, TPI);
    gdi.FillCircle(LastMouse.X, LastMouse.Y, TPI.LineWidth div 2, TPI);
  end;
  mdRect     : gdi.Rectangle(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, TPI);
  mdBar      : gdi.barStyle(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, TPI);
  mdCircle   : gdi.Ellipse(FirstPress.X + P.X, FirstPress.Y + P.Y, Abs(P.X*2), Abs(P.Y*2), TPI);
  mdFillCirc : gdi.FillEllipse(FirstPress.X + P.X, FirstPress.Y + P.Y, Abs(P.X*2), Abs(P.Y*2), TPI);
  mdRoundRect: gdi.RoundRect(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, Abs(P.X), Abs(P.Y), TPI);
  mdRoundBar : gdi.RoundBar(FirstPress.X, FirstPress.Y, LastMouse.X, LastMouse.Y, Abs(P.X), Abs(P.Y), TPI);
  else end;
  ShowMouse;
  SetDevice(False);
  MakeLocal(FirstPress, R.A);
  MakeLocal(LastMouse,  R.B);
  R.Move(Delta.X, Delta.Y);
  case Mode of
  mdLinear   : begin
    gdi.FillCircle(R.A.X, R.A.Y, TPI.LineWidth div 2, TPI);
    gdi.FillCircle(R.B.X, R.B.Y, TPI.LineWidth div 2, TPI);
    gdi.Line(R.A.X, R.A.Y, R.B.X, R.B.Y, TPI);
  end;
  mdRect     : gdi.Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, TPI);
  mdBar      : gdi.barStyle(R.A.X, R.A.Y, R.B.X, R.B.Y, TPI);
  mdCircle   : gdi.Ellipse(R.A.X + P.X, R.A.Y + P.Y, Abs(P.X*2), Abs(P.Y*2), TPI);
  mdFillCirc : gdi.FillEllipse(R.A.X + P.X, R.A.Y + P.Y, Abs(P.X*2), Abs(P.Y*2), TPI);
  mdRoundRect: gdi.RoundRect(R.A.X, R.A.Y, R.B.X, R.B.Y, Abs(P.X), Abs(P.Y), TPI);
  mdRoundBar : gdi.RoundBar(R.A.X, R.A.Y, R.B.X, R.B.Y, Abs(P.X), Abs(P.Y), TPI);
  else end;
  TPI.LineWidth := LW;
End;

Begin
  Repeat
    GetMouseEvent(Event);
  Until Event.What = evMouseUp;
  LastOp := TPI.Operation;
  Repeat
    LastMouse := Event.Where;
    XorFigure(Mode);
    Repeat
      GetMouseEvent(Event);
    Until Event.What <> evNothing;
    XorFigure(Mode);
    if (Event.What = evMouseDown) and (Event.Buttons = mbRightButton) then Break;
    if ((Event.What = evMouseDown) and (Event.Buttons = mbLeftButton)) or (Event.What = evMouseUp) then begin
      PutFigure(Mode);
      Break;
    end;
    TPI.Operation := LastOp;
  Until False;
  TPI.Operation := LastOp;
  if BufferedStrategy then DrawView;
  ClearEvent(Event);
End;

Procedure Erase(Mode : Byte);
var
  P : TPaintInfo;

Begin
  InvalidateSelf;
  P := TPI;
  DefaultPaint(TPI);
  TPI.Fore := P.Back;
  TPI.LineStyle := lsPattern;
  LastMouse := FirstPress;
  if Mode = mdEraseC then begin
    TPI.Operation := UserBitBlt;
    SetColorBitBlt(TPI.Fore, Event.Buttons = mbLeftButton);
  end;
  Repeat
    HideMouse;
    SetDevice(True);
    gdi.BarStyle(LastMouse.X, LastMouse.Y, LastMouse.X+P.LineWidth, LastMouse.Y+P.LineWidth, TPI);
    ShowMouse;
    MakeLocal(LastMouse,   R.A);
    SetDevice(False);
    R.Move(Delta.X, Delta.Y);
    gdi.BarStyle(R.A.X, R.A.Y, R.A.X+P.LineWidth, R.A.Y+P.LineWidth, TPI);
    LastMouse := Event.Where;
    WaitForMove;
    if Event.What = evMouseUp then Break;
  Until False;
  if BufferedStrategy then DrawView;
  ClearEvent(Event);
  TPI := P;
  if Mode = mdEraseC then SetUserBitBltProc(Nil);
End;

Function GetSelectRect(var R : TRect) : Boolean;
Label NoWay;
Var
  P : TPaintInfo;
  I : Integer;
Begin
  P := TPI;
  DefaultPaint(TPI);
  TPI.LinePattern := psDot;
  TPI.Operation   := XORPut;
  GetSelectRect := False;
  Repeat
    GetMouseEvent(Event);
  Until Event.What = evMouseUp;

  Repeat
    LastMouse := Event.Where;
    HideMouse;
    SetDevice(True);
    gdi.Rectangle(R.A.X, R.A.Y, LastMouse.X, LastMouse.Y, TPI);
    SetDevice(False);
    ShowMouse;
    Repeat
      GetMouseEvent(Event);
    Until Event.What <> evNothing;
    HideMouse;
    SetDevice(True);
    gdi.Rectangle(R.A.X, R.A.Y, LastMouse.X, LastMouse.Y, TPI);
    SetDevice(False);
    ShowMouse;
    if (Event.What = evMouseDown) and (Event.Buttons = mbRightButton) then Goto NoWay;
    if ((Event.What = evMouseDown) and (Event.Buttons = mbLeftButton)) or (Event.What = evMouseUp) then Break;
  Until False;
  R.B := LastMouse;
  Repeat
    GetMouseEvent(Event);
  Until (Event.What = evMouseUp);
  if (R.B.X = R.A.X) or (R.B.Y = R.A.Y) then Goto NoWay;
  GetSelectRect := True;

  if R.A.X > R.B.X then begin
    I := R.B.X;
    R.B.X := R.A.X;
    R.A.X := I;
  end;
  if R.A.Y > R.B.Y then begin
    I := R.B.Y;
    R.B.Y := R.A.Y;
    R.A.Y := I;
  end;
NoWay:
  TPI := P;
End;

Procedure Tag;
Var
  R, S : TRect;
  P : TPaintInfo;
  E : TEvent;
  I : PImage;
  J, OS : TPoint;
  Drag, Lapse, FirstRun : Boolean;
Begin
  R.A := FirstPress;
  R.B.X := R.A.X + 30;
  R.B.Y := R.A.Y + 30;
  if not GetSelectRect(R) then Exit;
  I := CreateDImage(R.B.X - R.A.X, R.B.Y - R.A.Y);
  if I = Nil then begin
    MessageBox('Rectangle''s too large.', nil, mfError+mfOkButton);
    Exit;
  end;
  {GetDIBitmap(I, R.A.X, R.A.Y);}
  S.Copy(R);
  MakeLocal(S.A, S.A);
  MakeLocal(S.B, S.B);
  S.Move(Delta.X, Delta.Y);
  ImplantDIBitmap(Pane, I, 0, 0, S.A.X, S.A.Y, S.B.X-S.A.X+1, S.B.Y-S.A.Y+1, CopyPut, Nil);
  FirstRun := True;

  Repeat
    DefaultPaint(P);
    P.LinePattern := psDot;
    P.Operation   := XORPut;
    SetDevice(True);
    HideMouse;
    P.ClipRect := TPI.ClipRect;
    gdi.Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, P);
    ShowMouse;
    SetDevice(False);

    Repeat
      GetEvent(E);
    Until (E.What = evMouseDown);
    if FirstRun then Lapse := (GetShiftState and 3) = 0 else Lapse := False;

    SetDevice(True);
    HideMouse;
    P.ClipRect := TPI.ClipRect;
    gdi.Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, P);
    ShowMouse;
    SetDevice(False);

    if not R.Contains(E.Where) then begin
      if not FirstRun then begin
        S.Move(Delta.X, Delta.Y);
        if not Drag then ImplantDIBitmap(I, Pane, S.A.X, S.A.Y, 0, 0, S.B.X-S.A.X+1, S.B.Y-S.A.Y+1, TPI.Operation, Nil);
      end;
      PutEvent(E);
      FreeDImage(I);
      Exit;
    end;

    Drag := (E.Buttons and mbRightButton) <> 0;

    OS := Size;
    DrawOverride := True;
    GrowTo(MinInteger(MaxX, Size.X), MinInteger(MaxY, Size.Y));
    DrawOverride := False;

    J.X := R.A.X - E.Where.X;
    J.Y := R.A.Y - E.Where.Y;
    S.Copy(R);
    MakeLocal(S.A, S.A);
    MakeLocal(S.B, S.B);

    if Lapse then begin
      DefaultPaint(P);
      P.LineStyle := lsPattern;
      P.Fore := 0;
      SetDevice(True);
      InvalidateSelf;
      HideMouse;
      P.ClipRect := TPI.ClipRect;
      gdi.BarStyle(R.A.X, R.A.Y, R.B.X, R.B.Y, P);
      ShowMouse;
      SetDevice(False);
      P.Device := Pane;
      P.ClipRect := TPI.ClipRect;
      S.Move(Delta.X, Delta.Y);
      gdi.BarStyle(S.A.X, S.A.Y, S.B.X, S.B.Y, P);
      S.Move(-Delta.X, -Delta.Y);
    end;

    Repeat
      WaitForMove;
      if (Event.Buttons = 0) then Break;

      if (Event.What = evMouseMove) then begin
        if not Drag then begin
          ValidateSelf;
          InvalidateRect(S);
          DrawInvalidated;
        end;
        Inc(Event.Where.X, J.X);
        Inc(Event.Where.Y, J.Y);
        R.Move(Event.Where.X - R.A.X, Event.Where.Y - R.A.Y);
        S.Copy(R);
        MakeLocal(S.A, S.A);
        MakeLocal(S.B, S.B);
        if not Drag then begin
          HideMouse;
          SetDevice(True);
          InvalidateSelf;
          PutBMPOp(I, S.A.X, S.A.Y, TPI.Operation);
          SetDevice(False);
          ShowMouse;
        end else begin
          S.Move(Delta.X, Delta.Y);
          ImplantDIBitmap(I, Pane, S.A.X, S.A.Y, 0, 0, S.B.X-S.A.X+1, S.B.Y-S.A.Y+1, TPI.Operation, Nil);
          S.Move(-Delta.X, -Delta.Y);
          ValidateSelf;
          InvalidateRect(S);
          DrawInvalidated;
        end;
      end;
    Until (E.What = evMouseUp);
    {S.Move(Delta.X, Delta.Y);
    if not Drag then ImplantDIBitmap(I, Pane, S.A.X, S.A.Y, 0, 0, S.B.X-S.A.X+1, S.B.Y-S.A.Y+1, TPI.Operation, Nil);}
    DrawOverride := True;
    GrowTo(OS.X, OS.Y);
    DrawOverride := False;
    SetDevice(False);
    if BufferedStrategy then DrawView;
    FirstRun := False;
  Until False;
End;

Procedure Flood;
Begin
  SetMouseWait;
  SetDevice(False);
  MakeLocal(Event.Where, Event.Where);
  FloodFillArea(Event.Where.X + Delta.X, Event.Where.Y + Delta.Y, TPI);
  DrawView;
End;

Var
  F, B : Word;
  PS   : PString;

Begin
  Inherited HandleEvent(Event);
  case Event.What of
  evMouseDown: begin
    FirstPress := Event.Where;
    case PEDialog(Owner)^.Tools^.Mode of
    mdFreeHand : FreeHand;
    mdLinear..mdRoundBar : Figure(PEDialog(Owner)^.Tools^.Mode);
    mdErase, mdEraseC  : Erase(PEDialog(Owner)^.Tools^.Mode);
    mdTag              : Tag;
    mdFlood            : Flood;
    else end;
    Message(Owner, evBroadCast, cmPanelChanged, @Self);
  end;
  evBroadCast :
    case Event.Command of
    cmColorChanged : begin
      TPI.Fore := PEDialog(Owner)^.Global.Fore;
      TPI.Back := PEDialog(Owner)^.Global.Back;
    end;
    cmPaintChanged : begin
      F := TPI.Fore; B := TPI.Back;
      TPI := PEDialog(Owner)^.Global;
      Font := PEDialog(Owner)^.GFont;
      TPI.Fore := F; TPI.Back := B;
    end;
    cmWriteLn :  begin
      PS := Event.InfoPtr;
      Repeat
        GetMouseEvent(Event);
        if (Event.What = evMouseDown) and (Event.Buttons = mbRightButton) then Exit;
      Until Event.What = evMouseDown;
      SetDevice(True);
      HideMouse;
      SelectFont(Font.Font);
      EgFont.WrStr(Event.Where.X, Event.Where.Y, PS^, TPI);
      ShowMouse;
      SetDevice(False);
      MakeLocal(Event.Where, Event.Where);
      EgFont.WrStr(Event.Where.X + Delta.X, Event.Where.Y + Delta.Y, PS^, TPI);
      if BufferedStrategy then DrawView;
    end;
    else end;
  else end;
End;

Procedure TEPanel.Draw;
Var
  R, IR  : TRect;
  DX, DY : Integer;
Begin
  if DrawOverride then Exit;
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
  Bar(0, 0, Size.X, Size.Y, 5);
  PutBMPPart(Pane, 0, 0, Delta.X, Delta.Y);
End;

Constructor TETools.Init;
Var
  P       : PImage;
  I, X, Y : Word;
Begin
  R.B.Y := R.A.Y + 32 * 8;
  Inherited Init(R);
  GrowMode := 0;
  Mode := 0;
  CurrentLine := '';
  if not LoadBMP('BUTTON16.BMP', 811, Nil, im16) then Exit;
  P := GetImage(811);
  for I := 0 to 15 do begin
    X := I div 8;
    Y := I mod 8;
    Buttons[I].Def := CreateDImage(32, 32);
    ImplantDIBitmap(P, Buttons[I].Def, 0, 0, 1 + X * 64, 1 + Y * 32, 32, 32, CopyPut, Nil);
    Buttons[I].Press := CreateDImage(32, 32);
    ImplantDIBitmap(P, Buttons[I].Press, 0, 0, 33 + X * 64, 1 + Y * 32, 32, 32, CopyPut, Nil);
  end;

  X := 3;
  for I := 0 to 2 do begin
    Y := I + 5;
    Texts[I].Def := CreateDImage(32, 32);
    ImplantDIBitmap(P, Texts[I].Def, 0, 0, 1 + X * 64, 1 + Y * 32, 32, 32, CopyPut, Nil);
    Texts[I].Press := CreateDImage(32, 32);
    ImplantDIBitmap(P, Texts[I].Press, 0, 0, 33 + X * 64, 1 + Y * 32, 32, 32, CopyPut, Nil);
  end;
  FreeDImage(P);
End;

Destructor TETools.Done;
Var
  I : Word;
Begin
  for I := 2 downto 0 do begin
    FreeDImage(Texts[I].Press);
    FreeDImage(Texts[I].Def);
  end;
  for I := 15 downto 0 do begin
    FreeDImage(Buttons[I].Press);
    FreeDImage(Buttons[I].Def);
  end;
  Inherited Done;
End;

Procedure TETools.HandleEvent(var Event : TEvent);
Var
  Mouse: TPoint;
  ClickRect: TRect;
  Down: Boolean;
  Num, Old : Word;
  CX, CY : Word;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evMouseDown then begin
    CX := Size.X div 2;
    CY := Size.Y div 8;
    GetExtent(ClickRect);
    Inc(ClickRect.A.X);
    Dec(ClickRect.B.X);
    Dec(ClickRect.B.Y);
    MakeLocal(Event.Where, Mouse);
    if not ClickRect.Contains(Mouse) then begin
      ClearEvent(Event);
      Exit;
    end;
    {Num := (Mouse.X div 32) * 8 + (Mouse.Y div 32);}
    Num := (Mouse.X div CX) * 8 + (Mouse.Y div CY);
    if Num > 15 then begin
      ClearEvent(Event);
      Exit;
    end;
    Inc(ClickRect.B.X);
    ClickRect.Assign(Num div 8 * cx, Num mod 8 * cy, Num div 8 * cx + cx, Num mod 8 * cy + cy);
    Down := False;
    Old := Mode;
    repeat
      MakeLocal(Event.Where, Mouse);
      if Down <> ClickRect.Contains(Mouse) then  begin
        Down := not Down;
        if Down then Mode := Num else Mode := Old;
        InvalidateRect(ClickRect);
        DrawInvalidated;
    end;
    until not MouseEvent(Event, evMouseMove);
    ClickRect.Assign(Old div 8 * cx, Old mod 8 * cy, Old div 8 * cx + cx, Old mod 8 * cy + cy);
    Mode := 16;
    InvalidateRect(ClickRect);
    DrawInvalidated;
    Mode := Old;
    if Down then SelectTool(Num);
    ClickRect.Assign(Mode div 8 * cx, Mode mod 8 * cy, Mode div 8 * cx + cx, Mode mod 8 * cy + cy);
    InvalidateRect(ClickRect);
    DrawInvalidated;
    ValidateSelf;
  end;

End;

{start of incorporated section - local paint dialog/etc}
Type
  PLineWidth = ^TLineWidth;
  TLineWidth = Object(TView)
    SC : PScrollBar;
    Data : ^TPaintInfo;
    Constructor Init(var R : TRect; ASC : PScrollBar; var AData : TPaintInfo);
    Procedure   Draw; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
  End;

Constructor TLineWidth.Init;
Begin
  Inherited Init(R);
  SC := ASC;
  Data := @AData;
  SC^.SetParams(51-Data^.LineWidth, 1, 50, 5, 1);
  Options := Options or ofSelectable;
  EventMask := $FFFF;
End;

Procedure TLineWidth.Draw;
Var
  S : String[5];
Begin
  PaintInfo.LineWidth   := Data^.LineWidth;
  PaintInfo.Fore := 0;
  PaintInfo.Back := 15;
  Bar(0, 0, Size.X, Size.Y, 15);
  Line(0, Size.Y div 2, Size.X, Size.Y div 2);
  SelectFont(GlobalFont.Font);
  S := Long2Str(Data^.LineWidth);
  Bar((Size.X - Length(S) * 8) div 2, (Size.Y - 16) div 2, (Size.X + Length(S) * 8) div 2, (Size.Y + 16) div 2, 15);
  WrStr((Size.X - Length(S) * 8) div 2, (Size.Y - 16) div 2, S, 0);
End;

Procedure TLineWidth.HandleEvent;
Var
  W : Word;
Begin
  Inherited HandleEvent(Event);
  case Event.What of
  evKeyDown : begin
    W := Data^.LineWidth;
    case Event.KeyCode of
    kbUp   : begin
      if W < 50 then Inc(W);
      ClearEvent(Event);
    end;
    kbDown : begin
      if W > 1 then Dec(W);
      ClearEvent(Event);
    end;
    else end;
    Data^.LineWidth := W;
    SC^.SetValue(51-W);
  end;
  evBroadCast : if (Event.Command = cmScrollBarChanged) and (Event.InfoPtr = SC) then begin
    Data^.LineWidth := 51-SC^.Value;
    DrawView;
  end;
  else end;
End;

Type
  PLinePat = ^TLinePat;
  TLinePat = Object(TView)
    Data : ^TPaintInfo;
    Pf   : array[0..5] of Word;
    Constructor Init(var R : TRect; var AData : TPaintInfo);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
  End;

Constructor TLinePat.Init;
Begin
  Inherited Init(R);
  Data := @AData;
  Pf[0] := psNull;
  Pf[1] := psSolid;
  Pf[2] := psDash;
  Pf[3] := psDot;
  Pf[4] := psDashDot;
  Pf[5] := psDashDotDot;
  Options := Options or ofSelectable;
End;

Procedure TLinePat.Draw;
Var
  I, J : Word;
  S : Boolean;
  C : array[Boolean] of Word;
Begin
  C[True] := 0; C[False] := 15;
  Bar(0, 0, Size.X, Size.Y, 7);
  Rectangle(0, 0, Size.X, Size.Y, 1, 8, 15);
  for I := 0 to 15 do begin
    S := Data^.LinePattern and (Word(1) shl I) <> 0;
    Bar(2+I * 8, 2+0, 2+(I + 1) * 8, 2+8, C[S]);
    if not s then Rectangle(2+I * 8, 2+0, 2+(I + 1) * 8, 2+8, 1, 0, 0);
  end;
  for J := 0 to 5 do begin
    for I := 0 to 15 do begin
      S := Pf[J] and (Word(1) shl I) <> 0;
      Bar(2+I * 8, 16 + J*10, 2+(I + 1) * 8,24  + J*10, C[S]);
      if not s then Rectangle(2+I * 8, 16 +J*10, 2+(I + 1) * 8, 24 + J*10, 1, 0, 0);
    end;
  end;
End;

Procedure TLinePat.HandleEvent(var Event : TEvent);
Var
  Mouse : TPoint;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evMouseDown then begin
    MakeLocal(Event.Where, Mouse);
    ClearEvent(Event);
    if Mouse.Y < 11 then begin
      Mouse.X := (Mouse.X - 2) div 8;
      if Mouse.X > 15 then Exit;
      Data^.LinePattern := Data^.LinePattern xor (Word(1) shl Mouse.X);
      DrawView;
      Message(Owner, evBroadCast, cmPaintChanged, Data);
    end else if Mouse.Y > 16 then begin
      Mouse.Y := (Mouse.Y - 16) div 10;
      if Mouse.Y > 5 then Exit;
      Data^.LinePattern := Pf[Mouse.Y];
      DrawView;
      Message(Owner, evBroadCast, cmPaintChanged, Data);
    end;
  end;
End;

Const
  cmRadioButtonPressed = $10DD;
  cmLoadBMP            = $10DE;
  cmClearBMP           = $10DF;

Type
  PBrush = ^TBrush;
  TBrush = Object(TView)
    Data : ^TPaintInfo;
    Mode, Perc : Word;
    SB   : PScrollBar;
    CBU  : PRadioButtons;
    Constructor Init(var R : TRect; var AData : TPaintInfo; ASC : PScrollBar; ACB : PRadioButtons);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
  End;

Constructor TBrush.Init;
Begin
  Inherited Init(R);
  Data := @AData;
  Options := Options or ofSelectable;
  Mode := 1;
  Perc := 0;
  SB   := ASC;
  SB^.SetParams(Perc, 0, 64, 8, 1);
  CBU := ACB;
  CBU^.Value := Mode;
  Eventmask := $FFFF;
End;

Procedure TBrush.Draw;
Var
  I, J : Word;
  S : Boolean;
  C : array[Boolean] of Word;
Begin
  C[True] := 0; C[False] := 15;
  Bar(0, 0, Size.X, Size.Y, 7);
  Rectangle(0, 0, Size.X, Size.Y, 1, 8, 15);
  for I := 0 to 7 do
    for J := 0 to 7 do begin
      S := Data^.Pattern[J] and (Word(1) shl I) <> 0;
      Bar(2+I * 8, 2+J * 8, 2+(I + 1) * 8, 2+J * 8 + 8, C[S]);
      if not s then Rectangle(2+I * 8, 2+J * 8, 2+(I + 1) * 8, 2+J * 8 + 8, 1, 0, 0);
    end;

  PaintInfo.Fore := ColorIndex^[Black];
  PaintInfo.Back := ColorIndex^[White];
  PaintInfo.LineStyle := lsPattern;
  PaintInfo.Operation := CopyPut;
  if Mode = 1 then begin
    for I := 0 to 3 do for J := 0 to 3 do begin
      Move(FillPatterns[J*4+I], PaintInfo.Pattern, 8);
      Rectangle(2+I*16, 80+J*16, 2+16+I*16, 80+16+J*16, 1, 0, 0);
      BarStyle(2+I*16, 80+J*16, 2+16+I*16, 80+16+J*16);
    end;
  end else begin
    PaintInfo.Pattern := FillPatterns[fsSolid];
    I := Perc * 101 div 64;
    if I > 100 then I := 100;
    WrStr(30, 80, Long2Str(I)+'%', 0);
  end;
End;

Procedure TBrush.HandleEvent(var Event : TEvent);
Var
  Mouse : TPoint;
  Old, I, J : Word;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evMouseDown then begin
    MakeLocal(Event.Where, Mouse);
    if Mouse.Y < 65 then begin
      Mouse.X := (Mouse.X - 2) div 8;
      Mouse.Y := (Mouse.Y - 2) div 8;
      if Mouse.X > 7 then Exit;
      Data^.Pattern[Mouse.Y] := Data^.Pattern[Mouse.Y] xor (Word(1) shl Mouse.X);
      DrawView;
      ClearEvent(Event);
      Message(Owner, evBroadCast, cmPaintChanged, Data);
    end else if Mouse.Y > 80 then begin
      if Mode <> 1 then Exit;
      Mouse.Y := (Mouse.Y - 80) div 16;
      Mouse.X := Mouse.X div 16;
      if Mouse.Y > 3 then Exit;
      if Mouse.X > 3 then Exit;
      Move(FillPatterns[Mouse.X + Mouse.Y * 4], Data^.Pattern, 8);
      ClearEvent(Event);
      DrawView;
      Message(Owner, evBroadCast, cmPaintChanged, Data);
    end;
  end;

  if (Event.What = evBroadCast) then
  case Event.Command of
  cmRadioButtonPressed:
    if Event.InfoPtr = CBU then begin
    Old  := Mode;
    Mode := CBU^.Value;
    if (Mode = 0) and (Old <> 0) then begin
      Perc := 0;
      for I := 0 to 7 do for J := 0 to 7 do
        if Data^.Pattern[J] and (Word(1) shl I) <> 0 then Inc(Perc);
      SB^.SetValue(Perc);
      GrowTo(Size.X, Size.Y-30);
    end;
    if (Mode = 1) and (Old <> 1) then GrowTo(Size.X, Size.Y+30);
    ClearEvent(Event);
  end;
  cmScrollBarChanged: if Event.InfoPtr = SB then begin
    Perc := SB^.Value;
    MakePercentilePattern(Data^.Pattern, Perc);
    DrawView;
    ClearEvent(Event);
    Message(Owner, evBroadCast, cmPaintChanged, Data);
  end;
  else end;
End;

Type
  PTellingRadioButtons = ^TTellingRadioButtons;
  TTellingRadioButtons = Object(TRadioButtons)
    procedure Press(Item: Integer); virtual;
    procedure HandleEvent(var Event : TEvent); Virtual;
    Procedure SetData(var Rec); Virtual;
    Procedure GetData(var Rec); Virtual;
  End;

Procedure TTellingRadioButtons.Press(Item : Integer);
Begin
  Inherited Press(Item);
  Message(Owner, evBroadCast, cmRadioButtonPressed, @Self);
End;

procedure TTellingRadioButtons.HandleEvent(var Event : TEvent);
var
  Is : Boolean;
Begin
  Is := (State and sfFocused <> 0) and (Event.What = evKeyDown) and
     (Event.ScanCode in [Hi(kbLeft), Hi(kbRight), Hi(kbUp), Hi(kbDown)]);
  Inherited HandleEvent(Event);
  if Is then Message(Owner, evBroadCast, cmRadioButtonPressed, @Self);
End;

Procedure TTellingRadioButtons.SetData(var Rec);
Var
  D : Word;
Begin
  case Byte(Rec) of
  lsLinePattern : D := 0;
  lsPattern     : D := 1;
  else D := 2; end;
  Value := D;
End;

Procedure TTellingRadioButtons.GetData(var Rec);
Var
  D : Word;
Begin
  case Value of
  0 : D := lsLinePattern;
  1 : D := lsPattern;
  else D := lsBitmap; end;
  Byte(Rec) := D;
End;

Type
  PTestPane = ^TTestPane;
  TTestPane = Object(TView)
    Procedure Draw; Virtual;
  End;

Procedure TTestPane.Draw;
Var
  R : TPaintInfo;
Begin
  if (PaintInfo.LineStyle = lsBitmap) and (PaintInfo.Bitmap = Nil) then begin
    R := PaintInfo;
    DefaultPaint(PaintInfo);
    PaintInfo.Fore := 0;
    Bar(0, 0, Size.X, Size.Y, 15);
    Line(0, 0, Size.X, Size.Y);
    Line(0, Size.Y, Size.X, 0);
    PaintInfo := R;
  end else begin
    Bar(0, 0, Size.X, Size.Y, 15);
    BarStyle(0, 0, Size.X, Size.Y);
  end;
  Rectangle(0, 0, Size.X, Size.Y, 1, 8, 15);
End;

Type
  PBMPTestPane = ^TBMPTestPane;
  TBMPTestPane = Object(TView)
    Procedure Draw; Virtual;
  End;

Procedure TBMPTestPane.Draw;
Var
  R : TPaintInfo;
Begin
  if (PaintInfo.LineStyle = lsBitmap) and (PaintInfo.Bitmap = Nil) then begin
    R := PaintInfo;
    DefaultPaint(PaintInfo);
    PaintInfo.Fore := 0;
    Bar(0, 0, Size.X, Size.Y, 15);
    Line(0, 0, Size.X, Size.Y);
    Line(0, Size.Y, Size.X, 0);
    PaintInfo := R;
  end else begin
    PaintInfo.LineStyle := lsBitmap;
    BarStyle(0, 0, Size.X, Size.Y);
  end;
  Rectangle(0, 0, Size.X, Size.Y, 1, 8, 15);
End;

Type
  POpListBox = ^TOpListBox;
  TOpListBox = Object(TListBox)
    P : ^TPaintInfo;
    constructor Init(var Bounds: TRect; AScrollBar:PScrollBar; var AP : TPaintInfo);
    procedure FocusItem(Item: Integer); virtual;
  End;

Constructor TOpListBox.Init(var Bounds: TRect; AScrollBar:PScrollBar; var AP : TPaintInfo);
Var
  S     : PStringCollection;
  Op    : Byte;
Begin
  Inherited Init(Bounds, 1, AScrollBar);
  P := @AP;
  Op := P^.Operation;

  New(S, Init(21, 1));
  S^.AtInsert(0, NewStr('Normal'));
  S^.AtInsert(1, NewStr('^'));
  S^.AtInsert(2, NewStr('&'));
  S^.AtInsert(3, NewStr('|'));
  S^.AtInsert(4, NewStr('!'));
  S^.AtInsert(5, NewStr('!Black'));
  S^.AtInsert(6, NewStr('!Dest^'));
  S^.AtInsert(7, NewStr('!Dest&'));
  S^.AtInsert(8, NewStr('!Dest|'));
  S^.AtInsert(9, NewStr('!Src^'));
  S^.AtInsert(10, NewStr('!Src&'));
  S^.AtInsert(11, NewStr('!Src|'));
  S^.AtInsert(12, NewStr('!^'));
  S^.AtInsert(13, NewStr('!&'));
  S^.AtInsert(14, NewStr('!|'));
  S^.AtInsert(15, NewStr('!Black^'));
  S^.AtInsert(16, NewStr('!Black&'));
  S^.AtInsert(17, NewStr('!Black|'));
  S^.AtInsert(18, NewStr('NOP'));
  S^.AtInsert(19, NewStr('Black'));
  S^.AtInsert(20, NewStr('White'));
  NewList(S);

  FocusItem(Op);
End;

Procedure TOpListBox.FocusItem(Item: Integer);
Begin
  Inherited FocusItem(Item);
  P^.Operation := Item;
  Message(Owner, evBroadCast, cmPaintChanged, Nil);
End;

Type
  PLinePatDialog = ^TLinePatDialog;
  TLinePatDialog = Object(TDialog)
    P  : ^TPaintInfo;
    TP : TPaintInfo;
    Pat : PLinePat;
    Wid : PLineWidth;
    Bru : PBrush;
    Typ : PTellingRadioButtons;
    Tst : PTestPane;
    BMP : PBMPTestPane;
    LBx : POpListBox;
    Constructor Init(var T : TPaintInfo);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
  End;

Constructor TLinePatDialog.Init;
Var
  R : TRect;
  C, DP : PView;
Begin
  R.Assign(0, 0, 520, 250);
  Inherited Init(R, 'Draw settings');
  Options := Options or ofCentered;
  P := @T;
  TP:= T;

  R.Assign(60, 46, 76, 116);
  C := New(PScrollBar, Init(R));
  Insert(C);
  R.Assign(15, 46, 60, 116);
  Wid := New(PLineWidth, Init(R, PScrollBar(C), TP));
  C := Wid;
  Insert(C);
  R.Assign(15, 28, 100, 46);
  Insert(New(PLabel, Init(R, '~W~idth', C)));

  R.Assign(90, 46, 222, 122);
  Pat := New(PLinePat, Init(R, TP));
  Insert(Pat);
  R.Assign(90,  28, 190, 46);
  Insert(New(PLabel, Init(R, 'Pattern', Pat)));

  R.Assign(230, 170, 297, 185);
  C := New(PScrollBar, Init(R));
  Insert(C);

  R.Assign(230, 200, 300, 216);
  DP := New(PTellingRadioButtons, Init(R,
    NewSItem('~%~',
    NewSItem('~D~',
  nil))));
  PCluster(DP)^.StepColumn := 4;
  Insert(DP);

  R.Assign(230, 46, 298, 192);
  Bru := New(PBrush, Init(R, TP, PSCrollBar(C), PRadioButtons(DP)));
  Insert(Bru);
  R.Assign(230,  28, 290, 46);
  Insert(New(PLabel, Init(R, 'Brush', Bru)));

  R.Assign(15, 145, 120, 200);
  Typ := New(PTellingRadioButtons, Init(R,
    NewSItem('~L~ine',
    NewSItem('B~r~ush',
    NewSItem('~B~itmap',
  nil)))));
  Insert(Typ);
  R.Assign(15, 128, 120, 146);
  Insert(New(PLabel, Init(R, '~F~ill style', Typ)));
  Typ^.SetData(TP.LineStyle);

  R.Assign(130, 145, 200, 200);
  Tst := New(PTestPane, Init(R));
  Tst^.PaintInfo := TP;
  Insert(Tst);
  R.Assign(130, 128, 200, 144);
  Insert(New(PStaticText, Init(R, 'Test')));

  R.Assign(310, 46, 415, 70);
  Insert(New(PButton, Init(R, 'L~o~ad bitmap', cmLoadBMP, bfNormal)));
  R.Assign(310, 76, 415, 100);
  Insert(New(PButton, Init(R, 'Cl~e~ar bitmap', cmClearBMP, bfNormal)));

  R.Assign(310, 128, 415, 230);
  BMP := New(PBMPTestPane, Init(R));
  BMP^.PaintInfo.Bitmap    := TP.Bitmap;
  BMP^.PaintInfo.LineStyle := lsBitmap;
  Insert(BMP);
  R.Assign(310, 110, 400, 126);
  Insert(New(PStaticText, Init(R, 'Bitmap')));

  R.Assign(490, 46, 506, 230);
  C := New(PScrollBar, Init(R));
  Insert(C);
  R.Assign(420, 46, 490, 230);
  LBx := New(POpListBox, Init(R, PScrollBar(C), TP));
  Insert(LBx);
  R.Assign(420, 28, 506, 43);
  Insert(New(PLabel, Init(R, 'Lo~g~ic mode', LBx)));

  R.Assign(0, 0, 80, 32);
  R.Move(20, 205);
  C := MakeOKButton(R, cmOk, bfDefault);
  Insert(C);
  R.Assign(0, 0, 80, 32);
  R.Move(100, 205);
  C := MakeCancelButton(R, cmCancel, bfNormal);
  Insert(C);
  SelectNext(False);
End;


Procedure TLinePatDialog.HandleEvent(var Event : TEvent);

Procedure LoadBitmap;
Var
  S : String;
  P : PImage;
  L : Integer;
Begin
  S:= '*.BMP';
  if Application^.ExecuteDialog(New(PFileDialog, Init('*.BMP', 'Load bitmap pattern',
    '~N~ame', fdOkButton, 0)), @S) <> cmCancel then begin
    L := GetFreeID;
    if LoadBMP(S, L, Nil, im16) then begin
      P := GetImage(L);
      if TP.Bitmap <> Nil then DisposeImage(GetImageID(TP.Bitmap));
      TP.Bitmap := P;
      BMP^.PaintInfo.Bitmap := P;
      Tst^.PaintInfo.Bitmap := P;
      BMP^.DrawView;
      if TP.LineStyle = lsBitmap then Tst^.DrawView;
    end else MessageBox('Cannot load '+S, nil, mfError+mfOkButton);
  end;
End;

Begin
  if (Event.Command = cmOK) then P^ := TP;
  if (Event.Command = cmCancel) then if (TP.Bitmap <> Nil) and (TP.Bitmap <> P^.Bitmap) then
    DisposeImage(GetImageID(TP.Bitmap));
  Inherited HandleEvent(Event);
  if (Event.What = evBroadCast) then begin
    if (Event.Command = cmRadioButtonPressed) and (Event.InfoPtr = Typ) then begin
      Event.Command := cmPaintChanged;
      Typ^.GetData(TP.LineStyle);
    end;
    if Event.Command = cmPaintChanged then begin
      Tst^.PaintInfo := TP;
      Tst^.DrawView;
      ClearEvent(Event);
    end;
  end;

  if (Event.What = evCommand) then begin
    case Event.Command of
    cmLoadBMP : begin
      LoadBitmap;
      ClearEvent(Event);
    end;
    cmClearBMP : begin
      if TP.Bitmap <> Nil then begin
        FreeDImage(TP.Bitmap);
        TP.Bitmap := Nil;
        BMP^.PaintInfo.Bitmap := Nil;
        Tst^.PaintInfo.Bitmap := Nil;
        BMP^.DrawView;
        if TP.LineStyle = lsBitmap then Tst^.DrawView;
      end;
    end;
    else end;
  end;
End;

Type
  PColorBar = ^TColorBar;
  TColorBar  = Object(TView)
    R, G, B : Byte;
    Constructor Init(var Bounds : TRect);
    Procedure   Draw; Virtual;
    Procedure   SetColor(_R, _G, _B : Byte);
  End;

  PColorDialog = ^TColorDialog;
  TColorDialog = Object(TDialog)
    TP : ^TPaintInfo;
    Ctrl : array[1..3] of Record
             Scroll : PScrollBar;
             Value  : PParamText;
           end;
    ColorBar : PColorBar;
    Constructor Init(var T : TPaintInfo);
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

Const
  LastRGB : array[1..3] of Byte = (0,0,0);

Constructor TColorDialog.Init(var T : TPaintInfo);
Var
  R : TRect;
  P : PView;
  I : Byte;
Const
  RGBS : array[1..3] of Char = 'RGB';
Begin
  R.Assign(100, 100, 415, 250);
  Inherited Init(R, 'Colors');
  Options := Options or ofCentered;
  R.Assign(20, 30, 220, 46);
  TP := @T;

  for I := 1 to 3 do begin
    R.Assign(8, 7 + I * 20, 20, 23 + I * 20);
    P := New(PStaticText, Init(R, RGBS[I]));
    Insert(P);
    R.Assign(20, 6 + I * 20, 220, 22 + I * 20);
    P := New(PScrollBar, Init(R));
    PScrollbar(P)^.SetParams(0, 0, 255, 10, 1);
    Insert(P);
    Ctrl[I].Scroll := PScrollBar(P);
    Ctrl[I].Scroll^.Value := LastRGB[I];
    R.Assign(224, 6 + I * 20, 252, 22 + I * 20);
    P := New(PParamText, Init(R, '%d', 1));
    P^.SetData(Ctrl[I].Scroll^.Value);
    Insert(P);
    Ctrl[I].Value := PParamText(P);
  end;

  R.Assign(252, 26, 300, 82);
  New(ColorBar, Init(R));
  Insert(ColorBar);
  ColorBar^.SetColor(Ctrl[1].Scroll^.Value, Ctrl[2].Scroll^.Value, Ctrl[3].Scroll^.Value);

  R.Assign(0, 0, 80, 32);
  R.Move(20, 100);
  P := MakeOKButton(R, cmOk, bfDefault);
  Insert(P);
  R.Assign(0, 0, 80, 32);
  R.Move(100, 100);
  P := MakeCancelButton(R, cmCancel, bfNormal);
  Insert(P);

  SelectNext(False);
End;

Procedure TColorDialog.HandleEvent(var Event : TEvent);
Var
  I : Byte;
Begin
  if (Event.What = evCommand) and (Event.Command = cmOK) then with ColorBar^ do begin
    ColorBar^.SetColor(Ctrl[1].Scroll^.Value, Ctrl[2].Scroll^.Value, Ctrl[3].Scroll^.Value);
    MakeDither(R, G, B, 256, TP^, Nil);
    LastRGB[1] := R; LastRGB[2] := G; LastRGB[3] := B;
    TP^.LineStyle := lsPattern;
  end;
  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmScrollBarChanged) then begin
    for I := 1 to 3 do Ctrl[I].Value^.DrawView;
    ColorBar^.SetColor(Ctrl[1].Scroll^.Value, Ctrl[2].Scroll^.Value, Ctrl[3].Scroll^.Value);
  end;
End;

Constructor TColorBar.Init;
Begin
  Inherited Init(Bounds);
  LogPalette.Mode := pmUseRGB or pmHiColor;
End;

Procedure TColorBar.Draw;
Begin
  if MaxColors <= 256 then begin
    MakeDither(R, G, B, 256, PaintInfo, Nil);
    BarStyle(0, 0, Size.X, Size.Y)
  end else
    if MaxColors <= 65536 then
      Bar(0, 0, Size.X, Size.Y, HiColor(R, G, B))
    else Bar(0, 0, Size.X, Size.Y, TrueColor(R, G, B))
End;

Procedure TColorBar.SetColor;
Begin
  R := _R; G := _G; B := _B;
  DrawView;
End;


Type
  PPopupButton = ^TPopupButton;
  TPopupButton = Object(TView)
    Press, Def : PImage;
    Cmd        : Word;
    Down       : Boolean;
    Constructor Init(var R : TRect; APress, ADef : PImage; ACmd : Word);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
  End;

Constructor TPopupButton.Init;
Begin
  Inherited Init(R);
  Press := APress; Def := ADef;
  Cmd := ACmd;
  Down := False;
  Options := Options or ofPreProcess;
End;

Procedure TPopupButton.Draw;
Begin
  if Down then PutBMP(Press, 0, 0) else PutBMP(Def, 0, 0);
End;

Procedure TPopupButton.HandleEvent(var Event : TEvent);
Var
  Mouse: TPoint;
  ClickRect: TRect;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evMouseDown then begin
    GetExtent(ClickRect);
    Inc(ClickRect.A.X);
    Dec(ClickRect.B.X);
    Dec(ClickRect.B.Y);
    MakeLocal(Event.Where, Mouse);
    if not ClickRect.Contains(Mouse) then begin
      ClearEvent(Event);
      Exit;
    end;
    Down := False;
    repeat
      MakeLocal(Event.Where, Mouse);
      if Down <> ClickRect.Contains(Mouse) then  begin
        Down := not Down;
        InvalidateRect(ClickRect);
        DrawInvalidated;
    end;
    until not MouseEvent(Event, evMouseMove);
    if Down then Message(Owner, evCommand, Cmd, @Self);
  end;
End;

Type
  PTextsPopup = ^TTextsPopup;
  TTextsPopup = Object(TDialog)
    Bts : array[0..2] of PPopupButton;
    Constructor Init(var R : TRect; var Texts : array of TBRec);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
  End;


Constructor TTextsPopup.Init;
var
  I : Word;
Begin
  Inherited Init(R, '');
  Flags := 0;
  R.Assign(2, 1, 34, 33);
  for I := 0 to 2 do begin
    Bts[i] := New(PPopupButton, Init(R, Texts[I].Press, Texts[I].Def, 78 + I));
    Insert(Bts[I]);
    R.Move(31, 0);
  end;
  SelectNext(False);
  Options := Options and not (ofPreProcess) or ofPostProcess;
End;

Procedure TTextsPopup.HandleEvent(var Event : TEvent);
Var
  R : TRect;
  M : TPoint;
Begin
  if (Event.What = evMouseDown) then begin
    GetExtent(R);
    MakeLocal(Event.Where, M);
    if not R.Contains(M) then begin
      EndModal(cmCancel);
      ClearEvent(Event);
    end;
  end;

  if (Event.What = evCommand) then begin
    if Event.Command in [78..80] then begin
      ClearEvent(Event);
      EndModal(Event.Command - 77);
    end;
  end;
  Inherited HandleEvent(Event);
End;
{end of incorporated section}

Procedure TETools.SelectTool(Tool : Word);
Var
  Save, Xm : Word;
  Ok   : Boolean;
  S    : String;
  R    : TRect;

Begin
  if Tool in [mdLinePat, mdText] then begin
    Save := Mode;
    Mode := Tool;
    DrawView;
    Ok := False;
    case Tool of
    mdLinePat   : Ok := Application^.ExecuteDialog(New(PLinePatDialog,
                        Init(PEDialog(Owner)^.Global)), Nil) = cmOk;
    mdText      : begin
      R.Assign(64, 128, 161, 163);
      Owner^.MakeGlobal(R.A, R.A);
      Owner^.MakeGlobal(R.B, R.B);
      Xm := Application^.ExecuteDialog(New(PTextsPopup, Init(R, Texts)), Nil);
      case Xm of
      1 : InputBox('Line', 'Enter text', CurrentLine, 255);
      2 : Message(Owner, evBroadcast, cmWriteLn, @CurrentLine);
      3 : Ok := Application^.ExecuteDialog(New(PFontDialog,
              Init), @PEDialog(Owner)^.GFont) = cmOk;
      else end;
    end;
    else end;
    Mode := Save;
    DrawView;
    if Ok then Message(Owner, evBroadCast, cmPaintChanged, Nil);
  end else begin
    Mode := Tool;
    DrawView;
    Message(Owner, evBroadCast, cmToolChanged, @Self);
  end;
End;

Procedure TETools.Draw;
Var
  I : Word;
  CX, CY : Word;
Begin
  CX := Size.X div 2;
  CY := Size.Y div 8;
  for I := 0 to 15 do begin
    {if Mode = I then
         PutBMP(Buttons[I].Press, I div 8 * 32, I mod 8 * 32)
    else PutBMP(Buttons[I].Def,   I div 8 * 32, I mod 8 * 32);}
    if Mode = I then
         StretchBMP(Buttons[I].Press, I div 8 * CX, I mod 8 * CY, CX, CY)
    else StretchBMP(Buttons[I].Def,   I div 8 * CX, I mod 8 * CY, CX, CY);
  end;
End;

Procedure  TETools.SetState(AState: Word; Enable: Boolean);
Var
  R : TRect;
Begin
  Inherited SetState(AState, Enable);
  if (AState and sfDragging) <> 0 then begin
    PWindow(Owner)^.Frame^.GetClientExtent(R);
    GrowTo(Size.X, MinInteger(R.B.Y - R.A.Y, 32 * 8));
  end;
End;


Constructor TEColor.Init;
Begin
  Inherited Init(R);
  Fore := ColorIndex^[15];
  Back := 0;
  GrowMode := gfGrowLoX + gfGrowLoY;
  AutoorgX := Origin.X;
  EventMask := $FFFF;
  LogPalette.Mode := LogPalette.Mode or pmHiColor;
End;

Procedure TEColor.Draw;
Var
  I, J : Word;
  CX, CY : Word;
Begin
  Bar(0, 0, Size.X, Size.Y, 7);
  CX := (Size.X - 32) div 16;
  CY := Size.Y div 2;
  Bar(0, 0, 31, 31, ColorIndex^[15]);
  Bar(6, 6, 25, 25, MapColor(Fore));
  Rectangle(6, 6, 25, 25, 3, MapColor(Back), MapColor(Back));
  for I := 0 to 1 do for J := 0 to 15 do begin
    Bar(J * CX + 32, I * CY, (J + 1) * CX + 32, (I + 1) * CY, ColorIndex^[MapColor(I * CY + J)]);
    Rectangle(J * CX + 32, I * CY, (J + 1) * CX + 33, (I + 1) * CY+1, 1, ColorIndex^[0], ColorIndex^[0]);
  end;
End;

Function TEColor.MapColor(Num : Word) : Word;
Begin
  MapColor := Num;
End;

Procedure TEColor.HandleEvent(var Event : TEvent);
Var
  ClickRect : TRect;
  Mouse     : TPoint;
  Num       : Word;
  F         : Boolean;
  CX, CY    : Word;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evMouseDown then begin
    CX := (Size.X - 32) div 16;
    CY := Size.Y div 2;
    GetExtent(ClickRect);
    Inc(ClickRect.A.X, 32);
    Dec(ClickRect.B.X);
    Dec(ClickRect.B.Y);
    MakeLocal(Event.Where, Mouse);
    if not ClickRect.Contains(Mouse) then begin
      ClearEvent(Event);
      Exit;
    end;
    Num := ((Mouse.X - 32) div CX) + (Mouse.Y div CY) * 16;
    if Num > 31 then begin
      ClearEvent(Event);
      Exit;
    end;
    if Event.Buttons = mbRightButton then Back := ColorIndex^[Num] else Fore := ColorIndex^[Num];
    SetColor(Fore, Back);
  end;

  if (Event.What = evBroadcast) and (Event.Command = cmMoreColors) then begin
    if Application^.ExecuteDialog(New(PColorDialog, Init(PEDialog(Owner)^.Global)), Nil) = cmOk then begin
      SetColor(PEDialog(Owner)^.Global.Fore, PEDialog(Owner)^.Global.Back);
      Message(Owner, evBroadCast, cmPaintChanged, Nil);
    end;
    ClearEvent(Event);
  end;
End;

Procedure  TEColor.SetState(AState: Word; Enable: Boolean);
Var
  R : TRect;
Begin
  Inherited SetState(AState, Enable);
  if (AState and sfDragging) <> 0 then begin
    PWindow(Owner)^.Frame^.GetClientExtent(R);
    GrowTo(MinInteger(R.B.X - R.A.X - AutoorgX, 32 * 17), MinInteger(R.B.Y - R.A.Y, 32));
    MoveTo(AutoOrgX, Origin.Y);
  end;
End;

Procedure  TEColor.SetColor(AFore, ABack : Word);
Begin
  Fore := AFore;
  Back := ABack;
  PEDialog(Owner)^.Global.Fore := Fore;
  PEDialog(Owner)^.Global.Back := Back;
  Message(Owner, evBroadCast, cmColorChanged, Nil);
  DrawView;
End;

Var
  T : TApplication;
  fnt : PScaledSimpleFont;

Begin
  TryBufferedStrategy := False;
  T.Init;

  New(fnt, Init(@Small^, 'Small'));
  RegisterFont(fnt, 2);
  New(fnt, Init(@Courier^, 'Courier'));
  RegisterFont(fnt, 3);
  New(fnt, Init(@Large^, 'Large courier'));
  RegisterFont(fnt, 4);
  New(PTrueTypeFont(fnt), Init('c:\windows\system\aricyr.ttf'));
  RegisterFont(fnt, 5);


  BufferedSVGA := BufferedStrategy;
  T.ExecuteDialog(New(PEDialog, Init), Nil);
  T.Done;
End.