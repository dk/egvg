{-cp -m}
{ $DEFINE SVGADEBUG}
{$DEFINE RUSSIAN}
{$DEFINE USESTANDARDBITMAPS}
Uses Objects, App, Msgbox, Menus, Drivers, Views, Dialogs, EGFont,
     svga256, GDI, Wallpapers, BGIFont, FntLib, AfterDrk, Bitmaps, Streams,
     TrueType, Image, MZResource, EgString, Palettes;

Const
  AndID    = 10030;
  XorID    = 10040;

Type
  TApp = Object(TApplication)
    Constructor Init;
    Procedure   InitMenuBar; Virtual;
    Procedure   InitDesktop; Virtual;
    procedure   InitStatusLine; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
  End;

Constructor TApp.Init;
Var
  LP : TLogPalette;
  fnt: PSimpleFont;
  MZ : PMZResource;
  i : Integer;
  ImgXor, ImgAnd: PImage;
  S : String;

Procedure InsIcon(Title : String; Id : Word);
Var
  D       : PDialog;
  R       : TRect;
  P       : PImage;
Begin
  R.Assign(0, 0, 35 * GetCharWidth, 10 * GetHeight);
  R.Move(Random(40) * GetCharWidth, Random(20) * GetHeight);
  D := New(PDialog, Init(R, Title));
  if Id = 2 then begin
 {   D^.GetClientExtent(R);
    D^.Insert(New(PShade, Init(R, $0708, $0708, 669, 3, shBitmap)));}
    R.Assign(0, 0, 10 * GetCharWidth, 2 * GetHeight);
    R.Move(13 * GetCharWidth, 5 * GetHeight);
    D^.Insert(New(PButton, Init(R, 'Shutdown', cmQuit, bfDefault)));
  end;
  if Id = 1 then begin
    D^.GetClientExtent(R);
    D^.Insert(New(PShade, Init(R, $0708, $0708, 667, 1, shBitmap)));
  end;
{  if Id = 4 then begin
    D^.GetClientExtent(R);
    D^.Insert(New(PShade, Init(R, $0708, $0708, 670, 4, shBitmap)));
  end;}
  if Id = 3 then begin
    D^.GetClientExtent(R);
    D^.Insert(New(PShade, Init(R, $0708, $0708, 668, 2, shStretch)));
    D^.GrowTo(140, 250);
  end;
  D^.Flags := (D^.Flags or wfGrow or wfZoom) and (not wfClose);
  Desktop^.Insert(D);
  D^.SetIcon(AndId + Id,  XorID + Id);
  D^.Iconize;
  D^.Icon^.MoveTo(30, Id * 100 - 20);
End;

Begin
  useDMM := True;
  if LoadGIF('WARPD.GIF', 666, @LP, 0) then begin
    WallPaper := GetImage(666);
    WallPaperPalette := LP.Palette;
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
  RegisterFont(New(PScaledWindowsFont, Init('sserife.fon')), 14);
  SelectFont(GlobalFont.Font);
  Redraw;
  New(MZ, Init);
  if MZ <> Nil then begin
    if MZ^.Read('GS.DLL') then begin
      for i := 1 to 4 do begin
        S := 'ICON_'+Long2Str(1+I)+#0;
        LoadIcon(MZ, MakeIntResource(@S[1]), ImgAnd, ImgXor);
        RegisterImageInHeap(AndID+i, ImgAnd);
        RegisterImageInHeap(XorID+i, ImgXor);
      end;
      RegisterImageInHeap(667, LoadBitMap(MZ, 'BITMAP_2', @LP));
      RegisterPalette(@LP, 1);
      RegisterImageInHeap(668, LoadBitMap(MZ, 'BITMAP_3', @LP));
      RegisterPalette(@LP, 2);
{      RegisterImageInHeap(669, LoadBitMap(MZ, 'BITMAP_4', @LP));
      RegisterPalette(@LP, 3);
      RegisterImageInHeap(670, LoadBitMap(MZ, 'BITMAP_5', @LP));
      RegisterPalette(@LP, 4);}
    end;
    Dispose(MZ, Done);
    IconsFont.Font := 14;
    Dec(IconsFont.Width);
    Lock;
    InsIcon('Applications', 1);
    InsIcon('OS/2 System', 2);
    InsIcon('Shredder', 3);
    InsIcon('Connections', 4);
    UnLock;
  end;
End;

procedure TApp.InitMenuBar;
Begin
  MenuBar := Nil;
End;

procedure TApp.InitDesktop;
Var
  R, R1 : TRect;
Begin
  GetExtent(R);
  New(Desktop, Init(R));
End;

procedure TApp.InitStatusLine;
var
  R: TRect;
begin
  StandardStatusRect(R);
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ ‚λε®¤', kbAltX, cmQuit,
      StdStatusKeys(nil)), nil)));
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      StdStatusKeys(nil)), nil)));
  StatusLine^.Hide;
end;


Procedure   TApp.HandleEvent(var Event : TEvent);

Var
  R : TRect;

Function DoSelectArea : Boolean;
Label failed;
Var
  T : TPaintInfo;
  E : TEvent;
  I : Integer;
Begin
  DoSelectArea := False;
  DefaultPaint(T);
  T.Fore := RGB(255, 255, 255);
  T.LineStyle := lsLinePattern;
  T.LinePattern := psDot;
  T.Operation := XorPut;
  R.A := Event.Where;
  MakeGlobal(R.A, R.A);
  R.B := R.A;
  GDI.Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, T);
  repeat
    GetEvent(E);
    if E.What = evNothing then Continue;
    HideMouse;
    SetOutput(False);
    GDI.Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, T);
    SetOutput(True);
    R.B := E.Where;
    ShowMouse;
    if (E.What and evMouse) = 0 then Goto failed;
    if E.What = evMouseUp then Break;
    HideMouse;
    T.LinePattern := not T.LinePattern;
    SetOutput(False);
    GDI.Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, T);
    SetOutput(True);
    ShowMouse;
  until False;
  DoSelectArea := True;
failed:
  MakeLocal(R.A, R.A);
  MakeLocal(R.B, R.B);
  if R.B.X < R.A.X then begin
    I := R.B.X;
    R.B.X := R.A.X;
    R.A.X := I;
  end;
  if R.B.Y < R.A.Y then begin
    I := R.B.Y;
    R.B.Y := R.A.Y;
    R.A.Y := I;
  end;
End;


Procedure Check(P : PIconizedWindow); Far;
Begin
  if (TypeOf(P^) = TypeOf(TIconizedWindow)) and (P^.Link <> Nil)
     and R.Contains(P^.Origin) then P^.Link^.Restore;
End;

Begin
  Inherited HandleEvent(Event);
  if (Event.What = evMouseDown) and DoSelectArea then Desktop^.ForEach(@Check);
End;

Var
  T : TApp;

Begin
  SelectDriver(GetAppropriateDriver(640, 480, 256, False));
  T.Init;
  T.Run;
  T.Done;
  WriteLn('DK Inc. 1997  2:464/46.36 •®ε¬ !:)');
End.