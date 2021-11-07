{-cp}
Uses Objects, VApp, Bitmaps, SVGA256, GDI, Dialogs, Views, Drivers,
     App, StdDlg, MsgBox, Menus, Flick, GGadgets, Memory;


Type
  PApp = ^TApp;
  TApp = Object(TVApplication)
    Heap: PHeapView;
    Procedure OpenFLI(var Event : TEvent); Virtual cmOpen;
    Procedure InitStatusLine; Virtual;
    Procedure OutOfMemory;    Virtual;
  End;

  PFuckingStatic = ^TFuckingStatic;
  TFuckingStatic = Object(TView)
    Constructor Init;
    Procedure   Draw; Virtual;
  End;


  PFLIDialog = ^TFLIDialog;
  TFLIDialog = Object(TDialog)
    Constructor Init(FN : String);
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
  TimedMessage(1, P, evBroadcast, cmNextFlickFrame, Nil, 0);
End;

Procedure TApp.OpenFLI(var Event : TEvent);
Var
  S : String;
  P : PDialog;
Begin
  S:= '*.FL*';
  if ExecuteDialog(New(PFileDialog, Init('*.FL*', 'Load flick',
    '~N~ame', fdOkButton, 0)), @S) <> cmCancel then begin
    P := New(PFLIDialog, Init(S));
    if P = Nil then begin
      P := @S;
      MessageBox('%s: not a flick file or no memory', @P,
                               mfError + mfOkButton)
    end
    else Desktop^.Insert(P);
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
      NewStatusKey('~F3~ Load', kbF3, cmOpen, nil)),
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
  WrStr(0, 12, '   FLI viewer', 0);
  WrStr(4, 34, 'current palette', 0);
  for I := 0 to 15 do
    for J := 0 to 15 do
      Bar(J * 8, 50 + I * 8, (J + 1) * 8, 50 +(I + 1) * 8, I * 16 + J);
End;

Var
  T : TApp;
  I : Byte;
  R : TRect;

Begin
  T.Init;

  with PApp(Application)^ do begin
    R.Assign(0, 0, 9*8, 0);
    Heap:= New(PHeapView, Init(R));
    Insert(PlaceInStatusLine(Heap,4));
  end;

  Desktop^.Insert(New(PFuckingStatic, Init));
  if ParamCount > 0 then for I := 1 to ParamCount do
    Application^.InsertWindow(New(PFLIDialog, Init(ParamStr(I))));
  T.Run;
  T.Done;
End.