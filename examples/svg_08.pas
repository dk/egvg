{-cp}
{$F+,I-}
Uses DOS, Objects, Views, Drivers, Memory, Dialogs, App,
GDI, svga256, EGFont, EGString;

Type
  PGV = ^TGV;
  TGV = Object(TView)
    Constructor Init(var R : TRect);
    Procedure Draw; Virtual;
    Procedure HandleEvent(var Event : TEvent);Virtual;
  End;

Constructor TGV.Init;
Begin
  Inherited Init(R);
  Options := Options or ofSelectable;
  FillChar(PaintInfo.Pattern, 8, 0);
  PaintInfo.LineStyle := lsPattern;
  PaintInfo.Fore := 15;
  PaintInfo.Back := 0;
End;

Procedure TGV.Draw;
Var
  I, J : Word;
  H, V : Word;
Begin
  H := (Size.X - 140) div 8;
  V := (Size.Y - 20) div 8;
  Bar(0, 0, Size.X, Size.Y, 7);
  for i := 0 to 8 do HLine(1, 1 + i * V, Size.X - 140, 0);
  for i := 0 to 8 do VLine(1 + i * H, 1, Size.Y - 20, 0);
  for I := 0 to 7 do begin
    for j := 0 to 7 do
      if (PaintInfo.Pattern[i] and (1 shl j)) <> 0 then
        Bar(2 + J * H, 2 + I * V, (J + 1) * H - 1, (I + 1) * V - 1, 9);
    WrStr(Size.X - 120, I * V, HexB(PaintInfo.Pattern[i]), 0);
  end;
  PaintInfo.Fore := 15;
  PaintInfo.Back := 0;

  BarStyle(Size.X - 80, 10, Size.X - 5, Size.Y - 100);
End;

Procedure TGV.HandleEvent(var Event : TEvent);
Var
  Mouse : TPoint;
  F : Text;
  I : Byte;
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evMouseDown) or (Event.What = evMouseAuto) then begin
    MakeLocal(Event.Where, Mouse);
    Mouse.X := (Mouse.X - 1) div ((Size.X - 140) div 8);
    Mouse.Y := (Mouse.Y - 1) div ((Size.Y - 20) div 8);
    if (Mouse.X >= 0) and (Mouse.X < 8)
       and (Mouse.Y >= 0) and (Mouse.Y < 8) then begin
       PaintInfo.Pattern[Mouse.Y] := PaintInfo.Pattern[Mouse.Y] xor (1 shl Mouse.X);
       DrawView;
       ClearEvent(Event);
    end;
  end;
  if (Event.What = evKeyDown) and (Event.KeyCode = kbF2) then begin
    Assign(F, 'gridda');
    Append(F);
    for I := 0 to 7 do Write(F, HexB(PaintInfo.Pattern[i]), ' ');
    WriteLn(F);
    Close(F);
    ClearEvent(Event);
  end;
End;

Var
  T : TApplication;
  R : TRect;
  P : PDialog;

Begin
  T.Init;
  R.Assign(20, 30, 400, 300);
  New(P, Init(R, ''));
  with P^ do begin
    Frame^.GetClientExtent(R);
    R.Grow(-3, -3);
    Insert(New(PGV, Init(R)));
  end;
  T.ExecuteDialog(P, nil);
  T.Done;
End.