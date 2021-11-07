{Nifty ScreenSavers by DK}
{$G+}
Unit AfterDrk;

Interface

Uses Objects, GDI, Drivers, Menus, Views, App, Memory;

{1.Flying balls}
Type
  PBall = ^TBall;
  TBall = Object(TObject)
    X, Y   : Integer;
    DX, DY : ShortInt;
    Step, D: Byte;
    Constructor Init(AX, AY : Integer; AStep, AD : Byte; ADX, ADY : ShortInt);
    Procedure GetRect(var R : TRect);
    Procedure Move;
    Procedure Draw(var T : TPaintInfo);
    Procedure Acquire;
  End;

  PFlyingBallsScreenSaver = ^TFlyingBallsScreenSaver;
  TFlyingBallsScreenSaver = Object(TScreenSaver)
    Balls : array[0..5] of PBall;
    BallCount : Byte;
    Constructor Init(ATime : Word);
    Destructor  Done; Virtual;
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

{2. Moving beams}
{Pascalized by DK Inc. 1995 from original COLINES.COM file}
  PBeamsScreenSaver = ^TBeamsScreenSaver;
  TBeamsScreenSaver = Object(TScreenSaver)
    Beams : array[1..3] of Record
      Pel   : Byte;
      Pos   : Word;
      Speed : Integer;
    End;
    Constructor Init(ATime : Word);
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

{3.Flame}
  PFlameScreenSaver = ^TFlameScreenSaver;
  TFlameScreenSaver = Object(TScreenSaver)
    Map : Word;
    Mode : Byte;
    Constructor Init(ATime : Word);
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

Procedure DrawFlameStepSeg(Segment, Offs : Word);

Implementation

{TFlyingBallsScreenSaver}
Constructor TBall.Init;
Begin
  Inherited Init;
  X := AX; Y := AY; Step := AStep; DX := ADX; DY := ADY; D := AD;
End;

Procedure TBall.Acquire;
Begin
  if X > ScreenWidth - 1 then X := ScreenWidth - 1;
  if Y > ScreenHeight - 1 then Y := ScreenHeight - 1;
End;

Procedure TBall.Move;
Begin
  if (X > ScreenWidth + D) or (X < - D) then DX :=  - DX;
  if (Y > ScreenHeight + D) or (Y < - D) then DY :=  - DY;
  Inc(X, DX);
  Inc(Y, DY);
End;

Procedure TBall.GetRect(var R : TRect);
Begin
  R.Assign(X - D - 1, Y - D - 1, X + D + 2, Y + D + 2);
  R.Grow(Abs(DX), Abs(DY));
  if R.A.X < 0 then R.A.X := 0;
  if R.A.Y < 0 then R.A.Y := 0;
  if R.B.X >= ScreenWidth then R.B.X := ScreenWidth;
  if R.B.Y >= ScreenHeight then R.B.Y := ScreenHeight;
End;

Procedure TBall.Draw(var T : TPaintInfo);
Begin
  GDI.FillCircle(X, Y, D, T);
End;


Constructor TFlyingBallsScreenSaver.Init;
Var
  R : TRect;
  I : Byte;
Begin
  R.Copy(AllScreen);
  Inherited Init(R, ssfAllOptions, ATime);
  Balls[0] := New(PBall, Init(320, 200, 1, 26, -1, 1));
  Balls[1] := New(PBall, Init(320, 200, 1, 12, 2, 2));
  BallCount := 2;
End;

Destructor TFlyingBallsScreenSaver.Done;
Var
  I : Byte;
Begin
  for I := 0 to BallCount - 1 do Dispose(Balls[I], Done);
  Inherited Done;
End;

Procedure TFlyingBallsScreenSaver.HandleEvent(var Event : TEvent);
Var
  R : TRect;
  I : Byte;
Begin
  if (Event.What = evCommand) and (Event.Command = cmScreenSave) then begin
    SetPaint(0, 0, CopyPut, psSolid, lwHollow, lsBitMap, fsSolid);
    PaintInfo.Bitmap      := WallPaper;
    PaintInfo.ColorRef    := Desktop^.BackGround^.LogPalette.ColorRef;
    PaintInfo.ClipRect.Assign(0, 0, ScreenWidth-1, ScreenHeight-1);
    for I := 0 to BallCount - 1 do Balls[I]^.Acquire;
  end;
  Inherited HandleEvent(Event);
  if (Event.What = evNothing) and ((State and sfVisible) <> 0) then begin
    PaintInfo.LineStyle := lsPattern;
    PaintInfo.Fore := 0;
    for I := 0 to BallCount - 1 do begin
      Balls[I]^.Draw(PaintInfo);
      Balls[I]^.Move;
    end;

    if PaintInfo.BitMap <> Nil then PaintInfo.LineStyle := lsBitMap
      else PaintInfo.Fore := ColorIndex^[LightGray];

    for I := 0 to BallCount - 1 do Balls[I]^.Draw(PaintInfo);

    for I := 0 to BallCount - 1 do begin
      Balls[I]^.GetRect(R);
      with R do PutBufferPart(A.X, A.Y, B.X, B.Y);
    end;
  end;
End;

{TBeamsScreenSaver}
Const
  MaxBright      = $3f;
  BeamHei        = $4a;

Constructor TBeamsScreenSaver.Init;
Var
  R : TRect;
  I : Integer;
Begin
  R.Copy(AllScreen);
  Inherited Init(R, ssfAllOptions, ATime);
  for I := 1 to 3 do begin
    Beams[I].Pel := 0;
    Beams[I].Pos := 204;
    Beams[I].Speed := - 1 - I;
  end;
End;

Procedure TBeamsScreenSaver.HandleEvent(var Event : TEvent);

Type
  TPEL = Record
    Pel   : Byte;
    Pos   : Word;
    Speed : Integer;
  End;

Var
  Count : Word;

Procedure Upload(var S : TPEL);
Begin
  if (S.Pos + S.Speed <= BeamHei) or
   ((S.Pos + S.Speed > BeamHei) and (S.Pos + S.Speed >= ScreenHeight - BeamHei)) then
     S.Speed := -1 * S.Speed;
  S.Pos := S.Pos + S.Speed;
End;

Procedure DownLoad(var S : TPEL);
Var
  X : Integer;
Begin
  X := Abs(Integer(S.Pos - Count));
  if X > MaxBright then S.Pel := 0 else S.Pel := MaxBright - X;
End;

Procedure MoveBeams;
Var
  I, J : Word;
Begin
  for J := 1 to 3 do Upload(TPEL(Beams[J]));
  WaitForRetrace;
  for I := 0 to ScreenHeight - 1 do begin
    Count := I;
    for J := 1 to 3 do Download(TPEL(Beams[J]));
    Port[$3C8] := 0;
    WaitForVRetrace;
    for J := 1 to 3 do Port[$3C9] := Beams[J].Pel;
  end;
End;

Begin
  Inherited HandleEvent(Event);
  if (Event.What = evNothing) and ((State and sfVisible) <> 0) then MoveBeams;
End;

Constructor TFlameScreenSaver.Init;
Var
  R : TRect;
  I : Integer;
Begin
  R.Copy(AllScreen);
  Inherited Init(R, ssfAllOptions, ATime);
  Map := $51AC;
End;

Procedure DrawFlameStepSeg(Segment, Offs : Word); Assembler;
Asm
  mov ch, 0
  mov ax, Segment
  mov es, ax
 @@x:
   mov cl, 30
   @@y:
       mov al, cl
       xor ah, ah
       mov di, ax
       shl di, 1
       mov al, ch
       dec ax
       shl ax, 1

       shl di, 6
       mov bx, di
       shl bx, 2
       add di, bx
       add di, ax
       mov bx, ax

       add di, Offs  {}
       xor dx, dx
       mov al, dl
       add dl, byte ptr es:[di]
       inc di
       inc di
       adc dh, al
       add dl, byte ptr es:[di]
       adc dh, al
       add dl, byte ptr es:[di+2]
       adc dh, al
       add dl, byte ptr es:[di+642]
       adc dh, al
       shr dx, 2
       sub di, Offs  {}

       or dl, dl
       je @@1
       dec dl
    @@1:
       mov dh, dl
       mov word ptr es:[di-320], dx
       mov word ptr es:[di-640], dx
     inc cl
     cmp cl, 101
     jb @@y

     ror si, 1
     xor si, $aa15
     inc si
     mov ax, si
     and al, 1
     mov ah, al
     shl ah, 2
     add al, ah
     shl al, 5
     mov byte ptr es:[di], al
   inc ch
   cmp ch, 160
   jb  @@x
End;


Procedure TFlameScreenSaver.HandleEvent(var Event : TEvent);
Var
  WasSave : Boolean;

Procedure Restore;
Var
  i : Byte;
Begin
  i := Mode;
  asm
    mov ah, 0
    mov al, i
    int 10h
  end;
  SetVGAPalette(0, 256, PMainPalette);
  Application^.Redraw;
End;

Procedure Save;
var
  x : Integer;
  i : Byte;
  Pal : array[1..128] of TVGARegister;

Procedure SetRGB(C, R, G, B : Byte);
Begin
  Pal[C, 1] := R shl 2;
  Pal[C, 2] := G shl 2;
  Pal[C, 3] := B shl 2;
End;


Begin
  asm
    mov ah, 0fh
    int 10h
    mov i, al
    mov ax, 13h
    int 10h
  end;
  Mode := i;
  for x := 1 to 32 do  begin
    setrgb(x, (x shl 1)-1, 0, 0 );
    setrgb(x+32, 63, (x shl 1)-1, 0 );
    setrgb(x+64, 63, 63, (x shl 1)-1 );
    setrgb(x+96, 63, 63, 63 );
  end;
  SetVGAPalette(0, 128, @Pal);
End;

Begin
  WasSave := False;
  case Event.Command of
  cmScreenSave     : WasSave := True;
  cmScreenRestore  : Restore;
  else end;

  Inherited HandleEvent(Event);

  if WasSave then Save;

  if (Event.What = evNothing) and ((State and sfVisible) <> 0) then asm
    les di, Self
    mov si, es:[di].TFlameScreenSaver.Map
    push SegA000
    push 0
    call DrawFlameStepSeg
    les di, Self
    mov es:[di].TFlameScreenSaver.Map, si
  end;
End;

End.