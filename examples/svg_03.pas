{$G+}
Uses DOS, Objects, Views, Drivers, Memory, MsgBox, Dialogs, Menus, App,
GDI, svga256, afterdrk;

const
  cmMoveFlame = 119;

type
  PFlameBar = ^TFlameBar;
  TFlameBar = Object(TView)
    Map : Word;
    P   : PImage;
    Constructor Init(var Bounds : TRect);
    Destructor Done; Virtual;
    Procedure HandleEvent(var Event : TEvent); Virtual;
    Procedure Draw; Virtual;
    {procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;}
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
     cmp cl, 100
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


Constructor TFlameBar.Init;
Var
  x : Byte;

Procedure SetRGB(C, R, G, B : Byte);
Begin
  LogPalette.Palette^[C, 1] := R shl 2;
  LogPalette.Palette^[C, 2] := G shl 2;
  LogPalette.Palette^[C, 3] := B shl 2;
End;


Begin
  Inherited Init(Bounds);
  Map := $51AC;
  P := CreateDImageIndirect(320, 200, 256, cbwSetAlloc + cbwAllocFlat);
  if P = Nil then Fail;
  with PSImage(P)^ do begin
    check := imcheck;
    x := 320;
    y := 200;
    nbp := im256;
  end;
  FillChar(PSImage(P)^.Data, 64000, 0);
  CreatePalette(Nil, LogPalette, cbwInit, 128);
  setrgb(0, 0, 0, 0 );
  for x := 1 to 32 do begin
    setrgb(x, (x shl 1)-1, 0, 0 );
    setrgb(x+32, 63, (x shl 1)-1, 0 );
    setrgb(x+64, 63, 63, (x shl 1)-1 );
    setrgb(x+96, 63, 63, 63 );
  end;
  LogPalette.Mode := LogPalette.Mode or pmOptimize or pmUseRGB;
  EventMask := $FFFF;
  TimedMessage(1, @Self, evBroadcast, cmMoveFlame, Nil, 3);
  GrowMode := gfGrowHiX + gfGrowHiY;
End;

Destructor TFlameBar.Done;
Begin
  FreeDImage(P);
  Inherited Done;
End;

Procedure TFlameBar.HandleEvent(var Event : TEvent);
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmMoveFlame) then begin
    ClearEvent(Event);
    TimedMessage(1, Owner, evBroadcast, cmMoveFlame, Nil, 3);
    asm
      les di, Self
      mov si, es:[di].TFlameBar.Map
    end;
    DrawFlameStepSeg(Seg(P^), Ofs(PSImage(P)^.Data));
    asm
      les di, Self
      mov es:[di].TFlameBar.Map, si
    end;
    DrawView;
  end;
End;

Procedure TFlameBar.Draw;
Begin
  StretchBMP(P, 0, 0, Size.X, Size.Y);
End;

{Procedure TFlameBar.CalcBounds;
Begin
  PWindow(Owner)^.Frame^.GetClientExtent(Bounds);
End;}

var
  t:tapplication;
  r:trect;
  p:pdialog;

begin
  t.init;
  R.Assign(100, 100, 430, 330);
  new(p, init(R, 'Flame'));
  p^.flags := p^.flags or wfGrow;
  t.insert(p);
  p^.Frame^.GetClientExtent(R);
  p^.Insert(New(PFlameBar, Init(R)));
  t.run;
  t.done;
end.
