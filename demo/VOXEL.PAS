
type lrgarr=array[0..65534] of byte;

const
 pal:array[1..384] of byte=(
  0,0,0,48,48,48,1,0,43,1,3,43,2,5,44,2,7,44,3,9,45,4,11,46,5,13,47,6,15,48,
  7,17,49,8,19,50,9,21,51,10,22,52,11,24,52,12,26,54,13,28,54,14,30,56,15,32,
  56,16,34,58,17,34,58,17,36,58,18,38,60,19,40,60,20,42,62,21,44,62,10,31,0,
  11,31,0,11,31,1,11,32,1,12,32,1,12,32,2,12,33,2,13,33,2,14,33,3,15,33,3,15,
  34,3,15,34,4,15,35,4,16,35,4,16,35,5,16,36,5,17,36,5,17,36,6,18,37,6,18,38,
  7,19,38,8,20,39,8,20,40,9,21,40,10,22,41,10,22,42,11,23,42,12,24,43,12,24,
  44,13,25,44,14,25,45,14,26,46,15,27,46,16,27,47,17,28,47,18,28,48,19,29,49,
  19,30,49,20,30,50,21,31,51,21,32,51,22,32,52,23,33,53,23,34,53,24,34,54,25,
  35,55,25,36,55,26,36,56,27,37,57,27,38,57,27,39,57,27,41,57,27,42,57,27,43,
  57,27,44,57,27,45,57,27,46,57,27,47,57,27,49,57,27,50,57,27,51,57,27,52,57,
  27,53,57,27,55,57,27,56,57,27,57,57,27,58,57,27,58,57,26,58,57,25,58,57,24,
  58,56,23,58,55,22,58,54,20,58,53,19,58,51,18,58,50,17,58,50,16,58,49,15,58,
  48,14,58,47,13,58,46,12,58,45,11,58,44,11,58,44,10,58,43,10,58,42,9,57,41,
  8,57,40,8,56,39,7,56,38,6,55,37,5,55,35,4,54,33,4,54,31,2,32,32,32,63,63,63,
  63,63,63,63,63,63,63,63,63,48,48,48,63,63,63,63,63,63);

Type
  PLandscape = ^TLandscape;
  TLandscape = Object(TView)
    P         : PImage;
    mp,scr    : ^lrgarr;
    rng       : array[0..320] of byte;
    dir,_x,_y : integer;
    Constructor Init;
    Destructor  Done; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
    procedure   plasma(x1,y1,x2,y2:word);
    procedure   _Draw(xp,yp:integer);
  End;

Const
  FirstRun:Boolean=False;

Constructor TLandscape.Init;
Var
  R : TRect;
Begin
  R.Assign(0, 0, 319, 199);
  R.Move(8, 24);
  Inherited Init(R);
  P := CreateDImageIndirect(320, 200, 256, cbwSetAlloc + cbwAllocFlat);
  if P = Nil then Fail;
  randomize;
  _x:=0;
  _y:=0;
  dir:=0;
  new(mp);
  {$IFDEF DPMI}
  SetSegmentLimit(Seg(MP^), $10000);
  {$ENDIF}
  if mp = nil then begin
    FreeDImage(P);
    Fail;
  end;
  fillchar(mp^,65535,0);
  scr := @PSImage(P)^.PS;
  mp^[$0000]:=128;
  plasma(0,0,256,256);
  _Draw(_x,_y);

  if not FirstRun then begin
    CreatePalette(@Pal, LogPalette, cbwTreatLoColor, 128);
    FirstRun := True;
  end else CreatePalette(@Pal, LogPalette, 0, 128);
  LogPalette.Mode := LogPalette.Mode or pmOptimize or pmUseRGB;
  EventMask := $FFFF;
  Options := Options or ofPreProcess;
End;

Destructor TLandscape.Done;
Begin
  FreeDImage(P);
  Dispose(mp);
  Inherited Done;
End;

function NCol(mc,n,dvd:integer) : integer;
var
loc : integer;

begin
  loc:=(mc+n-random(2*n)) div dvd;
  ncol:=loc;
  if loc>250 then ncol:=250;
  if loc<5 then ncol:=5
end;

procedure TLandscape._Draw(xp,yp:integer);
var
  z,zobs,ix,iy,iy1,iyp,ixp,x,y,s,csf,snf,mpc,i,j:integer;

begin
  fillchar(rng,sizeof(rng),199);
  zobs:=100+mp^[256*yp+xp];
  csf:=round(256*cos(dir/180*pi));
  snf:=round(256*sin(dir/180*pi));
  fillchar(scr^,64000,0);
  for iy:=yp to yp+55 do
    begin
      iy1:=1+2*(iy-yp);
      s:=4+300 div iy1;
        for ix:=xp+yp-iy to xp-yp+iy do
          begin
            ixp:=xp+((ix-xp)*csf+(iy-yp)*snf) shr 8;
            iyp:=yp+((iy-yp)*csf-(ix-xp)*snf) shr 8;
            x:=160+360*(ix-xp) div iy1;
              if (x>=0) and (x+s<=318) then
                begin
                  z:=mp^[iyp shl 8+ixp];
                  mpc:=z shr 1;
                    if z<47 then z:=46;
                    y:=100+(zobs-z)*30 div iy1;
                      if (y<=199) and (y>=0) then for j:=x to x+s do
                        begin
                         for i:=y to rng[j] do scr^[320*i+j]:=mpc;
                           if y<rng[j] then rng[j]:=y
        end;
      end;
    end;
  end;
End;


Procedure TLandscape.HandleEvent(var Event : TEvent);

Procedure DrawIt;
Var
  A : Boolean;
Begin
  dir:=dir mod 360;
  _Draw(_x,_y);
  DrawView{Direct};
End;

Begin
  case Event.What of
  evKeyDown : case Event.KeyCode of
    kbLeft : begin
      dec(dir,10);
      Drawit;
      ClearEvent(Event);
    end;
    kbRight: begin
      inc(dir,10);
      Drawit;
      ClearEvent(Event);
    end;
    kbUp   : begin
      _y:=_y+round(5*cos(dir/180*pi));
      _x:=_x+round(5*sin(dir/180*pi));
      Drawit;
      ClearEvent(Event);
    end;
    kbDown : begin
      _y:=_y-round(5*cos(dir/180*pi));
      _x:=_x-round(5*sin(dir/180*pi));
      Drawit;
      ClearEvent(Event);
    end;
    else end;
  else end;
  Inherited HandleEvent(Event);
End;

procedure TLandscape.plasma(x1,y1,x2,y2:word);

var
  xn,yn,dxy,p1,p2,p3,p4:word;

begin
  if (x2-x1<2) and (y2-y1<2) then exit;

  p1:=mp^[256*y1+x1];
  p2:=mp^[256*y2+x1];
  p3:=mp^[256*y1+x2];
  p4:=mp^[256*y2+x2];

  xn:=(x2+x1) shr 1;
  yn:=(y2+y1) shr 1;

  dxy:=5*(x2-x1+y2-y1) div 3;

  if mp^[256*y1+xn]=0 then mp^[256*y1+xn]:=ncol(p1+p3,dxy,2);
  if mp^[256*yn+x1]=0 then mp^[256*yn+x1]:=ncol(p1+p2,dxy,2);
  if mp^[256*yn+x2]=0 then mp^[256*yn+x2]:=ncol(p3+p4,dxy,2);
  if mp^[256*y2+xn]=0 then mp^[256*y2+xn]:=ncol(p2+p4,dxy,2);

  mp^[256*yn+xn]:=ncol(p1+p2+p3+p4,dxy,4);

  plasma(x1,y1,xn,yn);
  plasma(xn,y1,x2,yn);
  plasma(x1,yn,xn,y2);
  plasma(xn,yn,x2,y2);
end;


Procedure TLandscape.Draw;
Begin
  PutBMP(P, 0, 0);
End;

Procedure LoadLandscape;
Var
  P : PDialog;
  R : TRect;
  V : PView;
Begin
  V := New(PLandscape, Init);
  if V = Nil then begin
    Application^.OutOfMemory;
    Exit;
  end;

  R.Assign(0, 0, 336, 230);
  R.Move(100, 100);
  New(P, Init(R, 'Landscape'));
  P^.Insert(V);
  P^.SelectNext(False);
  Desktop^.Insert(P);
end;
