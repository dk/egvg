{$A+,B-,E+,F+,G+,I-,N+,O-,P-,Q-,R-,S-,T-,V-,X+,Y-}
                                UNIT Labels;
                                Interface
Uses GDI,Views,Objects,Drivers,Dialogs,BitMaps,Fields;
Const cmUpPoint = 50000; cmDownPoint = 50001; cmInValid = 50002;

Type PReal = ^Real;

  PStaticLine = ^TStaticLine;
  TStaticLine = Object(TInputLine)
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  PSpecialLabel = ^TSpecialLabel;
  TSpecialLabel = Object(TLabel)
    procedure Draw;                           virtual;
  end;

  PSpecialText = ^TSpecialText;
  TSpecialText = Object(TStaticText)
    procedure Draw;                           virtual;
  end;

  PSimpleText = ^TSimpleText;
  TSimpleText = Object(TStaticText)
    Color: Word;
    constructor Init(Bounds: TRect; AText: String; AColor: Word);
    constructor Load(var S: TStream);
    procedure   Store(var S: TStream);
    procedure   Draw;                         virtual;
  end;

  PStatic = ^TStatic;
  TStatic = Object(TStaticText)
    constructor Init(Bounds: TRect; AText: String);
    procedure Draw;                           virtual;
  end;

  PBitMapButton = ^TBitMapButton;
  TBitMapButton = Object(TButton)
    BitMap: PBitMap;
    constructor Init(var Bounds: TRect; ATitle: TTitleStr;
                     ACommand: Word; AFlags: Word; ID: Word);
    constructor Load(var S: TStream);
    procedure   Store(var S: TStream);
    destructor  Done;                         virtual;
    procedure   DrawTitle(Color: Word);       virtual;
  end;

  PSmallButton = ^TSmallButton;
  TSmallButton = Object(TButton)
{    Shift: Byte;}
    constructor Init(Bounds: TRect; ATitle: TTitleStr;
                       ACommand: Word; AFlags: Word{; AShift: Byte});
    procedure DrawTitle(Color: Word);      virtual;
{    procedure Store(var S: TStream);
    constructor Load(var S: TStream);}
  end;

  PPointButton = ^TPointButton;
  TPointButton = Object(TSmallButton)
    Link: PView;
    constructor Init(Bounds: TRect; ATitle: TTitleStr;
          ACommand: Word; AFlags: Word;{ AShift: Byte; }ALink: PView);
    constructor Load(var S: TStream);
    procedure   HandleEvent(var Event: TEvent); virtual;
    procedure   Press;                          virtual;
    procedure   Store(var S: TStream);
  end;

  PPointView = ^TPointView;
  TPointView = Object(TNumericInput)
    Step:    Real;
    RealVar: PReal;
    Min:     Real;
    constructor Init(var Bounds: TRect; const AMask: string;
                     AStep: Real; V: PReal; AMin: Real);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Str2Val;                        virtual;
  end;

  PNumInputLine = ^TNumInputLine;
  TNumInputLine = Object(TInputLine)
    function Valid(Command: Word): Boolean;   virtual;
  end;

  procedure RegisterLabels;
  procedure InsertFrameView(S: PWindow; V: PView; ATitle: String;
    AFrame: Byte; ALeft,ARight: Word);
  function  InsertFrameInput(S: PWindow; R:TRect; ATitle: String;
    AMaxLen,AHistID: Integer; AFrame: Byte; ALeft: Word;
    LockInput: Boolean): PInputLine;

  Const
    frFrame = 1;
    frBar   = 2;
    frDown  = 4;
    frAll   = frFrame+frBar+frDown;

    NoHistory = 0;

    RStatic: TStreamRec = (
      ObjType: 10001;
      VmtLink: Ofs(TypeOf(TStatic)^);
      Load:    @TStatic.Load;
      Store:   @TStatic.Store
    );

    RSimpleText: TStreamRec = (
      ObjType: 10002;
      VmtLink: Ofs(TypeOf(TSimpleText)^);
      Load:    @TSimpleText.Load;
      Store:   @TSimpleText.Store
    );

    RSpecialText: TStreamRec = (
      ObjType: 10003;
      VmtLink: Ofs(TypeOf(TSpecialText)^);
      Load:    @TSpecialText.Load;
      Store:   @TSpecialText.Store
    );

    RSpecialLabel: TStreamRec = (
      ObjType: 10004;
      VmtLink: Ofs(TypeOf(TSpecialLabel)^);
      Load:    @TSpecialLabel.Load;
      Store:   @TSpecialLabel.Store
    );

    RSmallButton: TStreamRec = (
      ObjType: 10005;
      VmtLink: Ofs(TypeOf(TSmallButton)^);
      Load:    @TSmallButton.Load;
      Store:   @TSmallButton.Store
    );

    RBitMapButton: TStreamRec = (
      ObjType: 10006;
      VmtLink: Ofs(TypeOf(TBitMapButton)^);
      Load:    @TBitMapButton.Load;
      Store:   @TBitMapButton.Store
    );

    RPointButton: TStreamRec = (
      ObjType: 10007;
      VmtLink: Ofs(TypeOf(TPointButton)^);
      Load:    @TPointButton.Load;
      Store:   @TPointButton.Store
    );

                           Implementation

  procedure TSpecialLabel.Draw;
  var Color, Back: Word; S,S1: String; KF,KP: Byte; Sum: Integer; F: Integer;
      H,L: Integer; Up: Byte;

   procedure StandardColor;
   begin
     if Light then begin Color := GetColor($0603); Back := GetColor($04) end
     else begin Color := GetColor($0501); Back := GetColor($02) end;
   end;

   procedure StandardFont;
   begin
     Font.Font:=F; Up:=0;
   end;

  begin F:=Font.Font; H:=FontHeight; L:=CharWidth;
    if Text = nil then Exit else StandardColor; Up:=0;
    Bar(0, 0, Size.X, Size.Y, Back); S:=Text^; Sum:=0;
    repeat KF:=Pos(^F,S); KP:=Pos(^P,S);
      if KP=0 then KP:=255; if KF=0 then KF:=255;
      if KF<KP then begin S1:=Copy(S,1,KF-1);
        WrCStr(Sum*L,Size.Y-H-Up,S1,Color);
        if S[KF+1]='S' then StandardFont
        else begin Up:=Byte(S[KF+2]); Font.Font:=Byte(S[KF+1]); end;
        H:=FontHeight; L:=CharWidth;
        S:=Copy(S,KF+3,255);
        if Pos('~',S1)<>0 then Inc(Sum,KF-3) else Inc(Sum,KF-1);
      end else if KF>KP then begin S1:=Copy(S,1,KP-1);
        WrCStr(Sum*L,Size.Y-H-Up,S1,Color);
        if S[KP+1]='S' then StandardColor else
         if Light then Color:= GetColor($06)*256+Byte(S[KP+1])
                  else Color := GetColor($05)*256+Byte(S[KP+2]);
        S:=Copy(S,KP+3,255);
        if Pos('~',S1)<>0 then Inc(Sum,KP-3) else Inc(Sum,KP-1);
      end else WrCStr(Sum*L,Size.Y-H-Up,S,Color);
    until KP=KF;
    Font.Font:=F;
  end;


  procedure TSpecialText.Draw;
  var Color: Word; S: String; KF,KP: Byte; Sum: Integer; F: Integer;
      H,L: Integer; Up: Byte;

   procedure StandardColor;
   begin
     Color:=GetColor(1);
   end;

   procedure StandardFont;
   begin
     Font.Font:=F; Up:=0;
   end;

  begin F:=Font.Font; H:=FontHeight; L:=CharWidth;
    if Text = nil then Exit else StandardColor; Up:=0;
    Bar(0, 0, Size.X, Size.Y, GetColor(2)); S:=Text^; Sum:=0;
    repeat KF:=Pos(^F,S); KP:=Pos(^P,S);
      if KP=0 then KP:=255; if KF=0 then KF:=255;
      if KF<KP then begin
        WrStr(Sum*L,Size.Y-H-Up,Copy(S,1,KF-1),Color);
        if S[KF+1]='S' then StandardFont
        else begin Up:=Byte(S[KF+2]); Font.Font:=Byte(S[KF+1]); end;
        H:=FontHeight; L:=CharWidth; S:=Copy(S,KF+3,255); Inc(Sum,KF-1);
      end else if KF>KP then begin
        WrStr(Sum*L,Size.Y-H-Up,Copy(S,1,KP-1),Color);
        if S[KP+1]='S' then StandardColor
                       else Color:=GetColor(1)*256+Byte(S[KP+2]);
        S:=Copy(S,KP+3,255); Inc(Sum,KP-1);
      end else WrStr(Sum*L,Size.Y-H-Up,S,Color);
    until KP=KF;
    Font.Font:=F;
  end;
{------------------ TSimpleText ---------------}
  Constructor TSimpleText.Init;
  begin
   Inherited Init(Bounds,AText);
   Color:=AColor;
   Font.Font :=1;
  end;

  constructor TSimpleText.Load;
  begin Inherited Load(S);
    S.Read(Color, SizeOf(Color));
  end;

  procedure TSimpleText.Store;
  begin Inherited Store(S);
    S.Write(Color, SizeOf(Color));
  end;

  Procedure TSimpleText.Draw;
  begin
    Bar(0,0,Size.X,Size.Y,Hi(Color));
    WrStr(0,0,Text^,Color)
  end;

  constructor TStatic.Init;
  begin
   Inherited Init(Bounds, Atext);
   Font.Font :=1
  end;

 procedure TStatic.Draw;
 begin
   Bar(0,0,Size.X,Size.Y,$07);
   WrStr(1,1,Text^,$070E);
   WrStr(0,0,Text^,$0701);
 end;


    {█▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█─┐
     █ TBitMapButton...█ │
     █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█ │
      └──────────────────┘}
constructor TBitMapButton.Init;
begin Inherited Init(Bounds, ATitle, ACommand, AFlags);
  BitMap:= New(PBitMap, Init(ID));
end;

constructor TBitMapButton.Load;
begin Inherited Load(S);
  S.Put(BitMap);
end;

procedure TBitMapButton.Store;
begin Inherited Store(S);
  PObject(BitMap):=S.Get;
end;

destructor TBitMapButton.Done;
begin
  Dispose(BitMap, Done);
  Inherited Done;
end;

procedure TBitMapButton.DrawTitle;
var L,H: Integer;
begin Font.Font:=5; SetFont(Font.Font);
  L := (Size.X - CStrLen(Title^) * CharWidth) div 2;
  if L < 2 then L := 2;
  H := Size.Y - 30;
  WrCStr(L{ + D}, H{ + D}, Copy(Title^, 1, Size.X div CharWidth), Color);
  PutBMP(BitMap^.ToDraw,5,5);
end;

{constructor TSmallButton.Load;
begin Inherited Load(S);
  S.Read(Shift, SizeOf(Shift));
end;

procedure TSmallButton.Store;
begin Inherited Store(S);
  S.Write(Shift, SizeOf(Shift));
end;}

constructor TSmallButton.Init;
begin Inherited Init(Bounds, ATitle, ACommand, AFlags);
  Dec(Options, ofSelectable); {Shift:=AShift;}
end;

procedure TSmallButton.DrawTitle;
begin Font.Font:=2;
  if Size.Y<FontHeight then WrCStr({D+}4,-1{Shift+D-1}, Title^, Color)
                       else WrCStr({D+}4,2{Shift+D-1}, Title^, Color);
end;

constructor TPointButton.Init;
begin Inherited Init(Bounds,ATitle,ACommand,AFlags{,AShift});
  Link:=ALink;
end;

procedure TPointButton.Press;
var Event: TEvent;
begin
  if Link<>nil then Message(Link, evBroadcast, Command, @Self);
  Message(Owner, evCommand, Command, @Self);
end;

procedure TPointButton.HandleEvent;
var ClickRect: TRect; Down: Boolean; Mouse: TPoint;
begin GetExtent(ClickRect);
  case Event.What of
    evMouseDown:
      if (Event.Buttons and mbLeftButton<>0) then begin
        Inc(ClickRect.B.X); Down := False;
          repeat MakeLocal(Event.Where, Mouse);
            if Down <> ClickRect.Contains(Mouse) then begin
              Down := not Down; DrawState(Down);
            end;
            if Down then Press;
          until not MouseEvent(Event, evMouseAuto);
          DrawState(False); ClearEvent(Event); Exit;
      end;
  end;
  Inherited HandleEvent(Event);
end;

constructor TPointButton.Load;
begin Inherited Load(S);
  GetPeerViewPtr(S, Link);
end;

procedure TPointButton.Store;
begin Inherited Store(S);
  PutPeerViewPtr(S, Link);
end;


constructor TPointView.Init;
var R: TRect;
begin Inherited Init(Bounds, AMask, True);
  Step:=AStep; RealVar:=V; SetData(V^); Min:=AMin;
end;

procedure TPointView.HandleEvent;
 procedure Up;
 begin Val:=Val+Step; Val2Str; NoUpDate:=False; DrawView
 end;
 procedure Down;
 begin Val:=Val-Step; Val2Str; NoUpDate:=False; DrawView
 end;
begin Inherited HandleEvent(Event);
  case Event.What of
    evKeyDown: case Event.KeyCode of
                 kbPgUp: Up;
                 kbPgDn: Down;
                 else Exit;
               end;
    evBroadCast: case Event.Command of
                 cmUpPoint:   Up;
                 cmDownPoint: Down;
               else Exit;
               end;
    else Exit;
  end;
  ClearEvent(Event);
end;

procedure TPointView.Str2Val;
begin Inherited Str2Val;
  if Val<=Min then Val:=RealVar^ else RealVar^:=Val;
end;

procedure RegisterLabels;
begin
  RegisterType(RSmallButton);
  RegisterType(RPointButton);
  RegisterType(RBitMapButton);
  RegisterType(RSimpleText);
  RegisterType(RSpecialText);
  RegisterType(RStatic);
  RegisterType(RSpecialLabel);
end;

procedure TStaticLine.HandleEvent;
begin
  case Event.What of
    evKeyBoard: if not ((Event.KeyCode=kbLeft) or (Event.KeyCode=kbRight)) then Exit;
  end;
  Inherited HandleEvent(Event);
end;


function InsertFrameInput;
var V: PInputLine; H,W,Max: Integer; His: Boolean;
begin Max:=R.B.X; His:=AHistID<>NoHistory;
  with S^ do begin H:=FontHeight; W:=CharWidth; Dec(R.B.X,16);
    if LockInput then V:=New(PStaticLine, Init(R,AMaxLen))
                 else V:=New(PInputLine, Init(R,AMaxLen));
    InsertFrameView(S,V,ATitle,AFrame,ALeft,16*Byte(His));
    Insert(V); InsertFrameInput:=V;
    if His then begin V^.GetBounds(R);
      R.A.X:=Max-W-6; R.B.X:=R.A.X+W; R.Move(0,-1);
      Insert(New(PHistory, Init(R, PInputLine(V), AHistID)));
    end;
  end;
end;

procedure InsertFrameView;
var H,W: Integer; R: TRect;
begin V^.GetBounds(R);
  with S^ do begin H:=FontHeight; W:=CharWidth;
    R.Grow(12,12); Dec(R.A.Y,H); R.Move(0,2); Inc(R.B.X,ARight);
    if AFrame and frBar<>0 then Insert(New(PShade, Init(R, $05, $0F08, shBar)));
    R.Grow(-6,-6); Inc(R.A.Y,H div 2-3); Inc(R.B.Y);
    if AFrame and frFrame<>0 then Insert(New(PShade, Init(R, $05, $0F08, shFrame)));
    R.Grow(-5,-6); Inc(R.A.Y,H div 2); Dec(R.B.Y,2); Dec(R.B.X,ARight);
    if AFrame and frDown<>0 then Insert(New(PShade, Init(R, $07, $000F, shBar)));
    if ATitle<>'' then begin R.Grow(-1,-1); R.Move(ALeft,-H-5);
      R.B.Y:=R.A.Y+H; R.B.X:=R.A.X+W*(Length(ATitle))-2;
      Insert(New(PLabel, Init(R,' '+ATitle,V)));
    end;
    Insert(V);
  end;
end;

function TNumInputLine.Valid;
var er: Integer; R: Extended; Inh: Boolean; Event: TEvent;
begin Inh:=Inherited Valid(Command);
  Val(Data^,R,er);
  if er<>0 then begin Valid:=false;
    Event.What:=evBroadCast; Event.Command:=cmInValid;
    PutEvent(Event);
  end else Valid:=Inh;
end;

End.