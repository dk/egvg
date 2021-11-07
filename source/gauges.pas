{************************************************}
{                                                }
{   Turbo Vision File Manager Demo               }
{   Copyright (c) 1992 by Borland International  }
{                                                }
{************************************************}
{DK:Videodriver must support NotSrcXor mode}
{$O+,F+,I-,R-,S-,C-,V-,D-,P+}
unit Gauges;


interface

uses Drivers, Objects, Views, Dialogs, App, GDI;

const
  cmUpdateGauge        = 12000;
  cmResetGauge         = 12001;
  cmAddGauge           = 12002;
  cmStatusUpdate       = 12003;
  cmStatusSetNewMsg    = 12004;

type
  PPercentGauge = ^TPercentGauge;
  TPercentGauge = object(TView)
    MaxValue: Longint;
    CurValue: Longint;
    constructor Init(var Bounds: TRect; AMaxValue: Longint);
    procedure Draw; virtual;
    procedure Update(Progress: Longint); virtual;
    procedure AddProgress(Progress: Longint);
    procedure HandleEvent(var Event: TEvent); virtual;
    function SolveForX(Y, Z: Longint): Integer;
    function SolveForY(X, Z: Longint): Integer;
  end;

  PStatusBox = ^TStatusBox;
  TStatusBox = object(TDialog)
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  PGaugeStatusBox = ^TGaugeStatusBox;
  TGaugeStatusBox = object(TDialog)
    Rank  : Byte;
    Gauge : PPercentGauge;
    Text  : PView;
    constructor Init(var Bounds: TRect; ATitle, AMsg: TTitleStr);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;


procedure ShowStatusBox(const StatusMsg : String);
procedure ShowGaugeStatusBox(const StatusMsg : String);
procedure KillStatusBox;

Const
  StatusBox   : PStatusBox = Nil;
  OwnerReturn : PGroup     = Nil;


implementation

constructor TPercentGauge.Init(var Bounds: TRect; AMaxValue: Longint);
begin
  inherited Init(Bounds);
  EventMask := EventMask or evBroadcast;
  MaxValue := AMaxValue;
  CurValue := 0;
end;

procedure TPercentGauge.Draw;
var
  C: Word;
  S: string;
  PercentDone, FillSize: Longint;
  X, Y : Integer;
begin
  PercentDone := SolveForY(CurValue, MaxValue);
  FillSize := SolveForX(PercentDone, Size.X);
  if FillSize > Size.X then FillSize := Size.X;
  FormatStr(S, '%-3d%%', PercentDone);
  C := FontWidth(S);
  X := (Size.X - C) div 2;
  Y := (Size.Y - FontHeight) div 2;
  Bar(0, 0, FillSize, Size.Y, LightBlue);
  Bar(FillSize, 0, Size.X, Size.Y, LightGray);
  PaintInfo.Operation := NotSrcXor;
  WrStr(X, Y, S, LightBlue);
  Rectangle(0, 0, Size.X, Size.Y, 1, 15, 0);
  PaintInfo.Operation := CopyPut;
end;

procedure TPercentGauge.Update(Progress: Longint);
begin
  CurValue := Progress;
  DrawView;
end;

procedure TPercentGauge.AddProgress(Progress: Longint);
begin
  Update(Progress + CurValue);
end;

procedure TPercentGauge.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if Event.What = evBroadcast then
  begin
    case Event.Command of
      cmUpdateGauge :
        begin
          Update(Event.InfoLong);
        end;
      cmResetGauge:
        begin
          MaxValue := Event.InfoLong;
          Update(0);
        end;
      cmAddGauge:
        begin
          AddProgress(Event.InfoLong);
        end;
    end;
  end;
end;

{ This function solves for x in the equation "x is y% of z". }
function TPercentGauge.SolveForX(Y, Z: Longint): Integer;
begin
  SolveForX := Trunc( Z * (Y * 0.01) );
end;

{ This function solves for y in the equation "x is y% of z". }
function TPercentGauge.SolveForY(X, Z: Longint): Integer;
begin
  if Z = 0 then SolveForY := 0
  else SolveForY := Trunc( (X * 100) / Z );
end;


procedure TStatusBox.HandleEvent(var Event:TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmStatusUpdate) then
    DrawView;
end;

constructor TGaugeStatusBox.Init(var Bounds: TRect; ATitle, AMsg: TTitleStr);
var
  R: TRect;
  P: PView;
Begin
  Inherited Init(Bounds, ATitle);
  R.Assign(12, Bounds.B.Y - 34, Bounds.B.X - 12, Bounds.B.Y - 14);
  Gauge := New(PPercentGauge, Init(R, 100));
  Insert(Gauge);
  Rank := 0;
  Options := Options or ofCentered;
  Options := Options and (not ofBuffered);
  Flags := Flags and (not wfClose) and (not wfMove);
  R.Assign(12, 24, Bounds.B.X - 12, Bounds.B.Y - 36);
  P := New(PStaticText, Init(R, #3+AMsg));
  Text := P;
  Insert(P);
End;

procedure TGaugeStatusBox.HandleEvent(var Event : TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What=evBroadcast) and (Event.Command = cmStatusUpdate) then begin
    Rank := Event.InfoByte;
    Event.InfoLong := Event.InfoByte;
    Message(Gauge, evBroadcast, cmUpdateGauge, Event.InfoPtr);
    Gauge^.DrawView;
  end;
  if (Event.What=evCommand) and (Event.Command = cmStatusSetNewMsg) then begin
    ReStr(PStaticText(Text)^.Text, #3+String(Event.InfoPtr^));
    Text^.DrawView;
  end;
end;


procedure ShowStatusBox(const StatusMsg : String);
var
  R: TRect;
  P: PView;
  StatusPMsg : Pointer;
begin
  if StatusBox <> nil then exit;
  OwnerReturn := PGroup(Desktop^.Current);
  R.Assign(0,0,300,80);
  {$IFNDEF RUSSIAN}
  StatusBox := New(PStatusBox, Init(R, 'Status'));
  {$ELSE}
  StatusBox := New(PStatusBox, Init(R, 'Статус'));
  {$ENDIF}
  with StatusBox^ do
  begin
    Options := Options or ofCentered;
    Options := Options and (not ofBuffered);
    Flags := Flags and (not wfClose) and (not wfMove);
    R.Assign(20,20,280,70);
    P := New(PParamText, Init(R, ^C'%s', 1));
    Insert(P);
  end;
  StatusPMsg := @StatusMsg;
  StatusBox^.SetData(StatusPMsg);
  Desktop^.Insert(StatusBox);
end;

procedure ShowGaugeStatusBox(const StatusMsg : String);
var
  R: TRect;
  L:Integer;
begin
  if StatusBox <> nil then exit;
  OwnerReturn := PGroup(Desktop^.Current);
  R.Assign(0,0,300,80);
  L := Desktop^.FontWidth(StatusMsg);
  if L > 260 then R.B.X := L + 40;
  {$IFNDEF RUSSIAN}
  StatusBox := PStatusBox(New(PGaugeStatusBox, Init(R, 'Status', StatusMsg)));
  {$ELSE}
  StatusBox := PStatusBox(New(PGaugeStatusBox, Init(R, 'Статус', StatusMsg)));
  {$ENDIF}
  Desktop^.Insert(StatusBox);
end;

procedure KillStatusBox;
Var
  ModalRedraw : PGroup;
begin
  ModalRedraw := OwnerReturn;
  if ModalRedraw <> Nil then
    with ModalRedraw^ do if GetState(sfModal) then Options := Options or ofSelectable;
  if StatusBox <> nil then
  begin
    Dispose(StatusBox, Done);
    StatusBox := nil;
  end;
  if ModalRedraw <> Nil then
    with ModalRedraw^ do if GetState(sfModal) then Options := Options and not ofSelectable;
end;


Var
  SaveNotify : TSysNotifyProc;

Procedure GaugesNotifyProc(EventWhat, EventCommand : Word; EventInfo : Pointer); Far;
Begin
  SaveNotify(EventWhat, EventCommand, EventInfo);
  if (EventWhat = snUser) and (Application <> Nil) then begin
    case EventCommand of
    unPercent : Message(Application, evBroadcast, cmStatusUpdate, EventInfo);
    unMessage : Message(Application, evBroadcast, cmStatusSetNewMsg, EventInfo);
    else end;
  end;
End;

begin
  SaveNotify    := SysNotifyProc;
  SysNotifyProc := GaugesNotifyProc;
end.