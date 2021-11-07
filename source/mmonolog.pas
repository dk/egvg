                                   UNIT MMonolog;
                                   Interface
Uses Objects,Drivers,Views,Dialogs,App;
Type
  PMonolog = ^TMonolog;
  TMonolog = object(TDialog)
    procedure   HandleEvent(var Event: TEvent); virtual;
  end;

function ExecuteMonolog(P: PMonolog; Data: Pointer; PerformFree: boolean): Word;
procedure RegisterMonolog;

Const

  RMonolog: TStreamRec = (
     ObjType: 1000;
     VmtLink: Ofs(TypeOf(TMonolog)^);
     Load:    @TMonolog.Load;
     Store:   @TMonolog.Store
  );

                     Implementation
procedure RegisterMonolog;
begin
  RegisterType(RMonolog);
end;

{
  ┌──────────────────────────────────────────────────────────╖
  │ TMonolog                                                 ║
  ╘══════════════════════════════════════════════════════════╝
}
procedure TMonolog.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  case Event.What of
    evKeyDown: begin
      case Event.KeyCode of
        kbPgDn,kbDown, kbRight, kbCtrlRight: begin
          Event.KeyCode := kbTab;
          Event.InfoPtr := nil;
          PutEvent(Event);
        end;
        kbPgUp,kbUp, kbLeft, kbCtrlLeft: begin
          Event.KeyCode := kbShiftTab;
          Event.InfoPtr := nil;
          PutEvent(Event);
        end;
        kbCtrlEnter: begin
          Event.Command := cmOk;
          Event.What := evCommand;
          Event.InfoPtr := nil;
          PutEvent(Event);
        end;
      else Exit;
      end;
      ClearEvent(Event);
    end;
  end;
end;

function ExecuteMonolog(P: PMonolog; Data: Pointer; PerformFree: boolean): Word;
var C: Word;
begin
  ExecuteMonolog:= cmCancel;
  if Application^.ValidView(P) <> nil then
  begin
    if Data <> nil then P^.SetData(Data^);
    C := Desktop^.ExecView(P);
    if (C <> cmCancel) and (Data <> nil) then P^.GetData(Data^);
    if (PerformFree) then Dispose(P, Done);
    ExecuteMonolog:= C;
  end;
end;

End.