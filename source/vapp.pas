{$F+,G+,S-,I-}
{
  inside  TVApplication you can use construction like
  Procedure OpenWindow(var Event : TEvent); Virtual cmOpenWidndow;
}

Unit VApp;

Interface

Uses App, Drivers, Views;

Type
  PVApplication = ^TVApplication;
  TVApplication = Object(TApplication)
    Procedure CommandsEvent(var Event: TEvent);
    Procedure GetEvent(var Event: TEvent); Virtual;
  End;


Implementation

Function Dynamic(VMT : Pointer; Index : Word): Pointer; assembler;
asm
   std
   mov  ax, [Index]
   les  di, [VMT]
   mov  di, es:[di+4]
@Check:
   or   di, di
   jnz  @Exists
   mov  ax, di
   mov  dx, di
   jmp  @Exit
@Exists:
   mov  cx, es:[di+6]
   mov  bx, cx
   shl  bx, 1
   mov  si, di    {si--> DMT}
   lea  di, [di+bx+6]
   repne scasw
   jne  @NoMatch
@Match:
   shl  cx, 1
   shl  cx, 1
   add  bx, cx
   les  ax, [es:si+bx+8]
   mov  dx, es
   jmp  @Exit
@NoMatch:
   mov  di, es:[si]
   jmp  @Check
@Exit:
end;

Procedure TVApplication.CommandsEvent(var Event: TEvent);
Type
  TEventProc = Procedure (var Event: TEvent);
Var
  EventProc: TEventProc;
  ProcAdr: Pointer absolute EventProc;
  P  : PView;
Label
  LPView, LSelfView;

Procedure RunGroup(View : PView); Far;
Begin
  ProcAdr := Dynamic(TypeOf(View^), Event.Command);
  if ProcAdr = Nil then Exit;
  asm
    les     bx, Self
    mov     di, bx
    les     di, ss:[di+$a]
    push    es
    push    di
    les     di, dword ptr [View]
    push    es
    push    di
    call    dword ptr ss:[bx-4]
  end;
End;

Begin
  if Event.What <> evCommand then Exit;
  P := Desktop^.Current;
  {make sure that P is PGroup derivate.}
  if (P = Nil) or ((P^.Options and ofBuffered) = 0) then Goto LPView;
  PGroup(P)^.ForEach(@RunGroup);

  LPView:
  P := Desktop^.Current;
  if P = Nil then P := @Self; {else if (P^.State and sfModal) <> 0 then Exit}
  ProcAdr := Dynamic(TypeOf(P^), Event.Command);
  if ProcAdr = nil then Goto LSelfView;
  asm
    les     di, Event
    push    es
    push    di
    les     di, dword ptr [p]
    push    es
    push    di
    call    EventProc
  end;
  ClearEvent(Event);

  LSelfView:
  if P <> @Self then begin
    if (P^.State and sfModal) <> 0 then Exit;
    P := @Self;
    ProcAdr := Dynamic(TypeOf(P^), Event.Command);
    if ProcAdr = nil then Exit;
    asm
      les     di, Event
      push    es
      push    di
      les     di, dword ptr [p]
      push    es
      push    di
      call    EventProc
    end;
    ClearEvent(Event);
  end;
End;


Procedure TVApplication.GetEvent(var Event: TEvent);
Begin
  TApplication.GetEvent(Event);
  if Event.What = evCommand then CommandsEvent(Event);
End;

End.
