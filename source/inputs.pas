unit Inputs;
{$O+,F+,I-,R-,S-,C-,V-,X+}

interface

uses Objects, Drivers, Dialogs, Views, Validate, Editors, EgDate, EgString;

const
  cmInputChanged = $88F0;

type
  TClipInputLine = Object(TInputLine)
    Procedure HandleEvent(var Event : TEvent); Virtual;
    Procedure OnChange; Virtual;
    Procedure SetState(AState: Word; Enable : Boolean); Virtual;
  End;

  TInputLine = TClipInputLine;
  PInputLine = ^TInputLine;

  { Accepts only valid numeric input between Min and Max }
  PNumInputLine = ^TNumInputLine;
  TNumInputLine = object(TInputLine)
    Min: Longint;
    Max: Longint;
    constructor Init(var Bounds: TRect; AMaxLen: Integer;
      AMin, AMax: Longint);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  end;

  PRealInputLine = ^TRealInputLine;
  TRealInputLine = object(TInputLine)
    Str1 : Integer;
    Str2 : Integer;
    Min: Real;
    Max: Real;
    constructor Init(var Bounds: TRect; AMaxLen: Integer;
      AMin, AMax: Real; AStr1, AStr2 : Integer);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  end;

  PDateInputLine = ^TDateInputLine;
  TDateInputLine = Object(TInputLine)
    procedure SetData(var Rec); virtual;
    function Valid(Command: Word): Boolean; Virtual;
  End;

  TDumbInputLine = Object(TInputLine)
    constructor Init(var Bounds: TRect; AMaxLen: Integer);
    Procedure   HandleEvent(var Event: TEvent); Virtual;
  End;
  PDumbInputLine = ^TDumbInputLine;

  TActionInputLine = Object(TInputLine)
    WriteEnable : Boolean;
    Constructor Init(var Bounds: TRect; AMaxLen: Integer; AWriteEnable : Boolean);
    Procedure   HandleEvent(var Event: TEvent); Virtual;
    Procedure   Action;                         Virtual;
  End;
  PActionInputLine = ^TActionInputLine;

  TQuickSendInputLine = Object(TInputLine)
    Procedure HandleEvent(var Event : TEvent); Virtual;
    Function  DataSize: Word;                  Virtual;
    Procedure SetData(var Rec);                Virtual;
    Procedure GetData(var Rec);                Virtual;
  End;
  PQuickSendInputLine = ^TQuickSendInputLine;

  TMovableButton = Object(TButton)
    Constructor Init(var Bounds: TRect; ATitle: TTitleStr; ACommand: Word; AFlags: Byte);
  End;
  PMovableButton = ^TMovableButton;

  TKeyButton = Object(TMovableButton)
    Key        : Word;
    Constructor Init(var Bounds: TRect; ATitle: TTitleStr; ACommand: Word; AFlags: Byte; AKey : Word);
    Procedure   HandleEvent(var Event: TEvent);  Virtual;
    Constructor Load(var S : TStream);
    Procedure   Store(var S : TStream);
  End;
  PKeyButton = ^TKeyButton;

  PPartnerButton = ^TPartnerButton;
  TPartnerButton = Object(TMovableButton)
    Partner   : PPartnerButton;
    Constructor Init(var Bounds: TRect; ATitle: TTitleStr; ACommand: Word; AFlags: Byte; APartner : PPartnerButton);
    Constructor Load(var S : TStream);
    Procedure   Store(var S : TStream);
    Procedure   HandleEvent(var Event: TEvent);  Virtual;
  End;

  PForceTypeInputLine = ^TForceTypeInputLine;
  TForceTypeInputLine = Object(TInputLine)
    Function  Valid(Command : Word) : Boolean; Virtual;
  End;

  PCodeInputLine = ^TCodeInputLine;
  TCodeInputLine = Object(TInputLine)
    MinLen  : Integer;
    Align   : Byte;
    Constructor Init(var Bounds: TRect; AMaxLen: Integer);
    Constructor Load(var S : TStream);
    Procedure   Store(var S : TStream);
    Procedure   GetData(var Rec);                Virtual;
    Procedure   SetData(var Rec);                Virtual;
    Function    Valid(Command : Word) : Boolean; Virtual;
  End;



Const
  afAlignMask = $07;
  afNormal    = $00;
  afLeft      = afNormal;
  afRight     = $01;
  afCenter    = $02;
  afRightPad  = $08;

  cmActionInputSend     = $9040;
  cmPartnerButtonToggle = $9041;
  cmQuickSendChanged    = $9042;

  HDatePicture : String[10] =
{$IFDEF RUSSIAN}
  'DD.MM.YY'
{$ELSE}
  'MM/DD/YY'
{$ENDIF}
  ;
  

procedure RegisterInputs;

Function  GetClipText : String;
Procedure SetClipText(S : String);

const
  RInputLine: TStreamRec = (
     ObjType: 10089;
     VmtLink: Ofs(TypeOf(TInputLine)^);
     Load:    @TInputLine.Load;
     Store:   @TInputLine.Store
  );
  RNumInputLine: TStreamRec = (
     ObjType: 10090;
     VmtLink: Ofs(TypeOf(TNumInputLine)^);
     Load:    @TNumInputLine.Load;
     Store:   @TNumInputLine.Store
  );
  RRealInputLine: TStreamRec = (
     ObjType: 10091;
     VmtLink: Ofs(TypeOf(TRealInputLine)^);
     Load:    @TRealInputLine.Load;
     Store:   @TRealInputLine.Store
  );
  RDateInputLine: TStreamRec = (
     ObjType: 10092;
     VmtLink: Ofs(TypeOf(TDateInputLine)^);
     Load:    @TDateInputLine.Load;
     Store:   @TDateInputLine.Store
  );
  RActionInputLine: TStreamRec = (
     ObjType: 10093;
     VmtLink: Ofs(TypeOf(TActionInputLine)^);
     Load:    @TActionInputLine.Load;
     Store:   @TActionInputLine.Store
  );
  RDumbInputLine: TStreamRec = (
     ObjType: 10094;
     VmtLink: Ofs(TypeOf(TDumbInputLine)^);
     Load:    @TDumbInputLine.Load;
     Store:   @TDumbInputLine.Store
  );
  RForceTypeInputLine: TStreamRec = (
     ObjType: 10095;
     VmtLink: Ofs(TypeOf(TForceTypeInputLine)^);
     Load:    @TForceTypeInputLine.Load;
     Store:   @TForceTypeInputLine.Store
  );
  RQuickSendInputLine: TStreamRec = (
     ObjType: 10096;
     VmtLink: Ofs(TypeOf(TQuickSendInputLine)^);
     Load:    @TQuickSendInputLine.Load;
     Store:   @TQuickSendInputLine.Store
  );
  RMovableButton: TStreamRec = (
     ObjType: 10097;
     VmtLink: Ofs(TypeOf(TMovableButton)^);
     Load:    @TMovableButton.Load;
     Store:   @TMovableButton.Store
  );
  RKeyButton: TStreamRec = (
     ObjType: 10098;
     VmtLink: Ofs(TypeOf(TKeyButton)^);
     Load:    @TKeyButton.Load;
     Store:   @TKeyButton.Store
  );
  RPartnerButton: TStreamRec = (
     ObjType: 10099;
     VmtLink: Ofs(TypeOf(TPartnerButton)^);
     Load:    @TPartnerButton.Load;
     Store:   @TPartnerButton.Store
  );
  RCodeInputLine: TStreamRec = (
     ObjType: 10100;
     VmtLink: Ofs(TypeOf(TCodeInputLine)^);
     Load:    @TCodeInputLine.Load;
     Store:   @TCodeInputLine.Store
  );


Function WordStr(S : String) : Word;
Function StrWord(W, Len : Word) : String;


implementation

uses MsgBox;

procedure RegisterInputs;
begin
  RegisterType(RInputLine);
  RegisterType(RNumInputLine);
  RegisterType(RRealInputLine);
  RegisterType(RDateInputLine);
  RegisterType(RActionInputLine);
  RegisterType(RDumbInputLine);
  RegisterType(RQuickSendInputLine);
  RegisterType(RForceTypeInputLine);
  RegisterType(RMovableButton);
  RegisterType(RKeyButton);
  RegisterType(RPartnerButton);
  RegisterType(RCodeInputLine);
end;

Function WordStr(S : String) : Word;
var
  I : Byte;
  d : array[0..6] of byte absolute S;
Begin
  S := LeftPadCh(S, '0', 5);
  for i := 1 to 5 do Dec(S[i], Byte('0'));
  WordStr := 10000 * d[1] + 1000 * d[2] + word(100) * d[3] + word(10) * d[4] + d[5];
End;

Function StrWord(W, Len : Word) : String;
Begin
  StrWord := LeftPadCh(Long2Str(W), '0', Len);
End;

Function  GetClipText : String;
Var
  S : String;
  W : Word;
Begin
  if ClipBoard <> Nil then begin
    W := ClipBoard^.SelEnd - ClipBoard^.SelStart;
    if W > 255 then W := 255;
    Move(Pointer(LongInt(ClipBoard^.Buffer) + ClipBoard^.BufPtr(ClipBoard^.SelStart))^, S[1], W);
    S[0] := Char(W);
    GetClipText := S;
  end else GetClipText := '';
End;

Procedure SetClipText(S : String);
Begin
  if ClipBoard <> Nil then ClipBoard^.InsertText(@S[1], Length(S), True);
End;


Procedure TClipInputLine.HandleEvent(var Event : TEvent);
Var
  MC : Boolean;

Procedure Copy;
Begin
  if SelStart <> SelEnd then begin
    SetClipText(system.Copy(Data^, SelStart+1, SelEnd - SelStart));
    ClearEvent(Event);
  end;
End;

Procedure Cut;
Begin
  if SelStart <> SelEnd then begin
    SetClipText(system.Copy(Data^, SelStart+1, SelEnd - SelStart));
    Delete(Data^, SelStart+1, SelEnd - SelStart);
    SelEnd := SelStart;
    DrawView;
    ClearEvent(Event);
    OnChange;
  end;
End;

Procedure Paste;
Var
  InputClipBoard : String;
Begin
  SelStart := CurPos;
  SelEnd   := CurPos;
  InputClipboard := GetClipText;
  if Byte(Data^[0]) + Byte(InputClipBoard[0]) > MaxLen then
    Byte(InputClipBoard[0]) := MaxLen - Byte(Data^[0]);
  Insert(InputClipBoard, Data^, SelStart+1);
  Inc(SelEnd, Byte(InputClipBoard[0]));
  CurPos := SelEnd;
  DrawView;
  ClearEvent(Event);
  OnChange;
End;

Begin
  MC := False;
  case Event.What of
  evKeyDown :
    case Event.KeyCode of
    kbCtrlIns : Copy;
    kbShiftDel : Cut;
    kbShiftIns : Paste;
    kbDel : MC := True;
    else begin
      if Event.CharCode > #0 then MC := True;
    end;
    end;
  evCommand :
    case Event.Command of
    cmCopy  : Copy;
    cmCut   : Cut;
    cmPaste : Paste;
    else end;
  else end;
  Inherited HandleEvent(Event);
  if MC then OnChange;
End;

Procedure TClipInputLine.OnChange;
Begin
  Message(Owner, evBroadcast, cmInputChanged, @Self);
End;


Procedure TClipInputLine.SetState(AState: Word; Enable : Boolean);
Begin
  Inherited SetState(AState, Enable);
  if State = sfActive then EnableCommands([cmCut, cmCopy, cmPaste]);
End;

Function RealRound(R : Real; Mantisse : Byte) : Real;
Const
  Powers : array[0..4] of Real = (1, 10, 100, 1000, 10000);
Var
  M : Real;
Begin
  M := R * Powers[Mantisse];
  M := Int(M);
  RealRound := M / Powers[Mantisse];
End;


{ TNumInputLine }
constructor TNumInputLine.Init(var Bounds: TRect; AMaxLen: Integer;
  AMin, AMax: Longint);
begin
  TInputLine.Init(Bounds, AMaxLen);
  Min := AMin;
  Max := AMax;
end;

constructor TNumInputLine.Load(var S: TStream);
begin
  TInputLine.Load(S);
  S.Read(Min, SizeOf(LongInt) * 2);
end;

function TNumInputLine.DataSize: Word;
begin
  DataSize := SizeOf(LongInt);
end;

procedure TNumInputLine.GetData(var Rec);
var
  Code: Integer;
  R   : LongInt;
begin
  Val(Data^, R, Code);
  {if R > Max then R := Max;
  if (R < Min) or (Code <> 0) then R := Min;}
  if (R <> Longint(Rec)) then Longint(Rec) := R;
end;

procedure TNumInputLine.Store(var S: TStream);
begin
  Inherited Store(S);
  S.Write(Min, SizeOf(Longint) * 2);
end;

procedure TNumInputLine.SetData(var Rec);
var
  S: string[12];
begin
  Str(Longint(Rec), Data^);
  SelectAll(True);
end;

function TNumInputLine.Valid(Command: Word): Boolean;
var
  Code: Integer;
  Value: Longint;
  Params: array[0..1] of LongInt;
  Ok: Boolean;
  St : String[20];
begin
  Ok := True;
  if (Command <> cmCancel) and (Command <> cmValid) and (Command <> cmClose) and (Command <> cmQuit) then
  begin
    if Data^ = '' then Data^ := '0';
    Val(Data^, Value, Code);
    if (Code <> 0) or ((Min < Max) and ((Value < Min) or (Value > Max))) then
    begin
      Select;
      Params[0] := Min;
      Params[1] := Max;
      MessageBox(
      {$IFNDEF RUSSIAN}
      'Number must be from %D to %D.'
      {$ELSE}
      'Число должно лежать в диапазоне от %D до %D'
      {$ENDIF}, @Params, mfError + mfOkButton);
      SelectAll(True);
      Value := Min;
      Str(Min, St);
      if MaxLen < Byte(St[0]) then Byte(St[0]) := MaxLen;
      Data^ := St;
      Ok := False;
    end;
  end;
  if Ok then Valid := TInputLine.Valid(Command)
  else Valid := False;
end;

{ TRealInputLine }
constructor TRealInputLine.Init(var Bounds: TRect; AMaxLen: Integer;
  AMin, AMax: Real; AStr1, AStr2 : Integer);
begin
  TInputLine.Init(Bounds, AMaxLen);
  Min := AMin;
  Max := AMax;
  Str1 := AStr1;
  Str2 := AStr2;
  EventMask := EventMask or evBroadCast;
end;

constructor TRealInputLine.Load(var S: TStream);
begin
  TInputLine.Load(S);
  S.Read(Min, SizeOf(Real) * 2);
  S.Read(Str1, SizeOf(Integer) * 2);
end;

function TRealInputLine.DataSize: Word;
begin
  DataSize := SizeOf(Real);
end;

procedure TRealInputLine.GetData(var Rec);
var
  Code: Integer;
  R   : Real;
  S   : String[20];
  I   : Byte;
begin
  Move(Data^, S[0], MaxLen + 1);
  I := Pos('.', S);
  if (I > 0) and (I + Str1 < Byte(S[0])) then Byte(S[0]):=I + Str1;
  Val(S, R, Code);
  if (R <> Real(Rec)) then Real(Rec) := R;
end;

procedure TRealInputLine.Store(var S: TStream);
begin
  TInputLine.Store(S);
  S.Write(Min, SizeOf(Real) * 2);
  S.Write(Str1, SizeOf(Integer) * 2);
end;

procedure TRealInputLine.SetData(var Rec);
var
  S: string[20];
begin
  Str(Real(Rec) : Str1 : Str2, S);
  if MaxLen < Byte(S[0]) then Byte(S[0]) := MaxLen;
  Data^ := S;
  SelectAll(True);
end;

function TRealInputLine.Valid(Command: Word): Boolean;
var
  Code: Integer;
  Value: Real;
  Params: array[0..1] of String[15];
  Ok: Boolean;
  S : String;

begin
  Ok := True;
  if (Command <> cmCancel) and (Command <> cmValid) and (Command <> cmClose) and (Command <> cmQuit) then
  begin
    if Data^ = '' then Data^ := '0';
    Val(Data^, Value, Code);
    if (Code <> 0) or ((Min < Max) and ((Value < Min) or (Value > Max))) then
    begin
       Select;
       Str(Min : Str1 : Str2, Params[0]);
       Str(Max : Str1 : Str2, Params[1]);
       {$IFNDEF RUSSIAN}
       S := 'Number must be from '+ Params[0] + ' to ' + Params[1] + '.';
       {$ELSE}
       S := 'Число должно лежать в диапазоне от '+ Params[0] + ' до ' + Params[1] + '.';
       {$ENDIF}
       MessageBox(S, NIL, mfError + mfOkButton);
       Value := Min;
       Str(Min : Str1 : Str2, S);
       if MaxLen < Byte(S[0]) then Byte(S[0]) := MaxLen;
       Data^ := S;
       SelectAll(True);
       Ok := False;
    end;
  end;
  if Ok then Valid := TInputLine.Valid(Command)
  else Valid := False;
end;


procedure TDateInputLine.SetData(var Rec);
Var
  I : Byte;
  S : String absolute Rec;
begin
  if Byte(Rec) > 0 then for I := 1 to Byte(Rec) do
    if S[I] in [#0, ' '] then S[I] := '0';
  S := Pad(S, MaxLen);
  Inherited SetData(Rec);
end;


function TDateInputLine.Valid(Command: Word): Boolean;
Var
  Ok : Boolean;
  D  : Date;
  S : String;

Function Empty : Boolean;
Var
  I : Byte;
Begin
  Empty := False;
  if Length(HDatePicture) > Length(Data^) then Exit;
  for I := 1 to Length(HDatePicture) do
    if (HDatePicture[I] in ['M', 'D', 'Y']) and
      not (Data^[I] in ['0', #0, ' ']) then Exit;
  Empty := True;
End;

begin
  Ok := True;
  if (Command <> cmCancel) and (Command <> cmValid) and (Command <> cmClose) and (Command <> cmQuit) then
  begin
    if not(Inherited Valid(Command)) then begin
      Valid := False;
      Exit;
    end;
    Data^ := Pad(Data^, MaxLen);
    D := DateStringToDate(HDatePicture, Data^);
    if (D = -1) and not Empty then begin
      Select;
      {$IFNDEF RUSSIAN} S := 'Invalid date specifier'; {$ELSE}
                        S := 'Неверный формат даты'; {$ENDIF}
      MessageBox(S, NIL, mfError + mfOkButton);
      SelectAll(True);
      Ok := False;
    end;
  end;
  if Ok then Valid := Inherited Valid(Command) else Valid := False;
end;

Constructor TActionInputLine.Init(var Bounds: TRect; AMaxLen: Integer; AWriteEnable : Boolean);
Begin
  Inherited Init(Bounds, AMaxLen);
  WriteEnable := AWriteEnable;
End;

Procedure   TActionInputLine.HandleEvent(var Event: TEvent);
Begin
  if WriteEnable then Inherited HandleEvent(Event) else TView.HandleEvent(Event);
  case Event.What of
  evCommand   : if Event.Command = cmActionInputSend then begin
    Action;
    ClearEvent(Event);
  end;
  evMouseDown : if Event.Double then begin
    Action;
    ClearEvent(Event);
  end;
  evKeyDown   : if {(Event.KeyCode = kbDown) or }(Event.CharCode = ' ') then begin
    Action;
    ClearEvent(Event);
  end;
  else end;
End;

Procedure   TActionInputLine.Action;
Begin
End;

Function  TQuickSendInputLine.DataSize: Word;
Begin
  DataSize := 0;
End;

Procedure TQuickSendInputLine.SetData(var Rec);
Begin
End;

Procedure TQuickSendInputLine.GetData(var Rec);
Begin
End;

Procedure TQuickSendInputLine.HandleEvent(var Event : TEvent);
Var
  Is : Boolean;
Begin
  Is := ((State and sfFocused) <> 0) and (Event.What = evKeyDown) and
     ((Event.CharCode in [#8, #25, ' '..#255]) or (Event.KeyCode = kbDel));
  Inherited HandleEvent(Event);
  if Is then Message(Owner, evBroadCast, cmQuickSendChanged, @Self);
End;

Constructor TMovableButton.Init;
Begin
  Inherited Init(Bounds, ATitle, ACommand, AFlags);
  GrowMode := gfGrowAll;
End;

Constructor TKeyButton.Init;
Begin
  Inherited Init(Bounds, ATitle, ACommand, AFlags);
  Key := AKey;
  Options := Options and (not ofPreProcess) or ofPostProcess;
End;

Procedure   TKeyButton.HandleEvent(var Event: TEvent);
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evKeyDown) and (Event.KeyCode = Key) then begin
    Press;
    ClearEvent(Event);
  end;
End;

Constructor TKeyButton.Load(var S : TStream);
Begin
  Inherited Load(S);
  S.Read(Key, 2);
End;

Procedure   TKeyButton.Store(var S : TStream);
Begin
  Inherited Store(S);
  S.Write(Key, 2);
End;


Constructor TPartnerButton.Init;
Begin
  Inherited Init(Bounds, ATitle, ACommand, AFlags);
  Partner := APartner;
End;

Procedure   TPartnerButton.HandleEvent(var Event: TEvent);
Var
  Toggle : Boolean;
Begin
  Toggle := AmDefault and (Event.What = evBroadCast) and (Event.Command = cmDefault);
  Inherited HandleEvent(Event);
  if (Partner <> Nil) and
  ((Event.What = evBroadcast) and (Event.Command = cmPartnerButtonToggle)) or
    Toggle then begin
    MakeDefault(False);
    Partner^.MakeDefault(True);
    ClearEvent(Event);
  end;
End;

Constructor TPartnerButton.Load(var S : TStream);
Begin
  Inherited Load(S);
  GetPeerViewPtr(S, Partner);
End;

Procedure   TPartnerButton.Store(var S : TStream);
Begin
  Inherited Store(S);
  PutPeerViewPtr(S, Partner);
End;

Function MakeGrow(P : PView; GrowFlag : Word) : PView;
Begin
  P^.GrowMode := GrowFlag;
  MakeGrow := P;
End;

constructor TDumbInputLine.Init(var Bounds: TRect; AMaxLen: Integer);
Begin
  Inherited Init(Bounds, AMaxLen);
  HideCursor;
  SetState(sfDisabled, True);
End;

Procedure   TDumbInputLine.HandleEvent(var Event: TEvent);
Begin
  TView.HandleEvent(Event);
End;

Function  TForceTypeInputLine.Valid(Command : Word) : Boolean;
Begin
  Valid := False;
  if not(Inherited Valid(Command)) then Exit;

  if (Command <> cmCancel) and (Command <> cmValid) and (Command <> cmClose) and (Command <> cmQuit) then
    if Trim(Data^) = '' then begin
      Select;
      MessageBox(
        {$IFNDEF RUSSIAN}
        'Line cannot be empty'
        {$ELSE}
        'Строка не может быть пуста'
        {$ENDIF}
      , NIL, mfError + mfOkButton);
        SelectAll(True);
      Exit;
    end;
  Valid := True;
End;

Constructor TCodeInputLine.Init;
Begin
  Inherited Init(Bounds, AMaxLen);
  MinLen := AMaxLen;
  Align := afNormal;
End;

Constructor TCodeInputLine.Load;
Begin
  Inherited Load(S);
  S.Read(MinLen, SizeOf(MinLen));
  S.Read(Align, SizeOf(Align));
End;

Procedure TCodeInputLine.Store;
Begin
  Inherited Store(S);
  S.Write(MinLen, SizeOf(MinLen));
  S.Write(Align, SizeOf(Align));
End;

Procedure TCodeInputLine.GetData;
Var
  S : String absolute Rec;
Begin
{  case Align and afAlignMask of
  afRight  : begin
    S := LeftPad(Trim(Data^), MaxLen);
    Data^ := S;
  end;
  afCenter : begin
    S := Center(Trim(Data^), MaxLen);
    Data^ := S;
  end;
  else end;}
  Inherited GetData(Rec);
End;


Procedure TCodeInputLine.SetData;
Begin
  if String(Rec) = '' then begin
    Data^ := CharStr('0', MinLen);
    case Align and afAlignMask of
    afRight  : String(Rec) := LeftPad(Trim(String(Rec)), MaxLen);
    afCenter : String(Rec) := Center(Trim(String(Rec)), MaxLen);
    else end;
    SelectAll(True);
  end else begin
    case Align and afAlignMask of
    afRight  : String(Rec) := LeftPad(Trim(String(Rec)), MaxLen);
    afCenter : String(Rec) := Center(Trim(String(Rec)), MaxLen);
    else end;
    Inherited SetData(Rec);
  end;
End;


Function  TCodeInputLine.Valid(Command : Word) : Boolean;
Var
  I : Real;
  J : Integer;
Begin
  Valid := False;
  if not(Inherited Valid(Command)) then Exit;

  if (Command <> cmCancel) and (Command <> cmValid) and (Command <> cmClose) and (Command <> cmQuit) then begin
    Val(Trim(Data^), I, J);
    case Align and afAlignMask of
    afRight  : Data^ := LeftPad(Trim(Data^), MaxLen);
    afCenter : Data^ := Center(Trim(Data^), MaxLen);
    else end;
    if (J <> 0) or (Length(Trim(Data^)) < MinLen) then begin
      Select;
      MessageBox(
        {$IFNDEF RUSSIAN}
        'Line must contain ' + Long2Str(MinLen) + ' digits minimum.'
        {$ELSE}
        'Строка должна содержать минимум ' + Long2Str(MinLen) + ' цифр.'
        {$ENDIF}
      , NIL, mfError + mfOkButton);
      if Length(Trim(Data^)) < MinLen then begin
        if (Align and afRightPad) <> 0 then Data^ := PadCh(Data^, '0', MinLen)
        else Data^ := LeftPadCh(Data^, '0', MinLen);
      end;
      for J := 1 to MaxLen do
        if not (Data^[J] in ['0'..'9']) then Data^[J] := '0';
      SelectAll(True);
      Exit;
    end;
  end;
  Valid := True;
End;

end.
