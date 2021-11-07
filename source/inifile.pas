{
  DK Inc. 1996
  .INI file support unit
}
{$O+}
{$F+,I-,R-,S-,C-,V-}
Unit INIFile;

Interface

Uses DOS, Objects, EgString;

{Text processing flags}
Const
  ltNone     = 0;
  ltTopics   = 1;
  ltLines    = 2;
  ltComments = 4;
  ltSpaces   = 8;
  ltAll      = 15;

{Variable processing flag}
  coString = 0;
  coChar   = 1;
  coByte   = 2;
  coWord   = 3;
  coLong   = 4;
  coBool   = 5;
  coReal   = 6;


Type
  PINIFile = ^TINIFile;
  TINIFile = Object(TObject)
    FileName : String;
    Keys     : PCollection;
    Modified : Boolean;
    Constructor Init(AFileName : String);
    Destructor  Done; Virtual;
    Procedure   Rewrite;
    Function    GetValue(Topic, Line, Default : String; UseTearLine : Boolean) : String;
    Function    UpdateValue(Topic, Line, Default : String) : String;
    Procedure   AddTopic(Topic, Line, Value : String);
    Procedure   FreeTopic(Topic : String);
    Procedure   ForEach(Topic : String; Action : Pointer);
                {Action must be Proc(Line, Value : PString); far local;}
    Function    GetTopicIndex(Topic : String) : Integer;
    Function    GetTopic(Index : Word) : String;
    Function    CheckTopicIndex(Topic : String) : Integer;
    Procedure   UpdateVar(var V; Topic, Line : String; AType : Word; AMaxStrLen : Byte; UseVar : Boolean);
    Procedure   DeleteValue(Topic, Line : String);
  Private
    Function GetStream : PBufStream;
  End;

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    Status : Integer;
    procedure Error(Code, Info: Integer); virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

  PTopicLine = ^TTopicLine;
  TTopicLine = Object(TObject)
    Lines, Values, Comments : PLineCollection;
    Theme         : PString;
    Constructor Init(ATheme : String);
    Constructor LoadProfile(var S : TStream);
    Destructor  Done; Virtual;
    Procedure   NewEntry(Entry : String);
    Function    NewEntryIndirect(Entry : String; EnableMultiple : Boolean) : Boolean;
    Procedure   RewriteTheme(Action : Pointer; Flags : Word);
    Function    GetValue(Line, Default : String; UseTearLine : Boolean) : String;
    Function    GetValueAt(Line : Integer; Default : String; UseTearLine : Boolean) : String;
    Procedure   SetValue(Line, Value : String; DelComments, EnableMultiple : Boolean);
  End;

Implementation

Type
  PNCEC = ^TNCEC;
  TNCEC = Object(TCollection)
    Procedure Error(Code, Info: Integer); virtual;
  End;
Procedure TNCEC.Error; Begin End;

{TTopicLine}
Constructor TTopicLine.Init;
Begin
  Inherited Init;
  Theme := NewStr(ATheme);
  Lines    := New(PLineCollection, Init(2, 2));
  Values   := New(PLineCollection, Init(2, 2));
  Comments := New(PLineCollection, Init(2, 2));
End;

Constructor TTopicLine.LoadProfile(var S : TStream);

Var
  LastPos, CurPos : LongInt;
  WasSC : Boolean;
  CanLineOK : Boolean;
  LineCount : Word;

Function Wait(FirstCD : Boolean) : String;
Label Again;
Var
  C : Char;
  X : String;
Begin
  LastPos := CurPos;
  CanLineOK := True;
  X := '';
  Wait := '';
  Again:S.Read(C, 1);
  Inc(CurPos);
  if S.Status <> 0 then begin
    S.Reset;
    Wait := '';
    WasSC := False;
    Exit;
  end;
  if FirstCD then begin
    if (C = ' ') or (C = #9) or (C = #0) then Goto Again;
  end else begin
    if (C = ' ') or (C = #13) or (C = #10) or (C = #9) or (C = #0) then Goto Again;
  end;
  X[0] := #1;
  X[1] := C;
  Wait := X;
  if not WasSC and ((C = '[') or (C = ']') or (C = '=')) then Exit;
  if C = #13 then begin
    S.Read(C, 1);
    Inc(CurPos);
    Wait := '';
    if C = #10 then begin
      WasSC := False;
      Inc(LineCount);
      Exit;
    end else begin
      S.Seek(CurPos - 1);
      Dec(CurPos);
      WasSC := False;
      Exit;
    end;
  end;
  if (C = '=') or (C = ';') then begin
    WasSC := True;
    if C = ';' then CanLineOK := False;
  end;
  Repeat
    S.Read(C, 1);
    Inc(CurPos);
    if C = #13 then begin
      S.Read(C, 1);
      Inc(CurPos);
      if C = #10 then begin
        CanLineOK := False;
        Inc(LineCount);
        Break;
      end;
    end;
    if S.Status <> 0 then begin
      S.Reset;
      Break;
    end;
    if not WasSC and ((C = '[') or (C = ']') or (C = '=')) then begin
      S.Seek(CurPos - 1);
      Dec(CurPos);
      Break;
    end;
    X := X + C;
    if (C = '=') or (C = ';') then begin
      WasSC := True;
      if C = ';' then CanLineOK := False;
    end;
  Until False;
  Wait := Trim(X);
  WasSC := False;
End;

Function Next(X : String) : Boolean;
Begin
  Next := False;
  if X = '[' then begin
    S.Seek(LastPos);
    Next := True;
  end;
End;


Label NoTheme;
Var
  X, XD : String;
  I, J : Word;

Begin
  Inherited Init;
  LineCount := 0;
  CurPos := S.GetPos;
  WasSC := False;
  X := Wait(False);
  if X <> '[' then begin
    if X = '' then Fail;
    Theme := Nil;
    Lines    := New(PLineCollection, Init(2, 2));
    Values   := New(PLineCollection, Init(2, 2));
    Comments := New(PLineCollection, Init(2, 2));
    Goto NoTheme;
  end;
  Theme := NewStr(Wait(False));
  if Theme = Nil then Fail;
  X := Wait(False);
  if X <> ']' then Fail;
  Lines    := New(PLineCollection, Init(2, 2));
  Values   := New(PLineCollection, Init(2, 2));
  Comments := New(PLineCollection, Init(2, 2));
  Repeat
    X := Wait(False);
  NoTheme:
    if X = '' then Break;
    if Next(X) then Exit;
    if X[1] = ';' then begin
      Comments^.Insert(NewStr(X+Char(Lo(LineCount))+Char(Hi(LineCount))));
      Continue;
    end;
    if not CanLineOK then begin
      I := Pos(';', X);
      if I = 0 then SetValue(X, ' ', True, False) else
        SetValue(Copy(X, 1, I - 1), Copy(X, I, Length(X)-I+1), True, False);
      Continue;
    end;
    XD := Wait(True);
    if Next(X) then Exit;
    if XD = '=' then begin
      WasSC := True;
      XD := Wait(True);
      if XD = '' then XD := ' ';
    end else begin
      I := Pos(';', X);
      if I = 0 then SetValue(X, ' ', True, False) else
        SetValue(Copy(X, 1, I - 1), Copy(X, I, Length(X)-I+1), True, False);
      S.Seek(LastPos);
      Continue;
    end;
    SetValue(X, XD, True, True);
  Until False;
End;

Destructor TTopicLine.Done;
Begin
  if Theme <> Nil then DisposeStr(Theme);
  Dispose(Lines, Done);
  Dispose(Values, Done);
  Dispose(Comments, Done);
  Inherited Done;
End;

Procedure TTopicLine.NewEntry;
Begin
  NewEntryIndirect(Entry, False);
End;

Function  TTopicLine.NewEntryIndirect;
Var
  Line, Value, XLine : String;
  I, J : Word;
  P : Pointer;

function Match(Item: PString): Boolean; far;
Begin
  Match := StUpcase(Item^) = XLine;
End;

Begin
  NewEntryIndirect := False;
  I := Pos('=', Entry);
  if (Entry = '') or (I = 0) then Exit;
  Line := Copy(Entry, 1, I-1);
  Value := Copy(Entry, I+1, Length(Entry) - I + 1);
  XLine := StUpCase(Line);
  P := Lines^.FirstThat(@Match);
  if Value = '' then Value := ' ';
  if (P = Nil) or EnableMultiple then begin
    if Line = '' then Exit;
    Lines^.Insert(NewStr(Line));
    Values^.Insert(NewStr(Value));
  end else begin
    I := Lines^.IndexOf(P);
    Values^.AtFree(I);
    Values^.AtInsert(I, NewStr(Value));
  end;
  NewEntryIndirect := True;
End;

Procedure TTopicLine.RewriteTheme;
Var
  I, J : Word;
  P : Pointer;
  S : String;
  LC : Word;

Procedure Process; Inline($c4/$7E/<P/$06/$57/$FF/$76/$00/$FF/$5E/<Action);

Begin
  I := Lines^.Count;
  LC := 1;
  if Theme <> Nil then begin
    S := '[' + Theme^ + ']' + #13#10;
  end else S := '';
  P := @S;
  if (Flags and ltTopics) <> 0 then Process;

  if I > 0 then for I := 0 to Lines^.Count - 1 do begin
    if (Flags and ltSpaces) <> 0 then
    if Comments^.Count > 0 then for J := 0 to Comments^.Count - 1 do begin
      P := Comments^.At(J);
      if Word(Pointer(LongInt(P) + Byte(P^) - 1)^) = LC then begin
        S := String(P^);
        P := @S;
        Dec(Byte(S[0]), 2);
        S := S + #13#10;
        Inc(LC);
        Process;
      end;
    end;
    S := String(Lines^.At(I)^) + '='+ String(Values^.At(I)^) + #13#10;
    Inc(LC);
    P := @S;
    if (Flags and ltSpaces) <> 0 then Process;
  end;

  if (Flags and ltSpaces) <> 0 then
  if Comments^.Count > 0 then for J := 0 to Comments^.Count - 1 do begin
    P := Comments^.At(J);
    if Word(Pointer(LongInt(P) + Byte(P^) - 1)^) >= LC then begin
      S := String(P^);
      P := @S;
      Dec(Byte(S[0]), 2);
      S := S + #13#10;
      Inc(LC);
      Process;
    end;
  end;

  S := #13#10;
  P := @S;
  if (Flags and ltSpaces) <> 0 then Process;
End;

Function  TTopicLine.GetValue(Line, Default : String; UseTearLine : Boolean) : String;

function Match(Item: PString): Boolean; far;
Begin
  Match := StUpCase(Item^) = Line;
End;

Var
  P : Pointer;
  I : Word;
Begin
  Line := StUpcase(Line);
  if Line = '' then begin
    if Theme = Nil then GetValue := '' else GetValue := StUpCase(Theme^);
    Exit;
  end;
  P := Lines^.FirstThat(@Match);
  if P = Nil then GetValue := Default else begin
    I := Lines^.IndexOf(P);
    Line := String(Values^.At(I)^);
    if not UseTearLine then begin
      I := Pos(';', Line);
      if I <> 0 then Line[0] := Char(I-1);
    end;
    GetValue := Trim(Line);
  end;
End;

Function  TTopicLine.GetValueAt(Line : Integer; Default : String; UseTearLine : Boolean) : String;
Var
  P : PString;
Begin
  P := Lines^.At(Line);
  if P = Nil then GetvalueAt := Default else GetValueAt := GetValue(P^, Default, UseTearLine);
End;

Procedure  TTopicLine.SetValue;
Var
  S : String;
  I : Integer;
Begin
  if not DelComments then begin
    S := GetValue(Line, '', True);
    I := Pos(';', S);
    if I <> 0 then S := Copy(S, I, Length(S) - I + 1) else S := '';
    Value := Value + S;
  end;
  NewEntryIndirect(Line+'='+Value, EnableMultiple);
End;

{TINIFile}
Constructor TINIFile.Init;
Var
  S : PBufStream;
  _D : DirStr;
  _N : NameStr;
  _E : ExtStr;
  P  : PTopicLine;

Begin
  Inherited Init;
  FileName := AFileName;
  FSplit(FileName, _D, _N, _E);
  if _E = '' then _E := '.INI';
  FileName := _D+_N+_E;
  S := GetStream;
  if S = Nil then Fail;
  Keys := New(PNCEC, Init(2, 2));
  Repeat
    New(P, LoadProfile(S^));
    if P = Nil then Break;
    Keys^.Insert(P);
  Until False;
  Dispose(S, Done);
  Modified := False;
End;

Destructor TINIFile.Done;
Begin
  if Modified then Rewrite;
  Dispose(Keys, Done);
  Inherited Done;
End;

Procedure TINIFile.Rewrite;
Var
  S : PBufStream;
  F : File;
  I : Word;

Procedure Send(Item : Pointer); Far;
Begin
  S^.Write(Pointer(LongInt(Item)+1)^, Byte(Item^));
End;

Begin
  Assign(F, FileName);
  Erase(F);
  S := GetStream;
  if S = Nil then Exit;
  if Keys^.Count > 0 then for I := 0 to Keys^.Count - 1 do
     PTopicLine(Keys^.At(I))^.RewriteTheme(@Send, ltAll);
  Dispose(S, Done);
  Modified := False;
End;


Function TINIFile.GetStream;
Var
  F : PBufStream;
Begin
  GetStream := Nil;
  New(F, Init(FileName, stOpen, 512));
  if F = Nil then Exit;
  if F^.Status <> 0 then begin
    F^.Done;
    F^.Init(FileName, stCreate, 512);
    if F^.Status <> 0 then begin
      Dispose(F, Done);
      Exit;
    end;
  end;
  GetStream := F;
End;

Function  TINIFile.GetValue(Topic, Line, Default : String; UseTearLine : Boolean) : String;
Var
  P : PTopicLine;
function Match(Item: PTopicLine): Boolean; far;
Begin
  if Item^.Theme = Nil then Match := Topic = '' else
                            Match := StUpCase(Item^.Theme^) = Topic;
End;

Begin
  Topic := StUpCase(Topic);
  P := Keys^.FirstThat(@Match);
  if P = Nil then GetValue := Default else
    GetValue := P^.GetValue(Line, Default, UseTearLine);
End;

Function  TINIFile.UpdateValue(Topic, Line, Default : String) : String;
Var
  D : String;
Begin
  D := GetValue(Topic, Line, Default, False);
  AddTopic(Topic, Line, D);
  UpDateValue := D;
  if not Modified then Modified := D <> Default;
End;

Procedure  TINIFile.AddTopic(Topic, Line, Value : String);

function Match(Item: PTopicLine): Boolean; far;
Begin
  if Item^.Theme = Nil then Match := Topic = '' else
    Match := StUpCase(Item^.Theme^) = StUpCase(Topic);
End;

Var
  P : PTopicLine;

Begin
  P := Keys^.FirstThat(@Match);
  if P = Nil then begin
    P := New(PTopicLine, Init(Topic));
    Keys^.Insert(P);
  end;
  if Line = '' then begin
    P^.Lines^.FreeAll;
    P^.Values^.FreeAll;
    P^.Comments^.FreeAll;
  end else P^.SetValue(Line, Value, False, False);
  Modified := True;
End;

Procedure   TINIFile.FreeTopic(Topic : String);
function Match(Item: PTopicLine): Boolean; far;
Begin
  if Item^.Theme = Nil then Match := Topic = '' else
  Match := StUpCase(Item^.Theme^) = StUpCase(Topic);
End;
Var
  P : PTopicLine;
Begin
  P := Keys^.FirstThat(@Match);
  if P <> Nil then Keys^.Free(P);
  Modified := True;
End;

Procedure TINIFile.ForEach;
Var
  X : Integer;
  P : PTopicLine;
  Line, Value : PString;
Begin
  X := GetTopicIndex(Topic);
  if X < 0 then Exit;
  P := PTopicLine(Keys^.At(X));
  if P^.Lines^.Count > 0 then for X := 0 to P^.Lines^.Count - 1 do begin
    Line := P^.Lines^.At(X);
    Value:= P^.Values^.At(X);
    asm
      les di, Line
      push es
      push di
      les di, Value
      push es
      push di
      push word ptr [bp]
      call Action
    end;
  end;
End;

Function    TINIFile.GetTopicIndex(Topic : String) : Integer;
function Match(Item: PTopicLine): Boolean; far;
Begin
  if Item^.Theme = Nil then Match := Topic = '' else
  Match := StUpCase(Item^.Theme^) = StUpCase(Topic);
End;
Begin
  GetTopicIndex := Keys^.IndexOf(Keys^.FirstThat(@Match));
End;

Function   TINIFile.CheckTopicIndex(Topic : String) : Integer;
Var
  X : Integer;
Begin
  X := GetTopicIndex(Topic);
  if X < 0 then begin
    AddTopic(Topic, '', '');
    Modified := True;
    X := GetTopicIndex(Topic);
  end;
  CheckTopicIndex := X;
End;

Function    TINIFile.GetTopic(Index : Word) : String;
Var
  P : PString;
Begin
  P := PTopicLine(Keys^.At(Index))^.Theme;
  if P = Nil then GetTopic := '' else  GetTopic := P^;
End;

Procedure  TINIFile.DeleteValue;
Var
  X : Integer;
  P : PTopicLine;

function Match(Item: PString): Boolean; far;
Begin
  Match := StUpcase(Item^) = StUpcase(Line);
End;

Begin
  X := GetTopicIndex(Topic);
  if X < 0 then Exit;
  P := PTopicLine(Keys^.At(X));
  X := P^.Lines^.IndexOf(P^.Lines^.FirstThat(@Match));
  if X < 0 then Exit;
  P^.Lines^.AtFree(X);
  P^.Values^.AtFree(X);
End;

Procedure  TINIFile.UpdateVar;
Var
  S : String;
  P : Pointer;
  R : Integer;
  F : Real;
  W : Word;
  L : LongInt;

Begin
  P := @V;
  case AType of
  coChar : W := 1;
  coByte : W := 1;
  coWord : W := 2;
  coLong : W := 4;
  coReal : W := 6;
  coBool : W := 1;
  else W := 256; end;

  case AType of
  coChar : S := Char(P^);
  coByte : S := Long2Str(Byte(P^));
  coWord : S := Long2Str(Word(P^));
  coLong : S := Long2Str(LongInt(P^));
  coReal : S := Real2Str(Real(P^),AMaxStrLen,AMaxStrLen);
  coBool : S := Long2Str(Byte(P^));
  else begin
    S := String(P^);
    if Byte(S[0]) > AMaxStrLen then Byte(S[0]) := AMaxStrLen;
  end end;

  if not UseVar then begin
    AddTopic(Topic, Line, S);
    Exit;
  end;

  S := UpdateValue(Topic, Line, S);
  case AType of
  coChar : Char(P^) := S[1];
  coByte, coWord, coLong, coBool : begin
    Val(S, L, R);
    if R = 0 then Move(L, P^, W);
  end;
  coReal : begin
    Val(S, F, R);
    if R = 0 then Move(F, P^, W);
  end;
  else begin
    if Byte(S[0]) > AMaxStrLen then Byte(S[0]) := AMaxStrLen;
    String(P^) := S;
  end;
  end;
End;

procedure TLineCollection.Error(Code, Info: Integer);
{ Save error status instead of giving a runtime error }
begin
  Status := Code;
end;

procedure TLineCollection.FreeItem(Item: Pointer);
begin
  if Item <> nil then DisposeStr(Item);
end;

function TLineCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem := S.ReadStr;
end;

procedure TLineCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.WriteStr(Item);
end;

End.