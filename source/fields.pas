{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        :  FIELDS                                              █
  █                                                                    █
  █ Description :  Some useful miscellaneous objects and routines      █
  █                                                                    █
  █ Author      :  Tony Berezin, Yuri Shimon, Oleg Oleinick            █
  █════════════════════════════════════════════════════════════════════█
  █                       Edit Fields                                  █
  █ TAbsEditField  - abstract type, don't use it !                     █
  █ TPatternEdit                                                       █
  █ TDateValidator                                                     █
  █ TDateInput                                                         █
  █ TNumericInput                                                      █
  █ TMoneyValidator                                                    █
  █ TMoneyInput                                                        █
  █                                                                    █
  █ TViewListBox                                                       █
  █ TEditListBox                                                       █
  █ TWordValidator                                                     █
  █ TLongValidator                                                     █
  █ TByteValidator                                                     █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}


unit Fields;

interface uses  { Why I use these units }
  Objects,  { Unit Fields is EG-TV-unit at all ! }
  Views,    { --- " --- }
  Drivers,  { --- " --- }
  Dialogs,  { --- " --- }
  Validate, { TAbsEditField uses a TValidator, and TDateValidator IS a TValidator }
  EGFont,   { TPatternEdit may change Bounds respectively to font dimensions }
  MsgBox,   { TDateValidator.Error calls MsgBox }
  EGDate,   { TDateInput is an input field for inputing DATES... }
  EGString, { TNumericInput uses it }
  EGInline
  ;


function LoCase(c: Char): Char;  { A la Driver's UpCase }

Const
  AllCharSet         {: CharSet} = [#32..#126, 'А'..'п', 'р'..'я'];
  RussianUpperCharSet{: CharSet} = [' ', 'А'..'Я'];
  RussianLowerCharSet{: CharSet} = [' ', 'а'..'п', 'р'..'я'];
  RussianCharSet     {: CharSet} = [' ', 'А'..'Я', 'а'..'п', 'р'..'я'];
  LetterUpperCharSet {: CharSet} = [' ', 'A'..'Z', 'А'..'Я'];
  LetterLowerCharSet {: CharSet} = [' ', 'a'..'z', 'а'..'п', 'р'..'я'];
  LetterCharSet      {: CharSet} = [' ', 'A'..'Z', 'А'..'Я', 'a'..'z', 'а'..'п', 'р'..'я'];
  NumberCharSet      {: CharSet} = [' ', '0'..'9'];
  DigitCharSet       {: CharSet} = [' ', '0'..'9', '+', '-', '.'];
  ScientificCharSet  {: CharSet} = [' ', '0'..'9', '+', '-', '.', 'e', 'E'];
  HexCharSet         {: CharSet} = [' ', '0'..'9', 'A'..'F', 'a'..'f'];
  BoolCharSet        {: CharSet} = [' ', 'f', 'F', 't', 'T'];
  YesNoCharSet       {: CharSet} = [' ', 'y', 'Y', 'n', 'N', 'д', 'Д', 'н', 'Н'];

Const
  InsOvrFlag: boolean = True;  { True - insert; that's default setting }

Const
  VisibleChars = [#32..#126, #128..#255];

Type
  TInsOvrMode = (UseGlobalFlag, AlwaysInsert, AlwaysOverwrite);

  PAbsEditField = ^TAbsEditField;
  TAbsEditField = object(TInputLine)
    constructor Init(var Bounds: TRect; InsOvrMode: TInsOvrMode;
                     AMaxLen: Integer; ComputeBounds: boolean);
    constructor Load(var S: TStream);
    destructor  Done;                                         virtual;
    procedure   Store(var S: TStream);
    procedure   GetData(var Rec);                             virtual;
    procedure   SetData(var Rec);                             virtual;
    function    DataSize: word;                               virtual;
    procedure   HandleEvent(var Event: TEvent);               virtual;
    procedure   Draw;                                         virtual;
    procedure   SetState(AState: Word; Enable: Boolean);      virtual;
    function    Valid(Command: Word): Boolean;                virtual;
    function    CheckCurPos: word;                            virtual;
    function    Ins: boolean;
   public
    InsOvr: TInsOvrMode;
    TextSelected: boolean;
    DefaultCurPos: Integer;
  end;

Const
  AnyChar      =    'X';  { #32..#126, #128..#255 }
  ForceUp      =    '!';
  ForceLo      =    'L';
  ForceMix     =    'x';

  AlphaOnly    =    'a';
  UpperAlpha   =    'U';
  LowerAlpha   =    'l';
  MixedAlpha   =    'm';

  RussianOnly  =    'а';
  UpperRussian =    'В';
  LowerRussian =    'н';
  MixedRussian =    'м';

  Numbers      =    '9';  { 0..9, space }
  Digits       =    '#';  { Same as Numbers, plus '+', '-', '.' }
  Scientific   =    'E';  { Same as Digits, plus 'e' }
  HexOnly      =    'K';  { 0..9, A..F, force upper }
  BoolOnly     =    'B';  { t, T, f, F }
  YesNoOnly    =    'Y';  { y, Y, n, N, д, Д, н, Н }

Const
  PatternsSet = [AnyChar, ForceUp, ForceLo, ForceMix,
                 AlphaOnly, UpperAlpha, LowerAlpha, MixedAlpha,
                 RussianOnly, UpperRussian, LowerRussian, MixedRussian,
                 Numbers, Digits, Scientific, HexOnly,
                 BoolOnly, YesNoOnly];

Type
  PPatternEdit = ^TPatternEdit;
  TPatternEdit = object(TAbsEditField)
    constructor Init(var Bounds: TRect; const APattern: string;
                     InsOvrMode: TInsOvrMode; ComputeBounds: boolean);
    constructor Load(var S: TStream);
    destructor  Done;                                        virtual;
    procedure   Store(var S: TStream);
    {procedure   Draw;                                        virtual;}
    procedure   SetData(var Rec);                            virtual;
    procedure   HandleEvent(var Event: TEvent);              virtual;
    function    ValidChar(Pos: word; var Ch: char): boolean; virtual;
    function    CheckCurPos: word;                           virtual;
    procedure   EmptyFill;
   public
    Pattern: PString;
    FillChar: char;
  end;


{
  Общая схема работы с полем ввода по образцу:
    - пробел всегда возможен;
    - если в поле не хватает символов, оно дополняется пробелами;
    - поле делится на подполя двух видов - "подполя переходов" и
      "подполя набора";  подполя набора выделяются по последовательностям
      одинаковых значимых (упомянутых в PatternsSet) символов в образце;
      подполя перехода выделяются по незначимым (не упомянутым в PatternsSet)
      символам в образце;  пример разбиения на подполя:
      "XXXX99.99XXXX";  здесь 2 подполя переходов - "XXXX99" и "99XXXX"
      и четыре подполя набора - "XXXX", "99", "99" и "XXXX";
    - текущая позиция в поле никогда не может совпадать с позицией
      символа в образце, не включенного в PatternsSet;
    - переход между подполями переходов по достижению в процессе
      перемещения по подполю перехода края подполя перехода, по Ctrl-Left,
      Ctrl-Right;
    - режим замены как обычно, только с перескоком через символы в
      образце, не включенные в PatternsSet;
    - режим вставки действует в пределах подполя набора, сдвигающиеся за
      правую границу подполя набора символы теряются;
    - удаление по BackSpace и по Del действует в пределах подполя набора,
      недостающие символы справа в подполе набора дополняются пробелами;
    - данные, передаваемые по SetData, должны соответствовать образцу,
      если соответствия нет, подполя инициализируются пробелами;
}


Const
  DatePict      = 'dd/mm/yyyy';
  DatePattern   = '99.99.9999';
  MoneyPattern  = '###,###,###,###.##';

Type
  PDateValidator = ^TDateValidator;
  TDateValidator = object(TValidator)
    constructor Init;
    procedure Error; virtual;
    function IsValid(const S: string): Boolean; virtual;
    function Transfer(var S: String; Buffer: Pointer;
      Flag: TVTransfer): Word; virtual;
  end;

Type
  PDateInput = ^TDateInput;   { Автоматически подсобачивается валидатор }
  TDateInput = object(TPatternEdit)
    constructor Init(var Bounds: TRect; InsOvrMode: TInsOvrMode; ComputeBounds: boolean);
  end;

Type
  PNumericInput = ^TNumericInput;
  TNumericInput = object(TAbsEditField)
   public
    Mask: PString;
    Decimal: word;
    NoUpdate: boolean;
    Val: Float;
    CanChangeSign: boolean;
   public
    constructor Init(var Bounds: TRect; const AMask: string; ComputeBounds: Boolean);
    destructor  Done;                                        virtual;
    procedure   Draw;                                        virtual;
    procedure   HandleEvent(var Event: TEvent);              virtual;
    procedure   GetData(var Rec);                            virtual;
    procedure   SetData(var Rec);                            virtual;
    function    DataSize: word;                              virtual;
    procedure   Str2Val;                                     virtual; {GIO}
    procedure   Val2Str;                                     virtual; {GIO}
  end;

Type
  PMoneyValidator = ^TMoneyValidator;
  TMoneyValidator = object(TValidator)
    constructor Init;
    function Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word; virtual;
  end;

Type
  PMoneyInput = ^TMoneyInput;   { Автоматически подсобачивается валидатор }
  TMoneyInput = object(TNumericInput)
    constructor Init(var Bounds: TRect; ComputeBounds: boolean);
  end;

{ TEditBox }

  PViewListBox = ^TEditListBox;
  TViewListBox = object(TListBox)
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure NewList(AList: PCollection); virtual;
    function  ValidFocused: boolean; virtual;
    function  ValidItem(Item: integer): boolean;
  end;

  PEditListBox = ^TEditListBox;
  TEditListBox = object(TViewListBox)
    procedure HandleEvent(var Event: TEvent); virtual;
    function CreateItem: Pointer; virtual;
    function EditItem(P: Pointer): Boolean; virtual;
    procedure DeleteItem(Item: Integer); virtual;
  end;

  PWordValidator = ^TWordValidator;
  TWordValidator = object(TRangeValidator)
    function IsValid(const S: string): Boolean; virtual;
    function Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word; virtual;
  end;

  PByteValidator = ^TByteValidator;
  TByteValidator = object(TWordValidator)
    function Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word; virtual;
  end;

  PLongValidator = ^TLongValidator;
  TLongValidator = object(TWordValidator)
    function Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word; virtual;
  end;

{ Fields registration procedure }

procedure RegisterFields;


{ Stream Registration Records }

const
  RPatternEdit: TStreamRec = (
     ObjType: 801;
     VmtLink: Ofs(TypeOf(TPatternEdit)^);
     Load:    @TPatternEdit.Load;
     Store:   @TPatternEdit.Store
  );

const
  RDateInput: TStreamRec = (
     ObjType: 802;
     VmtLink: Ofs(TypeOf(TDateInput)^);
     Load:    @TDateInput.Load;
     Store:   @TDateInput.Store
  );

function InsertView(Target: PGroup;
                    const Lbl: string; LblGrow: word; LeftLbl: boolean;
                    const Hdr: string; HdrGrow: word;
                    View: PView; ViewGrow: word): PView;

implementation

function InsertView(Target: PGroup;
                    const Lbl: string; LblGrow: word; LeftLbl: boolean;
                    const Hdr: string; HdrGrow: word;
                    View: PView; ViewGrow: word): PView;
var
  Control: PView;
  R      : TRect;
  w      : integer;
begin
  if Assigned(View) then begin
    if (Lbl <> '') or (Hdr <> '') then begin
      View^.GetBounds(R);
      SetFont(3);
      if Hdr <> '' then begin
        R.B.Y := R.A.Y - 1;
        R.A.Y := R.B.Y - GetHeight;
        Control := New(PStaticText, Init(R, Hdr));
        Control^.GrowMode := HdrGrow;
        Target^.Insert(Control);
      end;
      if Lbl <> '' then begin
        w := Target^.FontWidth(Filter(Lbl, ['~']));
        if LeftLbl and (R.A.X > w) then begin
          R.B.X := R.A.X;
          Dec(R.A.X, w);
        end else begin
          R.B.X := R.A.X + w;
          Dec(R.A.Y, Target^.FontHeight + 2);
        end;
        R.B.Y := R.A.Y + Target^.FontHeight;
        Control := New(PLabel, Init(R, Lbl, View));
        Control^.GrowMode := LblGrow;
        Target^.Insert(Control);
      end;
      RestoreFont;
    end;
    Target^.Insert(View);
    View^.GrowMode := ViewGrow;
  end;
  InsertView := View;
end;


{---No Object---}

function LoCase(c: Char): Char; assembler;
asm
    MOV    BX, SP
    MOV    AL, c
    CMP    AL, 'A'
    JB     @@End
    CMP    AL, 'Z'
    JA     @@1
    ADD    AL, 'a'-'A'
{$IFDEF Russian}
    JMP    @@End
@@1:
    CMP    AL, 'А'
    JB     @@End
    CMP    AL, 'П'
    JA     @@2
    ADD    AL, 'а'-'А'
    JMP    @@End
@@2:
    CMP    AL, 'Р'
    JB     @@End
    CMP    AL, 'Я'
    JA     @@3
    ADD    AL, 'р'-'Р'
    JMP    @@End
@@3:
    CMP    AL, '°'
    JA     @@End
    TEST   AL,1h
    JNZ    @@End
    INC    AL
{$ELSE}
@@1:
{$ENDIF}
@@End:
end;



{---TAbsEditField---}

constructor TAbsEditField.Init(var Bounds: TRect; InsOvrMode: TInsOvrMode; AMaxLen: Integer; ComputeBounds: boolean);
begin
  if ComputeBounds then begin
    SelectFontCaps(GlobalFont);
    Bounds.B.X := Bounds.A.X + AMaxLen*GetCharWidth;
    Bounds.B.Y := Bounds.A.Y + GetHeight;
  end;
  TView.Init(Bounds);
  MaxLen := AMaxLen;
  State := State or sfCursorVis;
  Options := Options or (ofSelectable + ofFirstClick + ofVersion20);
  GetMem(Data, MaxLen + 1);
  Data^ := '';
  DefaultCurPos := 1;
  InsOvr := InsOvrMode;
end;

constructor TAbsEditField.Load(var S: TStream);
begin
  TView.Load(S);
  S.Read(MaxLen, SizeOf(Integer) * 3 + SizeOf(Byte) * 2);
  GetMem(Data, MaxLen + 1);
  S.Read(Data^[0], 1);
  S.Read(Data^[1], Length(Data^));
  if Options and ofVersion >= ofVersion20 then
    Validator := PValidator(S.Get);
  Options := Options or ofVersion20;
end;

destructor TAbsEditField.Done;
begin
  FreeMem(Data, MaxLen + 1);
  SetValidator(nil);
  TView.Done;
end;

procedure TAbsEditField.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(MaxLen, SizeOf(Integer) * 3 + SizeOf(Byte) * 2);
  S.WriteStr(Data);
  S.Put(Validator);
end;

procedure TAbsEditField.HandleEvent(var Event: TEvent);
begin
  TView.HandleEvent(Event);
  if State and sfSelected = 0 then Exit;
  case Event.What of
    evKeydown: case Event.Keycode of
      kbIns: begin
        ClearEvent(Event);
        if InsOvr = UseGlobalFlag then begin
          InsOvrFlag := not InsOvrFlag;
          if TextSelected then begin
            TextSelected := False;
            DrawView;
          end;
          SetState(sfCursorIns, InsOvrFlag);
        end;
      end;
    end;
  end;
end;

function TAbsEditField.Valid(Command: Word): Boolean;
begin
  Valid := TView.Valid(Command);
  if (Validator <> nil) and (State and sfDisabled = 0) then
    if Command = cmValid then
      Valid := Validator^.Status = vsOk
    else if Command <> cmCancel then
      if not Validator^.Valid(Data^) then
      begin
        Select;
        Valid := False;
      end;
end;

function TAbsEditField.Ins: boolean;
begin
  case InsOvr of
    UseGlobalFlag:  Ins := InsOvrFlag;
    AlwaysInsert:   Ins := True;
    AlwaysOverwrite:Ins := False;
  end;
end;

procedure TAbsEditField.SetData(var Rec);
begin
  if (Validator = nil) or
    (Validator^.Transfer(Data^, @Rec, vtSetData) = 0) then
    Move(Rec, Data^[0], DataSize);

  if FlagIsSet(State, sfFocused) then TextSelected := True;
end;

procedure TAbsEditField.GetData(var Rec);
begin
  if (Validator = nil) or
    (Validator^.Transfer(Data^, @Rec, vtGetData) = 0) then
  begin
    FillChar(Rec, DataSize, #0);
    Move(Data^, Rec, Length(Data^) + 1);
  end;
end;

function TAbsEditField.DataSize: Word;
var
  DSize: Word;
begin
  DSize := 0;

  if Validator <> nil then
    DSize := Validator^.Transfer(Data^, nil, vtDataSize);

  if DSize <> 0 then
    DataSize := DSize
  else
    DataSize := MaxLen + 1;
end;

function TAbsEditField.CheckCurPos: word;
begin
  CheckCurPos := CurPos;
end;

procedure TAbsEditField.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if AState = sfFocused then begin
    TextSelected := Enable;
    CurPos := DefaultCurPos; CheckCurPos;
    DrawView;
  end;
end;

procedure TAbsEditField.Draw;
var
  Color: Word;
begin
  if TextSelected then
    Color := GetColor($0506)
  else
    if State and sfFocused = sfFocused then
      Color := GetColor($0304)
    else
      Color := GetColor($0102);

  Bar{Pattern}(0, 0, Size.X, Size.Y, Lo(Color));
  WrStr(0, 0, Copy(Data^, 1, Size.X div CharWidth), Hi(Color));

  SetCursor(Pred(CurPos) * CharWidth, 0);
end;

{---TPatternEdit---}

procedure TPatternEdit.EmptyFill;
var
  i: word;
begin
  Data^ := Pattern^;
  for i:=1 to MaxLen do if Data^[i] in PatternsSet then Data^[i] := FillChar;
end;

procedure TPatternEdit.SetData(var Rec);
var
  i: word;
begin
  inherited SetData(Rec);  { Вначале валидатором }

{  if Length(Data^)<>MaxLen then EmptyFill else}
  if Length(Data^)<>MaxLen then Data^ := Copy(Pad(Data^, MaxLen), 1, MaxLen);
  for i:=1 to MaxLen do if not ValidChar(i, Data^[i]) then begin
    EmptyFill;
    Exit;
  end;
end;

{ Функция несколько выпадает из канвы TurboVision'а }
function TPatternEdit.ValidChar(Pos: word; var Ch: char): boolean;
var
  c: char;
  r: boolean;
begin
  r := True;
  ValidChar := false;
  if (Pos>MaxLen) or (Pos=0) then Exit;
  c := Pattern^[Pos];
  if not (c in PatternsSet) then begin
    if c=Ch then ValidChar := r;
    Exit;
  end;
  case c of
    AnyChar: begin
      if Ch in AllCharSet then ValidChar := r;
      Exit;
    end;
    ForceUp: begin
      if Ch in AllCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    ForceLo: begin
      if Ch in AllCharSet then begin
        Ch := LoCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    ForceMix: begin
      if Ch in AllCharSet then begin
        if (Pos=1) or (Pattern^[Pos-1]<>c) or (Data^[Pos-1]=' ') then
          Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    AlphaOnly: begin
      if Ch in LetterCharSet then ValidChar := r;
      Exit;
    end;
    UpperAlpha: begin
      if Ch in LetterCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    LowerAlpha: begin
      if Ch in LetterCharSet then begin
        Ch := LoCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    MixedAlpha: begin
      if Ch in LetterCharSet then begin
        if (Pos=1) or (Pattern^[Pos-1]<>c) or (Data^[Pos-1]=' ') then
          Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    RussianOnly: begin
      if Ch in RussianCharSet then ValidChar := r;
      Exit;
    end;
    UpperRussian: begin
      if Ch in RussianCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    LowerRussian: begin
      if Ch in RussianCharSet then begin
        Ch := LoCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    MixedRussian: begin
      if Ch in RussianCharSet then begin
        if (Pos=1) or (Pattern^[Pos-1]<>c) or (Data^[Pos-1]=' ') then
          Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    Numbers: begin
      if Ch in NumberCharSet then ValidChar := r;
      Exit;
    end;
    Digits: begin
      if Ch in DigitCharSet then ValidChar := r;
      Exit;
    end;
    Scientific: begin
      if Ch in ScientificCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    HexOnly: begin
      if Ch in HexCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    BoolOnly: begin
      if Ch in BoolCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
    YesNoOnly: begin
      if Ch in YesNoCharSet then begin
        Ch := UpCase(Ch);
        ValidChar := r;
      end;
      Exit;
    end;
  end;
end;

constructor TPatternEdit.Init(var Bounds: TRect; const APattern: string; InsOvrMode: TInsOvrMode; ComputeBounds: boolean);
var
  b: byte;
begin
  inherited Init(Bounds, InsOvrMode, Length(APattern), ComputeBounds);
  EventMask := EventMask or evMouseMove;
  Pattern := NewStr(APattern);
  FillChar := ' ';

  { Проверка на корректность шаблона }
  b := 1;
  while (b<=MaxLen) and (not (APattern[b] in PatternsSet)) do Inc(b);
  if b>MaxLen then begin
    Size.X := 0; Size.Y := 0;  { Вместо Fail, обсудить с Олегом }
  end else begin
    EmptyFill;
  end;
end;

constructor TPatternEdit.Load(var S: TStream);
var
  st: string;
begin
  TAbsEditField.Load(S);
  S.Read(FillChar, SizeOf(Char));
  S.Read(st[0], 1);
  S.Read(st[1], Length(st));
  Pattern := NewStr(st);
end;

destructor TPatternEdit.Done;
begin
  DisposeStr(Pattern);
  inherited Done;
end;

procedure TPatternEdit.Store(var S: TStream);
begin
  TAbsEditField.Store(S);
  S.Write(FillChar, SizeOf(Char));
  S.WriteStr(Pattern);
end;

function TPatternEdit.CheckCurPos: word;
var
  i: word;
begin
  i := CurPos;
  while (i<=MaxLen) and (not (Pattern^[i] in PatternsSet)) do Inc(i);
  if i>MaxLen then begin
    i := CurPos;
    while (i>=1) and (not (Pattern^[i] in PatternsSet)) do Dec(i);
  end;
  { i не может быть равным нулю благодаря проверке в Init }
  CurPos := i;
  CheckCurPos := i;
end;

function Min(A, B: integer): integer;
inline (
    $5B/      {pop  bx (A)}
    $58/      {pop  ax (B)}
    $39/$D8/  {cmp  ax, bx}
    $7C/$02/  {jl   @@1}
    $89/$D8   {mov  ax, bx}
{@@1:}
);

procedure TPatternEdit.HandleEvent(var Event: TEvent);

  procedure SubJumpBounds(var Left, Right: word);
  var
    i, l: word;
  begin
    i := CheckCurPos;
    while (i>=1) and (Pattern^[i] in PatternsSet) do Dec(i);
    l := Succ(i);  { На случай вызова SubJumpBounds(CurPos, ...) }
    i := CurPos;
    while (i<=MaxLen) and (Pattern^[i] in PatternsSet) do Inc(i);
    Right := i - 1;
    Left := l;
  end;

  procedure SubTypeWriteBounds(var Left, Right: word);
  var
    i, l: word;
    c: char;
  begin
    i := CheckCurPos; c := Pattern^[i];
    while (i>=1) and (Pattern^[i] = c) do Dec(i);
    l := Succ(i);  { На случай вызова SubTypeWriteBounds(CurPos, ...) }
    i := CurPos;
    while (i<=MaxLen) and (Pattern^[i] = c) do Inc(i);
    Right := i - 1;
    Left := l;
  end;

  function NextSubJump(var Left, Right: word): boolean;
  var
    i, l, r: word;
  begin
    NextSubJump := False;
    SubJumpBounds(l, r);  { Для текущего поля }
    i := Succ(r);
    while (i<=MaxLen) and (not (Pattern^[i] in PatternsSet)) do Inc(i);
    if i>MaxLen then Exit;
    CurPos := i;
    SubJumpBounds(Left, Right);  { Для следующего поля }
    NextSubJump := True;
  end;

  function PrevSubJump(var Left, Right: word): boolean;
  var
    i, l, r: word;
  begin
    PrevSubJump := False;
    SubJumpBounds(l, r);  { Для текущего поля }
    i := l - 1;
    while (i>=1) and (not (Pattern^[i] in PatternsSet)) do Dec(i);
    if i=0 then Exit;
    CurPos := i;
    SubJumpBounds(Left, Right);  { Для предыдущего поля }
    PrevSubJump := True;
  end;

  function NextSubTypeWrite(var Left, Right: word): boolean;
  var
    i, l, r: word;
  begin
    NextSubTypeWrite := False;
    SubTypeWriteBounds(l, r);  { Для текущего поля }
    i := Succ(r);
    while (i<=MaxLen) and (not (Pattern^[i] in PatternsSet)) do Inc(i);
    if i>MaxLen then Exit;
    CurPos := i;
    SubTypeWriteBounds(Left, Right);  { Для следующего поля }
    NextSubTypeWrite := True;
  end;

  function PrevSubTypeWrite(var Left, Right: word): boolean;
  var
    i, l, r: word;
  begin
    PrevSubTypeWrite := False;
    SubTypeWriteBounds(l, r);  { Для текущего поля }
    i := l - 1;
    while (i>=1) and (not (Pattern^[i] in PatternsSet)) do Dec(i);
    if i=0 then Exit;
    CurPos := i;
    SubTypeWriteBounds(Left, Right);  { Для предыдущего поля }
    PrevSubTypeWrite := True;
  end;

  procedure ClearJustMode;
  begin
    if TextSelected then begin
      TextSelected := False;
      DrawView;
    end;
  end;

procedure SetCurPos;
begin
  DrawView; { Потом сделать умнее }
  {SetCursor(Pred(CurPos) * CharWidth, 0);}
end;

var
  l, r, cp: word;
  Rectan: TRect;

begin
  inherited HandleEvent(Event);
  if State and sfSelected = 0 then Exit;
  case Event.What of
    evMouseDown, evMouseMove: if (Event.Buttons and mbLeftButton) = mbLeftButton then begin
      GetExtent(Rectan);
      MakeGlobal(Rectan.A, Rectan.A);
      MakeGlobal(Rectan.B, Rectan.B);
      CurPos := Min(MaxLen, (Event.Where.X - Rectan.A.X) div CharWidth  +  1);
      CheckCurPos;
      ClearJustMode; SetCurPos;
    end;
    evKeydown: begin
      case Event.Keycode of
        kbHome: begin
          ClearJustMode;
          cp := CurPos; CurPos := 1; CheckCurPos;
          if cp<>CurPos then SetCurPos;
        end;
        kbEnd: begin
          ClearJustMode;
          cp := CurPos; CurPos := MaxLen; CheckCurPos;
          if cp<>CurPos then SetCurPos;
        end;
        kbRight: begin
          ClearJustMode;
          cp := CurPos; SubJumpBounds(l, r);
          if CurPos<>r then Inc(CurPos) else
          if NextSubJump(l, r) then CurPos := l;
          if cp<>CurPos then SetCurPos;
        end;
        kbCtrlRight: begin
          ClearJustMode;
          cp := CurPos; if NextSubJump(l, r) then CurPos := l;
          if cp<>CurPos then SetCurPos;
        end;
        kbLeft: begin
          ClearJustMode;
          cp := CurPos; SubJumpBounds(l, r);
          if CurPos<>l then Dec(CurPos) else
          if PrevSubJump(l, r) then CurPos := r;
          if cp<>CurPos then SetCurPos;
        end;
        kbCtrlLeft: begin
          ClearJustMode;
          cp := CurPos; if PrevSubJump(l, r) then CurPos := l;
          if cp<>CurPos then SetCurPos;
        end;
        kbDel: begin
          if TextSelected then begin
            EmptyFill; ClearJustMode;
          end else begin
            SubTypeWriteBounds(l, r);
            if CurPos<r then Move(Data^[CurPos+1], Data^[CurPos], r-CurPos);
            Data^[r] := FillChar;
            DrawView;
          end;
        end;
        kbBack: begin
          if TextSelected then begin
            EmptyFill; ClearJustMode;
          end else begin
            { Perform LEFT }
            cp := CurPos; SubJumpBounds(l, r);
            if CurPos<>l then Dec(CurPos) else
            if PrevSubJump(l, r) then CurPos := r;
            if cp<>CurPos then begin
              { Perform DEL }
              SubTypeWriteBounds(l, r);
              if CurPos<r then Move(Data^[CurPos+1], Data^[CurPos], r-CurPos);
              Data^[r] := FillChar;
              DrawView;
            end;
          end;
        end;
      else
        if ValidChar(CurPos, Event.CharCode) then begin
          if TextSelected then begin
            EmptyFill;
            Data^[1] := Event.CharCode;
            CurPos := 2;
            ClearJustMode;
          end else begin
            if Ins then begin
              { Insert mode }
              SubTypeWriteBounds(l, r);
              if CurPos<r then Move(Data^[CurPos], Data^[CurPos+1], r-CurPos);
            end;
            { Set CHARACTER }
            Data^[CurPos] := Event.CharCode;
            { Perform RIGHT }
            cp := CurPos; SubJumpBounds(l, r);
            if CurPos<>r then Inc(CurPos) else
            if NextSubJump(l, r) then CurPos := l;
            DrawView;
          end;
        end else Exit;
      end;
    end;
  else
    Exit;
  end;
  ClearEvent(Event);
end;



{---TDateValidator---}

constructor TDateValidator.Init;
begin
  inherited Init;
  Options := voTransfer;
end;

procedure TDateValidator.Error;
begin
  {$IFDEF Russian}
    MessageBox(#$D#3'Неверная дата', nil, mfError + mfOKButton);
  {$ELSE}
    MessageBox(#$D#3'Illegal date', nil, mfError + mfOKButton);
  {$ENDIF}
end;

function TDateValidator.IsValid(const S: string): Boolean;
begin
  IsValid:= inherited IsValid(S) and
    (DateStringToDate(DatePict, S) <> BadDate);
end;

function TDateValidator.Transfer(var S: String; Buffer: Pointer;
  Flag: TVTransfer): Word;
begin
  Transfer := SizeOf(Date);
  case Flag of
   vtGetData: Date(Buffer^):= DateStringToDate(DatePict, S);
   vtSetData: S:= DateToDateString(DatePict, Date(Buffer^));
  end;
end;



{---TDateInput---}

constructor TDateInput.Init(var Bounds: TRect; InsOvrMode: TInsOvrMode; ComputeBounds: boolean);
begin
  inherited Init(Bounds, DatePattern, InsOvrMode, ComputeBounds);
  SlashChar := '.';
  SetValidator(New(PDateValidator, Init));
end;


{---TNumericInput---}

constructor TNumericInput.Init(var Bounds: TRect; const AMask: string; ComputeBounds: Boolean);
var l: word;
begin
  l := Length(AMask);
  inherited Init(Bounds, AlwaysInsert, l+1, ComputeBounds);
  DefaultCurPos := l+1;
  Mask := NewStr(AMask);
  Decimal := l - Pos('.', AMask);
  if Decimal=l then Decimal:=0;
  Val := 0.0;
end;

procedure TNumericInput.Str2Val;
var
  i: word;
begin
  if Data^<>'' then Str2Real(Data^, Val) else Val := 0.0;
  for i:=1 to Decimal do Val := Val / 10.0;
end;

procedure TNumericInput.Val2Str;
begin
  Data^ := Trim(Real2Str(Val, Length(Mask^), Decimal));
  if Decimal<>0 then Delete(Data^, Pos('.', Data^), 1);
end;

procedure TNumericInput.SetData(var Rec);
var
  S: string absolute Val;
begin
  if (Validator = nil) or (Validator^.Transfer(S, @Rec, vtSetData) = 0) then
    Val := Float(Rec);
  Val2Str;
  if FlagIsSet(State, sfFocused) then TextSelected := True;
end;

procedure TNumericInput.GetData(var Rec);
var
  S: string absolute Val;
begin
  Str2Val;
  if (Validator = nil) or (Validator^.Transfer(S, @Rec, vtGetData) = 0) then
    Float(Rec) := Val;
end;

function TNumericInput.DataSize: word;
var
  DSize: Word;
begin
  DSize := 0;

  if Validator <> nil then
    DSize := Validator^.Transfer(Data^, nil, vtDataSize);

  if DSize <> 0 then
    DataSize := DSize
  else
    DataSize := SizeOf(Float);
end;

destructor TNumericInput.Done;
begin
  DisposeStr(Mask);
  inherited Done;
end;

procedure TNumericInput.Draw;
var
  sss        : string;
  Color      : word;
begin
  if TextSelected then
    Color := GetColor($0506)
  else
    if State and sfFocused = sfFocused then
      Color := GetColor($0304)
    else
      Color := GetColor($0102);

  Str2Val;
  sss := Form(Mask^, Val);
  NoUpdate := Length(sss) = Length(Trim(sss));

  Bar{Pattern}(0, 0, Size.X, Size.Y, Lo(Color));
  WrStr(0, 0, Copy(sss, 1, Size.X div CharWidth), Hi(Color));
  SetCursor(Pred(CurPos) * CharWidth, 0);
end;

procedure TNumericInput.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  case Event.What of
    evKeydown:
      case Event.KeyCode of
        kbLeft, kbRight, kbHome, kbEnd, kbDel, kbIns:  ;
        kbBack: begin
          if TextSelected then begin
            TextSelected := False;
            Val := 0.0;
            Val2Str;
            NoUpdate := False;
            DrawView;
          end else begin
            Str2Val;
            if Val<>0.0 then begin
              Delete(Data^, Length(Data^), 1);
              DrawView;
            end;
          end;
        end;
      else if Event.ScanCode <> 0 then begin
        if Event.CharCode in ['0'..'9'] then begin
          if TextSelected then begin
            NoUpdate := False;
            TextSelected := False;
            Val := 0.0;
            Val2Str;
          end;
          if not NoUpdate then begin
             if not ((Val=0) and (Event.CharCode='0')) then Inc(Data^[0]);
             Data^[Byte(Data^[0])] := Event.CharCode;
             DrawView;
           end;
        end else if (Event.CharCode = '-') and (CanChangeSign) then begin
          if not NoUpdate then begin
            if TextSelected then TextSelected := False;
            Val := -Val;
            Val2Str;
            DrawView;
          end;
        end else Exit;
      end end;
  else
    Exit
  end;
  ClearEvent(Event);
end;


{---TMoneyValidator---}

constructor TMoneyValidator.Init;
begin
  inherited Init;
  Options := voTransfer;
end;

function TMoneyValidator.Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word;
var Ext: Float absolute S;
begin
{$IFOPT N+}
  Transfer := SizeOf(Comp);
  case Flag of
   vtGetData: Comp(Buffer^):= Ext * 100.0;
   vtSetData: Ext := Comp(Buffer^) / 100.0;
  end;
{$ELSE}
  Transfer := SizeOf(Word);
  case Flag of
   vtGetData: Word(Buffer^):= Round(Ext * 100);
   vtSetData: Ext := Word(Buffer^) / 100;
  end;
{$ENDIF}
end;


{---TMoneyInput---}

constructor TMoneyInput.Init(var Bounds: TRect; ComputeBounds: boolean);
begin
  inherited Init(Bounds, MoneyPattern, ComputeBounds);
  SetValidator(New(PMoneyValidator, Init));
end;

{ TEditBox }

function TViewListBox.ValidItem(Item: integer): boolean;
begin
  ValidItem := (Item >= 0) and Assigned(List) and (Item < List^.Count);
end;

function TViewListBox.ValidFocused: boolean;
begin
  ValidFocused := ValidItem(Focused);
end;

procedure TViewListBox.NewList(AList: PCollection);
begin
  if List <> nil then Dispose(List, Done);
  List := AList;
  if AList <> nil then SetRange(AList^.Count + 1) else SetRange(1);
  FocusItem(0);
  DrawView;
end;

procedure TViewListBox.HandleEvent(var Event: TEvent);
begin
  if (Event.What = evBroadcast) and
    (Event.Command = cmListItemSelected) and
    (State and sfSelected <> 0) then
  begin
    Event.What:= evKeyDown;
    Event.KeyCode:= kbEnter;
  end;
  inherited HandleEvent(Event);
end;

procedure TEditListBox.HandleEvent(var Event: TEvent);

  procedure InsertItem;
  var
    P: Pointer;
  begin
    P:= CreateItem;
    if not EditItem(P) then List^.FreeItem(P) else
    begin
      List^.Insert(P);
      SetRange(List^.Count + 1);
      FocusItem(List^.IndexOf(P));
      DrawView;
    end;
  end;

begin
  if (Event.What = evBroadcast) and
    (Event.Command = cmListItemSelected) and
    (State and sfSelected <> 0) then
  begin
    Event.What:= evKeyDown;
    Event.KeyCode:= kbSpace;
  end;

  if Event.What = evKeyDown then
  case Event.KeyCode of
      kbSpace:
        if ValidFocused then
          if EditItem(List^.At(Focused)) then DrawView else
          else InsertItem;
      kbIns:
        InsertItem;
      kbDel:
        if ValidFocused then DeleteItem(Focused);
      else {case}
        TListBox.HandleEvent(Event);
        Exit;
      end {case}

  else {if}
    begin
      TListBox.HandleEvent(Event);
      Exit;
    end;

  ClearEvent(Event);
end;

function TEditListBox.CreateItem: Pointer;
begin
  RunError(211);
end;

function TEditListBox.EditItem(P: Pointer): Boolean;
begin
  RunError(211);
end;

procedure TEditListBox.DeleteItem(Item: Integer);
begin
  if MessageBox(^M^F#3'Удалить строку ?', nil, mfWarning + mfYesButton + mfNoButton) = cmYes then
    begin
      List^.AtFree(Item);
      Owner^.Lock;
      SetRange(List^.Count + 1);
      DrawView;
      Owner^.Unlock;
    end;
end;

{───────────────────────────────────────────────────────────────────────────}

function TWordValidator.Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word;
var
  Value: Word;
  Code: Integer;
begin
  if Options and voTransfer <> 0 then begin
    Transfer := SizeOf(Value);
    case Flag of
     vtGetData: begin
                  Val(Trim(S), Value, Code);
                  Word(Buffer^) := Value;
                end;
     vtSetData: Str(Word(Buffer^), S);
    end;
  end else
    Transfer := 0;
end;

function TWordValidator.IsValid(const S: string): Boolean;
var
  Value: LongInt;
  Code: Integer;
begin
  IsValid := False;
  if inherited IsValid(Trim(S)) then
  begin
    Val(Trim(S), Value, Code);
    if (Code = 0) and (Value >= Min) and (Value <= Max) then
      IsValid := True;
  end;
end;

function TByteValidator.Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word;
var
  Value: Byte;
  Code: Integer;
begin
  if Options and voTransfer <> 0 then begin
    Transfer := SizeOf(Value);
    case Flag of
     vtGetData: begin
                  Val(Trim(S), Value, Code);
                  Byte(Buffer^) := Value;
                end;
     vtSetData: Str(Byte(Buffer^), S);
    end;
  end else
    Transfer := 0;
end;

function TLongValidator.Transfer(var S: String; Buffer: Pointer; Flag: TVTransfer): Word;
var
  Value: longint;
  Code: Integer;
begin
  if Options and voTransfer <> 0 then begin
    Transfer := SizeOf(Value);
    case Flag of
     vtGetData: begin
                  Val(Trim(S), Value, Code);
                  longint(Buffer^) := Value;
                end;
     vtSetData:
       Str(longint(Buffer^), S);
    end;
  end else
    Transfer := 0;
end;

procedure RegisterFields;
begin
  RegisterType(RPatternEdit);
  RegisterType(RDateInput);
end;

end.
