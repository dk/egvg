{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : MZResource 1.00                                      █
  █               Based upon RscMZ 1.00                                █
  █ Description : Supports new-executables resources reading           █
  █                                                                    █
  █ Author      : Tony Berezin                                         █
  █════════════════════════════════════════════════════════════════════█
  █                                                                    █
  █                                                                    █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}
{$F+,S+,I-,R-,V-}
unit MZResource;

interface uses
  Objects,Drivers,  { I use TObject, TBufStream, TCollection }
  Strings;{,  { I work with ASCIIZ strings }
  {Drivers;}
  (*EGString; { I work advanced with strings }*)

Type
  MakeIntResource = PChar;   { Use it as a function !!! }

  PRes = ^TRes;
  TRes = object(TObject)
    constructor Init;
    destructor  Done;    virtual;
    function    GetName: string;
    function    GetSize: LongInt;
   public
    Size: LongInt;
    Site: Longint;
    Num: Integer;
    Name: PString;
    FlatName: PString;
  end;

  PType = ^TType;
  TType = object(TCollection)
    constructor Init;
    destructor  Done;    virtual;
    function    GetName: string;
    function    FindResource(RName: PChar): PRes;
   public
    Num: Integer;
    Name: PString;
  end;

  PNamesCollection = ^TNamesCollection;
  TNamesCollection = object(TStringCollection)
    constructor Init(ALimit, ADelta: integer);
  end;

  PTypeCollection = ^TTypeCollection;
  TTypeCollection = object(TCollection)
  end;

  PMZResource = ^TMZResource;
  TMZResource = object(TObject)
    constructor Init;
    destructor  Done;    virtual;
    procedure   Clean;
    function    Read(const AFName: string): boolean;
    function    FindType(Name: PChar): PType;
    function    FindResource(TypeName, ResName: PChar): PRes;
    function    GetType(Res: PRes): PType;
    function    IsMine(Res: PRes): boolean;
    function    ReadResource(Res: PRes; var StoreTo): boolean;
    function    ReadResourcePortion(Res: PRes; var StoreTo; Size: word): boolean;
    function    GetErrorString: string;
    function    GetFName: string;
    function    CreateAliasStream(Res: PRes) : PStream;
   public
    HiddenNameTable: boolean;     { Default: TRUE }
    Status: Integer;
    Types: PTypeCollection;
    Names: PNamesCollection;      { Имена за концом таблицы ресурсов }
    FlatNames: PNamesCollection;  { Имена из RT_NAMETABLE }
   private
    FName: String;
  end;

{--- Коды типов ресурсов (поле Num типа TType) ---}
{ Поклонникам MS Windows или OS/2 - ВНИМАНИЕ,
  коды, приведенные здесь, не однозначно соответсвуют
  кодам RT_XXXX из windows.h (wintypes.pas) !!! }
Const
  mzNamed           = $0000; { Refer this resource type by TType.Name }
  mzCursor          = $0001;
  mzBitMap          = $0002;
  mzIcon            = $0003;
  mzMenu            = $0004;
  mzDialog          = $0005;
  mzStringTable     = $0006;
  mzFontLibrary     = $0007;
  mzFont            = $0008;
  mzAccelerator     = $0009;
  mzRCData          = $000A;
  mzGroupIcon       = $000C;
  mzGroupCursor     = $000E;
  mzNameTable       = $000F;

{--- Коды ошибок ---}
Const  { Another values means corresponding stream error (stXXXX) constants }
  mzOK                   = 0;  { Все в порядке }
  mzIllegalFileFormat    = 1;  { 'Явное не то' }
  mzNoMemory             = 2;  { Allocation error }
  mzFormatNotSupported   = 3;  { 'Такое может быть, но я этого не понимаю' }
  mzInternal01           = 1001; { Ни в какие ворота не лезет }

Type
  ResTypeNum2StringProc = function (ResTypeNum: integer): string;

function DefaultRTN2S(ResTypeNum: integer): string;

Const
  ResTypeNum2String : ResTypeNum2StringProc = DefaultRTN2S;

function FileExists(FileName: String): Boolean;
function AddBackSlash(const DirName : String): String;
function FullPathName(const FName : String): String;
function FindFileInPATH(const FName, Dirs: String): String;

implementation

{ No object }

function UpCaseStr(const S: string): string;
var
  S1: string;
  i: byte absolute S1;
begin
  for i:=1 to byte(S[0]) do S1[i] := UpCase(S[i]);
  UpCaseStr := S1;
end;

{---TRes---}

constructor TRes.Init; begin inherited Init end;

destructor TRes.Done;
begin
  inherited Done;
end;

function TRes.GetName: string;
var
  S: string;
begin
  if FlatName <> Nil then
    GetName := FlatName^
  else if Name <> Nil then
    GetName := Name^
  else begin
    Str(Num, S);
    GetName := S;
  end;
end;

function TRes.GetSize: longint;
begin
  GetSize := Size;
end;


{---TType---}

constructor TType.Init; begin inherited Init(10, 5) end;

destructor TType.Done;
begin
  inherited Done;
end;

function TType.GetName: string;
begin
  if Name <> Nil then
    GetName := Name^
  else
    GetName := ResTypeNum2String(Num);
end;

function TType.FindResource(RName: PChar): PRes;
var
  WordName: record
    LowWord: Word;
    HiWord: Word;
  end absolute RName;
  StrName: String;

  function FindOnNum(Item: PRes): boolean; Far;
  begin
    FindOnNum := (Item^.Num = WordName.LowWord);
  end;

  function FindOnName(Item: PRes): boolean; Far;
  var
    R: boolean;
  begin
    R := False;
    if (Item^.FlatName <> Nil) then R := (Item^.FlatName^ = StrName);
    if (Item^.Name <> Nil) then R := (R) or (Item^.Name^ = StrName);
    FindOnName := R;
  end;

begin
  if WordName.HiWord = 0 then
    FindResource := FirstThat(@FindOnNum)
  else begin
    StrName := UpCaseStr(StrPas(RName));
    FindResource := FirstThat(@FindOnName);
  end;
end;

function DefaultRTN2S(ResTypeNum: integer): string;
var
  S: string;
begin
  Str(ResTypeNum, S);
  DefaultRTN2S := S;
end;

{---TNamesCollection---}

constructor TNamesCollection.Init(ALimit, ADelta: integer);
begin
  inherited Init(ALimit, ADelta);
  Duplicates := True;
end;

{---TypeCollection---} { Implementation is empty }


{---TMZResource---}

constructor TMZResource.Init;
begin
  inherited Init;
  HiddenNameTable := True;
end;

destructor TMZResource.Done;
begin
  Clean;
  inherited Done;
end;

procedure TMZResource.Clean;
begin
  if Types<>Nil then Dispose(Types, Done);
  if Names<>Nil then Dispose(Names, Done);
  if FlatNames<>Nil then Dispose(FlatNames, Done);
  Types := Nil;
  Names := Nil;
  FlatNames := Nil;
  Status := mzOK;
end;

{--- MZ-file structures description ---}
{ Приведены только интересующие нас в данном контексте поля }
Type
  MZHeader = record
    Signature : Word;  { OFFS = 0     MZ or ZM = 5A4D or 4D5A }
    DontInterest: array [2..$3C-1] of byte; { Может, по $18 интересно }
    NewExeOffset: Longint;  { 0 для обычных файлов }
  end;

procedure UpperInString(S: PString);
var
  i: Integer;
begin
  if S <> Nil then for i:=1 to Length(S^) do S^[i] := Upcase(S^[i]);
end;

function TMZResource.Read(const AFName: string): boolean; {VERY BIG FUNCTION}
var
  Inp: PBufStream;
  HdrOffs: LongInt;
  Scale: Word;
  ExeType: (etUnknown, etNew, etLinear);
  ResTableInfo : record
    Offset: LongInt;
    Size: LongInt;
  end;
  DWord : record
    LWord: Word;
    Hword: Word;
  end;
  LastType: PType;
  FlatNamesTable: Pointer;
  FlatNamesTableLength: Word;

  procedure Clean1;
  begin
    Clean;
    if (FlatNamesTableLength <> 0) and (FlatNamesTable <> Nil) then FreeMem(FlatNamesTable, FlatNamesTableLength);
    FlatNamesTableLength := 0;
    FlatNamesTable := Nil;
    if Inp<>Nil then Dispose(Inp, Done);
    Inp := Nil;
  end;

  function InputFails: boolean;
  begin
    if (Inp = Nil) or (Inp^.Status <> stOK) then begin
      Clean1;
      if Inp = Nil then
        Status := stInitError
      else
        Status := Inp^.Status;
      InputFails := True;
    end else InputFails := False;
  end;

  function HeaderFails: boolean;
  var
    Stru: MZHeader;
  begin
    HeaderFails := True;
    Inp^.Read(Stru, SizeOf(Stru));
    if InputFails then Exit;
    if ((Stru.Signature <> $5A4D) and (Stru.Signature <> $4D5A)) or (Stru.NewExeOffset = 0) then begin
      Clean1; Status := mzIllegalFileFormat; Exit;
    end;
    Inp^.Seek(Stru.NewExeOffset);  { На начало заголовка }
    if InputFails then Exit;
    HdrOffs := Stru.NewExeOffset;
    HeaderFails := False;
  end;

  function NewHeaderFails: boolean;
  const
    NE = $454E;  { New-EXE signature (MS Windows, OS/2 16-bit apps) }
    LE = $454C;  { Linear executeable (MS Windows 32-bit apps) }
    LX = $584C;  { Linear executeable (OS/2 32-bit apps) }
  var
    Stru: MZHeader;
    Signature: Word;

  begin
    NewHeaderFails := True;
    Inp^.Read(Signature, SizeOf(Signature));
    if InputFails then Exit;
    if (Signature = NE) then begin
      Inp^.Seek(HdrOffs + $24);
      if InputFails then Exit;
      Inp^.Read(DWord, SizeOf(DWord));
      if InputFails then Exit;
      ResTableInfo.Offset := HdrOffs + DWord.LWord;
      { Нелегальное определение размера - из начала следующей таблицы
      вычитаем начало нашей таблицы }
      ResTableInfo.Size := DWord.HWord - DWord.LWord;
      Inp^.Seek(ResTableInfo.Offset);
      if InputFails then Exit;
      ExeType := etNew;
    end else if (Signature = LE) or (Signature = LX) then begin
      Clean1; Status := mzFormatNotSupported; Exit;
(* { Пока не поддерживаем LINEAR EXECUTABLE file format, ГОРИ ОН ОГНЕМ }
      Inp^.Read(DWord.LWord, SizeOf(DWord.LWord));
      if InputFails then Exit;
      if DWord.LWord <> 0 then begin  { Big-endian - это НОНСЕНС }
        Status := mzFormatNotSupported; Clean1; Exit;
      end;
      Inp^.Seek(HdrOffs + $50);
      if InputFails then Exit;
      Inp^.Read(ResTableInfo, SizeOf(ResTableInfo));
      if InputFails then Exit;
      Inp^.Read(ResTableInfo.Size, SizeOf(ResTableInfo.Size));
      if InputFails then Exit;
      { Нелегальное определение размера - из начала следующей таблицы
      вычитаем начало нашей таблицы }
      ResTableInfo.Size := ResTableInfo.Size - ResTableInfo.Offset;
      Inp^.Seek(ResTableInfo.Offset);
      if InputFails then Exit;
*)
    end else begin
      Clean1; Status := mzIllegalFileFormat; Exit;
    end;
    with ResTableInfo do
      if (Offset <> 0) and (Size <> 0) then Inp^.Read(Scale, SizeOf(Scale));
    if InputFails then Exit;
    NewHeaderFails := False;
  end;

  function MakeOneRes: boolean;
  var
    ResRec: record
      Site: Word;  { Shift it Scale times ! }
      Size: Word;  { Shift it Scale times ! }
      Flags: Word;
      ID: Word;
      Reserved: Longint;
    end;
    LastRes: PRes;
  begin
    MakeOneRes := False;
    Inp^.Read(ResRec, SizeOf(ResRec));
    if InputFails then Exit;
    New(LastRes, Init);
    if LastRes = Nil then begin
      Clean1; Status := mzNoMemory; Exit;
    end;
    if ResRec.ID > $8000 then begin
      { integer resource ID }
      LastRes^.Num := ResRec.ID - $8000;
    end else begin
      { string resource ID - it's a TRICK !!! }
      word(LastRes^.Name) := ResRec.ID;  { OFFSET to NAME }
    end;
    LastRes^.Site := LongInt(ResRec.Site) SHL Scale;
    LastRes^.Size := LongInt(ResRec.Size) SHL Scale;
    LastType^.Insert(LastRes);
    MakeOneRes := True;
  end;

  function MakeOneType: boolean;
  var
    TypeRec : record
      ID: Word;
      Num: Word;
      DontInterest: LongInt;
    end;
    i: word;
  begin
    MakeOneType := False;
    Inp^.Read(TypeRec, SizeOf(TypeRec));
    if InputFails then Exit;
    if TypeRec.ID = 0 then begin
      { Позиционируем на конец последнего ресурса, т. е. на начало таблицы
        имен }
      Inp^.Seek(Inp^.GetPos - SizeOf(TypeRec) + SizeOf(TypeRec.ID));
      if InputFails then Exit;
      LastType := Nil;
      MakeOneType := True;
      Exit;
    end;
    New(LastType, Init);
    if LastType = Nil then begin
      Clean1; Status := mzNoMemory; Exit;
    end;
    if TypeRec.ID > $8000 then begin
      { integer resource ID }
      LastType^.Num := TypeRec.ID - $8000;
    end else begin
      { string resource ID - it's a TRICK !!! }
      word(LastType^.Name) := TypeRec.ID;  { OFFSET to NAME }
    end;
    for i:=1 to TypeRec.Num do if not MakeOneRes then Exit;
    Types^.Insert(LastType);
    MakeOneType := True;
  end;

  function FillTypeNames(Item: PType): Boolean; far;
  var
    NameOffs: Word;
    CurType: PType absolute Item;

    function FillResNames(Item: PRes): Boolean; far;
    type
      PNameTable = ^NameTable;
      NameTable = record
          Len : word;
          Num : word;
          Out : word;
          Rsrv : byte;
          Name : array [0..255] of char;
      end;
    var
      NameOffs: Word;
      p1: PChar;
      p: PNameTable absolute p1;
    begin
      FillResNames := True;
      NameOffs := Word(Item^.Name);
      if NameOffs <> 0 then begin
        Inp^.Seek(ResTableInfo.Offset + NameOffs);
        if InputFails then Exit;
        Item^.Name := Inp^.ReadStr;
        if InputFails then Exit;
        UpperInString(Item^.Name);
        Names^.Insert(Item^.Name);
      end;
      if FlatNamesTable <> Nil then begin
        p1 := FlatNamesTable;
        while p^.Len<>0 do begin
          if ((p^.Num = CurType^.Num) or
            ((p^.Num = mzGroupIcon) and (CurType^.Num = mzCursor)) or
            ((p^.Num = mzGroupCursor) and (CurType^.Num = mzIcon))
            )
          and (p^.Out = (Item^.Num or $8000))
          then begin
            Item^.FlatName := NewStr(StrPas(p^.Name));
            UpperInString(Item^.FlatName);
            FlatNames^.Insert(Item^.FlatName);
            Break;
          end;
          Inc(p1, p^.Len);
        end;
      end;
      FillResNames := False;
    end;

  begin
    FillTypeNames := True;
    NameOffs := Word(Item^.Name);
    if NameOffs <> 0 then begin
      Inp^.Seek(ResTableInfo.Offset + NameOffs);
      if InputFails then Exit;
      Item^.Name := Inp^.ReadStr;
      if InputFails then Exit;
      UpperInString(Item^.Name);
      Names^.Insert(Item^.Name);
    end;
    if Item^.FirstThat(@FillResNames) <> Nil then
      Exit; { An error occured during filling resource names }
    FillTypeNames := False;
  end;

  function ReadFlatNamesFails: boolean;
  var
    NameTable: PRes;
    NameType: PType;
  label
    Success;
  begin
    { В этой функции возможны только файловые ошибки - если не хватает
     памяти и т.п., то мы считаем, что этих имен просто нет }
    ReadFlatNamesFails := True;
    NameType := FindType(MakeIntResource(mzNameTable));
    if NameType = Nil then goto Success;
    if NameType^.Count <> 1 then goto Success;
    NameTable := NameType^.At(0);
    if (NameTable^.Size > $FFFF) or (NameTable^.Size > MaxAvail) then goto Success;
    FlatNamesTableLength := NameTable^.Size;
    GetMem(FlatNamesTable, FlatNamesTableLength);
    Inp^.Seek(NameTable^.Site);
    if InputFails then Exit;
    Inp^.Read(FlatNamesTable^, FlatNamesTableLength);
  Success:
    if (NameType <> Nil) and (HiddenNameTable) then begin
      Types^.Free(NameType);
    end;
    ReadFlatNamesFails := False;
  end;

begin
  Read := False;
  Inp := Nil;
  FlatNamesTableLength := 0;
  FlatNamesTable := Nil;
  Clean;
  FName := AFName;
  New(Inp, Init(AFName, stOpenRead, 2048));
  (*
  if Inp = Nil then begin
    Clean1; Status := mzNoMemory; Exit;
  end;
  *)
  if InputFails then Exit;
  {Pos=Header=0}
  if HeaderFails then Exit;
  {Pos=NewHeader}
  if NewHeaderFails then Exit;
  {Pos=ResourceTable}
  New(Types, Init(3, 5));
  if Types = Nil then begin
    Clean1; Status := mzNoMemory; Exit;
  end;
  New(Names, Init(10, 5));
  if Names = Nil then begin
    Clean1; Status := mzNoMemory; Exit;
  end;
  New(FlatNames, Init(10, 5));
  if FlatNames = Nil then begin
    Clean1; Status := mzNoMemory; Exit;
  end;
  if (ResTableInfo.Offset = 0) or (ResTableInfo.Size = 0) then begin  { Нету никаких ресурсов }
    if Inp<>Nil then Dispose(Inp, Done);
    Read := True; Exit;
  end;
  case ExeType of
    etNew: begin
      {--- Стоим на начале таблицы ресурсов ---}
      LastType := Nil;
      repeat
        if not MakeOneType then Exit;
      until LastType = Nil;
      {--- Заполним FlatNames-коллекцию ---}
      if ReadFlatNamesFails then Exit;
      {--- А теперь установим все имена ---}
      if Types^.FirstThat(@FillTypeNames)<>Nil then Exit; { Была ошибка }
      if (FlatNamesTableLength <> 0) and (FlatNamesTable <> Nil) then FreeMem(FlatNamesTable, FlatNamesTableLength);
      FlatNamesTableLength := 0;
      FlatNamesTable := Nil;
    end;
  else
    Clean1; Status := mzInternal01; Exit;
  end;
  if Inp<>Nil then Dispose(Inp, Done);
  Read := True;
end;

function TMZResource.GetErrorString: string;
var
  S: string;
begin
  case Status of
    mzOK: S := 'Нет ошибки';
    mzIllegalFileFormat: S := 'Неверный формат файла '+FName;
    mzNoMemory: S := 'Недостаточно памяти';
    mzFormatNotSupported: S := 'Не поддерживаемый формат у файла '+FName;
    mzInternal01: S := 'Внутренняя ошибка загрузки ресурсов';
    stError: S := 'Ошибка доступа';
    stInitError: S := 'Ошибка открытия файла '+FName;
    stReadError: S := 'Ошибка чтения файла '+FName;
  else
    S := 'Неизвестная ошибка';
  end;
  GetErrorString := S;
end;

function TMZResource.FindType(Name: PChar): PType;
var
  WordName: record
    LowWord: Word;
    HiWord: Word;
  end absolute Name;
  StrName: String;

  function FindOnNum(Item: PType): boolean; Far;
  begin
    FindOnNum := (Item^.Num = WordName.LowWord);
  end;

  function FindOnName(Item: PType): boolean; Far;
  begin
    if Item^.Name = Nil then FindOnName := False else
      FindOnName := (Item^.Name^ = StrName);
  end;

begin
  if Types=Nil then begin
    FindType := Nil;
  end;
  if WordName.HiWord = 0 then
    FindType := Types^.FirstThat(@FindOnNum)
  else begin
    StrName := UpCaseStr(StrPas(Name));
    FindType := Types^.FirstThat(@FindOnName);
  end;
end;

function TMZResource.FindResource(TypeName, ResName: PChar): PRes;
var
  T: PType;
begin
  T := FindType(TypeName);
  if T = Nil then FindResource := Nil else
    FindResource := T^.FindResource(ResName);
end;

function TMZResource.GetType(Res: PRes): PType;

  function FindRes(T: PType): boolean; far;
  begin
    FindRes := (T^.IndexOf(Res) <> -1);
  end;

begin
  if (Types=Nil) or (Res=Nil) then begin
    GetType := Nil;
  end;
  GetType := Types^.FirstThat(@FindRes);
end;

function TMZResource.IsMine(Res: PRes): boolean;
begin
  IsMine := (GetType(Res) <> Nil);
end;

function TMZResource.CreateAliasStream(Res: PRes) : PStream;
var
  Inp: PDosStream;
begin
  CreateAliasStream := Nil;
  if (Types = Nil) or (not IsMine(Res)) then Exit;
  New(Inp, Init(FName, stOpenRead));
  if Inp = Nil then begin
    Status := stInitError;
    Exit;
  end;
  Inp^.Seek(Res^.Site);
  if Inp^.Status <> stOK then begin
    Status := Inp^.Status;
    Dispose(Inp, Done);
    Exit;
  end;
  CreateAliasStream := Inp;
end;


function TMZResource.ReadResource(Res: PRes; var StoreTo): boolean;
var
  Inp: PStream;
label
  ByeBye;
begin
  ReadResource := False;
  if Res^.Size > $FFFF then Exit;
  Inp := CreateAliasStream(Res);
  Inp^.Read(StoreTo, Res^.Size);
  if Inp^.Status <> stOK then begin
    Status := Inp^.Status;
    goto ByeBye;
  end;
  ReadResource := True;
ByeBye:
  Dispose(Inp, Done);
end;

function TMZResource.ReadResourcePortion(Res: PRes; var StoreTo; Size: word): boolean;
var
  Inp: PStream;
label
  ByeBye;
begin
  ReadResourcePortion := False;
  Inp := CreateAliasStream(Res);
  Inp^.Read(StoreTo, Size);
  if Inp^.Status <> stOK then begin
    Status := Inp^.Status;
    goto ByeBye;
  end;
  ReadResourcePortion := True;
ByeBye:
  Dispose(Inp, Done);
end;

function TMZResource.GetFName: string;
begin
  GetFName := FName;
end;

{ FileName handling }

function FileExists(FileName: String): Boolean; assembler; {OOA}
asm
        push    DS
        lds     BX, DWORD PTR [FileName]
        mov     dx, bx
        inc     DX
        add     bl, [bx]
        adc     bh, 0
        inc     bx
        mov     byte ptr [bx], 0
        mov     AX, 04300h {0B600h for Netware loaded}
        int     21h
        jc      @@Error
        mov     AL,True
        jmp     @@Done
@@Error:
        xor     AL,AL
@@Done:
        pop     DS
end;

function FindFileInPATH(const FName, Dirs: String): String; {OOA}
var
  I0, I1: Byte;

  function GetNextDir: String;
  begin
    I0:= I1;
    while (I1 <= Length(Dirs)) and (Dirs[I1] <> ';') do Inc(I1);
    GetNextDir:= Copy(Dirs, I0, I1 - I0);
    Inc(I1);
  end;

var
  S: String;
begin
  S:= FullPathName(FName);
  if FileExists(S) then
    FindFileInPATH:= S
  else if (Pos('\', FName) <> 0) or (Pos(':', FName) <> 0) then
    FindFileInPATH:= FName
  else
  begin
    FindFileInPATH:= FName;
    I1:= 1;
    S:= GetNextDir;
    while S <> '' do
    begin
      S:= FullPathName(AddBackSlash(S) + FName);
      if FileExists(S) then
      begin
        FindFileInPATH:= S;
        Exit;
      end;
      S:= GetNextDir;
    end;
  end;
end;

{ FROM OPString. OOA }

  function AddBackSlash(const DirName : string) : string;
    {-Add a default backslash to a directory name}
  begin
    if DirName[Length(DirName)] in ['\', ':', #0] then
      AddBackSlash := DirName
    else
      AddBackSlash := DirName+'\';
  end;

  function FullPathName(const FName : string) : string;
    {-Given FName (known to exist), return a full pathname}
  var
    CurDir : string[64];
    Cpos : Byte;
  begin
    Cpos := Pos(':', FName);
    if Cpos <> 0 then begin
      {Drive letter specified}
      if FName[Succ(Cpos)] = '\' then
        {Complete path already specified}
        FullPathName := FName
      else begin
        {Drive specified, but incomplete path}
        GetDir(Pos(Upcase(FName[1]), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'), CurDir);
        FullPathName := AddBackSlash(CurDir)+Copy(FName, Succ(Cpos), 100);
      end;
    end else begin
      {No drive specified}
      GetDir(0, CurDir);
      if FName[1] = '\' then
        {Complete path but no drive}
        FullPathName := Copy(CurDir, 1, 2)+FName
      else
        {No drive, incomplete path}
        FullPathName := AddBackSlash(CurDir)+FName;
    end;
  end;

end.