
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit StdDlg;

{$O+,F+,V-,X+,I-,S-}

interface

uses Objects, Drivers, Views, Dialogs, Dos, EGFont, GDI, Bitmaps;

const

{ Commands }

  cmFileOpen    = 800;   { Returned from TFileDialog when Open pressed }
  cmFileReplace = 801;   { Returned from TFileDialog when Replace pressed }
  cmFileClear   = 802;   { Returned from TFileDialog when Clear pressed }
  cmFileInit    = 803;   { Used by TFileDialog internally }
  cmChDir       = 804;   { Used by TChDirDialog internally }
  cmRevert      = 805;   { Used by TChDirDialog internally }

{ Messages }

  cmFileFocused = 806;    { A new file was focused in the TFileList }
  cmFileDoubleClicked     { A file was selected in the TFileList }
                = 807;

type

  { TSearchRec }

  {  Record used to store directory information by TFileDialog }

  TSearchRec = record
    Attr: Byte;
    Time: Longint;
    Size: Longint;
    Name: string[12];
  end;

type

  { TFileInputLine is a special input line that is used by      }
  { TFileDialog that will update its contents in response to a  }
  { cmFileFocused command from a TFileList.                     }

  PFileInputLine = ^TFileInputLine;
  TFileInputLine = object(TInputLine)
    constructor Init(var Bounds: TRect; AMaxLen: Integer);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { TFileCollection is a collection of TSearchRec's.            }

  PFileCollection = ^TFileCollection;
  TFileCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

  { TSortedListBox is a TListBox that assumes it has a          }
  { TStoredCollection instead of just a TCollection.  It will   }
  { perform an incremental search on the contents.              }

  PSortedListBox = ^TSortedListBox;
  TSortedListBox = object(TListBox)
    SearchPos: Word;
    ShiftState: Byte;
    constructor Init(var Bounds: TRect; ANumCols: Word;
      AScrollBar: PScrollBar);
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure NewList(AList: PCollection); virtual;
  end;

  { TFileList is a TSortedList box that assumes it contains     }
  { a TFileCollection as its collection.  It also communicates  }
  { through broadcast messages to TFileInput and TInfoPane      }
  { what file is currently selected.                            }

  PFileList = ^TFileList;
  TFileList = object(TSortedListBox)
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure FocusItem(Item: Integer); virtual;
    procedure GetData(var Rec); virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    function GetKey(var S: String): Pointer; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ReadDirectory(AWildCard: PathStr);
    procedure SetData(var Rec); virtual;
  end;

  { TFileInfoPane is a TView that displays the information      }
  { about the currently selected file in the TFileList          }
  { of a TFileDialog.                                           }

  PFileInfoPane = ^TFileInfoPane;
  TFileInfoPane = object(TView)
    S: TSearchRec;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

  { TFileDialog is a standard file name input dialog            }

  TWildStr = PathStr;

const
  fdOkButton      = $0001;      { Put an OK button in the dialog }
  fdOpenButton    = $0002;      { Put an Open button in the dialog }
  fdReplaceButton = $0004;      { Put a Replace button in the dialog }
  fdClearButton   = $0008;      { Put a Clear button in the dialog }
  fdHelpButton    = $0010;      { Put a Help button in the dialog }
  fdNoLoadDir     = $0100;      { Do not load the current directory }
                                { contents into the dialog at Init. }
                                { This means you intend to change the }
                                { WildCard by using SetData or store }
                                { the dialog on a stream. }

type

  PFileDialog = ^TFileDialog;
  TFileDialog = object(TDialog)
    FileName: PFileInputLine;
    FileList: PFileList;
    WildCard: TWildStr;
    Directory: PString;
    constructor Init(AWildCard: TWildStr; const ATitle,
      InputName: String; AOptions: Word; HistoryId: Byte);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure GetData(var Rec); virtual;
    procedure GetFileName(var S: PathStr);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  private
    procedure ReadDirectory;
  end;

  { TDirEntry }

  PDirEntry = ^TDirEntry;
  TDirEntry = record
    DisplayText: PString;
    Directory: PString;
  end;

  { TDirCollection is a collection of TDirEntry's used by       }
  { TDirListBox.                                                }

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TCollection)
    function GetItem(var S: TStream): Pointer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

  { TDirListBox displays a tree of directories for use in the }
  { TChDirDialog.                                               }

  PDirListBox = ^TDirListBox;
  TDirListBox = object(TListBox)
    Dir: DirStr;
    Cur: Word;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar);
    destructor Done; virtual;
    procedure DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt); virtual; {DK}
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function IsSelected(Item: Integer): Boolean; virtual;
    procedure NewDirectory(var ADir: DirStr);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
  end;

  { TChDirDialog is a standard change directory dialog.         }

const
  cdNormal     = $0000; { Option to use dialog immediately }
  cdNoLoadDir  = $0001; { Option to init the dialog to store on a stream }
  cdHelpButton = $0002; { Put a help button in the dialog }

type

  PChDirDialog = ^TChDirDialog;
  TChDirDialog = object(TDialog)
    DirInput: PInputLine;
    DirList: PDirListBox;
    OkButton: PButton;
    ChDirButton: PButton;
    constructor Init(AOptions: Word; HistoryId: Word);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  private
    procedure SetUpDialog;
  end;

const

{                тХФтХРтХРтХРтХдтХРтХРтХРтХЧ }
{ CInfoPane      тХС 1 тФВ 2 тХС }
{                тХЪтХРтХдтХРтХзтХРтХдтХРтХЭ }
{ Fore тФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФШ   тФВ   }
{ Back тФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФАтФШ   }

  CInfoPane = #67#68;

  { TStream registration records }

const
  RFileInputLine: TStreamRec = (
     ObjType: 60;
     VmtLink: Ofs(TypeOf(TFileInputLine)^);
     Load:    @TFileInputLine.Load;
     Store:   @TFileInputLine.Store
  );

const
  RFileCollection: TStreamRec = (
     ObjType: 61;
     VmtLink: Ofs(TypeOf(TFileCollection)^);
     Load:    @TFileCollection.Load;
     Store:   @TFileCollection.Store
  );

const
  RFileList: TStreamRec = (
     ObjType: 62;
     VmtLink: Ofs(TypeOf(TFileList)^);
     Load:    @TFileList.Load;
     Store:   @TFileList.Store
  );

const
  RFileInfoPane: TStreamRec = (
     ObjType: 63;
     VmtLink: Ofs(TypeOf(TFileInfoPane)^);
     Load:    @TFileInfoPane.Load;
     Store:   @TFileInfoPane.Store
  );

const
  RFileDialog: TStreamRec = (
     ObjType: 64;
     VmtLink: Ofs(TypeOf(TFileDialog)^);
     Load:    @TFileDialog.Load;
     Store:   @TFileDialog.Store
  );

const
  RDirCollection: TStreamRec = (
     ObjType: 65;
     VmtLink: Ofs(TypeOf(TDirCollection)^);
     Load:    @TDirCollection.Load;
     Store:   @TDirCollection.Store
  );

const
  RDirListBox: TStreamRec = (
     ObjType: 66;
     VmtLink: Ofs(TypeOf(TDirListBox)^);
     Load:    @TDirListBox.Load;
     Store:   @TDirListBox.Store
  );

const
  RChDirDialog: TStreamRec = (
     ObjType: 67;
     VmtLink: Ofs(TypeOf(TChDirDialog)^);
     Load:    @TChDirDialog.Load;
     Store:   @TChDirDialog.Store
  );

const
  RSortedListBox: TStreamRec = (
     ObjType: 68;
     VmtLink: Ofs(TypeOf(TSortedListBox)^);
     Load:    @TSortedListBox.Load;
     Store:   @TSortedListBox.Store
  );

procedure RegisterStdDlg;

Procedure Dir_ClosedFolder;
Procedure Dir_OpenedFolder;
Procedure Dir_RootFolder;
Procedure Dir_HardDrive;
Procedure Dir_FloppyDrive;
Procedure Dir_CDROM;
Procedure Dir_Net;
Procedure Dir_Memory;


implementation

uses App, Memory, HistList, MsgBox;


function DriveValid(Drive: Char): Boolean; near; assembler;
asm
        MOV     AH,19H          { Save the current drive in BL }
        INT     21H
        MOV     BL,AL
        MOV     DL,Drive        { Select the given drive }
        SUB     DL,'A'
        MOV     AH,0EH
        INT     21H
        MOV     AH,19H          { Retrieve what DOS thinks is current }
        INT     21H
        MOV     CX,0            { Assume false }
        CMP     AL,DL           { Is the current drive the given drive? }
        JNE     @@1
        MOV     CX,1            { It is, so the drive is valid }
        MOV     DL,BL           { Restore the old drive }
        MOV     AH,0EH
        INT     21H
@@1:    XCHG    AX,CX           { Put the return value into AX }
end;

function PathValid(var Path: PathStr): Boolean;
var
  ExpPath: PathStr;
  SR: SearchRec;
begin
  ExpPath := FExpand(Path);
  if Length(ExpPath) <= 3 then PathValid := DriveValid(ExpPath[1])
  else
  begin
    if ExpPath[Length(ExpPath)] = '\' then Dec(ExpPath[0]);
    FindFirst(ExpPath, Directory, SR);
    PathValid := (DosError = 0) and (SR.Attr and Directory <> 0);
  end;
end;

function ValidFileName(var FileName: PathStr): Boolean;
const
  IllegalChars = ';,=+<>|"[] \';
var
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;

{ Contains returns true if S1 contains any characters in S2 }
function Contains(S1, S2: String): Boolean; near; assembler;
asm
        PUSH    DS
        CLD
        LDS     SI,S1
        LES     DI,S2
        MOV     DX,DI
        XOR     AH,AH
        LODSB
        MOV     BX,AX
        OR      BX,BX
        JZ      @@2
        MOV     AL,ES:[DI]
        XCHG    AX,CX
@@1:    PUSH    CX
        MOV     DI,DX
        LODSB
        REPNE   SCASB
        POP     CX
        JE      @@3
        DEC     BX
        JNZ     @@1
@@2:    XOR     AL,AL
        JMP     @@4
@@3:    MOV     AL,1
@@4:    POP     DS
end;

begin
  ValidFileName := True;
  FSplit(FileName, Dir, Name, Ext);
  if not ((Dir = '') or PathValid(Dir)) or Contains(Name, IllegalChars) or
    Contains(Dir, IllegalChars) then ValidFileName := False;
end;

function GetCurDir: DirStr;
var
  CurDir: DirStr;
begin
  GetDir(0, CurDir);
  if Length(CurDir) > 3 then
  begin
    Inc(CurDir[0]);
    CurDir[Length(CurDir)] := '\';
  end;
  GetCurDir := CurDir;
end;

type
  PSearchRec = ^TSearchRec;

function IsWild(const S: String): Boolean;
begin
  IsWild := (Pos('?',S) > 0) or (Pos('*',S) > 0);
end;

function IsDir(const S: String): Boolean;
var
  SR: SearchRec;
begin
  FindFirst(S, Directory, SR);
  if DosError = 0 then
    IsDir := SR.Attr and Directory <> 0
  else IsDir := False;
end;

{ TFileInputLine }

constructor TFileInputLine.Init(var Bounds: TRect; AMaxLen: Integer);
begin
  TInputLine.Init(Bounds, AMaxLen);
  EventMask := EventMask or evBroadcast;
end;

procedure TFileInputLine.HandleEvent(var Event: TEvent);
var
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
begin
  TInputLine.HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmFileFocused) and
    (State and sfSelected = 0) then
  begin
     if PSearchRec(Event.InfoPtr)^.Attr and Directory <> 0 then
        Data^ := PSearchRec(Event.InfoPtr)^.Name + '\'+
          PFileDialog(Owner)^.WildCard
     else Data^ := PSearchRec(Event.InfoPtr)^.Name;
     DrawView;
  end;
end;

{ TFileCollection }

function TFileCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  if PSearchRec(Key1)^.Name = PSearchRec(Key2)^.Name then Compare := 0
  else if PSearchRec(Key1)^.Name = '..' then Compare := 1
  else if PSearchRec(Key2)^.Name = '..' then Compare := -1
  else if (PSearchRec(Key1)^.Attr and Directory <> 0) and
     (PSearchRec(Key2)^.Attr and Directory = 0) then Compare := 1
  else if (PSearchRec(Key2)^.Attr and Directory <> 0) and
     (PSearchRec(Key1)^.Attr and Directory = 0) then Compare := -1
  else if PSearchRec(Key1)^.Name > PSearchRec(Key2)^.Name then
    Compare := 1
  else Compare := -1;
end;

procedure TFileCollection.FreeItem(Item: Pointer);
begin
  Dispose(PSearchRec(Item));
end;

function TFileCollection.GetItem(var S: TStream): Pointer;
var
  Item: PSearchRec;
begin
  New(Item);
  if Item = Nil then Exit;
  S.Read(Item^, SizeOf(TSearchRec));
  GetItem := Item;
end;

procedure TFileCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Write(Item^, SizeOf(TSearchRec));
end;

{ TSortedListBox }

constructor TSortedListBox.Init(var Bounds: TRect; ANumCols: Word;
  AScrollBar: PScrollBar);
begin
  TListBox.Init(Bounds, ANumCols, AScrollBar);
  SearchPos := 0;
  ShowCursor;
  SetCursor(CharWidth, 0);
end;

procedure TSortedListBox.HandleEvent(var Event: TEvent);
var
  CurString, NewString: String;
  K: Pointer;
  Value, OldPos, OldValue: Integer;
  T: Boolean;

function Equal(const S1, S2: String; Count: Word): Boolean;
var
  I: Word;
begin
  Equal := False;
  if (Length(S1) < Count) or (Length(S2) < Count) then Exit;
  for I := 1 to Count do
    if UpCase(S1[I]) <> UpCase(S2[I]) then Exit;
  Equal := True;
end;

begin
  OldValue := Focused;
  TListBox.HandleEvent(Event);
  if OldValue <> Focused then SearchPos := 0;
  if Event.What = evKeyDown then
  begin
    if Event.CharCode <> #0 then
    begin
      Value := Focused;
      if Value < Range then CurString := GetText(Value, 255)
      else CurString := '';
      OldPos := SearchPos;
      if Event.KeyCode = kbBack then
      begin
        if SearchPos = 0 then Exit;
        Dec(SearchPos);
        if SearchPos = 0 then ShiftState := GetShiftState;
        CurString[0] := Char(SearchPos);
      end
      else if (Event.CharCode = '.') then SearchPos := Pos('.',CurString)
      else
      begin
        Inc(SearchPos);
        if SearchPos = 1 then ShiftState := GetShiftState;
        CurString[0] := Char(SearchPos);
        CurString[SearchPos] := Event.CharCode;
      end;
      K := GetKey(CurString);
      T := PSortedCollection(List)^.Search(K, Value);
      if Value < Range then
      begin
        if Value < Range then NewString := GetText(Value, 255)
        else NewString := '';
        if Equal(NewString, CurString, SearchPos) then
        begin
          if Value <> OldValue then
          begin
            FocusItem(Value);
            { Assumes ListControl will set the cursor to the first character }
            { of the sfFocused item }
            SetCursor(Cursor.X+SearchPos*CharWidth, Cursor.Y);
          end
          else SetCursor(Cursor.X+(SearchPos-OldPos)*CharWidth, Cursor.Y);
        end
        else SearchPos := OldPos;
      end
      else SearchPos := OldPos;
      if (SearchPos <> OldPos) or (Event.CharCode in ['A'..'Z','a'..'z']) then
        ClearEvent(Event);
    end;
  end;
end;

function TSortedListBox.GetKey(var S: String): Pointer;
begin
  GetKey := @S;
end;

procedure TSortedListBox.NewList(AList: PCollection);
begin
  TListBox.NewList(AList);
  SearchPos := 0;
end;

{ TFileList }

constructor TFileList.Init(var Bounds: TRect; AScrollBar: PScrollBar);
begin
  TSortedListBox.Init(Bounds, 2, AScrollBar);
end;

destructor TFileList.Done;
begin
  if List <> nil then Dispose(List, Done);
  TListBox.Done;
end;

function TFileList.DataSize: Word;
begin
  DataSize := 0;
end;

procedure TFileList.FocusItem(Item: Integer);
begin
  Owner^.Lock;
  TSortedListBox.FocusItem(Item);
  Message(Owner, evBroadcast, cmFileFocused, List^.At(Item));
  Owner^.Unlock;
end;

procedure TFileList.GetData(var Rec);
begin
end;

function TFileList.GetKey(var S: String): Pointer;
const
  SR: TSearchRec = ();

procedure UpStr(var S: String);
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := UpCase(S[I]);
end;

begin
  if (ShiftState and $03 <> 0) or ((S <> '') and (S[1]='.')) then
    SR.Attr := Directory
  else SR.Attr := 0;
  SR.Name := S;
  UpStr(SR.Name);
  GetKey := @SR;
end;

function TFileList.GetText(Item: Integer; MaxLen: Integer): String;
var
  S: String;
  SR: PSearchRec;
begin
  SR := PSearchRec(List^.At(Item));
  S := SR^.Name;
  if SR^.Attr and Directory <> 0 then
  begin
    S[Length(S)+1] := '\';
    Inc(S[0]);
  end;
  GetText := S;
end;

procedure TFileList.HandleEvent(var Event: TEvent);
begin
  if (Event.What = evMouseDown) and (Event.Double) then
  begin
    Event.What := evCommand;
    Event.Command := cmOK;
    PutEvent(Event);
    ClearEvent(Event);
  end
  else TSortedListBox.HandleEvent(Event);
end;

procedure TFileList.ReadDirectory(AWildCard: PathStr);
const
  FindAttr = ReadOnly + Archive;
  AllFiles = '*.*';
  PrevDir  = '..';
var
  S: SearchRec;
  P: PSearchRec;
  FileList: PFileCollection;
  NumFiles: Word;
  CurPath: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
  Event: TEvent;
  Tmp: PathStr;
  Flag: Integer;
begin
  NumFiles := 0;
  AWildCard := FExpand(AWildCard);
  FSplit(AWildCard, Dir, Name, Ext);
  FileList := New(PFileCollection, Init(5, 5));
  if FileList = Nil then Exit;
  FindFirst(AWildCard, FindAttr, S);
  P := @P;
  while (P <> nil) and (DosError = 0) do
  begin
    if (S.Attr and Directory = 0) then
    begin
      P := MemAlloc(SizeOf(P^));
      if P <> nil then
      begin
        Move(S.Attr, P^, SizeOf(P^));
        FileList^.Insert(P);
      end;
    end;
    FindNext(S);
  end;
  Tmp := Dir + AllFiles;
  FindFirst(Tmp, Directory, S);
  while (P <> nil) and (DosError = 0) do
  begin
    if (S.Attr and Directory <> 0) and (S.Name[1] <> '.') then
    begin
      P := MemAlloc(SizeOf(P^));
      if P <> nil then
      begin
        Move(S.Attr, P^, SizeOf(P^));
        FileList^.Insert(PObject(P));
      end;
    end;
    FindNext(S);
  end;
  if Length(Dir) > 4 then
  begin
    P := MemAlloc(SizeOf(P^));
    if P <> nil then
    begin
      FindFirst(Tmp, Directory, S);
      FindNext(S);
      if (DosError = 0) and (S.Name = PrevDir) then
        Move(S.Attr, P^, SizeOf(P^))
      else
      begin
        P^.Name := PrevDir;
        P^.Size := 0;
        P^.Time := $210000;
        P^.Attr := Directory;
      end;
      FileList^.Insert(PObject(P));
    end;
  end;
{$IFDEF Ukrainian}
  if P = nil then MessageBox('Слишком много файлов.', nil, mfOkButton + mfWarning);
{$ELSE}
  {$IFDEF Russian}
  if P = nil then MessageBox('Слишком много файлов.', nil, mfOkButton + mfWarning);
  {$ELSE}
  if P = nil then MessageBox('Too many files.', nil, mfOkButton + mfWarning);
  {$ENDIF}
{$ENDIF}
  NewList(FileList);
  if List^.Count > 0 then
  begin
    Event.What := evBroadcast;
    Event.Command := cmFileFocused;
    Event.InfoPtr := List^.At(0);
    Owner^.HandleEvent(Event);
  end;
end;

procedure TFileList.SetData(var Rec);
begin
  with PFileDialog(Owner)^ do
    Self.ReadDirectory(Directory^ + WildCard);
end;

{ TFileInfoPane }

constructor TFileInfoPane.Init(var Bounds: TRect);
begin
  TView.Init(Bounds);
  EventMask := EventMask or evBroadcast;
end;

procedure TFileInfoPane.Draw;   {TONY}

var
  D: String[9];
  PM: Boolean;
  Color: Word;
  Time: DateTime;
  Path: PathStr;
  FmtId: String;
  Params: array[0..7] of LongInt;
  Str: String[80];
{$IFDEF Ukrainian}
  M: String[8];
const
  sDirectoryLine = ' %-12s %-9s %2d %8s %4d,%2d:%02d ';
  sFileLine      = ' %-12s %-9d %2d %8s %4d,%2d:%02d ';
  Month: array[1..12] of String[8] =
    ('января','февраля','марта','апреля','мая','июня',
     'июля','августа','сентября','октября','ноября','декабря');
{$ELSE}
  {$IFDEF Russian}
  M: String[8];
const
  sDirectoryLine = ' %-12s %-9s %2d %8s %4d,%2d:%02d ';
  sFileLine      = ' %-12s %-9d %2d %8s %4d,%2d:%02d ';
  Month: array[1..12] of String[8] =
    ('января','февраля','марта','апреля','мая','июня',
     'июля','августа','сентября','октября','ноября','декабря');
  {$ELSE}
  M: String[3];
const
  sDirectoryLine = ' %-12s %-9s %3s %2d, %4d  %2d:%02d%cm';
  sFileLine      = ' %-12s %-9d %3s %2d, %4d  %2d:%02d%cm';
  Month: array[1..12] of String[3] =
    ('Jan','Feb','Mar','Apr','May','Jun',
     'Jul','Aug','Sep','Oct','Nov','Dec');
  {$ENDIF}
{$ENDIF}
begin
  { Clear area }
  Color := GetColor($0102);
  Bar(0, 0, Size.X, Size.Y, Lo(Color));
  { Display path }
  Path := FExpand(PFileDialog(Owner)^.Directory^+PFileDialog(Owner)^.WildCard);
  WrStr(CharWidth, 0, Path, Hi(Color));

  { Display file }
  Params[0] := LongInt(@S.Name);
  Params[0] := LongInt(@S.Name);
  if S.Attr and Directory <> 0 then
  begin
    FmtId := sDirectoryLine;
{$IFDEF Ukrainian}
    D := 'Каталог  ';
{$ELSE}
  {$IFDEF Russian}
    D := 'Каталог  ';
  {$ELSE}
    D := 'Directory';
  {$ENDIF}
{$ENDIF}
    Params[1] := LongInt(@D);
  end else
  begin
    FmtId := sFileLine;
    Params[1] := S.Size;
  end;
  UnpackTime(S.Time, Time);
  M := Month[Time.Month];
{$IFDEF Ukrainian}
  Params[2] := Time.Day;
  Params[3] := LongInt(@M);
  Params[4] := Time.Year;
  Params[5] := Time.Hour;
  Params[6] := Time.Min;
{$ELSE}
  {$IFDEF Russian}
  Params[2] := Time.Day;
  Params[3] := LongInt(@M);
  Params[4] := Time.Year;
  Params[5] := Time.Hour;
  Params[6] := Time.Min;
  {$ELSE}
  Params[2] := LongInt(@M);
  Params[3] := Time.Day;
  Params[4] := Time.Year;
  PM := Time.Hour >= 12;
  Time.Hour := Time.Hour mod 12;
  if Time.Hour = 0 then Time.Hour := 12;
  Params[5] := Time.Hour;
  Params[6] := Time.Min;
  if PM then Params[7] := Byte('p')
  else Params[7] := Byte('a');
  {$ENDIF}
{$ENDIF}
  FormatStr(Str, FmtId, Params);
  WrStr(0, FontHeight, Str, Hi(Color));
end;

function TFileInfoPane.GetPalette: PPalette;
const
  P: String[Length(CInfoPane)] = CInfoPane;
begin
  GetPalette := @P;
end;

procedure TFileInfoPane.HandleEvent(var Event: TEvent);
begin
  TView.HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmFileFocused) then
  begin
    S := PSearchRec(Event.InfoPtr)^;
    DrawView;
  end;
end;

{ TFileDialog }

constructor TFileDialog.Init(AWildCard: TWildStr; const ATitle,  {TONY}
  InputName: String; AOptions: Word; HistoryId: Byte);
var
  Control: PView;
  R: TRect;
  Opt: Word;
  CH, CW: Integer;
begin
  SelectFontCaps(GlobalFont);
  CH := GetHeight;
  CW := GetCharWidth;
  R.Assign(15*CW, CH, 64*CW, 20*CH);
  TDialog.Init(R, ATitle);
  Options := Options or ofCentered;
  WildCard := AWildCard;

  R.Assign(3*CW, 3*CH, 31*CW, 4*CH);
  FileName := New(PFileInputLine, Init(R, 79));
  FileName^.Data^ := WildCard;
  Insert(FileName);
  R.Assign(2*CW, 2*CH, (3+CStrLen(InputName))*CW, 3*CH);
  Control := New(PLabel, Init(R, InputName, FileName));
  Insert(Control);
  R.Assign(31*CW, 3*CH, 34*CW, 4*CH);
  Control := New(PHistory, Init(R, FileName, HistoryId));
  Insert(Control);

  R.Assign(3*CW, 14*CH, 34*CW, 15*CH);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3*CW, 6*CH, 34*CW, 14*CH);
  FileList := New(PFileList, Init(R, PScrollBar(Control)));
  Insert(FileList);
  R.Assign(2*CW, 5*CH, 8*CW, 6*CH);
{$IFDEF Ukrainian}
  Control := New(PLabel, Init(R, '~Ф~айлы', FileList));
  Insert(Control);

  R.Assign(35*CW, 3*CH, 46*CW, 5*CH);
  Opt := bfDefault;
  if AOptions and fdOpenButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~О~ткрыть', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdOkButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~В~вод', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdReplaceButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~З~аменить',cmFileReplace, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdClearButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~О~чистить',cmFileClear, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  if AOptions and fdHelpButton <> 0 then
  begin
    Insert(New(PButton, Init(R, 'Помощь',cmHelp, bfNormal)));
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
{$ELSE}
  {$IFDEF Russian}
  Control := New(PLabel, Init(R, '~Ф~айлы', FileList));
  Insert(Control);

  R.Assign(35*CW, 3*CH, 46*CW, 5*CH);
  Opt := bfDefault;
  if AOptions and fdOpenButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~О~ткрыть', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdOkButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~В~вод', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdReplaceButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~З~аменить',cmFileReplace, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdClearButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~О~чистить',cmFileClear, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  if AOptions and fdHelpButton <> 0 then
  begin
    Insert(New(PButton, Init(R, 'Помощь',cmHelp, bfNormal)));
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  {$ELSE}
  Control := New(PLabel, Init(R, '~F~iles', FileList));
  Insert(Control);

  R.Assign(35*CW, 3*CH, 46*CW, 5*CH);
  Opt := bfDefault;
  if AOptions and fdOpenButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~O~pen', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdOkButton <> 0 then
  begin
    Insert(New(PButton, Init(R, 'O~K~', cmFileOpen, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdReplaceButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~R~eplace',cmFileReplace, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  if AOptions and fdClearButton <> 0 then
  begin
    Insert(New(PButton, Init(R, '~C~lear',cmFileClear, Opt)));
    Opt := bfNormal;
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  if AOptions and fdHelpButton <> 0 then
  begin
    Insert(New(PButton, Init(R, 'Help',cmHelp, bfNormal)));
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  end;
  {$ENDIF}
{$ENDIF}

  R.Assign(1*CW ,16*CH, 48*CW, 18*CH);
  Control := New(PFileInfoPane, Init(R));
  Insert(Control);

  SelectNext(False);

  if AOptions and fdNoLoadDir = 0 then ReadDirectory;
end;

constructor TFileDialog.Load(var S: TStream);
var
  ACurDir: DirStr;
  ViewId: Word;
begin
  TDialog.Load(S);
  S.Read(WildCard, SizeOf(TWildStr));
  GetSubViewPtr(S, FileName);
  GetSubViewPtr(S, FileList);

  ReadDirectory;
end;

destructor TFileDialog.Done;
begin
  DisposeStr(Directory);
  TDialog.Done;
end;

procedure TFileDialog.GetData(var Rec);
begin
  GetFilename(PathStr(Rec));
end;

procedure TFileDialog.GetFileName(var S: PathStr);
var
  Path: PathStr;
  Name: NameStr;
  Ext: ExtStr;
  TPath: PathStr;
  TName: NameStr;
  TExt: NameStr;

function LTrim(const S: String): String;
var
  I: Integer;
begin
  I := 1;
  while (I < Length(S)) and (S[I] = ' ') do Inc(I);
  LTrim := Copy(S, I, 255);
end;

function RTrim(const S: String): String;
var
  I: Integer;
begin
  I := Length(S);
  while S[I] = ' ' do Dec(I);
  RTrim := Copy(S, 1, I);
end;

function RelativePath(var S: PathStr): Boolean;
begin
  S := LTrim(RTrim(S));
  RelativePath := not ((S <> '') and ((S[1] = '\') or (S[2] = ':')));
end;

function NoWildChars(S: String): String; near; assembler;
asm
        PUSH    DS
        LDS     SI,S
        XOR     AX,AX
        LODSB
        XCHG    AX,CX
        LES     DI,@Result
        INC     DI
        JCXZ    @@3
@@1:    LODSB
        CMP     AL,'?'
        JE      @@2
        CMP     AL,'*'
        JE      @@2
        STOSB
@@2:    LOOP    @@1
@@3:    XCHG    AX,DI
        MOV     DI,WORD PTR @Result
        SUB     AX,DI
        DEC     AX
        STOSB
        POP     DS
end;

begin
  S := FileName^.Data^;
  if RelativePath(S) then S := FExpand(Directory^ + S)
  else S := FExpand(S);
  FSplit(S, Path, Name, Ext);
  if ((Name = '') or (Ext = '')) and not IsDir(S) then
  begin
    FSplit(WildCard, TPath, TName, TExt);
    if ((Name = '') and (Ext = '')) then S := Path + TName + TExt
    else if Name = '' then S := Path + TName + Ext
    else if Ext = '' then
    begin
      if IsWild(Name) then S := Path + Name + TExt
      else S := Path + Name + NoWildChars(TExt);
    end;
  end;
end;

procedure TFileDialog.HandleEvent(var Event: TEvent);
begin
  TDialog.HandleEvent(Event);
  if Event.What = evCommand then
    case Event.Command of
      cmFileOpen, cmFileReplace, cmFileClear:
        begin
          EndModal(Event.Command);
          ClearEvent(Event);
        end;
    end;
end;

procedure TFileDialog.SetData(var Rec);
begin
  TDialog.SetData(Rec);
  if (PathStr(Rec) <> '') and (IsWild(TWildStr(Rec))) then
  begin
    Valid(cmFileInit);
    FileName^.Select;
  end;
end;

procedure TFileDialog.ReadDirectory;
begin
  FileList^.ReadDirectory(WildCard);
  Directory := NewStr(GetCurDir);
end;

procedure TFileDialog.Store(var S: TStream);
begin
  TDialog.Store(S);
  S.Write(WildCard, SizeOf(TWildStr));
  PutSubViewPtr(S, FileName);
  PutSubViewPtr(S, FileList);
end;

function TFileDialog.Valid(Command: Word): Boolean;
var
  T: Boolean;
  FName: PathStr;
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;

function CheckDirectory(var S: PathStr): Boolean;
begin
  if not PathValid(S) then
  begin
{$IFDEF Ukrainian}
    MessageBox('Неверный диск или каталог.', nil, mfError + mfOkButton);
{$ELSE}
  {$IFDEF Russian}
    MessageBox('Неверный диск или каталог.', nil, mfError + mfOkButton);
  {$ELSE}
    MessageBox('Invalid drive or directory.', nil, mfError + mfOkButton);
  {$ENDIF}
{$ENDIF}
    FileName^.Select;
    CheckDirectory := False;
  end else CheckDirectory := True;
end;

begin
  if Command = 0 then
  begin
    Valid := True;
    Exit;
  end else Valid := False;
  if TDialog.Valid(Command) then
  begin
    GetFileName(FName);
    if (Command <> cmCancel) and (Command <> cmFileClear) then
    begin
      if IsWild(FName) then
      begin
        FSplit(FName, Dir, Name, Ext);
        if CheckDirectory(Dir) then
        begin
          DisposeStr(Directory);
          Directory := NewStr(Dir);
          WildCard := Name+Ext;
          if Command <> cmFileInit then FileList^.Select;
          FileList^.ReadDirectory(Directory^+WildCard);
        end
      end
      else if IsDir(FName) then
      begin
        if CheckDirectory(FName) then
        begin
          DisposeStr(Directory);
          Directory := NewSTr(FName+'\');
          if Command <> cmFileInit then FileList^.Select;
          FileList^.ReadDirectory(Directory^+WildCard);
        end
      end else if ValidFileName(FName) then Valid := True
      else
      begin
{$IFDEF Ukrainian}
        MessageBox('Неверное имя файла.', nil, mfError + mfOkButton);
{$ELSE}
  {$IFDEF Russian}
        MessageBox('Неверное имя файла.', nil, mfError + mfOkButton);
  {$ELSE}
        MessageBox('Invalid file name.', nil, mfError + mfOkButton);
  {$ENDIF}
{$ENDIF}
        Valid := False;
      end
    end
    else Valid := True;
  end;
end;

{ TDirCollection }

function TDirCollection.GetItem(var S: TStream): Pointer;
var
  DirItem: PDirEntry;
begin
  New(DirItem);
  GetItem := DirItem;
  if DirItem = Nil then Exit;
  DirItem^.DisplayText := S.ReadStr;
  DirItem^.Directory := S.ReadStr;
end;

procedure TDirCollection.FreeItem(Item: Pointer);
var
  DirItem: PDirEntry absolute Item;
begin
  DisposeStr(DirItem^.DisplayText);
  DisposeStr(DirItem^.Directory);
  Dispose(DirItem);
end;

procedure TDirCollection.PutItem(var S: TStream; Item: Pointer);
var
  DirItem: PDirEntry absolute Item;
begin
  S.WriteStr(DirItem^.DisplayText);
  S.WriteStr(DirItem^.Directory);
end;

{ TDirListBox }

const
{$IFDEF Ukrainian}
  DrivesS: String[5] = 'Диски';
{$ELSE}
  {$IFDEF Russian}
  DrivesS: String[5] = 'Диски';
  {$ELSE}
  DrivesS: String[6] = 'Drives';
  {$ENDIF}
{$ENDIF}
  Drives: PString = @DrivesS;

constructor TDirListBox.Init(var Bounds: TRect; AScrollBar:
  PScrollBar);
begin
  TListBox.Init(Bounds, 1, AScrollBar);
  Dir := '';
end;

destructor TDirListBox.Done;
begin
  if List <> nil then Dispose(List, Done);
  TListBox.Done;
end;

Const
  NoFolder  = 0;
  Closed    = 1;
  Opened    = 2;
  Root      = 3;
  HardDrive = 4;
  Floppy    = 5;
  CDROM     = 6;
  NetDrive  = 7;
  MemDrive  = 8;

procedure TDirListBox.DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt); {DK}
var
  Text, S: String;
  ColWidth: Integer;
  I, J, K : Byte;
  L : Integer;
  BM : PImage;
  CT, LineColor : Byte;
begin
  ColWidth:= (Bounds.B.X - Bounds.A.X) div Step.X;
  FillChar(Text[1], ColWidth, 32);
  Text[0]:= Char(ColWidth);
  S := GetText(Item, 255);
  CT := Byte(S[Byte(S[0])]);

  System.Insert(GetText(Item, ColWidth + Indent), Text, 2);
    Bar(Bounds.A.X, Bounds.A.Y, Bounds.B.X, Bounds.B.Y, Lo(Color));

  if (Bounds.B.Y - Bounds.A.Y) > FontHeight then
    Inc(Bounds.A.Y, (Bounds.B.Y - Bounds.A.Y - FontHeight) div 2);

  Text := Copy(Text, 1, ColWidth);
  S := Text;

  if CT > 0 then BM := GetImage(240 + CT - 1) else BM := GetImage(0);
  if S[0] > #0 then for I := 1 to Byte(S[0]) do
    if S[I] in [#0..#10, #192..#196] then S[I] := ' ';

  WrStr(Bounds.A.X + BitmapWidth(BM), Bounds.A.Y, S, Hi(Color));
  J := FontWidth(' '); K := FontHeight;
  {if Item = Focused then LineColor := 15 else }LineColor := 0;
  if Text[0] > #0 then for I := 1 to Byte(Text[0]) do begin
    if Text[I] in [#192..#196] then with Bounds do begin
      case Text[I] of
      #192 : begin
        VLine(A.X + J div 2, A.Y, A.Y + K div 2, LineColor);
        HLine(A.X + J div 2, A.Y + K div 2, A.X + J, LineColor);
      end;
      #194 : begin
        VLine(A.X + J div 2, A.Y + K div 2, A.Y + K, LineColor);
        HLine(A.X, A.Y + K div 2, A.X + J, LineColor);
      end;
      #195 : begin
        VLine(A.X + J div 2, A.Y, A.Y + K, LineColor);
        HLine(A.X + J div 2, A.Y + K div 2, A.X + J, LineColor);
      end;
      #196 : HLine(A.X, A.Y + K div 2, A.X + J, LineColor);
      else end;
      L := Bounds.A.X;
    end;
    Inc(Bounds.A.X, J);
  end;
  SetColorBitBlt(ColorIndex^[4], True);
  if CT > 0 then
    PutBMPOp(BM, L, Bounds.A.Y + K div 2 - BitmapHeight(BM) div 2, UserBitBlt);
End;

function TDirListBox.GetText(Item: Integer; MaxLen: Integer): String;
begin
  GetText := PDirEntry(List^.At(Item))^.DisplayText^;
end;

procedure TDirListBox.HandleEvent(var Event: TEvent);
begin
  if (Event.What = evMouseDown) and (Event.Double) then
  begin
    Event.What := evCommand;
    Event.Command := cmChangeDir;
    PutEvent(Event);
    ClearEvent(Event);
  end
  else TListBox.HandleEvent(Event);
end;

function TDirListBox.IsSelected(Item: Integer): Boolean;
begin
  IsSelected := Item = Cur;
end;

procedure TDirListBox.NewDirectory(var ADir: DirStr);
const
  PathDir            = '└─┬';
  FirstDir           =   '└┬─';
  MiddleDir          =   ' ├─';
  LastDir            =   ' └─';
  IndentSize         = '  ';
var
  AList: PCollection;
  NewDir, Dirct: DirStr;
  C, OldC, DrvC: Char;
  S, Indent: String[80];
  P: PString;
  isFirst: Boolean;
  SR: SearchRec;
  I: Integer;
  DirEntry: PDirEntry;
  CDList:array[0..20] of Byte;
  CDCount, CDFirst:Byte;

function NewDirEntry(const DisplayText, Directory: String): PDirEntry; near;
var
  DirEntry: PDirEntry;
begin
  New(DirEntry);
  NewDirEntry := DirEntry;
  if DirEntry = Nil then Exit;
  DirEntry^.DisplayText := NewStr(DisplayText);
  DirEntry^.Directory := NewStr(Directory);
end;

function GetCurDrive: Char; near; assembler;
asm
        MOV     AH,19H
        INT     21H
        ADD     AL,'A'
end;

function IsNetDrive(Drive : Byte) : Boolean; Assembler; Asm
  mov ax, 4409h
  mov bl, Drive
  int 21h
  jc @@1
  test dx, 1000h
  jz @@1
  mov al, 1
  jmp @@2
@@1:
  xor al, al
@@2:
End;

Procedure CheckDrive;
Var
  I : Integer;
Begin
  if OldC in ['A', 'B'] then DrvC := Char(Floppy) else DrvC := Char(HardDrive);
  if CDCount > 0 then for I := 0 to CDCount - 1 do
    if OldC = Char(Ord('A') + CDList[I]) then begin
      DrvC := Char(CDROM);
      Break;
    end;
  if IsNetDrive(Byte(OldC) - Ord('A')) then DrvC := Char(NetDrive);
End;

begin
  Dir := ADir;
  Asm {CD ROM detection}
    mov cdcount, 0
    xor al, al
    xor bx, bx
    mov ah, 15h
    int 2fh
    or bx, bx
    jz @@Exit
    mov cdcount, bl
    mov cdfirst, cl
    mov ax, ss
    mov es, ax
    mov al, 0dh
    mov ah, 15h
    lea bx, cdlist
    int 2fh
@@Exit:
  end;
  AList := New(PDirCollection, Init(5,5));
  if AList= Nil then Exit;
  AList^.Insert(NewDirEntry(Drives^ + Char(NoFolder), Drives^));
  if Dir = Drives^ then
  begin
    isFirst := True;
    OldC := ' ';
    for C := 'A' to 'Z' do begin
      if (C < 'C') or DriveValid(C) then  begin
        if OldC <> ' ' then begin
          if isFirst then begin
            S := FirstDir + OldC;
            isFirst := False;
          end
          else S := MiddleDir + OldC;
          CheckDrive;
          AList^.Insert(NewDirEntry(S+ DrvC, OldC + ':\'));
        end;
        if C = GetCurDrive then Cur := AList^.Count;
        OldC := C;
      end;
    end;
    CheckDrive;
    if OldC <> ' ' then AList^.Insert(NewDirEntry(LastDir + OldC + DrvC, OldC + ':\'));
  end else begin
    Indent := IndentSize;
    NewDir := Dir;
    Dirct := Copy(NewDir,1,3);
    AList^.Insert(NewDirEntry(PathDir + Dirct + Char(Root), Dirct));
    NewDir := Copy(NewDir,4,255);
    while NewDir <> '' do
    begin
      I := Pos('\',NewDir);
      if I <> 0 then
      begin
        S := Copy(NewDir,1,I-1);
        Dirct := Dirct + S;
        AList^.Insert(NewDirEntry(Indent + PathDir + S + Char(Opened), Dirct));
        NewDir := Copy(NewDir,I+1,255);
      end
      else
      begin
        Dirct := Dirct + NewDir;
        AList^.Insert(NewDirEntry(Indent + PathDir + NewDir+ Char(Opened), Dirct));
        NewDir := '';
      end;
      Indent := Indent + IndentSize;
      Dirct := Dirct + '\';
    end;
    Cur := AList^.Count-1;
    isFirst := True;
    NewDir := Dirct + '*.*';
    FindFirst(NewDir, Directory, SR);
    while DosError = 0 do
    begin
      if (SR.Attr and Directory <> 0) and (SR.Name[1] <> '.') then
      begin
        if isFirst then
        begin
          S := FirstDir;
          isFirst := False;
        end else S := MiddleDir;
        AList^.Insert(NewDirEntry(Indent + S + SR.Name+ Char(Closed{}), Dirct + SR.Name));
      end;
      FindNext(SR);
    end;
    P := PDirEntry(AList^.At(AList^.Count-1))^.DisplayText;
    I := Pos('└',P^);
    if I = 0 then begin
      I := Pos('├',P^);
      if I <> 0 then P^[I] := '└';
    end else begin
      P^[I+1] := '─';
      P^[I+2] := '─';
    end;
  end;
  NewList(AList);
  FocusItem(Cur);
end;

procedure TDirListBox.SetState(AState: Word; Enable: Boolean);
begin
  TListBox.SetState(AState, Enable);
  if AState and sfFocused <> 0 then
    PChDirDialog(Owner)^.ChDirButton^.MakeDefault(Enable);
end;

{ TChDirDialog }

constructor TChDirDialog.Init(AOptions: Word; HistoryId: Word); {TONY}
var
  R: TRect;
  Control: PView;
  CurDir: DirStr;
  CH, CW: Integer;
begin
  SelectFontCaps(GlobalFont);
  CH := GetHeight;
  CW := GetCharWidth;
  R.Assign(16*CW, 2*CH, 64*CW, 20*CH);
{$IFDEF Ukrainian}
  TDialog.Init(R, 'Сменить каталог');
  Options := Options or ofCentered;

  R.Assign(3*CW, 3*CH, 30*CW, 4*CH);
  DirInput := New(PInputLine, Init(R, 68));
  Insert(DirInput);
  R.Assign(2*CW, 2*CH, 17*CW, 3*CH);
  Control := New(PLabel, Init(R, '~И~мя каталога', DirInput));
  Insert(Control);
  R.Assign(30*CW, 3*CH, 33*CW, 4*CH);
  Control := New(PHistory, Init(R, DirInput, HistoryId));
  Insert(Control);

  R.Assign(32*CW, 6*CH, 33*CW, 16*CH);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3*CW, 6*CH, 32*CW, 16*CH);
  DirList := New(PDirListBox, Init(R, PScrollBar(Control)));
  Insert(DirList);
  R.Assign(2*CW, 5*CH, 20*CW, 6*CH);
  Control := New(PLabel, Init(R, '~Д~ерево каталогов', DirList));
  Insert(Control);

  R.Assign(35*CW, 6*CH, 45*CW, 8*CH);
  OkButton := New(PButton, Init(R, '~В~вод', cmOK, bfDefault));
  Insert(OkButton);
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  ChDirButton := New(PButton, Init(R, '~П~ереход', cmChangeDir, bfNormal));
  Insert(ChDirButton);
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  Insert(New(PButton, Init(R, '~О~братно', cmRevert, bfNormal)));
  if AOptions and cdHelpButton <> 0 then
  begin
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
    Insert(New(PButton, Init(R, 'Помощь', cmHelp, bfNormal)));
  end;
{$ELSE}
  {$IFDEF Russian}
  TDialog.Init(R, 'Сменить каталог');
  Options := Options or ofCentered;

  R.Assign(3*CW, 3*CH, 30*CW, 4*CH);
  DirInput := New(PInputLine, Init(R, 68));
  Insert(DirInput);
  R.Assign(2*CW, 2*CH, 17*CW, 3*CH);
  Control := New(PLabel, Init(R, '~И~мя каталога', DirInput));
  Insert(Control);
  R.Assign(30*CW, 3*CH, 33*CW, 4*CH);
  Control := New(PHistory, Init(R, DirInput, HistoryId));
  Insert(Control);

  R.Assign(32*CW, 6*CH, 33*CW, 16*CH);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3*CW, 6*CH, 32*CW, 16*CH);
  DirList := New(PDirListBox, Init(R, PScrollBar(Control)));
  Insert(DirList);
  R.Assign(2*CW, 5*CH, 20*CW, 6*CH);
  Control := New(PLabel, Init(R, '~Д~ерево каталогов', DirList));
  Insert(Control);

  R.Assign(35*CW, 6*CH, 45*CW, 8*CH);
  OkButton := New(PButton, Init(R, '~В~вод', cmOK, bfDefault));
  Insert(OkButton);
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  ChDirButton := New(PButton, Init(R, '~П~ереход', cmChangeDir, bfNormal));
  Insert(ChDirButton);
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  Insert(New(PButton, Init(R, '~О~братно', cmRevert, bfNormal)));
  if AOptions and cdHelpButton <> 0 then
  begin
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
    Insert(New(PButton, Init(R, 'Помощь', cmHelp, bfNormal)));
  end;
  {$ELSE}
  TDialog.Init(R, 'Change Directory');
  Options := Options or ofCentered;

  R.Assign(3*CW, 3*CH, 30*CW, 4*CH);
  DirInput := New(PInputLine, Init(R, 68));
  Insert(DirInput);
  R.Assign(2*CW, 2*CH, 17*CW, 3*CH);
  Control := New(PLabel, Init(R, 'Directory ~n~ame', DirInput));
  Insert(Control);
  R.Assign(30*CW, 3*CH, 33*CW, 4*CH);
  Control := New(PHistory, Init(R, DirInput, HistoryId));
  Insert(Control);

  R.Assign(32*CW, 6*CH, 33*CW, 16*CH);
  Control := New(PScrollBar, Init(R));
  Insert(Control);
  R.Assign(3*CW, 6*CH, 32*CW, 16*CH);
  DirList := New(PDirListBox, Init(R, PScrollBar(Control)));
  Insert(DirList);
  R.Assign(2*CW, 5*CH, 17*CW, 6*CH);
  Control := New(PLabel, Init(R, 'Directory ~t~ree', DirList));
  Insert(Control);

  R.Assign(35*CW, 6*CH, 45*CW, 8*CH);
  OkButton := New(PButton, Init(R, 'O~K~', cmOK, bfDefault));
  Insert(OkButton);
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  ChDirButton := New(PButton, Init(R, '~C~hdir', cmChangeDir, bfNormal));
  Insert(ChDirButton);
  Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
  Insert(New(PButton, Init(R, '~R~evert', cmRevert, bfNormal)));
  if AOptions and cdHelpButton <> 0 then
  begin
    Inc(R.A.Y, 3*CH); Inc(R.B.Y, 3*CH);
    Insert(New(PButton, Init(R, 'Help', cmHelp, bfNormal)));
  end;
  {$ENDIF}
{$ENDIF}

  if AOptions and cdNoLoadDir = 0 then SetUpDialog;

  SelectNext(False);
end;

constructor TChDirDialog.Load(var S: TStream);
var
  CurDir: DirStr;
begin
  TDialog.Load(S);
  GetSubViewPtr(S, DirList);
  GetSubViewPtr(S, DirInput);
  GetSubViewPtr(S, OkButton);
  GetSubViewPtr(S, ChDirbutton);
  SetUpDialog;
end;

function TChDirDialog.DataSize: Word;
begin
  DataSize := 0;
end;

procedure TChDirDialog.GetData(var Rec);
begin
end;

procedure TChDirDialog.HandleEvent(var Event: TEvent);
var
  CurDir: DirStr;
  P: PDirEntry;
begin
  TDialog.HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmRevert: GetDir(0,CurDir);
          cmChangeDir:
            begin
              P := DirList^.List^.At(DirList^.Focused);
              if (P^.Directory^ = Drives^) or DriveValid(P^.Directory^[1]) then
                CurDir := P^.Directory^
              else Exit;
            end;
        else
          Exit;
        end;
        if (Length(CurDir) > 3) and (CurDir[Length(CurDir)] = '\') then
          CurDir := Copy(CurDir,1,Length(CurDir)-1);
        DirList^.NewDirectory(CurDir);
        DirInput^.Data^ := CurDir;
        DirInput^.DrawView;
        DirList^.Select;
        ClearEvent(Event);
      end;
  end;
end;

procedure TChDirDialog.SetData(var Rec);
begin
end;

procedure TChDirDialog.SetUpDialog;
var
  CurDir: DirStr;
begin
  if DirList <> nil then
  begin
    CurDir := GetCurDir;
    DirList^.NewDirectory(CurDir);
    if (Length(CurDir) > 3) and (CurDir[Length(CurDir)] = '\') then
      CurDir := Copy(CurDir,1,Length(CurDir)-1);
    if DirInput <> nil then
    begin
      DirInput^.Data^ := CurDir;
      DirInput^.DrawView;
    end;
  end;
end;

procedure TChDirDialog.Store(var S: TStream);
begin
  TDialog.Store(S);
  PutSubViewPtr(S, DirList);
  PutSubViewPtr(S, DirInput);
  PutSubViewPtr(S, OkButton);
  PutSubViewPtr(S, ChDirButton);
end;

function TChDirDialog.Valid(Command: Word): Boolean;
var
  P: PathStr;
begin
  Valid := True;
  if Command = cmOk then
  begin
    P := FExpand(DirInput^.Data^);
    if (Length(P) > 3) and (P[Length(P)] = '\') then Dec(P[0]);
    {$I-}
    ChDir(P);
    if IOResult <> 0 then
    begin
{$IFDEF Ukrainian}
      MessageBox('Неверный директорий.', nil, mfError + mfOkButton);
{$ELSE}
  {$IFDEF Russian}
      MessageBox('Неверный директорий.', nil, mfError + mfOkButton);
  {$ELSE}
      MessageBox('Invalid directory.', nil, mfError + mfOkButton);
  {$ENDIF}
{$ENDIF}
      Valid := False;
    end;
    {$I+}
  end;
end;

procedure RegisterStdDlg;
begin
  RegisterType(RFileInputLine);
  RegisterType(RFileCollection);
  RegisterType(RFileList);
  RegisterType(RFileInfoPane);
  RegisterType(RFileDialog);
  RegisterType(RDirCollection);
  RegisterType(RDirListBox);
  RegisterType(RSortedListBox);
  RegisterType(RChDirDialog);
end;

{$L FOLDERS.OBJ}
Procedure Dir_ClosedFolder; External;
Procedure Dir_OpenedFolder; External;
Procedure Dir_RootFolder; External;
Procedure Dir_HardDrive; External;
Procedure Dir_FloppyDrive; External;
Procedure Dir_CDROM; External;
Procedure Dir_Net; External;
Procedure Dir_Memory; External;

Procedure RegisterStdDlgImages;
Begin
  {$IFDEF UseStandardBitMaps}
  RegisterImageInCode(240, @Dir_ClosedFolder);
  RegisterImageInCode(241, @Dir_OpenedFolder);
  RegisterImageInCode(242, @Dir_RootFolder);
  RegisterImageInCode(243, @Dir_HardDrive);
  RegisterImageInCode(244, @Dir_FloppyDrive);
  RegisterImageInCode(245, @Dir_CDROM);
  RegisterImageInCode(246, @Dir_Net);
  RegisterImageInCode(247, @Dir_Memory);
  {$ENDIF}
End;

begin
  RegisterStdDlgImages;
end.
