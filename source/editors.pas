
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Editors;

{$I-,O+,F+,V-,X+,S-}

interface

uses Drivers, Objects, Views, Dialogs, EGFont, Bitmaps {SHIM};

const
  cmFind        = 82;
  cmReplace     = 83;
  cmSearchAgain = 84;

const
  cmCharLeft    = 500;
  cmCharRight   = 501;
  cmWordLeft    = 502;
  cmWordRight   = 503;
  cmLineStart   = 504;
  cmLineEnd     = 505;
  cmLineUp      = 506;
  cmLineDown    = 507;
  cmPageUp      = 508;
  cmPageDown    = 509;
  cmTextStart   = 510;
  cmTextEnd     = 511;
  cmNewLine     = 512;
  cmBackSpace   = 513;
  cmDelChar     = 514;
  cmDelWord     = 515;
  cmDelStart    = 516;
  cmDelEnd      = 517;
  cmDelLine     = 518;
  cmInsMode     = 519;
  cmStartSelect = 520;
  cmHideSelect  = 521;
  cmIndentMode  = 522;
  cmUpdateTitle = 523;

const
  edOutOfMemory   = 0;
  edReadError     = 1;
  edWriteError    = 2;
  edCreateError   = 3;
  edSaveModify    = 4;
  edSaveUntitled  = 5;
  edSaveAs        = 6;
  edFind          = 7;
  edSearchFailed  = 8;
  edReplace       = 9;
  edReplacePrompt = 10;

const
  efCaseSensitive   = $0001;
  efWholeWordsOnly  = $0002;
  efPromptOnReplace = $0004;
  efReplaceAll      = $0008;
  efDoReplace       = $0010;
  efBackupFiles     = $0100;

const
  CIndicator = #5#6;
{           тХФтХРтХРтХРтХдтХРтХРтХРтХдтХРтХРтХРтХдтХРтХРтХРтХЧ }
{ CEditor   тХС 1 тФВ 2 тФВ 3 тФВ 4 тХС }
{           тХЪтХРтХдтХРтХзтХРтХдтХРтХзтХРтХдтХРтХзтХРтХдтХРтХЭ }
{ Normal fore тФШ   тФВ   тФВ   тФВ   }
{ Normal back тФАтФАтФАтФАтФШ   тФВ   тФВ   }
{ Highlight fore тФАтФАтФАтФАтФАтФШ   тФВ   }
{ Highlight back тФАтФАтФАтФАтФАтФАтФАтФАтФАтФШ   }

  CEditor    = #10#11#12#13; {=CScroller}
  CMemo      = #63#64#65#66;

const
  MaxLineLength = 256;

type
  TEditorDialog = function(Dialog: Integer; Info: Pointer): Word;

type
  PIndicator = ^TIndicator;
  TIndicator = object(TView)
    Location: TPoint;
    Modified: Boolean;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure SetValue(ALocation: TPoint; AModified: Boolean);
  end;

type
  PEditBuffer = ^TEditBuffer;
  TEditBuffer = array[0..65519] of Char;

type
  PEditor = ^TEditor;
  TEditor = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Indicator: PIndicator;
    Buffer: PEditBuffer;
    BufSize: Word;
    BufLen: Word;
    GapLen: Word;
    SelStart: Word;
    SelEnd: Word;
    CurPtr: Word;
    CurPos: TPoint;
    Delta: TPoint;
    Limit: TPoint;
    DrawLine: Integer;
    DrawPtr: Word;
    DelCount: Word;
    InsCount: Word;
    IsValid: Boolean;
    CanUndo: Boolean;
    Modified: Boolean;
    Selecting: Boolean;
    Overwrite: Boolean;
    AutoIndent: Boolean;
    constructor Init(var Bounds: TRect;
      AHScrollBar, AVScrollBar: PScrollBar;
      AIndicator: PIndicator; ABufSize: Word);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function BufChar(P: Word): Char;
    function BufPtr(P: Word): Word;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure ConvertEvent(var Event: TEvent); virtual;
    function CursorVisible: Boolean;
    procedure DeleteSelect;
    procedure DoneBuffer; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitBuffer; virtual;
    function InsertBuffer(var P: PEditBuffer; Offset, Length: Word;
      AllowUndo, SelectText: Boolean): Boolean;
    function InsertFrom(Editor: PEditor): Boolean; virtual;
    function InsertText(Text: Pointer; Length: Word;
      SelectText: Boolean): Boolean;
    procedure ScrollTo(X, Y: Integer);
    function Search(const FindStr: String; Opts: Word): Boolean;
    function SetBufSize(NewSize: Word): Boolean; virtual;
    procedure SetCmdState(Command: Word; Enable: Boolean);
    procedure SetSelect(NewStart, NewEnd: Word; CurStart: Boolean);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
    procedure TrackCursor(Center: Boolean);
    procedure Undo;
    procedure UpdateCommands; virtual;
    function Valid(Command: Word): Boolean; virtual;
  private
    LockCount: Byte;
    UpdateFlags: Byte;
    KeyState: Integer;
    PageSize: TPoint;    {OOA. Size in Chars}
    Step: TPoint;        {OOA. Char's dimensions}
    DrawUpdate: Boolean; {OOA. True if Draw from DoUpdate}
    PageTail  : TPoint; {DK}
    LastDelta : TPoint; {DK}
    function CharPos(P, Target: Word): Integer;
    function CharPtr(P: Word; Target: Integer): Word;
    function ClipCopy: Boolean;
    procedure ClipCut;
    procedure ClipPaste;
    procedure DeleteRange(StartPtr, EndPtr: Word; DelSelect: Boolean);
    procedure DoUpdate;
    procedure DoSearchReplace;
{    procedure DrawLines(Y, Count: Integer; LinePtr: Word);}
    procedure FormatLine(var DrawBuf; LinePtr: Word;
      Width: Integer; Colors: Word);
    procedure Find;
    function GetMousePtr(Mouse: TPoint): Word;
    function HasSelection: Boolean;
    procedure HideSelect;
    function IsClipboard: Boolean;
    function LineEnd(P: Word): Word;
    function LineMove(P: Word; Count: Integer): Word;
    function LineStart(P: Word): Word;
    procedure Lock;
    procedure NewLine;
    function NextChar(P: Word): Word;
    function NextLine(P: Word): Word;
    function NextWord(P: Word): Word;
    function PrevChar(P: Word): Word;
    function PrevLine(P: Word): Word;
    function PrevWord(P: Word): Word;
    procedure Replace;
    procedure SetBufLen(Length: Word);
    procedure SetCurPtr(P: Word; SelectMode: Byte);
    procedure StartSelect;
    procedure ToggleInsMode;
    procedure Unlock;
    procedure Update(AFlags: Byte);

    function  SubStrWidth(X, Y : Integer) : Word; {DK}
    function  SubStrLen(X, Y : Integer) : Word; {DK}
  end;

type
  TMemoData = record
    Length: Word;
    Buffer: TEditBuffer;
  end;

type
  PMemo = ^TMemo;
  TMemo = object(TEditor)
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

type
  PFileEditor = ^TFileEditor;
  TFileEditor = object(TEditor)
    FileName: FNameStr;
    constructor Init(var Bounds: TRect;
      AHScrollBar, AVScrollBar: PScrollBar;
      AIndicator: PIndicator; AFileName: FNameStr);
    constructor Load(var S: TStream);
    procedure DoneBuffer; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitBuffer; virtual;
    function LoadFile: Boolean;
    function Save: Boolean;
    function SaveAs: Boolean;
    function SaveFile: Boolean;
    function SetBufSize(NewSize: Word): Boolean; virtual;
    procedure Store(var S: TStream);
    procedure UpdateCommands; virtual;
    function Valid(Command: Word): Boolean; virtual;
  end;

type
  PEditWindow = ^TEditWindow;
  TEditWindow = object(TWindow)
    Editor: PFileEditor;
    constructor Init(var Bounds: TRect;
      FileName: FNameStr; ANumber: Integer);
    constructor Load(var S: TStream);
    procedure Close; virtual;
    function GetTitle(MaxSize: Integer): TTitleStr; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SizeLimits(var Min, Max: TPoint); virtual;
    procedure Store(var S: TStream);
  end;

function DefEditorDialog(Dialog: Integer; Info: Pointer): Word;
function CreateFindDialog: PDialog;
function CreateReplaceDialog: PDialog;
function StdEditorDialog(Dialog: Integer; Info: Pointer): Word;

const
{$IFDEF Ukrainian}
  WordChars: set of Char = ['0'..'9', 'A'..'Z', '_', 'a'..'z',
                            'А'..'Я', 'а'..'п', 'р'..'я', 'Ё'..'∙'];
{$ELSE}
  {$IFDEF Russian}
  WordChars: set of Char = ['0'..'9', 'A'..'Z', '_', 'a'..'z',
                            'А'..'Я', 'а'..'п', 'р'..'я', 'Ё'..'ё'];
  {$ELSE} {English}
  WordChars: set of Char = ['0'..'9', 'A'..'Z', '_', 'a'..'z'];
  {$ENDIF}
{$ENDIF}
  EditorDialog: TEditorDialog = DefEditorDialog;
  EditorFlags: Word = efBackupFiles + efPromptOnReplace;
  FindStr: String[80] = '';
  ReplaceStr: String[80] = '';
  Clipboard: PEditor = nil;

type
  TFindDialogRec = record
    Find: String[80];
    Options: Word;
  end;

type
  TReplaceDialogRec = record
    Find: String[80];
    Replace: String[80];
    Options: Word;
  end;

const
  REditor: TStreamRec = (
    ObjType: 70;
    VmtLink: Ofs(TypeOf(TEditor)^);
    Load: @TEditor.Load;
    Store: @TEditor.Store
  );
  RMemo: TStreamRec = (
    ObjType: 71;
    VmtLink: Ofs(TypeOf(TMemo)^);
    Load: @TMemo.Load;
    Store: @TMemo.Store
  );
  RFileEditor: TStreamRec = (
    ObjType: 72;
    VmtLink: Ofs(TypeOf(TFileEditor)^);
    Load: @TFileEditor.Load;
    Store: @TFileEditor.Store
  );
  RIndicator: TStreamRec = (
    ObjType: 73;
    VmtLink: Ofs(TypeOf(TIndicator)^);
    Load: @TIndicator.Load;
    Store: @TIndicator.Store
  );
  REditWindow: TStreamRec = (
    ObjType: 74;
    VmtLink: Ofs(TypeOf(TEditWindow)^);
    Load: @TEditWindow.Load;
    Store: @TEditWindow.Store
  );

procedure RegisterEditors;

implementation

uses Memory, Dos, App, StdDlg, MsgBox;

const
  ufUpdate = $01;
  ufLine   = $02;
  ufView   = $04;
  ufScroll = $08;

const
  smExtend = $01;
  smDouble = $02;

const
  sfSearchFailed = $FFFF;

const
  FirstKeys: array[0..37 * 2] of Word = (37,
    Ord(^A), cmWordLeft, Ord(^C), cmPageDown,
    Ord(^D), cmCharRight, Ord(^E), cmLineUp,
    Ord(^F), cmWordRight, Ord(^G), cmDelChar,
    Ord(^H), cmBackSpace, Ord(^K), $FF02,
    Ord(^L), cmSearchAgain, Ord(^M), cmNewLine,
    Ord(^O), cmIndentMode, Ord(^Q), $FF01,
    Ord(^R), cmPageUp, Ord(^S), cmCharLeft,
    Ord(^T), cmDelWord, Ord(^U), cmUndo,
    Ord(^V), cmInsMode, Ord(^X), cmLineDown,
    Ord(^Y), cmDelLine, kbLeft, cmCharLeft,
    kbRight, cmCharRight, kbCtrlLeft, cmWordLeft,
    kbCtrlRight, cmWordRight, kbHome, cmLineStart,
    kbEnd, cmLineEnd, kbUp, cmLineUp,
    kbDown, cmLineDown, kbPgUp, cmPageUp,
    kbPgDn, cmPageDown, kbCtrlPgUp, cmTextStart,
    kbCtrlPgDn, cmTextEnd, kbIns, cmInsMode,
    kbDel, cmDelChar, kbShiftIns, cmPaste,
    kbShiftDel, cmCut, kbCtrlIns, cmCopy,
    kbCtrlDel, cmClear);
  QuickKeys: array[0..8 * 2] of Word = (8,
    Ord('A'), cmReplace, Ord('C'), cmTextEnd,
    Ord('D'), cmLineEnd, Ord('F'), cmFind,
    Ord('H'), cmDelStart, Ord('R'), cmTextStart,
    Ord('S'), cmLineStart, Ord('Y'), cmDelEnd);
  BlockKeys: array[0..5 * 2] of Word = (5,
    Ord('B'), cmStartSelect, Ord('C'), cmPaste,
    Ord('H'), cmHideSelect, Ord('K'), cmCopy,
    Ord('Y'), cmCut);
  KeyMap: array[0..2] of Pointer = (@FirstKeys, @QuickKeys, @BlockKeys);

function DefEditorDialog(Dialog: Integer; Info: Pointer): Word;
begin
  DefEditorDialog := cmCancel;
end;

function CreateFindDialog: PDialog {SHIM};
var
  D: PDialog;
  Control: PView;
  R: TRect;
  CharWidth, FontHeight: integer;
begin
  SelectFontCaps(GlobalFont);
  CharWidth := GetCharWidth;
  FontHeight := GetHeight;
  R.Assign(0, 0, 38*CharWidth, 12*FontHeight);
{$IFDEF Ukrainian}
  D := New(PDialog, Init(R, 'Поиск'));
{$ELSE}
  {$IFDEF Russian}
  D := New(PDialog, Init(R, 'Поиск'));
  {$ELSE} {English}
  D := New(PDialog, Init(R, 'Find'));
  {$ENDIF}
{$ENDIF}
  with D^ do
  begin
    Options := Options or ofCentered;

    R.Assign(3*CharWidth, 3*FontHeight, 32*CharWidth, 4*FontHeight);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);
    R.Assign(2*CharWidth, 2*FontHeight, 15*CharWidth, 3*FontHeight);
{$IFDEF Ukrainian}
    Insert(New(PLabel, Init(R, '~Т~екст поиска', Control)));
{$ELSE}
  {$IFDEF Russian}
    Insert(New(PLabel, Init(R, '~Т~екст поиска', Control)));
  {$ELSE} {English}
    Insert(New(PLabel, Init(R, '~T~ext to find', Control)));
  {$ENDIF}
{$ENDIF}
    R.Assign(32*CharWidth, 3*FontHeight, 35*CharWidth, 4*FontHeight);
    Insert(New(PHistory, Init(R, PInputLine(Control), 10)));

    R.Assign(3*CharWidth, 5*FontHeight, 35*CharWidth, 7*FontHeight);
{$IFDEF Ukrainian}
    Insert(New(PCheckBoxes, Init(R,
      NewSItem('~У~читывать регистр',
      NewSItem('~Ц~елые слова', nil)))));
{$ELSE}
  {$IFDEF Russian}
    Insert(New(PCheckBoxes, Init(R,
      NewSItem('~У~читывать регистр',
      NewSItem('~Ц~елые слова', nil)))));
  {$ELSE} {English}
    Insert(New(PCheckBoxes, Init(R,
      NewSItem('~C~ase sensitive',
      NewSItem('~W~hole words only', nil)))));
  {$ENDIF}
{$ENDIF}
    R.Assign(14*CharWidth, 9*FontHeight, 24*CharWidth, 11*FontHeight);
{$IFDEF UseStandardBitMaps}
    Insert(MakeOKButton(R, cmOk, bfDefault));
{$ELSE}
  {$IFDEF Ukrainian}
      Insert(New(PButton, Init(R, '~В~вод', cmOk, bfDefault)));
  {$ELSE}
    {$IFDEF Russian}
      Insert(New(PButton, Init(R, '~В~вод', cmOk, bfDefault)));
    {$ELSE} {English}
      Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
    Inc(R.A.X, 12*CharWidth); Inc(R.B.X, 12*CharWidth);
{$IFDEF UseStandardBitMaps}
   Insert(MakeCancelButton(R, cmCancel, bfNormal));
{$ELSE}
 {$IFDEF Ukrainian}
     Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
 {$ELSE}
   {$IFDEF Russian}
     Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
   {$ELSE} {English}
     Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
   {$ENDIF}
 {$ENDIF}
{$ENDIF}

    SelectNext(False);
  end;
  CreateFindDialog := D;
end;

function CreateReplaceDialog: PDialog {SHIM};
var
  D: PDialog;
  Control: PView;
  R: TRect;
  CharWidth, FontHeight: integer;
begin
  SelectFontCaps(GlobalFont);
  CharWidth := GetCharWidth;
  FontHeight := GetHeight;
  R.Assign(0, 0, 40*CharWidth, 16*FontHeight);
{$IFDEF Ukrainian}
  D := New(PDialog, Init(R, 'Замена'));
{$ELSE}
  {$IFDEF Russian}
  D := New(PDialog, Init(R, 'Замена'));
  {$ELSE} {English}
  D := New(PDialog, Init(R, 'Replace'));
  {$ENDIF}
{$ENDIF}
  with D^ do
  begin
    Options := Options or ofCentered;

    R.Assign(3*CharWidth, 3*FontHeight, 34*CharWidth, 4*FontHeight);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);
    R.Assign(2*CharWidth, 2*FontHeight, 15*CharWidth, 3*FontHeight);
{$IFDEF Ukrainian}
    Insert(New(PLabel, Init(R, '~Т~екст поиска', Control)));
{$ELSE}
  {$IFDEF Russian}
    Insert(New(PLabel, Init(R, '~Т~екст поиска', Control)));
  {$ELSE} {English}
    Insert(New(PLabel, Init(R, '~T~ext to find', Control)));
  {$ENDIF}
{$ENDIF}
    R.Assign(34*CharWidth, 3*FontHeight, 37*CharWidth, 4*FontHeight);
    Insert(New(PHistory, Init(R, PInputLine(Control), 10)));

    R.Assign(3*CharWidth, 6*FontHeight, 34*CharWidth, 7*FontHeight);
    Control := New(PInputLine, Init(R, 80));
    Insert(Control);
    R.Assign(2*CharWidth, 5*FontHeight, 12*CharWidth, 6*FontHeight);
{$IFDEF Ukrainian}
    Insert(New(PLabel, Init(R, '~Н~овый текст', Control)));
{$ELSE}
  {$IFDEF Russian}
    Insert(New(PLabel, Init(R, '~Н~овый текст', Control)));
  {$ELSE} {English}
    Insert(New(PLabel, Init(R, '~N~ew text', Control)));
  {$ENDIF}
{$ENDIF}
    R.Assign(34*CharWidth, 6*FontHeight, 37*CharWidth, 7*FontHeight);
    Insert(New(PHistory, Init(R, PInputLine(Control), 11)));

    R.Assign(3*CharWidth, 8*FontHeight, 37*CharWidth, 12*FontHeight);
{$IFDEF Ukrainian}
    Insert(New(PCheckBoxes, Init(R,
      NewSItem('~У~читывать регистр',
      NewSItem('~Ц~елые слова',
      NewSItem('~З~апрос замены',
      NewSItem('З~а~менить все', nil)))))));

    {$IFDEF UseStandardBitmaps}
    R.Assign(17*CharWidth, 13*FontHeight, 27*CharWidth, 15*FontHeight);
    Insert(MakeOKButton(R, cmOk, bfDefault));
    R.Assign(28*CharWidth, 13*FontHeight, 38*CharWidth, 15*FontHeight);
    Insert(MakeCancelButton(R, cmCancel, bfNormal));
    {$ELSE}
    R.Assign(17*CharWidth, 13*FontHeight, 27*CharWidth, 15*FontHeight);
    Insert(New(PButton, Init(R, '~В~вод', cmOk, bfDefault)));
    R.Assign(28*CharWidth, 13*FontHeight, 38*CharWidth, 15*FontHeight);
    Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
    {$ENDIF}
{$ELSE}
  {$IFDEF Russian}
    Insert(New(PCheckBoxes, Init(R,
      NewSItem('~У~читывать регистр',
      NewSItem('~Ц~елые слова',
      NewSItem('~З~апрос замены',
      NewSItem('З~а~менить все', nil)))))));

    {$IFDEF UseStandardBitmaps}
    R.Assign(17*CharWidth, 13*FontHeight, 27*CharWidth, 15*FontHeight);
    Insert(MakeOKButton(R, cmOk, bfDefault));
    R.Assign(28*CharWidth, 13*FontHeight, 38*CharWidth, 15*FontHeight);
    Insert(MakeCancelButton(R, cmCancel, bfNormal));
    {$ELSE}
    R.Assign(17*CharWidth, 13*FontHeight, 27*CharWidth, 15*FontHeight);
    Insert(New(PButton, Init(R, '~В~вод', cmOk, bfDefault)));
    R.Assign(28*CharWidth, 13*FontHeight, 38*CharWidth, 15*FontHeight);
    Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
    {$ENDIF}
  {$ELSE} {English}
    Insert(New(PCheckBoxes, Init(R,
      NewSItem('~C~ase sensitive',
      NewSItem('~W~hole words only',
      NewSItem('~P~rompt on replace',
      NewSItem('~R~eplace all', nil)))))));

    {$IFDEF UseStandardBitmaps}
    R.Assign(17*CharWidth, 13*FontHeight, 27*CharWidth, 15*FontHeight);
    Insert(MakeOKButton(R, cmOk, bfDefault));
    R.Assign(28*CharWidth, 13*FontHeight, 38*CharWidth, 15*FontHeight);
    Insert(MakeCancelButton(R, cmCancel, bfNormal));
    {$ELSE}
    R.Assign(17*CharWidth, 13*FontHeight, 27*CharWidth, 15*FontHeight);
    Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    R.Assign(28*CharWidth, 13*FontHeight, 38*CharWidth, 15*FontHeight);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

    SelectNext(False);
  end;
  CreateReplaceDialog := D;
end;

function StdEditorDialog(Dialog: Integer; Info: Pointer): Word {SHIM};
var
  R: TRect;
  T: TPoint;
  CharWidth, FontHeight: integer;
begin
  SelectFontCaps(GlobalFont);
  CharWidth := GetCharWidth;
  FontHeight := GetHeight;
{$IFDEF Ukrainian}
  case Dialog of
    edOutOfMemory:
      StdEditorDialog := MessageBox('Недостаточно памяти для этой операции.',
        nil, mfError + mfOkButton);
    edReadError:
      StdEditorDialog := MessageBox('Ошибка при чтении файла %s.',
        @Info, mfError + mfOkButton);
    edWriteError:
      StdEditorDialog := MessageBox('Ошибка при записи файла %s.',
        @Info, mfError + mfOkButton);
    edCreateError:
      StdEditorDialog := MessageBox('Ошибка при создании файла %s.',
        @Info, mfError + mfOkButton);
    edSaveModify:
      StdEditorDialog := MessageBox('%s изменен. Сохранить?',
        @Info, mfInformation + mfYesNoCancel);
    edSaveUntitled:
      StdEditorDialog := MessageBox('Сохранить безымянный файл?',
        nil, mfInformation + mfYesNoCancel);
    edSaveAs:
      StdEditorDialog :=
        Application^.ExecuteDialog(New(PFileDialog, Init('*.*',
        'Сохранить под именем', '~И~мя', fdOkButton, 101)), Info);
    edFind:
      StdEditorDialog :=
        Application^.ExecuteDialog(CreateFindDialog, Info);
    edSearchFailed:
      StdEditorDialog := MessageBox('Искомая строка не найдена.',
        nil, mfError + mfOkButton);
    edReplace:
      StdEditorDialog :=
        Application^.ExecuteDialog(CreateReplaceDialog, Info);
    edReplacePrompt:
      begin
        { Avoid placing the dialog on the same line as the cursor }
        R.Assign(0, 1*FontHeight, 40*CharWidth, 8*FontHeight);
        R.Move((Desktop^.Size.X - R.B.X) div 2, 0);
        Desktop^.MakeGlobal(R.B, T);
        Inc(T.Y, FontHeight);
        if TPoint(Info).Y <= T.Y then
          R.Move(0, Desktop^.Size.Y - R.B.Y - 2*FontHeight);
        StdEditorDialog := MessageBoxRect(R, 'Заменить?',
          nil, mfYesNoCancel + mfInformation);
      end;
  end;
{$ELSE}
  {$IFDEF Russian}
  case Dialog of
    edOutOfMemory:
      StdEditorDialog := MessageBox('Недостаточно памяти для этой операции.',
        nil, mfError + mfOkButton);
    edReadError:
      StdEditorDialog := MessageBox('Ошибка при чтении файла %s.',
        @Info, mfError + mfOkButton);
    edWriteError:
      StdEditorDialog := MessageBox('Ошибка при записи файла %s.',
        @Info, mfError + mfOkButton);
    edCreateError:
      StdEditorDialog := MessageBox('Ошибка при создании файла %s.',
        @Info, mfError + mfOkButton);
    edSaveModify:
      StdEditorDialog := MessageBox('%s изменен. Сохранить?',
        @Info, mfInformation + mfYesNoCancel);
    edSaveUntitled:
      StdEditorDialog := MessageBox('Сохранить безымянный файл?',
        nil, mfInformation + mfYesNoCancel);
    edSaveAs:
      StdEditorDialog :=
        Application^.ExecuteDialog(New(PFileDialog, Init('*.*',
        'Сохранить под именем', '~И~мя', fdOkButton, 101)), Info);
    edFind:
      StdEditorDialog :=
        Application^.ExecuteDialog(CreateFindDialog, Info);
    edSearchFailed:
      StdEditorDialog := MessageBox('Искомая строка не найдена.',
        nil, mfError + mfOkButton);
    edReplace:
      StdEditorDialog :=
        Application^.ExecuteDialog(CreateReplaceDialog, Info);
    edReplacePrompt:
      begin
        { Avoid placing the dialog on the same line as the cursor }
        R.Assign(0, 1*FontHeight, 40*CharWidth, 8*FontHeight);
        R.Move((Desktop^.Size.X - R.B.X) div 2, 0);
        Desktop^.MakeGlobal(R.B, T);
        Inc(T.Y, FontHeight);
        if TPoint(Info).Y <= T.Y then
          R.Move(0, Desktop^.Size.Y - R.B.Y - 2*FontHeight);
        StdEditorDialog := MessageBoxRect(R, 'Заменить?',
          nil, mfYesNoCancel + mfInformation);
      end;
  end;
  {$ELSE} {English}
  case Dialog of
    edOutOfMemory:
      StdEditorDialog := MessageBox('Not enough memory for this operation.',
        nil, mfError + mfOkButton);
    edReadError:
      StdEditorDialog := MessageBox('Error reading file %s.',
        @Info, mfError + mfOkButton);
    edWriteError:
      StdEditorDialog := MessageBox('Error writing file %s.',
        @Info, mfError + mfOkButton);
    edCreateError:
      StdEditorDialog := MessageBox('Error creating file %s.',
        @Info, mfError + mfOkButton);
    edSaveModify:
      StdEditorDialog := MessageBox('%s has been modified. Save?',
        @Info, mfInformation + mfYesNoCancel);
    edSaveUntitled:
      StdEditorDialog := MessageBox('Save untitled file?',
        nil, mfInformation + mfYesNoCancel);
    edSaveAs:
      StdEditorDialog :=
        Application^.ExecuteDialog(New(PFileDialog, Init('*.*',
        'Save file as', '~N~ame', fdOkButton, 101)), Info);
    edFind:
      StdEditorDialog :=
        Application^.ExecuteDialog(CreateFindDialog, Info);
    edSearchFailed:
      StdEditorDialog := MessageBox('Search string not found.',
        nil, mfError + mfOkButton);
    edReplace:
      StdEditorDialog :=
        Application^.ExecuteDialog(CreateReplaceDialog, Info);
    edReplacePrompt:
      begin
        { Avoid placing the dialog on the same line as the cursor }
        R.Assign(0, 1*FontHeight, 40*CharWidth, 8*FontHeight);
        R.Move((Desktop^.Size.X - R.B.X) div 2, 0);
        Desktop^.MakeGlobal(R.B, T);
        Inc(T.Y, FontHeight);
        if TPoint(Info).Y <= T.Y then
          R.Move(0, Desktop^.Size.Y - R.B.Y - 2*FontHeight);
        StdEditorDialog := MessageBoxRect(R, 'Replace this occurence?',
          nil, mfYesNoCancel + mfInformation);
      end;
  end;
  {$ENDIF}
{$ENDIF}
end;

function Min(X, Y: Integer): Integer; near; assembler;
asm
        MOV     AX,X
        CMP     AX,Y
        JLE     @@1
        MOV     AX,Y
@@1:
end;

function Max(X, Y: Integer): Integer; near; assembler;
asm
        MOV     AX,X
        CMP     AX,Y
        JGE     @@1
        MOV     AX,Y
@@1:
end;

function MinWord(X, Y: Word): Word; near; assembler;
asm
        MOV     AX,X
        CMP     AX,Y
        JBE     @@1
        MOV     AX,Y
@@1:
end;

function MaxWord(X, Y: Word): Word; near; assembler;
asm
        MOV     AX,X
        CMP     AX,Y
        JAE     @@1
        MOV     AX,Y
@@1:
end;

function CountLines(var Buf; Count: Word): Integer; near; assembler;
asm
        LES     DI,Buf
        MOV     CX,Count
        XOR     DX,DX
        MOV     AL,0DH
        CLD
@@1:    JCXZ    @@2
        REPNE   SCASB
        JNE     @@2
        INC     DX
        JMP     @@1
@@2:    MOV     AX,DX
end;

function ScanKeyMap(KeyMap: Pointer; KeyCode: Word): Word; near; assembler;
asm
        PUSH    DS
        LDS     SI,KeyMap
        MOV     DX,KeyCode
        CLD
        LODSW
        MOV     CX,AX
@@1:    LODSW
        MOV     BX,AX
        LODSW
        CMP     BL,DL
        JNE     @@3
        OR      BH,BH
        JE      @@4
        CMP     BH,DH
        JE      @@4
@@3:    LOOP    @@1
        XOR     AX,AX
@@4:    POP     DS
end;

function Scan(var Block; Size: Word; Str: String): Word; near; assembler;
asm
        PUSH    DS
        LES     DI,Block
        LDS     SI,Str
        MOV     CX,Size
        JCXZ    @@3
        CLD
        LODSB
        CMP     AL,1
        JB      @@5
        JA      @@1
        LODSB
        REPNE   SCASB
        JNE     @@3
        JMP     @@5
@@1:    XOR     AH,AH
        MOV     BX,AX
        DEC     BX
        MOV     DX,CX
        SUB     DX,AX
        JB      @@3
        LODSB
        INC     DX
        INC     DX
@@2:    DEC     DX
        MOV     CX,DX
        REPNE   SCASB
        JNE     @@3
        MOV     DX,CX
        MOV     CX,BX
        REP     CMPSB
        JE      @@4
        SUB     CX,BX
        ADD     SI,CX
        ADD     DI,CX
        INC     DI
        OR      DX,DX
        JNE     @@2
@@3:    XOR     AX,AX
        JMP     @@6
@@4:    SUB     DI,BX
@@5:    MOV     AX,DI
        SUB     AX,WORD PTR Block
@@6:    DEC     AX
        POP     DS
end;

function IScan(var Block; Size: Word; Str: String): Word; near; assembler;
var
  S: String;
asm
        PUSH    DS
        MOV     AX,SS
        MOV     ES,AX
        LEA     DI,S
        LDS     SI,Str
        XOR     AH,AH
        LODSB
        STOSB
        MOV     CX,AX
        MOV     BX,AX
        JCXZ    @@9
@@1:    LODSB
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
@@2:    STOSB
        LOOP    @@1
        SUB     DI,BX
        LDS     SI,Block
        MOV     CX,Size
        JCXZ    @@8
        CLD
        SUB     CX,BX
        JB      @@8
        INC     CX
@@4:    MOV     AH,ES:[DI]
        AND     AH,$DF
@@5:    LODSB
        AND     AL,$DF
        CMP     AL,AH
        LOOPNE  @@5
        JNE     @@8
        DEC     SI
        MOV     DX,CX
        MOV     CX,BX
@@6:    REPE    CMPSB
        JE      @@10
        MOV     AL,DS:[SI-1]
        CMP     AL,'a'
        JB      @@7
        CMP     AL,'z'
        JA      @@7
        SUB     AL,20H
@@7:    CMP     AL,ES:[DI-1]
        JE      @@6
        SUB     CX,BX
        ADD     SI,CX
        ADD     DI,CX
        INC     SI
        MOV     CX,DX
        OR      CX,CX
        JNE     @@4
@@8:    XOR     AX,AX
        JMP     @@11
@@9:    MOV     AX, 1
        JMP     @@11
@@10:   SUB     SI,BX
        MOV     AX,SI
        SUB     AX,WORD PTR Block
        INC     AX
@@11:   DEC     AX
        POP     DS
end;

{ TIndicator }

constructor TIndicator.Init(var Bounds: TRect);
var
  R: TRect;
begin
  inherited Init(Bounds);
  GrowMode := gfGrowLoY + gfGrowHiY;
end;

procedure TIndicator.Draw {SHIM/DK};
var
  L: array[0..1] of Longint;
  S: String[15];
begin
  L[0] := Location.Y + 1;
  L[1] := Location.X + 1;
  FormatStr(S, ' %d:%d ', L);
  if Modified then S := '*' + S;
  Bar(0, 0, Size.X, Size.Y, GetColor(2));
  Rectangle(0, 0, Pred(Size.X), Pred(Size.Y), 1, 0, 15);{DK}
  WrStr(1, 1, S, GetColor(1));
end;

function TIndicator.GetPalette: PPalette;
const
  P: string[Length(CIndicator)] = CIndicator;
begin
  GetPalette := @P;
end;

procedure TIndicator.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState, Enable);
  if AState = sfDragging then DrawView;
end;

procedure TIndicator.SetValue(ALocation: TPoint; AModified: Boolean);
begin
  if (Longint(Location) <> Longint(ALocation)) or
    (Modified <> AModified) then
  begin
    Location := ALocation;
    Modified := AModified;
    DrawView;
  end;
end;

{ TEditor }

constructor TEditor.Init(var Bounds: TRect;
  AHScrollBar, AVScrollBar: PScrollBar;
  AIndicator: PIndicator; ABufSize: Word);
var
  W : Integer;
begin
  inherited Init(Bounds);
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options := Options or ofSelectable;
  EventMask := evMouseDown + evKeyDown + evCommand + evBroadcast;
  ShowCursor;
  HScrollBar := AHScrollBar;
  VScrollBar := AVScrollBar;
  Indicator := AIndicator;
  BufSize := ABufSize;
  CanUndo := True;
  InitBuffer;
  if Buffer <> nil then IsValid := True else
  begin
    EditorDialog(edOutOfMemory, nil);
    BufSize := 0;
  end;
  SetBufLen(0);
  SetFont(Font);                       {OOA}
  Step.X:= CharWidth;                            {OOA}
  Step.Y:= FontHeight;                           {OOA}
  PageSize.X:= (Size.X + Step.X - 1) div Step.X; {OOA}
  PageSize.Y:= (Size.Y + Step.Y - 1) div Step.Y; {OOA}
  PageTail.X:= (Size.X + Step.X - 1) mod Step.X; {DK}
  PageTail.Y:= (Size.Y + Step.Y - 1) mod Step.Y; {DK}
  if PageTail.X <> 0 then PageTail.X := 1;       {DK}
  if PageTail.Y <> 0 then PageTail.Y := 1;       {DK}
  RestoreFont;                                   {OOA}
  CursorShape := @InputMouseCursor;              {DK}
end;

constructor TEditor.Load(var S: TStream);
begin
  inherited Load(S);
  GetPeerViewPtr(S, HScrollBar);
  GetPeerViewPtr(S, VScrollBar);
  GetPeerViewPtr(S, Indicator);
  S.Read(BufSize, SizeOf(Word));
  S.Read(CanUndo, SizeOf(Boolean));
  InitBuffer;
  if Buffer <> nil then IsValid := True else
  begin
    EditorDialog(edOutOfMemory, nil);
    BufSize := 0;
  end;
  Lock;
  SetBufLen(0);
end;

destructor TEditor.Done;
begin
  DoneBuffer;
  inherited Done;
end;

function TEditor.BufChar(P: Word): Char; assembler;
asm
        LES     DI,Self
        MOV     BX,P
        CMP     BX,ES:[DI].TEditor.CurPtr
        JB      @@1
        ADD     BX,ES:[DI].TEditor.GapLen
@@1:    LES     DI,ES:[DI].TEditor.Buffer
        MOV     AL,ES:[DI+BX]
end;

function TEditor.BufPtr(P: Word): Word; assembler;
asm
        LES     DI,Self
        MOV     AX,P
        CMP     AX,ES:[DI].TEditor.CurPtr
        JB      @@1
        ADD     AX,ES:[DI].TEditor.GapLen
@@1:
end;

procedure TEditor.ChangeBounds(var Bounds: TRect);
begin
  SetBounds(Bounds);
  PageSize.X:= (Size.X + Step.X - 1) div Step.X; {OOA}
  PageSize.Y:= (Size.Y + Step.Y - 1) div Step.Y; {OOA}
  PageTail.X:= (Size.X + Step.X - 1) mod Step.X; {DK}
  PageTail.Y:= (Size.Y + Step.Y - 1) mod Step.Y; {DK}
  if PageTail.X <> 0 then PageTail.X := 1;       {DK}
  if PageTail.Y <> 0 then PageTail.Y := 1;       {DK}
  Delta.X := Max(0, Min(Delta.X, Limit.X - PageSize.X));
  Delta.Y := Max(0, Min(Delta.Y, Limit.Y - PageSize.Y));
  Update(ufView);
end;

function TEditor.CharPos(P, Target: Word): Integer;
var
  Pos: Integer;
begin
  Pos := 0;
  while P < Target do
  begin
    if BufChar(P) = #9 then Pos := Pos or 7;
    Inc(Pos);
    Inc(P);
  end;
  CharPos := Pos;
end;

function TEditor.CharPtr(P: Word; Target: Integer): Word;
var
  Pos: Integer;
begin
  Pos := 0;
  while (Pos < Target) and (P < BufLen) and (BufChar(P) <> #13) do
  begin
    if BufChar(P) = #9 then Pos := Pos or 7;
    Inc(Pos);
    Inc(P);
  end;
  if Pos > Target then Dec(P);
  CharPtr := P;
end;

function TEditor.ClipCopy: Boolean;
begin
  ClipCopy := False;
  if (Clipboard <> nil) and (Clipboard <> @Self) then
  begin
    ClipCopy := Clipboard^.InsertFrom(@Self);
    Selecting := False;
    Update(ufUpdate);
  end;
end;

procedure TEditor.ClipCut;
begin
  if ClipCopy then DeleteSelect;
end;

procedure TEditor.ClipPaste;
begin
  if (Clipboard <> nil) and (Clipboard <> @Self) then InsertFrom(Clipboard);
end;

procedure TEditor.ConvertEvent(var Event: TEvent);
var
  {ShiftState: Byte absolute $40:$17;} {SHIM}
  Key: Word;
begin
  if Event.What = evKeyDown then
  begin
    if (Mem[Seg0040:$17] {ShiftState} and $03 <> 0) and {SHIM}
      (Event.ScanCode >= $47) and (Event.ScanCode <= $51) then
      Event.CharCode := #0;
    Key := Event.KeyCode;
    if KeyState <> 0 then
    begin
      if (Lo(Key) >= $01) and (Lo(Key) <= $1A) then Inc(Key, $40);
      if (Lo(Key) >= $61) and (Lo(Key) <= $7A) then Dec(Key, $20);
    end;
    Key := ScanKeyMap(KeyMap[KeyState], Key);
    KeyState := 0;
    if Key <> 0 then
      if Hi(Key) = $FF then
      begin
        KeyState := Lo(Key);
        ClearEvent(Event);
      end else
      begin
        Event.What := evCommand;
        Event.Command := Key;
      end;
  end;
end;

function TEditor.CursorVisible: Boolean;
begin
  CursorVisible := (CurPos.Y >= Delta.Y) and (CurPos.Y < Delta.Y + PageSize.Y - PageTail.Y);
end;

procedure TEditor.DeleteRange(StartPtr, EndPtr: Word; DelSelect: Boolean);
begin
  if HasSelection and DelSelect then DeleteSelect else
  begin
    SetSelect(CurPtr, EndPtr, True);
    DeleteSelect;
    SetSelect(StartPtr, CurPtr, False);
    DeleteSelect;
  end;
end;

procedure TEditor.DeleteSelect;
begin
  InsertText(nil, 0, False);
end;

procedure TEditor.DoneBuffer;
begin
  if Buffer <> nil then
  begin
    FreeMem(Buffer, BufSize);
    Buffer := nil;
  end;
end;

procedure TEditor.DoSearchReplace;
var
  I: Word;
  C: TPoint;
begin
  repeat
    I := cmCancel;
    if not Search(FindStr, EditorFlags) then
    begin
      if EditorFlags and (efReplaceAll + efDoReplace) <>
          (efReplaceAll + efDoReplace) then
        EditorDialog(edSearchFailed, nil)
    end
    else if EditorFlags and efDoReplace <> 0 then
    begin
      I := cmYes;
      if EditorFlags and efPromptOnReplace <> 0 then
      begin
        MakeGlobal(Cursor, C);
        I := EditorDialog(edReplacePrompt, Pointer(C));
      end;
      if I = cmYes then
      begin
        Lock;
        InsertText(@ReplaceStr[1], Length(ReplaceStr), False);
        TrackCursor(False);
        Unlock;
      end;
    end;
  until (I = cmCancel) or (EditorFlags and efReplaceAll = 0);
end;

procedure TEditor.DoUpdate;
Var
  X : Integer;
begin
  if UpdateFlags <> 0 then
  begin
    {SetCursor((CurPos.X - Delta.X) * Step.X, (CurPos.Y - Delta.Y) * Step.Y);}
    X := SubStrWidth(CurPos.X, CurPos.Y);
    SetCursor(X, (CurPos.Y - Delta.Y) * Step.Y);  {DK}
    if X + Step.X > Size.X then ScrollTo(Delta.X + 5, Delta.Y); {DK}
    if UpdateFlags and ufView <> 0 then DrawView else
      if UpdateFlags and ufLine <> 0 then
      begin
        {DrawLines(CurPos.Y - Delta.Y, 1, LineStart(CurPtr)); OOA}
        DrawUpdate:= True;
        DrawView;
        DrawUpdate:= False;
      end;
    if HScrollBar <> nil then
      HScrollBar^.SetParams(Delta.X, 0, Limit.X - PageSize.X + PageTail.X, PageSize.X div 2, 1);
    if VScrollBar <> nil then
      VScrollBar^.SetParams(Delta.Y, 0, Limit.Y - PageSize.Y + PageTail.Y, PageSize.Y - 1, 1);
    if Indicator <> nil then Indicator^.SetValue(CurPos, Modified);
    if State and sfActive <> 0 then UpdateCommands;
    UpdateFlags := 0;
  end;
end;

procedure TEditor.Draw;
var
  Color: array [0..1] of byte; Clr: word absolute Color;
  Back : array [0..1] of byte; Bck: word absolute Back;

  function GetStr(var Buf; Start, MaxLen: word; var Str: string): byte; assembler;
  asm
    push   ds
    lds    si, Buf
    add    si, Start
    add    si, Start
    les    di, Str
    mov    bx, di
    xor    ax, ax
    stosb
    mov    dx, ds:[si]
    mov    cx, MaxLen
    sub    cx, Start
    jle    @@2
  @@1:
    lodsw
    cmp    ah, dh
    jne    @@2
    stosb
    inc    byte ptr es:[bx]
    loop   @@1
  @@2:
    pop    ds
    mov    al, dh
  end;

  procedure WriteBuf(X, Y, W, H: Integer; var Buf);
  var
    AttrBuf: array[0..128] of record C: Char; A: Byte; end absolute Buf;
    S: String;
    Attr: byte;
    xx, yy : integer;
    XOffs : integer; {DK}

  begin
    Y:= Y * Step.Y;
    yy := FontHeight;
    S[0] := #0;
    XOffs := 0;
    xx := 0;
    while X < W do begin
      Inc(xx, XOffs);
      Attr := GetStr(Buf, X, W, S);
      XOffs := FontWidth(S);
      Bar(xx, Y, xx + XOffs, Y + yy, Back[Attr]);
      Inc(X, length(S));
      if S[0] > #0 then repeat {DK}
        if S[Byte(S[0])] = ' ' then Dec(S[0]) else begin
          if S[0] <> #0 then WrStr(xx, Y, S, Color[Attr]);
          Break;
        end;
      until false;
      {WrStr(xx, Y, S, Color[Attr]); <- too slow. DK}
    end;
    if xx + XOffs < Size.X then Bar(xx + XOffs, Y, Size.X, Y + yy, Back[Attr]);
  end;

var
  B: array[0..MaxLineLength - 0{DK}] of Word;
  Y, Count: Integer;
  LinePtr: Word;
begin
  if DrawUpdate then
  begin
    Y:= CurPos.Y - Delta.Y;
    Count:= 1;
    LinePtr:= LineStart(CurPtr);
  end else
  begin
    if DrawLine <> Delta.Y then
    begin
      DrawPtr := LineMove(DrawPtr, Delta.Y - DrawLine);
      DrawLine := Delta.Y;
    end;
    Y:= 0;
    Count:= PageSize.Y;
    LinePtr:= DrawPtr;
  end;

  Clr := GetColor($0301);
  Bck := GetColor($0402);
  Color[0] := MonoColor(Color[0], 0);
  Back [0] := MonoColor(Back [0], 1);
  Color[1] := MonoColor(Color[1], 1);
  Back [1] := MonoColor(Back [1], 0);
  while Count > 0 do begin
    FormatLine(B, LinePtr, MaxLineLength - 1, $0100); {DK}
    WriteBuf(0, Y, MaxLineLength - 1, 1, B[Delta.X]); {DK}
    LinePtr := NextLine(LinePtr);
    Inc(Y);
    Dec(Count);
  end;
end;
{
procedure TEditor.DrawLines(Y, Count: Integer; LinePtr: Word);
var
  Color: Word;
  B: array[0..MaxLineLength - 1] of Word;
begin
  Color := GetColor($0201);
  Bar(0 * Step.X, Y * Step.Y, PageSize.X, Count * Step.Y, Attr2Back(Color));
  while Count > 0 do
  begin
    FormatLine(B, LinePtr, Delta.X + PageSize.X, Color);
    WriteBuf(0, Y, PageSize.X, 1, B[Delta.X]);
    LinePtr := NextLine(LinePtr);
    Inc(Y);
    Dec(Count);
  end;
end;
}
procedure TEditor.Find;
var
  FindRec: TFindDialogRec;
begin
  with FindRec do
  begin
    Find := FindStr;
    Options := EditorFlags;
    if EditorDialog(edFind, @FindRec) <> cmCancel then
    begin
      FindStr := Find;
      EditorFlags := Options and not efDoReplace;
      DoSearchReplace;
    end;
  end;
end;

procedure TEditor.FormatLine(var DrawBuf; LinePtr: Word;
  Width: Integer; Colors: Word); assembler;
asm
        PUSH    DS
        LDS     BX,Self
        LES     DI,DrawBuf
        MOV     SI,LinePtr
        XOR     DX,DX
        CLD
        MOV     AH,Colors.Byte[0]
        MOV     CX,DS:[BX].TEditor.SelStart
        CALL    @@10
        MOV     AH,Colors.Byte[1]
        MOV     CX,DS:[BX].TEditor.CurPtr
        CALL    @@10
        ADD     SI,DS:[BX].TEditor.GapLen
        MOV     CX,DS:[BX].TEditor.SelEnd
        ADD     CX,DS:[BX].TEditor.GapLen
        CALL    @@10
        MOV     AH,Colors.Byte[0]
        MOV     CX,DS:[BX].TEditor.BufSize
        CALL    @@10
        JMP     @@31
@@10:   SUB     CX,SI
        JA      @@11
        RETN
@@11:   LDS     BX,DS:[BX].TEditor.Buffer
        ADD     SI,BX
        MOV     BX,Width
@@12:   LODSB
        CMP     AL,' '
        JB      @@20
@@13:   STOSW
        INC     DX
@@14:   CMP     DX,BX
        JAE     @@30
        LOOP    @@12
        LDS     BX,Self
        SUB     SI,DS:[BX].TEditor.Buffer.Word[0]
        RETN
@@20:   CMP     AL,0DH
        JE      @@30
        CMP     AL,09H
        JNE     @@13
        MOV     AL,' '
@@21:   STOSW
        INC     DX
        TEST    DL,7
        JNE     @@21
        JMP     @@14
@@30:   POP     CX
@@31:   MOV     AL,' '
        MOV     CX,Width
        SUB     CX,DX
        JBE     @@32
        REP     STOSW
@@32:   POP     DS
end;

function TEditor.GetMousePtr(Mouse: TPoint): Word;
begin
  MakeLocal(Mouse, Mouse);
  {Mouse.X:= Mouse.X div Step.X; {OOA}
  Mouse.Y := Mouse.Y div Step.Y; {OOA}
  Mouse.X := SubStrLen(Mouse.X, Mouse.Y); {DK}
  Mouse.X := Max(0, Min(Mouse.X, PageSize.X - 1));
  Mouse.Y := Max(0, Min(Mouse.Y, PageSize.Y - 1));
  GetMousePtr := CharPtr(LineMove(DrawPtr, Mouse.Y + Delta.Y - DrawLine),
    Mouse.X + Delta.X);
end;

function TEditor.GetPalette: PPalette;
const
  P: String[Length(CEditor)] = CEditor;
begin
  GetPalette := @P;
end;

procedure TEditor.HandleEvent(var Event: TEvent);
var
  {ShiftState: Byte absolute $40:$17;} {SHIM}
  CenterCursor: Boolean;
  SelectMode: Byte;
  I: Integer;
  NewPtr: Word;
  D, Mouse: TPoint;

procedure CheckScrollBar(P: PScrollBar; var D: Integer);
begin
  if (Event.InfoPtr = P) and (P^.Value <> D) then
  begin
    D := P^.Value;
    Update(ufView);
  end;
end;

begin
  inherited HandleEvent(Event);
  ConvertEvent(Event);
  CenterCursor := not CursorVisible;
  SelectMode := 0;
  if Selecting or (Mem[Seg0040:$17]{ShiftState} and $03 <> 0) then SelectMode := smExtend; {SHIM}
  case Event.What of
    evMouseDown:
      begin
        if Event.Double then SelectMode := SelectMode or smDouble;
        repeat
          if Event.What = evMouseAuto then
          begin
            MakeLocal(Event.Where, Mouse);
            D := Delta;
            if Mouse.X < 0 then Dec(D.X);
            if Mouse.X >= Size.X then Inc(D.X);
            if Mouse.Y < 0 then Dec(D.Y);
            if Mouse.Y >= Size.Y then Inc(D.Y);
            ScrollTo(D.X, D.Y);
          end;
          SetCurPtr(GetMousePtr(Event.Where), SelectMode);
          SelectMode := SelectMode or smExtend;
        until not MouseEvent(Event, evMouseMove + evMouseAuto);
      end;
    evKeyDown:
      case Event.CharCode of
        #9,#32..#255:
          begin
            Lock;
            if Overwrite and not HasSelection then
              if CurPtr <> LineEnd(CurPtr) then SelEnd := NextChar(CurPtr);
            InsertText(@Event.CharCode, 1, False);
            TrackCursor(CenterCursor);
            Unlock;
          end;
      else
        Exit;
      end;
    evCommand:
      case Event.Command of
        cmFind: Find;
        cmReplace: Replace;
        cmSearchAgain: DoSearchReplace;
      else
        begin
          Lock;
          case Event.Command of
            cmCut: ClipCut;
            cmCopy: ClipCopy;
            cmPaste: ClipPaste;
            cmUndo: Undo;
            cmClear: DeleteSelect;
            cmCharLeft: SetCurPtr(PrevChar(CurPtr), SelectMode);
            cmCharRight: SetCurPtr(NextChar(CurPtr), SelectMode);
            cmWordLeft: SetCurPtr(PrevWord(CurPtr), SelectMode);
            cmWordRight: SetCurPtr(NextWord(CurPtr), SelectMode);
            cmLineStart: SetCurPtr(LineStart(CurPtr), SelectMode);
            cmLineEnd: SetCurPtr(LineEnd(CurPtr), SelectMode);
            cmLineUp: SetCurPtr(LineMove(CurPtr, -1), SelectMode);
            cmLineDown: SetCurPtr(LineMove(CurPtr, 1), SelectMode);
            cmPageUp: SetCurPtr(LineMove(CurPtr, -(PageSize.Y - 1)), SelectMode);
            cmPageDown: SetCurPtr(LineMove(CurPtr, PageSize.Y - 1), SelectMode);
            cmTextStart: SetCurPtr(0, SelectMode);
            cmTextEnd: SetCurPtr(BufLen, SelectMode);
            cmNewLine: NewLine;
            cmBackSpace: DeleteRange(PrevChar(CurPtr), CurPtr, True);
            cmDelChar: DeleteRange(CurPtr, NextChar(CurPtr), True);
            cmDelWord: DeleteRange(CurPtr, NextWord(CurPtr), False);
            cmDelStart: DeleteRange(LineStart(CurPtr), CurPtr, False);
            cmDelEnd: DeleteRange(CurPtr, LineEnd(CurPtr), False);
            cmDelLine: DeleteRange(LineStart(CurPtr), NextLine(CurPtr), False);
            cmInsMode: ToggleInsMode;
            cmStartSelect: StartSelect;
            cmHideSelect: HideSelect;
            cmIndentMode: AutoIndent := not AutoIndent;
          else
            Unlock;
            Exit;
          end;
          TrackCursor(CenterCursor);
          Unlock;
        end;
      end;
    evBroadcast:
      case Event.Command of
        cmScrollBarChanged:
          if (Event.InfoPtr = HScrollBar) or
            (Event.InfoPtr = VScrollBar) then
          begin
            CheckScrollBar(HScrollBar, Delta.X);
            CheckScrollBar(VScrollBar, Delta.Y);
          end
          else
            Exit;
      else
        Exit;
      end;
  end;
  ClearEvent(Event);
end;

function TEditor.HasSelection: Boolean;
begin
  HasSelection := SelStart <> SelEnd;
end;

procedure TEditor.HideSelect;
begin
  Selecting := False;
  SetSelect(CurPtr, CurPtr, False);
end;

procedure TEditor.InitBuffer;
begin
  Buffer := MemAlloc(BufSize);
end;

function TEditor.InsertBuffer(var P: PEditBuffer; Offset, Length: Word;
  AllowUndo, SelectText: Boolean): Boolean;
var
  SelLen, DelLen, SelLines, Lines: Word;
  NewSize: Longint;
begin
  InsertBuffer := True;
  Selecting := False;
  SelLen := SelEnd - SelStart;
  if (SelLen = 0) and (Length = 0) then Exit;
  DelLen := 0;
  if AllowUndo then
    if CurPtr = SelStart then DelLen := SelLen else
      if SelLen > InsCount then DelLen := SelLen - InsCount;
  NewSize := Longint(BufLen + DelCount - SelLen + DelLen) + Length;
  if NewSize > BufLen + DelCount then
    if (NewSize > $FFF0) or not SetBufSize(NewSize) then
    begin
      EditorDialog(edOutOfMemory, nil);
      InsertBuffer := False;
      SelEnd := SelStart;
      Exit;
    end;
  SelLines := CountLines(Buffer^[BufPtr(SelStart)], SelLen);
  if CurPtr = SelEnd then
  begin
    if AllowUndo then
    begin
      if DelLen > 0 then Move(Buffer^[SelStart],
        Buffer^[CurPtr + GapLen - DelCount - DelLen], DelLen);
      Dec(InsCount, SelLen - DelLen);
    end;
    CurPtr := SelStart;
    Dec(CurPos.Y, SelLines);
  end;
  if Delta.Y > CurPos.Y then
  begin
    Dec(Delta.Y, SelLines);
    if Delta.Y < CurPos.Y then Delta.Y := CurPos.Y;
  end;
  if Length > 0 then Move(P^[Offset], Buffer^[CurPtr], Length);
  Lines := CountLines(Buffer^[CurPtr], Length);
  Inc(CurPtr, Length);
  Inc(CurPos.Y, Lines);
  DrawLine := CurPos.Y;
  DrawPtr := LineStart(CurPtr);
  CurPos.X := CharPos(DrawPtr, CurPtr);
  if not SelectText then SelStart := CurPtr;
  SelEnd := CurPtr;
  Inc(BufLen, Integer(Length) - SelLen); {OOA}
  Dec(GapLen, Integer(Length) - SelLen); {OOA}
  if AllowUndo then
  begin
    Inc(DelCount, DelLen);
    Inc(InsCount, Length);
  end;
  Inc(Limit.Y, (Lines - SelLines));
  Delta.Y := Max(0, Min(Delta.Y, Limit.Y - PageSize.Y));
  if not IsClipboard then Modified := True;
  SetBufSize(BufLen + DelCount);
  if (SelLines = 0) and (Lines = 0) then Update(ufLine) else Update(ufView);
end;

function TEditor.InsertFrom(Editor: PEditor): Boolean;
begin
  InsertFrom := InsertBuffer(Editor^.Buffer,
    Editor^.BufPtr(Editor^.SelStart),
    Editor^.SelEnd - Editor^.SelStart, CanUndo, IsClipboard);
end;

function TEditor.InsertText(Text: Pointer; Length: Word;
  SelectText: Boolean): Boolean;
begin
  InsertText := InsertBuffer(PEditBuffer(Text),
    0, Length, CanUndo, SelectText);
end;

function TEditor.IsClipboard: Boolean;
begin
  IsClipboard := Clipboard = @Self;
end;

function TEditor.LineEnd(P: Word): Word; assembler;
asm
        PUSH    DS
        LDS     SI,Self
        LES     BX,DS:[SI].TEditor.Buffer
        MOV     DI,P
        MOV     AL,0DH
        CLD
        MOV     CX,DS:[SI].TEditor.CurPtr
        SUB     CX,DI
        JBE     @@1
        ADD     DI,BX
        REPNE   SCASB
        JE      @@2
        MOV     DI,DS:[SI].TEditor.CurPtr
@@1:    MOV     CX,DS:[SI].TEditor.BufLen
        SUB     CX,DI
        JCXZ    @@4
        ADD     BX,DS:[SI].TEditor.GapLen
        ADD     DI,BX
        REPNE   SCASB
        JNE     @@3
@@2:    DEC     DI
@@3:    SUB     DI,BX
@@4:    MOV     AX,DI
        POP     DS
end;

function TEditor.LineMove(P: Word; Count: Integer): Word;
var
  Pos: Integer;
  I: Word;
begin
  I := P;
  P := LineStart(P);
  Pos := CharPos(P, I);
  while Count <> 0 do
  begin
    I := P;
    if Count < 0 then
    begin
      P := PrevLine(P);
      Inc(Count);
    end else
    begin
      P := NextLine(P);
      Dec(Count);
    end;
  end;
  if P <> I then P := CharPtr(P, Pos);
  LineMove := P;
end;

function TEditor.LineStart(P: Word): Word; assembler;
asm
        PUSH    DS
        LDS     SI,Self
        LES     BX,DS:[SI].TEditor.Buffer
        MOV     DI,P
        MOV     AL,0DH
        STD
        MOV     CX,DI
        SUB     CX,DS:[SI].TEditor.CurPtr
        JBE     @@1
        ADD     BX,DS:[SI].TEditor.GapLen
        ADD     DI,BX
        DEC     DI
        REPNE   SCASB
        JE      @@2
        SUB     BX,DS:[SI].TEditor.GapLen
        MOV     DI,DS:[SI].TEditor.CurPtr
@@1:    MOV     CX,DI
        JCXZ    @@4
        ADD     DI,BX
        DEC     DI
        REPNE   SCASB
        JNE     @@3
@@2:    INC     DI
        INC     DI
        SUB     DI,BX
        CMP     DI,DS:[SI].TEditor.CurPtr
        JE      @@4
        CMP     DI,DS:[SI].TEditor.BufLen
        JE      @@4
        CMP     ES:[BX+DI].Byte,0AH
        JNE     @@4
        INC     DI
        JMP     @@4
@@3:    XOR     DI,DI
@@4:    MOV     AX,DI
        POP     DS
end;

procedure TEditor.Lock;
begin
  Inc(LockCount);
end;

procedure TEditor.NewLine;
const
  CrLf: array[1..2] of Char = #13#10;
var
  I, P: Word;
begin
  P := LineStart(CurPtr);
  I := P;
  while (I < CurPtr) and ((Buffer^[I] = ' ') or (Buffer^[I] = #9)) do Inc(I);
  InsertText(@CrLf, 2, False);
  if AutoIndent then InsertText(@Buffer^[P], I - P, False);
end;

function TEditor.NextChar(P: Word): Word; assembler;
asm
        PUSH    DS
        LDS     SI,Self
        MOV     DI,P
        CMP     DI,DS:[SI].TEditor.BufLen
        JE      @@2
        INC     DI
        CMP     DI,DS:[SI].TEditor.BufLen
        JE      @@2
        LES     BX,DS:[SI].TEditor.Buffer
        CMP     DI,DS:[SI].TEditor.CurPtr
        JB      @@1
        ADD     BX,DS:[SI].TEditor.GapLen
@@1:    CMP     ES:[BX+DI-1].Word,0A0DH
        JNE     @@2
        INC     DI
@@2:    MOV     AX,DI
        POP     DS
end;

function TEditor.NextLine(P: Word): Word;
begin
  NextLine := NextChar(LineEnd(P));
end;

function TEditor.NextWord(P: Word): Word;
begin
  while (P < BufLen) and (BufChar(P) in WordChars) do
    P := NextChar(P);
  while (P < BufLen) and not (BufChar(P) in WordChars) do
    P := NextChar(P);
  NextWord := P;
end;

function TEditor.PrevChar(P: Word): Word; assembler;
asm
        PUSH    DS
        LDS     SI,Self
        MOV     DI,P
        OR      DI,DI
        JE      @@2
        DEC     DI
        JE      @@2
        LES     BX,DS:[SI].TEditor.Buffer
        CMP     DI,DS:[SI].TEditor.CurPtr
        JB      @@1
        ADD     BX,DS:[SI].TEditor.GapLen
@@1:    CMP     ES:[BX+DI-1].Word,0A0DH
        JNE     @@2
        DEC     DI
@@2:    MOV     AX,DI
        POP     DS
end;

function TEditor.PrevLine(P: Word): Word;
begin
  PrevLine := LineStart(PrevChar(P));
end;

function TEditor.PrevWord(P: Word): Word;
begin
  while (P > 0) and not (BufChar(PrevChar(P)) in WordChars) do
    P := PrevChar(P);
  while (P > 0) and (BufChar(PrevChar(P)) in WordChars) do
    P := PrevChar(P);
  PrevWord := P;
end;

procedure TEditor.Replace;
var
  ReplaceRec: TReplaceDialogRec;
begin
  with ReplaceRec do
  begin
    Find := FindStr;
    Replace := ReplaceStr;
    Options := EditorFlags;
    if EditorDialog(edReplace, @ReplaceRec) <> cmCancel then
    begin
      FindStr := Find;
      ReplaceStr := Replace;
      EditorFlags := Options or efDoReplace;
      DoSearchReplace;
    end;
  end;
end;

procedure TEditor.ScrollTo(X, Y: Integer);
begin
  X := Max(0, Min(X, Limit.X - PageSize.X + PageTail.X));
  Y := Max(0, Min(Y, Limit.Y - PageSize.Y + PageTail.Y));
  if (X <> Delta.X) or (Y <> Delta.Y) then
  begin
    LastDelta := Delta;
    Delta.X := X;
    Delta.Y := Y;
    Update(ufView);
  end;
end;

function TEditor.Search(const FindStr: String; Opts: Word): Boolean;
var
  I, Pos: Word;
begin
  Search := False;
  Pos := CurPtr;
  repeat
    if Opts and efCaseSensitive <> 0 then
      I := Scan(Buffer^[BufPtr(Pos)], BufLen - Pos, FindStr)
    else I := IScan(Buffer^[BufPtr(Pos)], BufLen - Pos, FindStr);
    if (I <> sfSearchFailed) then
    begin
      Inc(I, Pos);
      if (Opts and efWholeWordsOnly = 0) or
         not (((I <> 0) and (BufChar(I - 1) in WordChars)) or
              ((I + Length(FindStr) <> BufLen) and
               (BufChar(I + Length(FindStr)) in WordChars))) then
      begin
        Lock;
        SetSelect(I, I + Length(FindStr), False);
        TrackCursor(not CursorVisible);
        Unlock;
        Search := True;
        Exit;
      end else Pos := I + 1;
    end;
  until I = sfSearchFailed;
end;

procedure TEditor.SetBufLen(Length: Word);
begin
  BufLen := Length;
  GapLen := BufSize - Length;
  SelStart := 0;
  SelEnd := 0;
  CurPtr := 0;
  Longint(CurPos) := 0;
  Longint(Delta) := 0;
  Limit.X := MaxLineLength;
  Limit.Y := CountLines(Buffer^[GapLen], BufLen) + 1;
  DrawLine := 0;
  DrawPtr := 0;
  DelCount := 0;
  InsCount := 0;
  Modified := False;
  Update(ufView);
end;

function TEditor.SetBufSize(NewSize: Word): Boolean;
begin
  SetBufSize := NewSize <= BufSize;
end;

procedure TEditor.SetCmdState(Command: Word; Enable: Boolean);
var
  S: TCommandSet;
begin
  S := [Command];
  if Enable and (State and sfActive <> 0) then
    EnableCommands(S) else DisableCommands(S);
end;

procedure TEditor.SetCurPtr(P: Word; SelectMode: Byte);
var
  Anchor: Word;
begin
  if SelectMode and smExtend = 0 then Anchor := P else
    if CurPtr = SelStart then Anchor := SelEnd else Anchor := SelStart;
  if P < Anchor then
  begin
    if SelectMode and smDouble <> 0 then
    begin
      P := PrevLine(NextLine(P));
      Anchor := NextLine(PrevLine(Anchor));
    end;
    SetSelect(P, Anchor, True);
  end else
  begin
    if SelectMode and smDouble <> 0 then
    begin
      P := NextLine(P);
      Anchor := PrevLine(NextLine(Anchor));
    end;
    SetSelect(Anchor, P, False);
  end;
end;

procedure TEditor.SetSelect(NewStart, NewEnd: Word; CurStart: Boolean);
var
  Flags: Byte;
  P, L: Word;
begin
  if CurStart then P := NewStart else P := NewEnd;
  Flags := ufUpdate;
  if (NewStart <> SelStart) or (NewEnd <> SelEnd) then
    if (NewStart <> NewEnd) or (SelStart <> SelEnd) then
      Flags := ufView;
  if P <> CurPtr then
  begin
    if P > CurPtr then
    begin
      L := P - CurPtr;
      Move(Buffer^[CurPtr + GapLen], Buffer^[CurPtr], L);
      Inc(CurPos.Y, CountLines(Buffer^[CurPtr], L));
      CurPtr := P;
    end else
    begin
      L := CurPtr - P;
      CurPtr := P;
      Dec(CurPos.Y, CountLines(Buffer^[CurPtr], L));
      if L > 0 then Move(Buffer^[CurPtr], Buffer^[CurPtr + GapLen], L);
    end;
    DrawLine := CurPos.Y;
    DrawPtr := LineStart(P);
    CurPos.X := CharPos(DrawPtr, P);
    DelCount := 0;
    InsCount := 0;
    SetBufSize(BufLen);
  end;
  SelStart := NewStart;
  SelEnd := NewEnd;
  Update(Flags);
end;

procedure TEditor.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState, Enable);
  case AState of
    sfActive:
      begin
        if HScrollBar <> nil then HScrollBar^.SetState(sfVisible, Enable);
        if VScrollBar <> nil then VScrollBar^.SetState(sfVisible, Enable);
        if Indicator <> nil then Indicator^.SetState(sfVisible, Enable);
        UpdateCommands;
      end;
    sfExposed:
      if Enable then Unlock;
  end;
end;

procedure TEditor.StartSelect;
begin
  HideSelect;
  Selecting := True;
end;

procedure TEditor.Store(var S: TStream);
begin
  inherited Store(S);
  PutPeerViewPtr(S, HScrollBar);
  PutPeerViewPtr(S, VScrollBar);
  PutPeerViewPtr(S, Indicator);
  S.Write(BufSize, SizeOf(Word));
  S.Write(CanUndo, SizeOf(Boolean));
end;

procedure TEditor.ToggleInsMode;
begin
  Overwrite := not Overwrite;
  SetState(sfCursorIns, not GetState(sfCursorIns));
end;

procedure TEditor.TrackCursor(Center: Boolean);
begin
  if Center then
    ScrollTo(CurPos.X - PageSize.X + 2 + PageTail.X, CurPos.Y - PageSize.Y div 2) else
    ScrollTo(Max(CurPos.X - PageSize.X + 2 + PageTail.X, Min(Delta.X, CurPos.X)),
      Max(CurPos.Y - PageSize.Y + 1 + PageTail.Y, Min(Delta.Y, CurPos.Y)));
end;

procedure TEditor.Undo;
var
  Length: Word;
begin
  if (DelCount <> 0) or (InsCount <> 0) then
  begin
    SelStart := CurPtr - InsCount;
    SelEnd := CurPtr;
    Length := DelCount;
    DelCount := 0;
    InsCount := 0;
    InsertBuffer(Buffer, CurPtr + GapLen - Length, Length, False, True);
  end;
end;

procedure TEditor.Unlock;
begin
  if LockCount > 0 then
  begin
    Dec(LockCount);
    if LockCount = 0 then DoUpdate;
  end;
end;

procedure TEditor.Update(AFlags: Byte);
begin
  UpdateFlags := UpdateFlags or AFlags;
  if LockCount = 0 then DoUpdate;
end;

procedure TEditor.UpdateCommands;
begin
  SetCmdState(cmUndo, (DelCount <> 0) or (InsCount <> 0));
  if not IsClipboard then
  begin
    SetCmdState(cmCut, HasSelection);
    SetCmdState(cmCopy, HasSelection);
    SetCmdState(cmPaste, (Clipboard <> nil) and (Clipboard^.HasSelection));
  end;
  SetCmdState(cmClear, HasSelection);
  SetCmdState(cmFind, True);
  SetCmdState(cmReplace, True);
  SetCmdState(cmSearchAgain, True);
end;

function TEditor.Valid(Command: Word): Boolean;
begin
  Valid := IsValid;
end;

function  TEditor.SubStrWidth(X, Y : Integer) : Word; {DK}
Var
  S : String;
Begin
  if X > Delta.X then begin
    Move(Buffer^[LineMove(0, Y) + Delta.X], S[1], X - Delta.X);
    S[0] := Char(X - Delta.X);
    SubStrWidth := FontWidth(S);
  end else SubStrWidth := 0;
End;

function  TEditor.SubStrLen(X, Y : Integer) : Word; {DK}
Var
  S        : String;
  XGetSize : Word;
  FirstLetter : Word;
  C : Char;
  WasPass : Boolean;
Begin
  S := ''; XGetSize := 0;
  FirstLetter := LineMove(0, Y) + Delta.X;
  WasPass := False;
  repeat
    if not WasPass then begin
      C := Buffer^[FirstLetter];
      if C = #13 then begin
        WasPass := True;
        C := ' ';
        Continue;
      end;
    end else C := ' ';
    Inc(XGetSize, FontWidth(C));
    if XGetSize > X then Break;
    S := S + C;
    Inc(FirstLetter);
  until False;
  SubStrLen := Length(S);
End;


{ TMemo }

constructor TMemo.Load(var S: TStream);
var
  Length: Word;
begin
  inherited Load(S);
  S.Read(Length, SizeOf(Word));
  if IsValid then
  begin
    S.Read(Buffer^[BufSize - Length], Length);
    SetBufLen(Length);
  end
  else S.Seek(S.GetPos + Length);
end;

function TMemo.DataSize: Word;
begin
  DataSize := BufSize + SizeOf(Word);
end;

procedure TMemo.GetData(var Rec);
var
  Data: TMemoData absolute Rec;
begin
  Data.Length := BufLen;
  Move(Buffer^, Data.Buffer, CurPtr);
  Move(Buffer^[CurPtr + GapLen], Data.Buffer[CurPtr], BufLen - CurPtr);
  FillChar(Data.Buffer[BufLen], BufSize - BufLen, 0);
end;

function TMemo.GetPalette: PPalette;
const
  P: String[Length(CMemo)] = CMemo;
begin
  GetPalette := @P;
end;

procedure TMemo.HandleEvent(var Event: TEvent);
begin
  if (Event.What <> evKeyDown) or (Event.KeyCode <> kbTab) then
    inherited HandleEvent(Event);
end;

procedure TMemo.SetData(var Rec);
var
  Data: TMemoData absolute Rec;
begin
  Move(Data.Buffer, Buffer^[BufSize - Data.Length], Data.Length);
  SetBufLen(Data.Length);
end;

procedure TMemo.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(BufLen, SizeOf(Word));
  S.Write(Buffer^, CurPtr);
  S.Write(Buffer^[CurPtr + GapLen], BufLen - CurPtr);
end;

{ TFileEditor }

constructor TFileEditor.Init(var Bounds: TRect;
  AHScrollBar, AVScrollBar: PScrollBar;
  AIndicator: PIndicator; AFileName: FNameStr);
begin
  inherited Init(Bounds, AHScrollBar, AVScrollBar, AIndicator, 0);
  if AFileName <> '' then
  begin
    FileName := FExpand(AFileName);
    if IsValid then IsValid := LoadFile;
  end;
end;

constructor TFileEditor.Load(var S: TStream);
var
  SStart, SEnd, Curs: Word;
begin
  inherited Load(S);
  BufSize := 0;
  S.Read(FileName[0], SizeOf(Char));
  S.Read(Filename[1], Length(FileName));
  if IsValid then IsValid := LoadFile;
  S.Read(SStart, SizeOf(Word));
  S.Read(SEnd, SizeOf(Word));
  S.Read(Curs, SizeOf(Word));
  if IsValid and (SEnd <= BufLen) then
  begin
    SetSelect(SStart, SEnd, Curs = SStart);
    TrackCursor(True);
  end;
end;

procedure TFileEditor.DoneBuffer;
begin
  if Buffer <> nil then DisposeBuffer(Buffer);
end;

procedure TFileEditor.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      case Event.Command of
        cmSave: Save;
        cmSaveAs: SaveAs;
      else
        Exit;
      end;
  else
    Exit;
  end;
  ClearEvent(Event);
end;

procedure TFileEditor.InitBuffer;
begin
  NewBuffer(Pointer(Buffer), $1000);
end;

function TFileEditor.LoadFile: Boolean;
var
  Length: Word;
  FSize: Longint;
  F: File;
begin
  LoadFile := False;
  Length := 0;
  Assign(F, FileName);
  Reset(F, 1);
  if IOResult <> 0 then EditorDialog(edReadError, @FileName)
  else
  begin
    FSize := FileSize(F);
    if (FSize > $FFF0) or not SetBufSize(Word(FSize)) then
      EditorDialog(edOutOfMemory, nil) else
    begin
      BlockRead(F, Buffer^[BufSize - Word(FSize)], FSize);
      if IOResult <> 0 then EditorDialog(edReadError, @FileName) else
      begin
        LoadFile := True;
        Length := FSize;
      end;
    end;
    Close(F);
  end;
  SetBufLen(Length);
end;

function TFileEditor.Save: Boolean;
begin
  if FileName = '' then Save := SaveAs else Save := SaveFile;
end;

function TFileEditor.SaveAs: Boolean;
begin
  SaveAs := False;
  if EditorDialog(edSaveAs, @FileName) <> cmCancel then
  begin
    FileName := FExpand(FileName);
    Message(Owner, evBroadcast, cmUpdateTitle, nil);
    SaveAs := SaveFile;
    if IsClipboard then FileName := '';
  end;
end;

function TFileEditor.SaveFile: Boolean;
var
  F: File;
  BackupName: FNameStr;
  D: DirStr;
  N: NameStr;
  E: ExtStr;
begin
  SaveFile := False;
  if EditorFlags and efBackupFiles <> 0 then
  begin
    FSplit(FileName, D, N, E);
    BackupName := D + N + '.BAK';
    Assign(F, BackupName);
    Erase(F);
    Assign(F, FileName);
    Rename(F, BackupName);
    InOutRes := 0;
  end;
  Assign(F, FileName);
  Rewrite(F, 1);
  if IOResult <> 0 then EditorDialog(edCreateError, @FileName) else
  begin
    BlockWrite(F, Buffer^, CurPtr);
    BlockWrite(F, Buffer^[CurPtr + GapLen], BufLen - CurPtr);
    if IOResult <> 0 then EditorDialog(edWriteError, @FileName) else
    begin
      Modified := False;
      Update(ufUpdate);
      SaveFile := True;
    end;
    Close(F);
  end;
end;

function TFileEditor.SetBufSize(NewSize: Word): Boolean;
var
  N: Word;
begin
  SetBufSize := False;
  if NewSize = 0 then NewSize := $1000 else
    if NewSize > $F000 then NewSize := $FFF0 else
      NewSize := (NewSize + $0FFF) and $F000;
  if NewSize <> BufSize then
  begin
    if NewSize > BufSize then
      if not SetBufferSize(Buffer, NewSize) then Exit;
    N := BufLen - CurPtr + DelCount;
    Move(Buffer^[BufSize - N], Buffer^[NewSize - N], N);
    if NewSize < BufSize then SetBufferSize(Buffer, NewSize);
    BufSize := NewSize;
    GapLen := BufSize - BufLen;
  end;
  SetBufSize := True;
end;

procedure TFileEditor.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(FileName, Length(FileName) + 1);
  S.Write(SelStart, SizeOf(Word) * 3);
end;

procedure TFileEditor.UpdateCommands;
begin
  inherited UpdateCommands;
  SetCmdState(cmSave, True);
  SetCmdState(cmSaveAs, True);
end;

function TFileEditor.Valid(Command: Word): Boolean;
var
  D: Integer;
begin
  if Command = cmValid then Valid := IsValid else
  begin
    Valid := True;
    if Modified then
    begin
      if FileName = '' then D := edSaveUntitled else D := edSaveModify;
      case EditorDialog(D, @FileName) of
        cmYes: Valid := Save;
        cmNo: Modified := False;
        cmCancel: Valid := False;
      end;
    end;
  end;
end;

{ TEditWindow }

constructor TEditWindow.Init(var Bounds: TRect;
  FileName: FNameStr; ANumber: Integer) {SHIM};
var
  HScrollBar, VScrollBar: PScrollBar;
  Indicator: PIndicator;
  R: TRect;
begin
  inherited Init(Bounds, '', ANumber);
  Options := Options or ofTileable;

  Frame^.GetClientExtent(R);                    {OOA}
  Inc(R.A.X, 14 * CharWidth);                   {OOA/DK}
  Dec(R.B.Y, 2);                                {DK}
  R.A.Y:= R.B.Y - BitMapWidth(@ScrollUpDef);  {OOA/DK}
  Dec(R.B.X, BitMapWidth(@ScrollUpDef)+0);      {OOA}
  HScrollBar := New(PScrollBar, Init(R));
  HScrollBar^.Hide;
  Insert(HScrollBar);

  Frame^.GetClientExtent(R);                      {OOA}
  Dec(R.B.X, 2);                                  {DK}
  R.A.X := R.B.X - BitMapWidth(@ScrollUpDef)+0;  {OOA/DK}
  VScrollBar := New(PScrollBar, Init(R));
  VScrollBar^.Hide;
  Insert(VScrollBar);

  Frame^.GetClientExtent(R);            {DK}
  R.Assign(CharWidth, R.B.Y - FontHeight - 2, 14*CharWidth+4, R.B.Y); {DK}
  Indicator := New(PIndicator, Init(R));
  Indicator^.Hide;
  Insert(Indicator);
  Frame^.GetClientExtent(R);
  Dec(R.B.X, VScrollBar^.Size.X);
  Dec(R.B.Y, HScrollBar^.Size.Y);
  Editor := New(PFileEditor, Init(
    R, HScrollBar, VScrollBar, Indicator, FileName));
  Insert(Editor);
end;

constructor TEditWindow.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S, Editor);
end;

procedure TEditWindow.Close;
begin
  if Editor^.IsClipboard then Hide else inherited Close;
end;

function TEditWindow.GetTitle(MaxSize: Integer): TTitleStr;
begin
{$IFDEF Ukrainian}
  if Editor^.IsClipboard then GetTitle := 'Карман' else
    if Editor^.FileName = '' then GetTitle := 'Безымянный' else
      GetTitle := Editor^.FileName;
{$ELSE}
  {$IFDEF Russian}
  if Editor^.IsClipboard then GetTitle := 'Карман' else
    if Editor^.FileName = '' then GetTitle := 'Безымянный' else
      GetTitle := Editor^.FileName;
  {$ELSE} {English}
  if Editor^.IsClipboard then GetTitle := 'Clipboard' else
    if Editor^.FileName = '' then GetTitle := 'Untitled' else
      GetTitle := Editor^.FileName;
  {$ENDIF}
{$ENDIF}
end;

procedure TEditWindow.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmUpdateTitle) then
  begin
    Frame^.DrawView;
    ClearEvent(Event);
  end;
end;

procedure TEditWindow.SizeLimits(var Min, Max: TPoint);
begin
  inherited SizeLimits(Min, Max);
  Min.X := 26 * CharWidth; {DK}
end;

procedure TEditWindow.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S, Editor);
end;

procedure RegisterEditors;
begin
  RegisterType(REditor);
  RegisterType(RMemo);
  RegisterType(RFileEditor);
  RegisterType(RIndicator);
  RegisterType(REditWindow);
end;

end.
