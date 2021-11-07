{$DEFINE USESTANDARDBITMAPS}
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Dialogs;

{$O+,F+,X+,I-,S-}

interface

uses Objects, Drivers, Views, Validate, BitMaps, GDI, Palettes;

const

{ Color palettes }

  CGrayDialog    =  CGrayWindow;
  CBlueDialog    =  CBlueWindow;
  CCyanDialog    =  CCyanWindow;

  CDialog        =  CGrayDialog;


{                     ÉÍÍÍÑÍÍÍ» }
{ CStaticText         º 1 ³ 2 º }
{                     ÈÍÑÍÏÍÑÍ¼ }
{         Text fore ÄÄÄÄÙ   ³   }
{              back ÄÄÄÄÄÄÄÄÙ   }

  CStaticText    = #18#19;

{                ÉÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍ»                  }
{ CLabel         º 1 ³ 2 ³ 3 ³ 4 ³ 5 ³ 6 º                  }
{                ÈÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍ¼                  }
{ Text Normal fore Ù   ³   ³   ³   ³   ÀÄ Shortcut Selected }
{             back ÄÄÄÄÙ   ³   ³   ÀÄÄÄÄÄÄÄ Shortcut Normal }
{ Text Selected fore ÄÄÄÄÄÄÙ   ³                            }
{               back ÄÄÄÄÄÄÄÄÄÄÙ                            }


  CLabel         = #20#21#22#23#24#25;

{            ÉÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍ» }
{ CInputLine º 1 ³ 2 ³ 3 ³ 4 ³ 5 ³ 6 ³ 7 º }
{            ÈÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍ¼ }
{ Passive fore Ù   ³   ³   ³   ³   ³   ³   }
{ Passive back ÄÄÄÄÙ   ³   ³   ³   ³   ³   }
{ Active fore ÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   }
{ Active back ÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   }
{ Selected fore ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   }
{ Selected back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   }
{ Arrow ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   }

  CInputLine     = #26#27#28#29#30#31#32;

{              ÉÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍ» }
{ CButton      º 1 ³ 2 ³ 3 ³ 4 ³ 5 ³ 6 ³ 7 ³ 8 ³ 9 ³ A ³ B ³ C ³ D º }
{              ÈÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍ¼ }
{ Normal fore ÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   ³   ³   ³   ³   ³   }
{        back ÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   ³   ³   ³   ³   }
{ Default fore ÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   ³   ³   ³   }
{         back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   ³   ³   }
{ Selected fore ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   ³   }
{          back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   }
{ Disabled fore ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   }
{          back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   }
{ Shortcut Normal ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   }
{ Shortcut Default ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   }
{ Shortcut Selected ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   }
{ 3D bright ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   }
{ 3D dark ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   }

  CButton        = #33#34#35#36#37#38#39#40#41#42#43#44#45;

{                ÉÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍ»                  }
{ CCluster       º 1 ³ 2 ³ 3 ³ 4 ³ 5 ³ 6 ³ 7 ³ 8 º                  }
{                ÈÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍ¼                  }
{ Text Normal fore Ù   ³   ³   ³   ³   ³   ³   ÀÄ Shortcut Selected }
{             back ÄÄÄÄÙ   ³   ³   ³   ³   ÀÄÄÄÄÄÄÄ Shortcut Normal }
{ Text Selected fore ÄÄÄÄÄÄÙ   ³   ³   ³                            }
{               back ÄÄÄÄÄÄÄÄÄÄÙ   ³   ³                            }
{ Text Disabled fore ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³                            }
{               back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ                            }

  CCluster       = #46#47#48#49#50#51#52#53#54;

{          ÉÍÍÍ» }
{ CHistory º 1 º }
{          ÈÍÑÍ¼ }
{ back ÄÄÄÄÄÄÙ   }

  CHistory       = #27; {passive input line back}


{                    ÉÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍ» }
{ CHistoryWindow     º 1 ³ 2 ³ 3 ³ 4 ³ 5 ³ 6 ³ 7 ³ 8 ³ 9 º }
{                    ÈÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍ¼ }
{ Passive Frame back ÄÄÙ   ³   ³   ³   ³   ³   ³   ³   ³   }
{ Active  Frame back ÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   ³   }
{ Passive Title fore ÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   ³   }
{               back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   ³   }
{ Active  Title fore ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   ³   }
{               back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³   }
{ 3D  bright ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   }
{     dark ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   }
{ Window background ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   }
{                ÉÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÑÍÍÍÍÑÍÍÍÍ»}
{                º A ³ B ³ C ³ D ³ E ³ F ³ 10 ³ 11 º}
{                ÈÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÏÍÑÍÍÏÍÑÍÍ¼}
{ Normal Text fore Ù   ³   ³   ³   ³   ³   ³    ³  }
{             back ÄÄÄÄÙ   ³   ³   ³   ³   ³    ³  }
{ Selected Text fore ÄÄÄÄÄÄÙ   ³   ³   ³   ³    ³  }
{               back ÄÄÄÄÄÄÄÄÄÄÙ   ³   ³   ³    ³  }
{ Focused  Text fore ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³   ³    ³  }
{               back ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   ³    ³  }
{ Divider 3D  bright ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ³  }
{               dark ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  }

  CHistoryWindow = #55#55#56#55#56#55#57#58#55#59#60#61#55#61#62;
  CHistoryViewer = #10#11#12#13#14#15#7#8;

{ TDialog palette entires }

  dpBlueDialog = wpBlueWindow;
  dpCyanDialog = wpCyanWindow;
  dpGrayDialog = wpGrayWindow;

{ TButton flags }

  bfNormal    = $00;
  bfDefault   = $01;
  bfLeftJust  = $02;
  bfBroadcast = $04;
  bfGrabFocus = $08;

{ TMultiCheckboxes flags }
{ hibyte = number of bits }
{ lobyte = bit mask }

  cfOneBit       = $0101;
  cfTwoBits      = $0203;
  cfFourBits     = $040F;
  cfEightBits    = $08FF;

  hcOkButton     = 64000;
  hcCancelButton = 64001;

type

{ TDialog object }

  { Palette layout }
  {  1 = Frame passive }
  {  2 = Frame active }
  {  3 = Frame icon }
  {  4 = ScrollBar page area }
  {  5 = ScrollBar controls }
  {  6 = StaticText }
  {  7 = Label normal }
  {  8 = Label selected }
  {  9 = Label shortcut }
  { 10 = Button normal }
  { 11 = Button default }
  { 12 = Button selected }
  { 13 = Button disabled }
  { 14 = Button shortcut }
  { 15 = Button shadow }
  { 16 = Cluster normal }
  { 17 = Cluster selected }
  { 18 = Cluster shortcut }
  { 19 = InputLine normal text }
  { 20 = InputLine selected text }
  { 21 = InputLine arrows }
  { 22 = History arrow }
  { 23 = History sides }
  { 24 = HistoryWindow scrollbar page area }
  { 25 = HistoryWindow scrollbar controls }
  { 26 = ListViewer normal }
  { 27 = ListViewer focused }
  { 28 = ListViewer selected }
  { 29 = ListViewer divider }
  { 30 = InfoPane }
  { 31 = Cluster disabled }
  { 32 = Reserved }

  PDialog = ^TDialog;
  TDialog = object(TWindow)
    constructor Init(var Bounds: TRect; ATitle: TTitleStr);
    constructor Load(var S: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Valid(Command: Word): Boolean; virtual;
  end;

{ TSItem }

  PSItem = ^TSItem;
  TSItem = record
    Value: PString;
    Next: PSItem;
  end;

{ TInputLine object }

  { Palette layout }
  { 1 = Passive }
  { 2 = Active }
  { 3 = Selected }
  { 4 = Arrows }

  PInputLine = ^TInputLine;
  TInputLine = object(TView)
    Data: PString;
    MaxLen: Integer;
    CurPos: Integer;
    FirstPos: Integer;
    SelStart: Integer;
    SelEnd: Integer;
    Validator: PValidator;
    constructor Init(var Bounds: TRect; AMaxLen: Integer);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SelectAll(Enable: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure SetValidator(AValid: PValidator);
    procedure Store(var S: TStream);
    function Valid(Command: Word): Boolean; virtual;
  private
    function CanScroll(Delta: Integer): Boolean;
    Function StringByX(FirstLetter, XSize : Word) : String; {DK}
  end;

{ TButton object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Default text }
  { 3 = Selected text }
  { 4 = Disabled text }
  { 5 = Normal shortcut }
  { 6 = Default shortcut }
  { 7 = Selected shortcut }
  { 8 = Shadow }

  PButton = ^TButton;
  TButton = object(TView)
    Title: PString;
    Command: Word;
    Flags: Byte;
    AmDefault: Boolean;
    _Down: Boolean; {OOA}
    constructor Init(var Bounds: TRect; ATitle: TTitleStr; ACommand: Word;
      AFlags: Word);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure DrawState(Down: Boolean);  virtual; {TONY}
    procedure DrawTitle(Color: Word); virtual; {GIO}
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure MakeDefault(Enable: Boolean);
    procedure Press; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  end;

  PBmpButton = ^TBmpButton; {SHIM}
  TBmpButton = object(TButton)
    BitMaps : array[0..4] of Integer; {TONY}
    constructor Init(var Bounds: TRect; ACommand: Word; AFlags: Word;
                     Normal, Default, Selected, Pressed, Disabled: Integer);
    constructor Load(var S: TStream); {OOA}
    destructor Done; virtual; {TONY}
    procedure Draw; virtual; {Shim}
    procedure Store(var S: TStream); {OOA}
  end;

  PSpeedButton = ^TSpeedButton; {DK}
  TSpeedButton = object(TButton)
    BitMap : Integer;
    TransparentColor : LongInt;
    constructor Init(var Bounds: TRect; ATitle : String; ACommand: Word;
      AFlags: Word; ABitmap : Integer; APalette : PLogPalette);
    procedure DrawTitle(Color: Word); virtual; {GIO}
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Store(var S: TStream);
  end;

{ TCluster }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }
  { 5 = Disabled text }

  PCluster = ^TCluster;
  TCluster = object(TView)
    Value: LongInt;
    Sel: Integer;
    EnableMask: LongInt;
    Strings: TStringCollection;
    Pressed: Boolean;
    StepColumn: Byte;
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function ButtonState(Item: Integer): Boolean;
    function DataSize: Word; virtual;
    {procedure DrawBox(const Icon: String; Marker: Char);}
    procedure DrawBox(const Icon: String; const Marker);
    {procedure DrawMultiBox(const Icon, Marker: String);}
    procedure DrawMultiBox(const Icon: String; const Marker);
    procedure GetData(var Rec); virtual;
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Mark(Item: Integer): Boolean; virtual;
    function MultiMark(Item: Integer): Byte; virtual;
    procedure Press(Item: Integer); virtual;
    procedure MovedTo(Item: Integer); virtual;
    procedure SetButtonState(AMask: Longint; Enable: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  private
    function Column(Item: Integer): Integer;
    function FindSel(P: TPoint): Integer;
    function Row(Item: Integer): Integer;
    function StringHeight: Integer; virtual;
  end;

{ TRadioButtons }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = object(TCluster)
    procedure Draw; virtual;
    function Mark(Item: Integer): Boolean; virtual;
    procedure MovedTo(Item: Integer); virtual;
    procedure Press(Item: Integer); virtual;
    procedure SetData(var Rec); virtual;
  private
    function StringHeight: Integer; virtual;
  end;

{ TCheckBoxes }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = object(TCluster)
    procedure Draw; virtual;
    function Mark(Item: Integer): Boolean; virtual;
    procedure Press(Item: Integer); virtual;
  private
    function StringHeight: Integer; virtual;
  end;

{ TMultiCheckBoxes }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = object(TCluster)
    SelRange: Byte;
    Flags: Word;
    States: PString;
    constructor Init(var Bounds: TRect; AStrings: PSItem;
      ASelRange: Byte; AFlags: Word; const AStates: String);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function DataSize: Word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function MultiMark(Item: Integer): Byte; virtual;
    procedure Press(Item: Integer); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

{ TListBox }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PListBox = ^TListBox;
  TListBox = object(TListViewer)
    List: PCollection;
    constructor Init(var Bounds: TRect; ANumCols: Word;
      AScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure NewList(AList: PCollection); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

{ TStaticText }

  { Palette layout }
  { 1 = Text }

  PStaticText = ^TStaticText;
  TStaticText = object(TView)
    Text: PString;
    constructor Init(var Bounds: TRect; const AText: String);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetText(var S: String); virtual;
    procedure Store(var S: TStream);
  end;

{ TStaticBitMap }  {TONY}
  { Palette layout is empty }

  PStaticBitMap = ^TStaticBitMap;
  TStaticBitMap = object(TView)
    BitMap: PBitMap;
    constructor Init(var Bounds: TRect; ABitMap: PBitMap; APalette : PLogPalette; CalcSize: boolean);
    constructor Load(var S: TStream); {GIO}
    destructor Done; virtual;
    procedure Draw; virtual;
    {function GetPalette: PPalette; virtual;}
    procedure Store(var S: TStream);  {GIO}
  end;

  POperationalBitMap = ^TOperationalBitMap;
  TOperationalBitMap = object(TStaticBitMap)
    Operation : Byte;
    constructor Init(var Bounds: TRect; ABitMap: PBitMap; APAlette : PLogPalette; CalcSize: boolean; AOperation : Byte);
    procedure   Draw; virtual;
  end;

  PStaticIcon = ^TStaticIcon;
  TStaticIcon = object(TStaticBitMap)
    BitMapAnd: PBitMap;
    constructor Init(var Bounds: TRect; ABitMapXor, ABitMapAnd: PBitMap; CalcSize: boolean);
    destructor  Done; virtual;
    procedure   Draw; virtual;
  end;

{ TParamText }

  { Palette layout }
  { 1 = Text }

  PParamText = ^TParamText;
  TParamText = object(TStaticText)
    ParamCount: Integer;
    ParamList: Pointer;
    constructor Init(var Bounds: TRect; const AText: String;
      AParamCount: Integer);
    constructor Load(var S: TStream);
    function DataSize: Word; virtual;
    procedure GetText(var S: String); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
  end;

{ TLabel }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PLabel = ^TLabel;
  TLabel = object(TStaticText)
    Link: PView;
    Light: Boolean;
    constructor Init(var Bounds: TRect; const AText: String; ALink: PView);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var S: TStream);
  end;

{ THistoryViewer }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = object(TListViewer)
    HistoryId: Word;
    constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      AHistoryId: Word);
    function GetPalette: PPalette; virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function HistoryWidth: Integer;
  end;

{ THistoryWindow }

  { Palette layout }
  { 1 = Frame passive }
  { 2 = Frame active }
  { 3 = Frame icon }
  { 4 = ScrollBar page area }
  { 5 = ScrollBar controls }
  { 6 = HistoryViewer normal text }
  { 7 = HistoryViewer selected text }

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = object(TWindow)
    Viewer: PListViewer;
    constructor Init(var Bounds: TRect; HistoryId: Word);
    function GetPalette: PPalette; virtual;
    function GetSelection: String; virtual;
    procedure InitViewer(HistoryId: Word); virtual;
    procedure InitFrame; virtual; {TONY}
  end;

{ THistory }

  { Palette layout }
  { 1 = Arrow }
  { 2 = Sides }

  PHistory = ^THistory;
  THistory = object(TView)
    Link: PInputLine;
    HistoryId: Word;
    constructor Init(var Bounds: TRect; ALink: PInputLine; AHistoryId: Word);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function InitHistoryWindow(var Bounds: TRect): PHistoryWindow; virtual;
    procedure RecordHistory(const S: String); virtual;
    procedure Store(var S: TStream);
  end;

{ TListBox } {Moved to Interface section by Shimon!!!}

type
  TListBoxRec = record
    List: PCollection;
    Selection: Word;
  end;

  shStyles = (shBar, shFrame, shBarPattern, shBarBMP, shBitmap, shStretch);

  PShade = ^TShade;
  TShade = object(TView)
    Color, Shadow : word;
    Style : shStyles;
    constructor Init(var Bounds: TRect; AColor, AShadow, ABMP, APal: Word; AStyle: shStyles);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure Draw; virtual;
  end;

const shpUser: Word = fsInterleave; {§¤¥áï ¤«ï  «ìâ¥à­ Fill ¢ Shade}

{ Image registration procedure uses IDs : 101..200 }
procedure RegisterDialogsImages;
procedure RegisterCheckBoxImages;
procedure RegisterRadioButtonImages;
{procedure RegisterComboBoxImages;}

{ These aren't procedures. These are images!  Don't call them! }
procedure CheckBoxNoDef;   {101}
procedure CheckBoxYesDef;  {103}
procedure RadioButtonNoDef;   {105}
procedure RadioButtonYesDef;  {107}

{$IFDEF UseStandardBitMaps}

{ These aren't procedures. These are images!  Don't call them! }
procedure Btn_OK_Normal;       {151}
procedure Btn_OK_Default;      {152}
{procedure Btn_OK_Selected;     {153}
procedure Btn_OK_Pressed;      {154}
procedure Btn_Cancel_Normal;   {155}
procedure Btn_Cancel_Default;  {156}
{procedure Btn_Cancel_Selected; {157}
procedure Btn_Cancel_Pressed;  {158}
procedure Btn_Yes_Normal;      {159}
procedure Btn_Yes_Default;     {160}
{procedure Btn_Yes_Selected;    {161}
procedure Btn_Yes_Pressed;     {162}
procedure Btn_No_Normal;       {163}
procedure Btn_No_Default;      {164}
{procedure Btn_No_Selected;     {165}
procedure Btn_No_Pressed;      {166}
{procedure Btn_Exit_Normal;     {167}
{procedure Btn_Exit_Default;    {168}
{procedure Btn_Exit_Selected;   {169}
{procedure Btn_Exit_Pressed;    {170}
(*procedure Btn_Help_Normal;       {171}
procedure Btn_Help_Default;      {172}
{procedure Btn_No_Selected;      {173}
procedure Btn_Help_Pressed;      {174}
procedure Btn_Abort_Normal;       {175}
procedure Btn_Abort_Default;      {176}
{procedure Btn_No_Selected;      {177}
procedure Btn_Abort_Pressed;      {178}
procedure Btn_Retry_Normal;       {179}
procedure Btn_Retry_Default;      {180}
{procedure Btn_No_Selected;      {181}
procedure Btn_Retry_Pressed;      {182}
procedure Btn_Ignore_Normal;       {179}
procedure Btn_Ignore_Default;      {180}
{procedure Btn_No_Selected;      {181}
procedure Btn_Ignore_Pressed;      {182}*)


procedure RegisterBtn_OK_Images;
procedure RegisterBtn_Cancel_Images;
procedure RegisterBtn_Yes_Images;
procedure RegisterBtn_No_Images;
{procedure RegisterBtn_Exit_Images;
procedure RegisterBtn_Help_Images;
procedure RegisterBtn_Abort_Images;
procedure RegisterBtn_Retry_Images;
procedure RegisterBtn_Ignore_Images;}

{ ¯à®æ¥¤ãàë ¡ëáâà®£® á®§¤ ­¨ï áâ ­¤ àâ­ëå ª­®¯®ª }
function MakeOKButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeCancelButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeYesButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeNoButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
{function MakeExitButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeHelpButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeAbortButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeRetryButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
function MakeIgnoreButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;}

{$ENDIF}

{ SItem routines }

function NewSItem(const Str: String; ANext: PSItem): PSItem;

{ Dialogs registration procedure }

procedure RegisterDialogs;

{ Stream Registration Records }

const
  RDialog: TStreamRec = (
     ObjType: 10;
     VmtLink: Ofs(TypeOf(TDialog)^);
     Load:    @TDialog.Load;
     Store:   @TDialog.Store
  );

const
  RInputLine: TStreamRec = (
     ObjType: 11;
     VmtLink: Ofs(TypeOf(TInputLine)^);
     Load:    @TInputLine.Load;
     Store:   @TInputLine.Store
  );

const
  RButton: TStreamRec = (
     ObjType: 12;
     VmtLink: Ofs(TypeOf(TButton)^);
     Load:    @TButton.Load;
     Store:   @TButton.Store
  );

const
  RCluster: TStreamRec = (
     ObjType: 13;
     VmtLink: Ofs(TypeOf(TCluster)^);
     Load:    @TCluster.Load;
     Store:   @TCluster.Store
  );

const
  RRadioButtons: TStreamRec = (
     ObjType: 14;
     VmtLink: Ofs(TypeOf(TRadioButtons)^);
     Load:    @TRadioButtons.Load;
     Store:   @TRadioButtons.Store
  );

const
  RCheckBoxes: TStreamRec = (
     ObjType: 15;
     VmtLink: Ofs(TypeOf(TCheckBoxes)^);
     Load:    @TCheckBoxes.Load;
     Store:   @TCheckBoxes.Store
  );

const
  RMultiCheckBoxes: TStreamRec = (
     ObjType: 27;
     VmtLink: Ofs(TypeOf(TMultiCheckBoxes)^);
     Load:    @TMultiCheckBoxes.Load;
     Store:   @TMultiCheckBoxes.Store
  );

const
  RListBox: TStreamRec = (
     ObjType: 16;
     VmtLink: Ofs(TypeOf(TListBox)^);
     Load:    @TListBox.Load;
     Store:   @TListBox.Store
  );

const
  RStaticText: TStreamRec = (
     ObjType: 17;
     VmtLink: Ofs(TypeOf(TStaticText)^);
     Load:    @TStaticText.Load;
     Store:   @TStaticText.Store
  );

const
  RLabel: TStreamRec = (
     ObjType: 18;
     VmtLink: Ofs(TypeOf(TLabel)^);
     Load:    @TLabel.Load;
     Store:   @TLabel.Store
  );

const
  RHistory: TStreamRec = (
     ObjType: 19;
     VmtLink: Ofs(TypeOf(THistory)^);
     Load:    @THistory.Load;
     Store:   @THistory.Store
  );

const
  RParamText: TStreamRec = (
     ObjType: 20;
     VmtLink: Ofs(TypeOf(TParamText)^);
     Load:    @TParamText.Load;
     Store:   @TParamText.Store
  );

{$IFDEF UseStandardBitMaps}
const
  RBmpButton: TStreamRec = (
     ObjType: 21;
     VmtLink: Ofs(TypeOf(TBmpButton)^);
     Load:    @TBmpButton.Load;
     Store:   @TBmpButton.Store
  );
{$ENDIF}
const
  RShade: TStreamRec = (
     ObjType: 22;
     VmtLink: Ofs(TypeOf(TShade)^);
     Load:    @TShade.Load;
     Store:   @TShade.Store
  );

  RStaticBitMap: TStreamRec = (
     ObjType: 23;
     VmtLink: Ofs(TypeOf(TStaticBitMap)^);
     Load:    @TStaticBitMap.Load;
     Store:   @TStaticBitMap.Store
  );

   RSpeedButton: TStreamRec = (
     ObjType: 24;
     VmtLink: Ofs(TypeOf(TSpeedButton)^);
     Load:    @TSpeedButton.Load;
     Store:   @TSpeedButton.Store
  );


const

{ Dialog broadcast commands }

  cmRecordHistory = 60;

{ Command send to InputLine from attached History after
  inserting history in a Data }

  cmDropHistory   = 63; {SHIM}

{ Command send to attached History from InputLine
  to execute HistoryWindow }

  cmExecHistory   = 64; {SHIM}

implementation

uses HistList;

const

{ TButton messages }

  cmGrabDefault    = 61;
  cmReleaseDefault = 62;
  cmDefaultTrue    = 63;

{ Utility functions }

function IsBlank(Ch: Char): Boolean;
begin
  IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
end;

{ TDialog }

constructor TDialog.Init(var Bounds: TRect; ATitle: TTitleStr);
begin
  inherited Init(Bounds, ATitle, wnNoNumber);
  Options := Options or ofVersion20;
  GrowMode := 0;
  Flags := wfMove + wfClose;
  Palette := dpGrayDialog;
end;

constructor TDialog.Load(var S: TStream);
begin
  inherited Load(S);
  if Options and ofVersion = ofVersion10 then
  begin
    Palette := dpGrayDialog;
    Inc(Options, ofVersion20);
  end;
end;

function TDialog.GetPalette: PPalette;
const
  P: array[dpBlueDialog..dpGrayDialog] of string[Length(CBlueDialog)] =
    (CBlueDialog, CCyanDialog, CGrayDialog);
begin
  GetPalette := @P[Palette];
end;

procedure TDialog.HandleEvent(var Event: TEvent);
begin
  TWindow.HandleEvent(Event);
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        kbEsc:
          begin
            Event.What := evCommand;
            Event.Command := cmCancel;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
          end;
        kbEnter:
          begin
            Event.What := evBroadcast;
            Event.Command := cmDefault;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
          end;
      end;
    evCommand:
      case Event.Command of
        cmOk, cmCancel, cmYes, cmNo:
          if State and sfModal <> 0 then
          begin
            EndModal(Event.Command);
            ClearEvent(Event);
          end;
      end;
  end;
end;

function TDialog.Valid(Command: Word): Boolean;
begin
  if Command = cmCancel then Valid := True
  else Valid := TGroup.Valid(Command);
end;

function NewSItem(const Str: String; ANext: PSItem): PSItem;
var
  Item: PSItem;
begin
  New(Item);
  NewSItem := Item;
  if Item = Nil then Exit;
  Item^.Value := NewStr(Str);
  Item^.Next := ANext;
end;

function Max(A, B: Integer): Integer;
inline(
   $58/     {pop   ax   }
   $5B/     {pop   bx   }
   $3B/$C3/ {cmp   ax,bx}
   $7F/$01/ {jg    @@1  }
   $93);    {xchg  ax,bx}
       {@@1:            }

function HotKey(const S: String): Char;
var
  P: Word;
begin
  P := Pos('~',S);
  if P <> 0 then HotKey := UpCase(S[P+1])
  else HotKey := #0;
end;

{ TInputLine }

constructor TInputLine.Init(var Bounds: TRect; AMaxLen: Integer);
begin
  TView.Init(Bounds);
  State := State or sfCursorVis;
  Options := Options or (ofSelectable + ofFirstClick + ofVersion20);
  GetMem(Data, AMaxLen + 1);
  Data^ := '';
  MaxLen := AMaxLen;
  CursorShape := @InputMouseCursor;
end;

constructor TInputLine.Load(var S: TStream);
begin
  TView.Load(S);
  S.Read(MaxLen, SizeOf(Integer) * 5);
  GetMem(Data, MaxLen + 1);
  S.Read(Data^[0], 1);
  S.Read(Data^[1], Length(Data^));
  if Options and ofVersion >= ofVersion20 then
    Validator := PValidator(S.Get);
  Options := Options or ofVersion20;
end;

destructor TInputLine.Done;
begin
  FreeMem(Data, MaxLen + 1);
  SetValidator(nil);
  TView.Done;
end;

function TInputLine.CanScroll(Delta: Integer): Boolean;
begin
  if Delta < 0 then
    CanScroll := FirstPos > 0 else
  if Delta > 0 then
    {CanScroll := Length(Data^) - FirstPos + 2 > Size.X div CharWidth {SHIM}
    CanScroll := Length(Data^) - FirstPos > Length(StringByX(FirstPos, Size.X - 2 * FontWidth('N'))) {DK} else
    CanScroll := False;
end;

function TInputLine.DataSize: Word;
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

procedure TInputLine.Draw; {SHIM/DK}
var
  Color, Sel, Arr: word;
  L, R: Integer;
  AdvCharWidth, AdvCharHeight, AdvStrLen : Byte;

  Trio : array[0..2] of TPoint;

Function GetStartX(PosX : Byte) : Word;
Begin
  GetStartX := FontWidth(Copy(Data^, FirstPos + 1, PosX - FirstPos)) + AdvCharWidth;
End;

begin
  if State and sfFocused = sfFocused then begin
    Color := GetColor($0304);
    Sel := GetColor($0506);
  end else
    Color := GetColor($0102);
  Arr := GetColor($07);
  AdvCharWidth := FontWidth('N');
  AdvCharHeight := FontHeight;

  Bar(0, 0, Size.X, Size.Y, Lo(Color));
  AdvStrLen := Length(StringByX(FirstPos, Size.X - 2 * AdvCharWidth));
  WrStr(AdvCharWidth, 0, Copy(Data^, FirstPos + 1, AdvStrLen), Hi(Color));
  PaintInfo.Fore := ColorIndex^[Arr];

  if CanScroll(1) then {WrStr((Size.X div CharWidth - 1) * CharWidth, 0, #16, Arr);} begin
    {almost all of Windows fonts doesn't contain #16/#17}
    Trio[0].X := Size.X - 2;
    Trio[0].Y := 2;
    Trio[1].X := Size.X - 2;
    Trio[1].Y := Size.Y - 2;
    Trio[2].X := Size.X - AdvCharWidth + 2;
    Trio[2].Y := Size.Y div 2;
    FillPoly(3, Trio);
  end;


  if State and sfFocused = sfFocused then
  begin
    if CanScroll(-1) then {WrStr(0, 0, #17, Arr);} begin
      Trio[0].X := 2;
      Trio[0].Y := 2;
      Trio[1].X := 2;
      Trio[1].Y := Size.Y - 2;
      Trio[2].X := AdvCharWidth - 2;
      Trio[2].Y := Size.Y div 2;
      FillPoly(3, Trio);
    end;
    L := SelStart - FirstPos;
    R := SelEnd - FirstPos;
    if L < 0 then L := 0;
    if R > AdvStrLen then R := AdvStrLen;
    if L < R then begin
      Bar(GetStartX(L + FirstPos), 0, GetStartX(R + FirstPos), FontHeight, Lo(Sel));
      WrStr(GetStartX(L + FirstPos), 0, Copy(Data^, Succ(Max(SelStart, FirstPos)), R - L), Hi(Sel));
    end;
  end;

  SetCursor(GetStartX(CurPos), 0);
end;

procedure TInputLine.GetData(var Rec);
begin
  if (Validator = nil) or
    (Validator^.Transfer(Data^, @Rec, vtGetData) = 0) then
  begin
    FillChar(Rec, DataSize, #0);
    Move(Data^, Rec, Length(Data^) + 1);
  end;
end;

function TInputLine.GetPalette: PPalette;
const
  P: String[Length(CInputLine)] = CInputLine;
begin
  GetPalette := @P;
end;

procedure TInputLine.HandleEvent(var Event: TEvent);
const
  PadKeys = [$47, $4B, $4D, $4F, $73, $74];
var
  Delta, Anchor, I: Integer;
  ExtendBlock: Boolean;
  OldData: string;
  OldCurPos, OldFirstPos,
  OldSelStart, OldSelEnd: Integer;
  WasAppending: Boolean;

function MouseDelta: Integer {SHIM};
var
  Mouse: TPoint;
begin
  MakeLocal(Event.Where, Mouse);
  if Mouse.X div CharWidth <= 0 then MouseDelta := -1 else
  if Mouse.X div CharWidth >= Size.X div CharWidth - 1 then MouseDelta := 1 else
  MouseDelta := 0;
end;

function MousePos: Integer;
var
  Pos: Integer;
  Mouse: TPoint;
begin
  MakeLocal(Event.Where, Mouse);
  if Mouse.X < CharWidth then Mouse.X := CharWidth;
  {Pos := Mouse.X div CharWidth + FirstPos - 1;}
  Pos := FirstPos + Length(StringByX(FirstPos, Mouse.X - FontWidth('N')));
  if Pos < 0 then Pos := 0;
  if Pos > Length(Data^) then Pos := Length(Data^);
  MousePos := Pos;
end;

procedure DeleteSelect;
begin
  if SelStart <> SelEnd then
  begin
    Delete(Data^, SelStart + 1, SelEnd - SelStart);
    CurPos := SelStart;
  end;
end;

procedure AdjustSelectBlock;
begin
  if CurPos < Anchor then
  begin
    SelStart := CurPos;
    SelEnd := Anchor;
  end else
  begin
    SelStart := Anchor;
    SelEnd := CurPos;
  end;
end;

procedure SaveState;
begin
  if Validator <> nil then
  begin
    OldData := Data^;
    OldCurPos := CurPos;
    OldFirstPos := FirstPos;
    OldSelStart := SelStart;
    OldSelEnd := SelEnd;
    WasAppending := Length(Data^) = CurPos;
  end;
end;

procedure RestoreState;
begin
  if Validator <> nil then
  begin
    Data^ := OldData;
    CurPos := OldCurPos;
    FirstPos := OldFirstPos;
    SelStart := OldSelStart;
    SelEnd := OldSelEnd;
  end;
end;

function CheckValid(NoAutoFill: Boolean): Boolean;
var
  OldLen: Integer;
  NewData: String;
begin
  if Validator <> nil then
  begin
    CheckValid := False;
    OldLen := Length(Data^);
    if (Validator^.Options and voOnAppend = 0) or
      (WasAppending and (CurPos = OldLen)) then
    begin
      NewData := Data^;
      if not Validator^.IsValidInput(NewData, NoAutoFill) then
        RestoreState
      else
      begin
        if Length(NewData) > MaxLen then NewData[0] := Char(MaxLen);
        Data^ := NewData;
        if (CurPos >= OldLen) and (Length(Data^) > OldLen) then
          CurPos := Length(Data^);
        CheckValid := True;
      end;
    end
    else
    begin
      CheckValid := True;
      if CurPos = OldLen then
        if not Validator^.IsValidInput(Data^, False) then
        begin
          Validator^.Error;
          CheckValid := False;
        end;
    end;
  end
  else
    CheckValid := True;
end;

begin
  TView.HandleEvent(Event);
  if State and sfSelected <> 0 then
  begin
    case Event.What of
      evMouseDown:
        if (Event.Buttons and mbLeftButton = mbLeftButton) then begin
          Delta := MouseDelta;
          if CanScroll(Delta) then
          begin
            repeat
              if CanScroll(Delta) then
              begin
                Inc(FirstPos, Delta);
                DrawView;
              end;
            until not MouseEvent(Event, evMouseAuto);
          end else
          if Event.Double then SelectAll(True) else
          begin
            Anchor := MousePos;
            repeat
              if (Event.What = evMouseAuto) and (Event.Buttons and mbLeftButton = mbLeftButton) then
              begin
                Delta := MouseDelta;
                if CanScroll(Delta) then Inc(FirstPos, Delta);
              end;
              CurPos := MousePos;
              AdjustSelectBlock;
              DrawView;
            until not MouseEvent(Event, evMouseMove + evMouseAuto);
          end;
          ClearEvent(Event);
        end;
      evKeyDown:
        begin
          SaveState;
          Event.KeyCode := CtrlToArrow(Event.KeyCode);
          if (Event.ScanCode in PadKeys) and
             (GetShiftState and $03 <> 0) then
          begin
            Event.CharCode := #0;
            if CurPos = SelEnd then Anchor := SelStart
            else Anchor := SelEnd;
            ExtendBlock := True;
          end
          else
            ExtendBlock := False;
          case Event.KeyCode of
            kbLeft:
              if CurPos > 0 then Dec(CurPos);
            kbRight:
              if CurPos < Length(Data^) then
              begin
                Inc(CurPos);
                CheckValid(True);
              end;
            kbHome:
              CurPos := 0;
            kbEnd:
              begin
                CurPos := Length(Data^);
                CheckValid(True);
              end;
            kbBack:
              if CurPos > 0 then
              begin
                Delete(Data^, CurPos, 1);
                Dec(CurPos);
                if FirstPos > 0 then Dec(FirstPos);
                CheckValid(True);
              end;
            kbDel:
              begin
                if SelStart = SelEnd then
                  if CurPos < Length(Data^) then
                  begin
                    SelStart := CurPos;
                    SelEnd := CurPos + 1;
                  end;
                DeleteSelect;
                CheckValid(True);
              end;
            kbIns:
              SetState(sfCursorIns, State and sfCursorIns = 0);
          else
            case Event.CharCode of
              ' '..#255:
                begin
                  if State and sfCursorIns <> 0 then
                    Delete(Data^, CurPos + 1, 1) else DeleteSelect;
                  if CheckValid(True) then
                  begin
                    if Length(Data^) < MaxLen then
                    begin
                      if FirstPos > CurPos then FirstPos := CurPos;
                      Inc(CurPos);
                      Insert(Event.CharCode, Data^, CurPos);
                    end;
                    CheckValid(False);
                  end;
                end;
              ^Y:
                begin
                  Data^ := '';
                  CurPos := 0;
                end;
            else
              Exit;
            end
          end;
          if ExtendBlock then
            AdjustSelectBlock
          else
          begin
            SelStart := CurPos;
            SelEnd := CurPos;
          end;
          if FirstPos > CurPos then FirstPos := CurPos;
          I := CurPos - Length(StringByX(FirstPos, Size.X - 2 * FontWidth('N'))); {DK}
          if FirstPos < I then FirstPos := I;
          I := CurPos - Length(StringByX(FirstPos, Size.X - 2 * FontWidth('N'))); {DK}
          if FirstPos < I then FirstPos := I;
          DrawView;
          ClearEvent(Event);
        end;
    end;
  end;
end;

procedure TInputLine.SelectAll(Enable: Boolean);
begin
  CurPos := 0;
  FirstPos := 0;
  SelStart := 0;
  if Enable then SelEnd := Length(Data^) else SelEnd := 0;
  DrawView;
end;

procedure TInputLine.SetData(var Rec);
begin
  if (Validator = nil) or
    (Validator^.Transfer(Data^, @Rec, vtSetData) = 0) then
    Move(Rec, Data^[0], DataSize);

  SelectAll(True);
end;

procedure TInputLine.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if (AState = sfSelected) or ((AState = sfActive) and
     (State and sfSelected <> 0)) then
    SelectAll(Enable)
  else if AState = sfFocused then
    DrawView;
end;

procedure TInputLine.SetValidator(AValid: PValidator);
begin
  if Validator <> nil then Validator^.Free;
  Validator := AValid;
end;

procedure TInputLine.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(MaxLen, SizeOf(Integer) * 5);
  S.WriteStr(Data);
  S.Put(Validator);
end;

Function TInputLine.StringByX(FirstLetter, XSize : Word) : String;
Var
  S        : String;
  XGetSize : Word;
Begin
  S := ''; XGetSize := 0;
  repeat
    Inc(FirstLetter);
    Inc(XGetSize, FontWidth(Data^[FirstLetter]));
    if (XGetSize > XSize) or (FirstLetter > Byte(Data^[0])) then Break;
    S := S + Data^[FirstLetter];
  until False;
  StringByX := S;
End;


function TInputLine.Valid(Command: Word): Boolean;

  function AppendError(Validator: PValidator): Boolean;
  begin
    AppendError := False;
    with Validator^ do
      if (Options and voOnAppend <> 0) and (CurPos <> Length(Data^))
          and not IsValidInput(Data^, True) then
      begin
        Error;
        AppendError := True;
      end;
  end;

begin
  Valid := inherited Valid(Command);
  if (Validator <> nil) and (State and sfDisabled = 0) then
    if Command = cmValid then
      Valid := Validator^.Status = vsOk
    else if Command <> cmCancel then
      if AppendError(Validator) or not Validator^.Valid(Data^) then
      begin
        Select;
        Valid := False;
      end;
end;

{ TButton }

constructor TButton.Init(var Bounds: TRect; ATitle: TTitleStr;
  ACommand: Word; AFlags: Word);
begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable + ofFirstClick +
    ofPreProcess + ofPostProcess);
  EventMask := EventMask or evBroadcast;
  if not CommandEnabled(ACommand) then State := State or sfDisabled;
  Flags := AFlags;
  if AFlags and bfDefault <> 0 then AmDefault := True
  else AmDefault := False;
  Title := NewStr(ATitle);
  Command := ACommand;
end;

constructor TButton.Load(var S: TStream);
begin
  TView.Load(S);
  Title := S.ReadStr;
  S.Read(Command, SizeOf(Word) + SizeOf(Byte) + SizeOf(Boolean));
  if not CommandEnabled(Command) then State := State or sfDisabled
  else State := State and not sfDisabled;
end;

destructor TButton.Done;
begin
  DisposeStr(Title);
  TView.Done;
end;

procedure TButton.DrawState(Down: Boolean);
begin
  _Down:= Down;
  DrawView;
end;

procedure TButton.DrawTitle {SHIM}; {GIO}
var L,H: Integer;

Procedure DrawVeil; {DK/GIO}
Var
  OP : TPaintInfo;
  XC,YC,XC1,YC1:Integer;
Begin
  OP := paintInfo;
  PaintInfo.Back := ColorIndex^[GetColor($02)];
  with PaintInfo do begin
    LinePattern := psDot;
    LineWidth:=1;
    LineStyle:=lsLinePattern;
    Fore := ColorIndex^[0];
    Operation:=CopyPut;
  end;
  XC:=Size.X shr 1;  YC:= Size.Y shr 1;
  XC1:=(CStrLen(Title^)+1) * CharWidth shr 1;
  YC1:=FontHeight shr 1;
  FrameStyle(XC-XC1,YC-YC1,XC+XC1,YC+YC1);
  PaintInfo :=  OP;
End;

begin
  if Flags and bfLeftJust <> 0 then begin L := 3; H := 3 end
  else begin
    L := (Size.X - CStrLen(Title^) * CharWidth) div 2;
    if L < 3 then L := 3;
    H := (Size.Y - FontHeight) div 2;
    if H < 3 then H := 3;
  end;
  if _Down  and (MaxColors > 2) then
    WrCStr(L+2, H+2, Copy(Title^, 1, (Size.X div CharWidth) + Length(Title^) - CStrLen(Title^)), GetColor($0D0D)); {DK}
  WrCStr(L, H, Copy(Title^, 1, (Size.X div CharWidth) + Length(Title^) - CStrLen(Title^)), Color);
  if State and sfSelected <> 0 then DrawVeil;
end;

procedure TButton.Draw;
var
  Color, Shadow, Back : Word;
  S             : Integer;
  XC,YC, XC1, YC1: Word;


begin {SHIM}
  if State and sfDisabled <> 0 then begin
    Color := GetColor($0707);
    Back  := GetColor($08);
  end else begin
    Color := GetColor($0901);
    Back  := GetColor($02);
    if State and sfActive = sfActive then
      if State and sfSelected = sfSelected then begin
        Color := GetColor($0B05);
        Back  := GetColor($06);
      end else
        if AmDefault then begin
          Color := GetColor($0A03);
          Back  := GetColor($04);
        end;
  end;
  Shadow := GetColor($0C0D);

  FrameBar(0, 0, Size.X, Size.Y, GetColor($01), GetColor($01), Back);
  if _Down then
  begin
    VLine(1, 1, Size.Y - 1, Lo(Shadow));
    HLine(1, 1, Size.X - 1, Lo(Shadow));
    {if State and sfActive <> 0 then  if State and sfSelected <> 0 then DrawVeil;}
  end else begin
    Rectangle(1, 1, Size.X-1, Size.Y-1, 1, Hi(Shadow), Lo(Shadow));
    if State and sfActive <> 0 then
      if State and sfSelected <> 0 then else
        if AmDefault then begin
          Rectangle(2, 2, Size.X-2, Size.Y-2, 1, Hi(Shadow), Lo(Shadow));
        end;
  end;

  DrawTitle(Color);
end;

function TButton.GetPalette: PPalette;
const
  P: String[Length(CButton)] = CButton;
begin
  GetPalette := @P;
end;

procedure TButton.HandleEvent(var Event: TEvent); {SHIM}
var
  Down: Boolean;
  C: Char;
  Mouse: TPoint;
  ClickRect: TRect;

  procedure PressAction; begin
    DrawState(True);
    Press;
    ClearEvent(Event);
    WaitForRetrace; WaitForRetrace;
    DrawState(False);
  end;

begin
  GetExtent(ClickRect);
  Inc(ClickRect.A.X);
  Dec(ClickRect.B.X);
  Dec(ClickRect.B.Y);
  if (Event.What = evMouseDown) and (Event.Buttons and mbLeftButton = mbLeftButton) then
  begin
    MakeLocal(Event.Where, Mouse);
    if not ClickRect.Contains(Mouse) then ClearEvent(Event);
  end;
  if Flags and bfGrabFocus <> 0 then
    TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      if (Event.Buttons and mbLeftButton = mbLeftButton) then begin
        if State and sfDisabled = 0 then
        begin
          Inc(ClickRect.B.X);
          Down := False;
          repeat
            MakeLocal(Event.Where, Mouse);
            if Down <> ClickRect.Contains(Mouse) then
            begin
              Down := not Down;
              DrawState(Down);
            end;
          until not MouseEvent(Event, evMouseMove);
          if Down then
          begin
            Press;
            DrawState(False);
          end;
        end;
        ClearEvent(Event);
      end;
    evKeyDown:
      begin
        C := HotKey(Title^);
        if (Event.KeyCode = GetAltCode(C)) or
          (Owner^.Phase = phPostProcess) and (C <> #0) and
            (Upcase(Event.CharCode) = C) or
          (State and sfFocused <> 0) and (Event.CharCode = ' ')
        then
          PressAction;
      end;
    evBroadcast:
      case Event.Command of
        cmDefault:
          if AmDefault then PressAction;
        cmReleaseDefault: if (Flags and bfDefault)<>0 then begin
           LPMessage(1,@Self,evBroadCast, cmDefaultTrue,nil);
           Exit;
        end;
        cmDefaultTrue: if (Flags and bfDefault)<>0 then begin
{GIO}      AmDefault:=True; DrawView; Exit;
        end;
        cmGrabDefault: if (Flags and bfDefault)<>0 then begin
         if AmDefault then begin AmDefault:=False; DrawView;
         end else ClearLPM(@Self, evBroadCast, cmDefaultTrue);
         Exit;
        end;
        cmCommandSetChanged:
          begin
            SetState(sfDisabled, not CommandEnabled(Command));
            DrawView;
          end;
      end;
  end;
end;

procedure TButton.MakeDefault(Enable: Boolean);
var
  C: Word;
begin
  if Flags and bfDefault = 0 then
  begin
    if Enable then C := cmGrabDefault else C := cmReleaseDefault;
    Message(Owner, evBroadcast, C, @Self);
    AmDefault := Enable;
    DrawView;
  end;
end;

procedure TButton.Press;
var
  E: TEvent;
begin
  Message(Owner, evBroadcast, cmRecordHistory, nil);
  if Flags and bfBroadcast <> 0 then
    Message(Owner, evBroadcast, Command, @Self) else
  begin
    E.What := evCommand;
    E.Command := Command;
    E.InfoPtr := @Self;
    PutEvent(E);
  end;
end;

procedure TButton.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if AState and (sfSelected + sfActive) <> 0 then DrawView;
  if AState and sfFocused <> 0 then MakeDefault(Enable);
end;

procedure TButton.Store(var S: TStream);
begin
  TView.Store(S);
  S.WriteStr(Title);
  S.Write(Command, SizeOf(Word) + SizeOf(Byte) + SizeOf(Boolean));
end;

{ TBmpButton } {SHIM}

constructor TBmpButton.Init(var Bounds: TRect; ACommand: Word; AFlags: Word;
                            Normal, Default, Selected, Pressed, Disabled: Integer);
var
  R: TRect;
begin
  R.A := Bounds.A;
  R.B.X := R.A.X + BitmapWidth(GetImage(Normal));
  R.B.Y := R.A.Y + BitmapHeight(GetImage(Normal));
  inherited Init(R, ' ', ACommand, AFlags);  {TONY} { Space, don't empty }

  BitMaps[0] := Normal;
  BitMaps[1] := Default;
  BitMaps[2] := Selected;
  BitMaps[3] := Pressed;
  BitMaps[4] := Disabled;
end;

constructor TBmpButton.Load(var S: TStream);
begin
  TButton.Load(S);
  S.Read(BitMaps, SizeOf(BitMaps));
end;

procedure TBmpButton.Draw;
var
  CButton : Byte;
begin
  if _Down then
    CButton := 3 {Pressed}
  else begin
    CButton := 0; {Normal}
    if State and sfDisabled <> 0 then
      CButton := 4  {Disabled}
    else
    begin
      if State and sfActive <> 0 then
        if State and sfSelected <> 0 then
          CButton := 2 {Selected}
        else
          if AmDefault then
            CButton := 1; {Default}
    end;
  end;
  PutBmp(GetImage(BitMaps[CButton]), 0, 0);
  if CButton = 4 then begin
    SetPaint(7, 0, NotBlack, 0, 1, lsPattern, fsSimpleDots);
    BarStyle(0, 0, Size.X-1, Size.Y-1);
  end;
end;

destructor TBmpButton.Done;
begin
  DisposeImage(BitMaps[0]);
  DisposeImage(BitMaps[1]);
  DisposeImage(BitMaps[2]);
  DisposeImage(BitMaps[3]);
  DisposeImage(BitMaps[4]);
  inherited Done;
end;

procedure TBmpButton.Store(var S: TStream);
begin
  TButton.Store(S);
  S.Write(BitMaps, SizeOf(BitMaps));
end;

constructor TSpeedButton.Init;
begin
  inherited Init(Bounds, ATitle, ACommand, AFlags);
  BitMap := ABitmap;
  if APalette <> Nil then LogPalette := APalette^;
  TransparentColor := ReadPixelBM(0, 0, GetImage(Bitmap));
  if (LogPalette.Colors > 0) and (MaxColors = 256) then
    Logpalette.Mode := Logpalette.Mode or pmOptimize or pmUseRGB;
  if (LogPalette.Colors > 0) and (MaxColors > 256) then
    Logpalette.Mode := Logpalette.Mode or pmComplex;
end;

constructor TSpeedButton.Load(var S: TStream);
begin
  TButton.Load(S);
  S.Read(BitMap, SizeOf(BitMap));
end;

procedure TSpeedButton.DrawTitle;
var
  CButton : Byte;
  AtX, AtY : Integer;
  L,H: Integer;

Procedure DrawVeil; {DK/GIO}
Var
  OP : TPaintInfo;
  XC,YC,XC1,YC1 : Integer;
Begin
  OP := paintInfo;
  PaintInfo.Back := ColorIndex^[GetColor($02)];
  with PaintInfo do begin
    LinePattern := psDot;
    LineWidth:=1;
    LineStyle:=lsLinePattern;
    Fore := ColorIndex^[0];
    Operation:=CopyPut;
  end;
  XC := (Size.X - AtX) shr 1 + AtX;  YC:= Size.Y shr 1;
  XC1:=(CStrLen(Title^)+1) * CharWidth shr 1;
  YC1:=FontHeight shr 1;
  FrameStyle(XC-XC1,YC-YC1,XC+XC1,YC+YC1);
  PaintInfo :=  OP;
End;


begin
  AtX := 0;
  AtY := 0;
  if _Down then begin
    AtX := 2;
    AtY := 2;
  end;
  SetColorBitBlt(MapColor(TransparentColor, LogPalette), True);
  PutBmpOp(GetImage(BitMap), AtX, AtY, UserBitBlt);
  if State and sfDisabled <> 0 then begin
    SetPaint(7, 0, NotBlack, 0, 1, lsPattern, fsSimpleDots);
    BarStyle(0, 0, Size.X-1, Size.Y-1);
  end;

  AtX := BitmapWidth(GetImage(Bitmap));
  if Flags and bfLeftJust <> 0 then begin
    L := AtX + 3;
    H := 3;
  end else begin
    L := (Size.X - AtX - CStrLen(Title^) * CharWidth) div 2 + AtX;
    if L < 3 then L := 3;
    H := (Size.Y - FontHeight) div 2;
    if H < 3 then H := 3;
  end;

  if Title = Nil then Exit;
  if _Down  and (MaxColors > 2) then
    WrCStr(L+2, H+2, Copy(Title^, 1, (Size.X div CharWidth) + Length(Title^) - CStrLen(Title^)), GetColor($0D0D)); {DK}
  WrCStr(L, H, Copy(Title^, 1, (Size.X div CharWidth) + Length(Title^) - CStrLen(Title^)), Color);
  if State and sfSelected <> 0 then DrawVeil;
end;

destructor TSpeedButton.Done;
begin
  DisposeImage(BitMap);
  inherited Done;
end;

procedure TSpeedButton.Store(var S: TStream);
begin
  TButton.Store(S);
  S.Write(BitMap, SizeOf(BitMap));
end;


{ TCluster }

constructor TCluster.Init(var Bounds: TRect; AStrings: PSItem);
var
  I: Integer;
  P: PSItem;
begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable + ofFirstClick + ofPreProcess +
    ofPostProcess + ofVersion20);
  I := 0;
  P := AStrings;
  while P <> nil do
  begin
    Inc(I);
    P := P^.Next;
  end;
  Strings.Init(I,0);
  while AStrings <> nil do
  begin
    P := AStrings;
    Strings.AtInsert(Strings.Count, AStrings^.Value);
    AStrings := AStrings^.Next;
    Dispose(P);
  end;
  Value := 0;
  Sel := 0;
  {SetCursor(2 * CharWidth, 0); ShowCursor; Shim}
  EnableMask := $FFFFFFFF;
  StepColumn :=6;
end;

constructor TCluster.Load(var S: TStream);
begin
  TView.Load(S);
  if (Options and ofVersion) >= ofVersion20 then
  begin
    S.Read(Value, SizeOf(Longint) * 2 + SizeOf(Integer));
  end
  else
  begin
    S.Read(Value, SizeOf(Value));
    S.Read(Sel, SizeOf(Sel));
    S.Read(StepColumn, SizeOf(StepColumn)); {GIO}
    EnableMask := $FFFFFFFF;
    Options := Options or ofVersion20;
  end;
  Strings.Load(S);
  SetButtonState(0, True);
end;

destructor TCluster.Done;
begin
  Strings.Done;
  TView.Done;
end;

function TCluster.ButtonState(Item: Integer): Boolean; assembler;
asm
        XOR     AL,AL
        MOV     CX,Item
        CMP     CX,31
        JA      @@3
        MOV     AX,1
        XOR     DX,DX
        JCXZ    @@2
@@1:    SHL     AX,1
        RCL     DX,1
        LOOP    @@1
@@2:    LES     DI,Self
        AND     AX,ES:[DI].TCluster.EnableMask.Word[0]
        AND     DX,ES:[DI].TCluster.EnableMask.Word[2]
        OR      AX,DX
        JZ      @@3
        MOV     AL,1
@@3:
end;

function TCluster.DataSize: Word;
begin
  DataSize := SizeOf(Word);
end;

procedure TCluster.DrawBox(const Icon: String; const Marker);
begin
  DrawMultiBox(Icon, Marker);
end;

procedure TCluster.DrawMultiBox(const Icon: String; const Marker) {SHIM};
var
  I,J,K,Cur,Col, H, x, y: Integer;
  CNorm, CSel, CDis, CNormBack, CSelBack, CDisBack,
  Color, Back, CMark, CMarkBack, cw: Word;
  t : string;

  Markers : Array[0..3] of Pointer absolute Marker;

begin
  CNorm := GetColor($0701); CNormBack := GetColor($02);
  CSel  := GetColor($0803); CSelBack  := GetColor($04);
  CDis  := GetColor($0701); CDisBack  := GetColor($06);
  CMark := GetColor($0709); CMarkBack := GetColor($02); {GIO}

  H := StringHeight;
  K := Size.Y div H;
  if K < 1 then exit; {K := 1;} {SHIM}
  Bar{Pattern}(0, 0, Size.X, Size.Y, CNormBack);
  for I := 0 to K do
  begin
    for J := 0 to (Strings.Count - 1) div K + 1 do
    begin
      Cur := J * K + I;
      if Cur < Strings.Count then
      begin
        Col := Column(Cur);
        cw := CharWidth;
        if (Col < Size.X div CharWidth) then
        begin
          if not ButtonState(Cur) then begin
            Color := CDis; Back := CDisBack;
          end else if Cur = Sel then
           if State and sfFocused <> 0 then begin
            Color := CSel; Back := CSelBack;
           end else begin
            Color := CMark; Back := CMarkBack; {GIO}
           end else begin
            Color := CNorm; Back := CNormBack;
           end;
          PutBmp(Markers[byte(Pressed) shl 1 + MultiMark(Cur)], Col * CharWidth, Row(Cur) * H);
          x := Col * CharWidth + BitMapWidth(Markers[0]);
          y := Row(Cur) * H;
          t := ' ' + PString(Strings.At(Cur))^;
          if Back <> CNormBack then
            Bar{Pattern}(x + CharWidth div 2, y, x + CharWidth div 2 + FontWidth(t), y + H, Back);
          WrCStr(x, y, t, Color);
          if not ButtonState(Cur) or ((State and sfDisabled) <> 0) then begin
            SetPaint(Back, Black, NotBlack, psSolid, lwThin, lsPattern, fsSimpleDots);
            BarStyle(x, y, x + FontWidth(t), y + FontHeight);
            DefaultPaint(PaintInfo);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCluster.GetData(var Rec);
begin
  Word(Rec) := Value;
end;

function TCluster.GetHelpCtx: Word;
begin
  if HelpCtx = hcNoContext then GetHelpCtx := hcNoContext
  else GetHelpCtx := HelpCtx + Sel;
end;

function TCluster.GetPalette: PPalette;
const
  P: String[Length(CCluster)] = CCluster;
begin
  GetPalette := @P;
end;

procedure TCluster.HandleEvent(var Event: TEvent);
var
  Mouse: TPoint;
  I, S: Integer;
  C: Char;

procedure MoveSel;
begin
  if I <= Strings.Count then
  begin
    Sel := S;
    MovedTo(Sel);
    DrawView;
  end;
end;

begin
  TView.HandleEvent(Event);
  if (Options and ofSelectable) = 0 then Exit;
  if (Event.What = evMouseDown) and (Event.Buttons and mbLeftButton = mbLeftButton) then
  begin
    MakeLocal(Event.Where, Mouse);
    I := FindSel(Mouse);
    if I <> -1 then if ButtonState(I) then Sel := I;
(*    DrawView; {GIO}
    repeat
      MakeLocal(Event.Where, Mouse);
      {if FindSel(Mouse) = Sel then
        ShowCursor else
        HideCursor;}
    until not MouseEvent(Event,evMouseMove); {Wait for mouse up}
    {ShowCursor;}
    MakeLocal(Event.Where, Mouse);
*)
    if (FindSel(Mouse) = Sel) and ButtonState(Sel) then
    begin
      Press(Sel);
      MovedTo(Sel);
      DrawView;
    end;
    ClearEvent(Event);
  end else if Event.What = evKeyDown then
  begin
    S := Sel;
    case CtrlToArrow(Event.KeyCode) of
      kbUp:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            Dec(S);
            if S < 0 then S := Strings.Count - 1;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
      kbDown:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            Inc(S);
            if S >= Strings.Count then S := 0;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
      kbRight:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            Inc(S,Size.Y);
            if S >= Strings.Count then
            begin
              S := (S+1) mod Size.Y;
              if S >= Strings.Count then S := 0;
            end;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
      kbLeft:
        if State and sfFocused <> 0 then
        begin
          I := 0;
          repeat
            Inc(I);
            if S > 0 then
            begin
              Dec(S, Size.Y);
              if S < 0 then
              begin
                S := ((Strings.Count + Size.Y - 1) div Size.Y)*Size.Y + S - 1;
                if S >= Strings.Count then S := Strings.Count-1;
              end;
            end else S := Strings.Count-1;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
        end;
    else
      begin
        for I := 0 to Strings.Count-1 do
        begin
          C := HotKey(PString(Strings.At(I))^);
          if (GetAltCode(C) = Event.KeyCode) or
             (((Owner^.Phase = phPostProcess) or (State and sfFocused <> 0))
               and (C <> #0) and (UpCase(Event.CharCode) = C)) then
          begin
            if ButtonState(I) then
            begin
              if Focus then
              begin
                Sel := I;
                MovedTo(Sel);
                Press(Sel);
                DrawView;
              end;
              ClearEvent(Event);
            end;
            Exit;
          end;
        end;
        if (Event.CharCode = ' ') and (State and sfFocused <> 0)
          and ButtonState(Sel) then
        begin
          Press(Sel);
          DrawView;
          ClearEvent(Event);
        end;
      end
    end
  end;
end;

procedure TCluster.SetButtonState(AMask: Longint; Enable: Boolean); assembler;
asm
        LES     DI,Self
        MOV     AX,AMask.Word[0]
        MOV     DX,AMask.Word[2]
        TEST    Enable,0FFH
        JNZ     @@1
        NOT     AX
        NOT     DX
        AND     ES:[DI].TCluster.EnableMask.Word[0],AX
        AND     ES:[DI].TCluster.EnableMask.Word[2],DX
        JMP     @@2
@@1:    OR      ES:[DI].TCluster.EnableMask.Word[0],AX
        OR      ES:[DI].TCluster.EnableMask.Word[2],DX
@@2:    MOV     CX,ES:[DI].Strings.TCollection.Count
        CMP     CX,32
        JA      @@6
        MOV     BX,ES:[DI].TCluster.Options
        AND     BX,not ofSelectable
        MOV     AX,ES:[DI].TCluster.EnableMask.Word[0]
        MOV     DX,ES:[DI].TCluster.EnableMask.Word[2]
@@3:    SHR     DX,1
        RCR     AX,1
        JC      @@4
        LOOP    @@3
        JMP     @@5
@@4:    OR      BX,ofSelectable
@@5:    MOV     ES:[DI].TCluster.Options,BX
@@6:
end;

procedure TCluster.SetData(var Rec);
begin
  Value := Word(Rec);
  DrawView;
end;

procedure TCluster.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if AState = sfFocused then DrawView;
end;

function TCluster.Mark(Item: Integer): Boolean;
begin
  Mark := False;
end;

function TCluster.MultiMark(Item: Integer): Byte;
begin
  MultiMark := Byte(Mark(Item) = True);
end;

procedure TCluster.MovedTo(Item: Integer);
begin
end;

procedure TCluster.Press(Item: Integer);
begin
end;

procedure TCluster.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(Value, SizeOf(Value));
  S.Write(Sel, SizeOf(Sel));
  S.Write(StepColumn, SizeOf(StepColumn)); {GIO}
  Strings.Store(S);
end;

function TCluster.Column(Item: Integer): Integer {SHIM};
var
  I, Col, Width, L, K: Integer;
begin
  K := Size.Y div StringHeight;
  if Item < K then Column := 0
  else
  begin
    Width := 0;
    Col := -StepColumn;            {GIO}
    for I := 0 to Item do
    begin
      if I mod K = 0 then
      begin
        Inc(Col, Width + StepColumn);  {GIO}
        Width := 0;
      end;
      if I < Strings.Count then
        L := CStrLen(PString(Strings.At(I))^);
      if L > Width then Width := L;
    end;
    Column := Col;
  end;
end;

function TCluster.FindSel(P: TPoint): Integer {SHIM};
var
  I, S, H: Integer;
  R: TRect;
begin
  GetExtent(R);
  if not R.Contains(P) then FindSel := -1 else
  begin
    H := StringHeight;
    I := 0;
    while P.X div CharWidth >= Column(I + Size.Y div H) do
      Inc(I, Size.Y div H);
    S := I + P.Y div H;
    if S >= Strings.Count then
      FindSel := -1 else
      FindSel := S;
  end;
end;

function TCluster.Row(Item: Integer): Integer;
begin
  Row := Item mod (Size.Y div StringHeight {SHIM});
end;

function TCluster.StringHeight: integer; {SHIM}
begin
  if Size.Y < FontHeight then
    StringHeight := Size.Y
  else
    StringHeight := FontHeight;
end;

{ TRadioButtons }

function TRadioButtons.StringHeight: integer; {SHIM}
var
  h: integer;
begin
  if Size.Y < FontHeight then
    StringHeight := Size.Y
  else begin
    h := BitMapWidth(@RadioButtonNoDef) + 1;
    if H < FontHeight then H := FontHeight;
    StringHeight := H;
  end;
end;

procedure TRadioButtons.Draw;
const
  Buttons : Array [0..3] of Pointer =
   (@RadioButtonNoDef, @RadioButtonYesDef, @RadioButtonNoDef, @RadioButtonYesDef);
var
  C : Char absolute Buttons;
begin
  DrawMultiBox('', C);
end;

function TRadioButtons.Mark(Item: Integer): Boolean;
begin
  Mark := Item = Value;
end;

procedure TRadioButtons.Press(Item: Integer);
begin
  Value := Item;
end;

procedure TRadioButtons.MovedTo(Item: Integer);
begin
  Value := Item;
end;

procedure TRadioButtons.SetData(var Rec);
begin
  TCluster.SetData(Rec);
  Sel := Integer(Value);
end;

{ TCheckBoxes }

function TCheckBoxes.StringHeight: integer; {SHIM}
var
  h: integer;
begin
  if Size.Y < FontHeight then
    StringHeight := Size.Y
  else begin
    h := BitMapWidth(@CheckBoxNoDef) + 1;
    if H < FontHeight then H := FontHeight;
    StringHeight := H;
  end;
end;

procedure TCheckBoxes.Draw;
const
  Buttons : Array [0..3] of Pointer =
   (@CheckBoxNoDef, @CheckBoxYesDef, @CheckBoxNoDef, @CheckBoxYesDef);
var
  C: Char absolute Buttons;
begin
  DrawMultiBox('', C);
end;

function TCheckBoxes.Mark(Item: Integer): Boolean;
begin
  Mark := Value and (1 shl Item) <> 0;
end;

procedure TCheckBoxes.Press(Item: Integer);
begin
  Value := Value xor (1 shl Item);
end;

{ TMultiCheckBoxes }

constructor TMultiCheckBoxes.Init(var Bounds: TRect; AStrings: PSItem;
  ASelRange: Byte; AFlags: Word; const AStates: String);
begin
  Inherited Init(Bounds, AStrings);
  SelRange := ASelRange;
  Flags := AFlags;
  States := NewStr(AStates);
end;

constructor TMultiCheckBoxes.Load(var S: TStream);
begin
  TCluster.Load(S);
  S.Read(SelRange, SizeOf(Byte));
  S.Read(Flags, SizeOf(Word));
  States := S.ReadStr;
end;

destructor TMultiCheckBoxes.Done;
begin
  DisposeStr(States);
  TCluster.Done;
end;

procedure TMultiCheckBoxes.Draw;
const
  Button = ' [ ] ';
begin
  DrawMultiBox(Button, States^);
end;

function TMultiCheckBoxes.DataSize: Word;
begin
  DataSize := SizeOf(Longint);
end;

function TMultiCheckBoxes.MultiMark(Item: Integer): Byte;
begin
  MultiMark := (Value shr (Word(Item) * WordRec(Flags).Hi))
    and WordRec(Flags).Lo;
end;

procedure TMultiCheckBoxes.GetData(var Rec);
begin
  Longint(Rec) := Value;
end;

procedure TMultiCheckBoxes.Press(Item: Integer);
var
  CurState: ShortInt;
begin
  CurState := (Value shr (Word(Item) * WordRec(Flags).Hi))
    and WordRec(Flags).Lo;

  Dec(CurState);
  if (CurState >= SelRange) or (CurState < 0) then
    CurState := SelRange - 1;
  Value := (Value and not (LongInt(WordRec(Flags).Lo)
    shl (Word(Item) * WordRec(Flags).Hi))) or
    (LongInt(CurState) shl (Word(Item) * WordRec(Flags).Hi));
end;

procedure TMultiCheckBoxes.SetData(var Rec);
begin
  Value := Longint(Rec);
  DrawView;
end;

procedure TMultiCheckBoxes.Store(var S: TStream);
begin
  TCluster.Store(S);
  S.Write(SelRange, SizeOf(Byte));
  S.Write(Flags, SizeOf(Word));
  S.WriteStr(States);
end;


constructor TListBox.Init(var Bounds: TRect; ANumCols: Word;
  AScrollBar: PScrollBar);
begin
  TListViewer.Init(Bounds, ANumCols, nil, AScrollBar);
  List := nil;
  SetRange(0);
end;

constructor TListBox.Load(var S: TStream);
begin
  TListViewer.Load(S);
  List := PCollection(S.Get);
end;

function TListBox.DataSize: Word;
begin
  DataSize := SizeOf(TListBoxRec);
end;

procedure TListBox.GetData(var Rec);
begin
  TListBoxRec(Rec).List := List;
  TListBoxRec(Rec).Selection := Focused;
end;

function TListBox.GetText(Item: Integer; MaxLen: Integer): String;
begin
  if List <> nil then GetText := PString(List^.At(Item))^
  else GetText := '';
end;

procedure TListBox.NewList(AList: PCollection);
begin
  if List <> nil then Dispose(List, Done);
  List := AList;
  if AList <> nil then SetRange(AList^.Count)
  else SetRange(0);
  if Range > 0 then FocusItem(0);
  DrawView;
end;

procedure TListBox.SetData(var Rec);
begin
  NewList(TListBoxRec(Rec).List);
  FocusItem(TListBoxRec(Rec).Selection);
  DrawView;
end;

procedure TListBox.Store(var S: TStream);
begin
  TListViewer.Store(S);
  S.Put(List);
end;

{ TStaticText }

constructor TStaticText.Init(var Bounds: TRect; const AText: String);
begin
  TView.Init(Bounds);
  Text := NewStr(AText);
end;

constructor TStaticText.Load(var S: TStream);
begin
  TView.Load(S);
  Text := S.ReadStr;
end;

destructor TStaticText.Done;
begin
  DisposeStr(Text);
  TView.Done;
end;

procedure TStaticText.Draw {SHIM, OOA};
var
  Color: word;
  Center: Boolean;
  Y, H, W, i, j, k, x: Integer;
  S, S1, S2: String;
  Fnt: Integer;
  PrevPos: Byte; {OOA}
  PrevWidth: Integer; {OOA}
begin
  Color := GetColor($0102);

  GetText(S);
  Y := 0;
  Center := False;
  Bar{Pattern}(0, 0, Size.X, Size.Y, Lo(Color));
  Fnt := Font.Font;

  i := 1;
  while true do begin
    {®¤­  áâà®ª }
    W := 0;
    S1 := '';
    H := FontHeight;
    PrevPos:= 1;
    PrevWidth:= 0;
    while (i <= Length(S)) and (S[i] <> #13)  do
    begin
      case S[i] of
        #3 : begin
               Center := True;
               Inc(i);
             end;
        #6 : begin
               Inc(i);
               S1 := S1 + #6 + S[i];
               H := Max(H, FontHeight);
               Font.Font := byte(S[i]);
               H := Max(H, FontHeight);
               Inc(i);
             end;
      else
        S1 := S1 + S[i];
        W := W + FontWidth(S[i]);
        if S[i] = ' ' then {OOA}
        begin
          PrevWidth:= W - FontWidth(' ');
          PrevPos:= I;
        end else if W > Size.X then
        begin
          System.Delete(S1, Length(S1) - (I - PrevPos), I - PrevPos + 1);
          W:= PrevWidth;
          I:= PrevPos + 1;
          {Inc(i); { ???? DK}
          {Dec(Byte(S1[0]));}
          Break;
        end;
        Inc(i);
      end;
    end;

    { „®áâ¨£­ãâ ª®­¥æ ®¤­®© áâà®ª¨ }
    if Center then x := (Size.X-W) div 2 else x := 0;
    j := 1; k := 1;
    while j<Length(S1) do begin
      if S1[j]=#6 then begin
        Inc(j);
        if j<>k then begin
          S2 := Copy(S1, k, j-k-1);
          WrStr(x, y + (H - FontHeight) div 2, S2, Hi(Color));
          Inc(x, FontWidth(S2));
        end;
        Font.Font := Byte(S1[j]);
        k := j+1;
      end;
      Inc(j);
    end;
    { ‚ë¢®¤ å¢®áâ¨ª  }
    S2 := Copy(S1, k, j-k+1);
    {if j<>k then} WrStr(x, y + (H - FontHeight) div 2, S2, Hi(Color));
    Inc(Y, H);
    if Y>Size.Y then break;
    if S[i]=#13 then begin
      Inc(i);
      if S[i]=#10 then Inc(i);
      Center := False; {OOA}
      Font.Font := Fnt; {OOA}
    end;
    if i>Length(S) then Break;
  end;
  Font.Font := Fnt;
end;

function TStaticText.GetPalette: PPalette;
const
  P: String[Length(CStaticText)] = CStaticText;
begin
  GetPalette := @P;
end;

procedure TStaticText.GetText(var S: String);
begin
  if Text <> nil then S := Text^
  else S := '';
end;

procedure TStaticText.Store(var S: TStream);
begin
  TView.Store(S);
  S.WriteStr(Text);
end;

{ TParamText }

constructor TParamText.Init(var Bounds: TRect; const AText: String;
  AParamCount: Integer);
begin
  TStaticText.Init(Bounds, AText);
  ParamCount := AParamCount;
end;

constructor TParamText.Load(var S: TStream);
begin
  TStaticText.Load(S);
  S.Read(ParamCount, SizeOf(Integer));
end;

function TParamText.DataSize: Word;
begin
  DataSize := ParamCount * SizeOf(Longint);
end;

procedure TParamText.GetText(var S: String);
begin
  if Text <> nil then FormatStr(S, Text^, ParamList^)
  else S := '';
end;

procedure TParamText.SetData(var Rec);
begin
  ParamList := @Rec;
  DrawView;
end;

procedure TParamText.Store(var S: TStream);
begin
  TStaticText.Store(S);
  S.Write(ParamCount, SizeOf(Integer));
end;

{ TLabel }

constructor TLabel.Init(var Bounds: TRect; const AText: String; ALink: PView);
begin
  TStaticText.Init(Bounds, AText);
  Link := ALink;
  Options := Options or (ofPreProcess + ofPostProcess);
  EventMask := EventMask or evBroadcast;
end;

constructor TLabel.Load(var S: TStream);
begin
  TStaticText.Load(S);
  GetPeerViewPtr(S, Link);
end;

procedure TLabel.Draw {SHIM};
var
  Color, Back: Word;
begin
  if Text = nil then Exit;
  if Light then begin
    Color := GetColor($0603);
    Back := GetColor($04);
  end else begin
    Color := GetColor($0501);
    Back := GetColor($02);
  end;
  Bar{Pattern}(0, 0, Size.X, Size.Y, Back);
  WrCStr(0, 0, Text^, Color);
end;

function TLabel.GetPalette: PPalette;
const
  P: String[Length(CLabel)] = CLabel;
begin
  GetPalette := @P;
end;

procedure TLabel.HandleEvent(var Event: TEvent);
var
  C: Char;

  procedure FocusLink;
  begin
    if (Link <> nil) and (Link^.Options and ofSelectable <> 0) then
      Link^.Focus;
    ClearEvent(Event);
  end;

begin
  TStaticText.HandleEvent(Event);
  if Event.What = evMouseDown then FocusLink
  else if Event.What = evKeyDown then
  begin
    C := HotKey(Text^);
    if (GetAltCode(C) = Event.KeyCode) or
       ((C <> #0) and (Owner^.Phase = phPostProcess) and
        (UpCase(Event.CharCode) = C)) then FocusLink
  end
  else if Event.What = evBroadcast then
    if ((Event.Command = cmReceivedFocus) or
       (Event.Command = cmReleasedFocus)) and
       (State and sfActive <> 0 {OOA}) and (Link <> nil) then
    begin
      Light := Link^.State and sfFocused <> 0;
      DrawView;
    end;
end;

procedure TLabel.Store(var S: TStream);
begin
  TStaticText.Store(S);
  PutPeerViewPtr(S, Link);
end;

{ THistoryViewer }

constructor THistoryViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; AHistoryId: Word);
begin
  TListViewer.Init(Bounds, 1, AHScrollBar, AVScrollBar);
  HistoryId := AHistoryId;
  SetRange(HistoryCount(AHistoryId));
  if Range > 1 then FocusItem(1);
  {HScrollBar^.SetRange(1, HistoryWidth- (Size.X div CharWidth) + 3);} {TONY}
end;

function THistoryViewer.GetPalette: PPalette;
const
  P: String[Length(CHistoryViewer)] = CHistoryViewer;
begin
  GetPalette := @P;
end;

function THistoryViewer.GetText(Item: Integer; MaxLen: Integer): String;
begin
  GetText := HistoryStr(HistoryId, Item);
end;

procedure THistoryViewer.HandleEvent(var Event: TEvent);
begin
  if ((Event.What = evMouseDown) and (Event.Double) and (Event.Buttons and mbLeftButton = mbLeftButton)) or
     ((Event.What = evKeyDown) and (Event.KeyCode = kbEnter)) then
  begin
    EndModal(cmOk);
    ClearEvent(Event);
  end else if ((Event.What = evKeyDown) and (Event.KeyCode = kbEsc)) or
    ((Event.What = evCommand) and (Event.Command = cmCancel)) then
  begin
    EndModal(cmCancel);
    ClearEvent(Event);
  end else TListViewer.HandleEvent(Event);
end;

function THistoryViewer.HistoryWidth: Integer;
var
  Width, T, Count, I: Integer;
begin
  Width := 0;
  Count := HistoryCount(HistoryId);
  for I := 0 to Count-1 do
  begin
    T := Length(HistoryStr(HistoryId, I));
    if T > Width then Width := T;
  end;
  HistoryWidth := Width;
end;

{ THistoryFrame } {TONY}
Type
  PHistoryFrame = ^THistoryFrame;
  THistoryFrame = object(TFrame)
    constructor Init(var Bounds: TRect);
    procedure   Draw;  virtual;
  end;

constructor THistoryFrame.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  UpBarHeight := 0;
  FrameWidth := 1;
end;

procedure THistoryFrame.Draw;
var
  CFrame, CTitle, CShadow, CBack: Word;
  fw3: integer;
begin
  if State and sfActive = sfActive then begin
    CFrame := $02;
    CTitle := $0506;
    CShadow := $0708;
  end else begin
    CFrame := $01;
    CTitle := $0304;
    CShadow := $0708;
  end;

  CFrame := GetColor(CFrame);
  CTitle := GetColor(CTitle);
  CShadow := GetColor(CShadow);
  CBack := GetColor($09);

  (*
  FrameBar(0, 0, Size.X, Size.Y, Hi(CShadow), Lo(CShadow), CFrame);
  Rectangle(FrameWidth, FrameWidth, Size.X - FrameWidth, Size.Y - FrameWidth, 1,
    Lo(CShadow), Hi(CShadow));
  Bar{Pattern}(FrameWidth + 1, FrameWidth + 1, Size.X - FrameWidth - 1, Size.Y - FrameWidth - 1, CBack);
  *)
  FrameBar(0, 0, Size.X, Size.Y, Hi(CShadow), Lo(CShadow), CBack);
  Rectangle(FrameWidth, FrameWidth, Size.X - FrameWidth, Size.Y - FrameWidth, 1,
    Lo(CShadow), Hi(CShadow));
  if FrameWidth > 1 then
    Rectangle(1, 1, Size.X - 1, Size.Y - 1, FrameWidth - 1, CFrame, CFrame);

  if FrameWidth > 2 then begin
    fw3 := FrameWidth * 3;
    HLine(0, fw3, FrameWidth, Lo(CShadow));
    HLine(0, fw3 + 1, FrameWidth, Hi(CShadow));
    HLine(Size.X - FrameWidth - 1, fw3, Size.X - 1, Lo(CShadow));
    HLine(Size.X - FrameWidth - 1, fw3 + 1, Size.X - 1, Hi(CShadow));

    HLine(0, Size.Y - fw3 - 2 - 1, FrameWidth, Lo(CShadow));
    HLine(0, Size.Y - fw3 - 2, FrameWidth, Hi(CShadow));
    HLine(Size.X - FrameWidth - 1, Size.Y - fw3 - 2 - 1, Size.X - 1, Lo(CShadow));
    HLine(Size.X - FrameWidth - 1, Size.Y - fw3 - 2, Size.X - 1, Hi(CShadow));

    VLine(fw3, 0, FrameWidth, Lo(CShadow));
    VLine(fw3 + 1, 0, FrameWidth, Hi(CShadow));
    VLine(Size.X - fw3 - 2, 0, FrameWidth, Lo(CShadow));
    VLine(Size.X - fw3 - 1, 0, FrameWidth, Hi(CShadow));

    VLine(fw3, Size.Y - FrameWidth, Size.Y, Lo(CShadow));
    VLine(fw3 + 1, Size.Y - FrameWidth, Size.Y, Hi(CShadow));
    VLine(Size.X - fw3 - 2, Size.Y - FrameWidth, Size.Y, Lo(CShadow));
    VLine(Size.X - fw3 - 1, Size.Y - FrameWidth, Size.Y, Hi(CShadow));
  end;

  if UpBarHeight <> 0 then begin {TONY}
    Bar{Pattern}(FrameWidth - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight - 1, CBack);
    if State and sfActive = sfActive then
      Rectangle(FrameWidth - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight - 1,
        1, Hi(CShadow), Lo(CShadow))
    else
      Rectangle(FrameWidth - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight - 1,
        1, Lo(CShadow), Hi(CShadow));
  end;
end;


{ THistoryWindow }

procedure THistoryWindow.InitFrame; {TONY}
var
  R: TRect;
begin
  GetExtent(R);
  Frame := New(PHistoryFrame, Init(R));
end;

constructor THistoryWindow.Init(var Bounds: TRect; HistoryId: Word);
begin
  TWindow.Init(Bounds, '', wnNoNumber);
  Flags := wfClose;
  InitViewer(HistoryId);
end;

function THistoryWindow.GetPalette: PPalette;
const
  P: String[Length(CHistoryWindow)] = CHistoryWindow;
begin
  GetPalette := @P;
end;

function THistoryWindow.GetSelection: String;
begin
  GetSelection := Viewer^.GetText(Viewer^.Focused,255);
end;

procedure THistoryWindow.InitViewer(HistoryId: Word); {TONY}
var
  R: TRect;
begin
  Frame^.GetClientExtent(R);
  Dec(R.B.X, 19); {   è¨à¨­ã Frame'  ¯«îá è¨à¨­ã áªà®««¡ à  }
  Viewer := New(PHistoryViewer, Init(R,
    Nil{StandardScrollBar(sbHorizontal + sbHandleKeyboard)},
    StandardScrollBar(sbVertical + sbHandleKeyboard),
    HistoryId));
  Insert(Viewer);
end;

{ THistory }

constructor THistory.Init(var Bounds: TRect; ALink: PInputLine;
  AHistoryId: Word);
begin
  Bounds.B.X := Bounds.A.X + BitMapWidth(@ComboDef);
  Bounds.B.Y := Bounds.A.Y + BitMapHeight(@ComboDef);
  TView.Init(Bounds);
  Options := Options or ofPostProcess;
  EventMask := EventMask or evBroadcast;
  Link := ALink;
  HistoryId := AHistoryId;
end;

constructor THistory.Load(var S: TStream);
begin
  TView.Load(S);
  GetPeerViewPtr(S, Link);
  S.Read(HistoryId, SizeOf(Word));
end;

procedure THistory.Draw {TONY!!!};
begin
  Bar{Pattern}(0, 0, Size.X, Size.Y, GetColor($01));
  PutBMP(@ComboDef, 0, 0);
end;

function THistory.GetPalette: PPalette;
const
  P: String[Length(CHistory)] = CHistory;
begin
  GetPalette := @P;
end;

procedure THistory.HandleEvent(var Event: TEvent);  {TONY}
var
  HistoryWindow: PHistoryWindow;
  R,P: TRect;
  C: Word;
  Rslt: String;
begin
  TView.HandleEvent(Event);
  if (
     ((Event.What = evMouseDown) and (Event.Buttons and mbLeftButton = mbLeftButton)) or
     ((Event.What = evKeyDown) and (CtrlToArrow(Event.KeyCode) = kbDown) and
      (Link^.State and sfFocused <> 0))
     ) or ( {SHIM!}
       (Event.What = evCommand) and (Event.Command = cmExecHistory) and (Event.InfoPtr = Link)
     )
  then begin
    if not Link^.Focus then
    begin
      ClearEvent(Event);
      Exit;
    end;
    RecordHistory(Link^.Data^);
    Link^.GetBounds(R);
    Inc(R.B.Y, 15*FontHeight div 2);
    Inc(R.B.X, Size.X+3);
    Owner^.GetExtent(P);
    R.Move(0, Size.Y);
    R.Intersect(P);
    HistoryWindow := InitHistoryWindow(R);
    if HistoryWindow <> nil then
    begin
      C := Owner^.ExecView(HistoryWindow);
      if C = cmOk then
      begin
        Rslt := HistoryWindow^.GetSelection;
        if Length(Rslt) > Link^.MaxLen then Rslt[0] := Char(Link^.MaxLen);
        Link^.Data^ := Rslt;
        Link^.SelectAll(True);
        Link^.DrawView;
        Message(Link^.Owner, evCommand, cmDropHistory , @Self); {GIO}
      end;
      Dispose(HistoryWindow, Done);
    end;
    ClearEvent(Event);
  end
  else if (Event.What = evBroadcast) then
    if ((Event.Command = cmReleasedFocus) and (Event.InfoPtr = Link))
      or (Event.Command = cmRecordHistory) then
    RecordHistory(Link^.Data^);
end;

function THistory.InitHistoryWindow(var Bounds: TRect): PHistoryWindow;
var
  P: PHistoryWindow;
begin
  P := New(PHistoryWindow, Init(Bounds, HistoryId));
  P^.HelpCtx := Link^.HelpCtx;
  InitHistoryWindow := P;
end;

procedure THistory.RecordHistory(const S: String);
begin
  HistoryAdd(HistoryId, S);
end;

procedure THistory.Store(var S: TStream);
begin
  TView.Store(S);
  PutPeerViewPtr(S, Link);
  S.Write(HistoryId, SizeOf(Word));
end;

{SHIMON Additions!}

constructor TShade.Init(var Bounds: TRect; AColor, AShadow, ABMP, APal: Word; AStyle: shStyles);
begin
  inherited Init(Bounds);
  Color := AColor;
  Shadow := AShadow;
  Style := AStyle;
  if Style = shBarPattern then
    SetPaint(Lo(Color), Hi(Color), CopyPut, 0, 1, lsPattern, shpUser);
  if Style in [shBarBMP, shBitmap, shStretch] then begin
    PaintInfo.BitMap := GetImage(ABMP);
    if RetrievePalette(APal) <> @StdLogPalette then begin
      LogPalette := RetrievePalette(APal)^;
      LogPalette.Mode := LogPalette.Mode or pmUseRGB or pmOptimize;
      PaintInfo.ColorRef := LogPalette.ColorRef;
    end;
  end;
  GrowMode := gfGrowHiX + gfGrowHiY;
End;

constructor TShade.Load(var S: TStream);
Var
  I : Word;
begin
  inherited Load(S);
  S.Read(Color, sizeof(Color));
  S.Read(Shadow, sizeof(Shadow));
  S.Read(Style, sizeof(Style));
  if Style = shBarPattern then S.Read(PaintInfo.Pattern, sizeof(PaintInfo.Pattern));
  if Style in [shBarBMP, shBitmap, shStretch]  then begin
    S.Read(I, 2);
    PaintInfo.Bitmap := GetImage(I);
  end;
end;

procedure TShade.Store(var S: TStream);
Var
  I : Word;
begin
  inherited Store(S);
  S.Write(Color, sizeof(Color));
  S.Write(Shadow, sizeof(Shadow));
  S.Write(Style, sizeof(Style));
  if Style = shBarPattern then S.Write(PaintInfo.Pattern, sizeof(PaintInfo.Pattern));
  if Style in [shBarBMP, shBitmap, shStretch]   then begin
    I := GetImageID(PaintInfo.Bitmap);
    S.Write(I, 2);
  end;
end;

procedure TShade.Draw;
var
  x, y: Integer;
begin
  case Style of
    shBar   : begin
      FrameBar(0, 0, Size.X, Size.Y, Hi(Shadow), Lo(Shadow), Color);
    end;
    shFrame : begin
      Bar(0, 0, Size.X, Size.Y, Color);
      Rectangle(1, 1, Size.X, Size.Y, 1, Hi(Shadow), Hi(Shadow));
      Rectangle(0, 0, Size.X - 1, Size.Y - 1, 1, Lo(Shadow), Lo(Shadow));
    end;
    shBarPattern: begin
      SetPaint(Lo(Color),Hi(Color),CopyPut,0,1, lsPattern, fsNoUse);
      BarStyle(0, 0, Size.X, Size.Y);
      Rectangle(1, 1, Size.X, Size.Y, 1, Hi(Shadow), Hi(Shadow));
      Rectangle(0, 0, Size.X - 1, Size.Y - 1, 1, Lo(Shadow), Lo(Shadow));
    end;
    shBarBMP: begin
      SetPaint(Lo(Color),Hi(Color),CopyPut,0,1, lsBitmap, fsNoUse);
      BarStyle(0, 0, Size.X, Size.Y);
      Rectangle(1, 1, Size.X, Size.Y, 1, Hi(Shadow), Hi(Shadow));
      Rectangle(0, 0, Size.X - 1, Size.Y - 1, 1, Lo(Shadow), Lo(Shadow));
    end;
    shBitmap: if BitMapWidth(PaintInfo.Bitmap) > 0 then begin
      y := 0; x := 0;
      while y < Size.Y do begin
        while x < Size.X do begin
          PutBMP(PaintInfo.Bitmap, x, y);
          Inc(x, BitMapWidth(PaintInfo.Bitmap));
        end;
        Inc(y, BitMapHeight(PaintInfo.Bitmap));
        x := 0;
      end;
      Rectangle(1, 1, Size.X, Size.Y, 1, Hi(Shadow), Hi(Shadow));
      Rectangle(0, 0, Size.X - 1, Size.Y - 1, 1, Lo(Shadow), Lo(Shadow));
    end;
    shStretch: if BitMapWidth(PaintInfo.Bitmap) > 0 then begin
      StretchBMP(PaintInfo.Bitmap, 0, 0, Size.X, Size.Y);
      Rectangle(1, 1, Size.X, Size.Y, 1, Hi(Shadow), Hi(Shadow));
      Rectangle(0, 0, Size.X - 1, Size.Y - 1, 1, Lo(Shadow), Lo(Shadow));
    end;
  end;
end;

{ TStaticBitMap }  {TONY}

constructor TStaticBitMap.Init;
begin
  inherited Init(Bounds);
  BitMap := ABitMap;
  if APalette <> Nil then LogPalette := APalette^;
  if CalcSize then begin
    Size.X := BitMap^.Width;
    Size.Y := BitMap^.Height;
  end;
end;

constructor TStaticBitMap.Load; {GIO}
var ID: Word;
begin Inherited Load(S);
  S.Read(ID, SizeOf(ID));
  BitMap:=New(PBitMap, Init(ID));
end;

procedure TStaticBitMap.Store; {GIO}
var ID: Word;
begin Inherited Store(S);
  ID:=BitMap^.ImageID;
  S.Write(ID, SizeOf(ID));
end;

destructor TStaticBitMap.Done;
begin
  Dispose(BitMap, Done); {TONY}
  inherited Done;
end;


procedure TStaticBitMap.Draw;
begin
  PutBMP(BitMap^.ToDraw, 0, 0);
end;

{TStaticOperationalBitMap}
constructor TOperationalBitmap.Init;
begin
  inherited Init( Bounds, ABitMap, APalette, CalcSize );
  Options:=Options or ofTransparent;
  Operation := AOperation;
end;

procedure TOperationalBitMap.Draw;
begin
  PutBMPOp(BitMap^.ToDraw, 0, 0, Operation);
end;

{ TStaticIcon } {TONY}
constructor TStaticIcon.Init(var Bounds: TRect; ABitMapXor, ABitMapAnd: PBitMap; CalcSize: boolean);
begin
  inherited Init( Bounds, ABitMapXor, Nil, CalcSize );
  Options:=Options or ofTransparent;
  BitMapAnd:= ABitMapAnd;
end;

destructor TStaticIcon.Done;
begin
  Dispose(BitMapAnd, Done);
  inherited Done;
end;

procedure TStaticIcon.Draw;
const
  DrawAnd = 2;
  DrawXor = 1;
begin
  PutBMPOp(BitMapAnd^.ToDraw, 0, 0, DrawAnd);
  PutBMPOp(BitMap^.ToDraw, 0, 0, DrawXor);
end;


{$L _CHECKS.OBJ}
procedure CheckBoxNoDef; external;
procedure CheckBoxYesDef; external;
procedure RadioButtonNoDef; external;
procedure RadioButtonYesDef; external;

{procedure ComboBoxDef; external;
procedure ComboBoxPress; external;}


procedure RegisterCheckBoxImages;
begin
  RegisterImageInCode(101, @CheckBoxNoDef);
  RegisterImageInCode(103, @CheckBoxYesDef);
end;

procedure RegisterRadioButtonImages;
begin
  RegisterImageInCode(105, @RadioButtonNoDef);
  RegisterImageInCode(107, @RadioButtonYesDef);
end;

{procedure RegisterComboBoxImages;
begin
  RegisterImageInCode(109, @ComboBoxDef);
  RegisterImageInCode(110, @ComboBoxPress);
end;}


{$IFDEF UseStandardBitMaps}
{$IFDEF Russian}
   {$L _BUTTONR.OBJ}
{$ELSE}
  {$IFDEF Ukrainian}
    {$L _BUTTONR.OBJ}
  {$ELSE}
    {$L _BUTTONS.OBJ}
  {$ENDIF}
{$ENDIF}

{ $L OKBTNS.OBJ}
{procedure Btn_OK_Selected; external;}
procedure Btn_OK_Normal;   external;
procedure Btn_OK_Pressed;  external;
procedure Btn_OK_Default;  external;

{ $L CANCBTNS.OBJ}
procedure Btn_Cancel_Default; external;
{procedure Btn_Cancel_Selected;external;}
procedure Btn_Cancel_Normal;  external;
procedure Btn_Cancel_Pressed; external;

{ $L YESBTNS.OBJ}
{procedure Btn_Yes_Selected;   external;}
procedure Btn_Yes_Normal;     external;
procedure Btn_Yes_Pressed;    external;
procedure Btn_Yes_Default;    external;

{ $L NOBTNS.OBJ}
{procedure Btn_No_Selected;   external;}
procedure Btn_No_Normal;     external;
procedure Btn_No_Pressed;    external;
procedure Btn_No_Default;    external;

{ $L EXITBTNS.OBJ
procedure Btn_Exit_Selected;   external;
procedure Btn_Exit_Normal;     external;
procedure Btn_Exit_Pressed;    external;
procedure Btn_Exit_Default;    external;}

procedure RegisterBtn_OK_Images;
begin
  RegisterImageInCode(151, @Btn_OK_Normal);
  RegisterImageInCode(152, @Btn_OK_Default);
 { RegisterImageInCode(153, @Btn_OK_Selected);}
  RegisterImageInCode(154, @Btn_OK_Pressed);
end;

procedure RegisterBtn_Cancel_Images;
begin
  RegisterImageInCode(155, @Btn_Cancel_Normal);
  RegisterImageInCode(156, @Btn_Cancel_Default);
{  RegisterImageInCode(157, @Btn_Cancel_Selected);}
  RegisterImageInCode(158, @Btn_Cancel_Pressed);
end;

procedure RegisterBtn_Yes_Images;
begin
  RegisterImageInCode(159, @Btn_Yes_Normal);
  RegisterImageInCode(160, @Btn_Yes_Default);
{  RegisterImageInCode(161, @Btn_Yes_Selected);}
  RegisterImageInCode(162, @Btn_Yes_Pressed);
end;

procedure RegisterBtn_No_Images;
begin
  RegisterImageInCode(163, @Btn_No_Normal);
  RegisterImageInCode(164, @Btn_No_Default);
{  RegisterImageInCode(165, @Btn_No_Selected);}
  RegisterImageInCode(166, @Btn_No_Pressed);
end;

(*procedure RegisterBtn_Exit_Images;
begin
  RegisterImageInCode(167, @Btn_Exit_Normal);
  RegisterImageInCode(168, @Btn_Exit_Default);
  RegisterImageInCode(169, @Btn_Exit_Selected);
  RegisterImageInCode(170, @Btn_Exit_Pressed);
end;}

procedure RegisterBtn_Help_Images;
begin
  RegisterImageInCode(171, @Btn_Help_Normal);
  RegisterImageInCode(172, @Btn_Help_Default);
{  RegisterImageInCode(173, @Btn_Help_Selected);}
  RegisterImageInCode(174, @Btn_Help_Pressed);
end;

procedure RegisterBtn_Abort_Images;
begin
  RegisterImageInCode(175, @Btn_Abort_Normal);
  RegisterImageInCode(176, @Btn_Abort_Default);
{  RegisterImageInCode(177, @Btn_Help_Selected);}
  RegisterImageInCode(178, @Btn_Abort_Pressed);
end;

procedure RegisterBtn_Retry_Images;
begin
  RegisterImageInCode(179, @Btn_Retry_Normal);
  RegisterImageInCode(180, @Btn_Retry_Default);
{  RegisterImageInCode(181, @Btn_Retry_Selected);}
  RegisterImageInCode(182, @Btn_Retry_Pressed);
end;

procedure RegisterBtn_Ignore_Images;
begin
  RegisterImageInCode(183, @Btn_Ignore_Normal);
  RegisterImageInCode(184, @Btn_Ignore_Default);
{  RegisterImageInCode(185, @Btn_Retry_Selected);}
  RegisterImageInCode(186, @Btn_Ignore_Pressed);
end;*)

function MakeOKButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
var V: PBmpButton;
begin
  V := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    151, 152, 152, 154, 151));
  V^.HelpCtx:=hcOkButton;
  MakeOKButton := V;
end;

function MakeCancelButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
var V: PBmpButton;
begin
  V := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    155, 156, 156, 158, 155));
  V^.HelpCtx:=hcCancelButton;
  MakeCancelButton := V;
end;

function MakeYesButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeYesButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    159, 160, 160, 162, 159));
end;

function MakeNoButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeNoButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    163, 164, 164, 166, 163));
end;

{function MakeExitButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeExitButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    167, 168, 178, 170, 167));
end;}

{function MakeHelpButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeHelpButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    171, 172, 172, 174, 171));
end;

function MakeAbortButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeAbortButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    175, 176, 176, 178, 175));
end;

function MakeRetryButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeRetryButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    179, 180, 180, 182, 179));
end;
function MakeIgnoreButton(var Bounds: TRect; ACommand: Word; AFlags: Word): PBmpButton;
begin
  MakeIgnoreButton := New(PBmpButton, Init(Bounds, ACommand, AFlags,
    183, 184, 184, 186, 184));
end;}

{$ENDIF}

procedure RegisterDialogsImages;
begin
  RegisterCheckBoxImages;
  RegisterRadioButtonImages;
  {RegisterComboBoxImages;}
  {$IFDEF UseStandardBitMaps}
  RegisterBtn_OK_Images;
  RegisterBtn_Cancel_Images;
  RegisterBtn_Yes_Images;
  RegisterBtn_No_Images;
  {RegisterBtn_Exit_Images;
  RegisterBtn_Help_Images;
  RegisterBtn_Abort_Images;
  RegisterBtn_Retry_Images;
  RegisterBtn_Ignore_Images;}
  {$ENDIF}
end;

{ Dialogs registration procedure }

procedure RegisterDialogs;
begin
  RegisterType(RDialog);
  RegisterType(RInputLine);
  RegisterType(RButton);
  RegisterType(RCluster);
  RegisterType(RRadioButtons);
  RegisterType(RCheckBoxes);
  RegisterType(RMultiCheckBoxes);
  RegisterType(RListBox);
  RegisterType(RStaticText);
  RegisterType(RLabel);
  RegisterType(RHistory);
  RegisterType(RParamText);
  {$IFDEF UseStandardBitMaps}
  RegisterType(RBmpButton);
  {$ENDIF}
  RegisterType(RShade);
  RegisterType(RStaticBitMap);
end;

begin
  RegisterDialogsImages; {??? OOA}
end.
