
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Views;

{.$DEFINE DEBUG}

{$O+,F+,X+,I-,S-,G+}

interface

uses Objects, Drivers, Memory,
     GDI, EGFont, EGInline, EGCursor, BitMaps {TONY}, Palettes{DK};

const

{ TView State masks }

  sfVisible     = $0001;
  sfCursorVis   = $0002;
  sfCursorIns   = $0004;
  sfShadow      = $0008;
  sfActive      = $0010;
  sfSelected    = $0020;
  sfFocused     = $0040;
  sfDragging    = $0080;
  sfDisabled    = $0100;
  sfModal       = $0200;
  sfDefault     = $0400;
  sfExposed     = $0800;
  sfIconized    = $1000;

{ TView Option masks }

  ofSelectable  = $0001;
  ofTopSelect   = $0002;
  ofFirstClick  = $0004;
  ofFramed      = $0008;
  ofPreProcess  = $0010;
  ofPostProcess = $0020;
  ofBuffered    = $0040;
  ofTileable    = $0080;
  ofCenterX     = $0100;
  ofCenterY     = $0200;
  ofCentered    = $0300;
  ofValidate    = $0400;
  ofTransparent = $0800; {TONY}
  ofIconizable  = $4000; {TONY, OOA}
  ofVersion     = $3000;
  ofVersion10   = $0000;
  ofVersion20   = $1000;

{ TView GrowMode masks }

  gfGrowLoX = $01;
  gfGrowLoY = $02;
  gfGrowHiX = $04;
  gfGrowHiY = $08;
  gfGrowAll = $0F;
  gfGrowRel = $10;

{ TView DragMode masks }

  dmDragMove = $01;
  dmDragGrow = $02;
  dmLimitLoX = $10;
  dmLimitLoY = $20;
  dmLimitHiX = $40;
  dmLimitHiY = $80;
  dmLimitAll = $F0;

{ TView DragView modes }

  dsLeft       = $0100;
  dsRight      = $0200;
  dsUpper      = $0400;
  dsUpperLeft  = $0500;
  dsUpperRight = $0600;
  dsLower      = $0800;
  dsLowerLeft  = $0900;
  dsLowerRight = $0A00;
  dsDrag       = $0F00;

{ TView Help context codes }

  hcNoContext = 0;
  hcDragging  = 1;

{ TView PalMode modes }
  pmUseRGB    = $001;   {¢™´ÓÁ†•‚·Ô ¢ Ø‡ÆÊ•·· Ø•‡•‡®·Æ¢™® Ø‡® c¨•≠• Ø†´®‚‡Î}
  pmOptimize  = $002;   {¢™´ÓÁ†•‚·Ô ¢ Ø‡ÆÊ•·· ÆØ‚®¨®ß†Ê®® Ê¢•‚Æ¢}
  pmDirectBM  = $004;   {Æ‚™´ÓÁ•≠ †¢‚Æ ¢Î¢Æ§ BMP Á•‡•ß ColorRef}
  pmMonopoly  = $008;   {ß†Ø‡•‚ ≠† §•´•≠®• Ê¢•‚Æ¢ · §‡„£®¨® Ø†´®‚‡†¨®}
  pmInHeap    = $010;   {Ø†´®‚‡† ¢ ™„Á• † ≠• ¢ ™Æ§•}
  pmEnable    = $020;   {®·ØÆ´Ïß„•‚·Ô ¢≠„‚‡® - ≠• ·‚†¢®‚Ï}
  pmDrawFirst = $040;   {®·ØÆ´Ïß„•‚·Ô ¢≠„‚‡® - ≠• ·‚†¢®‚Ï}
  pmKeepFocus = $080;   {•·´® ØÆ´„Á†•‚ ‰Æ™„· Ø•‡•§•´Î¢†•‚ Ø†´®‚‡„}
  pmHiColor   = $100;   {‰Æ‡·®‡„•‚ ®·ØÆ´ÏßÆ¢†≠®• ·‚†‡Ë®Â ¨Æ§•´•© Ø†´®‚‡Î}
  pmComplex   = $200;   {„·‚†≠Æ¢®‚Ï §´Ô ·¨•Ë†≠≠Æ£Æ ‡®·Æ¢†≠®Ô - °®‚¨†‡ · Ø†´®‚‡Æ©/‚•™·‚ ® Ø‡.}

{ TScrollBar part codes }

  sbLeftArrow  = 0;
  sbRightArrow = 1;
  sbPageLeft   = 2;
  sbPageRight  = 3;
  sbUpArrow    = 4;
  sbDownArrow  = 5;
  sbPageUp     = 6;
  sbPageDown   = 7;
  sbIndicator  = 8;

{ TScrollBar options for TWindow.StandardScrollBar }

  sbHorizontal     = $0000;
  sbVertical       = $0001;
  sbHandleKeyboard = $0002;

{ TWindow Flags masks }

  wfMove  = $01;
  wfGrow  = $02;
  wfClose = $04;
  wfZoom  = $08;

{ TWindow number constants }

  wnNoNumber = 0;

{ TWindow palette entries }

  wpBlueWindow = 0;
  wpCyanWindow = 1;
  wpGrayWindow = 2;

{ Standard command codes }

  cmValid   = 0;
  cmQuit    = 1;
  cmError   = 2;
  cmMenu    = 3;
  cmClose   = 4;
  cmZoom    = 5;
  cmResize  = 6;
  cmNext    = 7;
  cmPrev    = 8;
  cmHelp    = 9;

{ Application command codes }

  cmCut     = 20;
  cmCopy    = 21;
  cmPaste   = 22;
  cmUndo    = 23;
  cmClear   = 24;
  cmTile    = 25;
  cmCascade = 26;

{ TDialog standard commands }

  cmOK      = 10;
  cmCancel  = 11;
  cmYes     = 12;
  cmNo      = 13;
  cmDefault = 14;

{ Standard messages }

  cmReceivedFocus     = 50;
  cmReleasedFocus     = 51;
  cmCommandSetChanged = 52;

{ TScrollBar messages }

  cmScrollBarChanged  = 53;
  cmScrollBarClicked  = 54;

{ TWindow select messages }

  cmSelectWindowNum   = 55;

{ TListViewer messages }

  cmListItemSelected  = 56;

{ TScreenSaver messages }{TONY}

  cmScreenSave    = 990;
  cmScreenRestore = 991;

{ Window Iconization Messages }{TONY}
  cmCheckIconPosition = 992;
  cmWindowRestored    = 993;
  cmWindowIconized    = 994;
  cmRedrawIcons       = 995; {DK}
  cmResetMouseCursor  = 996; {DK}


{ Color palettes }

{                    …ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕª }
{ CFrame             ∫ 1 ≥ 2 ≥ 3 ≥ 4 ≥ 5 ≥ 6 ≥ 7 ≥ 8 ≥ 9 ∫ }
{                    »Õ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—Õº }
{ Passive Frame back ƒƒŸ   ≥   ≥   ≥   ≥   ≥   ≥   ≥   ≥   }
{ Active  Frame back ƒƒƒƒƒƒŸ   ≥   ≥   ≥   ≥   ≥   ≥   ≥   }
{ Passive Title fore ƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   ≥   ≥   ≥   ≥   }
{               back ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   ≥   ≥   ≥   }
{ Active  Title fore ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   ≥   ≥   }
{               back ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   ≥   }
{ 3D  bright ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   }
{     dark ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   }
{ Window background ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   }

  CFrame      = #1#2#3#4#5#6#7#8#9;

{            …ÕÕÕ—ÕÕÕ—ÕÕÕª          }
{ CScrollBar ∫ 1 ≥ 2 ≥ 3 ∫          }
{            »Õ—ÕœÕ—ÕœÕ—Õº          }
{ Page back ƒƒƒŸ   ≥   ¿ƒ 3D dark   }
{                  ¿ƒƒƒƒƒ 3D bright }

  CScrollBar  = #9#8#7;

{                …ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕª }
{ CScroller      ∫ 1 ≥ 2 ≥ 3 ≥ 4 ∫ }
{                »Õ—ÕœÕ—ÕœÕ—ÕœÕ—Õº }
{ Normal Text fore Ÿ   ≥   ≥   ≥   }
{             back ƒƒƒƒŸ   ≥   ≥   }
{ Selected Text fore ƒƒƒƒƒƒŸ   ≥   }
{               back ƒƒƒƒƒƒƒƒƒƒŸ   }

  CScroller   = #10#11#12#13;

{                …ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕ—ÕÕÕª}
{ CListViewer    ∫ 1 ≥ 2 ≥ 3 ≥ 4 ≥ 5 ≥ 6 ≥ 7 ≥ 8 ∫}
{                »Õ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—ÕœÕ—Õº}
{ Normal Text fore Ÿ   ≥   ≥   ≥   ≥   ≥   ≥   ≥  }
{             back ƒƒƒƒŸ   ≥   ≥   ≥   ≥   ≥   ≥  }
{ Selected Text fore ƒƒƒƒƒƒŸ   ≥   ≥   ≥   ≥   ≥  }
{               back ƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   ≥   ≥  }
{ Focused  Text fore ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥   ≥  }
{               back ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥   ≥  }
{ Divider 3D  bright ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ   ≥  }
{               dark ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ  }

  CListViewer = #10#11#12#13#14#15#16#17;

(*

 CWindow

           CFrame                 CScrollBar
           ------------------     ------------------
 …ÕÕÕÕª
 ∫  1 ∫    Passive back
 «ƒƒƒƒ∂
 ∫  2 ∫    Active Back
 «ƒƒƒƒ∂
 ∫  3 ∫    Passive Title fore
 «ƒƒƒƒ∂
 ∫  4 ∫    Passive Title back
 «ƒƒƒƒ∂
 ∫  5 ∫    Active Title fore
 «ƒƒƒƒ∂
 ∫  6 ∫    Active Title back
 «ƒƒƒƒ∂
 ∫  7 ∫    3D bright              3D dark
 «ƒƒƒƒ∂
 ∫  8 ∫    3D dark                3D bright
 «ƒƒƒƒ∂
 ∫  9 ∫    Window background      Page background
 »ÕÕÕÕº

           CScroller              CListViewer
           -------------------    ---------------------
 …ÕÕÕÕª
 ∫  A ∫    Normal text fore       Normal text fore
 «ƒƒƒƒ∂
 ∫  B ∫    Normal text back       Normal text back
 «ƒƒƒƒ∂
 ∫  C ∫    Selected text fore     Selected text fore
 «ƒƒƒƒ∂
 ∫  D ∫    Selected text back     Selected text back
 «ƒƒƒƒ∂
 ∫  E ∫                           Focused text fore
 «ƒƒƒƒ∂
 ∫  F ∫                           Focused text back
 «ƒƒƒƒ∂
 ∫ 10 ∫                           Divider 3D bright
 «ƒƒƒƒ∂
 ∫ 11 ∫                           Divider 3D dark
 »ÕÕÕÕº


           CStaticText
           ------------------
 …ÕÕÕÕª
 ∫ 12 ∫    Text fore
 «ƒƒƒƒ∂
 ∫ 13 ∫    Text back
 »ÕÕÕÕº

           CLabel
           ------------------
 …ÕÕÕÕª
 ∫ 14 ∫    Text Normal fore
 «ƒƒƒƒ∂
 ∫ 15 ∫    Text Normal back
 «ƒƒƒƒ∂
 ∫ 16 ∫    Text Selected fore
 «ƒƒƒƒ∂
 ∫ 17 ∫    Text Selected back
 «ƒƒƒƒ∂
 ∫ 18 ∫    Shortcut Normal
 «ƒƒƒƒ∂
 ∫ 19 ∫    Shortcut Selected
 »ÕÕÕÕº

           CInputLine             CHistory
           ------------------     ---------------
 …ÕÕÕÕª
 ∫ 1A ∫    Passive fore
 «ƒƒƒƒ∂
 ∫ 1B ∫    Passive back           back
 «ƒƒƒƒ∂
 ∫ 1C ∫    Active fore
 «ƒƒƒƒ∂
 ∫ 1D ∫    Active back
 «ƒƒƒƒ∂
 ∫ 1E ∫    Selected fore
 «ƒƒƒƒ∂
 ∫ 1F ∫    Selected back
 «ƒƒƒƒ∂
 ∫ 20 ∫    Arrow
 »ÕÕÕÕº

           CButton
 …ÕÕÕÕª    ---------------
 ∫ 21 ∫    Normal fore
 «ƒƒƒƒ∂
 ∫ 22 ∫    Normal back
 «ƒƒƒƒ∂
 ∫ 23 ∫    Default fore
 «ƒƒƒƒ∂
 ∫ 24 ∫    Default back
 «ƒƒƒƒ∂
 ∫ 25 ∫    Selected fore
 «ƒƒƒƒ∂
 ∫ 26 ∫    Selected back
 «ƒƒƒƒ∂
 ∫ 27 ∫    Disabled fore
 «ƒƒƒƒ∂
 ∫ 28 ∫    Disabled back
 «ƒƒƒƒ∂
 ∫ 29 ∫    Shortcut Normal
 «ƒƒƒƒ∂
 ∫ 2A ∫    Shortcut Default
 «ƒƒƒƒ∂
 ∫ 2B ∫    Shortcut Selected
 «ƒƒƒƒ∂
 ∫ 2C ∫    3D bright
 «ƒƒƒƒ∂
 ∫ 2D ∫    3D dark
 »ÕÕÕÕº

           CCluster
           -------------------
 …ÕÕÕÕª
 ∫ 2E ∫    Text Normal fore
 «ƒƒƒƒ∂
 ∫ 2F ∫    Text Normal back
 «ƒƒƒƒ∂
 ∫ 30 ∫    Text Selected fore
 «ƒƒƒƒ∂
 ∫ 31 ∫    Text Selected back
 «ƒƒƒƒ∂
 ∫ 32 ∫    Text Disabled fore
 «ƒƒƒƒ∂
 ∫ 33 ∫    Text Disabled back
 «ƒƒƒƒ∂
 ∫ 34 ∫    Shortcut Normal
 «ƒƒƒƒ∂
 ∫ 35 ∫    Shortcut Selected
 «ƒƒƒƒ∂
 ∫ 36 ∫    Text Normal Current
 »ÕÕÕÕº

           CHistoryWindow
           --------------

   36      Background

   37      Title fore

   38      3D bright

   39      3D dark

   3A      Normal text fore

   3B      Normal text back

   3C      Focused text fore

   3D      Focused text back


           CEditor
           -----------------

   3E      Normal fore

   3F      Normal back

   40      Highlight fore

   41      Highlight back


           CInfoPane
           ------------------

   42      Fore

   43      Back


*)


  CBlueWindow = #14#15#16#17#18#19#20#21#22#23 +
                #24#25#26#27#28#29#30#31#32#33 +
                #34#35#36#37#38#39#40#41#42#43 +
                #44#45#46#47#48#49#50#51#52#53 +
                #54#55#56#57#58#59#60#61#62#63 +
                #64#65#66#67#68#69#70#71#72#73 +
                #74#75#76#77#78#79#80#81;

  CCyanWindow = #82#83#84#85#86#87#88#89#90#91 +
                #92#93#94#95#96#97#98#99#100#101 +
                #102#103#104#105#106#107#108#109#110#111 +
                #112#113#114#115#116#117#118#119#120#121 +
                #122#123#124#125#126#127#128#129#130#131 +
                #132#133#134#135#136#137#138#139#140#141 +
                #142#143#144#145#146#147#148#149;

  CGrayWindow = #150#151#152#153#154#155#156#157#158#159 +
                #160#161#162#163#164#165#166#167#168#169 +
                #170#171#172#173#174#175#176#177#178#179 +
                #180#181#182#183#184#185#186#187#188#189 +
                #190#191#192#193#194#195#196#197#198#198 +
                #200#201#202#203#204#205#206#207#208#209 +
                #210#211#212#213#214#215#216#217;

{ Maximum view width }

  MaxViewWidth = 132 * 8;

  GlobalFont  : TFont = (Font:1; WIdth:8; Height:16) {TONY};
  TitleFont   : TFont = (Font:1; WIdth:8; Height:16) {TONY};
  StatusFont  : TFont = (Font:1; WIdth:8; Height:16) {TONY};
  MenusFont   : TFont = (Font:1; WIdth:8; Height:16) {TONY};
  IconsFont   : TFont = (Font:1; WIdth:8; Height:16) {TONY, OOA};
  IconsSpacing: Integer = 8 * 8 {TONY, OOA};
  IconsOffset : Integer = 8; {TONY}

function HiNibble(X: Byte): Byte {Shim};
inline(
   $58/        {pop ax}
   $C1/$E8/$04 {shr ax, 4}
);

function LoNibble(X: Byte): Byte {Shim};
inline(
   $58/        {pop ax}
   $25/$0F/$00 {and ax, 0Fh}
);

type

{ Command sets }

  PCommandSet = ^TCommandSet;
  TCommandSet = set of Byte;

{ Color palette type }

  PPalette = ^TPalette;
  TPalette = String;

{ Rectangles. OOA }

  PRectCollection = ^TRectCollection;
  TRectCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    procedure InsertRect(AX, AY, BX, BY: Integer);
    procedure InsertCopy(Rect: TRect);
    {function Invert(Limits: TRect): PRectCollection;}
    procedure Intersect(Rect: TRect);
    procedure Cut(Rect: TRect);
    procedure Sort;
    procedure Union(Rect: TRect);
    procedure Update;
    procedure UnitedRect(var Rect: TRect);
  end;

{ TView object Pointer }

  PView = ^TView;

{ TGroup object Pointer }

  PGroup = ^TGroup;

{ TView object }

  TView = object(TObject)
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: Byte;
    DragMode: Byte;
    HelpCtx: Word;
    State: Word;
    Options: Word;
    EventMask: Word;
    {Font: Integer {TONY}
    ClipRects: PRectCollection; {OOA}

    LogPalette : TLogPalette;
    PaintInfo  : TPaintInfo;
    Font       : TFont;
    CursorShape: PMouseCursor;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Awaken; virtual;
    procedure BlockCursor;
    procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure ClearEvent(var Event: TEvent);
    function CommandEnabled(Command: Word): Boolean;
    function DataSize: Word; virtual;
    procedure DisableCommands(Commands: TCommandSet);
    procedure DragView(Event: TEvent; Mode: Word {OOA};
      var Limits: TRect; MinSize, MaxSize: TPoint);
    procedure Draw; virtual;
    procedure DrawView;
    procedure DrawViewDirect;
    procedure EnableCommands(Commands: TCommandSet);
    procedure EndModal(Command: Word); virtual;
    function EventAvail: Boolean;
    function Execute: Word; virtual;
    function Exposed: Boolean;
    function Focus: Boolean;
    procedure GetBounds(var Bounds: TRect);
    procedure GetClipRect(var Clip: TRect);
    function GetColor(Color: Word): Word;
    procedure GetCommands(var Commands: TCommandSet);
    procedure GetData(var Rec); virtual;
    procedure GetEvent(var Event: TEvent); virtual;
    procedure GetExtent(var Extent: TRect);
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetPeerViewPtr(var S: TStream; var P);
    function GetState(AState: Word): Boolean;
    procedure GrowTo(X, Y: Integer);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Hide;
    procedure HideCursor;
    procedure KeyEvent(var Event: TEvent);
    procedure Locate(var Bounds: TRect);
    procedure MakeFirst;
    procedure MakeGlobal(Source: TPoint; var Dest: TPoint);
    procedure MakeLocal(Source: TPoint; var Dest: TPoint);
    function MouseEvent(var Event: TEvent; Mask: Word): Boolean;
    function MouseInView(Mouse: TPoint): Boolean;
    procedure MoveTo(X, Y: Integer);
    function NextView: PView;
    procedure NormalCursor;
    function Prev: PView;
    function PrevView: PView;
    procedure PutEvent(var Event: TEvent); virtual;
    procedure PutInFrontOf(Target: PView);
    procedure PutPeerViewPtr(var S: TStream; P: PView);
    procedure Select;
    procedure SetBounds(var Bounds: TRect);
    procedure SetCommands(Commands: TCommandSet);
    procedure SetCmdState(Commands: TCommandSet; Enable: Boolean);
    procedure SetCursor(X, Y: Integer);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Show;
    procedure ShowCursor;
    procedure SizeLimits(var Min, Max: TPoint); virtual;
    procedure Store(var S: TStream);
    function TopView: PView;
    function Valid(Command: Word): Boolean; virtual;

    procedure DrawInvalidated;               {OOA}
    procedure InvalidateSelf;                {OOA}
    procedure ValidateSelf;                  {OOA}
    procedure InvalidateRect(Rect: TRect); virtual; {OOA}
    procedure ValidateRect(Rect: TRect); virtual; {OOA}
    procedure GetInvalidExtent(var Extent: TRect); virtual; {OOA}
    function GetFrameWidth: Integer; virtual; {OOA}
    function GetCaptureHeight: Integer; virtual; {OOA}
    procedure MouseMastering(const Event: TEvent); virtual; {TONY}
    procedure RAssign(var Rect: TRect; AX,AY,BX,BY: Integer); virtual; {OOA}

    Procedure SetColor(Color : LongInt);
    Procedure SetPaint(Fore, Back : LongInt; Operation : Byte; LinePattern : Word;
                       LineWidth, LineStyle, Pattern : Byte);{DK}
    Procedure SetTextMetrics(AFont, Width, Height : Byte; Escapement, GapLength : Word;
                             Italic : Byte; FontStyle : Word);{DK}
    Procedure SetBitmapOrg(X, Y : Integer);
    Procedure SolidBrush(R, G, B : Byte);
    Function  MonoColor(Color, Purpose : LongInt) : LongInt; {DK}
    procedure HLine(X1, Y, X2 : Integer; Fore: LongInt) {TONY};
    procedure HLineStyle(X1, Y, X2: integer; Pattern : Byte; Clr0, Clr1: LongInt) {TONY};
    procedure FrameStyle(X1, Y1, X2, Y2: integer) {GIO};
    procedure VLine(X, Y1, Y2 : Integer; Fore: LongInt) {TONY};
    procedure Line(X1, Y1, X2, Y2: Integer); {GIO}
    procedure Bar(X1, Y1, X2, Y2:Integer; Fore: LongInt) {TONY};
    procedure Rectangle(X1, Y1, X2, Y2, W, UpLt, DnRt: integer) {TONY};
    procedure WrStr(X, Y: integer; const S: String; Attr: LongInt) {TONY};
    procedure WrCStr(X, Y: integer; const S: String; Attr: LongInt) {TONY};
    procedure PutBMP(Image: PImage; X, Y: integer); {TONY}
    procedure PutBMPOp(Image: PImage; X, Y: integer; Operation: byte); {TONY}
    procedure PutBMPPart(Image: PImage; X, Y, XFrom, YFrom : integer); {TONY}
    procedure StretchBMP(Image: PImage; X, Y, ToX, ToY : integer); {DK}
    procedure ScrollY(x1, y1, x2, y2, How: integer); {TONY}
    procedure Scroll(x1, y1, x2, y2, Dx, Dy: integer); {DK}
    procedure SetPixel(x, y: integer; Color : LongInt); {TONY}
    procedure FillCircle(X, Y, R: Integer); {DK}
    procedure Circle(X, Y, R: Integer);{DK}
    procedure FillEllipse(X, Y, Rx, Ry: Integer);{DK}
    procedure Ellipse(X, Y, Rx, Ry: Integer);{DK}
    procedure FillPoly(NumPoint : Integer; var Polygon);{DK}
    procedure BarStyle(X1, Y1, X2, Y2: integer);{DK}
    procedure RoundBar(X1, Y1, X2, Y2, X3, Y3: integer);{DK}
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: integer);{DK}
    procedure RealizePalette;
    Function  UpdateColors : Boolean;   virtual;

    function  FontWidth(const S: String): integer {TONY};
    function  CharWidth: integer {TONY};
    function  FontHeight: integer {TONY};

    procedure FrameBar(X1, Y1, X2, Y2: integer; UpLt, DnRt, Back: Integer); {SHIM}
  private
    ShouldCursor, ShouldMouse, SetTrap : Boolean;
    procedure CalcClipRects; virtual; {OOA}
    procedure DrawBegin {TONY,OOA};
    procedure DrawEnd {TONY,OOA};
    procedure DrawCursor;
    procedure DrawHide(LastView: PView);
    procedure DrawShow(LastView: PView);
    procedure DrawUnderRect(var R: TRect; LastView: PView);
    procedure DrawUnderView(DoShadow: Boolean; LastView: PView);
    procedure ResetCursor; virtual;
    procedure PaletteEnable(AEnable : Boolean); virtual; {DK}
  end;

{ TFrame types }

  TTitleStr = string[80];

{ TFrame object }

  { Palette layout }
  { 1 = Passive frame }
  { 2 = Passive title }
  { 3 = Active frame }
  { 4 = Active title }
  { 5 = Icons }

  PFrame = ^TFrame;
  TFrame = object(TView)
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure MouseMastering(const Event: TEvent); virtual; {TONY}
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
    function GetFrameWidth: Integer; virtual; {OOA}
    function GetCaptureHeight: Integer; virtual; {OOA}
    function GetDragGrowMode(const Event: TEvent): Word; {OOA}
    procedure GetClientExtent(var Extent: TRect); {OOA}
  public
    FrameMode: Word;
    FrameWidth: Integer {SHIM,OOA};
    UpBarHeight: Integer {SHIM};
  end;

{ ScrollBar characters }

  TScrollChars = array[0..4] of Integer;

{ TScrollBar object }

  { Palette layout }
  { 1 = Page areas }
  { 2 = Arrows }
  { 3 = Indicator }

  PScrollBar = ^TScrollBar;
  TScrollBar = object(TView)
    Value: Integer;
    Min: Integer;
    Max: Integer;
    PgStep: Integer;
    ArStep: Integer;
    IsVertical: Boolean; {SHIM}
    DrawPartCode: Integer;  {SHIM}
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ScrollDraw; virtual;
    function ScrollStep(Part: Integer): Integer; virtual;
    procedure SetParams(AValue, AMin, AMax, APgStep, AArStep: Integer);
    procedure SetRange(AMin, AMax: Integer);
    procedure SetStep(APgStep, AArStep: Integer);
    procedure SetValue(AValue: Integer);
    procedure Store(var S: TStream);
  private
    Chars: TScrollChars;
    _Pos: Integer; {OOA}
    Draw_Pos: Boolean; {OOA}
    procedure DrawPos(Pos: Integer);
    function GetPos: Integer;
    function GetSize: Integer;
  end;

{ TScroller object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }

  PScroller = ^TScroller;
  TScroller = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    Delta: TPoint;
    Limit: TPoint;
    Step: TPoint; {OOA}
    LastDelta: TPoint; {TONY}
    constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    procedure ChangeBounds(var Bounds: TRect); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ScrollDraw; virtual;
    procedure ScrollTo(X, Y: Integer);
    procedure SetLimit(X, Y: Integer);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
   private
    DrawLock: Byte;
    DrawFlag: Boolean;
    procedure CheckDraw;
  end;

{ TListViewer }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PListViewer = ^TListViewer;

  TListViewer = object(TView)
    HScrollBar: PScrollBar;
    VScrollBar: PScrollBar;
    NumCols: Integer;
    TopItem: Integer;
    Focused: Integer;
    Range: Integer;
    Step: TPoint; {OOA}
    constructor Init(var Bounds: TRect; ANumCols: Word;
      AHScrollBar, AVScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure Draw; virtual;
    procedure FocusItem(Item: Integer); virtual;
    function GetPalette: PPalette; virtual;
    function GetText(Item: Integer; MaxLen: Integer): String; virtual;
    function IsSelected(Item: Integer): Boolean; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SelectItem(Item: Integer); virtual;
    procedure SetRange(ARange: Integer);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);

    procedure DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt); virtual; {OOA}
  private
    procedure FocusItemNum(Item: Integer); virtual;
  end;

{ Selection modes }

  SelectMode = (NormalSelect, EnterSelect, LeaveSelect);

{ TGroup object }

  TGroup = object(TView)
    Last: PView;
    Current: PView;
    Phase: (phFocused, phPreProcess, phPostProcess);
    EndState: Word;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Awaken; virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    function DataSize: Word; virtual;
    procedure Delete(P: PView);
    procedure Draw; virtual;
    procedure EndModal(Command: Word); virtual;
    procedure EventError(var Event: TEvent); virtual;
    function ExecView(P: PView): Word;
    function Execute: Word; virtual;
    function First: PView;
    function FirstThat(P: Pointer): PView;
    function FocusNext(Forwards: Boolean): Boolean;
    procedure ForEach(P: Pointer);
    procedure GetData(var Rec); virtual;
    function GetHelpCtx: Word; virtual;
    procedure GetSubViewPtr(var S: TStream; var P);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Insert(P: PView);
    procedure InsertBefore(P, Target: PView);
    procedure Lock;
    procedure PutSubViewPtr(var S: TStream; P: PView);
    procedure Redraw;
    procedure SelectNext(Forwards: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
    procedure Unlock;
    function Valid(Command: Word): Boolean; virtual;
    procedure MouseMastering(const Event: TEvent); virtual; {TONY}
    procedure InvalidateRect(Rect: TRect); virtual; {OOA}
    procedure ValidateRect(Rect: TRect); virtual; {OOA}
    procedure GetInvalidExtent(var Extent: TRect); virtual; {OOA}
  private
    Clip: TRect;
    LockFlag: Byte;
    procedure CalcClipRects; virtual; {OOA}
    function At(Index: Integer): PView;
    procedure DrawSubViews(P, Bottom: PView);
    function FirstMatch(AState: Word; AOptions: Word): PView;
    function FindNext(Forwards: Boolean): PView;
    function IndexOf(P: PView): Integer;
    procedure InsertView(P, Target: PView);
    procedure RemoveView(P: PView);
    procedure ResetCurrent;
    procedure ResetCursor; virtual;
    procedure SetCurrent(P: PView; Mode: SelectMode);
    procedure PaletteEnable(AEnable : Boolean); virtual;
  end;


{ TIconizedWindow object }

  PWindow = ^TWindow;

  PIconizedWindow = ^TIconizedWindow;
  TIconizedWindow = object(TView)
    ANDImage, XORImage: Integer;
    Link: PWindow;
    constructor Init(AANDImage, AXORImage : Integer; ALink: PWindow);
    procedure   HandleEvent(var Event: TEvent); virtual;
    procedure   Draw; virtual;
    procedure   SetState(AState: Word; Enable: Boolean); virtual;
  end;


{ TWindow object }

  { Palette layout }
  { 1 = Frame passive }
  { 2 = Frame active }
  { 3 = Frame icon }
  { 4 = ScrollBar page area }
  { 5 = ScrollBar controls }
  { 6 = Scroller normal text }
  { 7 = Scroller selected text }
  { 8 = Reserved }

  TWindow = object(TGroup)
    Flags: Byte;
    ZoomRect: TRect;
    Number: Integer;
    Palette: Integer;
    Frame: PFrame;
    Title: PString;
    IconAND, IconXOR: Integer;
    Icon: PIconizedWindow; {TONY, OOA}
    constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Integer);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Close; virtual;
    function GetPalette: PPalette; virtual;
    function GetTitle(MaxSize: Integer): TTitleStr; virtual;
    Procedure GetClientExtent(var Extent : TRect);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitFrame; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure SizeLimits(var Min, Max: TPoint); virtual;
    function StandardScrollBar(AOptions: Word): PScrollBar;
    procedure Store(var S: TStream);
    procedure Zoom; virtual;

    procedure RAssign(var Rect: TRect; AX,AY,BX,BY: Integer); virtual; {OOA}
    function GetFrameWidth: Integer; virtual; {OOA}
    function GetCaptureHeight: Integer; virtual; {OOA}
    procedure SetIcon(AIconAND, AIconXOR: Integer); {TONY, OOA}
    procedure Iconize; {TONY, OOA}
    procedure Restore; {TONY, OOA}
  private
    LastIconPos : TPoint;
  end;

  PTimedView = ^TTimedView;   {TONY}
  TTimedView = object(TView)
    TicksInterval: Word;
    CommandToSend: Word;
    constructor Init(var Bounds: TRect; Command: Word; Interval: Word);
    constructor Load(var S: TStream);    {GIO}
    procedure   Store(var S: TStream);   {GIO}
    procedure   HandleEvent(var Event: TEvent); virtual;
    procedure   Update; virtual;
  end;

{ Message dispatch function }

function Message(Receiver: PView; What, Command: Word;
  InfoPtr: Pointer): Pointer;

{ Views registration procedure }

procedure RegisterViews;

{ GlobalFont stack }
procedure SetFont(Font: TFont);
procedure RestoreFont;

{ Low priority & timed message handling } {TONY}
procedure LPMessage(Priority: Integer; Receiver: PView; What, Command: Word;
                  InfoPtr: Pointer);
procedure TimedMessage(Priority: Integer; Receiver: PView;
              What, Command: Word; InfoPtr: Pointer; WaitTime: Word);
procedure ClearLPM(Receiver: PView; What, Command: Word);
procedure ClearAllAddressedLPM(Receiver: PView);


{ These aren't procedures. These are images!  Don't call them! }
procedure ScrollDownDef;      {01}
procedure ScrollDownPress;    {02}
procedure ScrollUpDef;        {03}
procedure ScrollUpPress;      {04}
procedure ScrollLeftDef;      {05}
procedure ScrollLeftPress;    {06}
procedure ScrollRightDef;     {07}
procedure ScrollRightPress;   {08}
procedure ScrollThumb;        {09}
procedure ComboDef;           {10}
procedure ComboPress;         {11}
procedure CloseWindowIcon;    {12}
procedure RestoreWindowIcon;  {13}
procedure ZoomWindowIcon;     {14}
procedure MinimizeWindowIcon; {15}


{ Image registration procedure uses IDs : 1..100 }
procedure RegisterViewsImages;
procedure RegisterScrollBarImages;
procedure RegisterComboImages;

const

{ Event masks }

  PositionalEvents: Word = evMouse;
  FocusedEvents: Word = evKeyboard + evCommand;

{ Minimum window size }

  MinWinSize: TPoint = (X: 100; Y: 100);

{ Markers control }

  ShowMarkers: Boolean = False;

{ MapColor error return value }

  ErrorAttr: Byte = $CF;

{ Stream Registration Records }

const
  RView: TStreamRec = (
     ObjType: 1;
     VmtLink: Ofs(TypeOf(TView)^);
     Load:    @TView.Load;
     Store:   @TView.Store
  );

const
  RFrame: TStreamRec = (
     ObjType: 2;
     VmtLink: Ofs(TypeOf(TFrame)^);
     Load:    @TFrame.Load;
     Store:   @TFrame.Store
  );

const
  RScrollBar: TStreamRec = (
     ObjType: 3;
     VmtLink: Ofs(TypeOf(TScrollBar)^);
     Load:    @TScrollBar.Load;
     Store:   @TScrollBar.Store
  );

const
  RScroller: TStreamRec = (
     ObjType: 4;
     VmtLink: Ofs(TypeOf(TScroller)^);
     Load:    @TScroller.Load;
     Store:   @TScroller.Store
  );

const
  RListViewer: TStreamRec = (
     ObjType: 5;
     VmtLink: Ofs(TypeOf(TListViewer)^);
     Load:    @TListViewer.Load;
     Store:   @TLIstViewer.Store
  );

const
  RGroup: TStreamRec = (
     ObjType: 6;
     VmtLink: Ofs(TypeOf(TGroup)^);
     Load:    @TGroup.Load;
     Store:   @TGroup.Store
  );

const
  RWindow: TStreamRec = (
     ObjType: 7;
     VmtLink: Ofs(TypeOf(TWindow)^);
     Load:    @TWindow.Load;
     Store:   @TWindow.Store
  );

{ Characters used for drawing selected and default items in  }
{ monochrome color sets                                      }

  SpecialChars: array[0..5] of Char = (#16, #17, #26, #27, ' ', ' ');

{ True if the command set has changed since being set to false }

  CommandSetChanged: Boolean = False;

{ Low priority & timed message handling (for internal use only) } {TONY}
procedure InitLPM;
procedure DoneLPM;
function LPMCanBeSent: boolean;
function Locked(P: PGroup): boolean;


const
  DrawLockCounter : Longint = 0 {TONY};
  PaletteLockCounter : LongInt = 0; {DK}

implementation
var
  RefreshRegion : TRect {TONY};

type
  PFixupList = ^TFixupList;
  TFixupList = array[1..4096] of Pointer;

const
  OwnerGroup: PGroup = nil;
  FixupList: PFixupList = nil;
  TheTopView: PView = nil;

const

{ Bit flags to determine how to draw the frame icons }

  fmCloseClicked   = $0001;
  fmZoomClicked    = $0002;
  fmIconizeClicked = $0004;

{ Current command set. All but window commands are active by default }

  CurCommandSet: TCommandSet =
    [0..255] - [cmZoom, cmClose, cmResize, cmNext, cmPrev];

function Min(A, B: integer): integer;
inline (
    $5B/      {pop  bx (A)}
    $58/      {pop  ax (B)}
    $39/$D8/  {cmp  ax, bx}
    $7C/$02/  {jl   @@1}
    $89/$D8   {mov  ax, bx}
{@@1:}
);

function Max(A, B: integer): integer;
inline (
    $5B/      {pop  bx (A)}
    $58/      {pop  ax (B)}
    $39/$D8/  {cmp  ax, bx}
    $7F/$02/  {jg   @@1}
    $89/$D8   {mov  ax, bx}
{@@1:}
);

{ Convert color into attribute                          }
{ In    AL = Color                                      }
{ Out   AL = Attribute                                  }

procedure MapColor; near; assembler;
const
  Self = 6;
  TView_GetPalette = vmtHeaderSize + $2C;
asm
        OR      AL,AL
        JE      @@3
        LES     DI,[BP].Self
@@1:    PUSH    ES
        PUSH    DI
        PUSH    AX
        PUSH    ES
        PUSH    DI
        MOV     DI,ES:[DI]
        CALL    DWORD PTR [DI].TView_GetPalette
        MOV     BX,AX
        MOV     ES,DX
        OR      AX,DX
        POP     AX
        POP     DI
        POP     DX
        JE      @@2
        CMP     AL,ES:[BX]
        JA      @@3
        SEGES   XLAT
{        OR      AL,AL
        JE      @@3}
@@2:    MOV     ES,DX
        LES     DI,ES:[DI].TView.Owner
        MOV     SI,ES
        OR      SI,DI
        JNE     @@1
        JMP     @@4
@@3:    MOV     AL,ErrorAttr
@@4:
end;

{ Convert color pair into attribute pair                }
{ In    AX = Color pair                                 }
{ Out   AX = Attribute pair                             }

procedure MapCPair; near; assembler;
asm
        OR      AH,AH
        JE      @@1
        XCHG    AL,AH
        CALL    MapColor
        XCHG    AL,AH
@@1:    CALL    MapColor
end;

{ TRectCollection. OOA }

type
  PRect = ^TRect;

procedure TRectCollection.FreeItem(P: Pointer);
begin
  Dispose(PRect(P));
end;

procedure TRectCollection.InsertRect(AX,AY,BX,BY: Integer);
var
  P: PRect;
begin
  if (AX >= BX) or (AY >= BY) then Exit;
  New(P);
  if P = Nil then Exit;
  P^.Assign(AX,AY,BX,BY);
  AtInsert(Count,P);
end;

procedure TRectCollection.InsertCopy(Rect: TRect);
var
  P: PRect;
begin
  if Rect.Empty then Exit;
  New(P);
  if P = Nil then Exit;
  P^.Copy(Rect);
  AtInsert(Count,P);
end;
{
function TRectCollection.Invert(Limits: TRect): PRectCollection;

var
  Holes: PRectCollection;

  procedure AddRect(Solid: PRect); far;
  var
    I, C: Integer;
    R, RI: TRect;
  begin
    C:= Holes^.Count;
    I:= 0;
    while I < C do with Solid^ do
    begin
      R.Copy(PRect(Holes^.At(I))^);
      RI.Copy(Solid^);
      RI.Intersect(R);
      if RI.Empty then Inc(I) else with RI do
      begin
        Holes^.AtFree(I);
        Dec(C);
        if Contains(R.A) and Contains(R.B) then Continue;
        Holes^.InsertRect(R.A.X, R.A.Y, R.B.X, A.Y);
        Holes^.InsertRect(R.A.X, B.Y, R.B.X, R.B.Y);
        Holes^.InsertRect(R.A.X, A.Y, A.X, B.Y);
        Holes^.InsertRect(B.X, A.Y, R.B.X, B.Y);
      end;
    end;
  end;

begin
  New(Holes, Init(Count, Delta));
  if Holes = Nil then Exit;
  with Limits do Holes^.InsertRect(A.X,A.Y,B.X,B.Y);
  ForEach(@AddRect);
  Invert:= Holes;
end;
}
procedure TRectCollection.Intersect(Rect: TRect);
var
  I: Integer;
begin
  I:= Count - 1;
  while I >= 0 do
  begin
    with PRect(At(I))^ do
    begin
      Intersect(Rect);
      if Empty then AtFree(I);
    end;
    Dec(I);
  end;
end;

procedure TRectCollection.Cut(Rect: TRect);
var
  I: Integer;
  R, RI: TRect;
begin
  I:= Count - 1;
  while I >= 0 do
  begin
    R:= PRect(At(I))^;
    RI.Copy(R);
    RI.Intersect(Rect);
    if not RI.Empty then
    begin
      AtFree(I);
      if not (Rect.Contains(R.A) and Rect.Contains(R.B)) then with RI do
      begin
        InsertRect(R.A.X, R.A.Y, R.B.X, A.Y);
        InsertRect(R.A.X, B.Y, R.B.X, R.B.Y);
        InsertRect(R.A.X, A.Y, A.X, B.Y);
        InsertRect(B.X, A.Y, R.B.X, B.Y);
      end;
    end;
    Dec(I);
  end;
end;

procedure TRectCollection.Sort;

  function compp(a, b: pointer): integer;
  begin
    if prect(a)^.a.y < prect(b)^.a.y then compp:= -1
    else if prect(a)^.a.y > prect(b)^.a.y then compp:= 1
    else if prect(a)^.a.x < prect(b)^.a.x then compp:= -1
    else if prect(a)^.a.x > prect(b)^.a.x then compp:= 1
    else compp:= 0;
  end;

  procedure DoSort(l, r: integer);
  var
    i, j: integer;
    x, t: pointer;
  begin
    i := l; j := r; x:= at((l+r) DIV 2);
    repeat
      while compp(at(i), x) < 0 do inc(i);
      while compp(x, at(j)) < 0 do dec(j);
      if i <= j then
      begin
        t:= at(i); atput(i, at(j)); atput(j, t);
        inc(i); dec(j);
      end;
    until i > j;
    if l < j then DoSort(l, j);
    if i < r then DoSort(i, r);
  end;

begin
  if Count <= 1 then Exit;
  DoSort(0, Count - 1);
end;

procedure TRectCollection.Update;
var
  I, J: Integer;
  R: TRect;
begin
  if Count <= 1 then Exit;
  Sort;
  I:= 0;
  while I < Count do with PRect(At(I))^ do
  begin
    J:= I + 1;
    while J < Count do
    begin
      R.Copy(PRect(At(J))^);
      if R.A.Y > B.Y then Break
      else if R.A.Y < B.Y then Inc(J)
      else if R.A.X > A.X then Break
      else if R.B.X > B.X then Break
      else if (R.A.X <> A.X) or (R.B.X <> B.X) then Inc(J) else
      begin
        B.Y:= R.B.Y;
        AtFree(J);
      end;
    end;
    Inc(I);
  end;
end;

procedure TRectCollection.Union(Rect: TRect);

procedure UnionWith(Item, AX, AY, BX, BY: Integer);
begin
  if (AX >= BX) or (AY >= BY) then Exit;
  if Item >= Count then InsertRect(AX, AY, BX, BY) else with PRect(At(Item))^ do
    if (AX >= A.X) and (AY >= A.Y) and (BX <= B.X) and (BY <= B.Y ) then
      {New Under Old}
      Exit
    else if (AX > B.X) or (AY > B.Y) or (BX < A.X) or (BY < A.Y) then
      {don't intersect}
      UnionWith(Item + 1, AX, AY, BX, BY)
    else if (AX < A.X) and (AY < A.Y) and (BX > B.X) and (BY > B.Y) then
      {New Over Old - optional, next branch do it (but slower)}
      begin
        UnionWith(Item+1, AX, AY, BX, BY);
        AtFree(Item);
      end
    else
      {New Breaks Old}
      begin
        UnionWith(Item+1, AX, AY, BX, A.Y);  {Upper}
        UnionWith(Item+1, AX, B.Y, BX, BY);  {Lower}
        UnionWith(Item+1, Min(AX, A.X), Max(AY, A.Y), Max(BX, B.X), Min(BY, B.Y)); {Middle}
        InsertRect(A.X, A.Y, B.X, AY);       {Under Upper}
        InsertRect(A.X, BY, B.X, B.Y);       {Over Lower}
        AtFree(Item);
      end;
end;

begin
  UnionWith(0, Rect.A.X, Rect.A.Y, Rect.B.X, Rect.B.Y);
end;

procedure TRectCollection.UnitedRect(var Rect: TRect);
var
  I: Integer;
begin
  if Count = 0 then
  begin
    Longint(Rect.A):= 0;
    Longint(Rect.B):= 0;
  end else
  begin
    Rect.Copy(PRect(At(0))^);
    for I:= 1 to Count - 1 do
      Rect.Union(PRect(At(I))^);
  end;
end;

{ TView }

constructor TView.Init(var Bounds: TRect);
begin
  TObject.Init;
  Owner := nil;
  State := sfVisible;
  SetBounds(Bounds);
  DragMode := dmLimitLoY;
  HelpCtx := hcNoContext;
  EventMask := evMouseDown + evKeyDown + evCommand;
  New(ClipRects, Init(10, 5));
  if ClipRects = Nil then Fail;
  LogPalette.Mode := 0;
  DefaultPaint(PaintInfo);
  Font := GlobalFont;
  CursorShape := @DefaultMouseCursor;
end;

constructor TView.Load(var S: TStream);
begin
  TObject.Init;
  S.Read(Origin,
    SizeOf(TPoint) * 3 +
    SizeOf(Byte) * 2 +
    SizeOf(Word) * 4
    {DK-=SizeOf(Integer) {Font. OOA}
  );
  S.Read(Font.Font, 1);
  New(ClipRects, Init(10, 5));
  if ClipRects = Nil then Fail;
end;

destructor TView.Done;
begin
  ClearAllAddressedLPM(@Self); {TONY}
  if PaletteUsers.IndexOf(@Self) >= 0 then PaletteUsers.Delete(@Self);
  Hide;
  DisposePalette(LogPalette);
  if Owner <> nil then Owner^.Delete(@Self);
  Dispose(ClipRects, Done);
end;

procedure TView.Awaken;
begin
end;

procedure TView.BlockCursor;
begin
  SetState(sfCursorIns, True);
end;

procedure TView.CalcBounds(var Bounds: TRect; Delta: TPoint);
var
  S, D: Integer;
  Min, Max: TPoint;

procedure Grow(var I: Integer);
begin
  if GrowMode and gfGrowRel = 0 then Inc(I, D) else
    I := (I * Longint(S) + (S - D) shr 1) div (S - D);
end;

function Range(Val, Min, Max: Integer): Integer;
begin
  if Val < Min then Range := Min else
    if Val > Max then Range := Max else
      Range := Val;
end;

begin
  GetBounds(Bounds);
  S := Owner^.Size.X;
  D := Delta.X;
  if GrowMode and gfGrowLoX <> 0 then Grow(Bounds.A.X);
  if GrowMode and gfGrowHiX <> 0 then Grow(Bounds.B.X);
  if Bounds.B.X - Bounds.A.X > MaxViewWidth then
    Bounds.B.X := Bounds.A.X + MaxViewWidth;
  S := Owner^.Size.Y;
  D := Delta.Y;
  if GrowMode and gfGrowLoY <> 0 then Grow(Bounds.A.Y);
  if GrowMode and gfGrowHiY <> 0 then Grow(Bounds.B.Y);
  SizeLimits(Min, Max);
  Bounds.B.X := Bounds.A.X + Range(Bounds.B.X - Bounds.A.X, Min.X, Max.X);
  Bounds.B.Y := Bounds.A.Y + Range(Bounds.B.Y - Bounds.A.Y, Min.Y, Max.Y);
end;

procedure TView.ChangeBounds(var Bounds: TRect);
begin
  SetBounds(Bounds);
  DrawView;
end;

procedure TView.ClearEvent(var Event: TEvent);
begin
  Event.What := evNothing;
  Event.InfoPtr := @Self;
end;

function TView.CommandEnabled(Command: Word): Boolean;
begin
  CommandEnabled := (Command > 255) or (Command in CurCommandSet);
end;

function TView.DataSize: Word;
begin
  DataSize := 0;
end;

procedure TView.DisableCommands(Commands: TCommandSet);
begin
  CommandSetChanged := CommandSetChanged or (CurCommandSet * Commands <> []);
  CurCommandSet := CurCommandSet - Commands;
end;

procedure TView.DragView(Event: TEvent; Mode: Word;
  var Limits: TRect; MinSize, MaxSize: TPoint);

const
  ManualStepX = 2  {OOA};
  ManualStepY = 4  {OOA};

var
  P, S: TPoint;
  SaveBounds: TRect;
  NewBounds: TRect {OOA};
  CursorIsShown: boolean;
  Full : Boolean; {DK}


function Min(I, J: Integer): Integer;
begin
  if I < J then Min := I else Min := J;
end;

function Max(I, J: Integer): Integer;
begin
  if I > J then Max := I else Max := J;
end;

procedure DrawFrame(R: TRect) {OOA};
begin
  if Full then Exit;
  Owner^.MakeGlobal(R.A, R.A);
  Owner^.MakeGlobal(R.B, R.B);
  HideMouse;
  with R do DrawXORFrame(a.x, a.y, b.x-1, b.y-1, GetFrameWidth);
  ShowMouse;
end;

procedure MoveGrow(P, S: TPoint);
var
  R: TRect;
begin
  S.X := Min(Max(S.X, MinSize.X), MaxSize.X);
  S.Y := Min(Max(S.Y, MinSize.Y), MaxSize.Y);
  P.X := Min(Max(P.X, Limits.A.X - S.X + 1), Limits.B.X - 1);
  P.Y := Min(Max(P.Y, Limits.A.Y - S.Y + 1), Limits.B.Y - 1);
  if Mode and dmLimitLoX <> 0 then P.X := Max(P.X, Limits.A.X);
  if Mode and dmLimitLoY <> 0 then P.Y := Max(P.Y, Limits.A.Y);
  if Mode and dmLimitHiX <> 0 then P.X := Min(P.X, Limits.B.X - S.X);
  if Mode and dmLimitHiY <> 0 then P.Y := Min(P.Y, Limits.B.Y - S.Y);
  R.Assign(P.X, P.Y, P.X + S.X, P.Y + S.Y);
{ ; - original. OOA}
  if Full then Locate(R) else begin
    DrawFrame(NewBounds)  {OOA};
    DrawFrame(R)          {OOA};
  end;
  NewBounds:= R         {OOA};
end;


procedure Change(DX, DY: Integer);
begin
  if (Mode and dmDragMove <> 0) and (GetShiftState and $03 = 0) then
  begin
    Inc(P.X, DX);
    Inc(P.Y, DY);
  end else
  if (Mode and dmDragGrow <> 0) and (GetShiftState and $03 <> 0) then
  begin
    Inc(S.X, DX);
    Inc(S.Y, DY);
  end;
end;

procedure Update(X, Y: Integer);
begin
  if Mode and dmDragMove <> 0 then
  begin
    P.X := X;
    P.Y := Y;
  end;
end;

procedure MouseChangeX(D: Integer); {OOA}
begin
  S.X:= D - P.X + GetFrameWidth;
  if S.X < MinSize.X then
  begin
    Inc(P.X, S.X - MinSize.X);
    S.X:= MinSize.X;
  end else if S.X > MaxSize.X then
  begin
    Inc(P.X, S.X - MaxSize.X);
    S.X:= MaxSize.X;
  end;
end;

procedure MouseChangeY(D: Integer); {OOA}
begin
  S.Y:= D - P.Y + GetFrameWidth;
  if S.Y < MinSize.Y then
  begin
    Inc(P.Y, S.Y - MinSize.Y);
    S.Y:= MinSize.Y;
  end else if S.Y > MaxSize.Y then
  begin
    Inc(P.Y, S.Y - MaxSize.Y);
    S.Y:= MaxSize.Y;
  end;
end;

var
  Mouse: TPoint;

begin
  CursorIsShown := TextCursor.IsShown;
  if CursorIsShown then TextCursor.Hide;
  LockMouseShape;
  SetState(sfDragging, True);
  Full := (Event.What = evMouseDown) and (Event.Buttons and mbRightButton = mbRightButton);

  GetBounds(NewBounds) {OOA};
  DrawFrame(NewBounds) {OOA};
  if (Event.What = evMouseDown) {and (Event.Buttons and mbLeftButton = mbLeftButton)} then
  begin
    if Mode and dmDragMove <> 0 then
    begin
      P.X := Origin.X - Event.Where.X;
      P.Y := Origin.Y - Event.Where.Y;
      if Full and (Owner <> Nil) then Owner^.UnLock;{DK}
      repeat
        Inc(Event.Where.X, P.X);
        Inc(Event.Where.Y, P.Y);
        MoveGrow(Event.Where, Size);
      until not MouseEvent(Event, evMouseMove);
      if Full and (Owner <> Nil) then Owner^.Lock;{DK}
    end else
    begin {OOA}
      if Full and (Owner <> Nil) then Owner^.UnLock;{DK}
      repeat
        P:= NewBounds.A;
        S.X := NewBounds.B.X - NewBounds.A.X;
        S.Y := NewBounds.B.Y - NewBounds.A.Y;
        Owner^.MakeLocal(Event.Where, Mouse);
        Dec(Mouse.X, 1);
        Dec(Mouse.Y, 1);
        case Mode and $0F00 of
          dsUpperLeft:
            begin
              S.Y:= S.Y - (Mouse.Y - P.Y);
              P.Y:= Mouse.Y;
              S.X:= S.X - (Mouse.X - P.X);
              P.X:= Mouse.X;
            end;
          dsLowerLeft:
            begin
              MouseChangeY(Mouse.Y);
              S.X:= S.X - (Mouse.X - P.X);
              P.X:= Mouse.X;
            end;
          dsUpperRight:
            begin
              S.Y:= S.Y - (Mouse.Y - P.Y);
              P.Y:= Mouse.Y;
              MouseChangeX(Mouse.X);
            end;
          dsLowerRight:
            begin
              MouseChangeX(Mouse.X);
              MouseChangeY(Mouse.Y);
            end;
          dsLeft:
            begin
              S.X:= S.X - (Mouse.X - P.X);
              P.X:= Mouse.X;
            end;
          dsRight:
            MouseChangeX(Mouse.X);
          dsUpper:
            begin
              S.Y:= S.Y - (Mouse.Y - P.Y);
              P.Y:= Mouse.Y;
            end;
          dsLower:
            MouseChangeY(Mouse.Y);
          dsDrag:;
        end;
        MoveGrow(P, S);
      until not MouseEvent(Event, evMouseMove);
      if Full and (Owner <> Nil) then Owner^.Lock;{DK}
    end;
    DrawFrame(NewBounds);
    Locate(NewBounds);
  end else
  begin
    SaveBounds:= NewBounds;
    repeat
      P:= NewBounds.A                      {OOA};
      S.X := NewBounds.B.X - NewBounds.A.X {OOA};
      S.Y := NewBounds.B.Y - NewBounds.A.Y {OOA};
      KeyEvent(Event);
      case Event.KeyCode and $FF00 of
        kbLeft: Change(-ManualStepX, 0);
        kbRight: Change(ManualStepX, 0);
        kbUp: Change(0, -ManualStepY);
        kbDown: Change(0, ManualStepY);
        kbCtrlLeft: Change(-8*ManualStepX, 0);
        kbCtrlRight: Change(8*ManualStepX, 0);
        kbHome: Update(Limits.A.X, P.Y);
        kbEnd: Update(Limits.B.X - S.X, P.Y);
        kbPgUp: Update(P.X, Limits.A.Y);
        kbPgDn: Update(P.X, Limits.B.Y - S.Y);
      end;
      MoveGrow(P, S);
    until (Event.KeyCode = kbEnter) or (Event.KeyCode = kbEsc);
    DrawFrame(NewBounds) {OOA};
    if Event.KeyCode = kbEsc then Locate(SaveBounds) else
      Locate(NewBounds)  {OOA};
  end;
  SetState(sfDragging, False);
  UnLockMouseShape;
  MouseMastering(Event);
  if CursorIsShown then TextCursor.Show;
  ResetCursor; {DK}
end;

procedure TView.RAssign(var Rect: TRect; AX,AY,BX,BY: Integer);
var
  W, H: Integer;
begin
  W:= CharWidth;
  H:= FontHeight;
  Rect.Assign(AX*W, AY*H, BX*W, BY*H);
end;

function TView.GetFrameWidth: Integer; {OOA}
begin
  GetFrameWidth:= 0;
end;

function TView.GetCaptureHeight: Integer; {OOA}
begin
  GetCaptureHeight:= 0;
end;

procedure TView.MouseMastering(const Event: TEvent); {TONY}
begin
  {SetMouseCursorShape(@DefaultMouseCursor);}
  SetMouseCursorShape(CursorShape);
end;

Procedure TView.SetColor(Color : LongInt);
Begin
  if (LogPalette.Mode and pmHiColor) = 0 then Color := ColorIndex^[Color];
  PaintInfo.Fore := Color;
End;

Procedure TView.SetPaint(Fore, Back:LongInt; Operation : Byte; LinePattern : Word;
                         LineWidth, LineStyle, Pattern : Byte);{DK}
Begin
  if (LogPalette.Mode and pmHiColor)= 0 then begin
    Fore := ColorIndex^[Fore];
    Back := ColorIndex^[Back];
  end;
  PaintInfo.Fore := Fore;
  PaintInfo.Back := Back;
  PaintInfo.Operation   := Operation;
  PaintInfo.LinePattern := LinePattern;
  PaintInfo.LineWidth   := LineWidth;
  PaintInfo.LineStyle   := LineStyle;
  if Pattern < fsNoUse then PaintInfo.Pattern := FillPatterns[Pattern];
End;

Procedure TView.SetTextMetrics(AFont, Width, Height : Byte; Escapement, GapLength : Word;
                               Italic : Byte; FontStyle : Word);{DK}
Begin
  Font.Font         := AFont;
  Font.Width        := Width;
  Font.Height       := Height;
  Font.GapLength    := GapLength;
  Font.Escapement   := Escapement;
  Font.Italic       := Italic;
  Font.Style        := FontStyle;
  SelectFontCaps(Font);
End;

Procedure TView.SetBitmapOrg(X, Y : Integer);
Var
  P : TPoint;
Begin
  P.X := X; P.Y := Y;
  MakeGlobal(P, PaintInfo.BitmapOrg);
End;

Procedure TView.SolidBrush(R, G, B : Byte);
Begin
  if MaxColors = 2 then MonoDither(R, G, B, PaintInfo) else
  if MaxColors <= 256 then MakeDither(R, G, B, MaxColors, PaintInfo, Nil)
    else if MaxColors <= 65536 then MakeHiDither(R, G, B, PaintInfo)
      else begin
        PaintInfo.Fore        := TrueColor(R, G, B);
        PaintInfo.LineStyle   := lsLinePattern;
        PaintInfo.LinePattern := psSolid;
      end;
  PaintInfo.Operation := CopyPut;
End;

Function  TView.MonoColor; {DK}
Begin
  if MaxColors > 2 then MonoColor := Color else
    if Purpose = 0 then MonoColor := RGB(0, 0, 0)
      else MonoColor := RGB(255, 255, 255);
End;

procedure TView.HLine;
var
  R: TRect;
  Max, Min : integer;

  function PrintItem(Item: PRect): boolean; far;
  begin
    PrintItem := False;
    if (Y >= Item^.A.Y) and (Y < Item^.B.Y) and
      (Item^.A.X < X2) and (Item^.B.X >= X1)
      then
    begin
      Max := X1; if Item^.A.X > Max then Max := Item^.A.X;
      Min := X2; if Item^.B.X < Min then Min := Item^.B.X;
      if Max >= Min then Exit; {TONY}
      R.Assign(Max, Y, Min, Y);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      {$IFDEF DEBUG}
      if (R.A.X < 0) or (R.A.Y < 0) or (R.B.X > ScreenWidth) then
        RunError(255);
      {$ENDIF}
      GDI.HLine(R.A.X, R.A.Y, Pred(R.B.X), Fore);
    end else if Item^.A.Y > Y then
      PrintItem := True;
  end;

begin
  if (LogPalette.Mode and pmHiColor)= 0 then Fore := ColorIndex^[Fore];
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.FrameStyle(X1, Y1, X2, Y2: integer);
Begin
  Line(X1, Y1, X2, Y1);
  Line(X1, Y1, X1, Y2);
  Line(X1, Y2, X2, Y2);
  Line(X2, Y1, X2, Y2);
End;

procedure TView.HLineStyle(X1, Y, X2: integer; Pattern:Byte; Clr0, Clr1: LongInt);
var
  R: TRect;
  Max, Min : integer;

  function PrintItem(Item: PRect): boolean; far;
  begin
    PrintItem := False;
    if (Y >= Item^.A.Y) and (Y < Item^.B.Y) and
    {
       (((Item^.A.X >= X1) and (Item^.A.X <= X2)) or
       ((Item^.B.X >= X1) and (Item^.B.X <= X2)))
    }
      (Item^.A.X < X2) and (Item^.B.X >= X1)
      then
    begin
      Max := X1; if Item^.A.X > Max then Max := Item^.A.X;
      Min := X2; if Item^.B.X < Min then Min := Item^.B.X;
      if Max >= Min then Exit; {TONY}
      R.Assign(Max, Y, Min, Y);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      {$IFDEF DEBUG}
      if (R.A.X < 0) or (R.A.Y < 0) or (R.B.X > ScreenWidth) then
        RunError(255);
      {$ENDIF}
      GDI.HLineStyle(R.A.X, R.A.Y, Pred(R.B.X), Pattern, Clr0, Clr1);
    end else if Item^.A.Y > Y then
      PrintItem := True;
  end;

begin
  if (LogPalette.Mode and pmHiColor)= 0 then Clr0 := ColorIndex^[Clr0];
  if (LogPalette.Mode and pmHiColor)= 0 then Clr1 := ColorIndex^[Clr1];
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.VLine;
var
  R: TRect;
  Max, Min : integer;

  function PrintItem(Item: PRect): boolean; far;
  begin
    PrintItem := False;
    if (X >= Item^.A.X) and (X < Item^.B.X) and
      (Item^.A.Y < Y2) and (Item^.B.Y >= Y1)
       then
    begin
      Max := Y1; if Item^.A.Y > Max then Max := Item^.A.Y;
      Min := Y2; if Item^.B.Y < Min then Min := Item^.B.Y;
      if Max >= Min then Exit; {TONY}
      R.Assign(X, Max, X, Min);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      {$IFDEF DEBUG}
      if (R.A.X < 0) or (R.A.Y < 0) or (R.B.Y > ScreenHeight) then
        RunError(255);
      {$ENDIF}
      GDI.VLine(R.A.X, R.A.Y, Pred(R.B.Y), Fore);
    end else if Item^.A.Y > Y2 then
      PrintItem := True;
  end;

begin
  if (LogPalette.Mode and pmHiColor)= 0 then Fore := ColorIndex^[Fore];
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.Bar;
var
  R: TRect;
  S:TPaintInfo;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X1, Y1, X2, Y2);
    R.Intersect(Item^);
    if not R.Empty then begin
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      GDI.Bar(R.A.X, R.A.Y, Pred(R.B.X), Pred(R.B.Y), Fore);
    end;
  end;

begin
  if MaxColors = 2 then begin
    S := PaintInfo;
    SolidBrush(PMainPalette^[Fore, 1], PMainPalette^[Fore, 2], PMainPalette^[Fore, 3]);
    BarStyle(x1, y1, x2, y2);
    PaintInfo := S;
    Exit;
  end;
  if (LogPalette.Mode and pmHiColor) = 0 then Fore := ColorIndex^[Fore];
  PaintInfo.ClipRect.Assign(0, 0, ScreenWidth, ScreenHeight);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.FillCircle(X, Y, R: Integer);{DK}
Begin
  FillEllipse(X, Y, R, R);
End;

procedure TView.Circle(X, Y, R: Integer);{DK}
Begin
  Ellipse(X, Y, R, R);
End;

procedure TView.FillEllipse(X, Y, Rx, Ry: Integer);{DK}
var
  R: TRect;
  P: Tpoint;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y + Ry then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X-Rx-1, Y-Ry-1, X+Rx+2, Y+Ry+2);
    R.Intersect(Item^);
    if not R.Empty then with R do begin
      MakeGlobal(A, A);
      MakeGlobal(B, B);
      P.X := X; P.Y := Y;
      MakeGlobal(P, P);
      PaintInfo.ClipRect.Copy(R);
      Dec(PaintInfo.ClipRect.B.X);
      Dec(PaintInfo.ClipRect.B.Y);
      GDI.FillEllipse(P.X, P.Y, Rx, Ry, PaintInfo);
    end;
  end;

begin
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.Ellipse(X, Y, Rx, Ry: Integer);{DK}
var
  R: TRect;
  P: Tpoint;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y + Ry then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X-Rx-1, Y-Ry-1, X+Rx+2, Y+Ry+2);
    R.Intersect(Item^);
    if not R.Empty then with R do begin
      MakeGlobal(A, A);
      MakeGlobal(B, B);
      P.X := X; P.Y := Y;
      MakeGlobal(P, P);
      PaintInfo.ClipRect.Copy(R);
      Dec(PaintInfo.ClipRect.B.X);
      Dec(PaintInfo.ClipRect.B.Y);
      GDI.Ellipse(P.X, P.Y, Rx, Ry, PaintInfo);
    end;
  end;

begin
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.FillPoly(NumPoint : Integer; var Polygon);{DK}
var
  R: TRect;
  P: Tpoint;

  function PrintItem(Item: PRect): boolean; far;
  begin
    PrintItem := False;
    R.Copy(Item^);
    if not R.Empty then with R do begin
      MakeGlobal(A, A);
      MakeGlobal(B, B);
      PaintInfo.ClipRect.Copy(R);
      A.X := 0; A.Y := 0;
      MakeGlobal(A, A);
      Dec(PaintInfo.ClipRect.B.X);
      Dec(PaintInfo.ClipRect.B.Y);
      GDI.FillPoly(NumPoint, Polygon, A, PaintInfo);
    end;
  end;

begin
  ClipRects^.FirstThat(@PrintItem);
end;


procedure TView.Line(X1, Y1, X2, Y2 : integer);{DK/GIO}
var YR: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    PrintItem := False;
    YR.Assign(X1, Y1, X2, Y2);
    PaintInfo.ClipRect.Copy(Item^);
    if not PaintInfo.ClipRect.Empty then begin
      MakeGlobal(PaintInfo.ClipRect.A, PaintInfo.ClipRect.A);
      MakeGlobal(PaintInfo.ClipRect.B, PaintInfo.ClipRect.B);
      MakeGlobal(YR.A, YR.A);
      MakeGlobal(YR.B, YR.B);
      Dec(PaintInfo.ClipRect.B.X);
      Dec(PaintInfo.ClipRect.B.Y);
      GDI.Line(YR.A.X, YR.A.Y, YR.B.X, YR.B.Y, PaintInfo);
    end;
  end;

begin
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.BarStyle(X1, Y1, X2, Y2: integer);{DK}
var
  R: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X1, Y1, X2, Y2);
    R.Intersect(Item^);
    if not R.Empty then begin
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      GDI.BarStyle(R.A.X, R.A.Y, Pred(R.B.X), Pred(R.B.Y), PaintInfo);
    end;
  end;

begin
  PaintInfo.ClipRect.Assign(0, 0, ScreenWidth, ScreenHeight);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.RoundBar(X1, Y1, X2, Y2, X3, Y3: integer);{DK}
var
  R: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X1, Y1, X2, Y2);
    R.Intersect(Item^);
    if not R.Empty then begin
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      GDI.RoundBar(R.A.X, R.A.Y, Pred(R.B.X), Pred(R.B.Y), X3, Y3, PaintInfo);
    end;
  end;

begin
  PaintInfo.ClipRect.Assign(0, 0, ScreenWidth, ScreenHeight);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.RoundRect(X1, Y1, X2, Y2, X3, Y3: integer);{DK}
var
  R: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X1, Y1, X2, Y2);
    R.Intersect(Item^);
    if not R.Empty then begin
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      GDI.RoundRect(R.A.X, R.A.Y, Pred(R.B.X), Pred(R.B.Y), X3, Y3, PaintInfo);
    end;
  end;

begin
  PaintInfo.ClipRect.Assign(0, 0, ScreenWidth, ScreenHeight);
  ClipRects^.FirstThat(@PrintItem);
end;



procedure TView.FrameBar(X1, Y1, X2, Y2: integer; UpLt, DnRt, Back: Integer);
begin
  Bar{Pattern}(X1, Y1, X2, Y2, Back);
  Rectangle(X1, Y1, X2, Y2, 1, UpLt, DnRt);
end;

procedure TView.SetPixel; {TONY}
var
  P: TPoint;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > y then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    if Item^.Contains(P) then begin
      MakeGlobal(P, P);
      GDI.SetPixel(P.X, P.Y, Color);
      PrintItem := True;
    end;
  end;

begin
  P.X := x; P.Y := y;
  if (LogPalette.Mode and pmHiColor)= 0 then Color := ColorIndex^[Color];
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.ScrollY(x1, y1, x2, y2, How: integer); {TONY}
var
  R, RV: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Copy(RV);
    R.Intersect(Item^);
    if not R.Empty then begin
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      GDI.ScrollY(R.A.X, R.A.Y, R.B.X, R.B.Y, How);
    end;
  end;

begin
  RV.Assign(x1, y1, x2, y2);
  ClipRects^.FirstThat(@PrintItem);
  if How > 0 then begin
    if RV.B.Y - How > RV.A.Y then
      RV.B.Y := RV.B.Y - How
    else
      RV.Assign(0, 0, 0, 0);
  end else if How < 0 then begin
    if RV.A.Y - How < RV.B.Y then
      RV.A.Y := RV.A.Y - How
    else
      RV.Assign(0, 0, 0, 0);
  end;
  ValidateRect(RV);
  ClipRects^.Sort;
end;

procedure TView.Scroll;
var
  R, RV, D: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if (Item^.A.Y > Y2) or (Item^.A.X > X2) then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;

    D.Copy(Item^);
    Dec(D.B.X); Dec(D.B.Y);
    if DY < 0 then Dec(D.A.Y, DY) else Dec(D.B.Y, DY);
    if DX < 0 then Dec(D.A.X, DX) else Dec(D.B.X, DX);
    R.Copy(RV);
    R.Intersect(D);
    if not R.Empty then begin
      R.Move(DX, DY);
      ValidateRect(R);
      R.Move(-DX, -DY);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      GDI.ScrollDraw(R.A.X, R.A.Y, R.B.X, R.B.Y, R.A.X + DX, R.A.Y + DY);
    end;
  end;

begin
  RV.Assign(x1, y1, x2, y2);
  ClipRects^.FirstThat(@PrintItem);
  ClipRects^.Sort;
end;


procedure TView.PutBMP(Image: PImage; X, Y: integer); {TONY}
Begin
  PutBMPPart(Image, X, Y, 0, 0);
End;

procedure TView.PutBMPPart(Image: PImage; X, Y, XFrom, YFrom: integer); {TONY}
var
  R, R1: TRect;
  Y2: Integer;
  SaveCI : Pointer;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X, Y, ScreenWidth, ScreenHeight);
    R.Intersect(Item^);
    if not R.Empty then begin
      R1.Copy(R);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      if MaxColors = 2 then begin
        SaveCI := PMainPalette;
        if LogPalette.Palette <> Nil then PMainPalette := LogPalette.Palette;
        GDI.PutBMPPart(Image, R.A.X, R.A.Y,
          R1.A.X-X+XFrom, R1.A.Y-Y+YFrom, R.B.X-R.A.X, R.B.Y-R.A.Y);
        PMainPalette := SaveCI;
      end else if MaxColors <= 256 then begin
        if ((LogPalette.Mode and pmOptimize) <> 0) and
           ((LogPalette.Mode and pmDirectBM) = 0)
        then
          {DK}GDI.PutBMPPartOp(Image, R.A.X, R.A.Y,
          R1.A.X-X+XFrom, R1.A.Y-Y+YFrom, R.B.X-R.A.X, R.B.Y-R.A.Y, CopyPut, LogPalette.ColorRef)
        else
        GDI.PutBMPPart(Image, R.A.X, R.A.Y,
          R1.A.X-X+XFrom, R1.A.Y-Y+YFrom, R.B.X-R.A.X, R.B.Y-R.A.Y);
      end else begin
        SaveCI := ColorIndex;
        if LogPalette.Palette <> Nil then begin
          MakeHiPalette(LogPalette, MaxColors > 65536);
          Pointer(ColorIndex) := LogPalette.ColorRef
        end;
        GDI.PutBMPPart(Image, R.A.X, R.A.Y,
          R1.A.X-X+XFrom, R1.A.Y-Y+YFrom, R.B.X-R.A.X, R.B.Y-R.A.Y);
        ColorIndex := SaveCI;
      end;
    end;
  end;

begin
  if IllegalImage(Image) then
  {$IFDEF Debug}
    RunError(255);
  {$ELSE}
    Exit;
  {$ENDIF}
  Y2 := Y + BitMapHeight(Image);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.StretchBMP; {DK}
var
  R, R1: TRect;
  Y2: Integer;
  RX, RY : Integer;
  A      : TPoint;
  SaveCI : Pointer;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X, Y, ScreenWidth, ScreenHeight);
    R.Intersect(Item^);
    if not R.Empty then begin
      R1.Copy(R);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      if MaxColors = 2 then begin
        SaveCI := PMainPalette;
        if LogPalette.Palette <> Nil then PMainPalette := LogPalette.Palette;
        GDI.StretchDIBitmap(Image, Nil, A.X, A.Y, 0, 0, Pred(RX), Pred(RY), ToX, ToY, nil, R);
        PMainPalette := SaveCI;
      end else if MaxColors = 256 then begin
        if ((LogPalette.Mode and pmOptimize) <> 0) and
           ((LogPalette.Mode and pmDirectBM) = 0)
        then
          GDI.StretchDIBitmap(Image, Nil, A.X, A.Y, 0, 0, Pred(RX), Pred(RY), ToX, ToY, LogPalette.ColorRef, R)
        else
          GDI.StretchDIBitmap(Image, Nil, A.X, A.Y, 0, 0, Pred(RX), Pred(RY), ToX, ToY, nil, R)
      end else begin
        SaveCI := ColorIndex;
        if LogPalette.Palette <> Nil then begin
          MakeHiPalette(LogPalette, MaxColors > 65536);
          Pointer(ColorIndex) := LogPalette.ColorRef
        end;
        StretchDIBitmap(Image, Nil, A.X, A.Y, 0, 0, Pred(RX), Pred(RY), ToX, ToY, LogPalette.ColorRef, R);
        ColorIndex := SaveCI;
      end;
    end;
  end;


begin
  if IllegalImage(Image) then
  {$IFDEF Debug}
    RunError(255);
  {$ELSE}
    Exit;
  {$ENDIF}
  if (ToX = 0) or (ToY = 0) then Exit;
  A.X := X;
  A.Y := Y;
  MakeGlobal(A, A);
  RX := BitMapWidth(Image);
  RY := BitMapHeight(Image);
  Y2 := Y + Abs(ToY);
  ClipRects^.FirstThat(@PrintItem);
end;


procedure TView.PutBMPOp(Image: PImage; X, Y: integer; Operation: byte); {TONY}
var
  R, R1: TRect;
  Y2: Integer;
  TLP : TLogPalette;
  Mode : Word;
  PP : PColorRef;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > Y2 then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R.Assign(X, Y, ScreenWidth, ScreenHeight);
    R.Intersect(Item^);
    if not R.Empty then begin
      R1.Copy(R);
      MakeGlobal(R.A, R.A);
      MakeGlobal(R.B, R.B);
      if MaxColors = 256 then begin
        if ((LogPalette.Mode and pmOptimize) <> 0) and
           ((LogPalette.Mode and pmDirectBM) = 0)
        then
          {DK}GDI.PutBMPPartOp(Image, R.A.X, R.A.Y,
          R1.A.X-X, R1.A.Y-Y, R.B.X-R.A.X, R.B.Y-R.A.Y, Operation, LogPalette.ColorRef)
        else
        GDI.PutBMPPartOp(Image, R.A.X, R.A.Y,
          R1.A.X-X, R1.A.Y-Y, R.B.X-R.A.X, R.B.Y-R.A.Y, Operation, @StdColorRefMap);
      end else begin
        if MaxColors > 256 then begin
          if (LogPalette.Mode and pmComplex) <> 0 then begin
            if MaxColors > 65536 then Mode := cbwCreateTCMap
              else Mode := cbwCreateHCMap;
            CreatePalette(LogPalette.Palette, TLP, cbwInit + Mode, LogPalette.Colors);
            PP := TLP.ColorRef;
          end else begin
            MakeHiPalette(LogPalette, MaxColors > 65536);
            PP := LogPalette.ColorRef;
          end;
        end;
        GDI.PutBMPPartOp(Image, R.A.X, R.A.Y,
          R1.A.X-X, R1.A.Y-Y, R.B.X-R.A.X, R.B.Y-R.A.Y, Operation, PP);
        if (MaxColors > 256) and ((LogPalette.Mode and pmComplex) <> 0) then
          DisposePalette(TLP);
      end;
    end;
  end;

begin
  if IllegalImage(Image) then
  {$IFDEF Debug}
    RunError(255);
  {$ELSE}
    Exit;
  {$ENDIF}
  Y2 := Y + BitMapHeight(Image);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.Rectangle(X1, Y1, X2, Y2, W, UpLt, DnRt: integer);
var
  i: integer;

  procedure DoRect;
  begin
    HLine(X1, Y1, X2, UpLt);
    VLine(X1, Y1, Y2, UpLt);
    HLine(X1, Pred(Y2), X2, DnRt);
    VLine(Pred(X2), Y1, Y2, DnRt);
  end;

begin
  for i := 1 to W do begin
    DoRect;
    Dec(X2);
    Dec(Y2);
    Inc(X1);
    Inc(Y1);
  end;
end;

procedure TView.WrStr(X, Y: integer; const S: String; Attr: LongInt);
var
  P: TPoint;
  R, R1: TRect;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > R.B.Y then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R1.Copy(R);
    R1.Intersect(Item^);
    if not R1.Empty then begin
      MakeGlobal(R1.A, R1.A);
      MakeGlobal(R1.B, R1.B);
      {$IFDEF DEBUG}
      if (R1.A.X < 0) or (R1.A.Y < 0) or (R1.B.X > ScreenWidth) or (R1.B.Y > ScreenHeight) then
        RunError(255);
      {$ENDIF}
      PaintInfo.Fore := Attr;
      if (LogPalette.Mode and pmHiColor)= 0 then PaintInfo.Fore := ColorIndex^[Lo(PaintInfo.Fore)];
      PaintInfo.ClipRect.Copy(R1);
      EGFont.WrStr( P.X, P.Y, S, PaintInfo);
    end;
  end;

begin
  P.X := X; P.Y := Y;
  MakeGlobal(P, P);
  SelectFont(Font.Font);
  if (GetFont(Font.Font)^.Flags and ffStroked) <> 0 then R.Assign(0, 0, ScreenWidth, ScreenHeight)
    else R.Assign(X, Y, X+FontWidth(S), Y+FontHeight);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.WrCStr(X, Y: integer; const S: String; Attr: LongInt);
var
  i, j: integer;
  P: TPoint;
  R, R1: TRect;
  XX: integer;
  S1: string;
  Underlined: boolean;

  function UnderStr(l: Integer): string;
  var
    s: string;
  begin
    FillChar(s[1], l, '_'); s[0] := char(l);
    UnderStr := s;
  end;

  function PrintItem(Item: PRect): boolean; far;
  begin
    if Item^.A.Y > R.B.Y then begin
      PrintItem := True;
      Exit;
    end;
    PrintItem := False;
    R1.Copy(R);
    R1.Intersect(Item^);
    if not R1.Empty then begin
      MakeGlobal(R1.A, R1.A);
      MakeGlobal(R1.B, R1.B);
      {$IFDEF DEBUG}
      if (R1.A.X < 0) or (R1.A.Y < 0) or (R1.B.X > ScreenWidth)  or (R1.B.Y > ScreenHeight) then
        RunError(255);
      {$ENDIF}
      PaintInfo.Fore := Attr;
      if (LogPalette.Mode and pmHiColor)= 0 then PaintInfo.Fore := ColorIndex^[Lo(PaintInfo.Fore)];
      PaintInfo.ClipRect.Copy(R1);
      EGFont.WrStr(P.X, P.Y, S1, PaintInfo);
      if Underlined then
        EGFont.WrStr(P.X, P.Y, UnderStr(Length(S1)), PaintInfo);
    end;
  end;

begin
  P.X := X;
  P.Y := Y;
  MakeGlobal(P, P);
  SelectFont(Font.Font);
  i := 1;
  j := i;
  Underlined := False;
  while j < length(S) do begin
    if S[j] = '~' then begin
      S1 := Copy(S, i, j-i);
      XX := FontWidth(S1);
      R.Assign(X, Y, X+XX, Y+FontHeight);
      ClipRects^.FirstThat(@PrintItem);
      Inc(X, XX);
      Inc(P.X, XX);
      Inc(j);
      i := j;
      Attr := Swap(Attr);
      Underlined := not Underlined;
    end;
    Inc(j);
  end;
  if (S[j] = '~') or (j > length(S)) then dec(j);
  S1 := Copy(S, i, j-i+1);
  XX := FontWidth(S1);
  R.Assign(X, Y, X+XX, Y+FontHeight);
  ClipRects^.FirstThat(@PrintItem);
end;

procedure TView.RealizePalette;{DK}
Begin
  if MaxColors <> 256 then Exit;
  if ((LogPalette.Mode and pmUseRGB) = 0) or (PaletteLockCounter <> 0) then Exit;
  RedrawPalette;
End;

{™‚Æ ®·ØÆ´Ïß„•‚ pmDirectBM §Æ´¶•≠ Ø•‡•™‡Î‚Ï UpdateColors Á‚Æ°Î ¢®§Æ®ß¨•≠®‚Ï
 ·¢Æ® Ê¢•‚Æ¢Î• ‚†°´®ÊÎ. ù‚Æ‚ Ì‰‰•™‚ ¨Æ¶•‚ °Î‚Ï ®·ØÆ´ÏßÆ¢†≠ §´Ô Æ°Æ•¢,
 ™Æ‚Æ‡Î• Ø‡® UpdateColors 1 ‡†ß Ø•‡•·Á®‚†Ó‚·Ô † §†´ÏË• °„§„‚ ¢Î¢Æ§®‚Ï·Ô
 °•ß Ø•‡•™Æ§®‡Æ¢™® ØÆ PaintInfo.ColorRef, Á‚Æ ≠•·™Æ´Ï™Æ °Î·‚‡••.
 PS - Ô·≠Æ• §•´Æ ‚Æ´Ï™Æ §´Ô 256 Ê¢•‚Æ¢}
Function TView.UpdateColors : Boolean; {DK}
Begin
  UpdateColors := True;
End;

function TView.FontWidth(const S: String): integer;
begin
  SelectFontCaps(Font);
  FontWidth := GetWidth(S);
end;

function TView.CharWidth: integer;
begin
  SelectFontCaps(Font);
  CharWidth := GetCharWidth;
end;

function TView.FontHeight: integer;
begin
  SelectFontCaps(Font);
  FontHeight := GetHeight;
end;

procedure TView.CalcClipRects; {OOA}

var
  Solid: TRect;

  procedure CheckSolidRect;
  var
    I, C: Integer;
    R, RS: TRect;
  begin
    C:= ClipRects^.Count;
    I:= 0;
    while I < C do
    begin
      R.Copy(PRect(ClipRects^.At(I))^);
      RS.Copy(Solid);
      RS.Intersect(R);
      with RS do if Empty then Inc(I) else
      begin
        ClipRects^.AtFree(I);
        Dec(C);
        if Contains(R.A) and Contains(R.B) then Continue;
        ClipRects^.InsertRect(R.A.X, R.A.Y, R.B.X, A.Y);
        ClipRects^.InsertRect(R.A.X, B.Y, R.B.X, R.B.Y);
        ClipRects^.InsertRect(R.A.X, A.Y, A.X, B.Y);
        ClipRects^.InsertRect(B.X, A.Y, R.B.X, B.Y);
      end;
    end;
  end;

var
  Own: PGroup;
  Vie, V: PView;
  P: TPoint;
  R: TRect;
begin
  if Owner = Nil then Exit;
  {Calc}GetClipRect(R);
  ClipRects^.Intersect(R);
{  ClipRects^.Update; ???}

  Longint(P):= 0;
  Vie:= @Self;
  Own:= Owner;
  while Own <> Nil do
  begin
    if Own^.Last <> Nil then
    begin
      V:= Own^.Last^.Next;
      Dec(P.X, Vie^.Origin.X);
      Dec(P.Y, Vie^.Origin.Y);
      while V <> Vie do
      begin
        if (V^.State and sfVisible <> 0) and (V^.Options and ofTransparent = 0)
        then begin
          V^.GetBounds(Solid);
          Solid.Move(P.X, P.Y);
          CheckSolidRect;
        end;
        V:= V^.NextView;
      end;
    end;
    Vie:= Own;
    Own:= Own^.Owner;
  end;
  ClipRects^.Update;
end;

procedure TView.DrawBegin {TONY,OOA,DK};
var
  R1: TRect;

  RR: TRect;
  MouseInOutput: boolean;
  MouseRegion: TRect;
  CursorIsShown, CursorInOutPut: boolean;
  CursorRegion: TRect;
begin
{  CalcClipRects;}
  if BufferedStrategy then begin
    if DrawLockCounter=0 then begin
      ClipRects^.UnitedRect(RefreshRegion); {OOA}
      MakeGlobal(RefreshRegion.A, RefreshRegion.A);
      MakeGlobal(RefreshRegion.B, RefreshRegion.B);
    end;
    Inc(DrawLockCounter);
    PrepareDrawing;
  end else begin
    ClipRects^.UnitedRect(RefreshRegion); {OOA}
    MakeGlobal(RefreshRegion.A, RefreshRegion.A);
    MakeGlobal(RefreshRegion.B, RefreshRegion.B);
    MouseRegion.Assign(MouseWhere.X-CurrentMouseCursor^.XHotSpot,
                       MouseWhere.Y-CurrentMouseCursor^.YHotSpot,
                       MouseWhere.X-CurrentMouseCursor^.XHotSpot+MouseCursorSize.X-1,
                       MouseWhere.Y-CurrentMouseCursor^.YHotSpot+MouseCursorSize.Y-1);
    TextCursor.GetCursorRect(CursorRegion);
    RR.Copy(RefreshRegion);
    with RR do
    begin
      A.X := (A.X SHR 4) SHL 4;
      B.X := ((B.X SHR 4) + 1) SHL 4;
    end;
    MouseRegion.Intersect(RR);
    CursorRegion.Intersect(RR);
    CursorInOutput := not CursorRegion.Empty;
    MouseInOutput := not MouseRegion.Empty;
    if MouseInOutput then HideMouse;
    ShouldCursor := TextCursor.IsShown;
    ShouldMouse  := MouseInOutput;
    if ShouldCursor {and CursorInOutPut }then TextCursor.Hide;
  {  UpdateMouse; HoldMouse(True);   }
    if not(ShouldMouse) and not(MouseTrapOccurs) and MouseTrapRect.Empty then begin
      MouseTrapRect.Copy(RefreshRegion);
      SetTrap := True;
    end;
  end;
end;

procedure TView.DrawEnd {TONY,DK};
var
  RR: TRect;
  MouseInOutput: boolean;
  MouseRegion: TRect;
  CursorIsShown, CursorInOutPut: boolean;
  CursorRegion: TRect;
begin
  {$IFDEF DEBUG}
  if DrawLockCounter = 0 then RunError(255);
  if ClipRects^.Count <> 0 then RunError(255);
  {$ENDIF}

  if BufferedStrategy then begin
    Dec(DrawLockCounter);
    if DrawLockCounter=0 then begin
      MouseRegion.Assign(MouseWhere.X-CurrentMouseCursor^.XHotSpot,
                         MouseWhere.Y-CurrentMouseCursor^.YHotSpot,
                         MouseWhere.X-CurrentMouseCursor^.XHotSpot+MouseCursorSize.X-1,
                         MouseWhere.Y-CurrentMouseCursor^.YHotSpot+MouseCursorSize.Y-1);
      TextCursor.GetCursorRect(CursorRegion);
      RR.Copy(RefreshRegion);
      with RR do
      begin
        A.X := (A.X SHR 4) SHL 4;
        B.X := ((B.X SHR 4) + 1) SHL 4;
      end;
      MouseRegion.Intersect(RR);
      CursorRegion.Intersect(RR);
      CursorInOutput := not CursorRegion.Empty;
      MouseInOutput := not MouseRegion.Empty;
      if MouseInOutput then HideMouse;
      CursorIsShown := TextCursor.IsShown;
      if CursorIsShown and CursorInOutPut then TextCursor.Hide;
      HoldMouse(True);
      PutBufferPart(RefreshRegion.A.X, RefreshRegion.A.Y,
                   RefreshRegion.B.X, RefreshRegion.B.Y);{ - ß†¨•≠•≠ ≠† unscrew.}
      HoldMouse(False);
      if CursorIsShown and CursorInOutPut then TextCursor.Show;
      if MouseInOutput then ShowMouse;
    end;
  end else begin
    if ShouldCursor then TextCursor.Show;
    if ShouldMouse then ShowMouse;
    if SetTrap then begin
      if MouseTrapOccurs then ShowMouse;
      MouseTrapOccurs := False;
      SetTrap := False;
      MouseTrapRect.Assign(0, 0, 0, 0);
    end;
    {HoldMouse(False); UpdateMouse;}
  end;
end;

procedure TView.InvalidateRect(Rect: TRect); {OOA}
begin
  if State and sfVisible <> 0 then ClipRects^.Union(Rect);
end;

procedure TView.ValidateRect(Rect: TRect); {OOA}
begin
  if State and sfVisible <> 0 then ClipRects^.Cut(Rect);
end;

procedure TView.InvalidateSelf; {OOA}
var
  R: TRect;
begin
  GetExtent(R);
  InvalidateRect(R);
end;

procedure TView.ValidateSelf; {OOA}
var
  R: TRect;
begin
  GetExtent(R);
  ValidateRect(R);
end;
(*
procedure TView.InvalidateUnderSelf; {OOA}
var
  R: TRect;
begin
  if Owner <> Nil then
  begin
    GetExtent(R);
    R.Move(Origin.X, Origin.Y); {convert to owner's coords}
    Owner^.InvalidateRect(R);
  end;
end;
*)
procedure TView.GetInvalidExtent(var Extent: TRect); {OOA}
begin
  ClipRects^.UnitedRect(Extent);
end;

  function Locked(P: PGroup): boolean;
  begin
    Locked := False;
    while P<>Nil do begin
      if P^.LockFlag <> 0 then begin
        Locked := True;
        Exit;
      end;
      P := P^.Owner;
    end;
  end;


procedure TView.DrawInvalidated; {OOA}


  {$IFDEF DEBUG}
  procedure DrawClip(P: PRect); far;
  var
    R1: TRect;
  begin
    R1.Copy(P^);
    MakeGlobal(R1.A, R1.A);
    MakeGlobal(R1.B, R1.B);
    with R1 do DrawXORFrame(a.x, a.y, b.x-1, b.y-1, 2);
    if (R1.A.X < 0) or (R1.A.Y < 0) or
      (R1.B.X > ScreenWidth) or (R1.B.Y > ScreenHeight) then
        RunError(255);
  end;
  {$ENDIF}

var
  I: Integer;
begin
  if Locked(Owner) then Exit;
  if Exposed then
  begin
    if Owner = Nil then Draw else
    begin
      CalcClipRects;
      if ClipRects^.Count > 0 then
      begin
        DrawBegin;
        {$IFDEF DEBUG}
        if GetShiftState and kbScrollState <> 0 then
        begin
          ClipRects^.ForEach(@DrawClip);
          GetKey;
          ClipRects^.ForEach(@DrawClip);
        end;
        {$ENDIF}
        Draw;
        ClipRects^.FreeAll;
        DrawEnd;
        DrawCursor;
      end;
    end;
  end;
end;

procedure TView.Draw {TONY};
begin
  Bar{Pattern}(0, 0, Size.X, Size.Y, GetColor(1));
end;

procedure TView.DrawCursor;
begin
  if State and sfFocused <> 0 then
    ResetCursor;
end;

procedure TView.DrawHide(LastView: PView);
begin
  {LogPalette.Mode := LogPalette.Mode and not(pmEnable);}
  PaletteEnable(False);
  RealizePalette;
  ClipRects^.FreeAll;
  DrawCursor;
  DrawUnderView(State and sfShadow <> 0, LastView);
end;

procedure TView.DrawShow(LastView: PView);
begin
  PaletteEnable(True);
  {LogPalette.Mode := LogPalette.Mode or pmEnable or pmDrawFirst;}
  RealizePalette;
  InvalidateSelf;
  DrawInvalidated;
{  if State and sfShadow <> 0 then DrawUnderView(True, LastView); OOA}
end;

procedure TView.DrawUnderRect(var R: TRect; LastView: PView);
var
  RR: TRect;

  procedure InvalidateSubview(P: PView); far;
  begin
    RR.Copy(R);
    RR.Move(-P^.Origin.X, -P^.Origin.Y);
    P^.InvalidateRect(RR);
  end;

begin
  Owner^.Clip.Intersect(R);
  Owner^.InvalidateRect(R);
{  Owner^.ForEach(@InvalidateSubview);}
  Owner^.DrawSubViews(NextView, LastView);
  Owner^.GetExtent(Owner^.Clip);
end;

procedure TView.DrawUnderView(DoShadow: Boolean; LastView: PView);
var
  R: TRect;
begin
  GetBounds(R);
  DrawUnderRect(R, LastView);
end;

procedure TView.DrawView;
begin
  InvalidateSelf;
  DrawInvalidated;
end;

procedure TView.DrawViewDirect;
Begin
  if BufferedStrategy then begin
    SetOutput(False);
    BufferedStrategy := False;
    DrawView;
    BufferedStrategy := True;
    SetOutput(True);
  end else DrawView;
End;

procedure TView.EnableCommands(Commands: TCommandSet);
begin
  CommandSetChanged := CommandSetChanged or
    (CurCommandSet * Commands <> Commands);
  CurCommandSet := CurCommandSet + Commands;
end;

procedure TView.EndModal(Command: Word);
var
  P: PView;
begin
  P := TopView;
  if TopView <> nil then TopView^.EndModal(Command);
end;

function TView.EventAvail: Boolean;
var
  Event: TEvent;
begin
  GetEvent(Event);
  if Event.What <> evNothing then PutEvent(Event);
  EventAvail := Event.What <> evNothing;
end;

procedure TView.GetBounds(var Bounds: TRect); assembler;
asm
        PUSH    DS
        LDS     SI,Self
        ADD     SI,OFFSET TView.Origin
        LES     DI,Bounds
        CLD
        LODSW                           {Origin.X}
        MOV     CX,AX
        STOSW
        LODSW                           {Origin.Y}
        MOV     DX,AX
        STOSW
        LODSW                           {Size.X}
        ADD     AX,CX
        STOSW
        LODSW                           {Size.Y}
        ADD     AX,DX
        STOSW
        POP     DS
end;

function TView.Execute: Word;
begin
  Execute := cmCancel;
end;

function TView.Exposed: Boolean; assembler;
var
  Target: Pointer;
asm
        LES     DI,Self
        TEST    ES:[DI].TView.State,sfExposed
        JE      @@2
        XOR     AX,AX
        CMP     AX,ES:[DI].TView.Size.X
        JGE     @@2
        CMP     AX,ES:[DI].TView.Size.Y
        JGE     @@2
@@1:    XOR     BX,BX
        MOV     CX,ES:[DI].TView.Size.X
        PUSH    AX
        CALL    @@11
        POP     AX
        JNC     @@3
        LES     DI,Self
        INC     AX
        CMP     AX,ES:[DI].TView.Size.Y
        JL      @@1
@@2:    MOV     AL,0
        JMP     @@30
@@3:    MOV     AL,1
        JMP     @@30
@@8:    STC
@@9:    RETN
@@10:   LES     DI,ES:[DI].TView.Owner
@@11:   MOV     Target.Word[0],DI
        MOV     Target.Word[2],ES
        ADD     AX,ES:[DI].TView.Origin.Y
        MOV     SI,ES:[DI].TView.Origin.X
        ADD     BX,SI
        ADD     CX,SI
        LES     DI,ES:[DI].TView.Owner
        MOV     SI,ES
        OR      SI,DI
        JE      @@9
        CMP     AX,ES:[DI].TGroup.Clip.A.Y
        JL      @@8
        CMP     AX,ES:[DI].TGroup.Clip.B.Y
        JGE     @@8
        CMP     BX,ES:[DI].TGroup.Clip.A.X
        JGE     @@12
        MOV     BX,ES:[DI].TGroup.Clip.A.X
@@12:   CMP     CX,ES:[DI].TGroup.Clip.B.X
        JLE     @@13
        MOV     CX,ES:[DI].TGroup.Clip.B.X
@@13:   CMP     BX,CX
        JGE     @@8
        LES     DI,ES:[DI].TGroup.Last
@@20:   LES     DI,ES:[DI].TView.Next
        CMP     DI,Target.Word[0]
        JNE     @@21
        MOV     SI,ES
        CMP     SI,Target.Word[2]
        JE      @@10
@@21:   TEST    ES:[DI].TView.State,sfVisible
        JE      @@20
        MOV     SI,ES:[DI].TView.Origin.Y
        CMP     AX,SI
        JL      @@20
        ADD     SI,ES:[DI].TView.Size.Y
        CMP     AX,SI
        JGE     @@20
        MOV     SI,ES:[DI].TView.Origin.X
        CMP     BX,SI
        JL      @@22
        ADD     SI,ES:[DI].TView.Size.X
        CMP     BX,SI
        JGE     @@20
        MOV     BX,SI
        CMP     BX,CX
        JL      @@20
        STC
        RETN
@@22:   CMP     CX,SI
        JLE     @@20
        ADD     SI,ES:[DI].TView.Size.X
        CMP     CX,SI
        JG      @@23
        MOV     CX,ES:[DI].TView.Origin.X
        JMP     @@20
@@23:   PUSH    Target.Word[2]
        PUSH    Target.Word[0]
        PUSH    ES
        PUSH    DI
        PUSH    SI
        PUSH    CX
        PUSH    AX
        MOV     CX,ES:[DI].TView.Origin.X
        CALL    @@20
        POP     AX
        POP     CX
        POP     BX
        POP     DI
        POP     ES
        POP     Target.Word[0]
        POP     Target.Word[2]
        JC      @@20
        RETN
@@30:
end;

function TView.Focus: Boolean;
var
  Result: Boolean;
begin
  Result := True;
  if State and (sfSelected + sfModal) = 0 then
  begin
    if Owner <> nil then
    begin
      Result := Owner^.Focus;
      if Result then
        if ((Owner^.Current = nil) or
          (Owner^.Current^.Options and ofValidate = 0) or
          (Owner^.Current^.Valid(cmReleasedFocus))) then
          Select
        else
          Result := False;
    end;
  end;
  Focus := Result;
end;

procedure TView.GetClipRect(var Clip: TRect);
var
  R: TRect;
begin
  GetBounds(Clip);
  if Owner <> nil then Clip.Intersect(Owner^.Clip);
  Clip.Move(-Origin.X, -Origin.Y);  {e.i convert to local coords}
  MakeLocal(AllScreen.A, R.A)       {e.i. MakeLocal screen's A-corner, OOA};
  MakeLocal(AllScreen.B, R.B)       {e.i. MakeLocal screen's B-corner, OOA};
  Clip.Intersect(R);                                                   {OOA}
end;

function TView.GetColor(Color: Word): Word; assembler;
asm
        MOV     AX,Color
        CALL    MapCPair
end;

procedure TView.GetCommands(var Commands: TCommandSet);
begin
  Commands := CurCommandSet;
end;

procedure TView.GetData(var Rec);
begin
end;

procedure TView.GetEvent(var Event: TEvent);
begin
  if Owner <> nil then Owner^.GetEvent(Event);
end;

procedure TView.GetExtent(var Extent: TRect); assembler;
asm
        PUSH    DS
        LDS     SI,Self
        ADD     SI,OFFSET TView.Size
        LES     DI,Extent
        CLD
        XOR     AX,AX
        STOSW
        STOSW
        MOVSW
        MOVSW
        POP     DS
end;

function TView.GetHelpCtx: Word;
begin
  if State and sfDragging <> 0 then
    GetHelpCtx := hcDragging else
    GetHelpCtx := HelpCtx;
end;

function TView.GetPalette: PPalette;
begin
  GetPalette := nil;
end;

procedure TView.GetPeerViewPtr(var S: TStream; var P);
var
  Index: Integer;
begin
  S.Read(Index, SizeOf(Word));
  if (Index = 0) or (OwnerGroup = nil) then Pointer(P) := nil
  else
  begin
    Pointer(P) := FixupList^[Index];
    FixupList^[Index] := @P;
  end;
end;

function TView.GetState(AState: Word): Boolean;
begin
  GetState := State and AState = AState;
end;

procedure TView.GrowTo(X, Y: Integer);
var
  R: TRect;
begin
  R.Assign(Origin.X, Origin.Y, Origin.X + X, Origin.Y + Y);
  Locate(R);
end;

procedure TView.HandleEvent(var Event: TEvent);
begin
  if Event.What = evMouseDown then
    if (State and (sfSelected + sfDisabled) = 0) and
       (Options and ofSelectable <> 0) then
      if not Focus or (Options and ofFirstClick = 0) then
        ClearEvent(Event);
end;

procedure TView.Hide;
begin
  if State and sfVisible <> 0 then SetState(sfVisible, False);
end;

procedure TView.HideCursor;
begin
  SetState(sfCursorVis, False);
end;

procedure TView.KeyEvent(var Event: TEvent);
begin
  repeat GetEvent(Event) until Event.What = evKeyDown;
end;

procedure TView.Locate(var Bounds: TRect);
var
  R: TRect;
  Min, Max: TPoint;

function Range(Val, Min, Max: Integer): Integer;
begin
  if Val < Min then Range := Min else
    if Val > Max then Range := Max else
      Range := Val;
end;

begin
  SizeLimits(Min, Max);
  Bounds.B.X := Bounds.A.X + Range(Bounds.B.X - Bounds.A.X, Min.X, Max.X);
  Bounds.B.Y := Bounds.A.Y + Range(Bounds.B.Y - Bounds.A.Y, Min.Y, Max.Y);
  GetBounds(R);
(*  if not Bounds.Equals(R) then {GIO - ÆË®°™† Ø‡® §¢®¶•≠®®}
  begin*)
  ChangeBounds(Bounds);
  if (Owner <> nil) and (State and sfVisible <> 0) then
    DrawUnderRect(R, nil);
(*  end;*)
end;

procedure TView.MakeFirst;
begin
  PutInFrontOf(Owner^.First);
end;

procedure TView.MakeGlobal(Source: TPoint; var Dest: TPoint); assembler;
asm
        LES     DI,Self
        XOR     AX,AX
        MOV     DX,AX
@@1:    ADD     AX,ES:[DI].TView.Origin.X
        ADD     DX,ES:[DI].TView.Origin.Y
        LES     DI,ES:[DI].TView.Owner
        MOV     SI,ES
        OR      SI,DI
        JNE     @@1
        ADD     AX,Source.X
        ADD     DX,Source.Y
        LES     DI,Dest
        CLD
        STOSW
        XCHG    AX,DX
        STOSW
end;

procedure TView.MakeLocal(Source: TPoint; var Dest: TPoint); assembler;
asm
        LES     DI,Self
        XOR     AX,AX
        MOV     DX,AX
@@1:    ADD     AX,ES:[DI].TView.Origin.X
        ADD     DX,ES:[DI].TView.Origin.Y
        LES     DI,ES:[DI].TView.Owner
        MOV     SI,ES
        OR      SI,DI
        JNE     @@1
        NEG     AX
        NEG     DX
        ADD     AX,Source.X
        ADD     DX,Source.Y
        LES     DI,Dest
        CLD
        STOSW
        XCHG    AX,DX
        STOSW
end;

function TView.MouseEvent(var Event: TEvent; Mask: Word): Boolean;
begin
  repeat GetEvent(Event) until Event.What and (Mask or evMouseUp) <> 0;
  MouseEvent := Event.What <> evMouseUp;
end;

function TView.MouseInView(Mouse: TPoint): Boolean;
var
  Extent: TRect;
begin
  MakeLocal(Mouse, Mouse);
  GetExtent(Extent);
  MouseInView := Extent.Contains(Mouse);
end;

procedure TView.MoveTo(X, Y: Integer);
var
  R: TRect;
begin
  R.Assign(X, Y, X + Size.X, Y + Size.Y);
  Locate(R);
end;

function TView.NextView: PView;
begin
  if @Self = Owner^.Last then NextView := nil else NextView := Next;
end;

procedure TView.NormalCursor;
begin
  SetState(sfCursorIns, False);
end;

function TView.Prev: PView; assembler;
asm
        LES     DI,Self
        MOV     CX,DI
        MOV     BX,ES
@@1:    MOV     AX,DI
        MOV     DX,ES
        LES     DI,ES:[DI].TView.Next
        CMP     DI,CX
        JNE     @@1
        MOV     SI,ES
        CMP     SI,BX
        JNE     @@1
end;

function TView.PrevView: PView;
begin
  if @Self = Owner^.First then PrevView := nil else PrevView := Prev;
end;

procedure TView.PutEvent(var Event: TEvent);
begin
  if Owner <> nil then Owner^.PutEvent(Event);
end;

procedure TView.PutInFrontOf(Target: PView);
var
  P, LastView: PView;

procedure MoveView;
begin
  Owner^.RemoveView(@Self);
  Owner^.InsertView(@Self, Target);
end;

begin
  if (Owner <> nil) and (Target <> @Self) and (Target <> NextView) and
    ((Target = nil) or (Target^.Owner = Owner)) then
    if State and sfVisible = 0 then MoveView else
    begin
      LastView := NextView;
      if LastView <> nil then
      begin
        P := Target;
        while (P <> nil) and (P <> LastView) do P := P^.NextView;
        if P = nil then LastView := Target;
      end;
      Owner^.Lock; {OOA}
      State := State and not sfVisible;
      if LastView = Target then DrawHide(LastView);
      MoveView;
      State := State or sfVisible;
      if LastView <> Target then DrawShow(LastView);
      if Options and ofSelectable <> 0 then
      begin
        Owner^.ResetCurrent;
        Owner^.ResetCursor;
      end;
      Owner^.Unlock; {OOA}
    end;
end;

procedure TView.PutPeerViewPtr(var S: TStream; P: PView);
var
  Index: Integer;
begin
  if (P = nil) or (OwnerGroup = nil) then Index := 0
  else Index := OwnerGroup^.IndexOf(P);
  S.Write(Index, SizeOf(Word));
end;

procedure TView.ResetCursor {TONY};
const
  CV = sfVisible+sfCursorVis+sfFocused;
var
  P: TPoint;
  MouseRegion: TRect;
  CursorRegion: TRect;
  MustResetMouse: boolean;
begin
  {HoldMouse(True);}
  MouseRegion.Assign(MouseWhere.X-CurrentMouseCursor^.XHotSpot,
                     MouseWhere.Y-CurrentMouseCursor^.YHotSpot,
                     MouseWhere.X-CurrentMouseCursor^.XHotSpot+MouseCursorSize.X-1,
                     MouseWhere.Y-CurrentMouseCursor^.YHotSpot+MouseCursorSize.Y-1);
  TextCursor.GetCursorRect(CursorRegion);
  CursorRegion.Intersect(MouseRegion);
  MustResetMouse := not CursorRegion.Empty;
  TextCursor.Shape((State and sfCursorIns) = 0);{DK}

  if (State and CV) <> CV then begin
    if MustResetMouse then HideMouse;
    TextCursor.Hide;
    if MustResetMouse then ShowMouse;
  end else begin
    TextCursor.SetHeight(FontHeight);
    TextCursor.SetWidth(CharWidth);
    P.X := Cursor.X;
    P.Y := Cursor.Y;
    if (P.X<0) or (P.Y<0) or
       (P.X+CharWidth>Size.X) or (P.Y+FontHeight>Size.Y) then begin
      if MustResetMouse then HideMouse;
      TextCursor.Hide;
      if MustResetMouse then ShowMouse;
    end else begin
      MakeGlobal(P, P);
      TextCursor.GetCursorRect(CursorRegion);
     { CursorRegion.A.X := P.X - (CursorRegion.B.X - CursorRegion.A.X);
      CursorRegion.A.Y := P.Y - (CursorRegion.B.Y - CursorRegion.A.Y);
      CursorRegion.B.X := P.X;
      CursorRegion.B.Y := P.Y; <- bug in TEditor}
      CursorRegion.Assign(P.X, P.Y,
       Max(P.X + CharWidth,  P.X + (CursorRegion.B.X - CursorRegion.A.X)),
       Max(P.Y + FontHeight, P.Y + (CursorRegion.B.Y - CursorRegion.A.Y))); {DK}

      CursorRegion.Intersect(MouseRegion);
      MustResetMouse := not CursorRegion.Empty;
      if MustResetMouse then HideMouse;
      TextCursor.GotoXY(P.X, P.Y);
      TextCursor.Show;
      if MustResetMouse then ShowMouse;
    end;
  end;
  {HoldMouse(False);}
end;

procedure TView.PaletteEnable(AEnable : Boolean); {DK}
Begin
  if AEnable then
    LogPalette.Mode := LogPalette.Mode or pmEnable or pmDrawFirst
  else
    LogPalette.Mode := LogPalette.Mode and not(pmEnable);
End;

procedure TView.Select;
begin
  if Options and ofSelectable <> 0 then
    if Options and ofTopSelect <> 0 then MakeFirst else
      if Owner <> nil then Owner^.SetCurrent(@Self, NormalSelect);
end;

procedure TView.SetBounds(var Bounds: TRect); assembler;
asm
        PUSH    DS
        LES     DI,Self
        LDS     SI,Bounds
        MOV     AX,[SI].TRect.A.X
        MOV     ES:[DI].Origin.X,AX
        MOV     AX,[SI].TRect.A.Y
        MOV     ES:[DI].Origin.Y,AX
        MOV     AX,[SI].TRect.B.X
        SUB     AX,[SI].TRect.A.X
        MOV     ES:[DI].Size.X,AX
        MOV     AX,[SI].TRect.B.Y
        SUB     AX,[SI].TRect.A.Y
        MOV     ES:[DI].Size.Y,AX
        POP     DS
end;

procedure TView.SetCmdState(Commands: TCommandSet; Enable: Boolean);
begin
  if Enable then EnableCommands(Commands)
  else DisableCommands(Commands);
end;

procedure TView.SetCommands(Commands: TCommandSet);
begin
  CommandSetChanged := CommandSetChanged or (CurCommandSet <> Commands);
  CurCommandSet := Commands;
end;

procedure TView.SetCursor(X, Y: Integer);
begin
  Cursor.X := X;
  Cursor.Y := Y;
{if DrawLockCounter = 0 then OOA : else bad unlocked filelistbox cursors}
  DrawCursor;
end;

procedure TView.SetData(var Rec);
begin
end;

procedure TView.SetState(AState: Word; Enable: Boolean);
var
  Command: Word;

begin
  if Enable then
    State := State or AState else
    State := State and not AState;
  if Owner <> nil then
    case AState of
      sfVisible:
        begin
          if Owner^.State and sfExposed <> 0 then
            SetState(sfExposed, Enable);
          if Enable then DrawShow(nil) else DrawHide(nil);
          if Options and ofSelectable <> 0 then Owner^.ResetCurrent;
        end;
      sfCursorVis, sfCursorIns:
        DrawCursor;
      sfShadow:
        {DrawUnderView(True, nil) OOA};
      sfFocused:
        begin
          ResetCursor;
          if Enable then
            Command := cmReceivedFocus else
            Command := cmReleasedFocus;
          if Enable then begin
            if (LogPalette.Mode and pmKeepFocus) <> 0 then begin
              LogPalette.Mode := LogPalette.Mode or pmMonopoly;
              RealizePalette;
            end
          end else begin
            if (LogPalette.Mode and pmKeepFocus) <> 0 then
              LogPalette.Mode := LogPalette.Mode and not pmMonopoly;
          end;
          Message(Owner, evBroadcast, Command, @Self);
        end;
    end;
end;

procedure TView.Show;
begin
  if State and sfVisible = 0 then SetState(sfVisible, True);
end;

procedure TView.ShowCursor;
begin
  SetState(sfCursorVis, True);
end;

procedure TView.SizeLimits(var Min, Max: TPoint);
begin
  Longint(Min) := 0;
  if Owner <> nil then
    Max := Owner^.Size else
    Longint(Max) := $7FFF7FFF;
end;

procedure TView.Store(var S: TStream);
var
  SaveState: Word;
begin
  SaveState := State;
  State := State and not (sfActive + sfSelected + sfFocused + sfExposed);
  S.Write(Origin,
    SizeOf(TPoint) * 3 +
    SizeOf(Byte) * 2 +
    SizeOf(Word) * 4
    {DK-=SizeOf(Integer) {Font. OOA}
   );
  S.Write(Font.Font, 1);
  State := SaveState;
end;

function TView.TopView: PView;
var
  P: PView;
begin
  if TheTopView = nil then
  begin
    P := @Self;
    while (P <> nil) and (P^.State and sfModal = 0) do P := P^.Owner;
    TopView := P;
  end
  else TopView := TheTopView;
end;

function TView.Valid(Command: Word): Boolean;
begin
  Valid := True;
end;


{ TFrame }

constructor TFrame.Init(var Bounds: TRect);
begin
  inherited Init(Bounds); {TONY}
  Font := TitleFont; {TONY}
  GrowMode := gfGrowHiX + gfGrowHiY;
  EventMask := EventMask or evBroadcast;
  UpBarHeight := FontHeight + 2 {SHIM};
  FrameWidth := 5;
end;

constructor TFrame.Load(var S: TStream); {OOA}
begin
  TView.Load(S);
  S.Read(FrameMode, SizeOf(Integer) + SizeOf(Word) * 2);
end;

procedure TFrame.Draw {SHIM, TONY, OOA};
var
  CFrame, CTitle, CShadow, CBack: Word;
  Title: TTitleStr;
  I, J: Integer;
var
  Img: PImage;
  OfsOfIcon: integer;
  Max, Min: TPoint;
  DeltaZoomIcon: integer;
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

  if {(State and sfActive = sfActive) and} (FrameWidth > 2) and
    ((Owner = Nil) or (PWindow(Owner)^.Flags and wfGrow <> 0)) {OOA} then
  begin
    I:= FrameWidth + UpBarHeight;
    HLine(0, I - 1, FrameWidth, Lo(CShadow));
    HLine(0, I, FrameWidth, Hi(CShadow));
    HLine(Size.X - FrameWidth - 1, I - 1, Size.X - 1, Lo(CShadow));
    HLine(Size.X - FrameWidth - 1, I, Size.X - 1, Hi(CShadow));

    HLine(0, Size.Y - I - 1, FrameWidth, Lo(CShadow));
    HLine(0, Size.Y - I, FrameWidth, Hi(CShadow));
    HLine(Size.X - FrameWidth - 1, Size.Y - I - 1, Size.X - 1, Lo(CShadow));
    HLine(Size.X - FrameWidth - 1, Size.Y - I, Size.X - 1, Hi(CShadow));

    VLine(I, 0, FrameWidth, Lo(CShadow));
    VLine(I + 1, 0, FrameWidth, Hi(CShadow));
    VLine(Size.X - I - 1, 0, FrameWidth, Lo(CShadow));
    VLine(Size.X - I, 0, FrameWidth, Hi(CShadow));

    VLine(I, Size.Y - FrameWidth, Size.Y, Lo(CShadow));
    VLine(I + 1, Size.Y - FrameWidth, Size.Y, Hi(CShadow));
    VLine(Size.X - I - 1, Size.Y - FrameWidth, Size.Y, Lo(CShadow));
    VLine(Size.X - I, Size.Y - FrameWidth, Size.Y, Hi(CShadow));
  end;

  if UpBarHeight <> 0 then begin {TONY}
    Bar{Pattern}(FrameWidth - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight {- 1 OOA}, Lo(CTitle));
    (*if State and sfActive <> sfActive then
      Rectangle(FrameWidth - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight {- 1 OOA},
        1, Lo(CShadow), Hi(CShadow))
    else*) begin
      Rectangle(FrameWidth - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight {- 1 OOA},
        1, Hi(CShadow), Lo(CShadow));

      if PWindow(Owner)^.Flags and wfClose <> 0 then  {Close button. SHIM}
      begin

        VLine(UpBarHeight + FrameWidth, FrameWidth, UpBarHeight + FrameWidth, Lo(CShadow));
        VLine(UpBarHeight + FrameWidth + 1, FrameWidth, UpBarHeight + FrameWidth, Hi(CShadow));
        if FrameMode = fmCloseClicked then begin
          Rectangle(FrameWidth - 2, FrameWidth - 2, UpBarHeight + FrameWidth + 2, UpBarHeight + FrameWidth,
            1, Lo(CShadow), Hi(CShadow));
          VLine(UpBarHeight + FrameWidth, FrameWidth, UpBarHeight + FrameWidth - 1, Lo(CTitle));
          OfsOfIcon := 0;
        end else
          OfsOfIcon := -1;
        Img:= GetImage( 12 );
        PutBmpOp( Img,
                  FrameWidth + ( UpBarHeight - BitMapWidth( Img ) ) div 2 + OfsOfIcon,
                  FrameWidth + ( UpBarHeight - BitMapHeight( Img ) ) div 2 + OfsOfIcon,
                  AndPut );
      end;

      DeltaZoomIcon := 0;

      if PWindow(Owner)^.Flags and wfZoom <> 0 then {Zoom button. SHIM}
      begin
        DeltaZoomIcon := UpBarHeight + FrameWidth - 1;

        VLine(Size.X - UpBarHeight - FrameWidth - 1, FrameWidth, UpBarHeight + FrameWidth, Lo(CShadow));
        VLine(Size.X - UpBarHeight - FrameWidth, FrameWidth, UpBarHeight + FrameWidth, Hi(CShadow));
        if FrameMode = fmZoomClicked then begin
{          Bar(Size.X - UpBarHeight - FrameWidth, FrameWidth,
            Size.X - FrameWidth, UpBarHeight + FrameWidth, Lo(CShadow));}
          Rectangle(Size.X - UpBarHeight - FrameWidth - 1, FrameWidth - 2,
            Size.X - FrameWidth + 2, UpBarHeight + FrameWidth,
            1, Lo(CShadow), Hi(CShadow));
          VLine(Size.X - UpBarHeight - FrameWidth, FrameWidth, UpBarHeight + FrameWidth - 1, Lo(CTitle));
          OfsOfIcon := 0;
        end else
          OfsOfIcon := -1;
        Owner^.SizeLimits(Min, Max);
        if Longint(Owner^.Size) = Longint(Max) then
          Img:= GetImage( 13 )
        else
          Img:= GetImage( 14 );
        PutBmpOp( Img,
                  Size.X - FrameWidth - (UpBarHeight + BitMapWidth(Img)) div 2 + 2 + OfsOfIcon,
                  FrameWidth + ( UpBarHeight - BitMapHeight( Img ) ) div 2 + OfsOfIcon,
                  AndPut );
      end;

      if PWindow(Owner)^.Options and ofIconizable <> 0 then
      {Iconize button. SHIM, TONY}
      begin
        VLine(Size.X - UpBarHeight - FrameWidth - 1 - DeltaZoomIcon, FrameWidth, UpBarHeight + FrameWidth, Lo(CShadow));
        VLine(Size.X - UpBarHeight - FrameWidth - DeltaZoomIcon, FrameWidth, UpBarHeight + FrameWidth, Hi(CShadow));
        if FrameMode = fmZoomClicked then begin
{          Bar(Size.X - UpBarHeight - FrameWidth, FrameWidth,
            Size.X - FrameWidth, UpBarHeight + FrameWidth, Lo(CShadow));}
          Rectangle(Size.X - UpBarHeight - FrameWidth - 1 - DeltaZoomIcon, FrameWidth - 2,
            Size.X - FrameWidth + 2 - DeltaZoomIcon, UpBarHeight + FrameWidth,
            1, Lo(CShadow), Hi(CShadow));
          VLine(Size.X - UpBarHeight - FrameWidth - DeltaZoomIcon, FrameWidth, UpBarHeight + FrameWidth - 1, Lo(CTitle));
          OfsOfIcon := 0;
        end else
          OfsOfIcon := -1;
        Owner^.SizeLimits(Min, Max);
        Img:= GetImage( 15 );
        PutBmpOp( Img,
                  Size.X - FrameWidth - (UpBarHeight + BitMapWidth(Img)) div 2 + 2 + OfsOfIcon - DeltaZoomIcon,
                  FrameWidth + ( UpBarHeight - BitMapHeight( Img ) ) div 2 + OfsOfIcon,
                  AndPut );
      end;

    end;
  end;

  if Owner <> nil then
    Title := PWindow(Owner)^.GetTitle(Size.X div CharWidth)
  else
    Title := '';

  if Title <> '' then
  begin
    I:= Size.X - FrameWidth shl 1;
    if PWindow(Owner)^.Flags and wfClose <> 0 then Dec(I, UpBarHeight);
    if PWindow(Owner)^.Flags and wfZoom <> 0 then Dec(I, UpBarHeight);
    if PWindow(Owner)^.Options and ofIconizable <> 0 then Dec(I, UpBarHeight);{DK}

    while (FontWidth(Title) > I) and (Title[0] > #0) do Dec(Byte(Title[0])) {OOA};
    J:= FrameWidth;
    if PWindow(Owner)^.Flags and wfClose <> 0 then Inc(J, UpBarHeight);
    WrStr(J + (I - FontWidth(Title)) div 2, FrameWidth, Title, Hi(CTitle));
  end;
end;

function TFrame.GetPalette: PPalette;
const
  P: String[Length(CFrame)] = CFrame;
begin
  GetPalette := @P;
end;

procedure TFrame.MouseMastering(const Event: TEvent); {TONY, OOA}
label
  ExitPoint;
begin
  if (PWindow(Owner)^.Flags and wfGrow <> 0) and
     (State and sfActive <> 0) and
     (Owner^.State and sfFocused <> 0) {TONY}
  then begin
      case GetDragGrowMode(Event) of
        dsUpperLeft: SetMouseCursorShape(@Diag1MouseCursor);
        dsLowerLeft: SetMouseCursorShape(@Diag2MouseCursor);
        dsUpperRight: SetMouseCursorShape(@Diag2MouseCursor);
        dsLowerRight: SetMouseCursorShape(@Diag1MouseCursor);
        dsUpper, dsLower: SetMouseCursorShape(@VerticalMouseCursor);
        dsRight, dsLeft: SetMouseCursorShape(@HorizontalMouseCursor);
      else
        goto ExitPoint;
      end;
      Exit;
  end;
ExitPoint:
  SetMouseCursorShape(@DefaultMouseCursor);
end;

procedure TFrame.HandleEvent(var Event: TEvent);
var
  Mouse: TPoint;

procedure DragWindow(Mode: Word);
var
  Limits: TRect;
  Min, Max: TPoint;
begin
  Owner^.Owner^.GetExtent(Limits);
  Owner^.SizeLimits(Min, Max);
  Owner^.DragView(Event, Owner^.DragMode or Mode, Limits, Min, Max);
  ClearEvent(Event);
end;

function CanResize: Boolean; {OOA}
var
  R: TRect;
begin
  GetExtent(R);
  R.Grow(-FrameWidth, -FrameWidth);
  CanResize:= not R.Contains(Mouse);
end;

var
  R: TRect;
  DeltaZoomIcon: integer;
begin
  TView.HandleEvent(Event);
  if (Event.What = evMouseDown){ and (Event.Buttons and mbLeftButton = mbLeftButton)} then
  begin
    MakeLocal(Event.Where, Mouse);
    if (Mouse.Y >= FrameWidth) and (Mouse.Y < (UpBarHeight + FrameWidth)) {SHIM} and
      (Mouse.X >= FrameWidth) and (Mouse.X < (Size.X - FrameWidth)) {OOA} then
    begin
      R.Assign(FrameWidth - 2, FrameWidth - 2, FrameWidth + UpBarHeight + 2, FrameWidth + UpBarHeight);
      if (PWindow(Owner)^.Flags and wfClose <> 0) and
        (State and sfActive <> 0) and R.Contains(Mouse) then {OOA}
      begin
        repeat
          MakeLocal(Event.Where, Mouse);
          if R.Contains(Mouse) then
            FrameMode := fmCloseClicked
          else FrameMode := 0;
          InvalidateRect(R);
          DrawInvalidated;
        until not MouseEvent(Event, evMouseMove + evMouseAuto);
        FrameMode := 0;
        if R.Contains(Mouse) then
        begin
          Event.What := evCommand;
          Event.Command := cmClose;
          Event.InfoPtr := Owner;
          PutEvent(Event);
        end;
        ClearEvent(Event);
        InvalidateRect(R);
        DrawInvalidated;
      end else
      { Iconize button handle }
      begin
        if (PWindow(Owner)^.Flags and wfZoom <> 0) and (State and sfActive <> 0) then
          DeltaZoomIcon := UpBarHeight + FrameWidth - 1
        else
          DeltaZoomIcon := 0;
        R.Assign(Size.X - FrameWidth - UpBarHeight - 2 - DeltaZoomIcon, FrameWidth - 2,
          Size.X - FrameWidth + 2 - DeltaZoomIcon, FrameWidth + UpBarHeight);
        if (PWindow(Owner)^.Options and ofIconizable <> 0) and
          (State and sfActive <> 0) and R.Contains(Mouse) then
        begin
          repeat
            MakeLocal(Event.Where, Mouse);
            if R.Contains(Mouse) then
              FrameMode := fmIconizeClicked
            else FrameMode := 0;
            InvalidateRect(R);
            DrawInvalidated;
          until not MouseEvent(Event, evMouseMove + evMouseAuto);
          FrameMode := 0;
          ClearEvent(Event);
          InvalidateRect(R);
          DrawInvalidated;
          if R.Contains(Mouse) then
            PWindow( Owner )^.Iconize;
        end else
        begin
          R.Assign(Size.X - FrameWidth - UpBarHeight - 2, FrameWidth - 2, Size.X - FrameWidth + 2, FrameWidth + UpBarHeight);
          if (PWindow(Owner)^.Flags and wfZoom <> 0) and
            (State and sfActive <> 0) and (Event.Double or R.Contains(Mouse)) then
          begin
            if not Event.Double then
              repeat
                MakeLocal(Event.Where, Mouse);
                if R.Contains(Mouse) then
                  FrameMode := fmZoomClicked
                else FrameMode := 0;
                InvalidateRect(R);
                DrawInvalidated;
              until not MouseEvent(Event, evMouseMove + evMouseAuto);
            FrameMode := 0;
            if R.Contains(Mouse) or Event.Double then
            begin
              Event.What := evCommand;
              Event.Command := cmZoom;
              Event.InfoPtr := Owner;
              PutEvent(Event);
            end;
            ClearEvent(Event);
            InvalidateRect(R);
            DrawInvalidated;
          end else
            if PWindow(Owner)^.Flags and wfMove <> 0 then
              DragWindow(dmDragMove);
        end
      end;
    end else
      if (State and sfActive <> 0) and CanResize then
        if (PWindow(Owner)^.Flags and wfGrow <> 0) then
          DragWindow(dmDragGrow + GetDragGrowMode(Event));
  end;
end;

procedure TFrame.SetState(AState: Word; Enable: Boolean);
begin
  TView.SetState(AState, Enable);
  if AState and (sfActive + sfDragging) <> 0 then DrawView;
end;

procedure TFrame.Store(var S: TStream); {OOA}
begin
  TView.Store(S);
  S.Write(FrameMode, SizeOf(Integer) + SizeOf(Word) * 2);
end;

procedure TFrame.GetClientExtent(var Extent: TRect); {OOA}
begin
  GetExtent(Extent);
  Extent.Grow(-FrameWidth-1, -FrameWidth-1);
  Inc(Extent.A.Y, UpBarHeight);
end;

function TFrame.GetFrameWidth: Integer; {OOA}
begin
  GetFrameWidth:= FrameWidth;
end;

function TFrame.GetCaptureHeight: Integer; {OOA}
begin
  GetCaptureHeight:= UpBarHeight;
end;

function TFrame.GetDragGrowMode(const Event: TEvent): Word; {OOA}
var
  Mouse: TPoint;
  Frame, Angle: Integer;
begin
  MakeLocal(Event.Where, Mouse);
  Frame:= GetFrameWidth;
  Angle:= Frame + GetCaptureHeight;
  if (Mouse.X >= Frame) and (Mouse.X < (Size.X - Frame)) and
    (Mouse.Y >= Frame) and (Mouse.Y < (Size.Y - Frame)) then GetDragGrowMode:= dsDrag
  else if (Mouse.X < Angle) and (Mouse.Y < Angle) then GetDragGrowMode:= dsUpperLeft
  else if (Mouse.X < Angle) and (Mouse.Y >= Size.Y - Angle) then GetDragGrowMode:= dsLowerLeft
  else if (Mouse.X >= Size.X - Angle) and (Mouse.Y < Angle) then GetDragGrowMode:= dsUpperRight
  else if (Mouse.X >= Size.X - Angle) and (Mouse.Y >= Size.Y - Angle) then GetDragGrowMode:= dsLowerRight
  else if (Mouse.X < Frame) then GetDragGrowMode:= dsLeft
  else if (Mouse.X >= Size.X - Frame) then GetDragGrowMode:= dsRight
  else if (Mouse.Y < Frame) then GetDragGrowMode:= dsUpper
  else if (Mouse.Y >= Size.Y - Frame) then GetDragGrowMode:= dsLower
  else GetDragGrowMode:= dsDrag;
end;


{ TScrollBar }

constructor TScrollBar.Init(var Bounds: TRect); {SHIM}
const
  VChars: TScrollChars = (3, 1, 9, 4, 2);
  HChars: TScrollChars = (5, 7, 9, 6, 8);
begin
  TView.Init(Bounds);
  Value := 0;
  Min := 0;
  Max := 0;
  PgStep := 1;
  ArStep := 1;
  IsVertical := Size.X < Size.Y;
  if IsVertical then
  begin
    GrowMode := gfGrowLoX + gfGrowHiX + gfGrowHiY;
    Chars := VChars;
    Size.X := BitMapWidth(GetImage(Chars[0])) + 2;
  end else
  begin
    GrowMode := gfGrowLoY + gfGrowHiX + gfGrowHiY;
    Chars := HChars;
    Size.Y := BitMapWidth(GetImage(Chars[0])) + 2;
  end;
  DrawPartCode := -1;
end;

constructor TScrollBar.Load(var S: TStream);
begin
  TView.Load(S);
  S.Read(Value, SizeOf(Integer) * 5 +
    SizeOf(Boolean) +    {IsVertical. OOA}
    SizeOf(Integer) +    {DrawPartCode. OOA}
    SizeOf(TScrollChars)
  );
end;

procedure TScrollBar.DrawPos(Pos: Integer);
begin
  _Pos:= Pos;
  Draw_Pos:= True;
  DrawView;
  Draw_Pos:= False;
end;

procedure TScrollBar.Draw;
var
  S, P : integer;
  CFrame, CShadow : word;
begin
  if not Draw_Pos then _Pos:= GetPos;
  S := GetSize;
  CFrame := GetColor($01);
  CShadow := GetColor($0203);
  FrameBar(0, 0, Size.X, Size.Y, Hi(CShadow), Lo(CShadow), CFrame);
  if (DrawPartCode = sbLeftArrow) or (DrawPartCode = sbUpArrow) then
    PutBmp(GetImage(Chars[3]), 1, 1)
  else
    PutBmp(GetImage(Chars[0]), 1, 1);
  if (DrawPartCode = sbRightArrow) or (DrawPartCode = sbDownArrow) then
    PutBmp(GetImage(Chars[4]),
      Size.X - BitMapWidth(GetImage(Chars[4])) - 1,
      Size.Y - BitMapHeight(GetImage(Chars[4])) - 1)
  else
    PutBmp(GetImage(Chars[1]),
      Size.X - BitMapWidth(GetImage(Chars[1])) - 1,
      Size.Y - BitMapHeight(GetImage(Chars[1])) - 1);
  if (Max <> Min) and (S <> 0) then
  begin
    if IsVertical then begin
      PutBmp(GetImage(Chars[2]), 1,
        _Pos + BitMapHeight(GetImage(Chars[0])));
    end else begin
      PutBmp(GetImage(Chars[2]),
        _Pos + BitMapWidth(GetImage(Chars[0])), 1);
    end;
  end;
end;

function TScrollBar.GetPalette: PPalette;
const
  P: String[Length(CScrollBar)] = CScrollBar;
begin
  GetPalette := @P;
end;

function TScrollBar.GetPos: Integer;
var
  R: Integer;
begin
  R := Max - Min;
  if R = 0 then
    GetPos := 1 else
    GetPos := LongDiv(LongMul(Value - Min, GetSize {- 3 SHIM}) + R shr 1, R) + 1;
end;

function TScrollBar.GetSize: Integer;
var
  S, Smin: Integer;
begin
  if IsVertical then begin
    S := Size.Y - 2 -
      BitMapHeight(GetImage(Chars[0])) - BitMapHeight(GetImage(Chars[1]));
    SMin := BitMapHeight(GetImage(Chars[2]));
  end else begin
    S := Size.X - 2 - BitMapWidth(GetImage(Chars[0])) - BitMapWidth(GetImage(Chars[1]));
    SMin := BitMapWidth(GetImage(Chars[2]));
  end;
  if S < SMin then GetSize := 0 else GetSize := S - SMin;
end;

procedure TScrollBar.HandleEvent(var Event: TEvent); {SHIM}
var
  Tracking: Boolean;
  I, P, S, ClickPart: Integer;
  Mouse: TPoint;
  Extent: TRect;

function GetPartCode: Integer; {SHIM}
var
  Part: Integer;
  BMSize: Integer;
begin
  Part := -1;
  if Extent.Contains(Mouse) then
  begin
    Part := sbIndicator;
    if IsVertical then begin
      BMSize:= BitMapHeight(GetImage(Chars[0]));
      if Mouse.Y < BMSize then Part := sbUpArrow else
      if Mouse.Y > Size.Y - BitMapHeight(GetImage(Chars[1])) then Part := sbDownArrow else
      if Mouse.Y < P + BMSize then Part := sbPageUp else
      if Mouse.Y > P + BMSize + BitMapHeight(GetImage(Chars[2])) then Part := sbPageDown;
    end else begin
      BMSize:= BitMapWidth(GetImage(Chars[0]));
      if Mouse.X < BMSize then Part := sbLeftArrow else
      if Mouse.X > Size.X - BitMapWidth(GetImage(Chars[1])) then Part := sbRightArrow else
      if Mouse.X < P + BMSize then Part := sbPageLeft else
      if Mouse.X > P + BMSize + BitMapWidth(GetImage(Chars[2])) then Part := sbPageRight;
    end;
  end;
  DrawPartCode := Part;
  GetPartCode := Part;
end;

procedure Clicked;
begin
  Message(Owner, evBroadcast, cmScrollBarClicked, @Self);
end;

var
  Start, D, BMSize : integer;

  procedure ReleaseIcon; begin DrawPartCode := -1; DrawPos(GetPos); end;

begin
  TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      if (Event.Buttons and mbLeftButton = mbLeftButton) then begin
        Clicked;
        MakeLocal(Event.Where, Mouse);
        GetExtent(Extent);
        Extent.Grow(1, 1);
        P := GetPos;
        S := GetSize;
        ClickPart := GetPartCode;
        if ClickPart <> sbIndicator then
        begin
          repeat
            MakeLocal(Event.Where, Mouse);
            if GetPartCode = ClickPart then
              SetValue(Value + ScrollStep(ClickPart))
            else
              ReleaseIcon;
          until not MouseEvent(Event, evMouseAuto);
          ReleaseIcon;
        end else
        begin
          MakeLocal(Event.Where, Mouse);
          if IsVertical then
          begin
            BMSize:= BitMapHeight(GetImage(Chars[0]));
            Start := Mouse.Y - BMSize
          end else
          begin
            BMSize:= BitMapWidth(GetImage(Chars[0]));
            Start := Mouse.X - BMSize;
          end;
          D := Start - P;
          repeat
            MakeLocal(Event.Where, Mouse);
            Tracking := Extent.Contains(Mouse);
            if Tracking then
            begin
              if IsVertical then I := Mouse.Y - BMSize - D
                            else I := Mouse.X - BMSize - D;
              if I <= 0 then I := 1;
              if I >= S + 1 then I := S + 1;
            end else I := GetPos;
            if I <> Start then
            begin
              DrawPos(I);
              P := I;
            end;
          until not MouseEvent(Event, evMouseMove);
          if Tracking and (S > 2) then
          begin
            Dec(S, 2);
            SetValue(LongDiv(LongMul(P - 1, Max - Min) + S shr 1, S) + Min);
          end;
        end;
        ClearEvent(Event);
        DrawPartCode := -1;
      end;
    evKeyDown:
      if State and sfVisible <> 0 then
      begin
        ClickPart := sbIndicator;
        if not IsVertical then
          case CtrlToArrow(Event.KeyCode) of
            kbLeft: ClickPart := sbLeftArrow;
            kbRight: ClickPart := sbRightArrow;
            kbCtrlLeft: ClickPart := sbPageLeft;
            kbCtrlRight: ClickPart := sbPageRight;
            kbHome: I := Min;
            kbEnd: I := Max;
          else
            Exit;
          end
        else
          case CtrlToArrow(Event.KeyCode) of
            kbUp: ClickPart := sbUpArrow;
            kbDown: ClickPart := sbDownArrow;
            kbPgUp: ClickPart := sbPageUp;
            kbPgDn: ClickPart := sbPageDown;
            kbCtrlPgUp: I := Min;
            kbCtrlPgDn: I := Max;
          else
            Exit;
          end;
        Clicked;
        if ClickPart <> sbIndicator then I := Value + ScrollStep(ClickPart);
        SetValue(I);
        ClearEvent(Event);
      end;
  end;
end;

procedure TScrollBar.ScrollDraw;
begin
  Message(Owner, evBroadcast, cmScrollBarChanged, @Self);
end;

function TScrollBar.ScrollStep(Part: Integer): Integer;
var
  Step: Integer;
begin
  if Part and 2 = 0 then Step := ArStep else Step := PgStep;
  if Part and 1 = 0 then ScrollStep := -Step else ScrollStep := Step;
end;

procedure TScrollBar.SetParams(AValue, AMin, AMax, APgStep,
  AArStep: Integer);
var
  SValue: Integer;
begin
  if AMax < AMin then AMax := AMin;
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
  SValue := Value;
  if (SValue <> AValue) or (Min <> AMin) or (Max <> AMax) then
  begin
    Value := AValue;
    Min := AMin;
    Max := AMax;
    DrawView;
    if SValue <> AValue then ScrollDraw;
  end;
  PgStep := APgStep;
  ArStep := AArStep;
end;

procedure TScrollBar.SetRange(AMin, AMax: Integer);
begin
  SetParams(Value, AMin, AMax, PgStep, ArStep);
end;

procedure TScrollBar.SetStep(APgStep, AArStep: Integer);
begin
  SetParams(Value, Min, Max, APgStep, AArStep);
end;

procedure TScrollBar.SetValue(AValue: Integer);
begin
  SetParams(AValue, Min, Max, PgStep, ArStep);
end;

procedure TScrollBar.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(Value, SizeOf(Integer) * 5 +
    SizeOf(Boolean) +    {IsVertical. OOA}
    SizeOf(Integer) +    {DrawPartCode. OOA}
    SizeOf(TScrollChars));
end;

{ TScroller }

constructor TScroller.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar);
begin
  TView.Init(Bounds);
  Options := Options or ofSelectable;
  EventMask := EventMask or evBroadcast;
  HScrollBar := AHScrollBar;
  VScrollBar := AVScrollBar;
  Step.X:= CharWidth;  {OOA}
  Step.Y:= FontHeight; {OOA}
end;

constructor TScroller.Load(var S: TStream);
begin
  TView.Load(S);
  GetPeerViewPtr(S, HScrollBar);
  GetPeerViewPtr(S, VScrollBar);
  S.Read(Delta, SizeOf(TPoint)*3); {OOA}
end;

procedure TScroller.ChangeBounds(var Bounds: TRect);
begin
  SetBounds(Bounds);
  Inc(DrawLock);
  SetLimit(Limit.X, Limit.Y);
  Dec(DrawLock);
  DrawFlag := False;
  DrawView;
end;

procedure TScroller.CheckDraw;
begin
  if (DrawLock = 0) and DrawFlag then
  begin
    DrawFlag := False;
    DrawView;
  end;
end;

function TScroller.GetPalette: PPalette;
const
  P: String[Length(CScroller)] = CScroller;
begin
  GetPalette := @P;
end;

procedure TScroller.HandleEvent(var Event: TEvent);
begin
  LastDelta := Delta; {TONY}
  TView.HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmScrollBarChanged) and
     ((Event.InfoPtr = HScrollBar) or (Event.InfoPtr = VScrollBar)) then
      ScrollDraw;
end;

procedure TScroller.ScrollDraw;
var
  D: TPoint;
begin
  if HScrollBar <> nil then D.X := HScrollBar^.Value
  else D.X := 0;
  if VScrollBar <> nil then D.Y := VScrollBar^.Value
  else D.Y := 0;
  if (D.X <> Delta.X) or (D.Y <> Delta.Y) then
  begin
    SetCursor(Cursor.X + (Delta.X - D.X) * Step.X,
      Cursor.Y + (Delta.Y - D.Y) * Step.Y); {OOA}
    LastDelta := Delta; {TONY}
    Delta := D;
    if DrawLock <> 0 then DrawFlag := True else DrawView;
    LastDelta := Delta; {TONY}
  end;
end;

procedure TScroller.ScrollTo(X, Y: Integer);
begin
  Inc(DrawLock);
  if HScrollBar <> nil then HScrollBar^.SetValue(X);
  if VScrollBar <> nil then VScrollBar^.SetValue(Y);
  Dec(DrawLock);
  CheckDraw;
end;

procedure TScroller.SetLimit(X, Y: Integer);
begin
  Limit.X := X;
  Limit.Y := Y;
  Inc(DrawLock);
  if HScrollBar <> nil then
    HScrollBar^.SetParams(HScrollBar^.Value, 0, X - (Size.X div Step.X),
      (Size.X div Step.X) - 1, HScrollBar^.ArStep); {OOA}
  if VScrollBar <> nil then
    VScrollBar^.SetParams(VScrollBar^.Value, 0, Y - (Size.Y div Step.Y),
      (Size.Y div Step.Y) - 1, VScrollBar^.ArStep); {OOA}
  Dec(DrawLock);
  CheckDraw;
end;

procedure TScroller.SetState(AState: Word; Enable: Boolean);

procedure ShowSBar(SBar: PScrollBar);
begin
 { if (SBar <> nil) then
    if GetState(sfActive + sfSelected) then SBar^.Show
    else SBar^.Hide; ??????? dk}
end;

begin
  TView.SetState(AState, Enable);
  if AState and (sfActive + sfSelected) <> 0 then
  begin
    ShowSBar(HScrollBar);
    ShowSBar(VScrollBar);
  end;
end;

procedure TScroller.Store(var S: TStream);
begin
  TView.Store(S);
  PutPeerViewPtr(S, HScrollBar);
  PutPeerViewPtr(S, VScrollBar);
  S.Write(Delta, SizeOf(TPoint)*3); {OOA}
end;

{ TListViewer }

constructor TListViewer.Init(var Bounds: TRect; ANumCols: Word; {SHIM}
  AHScrollBar, AVScrollBar: PScrollBar);
var
  ArStep, PgStep: Integer;
begin
  TView.Init(Bounds);
  Options := Options or (ofFirstClick + ofSelectable);
  EventMask := EventMask or evBroadcast;
  Range := 0;
  NumCols := ANumCols;
  Focused := 0;
  Step.X:= CharWidth;
  Step.Y:= FontHeight;
  if AVScrollBar <> nil then
  begin
    if NumCols = 1 then
    begin
      PgStep := Size.Y div Step.Y;
      ArStep := 1;
    end else
    begin
      PgStep := Size.Y div Step.Y * NumCols;
      ArStep := Size.Y;
    end;
    AVScrollBar^.SetStep(PgStep, ArStep);
  end;
  if AHScrollBar <> nil then AHScrollBar^.SetStep(Size.X div NumCols, 1);
  HScrollBar := AHScrollBar;
  VScrollBar := AVScrollBar;
end;

constructor TListViewer.Load(var S: TStream);
begin
  TView.Load(S);
  GetPeerViewPtr(S, HScrollBar);
  GetPeerViewPtr(S, VScrollBar);
  S.Read(NumCols, SizeOf(Word) * 4 + SizeOf(TPoint) {OOA});
end;

procedure TListViewer.ChangeBounds(var Bounds: TRect);
begin
  TView.ChangeBounds(Bounds);
  if HScrollBar <> nil then
    HScrollBar^.SetStep(Size.X div NumCols, HScrollBar^.ArStep);
  if VScrollBar <> nil then
    VScrollBar^.SetStep(Size.Y, VScrollBar^.ArStep);
end;

procedure TListViewer.Draw {OOA} {SHIM};
var
  I, J, Item: Integer;
  NormalColor, SelectedColor, FocusedColor, ShadowColor, Color: Word;
  ColWidth, CurCol, Indent: Integer;
  Text: String;
  {SCOff: Byte;}
  R: TRect;
begin
  NormalColor := GetColor($0102);
  SelectedColor := GetColor($0304);
  ShadowColor := GetColor($0708);
  if State and (sfSelected + sfActive) = (sfSelected + sfActive) then
    FocusedColor := GetColor($0506)
  else
    FocusedColor := SelectedColor;

  if HScrollBar <> nil then Indent := HScrollBar^.Value else Indent := 0;
  ColWidth := ((Size.X - 1) div Step.X) div NumCols + 1;

  Bar{Pattern}(1, 1, Size.X-1, Size.Y-1, Lo(NormalColor));

  for I := 0 to (Size.Y div Step.Y) - 1 do
  begin
    for J := 0 to NumCols-1 do
    begin
      Item := J*(Size.Y div Step.Y) + I + TopItem;
      CurCol := J*ColWidth;
      if (State and (sfSelected + sfActive) = (sfSelected + sfActive)) and
        (Focused = Item) and (Range > 0) then
      begin
        Color := FocusedColor;
        SetCursor(Succ(CurCol) * Step.X, I * Step.Y);
      end else if (Item < Range) and IsSelected(Item) then
        Color := SelectedColor
      else
        Color := NormalColor;

      if Item < Range then begin
        R.Assign(CurCol * Step.X, I * Step.Y, (CurCol + ColWidth) * Step.X, Succ(I) * Step.Y);
        DrawItem(Item, Indent, R, Color);
      end;
      if (NumCols > 1) and (CurCol > 0) then {SHIM} begin
        VLine(CurCol * Step.X - 1, 0, Size.Y, Hi(ShadowColor));
        VLine(CurCol * Step.X, 0, Size.Y, Lo(ShadowColor));
      end;
    end;
  end;
  Rectangle(0, 0, Size.X, Size.Y, 1, Lo(ShadowColor), Hi(ShadowColor));
end;

procedure TListViewer.FocusItem(Item: Integer);
var
  K : integer;
begin
  Focused := Item;
  K := Size.Y div Step.Y;
  if VScrollBar <> nil then VScrollBar^.SetValue(Item);
  if Item < TopItem then
    if NumCols = 1 then TopItem := Item
    else TopItem := Item - Item mod K
  else if Item >= TopItem + (K * NumCols) then
    if NumCols = 1 then TopItem := Item - K + 1
    else TopItem := Item - Item mod K - (K * (NumCols - 1));
end;

procedure TListViewer.FocusItemNum(Item: Integer);
begin
  if Item < 0 then Item := 0
  else if (Item >= Range) and (Range > 0) then Item := Range-1;
  if Range <> 0 then FocusItem(Item);
end;

function TListViewer.GetPalette: PPalette;
const
  P: String[Length(CListViewer)] = CListViewer;
begin
  GetPalette := @P;
end;

function TListViewer.GetText(Item: Integer; MaxLen: Integer): String;
begin
  Abstract;
end;

function TListViewer.IsSelected(Item: Integer): Boolean;
begin
  IsSelected := Item = Focused;
end;

procedure TListViewer.HandleEvent(var Event: TEvent) {SHIM, OOA};
const
  MouseAutosToSkip = 1 {was 4. maybe 2 ? OOA};
  LockCount: Integer = 0;

var
  OldFocused, OldTopItem: Integer;
  ColWidth, NumItems: Integer;

procedure Redraw; {OOA}
var
  R: TRect;

procedure CalcItemRect(Item: Integer);
begin
  R.A.X:= ((Item - TopItem) div NumItems) * ColWidth * Step.X;
  R.A.Y:= ((Item - TopItem) mod NumItems) * Step.Y;
  R.B.X:= R.A.X + ColWidth * Step.X;
  R.B.Y:= R.A.Y + Step.Y;
end;

begin
  if LockCount = 0 then
    if OldFocused < 0 then DrawView {Broadcast Changes}
    else if TopItem <> OldTopItem then
    begin
      DrawView;
      OldTopItem:= TopItem;
      OldFocused:= Focused;
    end
    else if OldFocused <> Focused then
    begin
      CalcItemRect(OldFocused);
      InvalidateRect(R);
      CalcItemRect(Focused);
      InvalidateRect(R);
      DrawInvalidated;
      OldFocused:= Focused;
    end;
end;

var
  OldItem, NewItem: Integer;
  Mouse: TPoint;
  Count: Word;
begin
  TView.HandleEvent(Event);
  OldFocused:= -1;
  NumItems := Size.Y div Step.Y;
  ColWidth := ((Size.X - 1) div Step.X) div NumCols + 1;
  if (Event.What = evMouseDown) and (Event.Buttons and mbLeftButton = mbLeftButton) then
  begin
    OldFocused:= Focused;
    OldTopItem:= TopItem;
    OldItem := Focused;
    MakeLocal(Event.Where, Mouse);
    if MouseInView(Event.Where) then
      NewItem := Mouse.Y div Step.Y + TopItem +
        (NumItems * ((Mouse.X div Step.X + 1) div ColWidth))
    else NewItem := OldItem;
    Count := 0;
    repeat
      if NewItem <> OldItem then
      begin
        Inc(LockCount);
        FocusItemNum(NewItem);
        Dec(LockCount);
        Redraw;
      end;
      OldItem := NewItem;
      MakeLocal(Event.Where, Mouse);
      if MouseInView(Event.Where) then
        NewItem := Mouse.Y div Step.Y + TopItem +
          (NumItems * ((Mouse.X div Step.X + 1) div ColWidth))
      else
      begin
        if NumCols = 1 then
        begin
          if Event.What = evMouseAuto then Inc(Count);
          if Count = MouseAutosToSkip then
          begin
            Count := 0;
            if Mouse.Y < 0 then NewItem := Focused - 1
            else if Mouse.Y div Step.Y >= NumItems then NewItem := Focused + 1;
          end;
        end
        else
        begin
          if Event.What = evMouseAuto then Inc(Count);
          if Count = MouseAutosToSkip then
          begin
            Count := 0;
            if Mouse.X < 0 then
              NewItem := Focused - NumItems
            else if Mouse.X >= NumCols * ColWidth * Step.X then
              NewItem := Focused + NumItems
            else if Mouse.Y < 0 then
              NewItem := Focused - Focused mod NumItems
            else if Mouse.Y > NumItems * Step.Y then
              NewItem := Focused - Focused mod NumItems + NumItems - 1;
          end
        end;
      end;
    until not MouseEvent(Event, evMouseMove + evMouseAuto);
    Inc(LockCount);
    FocusItemNum(NewItem);
    Dec(LockCount);
    Redraw;
    if Event.Double and (Range > Focused) then SelectItem(Focused);
    ClearEvent(Event);
  end
  else if Event.What = evKeyDown then
  begin
    OldFocused:= Focused;
    OldTopItem:= TopItem;
    if (Event.CharCode = ' ') and (Focused < Range) then
    begin
      SelectItem(Focused);
      NewItem := Focused;
    end
    else case CtrlToArrow(Event.KeyCode) of
      kbUp: NewItem := Focused - 1;
      kbDown: NewItem := Focused + 1;
      kbRight: if NumCols > 1 then NewItem := Focused + NumItems else Exit;
      kbLeft: if NumCols > 1 then NewItem := Focused - NumItems else Exit;
      kbPgDn: NewItem := Focused + NumItems * NumCols;
      kbPgUp: NewItem := Focused - NumItems * NumCols;
      kbHome: NewItem := TopItem;
      kbEnd: NewItem := TopItem + (NumItems * NumCols) - 1;
      kbCtrlPgDn: NewItem := Range - 1;
      kbCtrlPgUp: NewItem := 0;
    else
      Exit;
    end;
    Inc(LockCount);
    FocusItemNum(NewItem);
    Dec(LockCount);
    Redraw;
    ClearEvent(Event);
  end else if Event.What = evBroadcast then
    if Options and ofSelectable <> 0 then
      if (Event.Command = cmScrollBarClicked) and
         ((Event.InfoPtr = HScrollBar) or (Event.InfoPtr = VScrollBar)) then
        Select
      else if (Event.Command = cmScrollBarChanged) then
      begin
        if (VScrollBar = Event.InfoPtr) then
        begin
          FocusItemNum(VScrollBar^.Value);
          Redraw;
        end else if (HScrollBar = Event.InfoPtr) then Redraw;
      end;
end;

procedure TListViewer.SelectItem(Item: Integer);
begin
  Message(Owner, evBroadcast, cmListItemSelected, @Self);
end;

procedure TListViewer.SetRange(ARange: Integer);
begin
  Range := ARange;
  if VScrollBar <> nil then
  begin
    if Focused > ARange then Focused := 0;
    VScrollbar^.SetParams(Focused, 0, ARange-1, VScrollBar^.PgStep,
      VScrollBar^.ArStep);
  end;
end;

procedure TListViewer.SetState(AState: Word; Enable: Boolean);

procedure ShowSBar(SBar: PScrollBar);
begin
  if (SBar <> nil) then
    if GetState(sfActive) and GetState(sfVisible) then SBar^.Show
    else SBar^.Hide;
end;

begin
  TView.SetState(AState, Enable);
  if AState and (sfSelected + sfActive + sfVisible) <> 0 then
  begin
    ShowSBar(HScrollBar);
    ShowSBar(VScrollBar);
    DrawView;
  end;
end;

procedure TListViewer.Store(var S: TStream);
begin
  TView.Store(S);
  PutPeerViewPtr(S, HScrollBar);
  PutPeerViewPtr(S, VScrollBar);
  S.Write(NumCols, SizeOf(Word) * 4 + SizeOf(TPoint)); {SHIM!!!}
end;

procedure TListViewer.DrawItem(Item, Indent: Integer;
  Bounds: TRect; Color: LongInt); {OOA}
var
  Text: String;
  ColWidth: Integer;
begin
  ColWidth:= (Bounds.B.X - Bounds.A.X) div Step.X;
  FillChar(Text[1], ColWidth, 32);
  Text[0]:= Char(ColWidth);
  System.Insert(GetText(Item, ColWidth + Indent), Text, 2);
  Bar(Bounds.A.X, Bounds.A.Y, Bounds.B.X, Bounds.B.Y, Lo(Color));

  if (Bounds.B.Y - Bounds.A.Y) > FontHeight then
    Inc(Bounds.A.Y, (Bounds.B.Y - Bounds.A.Y - FontHeight) div 2);

  WrStr(Bounds.A.X, Bounds.A.Y, Copy(Text, 1, ColWidth), Hi(Color));
end;

{ TGroup }

constructor TGroup.Init(var Bounds: TRect);
begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable + ofBuffered);
  GetExtent(Clip);
  EventMask := $FFFF;
end;

constructor TGroup.Load(var S: TStream);
var
  FixupSave: PFixupList;
  Count, I: Integer;
  P, Q: ^Pointer;
  V: PView;
  OwnerSave: PGroup;
begin
  TView.Load(S);
  GetExtent(Clip);
  OwnerSave := OwnerGroup;
  OwnerGroup := @Self;
  FixupSave := FixupList;
  S.Read(Count, SizeOf(Word));
  asm
        MOV     CX,Count
        SHL     CX,2
        SUB     SP,CX
        MOV     FixupList.Word[0],SP
        MOV     FixupList.Word[2],SS
        MOV     DI,SP
        PUSH    SS
        POP     ES
        XOR     AL,AL
        CLD
        REP     STOSB
  end;
  for I := 1 to Count do
  begin
    V := PView(S.Get);
    if V <> nil then InsertView(V, nil);
  end;
  V := Last;
  for I := 1 to Count do
  begin
    V := V^.Next;
    P := FixupList^[I];
    while P <> nil do
    begin
      Q := P;
      P := P^;
      Q^ := V;
    end;
  end;
  OwnerGroup := OwnerSave;
  FixupList := FixupSave;
  GetSubViewPtr(S, V);
  SetCurrent(V, NormalSelect);
  if OwnerGroup = nil then Awaken;
end;

destructor TGroup.Done;
var
  P, T: PView;
begin
  Hide;
  P := Last;
  if P <> nil then
  begin
    repeat
      P^.Hide;
      P := P^.Prev;
    until P = Last;
    repeat
      T := P^.Prev;
      Dispose(P, Done);
      P := T;
    until Last = nil;
  end;
  TView.Done;
end;

function TGroup.At(Index: Integer): PView; assembler;
asm
        LES     DI,Self
        LES     DI,ES:[DI].TGroup.Last
        MOV     CX,Index
@@1:    LES     DI,ES:[DI].TView.Next
        LOOP    @@1
        MOV     AX,DI
        MOV     DX,ES
end;

procedure TGroup.Awaken;

  procedure DoAwaken(P: PView); far;
  begin
    P^.Awaken;
  end;

begin
  ForEach(@DoAwaken);
end;

procedure TGroup.ChangeBounds(var Bounds: TRect);
var
  D: TPoint;

procedure DoCalcChange(P: PView); far;
var
  R: TRect;
begin
  P^.CalcBounds(R, D);
  P^.ChangeBounds(R);
end;

begin
  D.X := Bounds.B.X - Bounds.A.X - Size.X;
  D.Y := Bounds.B.Y - Bounds.A.Y - Size.Y;
  if Longint(D) = 0 then
  begin
    SetBounds(Bounds);
    DrawView;
  end else
  begin
    SetBounds(Bounds);
    GetExtent(Clip);
    Lock;
    ForEach(@DoCalcChange);
    Unlock;
  end;
end;

function TGroup.DataSize: Word;
var
  T: Word;

procedure AddSubviewDataSize(P: PView); far;
begin
  Inc(T, P^.DataSize);
end;

begin
  T := 0;
  ForEach(@AddSubviewDataSize);
  DataSize := T;
end;

procedure TGroup.Delete(P: PView);
var
  SaveState: Word;
begin
  SaveState := P^.State;
  P^.Hide;
  RemoveView(P);
  P^.Owner := nil;
  P^.Next := nil;
  if SaveState and sfVisible <> 0 then P^.Show;
end;

procedure TGroup.Draw {SHIM};
var
  R: TRect;
begin
  {Inc(LockFlag);}
  GetClipRect(Clip);
  DrawSubViews(First, nil); {Redraw; OOA}
  GetExtent(Clip);
  {Dec(LockFlag);}
end;

procedure TGroup.DrawSubViews(P, Bottom: PView); {TONY}
var
  P0: PView;
begin
  if P <> nil then begin
    P0 := P;
    while P <> Bottom do
    begin
      if (P^.Options and ofTransparent = 0) then P^.DrawInvalidated {OOA};
      P := P^.NextView;
    end;
    P := P0;
    while P <> Bottom do
    begin
      if (P^.Options and ofTransparent <> 0) then P^.DrawInvalidated {OOA};
      P := P^.NextView;
    end;
  end;
end;

procedure TGroup.EndModal(Command: Word);
begin
  if State and sfModal <> 0 then EndState := Command
  else TView.EndModal(Command);
end;

procedure TGroup.EventError(var Event: TEvent);
begin
  if Owner <> nil then Owner^.EventError(Event);
end;

function TGroup.Execute: Word;
var
  E: TEvent;
begin
  repeat
    EndState := 0;
    repeat
      GetEvent(E);
      HandleEvent(E);
      if E.What <> evNothing then EventError(E);
    until EndState <> 0;
  until Valid(EndState);
  Execute := EndState;
end;

function TGroup.ExecView(P: PView): Word;
var
  SaveOptions: Word;
  SaveOwner: PGroup;
  SaveTopView: PView;
  SaveCurrent: PView;
  SaveCommands: TCommandSet;
begin
  if P <> nil then
  begin
    SaveOptions := P^.Options;
    SaveOwner := P^.Owner;
    SaveTopView := TheTopView;
    SaveCurrent := Current;
    GetCommands(SaveCommands);
    TheTopView := P;
    P^.Options := P^.Options and not ofSelectable;
    P^.SetState(sfModal, True);
    SetCurrent(P, EnterSelect);
    if SaveOwner = nil then Insert(P);
    ExecView := P^.Execute;
    if SaveOwner = nil then Delete(P);
    SetCurrent(SaveCurrent, LeaveSelect);
    P^.SetState(sfModal, False);
    P^.Options := SaveOptions;
    TheTopView := SaveTopView;
    SetCommands(SaveCommands);
  end else ExecView := cmCancel;
end;

function TGroup.First: PView;
begin
  if Last = nil then First := nil else First := Last^.Next;
end;

function TGroup.FirstMatch(AState: Word; AOptions: Word): PView;

function Matches(P: PView): Boolean; far;
begin
  Matches := (P^.State and AState = AState) and
    (P^.Options and AOptions = AOptions);
end;

begin
  FirstMatch := FirstThat(@Matches);
end;

function TGroup.FirstThat(P: Pointer): PView; assembler;
var
  ALast: Pointer;
asm
        LES     DI,Self
        LES     DI,ES:[DI].TGroup.Last
        MOV     AX,ES
        OR      AX,DI
        JE      @@3
        MOV     WORD PTR ALast[2],ES
        MOV     WORD PTR ALast[0],DI
@@1:    LES     DI,ES:[DI].TView.Next
        PUSH    ES
        PUSH    DI
        PUSH    ES
        PUSH    DI
        PUSH    WORD PTR [BP]
        CALL    P
        POP     DI
        POP     ES
        OR      AL,AL
        JNE     @@2
        CMP     DI,WORD PTR ALast[0]
        JNE     @@1
        MOV     AX,ES
        CMP     AX,WORD PTR ALast[2]
        JNE     @@1
        XOR     DI,DI
        MOV     ES,DI
@@2:    MOV     SP,BP
@@3:    MOV     AX,DI
        MOV     DX,ES
end;

function TGroup.FindNext(Forwards: Boolean): PView;
var
  P: PView;
begin
  FindNext := nil;
  if Current <> nil then
  begin
    P := Current;
    repeat
      if Forwards then P := P^.Next else P := P^.Prev;
    until ((P^.State and (sfVisible + sfDisabled) = sfVisible) and
      (P^.Options and ofSelectable <> 0)) or (P = Current);
    if P <> Current then FindNext := P;
  end;
end;

function TGroup.FocusNext(Forwards: Boolean): Boolean;
var
  P: PView;
begin
  P := FindNext(Forwards);
  FocusNext := True;
  if P <> nil then FocusNext := P^.Focus;
end;

procedure TGroup.ForEach(P: Pointer); assembler;
var
  ALast: Pointer;
asm
        LES     DI,Self
        LES     DI,ES:[DI].TGroup.Last
        MOV     AX,ES
        OR      AX,DI
        JE      @@4
        MOV     WORD PTR ALast[2],ES
        MOV     WORD PTR ALast[0],DI
        LES     DI,ES:[DI].TView.Next
@@1:    CMP     DI,WORD PTR ALast[0]
        JNE     @@2
        MOV     AX,ES
        CMP     AX,WORD PTR ALast[2]
        JE      @@3
@@2:    PUSH    WORD PTR ES:[DI].TView.Next[2]
        PUSH    WORD PTR ES:[DI].TView.Next[0]
        PUSH    ES
        PUSH    DI
        PUSH    WORD PTR [BP]
        CALL    P
        POP     DI
        POP     ES
        JMP     @@1
@@3:    PUSH    WORD PTR [BP]
        CALL    P
@@4:
end;

procedure TGroup.GetData(var Rec);
type
  Bytes = array[0..65534] of Byte;
var
  I: Word;
  V: PView;
begin
  I := 0;
  if Last <> nil then
  begin
    V := Last;
    repeat
      V^.GetData(Bytes(Rec)[I]);
      Inc(I, V^.DataSize);
      V := V^.Prev;
    until V = Last;
  end;
end;

function TGroup.GetHelpCtx: Word;
var
  H: Word;
begin
  H:= hcNoContext;
  if Current <> nil then H := Current^.GetHelpCtx;
  if H = hcNoContext then H := TView.GetHelpCtx;
  GetHelpCtx := H;
end;

procedure TGroup.GetSubViewPtr(var S: TStream; var P);
var
  Index: Word;
begin
  S.Read(Index, SizeOf(Word));
  if Index > 0 then
    Pointer(P) := At(Index)
  else
    Pointer(P) := nil;
end;

procedure TGroup.HandleEvent(var Event: TEvent);

procedure DoHandleEvent(P: PView); far;
begin
  if (P = nil) or ((P^.State and sfDisabled <> 0)
    and (Event.What and (PositionalEvents or FocusedEvents) <> 0)) then Exit;
  case Phase of
    phPreProcess: if P^.Options and ofPreProcess = 0 then Exit;
    phPostProcess: if P^.Options and ofPostProcess = 0 then Exit;
  end;
  if Event.What and P^.EventMask <> 0 then P^.HandleEvent(Event);
end;

function ContainsMouse(P: PView): Boolean; far;
begin
  ContainsMouse := (P^.State and sfVisible <> 0) and
    P^.MouseInView(Event.Where);
end;

begin
  TView.HandleEvent(Event);
  if Event.What and FocusedEvents <> 0 then
  begin
    Phase := phPreProcess;
    ForEach(@DoHandleEvent);
    Phase := phFocused;
    DoHandleEvent(Current);
    Phase := phPostProcess;
    ForEach(@DoHandleEvent);
  end else
  begin
    Phase := phFocused;
    if (Event.What and PositionalEvents <> 0) then
      DoHandleEvent(FirstThat(@ContainsMouse))
    else
      ForEach(@DoHandleEvent);
  end;
end;

function TGroup.IndexOf(P: PView): Integer; assembler;
asm
        LES     DI,Self
        LES     DI,ES:[DI].TGroup.Last
        MOV     AX,ES
        OR      AX,DI
        JE      @@3
        MOV     CX,DI
        MOV     BX,ES
        XOR     AX,AX
@@1:    INC     AX
        LES     DI,ES:[DI].TView.Next
        MOV     DX,ES
        CMP     DI,P.Word[0]
        JNE     @@2
        CMP     DX,P.Word[2]
        JE      @@3
@@2:    CMP     DI,CX
        JNE     @@1
        CMP     DX,BX
        JNE     @@1
        XOR     AX,AX
@@3:
end;

procedure TGroup.Insert(P: PView);
begin
  InsertBefore(P, First);
end;

procedure TGroup.InsertBefore(P, Target: PView);
var
  SaveState: Word;
begin
  if (P <> nil) and (P^.Owner = nil) and
    ((Target = nil) or (Target^.Owner = @Self)) then
  begin
    if P^.Options and ofCenterX <> 0 then
      P^.Origin.X := (Size.X - P^.Size.X) div 2;
    if P^.Options and ofCenterY <> 0 then
      P^.Origin.Y := (Size.Y - P^.Size.Y) div 2;
    SaveState := P^.State;
    P^.Hide;
    InsertView(P, Target);
    if (P^.LogPalette.Mode and pmUseRGB) <> 0 then
      PaletteUsers.Insert(P); {DK}
    if SaveState and sfVisible <> 0 then P^.Show;
    if State and sfActive <> 0 then
      P^.SetState(sfActive, True);
  end;
end;

procedure TGroup.InsertView(P, Target: PView);
begin
  P^.Owner := @Self;
  if Target <> nil then
  begin
    Target := Target^.Prev;
    P^.Next := Target^.Next;
    Target^.Next := P;
  end else
  begin
    if Last = nil then P^.Next := P else
    begin
      P^.Next := Last^.Next;
      Last^.Next := P;
    end;
    Last := P;
  end;
end;

procedure TGroup.Lock;
begin
  {if LockFlag <> 0 then} Inc(LockFlag);
end;

procedure TGroup.PutSubViewPtr(var S: TStream; P: PView);
var
  Index: Word;
begin
  if P = nil then Index := 0
  else Index := IndexOf(P);
  S.Write(Index, SizeOf(Word));
end;

procedure TGroup.Redraw;
begin
  InvalidateSelf;
  DrawSubViews(First, nil);
end;

procedure TGroup.RemoveView(P: PView); assembler;
asm
        PUSH    DS
        LDS     SI,Self
        LES     DI,P
        LDS     SI,DS:[SI].TGroup.Last
        PUSH    BP
        MOV     AX,DS
        OR      AX,SI
        JE      @@7
        MOV     AX,SI
        MOV     DX,DS
        MOV     BP,ES
@@1:    MOV     BX,WORD PTR DS:[SI].TView.Next[0]
        MOV     CX,WORD PTR DS:[SI].TView.Next[2]
        CMP     CX,BP
        JE      @@5
@@2:    CMP     CX,DX
        JE      @@4
@@3:    MOV     SI,BX
        MOV     DS,CX
        JMP     @@1
@@4:    CMP     BX,AX
        JNE     @@3
        JMP     @@7
@@5:    CMP     BX,DI
        JNE     @@2
        MOV     BX,WORD PTR ES:[DI].TView.Next[0]
        MOV     CX,WORD PTR ES:[DI].TView.Next[2]
        MOV     DS:WORD PTR [SI].TView.Next[0],BX
        MOV     DS:WORD PTR [SI].TView.Next[2],CX
        CMP     DX,BP
        JNE     @@7
        CMP     AX,DI
        JNE     @@7
        CMP     CX,BP
        JNE     @@6
        CMP     BX,DI
        JNE     @@6
        XOR     SI,SI
        MOV     DS,SI
@@6:    POP     BP
        PUSH    BP
        LES     DI,Self
        MOV     WORD PTR ES:[DI].TView.Last[0],SI
        MOV     WORD PTR ES:[DI].TView.Last[2],DS
@@7:    POP     BP
        POP     DS
end;

procedure TGroup.ResetCurrent;
begin
  SetCurrent(FirstMatch(sfVisible, ofSelectable), NormalSelect);
end;

procedure TGroup.ResetCursor;
begin
  if Current <> nil then Current^.ResetCursor;
end;

procedure TGroup.PaletteEnable(AEnable : Boolean); {DK}
var
  PalUser : Boolean;

Procedure DoPalFix(View : PView); Far;
Begin
  View^.PaletteEnable(AEnable);
  if (View^.LogPalette.Mode and pmUseRGB) <> 0 then PalUser := True;
End;

Begin
  PalUser := False;
  Inherited PaletteEnable(AEnable);
  ForEach(@DoPalFix);
  if PalUser then
    LogPalette.Mode := LogPalette.Mode or pmUseRGB
  else
    LogPalette.Mode := LogPalette.Mode and not pmUseRGB;
End;



procedure TGroup.SelectNext(Forwards: Boolean);
var
  P: PView;
begin
  P := FindNext(Forwards);
  if P <> nil then P^.Select;
end;

procedure TGroup.SetCurrent(P: PView; Mode: SelectMode);

procedure ResetMouse; {TONY}
var
  Event: TEvent;
begin
  Event.What := evMouseMove;
  Event.Buttons := 0;
  Event.Double := False;
  Event.Where := MouseWhere;
  MouseMastering(Event);
end;

procedure SelectView(P: PView; Enable: Boolean);
begin
  if P <> nil then P^.SetState(sfSelected, Enable);
end;

procedure FocusView(P: PView; Enable: Boolean);
begin
  if (State and sfFocused <> 0) and (P <> nil) then
    P^.SetState(sfFocused, Enable);
end;

begin
  if Current <> P then
  begin
    {Lock;}
    FocusView(Current, False);
    if Mode <> EnterSelect then SelectView(Current, False);
    if Mode <> LeaveSelect then SelectView(P, True);
    FocusView(P, True);
    Current := P;
    {Unlock;}
  end;
  ResetMouse; {TONY}
end;

procedure TGroup.SetData(var Rec);
type
  Bytes = array[0..65534] of Byte;
var
  I: Word;
  V: PView;
begin
  I := 0;
  if Last <> nil then
  begin
    V := Last;
    repeat
      V^.SetData(Bytes(Rec)[I]);
      Inc(I, V^.DataSize);
      V := V^.Prev;
    until V = Last;
  end;
end;

procedure TGroup.SetState(AState: Word; Enable: Boolean);

procedure DoSetState(P: PView); far;
begin
  P^.SetState(AState, Enable);
end;

procedure DoExpose(P: PView); far;
begin
  if P^.State and sfVisible <> 0 then P^.SetState(sfExposed, Enable);
end;

begin
  TView.SetState(AState, Enable);
  case AState of
(*
    sfActive, sfDragging:
      begin
        Lock;
        ForEach(@DoSetState);
        Unlock;
      end;
*)
    sfDragging: {TONY}
      begin
        if Enable then
          if Owner <> Nil then Owner^.Lock else Lock;
        ForEach(@DoSetState);
        if not Enable then
          if Owner <> Nil then Owner^.UnLock else UnLock;
      end;
    sfActive: {TONY}
      begin
        ForEach(@DoSetState);
      end;
    sfFocused:
      if Current <> nil then Current^.SetState(sfFocused, Enable);
    sfExposed:
      begin
        ForEach(@DoExpose);
      end;
  end;
end;

procedure TGroup.Store(var S: TStream);
var
  Count: Integer;
  OwnerSave: PGroup;

procedure DoPut(P: PView); far;
begin
  S.Put(P);
end;

begin
  TView.Store(S);
  OwnerSave := OwnerGroup;
  OwnerGroup := @Self;
  Count := IndexOf(Last);
  S.Write(Count, SizeOf(Word));
  ForEach(@DoPut);
  PutSubViewPtr(S, Current);
  OwnerGroup := OwnerSave;
end;

procedure TGroup.Unlock;
begin
  if LockFlag <> 0 then
  begin
    Dec(LockFlag);
    if LockFlag = 0 then DrawInvalidated;
  end;
end;

function TGroup.Valid(Command: Word): Boolean;

function IsInvalid(P: PView): Boolean; far;
begin
  IsInvalid := not P^.Valid(Command);
end;

begin
  Valid := True;
  if Command = cmReleasedFocus then
  begin
    if (Current <> nil) and (Current^.Options and ofValidate <> 0) then
      Valid := Current^.Valid(Command);
  end
  else
    Valid := FirstThat(@IsInvalid) = nil;
end;

procedure TGroup.MouseMastering(const Event: TEvent); {TONY}
var
  WhoHasMouse: PView; {TONY}

  function ContainsMouse(P: PView): Boolean; far;
  begin
    ContainsMouse := (P^.State and sfVisible <> 0) and
      P^.MouseInView(Event.Where);
  end;

begin
   WhoHasMouse := FirstThat(@ContainsMouse);
   if WhoHasMouse <> Nil then
     WhoHasMouse^.MouseMastering(Event);
end;

procedure TGroup.InvalidateRect(Rect: TRect); {OOA}
var
  R: TRect;

  procedure InvalidateSubview(P: PView); far;
  begin
    R.Copy(Rect);
    R.Move(-P^.Origin.X, -P^.Origin.Y);
    P^.InvalidateRect(R);
  end;

begin
  if State and sfVisible <> 0 then ForEach(@InvalidateSubview);
end;

procedure TGroup.ValidateRect(Rect: TRect); {OOA}
var
  R: TRect;

  procedure ValidateSubview(P: PView); far;
  begin
    R.Copy(Rect);
    R.Move(-P^.Origin.X, -P^.Origin.Y);
    P^.ValidateRect(R);
  end;

begin
  if State and sfVisible <> 0 then ForEach(@ValidateSubview);
end;

procedure TGroup.GetInvalidExtent(var Extent: TRect); {OOA}
var
  R: TRect;

  procedure CheckInvalidView(P: PView); far;
  begin
    if P^.State and sfVisible = 0 then Exit;
    P^.GetInvalidExtent(R);
    if not R.Empty then with Extent do
    begin
      R.Move(P^.Origin.X, P^.Origin.Y);
      if not Empty then Union(R) else Copy(R);
    end;
  end;

begin
  Longint(Extent.A):= 0;
  Longint(Extent.B):= 0;
  ForEach(@CheckInvalidView);
end;

procedure TGroup.CalcClipRects; {OOA}
var
  InvalidRect, ClipRect: TRect;
begin
  {$IFDEF DEBUG}
  if ClipRects^.Count <> 0 then RunError(255);
  {$ENDIF}
  {Calc}GetClipRect(ClipRect);
  GetInvalidExtent(InvalidRect);
  ClipRect.Intersect(InvalidRect);
  ClipRects^.InsertCopy(ClipRect);
end;

{ TIconizedWindow }

constructor TIconizedWindow.Init(AANDImage, AXORImage: Integer; ALink: PWindow);
var
  R: TRect;
begin
  SetFont(IconsFont);
  Font := GlobalFont;
  R.Assign(0, 0, MaxInteger(IconsSpacing, FontWidth(ALink^.Title^)), 32 + GetHeight + 4 {???});
  inherited Init(R);
  Options := Options or ofSelectable or ofTransparent or ofTopSelect
    or ofFirstClick;
  ANDImage := AANDImage;
  XORImage := AXORImage;
  Link:= ALink;
  RestoreFont;
  EventMask := Eventmask or evBroadcast;
  {Hide;}
end;

procedure TIconizedWindow.HandleEvent(var Event: TEvent);
var
  MC : PMouseCursor;
  Mouse: TPoint;
begin
  inherited HandleEvent(Event);
  if (Event.What = evMouseDown) and (Event.Buttons and mbLeftButton = mbLeftButton)
  then begin
    if (Event.Double) and Assigned(Link) then begin
      ClearEvent(Event);
      Link^.Restore;
    end else begin
      MakeLocal(Event.Where, Mouse);
      MC := Icon2Cursor(GetImage(ANDImage), GetImage(XORImage), Mouse.X - (Size.X - 32) div 2, Mouse.Y);
      if Assigned(MC) and Assigned(MC^.Image) then begin
        if MouseEvent(Event, evMouseMove) then begin
          State := State and (not sfVisible);
          DrawHide(Nil); SetMouseCursorShape(MC); LockMouseShape;
          repeat
          until not MouseEvent(Event, evMouseMove);
          UnLockMouseShape;
          SetMouseCursorShape(@DefaultMouseCursor);
          Owner^.MakeLocal(Event.Where, Mouse);
          MoveTo(Mouse.X - MC^.XHotSpot - (Size.X - 32) div 2, Mouse.Y - MC^.YHotSpot);
          State := State or (sfVisible);
          DrawShow(Nil);
        end;
        FreeCursor(MC^);
        Dispose(MC);
      end;
    end;
    ClearEvent(Event);
  end else if (Event.What = evKeyDown) and
  (Event.KeyCode = kbEnter) then begin
    ClearEvent(Event);
    if Link <> Nil then Link^.Restore;
  end else if (Event.What = evCommand) and
    (Event.Command = cmCheckIconPosition) then
      if (Longint(Origin) = Event.InfoLong) then
        ClearEvent(Event);
  if (Event.What = evBroadcast) and  (Event.Command = cmRedrawIcons) then DrawView;
end;

procedure TIconizedWindow.Draw;
var
  I: Integer;
  Clr: Integer;
  T  : TTitleStr; {SHIM}
begin
  I:= (Size.X - 32) div 2;
  PutBMPOp(GetImage(ANDImage), I, 0, 2);
  PutBMPOp(GetImage(XORImage), I, 0, 1);
{  if (State and sfSelected = sfSelected) then}
  if (State and sfFocused = sfFocused) then
    Clr := Link^.GetColor($0C0B)
  else
    Clr := Link^.GetColor($0102);
  if Link <> Nil then begin
    T := Link^.GetTitle(Size.X);                             {SHIM}
    I := (Size.X - FontWidth(T)) div 2;                      {SHIM}
    WrStr(I, 32 + 2, T, Hi(Clr));                            {SHIM}
  end;
end;

procedure TIconizedWindow.SetState(AState: Word; Enable: Boolean);
var
  WindowCommands: TCommandSet;
begin
  inherited SetState(AState, Enable);
  if AState = sfSelected then
    SetState(sfActive, Enable);
  if (AState = sfSelected) or ((AState = sfExposed) and
    (State and sfSelected <> 0)) then
  begin
    WindowCommands := [cmNext, cmPrev];
    if Enable then EnableCommands(WindowCommands)
    else DisableCommands(WindowCommands);
  end;
  if AState and (sfActive + sfDragging) <> 0 then DrawView;
end;


{ TWindow }

constructor TWindow.Init(var Bounds: TRect; ATitle: TTitleStr;
  ANumber: Integer);
begin
  TGroup.Init(Bounds);
  State := State or sfShadow;
  Options := Options or (ofSelectable + ofTopSelect) or ofFirstClick;
  GrowMode := gfGrowAll + gfGrowRel;
  Flags := wfMove + wfGrow + wfClose + wfZoom;
  Title := NewStr(ATitle);
  Number := ANumber;
  Palette := wpBlueWindow;
  InitFrame;
  if Frame <> nil then Insert(Frame);
  GetBounds(ZoomRect);
  LastIconPos.X := -1;
end;

constructor TWindow.Load(var S: TStream);
begin
  TGroup.Load(S);
  S.Read(Flags, SizeOf(Byte) + SizeOf(TRect) + 2 * SizeOf(Integer));
  GetSubViewPtr(S, Frame);
  Title := S.ReadStr;
end;

destructor TWindow.Done;
begin
  SetIcon(0, 0);
  DisposeStr(Title);
  TGroup.Done;
end;

procedure TWindow.Close;
begin
  if Valid(cmClose) then Free;
end;

function TWindow.GetPalette: PPalette;
const
  P: array[wpBlueWindow..wpGrayWindow] of string[Length(CBlueWindow)] =
    (CBlueWindow, CCyanWindow, CGrayWindow);
begin
  GetPalette := @P[Palette];
end;

function TWindow.GetTitle(MaxSize: Integer): TTitleStr;
begin
  if Title <> nil then GetTitle := Title^
  else GetTitle := '';
end;

Procedure TWindow.GetClientExtent(var Extent : TRect);
Begin
  if Frame <> Nil then Frame^.GetClientExtent(Extent)
    else GetExtent(Extent);
End;

procedure TWindow.HandleEvent(var Event: TEvent);
var
  Limits: TRect;
  Min, Max: TPoint;
begin
  TGroup.HandleEvent(Event);
  if (Event.What = evCommand) then
    case Event.Command of
      cmResize:
        if Flags and (wfMove + wfGrow) <> 0 then
        begin
          Owner^.GetExtent(Limits);
          SizeLimits(Min, Max);
          DragView(Event, DragMode or (Flags and (wfMove + wfGrow)),
            Limits, Min, Max);
          ClearEvent(Event);
        end;
      cmClose:
        if (Flags and wfClose <> 0) and
          ((Event.InfoPtr = nil) or (Event.InfoPtr = @Self)) then
        begin
          ClearEvent(Event);
          if State and sfModal = 0 then Close else
          begin
            Event.What := evCommand;
            Event.Command := cmCancel;
            PutEvent(Event);
            ClearEvent(Event);
          end;
        end;
      cmZoom:
        if (Flags and wfZoom <> 0) and
          ((Event.InfoPtr = nil) or (Event.InfoPtr = @Self)) then
        begin
          Zoom;
          ClearEvent(Event);
        end;
    end
  else if Event.What = evKeyDown then
    case Event.KeyCode of
      kbTab:
        begin
          FocusNext(False);
          ClearEvent(Event);
        end;
      kbShiftTab:
        begin
          FocusNext(True);
          ClearEvent(Event);
        end;
    end
  else if (Event.What = evBroadcast) and (Event.Command = cmSelectWindowNum)
         and (Event.InfoInt = Number) and (Options and ofSelectable <> 0) then
  begin
    Select;
    ClearEvent(Event);
  end;
end;

procedure TWindow.InitFrame;
var
  R: TRect;
begin
  GetExtent(R);
  Frame := New(PFrame, Init(R));
end;

procedure TWindow.SetState(AState: Word; Enable: Boolean);
var
  WindowCommands: TCommandSet;
begin
  TGroup.SetState(AState, Enable);
  if AState = sfSelected then
    SetState(sfActive, Enable);
  if (AState = sfSelected) or ((AState = sfExposed) and
    (State and sfSelected <> 0)) then
  begin
    WindowCommands := [cmNext, cmPrev];
    if Flags and (wfGrow + wfMove) <> 0 then
      WindowCommands := WindowCommands + [cmResize];
    if Flags and wfClose <> 0 then
      WindowCommands := WindowCommands + [cmClose];
    if Flags and wfZoom <> 0 then
      WindowCommands := WindowCommands + [cmZoom];
    if Enable then EnableCommands(WindowCommands)
    else DisableCommands(WindowCommands);
  end;
end;

function TWindow.StandardScrollBar(AOptions: Word): PScrollBar; {OOA}
var
  R: TRect;
  S: PScrollBar;
  I: Integer;
begin
  I := BitMapWidth(@ScrollUpDef)+2;
  Frame^.GetClientExtent(R);
  if AOptions and sbVertical = 0 then
    R.Assign(R.A.X, R.B.Y-I, R.B.X-I, R.B.Y)
  else
    R.Assign(R.B.X-I, R.A.Y, R.B.X, R.B.Y);
  S := New(PScrollBar, Init(R));
  if S = Nil then Exit;
  Insert(S);
  if AOptions and sbHandleKeyboard <> 0 then
    S^.Options := S^.Options or ofPostProcess;
  StandardScrollBar := S;
end;

procedure TWindow.SizeLimits(var Min, Max: TPoint);
begin
  TView.SizeLimits(Min, Max);
  Min.X := MinWinSize.X;
  Min.Y := MinWinSize.Y;
end;

procedure TWindow.Store(var S: TStream);
begin
  TGroup.Store(S);
  S.Write(Flags, SizeOf(Byte) + SizeOf(TRect) + 2 * SizeOf(Integer));
  PutSubViewPtr(S, Frame);
  S.WriteStr(Title);
end;

procedure TWindow.Zoom;
var
  R: TRect;
  Max, Min: TPoint;
begin
  SizeLimits(Min, Max);
  if Longint(Size) <> Longint(Max) then
  begin
    GetBounds(ZoomRect);
    Longint(R.A) := 0;
    R.B := Max;
    Locate(R);
  end else Locate(ZoomRect);
end;

procedure TWindow.RAssign(var Rect: TRect; AX,AY,BX,BY: Integer); {OOA}
var
  W, H: Integer;
begin
  if Frame = Nil then inherited RAssign(Rect, AX,AY,BX,BY) else
  begin
    W:= CharWidth;
    H:= FontHeight;
    Frame^.GetClientExtent(Rect);
    if Rect.A.Y >= H then Dec(Rect.A.Y, H);
    with Rect do Assign(A.X + AX*W, A.Y + AY*H, A.X + BX*W, A.Y + BY*H);
  end;
end;

function TWindow.GetFrameWidth: Integer; {OOA}
begin
  if Frame = Nil then GetFrameWidth:= 0 else
    GetFrameWidth:= Frame^.GetFrameWidth;
end;

function TWindow.GetCaptureHeight: Integer; {OOA}
begin
  if Frame = Nil then GetCaptureHeight:= 0 else
    GetCaptureHeight:= Frame^.GetCaptureHeight;
end;

procedure TWindow.SetIcon(AIconAND, AIconXOR: Integer); {TONY, OOA}
begin
  IconAND:= AIconAND;
  IconXOR:= AIconXOR;
  if (IconAND or IconXOR <> 0) then begin
    if Icon <> Nil then Icon^.DrawView;
    Options := Options or ofIconizable;
    if Assigned(Frame) then Frame^.DrawView;
  end;
end;

procedure TWindow.Iconize; {TONY, OOA}
var
  R: TRect;
  H, W, X: Integer;

  function AtPositionRA(P: PView): Boolean; far;
  begin
    AtPositionRA:= Message(P, evCommand, cmCheckIconPosition, Pointer(R.A)) <> Nil;
  end;

label
  Found;
begin
  if (Owner <> Nil) and (Options and ofIconizable = ofIconizable) and
    (IconAND <> 0) and (IconXOR <> 0) and (Icon = Nil) then
  begin
    Owner^.GetExtent(R);
    New(Icon, Init(IconAND, IconXOR, @Self));
    if Icon = Nil then Exit;
    Dec(R.B.Y, IconsOffset);
    Inc(R.A.X, IconsOffset);
    X:= R.A.X;
    W:= R.B.X;
    H:= 32 + Icon^.FontHeight;
    R.A.Y:= R.B.Y - H;
    while R.A.Y > H do
    begin
      while (R.A.X + IconsSpacing) < W do
      begin
        if Owner^.FirstThat(@AtPositionRA) = Nil then
          goto Found;
        Inc(R.A.X, IconsSpacing);
      end;
      R.A.X:= X;
      Dec(R.A.Y, H);
    end;
  Found:
    Owner^.Lock;
    Icon^.Hide;
    Owner^.Insert(Icon);
    if LastIconPos.X >= 0 then R.A := LastIconPos;
    Icon^.MoveTo(R.A.X, R.A.Y);
    if LastIconPos.X < 0 then LastIconPos := R.A;
    Icon^.Show;
    Hide;
    Owner^.Unlock;
    {Icon^.Select;}
    {Icon^.DrawView;}
    Message(@Self, evCommand, cmWindowIconized, @Self);
    State := State or sfIconized;
  end;
end;

procedure TWindow.Restore; {TONY, OOA}
begin
  State := State and not sfIconized;
  Show;
  Select;
  if Icon <> Nil then
  begin
    LastIconPos := Icon^.Origin;
    Owner^.Delete(Icon);
    Dispose(Icon, Done);
    Icon:= Nil;
  end;
  Message(@Self, evCommand, cmWindowRestored, @Self);
end;

{ TTimedView }

constructor TTimedView.Init(var Bounds: TRect; Command: Word; Interval: Word);
begin
  inherited Init(Bounds);
  TicksInterval := Interval;
  CommandToSend := Command;
  EventMask := EventMask or evBroadCast;
  TimedMessage(1, @Self, evBroadCast, CommandToSend, @Self, TicksInterval);
end;

constructor TTimedView.Load; {GIO}
begin Inherited Load(S);
  S.Read(TicksInterval, SizeOf(TicksInterval));
  S.Read(CommandToSend, SizeOf(CommandToSend));
  TimedMessage(1, @Self, evBroadCast, CommandToSend, @Self, TicksInterval);
end;

procedure TTimedView.Store;  {GIO}
begin Inherited Store(S);
  S.Write(TicksInterval, SizeOf(TicksInterval));
  S.Write(CommandToSend, SizeOf(CommandToSend));
end;

procedure TTimedView.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  if (Event.What = evBroadCast) and (Event.Command = CommandToSend) then begin
    ClearEvent(Event);
    Update;
    TimedMessage(1, @Self, evBroadCast, CommandToSend, @Self, TicksInterval);
  end;
end;

procedure TTimedView.Update; begin end; { An abstract method }


{ Message dispatch function }

function Message(Receiver: PView; What, Command: Word;
  InfoPtr: Pointer): Pointer;
var
  Event: TEvent;
begin
  Message := nil;
  if Receiver <> nil then
  begin
    Event.What := What;
    Event.Command := Command;
    Event.InfoPtr := InfoPtr;
    Receiver^.HandleEvent(Event);
    if Event.What = evNothing then Message := Event.InfoPtr;
  end;
end;

{ Views registration procedure }

procedure RegisterViews;
begin
  RegisterType(RView);
  RegisterType(RFrame);
  RegisterType(RScrollBar);
  RegisterType(RScroller);
  RegisterType(RListViewer);
  RegisterType(RGroup);
  RegisterType(RWindow);
end;

var
  FontStorage: TCollection;

procedure SetFont(Font: TFont);
Var
  P : PFont;
begin
  New(P);
  P^ := GlobalFont;
  FontStorage.Insert(P);
  GlobalFont := Font;
  SelectFontCaps(GlobalFont);
end;

procedure RestoreFont;
Var
  P : PFont;
begin
  if FontStorage.Count > 0 then begin
    P := PFont(FontStorage.At(FontStorage.Count-1));
    GlobalFont := P^;
    Dispose(P);
    FontStorage.AtDelete(FontStorage.Count - 1);
    SelectFontCaps(GlobalFont);
  end
  {$IFDEF DEBUG}
  else
  begin
    PrintStr('Views.RestoreFont. Count = 0 on Restore'^J^M);
    RunError(255);
  end;
  {$ENDIF}
end;

type
  PLPM = ^TLPM;
  TLPM = record
    Priority : Integer;
    Receiver : PView;
    What     : Word;
    Command  : Word;
    InfoPtr  : Pointer;
    SentAt   : Word; { Ç‡•¨Ô, ™Æ£§† Æ‚·Î´†‚Ï ·Æ°Î‚®• }
  end;
  PLPMCollection = ^TLPMCollection;
  TLPMCollection = object(TCollection)
    procedure   FreeItem(Item: Pointer); virtual;
  end;

procedure TLPMCollection.FreeItem(Item: Pointer);
var
  ToFree: PLPM absolute Item;
begin
  Dispose(ToFree);
end;

const
  LPMS: PLPMCollection = Nil;

procedure InitLPM;
begin
  if not Assigned(LPMS) then New(LPMS, Init(10, 5));
end;

procedure DoneLPM;
begin
  if Assigned(LPMS) then begin
    Dispose(LPMS, Done);
    LPMS := Nil;
  end;
end;

procedure TimedMessage(Priority: Integer; Receiver: PView; What, Command: Word; InfoPtr: Pointer; WaitTime: Word);
var
  LPM: PLPM;

  function Same(Item: PLPM): boolean; far;
  var
    R: Boolean;
  begin
    R := (Item^.Receiver = Receiver) and
         (Item^.What = What) and
         (Item^.Command = Command);
    if R then begin  { LPM exists at collection }
      Item^.Priority := Priority;  { Change priority }
      Item^.InfoPtr := InfoPtr;    { Change info field }
      Item^.SentAt := Drivers.GetTicks + WaitTime;
    end;
    Same := R;
  end;

begin
  if LPMS = Nil then Exit;
  if LPMS^.FirstThat(@Same)<>Nil then Exit;
  New(LPM);
  if LPM = Nil then Exit;
  LPM^.Priority := Priority;
  LPM^.Receiver := Receiver;
  LPM^.What := What;
  LPM^.Command := Command;
  LPM^.InfoPtr := InfoPtr;
  LPM^.SentAt := Drivers.GetTicks + WaitTime;
  LPMS^.Insert(LPM);
end;

procedure ClearLPM(Receiver: PView; What, Command: Word);
var
  LPM: PLPM;

  function Same(Item: PLPM): boolean; far;
  begin
    if (Item^.Receiver = Receiver) and
       (Item^.What = What) and
       (Item^.Command = Command) then LPM := Item;
    Same := (LPM = Item);
  end;

begin
  if LPMS = Nil then Exit;
  LPM := Nil;
  if LPMS^.FirstThat(@Same)=Nil then Exit;
  LPMS^.Free(LPM);
end;

procedure ClearAllAddressedLPM(Receiver: PView);
var
  i: Integer;
begin
  if LPMS = Nil then Exit;
  i := 0;
  while i<LPMS^.Count do
    if (PLPM(LPMS^.At(i))^.Receiver = Receiver) or
    (Receiver = Nil) { á†£´„Ë™† }
    then
      LPMS^.AtFree(i)
    else
      Inc(i);
end;

Const
  MultiCount: Byte = 255;

function LPMCanBeSent: boolean;
label
  CanGiveQuantum, CalcQuantum;
var
  Event: TEvent;
  LPM: PLPM;
  MaxPriority: Integer;
  Ticks: Word;
  MinTime: Word;

  procedure Maximum(Item: PLPM); far;
  begin
    if (Item^.Priority > MaxPriority) and
    (LongInt(Item^.SentAt) - LongInt(Ticks) <= 0)
    then begin
      LPM := Item;
      MaxPriority := Item^.Priority;
    end;
  end;

  procedure CalcMinTime(Item: PLPM); far;
  begin
    if (Word(LongInt(Item^.SentAt) - LongInt(Ticks)) <= MinTime) then
      MinTime := Word(LongInt(Item^.SentAt) - LongInt(Ticks));
  end;

begin
  if LPMS = Nil then goto CanGiveQuantum;
  LPM := Nil; MaxPriority := -1;
  Ticks := Drivers.GetTicks;
  LPMS^.ForEach(@Maximum);
  if (LPM = Nil) then goto CalcQuantum;
  LPMS^.Delete(LPM);
  if Assigned(LPM^.Receiver) then Message(LPM^.Receiver, LPM^.What, LPM^.Command, LPM^.InfoPtr);
  LPMS^.FreeItem(LPM);
CalcQuantum:
  MinTime := $FFFF;
  LPMS^.ForEach(@CalcMinTime);
  if MinTime <> 0 then begin
CanGiveQuantum:
    MultiCount := 255;
    LPMCanBeSent := True;
  end else begin
    Dec(MultiCount);
    if MultiCount = 255 then
      LPMCanBeSent := True
    else
      LPMCanBeSent := False; { èÆ-¢®§®¨Æ¨„, ≠•´ÏßÔ Æ‚§†¢†‚Ï ™¢†≠‚ ! }
  end;
end;

procedure LPMessage(Priority: Integer; Receiver: PView; What, Command: Word; InfoPtr: Pointer);
begin
  TimedMessage(Priority, Receiver, What, Command, InfoPtr, 0);
end;

{$L _SCROLLS.OBJ}
procedure ScrollDownDef; external;
procedure ScrollDownPress; external;
procedure ScrollUpDef; external;
procedure ScrollUpPress; external;
procedure ScrollThumb; external;
procedure ComboDef; external;
procedure ComboPress; external;
procedure ScrollLeftDef; external;
procedure ScrollLeftPress; external;
procedure ScrollRightDef; external;
procedure ScrollRightPress; external;

{$L WINICONS.OBJ}
procedure CloseWindowIcon; external;
procedure RestoreWindowIcon; external;
procedure ZoomWindowIcon; external;
procedure MinimizeWindowIcon; external;

procedure RegisterScrollBarImages;
begin
  RegisterImageInCode(1, @ScrollDownDef);
  RegisterImageInCode(2, @ScrollDownPress);
  RegisterImageInCode(3, @ScrollUpDef);
  RegisterImageInCode(4, @ScrollUpPress);
  RegisterImageInCode(5, @ScrollLeftDef);
  RegisterImageInCode(6, @ScrollLeftPress);
  RegisterImageInCode(7, @ScrollRightDef);
  RegisterImageInCode(8, @ScrollRightPress);
  RegisterImageInCode(9, @ScrollThumb);
end;

procedure RegisterComboImages;
begin
  RegisterImageInCode(10, @ComboDef);
  RegisterImageInCode(11, @ComboPress);
end;

procedure RegisterWindowIcons;
begin
  RegisterImageInCode(12, @CloseWindowIcon);
  RegisterImageInCode(13, @RestoreWindowIcon);
  RegisterImageInCode(14, @ZoomWindowIcon);
  RegisterImageInCode(15, @MinimizeWindowIcon);
end;

procedure RegisterViewsImages;
begin
  RegisterScrollBarImages;
  RegisterComboImages;
  RegisterWindowIcons;
end;

var
  SaveExit: Pointer;

procedure ExitViews; far;

Procedure DisposeAll(P : PFont); Far;
Begin
  Dispose(P);
End;

begin
  ExitProc := SaveExit;
  DoneLPM;
  if FontStorage.Count > 0 then
  begin
    {$IFDEF DEBUG}
    if ExitCode = 0 then
    begin
      PrintStr('Views.ExitProc. FontStack.Count != 0 on Exit'^J^M);
      RunError(255);
    end;
    {$ENDIF}
    FontStorage.ForEach(@DisposeAll);
    FontStorage.DeleteAll;
  end;
  FontStorage.Done;
end;

begin
  SaveExit := ExitProc;
  ExitProc := @ExitViews;
  FontStorage.Init(5,1);
  RegisterViewsImages; {??? OOA}
end.
