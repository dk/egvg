
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit App;

{$O+,F+,X+,I-,S+}

interface

uses Objects, Drivers, Memory, HistList, Views, Menus, Dialogs,
  EGFont, EGCursor, BitMaps {TONY}, GDI {$IFDEF DPMI}, EShield{$ENDIF};


const

{ TApplication palette entries }

  apColor      = 0;
  apMonoChrome = 1;


{ TApplication palettes }

{$IFNDEF ShimonColors}

  CAppColor =
{ desktop background } #7                  +

{ menu               } #0#7#8#7#4          + { normal   }  {2}
                       #0#11#8#5#12        + { selected }
                       #15#8               + { 3D       }

{ ==================== blue window ================================== }

{  - frame           } #7#7                + { passive, active border } {14}
                       #8#7#0#7            + { passive, active title  }
                       #15#8               + { 3D                     }
                       #7                  + { window background      }

{  - list viewer     } #0#5#14#5#15#1     + { normal, selected, focused text } {23}
                       #15#8               + { 3D                             }

{  - static text     } #0#7              +                                     {31}

{  - label           } #0#7#1#7        + { normal, selected text }           {33}
                       #4#12               + { normal, selected shortcut }

{  - input line      } #0#5#14#1#15#8   + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#1#7        + { normal, default                 46 }
                       #0#7#8#7        + { selected, disabled                 }
                       #4#14#12            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#7#12#7#8#7 + { normal, selected, disabled text } {59}
                       #4#14#1             + { normal, selected shortcut }

{  - history window  } #5#0               + { back, title            }          {67}
                       #15#8               + { 3D                     }
                       #0#5#11#1         + { normal, focused        }

{  - editor          } #0#15#7#1         + {normal, highlight       }    {75}

{  - file info pane  } #0#5              + { fore, back }                      {79}

{ ==================== cyan window ================================== }

{  - frame           } #3#3                + { passive, active border } {14}
                       #8#3#0#3            + { passive, active title  }
                       #11#8               + { 3D                     }
                       #3                  + { window background      }

{  - list viewer     } #0#7#14#7#15#1     + { normal, selected, focused text } {23}
                       #11#8               + { 3D                             }

{  - static text     } #0#3                +                                     {31}

{  - label           } #0#3#15#3           + { normal, selected text }           {33}
                       #12#14              + { normal, selected shortcut }

{  - input line      } #0#5#0#5#15#1    + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#1#7            + { normal, default                 46 }
                       #0#7#8#5            + { selected, disabled                 }
                       #4#14#12            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#7#15#7#8#7       + { normal, selected, disabled text } {59}
                       #4#14#1             + { normal, selected shortcut }

{  - history window  } #5#0               + { back, title            }          {67}
                       #15#8               + { 3D                     }
                       #0#5#11#1         + { normal, focused        }

{  - editor          } #0#15#7#1           + {normal, highlight       }    {75}

{  - file info pane  } #0#5              + { fore, back }                  {79}


{ ==================== gray window ================================== }

{  - frame           } #5#5                + { passive, active border } {14}
                       #8#5#0#5            + { passive, active title  }
                       #15#8               + { 3D                     }
                       #5                  + { window background      }

{  - list viewer     } #0#3#14#3#15#1     + { normal, selected, focused text } {23}
                       #15#8               + { 3D                             }

{  - static text     } #0#5                +                                     {31}

{  - label           } #0#5#15#5           + { normal, selected text }           {33}
                       #12#14              + { normal, selected shortcut }

{  - input line      } #0#7#14#1#15#8   + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#1#7        + { normal, default                 46 }
                       #0#7#8#5          + { selected, disabled                 }
                       #4#14#12            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#5#15#5#8#5       + { normal, selected, disabled text } {59}
                       #4#14#1             + { normal, selected shortcut }

{  - history window  } #5#0               + { back, title            }          {67}
                       #15#8               + { 3D                     }
                       #0#5#11#1         + { normal, focused        }

{  - editor          } #0#15#7#1         + {normal, highlight       }    {75}

{  - file info pane  } #11#1               ; { fore, back }                {79}

{$ELSE}
{═══════════════════════════════════════════════════════════════════════════}
  CAppColor =
{ desktop background } #7                  +

{ menu               } #0#7#8#7#4          + { normal   }  {2}
                       #0#11#8#11#4        + { selected }
                       #15#8               + { 3D       }

{ ==================== blue window ================================== }

{  - frame           } #5#3                + { passive, active border } {14}
                       #8#5#0#3            + { passive, active title  }
                       #11#8               + { 3D                     }
                       #5                  + { window background      }

{  - list viewer     } #0#6#8#11#15#3      + { normal, selected, focused text } {23}
                       #11#8               + { 3D                             }

{  - static text     } #0#5                +                                     {31}

{  - label           } #0#5#15#5           + { normal, selected text }           {33}
                       #4#14               + { normal, selected shortcut }

{  - input line      } #0#6#0#6#15#2       + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#14#7       + { normal, default                 46 }
                       #0#7#8#127        + { selected, disabled                 }
                       #4#15#15            + { normal, default, selected shortcut }
                       #11#8               + { 3D                     }

{  - cluster         } #0#7#12#7#8#7 + { normal, selected, disabled text } {59}
{GIO}                  #10#14#1            + { normal, selected shortcut }

{  - history window  } #6#0                + { back, title            }          {67}
                       #11#8               + { 3D                     }
                       #0#6#10#3           + { normal, focused        }

{  - editor          } #0#15#7#1         + {normal, highlight       }    {75}

{  - file info pane  } #0#5              + { fore, back }                      {79}

{ ==================== cyan window ================================== }

{  - frame           } #6#5                + { passive, active border } {14}
                       #8#6#0#5            + { passive, active title  }
                       #11#3               + { 3D                     }
                       #6                  + { window background      }

{  - list viewer     } #0#11#8#5#15#3     + { normal, selected, focused text } {23}
                       #5#3               + { 3D                             }

{  - static text     } #8#6                +                                     {31}

{  - label           } #8#6#0#6            + { normal, selected text }           {33}
                       #4#4                + { normal, selected shortcut }

{  - input line      } #0#5#0#5#15#1    + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#14#7       + { normal, default                 46 }
                       #0#7#8#7        + { selected, disabled                 }
                       #4#15#15            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#7#15#7#8#7 + { normal, selected, disabled text } {59}
                       #4#14#1             + { normal, selected shortcut }

{  - history window  } #85#0               + { back, title            }          {67}
                       #15#8               + { 3D                     }
                       #0#85#11#1         + { normal, focused        }

{  - editor          } #0#15#7#1         + {normal, highlight       }    {75}

{  - file info pane  } #0#5              + { fore, back }                      {79}


{ ==================== gray window ================================== }

{  - frame           } #7#7                + { passive, active border } {14}
                       #8#7#0#7            + { passive, active title  }
                       #15#8               + { 3D                     }
                       #7                  + { window background      }

{  - list viewer     } #0#7#0#5#15#8       + { normal, selected, focused text } {23}
                       #15#8               + { 3D                             }

{  - static text     } #0#7                +                                     {31}

{  - label           } #0#7#15#7           + { normal, selected text }           {33}
                       #4#12               + { normal, selected shortcut }

{  - input line      } #0#5#0#5#14#1 {GIO} + { passive, active, selected text }  {39}
                       #10                 + { arrow                          }

{  - button          } #0#7#14#7       + { normal, default                 46 }
                       #0#7#8#7         + { selected, disabled                 }
                       #4#15#15            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#5#15#5#8#5       + { normal, selected, disabled text } {59}
                       #15#15#1            + { normal, selected shortcut }

{  - history window  } #6#0                + { back, title            }          {67}
                       #10#3               + { 3D                     }
                       #0#6#0#11           + { normal, focused        }

{  - editor          } #0#5#15#8           + {normal, highlight       }    {75}

{  - file info pane  } #0#5              ; { fore, back }                      {79}
{$ENDIF}

  CAppMonochrome =
{ desktop background } #7                  +

{ menu               } #0#15#8#15#4        + { normal   }  {2}
                       #0#11#8#11#4        + { selected }
                       #15#8               + { 3D       }

{ ==================== blue window ================================== }

{  - frame           } #5#3                + { passive, active border } {14}
                       #8#5#0#15           + { passive, active title  }
                       #11#8               + { 3D                     }
                       #5                  + { window background      }

{  - list viewer     } #0#6#8#11#15#3      + { normal, selected, focused text } {23}
                       #11#8               + { 3D                             }

{  - static text     } #15#5                +                                     {31}

{  - label           } #0#5#15#5           + { normal, selected text }           {33}
                       #4#14               + { normal, selected shortcut }

{  - input line      } #0#6#0#6#15#2       + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#14#7       + { normal, default                 46 }
                       #0#7#8#127        + { selected, disabled                 }
                       #4#15#15            + { normal, default, selected shortcut }
                       #11#8               + { 3D                     }

{  - cluster         } #0#7#15#7#8#7 + { normal, selected, disabled text } {59}
{GIO}                  #10#14#1            + { normal, selected shortcut }

{  - history window  } #6#0                + { back, title            }          {67}
                       #11#8               + { 3D                     }
                       #0#6#10#3           + { normal, focused        }

{  - editor          } #0#15#7#1         + {normal, highlight       }    {75}

{  - file info pane  } #0#5              + { fore, back }                      {79}

{ ==================== cyan window ================================== }

{  - frame           } #6#5                + { passive, active border } {14}
                       #8#6#0#5            + { passive, active title  }
                       #11#3               + { 3D                     }
                       #6                  + { window background      }

{  - list viewer     } #0#11#8#5#15#3     + { normal, selected, focused text } {23}
                       #5#3               + { 3D                             }

{  - static text     } #8#6                +                                     {31}

{  - label           } #8#6#0#6            + { normal, selected text }           {33}
                       #4#4                + { normal, selected shortcut }

{  - input line      } #0#5#0#5#15#1    + { passive, active, selected text }  {39}
                       #11                 + { arrow                          }

{  - button          } #0#7#14#7       + { normal, default                 46 }
                       #0#7#8#7        + { selected, disabled                 }
                       #4#15#15            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#7#15#7#8#7 + { normal, selected, disabled text } {59}
                       #4#14#1             + { normal, selected shortcut }

{  - history window  } #85#0               + { back, title            }          {67}
                       #15#8               + { 3D                     }
                       #0#85#11#1         + { normal, focused        }

{  - editor          } #0#15#7#1         + {normal, highlight       }    {75}

{  - file info pane  } #0#5              + { fore, back }                      {79}


{ ==================== gray window ================================== }

{  - frame           } #15#7                + { passive, active border } {14}
                       #8#7#0#7            + { passive, active title  }
                       #15#8               + { 3D                     }
                       #7                  + { window background      }

{  - list viewer     } #0#15#15#8#15#0       + { normal, selected, focused text } {23}
                       #15#8               + { 3D                             }

{  - static text     } #0#7                +                                     {31}

{  - label           } #0#7#0#15           + { normal, selected text }           {33}
                       #4#0               + { normal, selected shortcut }

{  - input line      } #0#5#15#0#0#15 {GIO} + { passive, active, selected text }  {39}
                       #10                 + { arrow                          }

{  - button          } #0#7#14#7       + { normal, default                 46 }
                       #0#7#8#7         + { selected, disabled                 }
                       #4#15#15            + { normal, default, selected shortcut }
                       #15#8               + { 3D                     }

{  - cluster         } #0#14#15#0#8#5      + { normal, selected, disabled text } {59}
                       #15#15#1            + { normal, selected shortcut }

{  - history window  } #6#0                + { back, title            }          {67}
                       #10#3               + { 3D                     }
                       #0#6#0#11           + { normal, focused        }

{  - editor          } #0#5#15#8           + {normal, highlight       }    {75}

{  - file info pane  } #0#15              ; { fore, back }                      {79}


{ TBackground palette }

{                        ╔═══╗ }
{ CBackGround            ║ 1 ║ }
{                        ╚═╤═╝ }
{ Back ────────────────────┘   }

  CBackground = #1;

{ Standard application commands }

  cmNew       = 30;
  cmOpen      = 31;
  cmSave      = 32;
  cmSaveAs    = 33;
  cmSaveAll   = 34;
  cmChangeDir = 35;
  cmDosShell  = 36;
  cmCloseAll  = 37;

{ Standard application help contexts }

{ Note: range $FF00 - $FFFF of help contexts are reserved by Borland }

  hcNew          = $FF01;
  hcOpen         = $FF02;
  hcSave         = $FF03;
  hcSaveAs       = $FF04;
  hcSaveAll      = $FF05;
  hcChangeDir    = $FF06;
  hcDosShell     = $FF07;
  hcExit         = $FF08;

  hcUndo         = $FF10;
  hcCut          = $FF11;
  hcCopy         = $FF12;
  hcPaste        = $FF13;
  hcClear        = $FF14;

  hcTile         = $FF20;
  hcCascade      = $FF21;
  hcCloseAll     = $FF22;
  hcResize       = $FF23;
  hcZoom         = $FF24;
  hcNext         = $FF25;
  hcPrev         = $FF26;
  hcClose        = $FF27;

const
  {WallPaper flags}
  wpfPicture = 1; {if set, WallPaper points to a picture and WallPaperPalette to it's palette,
                    else WallPaper points to the pattern and WallPaperPalette contains 2 colors to draw}
  wpfSingle  = 2; {single picture is displayed}
  wpfStretch = 4; {stretches bitmap for entire screen, only if wpfSingle set}
  wpfVeil    = 8; {draw a veil over picture}


  WallPaper       :pointer = Nil;  {TONY}
  WallPaperPalette:pointer=Nil; {DK}
  WallPaperFlags  :Byte = wpfPicture;{DK}

type

{ TBackground object }

  PBackground = ^TBackground;
  TBackground = object(TView)
    Pattern: Char;
    Tile   : PImage;
    constructor Init(var Bounds: TRect; APattern: Char);
    constructor Load(var S: TStream);
    destructor Done; Virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure Store(var S: TStream);
    Function  UpdateColors : Boolean;   Virtual; {DK}
    Procedure UpdateWallpaper; {DK}
  end;

{ TDesktop object }

  PDesktop = ^TDesktop;
  TDesktop = object(TGroup)
    Background: PBackground;
    TileColumnsFirst: Boolean;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Cascade(var R: TRect);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitBackground; virtual;
    procedure Store(var S: TStream);
    procedure Tile(var R: TRect);
    procedure TileError; virtual;
  end;

{ TProgram object }

  { Palette layout }
  {     1 = TBackground }
  {  2- 7 = TMenuView and TStatusLine }
  {  8-15 = TWindow(Blue) }
  { 16-23 = TWindow(Cyan) }
  { 24-31 = TWindow(Gray) }
  { 32-63 = TDialog }

  PProgram = ^TProgram;
  TProgram = object(TGroup)
    constructor Init;
    destructor Done; virtual;
    function CanMoveFocus: Boolean;
    function ExecuteDialog(P: PDialog; Data: Pointer): Word;
    procedure GetEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Idle; virtual;
    procedure InitDesktop; virtual;
    procedure InitMenuBar; virtual;
    procedure InitScreen; virtual;
    procedure InitStatusLine; virtual;
    procedure InitScreenSaver; virtual; {TONY}
    function InsertWindow(P: PWindow): PWindow;
    procedure OutOfMemory; virtual;
    procedure PutEvent(var Event: TEvent); virtual;
    procedure Run; virtual;
    procedure SetScreenMode(Mode: Word);
    function ValidView(P: PView): PView;
    procedure StandardStatusRect(var R: TRect); {TONY}
    procedure StandardMenuBarRect(var R: TRect); {TONY}
  end;

{ TApplication object }

  PApplication = ^TApplication;
  TApplication = object(TProgram)
    constructor Init;
    destructor Done; virtual;
    procedure Cascade;
    procedure DosShell;
    procedure GetTileRect(var R: TRect); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Tile;
    procedure WriteShellMsg; virtual;
  end;

{ Standard menus and status lines }

function StdStatusKeys(Next: PStatusItem): PStatusItem;

function StdFileMenuItems(Next: PMenuItem): PMenuItem;
function StdEditMenuItems(Next: PMenuItem): PMenuItem;
function StdWindowMenuItems(Next: PMenuItem): PMenuItem;

{ App registration procedure }

procedure RegisterApp;

{ Wait mouse cursor }
Procedure SetMouseWait;

const

{ Public variables }

  Application: PProgram = nil;
  Desktop: PDesktop = nil;
  StatusLine: PStatusLine = nil;
  MenuBar: PMenuView = nil;
  AppPalette: Integer = apColor;
  ScreenSaver: PView = nil; {TONY}
  ReleaseTimeSlice: boolean = true; {TONY}

{ Stream registration records }

const
  RBackground: TStreamRec = (
    ObjType: 30;
    VmtLink: Ofs(TypeOf(TBackground)^);
    Load: @TBackground.Load;
    Store: @TBackground.Store);

const
  RDesktop: TStreamRec = (
    ObjType: 31;
    VmtLink: Ofs(TypeOf(TDesktop)^);
    Load: @TDesktop.Load;
    Store: @TDesktop.Store);

implementation

uses Dos, Palettes;

const

{ Private variables }

  Pending: TEvent = (What: evNothing);

{ TBackground }

constructor TBackground.Init(var Bounds: TRect; APattern: Char);
begin
  TView.Init(Bounds);
  GrowMode := gfGrowHiX + gfGrowHiY;
  Pattern := APattern;
  if WallPaper <> Nil then LogPalette.Mode := pmUseRGB; {DK}
  Tile := WallPaper;
  if WallPaperPalette <> Nil then begin
    if BitMapColorType(WallPaper) <= 256 then begin
      CreatePalette(WallpaperPalette, LogPalette, cbwMonoFix, DIBColors(WallPaper));
      LogPalette.Mode := pmUseRGB + pmOptimize + pmDirectBM;
      if (MaxColors = 256) and not IsImageStreamed(WallPaper) then Tile := CreateDIBitmap(WallPaper, 0) else Tile := Nil;
      if Tile = Nil then begin
        LogPalette.Mode := pmUseRGB + pmOptimize;
        Tile := WallPaper;
      end;
    end;
  end;
  PaintInfo.LineStyle := lsPattern;
end;

destructor TBackground.Done;
Begin
  if (LogPalette.Mode and pmDirectBM) <> 0 then FreeDImage(Tile);
  DisposeImage(GetImageID(PImage(Wallpaper)));
  Inherited Done;
End;

constructor TBackground.Load(var S: TStream);
begin
  TView.Load(S);
  S.Read(Pattern, SizeOf(Pattern));
end;

procedure TBackground.Draw {TONY};
var
  x, y: integer;
begin
  PaintInfo.Operation := CopyPut;
  if (WallPaperFlags and wpfPicture) = 0 then begin
    if WallPaper = Nil then Bar(0, 0, Size.X, Size.Y, GetColor($01))
    else begin
      Move(WallPaperPalette, PaintInfo.Fore, SizeOf(PaintInfo.Fore) * 2);
      Move(WallPaper^, PaintInfo.Pattern, SizeOf(PaintInfo.Pattern));
      BarStyle(0, 0, Size.X, Size.Y);
    end;
  end else begin
    if (Tile = Nil) or (BitMapHeight(Tile)=0) or (BitMapWidth(Tile)=0) then
      Bar(0, 0, Size.X, Size.Y, GetColor($01)) else begin
      if (WallPaperFlags and (wpfSingle + wpfStretch)) <> 0 then begin
        if (WallPaperFlags and wpfStretch) <> 0 then
          StretchBMP(Tile, 0, 0, Size.X, Size.Y)
        else begin
          Bar(0, 0, Size.X, Size.Y, 0);
          PutBMP(Tile, (ScreenWidth - BitMapWidth(Tile)) div 2, (ScreenHeight - BitMapHeight(Tile)) div 2);
        end;
      end else begin
        y := 0; x := 0;
        while y < Size.Y do begin
          while x < Size.X do begin
            PutBMP(Tile, x, y);
            Inc(x, BitMapWidth(Tile));
          end;
          Inc(y, BitMapHeight(Tile));
          x := 0;
        end;
      end;
    end;
  end;

  if (WallPaperFlags and wpfVeil) <> 0 then begin
    Move(FillPatterns[fsSimpleDots], PaintInfo.Pattern, 8);
    PaintInfo.Fore := $FFFF;
    PaintInfo.Back := 0;
    PaintInfo.Operation := AndPut;
    BarStyle(0, 0, Size.X, Size.Y);
  end;
end;

function TBackground.GetPalette: PPalette;
const
  P: string[Length(CBackground)] = CBackground;
begin
  GetPalette := @P;
end;

procedure TBackground.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(Pattern, SizeOf(Pattern));
end;

Function TBackGround.UpdateColors;
Begin
  if MaxColors <> 256 then Exit;
  if (LogPalette.Mode and pmDirectBM) <> 0 then begin
    CopyDIBits(WallPaper, Tile, 0);
    RemapImage(Tile, LogPalette.ColorRef);
  end;
  UpdateColors := False;
  DrawView;
  Message(Desktop, evBroadcast, cmRedrawIcons, Nil);
End;

Procedure TBackground.UpdateWallpaper;
Begin
  DisposePalette(LogPalette);
  if (LogPalette.Mode and pmDirectBM) <> 0 then FreeDImage(Tile);
  if WallPaperPalette <> Nil then begin
    CreatePalette(WallPaperPalette, LogPalette, 0, 256);
    LogPalette.Mode := LogPalette.Mode or (pmOptimize + pmDirectBM);
    Tile := Nil;
    if MaxColors = 256 then Tile := CreateDIBitmap(WallPaper, 0);
    if Tile = Nil then begin
      Tile := WallPaper;
      LogPalette.Mode := LogPalette.Mode and not pmDirectBM;
    end;
  end else begin
    LogPalette.Mode := LogPalette.Mode and not(pmOptimize + pmDirectBM);
    Tile := WallPaper;
  end;
  if MaxColors = 256 then RealizePalette else begin
    DrawView;
    Message(Desktop, evBroadcast, cmRedrawIcons, Nil);
  end;
End;

{ TDesktop object }

constructor TDesktop.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  GrowMode := gfGrowHiX + gfGrowHiY;
  InitBackground;
  if Background <> nil then Insert(Background);
end;

constructor TDesktop.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S, Background);
  S.Read(TileColumnsFirst, SizeOf(TileColumnsFirst));
end;

function Tileable(P: PView): Boolean;
begin
  Tileable := (P^.Options and ofTileable <> 0) and
    (P^.State and sfVisible <> 0);
end;

procedure TDesktop.Cascade(var R: TRect);
var
  CascadeNum: Integer;
  LastView: PView;
  Min, Max: TPoint;

procedure DoCount(P: PView); far;
begin
  if Tileable(P) then
  begin
    Inc(CascadeNum, P^.GetFrameWidth + P^.GetCaptureHeight {OOA});
    LastView := P;
  end;
end;

procedure DoCascade(P: PView); far;
var
  NR: TRect;
begin
  if Tileable(P) and (CascadeNum >= 0) then
  begin
    NR.Copy(R);
    Inc(NR.A.X, CascadeNum); Inc(NR.A.Y, CascadeNum);
    P^.Locate(NR);
    Dec(CascadeNum, P^.GetFrameWidth + P^.GetCaptureHeight {OOA});
  end;
end;

begin
  CascadeNum := 0;
  ForEach(@DoCount);
  if CascadeNum > 0 then
  begin
    LastView^.SizeLimits(Min, Max);
    if (Min.X > R.B.X - R.A.X - CascadeNum) or
       (Min.Y > R.B.Y - R.A.Y - CascadeNum) then TileError
    else
    begin
      Dec(CascadeNum, LastView^.GetFrameWidth + LastView^.GetCaptureHeight);
      Lock;
      ForEach(@DoCascade);
      Unlock;
    end;
  end;
end;

procedure TDesktop.HandleEvent(var Event: TEvent);
begin
  TGroup.HandleEvent(Event);
  if Event.What = evCommand then
  begin
    case Event.Command of
      cmNext: FocusNext(False);
      cmPrev:
        if Valid(cmReleasedFocus) then
          Current^.PutInFrontOf(Background);
    else
      Exit;
    end;
    ClearEvent(Event);
  end;
end;

procedure TDesktop.InitBackground;
var
  R: TRect;
begin
  GetExtent(R);
  New(Background, Init(R, #176));
end;

function ISqr(X: Integer): Integer; assembler;
asm
        MOV     CX,X
        MOV     BX,0
@@1:    INC     BX
        MOV     AX,BX
        IMUL    AX
        CMP     AX,CX
        JLE     @@1
        MOV     AX,BX
        DEC     AX
end;

procedure MostEqualDivisors(N: Integer; var X, Y: Integer; FavorY: Boolean);
var
  I: Integer;
begin
  I := ISqr(N);
  if ((N mod I) <> 0) then
    if (N mod (I+1)) = 0 then Inc(I);
  if I < (N div I) then I := N div I;
  if FavorY then
  begin
    X := N div I;
    Y := I;
  end
  else
  begin
    Y := N div I;
    X := I;
  end;
end;

procedure TDesktop.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S, Background);
  S.Write(TileColumnsFirst, SizeOf(TileColumnsFirst));
end;

procedure TDesktop.Tile(var R: TRect);
var
  NumCols, NumRows, NumTileable, LeftOver, TileNum: Integer;

procedure DoCountTileable(P: PView); far;
begin
  if Tileable(P) then Inc(NumTileable);
end;

function DividerLoc(Lo, Hi, Num, Pos: Integer): Integer;
begin
  DividerLoc := LongDiv(LongMul(Hi - Lo, Pos), Num) + Lo;
end;

procedure CalcTileRect(Pos: Integer; var NR: TRect);
var
  X,Y,D: Integer;
begin
  D := (NumCols - LeftOver) * NumRows;
  if Pos < D then
  begin
    X := Pos div NumRows;
    Y := Pos mod NumRows;
  end else
  begin
    X := (Pos - D) div (NumRows + 1) + (NumCols - LeftOver);
    Y := (Pos - D) mod (NumRows + 1);
  end;
  NR.A.X := DividerLoc(R.A.X, R.B.X, NumCols, X);
  NR.B.X := DividerLoc(R.A.X, R.B.X, NumCols, X+1);
  if Pos >= D then
  begin
    NR.A.Y := DividerLoc(R.A.Y, R.B.Y, NumRows+1, Y);
    NR.B.Y := DividerLoc(R.A.Y, R.B.Y, NumRows+1, Y+1);
  end else
  begin
    NR.A.Y := DividerLoc(R.A.Y, R.B.Y, NumRows, Y);
    NR.B.Y := DividerLoc(R.A.Y, R.B.Y, NumRows, Y+1);
  end;
end;

procedure DoTile(P: PView); far;
var
  R: TRect;
begin
  if Tileable(P) then
  begin
    CalcTileRect(TileNum, R);
    P^.Locate(R);
    Dec(TileNum);
  end;
end;

begin
  NumTileable := 0;
  ForEach(@DoCountTileable);
  if NumTileable > 0 then
  begin
    MostEqualDivisors(NumTileable, NumCols, NumRows, not TileColumnsFirst);
    if ((R.B.X - R.A.X) div NumCols = 0) or
       ((R.B.Y - R.A.Y) div NumRows = 0) then TileError
    else
    begin
      LeftOver := NumTileable mod NumCols;
      TileNum := NumTileable-1;
      Lock;
      ForEach(@DoTile);
      Unlock;
    end;
  end;
end;

procedure TDesktop.TileError;
begin
end;

{ TProgram }

constructor TProgram.Init;
var
  R: TRect;
  fnt : PSimpleFont {TONY};
begin
{  New(fnt, Load('font.fnt'));}
  New(fnt, Init((@DefaultSystemFont)^, 'System'));
  RegisterFont(fnt, 1);
  SelectFont(GlobalFont.Font);
  Application := @Self;
  InitScreen;
  R.Assign(0, 0, ScreenWidth, ScreenHeight);
  TGroup.Init(R);
  State := sfVisible + sfSelected + sfFocused + sfModal + sfExposed;
  Options := 0;
  InitDesktop;
  InitStatusLine;
  InitMenuBar;
  InitScreenSaver;
  if Desktop <> nil then Insert(Desktop);
  if StatusLine <> nil then Insert(StatusLine);
  if MenuBar <> nil then Insert(MenuBar);
  if ScreenSaver <> nil then Insert(ScreenSaver);

  InitLPM;
end;

destructor TProgram.Done;
begin
  DoneLPM;
  if Desktop <> nil then Dispose(Desktop, Done);
  if MenuBar <> nil then Dispose(MenuBar, Done);
  if StatusLine <> nil then Dispose(StatusLine, Done);
  Application := nil;
  inherited Done;
end;

function TProgram.CanMoveFocus: Boolean;
begin
  CanMoveFocus := Desktop^.Valid(cmReleasedFocus);
end;

function TProgram.ExecuteDialog(P: PDialog; Data: Pointer): Word;
var
  C: Word;
begin
  ExecuteDialog := cmCancel;
  if ValidView(P) <> nil then
  begin
    if Data <> nil then P^.SetData(Data^);
    C := Desktop^.ExecView(P);
    if (C <> cmCancel) and (Data <> nil) then P^.GetData(Data^);
    Dispose(P, Done);
    ExecuteDialog := C;
  end;
end;

procedure TProgram.GetEvent(var Event: TEvent);
var
  R: TRect;

function ContainsMouse(P: PView): Boolean; far;
begin
  ContainsMouse := (P^.State and sfVisible <> 0) and
    P^.MouseInView(Event.Where);
end;

begin
  if Pending.What <> evNothing then
  begin
    Event := Pending;
    Pending.What := evNothing;
  end else
  begin
    GetMouseEvent(Event);
    if Event.What = evNothing then
    begin
      GetKeyEvent(Event);
      if Event.What = evNothing then
      begin
        HoldMouse(True);
        TextCursor.Blink; {TONY}
        HoldMouse(False);
        if LPMCanBeSent and ReleaseTimeSlice then begin
          asm   {TONY}
            mov    ax, 1680h  { Idle reporting to multitasking environment }
            int    2Fh
          end;
        end;
        Idle;
      end;
    end else if (Event.What = evMouseMove) then MouseMastering(Event); {TONY}
  end;
  if ScreenSaver <> Nil then ScreenSaver^.HandleEvent(Event);
  if StatusLine <> nil then
    if (Event.What and evKeyDown <> 0) or
      (Event.What and evMouseDown <> 0) and
      (FirstThat(@ContainsMouse) = PView(StatusLine)) then
      StatusLine^.HandleEvent(Event);
end;

function TProgram.GetPalette: PPalette;
const
  P: array[apColor..apMonochrome] of string[Length(CAppColor)] =
    (CAppColor, CAppMonochrome);

begin
  GetPalette := @P[AppPalette];
end;

procedure TProgram.HandleEvent(var Event: TEvent);
var
  I: Word;
  C: Char;
begin
  if Event.What = evKeyDown then
  begin
    C := GetAltChar(Event.KeyCode);
    if (C >= '1') and (C <= '9') then
      if Message(Desktop, evBroadCast, cmSelectWindowNum,
        Pointer(Byte(C) - $30)) <> nil then ClearEvent(Event);
  end;
  TGroup.HandleEvent(Event);
  if Event.What = evCommand then
    case Event.Command of
    cmQuit : begin
      EndModal(cmQuit);
      ClearEvent(Event);
    end;
    cmResetMouseCursor : SetMouseCursorShape(@DefaultMouseCursor); {DK/GIO}
    end;
end;

procedure TProgram.Idle;
begin
  if StatusLine <> nil then StatusLine^.Update;
  if CommandSetChanged then
  begin
    Message(@Self, evBroadcast, cmCommandSetChanged, nil);
    CommandSetChanged := False;
  end;
end;

procedure TProgram.InitDesktop; {TONY}
var
  R, R1: TRect;
begin
  GetExtent(R);
  StandardMenuBarRect(R1);
  R.A.Y := R1.B.Y;
  StandardStatusRect(R1);
  R.B.Y := R1.A.Y;
  New(Desktop, Init(R));
end;

procedure TProgram.InitMenuBar;
var
  R: TRect;
begin
  StandardMenuBarRect(R);
  MenuBar := New(PMenuBar, Init(R, nil));
end;

procedure TProgram.InitScreen {TONY};
begin
  ShowMarkers := False;
  if MaxColors = 2 then AppPalette := apMonochrome else AppPalette := apColor;
end;

procedure TProgram.InitStatusLine;
var
  R: TRect;
begin
  StandardStatusRect(R);
{$IFDEF Ukrainian}
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Выход', kbAltX, cmQuit,
      StdStatusKeys(nil)), nil)));
{$ELSE}
  {$IFDEF Russian}
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Выход', kbAltX, cmQuit,
      StdStatusKeys(nil)), nil)));
  {$ELSE}
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      StdStatusKeys(nil)), nil)));
  {$ENDIF}
{$ENDIF}
end;

procedure TProgram.InitScreenSaver; {TONY}
begin
  ScreenSaver := Nil;
end;

function TProgram.InsertWindow(P: PWindow): PWindow;
var
  OldCurr: PView; {TONY}
begin
  InsertWindow := nil;
  if ValidView(P) <> nil then
    if CanMoveFocus then
    begin
      OldCurr := Desktop^.Current; {TONY}
      if (OldCurr <> Nil) and (OldCurr^.Options and ofBuffered <> 0) then
        PGroup(OldCurr)^.Lock; {TONY}
      P^.Lock; {TONY}
      Desktop^.Insert(P);
      InsertWindow := P;
      P^.UnLock; {TONY}
      if (OldCurr <> Nil) and (OldCurr^.Options and ofBuffered <> 0) then
        PGroup(OldCurr)^.UnLock; {TONY}
    end
    else
      Dispose(P, Done);
end;

procedure TProgram.OutOfMemory;
begin
end;

procedure TProgram.PutEvent(var Event: TEvent);
begin
  Pending := Event;
end;

procedure TProgram.Run;
begin
  Execute;
end;

procedure TProgram.SetScreenMode(Mode: Word);
var
  R: TRect;
begin
  HideMouse;
  DoneMemory;
  InitMemory;
  InitScreen;
  R.Assign(0, 0, ScreenWidth, ScreenHeight);
  ChangeBounds(R);
  ShowMouse;
end;

function TProgram.ValidView(P: PView): PView;
begin
  ValidView := nil;
  if P <> nil then
  begin
    if LowMemory then
    begin
      Dispose(P, Done);
      OutOfMemory;
      Exit;
    end;
    if not P^.Valid(cmValid) then
    begin
      Dispose(P, Done);
      Exit;
    end;
    ValidView := P;
  end;
end;

procedure TProgram.StandardStatusRect(var R: TRect); {TONY}
begin
  GetExtent(R);
  SetFont(StatusFont);
  R.A.Y := R.B.Y - GetHeight - 8;
  RestoreFont;
end;

procedure TProgram.StandardMenuBarRect(var R: TRect); {TONY}
begin
  GetExtent(R);
  SetFont(MenusFont);
  R.B.Y := R.A.Y + GetHeight + 8;
  RestoreFont;
end;


{ TApplication }
{$S-}
Procedure CloseApp; Far;
begin
  if Application <> Nil then begin
    Message(Application, evCommand, cmQuit, nil);
    Application^.Done;
  end;
end;

{$S+}


constructor TApplication.Init;
begin
  {$IFDEF DPMI}
  TrapProc := ProtectedTrapProc;
  {$ENDIF}
  InitMemory;
  InitVideo;
  InitEvents;
  InitSysError;
  InitHistory;
  TProgram.Init;
  SystemBeep(sbEntry);
  {$IFDEF DPMI}
  Shutdown := CloseApp;
  {$ENDIF}
end;

destructor TApplication.Done;
begin
  {$IFDEF DPMI}
  Shutdown := Nil;
  {$ENDIF}
  SystemBeep(sbExit);
  TProgram.Done;
  DoneHistory;
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneMemory;
end;

procedure TApplication.Cascade;
var
  R: TRect;
begin
  GetTileRect(R);
  if Desktop <> nil then Desktop^.Cascade(R);
end;

procedure TApplication.DosShell;
begin
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneDosMem;
  WriteShellMsg;
  SwapVectors;
  Exec(GetEnv('COMSPEC'), '');
  SwapVectors;
  InitDosMem;
  InitVideo;
  InitEvents;
  InitSysError;
  Redraw;
  RedrawPalette;
end;

procedure TApplication.GetTileRect(var R: TRect);
begin
  Desktop^.GetExtent(R);
end;

procedure TApplication.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmTile: Tile;
          cmCascade: Cascade;
          cmDosShell: DosShell;
        else
          Exit;
        end;
        ClearEvent(Event);
      end;
  end;
end;

procedure TApplication.Tile;
var
  R: TRect;
begin
  GetTileRect(R);
  if Desktop <> nil then Desktop^.Tile(R);
end;

procedure TApplication.WriteShellMsg;
begin
{$IFDEF Ukrainian}
  PrintStr('Наберите EXIT для возврата...'^M^J);
{$ELSE}
  {$IFDEF Russian}
  PrintStr('Наберите EXIT для возврата...'^M^J);
  {$ELSE}
  PrintStr('Type EXIT to return...'^M^J);
  {$ENDIF}
{$ENDIF}
end;

{ App registration procedure }

procedure RegisterApp;
begin
  RegisterType(RBackground);
  RegisterType(RDesktop);
end;

{ Standard menus and status lines }

function StdStatusKeys(Next: PStatusItem): PStatusItem;
begin
  StdStatusKeys :=
    NewStatusKey('', kbAltX, cmQuit,
    NewStatusKey('', kbF10, cmMenu,
    NewStatusKey('', kbAltF3, cmClose,
    NewStatusKey('', kbF5, cmZoom,
    NewStatusKey('', kbCtrlF5, cmResize,
    NewStatusKey('', kbF6, cmNext,
    NewStatusKey('', kbShiftF6, cmPrev,
    Next)))))));
end;

function StdFileMenuItems(Next: PMenuItem): PMenuItem;
begin
{$IFDEF Ukrainian}
  StdFileMenuItems :=
    NewItem('~Н~овый', '', kbNoKey, cmNew, hcNew,
    NewItem('~О~ткрыть...', 'F3', kbF3, cmOpen, hcOpen,
    NewItem('~С~охранить', 'F2', kbF2, cmSave, hcSave,
    NewItem('~П~ереименовать...', '', kbNoKey, cmSaveAs, hcSaveAs,
    NewItem('Сохранить ~в~се', '', kbNoKey, cmSaveAll, hcSaveAll,
    NewLine(
    NewItem('Сменить ~д~иректорий...', '', kbNoKey, cmChangeDir, hcChangeDir,
    NewItem('В~р~еменный выход', '', kbNoKey, cmDosShell, hcDosShell,
    NewItem('В~ы~ход', 'Alt+X', kbAltX, cmQuit, hcExit,
    Next)))))))));
{$ELSE}
  {$IFDEF Russian}
  StdFileMenuItems :=
    NewItem('~Н~овый', '', kbNoKey, cmNew, hcNew,
    NewItem('~О~ткрыть...', 'F3', kbF3, cmOpen, hcOpen,
    NewItem('~С~охранить', 'F2', kbF2, cmSave, hcSave,
    NewItem('~П~ереименовать...', '', kbNoKey, cmSaveAs, hcSaveAs,
    NewItem('Сохранить ~в~се', '', kbNoKey, cmSaveAll, hcSaveAll,
    NewLine(
    NewItem('Сменить ~д~иректорий...', '', kbNoKey, cmChangeDir, hcChangeDir,
    NewItem('В~р~еменный выход', '', kbNoKey, cmDosShell, hcDosShell,
    NewItem('В~ы~ход', 'Alt+X', kbAltX, cmQuit, hcExit,
    Next)))))))));
  {$ELSE}
  StdFileMenuItems :=
    NewItem('~N~ew', '', kbNoKey, cmNew, hcNew,
    NewItem('~O~pen...', 'F3', kbF3, cmOpen, hcOpen,
    NewItem('~S~ave', 'F2', kbF2, cmSave, hcSave,
    NewItem('S~a~ve as...', '', kbNoKey, cmSaveAs, hcSaveAs,
    NewItem('Save a~l~l', '', kbNoKey, cmSaveAll, hcSaveAll,
    NewLine(
    NewItem('~C~hange dir...', '', kbNoKey, cmChangeDir, hcChangeDir,
    NewItem('~D~OS shell', '', kbNoKey, cmDosShell, hcDosShell,
    NewItem('E~x~it', 'Alt+X', kbAltX, cmQuit, hcExit,
    Next)))))))));
  {$ENDIF}
{$ENDIF}
end;

function StdEditMenuItems(Next: PMenuItem): PMenuItem;
begin
{$IFDEF Ukrainian}
  StdEditMenuItems :=
    NewItem('~О~ткат', '', kbAltBack, cmUndo, hcUndo,
    NewLine(
    NewItem('~В~ырезать', 'Shift+Del', kbShiftDel, cmCut, hcCut,
    NewItem('С~к~опировать', 'Ctrl+Ins', kbCtrlIns, cmCopy, hcCopy,
    NewItem('Во~с~становить', 'Shift+Ins', kbShiftIns, cmPaste, hcPaste,
    NewItem('О~ч~истить', 'Ctrl+Del', kbCtrlDel, cmClear, hcClear,
    Next))))));
{$ELSE}
  {$IFDEF Russian}
  StdEditMenuItems :=
    NewItem('~О~ткат', '', kbAltBack, cmUndo, hcUndo,
    NewLine(
    NewItem('~В~ырезать', 'Shift+Del', kbShiftDel, cmCut, hcCut,
    NewItem('С~к~опировать', 'Ctrl+Ins', kbCtrlIns, cmCopy, hcCopy,
    NewItem('Во~с~становить', 'Shift+Ins', kbShiftIns, cmPaste, hcPaste,
    NewItem('О~ч~истить', 'Ctrl+Del', kbCtrlDel, cmClear, hcClear,
    Next))))));
  {$ELSE}
  StdEditMenuItems :=
    NewItem('~U~ndo', '', kbAltBack, cmUndo, hcUndo,
    NewLine(
    NewItem('Cu~t~', 'Shift+Del', kbShiftDel, cmCut, hcCut,
    NewItem('~C~opy', 'Ctrl+Ins', kbCtrlIns, cmCopy, hcCopy,
    NewItem('~P~aste', 'Shift+Ins', kbShiftIns, cmPaste, hcPaste,
    NewItem('C~l~ear', 'Ctrl+Del', kbCtrlDel, cmClear, hcClear,
    Next))))));
  {$ENDIF}
{$ENDIF}
end;

function StdWindowMenuItems(Next: PMenuItem): PMenuItem;
begin
{$IFDEF Ukrainian}
  StdWindowMenuItems :=
    NewItem('Парке~т~', '', kbNoKey, cmTile, hcTile,
    NewItem('~К~аскад', '', kbNoKey, cmCascade, hcCascade,
    NewItem('~З~акрыть все', '', kbNoKey, cmCloseAll, hcCloseAll,
    NewLine(
    NewItem('~Р~азместить','Ctrl+F5', kbCtrlF5, cmResize, hcResize,
    NewItem('Р~а~змер', 'F5', kbF5, cmZoom, hcZoom,
    NewItem('~С~ледующее', 'F6', kbF6, cmNext, hcNext,
    NewItem('~П~редыдущее', 'Shift+F6', kbShiftF6, cmPrev, hcPrev,
    NewItem('~З~акрыть', 'Alt+F3', kbAltF3, cmClose, hcClose,
    Next)))))))));
{$ELSE}
  {$IFDEF Russian}
  StdWindowMenuItems :=
    NewItem('~П~аркет', '', kbNoKey, cmTile, hcTile,
    NewItem('~К~аскад', '', kbNoKey, cmCascade, hcCascade,
    NewItem('~З~акрыть все', '', kbNoKey, cmCloseAll, hcCloseAll,
    NewLine(
    NewItem('~Р~азместить','Ctrl+F5', kbCtrlF5, cmResize, hcResize,
    NewItem('~Р~азмер', 'F5', kbF5, cmZoom, hcZoom,
    NewItem('~С~ледующее', 'F6', kbF6, cmNext, hcNext,
    NewItem('~П~редыдущее', 'Shift+F6', kbShiftF6, cmPrev, hcPrev,
    NewItem('~З~акрыть', 'Alt+F3', kbAltF3, cmClose, hcClose,
    Next)))))))));
  {$ELSE}
  StdWindowMenuItems :=
    NewItem('~T~ile', '', kbNoKey, cmTile, hcTile,
    NewItem('C~a~scade', '', kbNoKey, cmCascade, hcCascade,
    NewItem('Cl~o~se all', '', kbNoKey, cmCloseAll, hcCloseAll,
    NewLine(
    NewItem('~S~ize/Move','Ctrl+F5', kbCtrlF5, cmResize, hcResize,
    NewItem('~Z~oom', 'F5', kbF5, cmZoom, hcZoom,
    NewItem('~N~ext', 'F6', kbF6, cmNext, hcNext,
    NewItem('~P~revious', 'Shift+F6', kbShiftF6, cmPrev, hcPrev,
    NewItem('~C~lose', 'Alt+F3', kbAltF3, cmClose, hcClose,
    Next)))))))));
  {$ENDIF}
{$ENDIF}
end;

Procedure SetMouseWait; {DK/GIO}
Begin
  SetMouseCursorShape(@ClockMouseCursor);
  LPMessage(0, Application, evCommand, cmResetMouseCursor, Nil);
End;

Var
  SaveNotify : TSysNotifyProc;

Procedure AppNotifyProc(EventWhat, EventCommand : Word; EventInfo : Pointer); Far;
Begin
  SaveNotify(EventWhat, EventCommand, EventInfo);
  if (EventWhat = snRedraw) and (Application <> Nil) then Application^.Redraw;
End;

begin
  SaveNotify    := SysNotifyProc;
  SysNotifyProc := AppNotifyProc;
end.
