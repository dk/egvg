
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit Menus;

{$O+,F+,X+,I-,S-}

interface

uses Objects, Drivers, Views, EgInline{TONY}, GDI{DK};

const

{ Color palettes }

{                ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗ }
{ CMenuView      ║ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9 │ A │ B │ C ║ }
{                ╚═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╧═╤═╝ }
{ Text Normal fore ┘   │   │   │   │   │   │   │   │   │   │   │   }
{             back ────┘   │   │   │   │   │   │   │   │   │   │   }
{ Text Disabled fore ──────┘   │   │   │   │   │   │   │   │   │   }
{               back ──────────┘   │   │   │   │   │   │   │   │   }
{ Text Shortcut fore ──────────────┘   │   │   │   │   │   │   │   }
{ Selected Normal fore ────────────────┘   │   │   │   │   │   │   }
{                 back ────────────────────┘   │   │   │   │   │   }
{ Selected Disabled fore ──────────────────────┘   │   │   │   │   }
{                   back ──────────────────────────┘   │   │   │   }
{ Selected Shortcut fore ──────────────────────────────┘   │   │   }
{ 3D  bright ──────────────────────────────────────────────┘   │   }
{       dark ──────────────────────────────────────────────────┘   }

  CMenuView   = #2#3#4#5#6#7#8#9#10#11#12#13;
  CStatusLine = #2#3#4#5#6#7#8#9#10#11#12#13;

Const {TONY}
  ssfTimer         = $01;  { Activate on time }
  ssfMouseReset    = $02;  { Activate when mouse in upper right corner }
  ssfMousePreserve = $04;  { Don't activate when mouse in upper left }
  ssfAllOptions    = $07;

type

{ TMenu types }

  TMenuStr = string[31];

  PMenu = ^TMenu;

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    Next: PMenuItem;
    Name: PString;
    Command: Word;
    Disabled: Boolean;
    KeyCode: Word;
    HelpCtx: Word;
    case Integer of
      0: (Param: PString);
      1: (SubMenu: PMenu);
  end;

  TMenu = record
    Items: PMenuItem;
    Default: PMenuItem;
  end;

{ TMenuView object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuView = ^TMenuView;
  TMenuView = object(TView)
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    function Execute: Word; virtual;
    function FindItem(Ch: Char): PMenuItem;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function HotKey(KeyCode: Word): PMenuItem;
    function NewSubView(var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView): PMenuView; virtual;
    procedure Store(var S: TStream);
   private
    IsBox: Boolean;
    procedure Invalidate(Item: PMenuItem); virtual;
  end;

{ TMenuBar object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuBar = ^TMenuBar;
  TMenuBar = object(TMenuView)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
  private
    procedure Invalidate(Item: PMenuItem); virtual;
  end;

{ TMenuBox object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuBox = ^TMenuBox;
  TMenuBox = object(TMenuView)
    constructor Init(var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView);
    procedure Draw; virtual;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
  private
    procedure Invalidate(Item: PMenuItem); virtual;
  end;

{ TMenuPopup object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = object(TMenuBox)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

{ TStatusItem }

  PStatusItem = ^TStatusItem;
  TStatusItem = record
    Next: PStatusItem;
    Text: PString;
    KeyCode: Word;
    Command: Word;
  end;

{ TStatusDef }

  PStatusDef = ^TStatusDef;
  TStatusDef = record
    Next: PStatusDef;
    Min, Max: Word;
    Items: PStatusItem;
  end;

{ TStatusLine }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PStatusLine = ^TStatusLine;
  TStatusLine = object(TView)
    Items: PStatusItem;
    Defs: PStatusDef;
    constructor Init(var Bounds: TRect; ADefs: PStatusDef);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Hint(AHelpCtx: Word): String; virtual;
    procedure Store(var S: TStream);
    procedure Update; virtual;
  private
    Selected: PStatusItem;
    procedure FindItems;
  end;

{ TScreenSaver }
  PScreenSaver = ^TScreenSaver;  {TONY}
  TScreenSaver = object(TView)
    LastTime: Longint;
    Flags: Byte;
    Time: Longint;
    InCanLeave: Boolean;
    constructor Init(var Bounds: TRect; AFlags: Byte; ATime: Word);
    destructor  Done; virtual;
    procedure   HandleEvent(var Event: TEvent); virtual;
    procedure   ActionOnSave(JustActivated: Boolean); virtual;
    procedure   Draw; virtual;
    function    CanLeave: boolean; virtual;
  end;

{ TMenuItem routines }

function NewItem(Name, Param: TMenuStr; KeyCode: Word; Command: Word;
  AHelpCtx: Word; Next: PMenuItem): PMenuItem;
function NewLine(Next: PMenuItem): PMenuItem;
function NewSubMenu(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
  Next: PMenuItem): PMenuItem;

{ TMenu routines }

function NewMenu(Items: PMenuItem): PMenu;
procedure DisposeMenu(Menu: PMenu);

{ TStatusLine routines }

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
  ANext: PStatusDef): PStatusDef;
function NewStatusKey(const AText: String; AKeyCode: Word; ACommand: Word;
  ANext: PStatusItem): PStatusItem;

{ Menus registration procedure }

procedure RegisterMenus;

{ Stream registration records }

const
  RMenuBar: TStreamRec = (
     ObjType: 40;
     VmtLink: Ofs(TypeOf(TMenuBar)^);
     Load:    @TMenuBar.Load;
     Store:   @TMenuBar.Store
  );

const
  RMenuBox: TStreamRec = (
     ObjType: 41;
     VmtLink: Ofs(TypeOf(TMenuBox)^);
     Load:    @TMenuBox.Load;
     Store:   @TMenuBox.Store
  );

const
  RStatusLine: TStreamRec = (
     ObjType: 42;
     VmtLink: Ofs(TypeOf(TStatusLine)^);
     Load:    @TStatusLine.Load;
     Store:   @TStatusLine.Store
  );

const
  RMenuPopup: TStreamRec = (
     ObjType: 43;
     VmtLink: Ofs(TypeOf(TMenuPopup)^);
     Load:    @TMenuPopup.Load;
     Store:   @TMenuPopup.Store
  );


implementation

{ TMenuItem routines }

function NewItem(Name, Param: TMenuStr; KeyCode: Word; Command: Word;
  AHelpCtx: Word; Next: PMenuItem): PMenuItem;
const
  T: PView = nil;
var
  P: PMenuItem;
begin
  if (Name <> '') and (Command <> 0) then
  begin
    New(P);
    P^.Next := Next;
    P^.Name := NewStr(Name);
    P^.Command := Command;
    P^.Disabled := not T^.CommandEnabled(Command);
    P^.KeyCode := KeyCode;
    P^.HelpCtx := AHelpCtx;
    P^.Param := NewStr(Param);
    NewItem := P;
  end else
  NewItem := Next;
end;

function NewLine(Next: PMenuItem): PMenuItem;
var
  P: PMenuItem;
begin
  New(P);
  P^.Next := Next;
  P^.Name := nil;
  P^.HelpCtx := hcNoContext;
  NewLine := P;
end;

function NewSubMenu(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
  Next: PMenuItem): PMenuItem;
var
  P: PMenuItem;
begin
  if (Name <> '') and (SubMenu <> nil) then
  begin
    New(P);
    P^.Next := Next;
    P^.Name := NewStr(Name);
    P^.Command := 0;
    P^.Disabled := False;
    P^.HelpCtx := AHelpCtx;
    P^.SubMenu := SubMenu;
    NewSubMenu := P;
  end else
  NewSubMenu := Next;
end;

{ TMenu routines }

function NewMenu(Items: PMenuItem): PMenu;
var
  P: PMenu;
begin
  New(P);
  P^.Items := Items;
  P^.Default := Items;
  NewMenu := P;
end;

procedure DisposeMenu(Menu: PMenu);
var
  P, Q: PMenuItem;
begin
  if Menu <> nil then
  begin
    P := Menu^.Items;
    while P <> nil do
    begin
      if P^.Name <> nil then
      begin
        DisposeStr(P^.Name);
        if P^.Command <> 0 then
          DisposeStr(P^.Param) else
          DisposeMenu(P^.SubMenu);
      end;
      Q := P;
      P := P^.Next;
      Dispose(Q);
    end;
    Dispose(Menu);
  end;
end;

{ TMenuView }

constructor TMenuView.Init(var Bounds: TRect);
begin
  inherited Init(Bounds) {TONY};
  Font := MenusFont; {TONY}
  EventMask := EventMask or evBroadcast;
end;

constructor TMenuView.Load(var S: TStream);

function DoLoadMenu: PMenu;
var
  Item: PMenuItem;
  Last: ^PMenuItem;
  Menu: PMenu;
  Tok: Byte;
begin
  New(Menu);
  Last := @Menu^.Items;
  Item := nil;
  S.Read(Tok,1);
  while Tok <> 0 do
  begin
    New(Item);
    Last^ := Item;
    Last := @Item^.Next;
    with Item^ do
    begin
      Name := S.ReadStr;
      S.Read(Command, SizeOf(Word) * 3 + SizeOf(Boolean));
      if (Name <> nil) then
        if Command = 0 then SubMenu := DoLoadMenu
        else Param := S.ReadStr;
    end;
    S.Read(Tok, 1);
  end;
  Last^ := nil;
  Menu^.Default := Menu^.Items;
  DoLoadMenu := Menu;
end;

begin
  TView.Load(S);
  Menu := DoLoadMenu;
end;

function TMenuView.Execute: Word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
begin
  MakeLocal(E.Where, Mouse);
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until Current^.Name <> nil;
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (not ParentMenu^.IsBox {TONY}) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and not P^.MouseInView(E.Where) do P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          {TONY} if State and sfVisible = 0 then begin
            Current := Nil;
            SetState(sfVisible, True);
            Current := Menu^.Default;
          end;
          TrackMouse;
          if (not IsBox) {TONY} then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          {TONY} if State and sfVisible = 0 then begin
            Current := Nil;
            SetState(sfVisible, True);
            Current := Menu^.Default;
          end;
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if (IsBox) {TONY} then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if (IsBox) {TONY} then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if (not IsBox) {TONY} then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.IsBox {TONY} ) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if (not IsBox) {TONY} then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      Invalidate(ItemShown);
      ItemShown := Current;
      {TONY} if State and sfVisible = 0 then SetState(sfVisible, True) else
      DrawInvalidated;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          {TONY} Target^.Hide;
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    {TONY} if ParentMenu <> nil then Hide else
    DrawView;
  end;
  Execute := Result;
end;

function TMenuView.FindItem(Ch: Char): PMenuItem;
var
  P: PMenuItem;
  I: Integer;
begin
  Ch := UpCase(Ch);
  P := Menu^.Items;
  while P <> nil do
  begin
    if (P^.Name <> nil) and not P^.Disabled then
    begin
      I := Pos('~', P^.Name^);
      if (I <> 0) and (Ch = UpCase(P^.Name^[I + 1])) then
      begin
        FindItem := P;
        Exit;
      end;
    end;
    P := P^.Next;
  end;
  FindItem := nil;
end;

procedure TMenuView.GetItemRect(Item: PMenuItem; var R: TRect);
begin
end;

function TMenuView.GetHelpCtx: Word;
var
  C: PMenuView;
begin
  C := @Self;
  while (C <> nil) and
     ((C^.Current = nil) or (C^.Current^.HelpCtx = hcNoContext) or
      (C^.Current^.Name = nil)) do
    C := C^.ParentMenu;
  if C <> nil then GetHelpCtx := C^.Current^.HelpCtx
  else GetHelpCtx := hcNoContext;
end;

function TMenuView.GetPalette: PPalette;
const
  P: string[Length(CMenuView)] = CMenuView;
begin
  GetPalette := @P;
end;

procedure TMenuView.HandleEvent(var Event: TEvent);
var
  CallDraw: Boolean;
  P: PMenuItem;

procedure UpdateMenu(Menu: PMenu);
var
  P: PMenuItem;
  CommandState: Boolean;
begin
  P := Menu^.Items;
  while P <> nil do
  begin
    if P^.Name <> nil then
      if P^.Command = 0 then UpdateMenu(P^.SubMenu)
      else
      begin
        CommandState := CommandEnabled(P^.Command);
        if P^.Disabled = CommandState then
        begin
          P^.Disabled := not CommandState;
          CallDraw := True;
        end;
      end;
    P := P^.Next;
  end;
end;

procedure DoSelect;
begin
  PutEvent(Event);
  Event.Command := Owner^.ExecView(@Self);
  if (Event.Command <> 0) and CommandEnabled(Event.Command) then
  begin
    Event.What := evCommand;
    Event.InfoPtr := nil;
    PutEvent(Event);
  end;
  ClearEvent(Event);
end;

begin
  if Menu <> nil then
    case Event.What of
      evMouseDown:
        DoSelect;
      evKeyDown:
        if (FindItem(GetAltChar(Event.KeyCode)) <> nil) then
          DoSelect
        else
        begin
          P := HotKey(Event.KeyCode);
          if (P <> nil) and (CommandEnabled(P^.Command)) then
          begin
            Event.What := evCommand;
            Event.Command := P^.Command;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
          end;
        end;
      evCommand:
        if Event.Command = cmMenu then DoSelect;
      evBroadcast:
        if Event.Command = cmCommandSetChanged then
        begin
          CallDraw := False;
          UpdateMenu(Menu);
          if CallDraw then DrawView;
        end;
    end;
end;

function TMenuView.HotKey(KeyCode: Word): PMenuItem;

function FindHotKey(P: PMenuItem): PMenuItem;
var
  T: PMenuItem;
begin
  while P <> nil do
  begin
    if P^.Name <> nil then
      if P^.Command = 0 then
      begin
        T := FindHotKey(P^.SubMenu^.Items);
        if T <> nil then
        begin
          FindHotKey := T;
          Exit;
        end;
      end
      else if not P^.Disabled and (P^.KeyCode <> kbNoKey) and
        (P^.KeyCode = KeyCode) then
      begin
        FindHotKey := P;
        Exit;
      end;
    P := P^.Next;
  end;
  FindHotKey := nil;
end;

begin
  HotKey := FindHotKey(Menu^.Items);
end;

function TMenuView.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
var
  CurrentFont: Word;
begin
{TONY comment it
  CurrentFont := GlobalFont;
  GlobalFont := Font;
}
  NewSubView := New(PMenuBox, Init(Bounds, AMenu, AParentMenu));
{  GlobalFont := CurrentFont; TONY comment it}
end;

procedure TMenuView.Store(var S: TStream);

procedure DoStoreMenu(Menu: PMenu);
var
  Item: PMenuItem;
  Tok: Byte;
begin
  Tok := $FF;
  Item := Menu^.Items;
  while Item <> nil do
  begin
    with Item^ do
    begin
      S.Write(Tok, 1);
      S.WriteStr(Name);
      S.Write(Command, SizeOf(Word) * 3 + SizeOf(Boolean));
      if (Name <> nil) then
        if Command = 0 then DoStoreMenu(SubMenu)
        else S.WriteStr(Param);
    end;
    Item := Item^.Next;
  end;
  Tok := 0;
  S.Write(Tok, 1);
end;

begin
  TView.Store(S);
  DoStoreMenu(Menu);
end;

procedure TMenuView.Invalidate(Item: PMenuItem);
begin
  InvalidateSelf;
end;

{ TMenuBar }

constructor TMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
begin
  inherited Init(Bounds) {TONY};
  Font := MenusFont; {TONY}
  GrowMode := gfGrowHiX;
  Menu := AMenu;
  Options := Options or ofPreProcess;
end;

destructor TMenuBar.Done;
begin
  TMenuView.Done;
  DisposeMenu(Menu);
end;

procedure TMenuBar.Invalidate(Item: PMenuItem); {OOA}
var
  X, L: Integer;
  P: PMenuItem;
  R: TRect;
begin
  if Menu <> nil then
  begin
    X := CharWidth; {DK}
    P := Menu^.Items;
    while P <> nil do
    begin
      if P^.Name <> nil then
      begin
        L := FontWidth(CStr(P^.Name^));
        if (X + L < Size.X) and ((P = Current) or (P = Item)) then
        begin
          R.Assign(X - 4, 2, X + L + 4, Size.Y - 2);  {DK}
          InvalidateRect(R);
        end;
        Inc(X, L + 2 * CharWidth);
      end;
      P := P^.Next;
    end;
  end;
end;

procedure TMenuBar.Draw {SHIM, TONY, DK};
var
  X, L: Integer;
  CNormal, CSelect, CNormDisabled, CSelDisabled, Color, CShadow: Word;
  P: PMenuItem;
  Incr: Integer;
  i: Integer;
  CNormBack, CSelBack, CNormDisableBack, CSelDisableBack, BackColor : word;
begin
  CNormal := GetColor($0501);
  CSelect := GetColor($0A06);
  CNormDisabled := GetColor($0303);
  CSelDisabled := GetColor($0808);
  CShadow := GetColor($0B0C);

  CNormBack := GetColor($02);
  CSelBack := GetColor($07);
  CNormDisableBack := GetColor($04);
  CSelDisableBack := GetColor($09);

  FrameBar(0, 0, Size.X, Size.Y, Hi(CShadow), Lo(CShadow), CNormBack);

  if Menu <> nil then
  begin
    X := CharWidth;  {DK}
    P := Menu^.Items;
    while P <> nil do begin
      if P^.Name <> nil then begin
        L := FontWidth(CStr(P^.Name^));  {DK}
        if X + L < Size.X then begin
{GIO}     if P^.Disabled then
            if P = Current then begin Color := CSelDisabled;
              BackColor := CSelDisableBack
            end else begin Color := CNormDisabled;
              BackColor := CNormDisableBack;
            end
          else
            if P = Current then begin
              Color := CSelect; BackColor := CSelBack
            end else begin Color := CNormal; BackColor := CNormBack end;
{GIO}
          if P = Current then begin
            Incr := 1;
            FrameBar(X - 4, 2, X + L + 4, Size.Y - 2,           {DK}
              Lo(CShadow), Hi(CShadow), BackColor);
          end else begin
            Incr := 0;
            if BackColor <> CNormBack then
              Bar(X - 4, 2, X + L + 4, Size.Y - 2, BackColor);  {DK}
          end;

          WrCStr(X + Incr, 3 + Incr, P^.Name^, Color);          {DK}
        end;
        Inc(X, L + 2 * CharWidth);                              {DK}
      end;
      P := P^.Next;
    end;
  end;
end;

procedure TMenuBar.GetItemRect(Item: PMenuItem; var R: TRect) {SHIM};
var
  P: PMenuItem;
begin
  GetExtent(R); R.B.X := 1; {Dec(R.B.Y);}
  P := Menu^.Items;
  while True do
  begin
    R.A.X := R.B.X;
    if P^.Name <> nil then Inc(R.B.X, FontWidth(CStr(P^.Name^)) + 2 * CharWidth);  {DK}
    if P = Item then Exit;
    P := P^.Next;
  end;
end;

{ TMenuBox }

constructor TMenuBox.Init(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView);
var
  W, H, L: Integer;
  P: PMenuItem;
  R: TRect;
begin
  DefaultPaint(PaintInfo); {DK}
  DefaultFont(Font); {DK}
  Font := MenusFont; {TONY}
  W := 10 * CharWidth;
  H := 0;
  if AMenu <> nil then
  begin
    P := AMenu^.Items;
    while P <> nil do
    begin
      if P^.Name <> nil then
      begin
        L := FontWidth(CStr(P^.Name^)) + 6 * CharWidth;  {DK}
        if P^.Command = 0 then Inc(L, 3 * CharWidth) else
          if P^.Param <> nil then Inc(L, FontWidth(CStr(P^.Param^)) + 2 * CharWidth);  {DK}
        if L > W then W := L;
        Inc(H, FontHeight + 4{SHIM});
      end else Inc(H, FontHeight + 4) {SHIM};
      P := P^.Next;
    end;
  end;
  H := H + FontHeight;             {SHIM}
  Dec(W, CharWidth);               {DK}
  R.Copy(Bounds);
  if R.A.X + W < R.B.X then R.B.X := R.A.X + W else R.A.X := R.B.X - W;
  if R.A.Y + H < R.B.Y then R.B.Y := R.A.Y + H else R.A.Y := R.B.Y - H;
  TMenuView.Init(R);
  State := State or sfShadow;
  Options := Options or ofPreProcess;
  Menu := AMenu;
  ParentMenu := AParentMenu;
  IsBox := True;
end;

procedure TMenuBox.Invalidate(Item: PMenuItem);
var
  P: PMenuItem;
  Y: Integer;
  R: TRect;
begin
  if Menu <> nil then
  begin
    Y:= FontHeight shr 1;
    P := Menu^.Items;
    while P <> nil do
    begin
      if (P^.Name <> nil) and (P = Current) or (P = Item) then
      begin
        R.Assign(3, Y, Size.X-3, Y + FontHeight + 4);
        InvalidateRect(R);
      end;
      P := P^.Next;
      Inc(Y, FontHeight+4);
    end;
  end;
end;

procedure TMenuBox.Draw {SHIM, DK};
var
  CNormal, CSelect, CNormDisabled, CSelDisabled, Color, CShadow: Word;
  Y: Integer;
  P: PMenuItem;
  Incr: Integer;
  i : Integer;
  CNormBack, CSelBack, CNormDisableBack, CSelDisableBack, BackColor : word;
begin
  CNormal := GetColor($0501);
  CSelect := GetColor($0A06);
  CNormDisabled := GetColor($0303);
  CSelDisabled := GetColor($0808);
  CShadow := GetColor($0B0C);

  CNormBack := GetColor($02);
  CSelBack := GetColor($07);
  CNormDisableBack := GetColor($04);
  CSelDisableBack := GetColor($09);
  Y := FontHeight shr 1;
  FrameBar(0, 0, Size.X, Size.Y, Hi(CShadow), Lo(CShadow), CNormBack);
  if Menu <> nil then begin
    P := Menu^.Items;
    while P <> nil do begin
      Color := CNormal;
{GIO} if P^.Name <> nil then begin
        if P^.Disabled then
          if P = Current then begin BackColor := CSelDisableBack;
            Color := CSelDisabled
          end else begin BackColor := CNormDisableBack;
            Color := CNormDisabled
          end
        else
          if P = Current then begin
            Color := CSelect; BackColor := CSelBack
          end else begin BackColor := CNormBack; Color:=CNormal end;
{GIO}
        if P = Current then begin Incr := 1;
          FrameBar(3, Y, Size.X - 3, Y + FontHeight + 4,
            Lo(CShadow), Hi(CShadow), BackColor);
        end else begin Incr := 0;
          if BackColor <> CNormBack then
            Bar(3, Y, Size.X - 3, Y + FontHeight + 4, BackColor);
        end;

        WrCStr(CharWidth, Y+Incr, P^.Name^, Color);
        if P^.Command = 0 then
          WrStr(Size.X - 2 * CharWidth, Y+Incr,  #16, Lo(Color)) else
          if P^.Param <> nil then
            WrStr(Size.X - FontWidth(CStr(P^.Param^)) - CharWidth, Y+Incr,  {DK}
              P^.Param^, Lo(Color));
        Inc(Y, FontHeight+4);
      end else begin
        HLine(1, Y + FontHeight shr 1 + 1, Size.X-1, Lo(CShadow));
        HLine(1, Y + FontHeight shr 1 + 2, Size.X-1, Hi(CShadow));
        Inc(Y, FontHeight+4);
      end;
      P := P^.Next;
    end;
  end;
end;

procedure TMenuBox.GetItemRect(Item: PMenuItem; var R: TRect) {SHIM};
var
  Y, Y1: Integer;
  P: PMenuItem;
begin
  Y := FontHeight shr 1;
  P := Menu^.Items;
  while P <> Item do
  begin
    Inc(Y, FontHeight + 4);
    P := P^.Next;
  end;
  Y1 := FontHeight+4;
  R.Assign(1, Y, Size.X - 1, Y + Y1);
end;

constructor TMenuPopup.Init(var Bounds: TRect; AMenu: PMenu);
begin
  inherited Init(Bounds, AMenu, nil);
end;

procedure TMenuPopup.HandleEvent(var Event: TEvent);
var
  P: PMenuItem;
begin
  case Event.What of
    evKeyDown:
      begin
        P := FindItem(GetCtrlChar(Event.KeyCode));
        if P = nil then
          P := HotKey(Event.KeyCode);
        if (P <> nil) and (CommandEnabled(P^.Command)) then
        begin
          Event.What := evCommand;
          Event.Command := P^.Command;
          Event.InfoPtr := nil;
          PutEvent(Event);
          ClearEvent(Event);
        end
        else
          if GetAltChar(Event.KeyCode) <> #0 then
            ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

{ TStatusLine }

constructor TStatusLine.Init(var Bounds: TRect; ADefs: PStatusDef);
begin
  inherited Init(Bounds) {TONY};
  Font := StatusFont; {TONY}
  Options := Options or ofPreProcess;
  EventMask := EventMask or evBroadcast;
  GrowMode := gfGrowLoY + gfGrowHiX + gfGrowHiY;
  Defs := ADefs; FindItems;
end;

constructor TStatusLine.Load(var S: TStream);

function DoLoadStatusItems: PStatusItem;
var
  Count: Integer;
  Cur, First: PStatusItem;
  Last: ^PStatusItem;
begin
  Cur := nil;
  Last := @First;
  S.Read(Count, SizeOf(Integer));
  while Count > 0 do
  begin
    New(Cur);
    Last^ := Cur;
    Last := @Cur^.Next;
    Cur^.Text := S.ReadStr;
    S.Read(Cur^.KeyCode, SizeOf(Word) * 2);
    Dec(Count);
  end;
  Last^ := nil;
  DoLoadStatusItems := First;
end;

function DoLoadStatusDefs: PStatusDef;
var
  Cur, First: PStatusDef;
  Last: ^PStatusDef;
  Count: Integer;
begin
  Last := @First;
  S.Read(Count, SizeOf(Integer));
  while Count > 0 do
  begin
    New(Cur);
    Last^ := Cur;
    Last := @Cur^.Next;
    S.Read(Cur^.Min, 2 * SizeOf(Word));
    Cur^.Items := DoLoadStatusItems;
    Dec(Count);
  end;
  Last^ := nil;
  DoLoadStatusDefs := First;
end;

begin
  TView.Load(S);
  Defs := DoLoadStatusDefs;
  FindItems;
end;

destructor TStatusLine.Done;
var
  T: PStatusDef;

procedure DisposeItems(Item: PStatusItem);
var
  T: PStatusItem;
begin
  while Item <> nil do
  begin
    T := Item;
    Item := Item^.Next;
    DisposeStr(T^.Text);
    Dispose(T);
  end;
end;

begin
  while Defs <> nil do
  begin
    T := Defs;
    Defs := Defs^.Next;
    DisposeItems(T^.Items);
    Dispose(T);
  end;
  TView.Done;
end;

procedure TStatusLine.Draw {SHIM, DK};
var
  T: PStatusItem;
  I, L: Integer;
  CSelect, CNormal, CSelDisabled, CNormDisabled, CShadow: Word;
  Color: Word;
  HintBuf: String;
  Y1, Y2: Integer;
  CNormBack, CSelBack, CNormDisableBack, CSelDisableBack, BackColor : word;
begin
  CNormal := GetColor($0501);
  CSelect := GetColor($0A06);
  CNormDisabled := GetColor($0303);
  CSelDisabled := GetColor($0808);
  CShadow := GetColor($0B0C);

  CNormBack := GetColor($02);
  CSelBack := GetColor($07);
  CNormDisableBack := GetColor($04);
  CSelDisableBack := GetColor($09);

  Y1 := (Size.Y - FontHeight) shr 1; Y2 := Size.Y - Y1;
  FrameBar(0, 0, Size.X, Size.Y, Hi(CShadow), Lo(CShadow), CNormBack);
  T := Items;
  I := 0;
  while T <> nil do
  begin
    if T^.Text <> nil then
    begin
      L := FontWidth(CStr(T^.Text^));
      if I + L < Size.X then  {DK}
      begin
        if CommandEnabled(T^.Command) then
          if T = Selected then begin
            Color := CSelect;
            BackColor := CSelBack;
          end else begin
            Color := CNormal;
            BackColor := CNormBack;
          end
        else
          if T = Selected then begin
            Color := CSelDisabled;
            BackColor := CSelDisableBack;
          end else begin
            Color := CNormDisabled;
            BackColor := CNormDisableBack;
          end;
        Rectangle(4 + I, Y1 - 2, I + L + CharWidth + 8, Y2 + 2, 1, Lo(CShadow), Hi(CShadow));
        if BackColor <> CNormBack then
          Bar(6 + I, Y1, I + L + CharWidth + 6, Y2, BackColor);  {DK}
        WrCStr(6 + I + CharWidth shr 1, Y1, T^.Text^, Color);    {DK}
      end;
      Inc(I, L + 2 * CharWidth);                                 {DK}
    end;
    T := T^.Next;
  end;
  if I < Size.X - 2 * CharWidth then begin
    HintBuf := Hint(HelpCtx);
    if HintBuf <> '' then begin Inc(I, 2);
      if I + FontWidth(HintBuf) > Size.X then               {DK}
        HintBuf[0] := Char((Size.X - I) div CharWidth);     {DK}
      WrCStr(I, Y1, HintBuf, CNormal);                      {DK}
    end;
  end;
end;

procedure TStatusLine.FindItems;
var
  P: PStatusDef;
begin
  P := Defs;
  while (P <> nil) and ((HelpCtx < P^.Min) or (HelpCtx > P^.Max)) do
    P := P^.Next;
  if P = nil then Items := nil else Items := P^.Items;
end;

function TStatusLine.GetPalette: PPalette;
const
  P: string[Length(CStatusLine)] = CStatusLine;
begin
  GetPalette := @P;
end;

procedure TStatusLine.HandleEvent(var Event: TEvent);
var
  Mouse: TPoint;
  T: PStatusItem;

function ItemMouseIsIn: PStatusItem;
var
  I,K: Word;
  T: PStatusItem;
begin
  ItemMouseIsIn := nil;
  if (Mouse.Y >= Size.Y) or (Mouse.Y < 0) then Exit; {TONY}
  I := 0;
  T := Items;
  while T <> nil do
  begin
    if T^.Text <> nil then
    begin
      K := I + FontWidth(CStr(T^.Text^)) + 2 * CharWidth;  {DK}
      if (Mouse.X >= I) and (Mouse.X < K) then
      begin
        ItemMouseIsIn := T;
        Exit;
      end;
      I := K;
    end;
    T := T^.Next;
  end;
end;

begin
  TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
        T := nil;
        repeat
          MakeLocal(Event.Where, Mouse);
          if T <> ItemMouseIsIn then
          begin
            T := ItemMouseIsIn;
            Selected:= T;
            DrawView;
          end;
        until not MouseEvent(Event, evMouseMove);
        if (T <> nil) and CommandEnabled(T^.Command) then
        begin
          Event.What := evCommand;
          Event.Command := T^.Command;
          Event.InfoPtr := nil;
          PutEvent(Event);
        end;
        ClearEvent(Event);
        Selected:= Nil;
        DrawView;
      end;
    evKeyDown:
      begin
        T := Items;
        while T <> nil do
        begin
          if (Event.KeyCode = T^.KeyCode) and
            CommandEnabled(T^.Command) then
          begin
            Event.What := evCommand;
            Event.Command := T^.Command;
            Event.InfoPtr := nil;
            Exit;
          end;
          T := T^.Next;
        end;
      end;
    evBroadcast: if Event.Command = cmCommandSetChanged then DrawView
  end;
end;

function TStatusLine.Hint(AHelpCtx: Word): String;
begin
  Hint := '';
end;

procedure TStatusLine.Store(var S: TStream);

procedure DoStoreStatusItems(Cur: PStatusItem);
var
  T: PStatusItem;
  Count: Integer;
begin
  Count := 0;
  T := Cur;
  while T <> nil do
  begin
    Inc(Count);
    T := T^.Next
  end;
  S.Write(Count, SizeOf(Integer));
  while Cur <> nil do
  begin
    S.WriteStr(Cur^.Text);
    S.Write(Cur^.KeyCode, SizeOf(Word) * 2);
    Cur := Cur^.Next;
  end;
end;

procedure DoStoreStatusDefs(Cur: PStatusDef);
var
  Count: Integer;
  T: PStatusDef;
begin
  Count := 0;
  T := Cur;
  while T <> nil do
  begin
    Inc(Count);
    T := T^.Next
  end;
  S.Write(Count, SizeOf(Integer));
  while Cur <> nil do
  begin
    with Cur^ do
    begin
      S.Write(Min, SizeOf(Word) * 2);
      DoStoreStatusItems(Items);
    end;
    Cur := Cur^.Next;
  end;
end;

begin
  TView.Store(S);
  DoStoreStatusDefs(Defs);
end;

procedure TStatusLine.Update;
var
  H: Word;
  P: PView;
begin
  P := TopView;
  if P <> nil then H := P^.GetHelpCtx else H := hcNoContext;
  if HelpCtx <> H then
  begin
    HelpCtx := H;
    FindItems;
    DrawView;
  end;
end;

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
  ANext:PStatusDef): PStatusDef;
var
  T: PStatusDef;
begin
  New(T);
  with T^ do
  begin
    Next := ANext;
    Min := AMin;
    Max := AMax;
    Items := AItems;
  end;
  NewStatusDef := T;
end;

function NewStatusKey(const AText: String; AKeyCode: Word; ACommand: Word;
  ANext: PStatusItem): PStatusItem;
var
  T: PStatusItem;
begin
  New(T);
  T^.Text := NewStr(AText);
  T^.KeyCode := AKeyCode;
  T^.Command := ACommand;
  T^.Next := ANext;
  NewStatusKey := T;
end;

procedure RegisterMenus;
begin
  RegisterType(RMenuBar);
  RegisterType(RMenuBox);
  RegisterType(RStatusLine);
  RegisterType(RMenuPopup);
end;


{--- TScreenSaver ---}

function GetTime: longint; assembler;
asm
  xor    ah, ah
  int    1Ah
  mov    ax, dx
  mov    dx, cx
end;

constructor TScreenSaver.Init(var Bounds: TRect; AFlags: Byte; ATime: Word);
begin
  inherited Init(Bounds);
  GrowMode := gfGrowHiX or gfGrowHiY;
  Flags := AFlags;
  Time := LongInt(ATime) * 91;
  Time := Time div 5;
  LastTime := GetTime;
  Hide;
end;

destructor TScreenSaver.Done;
begin
  inherited Done;
end;

procedure TScreenSaver.HandleEvent(var Event: TEvent);
var
  NewTime: Longint;
begin
  if (Event.What = evCommand) then
    if (Event.Command = cmScreenSave) then begin
      ClearEvent(Event);
      if not InCanLeave then begin
        ActionOnSave(True);
        Show;
      end;
      Exit;
    end else if (Event.Command = cmScreenRestore) then begin
      ClearEvent(Event);
      if not InCanLeave then begin
        InCanLeave := True;
        if CanLeave then Hide;
        InCanLeave := False;
      end;
      Exit;
    end;
  if FlagIsSet(State, sfVisible) then begin
    ActionOnSave(False);
    if Event.What <> evNothing then begin
      Message(@Self, evCommand, cmScreenRestore, Nil);
      if not FlagIsSet(State, sfVisible) then ClearEvent(Event);
    end;
    LastTime := GetTime;
  end else begin
    if Event.What <> evNothing then begin
      LastTime := GetTime;
      if (Event.What = evMouseMove) and
         (ByteFlagIsSet(Flags, ssfMouseReset)) and
         (Event.Where.X = ScreenWidth - 1) and (Event.Where.Y = 0)
      then Message(@Self, evCommand, cmScreenSave, Nil);
    end else if ByteFlagIsSet(Flags, ssfTimer) then begin
      if (ByteFlagIsSet(Flags, ssfMousePreserve)) and
         (MouseWhere.X = 0) and (MouseWhere.Y = 0)
      then begin
        LastTime := GetTime;
        Exit;
      end;
      NewTime := GetTime;
      if NewTime - LastTime > 0 then NewTime := NewTime - LastTime else NewTime := LastTime - NewTime;
      if NewTime >= Time then Message(@Self, evCommand, cmScreenSave, Nil);
    end;
  end;
end;

procedure TScreenSaver.ActionOnSave(JustActivated: Boolean);
begin
 if JustActivated then begin
   MakeFirst;
   HideMouse;
 end;
end;

procedure TScreenSaver.Draw;
var
  R: TRect;
begin
  GetExtent(R);
  Bar(R.A.X, R.A.Y, R.B.X, R.B.Y, 0);
end;

function TScreenSaver.CanLeave: boolean;
begin
  ShowMouse;
  CanLeave := True;
end;

end.
