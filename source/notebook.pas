{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : NOTEBOOK                                             █
  █                                                                    █
  █ Description : Организация многостраничного диалога в виде книжки   █
  █               с переворачивающимися страницами - TNoteBook.        █
  █                                                                    █
  █               Организация циклического диалога в виде книжки       █
  █               с переворачивающимися страницами - TListBook.        █
  █                                                                    █
  █ Author      : Шимон Ю.И.  Березин А.В.                             █
  █════════════════════════════════════════════════════════════════════█
  █ Remarks     :                                                      █
  █                       TNoteBook                                    █
  █                                                                    █
  █    1. Используются команды с номерами 300..303.                    █
  █    2. Используется BitMap с номером 209.                           █
  █    3. Методы GetData и SetData вызываются для отдельных страниц    █
  █       в момент их переключения, а для текущей страницы также при   █
  █       старте и выходе.                                             █
  █    4. Вследствие постраничного механизма вызовов GetData/SetData   █
  █       ExecuteDialog(TNoteBook, ...) всегда возвращает cmOK.        █
  █    5. Объекты TVISION, возвращающие не равный нулю DataSize, не    █
  █       должны вставляться непосредственно в TNoteBook (только в     █
  █       страницы).                                                   █
  █    6. Неименованые страницы объединяются в группу с первой         █
  █       предшествующей именованной. Перемещения по страницам         █
  █       осуществляются при помощи кнопок "влево"/"вправо" или клавиш █
  █       PgUp/PgDn. На первую страницу группы, кроме того, можно      █
  █       переключиться, выбрав при помощи "мыши" закладку с           █
  █       названием этой страницы.                                     █
  █       Если количество групп настолько велико, что при отрисовке    █
  █       их закладки не помещаются в области диалога, то лишние       █
  █       закладки не отрисовываются, а переход к этим страницам       █
  █       осуществляется только при помощи кнопок или клавиш.          █
  █    7. Для установки начальной страницы после добавления всех       █
  █       страниц следует использовать SetPage(номер страницы).        █
  █       По умолчанию устанавливается 0-ая.                           █
  █                                                                    █
  █                       TListBook                                    █
  █                                                                    █
  █    1. В качестве данных используется ^PCollection.                 █
  █    2. Всегда устанавливается 0-ая начальная страница.              █
  █    3. Объекты TVISION должны вставляться непосредственно в         █
  █       TListBook (объекты, вставленные в страницы, теряются).       █
  █                                                                    █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}

unit NoteBook;

interface uses Views, Objects, Dialogs, Drivers, BitMaps,
               EGInline, EGString, GDI;

Const
  cmNoteBookPgUp        = 300;
  cmNoteBookPgDn        = 301;
  cmNoteBookSelectGroup = 302;
  cmNoteBookPageChanged = 303;

Type
  TNoteBookPlace = (nbVertical, nbHorizontal);

Type
  PPage = ^TPage;
  TPage = object(TGroup)
    Title  : PString;
    Dummy: PView;
    constructor Init(const ATitle: string);
    destructor Done; virtual;
  end;

  PNoteBook =^TNoteBook;
  TNoteBook = object(TDialog)
    Page  : PPage;
    Pages : PCollection;
    CheckInOut: PCollection;
    ButtonLeft, ButtonRight: PBMPButton;
    DataPtr: Pointer;
    Place: TNoteBookPlace;

    constructor Init(var Bounds: TRect; ATitle: TTitleStr; APlace: TNoteBookPlace);
    procedure   HandleEvent(var Event: TEvent); virtual;
    function    MaxPages : integer;
    function    PageIndex(APage: PPage) : integer;
    function    PageAt(APage: integer) : PPage;
    destructor  Done; virtual;
    procedure   InitFrame; virtual;

    function    InsertPage(APage: PPage): PPage; virtual;
    function    InsertView(APage: PPage; AView: PView): PView; virtual;

    procedure   SetPage(APage: integer); virtual;

    procedure   SetData(var Rec); virtual;
    procedure   GetData(var Rec); virtual;
    function    DataSize: word; virtual;

    procedure   SetPageData(APage: integer; var Rec); virtual;
    procedure   GetPageData(APage: integer; var Rec); virtual;
    function    PageDataSize(APage: integer): word; virtual;
    procedure   SelectPage(APage: PPage); virtual;
    function    FindPageByTitle(const ATitle: String): integer;

  private
    function    NumGroups : integer;
    function    NumGroupsBefore(APage: integer) : integer;
    function    GetPageTitle(APage: integer): PString;
    function    GetPageWidth(APage: integer): integer;
    function    GroupForPage(APage: integer): integer;
    function    PagesInGroup(APage: integer): integer;
    function    IndexInGroup(APage: integer): integer;
    function    NextGroup(APage: integer): integer;
    function    DataOffset(APage: integer): Pointer;
    procedure   SetGetCurPage(IsSet: boolean);
    function    PrevPagesDataSize(APage: integer): word;
    procedure   ViewsRemove; virtual;
    procedure   ViewsInsert; virtual;
  end;

  PNoteBookFrame = ^TNoteBookFrame;
  TNoteBookFrame = object(TFrame)
    WhiteSpace: TPoint;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function  GetTitleWidth: integer;
    procedure GetLastRectangle(var R: TRect); virtual;
    procedure GetFirstRectangle(var R: TRect); virtual;
    procedure DrawPage(var R: TRect; var Delta: TPoint; Sel: Boolean); virtual;
    procedure DrawTitle(var R: TRect; var Delta: TPoint; const Title: String; Sel: Boolean); virtual;
    procedure GetRectangles(APage: integer; var PageRect, TitleRect: TRect; var Direction: TPoint); virtual;
    procedure GetDirection(var RFirst, RLast: TRect; var Direction: TPoint);
    procedure DrawPageTitle(var R: TRect); virtual;
  end;

  PNoteBookHorizontalFrame = ^TNoteBookHorizontalFrame;
  TNoteBookHorizontalFrame = object(TNoteBookFrame)
    procedure DrawPage(var R: TRect; var Delta: TPoint; Sel: Boolean); virtual;
    procedure DrawTitle(var R: TRect; var Delta: TPoint; const Title: String; Sel: Boolean); virtual;
    procedure GetLastRectangle(var R: TRect); virtual;
    procedure GetFirstRectangle(var R: TRect); virtual;
    procedure GetRectangles(APage: integer; var PageRect, TitleRect: TRect; var Direction: TPoint); virtual;
  end;

  PListBook = ^TListBook;
  TListBook = object(TNoteBook)
    function    InsertPage(APage: PPage): PPage; virtual;
    function    InsertView(APage: PPage; AView: PView): PView; virtual;
    function    DataSize: word; virtual;
    procedure   SetPageData(APage: integer; var Rec); virtual;
    procedure   GetPageData(APage: integer; var Rec); virtual;
    function    PageDataSize(APage: integer): word; virtual;
    procedure   SetData(var Rec); virtual;
    function    GetPageTitle(APage: integer; Item: Pointer): String; virtual;
  private
    procedure   ViewsRemove; virtual;
    procedure   ViewsInsert; virtual;
  end;


procedure NoteBook_Hole;    {209}

implementation

{$L HOLE.OBJ}
procedure NoteBook_Hole; external;   {209}

{
  ┌─────────────────────────────────────────────────────────╖
  │                                                         ║
  ╘═════════════════════════════════════════════════════════╝
}
constructor TPage.Init(const ATitle: String);
var
  R: TRect;
begin
  inherited Init(AllScreen);
  Title := NewStr(ATitle);
  GetExtent(R);
  New(Dummy, Init(R));
  Insert(Dummy);
end;

destructor TPage.Done;
begin
  if Title <> Nil then DisposeStr(Title);
  inherited Done;
end;

{
  ┌─────────────────────────────────────────────────────────╖
  │                                                         ║
  ╘═════════════════════════════════════════════════════════╝
}
constructor TNoteBook.Init(var Bounds: TRect; ATitle: TTitleStr; APlace: TNoteBookPlace);
var
  R: TRect;
begin
  inherited Init(Bounds, ATitle);
  New(Pages, Init(10, 1));
  New(CheckInOut, Init(10, 5));
  Palette := dpBlueDialog;
  Place := APlace;
  GetExtent(R);
  case Place of
    nbVertical   : Frame := New(PNoteBookFrame, Init(R));
    nbHorizontal : Frame := New(PNoteBookHorizontalFrame, Init(R));
  end;
  if Frame <> nil then Insert(Frame);
end;

function TNoteBook.MaxPages : integer;
begin
  if Pages = Nil then MaxPages := 0 else MaxPages := Pages^.Count;
end;

function TNoteBook.PageIndex(APage: PPage) : integer;
begin
  if Pages = nil then PageIndex := -1 else PageIndex := Pages^.IndexOf(APage);
end;

function TNoteBook.PageAt(APage: integer) : PPage;
begin
  if Pages = nil then PageAt := Nil else PageAt := Pages^.At(APage);
end;

destructor TNoteBook.Done;
begin
  if Assigned(Pages) then Dispose(Pages, Done);
  CheckInOut^.DeleteAll;
  Dispose(CheckInOut, Done);
  inherited Done;
end;

procedure TNoteBook.InitFrame;
begin
  Frame := Nil;
end;

function TNoteBook.InsertPage(APage: PPage): PPage;
begin
  if (Pages <> nil) then Pages^.Insert(APage);
  InsertPage := APage;
end;

function TNoteBook.InsertView(APage: PPage; AView: PView): PView;
begin
  if APage <> nil then APage^.Insert(AView);
  InsertView := AView;
end;

procedure TNoteBook.SetPage(APage: integer);
begin
  if (Pages <> nil) and (APage >= 0) and (APage < MaxPages) then
    SelectPage(Pages^.At(APage));
end;

function TNoteBook.DataOffset(APage: integer): Pointer;
var
  Off: ^byte;
begin
  Off := DataPtr;
  if Assigned(DataPtr) then Inc(Off, PrevPagesDataSize(APage));
  DataOffset := Off;
end;

procedure TNoteBook.ViewsRemove;
  procedure ViewRemove(P: PView); far; begin
    if (CheckInOut^.IndexOf(P) < 0) then Exit;
    Delete(P);
    Page^.InsertBefore(P, Page^.Dummy);
  end;
begin
  ForEach(@ViewRemove);
end;

procedure TNoteBook.ViewsInsert;
  procedure ViewInsert(P: PView); far; begin
    if P = Page^.Dummy then Exit;
    Page^.Delete(P);
    InsertBefore(P, Frame);
    CheckInOut^.Insert(P);
  end;
begin
  CheckInOut^.DeleteAll;
  Page^.ForEach(@ViewInsert);
  SelectNext(False);
end;

procedure TNoteBook.SelectPage(APage: PPage);
var
  R: TRect;
  i : integer;
begin
  if (APage = nil) or (APage = Page) then exit;
  Lock;
  { Вставка стандартных буттончиков, если надо }
  if ButtonRight = nil then begin
    PNoteBookFrame(Frame)^.GetFirstRectangle(R);
    R.A.X := R.B.X - BitMapWidth(GetImage(7));
    R.A.Y := R.B.Y - BitMapHeight(GetImage(7));
    R.Move( -2, -2);
    ButtonRight := New(PBmpButton, Init(R, cmNoteBookPgDn, bfNormal,
      7, 7, 7, 8, 7));
    ClearFlag(ButtonRight^.Options, ofSelectable);
    ButtonRight^.GrowMode := gfGrowAll;
    Insert(ButtonRight);
  end;
  if ButtonLeft = nil then begin
    R.Move(- BitMapWidth(GetImage(7)) - 2, 0);
    ButtonLeft := New(PBmpButton, Init(R, cmNoteBookPgUp, bfNormal,
      5, 5, 5, 6, 5));
    ClearFlag(ButtonLeft^.Options, ofSelectable);
    ButtonLeft^.GrowMode := gfGrowAll;
    Insert(ButtonLeft);
  end;

  { Удаление существующих fields`ов}
  if Assigned(Page) then begin
    if Assigned(DataPtr) then begin
      i := PageIndex(Page);
      GetPageData(i, DataOffset(i)^);
    end;
    ViewsRemove;
  end;

  { Вставка новых }
  Page := APage;
  ViewsInsert;
  if Assigned(DataPtr) then begin
    i := PageIndex(Page);
    SetPageData(i, DataOffset(i)^);
  end;
  Message(@Self, evCommand, cmNoteBookPageChanged, @Self);
  Frame^.DrawView;
  UnLock;
end;

function TNoteBook.FindPageByTitle(const ATitle: String): integer;
var
  i : integer;
begin
  i := GroupForPage(0);
  while (i >= 0) do begin
    if (Trim(ATitle) = Trim(GetPageTitle(i)^)) then begin
      FindPageByTitle := i;
      Exit;
    end;
    i := NextGroup(i);
  end;
  FindPageByTitle := -1;
end;

procedure TNoteBook.HandleEvent(var Event: TEvent);

  procedure CE; begin ClearEvent(Event); end;
  procedure M(Cmd: word); begin Message(@Self, evCommand, Cmd, Nil); end;
  procedure ME(Page: integer); begin
    Message(@Self, evCommand, cmNoteBookSelectGroup, pointer(longint(Page)));
  end;

begin
  if Page = nil then SetPage(0);

  if ((Event.What and evMessage) <> 0) and (Event.Command = cmCancel) then
    Event.Command := cmOK; {для GetData`ирования текущего Page`а}

  inherited HandleEvent(Event);
  if (Event.What = evKeyDown) then begin
    case Event.KeyCode of
      kbPgUp: M(cmNoteBookPgUp);
      kbPgDn: M(cmNoteBookPgDn);
      kbCtrlPgUp: ME(0);
      kbCtrlPgDn: ME(MaxPages - 1);
    else
      Exit;
    end;
    CE;
  end else if ((Event.What and evMessage) <> 0) then begin
    case Event.Command of
      cmNoteBookPgUp: if Pages <> nil then SetPage(PageIndex(Page) - 1);
      cmNoteBookPgDn: if Pages <> nil then SetPage(PageIndex(Page) + 1);
      cmNoteBookSelectGroup: SetPage(Event.InfoInt);
    else
      Exit;
    end;
    CE;
  end;
end;

function TNoteBook.GetPageTitle(APage: integer): PString;
begin
  GetPageTitle := Nil;
  if (Pages = nil) or (APage < 0) or (APage >= MaxPages) then Exit;
  GetPageTitle := PPage(Pages^.At(APage))^.Title;
end;

function TNoteBook.GetPageWidth(APage: integer): integer;
var
  P: PString;
begin
  P := GetPageTitle(APage);
  if P = nil then GetPageWidth := 0 else GetPageWidth := FontWidth(P^);
end;

function TNoteBook.NumGroups : integer; { Число страниц с заголовками }
var
  i: integer;
begin
  i := MaxPages;
  if i > 0 then i := NumGroupsBefore(i - 1) + 1;
  NumGroups := i;
end;

function TNoteBook.NumGroupsBefore(APage: integer) : integer;
var
  i: integer;
  Pg: PPage;

  function Calc(Item: PPage): boolean; far;
  begin
    if Item <> Pg then if Assigned(Item^.Title) then Inc(i);
    Calc := Item = Pg;
  end;

begin
  i := 0;
  if Pages <> nil then begin;
    Pg := PageAt(GroupForPage(APage));
    Pages^.FirstThat(@Calc);
  end;
  NumGroupsBefore := i;
end;

function TNoteBook.GroupForPage(APage: integer): integer;
begin
  GroupForPage := -1;
  if (Pages = nil) or (APage < 0) or (APage >= MaxPages) then Exit;
  while (APage >= 0) and (PPage(Pages^.At(APage))^.Title = nil) do Dec(APage);
  GroupForPage := APage;
end;

function TNoteBook.PagesInGroup(APage: integer): integer;
var
  i, j: integer;
begin
  i := GroupForPage(APage); APage := i + 1;
  if APage > 0 then begin
    j := MaxPages;
    while (APage < j) and (PPage(Pages^.At(APage))^.Title = nil) do Inc(APage);
    Dec(APage, i);
  end;
  PagesInGroup := APage;
end;

function TNoteBook.IndexInGroup(APage: integer): integer;
var
  Group: integer;
begin
  Group := GroupForPage(APage);
  if Group >= 0 then
    IndexInGroup := APage - Group + 1
  else
    IndexInGroup := 0;
end;

function TNoteBook.NextGroup(APage: integer): integer;
begin
  NextGroup := GroupForPage(GroupForPage(APage) + PagesInGroup(APage));
end;

procedure TNoteBook.SetGetCurPage(IsSet: boolean);
var
  i : integer;
begin
  if Assigned(Page) then begin
    i := PageIndex(Page);
    if IsSet then
      SetPageData(i, DataOffset(i)^)
    else
      GetPageData(i, DataOffset(i)^)
  end;
end;

procedure TNoteBook.SetData(var Rec);
begin
  DataPtr := @Rec;
  SetGetCurPage(True);
end;

procedure TNoteBook.GetData(var Rec);
begin
  SetGetCurPage(False);
end;

procedure TNoteBook.SetPageData(APage: integer; var Rec);
var
  Pg: PPage;
begin
  Pg := PageAt(APage);
  if Pg = nil then exit;
  if Pg = Page then TDialog.SetData(Rec) else Pg^.SetData(Rec);
end;

procedure TNoteBook.GetPageData(APage: integer; var Rec);
var
  Pg: PPage;
begin
  Pg := PageAt(APage);
  if Pg = nil then exit;
  if Pg = Page then TDialog.GetData(Rec) else Pg^.GetData(Rec);
end;

function TNoteBook.PageDataSize(APage: integer): word;
var
  Pg: PPage;
begin
  PageDataSize := 0;
  Pg := PageAt(APage);
  if Pg <> nil then begin
    if Pg = Page then
      PageDataSize := TDialog.DataSize
    else
      PageDataSize := Pg^.DataSize;
  end;
end;

function TNoteBook.PrevPagesDataSize(APage: integer): word;
var
  S: word;
  i: integer;
begin
  S := 0;
  for i := 0 to APage - 1 do Inc(S, PageDataSize(i));
  PrevPagesDataSize := S;
end;

function TNoteBook.DataSize: word;
begin
  DataSize := PrevPagesDataSize(MaxPages);
end;

{
  ┌─────────────────────────────────────────────────────────╖
  │ TNoteBookFrame                                          ║
  ╘═════════════════════════════════════════════════════════╝
}
constructor TNoteBookFrame.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  WhiteSpace.X := 1;
  WhiteSpace.Y := 1;
end;

function TNoteBookFrame.GetTitleWidth: integer;
var
  NB: PNoteBook absolute Owner;
  NP: integer;
  i, TitleWidth : integer;
begin
  TitleWidth := NB^.GetPageWidth(0);
  NP := NB^.MaxPages;
  for i := 1 to NP - 1 do begin
    TitleWidth := MaxWord(TitleWidth, NB^.GetPageWidth(i));
  end;
  Inc(TitleWidth, 2 * CharWidth);
  GetTitleWidth := TitleWidth;
end;

procedure TNoteBookFrame.GetLastRectangle(var R: TRect);
var
  NB: PNoteBook absolute Owner;
  l, TitleWidth: integer;
begin
  TitleWidth := GetTitleWidth;
  l := NB^.NumGroups - 1;
  R.Assign(8 + FrameWidth + l * (WhiteSpace.X + 1), UpBarHeight + FontHeight + l * (WhiteSpace.Y + 1),
    Size.X - TitleWidth - 2 * CharWidth - FrameWidth, Size.Y - FontHeight - FrameWidth)
end;

procedure TNoteBookFrame.GetFirstRectangle(var R: TRect);
var
  NB: PNoteBook absolute Owner;
  l: integer;
begin
  l := NB^.NumGroups - 1;
  GetLastRectangle(R);
  R.Move(- l * (WhiteSpace.X + 1), - l * (WhiteSpace.Y + 1));
end;

procedure TNoteBookFrame.HandleEvent(var Event: TEvent);
var
  NB: PNoteBook absolute Owner;
  Mouse: TPoint;
  Group: integer;
  R0, R: TRect;
  D: TPoint;
  Ok : boolean;
  j : integer;

  function DT(i: integer): boolean; begin
    GetRectangles(i, R0, R, D);
    Ok := R.Contains(Mouse) or R0.Contains(Mouse);
    DT := Ok;
  end;

begin
  if (Event.What = evMouseDown) and (Event.Buttons and mbLeftButton = mbLeftButton) then
  begin
    MakeLocal(Event.Where, Mouse);
    Ok := False;
    j := NB^.GroupForPage(NB^.PageIndex(NB^.Page));
    Group := j; if not DT(j) then begin
      for Group := j + 1 to NB^.MaxPages - 1 do if DT(Group) then Break;
      if not OK then
      for Group := j - 1 downto 0 do if DT(Group) then Break;
    end;
    if Ok then begin
      ClearEvent(Event);
      Message(NB, evCommand, cmNoteBookSelectGroup, pointer(longint(Group)));
    end;
  end;
  inherited HandleEvent(Event);
end;

procedure TNoteBookFrame.GetDirection(var RFirst, RLast: TRect; var Direction: TPoint);
begin
  Direction.X := WhiteSpace.X + 1;
  Direction.Y := WhiteSpace.Y + 1;
  if RFirst.A.X > RLast.A.X then Direction.X := -Direction.X;
  if RFirst.A.Y > RLast.A.Y then Direction.Y := -Direction.Y;
end;

procedure TNoteBookFrame.GetRectangles(APage: integer; var PageRect, TitleRect: TRect; var Direction: TPoint);
var
  NB: PNoteBook absolute Owner;
  GroupNr, TitleNr, PageNr, LastGroup, OnTop, TitleHeight, TitleWidth, TitlesOnPage : integer;
  R: TRect;
begin
  if NB^.PageAt(APage)^.Title = nil then begin
    FillChar(PageRect, SizeOf(PageRect), 0);
    TitleRect.Copy(PageRect);
    Exit;
  end;
  GetFirstRectangle(PageRect);
  GetLastRectangle(R);
  GetDirection(PageRect, R, Direction);

  GroupNr   := NB^.NumGroupsBefore(APage);
  OnTop     := NB^.NumGroupsBefore(NB^.PageIndex(NB^.Page));
  LastGroup := NB^.NumGroups - 1;

  TitleWidth   := GetTitleWidth;
  TitleHeight  := 5 * FontHeight div 4;
  TitlesOnPage := (PageRect.B.Y - PageRect.A.Y - FontHeight - Direction.Y) div TitleHeight;
  TitleNr      := GroupNr mod TitlesOnPage;
  if GroupNr >= OnTop then
    GroupNr := GroupNr - OnTop
  else begin
    GroupNr := LastGroup - GroupNr;
  end;

  PageRect.Move(Direction.X * GroupNr, Direction.Y * GroupNr);
  TitleRect.Assign(PageRect.B.X, PageRect.A.Y,
                   PageRect.B.X + TitleWidth, PageRect.A.Y + TitleHeight);
  TitleRect.Move(0, TitleHeight div 2 + TitleNr * TitleHeight);
end;

procedure TNoteBookFrame.DrawPage(var R: TRect; var Delta: TPoint; Sel: Boolean);
var
  Color, Sh: word;
begin
  if Sel then Color := GetColor($05) else Color := GetColor($03);
  if Sel then begin
    Sh := GetColor($0708);
    Rectangle(R.A.X, R.A.Y, R.B.X + 1, R.B.Y + 1, 1, Color, Color);
    Rectangle(R.A.X+1, R.A.Y+1, R.B.X, R.B.Y, 1, Hi(Sh), Lo(Sh))
  end else begin
    HLine(R.A.X, R.B.Y, R.B.X + 1, Color);
    VLine(R.B.X, R.A.Y, R.B.Y + 1, Color);
    HLine(R.B.X - Delta.X, R.A.Y, R.B.X + 1, Color);
    VLine(R.A.X, R.B.Y - Delta.Y, R.B.Y + 1, Color);
  end;
end;

procedure TNoteBookFrame.DrawTitle(var R: TRect; var Delta: TPoint; const Title: String; Sel: Boolean);
var
  NB: PNoteBook absolute Owner;
  C, CFrame, CTitle, CShadow, CBack: Word;
begin
  if Sel then begin
    CFrame := $02;
    CTitle := $0506;
  end else begin
    CFrame := $01;
    CTitle := $0304;
  end;
  CFrame := GetColor(CFrame);
  CTitle := GetColor(CTitle);

  CShadow := GetColor($0708);
  CBack := GetColor($09);

  C := Hi(CTitle);
  Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, 1, C, C);
  Bar(R.A.X-1, R.A.Y+1, R.B.X-1, R.B.Y-1, Lo(CTitle));
  if Sel then
    Rectangle(R.A.X, R.A.Y+1, R.B.X-1, R.B.Y-1, 1, Hi(CShadow), Lo(CShadow));
  WrStr((R.A.X + R.B.X - FontWidth(Title)) div 2,
        (R.A.Y + R.B.Y - FontHeight) div 2 + 1, Title, C);
end;

procedure TNoteBookFrame.Draw;
var
  NP, NG: integer;
  NB: PNoteBook absolute Owner;
  R, R1: TRect;
  i, j, k, c, TitleWidth : integer;
  PI: PImage;
  PR : ^TRect;
  D  : TPoint;

  procedure Drw(i: integer); begin
    GetRectangles(i, R, R1, D);
    if not R.Empty then DrawPage(R, D, i = j);
    if not R1.Empty then DrawTitle(R1, D, NB^.PageAt(i)^.Title^, i = j);
  end;

begin
  inherited Draw;
  NG := NB^.NumGroups; if NG = 0 then exit;
  NP := NB^.MaxPages;
  j := NB^.GroupForPage(NB^.PageIndex(NB^.Page));

  for i := 0 to j - 1 do Drw(i);
  for i := NP - 1 downto j + 1 do Drw(i);
  Drw(j);

  GetFirstRectangle(R);
  PI := GetImage(209);
  Dec(R.A.X, 2);
  Inc(R.A.Y, FontHeight div 2);
  k := BitMapHeight(PI) + FontHeight div 2;
  while R.A.Y < R.B.Y - BitMapHeight(PI) do begin
    PutBMP(PI, R.A.X, R.A.Y);
    Inc(R.A.Y, k);
  end;

  DrawPageTitle(R);
end;

procedure TNoteBookFrame.DrawPageTitle(var R: TRect);
var
  NB: PNoteBook absolute Owner;
  s: string;
  i, j, k : integer;
begin
  i := NB^.PagesInGroup(NB^.PageIndex(NB^.Page));
  k := NB^.PageIndex(NB^.Page);
  s := NB^.PageAt(NB^.GroupForPage(k))^.Title^;
  if (i > 1) then begin
    j := NB^.IndexInGroup(k);
    {$IFDEF RUSSIAN}
    s := s + ' - ���.' + Long2Str(j) + ' �� ' + Long2Str(i);
    {$ELSE}
    s := s + ' - Pg.' + Long2Str(j) + ' of ' + Long2Str(i);
    {$ENDIF}
  end;
  WrStr(R.B.X - FontWidth(s) - CharWidth - 2 * BitMapWidth(GetImage(7)) - 3,
        R.B.Y - (BitMapHeight(GetImage(7)) + FontHeight) div 2 - 1, s, GetColor($05));
end;

{
  ┌─────────────────────────────────────────────────────────╖
  │ TNoteBookHorizontalFrame                                ║
  ╘═════════════════════════════════════════════════════════╝
}
procedure TNoteBookHorizontalFrame.GetLastRectangle(var R: TRect);
var
  NB: PNoteBook absolute Owner;
  l, TitleWidth: integer;
begin
  TitleWidth := GetTitleWidth;
  l := NB^.NumGroups - 1;
  R.Assign(8 + FrameWidth + l * (WhiteSpace.X + 1), UpBarHeight + 4 * FontHeight,
    Size.X - {TitleWidth -} 2 * CharWidth - FrameWidth, Size.Y - FontHeight - FrameWidth - l * (WhiteSpace.Y + 1))
end;

procedure TNoteBookHorizontalFrame.GetFirstRectangle(var R: TRect);
var
  NB: PNoteBook absolute Owner;
  l: integer;
begin
  GetLastRectangle(R);
  l := NB^.NumGroups - 1;
  R.Move(- l * (WhiteSpace.X + 1), l * (WhiteSpace.Y + 1));
end;

procedure TNoteBookHorizontalFrame.GetRectangles(APage: integer; var PageRect, TitleRect: TRect; var Direction: TPoint);
var
  NB: PNoteBook absolute Owner;
  GroupNr, TitleNr, PageNr, LastGroup, OnTop, TitleWidth, TitlesOnPage : integer;
  R: TRect;
begin
  if NB^.PageAt(APage)^.Title = nil then begin
    FillChar(PageRect, SizeOf(PageRect), 0);
    TitleRect.Copy(PageRect);
    Exit;
  end;
  GetFirstRectangle(PageRect);
  GetLastRectangle(R);
  GetDirection(PageRect, R, Direction);

  GroupNr   := NB^.NumGroupsBefore(APage);
  OnTop     := NB^.NumGroupsBefore(NB^.PageIndex(NB^.Page));
  LastGroup := NB^.NumGroups - 1;

  TitleWidth   := GetTitleWidth;
  TitlesOnPage := (PageRect.B.X - PageRect.A.X - CharWidth - Direction.X) div TitleWidth;
  TitleNr      := GroupNr mod TitlesOnPage;
  if GroupNr >= OnTop then
    GroupNr := GroupNr - OnTop
  else begin
    GroupNr := LastGroup - GroupNr;
  end;

  PageRect.Move(Direction.X * GroupNr, Direction.Y * GroupNr);
  TitleRect.Assign(PageRect.A.X, PageRect.A.Y - 5 * FontHeight div 4 + 1, PageRect.A.X + TitleWidth, PageRect.A.Y);
  TitleRect.Move(CharWidth + TitleNr * TitleWidth, 1);
end;

procedure TNoteBookHorizontalFrame.DrawPage(var R: TRect; var Delta: TPoint; Sel: Boolean);
var
  Color, Sh: word;
begin
  if Sel then Color := GetColor($05) else Color := GetColor($03);
  if Sel then begin
    Sh := GetColor($0708);
    Rectangle(R.A.X, R.A.Y, R.B.X + 1, R.B.Y + 1, 1, Color, Color);
    Rectangle(R.A.X+1, R.A.Y+1, R.B.X, R.B.Y, 1, Hi(Sh), Lo(Sh));
  end else begin
    HLine(R.A.X, R.A.Y, R.B.X + 1, Color);
    VLine(R.B.X, R.A.Y, R.B.Y + 1, Color);
    HLine(R.B.X - Delta.X, R.B.Y, R.B.X + 1, Color);
    VLine(R.A.X, R.A.Y + 1, R.A.Y - Delta.Y, Color);
  end;
end;

procedure TNoteBookHorizontalFrame.DrawTitle(var R: TRect; var Delta: TPoint; const Title: String; Sel: Boolean);
var
  C, CFrame, CTitle, CShadow, CBack: Word;
begin
  if Sel then begin
    CFrame := $02;
    CTitle := $0506;
  end else begin
    CFrame := $01;
    CTitle := $0304;
  end;
  CFrame := GetColor(CFrame);
  CTitle := GetColor(CTitle);

  CShadow := GetColor($0708);
  CBack := GetColor($09);

  Bar(R.A.X, R.A.Y, R.B.X, R.B.Y, Lo(CTitle));
  C := Hi(CTitle);
  Rectangle(R.A.X, R.A.Y, R.B.X, R.B.Y, 1, C, C);
  Bar(R.A.X+1, R.A.Y+1, R.B.X-1, R.B.Y+1, Lo(CTitle));
  if Sel then
    Rectangle(R.A.X+1, R.A.Y+1, R.B.X-1, R.B.Y, 1, Hi(CShadow), Lo(CShadow));
  WrStr((R.A.X + R.B.X - FontWidth(Title)) div 2,
        (R.A.Y + R.B.Y - FontHeight) div 2, Title, C);
end;


{
  ┌─────────────────────────────────────────────────────────╖
  │ TListBook                                               ║
  ╘═════════════════════════════════════════════════════════╝
}

function TListBook.DataSize: word;
begin
  DataSize := SizeOf(PCollection);
end;

procedure TListBook.SetPageData(APage: integer; var Rec);
var
  Coll: PCollection absolute Rec;
  P: Pointer;
begin
  if Coll <> nil then begin
    P := Coll^.At(APage);
    if P <> nil then TDialog.SetData(P^);
  end;
end;

procedure TListBook.GetPageData(APage: integer; var Rec);
var
  Coll: PCollection absolute Rec;
  P: Pointer;
begin
  if Coll <> nil then begin
    P := Coll^.At(APage);
    if P <> nil then TDialog.GetData(P^);
  end;
end;

function TListBook.PageDataSize(APage: integer): word;
begin
  PageDataSize := 0;
end;

procedure TListBook.ViewsRemove;
begin
end;

procedure TListBook.ViewsInsert;
begin
end;

function TListBook.GetPageTitle(APage: integer; Item: Pointer): String;
begin
  GetPageTitle := Long2Str(APage);
end;

procedure TListBook.SetData(var Rec);
var
  Coll: PCollection absolute Rec;

  procedure AddPageHeader(P: Pointer); far;
  begin
    Pages^.Insert(New(PPage, Init(GetPageTitle(Coll^.IndexOf(P), P))));
  end;

begin
  if Assigned(Pages) and (Pages^.Count = 0) and Assigned(Coll) then begin
    Coll^.ForEach(@AddPageHeader);
  end;
  inherited SetData(Rec);
end;

function TListBook.InsertPage(APage: PPage): PPage;
begin
  InsertPage := APage;
end;

function TListBook.InsertView(APage: PPage; AView: PView): PView;
begin
  Insert(AView);
  InsertView := AView;
end;

begin
  RegisterImageInCode(209, @NoteBook_Hole);
end.
