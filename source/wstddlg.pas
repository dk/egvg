{
  Standard dialogs by Dmitry Karasik
  1996
}
Unit WStdDlg;

Interface

Uses Objects, Strings, Views, Dialogs, Drivers, EGFont, GDI,
     MsgBox, StdDlg, Bitmaps, TrueType, App, VESA, EGDPMI, HistList,
     Gadgets, Printers;


{Font line object}
Type
  PTestPanel = ^TTestPanel;
  TTestPanel = Object(TView)
    Constructor Init(var R : TRect);
    Procedure Draw; Virtual;
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

{Fonts selection listbox}
  PFontListBox = ^TFontListBox;
  TFontListBox = Object(TListBox)
    dFont : PFont;
    First : PAbstractFont;
    Constructor Init(var Bounds : TRect; AScrollBar : PScrollBar; AFont : PFont);
    procedure   DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt); Virtual;
    function    GetText(Item: Integer; MaxLen: Integer): String; virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    function    FontXLAT(Num : Integer; Direct : Boolean) : Integer;
  End;

  {font size selection listbox}
  PFontSizesListBox = ^TFontSizesListBox;
  TFontSizesListBox = Object(TListBox)
    dFont : PFont;
    Constructor Init(var Bounds : TRect; AScrollBar : PScrollBar; AFont : PFont);
    Destructor  Done; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   RefreshColl(NewFont : Integer);
    Function    AtSize(Height : Integer) : Integer;
  End;

  PTellingCheckBoxes = ^TTellingCheckBoxes;
  TTellingCheckBoxes = Object(TCheckBoxes)
    procedure Press(Item: Integer); virtual;
  End;

  PTellingRadioButtons = ^TTellingRadioButtons;
  TTellingRadioButtons = Object(TRadioButtons)
    procedure Press(Item: Integer); virtual;
    procedure HandleEvent(var Event : TEvent); Virtual;
  End;

  {Statndard object for font selection. }
  PFontDialog = ^TFontDialog;
  TFontDialog = Object(TDialog)
    SavePI : TFont;
    CBox : PTellingCheckBoxes;
    CPan : PTestPanel;
    CSiz : PFontSizesListBox;
    CLst : PFontListBox;
    Constructor Init;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Constructor Load(var S : TStream);
    Procedure Store(var S : TStream);
    Procedure SetData(var Rec); Virtual;
    Procedure GetData(var Rec); Virtual;
  End;

  {standard color-selection section}
  {color test panel}
  PColorPanel = ^TColorPanel;
  TColorPanel = Object(TView)
    Color      : PVGARegister;
    ColorsUsed : LongInt;
    UseDither  : Boolean;
    Constructor Init(var Bounds : TRect; PColor : PVGARegister);
    Procedure Draw; Virtual;
    Procedure HandleEvent(var Event : TEvent); Virtual;
  End;

  {red/green select panel}
  PRGPanel = ^TRGPanel;
  TRGPanel = Object(TView)
    PColor    : PVGARegister;
    UseDither : Boolean;
    BM        : PImage;
    ColorsUsed: Word;
    Constructor Init(var Bounds : TRect; APColor : PVGARegister);
    Constructor Load(var S : TStream);
    Destructor  Done; Virtual;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
    Procedure   Repaint;
  End;

  {blue select panel}
  PBPanel = ^TBPanel;
  TBPanel = Object(TView)
    PColor     : PVGARegister;
    ColorsUsed : LongInt;
    SelfTrack  : Boolean;
    UseDither  : Boolean;
    Constructor Init(var Bounds : TRect; APColor : PVGARegister);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Procedure   Draw; Virtual;
  End;

  {standard dialog for selecting colors}
  PColorDialog = ^TColorDialog;
  TColorDialog = Object(TDialog)
    Constructor Init;
    Constructor Load(var S : TStream);
    Procedure Store(var S : TStream);
    Procedure SetData(var Rec); Virtual;
    Procedure GetData(var Rec); Virtual;
  Private
    SaveColor : TVGARegister;
    CBox : PTellingCheckBoxes;
  End;

  {standard object for videodriver selection}
  PVideoDriverDialog = ^TVideoDriverDialog;
  TVideoDriverDialog = object(TDialog)
    Col, Res : PTellingRadioButtons;
    Buf      : PTellingCheckBoxes;
    Drv      : PScreenDriver;
    Constructor Init;
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Constructor Load(var S : TStream);
    Procedure Store(var S : TStream);
    Procedure SetData(var Rec); Virtual;
    Procedure GetData(var Rec); Virtual;
    Procedure Recalc;
  end;

  TPrintSetupParms = Record
    Orientation : Word;
    Quality     : WordBool;
    Size        : Word;
    Resolution  : Word;
    FormFeed    : WordBool;
    case Byte of
    0 : (PDSP : Integer);
    1 : (Printer : String);
  End;

  PPrintSetupDialog = ^TPrintSetupDialog;
  TPrintSetupDialog = Object(TDialog)
    COrient : PRadioButtons;
    CQuality : PRadioButtons;
    CSize, CRes, CPrn : PComboBox;
    CB1, CB2 : PStaticBitmap;
    CFF  : PCheckBoxes;
    OkSetPrnCaps : Boolean;
    constructor Init;
    constructor Load(var S: TStream);
    Procedure HandleEvent(var Event : TEvent); Virtual;
    Procedure GetData(var Rec); Virtual;
    Procedure SetData(var Rec); Virtual;
    procedure Store(var S: TStream);
  Private
    Procedure Reselect;
    Function  GetResNo(Res : Word; ResAcc : Boolean) : Word;
  End;


const
  RTestPanel: TStreamRec = (
     ObjType: 2010;
     VmtLink: Ofs(TypeOf(TTestPanel)^);
     Load:    @TTestPanel.Load;
     Store:   @TTestPanel.Store
  );

  RFontListBox: TStreamRec = (
     ObjType: 2011;
     VmtLink: Ofs(TypeOf(TFontListBox)^);
     Load:    @TFontListBox.Load;
     Store:   @TFontListBox.Store
  );

  RFontSizesListBox: TStreamRec = (
     ObjType: 2012;
     VmtLink: Ofs(TypeOf(TFontSizesListBox)^);
     Load:    @TFontSizesListBox.Load;
     Store:   @TFontSizesListBox.Store
  );

  RTellingCheckBoxes: TStreamRec = (
     ObjType: 2013;
     VmtLink: Ofs(TypeOf(TTellingCheckBoxes)^);
     Load:    @TTellingCheckBoxes.Load;
     Store:   @TTellingCheckBoxes.Store
  );

  RFontDialog: TStreamRec = (
     ObjType: 2014;
     VmtLink: Ofs(TypeOf(TFontDialog)^);
     Load:    @TFontDialog.Load;
     Store:   @TFontDialog.Store
  );

  RColorPanel: TStreamRec = (
     ObjType: 2015;
     VmtLink: Ofs(TypeOf(TColorPanel)^);
     Load:    @TColorPanel.Load;
     Store:   @TColorPanel.Store
  );

  RRGPanel: TStreamRec = (
     ObjType: 2016;
     VmtLink: Ofs(TypeOf(TRGPanel)^);
     Load:    @TRGPanel.Load;
     Store:   @TRGPanel.Store
  );

  RBPanel: TStreamRec = (
     ObjType: 2017;
     VmtLink: Ofs(TypeOf(TBPanel)^);
     Load:    @TBPanel.Load;
     Store:   @TBPanel.Store
  );

  RColorDialog: TStreamRec = (
     ObjType: 2018;
     VmtLink: Ofs(TypeOf(TColorDialog)^);
     Load:    @TColorDialog.Load;
     Store:   @TColorDialog.Store
  );

  RTellingRadioButtons: TStreamRec = (
     ObjType: 2019;
     VmtLink: Ofs(TypeOf(TTellingRadioButtons)^);
     Load:    @TTellingRadioButtons.Load;
     Store:   @TTellingRadioButtons.Store
  );

  RVideoDriverDialog : TStreamRec = (
    ObjType: 2020;
    VmtLink: Ofs(Typeof(TVideoDriverDialog)^);
    Load : @TVideoDriverDialog.Load;
    Store : @TVideoDriverDialog.Store);

  RPrinterSetupDialog : TStreamRec = (
    ObjType: 2021;
    VmtLink: Ofs(Typeof(TPrintSetupDialog)^);
    Load : @TPrintSetupDialog.Load;
    Store : @TPrintSetupDialog.Store);


procedure RegisterWStdDlg;
Procedure RegisterWStdDlgImages;

Implementation

Const
  cmFontChanged        = $1022;
  cmCheckButtonPressed = $1023;
  cmColorChanged       = $1024;
  cmTrapLoad           = $1025;
  cmAdd                = $1027;
  cmDel                = $1028;


Constructor TTestPanel.Init;
Begin
  Inherited Init(R);
  EventMask := $FFFF;
  Options := Options or ofPostProcess;
End;

Procedure TTestPanel.Draw;
Const
  {$IFDEF RUSSIAN}
  XLine = 'AaBbАаБб';
  {$ELSE}
  XLine = 'AaBbCcZz';
  {$ENDIF}
Var
  X, Y : Integer;
Begin
  Bar(0, 0, Size.X, Size.Y, MonoColor(GetColor($01), White));
  X := (Size.X - FontWidth(XLine)) div 2;
  Y := (Size.Y - FontHeight) div 2;
  WrStr(X, Y, XLine, 0);
  Rectangle(0, 0, Size.X, Size.Y, 1, 8, 15);
End;

Procedure TTestPanel.HandleEvent(var Event : TEvent);
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmFontChanged) then begin
    Font := TFont(Event.InfoPtr^);
    DrawView;
  end;
  if (Event.What = evBroadcast) and (Event.Command = cmTrapLoad) then begin
    Font := TFont(Event.InfoPtr^);
    DrawView;
  end;
End;


Constructor TFontListBox.Init;
Var
  I, J : Integer;
Begin
  Inherited Init(Bounds, 1, AScrollBar);
  J := 1;
  First := GetFont(1);
  for I := 2 to MaxFontsNum do if GetFont(I) <> First then Inc(J);
  SetRange(J);
  dFont := AFont;
  FocusItem(FontXLAT(dFont^.Font, False));
End;


function TFontListBox.GetText(Item: Integer; MaxLen: Integer): String;
Begin
  GetText := GetFont(FontXLAT(Item, True))^.GetName;
End;

procedure TFontListBox.DrawItem(Item, Indent: Integer; Bounds: TRect; Color: LongInt);
Var
  BM : PImage;
begin
  Inc(Bounds.A.X, 2 * Step.X);
  inherited DrawItem(Item, Indent, Bounds, Color);
  Dec(Bounds.A.X, 2 * Step.X);
  Inc(Bounds.A.Y, ((Bounds.B.Y - Bounds.A.Y) - GetCharWidth * 2) div 2);
  Bar(Bounds.A.X, Bounds.A.Y, 2 * Step.X, Bounds.B.Y, Lo(Color));
  if (GetFont(FontXLAT(Item, True))^.Flags and ffTrueType) <> 0 then begin
    BM := GetImage(250);
    if BM = GetImage(0) then begin
      WrStr(Bounds.A.X + 2, Bounds.A.Y + 1, 'T', 8);
      WrStr(Bounds.A.X + 4, Bounds.A.Y + 3, 'T', 0);
    end else begin
      SetColorBitBlt(ColorIndex^[4], True);
      PutBMPOp(BM, Bounds.A.X, Bounds.A.Y + FontHeight div 2 - BitmapHeight(BM) div 2, UserBitBlt);
    end;
  end;
  if (GetFont(FontXLAT(Item, True))^.Flags and ffBitmap) <> 0 then begin
    BM := GetImage(251);
    if (GetFont(FontXLAT(Item, True))^.Flags and ffWinRes) <> 0 then BM := GetImage(252);
    SetColorBitBlt(ColorIndex^[4], True);
    PutBMPOp(BM, Bounds.A.X, Bounds.A.Y + FontHeight div 2 - BitmapHeight(BM) div 2, UserBitBlt);
  end;
end;


function  TFontListBox.FontXLAT;
Var
  I, J : Integer;
Begin
  if Direct then begin
    J := 0;
    if Num = 0 then begin
      FontXLAT := 1;
      Exit;
    end;
    for I := 2 to MaxFontsNum do begin
      if GetFont(I) <> First then Inc(J);
      if J = Num then begin
        FontXLAT := I;
        Exit;
      end;
    end;
  end else begin
    J := 0;
    if Num = 1 then begin
      FontXLAT := 0;
      Exit;
    end;
    for I := 2 to MaxFontsNum do begin
      if GetFont(I) <> First then Inc(J);
      if I = Num then begin
        FontXLAT := J;
        Exit;
      end;
    end;
  end;
End;

Procedure TFontListBox.HandleEvent(var Event : TEvent);
Begin
  Inherited HandleEvent(Event);
  if Event.What = evBroadCast then case Event.Command of
  cmScrollBarChanged : if Event.InfoPtr = VScrollBar then begin
    ClearEvent(Event);
    dFont^.Font := FontXLAT(Focused, True);
    Message(Owner, evBroadcast, cmFontChanged, dFont);
  end;
  cmTrapLoad : dFont := Event.InfoPtr;
  else end;
End;


Constructor TFontSizesListBox.Init;
Var
  I, R, X : Integer;
Begin
  Inherited Init(Bounds, 1, AScrollBar);
  dFont := AFont;
  List := Nil;
  RefreshColl(dFont^.Font);
  if List^.Count > 0 then for I := 0 to List^.Count - 1 do begin
    Val(PString(List^.At(I))^, R, X);
    if R >= dFont^.Height then begin
      FocusItem(I);
      Break;
    end;
  end;
End;

Destructor TFontSizesListBox.Done;
Begin
  Dispose(List, Done);
  Inherited Done;
End;

Procedure  TFontSizesListBox.RefreshColl(NewFont : Integer);
Var
  F : PAbstractFont;
  R : Real;
  I : Word;
  S : String;
Begin
  if List <> nil then Dispose(List, Done);
  F := GetFont(NewFont);
  New(PStringCollection(List), Init(5, 25));

  if (F^.Flags and (ffScaled + ffStroked)) <> 0 then begin
    {if (F^.Flags and ffTrueType) <> 0 then R := 16 else R := 9;}
    R := 7.6;
    while R < 100 do begin
      R := R * 1.1;
      Str(Round(R), S);
      List^.AtInsert(List^.Count, NewStr(S));
    end;
  end else begin
    for I := 1 to F^.CapsNumber do begin
      Str(F^.CapsHeight(I), S);
      List^.AtInsert(List^.Count, NewStr(S));
    end;
  end;
  SetRange(List^.Count);
End;


Function  TFontSizesListBox.AtSize(Height : Integer) : Integer;

Var
  K : Integer;

Function Check(P : PString) : Boolean; Far;
Var
  I, J : Integer;
Begin
  Val(P^, I, J);
  Check := I >= Height;
  Inc(K);
End;

Var
  P : Pointer;

Begin
  K := -1;
  if List^.Count < 2 then AtSize := 0 else begin
    P := List^.FirstThat(@Check);
    if P <> Nil then AtSize := K else AtSize := List^.Count - 1;
  end;
End;

Procedure TFontSizesListBox.HandleEvent(var Event : TEvent);
Var
  I : Integer;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evBroadCast then case Event.Command of
  cmScrollBarChanged : if Event.InfoPtr = VScrollBar then begin
    ClearEvent(Event);
    Val(PString(List^.At(Focused))^, dFont^.Height, I);
    if (GetFont(dFont^.Font)^.Flags and (ffTrueType or ffBGI)) = 0 then
      dFont^.Width := 0 else
      dFont^.Width := Round(dFont^.Height / 1.16);
    Message(Owner, evBroadcast, cmFontChanged, dFont);
  end;
  cmTrapLoad : dFont := Event.InfoPtr;
  else end;

  if (Event.What = evBroadcast) and (Event.Command = cmFontChanged) then begin
    RefreshColl(dFont^.Font);
    Val(PString(List^.At(Focused))^, dFont^.Height, I);
    if (GetFont(dFont^.Font)^.Flags and (ffTrueType or ffBGI)) = 0 then
      dFont^.Width := 0 else
      dFont^.Width := Round(dFont^.Height / 1.16);
    DrawView;
  end;
End;

Procedure TTellingCheckBoxes.Press(Item : Integer);
Begin
  Inherited Press(Item);
  Message(Owner, evBroadcast, cmCheckButtonPressed, @Self);
End;

Procedure TTellingRadioButtons.Press(Item : Integer);
Begin
  Inherited Press(Item);
  Message(Owner, evBroadCast, cmRadioButtonPressed, @Self);
End;

procedure TTellingRadioButtons.HandleEvent(var Event : TEvent);
var
  Is : Boolean;
Begin
  Is := (State and sfFocused <> 0) and (Event.What = evKeyDown) and
     (Event.ScanCode in [Hi(kbLeft), Hi(kbRight), Hi(kbUp), Hi(kbDown)]);
  Inherited HandleEvent(Event);
  if Is then Message(Owner, evBroadCast, cmRadioButtonPressed, @Self);
End;


Constructor TFontDialog.Init;
Var
  C: PView;
  R: TRect;
  E : TEvent;
Begin
  R.Assign(13, 54, 327, 290);
  Inherited Init(R, {$IFDEF RUSSIAN}'Шрифт'{$ELSE}'Font'{$ENDIF});

  DefaultFont(SavePI);
  Options := Options or ofCentered;

  R.Assign(156, 40, 170, 106);
  C := New(PSCrollBar, Init(R));
  Insert(C);
  R.Assign(8, 40, 156, 106);
  CLst := New(PFontListBox, Init(R, PScrollBar(C), @SavePI));
  Insert(CLst);
  R.Assign(8, 23, 60, 39);
  Insert(New(PLabel, Init(R, {$IFDEF RUSSIAN}'Шрифт'{$ELSE}'Font'{$ENDIF}, CLst)));


  R.Assign(213, 40, 225, 106);
  C := New(PScrollBar, Init(R));
  Insert(C);
  R.Assign(181, 40, 213, 106);
  CSiz := New(PFontSizesListBox, Init(R, PScrollBar(C), @SavePI));
  Insert(CSiz);
  R.Assign(181, 23, 231, 39);
  Insert(New(PLabel, Init(R, {$IFDEF RUSSIAN}'Размер'{$ELSE}'Size'{$ENDIF}, CSiz)));


  R.Assign(242, 40, 287, 72);
  Insert(MakeOKButton(R, cmOK, bfDefault));
  R.Assign(242, 74, 287,106);
  Insert(MakeCancelButton(R, cmCancel, bfNormal));
  R.Assign(236, 108, 305, 139);
  Insert(New(PButton, Init(R, {$IFDEF RUSSIAN}'Доб~а~вить'{$ELSE}'~A~dd'{$ENDIF}, cmAdd, bfNormal)));
  R.Assign(236, 142, 305, 173);
  Insert(New(PButton, Init(R, {$IFDEF RUSSIAN}'~У~далить'{$ELSE}'~D~elete'{$ENDIF}, cmDel, bfNormal)));


  R.Assign(8, 120, 235, 166);
  CBox := New(PTellingCheckBoxes, Init(R,
              {$IFDEF RUSSIAN}
              NewSItem('Наклон',
              NewSItem('Жирный',
              NewSItem('Подчеркнутый',
              NewSItem('Перечеркнутый',
              nil))))));
              {$ELSE}
              NewSItem('Italic',
              NewSItem('Bold',
              NewSItem('Underlined',
              NewSItem('Struck out',
              nil))))));
              {$ENDIF}

  Insert(CBox);
  with PCheckBoxes(CBox)^ do begin
    if SavePI.Italic <> 0 then Value := Value or 1;
    if (SavePI.Style and ftBold) <> 0 then Value := Value or 2;
    if (SavePI.Style and ftUnderlined) <> 0 then Value := Value or 4;
    if (SavePI.Style and ftStruckOut) <> 0 then Value := Value or 8;
  end;

  R.Assign(8, 162, 100, 178);
  Insert(New(PStaticText, Init(R, {$IFDEF RUSSIAN}'Образец'{$ELSE}'Sample'{$ENDIF})));
  R.Assign(10, 180, 302, 220);
  CPan := New(PTestPanel, Init(R));
  Insert(CPan);

  E.What := evBroadcast;
  E.Command := cmFontChanged;
  E.InfoPtr := @SavePI;
  Application^.PutEvent(E);

  SelectNext(False);
End;

Constructor TFontDialog.Load(var S : TStream);
Begin
  Inherited Load(S);
  GetSubViewPtr(S, CBox);
  GetSubViewPtr(S, CPan);
  GetSubViewPtr(S, CSiz);
  GetSubViewPtr(S, CLst);
  Message(@Self, evBroadcast, cmTrapLoad, @SavePI);
  Message(@Self, evBroadcast, cmCheckButtonPressed, CBox);
End;

Procedure TFontDialog.Store(var S : TStream);
Begin
  Inherited Store(S);
  PutSubViewPtr(S, CBox);
  PutSubViewPtr(S, CPan);
  PutSubViewPtr(S, CSiz);
  PutSubViewPtr(S, CLst);
End;

Procedure TFontDialog.SetData(var Rec);
Var
  H : Integer;
Begin
  SavePI := TFont(Rec);
  with PCheckBoxes(CBox)^ do begin
    if SavePI.Italic <> 0 then Value := Value or 1;
    if (SavePI.Style and ftBold) <> 0 then Value := Value or 2;
    if (SavePI.Style and ftUnderlined) <> 0 then Value := Value or 4;
    if (SavePI.Style and ftStruckOut) <> 0 then Value := Value or 8;
  end;
  H := SavePI.Height;
  CLst^.FocusItem(CLst^.FontXLAT(SavePI.Font, False));
  SavePI.Height := H;
  CSiz^.FocusItem(CSiz^.AtSize(SavePI.Height));
  Message(@Self, evBroadcast, cmFontChanged, @SavePI);
  Redraw;
End;

Procedure TFontDialog.GetData(var Rec);
Begin
  TFont(Rec) := SavePI;
End;

Procedure TFontDialog.HandleEvent(var Event : TEvent);
Var
  S : String;
  F, F1 : PAbstractFont;
  I : Integer;
  P : Pointer;
Begin
  if (Event.What = evBroadcast) and (Event.Command = cmFontChanged) then begin
    CLst^.HandleEvent(Event);
    CSiz^.HandleEvent(Event);
    CPan^.HandleEvent(Event);
    ClearEvent(Event);
  end;

  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmCheckButtonPressed) then
    with PCheckBoxes(Event.InfoPtr)^ do begin
      if (Value and 1) <> 0 then SavePI.Italic := 20 else SavePI.Italic := 0;
      if (Value and 2) <> 0 then SavePI.Style := SavePI.Style or ftBold else
                             SavePI.Style := SavePI.Style and not ftBold;
      if (Value and 4) <> 0 then SavePI.Style := SavePI.Style or ftUnderlined else
                             SavePI.Style := SavePI.Style and not ftUnderlined;
      if (Value and 8) <> 0 then SavePI.Style := SavePI.Style or ftStruckOut else
                             SavePI.Style := SavePI.Style and not ftStruckOut;
      ClearEvent(Event);
      Message(@Self, evBroadcast, cmFontChanged, @SavePI);
  end;

  if Event.What = evCommand then case Event.Command of
  cmAdd : begin
    S := '*.FON';
    HistoryAdd(0, '*.TTF; True Type Font');
    HistoryAdd(0, '*.FON; Windows Font');
    if Application^.ExecuteDialog(New(PFileDialog, Init(S, {$IFDEF RUSSIAN}'Шрифт'{$ELSE}'Font'{$ENDIF},
    {$IFDEF RUSSIAN}'~И~мя'{$ELSE}'~N~ame'{$ENDIF}, fdOkButton, 0)), @S) <> cmCancel then begin
      F := LoadFont(S);
      P := @S;
      if F = Nil then begin
        MessageBox({$IFDEF RUSSIAN}'Невозможно загрузить шрифт %s'
        {$ELSE}'Cannot load font %s'{$ENDIF}, @P, mfError + mfOkButton)
      end else if GetFontByName(F^.GetName, True) <> nil then begin
        S := F^.GetName;
        if MessageBox({$IFDEF RUSSIAN}'Шрифт %s уже установлен. Заменить существующий?'
        {$ELSE}'Font %s is already installed. Replace existing?'{$ENDIF}, @P, mfError + mfYesNoCancel) = cmYes then begin
          F1 := GetFontByName(F^.GetName, True);
          for I := 1 to MaxFontsNum do begin
            if GetFont(I) = F1 then begin
              DisposeFont(I);
              RegisterFont(F, I);
              if I = 1 then
                F^.Flags := F^.Flags and not (ffScaled + ffStroked);
              Application^.Redraw;
              Message(@Self, evBroadcast, cmFontChanged, @SavePI);
              Break;
            end;
          end;
        end else Dispose(F, Done);
      end else begin
        I := GetFreeFontID;
        RegisterFont(F, I);
        CLst^.SetRange(CLst^.Range + 1);
        CLst^.DrawView;
        CLst^.FocusItem(CLst^.FontXLAT(I, False));
        Application^.Redraw;
      end;
    end;
  end;
  cmDel : if CLst^.Range > 0 then begin
    if CLst^.FontXLAT(CLst^.Focused, True) = 1 then
            MessageBox({$IFDEF RUSSIAN}'Нельзя удалять системный шрифт'
      {$ELSE}'Cannot delete system font'{$ENDIF}, nil, mfError + mfOkButton)
    else if MessageBox(
  {$IFDEF RUSSIAN}'Удалить шрифт '{$ELSE}'Delete font '{$ENDIF} +
  + CLst^.GetText(CLst^.Focused, 255) + '?', nil, mfConfirmation + mfOkCancel
   ) = cmOk then begin
      DisposeFont(CLst^.FontXLAT(CLst^.Focused, True));
      CLst^.SetRange(CLst^.Range - 1);
      CLst^.DrawView;
      SavePI.Font := CLst^.FontXLAT(CLst^.Focused, True);
      Message(@Self, evBroadcast, cmFontChanged, @SavePI);
      Application^.Redraw;
    end;
  end;
  else end;
End;

Const
  cdDither   = 1;
  cd16Colors = 2;

Constructor TColorPanel.Init;
Begin
  Inherited Init(Bounds);
  Color := PColor;
  Eventmask := evBroadcast;
  ColorsUsed := 256;
  UseDither := MaxColors <= 256;
End;

Procedure TColorPanel.Draw;
var
  S : String[3];
  L : Word;
  CW, XC : LongInt;
Begin
  L := 40;
  CW := CharWidth;
  Bar(L, 0, Size.X, Size.Y, 7);
  Rectangle(0, 0, Size.X, Size.Y, 1, 0, 0);
  if UseDither then begin
    if MaxColors = 2 then
     MonoDither(Color^[1], Color^[2], Color^[3], PaintInfo)
    else
    MakeDither(Color^[1], Color^[2], Color^[3], ColorsUsed, PaintInfo, Nil);
    BarStyle(1, 1, L, Size.Y-1);
  end else begin
    if MaxColors > 65536 then
    XC := TrueColor(Color^[1], Color^[2], Color^[3]) else
    if MaxColors > 256 then
    XC := HiColor(Color^[1], Color^[2], Color^[3]) else
    XC := RGBIndirect(Color^[1], Color^[2], Color^[3], 0, ColorsUsed-1, Nil);
    if MaxColors > 256 then begin
      PaintInfo.Fore := XC;
      BarStyle(1, 1, L, Size.Y-1);
    end else Bar(1, 1, L, Size.Y-1, XC);
  end;
  Rectangle(1, 1, L, Size.Y-1, 1, 8, 15);
  DefaultPaint(PaintInfo);
  Str(Color^[1], S);
  WrStr(L + 4, 4, 'R:' + S, 0);
  Str(Color^[2], S);
  WrStr(L + 6 + CW * 5, 4, 'G:' + S, 0);
  Str(Color^[3], S);
  WrStr(L + 8 + CW * 10, 4, 'B:' + S, 0);
End;

Procedure TColorPanel.HandleEvent(var Event : TEvent);
Begin
  Inherited HandleEvent(Event);
  case Event.Command of
  cmColorChanged : DrawView;
  cmCheckButtonPressed : with PCheckBoxes(Event.InfoPtr)^ do begin
    DefaultPaint(Self.PaintInfo);
    UseDither := (Value and cdDither) <> 0;
    if (Value and cd16Colors) <> 0 then ColorsUsed := 16 else ColorsUsed := 256;
    Self.DrawView;
  end;
  cmTrapLoad: Color := Event.InfoPtr;
  else end;
End;


Constructor TRGPanel.Init;
Var
  T : TPaintInfo;
Begin
  Inherited Init(Bounds);
  Eventmask := $FFFF;
  PColor := APColor;
  BM := Nil;
  BM := CreateDImage(128, 128);
  if BM = Nil then Fail;
  ColorsUsed := 256;
  UseDither := MaxColors <= 256;
  Repaint;
  Options := Options or ofSelectable{ or ofPreprocess};
  LogPalette.Mode := LogPalette.Mode or pmHiColor;
End;

Constructor TRGPanel.Load(var S : TStream);
Begin
  Inherited Load(S);
  BM := CreateDImage(128, 128);
  if BM = Nil then Fail;
End;

Destructor TRGPanel.Done;
Begin
  FreeDImage(BM);
  Inherited Done;
End;


Procedure TRGPanel.Repaint;
Var
  T : TPaintInfo;
  R, G : Byte;
Begin
  DefaultPaint(T);
  T.Device := BM;
  T.ClipRect.Assign(0, 0, 128, 128);
  SetMouseWait;

  if UseDither then begin
    for R := 0 to 31 do for G := 0 to 31 do begin
      if MaxColors = 2 then
      MonoDither(R shl 3, G shl 3, 0, T) else
      MakeDither(R shl 3, G shl 3, 0, ColorsUsed, T, Nil);
      GDI.BarStyle(R shl 2, G shl 2, (R + 1) shl 2, (G + 1) shl 2, T);
    end;
  end else begin
    for R := 0 to 15 do for G := 0 to 15 do begin
      if MaxColors > 65536 then T.Fore := TrueColor(R shl 4, G shl 4, 0) else
      if MaxColors > 256 then T.Fore := HiColor(R shl 4, G shl 4, 0) else
      T.Fore := RGBIndirect(R shl 4, G shl 4, 0, 0, ColorsUsed-1, Nil);
      GDI.BarStyle(R shl 3, G shl 3, (R + 1) shl 3, (G + 1) shl 3, T);
    end;
  end;
End;

Procedure TRGPanel.Draw;
Begin
  PutBMP(BM, 0, 0);
  Rectangle(PColor^[1] shr 1 - 2, PColor^[2] shr 1 - 2,
            PColor^[1] shr 1 + 2, PColor^[2] shr 1 + 2, 1, 0, 0);
End;

Procedure TRGPanel.HandleEvent(var Event : TEvent);
Var
  Mouse : TPoint;
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evMouseDown) or (Event.What = evMouseAuto) then begin
    MakeLocal(Event.Where, Mouse);
    PColor^[1] := Mouse.X shl 1;
    PColor^[2] := Mouse.Y shl 1;
    ClearEvent(Event);
    Message(Owner, evBroadcast, cmColorChanged, nil);
    DrawView;
  end;
  if Event.What = evKeyDown then begin
    case Event.KeyCode of
    kbUp   : if PColor^[2] > 0   then Dec(PColor^[2]);
    kbDown : if PColor^[2] < 255 then Inc(PColor^[2]);
    kbLeft : if PColor^[1] > 0   then Dec(PColor^[1]);
    kbRight: if PColor^[1] < 255 then Inc(PColor^[1]);
    else end;
    if Hi(Event.KeyCode) in [Hi(kbUp), Hi(kbDown), Hi(kbLeft), Hi(kbRight)] then begin
      ClearEvent(Event);
      Message(Owner, evBroadcast, cmColorChanged, nil);
      DrawView;
    end;
  end;
  if Event.Command = cmCheckButtonPressed then with PCheckBoxes(Event.InfoPtr)^ do begin
    DefaultPaint(Self.PaintInfo);
    UseDither := (Value and cdDither) <> 0;
    if (Value and cd16Colors) <> 0 then ColorsUsed := 16 else ColorsUsed := 256;
    Repaint;
    Self.DrawView;
  end;
  if (Event.What = evBroadcast) and (Event.Command =  cmTrapLoad) then
    PColor := Event.InfoPtr;
End;

Constructor TBPanel.Init;
Begin
  Inherited Init(Bounds);
  Eventmask := $FFFF;
  PColor := APColor;
  UseDither := True;
  ColorsUsed := 256;
  Options := Options or ofSelectable;
  SelfTrack := False;
End;


Procedure TBPanel.Draw;
Var
  B : Byte;
Begin
  for B := 0 to 31 do begin
    if MaxColors = 2 then
      MonoDither(PColor^[1], PColor^[2], B shl 3, PaintInfo)
    else
    if MaxColors <= 256 then begin
      if UseDither then
        MakeDither(PColor^[1], PColor^[2], B shl 3, ColorsUsed, PaintInfo, Nil)
     else PaintInfo.Fore := RGBIndirect(PColor^[1], PColor^[2], B shl 3, 0, ColorsUsed-1, Nil);
    end else
    if MaxColors <= 65536 then
      PaintInfo.Fore := HiColor(PColor^[1], PColor^[2], B shl 3)
    else PaintInfo.Fore := TrueColor(PColor^[1], PColor^[2], B shl 3);
    BarStyle(1, B shl 2 + 2, Size.X-1, (B + 1) shl 2 + 2);
  end;
  Rectangle(0, 0, Size.X, Size.Y, 1, 7, 7);
  Rectangle(0, PColor^[3] shr 1, Size.X, PColor^[3] shr 1 + 4, 1, 0, 0);
End;

Procedure TBPanel.HandleEvent(var Event : TEvent);
Var
  Mouse : TPoint;
Begin
  Inherited HandleEvent(Event);
  if Event.What = evBroadcast then case Event.Command of
  cmColorChanged : if not SelfTrack then DrawView;
  cmTrapLoad: PColor := Event.InfoPtr;
  else end;

  if (Event.What = evMouseDown) or (Event.What = evMouseAuto) then begin
    MakeLocal(Event.Where, Mouse);
    PColor^[3] := Mouse.Y shl 1;
    ClearEvent(Event);
    SelfTrack := True;
    Message(Owner, evBroadcast, cmColorChanged, nil);
    SelfTrack := False;
    DrawView;
  end;
  if Event.Command = cmCheckButtonPressed then with PCheckBoxes(Event.InfoPtr)^ do begin
    DefaultPaint(Self.PaintInfo);
    UseDither := (Value and cdDither) <> 0;
    if (Value and cd16Colors) <> 0 then ColorsUsed := 16 else ColorsUsed := 256;
    Self.DrawView;
  end;
  if Event.What = evKeyDown then begin
    case Event.KeyCode of
    kbUp   : if PColor^[3] > 0   then Dec(PColor^[3]);
    kbDown : if PColor^[3] < 255 then Inc(PColor^[3]);
    else end;
    if Hi(Event.KeyCode) in [Hi(kbUp), Hi(kbDown)] then begin
      ClearEvent(Event);
      SelfTrack := True;
      Message(Owner, evBroadcast, cmColorChanged, nil);
      SelfTrack := False;
      DrawView;
    end;
  end;
End;

Constructor TColorDialog.Init;
Var
  R : TRect;
  Control : PView;
  I, J, K, L : Byte;
Begin
  R.Assign(13, 54, 340, 290);
  Inherited Init(R, {$IFDEF RUSSIAN}'Цвета'{$ELSE}'Colors'{$ENDIF});

  FillChar(SaveColor, 3, 0);
  R.Assign(8, 40, 136, 168);
  Control := New(PRGPanel, Init(R, @SaveColor));
  Insert(Control);
  R.Assign(8, 24, 136, 39);
  Insert(New(PLabel, Init(R, {$IFDEF RUSSIAN}'Красный/Зеленый'{$ELSE}'Red/Green'{$ENDIF}, Control)));

  R.Assign(146, 40, 160, 168);
  Control := New(PBPanel, Init(R, @SaveColor));
  Insert(Control);
  R.Assign(140, 24, 180, 39);
  Insert(New(PLabel, Init(R, {$IFDEF RUSSIAN}'Синий'{$ELSE}'Blue'{$ENDIF}, Control)));

  R.Assign(8, 190, 180, 222);
  Control := New(PColorPanel, Init(R, @SaveColor));
  Insert(Control);
  R.Assign(8, 171, 80, 188);
  Insert(New(PLabel, Init(R, {$IFDEF RUSSIAN}'Образец'{$ELSE}'Sample'{$ENDIF}, Control)));

  Options   := Options or ofCentered;

  R.Assign(255, 40, 300, 72);
  Insert(MakeOKButton(R, cmOK, bfDefault));
  R.Assign(255, 74, 300,106);
  Insert(MakeCancelButton(R, cmCancel, bfNormal));

  R.Assign(170, 150, 312, 190);
  CBox := New(PTellingCheckBoxes, Init(R,
              {$IFDEF RUSSIAN}
              NewSItem('Смешанные цвета',
              NewSItem('16 чистых цветов', nil))));
              {$ELSE}
              NewSItem('Mixed colors',
              NewSItem('16 pure colors', nil))));
              {$ENDIF}
  Insert(CBox);
  PCheckBoxes(CBox)^.Value := cdDither;
  if (MaxColors > 256) or (MaxColors = 2) then CBox^.SetState(sfDisabled, True);

  SelectNext(False);
  LogPalette.Mode := pmUseRGB;
End;

Constructor TColorDialog.Load(var S : TStream);
Begin
  Inherited Load(S);
  GetSubViewPtr(S, CBox);
  Message(@Self, evBroadcast, cmTrapLoad, @SaveColor);
  Message(@Self, evBroadcast, cmCheckButtonPressed, CBox);
End;

Procedure TColorDialog.Store(var S : TStream);
Begin
  Inherited Store(S);
  PutSubViewPtr(S, CBox);
End;

Procedure TColorDialog.SetData(var Rec);
Begin
  SaveColor := TVGARegister(Rec);
  Redraw;
End;

Procedure TColorDialog.GetData(var Rec);
Begin
  TVGARegister(Rec) := SaveColor;
End;

constructor TVideoDriverDialog.Init;
var
  R: TRect;
  Control : PView;
begin
  R.Assign(19*8, 4*16, 61*8, 17*16);
  {$IFDEF RUSSIAN}
  Inherited Init(R, 'Видео драйвера');
  {$ELSE}
  Inherited Init(R, 'Video drivers');
  {$ENDIF}
  Options := Options or ofCentered;

  RAssign(R, 2, 2, 14, 8);
  Col := New(PTellingRadioButtons, Init(R,
  NewSItem('Mono',
  NewSItem('16',
  NewSItem('256',
  NewSItem('32768',
  NewSItem('65536',
  NewSItem('16 M', Nil))))))));
  Control := Col;
  Insert(Control);

  RAssign(R, 1, 1, 8, 2);
  {$IFNDEF RUSSIAN}
  Insert(New(PLabel, Init(R, '~C~olors', Control)));
  {$ELSE}
  Insert(New(PLabel, Init(R, '~Ц~вета', Control)));
  {$ENDIF}

  RAssign(R, 16, 2, 31, 9);
  Res := New(PTellingRadioButtons, Init(R,
  NewSItem('320x200',
  NewSItem('640x400',
  NewSItem('640x480',
  NewSItem('800x600',
  NewSItem('1024x768',
  NewSItem('1280x1024',
  NewSItem('1600x1200', Nil)))))))));
  Control := Res;
  Insert(Control);

  RAssign(R, 15, 1, 26, 2);
  {$IFNDEF RUSSIAN}
  Insert(New(PLabel, Init(R, '~R~esolution', Control)));
  {$ELSE}
  Insert(New(PLabel, Init(R, '~Р~азрешение', Control)));
  {$ENDIF}

  RAssign(R, 2, 10, 31, 11);
  Buf := New(PTellingCheckboxes, Init(R,
  {$IFNDEF RUSSIAN}
  NewSItem('~B~uffers in videomemory', Nil)));
  {$ELSE}
  NewSItem('~Б~уферы в видеопамяти', Nil)));
  {$ENDIF}
  Control := Buf;
  Insert(Control);

  RAssign(R, 32, 2, 44, 4);
  Control := MakeOKButton(R, cmOK, bfDefault);
  Insert(Control);
  RAssign(R, 32, 4, 44, 6);
  Control := MakeCancelButton(R, cmCancel, bfNormal);
  Insert(Control);

  SelectNext(False);
  Recalc;
end;

Procedure   TVideoDriverDialog.HandleEvent(var Event : TEvent);
Begin
  Inherited HandleEvent(Event);
  if (Event.What = evBroadCast) and (Event.Command = cmRadioButtonPressed)
  then Recalc;
End;

Constructor TVideoDriverDialog.Load(var S : TStream);
Begin
  Inherited Load(S);
  GetSubViewPtr(S, Col);
  GetSubViewPtr(S, Res);
  GetSubViewPtr(S, Buf);
End;

Procedure TVideoDriverDialog.Store(var S : TStream);
Begin
  Inherited Store(S);
  PutSubViewPtr(S, Col);
  PutSubViewPtr(S, Res);
  PutSubViewPtr(S, Buf);
End;

Procedure TVideoDriverDialog.SetData(var Rec);
Var
  W : Word;
Begin
  Drv := PScreenDriver(Rec);
  if Drv = Nil then Drv := ScreenDriver;
  if Drv = Nil then Drv := DefaultDriver;
  if Drv^.NumberOfColors = 2 then W := 0 else
  if Drv^.NumberOfColors = 16 then W := 1 else
  if Drv^.NumberOfColors = 256 then W := 2 else
  if Drv^.NumberOfColors = 32768 then W := 3 else
  if Drv^.NumberOfColors = 65536 then W := 4 else
  if Drv^.NumberOfColors = 16777216 then W := 5;
  Col^.Setdata(W);

  case Drv^.ScreenHeight of
  200 : W := 0;
  400 : W := 1;
  480 : W := 2;
  600 : W := 3;
  768 : W := 4;
 1024 : W := 5;
  else end;
  Res^.Setdata(W);
  Recalc;
End;

Procedure TVideoDriverDialog.GetData(var Rec);
Begin
  PScreenDriver(Rec) := Drv;
End;

Procedure TVideoDriverDialog.Recalc;
Var
  P    : PScreenDriver;
  W, B : Word;
  X : LongInt;
  T : TSVGABoardInfo;
  Bufmem : Boolean;
  ModeList : array[0..100] of Word;

Function Support : Boolean;
Var
  I : Word;
  V : TVESAModeInfo;
Begin
  Support := False;
  if P^.ID < $100 then begin
    if (P^.ID > $10) and (VideoCard < VGA) then Exit;
  end else begin
    if VideoCard < SVGA then Exit;
    I := 0;
    while (ModeList[I] <> $FFFF) and (I < 100) do if ModeList[I] = P^.ID then begin
      {
      GetVesaModeInfo(P^.ID, V);
      if (V.Attribute and vmiaSupported) = 0 then Exit;
      }
      Support := True;
      Exit;
    end else Inc(I);
    Exit;
  end;
  Support := True;
End;

Var
  Sel : Word;

Begin
  Lock;
  GetVesaBoardInfo(T);
  {$IFDEF DPMI}
  GetSelectorForRealMem(T.ModeList, 200, Sel);
  Move(Mem[Sel:0], ModeList, 200);
  FreeLDTDescriptor(Sel);
  {$ELSE}
  Move(T.ModeList^, ModeList, 200);
  {$ENDIF}

  Col^.SetButtonState($FFFF, False);
  Res^.SetButtonState($FFFF, False);
  Buf^.SetButtonState($FFFF, False);
  P := ScreenDrivers;


  W := 0;
  while P <> Nil do begin
    if P^.NumberOfColors = 2 then W := W or 1 else
    if P^.NumberOfColors = 16 then W := W or 2 else
    if P^.NumberOfColors = 256 then W := W or 4 else
    if P^.NumberOfColors = 32768 then W := W or 8 else
    if P^.NumberOfColors = 65536 then W := W or 16 else
    if P^.NumberOfColors = 16777216 then W := W or 32;
    P := P^.NextDriver;
  end;
  Col^.SetButtonState(W, True);

  case Col^.Value of
  0 : X := 2;
  1 : X := 16;
  2 : X := 256;
  3 : X := 32768;
  4 : X := 65536;
  5 : X := 16777216;
  else end;

  case Res^.value of
  0 : begin W := 320; B := 200; end;
  1 : begin W := 640; B := 400; end;
  2 : begin W := 640; B := 480; end;
  3 : begin W := 800; B := 600; end;
  4 : begin W :=1024; B := 768; end;
  5 : begin W :=1280; B :=1024; end;
  6 : begin W :=1600; B :=1200; end;
  else end;
  Drv := GetAppropriateDriver(W, B, X, False);

  case Col^.Value of
  0 : B := 1;
  1 : B := 4;
  2 : B := 8;
  3 : B := 16;
  4 : B := 16;
  5 : B := 24;
  else end;

  P := ScreenDrivers;
  W := 0;
  while P <> Nil do begin
    if (P^.NumberOfColors = X) and Support then begin
      case P^.ScreenHeight of
      200 : W := W or 1;
      400 : W := W or 2;
      480 : W := W or 4;
      600 : W := W or 8;
      768 : W := W or 16;
     1024 : W := W or 32;
     1200 : W := W or 64;
      else end;
      Bufmem := LongInt(P^.ScreenWidth) * 2 * P^.ScreenHeight * B div 8192
        <= VideoBoardMem;
      if (P^.ID = $13) then BufMem := False;
      if P = Drv then Buf^.SetButtonState(1, Bufmem);
    end;
    P := P^.NextDriver;
  end;
  Res^.SetButtonState(W, True);
  DrawView;
  UnLock;
End;

constructor TPrintSetupDialog.Init;
var
  R: TRect;
  Control : PView;
  SItem : PSItem;
  I : Integer;

Function  AddSItems(P : PPrinter) : Boolean; Far;
Var
  AItem : PSItem;
Begin
  if SItem = Nil then SItem := NewSItem(P^.Name, Nil) else begin
    AItem := SItem;
    while AItem^.Next <> Nil do AItem := AItem^.Next;
    AItem^.Next := NewSItem(P^.Name, Nil);
  end;
  AddSItems := False;
End;

Function  Act(AP : PPrinter) : Boolean; Far;
Begin
  Act := AP = CurrentPrinter;
  Inc(I);
End;


begin
  Desktop^.RAssign(R, 13, 6, 67, 18);
  {$IFDEF RUSSIAN}
  inherited Init(R, 'Установки принтера');
  {$ELSE}
  inherited Init(R, 'Printer setup');
  {$ENDIF}

  RAssign(R, 3, 2, 5, 5);
  CB1 := New(PStaticBitmap, Init(R, New(PBitmap, Init(253)), Nil, True));
  Insert(CB1);
  CB2 := New(PStaticBitmap, Init(R, New(PBitmap, Init(254)), Nil, True));
  Insert(CB2);

  RAssign(R, 9, 2, 29, 4);
  COrient := New(PTellingRadioButtons, Init(R,
  {$IFDEF RUSSIAN}
  NewSItem('Книжная',
  NewSItem('Альбомная', Nil))));
  {$ELSE}
  NewSItem('Portrait',
  NewSItem('Landscape', Nil))));
  {$ENDIF}
  Insert(COrient);
  RAssign(R, 9, 1, 26, 2);
  {$IFDEF RUSSIAN}
  Insert(New(PLabel, Init(R, '~О~риентация листа', COrient)));
  {$ELSE}
  Insert(New(PLabel, Init(R, 'Paper ~o~rientation', COrient)));
  {$ENDIF}

  CB2^.SetState(sfVisible, COrient^.Value = 0);
  CB1^.SetState(sfVisible, COrient^.Value = 1);

  RAssign(R, 30, 2, 52, 4);
  CQuality := New(PRadioButtons, Init(R,
  {$IFDEF RUSSIAN}
  NewSItem('Черновик',
  NewSItem('Высокое качество', Nil))));
  {$ELSE}
  NewSItem('Low quality',
  NewSItem('High quality', Nil))));
  {$ENDIF}
  Insert(CQuality);
  RAssign(R, 30, 1, 46, 2);
  {$IFDEF RUSSIAN}
  Insert(New(PLabel, Init(R, '~К~ачество печати', CQuality)));
  {$ELSE}
  Insert(New(PLabel, Init(R, 'Print ~q~uality', CQuality)));
  {$ENDIF}

  RAssign(R, 2, 6, 11, 7);
  Inc(R.B.Y, 3);
  CSize := New(PComboBox, Init(R,
  NewSItem('A1',
  NewSItem('A2',
  NewSItem('A3',
  NewSItem('A4',
  NewSItem('A5',
  NewSItem('A6',
  NewSItem('A7', Nil)))))))));
  Insert(CSize);
  RAssign(R, 2, 5, 16, 6);
  {$IFDEF RUSSIAN}
  Insert(New(PLabel, Init(R, '~Р~азмер бумаги', CSize)));
  {$ELSE}
  Insert(New(PLabel, Init(R, 'Paper si~z~e', CSize)));
  {$ENDIF}

  RAssign(R, 18, 6, 32, 7);
  Inc(R.B.Y, 3);
  CRes := New(PComboBox, Init(R,
  NewSItem(' 75 dpi',
  NewSItem('120 dpi',
  NewSItem('150 dpi',
  NewSItem('200 dpi',
  NewSItem('300 dpi',
  NewSItem('600 dpi', Nil))))))));
  Insert(CRes);
  RAssign(R, 17, 5, 35, 6);
  {$IFDEF RUSSIAN}
  Insert(New(PLabel, Init(R, 'Р~а~зрешение печати', CRes)));
  {$ELSE}
  Insert(New(PLabel, Init(R, 'Printer ~r~esolution', CRes)));
  {$ENDIF}

  RAssign(R, 31, 8, 50, 9);
  CFF := New(PCheckBoxes, Init(R, NewSItem(
    {$IFDEF RUSSIAN}'Подача страницы'{$ELSE}'Form feed'{$ENDIF}, Nil)));
  Insert(CFF);


  RAssign(R, 2, 9, 51, 10);
  Inc(R.B.Y, 3);
  SItem := Nil;
  EnumPrinters(@AddSItems);
  CPrn := New(PComboBox, Init(R, SItem));
  Insert(CPrn);
  RAssign(R, 2, 8, 19, 9);
  {$IFDEF RUSSIAN}
  Insert(New(PLabel, Init(R, '~Д~райвер принтера', CPrn)));
  {$ELSE}
  Insert(New(PLabel, Init(R, 'Printer ~d~river', CPrn)));
  {$ENDIF}
  I := 0;
  if EnumPrinters(@Act) = Nil then CPrn^.Value := -1 else CPrn^.Value := I - 1;

  RAssign(R, 35, 5, 45, 7);
  Control := MakeOKButton(R, cmOK, bfDefault);
  Insert(Control);
  RAssign(R, 44, 5, 53, 7);
  Control := MakeCancelButton(R, cmCancel, bfNormal);
  Insert(Control);

  SelectNext(False);
  OkSetPrnCaps := True;
  Reselect;
end;


constructor TPrintSetupDialog.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S, COrient);
  GetSubViewPtr(S, CQuality);
  GetSubViewPtr(S, CSize);
  GetSubViewPtr(S, CRes);
  GetSubViewPtr(S, CB1);
  GetSubViewPtr(S, CB2);
  GetSubViewPtr(S, CFF);
  CB2^.SetState(sfVisible, COrient^.Value = 0);
  CB1^.SetState(sfVisible, COrient^.Value = 1);
  S.Read(OkSetPrnCaps, 1);
  Reselect;
end;

Procedure TPrintSetupDialog.HandleEvent(var Event : TEvent);
Var
  PDC : PPrinter;

Function Act(P : PPrinter) : Boolean; Far;
Begin
  Act := String(CPrn^.Strings.At(CPrn^.Value)^) = P^.Name;
End;

Begin
  if (Event.What = evCommand) and (Event.Command = cmOK) and OkSetPrnCaps then begin
    if (CPrn^.Value >= 0) and (CPrn^.Value < CPrn^.Strings.Count) then begin
      PDC := EnumPrinters(@Act);
      PDC^.SetPaperSize(TPaperSize(CSize^.Value), TOrientation(COrient^.Value));
      PDC^.SetQuality(WordBool(CQuality^.Value));
      PDC^.SetFormFeed(WordBool(CFF^.Value));
      PDC^.SetResolution(GetResNo(CRes^.Value, False), GetResNo(CRes^.Value, False));
      CurrentPrinter := PDC;
    end;
  end;
  Inherited HandleEvent(Event);
  if (Event.What = evBroadcast) and (Event.Command = cmRadioButtonPressed) then begin
    if Event.InfoPtr = COrient then begin
      CB2^.SetState(sfVisible, COrient^.Value = 0);
      CB1^.SetState(sfVisible, COrient^.Value = 1);
    end;
    if Event.InfoPtr = CPrn then Reselect;
  end;
End;

Function TPrintSetupDialog.GetResNo(Res : Word; ResAcc : Boolean) : Word;
Begin
  if ResAcc then begin
  case Res of
  0..119  : GetResNo := 0;
  120..149 : GetResNo := 1;
  150..199 : GetResNo := 2;
  200..299 : GetResNo := 3;
  300..599 : GetResNo := 4;
  600..1000 : GetResNo := 5;
  else end;
  end else begin
  case Res of
  0   : GetResNo := 75;
  1   : GetResNo := 120;
  2   : GetResNo := 150;
  3   : GetResNo := 200;
  4   : GetResNo := 300;
  5   : GetResNo := 600;
  else end;
  end;
End;


Procedure TPrintSetupDialog.SetData;
Var
  P : PPrinter;
  I : Word;

Function  Act(AP : PPrinter) : Boolean; Far;
Begin
  Act := AP^.Name = TPrintSetupParms(Rec).Printer;
  Inc(I);
End;

Begin
  TPrintSetupParms(Rec).Resolution := GetResNo(TPrintSetupParms(Rec).Resolution, True);
  I := 0;
  P := EnumPrinters(@Act);
  if P = Nil then TPrintSetupParms(Rec).PDSP := -1 else
    TPrintSetupParms(Rec).PDSP := I - 1;
  Inherited SetData(Rec);
  CB2^.SetState(sfVisible, COrient^.Value = 0);
  CB1^.SetState(sfVisible, COrient^.Value = 1);
  Reselect;

  TPrintSetupParms(Rec).Resolution := GetResNo(TPrintSetupParms(Rec).Resolution, False);
  if Integer(CPrn^.Value) < 0 then TPrintSetupParms(Rec).Printer := '' else
  TPrintSetupParms(Rec).Printer := String(CPrn^.Strings.At(CPrn^.Value)^);
End;

Procedure TPrintSetupDialog.GetData;
Begin
  Inherited GetData(Rec);
  TPrintSetupParms(Rec).Resolution := GetResNo(TPrintSetupParms(Rec).Resolution, False);
  if Integer(CPrn^.Value) < 0 then TPrintSetupParms(Rec).Printer := '' else
  TPrintSetupParms(Rec).Printer := String(CPrn^.Strings.At(CPrn^.Value)^);
End;


procedure TPrintSetupDialog.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S, COrient);
  PutSubViewPtr(S, CQuality);
  PutSubViewPtr(S, CSize);
  PutSubViewPtr(S, CRes);
  PutSubViewPtr(S, CB1);
  PutSubViewPtr(S, CB2);
  PutSubViewPtr(S, CFF);
  S.Write(OkSetPrnCaps, 1);
end;

Procedure TPrintSetupDialog.Reselect;
Var
  P : PPrinter;
  S : String;
  T : TPrinterCaps;
  A : array[0..3] of PCluster;
  I, J : Integer;
  Mask : LongInt;

Function  Act(AP : PPrinter) : Boolean; Far;
Begin
  Act := AP^.Name = S;
End;

Begin
  if (CPrn^.Strings.Count > 0) and (CPrn^.Value < CPrn^.Strings.Count) then
    S := String(CPrn^.Strings.At(CPrn^.Value)^) else S := '';
  P := EnumPrinters(@Act);
  A[0] := COrient;
  A[1] := CQuality;
  A[2] := CSize;
  A[3] := CRes;
  for I := 0 to 3 do A[I]^.SetState(sfDisabled, P = Nil);
  if P = Nil then Exit;

  for I := 0 to 2 do begin
    T.Cap := TPrinterCapSelection(I);
    P^.GetDevCaps(T);
    A[I]^.EnableMask := 0;
    if T.Entries > 0 then for J := 0 to T.Entries - 1 do
      A[I]^.EnableMask := A[I]^.EnableMask or (1 shl T.List[J]);
    if (A[I]^.EnableMask and (1 shl A[I]^.Value) = 0) then A[I]^.Value := T.List[0];
    A[I]^.DrawView;
  end;

  T.Cap := pcsResolution;
  P^.GetDevCaps(T);
  A[3]^.EnableMask := 0;
  if T.Entries > 0 then for J := 0 to T.Entries - 1 do
    A[3]^.EnableMask := A[3]^.EnableMask or (1 shl GetResNo(T.List[J], True));
  if (A[3]^.EnableMask and (1 shl A[3]^.Value) = 0) then
    A[3]^.Value := GetResNo(T.List[0], True);
  A[3]^.DrawView;

  T.Cap := pcsFormFeed;
  P^.GetDevCaps(T);
  CFF^.SetState(sfDisabled, T.Entries = 0);
End;


procedure RegisterWStdDlg;
begin
  RegisterType(RTestPanel);
  RegisterType(RFontListBox);
  RegisterType(RFontSizesListBox);
  RegisterType(RTellingCheckBoxes);
  RegisterType(RFontDialog);
  RegisterType(RColorPanel);
  RegisterType(RRGPanel);
  RegisterType(RBPanel);
  RegisterType(RColorDialog);
  RegisterType(RVideoDriverDialog);
  RegisterType(RPrinterSetupDialog);
end;

{$L FOLDERS.OBJ}
Procedure Font_TTF; External;
Procedure Font_CPI; External;
Procedure Font_Win; External;
Procedure Prn_Portrait; External;
Procedure Prn_Landscape; External;

Procedure RegisterWStdDlgImages;
Begin
  {$IFDEF UseStandardBitMaps}
  RegisterImageInCode(250, @Font_TTF);
  RegisterImageInCode(251, @Font_CPI);
  RegisterImageInCode(252, @Font_Win);
  RegisterImageInCode(253, @Prn_Portrait);
  RegisterImageInCode(254, @Prn_Landscape);
  {$ENDIF}
End;

begin
  RegisterWStdDlgImages;
End.