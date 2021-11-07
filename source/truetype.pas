{Interface to M.Koppe stroked fonts library.
 Accessibility and adtiidioitic pills made by DK Inc. 1996 }
Unit TrueType;

{$DEFINE USEFONDEMANDLOAD}
{$DEFINE USETTFDEMANDLOAD}
{$DEFINE USEFONDEMANDSCALABLE}

Interface

Uses DOS, Objects, GDI, EGFont, MFonts, GR, TTMyf, TTF, EgString;

Const
  {parameters to select inside .fon}
  pcNumber = 0;
  pcSize   = 1;
  pcHeight = 2;

Type
  PSmallCaps = ^TSmallCaps;
  TSmallCaps = Record
    F          : TFont;
    Number     : Byte;
    Handle     : Word;
  End;
  PCapsArray = ^TCapsArray;
  TCapsArray = array[1..200] of TSmallCaps;

  TMultiaccessFont = Object(TAbstractFont)
    Justify  : TPoint;
    FName    : PString;
    Font     : PFontRec;
    Handle   : Integer;
    LastCaps : TSmallCaps;
    Constructor Init;
    Function    GetCharWidth(C:Char): integer;              virtual;
    Function    GetWidth(const s: string): integer;         virtual;
    Function    GetHeight: integer;                         virtual;
    Function    WrStr(x, y, Len : integer; const s: string; var T : TPaintInfo): integer; virtual;
    Procedure   SetJustify(Horiz, Vert : Integer);
    Procedure   Select; Virtual;
    Procedure   SelectCaps(var F : TFont); Virtual;
    Procedure   GetCaps(var Caps : TSmallCaps; var F : TFont);
    Function    EqualCaps(var Caps : TSmallCaps) : Boolean;
    Function    GetResourceName : String; Virtual;
  End;

  PWindowsFont = ^TWindowsFont;
  TWindowsFont = Object(TMultiaccessFont)
    Cap      : PCapsArray;
    CapCount : Word;
    CapNo    : Word;
    Name     : PString;
    Constructor Init(const AFName : String);
    Destructor  Done; Virtual;
    Procedure   SelectCaps(var F : TFont); Virtual;
    Function    GetName : String; Virtual;
    Function    CapsNumber : Word; Virtual;
    Function    CapsHeight(CapsNo : Word) : Word; Virtual;
  End;

  TCharAcc = Record
    Width  : Word;
    Offset : Word;
  End;
  TFontTblArray = array[Byte] of TCharAcc;
  PFontTblArray = ^TFontTblArray;


  PScaledWindowsFont = ^TScaledWindowsFont;
  TScaledWindowsFont = Object(TWindowsFont)
    Constructor Init(const AFName : String);
    Destructor Done; Virtual;
    Procedure SelectCaps(var F : TFont); Virtual;
    Function  GetCharWidth(C:Char): integer;              virtual;
    Function  GetWidth(const s: string): integer;         virtual;
    Function  GetHeight: integer;                         virtual;
    Function  WrStr(x, y, Len : integer; const s: string; var T : TPaintInfo): integer; virtual;
  private
    Bits    : PLongArray;
    Scale   : TScaleRec;
    CMap    : PByteArray;
    CTbl    : PFontTblArray;
    CWid    : PWordArray;
    TblSize : Word;
    Procedure FreeBitmaps;
    Procedure CheckShared;
  End;

  PTrueTypeFont = ^TTrueTypeFont;
  TTrueTypeFont = Object(TMultiaccessFont)
    Matrix : TTransMatrix;
    Constructor Init(const AFName : String);
    Destructor  Done; Virtual;
    Procedure   SelectCaps(var F : TFont); Virtual;
    Function    GetName : String; Virtual;
    Function    CapsNumber : Word; Virtual;
    Function    GetMinSize : Word; Virtual;
  End;

  PBGIFont = ^TBGIFont;
  TBGIFont = Object(TMultiaccessFont)
    Constructor Init(const AFName : String; Width, Height, Attr : Word);
    Destructor  Done; Virtual;
    Procedure   SelectCaps(var F : TFont); Virtual;
    Function    GetName : String; Virtual;
    Function    CapsNumber : Word; Virtual;
  End;

  PBIOSFont = ^TBIOSFont;
  TBIOSFont = Object(TMultiaccessFont)
    Fonts    : array[0..2] of Word;
    Heights  : array[0..2] of Word;
    CapCount : Byte;
    Constructor Init;
    Procedure   SelectCaps(var F : TFont); Virtual;
    Function    CapsNumber : Word; Virtual;
    Function    CapsHeight(CapsNo : Word) : Word; Virtual;
    Function    GetName : String; Virtual;
  End;

Function LoadFont(FileName : String) : PAbstractFont;

Implementation

Constructor TMultiaccessFont.Init;
Begin
  Inherited Init;
  DefaultFont(LastCaps.F);
End;

Function TMultiaccessFont.GetCharWidth(C:Char): integer;
Begin
  GetCharWidth := TextWidth(C);
End;

Function TMultiaccessFont.GetWidth(const s: string): integer;
Begin
  GetWidth := TextWidth(S);
End;

Function TMultiaccessFont.GetHeight: integer;
Begin
  GetHeight := TextHeight('M');
End;

Procedure TMultiaccessFont.SetJustify(Horiz, Vert : Integer);
Begin
  Justify.X := Horiz;
  Justify.Y := Vert;
End;

Function  TMultiaccessFont.WrStr(x, y, Len : integer; const s: string; var T : TPaintInfo): integer;
Begin
  GRT := T;
  GRF := LastCaps.F;
  LocMaximalX := T.ClipRect.B.X;
  LocMaximalY := T.ClipRect.B.Y;
  SetTextParams(Handle, LastCaps.F.GapLength, T.Fore, False);
  SetTextJustify(Justify.X, Justify.Y);
  OutTextXY(x,y,s);
End;

Procedure   TMultiaccessFont.Select;
Begin
  if Handle > 0 then SetTextParams(Handle, 0, 0, False);
End;

Procedure   TMultiaccessFont.SelectCaps(var F : TFont);
Begin
  LastCaps.F := F;
End;

Procedure   TMultiaccessFont.GetCaps(var Caps : TSmallCaps; var F : TFont);
Begin
  Caps.F      := F;
  Caps.Number := 0;
End;

Function   TMultiaccessFont.EqualCaps;
Begin
  EqualCaps :=  (Caps.F.Height    = LastCaps.F.Height) and
                (Caps.F.Width     = LastCaps.F.Width) and
                (Caps.F.Italic    = LastCaps.F.Italic) and
                (Caps.F.Style     = LastCaps.F.Style) and
                (Caps.F.GapLength = LastCaps.F.GapLength);
End;

Function    TMultiaccessFont.GetResourceName : String;
Begin
  GetResourceName := FName^;
End;

Constructor TWindowsFont.Init;
Var
  MinAcc, I, AccNo : Integer;
  TempCaps : PCapsArray;
  Face : String;

Begin
  Inherited Init;
  Handle := -1;
  Flags := ffWinres + ffBitmap;
  CapCount := 0;
  GetMem(TempCaps, SizeOf(TSmallCaps) * 200);
  if TempCaps = nil then Fail;
  repeat
    Font := LoadWinFont(AFName, CapCount + 1, 0, 0, 0, 0, 0, 0, Face);
    if Font = nil then Break;
    Inc(CapCount);
    with Font^.TextMetric do begin
      FillChar(TempCaps^[CapCount], SizeOf(TSmallCaps), 0);
      TempCaps^[CapCount].F.Height   := tmHeight;
      TempCaps^[CapCount].F.Width    := tmAveCharWidth;
      TempCaps^[CapCount].F.Italic   := tmItalic;
      TempCaps^[CapCount].Number     := CapCount;
      TempCaps^[CapCount].Handle     := DefFont(Font);
    end;
  until CapCount >= 200;
  Name := NewStr(Face);
  if CapCount = 0 then begin
    FreeMem(TempCaps, SizeOf(TSmallCaps) * 200);
    Fail;
  end;
  GetMem(Cap, SizeOf(TSmallCaps) * CapCount);
  if Cap = Nil then begin
    CapCount := 0;
    FreeMem(TempCaps, SizeOf(TSmallCaps) * 200);
    Fail;
  end;
  Move(TempCaps^, Cap^, SizeOf(TSmallCaps) * CapCount);
  FreeMem(TempCaps, SizeOf(TSmallCaps) * 200);
  FName := NewStr(AFName);
  SetJustify(BottomText, TopText);
  Handle := Cap^[1].Handle;
  Font   := GetFontRec(Handle);
  LastCaps := Cap^[1];
  CapNo := 1;
End;

Destructor  TWindowsFont.Done;
Var
  I : Word;
Begin
  if Name <> Nil then DisposeStr(Name);
  if CapCount > 0 then for I := 1 to CapCount do FreeFont(Cap^[I].Handle);
  FreeMem(Cap, SizeOf(TSmallCaps) * CapCount);
  DisposeStr(FName);
  Font := Nil;
  Inherited Done;
End;

Function TWindowsFont.GetName;
Begin
  if Name = Nil then GetName := '' else GetName := Name^;
End;

Procedure TWindowsFont.SelectCaps;
Label Here;
var
  Caps      : TSmallCaps;
  I, AccNo  : Integer;
  MinCaps   : LongInt;
  Dif       : LongInt;
Begin
  GetCaps(Caps, F);
  if CapCount = 1 then Goto Here;
  if EqualCaps(Caps) then Goto Here;
  MinCaps := MaxLongInt;
  AccNo := 1;
  if Caps.F.Height = 0 then Caps.F.Height := Cap^[1].F.Height;
  for I := 1 to CapCount do begin
    Dif := Abs(Sqr(LongInt(Caps.F.Height - Cap^[I].F.Height))){ +
           Sqr(LongInt(Caps.Width  - Cap^[I].Width)) +
           Sqr(LongInt(Caps.Weight - Cap^[I].Weight))};
    if MinCaps > Dif then begin
      AccNo := I;
      MinCaps := Dif;
    end;
  end;
  if AccNo = CapNo then Goto Here;
  CapNo := AccNo;
  I := AccNo;
  Handle := Cap^[I].Handle;
  Font   := GetFontRec(Handle);
Here:
  SetTextParams(Handle, LastCaps.F.GapLength, 0, False);
  SetJustify(BottomText, TopText);
  LastCaps := Caps;
End;

Function  TWindowsFont.CapsNumber : Word;
Begin
  CapsNumber := CapCount;
End;

Function  TWindowsFont.CapsHeight(CapsNo : Word) : Word;
Begin
  CapsHeight := Cap^[CapsNo].F.Height;
End;

Constructor TScaledWindowsFont.Init;
Var
  I : Integer;
Begin
  Inherited Init(AFName);
  if CapCount = 0 then Fail; {inherited fail}
  FillScaled(scale, LastCaps.F.Width, LastCaps.F.Height, LastCaps.F.Width, LastCaps.F.Height, 0, 0);
  with Font^.TextMetric do TblSize := tmLastChar - tmFirstChar + 1;
  if Font^.FontMode = ftWin then Flags := Flags or ffScaled;
  if (Flags and ffScaled) <> 0 then begin
    GetMem(Bits, TblSize * SizeOf(Pointer));
    FillChar(Bits^, TblSize * SizeOf(Pointer), 0);
  end;
  CMap := nil;
  CTbl := nil;
  if (Flags and ffScaled) <> 0 then begin
    CheckShared;
    GetMem(CWid, TblSize * SizeOf(Word));
    CMap := SharedList^[Font^.FontHandle].P;
    CTbl := Pointer(LongInt(CMap) + 256);
    for I := 0 to TblSize - 1 do CWid^[I] := CTbl^[I].Width;
  end;
End;

Destructor TScaledWindowsFont.Done;
Begin
  FreeBitmaps;
  if CWid <> nil then FreeMem(CWid, TblSize * SizeOf(Word));
  if Bits <> nil then FreeMem(Bits, TblSize * SizeOf(Pointer));
  Inherited Done;
End;

Function  BPLW(Width : Word) : Word; Inline(
  $58/          {pop ax}
  $48/          {dec ax}
  $C1/$E8/$03/  {shr ax, 3}
  $40           {inc ax}
);

Procedure TScaledWindowsFont.FreeBitmaps;
Var
  I : Byte;
Begin
  if (Flags and ffScaled) = 0 then Exit;
  for I := 0 to TblSize - 1 do if Pointer(Bits^[I]) <> nil then begin
    FreeMem(Pointer(Bits^[I]), BPLW(CWid^[I] + Scale.Desc) * LastCaps.F.Height);
    Pointer(Bits^[I]) := Nil;
  end;
  FillChar(CWid^, TblSize * SizeOf(Word), 0);
End;

Procedure TScaledWindowsFont.CheckShared;
Begin
  if (Flags and ffScaled) = 0 then Exit;
  If SharedList^[Font^.FontHandle].P = nil then begin
    ReloadWinFont(Font);
    CMap := SharedList^[Font^.FontHandle].P;
    CTbl := Pointer(LongInt(CMap) + 256);
  end;
End;

Function  TScaledWindowsFont.GetCharWidth(C : Char): integer;
Begin
  if (Flags and ffScaled) <> 0 then begin
    CheckShared;
    GetCharWidth := Scale.Desc + CWid^[CMap^[Byte(C)]];
  end else GetCharWidth := Inherited GetCharWidth(C);
End;

Function  TScaledWindowsFont.GetWidth(const s: string): integer;
Var
  I, J, K : Word;
Begin
  if (Flags and ffScaled) <> 0 then begin
    I := Byte(S[0]);
    if I = 0 then GetWidth := 0 else begin
      CheckShared;
      K := 0;
      for J := 1 to I do Inc(K, CWid^[CMap^[Byte(S[J])]] + LastCaps.F.GapLength);
      GetWidth := K - LastCaps.F.Gaplength + Scale.Desc;
    end;
  end else GetWidth := Inherited GetWidth(S);
End;

Function  TScaledWindowsFont.GetHeight : Integer;
Begin
  if (Flags and ffScaled) <> 0 then GetHeight := LastCaps.F.Height
    else GetHeight := Inherited GetHeight;
End;


Procedure TScaledWindowsFont.SelectCaps;
Label Here;
Var
  C : TSmallCaps;
  I, Org : Integer;
  MulX : Real;

Function  _EqualCaps : Boolean;
Begin
  _EqualCaps :=  (C.F.Height = LastCaps.F.Height) and
                (C.F.Width  = LastCaps.F.Width) and
                (C.F.Italic = LastCaps.F.Italic) and
                (C.F.Style  = LastCaps.F.Style);
End;


Begin
  if (Flags and ffScaled) = 0 then begin
    Inherited SelectCaps(F);
    Exit;
  end;

  GetCaps(C, F);
  if _EqualCaps then Goto Here;
  Org := 1;
  {defaults/proportions}
  if C.F.Height = 0 then begin
    if C.F.Width = 0 then begin
      C.F.Width  := Cap^[1].F.Width;
      C.F.Height := Cap^[1].F.Height;
    end else begin
      if C.F.Width > Cap^[1].F.Width then
        for Org := CapCount downto 1 do if C.F.Width >= Cap^[Org].F.Width then Break;
      C.F.Height := Cap^[Org].F.Height * C.F.Width div Cap^[Org].F.Width;
    end;
  end else begin
    if C.F.Height > Cap^[1].F.Height then
      for Org := CapCount downto 1 do
        if (C.F.Height >= Cap^[Org].F.Height) and ((C.F.Width = 0)
        or (C.F.Width >= Cap^[Org].F.Width)) then Break;
    if C.F.Width = 0 then C.F.Width := Cap^[Org].F.Width * C.F.Height div Cap^[Org].F.Height;
  end;
  if ((F.Style and ftItalic) <> 0) and (F.Italic = 0) then C.F.Italic := fiItalic
    else C.F.Italic := F.Italic;
  if _EqualCaps then Goto Here;
  FreeBitmaps;
  if Org <> CapNo then begin
    Handle := Cap^[Org].Handle;
    Font   := GetFontRec(Handle);
    CapNo  := Org;
    CheckShared;
    CMap := SharedList^[Font^.FontHandle].P;
    CTbl := Pointer(LongInt(CMap) + 256);
  end;
  FillScaled(Scale, Cap^[Org].F.Width, Cap^[Org].F.Height, C.F.Width, C.F.Height, C.F.Style, C.F.Italic);
  if (F.Width <= 0) and (Scale.IncW <> 0) then begin
    {при embolding возникает расширение символа, и если ширина задана
    то приходится дополнительно его сжимать по ширине, а если нет -
    можно восстановить оригинальную пропорцию}
    Inc(C.F.Width, Scale.IncW);
    FillScaled(Scale, Cap^[Org].F.Width, Cap^[Org].F.Height, C.F.Width, C.F.Height, C.F.Style, C.F.Italic);
  end;
  Scale.BPL  := BPLW(Font^.Textmetric.tmMaxCharWidth);
  MulX := C.F.Width / Cap^[CapNo].F.Width;
  for I := 0 to TblSize - 1 do CWid^[I] := Round(CTbl^[I].Width * MulX);
Here:
  LastCaps := C;
End;

Function TScaledWindowsFont.WrStr(x, y, Len : integer; const s: string; var T : TPaintInfo): integer;
var
  i : integer;
  ww, l, l1 : integer;
  ind: word;
  CurFont : PByteArray;
  yf: integer;
  yh: integer;
  xout: integer;
  xbeg, xend, dlen: integer;
  xp: integer;
  Msk: byte;
  First: Integer;
  Special : array[0..1023] of Byte;
  sd:String;

Procedure GetBitmap(Index : Byte);
Begin
  if Scale.NoSc then CurFont := @CMap^[CTbl^[Index].Offset + Font^.BitmapDelta] else begin
    if Pointer(Bits^[Index]) = Nil then begin
      GetMem(Pointer(Bits^[Index]), BPLW(CWid^[Index] + Scale.Desc) * LastCaps.F.Height);
      ScaleMap(Scale, @CMap^[CTbl^[Index].Offset + Font^.BitmapDelta], Pointer(Bits^[Index]),
        {Cap^[CapNo].F.Width}
        CTbl^[Index].Width, Cap^[CapNo].F.Height, CWid^[Index], LastCaps.F.Height);
    end;
    CurFont := Pointer(Bits^[Index]);
  end;
End;


begin
  if (Flags and ffScaled) = 0 then begin
    Inherited WrStr(x, y, Len, S, T);
    Exit;
  end;

  if Len = 0 then Exit;
  CheckShared;
  WrStr := x;
  for I := 1 to Len do sd[I] := Char(CMap^[Byte(s[I])]);
  sd[0] := Char(Len);

  dlen := 0;
  for I := 1 to Len do Inc(dlen, CWid^[Byte(Sd[I])] + LastCaps.F.GapLength);
  Inc(dlen, Scale.Desc - LastCaps.F.GapLength);

  { Absolute no write checks }
  if (T.ClipRect.B.Y<=y) or (T.ClipRect.A.Y>=y+lastCaps.F.Height) then Exit;
  xout := x + dlen;
  if (T.ClipRect.B.X<=x) or (T.ClipRect.A.X>=xout) then Exit;

  { Vertical clip calculating }
  if T.ClipRect.A.Y>y then yf:=T.ClipRect.A.Y-y else yf:=0;
  if T.ClipRect.B.Y<=y+LastCaps.F.Height then yh := (T.ClipRect.B.Y-y)-yf else yh := LastCaps.F.Height-yf;
  { if yh=0 then Exit; }

  { Horizontal clip calculating }
  if T.ClipRect.A.X>x then xbeg:=T.ClipRect.A.X-x else xbeg:=0;
  if T.ClipRect.B.X<=xout then xend := (T.ClipRect.B.X-x) else xend := xout-x;
  { xbeg показывает, с какого пиксела относительно x мы можем начать
    реальный вывод строки, а xend - по какой пиксел относительно x
    мы должны закончить вывод. Теперь нам нужно усечь все символы,
    полностью помещающиеся в промежуток x <=  < x+xbeg, и все символы,
    полностью помещающиеся в промежуток x+xend <  <= xout, вывести
    правую часть самого левого помещающегося символа, левую часть
    самого правого помещающегося символа и полность остальные символы }

  Inc(xbeg, x);
  Inc(xend, x);

  { Perform possible output }
  { Расчет левого выводимого символа }
  First := 0;
  if x < xbeg then begin
    while x < xbeg do
      if x + CWid^[Byte(sd[Succ(First)])] + LastCaps.F.GapLength > xbeg then Break
        else begin
          inc(First);
          inc(x, CWid^[Byte(sd[First])] + LastCaps.F.GapLength);
        end;
    if (Scale.Desc > 0) and (First > 0) then begin
      Dec(x, CWid^[Byte(sd[First])] + LastCaps.F.GapLength);
      Dec(First);
    end;
  end;
  Inc(First);
  if First > Len then Exit;

  for i:=First to Len do begin
   GetBitmap(Byte(sd[i]));
   if x >= xend then Break;
   ww := 0;
   ind := 0;
   while ww < CWid^[Byte(sd[i])] + Scale.Desc do begin
      l := CWid^[Byte(sd[i])] + Scale.Desc - ww;
      if l > 8 then l := 8;
      if x < xbeg then begin
        if x + l <= xbeg then begin
          { Вообще не выводить }
        end else if x>=xend then begin
          { Вообще не выводить }
        end else begin
          { Частичный вывод }
          if x >= 0 then begin
            if x + l >= xend then l1 :=xend - x else l1 := l;
            if l1 > 0 then begin
              {Msk := $FF SHR (xbeg - x);}
              Msk := ($FF SHR (xbeg-x)) xor ($FF SHR l1);
              DisplayXx8Op(x, y+yf, @(CurFont^[ind+yf]), Msk, yh, T);
            end;
          end else begin
            { Особый вариант !  Сверхособый вариант ! }
            for l1:=0 to Pred(yh) do
              Special[l1] := BYTE(CurFont^[ind+yf+l1] SHL (-x) and (1 shl (8 - xbeg and 7) - 1));
            l1 := l + x;
            if l1 > xend then l1 := xend;
            if l1 > 0 then
              DisplayXx8Op(0, y+yf, @Special,  $FF xor ((Word(1) shl (8 - l1)) - 1), yh, T);
          end;
        end;
      end else begin
        { Слева нет усечения }
        if x >= xend then begin
          { Вообще не выводить }
        end else begin
          if x + l >= xend then l1 := xend - x else l1 := l;
          { Полный вывод }
          if l1 <> 0 then
           DisplayXx8Op(x, y + yf, @(CurFont^[ind + yf]),
              $FF xor ((Word(1) shl (8 - l1)) - 1), yh, T);
        end;
      end;
      Inc(x, l);
      Inc(ww, l);
      Inc(ind, LastCaps.F.Height);
    end;
    Inc(x, LastCaps.F.GapLength);
    Dec(x, Scale.Desc);
  end;
  WrStr := xout;
End;


Constructor TTrueTypeFont.Init;
Begin
  Inherited Init;
  Handle := -1;
  Flags := ffStroked + ffTrueType + ffScaled;
  LastCaps.F.Width  := 20;
  LastCaps.F.Height := 32;
  Font := LoadTTFontExt(AFName, LastCaps.F.Width, LastCaps.F.Height, Identity(@Matrix), 0);
  if Font = Nil then Fail;
  FName := NewStr(AFName);
  Handle := DefFont(Font);
  SetJustify(BottomText, TopText);
End;


Destructor TTrueTypeFont.Done;
Begin
  FreeFont(Handle);
  DisposeStr(FName);
  Font := Nil;
  Inherited Done;
End;


Procedure TTrueTypeFont.SelectCaps;
var
  Caps : TSmallCaps;
Begin
  GetCaps(Caps, F);
  if F.Width = 0 then begin
    if F.Height = 0 then F.Height := 32;
    F.Width := Round(F.Height / 1.6)
  end else if F.Height = 0 then F.Height := Round(F.Width / 1.6);
  if not EqualCaps(Caps) then
    RescaleTTF(Handle, F.Width, F.Height, F.Escapement, F.Italic, F.Style, @Matrix);
  SetTextParams(Handle, LastCaps.F.GapLength, 0, False);
  LastCaps := Caps;
End;

Function    TTrueTypeFont.GetName : String;
Var
  P : PString;
Begin
  P := PTTFont(SharedList^[Font^.SrcHandle].p)^.Name;
  if P = Nil then GetName := '' else GetName := P^;
End;

Function  TTrueTypeFont.CapsNumber : Word;
Begin
  CapsNumber := $FFFF;
End;

Function  TTrueTypeFont.GetMinSize;
Begin
  GetMinSize := 16; {because without hints}
End;

Constructor TBGIFont.Init(const AFName : String; Width, Height, Attr : Word);
Var
  X : Word;
Begin
  Inherited Init;
  Handle := -1;
  Flags := ffStroked + ffBGI;
  if Width = 0 then X := 0 else X := LongInt(Height) * 10000 div Width;
  Font := LoadBGIFileFont(AFName, Height, 10000, X, 1,1,0,0,0,Attr);
  if Font = Nil then Fail;
  FName := NewStr(AFName);
  Handle := DefFont(Font);
  SetJustify(BottomText, TopText);
End;

Destructor TBGIFont.Done;
Begin
  FreeFont(Handle);
  DisposeStr(FName);
  Font := Nil;
  Inherited Done;
End;


Procedure TBGIFont.SelectCaps;
Label Here;
var
  Name :String;
  Caps : TSmallCaps;
Begin
  GetCaps(Caps, F);
  if EqualCaps(Caps) then Goto Here;
  Name := FName^;
  Done;
  Init(Name, F.Width, F.Height, F.Style);
Here:
  SetTextParams(Handle, LastCaps.F.GapLength, 0, False);
  LastCaps := Caps;
End;

Function TBGIFont.GetName : String;
Var
  _D : DirStr;
  _N : NameStr;
  _E : ExtStr;
Begin
  FSplit(FName^, _D, _N, _E);
  GetName := _N;
End;

Function  TBGIFont.CapsNumber : Word;
Begin
  CapsNumber := $FFFF;
End;

Constructor TBIOSFont.Init;
Var
  I : Byte;
Begin
  Inherited Init;
  I := 0;
  Flags  := ffBitmap;
  Font   := LoadBIOSFont(8,0,0,0);
  Fonts[I]:= DefFont(Font);
  Heights[I] := 8;
  if Fonts[I] >= 0 then Inc(I);
  Font   := LoadBIOSFont(14,0,0,0);
  Fonts[I] := DefFont(Font);
  Heights[I] := 14;
  if Fonts[I] >= 0 then Inc(I);
  Font   := LoadBIOSFont(16,0,0,0);
  Fonts[I] := DefFont(Font);
  Heights[I] := 16;
  if Fonts[I] >= 0 then Inc(I);
  if I = 0 then Fail;
  CapCount := I;
  Handle := Fonts[I - 1];
End;

Procedure  TBIOSFont.SelectCaps;
Var
  I, Min : Byte;
Begin
  Min := $FF;
  for I := 0 to CapCount - 1 do begin
    if Min > Abs(Heights[I] - F.Height) then begin
      Min := Abs(Heights[I] - F.Height);
      Handle := Fonts[I];
    end;
  end;
  SetTextParams(Handle, LastCaps.F.GapLength, 0, False);
  SetJustify(BottomText, TopText);
End;

Function  TBIOSFont.CapsNumber : Word;
Begin
  CapsNumber := CapCount;
End;

Function TBIOSFont.CapsHeight(CapsNo : Word) : Word;
Begin
  CapsHeight := Heights[CapsNo - 1];
End;

Function TBIOSFont.GetName : String;
Begin
  GetName := 'BIOS';
End;

Function LoadFont(FileName : String) : PAbstractFont;
Var
  P : PAbstractFont;
  _D : DirStr;
  _N : NameStr;
  _E : ExtStr;
Begin
  P := Nil;
  FSplit(FileName, _D, _N, _E);
  _E := StUpcase(_E);
  {$IFDEF USEFONDEMANDLOAD}
       if _E = '.FON' then
       {$IFDEF USEFONDEMANDSCALABLE}
          P := New(PScaledWindowsFont, Init(FileName))
       {$ELSE}
          P := New(PWindowsFont, Init(FileName))
       {$ENDIF}
  {$ENDIF}
  {$IFDEF USETTFDEMANDLOAD}
  else if _E = '.TTF' then P := New(PTrueTypeFont, Init(FileName))
  {$ENDIF}
  {$IFDEF USEBGIDEMANDLOAD}
  else if _E = '.CHR' then P := New(PBGIFont, Init(FileName))
  {$ENDIF}
  ;
  LoadFont := P;
End;

End.