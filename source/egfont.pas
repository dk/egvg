{$G+}
Unit EGFont;

interface

uses Objects, GDI, EgInline, EgString;

const
  MaxFontsNum = 50;

  {font flags}
  ffBitmap   = $01;
  ffStroked  = $02;
  ffShared   = $04;
  ffWinres   = $08;
  ffBGI      = $10;
  ffTrueType = $20;
  ffScaled   = $40;


type
  PAbstractFont = ^TAbstractFont;
  TAbstractFont = object(TObject)
    Flags : Word;
    destructor  Done;                                       virtual;
    function    GetCharWidth(C:Char): integer;              virtual;
    function    GetWidth(const s: string): integer;         virtual;
    function    GetHeight: integer;                         virtual;
    function    WrStr(x, y, Len : integer; const s: string; var T : TPaintInfo): integer; virtual;
    procedure   Select; virtual;
    procedure   SelectCaps(var F : TFont); virtual;
    function    GetName : String; Virtual;
    Function    CapsNumber : Word; Virtual;
    Function    CapsHeight(CapsNo : Word) : Word; Virtual;
    Function    GetResourceName : String; Virtual;
    Function    GetMinSize : Word; Virtual;
  end;

procedure RegisterFont(Font: PAbstractFont; i: integer);
function  SelectFont(i: integer): boolean;
function  WrStr(x,y : integer; const s : string; var T : TPaintInfo): integer;
function  GetCharWidth: integer; { Returns zero for proportional fonts }
function  GetWidth(const s: string): integer;
function  GetHeight: integer;
function  GetCurrentFont: PAbstractFont;
function  GetFont(Num : Integer): PAbstractFont;
function  GetFontID(Font : PAbstractFont) : Integer;
Procedure AssignFont(FontID : Integer; var F : TFont);
procedure SelectFontCaps(var F : TFont);
function  GetFreeFontID : Integer;
procedure DisposeFont(Num : Integer);
function  GetFontByName(FontName : String; Strict : Boolean) : PAbstractFont;

Function  EnumFonts(dwCallback : Pointer) : PAbstractFont; {FirstThat analog}

Const
  SimpleFontMaxCharSize = 128;

type
  PSimpleFont = ^TSimpleFont;
  TSimpleFont = object(TAbstractFont)
    Name  : PString;
    RamBuf: pointer;
    constructor Load(const FName: string);
    constructor Init(const Data; AName : String);
    destructor  Done;                                       virtual;
    function    GetCharWidth(C:Char): integer;              virtual;
    function    GetWidth(const s: string): integer;         virtual;
    function    GetHeight: integer;                         virtual;
    function    WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer; virtual;
    function    GetName : String; Virtual;
   private
    w, h, cil, bs: integer;
  end;

  TLongRec = Record
    case Byte of
    0 : (Lo, Hi : Word);
    1 : (L : LongInt);
  End;

  TScaleRec = Record
    Vert : LongInt;   {коэффициент масштабирования по вертикали}
    Hors : LongInt;   {коэффициент масштабирования по горизонтали}
    Ital : LongInt;   {коэффициент масштабирования наклона}
    ItUp : Boolean;   {указатель направления наклона}
    Desc : Word;      {добавочная ширина символа, возникающая из-за наклона}
    IncW : Word;      {добавочная ширина символа, возникающая из-за embolding}
    Flag : Word;      {флаги - используются только подчеркивание и перечеркивание}
    BPL  : Word;      {размер оригинальной линейки в байтах}
    NoSc : Boolean;   {если true, масштабирование, наклон и флаги могут не применяться}
  End;

  {DK addition - TScaledSimpleFont}
  PScaledSimpleFont = ^TScaledSimpleFont;
  TScaledSimpleFont = object(TSimpleFont)
    constructor Init(const Data; AName : String);
    constructor Load(const FName: string);
    function    GetCharWidth(C:Char): integer;              virtual;
    function    GetWidth(const s: string): integer;         virtual;
    function    GetHeight: integer;                         virtual;
    procedure   SelectCaps(var F : TFont);                  virtual;
    function    WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer; virtual;
    destructor  Done;                                       virtual;
  private
    Bits    : PLongArray;
    gap, apx, apy, asize, aital : Word;
    scale   : TScaleRec;
  End;

{font developer functions}
Procedure FillScaled(var Scale : TScaleRec; W, H, AX, AY, Flags, ItalDef : Word);
Procedure ScaleMap(Scale : TScaleRec; Source, Dest : PByteArray; W, H, X, Y : Word);

var
  CurrentFont: integer;

implementation

var
  Fonts : array [1..MaxFontsNum] of PAbstractFont; {Read onle}


function  GetCurrentFont: PAbstractFont;
Begin
  GetCurrentFont := Fonts[CurrentFont];
End;

destructor TAbstractFont.Done; begin end;

function TAbstractFont.GetCharWidth(C:Char): integer; begin GetCharWidth := 0 end;

function TAbstractFont.GetWidth(const s: string): integer; begin GetWidth := 0 end;

function TAbstractFont.GetHeight: integer; begin GetHeight := 0 end;

function  TAbstractFont.WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer;
begin
  WrStr := x;
end;

procedure  TAbstractFont.Select; begin end;
procedure  TAbstractFont.SelectCaps; begin end;
function   TAbstractFont.GetName : String; begin end;
Function   TAbstractFont.CapsNumber; begin CapsNumber := 1; end;
Function   TAbstractFont.CapsHeight; begin CapsHeight := GetHeight; end;
Function   TAbstractFont.GetResourceName : String; begin GetResourceName := ''; end;

Function   TAbstractFont.GetMinSize : Word;
var
  I : Word;
begin
  I := CapsNumber;
  if I = 0 then GetMinSize := 1 else GetMinSize := CapsHeight(1);
end;

constructor TSimpleFont.Load(const FName: string);
label
  ErrAndClose, ErrAndDeallocate;
var
  cc : record w,h: integer; end;
  f: file;
  sz: integer;
begin
  rambuf := Nil;
  Assign(f, FName);
  {$I-}
  Reset(f, 1);
  if IOResult<>0 then Fail;

  if FALSE then begin
ErrAndDeallocate:
    FreeMem(rambuf, bs);
ErrAndClose:
    Close(f);
    Fail;
  end;

  BlockRead(f, cc, 4, sz);
  if (IOResult<>0) or (sz<>4) then goto ErrAndClose;

  w := cc.w;
  h := cc.h;
  cil := ((w+7) shr 3) * h;
  if cil > SimpleFontMaxCharSize then goto ErrAndClose;
  bs := cil*256;

  GetMem(rambuf, bs);
  if rambuf=Nil then goto ErrAndClose;
  BlockRead(f, rambuf^, bs, sz);
  if (IOResult<>0) or (bs<>sz) then goto ErrAndDeallocate;
  Close(f);
  {$I+}
  Flags := ffBitMap;
  Name := Nil;
end;

constructor TSimpleFont.Init;
var
  cc : record w,h, other: integer; end absolute Data;
begin
  rambuf := Nil;
  w := cc.w;
  h := cc.h;
  cil := ((w+7) shr 3) * h;
  if cil > SimpleFontMaxCharSize then Fail;
  bs := cil*256;

  GetMem(rambuf, bs);
  if rambuf=Nil then Fail;
  Move(cc.other, rambuf^, bs);
  Flags := ffBitMap;
  Name := NewStr(AName);
end;

destructor TSimpleFont.Done;
begin
  if Name <> Nil then begin
    DisposeStr(Name);
    Name := Nil;
  end;
  FreeMem(rambuf, bs);
end;


function TSimpleFont.GetWidth(const s: string): integer;
begin  GetWidth := w*length(s); end;
function TSimpleFont.GetHeight: integer; begin GetHeight := h end;
function TSimpleFont.GetCharWidth(C:Char): integer; begin GetCharWidth := w end;
function TSimpleFont.GetName;
Begin
  if Name = Nil then GetName := '' else GetName := Name^;
End;

Procedure FillScaled(var Scale : TScaleRec; W, H, AX, AY, Flags, ItalDef : Word);
Var
  Angle : Real;
  ADW   : WOrd;
Begin
  with Scale do begin
    IncW := Byte((Flags and ftBold) <> 0);
    adw  := W;
    if IncW <> 0 then IncW := Round(Trunc(AX / W + 0.5));
    if IncW <> 0 then Inc(ADW);
    Flag := Flags;
    BPL  := (W shr 3) + Byte((W and 7) <> 0);
    if W = 1 then Hors := $10000 else
    Hors := LongInt(Trunc(AX / W) * $10000) + Trunc(Frac(AX / W) * $10000);
    if H = 1 then Vert := $10000 else
    Vert := LongInt(Trunc(AY / H) * $10000) + Trunc(Frac(AY / H) * $10000);
    NoSc := (W = AX) and (H = AY) and (ItalDef = 0) and
      (Flags and (ftUnderlined + ftStruckOut + ftBold) = 0);
    Angle := ItalDef / 60;
    Desc := Round(Abs(Sin(Angle) / Cos(Angle)) * AY);
    ItUp := Desc >= AY;
    if ItUp then Ital := Round((AY / Desc) * $10000)
      else Ital := Round((Desc / AY) * $10000);
  end;
End;

Procedure ScaleMap(Scale : TScaleRec; Source, Dest : PByteArray; W, H, X, Y : Word);
Var
  SrcBuf, DestBuf : array[0..512] of Byte;
  I, J, BPLD, FullX, ItalX : Word;
  CounterXL, CounterYL : LongInt;
  CXLW  : LongRec absolute CounterXL;
  CYLW  : LongRec absolute CounterYL;
  LastX, LastY : Integer;
  DrawBPL : Word;

Procedure ImplodeX;
Var
  Pixel, XC, XD : Byte;
  J, JM : Word;
  LData : Pointer;
Begin
  CounterXL := 0;
  LastX := 0;
  JM := 0;
  XC := SrcBuf[0];
  asm
    rcl xc, 1
    rcl xd, 1
  end;
  for J := 0 to X shr 1 do Inc(CounterXL, Scale.Hors);
  LastX := CXLW.Hi;
  for J := 0 to X - 1 do begin
    if (Succ(J) and 7) = 0 then XC := SrcBuf[Succ(J) shr 3];
    if CXLW.Hi > LastX then begin
      asm
        rcl XC, 1
        rcl XD, 1
      end;
      Inc(JM);
      if (Succ(JM) and 7) = 0 then DestBuf[JM shr 3] := XD;
      LastX := CXLW.Hi;
    end else asm shl XC, 1 end;
    Inc(CounterXL, Scale.Hors);
  end;
  if (Succ(J) and 7) = 0 then XC := SrcBuf[Succ(J) shr 3];
  asm
    mov cl, 8
    mov ch, byte ptr JM
    inc ch
    and ch, 7
    jz @@1
    sub cl, ch
    shl XD, cl
  @@1:
  end;
  DestBuf[JM shr 3] := XD;
End;


Procedure ExplodeX;
Var
  Pixel, XC, XD : Byte;
  J, JM : Word;
  LData : Pointer;
Begin
  CounterXL := 0;
  LData := @SrcBuf;
  JM := 0;
  XC := SrcBuf[0];
  xd := 0;
  asm
    mov al, XC
    rcl al, 1
    rcl Pixel, 1
  end;
  for J := 0 to X - 1 do begin
    if CXLW.Hi <= J then begin
      asm
        rcl XC, 1
        rcl Pixel, 1
      end;
      if (Succ(JM) and 7) = 0 then XC := SrcBuf[Succ(JM) shr 3];
      Inc(JM);
      Inc(CounterXL, Scale.Hors);
    end;
    asm
      mov al, Pixel
      rcr al, 1
      rcl XD, 1
    end;
    if (Succ(J) and 7) = 0 then DestBuf[J shr 3] := XD;
  end;
  asm
    mov cl, 8
    mov ch, byte ptr J
    inc ch
    and ch, 7
    jz @@1
    sub cl, ch
    shl XD, cl
  @@1:
  end;
  DestBuf[J shr 3] := XD;
End;

Procedure FillYDest(ActY : Word);
Var
  I, J : Word;
Begin
  J := ActY;
  for I := 0 to BPLD - 1 do begin
    Dest^[J] := DestBuf[I];
    Inc(J, Y);
  end;
End;

Procedure ReadYDest(ActY : Word);
Var
  I, J : Word;
Begin
  J := ActY;
  for I := 0 to BPLD - 1 do begin
    DestBuf[I] := Dest^[J];
    Inc(J, Y);
  end;
End;


Procedure SetRor(Src : Pointer; HowMuch, Count : Word); Assembler; Asm
  mov cx, HowMuch
  or  cx, cx
  jz @@Exit
  shr cx, 3
  jz @@Shr

  std
  mov ax, Count
  shr ax, 3
  sub ax, cx
  xchg ax, cx
  push ds
  lds si, Src
  les di, Src
  add si, cx
  dec si
  add di, ax
  add di, cx
  dec di
  rep movsb
  mov cx, HowMuch
  shr cx, 3
  xor al, al
  rep stosb
  pop ds
  cld

@@Shr:
  mov cx, HowMuch
  and cl, 7
  jz @@Exit
  les di, Src
  mov dx, es:[di]
  xchg dl, dh
  shr es:[di], cl
  xchg bx, cx
  mov cx, Count
  shr cx, 3
  jz @@Exit
@@1:
  xchg bl, cl
  shr dx, cl
  inc di
  mov ax, es:[di]
  mov es:[di], dl
  mov dx, ax
  xchg dl, dh
  xchg bl, cl
  loop @@1
@@Exit:
End;

Procedure SetShrOr(Buffer : Pointer; LengthInBytes : Word); Assembler; Asm
  les di, Buffer
  mov cx, LengthInBytes
  mov dl, es:[di]
@@1:
  mov al, es:[di+1]
  mov ah, dl
  rcr dl, 1
  or  es:[di], dl
  rcr ah, 1
  mov dl, al
  inc di
  loop @@1
End;

Procedure FillYSrc(Y : Word);
Var
  I, J : Word;
Begin
  J := Y;
  for I := 0 to DrawBPL - 1 do begin
    SrcBuf[I] := Source^[J];
    Inc(J, H);
  end;
  SrcBuf[Succ(I)] := 0;
  for I := 0 to BPLD - 1 do DestBuf[I] := 0;
  if Scale.IncW > 0 then SetShrOr(@SrcBuf, DrawBPL + 1);
End;



Begin
  {init}
  FullX := X + Scale.Desc{ + Scale.IncW};
  ItalX := X + Scale.Desc;
  BPLD    := (FullX shr 3) + Byte((FullX and 7) <> 0);
  DrawBPL := (W shr 3) + Byte((W and 7) <> 0);

  {scaling}
  LastY     := 0;
  CounterYL := 0;
  J := 0;
  FillYSrc(0);
  if LongRec(Scale.Vert).Hi = 0 then begin        { IMPLODE Y }
    if LongRec(Scale.Hors).Hi = 0 then ImplodeX else ExplodeX;
    FillYDest(J);
    Inc(J);
    for I := 0 to H - 1 do begin
      if CYLW.Hi > LastY then begin
        if LongRec(Scale.Hors).Hi = 0 then ImplodeX else ExplodeX;
        FillYDest(J);
        Inc(J);
        LastY := CYLW.Hi;
      end;
      Inc(CounterYL, Scale.Vert);
      FillYSrc(I);
    end;
  end else if Scale.Vert <> $10000 then begin  { EXPLODE Y }
    if LongRec(Scale.Hors).Hi = 0 then ImplodeX else ExplodeX;
    for I := 0 to Y - 1 do begin
      if CYLW.Hi < LastY then begin
        FillYSrc(J);
        if (LongRec(Scale.Hors).Hi = 0) then ImplodeX else ExplodeX;
        Inc(CounterYL, Scale.Vert);
        Inc(J);
      end;
      Inc(LastY);
      FillYDest(I);
    end;
  end else for I := 0 to Y - 1 do begin {NOSCALE Y}
    FillYSrc(I);
    if (LongRec(Scale.Hors).Hi = 0) then ImplodeX else ExplodeX;
    FillYDest(I);
  end;

  {italizing}
  if Scale.Ital <> 0 then begin
    CounterYL := 0;
    if Scale.ItUp then for I := Pred(Scale.Desc) downto 0 do begin
      ReadYDest(CYLW.Hi);
      SetRor(@DestBuf, I, FullX);
      FillYDest(CYLW.Hi);
      Inc(CounterYL, Scale.Ital);
    end else for I := Pred(Y) downto 0 do begin
      ReadYDest(I);
      SetRor(@DestBuf, CYLW.Hi, FullX);
      FillYDest(I);
      Inc(CounterYL, Scale.Ital);
    end;
  end;


  {underlining}
  if (Scale.Flag and ftUnderlined) <> 0 then begin
    J := Y - 1;
    for I := 0 to BPLD - 1 do begin
      Dest^[J] := $FF;
      Inc(J, Y);
    end;
  end;

  {strucking out}
  if (Scale.Flag and ftStruckOut) <> 0 then begin
    J := Y shr 1;
    for I := 0 to BPLD - 1 do begin
      Dest^[J] := $FF;
      Inc(J, Y);
    end;
  end;
End;


constructor TScaledSimpleFont.Init(const Data; AName : String);
Begin
  Inherited Init(Data, AName);
  GetMem(Bits, 256 * SizeOf(Pointer));
  FillChar(Bits^, 256 * SizeOf(Pointer), 0);
  apx  := w;
  apy  := h;
  aital := 0;
  asize := ((w shr 3) + Byte((w and 7) <> 0)) * h;
  FillScaled(scale, w, h, apx, apy, 0, 0);
  Flags := Flags or ffScaled;
End;

constructor TScaledSimpleFont.Load(const FName: string);
Begin
  Inherited Load(FName);
  GetMem(Bits, 256 * SizeOf(Pointer));
  FillChar(Bits^, 256 * SizeOf(Pointer), 0);
  apx  := w;
  apy  := h;
  aital := 0;
  asize := ((w shr 3) + Byte((w and 7) <> 0)) * h;
  FillScaled(scale, w, h, apx, apy, 0, 0);
  Flags := Flags or ffScaled;
End;

function    TScaledSimpleFont.GetCharWidth(C:Char): integer;
Begin
  GetCharWidth := apx + Scale.Desc;
End;

function    TScaledSimpleFont.GetWidth(const s: string): integer;
var
  I:Byte;
begin
  I := Length(S);
  if I > 0 then GetWidth := (apx + gap) * length(s) - gap + Scale.Desc
    else GetWidth := 0;
End;

function    TScaledSimpleFont.GetHeight: integer;
Begin
  GetHeight := apy;
End;

procedure   TScaledSimpleFont.SelectCaps;
Var
  I, fullX, aptx, apty, apital : Integer;

Begin
  Gap := F.GapLength;
  if ((F.Style and ftItalic) <> 0) and (F.Italic = 0) then apital := fiItalic
    else apital := F.Italic;
  aptx := F.Width;
  apty := F.Height;
  if apty <= 0 then begin
    if aptx <= 0 then begin
      aptx := W;
      apty := H;
    end else apty := H * aptx div W;
  end;
  if aptx <= 0 then
    if apty = H then aptx := W
      else aptx := W * apty div H;
  if (aptx = apx) and (apty = apy) and (F.Style = Scale.Flag) and (apital = aital) then Exit;
  for I := 0 to 255 do
    if Pointer(Bits^[I]) <> Nil then begin
      FreeMem(Pointer(Bits^[I]), asize);
      Pointer(Bits^[I]) := Nil;
    end;
  apx   := aptx;
  apy   := apty;
  aital := apital;
  FillScaled(scale, w, h, apx, apy, F.Style, aital);
  if (F.Width <= 0) and (Scale.IncW <> 0) then begin
    {при embolding возникает расширение символа, и если ширина задана
    то приходится дополнительно его сжимать по ширине, а если нет -
    можно восстановить оригинальную пропорцию}
    Inc(apx, Scale.IncW);
    FillScaled(scale, w, h, apx, apy, F.Style, aital);
  end;
  fullX := apx + Scale.Desc;
  asize := ((fullX shr 3) + Byte((fullX and 7) <> 0)) * apy;
End;

function  TScaledSimpleFont.WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer;
var
  i : integer;
  ww, l, l1 : integer;
  ind: word;
  CurFont : PByteArray;
  Ram : PByteArray absolute RamBuf;
  Special: PByteArray;
  yf: integer;
  yh: integer;
  xout: integer;
  xbeg, xend, fullx: integer;
  xp: integer;
  Msk: byte;
  First: Integer;

Procedure GetBitmap(Index : Byte);
Begin
  if Scale.NoSc then CurFont := @Ram^[cil * Index] else begin
    if Pointer(Bits^[Index]) = Nil then begin
      GetMem(Pointer(Bits^[Index]), asize);
      ScaleMap(Scale, @Ram^[cil * Index], Pointer(Bits^[Index]), w, h, apx, apy);
    end;
    CurFont := Pointer(Bits^[Index]);
  end;
End;


begin
  if Len = 0 then Exit;
  CurFont := RamBuf;
  WrStr := x;
  fullx := apx + Gap{ + Scale.IncW};

  { Absolute no write checks }
  if (T.ClipRect.B.Y<=y) or (T.ClipRect.A.Y>=y+apy) then Exit;
  xout := x + Len * fullX - Gap + Scale.Desc;
  if (T.ClipRect.B.X<=x) or (T.ClipRect.A.X>=xout) then Exit;

  { Vertical clip calculating }
  if T.ClipRect.A.Y>y then yf:=T.ClipRect.A.Y-y else yf:=0;
  if T.ClipRect.B.Y<=y+apy then yh := (T.ClipRect.B.Y-y)-yf else yh := apy-yf;
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
  if x < xbeg then begin
    First := (xbeg - x) div (apx + Gap);
    Inc(x, First * fullX - Gap);
    if (Scale.Desc > 0) and (First > 0) then begin
      Dec(x, fullX + Gap);
      Dec(First);
    end;
  end else First := 0;
  Inc(First);
  if First > Len then Exit;

  for i:=First to Len do begin
   GetBitmap(Byte(s[i]));
   if x >= xend then Break;
   ww := 0;
   ind := 0;
   while ww<apx+Scale.Desc do begin
      l := apx+Scale.Desc-ww; if l>8 then l:=8;
      if x<xbeg then begin
        if x+l <= xbeg then begin
          { Вообще не выводить }
        end else if x>=xend then begin
          { Вообще не выводить }
        end else begin
          { Частичный вывод }
          if x>=0 then begin
            if x+l>=xend then l1:=xend-x else l1:=l;
            if l1>0 then begin
              Msk := ($FF SHR (xbeg-x)) xor ($FF SHR l1);
              DisplayXx8Op(x, y+yf, @(CurFont^[ind+yf]), Msk, yh, T);
            end;
          end else begin
            { Особый вариант !  Сверхособый вариант ! }
            GetMem(Special, yh);
            for l1:=0 to Pred(yh) do begin
              Special^[l1] := BYTE(CurFont^[ind+yf+l1] SHL (-x) and (1 shl (8 - xbeg and 7) - 1));
            end;
            l1 := l+x;
            if l1>xend then l1:=xend;
            if l1>0 then
              DisplayXx8Op(0, y+yf, Special,  $FF xor ((Word(1) shl (8 - l1)) - 1), yh, T);
            FreeMem(Special, yh);
          end;
        end;
      end else begin
        { Слева нет усечения }
        if x>=xend then begin
          { Вообще не выводить }
        end else begin
          if x+l>=xend then l1:=xend-x else l1:=l;
          { Полный вывод }
          if l1<>0 then
           DisplayXx8Op(x, y+yf, @(CurFont^[ind+yf]),
              $FF xor ((Word(1) shl (8 - l1)) - 1), yh, T);
        end;
      end;
      Inc(x, l);
      Inc(ww, l);
      Inc(ind, apy);
    end;
    Inc(x, Gap);
    Dec(x, Scale.Desc);
  end;
  WrStr := xout;
End;

destructor  TScaledSimpleFont.Done;
Var
  I : Integer;
Begin
  for I := 0 to 255 do
    if Pointer(Bits^[I]) <> Nil then begin
      FreeMem(Pointer(Bits^[I]), asize);
      Pointer(Bits^[I]) := Nil;
    end;
  Inherited Done;
End;


function  TSimpleFont.WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer;
var
  Width, Height: integer;
  CharImageSize: integer;
  First, FirstY: integer;
  TempBuf: array [1..SimpleFontMaxCharSize] of byte;
  XBeg, XEnd: integer;
  Buffer: pointer;
  FExit: Boolean;
  Local : Boolean;

function WrStrO(const Clip: TRect; x, y: integer; const s: string;
                    Len, Fore: integer): integer;
var
  i : integer;
  ww, l, l1 : integer;
  ind: word;
  CurFont : PByteArray;
  Special: PByteArray;
  yf: integer;
  yh: integer;
  xout: integer;
  xbeg, xend: integer;
  xp: integer;
  Msk: byte;
  First: Integer;

begin
  CurFont := RamBuf;
  WrStrO := x;

  { Absolute no write checks }
  if (Clip.B.Y<=y) or (Clip.A.Y>=y+h) then Exit;
  xout := x + Len * w;
  if (Clip.B.X<=x) or (Clip.A.X>=xout) then Exit;

  { Vertical clip calculating }
  if Clip.A.Y>y then yf:=Clip.A.Y-y else yf:=0;
  if Clip.B.Y<=y+h then yh := (Clip.B.Y-y)-yf else yh := h-yf;
  { if yh=0 then Exit; }

  { Horizontal clip calculating }
  if Clip.A.X>x then xbeg:=Clip.A.X-x else xbeg:=0;
  if Clip.B.X<=xout then xend := (Clip.B.X-x) else xend := xout-x;
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
  if x < xbeg then First := (xbeg - x) div w else First := 0;
  Inc(x, First * w); Inc(First);
  if First > Len then Exit;

  for i:=First to Len do begin
    if x >= xend then Break;
    ww := 0;
    ind := byte(s[i]) * cil;
    while ww<w do begin
      l := w-ww; if l>8 then l:=8;
      if x<xbeg then begin
        if x+l <= xbeg then begin
          { Вообще не выводить }
        end else if x>=xend then begin
          { Вообще не выводить }
        end else begin
          { Частичный вывод }
          if x>=0 then begin
            if x+l>=xend then l1:=xend-x else l1:=l;
            if l1>0 then begin
              {Msk := $FF SHR (xbeg-x);}
              Msk := ($FF SHR (xbeg-x)) xor ($FF SHR l1);
              if Local then
              DisplayXxYClip(x, y+yf, @(CurFont^[ind+yf]), Msk,
                l1, yh, Fore)
              else
                DisplayXx8Op(x, y+yf, @(CurFont^[ind+yf]), Msk, yh, T);
            end;
          end else begin
            { Особый вариант !  Сверхособый вариант ! }
            GetMem(Special, yh);
            for l1:=0 to Pred(yh) do begin
              Special^[l1] := BYTE(CurFont^[ind+yf+l1] SHL (-x) and (1 shl (8 - xbeg and 7) - 1));
            end;
            l1 := l+x;
            if l1>xend then l1:=xend;
            if l1>0 then begin
              if Local then
              DisplayXxY(0, y+yf, Special, l1, yh, Fore)
              else
              DisplayXx8Op(0, y+yf, Special, $FF xor ((Word(1) shl (8 - l1)) - 1), yh, T);
            end;
            FreeMem(Special, yh);
          end;
        end;
      end else begin
        { Слева нет усечения }
        if x>=xend then begin
          { Вообще не выводить }
        end else begin
          if x+l>=xend then l1:=xend-x else l1:=l;
          { Полный вывод }
          if l1<>0 then begin
            if Local then
            DisplayXxY(x, y+yf, @(CurFont^[ind+yf]),
              l1, yh, Fore)
            else DisplayXx8Op(x, y+yf, @(CurFont^[ind+yf]),
              $FF xor ((Word(1) shl (8 - l1)) - 1), yh, T);
          end;
        end;
      end;
      Inc(x, l);
      Inc(ww, l);
      Inc(ind, h);
    end;
  end;

  WrStrO := xout;
end;


begin FExit:=True;
  Local := (T.Device = Nil) and (T.Operation = 0) and (T.LineStyle = lsLinePattern);
 { Local := False;}
if Local then begin asm
    { Перенос тех вещей, которые нам понадобятся, из объекта }
    { в локальные переменные для упрощения доступа }
    les    di, Self
    mov    ax, es:[di].TSimpleFont.w
    mov    Width, ax
    mov    ax, es:[di].TSimpleFont.cil
    mov    CharImageSize, ax
    mov    ax, word ptr es:[di].TSimpleFont.rambuf[0]
    mov    word ptr Buffer[0], ax
    mov    ax, word ptr es:[di].TSimpleFont.rambuf[2]
    mov    word ptr Buffer[2], ax
    mov    ax, es:[di].TSimpleFont.h
    mov    Height, ax
    { Общие проверки на выводимость хотя бы куска строки }
    {les    di, Clip}
    les    di, t
    add    di, offset TPaintInfo.ClipRect
    mov    bx, y                   { Проверка y-выводимости }
    cmp    es:[di].TRect.B.Y, bx
    jle    @@Exit                  { y ниже Clip }
    add    bx, ax                  { bx <- y+h }
    cmp    es:[di].TRect.A.Y, bx
    jge    @@Exit                  { y+h выше Clip }
    mov    ax, Len                 { Проверка x-выводимости }
    mul    Width
    mov    bx, x
    add    ax, bx                  { ax <- x + Len*Width }
    cmp    es:[di].TRect.B.X, bx
    jle    @@Exit
    cmp    es:[di].TRect.A.X, ax
    jge    @@Exit
    push   ax
    { Расчет отсечения по вертикали }
    xor    cx, cx
    mov    ax, es:[di].TRect.A.Y
    cmp    ax, y
    jng    @@1
    sub    ax, y
    mov    cx, ax
@@1:
    mov    FirstY, cx
    { if Clip.B.Y<=y+h then yh := (Clip.B.Y-y)-yf else yh := h-yf; }
    mov    ax, es:[di].TRect.B.Y
    mov    bx, y
    add    bx, Height
    cmp    ax, bx
    jg     @@2
    sub    ax, y
    mov    Height, ax
@@2:
    sub    Height, cx
    { Расчет отсечения по горизонтали }
    xor    cx, cx
    mov    ax, es:[di].TRect.A.X
    mov    bx, x
    cmp    ax, bx
    jng    @@3
    sub    ax, bx
    mov    cx, ax
@@3:
    pop    dx                      { dx <- x + Len*Width }
    mov    ax, es:[di].TRect.B.X
    cmp    ax, dx
    jge    @@4
    mov    dx, ax
@@4:
    mov    XEnd, dx
    add    cx, bx
    mov    XBeg, cx
    mov    ax, FirstY
    add    y, ax
    { Теперь две большие ветви, одна попроще, другая - посложнее }
    cmp    Width, 8
    jg     @@OutBigChars
@@OutSmallChars:
    { Вывод символов, ширина которых до байта }
{ Вход -
  es:di - Clip (уже не нужно)
  bx - x
  cx - XBeg
  dx - XEnd
}
    xor    ax, ax
    cmp    bx, cx
    jnl    @@5
    mov    ax, cx
    sub    ax, bx
    xor    dx, dx
    idiv   Width
@@5:
    mov    First, ax
    imul   Width
    add    bx, ax       { bx - новый x - первый выводимый }
    inc    First        { С какого символа выводить }
    mov    x, bx
    cmp    bx, cx
    jnl    @@6          { Не понадобится усекать левый символ ! }
{ Здесь вывод первого УСЕКАЕМОГО СИМВОЛА }
    les    di, s
    mov    bx, First
    mov    bl, es:[di+bx]  { Символ }
    xor    bh, bh
    mov    ax, CharImageSize
    mul    bx
    les    di, Buffer
    add    di, ax
    add    di, FirstY      { es:di <- образ символа }
    mov    cx, x
    or     cx, cx
    jge    @@7
    { Особое усечение ! }
    xor    si, si
    neg    cx
    mov    dx, Height
@@Loop1:
    or     dx, dx
    jz     @@EndLoop1
    mov    al, es:[di]
    shl    al, cl
    mov    byte ptr TempBuf[si], al
    inc    si
    inc    di
    dec    dx
    jmp    @@Loop1
@@EndLoop1:
    mov    ax, Width
    sub    ax, cx
    cmp    ax, XEnd
    jng    @@8
    mov    ax, XEnd
@@8:
    or     ax, ax
    jz     @@EndOfFirst
    push   0
    push   y
    push   ss
    lea    bx, TempBuf
    push   bx
    push   ax
    push   Height
    {push   Fore}
    les    di, T
    db $66;mov    ax, word ptr es:[di].TPaintInfo.Fore
    db $66;push   ax
    call   GDI.DisplayXxY
    jmp    @@EndOfFirst
@@7:
    { Нормальное усечение ! }
    { cx <- x, es:di <- образ символа }
    mov    bx, Width
    mov    ax, cx
    add    ax, bx
    cmp    ax, XEnd
    jl     @@9
    mov    bx, XEnd
    sub    bx, cx
@@9:
    or     bx, bx
    jz     @@EndOfFirst
    mov    al, 0FFh
    mov    dx, cx
    mov    cx, XBeg
    sub    cx, dx
    shr    al, cl         { Маска вывода отсекаемого символа }
    push   dx
    push   y
    push   es
    push   di
    push   ax
    push   bx
    push   Height
    {push   Fore}
    les    di, T
    db $66; mov    ax, word ptr es:[di].TPaintInfo.Fore
    db $66; push   ax
    call   GDI.DisplayXxYClip
@@EndOfFirst:
    inc    First
    mov    ax, Width
    add    x, ax
@@6:
{ Самый первый усеченный символ (если он был) уже выведен }
{ Смотрим, что нам еще осталось выводить }
    mov    ax, x
    cmp    ax, XEnd
    jge    @@Exit         { Уже нечего выводить... }
    mov    bx, Len
    sub    bx, First
    mov    ax, Width
    imul   bx
    add    ax, x
    cmp    ax, XEnd
    jle    @@10
    mov    ax, XEnd
    sub    ax, x
    idiv   Width
    add    ax, First
    mov    Len, ax
@@10:
    mov    cx, Len
    sub    cx, First
    inc    cx
    mov    ax, Width
    mul    cx
    add    ax, x
    cmp    ax, XEnd
    jle    @@11
    dec    cx
@@11:
    jcxz   @@OutLast
@@Loop2:
{ Здесь вывод основной массы символов }
    { cx - счетчик }
    { DisplayXxY(x, y+yf, @(CurFont^[ind+yf]), l1, yh, Fore); }
    call   @@NormalOutPut
    loop   @@Loop2
@@OutLast:
    mov    ax, XEnd
    mov    bx, x
    cmp    bx, ax
    jge    @@Exit
    sub    ax, bx
    mov    Width, ax
    call   @@NormalOutPut
    jmp    @@Exit
@@NormalOutPut:
    les    di, s
    mov    bx, First
    mov    bl, es:[di+bx]  { Символ }
    xor    bh, bh
    mov    ax, CharImageSize
    mul    bx
    les    di, Buffer
    add    di, ax
    add    di, FirstY      { es:di <- образ символа }
    push   cx
    push   x
    push   y
    push   es
    push   di
    push   Width
    push   Height
    {push   Fore}
    les    di, T
    db $66; mov    ax, word ptr es:[di].TPaintInfo.Fore
    db $66; push   ax
    call   GDI.DisplayXxY
    pop    cx
    mov    ax, Width
    add    x, ax
    inc    First
    retn

@@Exit:
    push ax
    mov al,0
    mov FExit,al
    pop ax

@@OutBigChars:
end;
  if FExit then {WrStr:=WrStrO(Clip, x, y, s, Len, Fore);}
     WrStr:=WrStrO(T.ClipRect, x, y, s, Len, T.Fore);
end else WrStr := WrStrO(T.ClipRect, x, y, s, Len, T.Fore);
end;


procedure RegisterFont(Font: PAbstractFont; i: integer);
begin
  if (i>MaxFontsNum) or (i<1) or (Font = Nil{?? DK}) then Exit;
  if Fonts[i]<>Nil then Dispose(Fonts[i], Done);
  Fonts[i] := Font;
end;

{function WrStr(const Clip: TRect; x,y : integer; const s : string; Fore: Integer): integer;}
function WrStr(x,y : integer; const s : string; var T : TPaintInfo): integer;
begin
  if Fonts[CurrentFont]<>Nil then
    WrStr := Fonts[CurrentFont]^.WrStr(x, y, integer(byte(s[0])), s, T)
  else
    WrStr := x;
end;

function  GetWidth(const s: string): integer;
begin
  GetWidth := 0;
  if Fonts[CurrentFont]<>Nil then
    GetWidth := Fonts[CurrentFont]^.GetWidth(s);
end;

function GetCharWidth: integer;
begin
  GetCharWidth := 0;
  if Fonts[CurrentFont]<>Nil then
    GetCharWidth := Fonts[CurrentFont]^.GetCharWidth('A');
end;

function GetHeight: integer;
begin
  GetHeight := 0;
  if Fonts[CurrentFont]<>Nil then
    GetHeight := Fonts[CurrentFont]^.GetHeight;
end;

function SelectFont(i: integer): boolean;
begin
  if (i>MaxFontsNum) or (i<1) or (Fonts[i]=Nil) then begin
    {Exit;}
    CurrentFont := 1;{DK}
    SelectFont := False;
  end else begin
    CurrentFont := i;
    SelectFont := True;
    Fonts[i]^.Select;
  end;
end;

Procedure SelectFontCaps;
begin
  if SelectFont(F.Font) and (Fonts[F.Font] <> Nil) then
    Fonts[F.Font]^.SelectCaps(F) else Fonts[1]^.SelectCaps(F);
end;


function  GetFont(Num : Integer): PAbstractFont;
Begin
  if Fonts[Num]<>Nil then GetFont := Fonts[Num] else
    GetFont := Fonts[1];
  {$IFDEF DEBUG} {DK}
  if Fonts[1] = Nil then begin
    WriteLn('Font management error.');
    Halt;
  end;
  {$ENDIF}
End;

function  GetFontID(Font : PAbstractFont) : Integer;
Var
  I : Integer;
Begin
  GetFontID := -1;
  if Font = Nil then Exit;
  for I := 1 to MaxFontsNum do begin
    if Fonts[I] = Font then begin
      GetFontID := I;
      Exit;
    end;
  end;
End;

Procedure AssignFont(FontID : Integer; var F : TFont);
Var
  P : PAbstractFont;
Begin
  P := GetFont(FontID);
  FillChar(F, SizeOf(F), 0);
  F.Font   := FontID;
  if P = Nil then DefaultFont(F) else begin
    F.Width  := P^.GetCharWidth('M');
    F.Height := P^.GetHeight;
  end;
End;


function  GetFreeFontID : Integer;
Var
  I : Integer;
Begin
  for I := 1 to MaxFontsNum do if Fonts[I] = nil then Break;
  GetFreeFontID := I;
End;

procedure DisposeFont(Num : Integer);
Begin
  if Fonts[Num] <> Nil then begin
    Dispose(Fonts[Num], Done);
    Fonts[Num] := Nil;
  end;
End;

function  GetFontByName;
var
  Found : PAbstractFont;

Function DoFonts(F : PAbstractFont) : Boolean; Far;
Begin
  DoFonts := FontName = StUpcase(F^.GetName);
End;

Begin
  FontName := StUpcase(FontName);
  Found := EnumFonts(@DoFonts);
  if (Found = Nil) and not Strict then Found := Fonts[1];
  GetFontByName := Found;
End;

Function  EnumFonts;
Var
  F : PAbstractFont;
  I : Integer;
  Br : Boolean;
Begin
  EnumFonts := Nil;
  for I := 1 to MaxFontsNum do begin
    F := Fonts[I];
    if F <> Nil then asm
      les di, F
      push es
      push di
      push word ptr [bp]
      call dwCallback
      mov Br, al
    end;
    if Br then begin
      EnumFonts := F;
      Exit;
    end;
  end;
End;



procedure Init;
var
  i: integer;
begin
  for i:=1 to MaxFontsNum do Fonts[i] := Nil;
  CurrentFont := 1;
end;

var
  PrevExitProc: pointer;

procedure MyExitProc; far;
var
  i: integer;
begin
  ExitProc := PrevExitProc;
  for i:=1 to MaxFontsNum do if (Fonts[i] <> Nil) and ((Fonts[i]^.Flags and ffShared) = 0) then
  Dispose(Fonts[i], Done);
end;


begin
  Init;
  PrevExitProc := ExitProc;
  ExitProc := @MyExitProc;
end.
