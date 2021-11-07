{$F+,S-,R-,I-}
Unit BGIFont;
{
  Internal CHR stroked fonts handling.
  Written by unknown author. Great thanx to him.
  Adapted for SVG by Dmitry Karasik.
}

Interface

Uses Objects, GDI, EGFont, Memory;

Type
  TBgiFontPrefix = Record
    FontSignature       : Array[1..4]   of Char;
    FontDescription     : Array[1..32]  of Char;
    CopyRight           : Array[1..32]  of Char;
  End;
  PBgiFontPrefix    = ^TBgiFontPrefix;

  TBgiFontHeader = Record
    Signature           : Byte;
    HeaderSize          : Word;
    FontName            : Array[1..4]  of Char;
    FontSize            : Word;
    FontVersion         : Byte;
    FontModification    : Byte;
    BgiInformation      : Word;
  End;
  PBgiFontHeader = ^TBgiFontHeader;

  TBgiFontDsc = Record
    Signature           : Char;
    SymbolsInFont       : Word;
    Stub01              : Byte;
    FirstSymbol         : Byte;
    StrokesOffset       : Word;
    FillFlag            : Byte;
    CapitalHeight       : ShortInt;
    BaseLine            : ShortInt;
    Descender           : ShortInt;
    Stub02              : Array[1..5] of Byte;
  End;
  PBgiFontDsc  = ^TBgiFontDsc;

  OffSetTable    =  Array[0..255] of Word;
  POffSetTable   = ^OffSetTable;

  WidthTable     = Array[0..255]  of Byte;
  PWidthTable    = ^WidthTable;

  Vector         = Record
    X,Y : ShortInt;
  End;

  Vectors        =   Array[0..MaxInt-1]  of Vector;
  PVector        =  ^Vectors;

  {для некрученого вывода можно использовать TBGIPrimaryFont -
  он побыстрее.}
  TBGIPrimaryFont = object(TAbstractFont)
    Constructor Init(FontName : String);
    Constructor Register(Font : Pointer);
    Destructor  Done;                                 Virtual;
    Function    GetHeight                : Integer; Virtual;
    Function    SymbolFullHeight         : Integer; Virtual;
    Function    GetCharWidth(C : Char)   : Integer; Virtual;
    Function    CheckFont                : Boolean; Virtual;
    Function    GetWidth(const S : String) : Integer;     Virtual;
    Procedure   Transform(x1,y1,x,y : Integer;  Var XX,YY : Integer);  Virtual;
    Procedure   OutString(x0,y0 : Integer; S : String); Virtual;
    Procedure   SetParams(AFill : Boolean; AGap : Integer);

    function    WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer; Virtual;
    procedure   SelectCaps(var F : TFont); virtual;
    function    GetName : String; Virtual;
    Function    GetResourceName : String; Virtual;
  Private
    Loaded, GotMemory : Boolean;
    FileLen           : LongInt;
    FontPtr           : Pointer;
    PrefPtr           : PBgiFontPrefix;
    DefaultPtr        : PByteArray;
    BgiHeaderPtr      : PBgiFontHeader;
    BgiFontDscPtr     : PBgiFontDsc;
    Offs              : POffsetTable;
    Widths            : PWidthTable;
    Vect              : PVector;
    Fill              : Boolean;
    Gap               : Integer;
    AFontName         : String[80];
    Procedure   SetFontPtrs;
    Procedure   OutSymbol(x1,y1 : Integer; C : Char); Virtual;
  End;
  PBGIPrimaryFont = ^TBGIPrimaryFont;

{$IFOPT N+}
  Float = Double;
{$ELSE}
  Float = Real;
{$ENDIF}

  {но подлинный кайф от векторных шрифтов дает всеж TBGIFont}
  TBGIFont = object(TBGIPrimaryFont)
    Constructor Init(FontName : String);
    Procedure   SetParams(AFill : Boolean; AGap : Integer; MXX, MYY, A : Real; AItalic : Byte);
    Function    GetHeight                  : Integer; Virtual;
    Function    SymbolFullHeight           : Integer; Virtual;
    Function    GetCharWidth(C : Char)      : Integer; Virtual;
    Procedure   Transform(x1,y1,x,y : Integer;  Var XX,YY : Integer); Virtual;
    Procedure   OutString(xx0,yy0 : Integer; S : String); Virtual;
    function    WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer; virtual;
    procedure   SelectCaps(var F : TFont); virtual;
  Private
    MX, MY, Alpha, X0, Y0, SN, CN  :    Float;
    Italic : Byte;
  End;
  PBGIFont = ^TBGIFont;

Const
  HorizontDir = 0;
  VerticalDir = 90;

Implementation

Const
  Cursor : TPoint = (X:0; Y:0);
Var
  PaintInfo  : ^TPaintInfo;

Procedure  TBGIPrimaryFont.SetFontPtrs;
Var
  i,j,k,l : Word;
Begin
  PrefPtr      :=  FontPtr;
  DefaultPtr   :=  FontPtr;
  I := 0;
  While((DefaultPtr^[i]<>$1A) and (I<= 300)) do Inc(I);
  BgiHeaderPtr  := @DefaultPtr^[i];
  BgiFontDscPtr :=  @DefaultPtr^[BgiHeaderPtr^.HeaderSize];
  Offs := @DefaultPtr^[BgiHeaderPtr^.HeaderSize+SizeOf(BgiFontDscPtr^)];

  Widths := @DefaultPtr^[BgiHeaderPtr^.HeaderSize + SizeOf(BgiFontDscPtr^) +
    2 * BgiFontDscPtr^.SymbolsInFont];
  Vect := @DefaultPtr^[BgiHeaderPtr^.HeaderSize+BgiFontDscPtr^.StrokesOffset];
  Gap  := 0;
  Fill := False;
End;

Constructor TBGIPrimaryFont.Init(FontName : String);
Var
    P           : Pointer;
    MaxMem      : LongInt;
    NumRead     : Word;
    FontFile    : File;

Begin
  Inherited Init;
  AFontname := FontName;
  FontPtr := NIL;
  Assign(FontFile,FontName);
  Reset(FontFile,1);
  GotMemory := False;
  if IOResult = 0 then begin
    FileLen := FileSize(FontFile);
    MaxMem := MaxAvail;
    if FileLen < MaxMem then  begin
      FontPtr := MemAlloc(FileLen);
      GotMemory := FontPtr <> Nil;
      if GotMemory then begin
        BlockRead(FontFile,FontPtr^,Filelen,NumRead);
        if NumRead = FileLen then begin
          SetFontPtrs;
          Loaded := CheckFont;
        end else begin
          Loaded := False;
          Done;
        end;
      end else Loaded := False;
    end else    Loaded := False;
  end else    Loaded := False;
  Flags := ffStroked + ffBGI;
End;

Constructor TBGIPrimaryFont.Register(Font : Pointer);
Begin
  GotMemory := False;
  FontPtr := Font;
  SetFontPtrs;
  Loaded := CheckFont;
End;

Destructor  TBGIPrimaryFont.Done;
Begin
  if Gotmemory then Freemem(FontPtr,FileLen);
   Gotmemory  := False;
End;

Function   TBGIPrimaryFont.GetHeight : Integer;
Var
  Ret : Integer;
Begin
  Ret := BgiFontDscPtr^.CapitalHeight-BgiFontDscPtr^.BaseLine;
  if Ret = 0 then GethEight := 1 else GetHeight := Ret;
End;

Function    TBGIPrimaryFont.SymbolFullHeight : Integer;
Begin
  SymbolFullHeight := BgiFontDscPtr^.CapitalHeight - BgiFontDscPtr^.Descender;
End;

Function    TBGIPrimaryFont.GetCharWidth(C : Char) : Integer;
Var
  Ret : Integer;
Begin
  Ret :=  Widths^[Ord(C) - BgiFontDscPtr^.FirstSymbol];
  if Ret = 0 then GetCharWidth := 1 else GetCharWidth := Ret;
End;

Function  TBGIPrimaryFont.CheckFont : Boolean;
Begin
  CheckFont :=
     (PrefPtr^.FontSignature[1]= 'P') and
     (PrefPtr^.FontSignature[2]= 'K') and
     (PrefPtr^.FontSignature[3]= #8) and
     (PrefPtr^.FontSignature[4]= #8) and
     (BGIHeaderPtr^.Signature =$1A)  and
     (BGIFontDscPtr^.Signature ='+');
End;

Procedure   TBGIPrimaryFont.Transform(x1,y1,x,y : Integer; var XX,YY : Integer);
Begin
  XX := X1 + X;
  YY := Y1 - Y;
End;

Function    TBGIPrimaryFont.GetWidth(const S : String) : Integer;
Var
  I, L : Word;
Begin
  L := 0;
  for I := 1 to Length(S) do L := L + GetCharWidth(S[i])+ Gap;
  GetWidth := L;
End;

Procedure TBGIPrimaryFont.OutSymbol(x1,y1 : Integer; C : Char);
Var
      CC                       : Char;
      I,J,K,JF                 : Word;
      xx,yy,ux,uy,mx,my         : Integer;
      PolyLine                 : Array[1..255] of TPoint;
      At                       : LongInt;
Begin
  At := 0;
  Inc(Y1, GetHeight);
      I := Ord(C)-BgiFontDscPtr^.FirstSymbol;
      if(I <= BgiFontDscPtr^.SymbolsInFont) then
        K := I
      else K := 0;
      J := Offs^[K] div 2;
      Self.Transform(x1,y1,0,0,mx,my);
      Cursor.X := MX;
      Cursor.Y := MY;
      JF :=1;
      PolyLine[JF].x :=mx;
      PolyLine[JF].y :=my;
      repeat
       with Vect^[j]   do begin
          xx := x and $7F;
          if(y and $40) <>0 then yy := y or $FF80
            else yy := y and $7F;
          Transform(x1,y1,xx,yy,mx,my);
          if(Fill and ( BgiFontDscPtr^.FillFlag<>0)) then
           begin
            if(((y and  $80)=0) or((Vect^[j].x and $80)=0)) then
             begin
              if JF>2 then FillPoly(JF,PolyLine,TPoint(At),PaintInfo^);
              JF:=0;
              if((y and $80)=0) then begin
              JF :=1;
              PolyLine[JF].x := mx;
              PolyLine[JF].y := my;
              Cursor.X := MX;
              Cursor.Y := MY;
             end;
            end
        else begin
         Inc(JF);
         PolyLine[JF].x := mx;
         PolyLine[JF].y := my;
         Cursor.X := MX;
         Cursor.Y := MY;
         GDI.Line(Cursor.X, Cursor.Y, MX, MY, PaintInfo^);
        end
       end
      else begin
       if ((y and $80) <> 0) then GDI.Line(Cursor.X, Cursor.Y, MX, MY, PaintInfo^);
       Cursor.X := MX;
       Cursor.Y := MY;
      end;
      Inc(j);
      end;
     until ((Vect^[j].x and $80)=0);
End;

Procedure TBGIPrimaryFont.OutString(x0,y0 : Integer; S : String);
Var
  I,L,X1,y1 : Integer;
Begin
  X1 := X0;
  Y1 := Y0;
  for i :=1 to Length(S) do begin
    OutSymbol(x1,y1,S[i]);
    x1 := x1 + GetCharWidth(S[i]) + Gap;
  end;
End;

function   TBGIPrimaryFont.WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer;
Begin
  PaintInfo := @T;
{  SetParams((T.FontStyle and ftFill) <> 0, T.GapLength);}
  OutString(x, y, S);
End;

function    TBGIPrimaryFont.GetName;
Var
  S : String;
Begin
  Move(BgiHeaderPtr^.FontName, S[1], 4);
  S[0] := #4;
  GetName := S;
End;

Function    TBGIPrimaryFont.GetResourceName;
Begin
  GetResourceName := AFontName;
End;

procedure   TBGIPrimaryFont.SelectCaps(var F : TFont);
Begin
  SetParams((F.Style and ftFill) <> 0, F.GapLength);
End;

Procedure   TBGIPrimaryFont.SetParams(AFill : Boolean; AGap : Integer);
Begin
  Fill := AFill;
  Gap  := AGap;
End;

Constructor TBGIFont.Init(FontName : String);
Begin
  Inherited Init(FontName);
  if not Loaded then Fail;
  {SetParams(True, 1, 16 / Inherited GetCharWidth('A'), 16 / Inherited GetHeight, 0, 0);}
  MX := 16 / Inherited GetCharWidth('A');
  MY := 16 / Inherited GetHeight;
  Alpha := 0; Italic := 0; SN := 0; CN := 1;
  Inherited SetParams(True, Round(MX));
End;

Procedure  TBGIFont.SetParams(AFill : Boolean; AGap : Integer; MXX,MYY,A : Real; AItalic : Byte);
Begin
  Inherited SetParams(AFill, Round(AGap * MXX));
  MX := MXX;
  MY := MYY;
  SN := Sin(Pi*A/180.0);
  CN := Cos(Pi*A/180.0);
  Alpha := A;
  Italic := AItalic;
End;

Function  TBGIFont.GetHeight   : Integer;
Begin
  GetHeight := Round(Inherited GetHeight*MY);
End;

Function   TBGIFont.SymbolFullHeight           : Integer;
Begin
  SymbolFullHeight := Round(Inherited GetHeight*MY);
End;

Function   TBGIFont.GetCharWidth(C : Char)      : Integer;
Begin
  GetCharWidth :=  Round(Inherited GetCharWidth(C)*MX);
End;

Procedure TBGIFont.Transform(x1,y1,x,y : Integer;  Var XX,YY : Integer);
Var
  X2,Y2 : Float;
Begin
  X2 := X1 + X*MX-X0 + y*Italic/20;
  Y2 := Y1 - Y*MY-Y0;
  XX := Round(x0+CN*X2+SN*Y2);
  YY := Round(Y0-SN*X2+CN*Y2);
End;

Procedure  TBGIFont.OutString(xx0,yy0 : Integer; S : String);
Begin
  x0 := xx0; y0 := yy0;
  Inherited OutString(xx0, yy0, S);
End;

function  TBGIFont.WrStr(x, y, Len: integer; const s: string; var T : TPaintInfo): integer;
Begin
  PaintInfo := @T;
{  SetParams((T.FontStyle and ftFill) <> 0, T.GapLength, T.FontWidth / Inherited GetCharWidth('A'),
            T.FontHeight / Inherited GetHeight, T.Escapement, T.Italic);}
  x0 := x; y0 := y;
  Inherited OutString(x, y, S);
End;

procedure  TBGIFont.SelectCaps(var F : TFont);
Begin
  SetParams((F.Style and ftFill) <> 0, F.GapLength, F.Width / Inherited GetCharWidth('A'),
            F.Height / Inherited GetHeight, F.Escapement, F.Italic);
End;

End.