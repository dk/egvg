{
  Internal flick loading.
  Main code/idea by Vladimir Vysotsky 2:463/52.88
  Adapted for SVG by Dmitry Karasik.
  Can use 320x200x256 only
}

Unit Flick;

Interface

uses Objects, Memory, Drivers, Views, GDI;

const
  cmNextFlickFrame = $5288;

const
  bqPalette = 0;
  bqPicture = 1;
  bqErase   = 2;
  bqComp    = 3;

type
  PFlickFrame = ^TFlickFrame;
  TFlickFrame = Object
    What     : Byte;
    Data     : PByteArray;
    Next     : PFlickFrame;
    At, Size : Word;
    W, H     : Word;
    Function   InitPic(Src : Pointer; _W, _H, AAt : Word) : Boolean;
    Function   InitCompressed(Src : Pointer; _W, _H, AAt, As, ASize : Word) : Boolean;
    Function   InitPal(Pal : Pointer) : Boolean;
    Procedure  InitFX(FX : Byte);
    Procedure  Insert(ANext : PFlickFrame);
    Procedure  Delete;
    Function   UnCompress(P : PWordArray) : Pointer;
  End;

  PFLIView = ^TFLIView;
  TFLIView = Object(TView)
    F, N      : PFlickFrame;
    UseBar    : Boolean;
    DrawFirst : Boolean;
    Buf       : Pointer;
    FrameToShow : PImage;
    At, W, H    : Word;
    Constructor Init(var S : TStream);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
    Destructor  Done;   Virtual;
    Procedure   Draw;   Virtual;
    Procedure   ProcessFrame;
  End;

function   LoadFlick(var Flic : TStream; Decompress : Boolean) : PFlickFrame;
procedure  FreeFramesQueue(First : PFlickFrame);

Implementation

const
  sfeOk      = 0;
  sfeBadMode = -1;
  sfeStream  = -2;
  sfeBadFlic = -3;
  sfeTooBig  = -4;

  sgOldFlic = $AF11;
  sgNewFlic = $AF12;
  sgFrame   = $F1FA;

  btCompHiPal   = $0004;
  btWordDelta   = $0007;
  btCompPalette = $000B;
  btCompDelta   = $000C;
  btEraseScreen = $000D;
  btCompScreen  = $000F;
  btScreen64k   = $0010;

  MaxSize = 65528;

type
  PHeader     = ^THeader;
  THeader     = record
    Size      : longint;
    Signature : word;
  end;

  TFlicHeader = record
    Header    : THeader;
    FrameNo   : word;

    Width     : word;
    Height    : word;
    Some1     : word;
    Some2     : word;

    Speed     : word;
    Some3     : word;
    Some4     : word;
    Some5     : word;
    Pad : array [0..$67] of byte;
  end;

  PFrameHeader = ^TFrameHeader;
  TFrameHeader = record
    Header    : THeader;
    ChunkNo   : word;
    Pad       : array [0..7] of byte;
  end;

  TFrameRec  = record
    Start : longint;
    Size  : longint;
  end;

  PFrameList = ^TFrameList;
  TFrameList = array [0..0] of TFrameRec;


procedure RLE1 (RLE1Src,RLE1Dst : pointer; RLE1LineLen : word);
  far; external;
procedure RLE2 (RLE2Src,RLE2Dst : pointer; RLE2LineLen,RLE2LineCount : word);
  far; external;
procedure RLE3 (RLE3Src, RLE3Dst : pointer; RLE3LineLen : word);
  far; external;
{$L FlicUtil}

procedure  FreeFramesQueue(First : PFlickFrame);
Var
  F : PFlickFrame;
Begin
  while First <> Nil do begin
    First^.Delete;
    F := First^.Next;
    Dispose(First);
    First := F;
  end;
End;

Function  TFlickFrame.InitPal;
Begin
  Next := Nil;
  InitPal := False;
  What := bqPalette;
  Data := MemAlloc(768);
  if Data = Nil then Exit;
  Move(Pal^, Data^, 768);
  InitPal := True;
  Size := 768;
End;

Function  TFlickFrame.InitPic;
Var
  D : PWordArray;
Begin
  InitPic := False;
  Next := Nil;
  W := _W; H := _H;
  What := bqPicture; At := AAt;
  Data := MemAlloc(W * H + 9);
  if Data = Nil then Exit;
  Pointer(D) := Data;
  D^[0] := $1970;
  D^[1] := H;
  D^[2] := 256;
  D^[3] := W;
  Inc(LongInt(Src), W * At);
  Move(Src^, D^[4], W * H);
  Size := W * H + 9;
  InitPic := True;
End;

Function  TFlickFrame.InitCompressed;
Begin
  W := _W; H := _H;
  InitCompressed := False;
  Next := Nil;
  What := bqComp + As;
  At := AAt;
  Data := MemAlloc(ASize);
  if Data = Nil then Exit;
  Move(Src^, Data^, ASize);
  Size := ASize;
  InitCompressed := True;
End;

Procedure TFlickFrame.InitFX(FX : Byte);
Begin
  What := FX;
  Data := Nil;
  Next := Nil;
End;

Procedure TFlickFrame.Insert;
Begin
  Next := ANext;
End;

Procedure TFlickFrame.Delete;
Begin
  if Data <> Nil then FreeMem(Data, Size);
End;

Function TFlickFrame.UnCompress(P : PWordArray) : Pointer;
Begin
  if What >= bqComp then case What - bqComp of
  0: RLE1(Data, @P^[4], W);
  1: RLE2(Data, @P^[4], W, H);
  2: RLE3(Data, @P^[4], W);
  else end;
  Inc(LongInt(P), At * 320 + 8);
  UnCompress := P;
End;

function LoadFlick(var Flic : TStream; Decompress : Boolean) : PFlickFrame;
var
  RetValue : integer;

procedure StreamTest;
begin
  if Flic.Status<>stOk then
    RetValue:=sfeStream;
end;

var
  FirstFrame   : PFlickFrame;
  CurrentFrame : PFlickFrame;

var
  FlicStart  : longint;
  FlicHeader : TFlicHeader;
  FrameList  : PFrameList;
  FramePos   : longint;
  FrameCnt   : word;
  FrameHeader : TFrameHeader;
  FrameBuf     : PFrameHeader;
  MaxFrameSize : longint;
  ChunkCnt   : word;
  ChunkPtr   : PChar;
  CHPtr      : PHeader;
  ScreenPtr  : pointer;
  Count      : integer;
  AP         : TDOSStream;

Procedure FreeFrames;
Begin
  FreeFramesQueue(FirstFrame);
End;

Procedure Palette(Hi : Boolean);
Var
  Piece, I     : Word;
  Start, Len, J: Byte;
  P            : Pointer;
  Pal          : PByteArray;
Begin
  P := ChunkPtr;
  FillChar(ScreenPtr^, 768, 0);
  Piece := Word(ChunkPtr^);
  Inc(LongInt(P), 2);
  if Piece = 0 then Exit;
  Pal := ScreenPtr;
  for I := 0 to Piece - 1 do begin
    Start := Byte(P^); Inc(LongInt(P));
    Len   := Byte(P^) - 1; Inc(LongInt(P));
    for J := 0 to Len do begin
      Pal^[Start * 3 + 0] := Byte(P^); Inc(LongInt(P));
      Pal^[Start * 3 + 1] := Byte(P^); Inc(LongInt(P));
      Pal^[Start * 3 + 2] := Byte(P^); Inc(LongInt(P));
      Inc(Start);
    end;
  end;
  if not Hi then for I := 0 to 767 do Pal^[I] := Pal^[I] shl 2;
End;

Function  NewFrame : PFlickFrame;
Var
  F : PFlickFrame;
Begin
  New(F);
  if FirstFrame = Nil then FirstFrame := F else CurrentFrame^.Insert(F);
  NewFrame     := F;
  CurrentFrame := F;
End;

Label NoWay, FreeAll;

begin
  LoadFlick := Nil;
  RetValue := sfeOk;
  ScreenPtr := MemAlloc(64000);
  if ScreenPtr = Nil then Exit;
  FirstFrame   := Nil;
  CurrentFrame := Nil;
  repeat
    StreamTest;
    if RetValue <> sfeOk then Goto NoWay;
    FlicStart  := Flic.GetPos;
    Flic.Read(FlicHeader, SizeOf(FlicHeader));
    StreamTest;
    if RetValue<>sfeOk then Goto NoWay;
    with FlicHeader do begin
      if (not ((Header.Signature=sgOldFlic) or
               (Header.Signature=sgNewFlic))) or
         (Width>320) or (Height>200) then Goto NoWay;
      FrameList := MemAlloc((FrameNo+1)*SizeOf(TFrameRec));
      if FrameList = Nil then Goto NoWay;
      FrameCnt  := 0;
      MaxFrameSize := 0;
      repeat
        FramePos := Flic.GetPos;
        Flic.Read(FrameHeader, SizeOf(FrameHeader));
        if FrameHeader.Header.Signature=sgFrame then
        begin
          FrameList^[FrameCnt].Start:=FramePos;
          FrameList^[FrameCnt].Size :=FrameHeader.Header.Size;
          if MaxFrameSize<FrameHeader.Header.Size then
            MaxFrameSize:=FrameHeader.Header.Size;
          Inc(FrameCnt);
        end;
        Flic.Seek(FramePos+FrameHeader.Header.Size);
        StreamTest;
      until (FrameCnt=FrameNo+1) or (RetValue<>sfeOk);
      if MaxFrameSize>MaxSize then
        RetValue:=sfeBadFlic;
      if RetValue<>sfeOk then
        break;
      { do the show - everything Ok }
      FrameBuf := MemAlloc(MaxFrameSize);
      if FrameBuf = Nil then begin
        FreeMem(FrameList,(FrameNo+1)*SizeOf(TFrameRec));
        Goto NoWay;
      end;
      FrameCnt := 0;

      repeat
        Flic.Seek(FrameList^[FrameCnt].Start);
        Flic.Read(FrameBuf^,FrameList^[FrameCnt].Size);
        ChunkPtr := PChar(FrameBuf) + SizeOf(TFrameHeader) + SizeOf(THeader);


        for ChunkCnt := 1 to FrameBuf^.ChunkNo do
        begin
          CHPtr := PHeader(ChunkPtr - SizeOf(THeader));
          case CHPtr^.Signature of
            btCompHiPal   : begin
              Palette(True);
              if not NewFrame^.InitPal(ScreenPtr) then begin
                FreeFrames;
                Goto FreeAll;
              end;
            end;
            btWordDelta   : begin
              if Decompress then begin
                RLE3(ChunkPtr,ScreenPtr,Width);
                if not NewFrame^.InitPic(ScreenPtr, Width, {Word(ChunkPtr^)}Height, 0) then begin
                   FreeFrames;
                   Goto FreeAll;
                 end;
              end else if not NewFrame^.InitCompressed(ChunkPtr, Width,
                 {Word(ChunkPtr^)}Height, 0, 2, CHPtr^.Size-6)
                 then begin
                   FreeFrames;
                   Goto FreeAll;
                 end;
            end;
            btCompPalette : begin
              Palette(False);
              if not NewFrame^.InitPal(ScreenPtr) then begin
                FreeFrames;
                Goto FreeAll;
              end;
            end;
            btCompDelta   : begin
              if Decompress then begin
                RLE1(ChunkPtr,ScreenPtr,Width);
                if not NewFrame^.InitPic(ScreenPtr, Width,
                  Word(Pointer(LongInt(ChunkPtr) + 2)^),
                  Word(ChunkPtr^)) then begin
                  FreeFrames;
                  Goto FreeAll;
                end;
              end else
              if not NewFrame^.InitCompressed(ChunkPtr, Width,
                Word(Pointer(LongInt(ChunkPtr) + 2)^), Word(ChunkPtr^),
                0, CHPtr^.Size-6) then begin
                  FreeFrames;
                  Goto FreeAll;
                end;
            end;
            btEraseScreen : NewFrame^.InitFX(bqErase);
            btCompScreen  : begin
              if Decompress then begin
                RLE2(ChunkPtr,ScreenPtr,Width,Height);
                if not NewFrame^.InitPic(ScreenPtr, Width, Height, 0) then begin
                  Goto FreeAll;
                  FreeFrames;
                end;
              end else
              if not NewFrame^.InitCompressed(ChunkPtr, Width, Height, 0, 1,
              CHPtr^.Size-6) then begin
                Goto FreeAll;
                FreeFrames;
              end;
            end;
            btScreen64k   : begin
              Move(ChunkPtr^,ScreenPtr^,64000);
              if not NewFrame^.InitPic(ScreenPtr, 320, 200, 0) then begin
                FreeFrames;
                Goto FreeAll;
              end;
            end;
          end;
          Inc(ChunkPtr,CHPtr^.Size);
        end;

        inc(FrameCnt);
      until FrameCnt = FrameNo;

      LoadFlick:= FirstFrame;

      Flic.Seek(FlicStart);
FreeAll:
      FreeMem(FrameList,(FlicHeader.FrameNo+1)*SizeOf(TFrameRec));
      FreeMem(FrameBuf,MaxFrameSize);

    end; { with }
    break;
  until FALSE; { this is dummy loop end }
NoWay:
  FreeMem(ScreenPtr, 64000);
end;

Constructor TFLIView.Init;
Var
  R : TRect;
  _F : PFlickFrame;
Begin
  _F := LoadFlick(S, False);
  if _F = Nil then Fail;
  R.Assign(10, 27, 10 + 320, 27 + 200);
  Inherited Init(R);
  Buf := CreateDImageIndirect(320, 200, 256, cbwSetAlloc + cbwAllocFlat);
  if Buf = Nil then begin
    FreeFramesQueue(F);
    Fail;
  end;
  {if IsImageStreamed(Buf) then begin
    FreeDImage(Buf);
    FreeFramesQueue(F);
    Fail;
  end;}
  FillChar(PSImage(Buf)^.Data, GetImageSize(Buf) - 9, 0);
  F := _F;
  N := _F;
  EventMask := EventMask or evBroadCast;
  while N <> Nil do begin
    if (N^.What = bqPalette) then begin
      LogPalette.Mode := pmUseRGB + pmOptimize;
      CreatePalette(N^.Data, LogPalette, 0, 256);
      N := F;
      Break;
    end;
    N := N^.Next;
  end;
  UseBar := True;
  DrawFirst := False;
End;

Destructor TFLIView.Done;
Begin
  FreeDImage(Buf);
  FreeFramesQueue(F);
  Inherited Done;
End;

Procedure TFliView.HandleEvent(var Event : TEvent);
Begin
  if (Event.What = evBroadCast) then
    case Event.Command of
      cmNextFlickFrame: begin
        UseBar := False;
        TimedMessage(1, Owner, evBroadcast, cmNextFlickFrame, Nil, 0);
        DrawFirst := False;
        if not Locked(Owner) then begin
          ProcessFrame;
          DrawView;
          N := N^.Next;
          if N = Nil then N := F;
        end;
        UseBar := true;
      end;
   else end;
  Inherited HandleEvent(Event);
End;

Procedure TFliView.ProcessFrame;
Begin
  if N^.What = bqPalette then begin
    N := N^.Next;
    if N = Nil then N := F;
  end;

  case N^.What of
  bqErase  : begin
    FillChar(Pointer(LongInt(Buf) + 8)^, 64000, RGBIndirect(0,0,0,0,LogPalette.Colors,LogPalette.Palette));
    FrameToShow := Nil;
  end;
  bqPicture: begin
    Move(Pointer(LongInt(N^.Data) + 8)^,
         Pointer(LongInt(Buf) + 8 + 320 * N^.At)^, N^.W * N^.H);
    FrameToShow := Pointer(LongInt(Buf) + 8 + 320 * N^.At);
    At := N^.At;
    W  := N^.W;
    H  := N^.H;
  end;
  bqComp..bqComp+2: begin
      FrameToShow := N^.UnCompress(Buf);
      At := N^.At;
      W  := N^.W;
      H  := N^.H;
  end;
  else end;
End;

Procedure TFLIView.Draw;
Var
  P : PWordArray;
  RSave : array[0..7] of Byte;
Begin
  if UseBar then begin
    {if not DrawFirst then} PutBMP(PImage(Buf), 0, 0);
  end else if FrameToShow <> Nil then begin
    P := Pointer(FrameToShow);
    Dec(LongInt(P), 8);
    Move(P^, RSave, 8);
    P^[0] := $1970;
    P^[1] := H;
    P^[2] := 256;
    P^[3] := W;
    PutBMP(PImage(P), 0, At);
    Move(RSave, P^, 8);
  end else Bar(0, 0, Size.X, Size.Y, 0);
End;

End.
