{*******************************************************}
{                                                       }
{     L Z W   unit                                      }
{                                                       }
{     Процедуры для упаковки и распаковки данных        }
{     по методу LZW для Turbo Pascal 6.0                }
{                                                       }
{     Копирование запрещено (С), 1992, В.А.Володин      }
{     НТЦ "Модуль", Москва, Россия                      }
{                                                       }
{     Copyright (C) 1992, V.A.Volodin,                  }
{     NTC Module, Moscow, Russia                        }
{                                      Сентябрь 1992 г. }
{                                                       }
{*******************************************************}

{$O+,F+,X+,I-,R-}
unit LZW;

interface
uses
  Objects;

{    1. Обработка ошибок    }
{    -------------------    }


  const  { Коды ошибок }

    NoErrors         = 0;
    ReadStreamError  = 1;
    WriteStreamError = 2;
    CantOpenFile     = 3;
    SameFileExist    = 4;
    ReadFileError    = 5;
    WriteFileError   = 6;
    InvalidFormat    = 7;
    InvalidChecksum  = 8;
    InvalidDirectory = 9;
    NoMemory         = 10;

  const
    ErrorCode: Word = NoErrors;


{    2. поблочная упаковка/распаковка     }
{    --------------------------------     }

  const
    MaxStack   = $500;

  type
    PWord = ^TWord;
    TWord = array [0..32764] of Word;

    TIndex =
      record
        Pix  : Byte;
        Pref : Word;
        Next : Word;
        Used : Boolean;
      end;

    PIndexArray = ^TIndexArray;
    TIndexArray = array[0..$FFF] of TIndex;

    PLZWObject = ^TLZWObject;
    TLZWObject =
      object private
        StreamP: PStream;
        Format : Byte;

        ByteCount: Longint;

        LZWBuff : PWord;
        LZWInd  : Word;
        CurW    : Word;
        StartBit: Byte;

        CodePix : Byte;
        CodePref: Word;
        NextCode: Word;
        CodeLen : Byte; StartCodeLen: Byte;
        MaxCode : Word; StartMaxCode: Word;
        ClearCode: Word;
        EndCode  : Word;
        StartCode: Word;

        Indexes : TIndexArray;
      end;

    PLZWPacker = ^TLZWPacker;
    TLZWPacker =
      object (TLZWObject)
        constructor Init(var S: TStream; LZWFormat: Byte);
        destructor  Done;

        procedure BlockWrite(var Buf; Count: Word);

      private
        HashTable: array[0..$FFF] of Word;

        procedure PutCode(Code: Word);

        function  CodeExistinTable(APix: Byte; APref: Word): Boolean;
        procedure FreeHashTable;
      end;

    PLZWUnpacker = ^TLZWUnpacker;
    TLZWUnpacker =
      object (TLZWObject)
        constructor Init(var S: TStream; LZWFormat: Byte);
        destructor  Done;

        function  Eof: Boolean;

        procedure BlockRead(var Buf; Count: Word; var Result: Word);

      private
        LastPos : Longint;
        BuffSize: Word;
        EndFile : Boolean;

        Stack: array[1..MaxStack] of Byte;
        StackTop: Word;

        function GetCode: Word;
      end;


implementation
uses
  Dos,
  ExManag;

{ Обработка ошибок ------------------------------}


  var
    LZWEx: Exception;

  procedure LZWError(var Ex: Exception; Code: Word);
  begin
    ErrorCode:= Code;
    Raise(Ex, 0);
  end;

{ Нижний уровень --------------------------------}


  const
    MaxFindFree  = 100;
    LZWBuffSize  = 8192;

  type
    PByte = ^TByte;
    TByte = array [0..65530] of Byte;


{ TLZWPacker -------------------------------------}


  procedure TLZWPacker.PutCode(Code: Word);
  {- Записать в файл код CodePref длиной CodeLen -}
  var
    Remain: Byte;

{ ********************************************************************* }

    procedure WriteBuff;
    begin
      StreamP^.Write(LZWBuff^, LZWBuffSize shl 1);
      if StreamP^.Status <> stOk then
        LZWError(LZWEx, WriteStreamError);
      LZWInd:= 0;
    end;

  begin
    asm
      les   di, Self
      mov   cl, [es:di].TLZWPacker.StartBit
      mov   ch, 16
      sub   ch, cl
      mov   ax, Code
      shl   ax, cl
      or    ax, [es:di].TLZWPacker.CurW
      add   cl, [es:di].TLZWPacker.CodeLen
      cmp   cl, 16
      jb    @EndIf1
      mov   bx, [es:di].TLZWPacker.LZWInd
      les   di, [es:di].TLZWPacker.LZWBuff
      add   di, bx
      add   di, bx
      stosw
      les   di, Self
      inc   bx
      mov   [es:di].TLZWPacker.LZWInd, bx
      cmp   bx, LZWBuffSize
      jb    @EndIf2
      push  ax
      push  cx
      push  [bp].Word
      call  WriteBuff
      pop   cx
      pop   ax
      les   di, Self
@EndIf2:
      sub   cl, 16
      mov   ax, Code
      xchg  ch, cl
      shr   ax, cl
      xchg  ch, cl
@EndIf1:
      mov   [es:di].TLZWPacker.StartBit, cl
      mov   [es:di].TLZWPacker.CurW, ax
    end;
  end;


{ ********************************************************************* }

  procedure TLZWPacker.FreeHashTable;
    var i: Word;
  begin
    FillChar(HashTable, SizeOf(HashTable), #0);
    FillChar(Indexes, SizeOf(TIndexArray), #0);
  end;

{ ********************************************************************* }

  function TLZWPacker.CodeExistinTable(APix: Byte; APref: Word): boolean;
    var Ind, Ind0, HashF: word;
  begin
    asm
      xor   bh, bh
      mov   bl, APix
      mov   cl, 4
      shl   bx, cl
      xor   bx, APref
      and   bx, 0FFFh
      shl   bx, 1
      mov   HashF, bx
      les   di, Self
      mov   ax, [es:di].TLZWPacker.HashTable[bx].Word
      mov   Ind0, ax
@Loop:
      or    ax, ax   { while Ind <> 0 do }
      jz    @ELoop
      mov   bx, ax   { bx <- Ind*6 }
      shl   bx, 1
      add   bx, ax
      shl   bx, 1
      mov   dx, APref
      cmp   dx, [es:di].TLZWPacker.Indexes[bx+1].Word
      jne   @EndIf1
      mov   dl, APix
      cmp   dl, [es:di].TLZWPacker.Indexes[bx].Byte
      jne   @EndIf1
      mov   [es:di].TLZWPacker.CodePref.Word, ax
      mov   al, True
      mov   sp, bp
      pop   bp
      retf  6
@EndIf1:
      mov   ax, [es:di].TLZWPacker.Indexes[bx+3].Word
      jmp   @Loop
@ELoop:
      push  APref
      push  es
      push  di
      call  PutCode
      les   di, Self
      mov   dx, [es:di].TLZWPacker.NextCode
      cmp   dx, 0FFFh
      ja    @EndIf2
      mov   bx, dx
      shl   bx, 1
      add   bx, dx
      shl   bx, 1
      mov   al, APix
      mov   [es:di].TLZWPacker.Indexes[bx].Byte, al
      mov   ax, APref
      mov   [es:di].TLZWPacker.Indexes[bx+1].Word, ax
      mov   ax, Ind0
      mov   [es:di].TLZWPacker.Indexes[bx+3].Word, ax
      mov   bx, HashF
      mov   [es:di].TLZWPacker.HashTable[bx].Word, dx
@EndIf2:
    end;

    CodeExistinTable:= False;
  end;


{ ********************************************************************* }

  constructor TLZWPacker.Init(var S: TStream; LZWFormat: Byte);
    var i: Byte;
  begin
    if (ErrorCode <> NoErrors) or
       not (LZWFormat in [1,2,4,8])
    then
      exit;

    if EverythingOk(LZWEx) then
    begin
      StreamP:= @S;
      Format:= LZWFormat;

      FreeHashTable;

      if Format = 1 then
        StartCodeLen:= 3
      else
        StartCodeLen:= Format + 1;
      StartMaxCode:= (1 shl StartCodeLen) - 1;

      ClearCode:= 1 shl (StartCodeLen - 1);
      EndCode  := ClearCode + 1;
      StartCode:= ClearCode + 2;

      CodeLen:= StartCodeLen;
      MaxCode:= StartMaxCode;
      NextCode:= StartCode;

      if MaxAvail < LZWBuffSize shl 1 then LZWError(LZWEx, NoMemory);
      GetMem(LZWBuff, LZWBuffSize shl 1);

      LZWInd:= 0;
      CurW:= 0;
      StartBit:= 0;
      PutCode(ClearCode);

      ByteCount:= 0;
    end
    else
      Fail;
  end;


{ ********************************************************************* }

  destructor TLZWPacker.Done;
    var i, j: Word;
        PkSize: Longint;
  begin
    if ErrorCode = NoErrors then
    begin
      if EverythingOk(LZWEx) then
      begin
        if ByteCount <> 0 then
          PutCode(CodePref);

        PutCode(EndCode);

        if StartBit > 0 then
        begin
          LZWBuff^[LZWInd]:= CurW;
          if StartBit > 8 then
            StreamP^.Write(LZWBuff^, LZWInd shl 1 + 2)
          else
            StreamP^.Write(LZWBuff^, LZWInd shl 1 + 1)
        end
        else if LZWInd > 0 then
          StreamP^.Write(LZWBuff^, LZWInd shl 1);

        if StreamP^.Status <> stOk then
          LZWError(LZWEx, WriteStreamError);
      end;
    end;

    FreeMem(LZWBuff, LZWBuffSize shl 1);
  end;


{ ********************************************************************* }

  procedure TLZWPacker.BlockWrite(var Buf; Count: Word);
    var
      BuffP: PByte;
      k: Word;
  begin
    if ErrorCode <> NoErrors then
      exit;

    if EverythingOk(LZWEx) then
    begin
      BuffP:= @Buf;
      for k:= 0 to Count - 1 do
      begin
        if ByteCount = 0 then
        begin
          CodePref:= BuffP^[k];
          Inc(ByteCount);
        end
        else begin
          CodePix:= BuffP^[k];
          Inc(ByteCount);

          if not CodeExistinTable(CodePix, CodePref) then
          begin
            if NextCode > $FFF then
            begin
              PutCode(ClearCode);
              FreeHashTable;

              CodeLen:= StartCodeLen;
              MaxCode:= StartMaxCode;
              NextCode:= StartCode;
            end
            else
            begin
              if NextCode > MaxCode then
              begin
                inc(CodeLen);
                MaxCode:= MaxCode shl 1 + 1;
              end;
              inc(NextCode);
            end;
            CodePref:= CodePix;
          end;
        end;
      end;
    end;
  end;


{ TLZWUnpacker ---------------------------------------------------------}

{ ********************************************************************* }
  function TLZWUnpacker.GetCode: Word;
  {- Читать из файла код в Code длиной CodeLen -}
  var
    Res: Word;

    procedure ReadBuff;
    begin
      LastPos:= StreamP^.GetPos;
      if StreamP^.Status <> stOk then
        LZWError(LZWEx, ReadStreamError);

      BuffSize:= LZWBuffSize shl 1;
      if BuffSize > StreamP^.GetSize - LastPos then
        BuffSize:= StreamP^.GetSize - LastPos;

      if BuffSize = 0 then
        LZWError(LZWEx, InvalidFormat);

      StreamP^.Read(LZWBuff^, BuffSize);
      if StreamP^.Status <> stOk then
        LZWError(LZWEx, ReadStreamError);

      BuffSize:= (BuffSize + 1) shr 1;
      LZWInd:= 0;
    end;

  begin
    asm
      les   di, Self
      mov   dx, [es:di].TLZWUnpacker.MaxCode
      mov   cl, [es:di].TLZWUnpacker.StartBit
      mov   ch, 16
      sub   ch, cl
      mov   ax, [es:di].TLZWUnpacker.CurW
      shr   ax, cl
      and   ax, dx
      add   cl, [es:di].TLZWUnpacker.CodeLen
      cmp   cl, 16
      jb    @EndIf1
      mov   Res, ax
      mov   bx, [es:di].TLZWUnpacker.LZWInd
      cmp   bx, [es:di].TLZWUnpacker.BuffSize
      jb    @EndIf2
      push  cx
      push  dx
      push  [bp].Word
      call  ReadBuff
      pop   dx
      pop   cx
      les   di, Self
      mov   bx, 0
@EndIf2:
      les   di, [es:di].TLZWUnpacker.LZWBuff
      add   di, bx
      add   di, bx
      mov   ax, [es: di]
      les   di, Self
      mov   [es:di].TLZWUnpacker.CurW, ax
      inc   bx
      mov   [es:di].TLZWUnpacker.LZWInd, bx
      xchg  cl, ch
      shl   ax, cl
      xchg  cl, ch
      and   ax, dx
      or    ax, Res
      sub   cl, 16
@EndIf1:
      mov   Res, ax
      mov   [es:di].TLZWUnpacker.StartBit, cl
    end;

    GetCode:= Res;
  end;

{ ********************************************************************* }

  constructor TLZWUnpacker.Init(var S: TStream; LZWFormat: Byte);
  begin
    if ErrorCode <> NoErrors then
      exit;

    if EverythingOk(LZWEx) then
    begin
      StreamP:= @S;
      Format:= LZWFormat;

      if Format = 1 then
        StartCodeLen:= 3
      else
        StartCodeLen:= Format + 1;
      StartMaxCode:= (1 shl StartCodeLen) - 1;

      ClearCode:= 1 shl (StartCodeLen - 1);
      EndCode  := ClearCode + 1;
      StartCode:= ClearCode + 2;

      CodeLen:= StartCodeLen;
      MaxCode:= StartMaxCode;
      NextCode:= StartCode;

      StackTop:= 0;

      LZWBuff:= Nil;
      if MaxAvail < LZWBuffSize shl 1 then LZWError(LZWEx, NoMemory);
      GetMem(LZWBuff, LZWBuffSize shl 1);

      LZWInd:= 0;
      BuffSize:= 0;
      StartBit:= 16;

      if GetCode <> 0 then; { Читаем ClearCode }
      EndFile:= False;
      ByteCount:= 0;
    end
    else begin
      if LZWBuff <> Nil then
        FreeMem(LZWBuff, LZWBuffSize shl 1);
      Fail;
    end;
  end;

{ ********************************************************************* }

  destructor  TLZWUnpacker.Done;
  begin
    if ErrorCode = NoErrors then
    begin
      if EverythingOk(LZWEx) then
      begin
        if StartBit > 8 then
          StreamP^.Seek(LastPos + LZWInd shl 1)
        else
          StreamP^.Seek(LastPos + LZWInd shl 1 - 1);

        if StreamP^.Status <> stOk then
          LZWError(LZWEx, ReadStreamError);
      end
    end;

    FreeMem(LZWBuff, LZWBuffSize shl 1);
  end;

{ ********************************************************************* }

  function  TLZWUnpacker.Eof: Boolean;
  begin
    Eof:= EndFile;
  end;


{ ********************************************************************* }

  procedure TLZWUnpacker.BlockRead(var Buf; Count: Word; var Result: Word);
    var
      BuffP : PByte;
      k     : Word;
      Code  : Word;
      InCode: Word;
  begin
    if ErrorCode <> NoErrors then
      exit;

    if EndFile then
    begin
      Result:= 0;
      exit;
    end;

    if EverythingOk(LZWEx) then
    begin
      BuffP:= @Buf;
      k:= 0;
      while k < Count do
      begin
        if ByteCount = 0 then
        begin
          CodePref:= GetCode;
          CodePix := CodePref;
          BuffP^[k]:= CodePix;
          Inc(k);
          Inc(ByteCount);
        end
        else if StackTop > 0 then
        asm
          push  ds
          lds   si, Self
          mov   bx, [ds:si].TLZWUnpacker.StackTop
          les   di, BuffP
          add   di, k
          mov   cx, Count
          sub   cx, k
          mov   dx, cx
          cld
@Loop:
          sub   bx, 1
          jnz   @EndIf
          mov   al, [ds:si].TLZWUnpacker.Stack[bx].Byte
          stosb
          dec   cx
          jmp   @EndLoop
@EndIf:
          mov   al, [ds:si].TLZWUnpacker.Stack[bx].Byte
          stosb
          loop   @Loop
@EndLoop:
          mov   [ds:si].TLZWUnpacker.StackTop, bx
          sub   dx, cx
          add   k, dx
          add   [ds:si].TLZWUnpacker.ByteCount.Word, dx
          adc   [ds:si].TLZWUnpacker.ByteCount+2.Word, 0
          pop   ds
        end
        else begin
          Code:= GetCode; InCode:= Code;
          if Code = EndCode then
          begin
            EndFile:= True;
            Result:= k;
            Exit;
          end;

          if Code = ClearCode then
          begin
            CodeLen:= StartCodeLen;
            MaxCode:= StartMaxCode;
            NextCode:= StartCode;

            StackTop:= 0;

            CodePref:= GetCode;
            CodePix := CodePref;
            BuffP^[k]:= CodePix;
            Inc(k);
            Inc(ByteCount);
          end
          else
          asm
            les   di, Self
            mov   bx, [es:di].TLZWUnpacker.StackTop
            mov   ax, Code
            cmp   ax, [es:di].TLZWUnpacker.NextCode
            jb    @EndIf1
            mov   al, [es:di].TLZWUnpacker.CodePix
            mov   [es:di].TLZWUnpacker.Stack[bx].Byte, al
            inc   bx
            mov   ax, [es:di].TLZWUnpacker.CodePref
@EndIf1:
            mov   dx, bx
@Loop:
            cmp   ax, [es:di].TLZWUnpacker.EndCode
            jbe   @EndLoop
            mov   bx, ax
            shl   bx, 1
            add   bx, ax
            shl   bx, 1
            mov   al, [es:di].TLZWUnpacker.Indexes[bx].Byte
            xchg  bx, dx
            mov   [es:di].TLZWUnpacker.Stack[bx].Byte, al
            xchg  bx, dx
            inc   dx
            mov   ax, [es:di].TLZWUnpacker.Indexes[bx+1].Word
            jmp   @Loop
@EndLoop:
            mov   [es:di].TLZWUnpacker.StackTop, dx
            mov   [es:di].TLZWUnpacker.CodePix, al
            les   di, BuffP
            add   di, k
            inc   k
            stosb
            les   di, Self
            add   [es:di].TLZWUnpacker.ByteCount.Word, 1
            adc   [es:di].TLZWUnpacker.ByteCount+2.Word, 0
            mov   dx, [es:di].TLZWUnpacker.NextCode
            mov   bx, dx
            shl   bx, 1
            add   bx, dx
            shl   bx, 1
            mov   al, [es:di].TLZWUnpacker.CodePix
            mov   [es:di].TLZWUnpacker.Indexes[bx].Byte, al
            mov   ax, [es:di].TLZWUnpacker.CodePref
            mov   [es:di].TLZWUnpacker.Indexes[bx+1].Word, ax
            inc   dx
            mov   [es:di].TLZWUnpacker.NextCode, dx
            cmp   dx, [es:di].TLZWUnpacker.MaxCode
            jbe   @EndIf
            cmp   [es:di].TLZWUnpacker.CodeLen, 12
            jae   @EndIf
            inc   [es:di].TLZWUnpacker.CodeLen
            mov   bx, [es:di].TLZWUnpacker.MaxCode
            shl   bx, 1
            inc   bx
            mov   [es:di].TLZWUnpacker.MaxCode, bx
@EndIf:
            mov   ax, InCode
            mov   [es:di].TLZWUnpacker.CodePref, ax
          end
        end;
      end;
      Result:= Count;
    end
    else
      Result:= 0;
  end;
END.
