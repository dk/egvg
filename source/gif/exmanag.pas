{$R-,S-,F+,I-}

unit ExManag;

interface

  type Exception =
    record
      Result : word;
      Private: array[1..9] of byte;
    end;

  function  EverythingOk(var Ex: Exception): Boolean;
  procedure Raise(var Ex: Exception; Result: word);
  function Occured(var Ex: Exception): Boolean;

implementation

  function  EverythingOk(var Ex: Exception): Boolean;
  begin
    asm
      les   di, Ex
      cld

      xor   ax, ax          { Ex.Result:= 0 }
      stosw

      stosb                 { Ex.Occured:= False }

      mov   ax, bp          { Ex.SPReg:= BP + 10 }
      add   ax, 10
      stosw

      mov   ax, [bp].word   { Ex.BPReg:= [BP] }
      stosw

      mov   ax, [bp+2].word { Ex.ExitPoint:= [BP+2] }
      stosw
      mov   ax, [bp+4].word
      stosw
    end;
    EverythingOk:= true;
  end;

  procedure Raise(var Ex: Exception; Result: word);
  begin
    asm
      les   di, Ex
      cld

      mov   ax, Result { Ex.Result:= Result }
      stosw

      mov   al, True   { Ex.Occuder:= True  }
      stosb

      mov   sp, [es: di]   { SP:= Ex.SPReg }
      mov   bp, [es: di+2] { BP:= Ex.BPReg }

      mov   ax, [es: di+6] { Push Ex.Exitpoint }
      push  ax
      mov   ax, [es: di+4]
      push  ax

      xor   ax, ax         { EverythingOk:= false }
      retf
    end;
  end;

  function Occured(var Ex: Exception): Boolean;
    var
      Res: Boolean;
  begin
    asm
      les   di, Ex
      mov   al, [es: di+2]
      mov   Res, al
    end;
    Occured:= Res;
  end;

begin
end.