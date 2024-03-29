{TONY}
{$S-,R-,V-,I-,B-,F-,O-,A-}
{$IFNDEF Ver70}
  !!! This unit does purposed for only Borland Pascal 7.0 !!!
{$ENDIF}

{*********************************************************}
{*                  EGINLINE.PAS 1.00                    *}
{*  based upon OPINLINE.PAS 1.20 by TurboPower Software  *}
{*********************************************************}

unit EgInline;
  {-Assorted inline macros}

interface

type
  JumpRecord =
    record
      SpReg, BpReg : Word;
      JmpPt : Pointer;
    end;

  OS =
    record
      O, S : Word;
    end;

  LH =
    record
      L, H : Word;
    end;

function SetJump(var JumpDest : JumpRecord) : Word;
  {-Save current SP, BP, and a jump destination}
  inline(
    $5F/                     {pop di    ;di = Ofs(JmpDest)}
    $07/                     {pop es    ;es = Seg(JmpDest)}
    $FC/                     {cld}
    $89/$E0/                 {mov ax,sp ;save sp}
    $AB/                     {stosw}
    $89/$E8/                 {mov ax,bp ;save bp}
    $AB/                     {stosw}
    $E8/$00/$00/             {call null ;push IP onto stack}
                             {null:}
    $58/                     {pop ax    ;pop into ax}
    $05/$0A/$00/             {add ax,10 ;point to "next:"}
    $AB/                     {stosw     ;save jump offset}
    $8C/$C8/                 {mov ax,cs ;save jump segment}
    $AB/                     {stosw}
    $31/$C0);                {xor ax,ax ;return ax = 0}
                             {next:}

procedure LongJump(var JumpDest : JumpRecord; Result : Word);
  {-Restore SP, BP, and jump to JumpDest.JmpPt}
  inline(
    $58/                     {pop ax            ;ax = Result}
    $5F/                     {pop di            ;di = Ofs(JumpDest)}
    $07/                     {pop es            ;es = Seg(JumpDest)}
    $26/$8B/$25/             {mov sp,es:[di]    ;restore sp}
    $26/$8B/$6D/$02/         {mov bp,es:[di+2]  ;restore bp}
    $26/$FF/$6D/$04);        {jmp far es:[di+4] ;jump far to JumpDest.JmpPt}

procedure FarCall(ProcAddr : Pointer);
  {-ProcAddr is the address of a routine to be called far. Can be used to
    implement jump tables if procedures take no parameters.}
  inline(
    $89/$E3/                 {mov bx,sp}
    $36/$FF/$1F/             {call far dword ptr ss:[bx]}
    $81/$C4/$04/$00);        {add sp,4}

procedure NearCall(ProcOfs : Word);
  {-ProcOfs is the offset of a routine to be called near.}
  inline(
    $5B/                     {pop bx}
    $FF/$D3);                {call bx}

procedure JumpToOldIsr(OldIsr : Pointer);
  {-Jump to previous ISR from an interrupt procedure.}
  inline(
    $5B/                     {pop bx          ;BX = Ofs(OldIsr)}
    $58/                     {pop ax          ;AX = Seg(OldIsr)}
    $87/$5E/$0E/             {xchg bx,[bp+14] ;Switch old BX and Ofs(OldIsr)}
    $87/$46/$10/             {xchg ax,[bp+16] ;Switch old AX and Seg(OldIsr)}
    $89/$EC/                 {mov sp,bp       ;Restore SP}
    $5D/                     {pop bp          ;Restore BP}
    $07/                     {pop es          ;Restore ES}
    $1F/                     {pop ds          ;Restore DS}
    $5F/                     {pop di          ;Restore DI}
    $5E/                     {pop si          ;Restore SI}
    $5A/                     {pop dx          ;Restore DX}
    $59/                     {pop cx          ;Restore CX}
                             {;BX and AX restored earlier; their places on stack}
                             {;now have OldIsr, which is where return will go}
    $CB);                    {retf            ;Chain to OldIsr}

function MakeLongInt(H, L : Word) : LongInt;
  {-Constructs a LongInt from two Words}
  inline(
    $58/                     {pop ax  ;low word into AX}
    $5A);                    {pop dx  ;high word into DX}

function MakeInteger(H, L : Byte): Integer;
  {-Constructs an integer from two bytes}
  inline(
    $58/                     {pop ax    ;low byte into AL}
    $5B/                     {pop bx    ;high byte into BL}
    $88/$DC);                {mov ah,bl ;high byte into AH}

function MakeWord(H, L : Byte) : Word;
  {-Constructs a word from two bytes}
  inline(
    $58/                     {pop ax    ;low byte into AL}
    $5B/                     {pop bx    ;high byte into BL}
    $88/$DC);                {mov ah,bl ;high byte into AH}

function Array2Str(var A; Len : Byte) : string;
  {-Convert an array of char to a string}
  inline(
    $8C/$DB/                 {mov bx,ds ;save DS}
    $59/                     {pop cx    ;CX = Len}
    $30/$ED/                 {xor ch,ch}
    $5E/                     {pop si    ;ds:si => A}
    $1F/                     {pop ds}
    $5F/                     {pop di    ;es:di => result}
    $07/                     {pop es}
    $06/                     {push es   ;put the pointer back on the stack}
    $57/                     {push di}
    $FC/                     {cld       ;go forward}
    $88/$C8/                 {mov al,cl ;set the length byte}
    $AA/                     {stosb}
    $F2/$A4/                 {rep movsb ;move data into string}
    $8E/$DB);                {mov ds,bx ;restore DS}

procedure CallOldIsr(OldIsr : Pointer);
  {-Call previous ISR from an interrupt procedure. Destroys BX.}
  inline(
    $89/$E3/                 {mov bx,sp        ;set up stack frame}
    $9C/                     {pushf            ;push flags to simulate int}
    $36/$FF/$1F/             {call far ss:[bx] ;call OldIsr}
    $81/$C4/$04/$00);        {add sp,4         ;get rid of OldIsr}

procedure Reboot;
  {-Reboot the machine}
  inline(
    $B8/$40/$00/             {mov ax,$40}
    $8E/$D8/                 {mov ds,ax}
    $C7/$06/$72/$00/$34/$12/ {mov word ptr [$0072],$1234}
    $EA/$00/$00/$FF/$FF);    {jmp far $FFFF:$0000}

function HiWord(L : LongInt) : Word;
  {-Return high-order word of L}
  inline(
    $58/                     {pop ax ;ignore low word}
    $58);                    {pop ax ;pop high word into AX}

function LoWord(L : LongInt) : Word;
  {-Return low-order word of L}
  inline(
    $58/                     {pop ax ;pop low word into AX}
    $5A);                    {pop dx ;ignore high word}

function SwapNibble(B : Byte) : Byte;
  {-Swap the high and low nibbles of B: SwapNibble($F0) returns $0F.}
  inline(
    $58/                     {pop ax}
    $B1/$04/                 {mov cl,4}
    $D2/$C8);                {ror al,cl}

function SwapWord(L : LongInt) : LongInt;
  {-Swap low- and high-order words of L}
  inline(
    $5A/                     {pop dx ;pop low word into DX}
    $58);                    {pop ax ;pop high word into AX}

function Normalized(P : Pointer) : Pointer;
  {-Return P as a normalized pointer}
  inline(
    $58/                     {pop ax    ;pop offset into AX}
    $5A/                     {pop dx    ;pop segment into DX}
    $89/$C3/                 {mov bx,ax ;BX = Ofs(P^)}
    $B1/$04/                 {mov cl,4  ;CL = 4}
    $D3/$EB/                 {shr bx,cl ;BX = Ofs(P^) div 16}
    $01/$DA/                 {add dx,bx ;add BX to segment}
    $25/$0F/$00);            {and ax,$F ;mask out unwanted bits in offset}

procedure SetFlag(var Flags : Word; FlagMask : Word);
  {-Set the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                     {pop ax        ;FlagMask into AX}
    $5F/                     {pop di}
    $07/                     {pop es        ;ES:DI => Flags}
    $26/$09/$05);            {or es:[di],ax ;Flags := Flags or FlagMask}{!!.03}

procedure ClearFlag(var Flags : Word; FlagMask : Word);
  {-Clear the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                     {pop ax         ;FlagMask into AX}
    $5F/                     {pop di}
    $07/                     {pop es         ;ES:DI => Flags}
    $F7/$D0/                 {not ax         ;FlagMask := not FlagMask}
    $26/$21/$05);            {and es:[di],ax ;Flags := Flags and not FlagMask}

function FlagIsSet(Flags, FlagMask : Word) : Boolean;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  inline(
    $5A/                     {pop dx    ;FlagMask into DX}
    $58/                     {pop ax    ;Flags into AX}
    $21/$D0/                 {and ax,dx ;Mask out everything not in FlagMask}
    $74/$02/                 {jz  Exit}
    $B0/$01);                {mov al,1  ;AL = Ord(True)}
                             {Exit:}

procedure SetByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Set the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                     {pop ax        ;FlagMask into AL}
    $5F/                     {pop di}
    $07/                     {pop es        ;ES:DI => Flags}
    $26/$08/$05);            {or es:[di],al ;Flags := Flags or FlagMask}

procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Clear the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                     {pop ax         ;FlagMask into AL}
    $5F/                     {pop di}
    $07/                     {pop es         ;ES:DI => Flags}
    $F6/$D0/                 {not al         ;AL := not AL}
    $26/$20/$05);            {and es:[di],al ;Flags := Flags and not FlagMask}

function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  inline(
    $5A/                     {pop dx    ;FlagMask into DL}
    $58/                     {pop ax    ;Flags into AL}
    $20/$D0/                 {and al,dl ;Mask out everything not in FlagMask}
    $74/$02/                 {jz  Exit}
    $B0/$01);                {mov al,1  ;AL = Ord(True)}
                             {Exit:}

procedure SetLongFlag(var Flags : LongInt; FlagMask : LongInt);
  {-Set the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                     {pop ax        ;FlagMask into DX:AX}
    $5A/                     {pop dx}
    $5F/                     {pop di}
    $07/                     {pop es        ;ES:DI => Flags}
    $26/$09/$05/             {or es:[di],ax ;Flags := Flags or FlagMask}
    $26/$09/$55/$02);        {or es:[di+2],dx}

procedure ClearLongFlag(var Flags : LongInt; FlagMask : LongInt);
  {-Clear the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                     {pop ax         ;FlagMask into DX:AX}
    $5A/                     {pop dx}
    $5F/                     {pop di}
    $07/                     {pop es         ;ES:DI => Flags}
    $F7/$D0/                 {not ax         ;FlagMask := not FlagMask}
    $F7/$D2/                 {not dx}
    $26/$21/$05/             {and es:[di],ax ;Flags := Flags and not FlagMask}
    $26/$21/$55/$02);        {and es:[di+2],dx}

function LongFlagIsSet(Flags, FlagMask : LongInt) : Boolean;
  {-Returns True if the bit specified by FlagMask is set in Flags}
  inline(
    $5B/                     {pop bx    ;FlagMask into CX:BX}
    $59/                     {pop cx}
    $58/                     {pop ax    ;Flags into DX:AX}
    $5A/                     {pop dx}
    $21/$D8/                 {and ax,bx ;Mask out everything not in FlagMask}
    $21/$CA/                 {and dx,cx}
    $09/$D0/                 {or  ax,dx}
    $74/$02/                 {jz  Exit}
    $B0/$01);                {mov al,1  ;AL = Ord(True)}
                             {Exit:}

procedure ExchangeBytes(var I, J : Byte);
  {-Exchange bytes I and J}
  inline(
    $8C/$DB/                 {mov bx,ds       ;save DS}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $8A/$04/                 {mov al,[si]     ;AL = J}
    $26/$86/$05/             {xchg al,es:[di] ;I = J, AL = I}
    $88/$04/                 {mov [si],al     ;J = I}
    $8E/$DB);                {mov ds,bx       ;restore DS}

procedure ExchangeWords(var I, J : Word);
  {-Exchange words I and J}
  inline(
    $8C/$DB/                 {mov bx,ds       ;save DS}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $8B/$04/                 {mov ax,[si]     ;AX = J}
    $26/$87/$05/             {xchg ax,es:[di] ;I = J, AX = I}
    $89/$04/                 {mov [si],ax     ;J = I}
    $8E/$DB);                {mov ds,bx       ;restore DS}

procedure ExchangeLongInts(var I, J : LongInt);
  {-Exchange LongInts I and J}
  inline(
    $8C/$DB/               {mov bx,ds       ;save DS}
    $5E/                   {pop si}
    $1F/                   {pop ds          ;DS:SI => J}
    $5F/                   {pop di}
    $07/                   {pop es          ;ES:DI => I}
    $FC/                   {cld}
    $26/$8B/$05/           {mov ax,es:[di]}
    $A5/                   {movsw}
    $89/$44/$FE/           {mov [si-2],ax}
    $8B/$04/               {mov ax,[si]}
    $26/$87/$05/           {xchg ax,es:[di]}
    $89/$04/               {mov [si],ax}
    $8E/$DB);              {mov ds,bx       ;restore DS}

procedure ExchangeStructs(var I, J; Size : Word);
  {-Exchange structures I and J. Useful in sorts}
  inline(
    $FC/                     {cld       ;go forward}
    $8C/$DA/                 {mov dx,ds       ;save DS}
    $59/                     {pop cx          ;CX = Size}
    $5E/                     {pop si}
    $1F/                     {pop ds          ;DS:SI => J}
    $5F/                     {pop di}
    $07/                     {pop es          ;ES:DI => I}
    $D1/$E9/                 {shr cx,1        ;move by words}
    $E3/$0C/                 {jcxz odd}
    $9C/                     {pushf}
                             {start:}
    $89/$F3/                 {mov bx,si}
    $26/$8B/$05/             {mov ax,es:[di]  ;exchange words}
    $A5/                     {movsw}
    $89/$07/                 {mov [bx],ax}
    $E2/$F6/                 {loop start      ;again?}
    $9D/                     {popf}
                             {odd:}
    $73/$07/                 {jnc exit}
    $8A/$04/                 {mov al,[si]     ;exchange the odd bytes}
    $26/$86/$05/             {xchg al,es:[di]}
    $88/$04/                 {mov [si],al}
                             {exit:}
    $8E/$DA);                {mov ds,dx       ;restore DS}


function MinWord(A, B : Word) : Word;
  {-Returns the smaller of A and B}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $73/$02/                 {jae done}
    $89/$D8);                {mov ax,bx}
                             {done:}

function MaxWord(A, B : Word) : Word;
  {-Returns the greater of A and B}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $76/$02/                 {jbe done}
    $89/$D8);                {mov ax,bx}
                             {done:}

function MinInteger(A, B : Integer) : Integer;
  {-Returns the smaller of A and B}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $7F/$02/                 {jg done}
    $89/$D8);                {mov ax,bx}
                             {done:}

function MaxInteger(A, B : Integer) : Integer;
  {-Returns the greater of A and B}
  inline(
    $58/                     {pop ax}
    $5B/                     {pop bx}
    $39/$C3/                 {cmp bx,ax}
    $7C/$02/                 {jl done}
    $89/$D8);                {mov ax,bx}
                             {done:}


function MinLong(A, B : LongInt) : LongInt;
  {-Returns the smaller of A and B}
  inline(
    $5B/                     {pop bx       ;CX:BX = B}
    $59/                     {pop cx}
    $58/                     {pop ax       ;DX:AX = A}
    $5A/                     {pop dx}
    $39/$CA/                 {cmp dx,cx    ;compare high byte}
    $7C/$0A/                 {jl  done1    ;A < B?}
    $7F/$04/                 {jg  greater  ;A > B?}
    $39/$D8/                 {cmp ax,bx    ;compare low byte}
    $76/$04/                 {jbe done1    ;A <= B?}
                             {greater:}
    $89/$CA/                 {mov dx,cx    ;A is greater}
    $89/$D8);                {mov ax,bx}
                             {done1:}

function MaxLong(A, B : LongInt) : LongInt;
  {-Returns the greater of A and B}
  inline(
    $5B/                     {pop bx       ;CX:BX = B}
    $59/                     {pop cx}
    $58/                     {pop ax       ;DX:AX = A}
    $5A/                     {pop dx}
    $39/$CA/                 {cmp dx,cx    ;compare high byte}
    $7F/$0A/                 {jg  done2    ;A > B?}
    $7C/$04/                 {jl  less     ;A < B?}
    $39/$D8/                 {cmp ax,bx    ;compare low byte}
    $73/$04/                 {jae done2    ;A >= B?}
                             {less:}
    $89/$CA/                 {mov dx,cx    ;B is greater}
    $89/$D8);                {mov ax,bx}
                             {done2:}

procedure FillWord(var Dest; Count, Filler : Word);
  {-Fill memory starting at Dest with Count instances of Filler}
  inline(
    $58/                     {pop ax    ;AX = Filler}
    $59/                     {pop cx    ;CX = Count}
    $5F/                     {pop di    ;ES:DI => Dest}
    $07/                     {pop es}
    $FC/                     {cld       ;go forward}
    $F2/$AB);                {rep stosw ;fill memory}

procedure FillStruct(var Dest; Count : Word; var Filler; FillerSize : Word);
  {-Fill memory starting at Dest with Count instances of Filler}
  inline(
    $58/                     {pop ax     ;AX = FillerSize}
    $5B/                     {pop bx     ;DX:BX => Filler}
    $5A/                     {pop dx}
    $59/                     {pop cx     ;CX = Count}
    $5F/                     {pop di     ;ES:DI => Dest}
    $07/                     {pop es}
    $E3/$11/                 {jcxz done  ;done if Count = 0}
    $FC/                     {cld        ;go forward}
    $1E/                     {push ds    ;save DS}
    $8E/$DA/                 {mov ds,dx  ;DS:BX => Filler}
                             {again:}
    $89/$CA/                 {mov dx,cx  ;save loop count}
    $89/$DE/                 {mov si,bx  ;DS:SI => Filler}
    $89/$C1/                 {mov cx,ax  ;CX = FillerSize}
    $F2/$A4/                 {rep movsb  ;fill}
    $89/$D1/                 {mov cx,dx  ;restore loop count}
    $E2/$F4/                 {loop again ;repeat}
    $1F);                    {pop ds     ;restore DS}
                             {done:}

function AddWordToPtr(P : Pointer; W : Word) : Pointer;
  {-Add a Word to a pointer. No normalization or wrap checking performed}
  inline(
    $5B/                     {pop bx     ;bx = W}
    $58/                     {pop ax     ;ax = Ofs(P^)}
    $5A/                     {pop dx     ;dx = Seg(P^)}
    $01/$D8);                {add ax,bx  ;ax = Ofs(P^)+W}

function PtrToLong(P : Pointer) : LongInt;
  {-Convert pointer, in range $0:$0 to $FFFF:$000F, to LongInt}

function LongToPtr(L : LongInt) : Pointer;
  {-Return LongInt L as a normalized pointer}

function PtrDiff(P1, P2 : Pointer) : LongInt;
  {-Return the number of bytes between P1^ and P2^}

function AddLongToPtr(P : Pointer; L : LongInt) : Pointer;
  {-Add a LongInt to a pointer, returning a normalized pointer}

  {==========================================================================}

implementation

function PtrToLong(P : Pointer) : LongInt;
  {-Convert pointer, in range $0:$0 to $FFFF:$000F, to LongInt}
begin
  PtrToLong := (LongInt(OS(P).S) shl 4)+OS(P).O;
end;

function LongToPtr(L : LongInt) : Pointer;
  {-Return LongInt L as a normalized pointer}
begin
  LongToPtr := Ptr(Word(L shr 4), Word(L and $F));
end;

function PtrDiff(P1, P2 : Pointer) : LongInt;
  {-Return the number of bytes between P1^ and P2^}
begin
  PtrDiff := Abs(PtrToLong(P1)-PtrToLong(P2));
end;

function AddLongToPtr(P : Pointer; L : LongInt) : Pointer;
  {-Add a LongInt to a pointer, returning a normalized pointer}
begin
  AddLongToPtr := LongToPtr(L+PtrToLong(P));
end;

end.