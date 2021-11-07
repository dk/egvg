;  Flicks preprocessing by TripleVee

        .model tpascal
        .code
;macs.asm begin
; *******************************************************
; *							*
; *	VERSION CONTROL EQUATES				*
; *							*
; *******************************************************

; Boolean values

FALSE		EQU	0
TRUE		EQU	NOT FALSE

; Version equates

    IFDEF _Windows_
WindowsVersion	EQU	TRUE
DPMIVersion	EQU	FALSE
    ELSE
    IFDEF _DPMI_
WindowsVersion	EQU	FALSE
DPMIVersion	EQU	TRUE
    ELSE
WindowsVersion	EQU	FALSE
DPMIVersion	EQU	FALSE
    ENDIF
    ENDIF

; *******************************************************
; *							*
; *	MACROS						*
; *							*
; *******************************************************

	LOCALS	@@

; Public variable definition macro

VAR MACRO Symbol,SType,Count
  PUBLIC Symbol
  Symbol LABEL SType
  IF Count
    DB SType * Count DUP(?)
  ENDIF
ENDM

; Parameter definition macro

PAR MACRO Symbol,SType,Count
  LOCAL Offset
  @AP = @AP + SType * Count
  Offset = @AP
  Symbol EQU (SType PTR [BP+@AF-Offset])
ENDM

@AP = 0
@AF = 0

; Local variables definition macro

LOC MACRO Symbol,SType,Count
  LOCAL Offset
  @LP = @LP + SType * Count
  Offset = @LP
  Symbol EQU (SType PTR [BP+@LF-Offset])
ENDM

@LP = 0
@LF = 0

; Stack frame modifiers

sfFar		EQU	01H		;FAR frame
sfMarkBP	EQU	02H		;Make saved BP odd
sfSaveDS	EQU	04H		;Save DS at [BP-2]
sfInitDS	EQU	08H		;Init DS using SS

; Default stack frame type

sfDefault = 0

; Stack frame types

    IF WindowsVersion
WINFAR		EQU	sfFar+sfMarkBP+sfSaveDS
    ELSE
WINFAR		EQU	sfFar
    ENDIF

; Entry code generation macro

ENTRY MACRO FrameType
  IFB <FrameType>
    @SF = sfDefault
  ELSE
    IFIDNI <FrameType>,<NEAR>
      @SF = 0
    ELSE
      IFIDNI <FrameType>,<FAR>
        @SF = sfFar
      ELSE
        @SF = FrameType
      ENDIF
    ENDIF
  ENDIF
  IF @SF AND sfMarkBP
    INC BP
  ENDIF
  PUSH BP
  MOV BP,SP
  IF @SF AND sfFar
    @AF = @AP + 6
  ELSE
    @AF = @AP + 4
  ENDIF
  IF @SF AND sfSaveDS
    PUSH DS
    @LF = -2
  ELSE
    @LF = 0
  ENDIF
  IF @LP
    SUB SP,@LP
  ENDIF
  IF @SF AND sfInitDS
    PUSH DS
    PUSH SS
    POP DS
  ENDIF
ENDM

; Exit code generation macro

EXIT MACRO ArgSize
  IF @SF AND sfInitDS
    POP DS
  ENDIF
  IF @LF - @LP
    MOV SP,BP
  ENDIF
  POP BP
  IF @SF AND sfMarkBP
    DEC BP
  ENDIF
  IFNB <ArgSize>
    @AP = ArgSize
  ENDIF
  IF @SF AND sfFar
    RETF @AP
  ELSE
    RETN @AP
  ENDIF
  @AP = 0
  @LP = 0
ENDM
;macs.asm end

        public  RLE1
        public  RLE2
        public  RLE3
        public  PALBuffered
        public  PALPort
        public  HiPALPort
        public  SetPAL
        public  SetHiPAL
        public  SetColor0

; Procedure to decompress RLE1 block in chunk C

RLE1:
        PAR     RLE1Src,    dword, 1
        PAR     RLE1Dst,    dword, 1
        PAR     RLE1LineLen, word,  1

        LOC     RLE1LineCount, word, 1

        ENTRY   FAR
        push    ds
        cld
        lds     si, RLE1Src
        les     di, RLE1Dst
        lodsw                   ; start line
        mov     dx, RLE1LineLen
        mul     dx
        add     di, ax
        lodsw                   ; line count
        mov     RLE1LineCount, ax
        mov     dx, di
        xor     ah, ah
@@LineLoop:
        mov     di, dx
        lodsb                   ; piece count
        mov     bl,al
        test    bl, bl
        jz      @@NextLine
@@PieceLoop:
        lodsb                   ; piece offset
        add     di, ax
        lodsb                   ; piece size
        test    al, al
        js      @@Fill
        mov     cx, ax          ; move
        rep     movsb
        dec     bl
        jnz     @@PieceLoop
        jmp     short @@NextLine
@@Fill:
        neg     al              ; fill
        mov     cx, ax
        lodsb
        rep stosb
        dec     bl
        jnz     @@PieceLoop
@@NextLine:
        add     dx, RLE1LineLen
        dec     RLE1LineCount
        jnz     @@LineLoop
        pop     ds
        EXIT

; Procedure to decompress RLE2 Block in chunk F

RLE2:
        PAR     RLE2Src,       dword, 1
        PAR     RLE2Dst,       dword, 1
        PAR     RLE2LineLen,   word,  1
        PAR     RLE2LineCount, word, 1

        ENTRY   FAR
        push    ds
        cld
        lds     si, RLE2Src
        les     di, RLE2Dst
        mov     dx, di
        xor     ah, ah
@@LineLoop:
        mov     di, dx
        lodsb                   ; piece count
        mov     bl, al
        test    bl,bl
        jz      @@NextLine
@@PieceLoop:
        lodsb                   ; counter
        test    al, al
        js      @@Move
        mov     cx, ax          ; fill
        lodsb
        rep stosb
        dec     bl
        jnz     @@PieceLoop
        jmp     short @@NextLine
@@Move:
        neg     al              ; move
        mov     cx, ax
        rep movsb
        dec     bl
        jnz     @@PieceLoop
@@NextLine:
        add     dx, RLE2LineLen
        dec     RLE2LineCount
        jnz     @@LineLoop
        pop     ds
        EXIT

; Procedure to decompress RLE3 block in chunk 7

RLE3:
        PAR     RLE3Src,    dword, 1
        PAR     RLE3Dst,    dword, 1
        PAR     RLE3LineLen, word, 1

        LOC     RLE3LineCount, word, 1

        ENTRY   FAR
        push    ds
        cld
        lds     si, RLE3Src
        les     di, RLE3Dst
        lodsw                   ; line count
        mov     RLE3LineCount, ax
@@LineLoop:
        lodsw
        test    ax,ax
        jns     @@PieceCount
        neg     ax              ; y offset
        mul     RLE3LineLen
        add     word ptr RLE3Dst, ax
        jmp     @@LineLoop
@@PieceCount:
        mov     bx,ax           ; piece count
        mov     di, word ptr RLE3Dst
@@PieceLoop:
        lodsb                   ; piece offset
        xor     ah,ah
        add     di,ax
        lodsb
        test    al,al
        js      @@Fill
        mov     cx,ax           ; move
        rep movsw
        dec     bx
        jnz     @@PieceLoop
        jmp     short @@NextLine
@@Fill:
        neg     al              ; fill
        mov     cx,ax
        lodsw
        rep stosw
        dec     bx
        jnz     @@PieceLoop
@@NextLine:
        mov     ax, RLE3LineLen
        add     word ptr RLE3Dst, ax
        dec     RLE3LineCount
        jnz     @@LineLoop
        pop     ds
        EXIT

; Procedure to decompress palette to buffer

PalBuffered:
        PAR     PalSrc, dword, 1
        PAR     PalDst, dword, 1

        ENTRY   FAR
        push    ds
        cld
        lds     si, PalSrc
        les     di, PalDst
        lodsw                   ; piece count
        mov     bx, ax
@@PieceLoop:
        lodsb                   ; piece offset
        add     di, ax
        add     di, ax
        add     di, ax
        lodsb                   ; piece count
        or      al, al
        jnz     @@Start
        mov     ax, 100h        ; zero is 256
@@Start:
        mov     cx, ax
        add     cx, ax
        add     cx, ax
        rep movsb
        dec     bx
        jnz     @@PieceLoop
        pop     ds
        EXIT


; Procedure to decompress palette to registers

PalPort:
        PAR     _PalSrc, dword, 1

        ENTRY   FAR
        push    ds
        cld
        lds     si, _PalSrc
        xor     di, di
        lodsw                   ; piece count
        mov     bx, ax
@@PieceLoop:
        lodsb
        add     di, ax
        lodsb
        mov     cx, ax
        or      cx, cx
        jnz     @@Start
        mov     cx, 100h        ; zero is 256
@@Start:
        mov     dx, 3C8h
        mov     ax, di
        out     dx, al
        inc     di
        inc     dx              ; PEL Data
        lodsb
        out     dx, al
        lodsb
        out     dx, al
        lodsb
        out     dx, al
        loop    @@Start
        dec     bx
        jnz     @@PieceLoop
        pop     ds
        EXIT

; Procedure to decompress high palette to registers

HiPalPort:
        PAR     _HiPalSrc, dword, 1

        ENTRY   FAR
        push    ds
        cld
        lds     si, _HiPalSrc
        xor     di, di
        lodsw                   ; piece count
        mov     bx, ax
@@PieceLoop:
        lodsb
        add     di, ax
        lodsb
        mov     cx, ax
        or      cx, cx
        jnz     @@Start
        mov     cx, 100h        ; zero is 256
@@Start:
        mov     dx, 3C8h
        mov     ax, di
        out     dx, al
        inc     di
        inc     dx              ; PEL Data
        lodsb
        shr     al,1
        shr     al,1
        out     dx, al
        lodsb
        shr     al,1
        shr     al,1
        out     dx, al
        lodsb
        shr     al,1
        shr     al,1
        out     dx, al
        loop    @@Start
        dec     bx
        jnz     @@PieceLoop
        pop     ds
        EXIT

; procedure to set palette from buffer

SetPal:
        PAR     PalBuffer, dword, 1
        PAR     StartColor, word, 1
        PAR     CountColor, word, 1

        ENTRY   FAR
        push    ds
        cld
        mov     bx, StartColor
        mov     cx, CountColor
        lds     si, PalBuffer
@@CLoop:
        mov     dx, 3C8h
        mov     al, bl
        out     dx, al
        inc     bl
        inc     dx
        jmp     short @@1
@@1:
        lodsb
        out     dx, al
        jmp     short @@2
@@2:
        lodsb
        out     dx, al
        jmp     short @@3
@@3:
        lodsb
        out     dx, al
        loop    @@CLoop
        pop     ds
        EXIT

; procedure to set high palette from buffer

SetHiPal:
        PAR     HiPalBuffer, dword, 1
        PAR     StartColor, word, 1
        PAR     CountColor, word, 1

        ENTRY   FAR
        push    ds
        cld
        mov     bx, StartColor
        mov     cx, CountColor
        lds     si, PalBuffer
@@CLoop:
        mov     dx, 3C8h
        mov     al, bl
        out     dx, al
        inc     bl
        inc     dx
        lodsb
        shr     al,1
        shr     al,1
        out     dx, al
        lodsb
        shr     al,1
        shr     al,1
        out     dx, al
        lodsb
        shr     al,1
        shr     al,1
        out     dx, al
        loop    @@CLoop
        pop     ds
        EXIT

; is it required ?
SetColor0:
        PAR     Color0, word, 1

        ENTRY   FAR
        mov     dx, 3C8h
        xor     al, al
        out     dx, al
        inc     dx
        mov     al, byte ptr Color0
        jmp     short @@1
@@1:
        out     dx, al
        jmp     short @@2
@@2:
        out     dx, al
        jmp     short @@3
@@3:
        out     dx, al
        EXIT

        end
