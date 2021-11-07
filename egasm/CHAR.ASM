                TITLE   CHAR
;
;  Routines for DRIVERS.PAS by Oleg Olenick. Version 2.00.
;
;  1. Application controlled Russian/Ukrainian keyboard driver.
;  2. Upcase for Russian/Ukrainian characters.
;
                LOCALS  @@

;               BIOS Data Refs

KeyBufTail      EQU     1CH
KeyFlags        EQU     17H
LeftShift       EQU     02H
RightShift      EQU     01H
EitherShift     EQU     LeftShift + RightShift
CapsLock        EQU     40h

;               Drivers Data Refs

AltCodes0Begin  EQU     02H
AltCodes0End    EQU     0BH
AltCodes1Begin  EQU     10H
AltCodes1End    EQU     35H

DATA    SEGMENT WORD PUBLIC 'DATA'

        EXTRN   Seg0040:WORD
        EXTRN   AltKbdMode:BYTE
        EXTRN   AltKbdSwitch:WORD
        EXTRN   AltCodes0:BYTE
        EXTRN   AltCodes1:BYTE

OldInt09        DD      0
OldKeyBufTail   DW      0

DATA    ENDS

DGROUP  GROUP   DATA

CODE    SEGMENT BYTE PUBLIC 'CODE'

        ASSUME  CS:CODE,DS:DGROUP

; Publics

        PUBLIC  InitKbdDriver
        PUBLIC  DoneKbdDriver
        PUBLIC  UpCase

InitKbdDriver:
        MOV     AX, WORD PTR [OldInt09]
        OR      AX, WORD PTR [OldInt09+2]
        JNZ     @@1
ifdef   _DPMI_
        MOV     BL, 09H
        MOV     AX,0204H
        INT     31H
        MOV     WORD PTR [OldInt09+0], DX
        MOV     WORD PTR [OldInt09+2], CX
        MOV     BL, 09H
        MOV     DX, OFFSET Int09Handler
        MOV     CX, CS
        MOV     AX, 0205H
        INT     31H
else
        CLI
;        XOR     AX, AX
        MOV     ES, AX
        MOV     AX, ES:[09H*4+0]
        MOV     WORD PTR [OldInt09+0], AX
        MOV     AX, ES:[09H*4+2]
        MOV     WORD PTR [OldInt09+2], AX
        MOV     WORD PTR ES:[09H*4+0], OFFSET Int09Handler
        MOV     WORD PTR ES:[09H*4+2], CS
        STI
endif
        MOV     ES, [Seg0040]
        MOV     DI, ES:[KeyBufTail]
        MOV     [OldKeyBufTail], DI
@@1:
        RETF

DoneKbdDriver:
        MOV     AX, WORD PTR [OldInt09]
        OR      AX, WORD PTR [OldInt09+2]
        JZ      @@2
ifdef   _DPMI_
        MOV     BL, 09H
        MOV     DX, WORD PTR [OldInt09+0]
        MOV     CX, WORD PTR [OldInt09+2]
        MOV     AX, 0205H
        INT     31H
else
        CLI
        XOR     AX, AX
        MOV     ES, AX
        MOV     AX, WORD PTR [OldInt09+0]
        MOV     ES:[09H*4+0], AX
        MOV     AX, WORD PTR [OldInt09+2]
        MOV     ES:[09H*4+2], AX
        STI
endif
        XOR     AX, AX
        MOV     WORD PTR [OldInt09+0], AX
        MOV     WORD PTR [OldInt09+2], AX
@@2:
        RETF

Int09Handler:
        PUSH    DS
        PUSH    ES
        PUSH    DI
        PUSH    BX
        PUSH    AX
        MOV     AX, SEG DGROUP
        MOV     DS, AX
        PUSHF
        CALL    [OldInt09]
        MOV     ES, [Seg0040]
        MOV     DI, [OldKeyBufTail]
        CMP     DI, ES:[KeyBufTail]
        JE      @@4

        MOV     AX, ES:[DI] ; Last key (Hi = Scan, Lo = ASCII)
        CMP     AX, [AltKbdSwitch]
        JNE     @@3

        XOR     [AltKbdMode], 1
        MOV     DI, [OldKeyBufTail]
        MOV     ES:[KeyBufTail], DI
        JMP     SHORT @@4

@@3:
        MOV     BX, ES:[KeyBufTail]
        MOV     [OldKeyBufTail], BX
        CMP     [AltKbdMode], 1
        JNE     @@4
        CMP     AL, ' '
        JBE     @@4        ; Check Space and Alt and Ctrl key Space
        CALL    Convert
        MOV     ES:[DI], AX
@@4:
        POP     AX
        POP     BX
        POP     DI
        POP     ES
        POP     DS
        IRET

Convert:
        CMP     AH, AltCodes0Begin
        JB      @@7
        CMP     AH, AltCodes0End
        JA      @@5
        TEST    BYTE PTR ES:[KeyFlags], EitherShift
        JZ      @@7
        MOV     BX, OFFSET AltCodes0
        SUB     BX, AltCodes0Begin
        JMP     SHORT @@6
@@5:
        CMP     AH, AltCodes1Begin
        JB      @@7
        CMP     AH, AltCodes1End
        JA      @@7
        MOV     BX, OFFSET AltCodes1
        SUB     BX, AltCodes1Begin
@@6:
        PUSH    AX
        XOR     AL, AL
        XCHG    AL, AH
        ADD     BX, AX
        CMP     AH, [BX]
        POP     AX
        JZ      @@7
        MOV     AL, [BX]
        CMP     AH, AltCodes1Begin
        ;
        MOV     AH, 0   ; Scancode = 0 (for russian '‡'&'')
        ;
        JB      @@7
        MOV     BL, BYTE PTR ES:[KeyFlags]
        MOV     BH, BL
        SHR     BL, 1
        OR      BL, BH
        SHL     BL, 6
        XOR     BL, BH
        AND     BL, 40H
        JZ      @@7
        PUSH    AX
        PUSH    CS
        CALL    UpCase
@@7:
        RETN

UpCase:
	MOV	BX,SP
	MOV	AL,SS:[BX+4]
	CMP	AL,'a'
	JB	@@End
	CMP	AL,'z'
	JA	@@1
	SUB	AL,'a'-'A'
	JMP 	SHORT @@End
@@1:	CMP	AL,'†'
	JB	@@End
	CMP	AL,'Ø'
	JA	@@2
	SUB	AL,'†'-'Ä'
	JMP 	SHORT @@End
@@2:	CMP	AL,'‡'
	JB	@@End
	CMP	AL,'Ô'
	JA	@@3
	SUB	AL,'‡'-'ê'
	JMP 	SHORT @@End
@@3:	CMP	AL,'˘'
	JA	@@End
	TEST	AL,1h
	JZ	@@End
	DEC	AL
@@End:
	RETF	2

CODE	ENDS

	END
