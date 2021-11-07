DATA    SEGMENT BYTE PUBLIC
;OneMS   DW      ? 
Extrn   OneMS:Word
Extrn   BiosDataSele:Word
DATA    ENDS

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

;****************************************************** Sound

;procedure Sound(Hz: Word);
;Turn on the sound at the designated frequency

Hertz   EQU     WORD PTR SS:[BX+4]

Sound   PROC FAR
public  Sound

;        StackFrame
        mov     bx, sp
        MOV     BX,Hertz                ;BX = Hz
        MOV     AX,34DCh
        MOV     DX,0012h                ;DX:AX = $1234DC = 1,193,180
        CMP     DX,BX                   ;Make sure the division won't
        JAE     SoundExit               ; produce a divide by zero error
        DIV     BX                      ;Count (AX) = $1234DC div Hz
        MOV     BX,AX                   ;Save Count in BX

        IN      AL,61h                  ;Check the value in port $61
        TEST    AL,00000011b            ;Bits 0 and 1 set if speaker is on
        JNZ     SetCount                ;If they're already on, continue

        ;turn on speaker
        OR      AL,00000011b            ;Set bits 0 and 1
        OUT     61h,AL                  ;Change the value
        MOV     AL,182                  ;Tell the timer that the count is coming
        OUT     43h,AL                  ;by sending 182 to port $43

SetCount:
        MOV     AL,BL                   ;Low byte into AL
        OUT     42h,AL                  ;Load low order byte into port $42
        MOV     AL,BH                   ;High byte into AL
        OUT     42h,AL                  ;Load high order byte into port $42

SoundExit:
        RET     2

Sound   ENDP

;****************************************************** NoSound

;procedure NoSound;
;Turn off the sound

NoSound PROC FAR
Public  NoSound

        IN      AL,61h                  ;Get current value of port $61
        AND     AL,11111100b            ;Turn off bits 0 and 1
        OUT     61h,AL                  ;Reset the port
        RET

NoSound ENDP

;****************************************************** Delay

;procedure Delay(MS: Word);
;Delay for MS milliseconds

MSecs   EQU     WORD PTR SS:[BX+4]

Delay   PROC FAR
Public  Delay

;        StackFrame
        mov     bx, sp
        MOV     DX,MSecs                ;DX = MS
        OR      DX,DX                   ;Do nothing if MS = 0
        JZ      DelayExit

        MOV     AX,BiosDataSele         ;!!.20
        MOV     ES,AX                   ;!!.20
;        SetZero DI                      ;!!.20
        xor     di, di
        MOV     AL,ES:[DI]              ;!!.20

;!!.20  SetZero DI                      ;ES:DI points to dummy address
;!!.20  MOV     ES,DI                   ; which won't change
;!!.20  MOV     AL,ES:[DI]              ;AL has the value there

DelayLoop:
        MOV     CX,OneMS                ;loop count into CX
        CALL    DelayMS                 ;delay for one MS
        DEC     DX                      ;decrement counter
        JNZ     DelayLoop               ;repeat if not 0

DelayExit:
        RET     2

Delay   ENDP

;************************************************* DelayCalibrate

;procedure DelayCalibrate;
;Perform delay calibration

DelayCalibrate  PROC FAR
Public  DelayCalibrate

        ;set up delay count
        MOV     AX,BiosDataSele         ;ES = $40                 ;!!.20
        MOV     ES,AX
        MOV     DI,6Ch                  ;ES:DI => low word of BIOS timer count
        MOV     AL,ES:[DI]              ;AL has first byte there
        STI
WaitForChange:
        CMP     AL,ES:[DI]              ;wait for the byte to change
        JZ      WaitForChange

        XOR     DX,DX                   ;init high word of loop counter  !!.20
        MOV     AL,ES:[DI]              ;see how many times DelayMS has to
        MOV     CX,0FFFFh               ;loop before the timer count changes
Again:  CALL    DelayMS
        JNZ     Done                    ;finished if tick change         !!.20
        INC     DX                      ;no change, loop again           !!.20
        JMP     Again                   ;and keep checking               !!.20

Done:   MOV     AX,55                   ;now calculate OneMS
        XCHG    CX,AX
        NOT     AX
;       XOR     DX,DX                                                    !!.20
        DIV     CX
        MOV     OneMS,AX                ;AX has OneMS
        RET

DelayCalibrate  ENDP

;************************************************* DelayMS

;Delay one millisecond

DelayMS PROC NEAR

        MOV     BX,4
DMSdec: DEC     BX
        JNZ     DMSdec
        CMP     AL,ES:[DI]
        LOOPE   DelayMS
        RET

DelayMS ENDP

CODE    ENDS
END
