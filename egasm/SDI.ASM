; Разводка процедур для аудиодрайверов        DK
DATA    SEGMENT BYTE PUBLIC

        EXTRN   DriverMethods : BYTE

DATA    ENDS

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

       PUBLIC Init
       PUBLIC Done
       PUBLIC InitWave
       PUBLIC SetRate
       PUBLIC StartSound
       PUBLIC StopSound
       PUBLIC PauseSound
       PUBLIC ContinueSound

Init               : jmp dword ptr [DriverMethods + 00h]
Done               : jmp dword ptr [DriverMethods + 04h]
InitWave           : jmp dword ptr [DriverMethods + 08h]
SetRate            : jmp dword ptr [DriverMethods + 0ch]
StartSound         : jmp dword ptr [DriverMethods + 10h]
StopSound          : jmp dword ptr [DriverMethods + 14h]
PauseSound         : jmp dword ptr [DriverMethods + 18h]
ContinueSound      : jmp dword ptr [DriverMethods + 1ch]


CODE    ENDS

        END


