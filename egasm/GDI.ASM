; Разводка процедур для видеодрайверов        DK
DATA    SEGMENT BYTE PUBLIC

        EXTRN   DriverMethods : BYTE

DATA    ENDS

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE, DS:DATA

       PUBLIC SetPixel
       PUBLIC SetPixelOp
       PUBLIC GetPixel
       PUBLIC GetPixelBM
       PUBLIC HLine
       PUBLIC VLine
       PUBLIC DisplayXxY
       PUBLIC DisplayXxYClip
       PUBLIC ReadScanLine
       PUBLIC WriteScanLine
       PUBLIC WriteScanLineOp
       PUBLIC PutBMPPart
       PUBLIC PutBMPPartOp
       PUBLIC HLineStyleOp
       PUBLIC DisplayXx8Op
       PUBLIC QuickSave
       PUBLIC QuickRestore
       PUBLIC DirectMousePut
       PUBLIC VLineStyleT
       PUBLIC DirectGetImage
       PUBLIC DirectPutImage
       PUBLIC PutBufferPart
       PUBLIC MapBMPLineRead
       PUBLIC InitDriver
       PUBLIC DoneDriver
       PUBLIC StretchDIBitmap
       PUBLIC ImplantDIBitmap
       PUBLIC SetOutput
       PUBLIC PrepareDrawing
       PUBLIC SetUserBitBltProc
       PUBLIC SetColorBitBlt
       PUBLIC EMSAdjSelect
       PUBLIC DIBAdjSelect

SetPixel            : jmp dword ptr [DriverMethods + 00h]
SetPixelOp          : jmp dword ptr [DriverMethods + 04h]
GetPixel            : jmp dword ptr [DriverMethods + 08h]
GetPixelBM          : jmp dword ptr [DriverMethods + 0ch]
HLine               : jmp dword ptr [DriverMethods + 10h]
VLine               : jmp dword ptr [DriverMethods + 14h]
DisplayXxY          : jmp dword ptr [DriverMethods + 18h]
DisplayXxYClip      : jmp dword ptr [DriverMethods + 1ch]
ReadScanLine        : jmp dword ptr [DriverMethods + 20h]
WriteScanLine       : jmp dword ptr [DriverMethods + 24h]
WriteScanLineOp     : jmp dword ptr [DriverMethods + 28h]
PutBMPPart          : jmp dword ptr [DriverMethods + 2ch]
PutBMPPartOp        : jmp dword ptr [DriverMethods + 30h]
HLineStyleOp        : jmp dword ptr [DriverMethods + 34h]
DisplayXx8Op        : jmp dword ptr [DriverMethods + 38h]
QuickSave           : jmp dword ptr [DriverMethods + 3ch]
QuickRestore        : jmp dword ptr [DriverMethods + 40h]
DirectMousePut      : jmp dword ptr [DriverMethods + 44h]
VLineStyleT         : jmp dword ptr [DriverMethods + 48h]
DirectGetImage      : jmp dword ptr [DriverMethods + 4ch]
DirectPutImage      : jmp dword ptr [DriverMethods + 50h]
PutBufferPart       : jmp dword ptr [DriverMethods + 54h]
MapBMPLineRead      : jmp dword ptr [DriverMethods + 58h]
InitDriver          : jmp dword ptr [DriverMethods + 5ch]
DoneDriver          : jmp dword ptr [DriverMethods + 60h]
StretchDIBitmap     : jmp dword ptr [DriverMethods + 64h]
ImplantDIBitmap     : jmp dword ptr [DriverMethods + 68h]
SetOutput           : jmp dword ptr [DriverMethods + 6ch]
PrepareDrawing      : jmp dword ptr [DriverMethods + 70h]
SetUserBitBltProc   : jmp dword ptr [DriverMethods + 74h]
SetColorBitBlt      : jmp dword ptr [DriverMethods + 78h]
EMSAdjSelect        : jmp dword ptr [DriverMethods + 7ch]
DIBAdjSelect        : jmp dword ptr [DriverMethods + 80h]

CODE    ENDS

        END


