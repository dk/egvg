            Ideal
            Jumps
            Model      TPASCAL
            P186

            PUBLIC     PixelAddrBuf
            PUBLIC     HLine
            PUBLIC     HLineStyle
            PUBLIC     Bar
            PUBLIC     VLine
            PUBLIC     DisplayXxY
            PUBLIC     DisplayXxYClip
            PUBLIC     PutBufferPart
            PUBLIC     DirectPutImage
            PUBLIC     DirectGetImage
            PUBLIC     DirectHLine
            PUBLIC     HLineStyleT
            PUBLIC     VLineStyleT
            PUBLIC     ReMap16
            PUBLIC     MakeBWBuffer
            PUBLIC     Make16Buffer
            PUBLIC     PutBMPPart
            PUBLIC     PutBMPPartOp
            PUBLIC     GetBitMapBufferSize
            PUBLIC     DirectDisplayXxY
            PUBLIC     DirectPutPartOfImage
            PUBLIC     DirectMousePut
            Public     QuickSave
            Public     QuickRestore
            Public     DirectLine
            Public     ScrollY
            Public     SetPixel

;
            EXTRN      LineLength      : WORD
            EXTRN      BitPlaneSizeW   : WORD
            EXTRN      BitPlanes       : DWORD
            EXTRN      SegA000         : WORD
            EXTRN      RMWbits         : BYTE


            CODESEG

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HLine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        PROC       HLine     FAR   X1:WORD, Y:WORD, X2:WORD, __Dummy:WORD, Clr:BYTE
        LOCAL      BufOffs:WORD, ClrMask:BYTE
        LOCALS
        cld
        mov     ax, [Y]
        mov     bx, [X1]

        mov     cl, bl          ; CL := low-order byte of x
        mul     [LineLength]    ; AX := y * BytesPerLine
        shr     bx,3            ; BX := x/8
        add     bx,ax           ; BX := y*BytesPerLine + x/8
        and     cl,7            ; CL := x & 7
        xor     cl,7            ; CL := number of bits to shift left
        mov     ah,1            ; AH := unshifted bit mask
        mov     [BufOffs], bx   ; Save buffer offset for future use

        mov     dh, ah
        not     dh              ; DH := unshifted bit mask for leftmost
                                ;       byte
        mov     dl, 0FFh        ; DL := unshifted bit mask for
                                ;        rightmost byte

        shl     dh,cl           ; DH := reverse bit mask for first byte
        not     dh              ; DH := bit mask for first byte

        mov     cx, [X2]
        and     cl, 7
        xor     cl, 7           ; CL := number of bits to shift left
        shl     dl, cl          ; DL := bit mask for last byte

; determine byte offset of first and last pixel in the line

        mov     ax, [X2]        ; AX := x2
        mov     bx, [X1]        ; BX := x1

        shr     ax, 3           ; AX := byte offset of x2
        shr     bx, 3           ; BX := byte offset of x1
        mov     cx, ax
        sub     cx, bx          ; CX := (# bytes in line) - 1

; Цикл по битовым плоскостям

        xor     bx, bx          ; BX <- смещение на адрес
                                ; текущей битовой плоскости
        mov     [ClrMask], 8    ; Маска плоскости (используется
                                ; в качестве счетчика

@@PlanesLoop:
        les     di, [BitPlanes+bx]
        add     di, [BufOffs]
        mov     al, [ClrMask]
        or      al, al
        jz      @@EndPlanesLoop

; Для текущей плоскости, в зависимости от цвета (по маске), делаем
; al == FF, если нужно устанавливать биты в плоскости, и 00 в противном случае
        xor     ah, ah
        test    [Clr], al
        mov     al, ah
        jz      @@1
        not     al
@@1:
        shr     [ClrMask], 1    ; Маскируем для next plane
        add     bx, 4           ; Смещение на адрес следующей плоскости

        push    dx
        push    cx

; set pixels in leftmost byte of the line

            or         dh,dh
            js         @@L43           ; jump if byte-aligned (x1 is leftmost
                                       ;  pixel in byte)
            or         cx,cx
            jnz        @@L42           ; jump if more than one byte in the line

            and        dl,dh           ; bit mask for the line
            jmp        SHORT @@L44

@@L42:      mov        ah,al
            and        ah,dh           ; AH := masked pixel bits
            not        dh              ; DH := reverse bit mask for 1st byte
            and        [es:di], dh    ; zero masked pixels in buffer
            or         [es:di], ah    ; update masked pixels in buffer
            inc        di
            dec        cx

; Лепим центральную часть строки
@@L43:
        shr     cx, 1
        pushf
        jz      @@OneByte
        mov     ah, al
        rep     stosw
@@OneByte:
        popf
        jnc     @@NoOneByte
        stosb
@@NoOneByte:

; set pixels in the rightmost byte of the line

@@L44:      and        al, dl          ; AL := masked pixels for last byte
            not        dl
            and        [es:di], dl     ; zero masked pixels in buffer
            or         [es:di], al     ; update masked pixels in buffer

            pop        cx
            pop        dx
            jmp        @@PlanesLoop
@@EndPlanesLoop:

            RET
            ENDP



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HLineStyle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC       HLineStyle  FAR   X1:WORD, Y:WORD, X2:WORD, Pattern:BYTE, __Dummy:WORD,Clr0:BYTE, __Dummy1:WORD,Clr1:BYTE
            LOCAL      BufOffs:WORD, ClrMask:BYTE
            LOCALS
            cld
            mov        ax, [Y]
            mov        bx, [X1]

            mov        cl, bl          ; CL := low-order byte of x
            mul        [LineLength]    ; AX := y * BytesPerLine
            shr        bx,3            ; BX := x/8
            add        bx,ax           ; BX := y*BytesPerLine + x/8
            and        cl,7            ; CL := x & 7
            xor        cl,7            ; CL := number of bits to shift left
            mov        ah,1            ; AH := unshifted bit mask
            mov        [BufOffs], bx   ; Save buffer offset for future use

            mov        dh, ah
            not        dh              ; DH := unshifted bit mask for leftmost
                                       ;        byte
            mov        dl, 0FFh        ; DL := unshifted bit mask for
                                       ;        rightmost byte

            shl        dh,cl           ; DH := reverse bit mask for first byte
            not        dh              ; DH := bit mask for first byte

            mov        cx, [X2]
            and        cl, 7
            xor        cl, 7           ; CL := number of bits to shift left
            shl        dl, cl          ; DL := bit mask for last byte

; determine byte offset of first and last pixel in the line

            mov        ax, [X2]        ; AX := x2
            mov        bx, [X1]        ; BX := x1

            shr        ax, 3           ; AX := byte offset of x2
            shr        bx, 3           ; BX := byte offset of x1
            mov        cx, ax
            sub        cx, bx          ; CX := (# bytes in line) - 1

; Цикл по битовым плоскостям

            xor        bx, bx
            mov        [ClrMask], 8

@@PlanesLoop:
            les        di, [BitPlanes+bx]
            add        di, [BufOffs]
            mov        al, [ClrMask]
            or         al, al
            jz         @@EndPlanesLoop
            test       [Clr0], al
            jnz        @@Is0
            test       [Clr1], al
            jnz        @@No0Is1
; Оба нули
            xor        al, al
            jmp        @@Setting
@@No0Is1:
            mov        al, [Pattern]
            jmp        @@Setting
@@Is0:      test       [Clr1], al
            jnz        @@Is0Is1
@@Is0No1:
            mov        al, [Pattern]
            not        al
            jmp        @@Setting
@@Is0Is1:   mov        al, 0FFh
@@Setting:
            shr        [ClrMask], 1
            add        bx, 4

            push       dx
            push       cx

; set pixels in leftmost byte of the line

            or         dh,dh
            js         @@L43           ; jump if byte-aligned (x1 is leftmost
                                       ;  pixel in byte)
            or         cx,cx
            jnz        @@L42           ; jump if more than one byte in the line

            and        dl,dh           ; bit mask for the line
            jmp        SHORT @@L44

@@L42:      mov        ah,al
            and        ah,dh           ; AH := masked pixel bits
            not        dh              ; DH := reverse bit mask for 1st byte
            and        [es:di], dh    ; zero masked pixels in buffer
            or         [es:di], ah    ; update masked pixels in buffer
            inc        di
            dec        cx

; Лепим центральную часть строки
@@L43:
        shr     cx, 1
        pushf
        jz      @@OneByte
        mov     ah, al
        rep     stosw
@@OneByte:
        popf
        jnc     @@NoOneByte
        stosb
@@NoOneByte:

; set pixels in the rightmost byte of the line

@@L44:      and        al, dl          ; AL := masked pixels for last byte
            not        dl
            and        [es:di], dl     ; zero masked pixels in buffer
            or         [es:di], al     ; update masked pixels in buffer

            pop        cx
            pop        dx
            jmp        @@PlanesLoop
@@EndPlanesLoop:

            RET
            ENDP



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        PROC    Bar     FAR   X1, Y1, X2, Y2, __Dummy:WORD, Clr:BYTE
        LOCAL   BufOffs, OriginalBufOffs, ClrMask:BYTE
        LOCALS
        cld
        mov     ax, [Y1]
        mov     bx, [X1]

        mov     cl, bl          ; CL := low-order byte of x
        mul     [LineLength]    ; AX := y * BytesPerLine
        shr     bx,3            ; BX := x/8
        add     bx,ax           ; BX := y*BytesPerLine + x/8
        and     cl,7            ; CL := x & 7
        xor     cl,7            ; CL := number of bits to shift left
        mov     ah,1            ; AH := unshifted bit mask
        mov     [OriginalBufOffs], bx   ; Save buffer offset for future use

        mov     dh, ah
        not     dh              ; DH := unshifted bit mask for leftmost
                                       ;        byte
        mov     dl, 0FFh        ; DL := unshifted bit mask for
                                       ;        rightmost byte

        shl     dh, cl           ; DH := reverse bit mask for first byte
        not     dh              ; DH := bit mask for first byte

        mov     cx, [X2]
        and     cl, 7
        xor     cl, 7           ; CL := number of bits to shift left
        shl     dl, cl          ; DL := bit mask for last byte

; determine byte offset of first and last pixel in the line

        mov     ax, [X2]        ; AX := x2
        mov     bx, [X1]        ; BX := x1

        shr     ax, 3           ; AX := byte offset of x2
        shr     bx, 3           ; BX := byte offset of x1
        mov     cx, ax
        sub     cx, bx          ; CX := (# bytes in line) - 1

        mov     ax, [Y1]        ; Считаем, сколько строк выводить
        sub     [Y2], ax
        inc     [Y2]            ; Счетчик выводимых строк

; Цикл по битовым плоскостям
        xor     bx, bx          ; BX <- смещение на адрес
                                ; текущей битовой плоскости
        mov     [ClrMask], 8    ; Маска плоскости (используется
                                ; в качестве счетчика

@@PlanesLoop:

        mov     ax, [Y2]
        mov     [Y1], ax        ; Счетчик выводимых строк
;
        mov     ax, [OriginalBufOffs]
        mov     [BufOffs], ax
;
        mov     al, [ClrMask]
        or      al, al
        jz      @@EndPlanesLoop
;
        xor     ah, ah
        test    [Clr], al
        mov     al, ah
        jz      @@1
        not     al              ; al <- FF or 00, в зависимости от того,
                                ; трем ли мы текущую плоскость или единичим ее
@@1:
        mov     es, [word BitPlanes+bx+2]

; Цикл по строкам для отрисовки прямоугольника
@@Loop:
        cmp     [Y1], 0
        jne     @@2
        shr     [ClrMask], 1    ; Маскируем для next plane
        add     bx, 4           ; Смещение на адрес следующей плоскости
        jmp     @@PlanesLoop
;
@@2:
        mov     di, [word BitPlanes+bx]
        add     di, [BufOffs]
;
        push    dx
        push    cx
; set pixels in leftmost byte of the line
        or      dh,dh
        js      @@L43           ; jump if byte-aligned (x1 is leftmost
                                ;  pixel in byte)
        or      cx,cx
        jnz     @@L42           ; jump if more than one byte in the line

        and     dl,dh           ; bit mask for the line
        jmp     SHORT @@L44

@@L42:  mov     ah,al
        and     ah,dh           ; AH := masked pixel bits
        not     dh              ; DH := reverse bit mask for 1st byte
        and     [es:di], dh    ; zero masked pixels in buffer
        or      [es:di], ah    ; update masked pixels in buffer
        inc     di
        dec     cx

; use a fast 8086 machine instruction to draw the remainder of the line

@@L43:
        shr     cx, 1
        pushf
        jz      @@OneByte
        mov     ah, al
        rep     stosw
@@OneByte:
        popf
        jnc     @@NoOneByte
        stosb
@@NoOneByte:

; set pixels in the rightmost byte of the line

@@L44:
        mov     ah, al
        and     ah, dl          ; AH := masked pixels for last byte
        not     dl
        and     [es:di], dl     ; zero masked pixels in buffer
        or      [es:di], ah     ; update masked pixels in buffer

        dec     [Y1]
        mov     cx, [LineLength]
        add     [BufOffs], cx
        pop     cx
        pop     dx
        jmp     @@Loop

@@EndPlanesLoop:
        RET
        ENDP



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VLine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC       VLine     FAR   X:WORD, Y1:WORD, Y2, __Dummy:WORD, Clr:WORD
            LOCAL      BufOffs:WORD
            LOCALS
            cld
            mov        ax, [Y1]
            mov        bx, [X]

            mov        cl, bl          ; CL := low-order byte of x
            mul        [LineLength]    ; AX := y * BytesPerLine
            shr        bx,3            ; BX := x/8
            add        bx, ax          ; BX := y*BytesPerLine + x/8
            and        cl,7            ; CL := x & 7
            xor        cl,7            ; CL := number of bits to shift left
            mov        [BufOffs], bx   ; Save buffer offset for future use

            mov        ah, 01h         ; AH := unshifted bit mask
            shl        ah, cl          ; AH := bit mask for pixel
            mov        al, ah
            not        al

            mov        cx, [Y2]
            sub        cx, [Y1]
            inc        cx              ; CX := (# lines)

; развернутый цикл по битовым плоскостям

            les        di, [BitPlanes+0]
            add        di, [BufOffs]

            push       cx

            test       [Clr], 08h
            jnz        @@Set08
@@Clear08:
            and        [es:di], al
            add        di, [LineLength]
            loop       @@Clear08
            jmp        @@04
@@Set08:
            or         [es:di], ah
            add        di, [LineLength]
            loop       @@Set08
@@04:
            pop        cx
            les        di, [BitPlanes+4]
            add        di, [BufOffs]

            push       cx

            test       [Clr], 04h
            jnz        @@Set04
@@Clear04:
            and        [es:di], al
            add        di, [LineLength]
            loop       @@Clear04
            jmp        @@02
@@Set04:
            or         [es:di], ah
            add        di, [LineLength]
            loop       @@Set04
@@02:
            pop        cx
            les        di, [BitPlanes+8]
            add        di, [BufOffs]

            push       cx

            test       [Clr], 02h
            jnz        @@Set02
@@Clear02:
            and        [es:di], al
            add        di, [LineLength]
            loop       @@Clear02
            jmp        @@01
@@Set02:
            or         [es:di], ah
            add        di, [LineLength]
            loop       @@Set02
@@01:
            pop        cx
            les        di, [BitPlanes+12]
            add        di, [BufOffs]

            push       cx

            test       [Clr], 01h
            jnz        @@Set01
@@Clear01:
            and        [es:di], al
            add        di, [LineLength]
            loop       @@Clear01
            jmp        @@00
@@Set01:
            or         [es:di], ah
            add        di, [LineLength]
            loop       @@Set01
@@00:
            pop        cx

            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DisplayXxY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Отобразить блок пикселов размером XxY, где X<=8, Y любой
; BitMap     - указывает на табличку по байту
; Wdth       - ширина блока в диапазоне [1..8],
; Hght       - высота блока (кол-во байт в таблице),
; Fore       - цвет символа

            PROC          DisplayXxY     FAR  X, Y, BitMap:DWORD, Wdth, Hght, __Dummy, Fore:WORD
            LOCAL   BufOffs : WORD, LineLengthLocal : WORD
            LOCALS

            cld
            dec        [Wdth]
            mov        ax, [LineLength]
            mov        [LineLengthLocal], ax

            ;*** смещение в буфере
            mov        ax, [Y]
            mov        bx, [X]

            mul        [LineLengthLocal]; AX := y * BytesPerLine
            mov        dl, bl          ; DL := low-order byte of x
            shr        bx,3            ; BX := x/8
            add        bx, ax          ; BX := y*BytesPerLine + x/8
            mov        [BufOffs], bx   ; Save buffer offset for future use
            and        dl,7            ; DL := x & 7
            jz         @@ByteAligned

@@NotByteAligned:
            mov        cl, [byte ptr Wdth]
            xor        cl, 7
            mov        bx, 0FFh
            shl        bl, cl
            mov        cl, dl
            xor        cl, 7
            inc        cl
            shl        bx, cl
            mov        dx, bx
            not        dx

            les        di, [BitPlanes+0]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 08h
            jnz        @@NBASet08Fore

@@NBAClr08Fore:
            lodsb
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBAClr08Fore
            jmp        @@NBA04Fore

@@NBASet08Fore:
            lodsb
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBASet08Fore

@@NBA04Fore:pop        ds
            pop        [Hght]
            les        di, [BitPlanes+4]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 04h
            jnz        @@NBASet04Fore

@@NBAClr04Fore:
            lodsb
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBAClr04Fore
            jmp        @@NBA02Fore

@@NBASet04Fore:
            lodsb
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBASet04Fore

@@NBA02Fore:pop        ds
            pop        [Hght]
            les        di, [BitPlanes+8]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 02h
            jnz        @@NBASet02Fore

@@NBAClr02Fore:
            lodsb
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBAClr02Fore
            jmp        @@NBA01Fore

@@NBASet02Fore:
            lodsb
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBASet02Fore

@@NBA01Fore:pop        ds
            pop        [Hght]
            les        di, [BitPlanes+12]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 01h
            jnz        @@NBASet01Fore

@@NBAClr01Fore:
            lodsb
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBAClr01Fore
            jmp        @@NBA00Fore

@@NBASet01Fore:
            lodsb
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@NBASet01Fore

@@NBA00Fore:pop        ds
            pop        [Hght]
            jmp        @@Exit

@@ByteAligned:
            mov        cl, [byte ptr Wdth]
            xor        cl, 7
            mov        bl, 0FFh
            shl        bl, cl
            mov        bh, bl
            not        bh
@@BA08:
            les        di, [BitPlanes+0]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 08h
            jnz        @@BASet08Fore

@@BAClr08Fore:
            lodsb
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BAClr08Fore
            jmp        @@BA04

@@BASet08Fore:
            lodsb
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BASet08Fore

@@BA04:     pop        ds
            les        di, [BitPlanes+4]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 04h
            jnz        @@BASet04Fore

@@BAClr04Fore:
            lodsb
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BAClr04Fore
            jmp        @@BA02

@@BASet04Fore:
            lodsb
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BASet04Fore

@@BA02:     pop        ds
            les        di, [BitPlanes+8]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 02h
            jnz        @@BASet02Fore

@@BAClr02Fore:
            lodsb
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BAClr02Fore
            jmp        @@BA01

@@BASet02Fore:
            lodsb
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BASet02Fore

@@BA01:     pop        ds
            les        di, [BitPlanes+12]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 01h
            jnz        @@BASet01Fore

@@BAClr01Fore:
            lodsb
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BAClr01Fore
            jmp        @@BA00

@@BASet01Fore:
            lodsb
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@BASet01Fore

@@BA00:     pop        ds

@@Exit:
            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DisplayXxYClip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Отобразить блок пикселов размером XxY с клиппингом по маске, где X<=8, Y любой
; BitMap     - указывает на табличку по байту
; Msk        - байтовая маска левой границы отсечения
; Wdth       - ширина блока в диапазоне [1..8],
; Hght       - высота блока (кол-во байт в таблице),
; Fore       - цвет символа

            PROC          DisplayXxYClip  FAR  X, Y, BitMap:DWORD, Msk:BYTE, Wdth, Hght, __Dummy, Fore:WORD
            LOCAL   BufOffs : WORD, LineLengthLocal : WORD
            LOCALS

            cld
            dec        [Wdth]
            mov        ax, [LineLength]
            mov        [LineLengthLocal], ax

            ;*** смещение в буфере
            mov        ax, [Y]
            mov        bx, [X]

            mul        [LineLengthLocal]; AX := y * BytesPerLine
            mov        dl, bl          ; DL := low-order byte of x
            shr        bx,3            ; BX := x/8
            add        bx, ax          ; BX := y*BytesPerLine + x/8
            mov        [BufOffs], bx   ; Save buffer offset for future use
            and        dl,7            ; DL := x & 7
            jz         @@CByteAligned

@@CNotByteAligned:
            mov        cl, [byte ptr Wdth]
            xor        cl, 7
            mov        bx, 0FFh
            shl        bl, cl
            mov        cl, dl
            xor        cl, 7
            inc        cl
            shl        bx, cl
            mov        dx, bx
            not        dx

            les        di, [BitPlanes+0]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 08h
            jnz        @@CNBASet08Fore

@@CNBAClr08Fore:
            lodsb
            and        al, [Msk]
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBAClr08Fore
            jmp        @@CNBA04Fore

@@CNBASet08Fore:
            lodsb
            and        al, [Msk]
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBASet08Fore

@@CNBA04Fore:pop        ds
            pop        [Hght]
            les        di, [BitPlanes+4]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 04h
            jnz        @@CNBASet04Fore

@@CNBAClr04Fore:
            lodsb
            and        al, [Msk]
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBAClr04Fore
            jmp        @@CNBA02Fore

@@CNBASet04Fore:
            lodsb
            and        al, [Msk]
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBASet04Fore

@@CNBA02Fore:pop        ds
            pop        [Hght]
            les        di, [BitPlanes+8]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 02h
            jnz        @@CNBASet02Fore

@@CNBAClr02Fore:
            lodsb
            and        al, [Msk]
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBAClr02Fore
            jmp        @@CNBA01Fore

@@CNBASet02Fore:
            lodsb
            and        al, [Msk]
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBASet02Fore

@@CNBA01Fore:pop        ds
            pop        [Hght]
            les        di, [BitPlanes+12]
            add        di, [BufOffs]
            push       [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 01h
            jnz        @@CNBASet01Fore

@@CNBAClr01Fore:
            lodsb
            and        al, [Msk]
            xor        ah, ah
            not        al
            shl        ax, cl
            or         ax, dx
            xchg       al, ah
            and        [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBAClr01Fore
            jmp        @@CNBA00Fore

@@CNBASet01Fore:
            lodsb
            and        al, [Msk]
            shl        ax, cl
            and        ax, bx
            xchg       al, ah
            or         [es:di], ax
            add        di, [LineLengthLocal]
            dec        [Hght]
            jnz        @@CNBASet01Fore

@@CNBA00Fore:pop        ds
            pop        [Hght]
            jmp        @@CExit

@@CByteAligned:
            mov        cl, [byte ptr Wdth]
            xor        cl, 7
            mov        bl, 0FFh
            shl        bl, cl
            mov        bh, bl
            not        bh
@@CBA08:
            les        di, [BitPlanes+0]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 08h
            jnz        @@CBASet08Fore

@@CBAClr08Fore:
            lodsb
            and        al, [Msk]
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBAClr08Fore
            jmp        @@CBA04

@@CBASet08Fore:
            lodsb
            and        al, [Msk]
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBASet08Fore

@@CBA04:     pop        ds
            les        di, [BitPlanes+4]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 04h
            jnz        @@CBASet04Fore

@@CBAClr04Fore:
            lodsb
            and        al, [Msk]
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBAClr04Fore
            jmp        @@CBA02

@@CBASet04Fore:
            lodsb
            and        al, [Msk]
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBASet04Fore

@@CBA02:     pop        ds
            les        di, [BitPlanes+8]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 02h
            jnz        @@CBASet02Fore

@@CBAClr02Fore:
            lodsb
            and        al, [Msk]
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBAClr02Fore
            jmp        @@CBA01

@@CBASet02Fore:
            lodsb
            and        al, [Msk]
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBASet02Fore

@@CBA01:     pop        ds
            les        di, [BitPlanes+12]
            add        di, [BufOffs]
            mov        cx, [Hght]
            push       ds
            lds        si, [BitMap]
            test       [Fore], 01h
            jnz        @@CBASet01Fore

@@CBAClr01Fore:
            lodsb
            and        al, [Msk]
            not        al
            or         al, bh
            and        [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBAClr01Fore
            jmp        @@CBA00

@@CBASet01Fore:
            lodsb
            and        al, [Msk]
            and        al, bl
            or         [es:di], al
            add        di, [LineLengthLocal]
            loop       @@CBASet01Fore

@@CBA00:     pop        ds

@@CExit:
            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PutBufferPart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC        PutBufferPart  FAR   X1, Y1, X2, Y2
            LOCALS

            cld
            dec         [X2]
            mov         bx, [X1]
            shr         bx, 4      ;bx-> word(x1)

            mov         cx, [X2]
            shr         cx, 4      ;cx-> word(x2)

            sub         cx, bx
            inc         cx         ;cx-> количество word

            ; начальный адрес
            mov         ax, [Y1]
            mul         [LineLength]       ; ax-> y * BytesPerLine
            shl         bx, 1
            add         bx, ax             ; bx-> смещение в буфере

            mov         ax, [Y1]
            sub         [Y2], ax
;            inc         [Y2]               ;Y2-> alias number of rows

            mov         es, [SegA000]

            ; Setting up graph controller
            mov         dx, 03CEh
            mov         ax, 0003h
            out         dx, ax
            mov         ax, 0805h
            out         dx, ax
            mov         ax, 0007h
            out         dx, ax
            mov         ax, 0FF08h
            out         dx, ax

            ; dx-> index of sequencer register
            mov         dl, 0C4h

            ; цикл по строкам
@@RowLoop:
            ; для каждой строки делать:

            mov         ax, 0802h
            out         dx, ax           ; Setting up 3rd bit plane
            push        ds cx
            lds         si, [BitPlanes+0]
            add         si, bx
            mov         di, bx
            rep         movsw
            pop         cx ds

            mov         ax, 0402h
            out         dx, ax           ; Setting up 2nd bit plane
            push        ds cx
            lds         si, [BitPlanes+4]
            add         si, bx
            mov         di, bx
            rep         movsw
            pop         cx ds

            mov         ax, 0202h
            out         dx, ax           ; Setting up 1st bit plane
            push        ds cx
            lds         si, [BitPlanes+8]
            add         si, bx
            mov         di, bx
            rep         movsw
            pop         cx ds

            mov         ax, 0102h
            out         dx, ax           ; Setting up zero bit plane
            push        ds cx
            lds         si, [BitPlanes+12]
            add         si, bx
            mov         di, bx
            rep         movsw
            pop         cx ds

            add         bx, [LineLength]

            dec         [Y2]
            jnz         @@RowLoop

            mov         ax, 0F02h
            out         dx, ax
            mov         dl, 0CEh
            mov         ax, 0005h
            out         dx, ax
            mov         ax, 0F07h
            out         dx, ax

            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectGetImage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC       DirectGetImage FAR X0, Y0, X1, Y1, BitMap:DWORD
            LOCAL      PixelRows, PixelRowLen, BytesPerRow
            LOCALS
;YNOT BytesPerRow     EQU     80
ByteOffsetShift EQU     3

            cld
            mov     ax, [LineLength]
            mov     [BytesPerRow], ax
            push    ds

; compute dimensions of bit block

            mov     ax, [X1]
            sub     ax, [X0]

            inc     ax
            push    ax
            dec     ax

            mov     cx, 0FF07h      ; CH := unshifted bit mask
                                    ; CL := AND mask for AL
            and     cl, al          ; CL := number of pixels in last
                                    ;  byte of row
            xor     cl, 7           ; CL := number of bits to shift
            shl     ch, cl          ; CH := bit mask for last byte of row
            mov     cl, ch
            push    cx              ; save on stack

            mov     cl, ByteOffsetShift
            shr     ax, cl
            inc     ax              ; AX := number of bytes per row
            push    ax              ; save on stack

            mov     ax, [Y1]
            sub     ax, [Y0]
            inc     ax              ; AX := number of pixel rows
            push    ax              ; save on stack

; establish addressing

            mov     ax, [Y0]
            mov     bx, [X0]

            mul     [LineLength] ; AX <- y*BytesPerLine

            mov     cl, bl          ; CL <- low-order byte of x

            shr     bx, 3           ; BX <- x/8
            add     bx, ax          ; BX = y*BytesPerLine + x/8

            mov     es, [SegA000]

            and     cl, 7           ; CL = x & 7
            xor     cl, 7           ; CL = number of bits to shift left
            mov     ah, 1           ; AH = unshifted bit mask

            xor     cl, 7           ; CL := number of bits to shift left
            push    es
            pop     ds
            mov     si, bx          ; DS:SI -> video buffer

            les     di, [BitMap]    ; ES:DI -> buffer in system RAM

; build 6-byte bit block header

            mov     ax, 1970h
            stosw                   ; Put Image Sign (1970h)
            pop     ax
            mov     [PixelRows], ax
            stosw                   ; byte 0-1 := number of pixel rows
            pop     ax
            mov     [PixelRowLen], ax
            stosw                   ; byte 2-3 := bytes per pixel row
            pop     ax
            mov     ch,al           ; CH := bit mask for last byte in row
            pop     ax
            stosw

; set up Graphics Controller

            mov     dx, 3CEh        ; DX := Graphics Controller address port

            mov     ax, 0005        ; AH := 0 (read mode 0, write mode 0)
                                    ; AL := 5 (Mode register number)
            out     dx, ax          ; set up read mode 0

            mov     ax, 0304h       ; AH := 3 (first bit plane to read)
                                    ; AL := 4 (Read Map Select reg number)

; copy from video buffer to system RAM

LGetBl01:   out     dx, ax          ; select next memory map to read
            push    ax              ; preserve memory map number
            push    [PixelRows]     ; preserve number of pixel rows
            push    si              ; preserve offset of x0,y0

LGetBl02:   mov     bx, [PixelRowLen]
            push    si              ; preserve SI at start of pixel row

LGetBl03:   lodsw                   ; AL := next byte in video buffer
                                    ; AH := (next byte) + 1
            dec     si              ; DS:SI -> (next byte) + 1
            rol     ax,cl           ; AL := next 4 pixels in row
            stosb                   ; copy to system RAM
            dec     bx              ; loop across row
            jnz     LGetBl03

            and     [es:di-1], ch   ; mask last byte in row
            pop     si              ; DS:SI -> start of row
;YNOT            add     si, BytesPerRow ; DS:SI -> start of next row
            add     si, [BytesPerRow]

            dec     [PixelRows]
            jnz     LGetBl02        ; loop down rows

            pop     si              ; DS:SI -> start of bit block
            pop     [PixelRows]     ; restore number of pixel rows
            pop     ax              ; AH := last map read
                                    ; AL := Read Map Select reg number
            dec     ah
            jns     LGetBl01        ; loop across bit planes

            mov     ax, di
            sub     ax, [word ptr BitMap]; AX := return value (size of bit block
                                    ;  in system RAM)

            pop     ds

            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectPutImage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC       DirectPutImage BitMap:DWORD, X, Y
            LOCAL      BytesPerRow, PixelRows, PixelRowLen, RowCounter, StartMask, EndMaskL, EndMaskR, RMWStack: BYTE
            LOCALS
;YNOT   BytesPerRow     EQU     80
ByteOffsetShift EQU     3

            cld
            mov     ax, [LineLength]
            mov     [BytesPerRow], ax
            push    ds

            mov     al, [RMWBits]
            mov     [RMWStack], al

; establish addressing

            mov     ax, [Y]
            mov     bx, [X]

            mul     [BytesPerRow] ; AX <- y*BytesPerLine

            mov     cl, bl          ; CL <- low-order byte of x

            shr     bx, 3           ; BX <- x/8
            add     bx, ax          ; BX = y*BytesPerLine + x/8

            mov     es, [SegA000]

            and     cl, 7           ; CL = x & 7
            xor     cl, 7           ; CL = number of bits to shift left
            mov     ah, 1           ; AH = unshifted bit mask

            inc     cl
            and     cl, 7           ; CL := number of bits to shift left
            mov     di, bx          ; ES:DI -> x,y in video buffer
            lds     si, [BitMap]    ; DS:SI -> buffer in system RAM

; obtain dimensions of bit block from header
            lodsw                   ; Skip Image Sign (1970h)
            lodsw                   ; AX := number of pixel rows
            mov     [PixelRows], ax
            lodsw                   ; AX := bytes per pixel row
            mov     [PixelRowLen], ax
; НОВЫЙ ВАРИАНТ - берем длину в пикселах и считаем маску сами
; Вот подходящая формула Mask = 0FFh SHL (((Length-1) AND 7) XOR 7)
                lodsw                   ; AX := длина в пикселах
                dec     al
                and     al, 7
                xor     al, 7
                push    cx
                mov     cl, al
                mov     al, 0FFh
                shl     al, cl
                pop     cx
                mov     ch, al          ; CH := bit mask for last byte in row

; set up Graphics Controller

                mov     dx,3CEh         ; DX := Graphics Controller I/O port

                mov     ah, [RMWStack]  ; AH := value for Data Rotate/Function
                mov     al,3            ;  Select register
                out     dx,ax           ; update this register

                mov     ax,0805h        ; AH := 8 (read mode 1, write mode 0)
                                        ; AL := 5 (Mode register number)
                out     dx,ax           ; set up read mode 0

                mov     ax,0007         ; AH := 0 (don't care for all maps;
                                        ;  CPU reads always return 0FFH)
                                        ; AL := 7 (Color Don't Care reg number)
                out     dx,ax           ; set up Color Don't Care reg

                mov     ax,0FF08h       ; AH := 0FFH (value for Bit Mask reg)
                out     dx,ax           ; set up Bit Mask reg

                mov     dl,0C4h         ; DX := 3C4H (Sequencer I/O port)
                mov     ax,0802h        ; AH := 1000B (value for Map Mask reg)
                                        ; AL := 2 (Map Mask register number)

                cmp     cx,0FF00h       ; if mask <> 0FFH or bits to shift <> 0
                jne     LPutBl05        ;  jump if not byte-aligned

; routine for byte-aligned bit blocks

                mov     cx, [PixelRowLen]

LPutBl00:       out     dx,ax           ; enable one bit plane for writes
                push    ax              ; preserve Map Mask value
                push    di              ; preserve video buffer offset of x,y
                mov     bx, [PixelRows]

LPutBl01:       push    di              ; preserve DI and CX
                push    cx

LPutBl02:
                lodsb                   ; AL := next byte of pixels
                and     [es:di],al      ; update bit plane
                inc     di
                loop    LPutBl02

                pop     cx              ; restore DI and CX
                pop     di
;YNOT                add     di,BytesPerRow  ; ES:DI -> next pixel row in buffer
                add     di,[BytesPerRow]  ; ES:DI -> next pixel row in buffer
                dec     bx
                jnz     LPutBl01        ; loop down pixel rows

                pop     di              ; ES:DI -> video buffer offset of x,y
                pop     ax              ; AH := current Map Mask reg value
                shr     ah,1            ; AH := new Map Mask value
                jnz     LPutBl00        ; loop across all bit planes

                jmp     LPutBlExit

; routine for non-aligned bit blocks

LPutBl05:       push    ax              ; preserve Map Mask reg values

                mov     bx,0FFh         ; BH := 0 (mask for first byte in row)
                                        ; BL := 0FFh
                mov     al,ch           ; AL := mask for last byte in pixel row
                cbw                     ; AH := 0FFh (mask for last-1 byte)

                cmp     [PixelRowLen],1
                jne     LPutBl06        ; jump if more than one byte per row

                mov     bl,ch
                mov     ah,ch           ; AH := mask for last-1 byte
                xor     al,al           ; AL := 0 (mask for last byte)

LPutBl06:       shl     ax,cl           ; shift masks into position
                shl     bx,cl

                mov     bl,al           ; save masks along with ..
                mov     al,8            ; .. Bit Mask register number
                mov     [EndMaskL],ax
                mov     ah,bl
                mov     [EndMaskR],ax
                mov     ah,bh
                mov     [StartMask],ax

                mov     bx,[PixelRowLen]
                pop     ax              ; restore Map Mask reg values

; set pixels row by row in the bit planes

LPutBl07:       out     dx,ax           ; enable one bit plane for writes
                push    ax              ; preserve Map Mask value
                push    di              ; preserve video buffer offset of x,y
                mov     dl,0CEh         ; DX := 3CEH (Graphics Controller port)

                mov     ax,[PixelRows]
                mov     [RowCounter],ax ; initialize loop counter

; set pixels at start of row in currently enabled bit plane

LPutBl08:       push    di              ; preserve offset of start of pixel row
                push    si              ; preserve offset of row in bit block
                push    bx              ; preserve bytes per pixel row

                mov     ax,[StartMask]
                out     dx,ax           ; set Bit Mask reg for first byte of row

                lodsw                   ; AH := 2nd byte of pixels
                                        ; AL := 1st byte of pixels
                dec     si              ; DS:SI -> 2nd byte of pixels
                test    cl,cl
                jnz     LPutBl09        ; jump if not left-aligned

                dec     bx              ; BX := bytes per row - 1
                jnz     LPutBl20        ; jump if at least 2 bytes per row
                jmp     short LPutBl22  ; jump if only one byte per row

LPutBl09:       rol     ax,cl           ; AH := left part of 1st byte,
                                        ;        right part of 2nd byte
                                        ; AL := right part of 1st byte,
                                        ;        left part of 2nd byte
                and     [es:di],ah      ; set pixels for left part of first byte
                inc     di
                dec     bx              ; BX := bytes per row - 2

LPutBl20:       push    ax              ; preserve pixels
                mov     ax,0FF08h
                out     dx,ax           ; set Bit Mask reg for succeeding bytes
                pop     ax

                dec     bx
                jng     LPutBl22        ; jump if only 1 or 2 bytes in pixel row

; set pixels in middle of row

LPutBl21:       and     [es:di],al      ; set pixels in right part of current
                inc     di              ;  byte and left part of next byte

                lodsw                   ; AH := next+1 byte of pixels
                dec     si              ; AL := next byte of pixels
                rol     ax,cl           ; AH := left part of next byte, right
                                        ;        part of next+1 byte
                                        ; AL := right part of next byte, left
                                        ;        part of next+1 byte
                dec     bx
                jnz     LPutBl21        ; loop across pixel row

; set pixels at end of row

LPutBl22:       mov     bx,ax           ; BH := right part of last byte, left
                                        ;        part of last-1 byte
                                        ; BL := left part of last byte, right
                                        ;        part of last-1 byte
                mov     ax,[EndMaskL]   ; AH := mask for last-1 byte
                                        ; AL := Bit Mask reg number
                out     dx,ax           ; set Bit Mask register
                and     [es:di],bl      ; set pixels for last-1 byte

                mov     ax,[EndMaskR]   ; mask for last byte in pixel row
                out     dx,ax           ; .. last byte in pixel row
                and     [es:di+1],bh    ; set pixels for last byte

                pop     bx              ; BX := bytes per pixel row
                pop     si
                add     si,bx           ; DS:SI -> next row in bit block
                pop     di
;YNOT                add     di,BytesPerRow  ; ES:DI -> next pixel row in buffer
                add     di,[BytesPerRow]  ; ES:DI -> next pixel row in buffer
                dec     [RowCounter]
                jnz     LPutBl08        ; loop down pixel rows

                pop     di              ; ES:DI -> video buffer offset of x,y
                pop     ax              ; AX := current Map Mask value
                mov     dl,0C4h         ; DX := 3C4H
                shr     ah,1            ; AH := next Map Mask value
                jnz     LPutBl07        ; loop across bit planes

; restore Graphics Controller and Sequencer to their default states

LPutBlExit:     mov     ax,0F02h        ; default Map Mask value
                out     dx,ax

                mov     dl,0CEh         ; DX := 3CEh
                mov     ax,0003         ; default Data Rotate/Function Select
                out     dx,ax

                mov     ax,0005         ; default Mode value
                out     dx,ax

                mov     ax,0F07h        ; default Color Compare value
                out     dx,ax

                mov     ax,0FF08h       ; default Bit Mask value
                out     dx,ax

            pop     ds

            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectHLine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC    DirectHLine FAR X1, Y1, X2, __Dummy, Color: BYTE
            LOCALS

            cld
            push    ds

            mov     dx, 3CEh
            mov     ax, 0005h
            out     dx, ax          ; Read Mode=0, Write Mode=0
            mov     ah, [Color]
            xor     al, al
            out     dx, ax          ; Set/Reset - set some bit maps
            mov     ax, 0FF01h
            out     dx, ax          ; Enable Set/Reset - all
            mov     ah, [RMWbits]
            mov     al, 3
            out     dx, ax          ; Rotate/Move
            mov     ax, [Y1]
            mov     bx, [X1]

            push    dx
            mul     [LineLength] ; AX <- y*BytesPerLine
            pop     dx
            mov     cl, bl          ; CL <- low-order byte of x
            shr     bx, 3           ; BX <- x/8
            add     bx, ax          ; BX = y*BytesPerLine + x/8
            mov     es, [SegA000]
            and     cl, 7           ; CL = x & 7
            xor     cl, 7           ; CL = number of bits to shift left
            mov     ah, 1           ; AH = unshifted bit mask

            mov     di, bx
            mov     dh, ah
            not     dh
            shl     dh, cl
            not     dh

            mov     cx, [X2]
            and     cl, 7
            xor     cl, 7
            mov     dl, 0FFh
            shl     dl, cl

            mov     cx, [X2]
            mov     ax, [X1]
            shr     cx, 3
            shr     ax, 3
            sub     cx, ax
            mov     bx, dx

            mov     dx, 3CEh
            mov     al, 8
            push    es
            pop     ds
            mov     si, di
            cld
            or      bh, bh
            js      HLineByteAligned
            or      cx, cx
            jne     HLineMultiByte
            and     bl, bh
            jmp     HLineSingleByte
HLineMultiByte:
            mov     ah, bh
            out     dx, ax
            movsb
            dec     cx
HLineByteAligned:
            mov     ah, 0FFh
            out     dx, ax
            rep     movsb
HLineSingleByte:
            mov     ah, bl
            out     dx, ax
            movsb

; Retrieve Graph Controller
            xor     ax, ax
            out     dx, ax
            inc     ax
            out     dx, ax
            mov     al, 3
            out     dx, ax
            mov     ax, 0FF08h
            out     dx, ax

            pop     ds

            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PixelAddr10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                PROC  PixelAddr10  NEAR
                LOCALS
                push    dx
                mul     [LineLength]
                pop     dx              ; AX = y*BytesPerLine

                mov     cl,bl           ; CL = low-order byte of x

                shr     bx,3            ; BX = x/8
                add     bx,ax           ; BX = y*BytesPerLine + x/8

                mov     ax,[SegA000]
                mov     es,ax           ; ES:BX = byte address of pixel

                and     cl,7            ; CL = x & 7
                xor     cl,7            ; CL = number of bits to shift left
                mov     ah,1            ; AH = unshifted bit mask
                ret

                ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HLineStyleT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC   HLineStyleT  FAR   X1, Y, X2, __Dummy, ForeC:BYTE, Pattern:BYTE
            LOCALS

            cld
            mov    ax, [Y]
            mov    bx, [X1]
            call   PixelAddr10
            mov    di, bx
            mov    bh, ah
            not    bh
            shl    bh, cl
            not    bh             ; bh <- leftmost byte bitmask
            mov    cx, [X2]
            and    cl, 7
            xor    cl, 7          ; cl <- shift for rightmost byte
            mov    bl, 0FFh
            shl    bl, cl         ; bl <- bit mask for the rightmost byte
            mov    cx, [X2]
            mov    ax, [X1]
            shr    cx, 3
            shr    ax, 3
            sub    cx, ax         ; cx <- number of fully touched bytes

;--- Setting up all constanly needed values
            mov    dx, 3CEh
            mov    ax, 0FF01h
            out    dx, ax         ; Enable Set/Reset <- ALL
            mov    ax, 00005h
            out    dx, ax         ; Read Mode <- 0; Write Mode <- 0
            mov    ah, [RMWbits]
            mov    al, 3
            out    dx, ax         ; Set function without any rotate
            mov    ah, [ForeC]
            xor    al, al
            out    dx, ax         ; Set/Reset - bit maps corresponding ForeC

;--- Procedure execution analysis
            or     cx, cx
            jnz    @@1
            and    bl, bh
            jmp    HLineSTSingleByte
@@1:
            or     bh, bh
            js     HLineSTByteAligned

HLineSTMultiByte:
;--- Updating values for leftmost pixels
            mov    al, 8
            mov    ah, bh
            and    ah, [Pattern]
            out    dx, ax         ; Bit mask corresponding Pattern
            or     [byte ptr es:di], 0
            dec    cx
            inc    di
            jcxz   HLineSTSingleByte

HLineSTByteAligned:
;--- Updating values for middle pixels
            mov    al, 8
            mov    ah, [Pattern]
            out    dx, ax         ; Bit mask corresponding Pattern
@@2:
            or     [byte ptr es:di], 0
            inc    di
            loop   @@2

HLineSTSingleByte:
            mov    ah, bl
            and    ah, [Pattern]
            mov    al, 8
            out    dx, ax
            or     [byte ptr es:di], 0

;--- Retrieve Graph Controller
            xor    ax, ax
            out    dx, ax
            inc    ax
            out    dx, ax
            mov    al, 3
            out    dx, ax
            mov    ax, 0FF08h
            out    dx, ax

            RET
            ENDP



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VLineStyleT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            PROC   VLineStyleT  FAR   X, Y1, Y2, Wdth, __Dummy, ForeC:BYTE, Pattern
            LOCAL  BytesPerRow, DoubleBytesPerRow
            LOCALS

            cld
            mov    ax, [LineLength]
            mov    [BytesPerRow], ax
            shl    ax, 1
            mov    [DoubleBytesPerRow], ax
            mov    ax, [Y1]
            mov    bx, [X]
            call   PixelAddr10
            mov    di, bx
            mov    dh, ah
            not    dh
            shl    dh, cl
            not    dh
            mov    ax, [X]
            mov    cx, ax
            add    cx, [Wdth]
            dec    cx
            mov    bx, cx
            and    cl, 7
            xor    cl, 7
            mov    dl, 0FFh
            shl    dl, cl
            shr    ax, 3
            shr    bx, 3
            mov    cx, bx
            sub    cx, ax
            mov    bx, dx

;--- Setting up all constanly needed values
            mov    dx, 3CEh
            mov    ax, 0FF01h
            out    dx, ax
            mov    ah, [ForeC]
            xor    al, al
            out    dx, ax
            mov    ax, 00005h
            out    dx, ax
            mov    ah, [RMWbits]
            mov    al, 3
            out    dx, ax
;--- Analysis of an execution branch that needed
            or     cx, cx
            jnz    @@1
            and    bl, bh
            jmp    VLineSTSingleByte
@@1:
VLineSTTwoBytes:
;--- Updating values for leftmost pixels
            mov    al, 8
            mov    ah, bh
            and    ah, [byte ptr Pattern+0]
            out    dx, ax
            mov    cx, [Y2]
            sub    cx, [Y1]
            inc    cx
            push   di
            push   cx
            inc    cx
            shr    cx, 1
@@2:
            or     [byte ptr es:di], 0
            add    di, [DoubleBytesPerRow]
            loop   @@2
            pop    cx
            pop    di
          ; al already = 8 !
            mov    ah, bh
            and    ah, [byte ptr Pattern+1]
            out    dx, ax
            push   di
            add    di, [BytesPerRow]
            shr    cx, 1
@@3:
            or     [byte ptr es:di], 0
            add    di, [DoubleBytesPerRow]
            loop   @@3
            pop    di
            inc    di

VLineSTSingleByte:
            mov    al, 8
            mov    ah, bl
            and    ah, [byte ptr Pattern+0]
            out    dx, ax
            mov    cx, [Y2]
            sub    cx, [Y1]
            inc    cx
            push   di
            push   cx
            inc    cx
            shr    cx, 1
@@4:
            or     [byte ptr es:di], 0
            add    di, [DoubleBytesPerRow]
            loop   @@4
            pop    cx
            pop    di
            mov    ah, bl
            and    ah, [byte ptr Pattern+1]
            out    dx, ax
            add    di, [BytesPerRow]
            shr    cx, 1
@@5:
            or     [byte ptr es:di], 0
            add    di, [DoubleBytesPerRow]
            loop   @@5

;--- Retrieve Graph Controller
            xor    ax, ax
            out    dx, ax
            inc    ax
            out    dx, ax
            mov    al, 3
            out    dx, ax
            mov    ax, 0FF08h
            out    dx, ax

            RET
            ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MakeBWBuffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC    MakeBWBuffer Far Destin:DWORD, Srce:DWORD, WidthBuf, HeightBuf, WIB, Col0, Col1, Inve
        LOCALS
        PUSH    ds
        LES     di, [Destin]
        cld
        mov     ax, 1970h
        stosw                   ; Put Image Sign (1970h)
        MOV     ax, [HeightBuf]
        stosw
        MOV     ax, [WidthBuf]
        ADD     ax, 7
        SHR     ax, 3
        stosw
        MOV     dx, ax
        MOV     ax, [WidthBuf]
        stosw
        MOV     bl, [byte Col0]
        MOV     bh, [byte Col1]
        MOV     CX, 4
@@L1:
        PUSH    CX
        test    bl, 8
        JNE     @@L2
        test    bh, 8
        JE      MBW_ZeroFill
        JMP     MBW_Transparent
        JMP     @@L3
@@L2:
        test    bh, 8
        JE      MBW_Inverse
@@L3:
MBW_UnitFill:
        PUSH    dx
        MOV     ax,     [HeightBuf]
        mul     dx
        POP     dx
        MOV     cx, ax
        MOV     al, 0FFh
        rep     stosb
        JMP     MBW_EndPlain
MBW_ZeroFill:
        PUSH    dx
        MOV     ax, [HeightBuf]
        mul     dx
        POP     dx
        MOV     cx, ax
        XOR     al, al
        rep     stosb
        JMP     MBW_EndPlain
MBW_Transparent:
        LDS     si, [Srce]
        CMP     [Inve], 00
        JNE     @@L4
        MOV     CX, [HeightBuf]
@@L5:
        PUSH    CX
        MOV     cx, dx
        PUSH    si
        rep     movsb
        POP     si
        ADD     si, [WIB]
        POP     CX
        LOOP    @@L5
        JMP     @@L6
@@L4:
        PUSH    dx
        MOV     ax, [WIB]
        MOV     cx, [HeightBuf]
        mul     cx
        POP     dx
        ADD     si, ax
@@L7:
        PUSH    CX
        SUB     si, [WIB]
        MOV     cx, dx
        PUSH    si
        rep     movsb
        POP     si
        POP     CX
        LOOP    @@L7
@@L6:
        JMP     MBW_EndPlain
MBW_Inverse:
        LDS     si, [Srce]
        CMP     [Inve], 00
        JNE     @@L8
        MOV     CX, [HeightBuf]
@@L9:
        PUSH    CX
        PUSH    si
        MOV     CX, dx
@@L10:
        lodsb
        not     al
        stosb
        LOOP    @@L10
        POP     si
        ADD     si, [WIB]
        POP     CX
        LOOP    @@L9
        JMP     @@L11
@@L8:
        PUSH    dx
        MOV     ax, [WIB]
        MOV     cx, [HeightBuf]
        mul     cx
        POP     dx
        ADD     si, ax
@@L12:
        PUSH    CX
        SUB     si, [WIB]
        MOV     cx, dx
        PUSH    si
        MOV     cx, dx
@@L13:
        lodsb
        not     al
        stosb
        LOOP    @@L13
        POP     si
        POP     CX
        LOOP    @@L12
@@L11:
MBW_EndPlain:
        SHL     bl, 1
        SHL     bh, 1
        POP     CX
        LOOP    @@L1_1
        POP     ds
        RET
@@L1_1:
        jmp     @@L1
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Make16Buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC Make16Buffer Far Destin:DWORD, Srce:DWORD, WidthBuf, Height, WIB, Inve
     LOCAL      PlainSize:WORD
     LOCALS
        PUSH    ds
        LES     di,     [Destin]
        cld
        mov     ax, 1970h
        stosw                   ; Put Image Sign (1970h)
        MOV     ax,     [Height]
        MOV     bx,     ax
        stosw
        MOV     ax,     [WidthBuf]
        ADD     ax,     7
        SHR     ax,     1
        SHR     ax,     1
        SHR     ax,     1
        stosw
        MOV     cx,     ax
        mul     bx
        MOV     [PlainSize],    ax
        MOV     dx,     cx
        MOV     ax,     [WidthBuf]
        stosw
        CMP     [Inve], 00
        JNE     @@L1
        LDS     si,     [Srce]
        MOV     CX,     [Height]
@@L2:
        PUSH    CX
        PUSH    si
        PUSH    dx
@@L3:
        OR      dx,     dx
        JLE     @@L4
        PUSH    dx
        XOR     dx,     dx
        XOR     cx,     cx
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        MOV     [es:di],        dh
        MOV     bx,     [PlainSize]
        MOV     [es:di+bx],     dl
        SHL     bx,     1
        MOV     [es:di+bx],     ch
        ADD     bx,     [PlainSize]
        MOV     [es:di+bx],     cl
        POP     dx
        DEC     dx
        INC     di
        JMP     @@L3
@@L4:
        POP     dx
        POP     si
        ADD     si,     [WIB]
        POP     CX
        LOOP    @@L2xxxx
        JMP     @@L5
@@L2xxxx:
  JMP   @@L2
@@L1:
        LDS     si,     [Srce]
        PUSH    dx
        MOV     ax,     [WIB]
        MOV     cx,     [Height]
        mul     cx
        POP     dx
        ADD     si,     ax
        MOV     CX,     [Height]
@@L6:
        PUSH    CX
        SUB     si,     [WIB]
        PUSH    si
        PUSH    dx
@@L7:
        OR      dx,     dx
        JLE     @@L8
        PUSH    dx
        XOR     dx,     dx
        XOR     cx,     cx
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        lodsb
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        RCL     al,     1
        RCL     dh,     1
        RCL     al,     1
        RCL     dl,     1
        RCL     al,     1
        RCL     ch,     1
        RCL     al,     1
        RCL     cl,     1
        MOV     [es:di],        dh
        MOV     bx,     [PlainSize]
        MOV     [es:di+bx],     dl
        SHL     bx,     1
        MOV     [es:di+bx],     ch
        ADD     bx,     [PlainSize]
        MOV     [es:di+bx],     cl
        POP     dx
        DEC     dx
        INC     di
        JMP     @@L7
@@L8:
        POP     dx
        POP     si
        POP     CX
        LOOP    @@L6xxxx
  JMP @@L5
@@L6xxxx:
  JMP @@L6
@@L5:
        POP     ds
        RET
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ReMap16 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC ReMap16 Far
; Buffer:DWORD, BufSize, ColorTable: DWORD
        LOCALS
        ColO    EQU     [bp+6]
        ColS    EQU     [bp+8]
        BufSize EQU     [bp+10]
        BufO    EQU     [bp+12]
        BufS    EQU     [bp+14]
        @@LRETN =10
  cld
        PUSH    BP
        MOV     BP,     SP
        MOV     ax,     sp
        CMP     ax,     280
        JAE     @@L9
        MOV     ax,     0FFFFh
        JMP             RM16_Exit
@@L9:
        SUB     sp,     256
        PUSH    ds
        PUSH    es
        PUSH    si
        PUSH    di
        LDS     si,     ColO
        XOR     di,     di
        MOV     CX,     256
@@L10:
        MOV     bx,     di
        SHR     bl,     1
        SHR     bl,     1
        SHR     bl,     1
        SHR     bl,     1
        MOV     al,     [si+bx]
        SHL     al,     1
        SHL     al,     1
        SHL     al,     1
        SHL     al,     1
        MOV     bx,     di
        AND     bl,     0Fh
        OR      al,     [si+bx]
        MOV     [bp+di-256],    al
        INC     di
        LOOP    @@L10
        LES     di,     BufO
        PUSH    ss
        POP     ds
        LEA     bx,     [bp-256]
        MOV     CX,     BufSize
@@L11:
        MOV     al,     [es:di]
        xlat
        stosb
        LOOP    @@L11
        POP     di
        POP     si
        POP     es
        POP     ds
        XOR     ax,     ax
        MOV     sp,     bp
RM16_Exit:
        POP     BP
        RET     @@LRETN
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GetBitMapBufferSize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC GetBitMapBufferSize Far ImageWidth, ImageHeight
        LOCALS
; Запомните - я возвращаю FFFF (-1), если размер требуемого
;  буфера превышает 64 кБ
        mov     ax, [ImageWidth]
        add     ax, 7
        shr     ax, 3                   ; ax <- длина строки пикселов в байтах
        mov     bx, [ImageHeight]
        mul     bx                      ; dx,ax - число байт на одну плоскость
        or      dx, dx
        ja      @@TooBig
        cmp     ax, 3FF7h
        ja      @@TooBig
        shl     ax, 2                   ; 4 битовых плоскости
        add     ax, 8                   ; заголовок блока (8 байт)
        jmp     @@Exit
@@TooBig:
        mov     ax, 0FFFFh
@@Exit:
        ret
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PutBMPPart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;---
;
;  Оригинал последующей процедуры был разрабатон для прямого вывода
;  на экран и вложенность циклов была такая - внешний по строкам и
;  внутренний по плоскостям, с целью избежать неприятного цветового
;  мерцания выводимого изображения. В данной процедур вложенность
;  сменена на обратную - внешний по плоскостям, внутренний по строкам,
;  так как этот метод дает существенно лучшую производительность,
;  а мерцание отсутствует в условиях вывода в промежуточный буфер.
;
;;---
PROC PutBMPPart FAR Buffer:DWORD, xxx, yyy, xFrom, yFrom, xLen, yLen
LOCAL BytesPerRow, ShiftBuf:BYTE, ShiftScr:BYTE, MaskLeft:BYTE, MaskRight:BYTE, OffsLeft, OffsRight, WIB, BPS, PlCnt
        LOCALS
        cld
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        push    ds
;
;--- Проверки - можно ли выводить и сколько выводить
        lds     si, [Buffer]
        cld
        lodsw                   ; Skip Image Sign (1970h)
        lodsw
        mov     bx, ax
        cmp     ax, [yFrom]
        jle     @@Exit              ;TONY (was jl)
        cmp     [yLen], 0
        jne     @@1
        mov     [yLen], ax
@@1:
        sub     ax, [yFrom]
        cmp     ax, [yLen]
        jnl     @@2
        mov     [yLen], ax
@@2:
        lodsw
        mov     [WIB], ax
        mul     bx
        mov     [BPS], ax
        lodsw
        cmp     ax, [xFrom]
        jle     @@Exit              ;TONY (was jl)
        cmp     [xLen], 0
        jne     @@3
        mov     [xLen], ax
@@3:
        sub     ax, [xFrom]
        cmp     ax, [xLen]
        jnl     @@4
        mov     [xLen], ax
@@4:
;--- Считаем начальные адреса в буфере BMP
        mov     ax, [xFrom]
        shr     ax, 3
        add     si, ax
        mov     ax, [yFrom]
        mul     [WIB]
        add     si, ax
        pop     ds              ; ds <-- Data Segment
        push    ds
;--- Считаем адреса в буфере экрана
        mov     ax, [yyy]
        mov     bx, [xxx]
        call    PixelAddr10
        mov     [OffsLeft], bx
        mov     ax, [yyy]
        mov     bx, [xxx]
        add     bx, [xLen]
        dec     bx
        call    PixelAddr10
        mov     [OffsRight], bx
;--- Считаем значения сдвигов
        mov     ch, [byte xFrom]
        mov     cl, [byte xxx]
        and     cx, 0707h
        mov     [ShiftBuf], ch
        mov     [ShiftScr], cl
;--- Если оба сдвига нулевые, можем сделать очень быстро
        jz      @@ByteAligned           ; AND установил флаги
;--- Считаем маски
        mov     ax, 0FFFFh
        shr     ah, cl
        mov     [MaskLeft], ah
        mov     cx, [xxx]
        add     cx, [xLen]
        and     cl, 7
        shr     al, cl
        not     al
        jnz     @@5
        mov     al, 0FFh
@@5:
        mov     [MaskRight], al
;--- Считаем счетчик
        mov     cx, bx                  ; OffsRight
        sub     cx, [OffsLeft]
        jz      @@AloneByte             ; SUB установил флаги
        dec     cx
        jz      @@OutLeft               ; DEC установил флаги
@@Middle:
;--- Средняя часть
        mov     dl, [ShiftBuf]
        mov     dh, [ShiftScr]
        cmp     dl, dh
        jna     @@MiddleShiftRight
@@MiddleShiftLeft:
        sub     dl, dh                  ; dl <-- на сколько двигать влево
        push    si                      ; Для последующих частей
;--- Организуем внешний цикл по плоскостям
        mov     dh, 4
        inc     si
        mov     bx, offset BitPlanes
;--- Начало внешнего цикла по плоскостям
@@BegOfLoop_1:
        or      dh, dh
        jz      @@EndOfLoop_1
        dec     dh
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        inc     di
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        push    bx                      ; Сохр. смещение на адр. вирт. буфера
        mov     bx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@BegOfLoop_2:
        or      bx, bx
        jz      @@EndOfLoop_2
        dec     bx
        push    di
        push    si
        push    cx
;
@@InternalLoop_1:
        push    cx
        mov     cl, dl                  ; На сколько сдвигать влево
        mov     ax, [ds:si]
        xchg    ah, al
        shl     ax, cl
        mov     [es:di], ah
        inc     si
        inc     di
        pop     cx
        loop    @@InternalLoop_1
;
        pop     cx
        pop     si
        add     si, [WIB]               ; Следующая строка в BitMap'е
        pop     di
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        jmp     @@BegOfLoop_2
@@EndOfLoop_2:
        pop     bx                      ; Восст. смещ. на адр. вирт. буфера
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@BegOfLoop_1
@@EndOfLoop_1:
        pop     si                      ; Самое начало в BitMap'е
        jmp     @@OutLeft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@MiddleShiftRight:
        sub     dh, dl
        mov     dl, dh                  ; dl <-- на сколько двигать вправо
        push    si                      ; Для последующих частей
;--- Организуем внешний цикл по плоскостям
        mov     dh, 4
        mov     bx, offset BitPlanes
;--- Начало внешнего цикла по плоскостям
@@BegOfLoop_3:
        or      dh, dh
        jz      @@EndOfLoop_3
        dec     dh
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        inc     di
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        push    bx                      ; Сохр. смещение на адр. вирт. буфера
        mov     bx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@BegOfLoop_4:
        or      bx, bx
        jz      @@EndOfLoop_4
        dec     bx
        push    di
        push    si
        push    cx
;
@@InternalLoop_2:
        push    cx
        mov     cl, dl                  ; На сколько сдвигать вправо
        mov     ax, [ds:si]
        xchg    ah, al
        shr     ax, cl
        stosb
        inc     si
        pop     cx
        loop    @@InternalLoop_2
;
        pop     cx
        pop     si
        add     si, [WIB]               ; Следующая строка в BitMap'е
        pop     di
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        jmp     @@BegOfLoop_4
@@EndOfLoop_4:
        pop     bx                      ; Восст. смещ. на адр. вирт. буфера
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@BegOfLoop_3
@@EndOfLoop_3:
        pop     si                      ; Самое начало в BitMap'е
;;        jmp     @@OutLeft

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@OutLeft:
        push    cx
        push    si
        mov     [PlCnt], 4              ; Счетчик плоскостей
        mov     dl, [MaskLeft]          ; 1 - Что можно трогать
        mov     dh, dl
        not     dh                      ; 1 - Что нельзя трогать
        mov     bx, offset BitPlanes
        mov     cl, [ShiftBuf]
        mov     ch, [ShiftScr]
;
@@OutLeftLoop_1:
        cmp     [PlCnt], 0              ; Внешний цикл по плоскостям
        je      @@EndOutLeft
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        push    bx                      ; Указатель плоскостей
        mov     ds, [word Buffer+2]     ; сегмент BitMap-буфера
        mov     bx, [yLen]
        push    si
@@OutLeftLoop_2:
        or      bx, bx
        jz      @@OutLeftNextPlane
;
        mov     ax, [ds:si]
        xchg    al, ah
        shl     ax, cl
        xchg    ch, cl
        shr     ax, cl
        xchg    ch, cl
        and     [es:di], dh             ; маскируем, что нельзя трогать
        and     ah, dl                  ; что можно трогать
        or      [es:di], ah             ; обновляем
;
        dec     bx
        add     si, [WIB]
        add     di, [BytesPerRow]
        jmp     @@OutLeftLoop_2
@@OutLeftNextPlane:
        pop     si
        add     si, [BPS]
        pop     bx
        pop     ds
        jmp     @@OutLeftLoop_1
@@EndOutLeft:
;
        pop     si
        pop     cx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@OutRight:
        add     si, cx
        mov     [PlCnt], 4              ; Счетчик плоскостей
        mov     dl, [MaskRight]         ; 1 - Что можно трогать
        mov     dh, dl
        not     dh                      ; 1 - Что нельзя трогать
        mov     bx, offset BitPlanes
        mov     cl, [ShiftBuf]
        mov     ch, [ShiftScr]
        cmp     cl, ch
        jna     @@OutRight_2
;
        sub     cl, ch
        inc     si
;
@@OutRightLoop_1:
        cmp     [PlCnt], 0              ; Внешний цикл по плоскостям
        je      @@EndOutRight
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsRight]         ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        push    bx                      ; Указатель плоскостей
        mov     ds, [word Buffer+2]     ; сегмент BitMap-буфера
        mov     bx, [yLen]
        push    si
@@OutRightLoop_2:
        or      bx, bx
        jz      @@OutRightNextPlane
;
        mov     ax, [ds:si]
        xchg    al, ah
        shl     ax, cl
        and     [es:di], dh             ; маскируем, что нельзя трогать
        and     ah, dl                  ; что можно трогать
        or      [es:di], ah             ; обновляем
;
        dec     bx
        add     si, [WIB]
        add     di, [BytesPerRow]
        jmp     @@OutRightLoop_2
@@OutRightNextPlane:
        pop     si
        add     si, [BPS]
        pop     bx
        pop     ds
        jmp     @@OutRightLoop_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@OutRight_2:
        sub     cl, ch
        neg     cl
;
@@OutRightLoop_3:
        cmp     [PlCnt], 0              ; Внешний цикл по плоскостям
        je      @@EndOutRight
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsRight]         ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        push    bx                      ; Указатель плоскостей
        mov     ds, [word Buffer+2]     ; сегмент BitMap-буфера
        mov     bx, [yLen]
        push    si
@@OutRightLoop_4:
        or      bx, bx
        jz      @@OutRightNextPlane1
;
        mov     ax, [ds:si]
        xchg    al, ah
        shr     ax, cl
        and     [es:di], dh             ; маскируем, что нельзя трогать
        and     al, dl                  ; что можно трогать
        or      [es:di], al             ; обновляем
;
        dec     bx
        add     si, [WIB]
        add     di, [BytesPerRow]
        jmp     @@OutRightLoop_4
@@OutRightNextPlane1:
        pop     si
        add     si, [BPS]
        pop     bx
        pop     ds
        jmp     @@OutRightLoop_3
@@EndOutRight:
        jmp     @@Exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@AloneByte:
        mov     al, [MaskLeft]
        and     [MaskRight], al
        dec     si
        jmp     @@OutRight              ; Почти то же самое
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@ByteAligned:
; Процедура для выровненного на границу байта образа
    ; расчет маски для последнего байта по формуле
    ; MaskRight = 0FFh SHL (((xLen-1) AND 7) XOR 7)
        mov     cl, [byte xLen]
        dec     cl
        and     cl, 7
        xor     cl, 7
        mov     al, 0FFh
        shl     al, cl
        mov     [MaskRight], al
    ; определим ширину в байтах того, что мы должны выводить !
    ; Эта величина не обязательно должна быть равна WidthInBytes,
    ; а может быть и похитрее
        mov     dx, [xLen]
        add     dx, 7
        shr     dx, 3
        jcxz    @@101           ; Равносильно if MaskRight=0FFh goto @@101
        dec     dx
@@101:
        or      dx, dx
        je      @@ByteAligned_Right
        mov     bx, offset BitPlanes
        mov     [PlCnt], 4
        push    si
@@ByteAlignedLoop_1:
        cmp     [PlCnt], 0
        je      @@EndOfByteAlignedLoop_1
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        push    bx                      ; Сохр. смещение на адр. вирт. буфера
        mov     bx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@ByteAlignedLoop_2:
        or      bx, bx
        jz      @@EndOfByteAlignedLoop_2
        dec     bx
        mov     cx, dx
;
        push    si
        push    di
        rep     movsb
        pop     di
        pop     si
;
        add     si, [WIB]               ; Следующая строка в BitMap'е
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        jmp     @@ByteAlignedLoop_2
@@EndOfByteAlignedLoop_2:
        pop     bx                      ; Восст. смещ. на адр. вирт. буфера
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@ByteAlignedLoop_1
@@EndOfByteAlignedLoop_1:
        pop     si                      ; Самое начало в BitMap'е
        cmp     [MaskRight], 0FFh       ; Нет правоя части вообще ???
        je      @@Exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@ByteAligned_Right:
        add     si, dx
        ;add    di, dx  ; ЛУЧШЕ брать правую часть, она посчитана
        mov     bx, offset BitPlanes
        mov     [PlCnt], 4
        mov     dl, [MaskRight]         ; 1 - Что можно
        mov     dh, dl
        not     dh                      ; 1 - Что нельзя !
@@ByteAlignedLoop_3:
        cmp     [PlCnt], 0
        je      @@EndOfByteAlignedLoop_3
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsRight]         ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        mov     cx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@ByteAlignedLoop_4:
        mov     al, [ds:si]
        and     [es:di], dh
        and     al, dl
        or      [es:di], al
        add     si, [WIB]               ; Следующая строка в BitMap'е
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        loop    @@ByteAlignedLoop_4
;
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@ByteAlignedLoop_3
@@EndOfByteAlignedLoop_3:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@Exit:
        pop     ds
        RET
ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectDisplayXxY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC DirectDisplayXxY Far argX, argY, _Table:DWORD, _Width, _Height, __Dummy, Fore:BYTE, __Dummy1:WORD, Back:BYTE,  Erase:BYTE
        LOCAL   BytesPerRow, RMWStack:BYTE
        LOCALS
        cld
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        mov     al, [RMWBits]
        mov     [RMWStack], al
        PUSH    ds
        MOV     ax, [argY]
        MOV     bx, [argX]
        CALL    PixelAddr10
        INC     cl
        AND     cl, 7
        MOV     al, cl
        MOV     ch, 0FFh
        MOV     cl, 8
        SUB     cl, [byte _Width]
        SHL     ch, cl
        MOV     cl, al
        PUSH    es
        POP     ds
        MOV     si, bx
        les     di, [_Table]
        MOV     dx, 3CEh
        MOV     ax, 0A05h
        out     dx, ax
        MOV     al, 3
        MOV     ah, [RMWStack]
        out     dx, ax
        MOV     ax, 0007
        out     dx, ax
        CMP     [Erase], 0
        JE      @@L1
        MOV     bl, [Fore]
        MOV     bh, [Back]
        OR      cl, cl
        JNE     @@L2
        MOV     al, 8
@@L3:
        CMP     [_Height], 0
        JBE     @@L8
        MOV     ah, [es:di]
        MOV     cl, ah
        AND     ah, ch
        out     dx, ax
        and     [ds:si], bl
        MOV     ah, cl
        not     ah
        AND     ah, ch
        out     dx, ax
        and     [ds:si], bh
        INC     di
        ADD     si, [BytesPerRow]
        DEC     [_Height]
        JMP     @@L3
@@L2:
@@L6:
        CMP     [_Height], 0
        JBE     @@L7
        MOV     al, [es:di]
        AND     al, ch
        XOR     ah, ah
        SHL     ax, cl
        PUSH    ax
        MOV     al, 8
        out     dx, ax
        and     [ds:si], bl
        POP     ax
        MOV     ah, al
        MOV     al, 8
        out     dx, ax
        and     [ds:si+1], bl
        MOV     al, [es:di]
        not     al
        AND     al, ch
        XOR     ah, ah
        SHL     ax, cl
        PUSH    ax
        MOV     al, 8
        out     dx, ax
        and     [ds:si], bh
        POP     ax
        MOV     ah, al
        MOV     al, 8
        out     dx, ax
        and     [ds:si+1], bh
        INC     di
        ADD     si, [BytesPerRow]
        DEC     [_Height]
        JMP     @@L6
@@L7:
        JMP     @@L8
@@L1:
        MOV     bl, [Fore]
        OR      cl, cl
        JNE     @@L9
        MOV     al, 8
@@L10:
        CMP     [_Height], 0
        JBE     @@L12
        MOV     ah, [es:di]
        AND     ah, ch
        out     dx, ax
        and     [ds:si], bl
        INC     di
        ADD     si, [BytesPerRow]
        DEC     [_Height]
        JMP     @@L10
@@L9:
@@L13:
        CMP     [_Height], 0
        JBE     @@L14
        MOV     al, [es:di]
        AND     al, ch
        XOR     ah, ah
        SHL     ax, cl
        PUSH    ax
        MOV     al, 8
        out     dx, ax
        and     [ds:si], bl
        POP     ax
        MOV     ah, al
        MOV     al, 8
        out     dx, ax
        and     [ds:si+1], bl
        INC     di
        ADD     si, [BytesPerRow]
        DEC     [_Height]
        JMP     @@L13
@@L14:
@@L12:
@@L8:
        MOV     ax, 0FF08h
        out     dx, ax
        MOV     ax, 00005h
        out     dx, ax
        MOV     ax, 00003h
        out     dx, ax
        MOV     ax, 00F07h
        out     dx, ax
        POP     ds
        RET
ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectPutPartOfImage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC DirectPutPartOfImage Far Buf:DWORD, xxx, yyy, xFrom, yFrom, xLen, yLen
LOCAL BytesPerRow, ShiftBuf:BYTE, ShiftScr:BYTE, MaskLeft:BYTE, MaskRight:BYTE, OffsLeft, OffsRight, WIB, BPS, RMWStack:BYTE
LOCALS
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        PUSH    ds
        mov     al, [RMWBits]
        mov     [RMWStack], al
        LDS     si, [Buf]
        cld
        lodsw                   ; Skip Image Sign (1970h)
        lodsw
        MOV     bx, ax
        CMP     ax, [yFrom]
        JB      PPI_Exit1
        CMP     [yLen], 0
        JNE     @@L1
        MOV     [yLen], ax
@@L1:
        SUB     ax, [yFrom]
        CMP     ax, [yLen]
        JAE     @@L2
        MOV     [yLen], ax
@@L2:
        lodsw
        MOV     [WIB], ax
        mul     bx
        MOV     [BPS], ax
        lodsw
        CMP     ax, [xFrom]
        JB      PPI_Exit1
        CMP     [xLen], 0
        JNE     @@L3
        MOV     [xLen], ax
@@L3:
        SUB     ax, [xFrom]
        CMP     ax, [xLen]
        JAE     @@L4
        MOV     [xLen], ax
@@L4:
        MOV     dx, 03CEh
        MOV     ah, [RMWStack]
        MOV     al, 3
        out     dx, ax
        MOV     ax, [xFrom]
        SHR     ax, 3
        ADD     si, ax
        MOV     ax, [yFrom]
        MUL     [WIB]
        ADD     si, ax
        MOV     ax, [yyy]
        MOV     bx, [xxx]
        pop     ds
        CALL    PixelAddr10
        MOV     [OffsLeft], bx
        MOV     di, bx
        MOV     ax, [yyy]
        MOV     bx, [xxx]
        ADD     bx, [xLen]
        DEC     bx
        CALL    PixelAddr10
        push    ds
        mov     ds, [word Buf+2]
        MOV     [OffsRight], bx
        MOV     ch, [byte xFrom]
        AND     ch, 7
        MOV     [ShiftBuf], ch
        MOV     cl, [byte xxx]
        AND     cl, 7
        MOV     [ShiftScr], cl
        OR      cx, cx
        JE      PPI_ByteAligned
        MOV     al, 0FFh
        SHR     al, cl
        MOV     [MaskLeft], al
        MOV     cl, [byte xxx]
        ADD     cl, [byte xLen]
        AND     cl, 7
        MOV     al, 0FFh
        SHR     al, cl
        not     al
        OR      al, al
        JNE     @@L5
        MOV     al, 0FFh
@@L5:
        MOV     [MaskRight], al
        MOV     cx, bx
        SUB     cx, di
        jz      PPI_AloneByte
        DEC     cx
        jz      PPI_OutLeft
PPI_Middle:
        MOV     bx, [yLen]
        MOV     dx, 03C4h
        PUSH    si
        INC     di
        MOV     al, [ShiftBuf]
        MOV     ah, [ShiftScr]
        CMP     al, ah
        JBE     @@L6
        INC     si
@@L7:
        OR      bx, bx
        JBE     @@L8
        MOV     ax, 0802h
        PUSH    bx
        PUSH    si
        PUSH    di
        MOV     bl, [ShiftBuf]
        SUB     bl, [ShiftScr]
@@L9:
        OR      ah, ah
        JBE     @@L10
        out     dx, ax
        SHR     ah, 1
        PUSH    ax
        PUSH    cx
        PUSH    si
        PUSH    di
@@L11:
        PUSH    cx
        MOV     cl, bl
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        MOV     al, [es:di]
        MOV     [es:di], ah
        INC     si
        INC     di
        POP     cx
        LOOP    @@L11
        POP     di
        POP     si
        POP     cx
        POP     ax
        ADD     si, [BPS]
        JMP     @@L9
@@L10:
        POP     di
        POP     si
        POP     bx
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L7
@@L8    EQU     @@L12
@@L6:
@@L13:
        OR      bx, bx
        JBE     @@L14
        MOV     ax, 0802h
        PUSH    bx
        PUSH    si
        PUSH    di
        MOV     bl, [ShiftScr]
        SUB     bl, [ShiftBuf]
@@L15:
        OR      ah, ah
        JBE     @@L16
        out     dx, ax
        SHR     ah, 1
        PUSH    ax
        PUSH    cx
        PUSH    si
        PUSH    di
@@L17:
        PUSH    cx
        MOV     cl, bl
        MOV     ax, [ds:si]
        xchg    al, ah
        SHR     ax, cl
        MOV     ah, [es:di]
        MOV     [es:di], al
        INC     si
        INC     di
        POP     cx
        LOOP    @@L17
        POP     di
        POP     si
        POP     cx
        POP     ax
        ADD     si, [BPS]
        JMP     @@L15
@@L16:
        POP     di
        POP     si
        POP     bx
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L13
@@L14:
@@L12:
        POP     si
PPI_OutLeft:
        MOV     di, [OffsLeft]
        MOV     dx, 3CEh
        MOV     al, 8
        MOV     ah, [MaskLeft]
        out     dx, ax
        MOV     dx, 3C4h
        MOV     ax, 0802h
        PUSH    cx
        PUSH    si
        MOV     cl, [ShiftBuf]
        MOV     ch, [ShiftScr]
@@L18:
        OR      ah, ah
        JBE     @@L19
        MOV     bx, [yLen]
        out     dx, ax
        SHR     ah, 1
        PUSH    ax
        PUSH    si
        PUSH    di
@@L20:
        OR      bx, bx
        JBE     @@L21
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        xchg    ch, cl
        SHR     ax, cl
        xchg    ch, cl
        MOV     al, [es:di]
        MOV     [es:di], ah
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L20
@@L21:
        POP     di
        POP     si
        POP     ax
        ADD     si, [BPS]
        JMP     @@L18
@@L19:
        POP     si
        POP     cx
PPI_Right:
        MOV     di, [OffsRight]
        ADD     si, cx
        MOV     ah, [MaskRight]
PPI_AloneByte_Number2:
        MOV     dx, 3CEh
        MOV     al, 8
        out     dx, ax
        MOV     dx, 3C4h
        MOV     ax, 0802h
        MOV     cl, [ShiftBuf]
        MOV     ch, [ShiftScr]
        CMP     cl, ch
        JBE     @@L22
        SUB     cl, ch
        INC     si
@@L23:
        OR      ah, ah
        JBE     @@L24
        MOV     bx, [yLen]
        out     dx, ax
        SHR     ah, 1
        PUSH    ax
        PUSH    si
        PUSH    di
@@L25:
        OR      bx, bx
        JBE     @@L26
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        MOV     al, [es:di]
        MOV     [es:di], ah
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L25
@@L26:
        POP     di
        POP     si
        POP     ax
        ADD     si, [BPS]
        JMP     @@L23
@@L24   EQU     @@L27
@@L22:
        SUB     cl, ch
        neg     cl
@@L28:
        OR      ah, ah
        JBE     @@L29
        MOV     bx, [yLen]
        out     dx, ax
        SHR     ah, 1
        PUSH    ax
        PUSH    si
        PUSH    di
@@L30:
        OR      bx, bx
        JBE     @@L31
        MOV     ax, [ds:si]
        xchg    al, ah
        SHR     ax, cl
        MOV     ah, [es:di]
        MOV     [es:di], al
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L30
@@L31:
        POP     di
        POP     si
        POP     ax
        ADD     si, [BPS]
        JMP     @@L28
@@L29:
@@L27:
        JMP     PPI_Exit
PPI_AloneByte:
        MOV     ah, [MaskRight]
        AND     ah, [MaskLeft]
        DEC     si
        JMP     PPI_AloneByte_Number2
PPI_ByteAligned:
        MOV     cl, [byte xLen]
        DEC     cl
        AND     cl, 7
        XOR     cl, 7
        MOV     al, 0FFh
        SHL     al, cl
        MOV     [MaskRight], al
        MOV     bx, [xLen]
        ADD     bx, 7
        SHR     bx, 3
        CMP     [MaskRight], 0FFh
        JE      @@L32
        DEC     bx
@@L32:
        MOV     dx, 03C4h
        OR      bx, bx
        JE      PI_BA_RightPart
        PUSH    [yLen]
        PUSH    si
        PUSH    di
@@L33:
        CMP     [yLen], 0
        JBE     @@L34
        PUSH    si
        MOV     ax, 0000100000000010B
@@L35:
        OR      ah, ah
        JE      @@L36
        out     dx, ax
        SHR     ah, 1
        PUSH    si
        PUSH    di
        PUSH    ax
        MOV     cx, bx
@@L37:
        lodsb
        MOV     ah, [es:di]
        stosb
        LOOP    @@L37
        POP     ax
        POP     di
        POP     si
        ADD     si, [BPS]
        JMP     @@L35
@@L36:
        POP     si
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        DEC     [yLen]
        JMP     @@L33
@@L34:
        POP     di
        POP     si
        POP     [yLen]
        CMP     [MaskRight], 0FFh
        JE      PPI_Exit
PI_BA_RightPart:
        MOV     dx, 03CEh
        MOV     ah, [MaskRight]
        MOV     al, 8
        out     dx, ax
        ADD     si, bx
        ADD     di, bx
        MOV     dx, 03C4h
        MOV     ax, 0000100000000010B
        MOV     bx, [yLen]
@@L38:
        OR      ah, ah
        JE      @@L39
        out     dx, ax
        SHR     ah, 1
        MOV     cx, bx
        PUSH    si
        PUSH    di
        PUSH    ax
@@L40:
        MOV     al, [ds:si]
        MOV     ah, [es:di]
        stosb
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        dec     di
        LOOP    @@L40
        POP     ax
        POP     di
        POP     si
        ADD     si, [BPS]
        JMP     @@L38
@@L39:
PPI_Exit:
        MOV     dx, 3CEh
        MOV     ax, 0FF08h
        out     dx, ax
        MOV     ax, 3
        out     dx, ax
        MOV     dl, 0C4h
        MOV     ax, 00F02h
        out     dx, ax
PPI_Exit1:
        POP     ds
        RET
ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectMousePut ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC DirectMousePut Far Buf:DWORD, xxx, yyy, xFrom, yFrom, xLen, yLen
LOCAL BytesPerRow, ShiftBuf:BYTE, ShiftScr:BYTE, MaskLeft:BYTE, MaskRight:BYTE, OffsLeft, OffsRight, WIB, BPS, AddValue
LOCALS
        cld
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        PUSH    ds
        mov     [AddValue], 128   ; Временно константа
        LDS     si, [Buf]
        cld
        lodsw
        MOV     bx, ax
        CMP     ax, [yFrom]
        JB      @@PPI_Exit1
        CMP     [yLen], 0
        JNE     @@L1
        MOV     [yLen], ax
@@L1:
        SUB     ax, [yFrom]
        CMP     ax, [yLen]
        JAE     @@L2
        MOV     [yLen], ax
@@L2:
        lodsw
        MOV     [WIB], ax
        mul     bx
        MOV     [BPS], ax
        lodsw
        CMP     ax, [xFrom]
        JB      @@PPI_Exit1
        CMP     [xLen], 0
        JNE     @@L3
        MOV     [xLen], ax
@@L3:
        SUB     ax, [xFrom]
        CMP     ax, [xLen]
        JAE     @@L4
        MOV     [xLen], ax
@@L4:
        MOV     ax, [xFrom]
        SHR     ax, 3
        ADD     si, ax
        MOV     ax, [yFrom]
        MUL     [WIB]
        ADD     si, ax
        MOV     ax, [yyy]
        MOV     bx, [xxx]
        pop     ds
        CALL    PixelAddr10
        MOV     [OffsLeft], bx
        MOV     di, bx
        MOV     ax, [yyy]
        MOV     bx, [xxx]
        ADD     bx, [xLen]
        DEC     bx
        CALL    PixelAddr10
        push    ds
        mov     ds, [word Buf+2]
        MOV     [OffsRight], bx
        MOV     ch, [byte xFrom]
        AND     ch, 7
        MOV     [ShiftBuf], ch
        MOV     cl, [byte xxx]
        AND     cl, 7
        MOV     [ShiftScr], cl
        OR      cx, cx
        MOV     dx, 03CEh       ; !!!!!!
        JE      @@PPI_ByteAligned
        MOV     al, 0FFh
        SHR     al, cl
        MOV     [MaskLeft], al
        MOV     cl, [byte xxx]
        ADD     cl, [byte xLen]
        AND     cl, 7
        MOV     al, 0FFh
        SHR     al, cl
        not     al
        OR      al, al
        JNE     @@L5
        MOV     al, 0FFh
@@L5:
        MOV     [MaskRight], al
        MOV     cx, bx
        SUB     cx, di
        jz      @@PPI_AloneByte
        DEC     cx
        jz      @@PPI_OutLeft
@@PPI_Middle:
        MOV     bx, [yLen]
        PUSH    si
        INC     di
        MOV     al, [ShiftBuf]
        MOV     ah, [ShiftScr]
        CMP     al, ah
        JBE     @@L6
        INC     si
@@L7:
        OR      bx, bx
        JBE     @@L8
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;
        PUSH    bx
        PUSH    si
        PUSH    di
        MOV     bl, [ShiftBuf]
        SUB     bl, [ShiftScr]
        push    cx
@@L11:
        PUSH    cx
        MOV     cl, bl
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
;        or      [es:di], ah
        MOV     al, [es:di]
        MOV     [es:di], ah
        INC     si
        INC     di
        POP     cx
        LOOP    @@L11
        POP     cx
        POP     di
        POP     si
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;
        PUSH    si
        PUSH    di
        push    cx
        add     si, [AddValue]
@@L11_1:
        PUSH    cx
        MOV     cl, bl
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        MOV     al, [es:di]
        MOV     [es:di], ah
;        or      [es:di], ah
        INC     si
        INC     di
        POP     cx
        LOOP    @@L11_1

        pop     cx
        POP     di
        POP     si
        POP     bx
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L7
@@L8    EQU     @@L12
@@L6:
@@L13:
        OR      bx, bx
        JBE     @@L14
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;
        PUSH    bx
        PUSH    si
        PUSH    di
        MOV     bl, [ShiftScr]
        SUB     bl, [ShiftBuf]
        PUSH    cx
@@L17:
        PUSH    cx
        MOV     cl, bl
        MOV     ax, [ds:si]
        xchg    al, ah
        SHR     ax, cl
        MOV     ah, [es:di]
        MOV     [es:di], al
;        or      [es:di], al
        INC     si
        INC     di
        POP     cx
        LOOP    @@L17
        POP     cx
        POP     di
        POP     si
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;
        PUSH    si
        PUSH    di
        push    cx
        add     si, [AddValue]
@@L17_1:
        PUSH    cx
        MOV     cl, bl
        MOV     ax, [ds:si]
        xchg    al, ah
        SHR     ax, cl
        MOV     ah, [es:di]
        MOV     [es:di], al
;        or      [es:di], al
        INC     si
        INC     di
        POP     cx
        LOOP    @@L17_1
        POP     cx
;
        POP     di
        POP     si
        POP     bx
        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L13
@@L14:
@@L12:
        POP     si
@@PPI_OutLeft:
        MOV     di, [OffsLeft]
        MOV     al, 8
        MOV     ah, [MaskLeft]
        out     dx, ax
        PUSH    cx
        MOV     cl, [ShiftBuf]
        MOV     ch, [ShiftScr]
        MOV     bx, [yLen]
        PUSH    si
        PUSH    di
@@L20:
        OR      bx, bx
        JBE     @@L21
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;

        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        xchg    ch, cl
        SHR     ax, cl
        xchg    ch, cl
        MOV     al, [es:di]
        MOV     [es:di], ah

        add     si, [AddValue]
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;

        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        xchg    ch, cl
        SHR     ax, cl
        xchg    ch, cl
        MOV     al, [es:di]
        MOV     [es:di], ah

        sub     si, [AddValue]

        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L20
@@L21:
        POP     di
        POP     si
        POP     cx
;;;;;;;;;;;-----------
@@PPI_Right:
        MOV     di, [OffsRight]
        ADD     si, cx
        MOV     ah, [MaskRight]
@@PPI_AloneByte_Number2:
        MOV     al, 8
        out     dx, ax

        MOV     cl, [ShiftBuf]
        MOV     ch, [ShiftScr]
        CMP     cl, ch
        JBE     @@L22
        SUB     cl, ch
        INC     si
        MOV     bx, [yLen]
        PUSH    si
        PUSH    di
@@L25:
        OR      bx, bx
        JBE     @@L26
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        MOV     al, [es:di]
        MOV     [es:di], ah

        add     si, [AddValue]
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;
        MOV     ax, [ds:si]
        xchg    al, ah
        SHL     ax, cl
        MOV     al, [es:di]
        MOV     [es:di], ah

        sub     si, [AddValue]

        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L25
@@L26:
        POP     di
        POP     si
        jmp     @@PPI_Exit
@@L24   EQU     @@L27
@@L22:
        SUB     cl, ch
        neg     cl
        MOV     bx, [yLen]
        PUSH    si
        PUSH    di
@@L30:
        OR      bx, bx
        JBE     @@L31
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;
        MOV     ax, [ds:si]
        xchg    al, ah
        SHR     ax, cl
        MOV     ah, [es:di]
        MOV     [es:di], al

        add     si, [AddValue]
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;
        MOV     ax, [ds:si]
        xchg    al, ah
        SHR     ax, cl
        MOV     ah, [es:di]
        MOV     [es:di], al

        sub     si, [AddValue]

        DEC     bx
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        JMP     @@L30
@@L31:
        POP     di
        POP     si
@@L29:
@@L27:
        JMP     @@PPI_Exit
@@PPI_AloneByte:
        MOV     ah, [MaskRight]
        AND     ah, [MaskLeft]
        DEC     si
        JMP     @@PPI_AloneByte_Number2
@@PPI_ByteAligned:
        MOV     cl, [byte xLen]
        DEC     cl
        AND     cl, 7
        XOR     cl, 7
        MOV     al, 0FFh
        SHL     al, cl
        MOV     [MaskRight], al
        MOV     bx, [xLen]
        ADD     bx, 7
        SHR     bx, 3
        CMP     [MaskRight], 0FFh
        JE      @@L32
        DEC     bx
@@L32:
        OR      bx, bx
        JE      @@PI_BA_RightPart
        PUSH    [yLen]
        PUSH    si
        PUSH    di
@@L33:
        CMP     [yLen], 0
        JBE     @@L34
        PUSH    si
        PUSH    di
        MOV     cx, bx
;;;
        push    si
        push    di
        push    cx
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;
@@L37:
        lodsb
        MOV     ah, [es:di]
        stosb
        LOOP    @@L37
        pop     cx
        pop     di
        pop     si
        add     si, [AddValue]
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;
@@L37_1:
        lodsb
        MOV     ah, [es:di]
        stosb
        LOOP    @@L37_1
;;;
        POP     di
        POP     si
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        DEC     [yLen]
        JMP     @@L33
@@L34:
        POP     di
        POP     si
        POP     [yLen]
        CMP     [MaskRight], 0FFh
        JE      @@PPI_Exit
@@PI_BA_RightPart:
        MOV     ah, [MaskRight]
        MOV     al, 8
        out     dx, ax
        ADD     si, bx
        ADD     di, bx
        MOV     cx, [yLen]
@@L40:
;
        MOV     ah, (1 SHL 3)    ; AND Mask
        MOV     al, 3
        out     dx, ax
;
        MOV     al, [ds:si]
        MOV     ah, [es:di]
        stosb
        add     si, [AddValue]
        dec     di
;
        MOV     ah, (3 SHL 3)    ; XOR Mask
        MOV     al, 3
        out     dx, ax
;
        MOV     al, [ds:si]
        MOV     ah, [es:di]
        stosb
;
        sub     si, [AddValue]
        ADD     si, [WIB]
        ADD     di, [BytesPerRow]
        dec     di
        LOOP    @@L40
@@L39:
@@PPI_Exit:
@@PPI_Exit1:
        mov     dx, 3CEh
        MOV     ax, 0FF08h
        out     dx, ax
        MOV     ax, 3
        out     dx, ax
        POP     ds
        RET
ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QuickSave ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC QuickSave Far xxx, yyy, xLen, yLen
        LOCAL   BytesPerRow
        LOCALS
        cld
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        mov     dx, 3CEh
        mov     ax, 0105h   ; Read mode 0, write mode 1
        out     dx, ax
        mov     ax, [yyy]
        mov     bx, [xxx]
        call    PixelAddr10
        mov     si, bx

        mov     di, [BitPlaneSizeW]
        shl     di, 1                   ; начало заэкранной области
        push    ds
        push    es
        pop     ds
        mov     ax, [xxx]
        add     ax, 7
        mov     cx, ax
        add     cx, [xLen]
        shr     cx, 3
        shr     ax, 3
        sub     cx, ax     ; must be 4 or 5
        inc     cx
;        or      cx, cx
;        jge     @@1
;        mov     cx, 1
@@1:
        cmp     [yLen], 0
        jz      @@2
        dec     [yLen]
        push    cx
        push    si
        rep     movsb
        pop     si
        pop     cx
        add     si, [BytesPerRow]
        jmp     @@1
@@2:
        pop     ds
        mov     ax, 0005h   ; Read mode 0, write mode 0
        out     dx, ax
        ret
ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QuickRestore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC QuickRestore Far xxx, yyy, xLen, yLen
        LOCAL   BytesPerRow
        LOCALS
        cld
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        mov     dx, 3CEh
        mov     ax, 0105h   ; Read mode 0, write mode 1
        out     dx, ax
        mov     ax, [yyy]
        mov     bx, [xxx]
        call    PixelAddr10
        mov     di, bx
        mov     si, [BitPlaneSizeW]
        shl     si, 1                   ; начало заэкранной области
        push    ds
        push    es
        pop     ds
        mov     ax, [xxx]
        add     ax, 7
        mov     cx, ax
        add     cx, [xLen]
        shr     cx, 3
        shr     ax, 3
        sub     cx, ax     ; must be 4 or 5
        inc     cx
;        or      cx, cx
;        jge     @@1
;        mov     cx, 1
@@1:
        cmp     [yLen], 0
        jz      @@2
        dec     [yLen]
        push    cx
        push    di
        rep     movsb
        pop     di
        pop     cx
        add     di, [BytesPerRow]
        jmp     @@1
@@2:
        pop     ds
        mov     ax, 0005h   ; Read mode 0, write mode 0
        out     dx, ax
        ret
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DirectLine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC DirectLine Far ARGx1, ARGy1, ARGx2, ARGy2, __Dummy, ARGn: Byte
ByteOffsetShift EQU     3       ; used to convert pixels to byte offset
    LOCAL   VARvertincr, VARincr1, VARincr2, VARroutine
    LOCALS
; configure the Graphics Controller
    cld
    mov     dx, 3CEh         ; DX := Graphics Controller port addr
    mov     ah, [ARGn]       ; AH := pixel value
    xor     al, al           ; AL := Set/Reset Register number
    out     dx, ax
    mov     ax, 0F01h        ; AH := 1111b (bit plane mask for
                             ;  Enable Set/Reset
    out     dx, ax           ; AL := Enable Set/Reset Register #

    mov     ah, [RMWbits]    ; bits 3 and 4 of AH := function
    mov     al,3             ; AL := Data Rotate/Func Select reg #
    out     dx,ax

; check for vertical line

    mov     si, [LineLength] ; increment for video buffer

    mov     cx, [ARGx2]
    sub     cx, [ARGx1]      ; CX := x2 - x1
    jz      @@VertLine10     ; jump if vertical line

; force x1 < x2

    jns     @@L01            ; jump if x2 > x1

    neg     cx               ; CX := x1 - x2

    mov     bx, [ARGx2]      ; exchange x1 and x2
    xchg    bx, [ARGx1]
    mov     [ARGx2], bx

    mov     bx, [ARGy2]      ; exchange y1 and y2
    xchg    bx, [ARGy1]
    mov     [ARGy2], bx

; calculate dy = ABS(y2-y1)

@@L01:
    mov     bx, [ARGy2]
    sub     bx, [ARGy1]      ; BX := y2 - y1
    jz      @@HorizLine10    ; jump if horizontal line

    jns     @@L03            ; jump if slope is positive

    neg     bx               ; BX := y1 - y2
    neg     si               ; negate increment for buffer interleave

; select appropriate routine for slope of line

@@L03:
    mov     [VARvertincr], si ; save vertical increment

    mov     [VARroutine], offset @@LoSlopeLine10
    cmp     bx, cx
    jle     @@L04            ; jump if dy <= dx (slope <= 1)
    mov     [VARroutine], offset @@HiSlopeLine10
    xchg    bx, cx           ; exchange dy and dx

; calculate initial decision variable and increments

@@L04:
    shl     bx, 1            ; BX := 2 * dy
    mov     [VARincr1], bx   ; incr1 := 2 * dy
    sub     bx, cx
    mov     si, bx           ; SI := d = 2 * dy - dx
    sub     bx, cx
    mov     [VARincr2], bx   ; incr2 := 2 * (dy - dx)

; calculate first pixel address

    push    cx               ; preserve this register
    mov     ax, [ARGy1]      ; AX := y
    mov     bx, [ARGx1]      ; BX := x
    call    PixelAddr10      ; AH := bit mask
                             ; ES:BX -> buffer
                             ; CL := # bits to shift left

    mov     di, bx           ; ES:DI -> buffer
    shl     ah, cl           ; AH := bit mask in proper position
    mov     bl, ah           ; AH,BL := bit mask
    mov     al, 8            ; AL := Bit Mask Register number

    pop     cx               ; restore this register
    inc     cx               ; CX := # of pixels to draw

    jmp     [VARroutine]     ; jump to appropriate routine for slope

; routine for vertical lines

@@VertLine10:
    mov     ax, [ARGy1]      ; AX := y1
    mov     bx, [ARGy2]      ; BX := y2
    mov     cx, bx
    sub     cx, ax           ; CX := dy
    jge     @@L31            ; jump if dy >= 0

    neg     cx               ; force dy >= 0
    mov     ax, bx           ; AX := y2

@@L31:
    inc     cx               ; CX := # of pixels to draw
    mov     bx, [ARGx1]      ; BX := x
    push    cx               ; preserve this register
    call    PixelAddr10      ; AH := bit mask
                             ; ES:BX -> video buffer
                             ; CL := # bits to shift left
; set up Graphics Controller

    shl     ah, cl           ; AH := bit mask in proper position
    mov     al, 8            ; AL := Bit Mask reg number
    out     dx, ax

    pop     cx               ; restore this register

; draw the line

@@L32:
    or      [es:bx], al     ; set pixel
    add     bx, si          ; increment to next line
    loop    @@L32

    jmp     @@Lexit

; routine for horizontal lines (slope = 0)

@@HorizLine10:
    push    ds              ; preserve DS
    mov     ax, [ARGy1]
    mov     bx, [ARGx1]
    call    PixelAddr10     ; AH := bit mask
                            ; ES:BX -> video buffer
                            ; CL := # bits to shift left
    mov     di, bx          ; ES:DI -> buffer

    mov     dh, ah          ; DH := unshifted bit mask for leftmost
                            ;        byte
    not     dh
    shl     dh, cl          ; DH := reverse bit mask for first byte
    not     dh              ; DH := bit mask for first byte

    mov     cx, [ARGx2]
    and     cl, 7
    xor     cl, 7           ; CL := number of bits to shift left
    mov     dl, 0FFh        ; DL := unshifted bit mask for
                            ;        rightmost byte
    shl     dl, cl          ; DL := bit mask for last byte

; determine byte offset of first and last pixel in the line

    mov     ax, [ARGx2]     ; AX := x2
    mov     bx, [ARGx1]     ; BX := x1

    mov     cl, ByteOffsetShift    ; number of bits to shift to
                                   ;  convert pixels to bytes

    shr     ax, cl          ; AX := byte offset of x2
    shr     bx, cl          ; BX := byte offset of x1
    mov     cx, ax
    sub     cx, bx          ; CX := (# bytes in line) - 1

; get Graphics Controller port address into DX

    mov     bx, dx          ; BH := bit mask for first byte
                            ; BL := bit mask for last byte
    mov     dx, 3CEh        ; DX := Graphics Controller port
    mov     al, 8           ; AL := Bit Mask Register number

; make video buffer addressible through DS:SI

    push    es
    pop     ds
    mov     si, di          ; DS:SI -> video buffer

; set pixels in leftmost byte of the line

    or      bh, bh
    js      @@L43           ; jump if byte-aligned (x1 is leftmost
                            ;  pixel in byte)
    or      cx, cx
    jnz     @@L42           ; jump if more than one byte in the line

    and     bl, bh          ; BL := bit mask for the line
    jmp     @@L44

@@L42:
    mov     ah, bh          ; AH := bit mask for 1st byte
    out     dx, ax          ; update Graphics Controller

    movsb                   ; update bit planes
    dec     cx

; use a fast 8086 machine instruction to draw the remainder of the line

@@L43:
    mov     ah, 11111111b   ; AH := bit mask
    out     dx, ax          ; update Bit Mask Register

    rep     movsb           ; update all pixels in the line

; set pixels in the rightmost byte of the line

@@L44:
    mov     ah, bl          ; AH := bit mask for last byte
    out     dx, ax          ; update Graphics Controller

    movsb                   ; update bit planes
    pop     ds              ; restore DS
    jmp     @@Lexit


; routine for dy <= dx (slope <= 1)     ; ES:DI -> video buffer
                            ; AL = Bit Mask Register number
                            ; BL = bit mask for 1st pixel
                            ; CX = #pixels to draw
                            ; DX = Graphics Controller port addr
                            ; SI = decision variable
@@LoSlopeLine10:
@@L10:
    mov     ah, bl          ; AH := bit mask for next pixel

@@L11:
    or      ah, bl          ; mask current pixel position
    ror     bl, 1           ; rotate pixel value
    jc      @@L14           ; jump if bit mask rotated to
                            ;  leftmost pixel position

; bit mask not shifted out

    or      si, si          ; test sign of d
    jns     @@L12           ; jump if d >= 0

    add     si, [VARincr1]  ; d := d + incr1
    loop    @@L11

    out     dx, ax          ; update Bit Mask Register
    or      [es:di], al     ; set remaining pixel(s)
    jmp     @@Lexit

@@L12:
    add     si, [VARincr2]  ; d := d + incr2
    out     dx, ax          ; update Bit Mask Register

    or      [es:di], al     ; update bit planes

    add     di, [VARvertincr] ; increment y
    loop    @@L10
    jmp     @@Lexit

; bit mask shifted out

@@L14:
    out     dx, ax          ; update Bit Mask Register ...

    or      [es:di], al     ; update bit planes
    inc     di              ; increment x

    or      si, si          ; test sign of d
    jns     @@L15           ; jump if non-negative

    add     si, [VARincr1]  ; d := d + incr1
    loop    @@L10
    jmp     @@Lexit

@@L15:
    add     si, [VARincr2]  ; d := d + incr2
    add     di, [VARvertincr]  ; vertical increment
    loop    @@L10
    jmp     @@Lexit

; routine for dy > dx (slope > 1)       ; ES:DI -> video buffer
                            ; AH = bit mask for 1st pixel
                            ; AL = Bit Mask Register number
                            ; CX = #pixels to draw
                            ; DX = Graphics Controller port addr
                            ; SI = decision variable
@@HiSlopeLine10:
    mov     bx, [VARvertincr]  ; BX := y-increment
@@L21:
    out     dx, ax          ; update Bit Mask Register
    or      [es:di], al     ; update bit planes

    add     di, bx          ; increment y

@@L22:
    or      si, si          ; test sign of d
    jns     @@L23           ; jump if d >= 0

    add     si, [VARincr1]  ; d := d + incr1
    loop    @@L21
    jmp     @@Lexit

@@L23:
    add     si, [VARincr2]  ; d := d + incr2

    ror     ah, 1           ; rotate bit mask
    adc     di, 0           ; increment DI if when mask rotated to
                            ;  leftmost pixel position
    loop    @@L21

; restore default Graphics Controller state and return to caller

@@Lexit:
    xor     ax, ax          ; AH := 0, AL := 0
    out     dx, ax          ; restore Set/Reset Register

    inc     ax              ; AH := 0, AL := 1
    out     dx, ax          ; restore Enable Set/Reset Register

    mov     al, 3           ; AH := 0, AL := 3
    out     dx, ax          ; AL := Data Rotate/Func Select reg #

    mov     ax, 0FF08h      ; AH := 1111111b, AL := 8
    out     dx, ax          ; restore Bit Mask Register

    ret
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;; PixelAddrBuf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC    PixelAddrBuf    NEAR
        LOCALS
        mul     [LineLength]
        mov     cl, bl          ; CL = low-order byte of x
        shr     bx, 3           ; BX = x/8
        add     bx, ax          ; BX = y*BytesPerLine + x/8
        and     cl, 7           ; CL = x & 7
        xor     cl, 7           ; CL = number of bits to shift left
        mov     ah, 1           ; AH = unshifted bit mask
        ret
ENDP

; Временные переменные (инициализируется в Line, используются в
;       LoSlopeLine и в HiSlopeLine
VertIncr        DW      ?
Incr1           DW      ?
Incr2           DW      ?

; Подпроцедурка для малого угла наклона
PROC    LoSlopeLine     NEAR
        LOCALS
@@10:
        mov     ah, bl          ; AH <- маска для следующего пиксела
@@11:
        or      ah, bl          ; Маскируем текущую позицию
        ror     bl, 1           ; Вращаем значение пиксела
        jc      @@14            ; Переход, если довращали до левейшей позиции
; Битовая маска еще не вылезла, не надо менять адрес
        or      si, si          ; Определяем знак D
        jns     @@12            ; Если D >= 0, переходим
        add     si, [Incr1]     ; D := D + Incr1
        loop    @@11

        or      al, al          ; Чистим или ставим ?
        jz      @@Clear1
@@Set1:
        or      [es:di], ah     ; Ставим !
        jmp     @@Exit
@@Clear1:
        not     ah
        and     [es:di], ah     ; Чистим !
        jmp     @@Exit
@@12:
        add     si, [Incr2]     ; d := d + incr2
        or      al, al          ; Чистим или ставим ?
        jz      @@Clear2
@@Set2:
        or      [es:di], ah     ; Ставим !
        jmp     @@22
@@Clear2:
        mov     bh, ah
        not     bh
        and     [es:di], bh     ; Чистим !
@@22:
        add     di, [VertIncr]  ; increment y
        loop    @@10
        jmp     @@Exit
; bit mask shifted out
@@14:
        or      al, al          ; Чистим или ставим ?
        jz      @@Clear3
@@Set3:
        or      [es:di], ah     ; Ставим !
        jmp     @@33
@@Clear3:
        mov     bh, ah
        not     bh
        and     [es:di], bh     ; Чистим !
@@33:
        inc     di              ; increment x

        or      si, si          ; test sign of d
        jns     @@15            ; jump if non-negative

        add     si, [Incr1]     ; d := d + incr1
        loop    @@10
        jmp     @@Exit
@@15:
        add     si, [Incr2]     ; d := d + incr2
        add     di, [VertIncr]  ; vertical increment
        loop    @@10
@@Exit:
        ret
ENDP


PROC    HiSlopeLine     NEAR
        LOCALS
@@21:
        or      al, al          ; Чистим или ставим ?
        jz      @@Clear1
@@Set1:
        or      [es:di], ah     ; Ставим !
        jmp     @@1111
@@Clear1:
        mov     bh, ah
        not     bh
        and     [es:di], bh     ; Чистим !
@@1111:

        add     di, [VertIncr]  ; increment y
@@22:
        or      si, si          ; test sign of d
        jns     @@23            ; jump if d >= 0

        add     si, [Incr1]     ; d := d + incr1
        loop    @@21
        jmp     @@Exit
@@23:
        add     si, [Incr2]     ; d := d + incr2
        ror     ah, 1           ; rotate bit mask
        adc     di, 0           ; increment DI if when mask rotated to
                                ;  leftmost pixel position
        loop    @@21
@@Exit:
        ret
ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROC    Line    Far     X1, Y1, X2, Y2, __Dummy, Color: Byte
        LOCAL   Routine, BufOffs, ClrMask:BYTE
        LOCALS
        cld
; Проверка - может, линия вертикальная ?
        mov     si, [LineLength]; Размер строки буфера в байтах
        mov     cx, [X2]
        sub     cx, [X1]        ; cx <- x-размер линии (deltaX)
        jz      @@VertLine      ; если 0, то наша линия вертикальна
; Обменяем координаты (при необходимости), так что X1 < X2
        jns     @@1             ; Уже так и есть - не нужно менять
        neg     cx              ; deltaX должен быть >0
        mov     bx, [X2]        ; Обмен иксов
        xchg    bx, [X1]
        mov     [X2], bx
        mov     bx, [Y2]        ; Обмен игреков
        xchg    bx, [Y1]
        mov     [Y2], bx

; Расчет deltaY = ABS(y2-y1)
@@1:
        mov     bx, [Y2]
        sub     bx, [Y1]        ; BX := y2 - y1
        jz      @@HorizLine     ; Сразу проверяем - может, линия горизонтальная
        jns     @@3             ; Переход, если наклон линии положителен
        neg     bx              ; Делаем deltaY положительным
        neg     si              ; Делаем приращение по строкам отрицательным
@@3:
; Выбор подходящей процедуры для разных наклонов линии
        mov     [VertIncr], si  ; Сохраняем построчное приращение в переменной
        mov     [Routine], offset LoSlopeLine
        cmp     bx, cx
        jle     @@4             ; Переход, если малый наклон (deltaY <= deltaX)
        mov     [Routine], offset HiSlopeLine
        xchg    bx, cx          ; Обмен deltaX и deltaY
@@4:
; Считаем начальные значания переменных и приращений
        shl     bx, 1           ; bx <- deltaY * 2
        mov     [Incr1], bx     ; Incr1 <- deltaY * 2
        sub     bx, cx
        mov     si, bx          ; si <- d == 2 * dy - dx
        sub     bx, cx
        mov     [Incr2], bx     ; Incr2 <- 2 * (deltaY - deltaX)
; Расчет адреса первого пикселя
        push    cx              ; Сохраним, еще пригодится
        mov     ax, [Y1]
        mov     bx, [X1]
        call    PixelAddrBuf    ; AH <- 1, BX <- buffer offset
                                ; CL <- # bits to shift left, DX unknown
        mov     [BufOffs], bx
        shl     ah, cl          ; AH <- bit mask in proper position
        mov     bl, ah          ; AH,BL <- bit mask
        pop     cx              ; Можно восстанавливать
;
        inc     cx              ; CX <- Число пикселов, которые рисовать
; Крутим по отдельности для каждой битовой плоскости
        xor     di, di          ; BX <- смещение на адрес
                                ; текущей битовой плоскости
        mov     [ClrMask], 8    ; Маска плоскости (используется
                                ; в качестве счетчика
@@PlanesLoop:
        mov     al, [ClrMask]
        or      al, al
        jz      @@Exit
        test    [Color], al
        mov     al, 0
        jz      @@111
        not     al
@@111:                          ; Здесь al=00, если нужно Clear, и FF, если Set
        push    di
        push    bx
        push    si
        push    ax
        push    cx
        les     di, [BitPlanes+di]
        add     di, [BufOffs]
        call    [Routine]       ; А теперь - дискотека...
        pop     cx
        pop     ax
        pop     si
        pop     bx
        pop     di
        add     di, 4
        shr     [ClrMask], 1
        jmp     @@PlanesLoop
;
@@VertLine:
; Чтоб не мучаться, вызываем фирменную
        push    [X1]
        push    [Y1]
        push    [Y2]
        push    [word Color]
        call    VLine
        jmp     @@Exit
;
@@HorizLine:
; Чтоб не мучаться, вызываем фирменную
        push    [X1]
        push    [Y1]
        push    [X2]
        push    [word Color]
        call    HLine
        jmp     @@Exit

; restore default Graphics Controller state and return to caller

@@Exit:
        ret
ENDP


;;--------------------- ScrollY --------------------------------
;;
;;  Beginning date: 02-JUN-1994
;;  Ending date: 02-JUN-1994
;;  Upgrades:
;;  Caller: Borland Pascal 7.0
;;          procedure ScrollY(x1, y1, x2, y2, How: integer);
;;
PROC    ScrollY FAR     X1, Y1, X2, Y2, How
        LOCAL   SubY, Count, BufOffs, Difference, LocalLineLength, ClrMask: BYTE
        LOCALS
        cld
        mov     ax, [Y2]
        sub     ax, [Y1]
        mov     [SubY], ax
        cmp     [How], 0
        jz      @@Exit
        jl      @@Up2Down
@@Down2Up:
        sub     ax, [How]
        jle     @@Exit
        mov     [Count], ax     ; Число пересылаемых строк
        mov     ax, [LineLength]
        mov     [LocalLineLength], ax
        mul     [How]
        mov     [Difference], ax
;
        mov     ax, [Y1]
        mov     bx, [X1]
        call    PixelAddrBuf    ; AH <- 1, BX <- buffer offset
                                ; CL <- # bits to shift left, DX unknown
;
@@EverybodyJumpsAtThisPoint:
        mov     [BufOffs], bx
        mov     dh, ah
        not     dh              ; DH := unshifted bit mask for leftmost
                                ;       byte
        mov     dl, 0FFh        ; DL := unshifted bit mask for
                                ;        rightmost byte

        shl     dh,cl           ; DH := reverse bit mask for first byte
        not     dh              ; DH := bit mask for first byte

        mov     cx, [X2]
        and     cl, 7
        xor     cl, 7           ; CL := number of bits to shift left
        shl     dl, cl          ; DL := bit mask for last byte

; determine byte offset of first and last pixel in the line

        mov     ax, [X2]        ; AX := x2
        mov     bx, [X1]        ; BX := x1

        shr     ax, 3           ; AX := byte offset of x2
        shr     bx, 3           ; BX := byte offset of x1
        mov     cx, ax
        sub     cx, bx          ; CX := (# bytes in line) - 1

; Цикл по битовым плоскостям

        xor     bx, bx          ; BX <- смещение на адрес
                                ; текущей битовой плоскости
        mov     [ClrMask], 8    ; Маска плоскости (используется
                                ; в качестве счетчика
        cld

@@PlanesLoop:
        les     di, [BitPlanes+bx]
        add     di, [BufOffs]
        mov     al, [ClrMask]
        or      al, al
        jz      @@EndPlanesLoop
        shr     [ClrMask], 1    ; Маскируем для next plane
        add     bx, 4           ; Смещение на адрес следующей плоскости

        push    ds
        push    es
        pop     ds
        mov     si, di
        add     si, [Difference]

        push    bx
        push    dx
        push    cx
        mov     bx, [LocalLineLength]

        or      dh,dh
        js      @@L43           ; jump if byte-aligned (x1 is leftmost
                                ;  pixel in byte)
        or      cx,cx
        jnz     @@L42           ; jump if more than one byte in the line

        and     dl,dh           ; bit mask for the line
        jmp     SHORT @@L44

@@L42:
; левая часть скроллируется
        push    si
        push    di
        push    dx
        mov     dl, dh
        not     dl
        mov     cx, [Count]
@@3:
        mov     al, [ds:si]
        and     al, dh
        and     [es:di], dl
        or      [es:di], al
        add     si, bx
        add     di, bx
        loop    @@3
        pop     dx
        pop     di
        pop     si
        inc     si
        inc     di
        pop     cx
        push    cx
        dec     cx
; Лепим центральную часть строки
@@L43:
        push    cx

        shr     cx, 1
        mov     ax, cx
        jz      @@OneByte
        mov     cx, [Count]
        push    si
        push    di
@@2:
        push    cx
        push    si
        push    di
        mov     cx, ax
        rep     movsw
        pop     di
        pop     si
        pop     cx
        add     si, bx
        add     di, bx
        loop    @@2
        pop     di
        pop     si
@@OneByte:
        shl     ax, 1
        add     si, ax
        add     di, ax
        pop     cx
        sub     cx, ax
        jcxz    @@L44
        push    si
        push    di
        mov     cx, [Count]
@@4:
        mov     al, [ds:si]
        mov     [es:di], al
        add     si, bx
        add     di, bx
        loop    @@4
        pop     di
        pop     si
        inc     si
        inc     di

; set pixels in the rightmost byte of the line

@@L44:
; правая часть скроллируется
        mov     dh, dl
        not     dl
        mov     cx, [Count]
@@1:
        mov     al, [ds:si]
        and     al, dh
        and     [es:di], dl
        or      [es:di], al
        add     si, bx
        add     di, bx
        loop    @@1

        pop     cx
        pop     dx
        pop     bx
        pop     ds
        jmp     @@PlanesLoop

@@Up2Down:
; ax == SubY, How < 0
        add     ax, [How]
        jle     @@Exit
        mov     [Count], ax     ; Число пересылаемых строк
        mov     ax, [LineLength]
        neg     ax              ; Приращение отрицательно !
        mov     [LocalLineLength], ax
        neg     ax
        mov     bx, [How]
        neg     bx
        mul     bx
        neg     ax
        mov     [Difference], ax
;
        mov     ax, [Y2]
        dec     ax
        mov     bx, [X1]
        call    PixelAddrBuf    ; AH <- 1, BX <- buffer offset
                                ; CL <- # bits to shift left, DX unknown
        jmp     @@EverybodyJumpsAtThisPoint
@@EndPlanesLoop:
@@Exit:
        ret
ENDP


;;--------------------- ScrollY --------------------------------
;;
;;  Beginning date: 08-JUN-1994
;;  Ending date: 08-JUN-1994
;;  Upgrades:
;;  Caller: Borland Pascal 7.0
;;          procedure SetPixel(x, y: integer; Color: LongInt);
;;
PROC    SetPixel  FAR     X, Y, __dummy, Color:byte
        locals
        cld
        mov     ax, [Y]
        mov     bx, [X]
        call    PixelAddrBuf    ; ah <- 1, bx <- buffer offset
                                ; cl <- # bits to shift left, DX unknown
        shl     ah, cl          ; ah <- полноценная маска
        mov     si, bx          ; si <- смещение в буфере плоскости
        xor     bx, bx          ; bx <- смещение на адрес текущей битовой плоскости
        mov     cx, 8           ; cx <- Маска плоскости (используется в качестве счетчика)
        mov     al, ah
        not     al              ; al <- Маска наоборот

@@PlanesLoop:
        les     di, [BitPlanes+bx]
        add     di, si
        jcxz    @@EndPlanesLoop

; Для текущей плоскости, в зависимости от цвета (по маске), ставим или чистим
        test    [Color], cl
        jnz     @@Set
@@Clear:
        and     [byte es:di], al
        jmp     @@1
@@Set:
        or      [byte es:di], ah
@@1:
        shr     cl, 1           ; Маскируем для next plane
        add     bx, 4           ; Смещение на адрес следующей плоскости
        jmp     @@PlanesLoop
@@EndPlanesLoop:
        ret
endp


proc    OpXOR   near
        xor     [es:di], al
        ret
endp

proc    OpXOREdge       near
        xor     al, [es:di]             ; выполняем собсвенно операцию
        and     [es:di], dh             ; маскируем, что нельзя трогать
        and     al, dl                  ; что можно трогать
        or      [es:di], al             ; обновляем
        ret
endp

proc    OpAND   near
        and     [es:di], al
        ret
endp

proc    OpANDEdge       near
        and     al, [es:di]             ; выполняем собсвенно операцию
        and     [es:di], dh             ; маскируем, что нельзя трогать
        and     al, dl                  ; что можно трогать
        or      [es:di], al             ; обновляем
        ret
endp

proc    OpOR    near
        or      [es:di], al
        ret
endp

proc    OpOREdge      near
        or      al, [es:di]             ; выполняем собсвенно операцию
        and     [es:di], dh             ; маскируем, что нельзя трогать
        and     al, dl                  ; что можно трогать
        or      [es:di], al             ; обновляем
        ret
endp

;;------------------------------- PutBMPPartOp -------------------------
;;
;;  Beginning date: 12-JUN-1994
;;  Ending date: 12-JUN-1994
;;  Upgrades:
;;  Caller: Borland Pascal 7.0
;;          procedure PutBMPPartOp(var Buf;
;;            xxx, yyy, xFrom, yFrom, xLen, yLen: integer;
;;            Operation: Byte);
;;
;;---
PROC PutBMPPartOp FAR Buffer:DWORD, xxx, yyy, xFrom, yFrom, xLen, yLen, Oper:BYTE
LOCAL BytesPerRow,ShiftBuf:BYTE,ShiftScr:BYTE,MaskLeft:BYTE,MaskRight:BYTE,OffsLeft,OffsRight,WIB,BPS,PlCnt,OpNorm,OpEdge
        LOCALS
        mov     ax, [LineLength]
        mov     [BytesPerRow], ax
        push    ds
        cmp     [Oper], 1
        jne     @@5001
        mov     [OpNorm], offset OpXOR
        mov     [OpEdge], offset OpXOREdge
        jmp     @@5010
@@5001:
        cmp     [Oper], 2
        jne     @@5002
        mov     [OpNorm], offset OpAND
        mov     [OpEdge], offset OpANDEdge
        jmp     @@5010
@@5002:
        cmp     [Oper], 3
        jne     @@Exit
        mov     [OpNorm], offset OpOR
        mov     [OpEdge], offset OpOREdge
@@5010:
;
;--- Проверки - можно ли выводить и сколько выводить
        lds     si, [Buffer]
        cld
        lodsw                   ; Skip Image Sign (1970h)
        lodsw
        mov     bx, ax
        cmp     ax, [yFrom]
        jle     @@Exit              ;TONY (was jl)
        cmp     [yLen], 0
        jne     @@1
        mov     [yLen], ax
@@1:
        sub     ax, [yFrom]
        cmp     ax, [yLen]
        jnl     @@2
        mov     [yLen], ax
@@2:
        lodsw
        mov     [WIB], ax
        mul     bx
        mov     [BPS], ax
        lodsw
        cmp     ax, [xFrom]
        jle     @@Exit              ;TONY (was jl)
        cmp     [xLen], 0
        jne     @@3
        mov     [xLen], ax
@@3:
        sub     ax, [xFrom]
        cmp     ax, [xLen]
        jnl     @@4
        mov     [xLen], ax
@@4:
;--- Считаем начальные адреса в буфере BMP
        mov     ax, [xFrom]
        shr     ax, 3
        add     si, ax
        mov     ax, [yFrom]
        mul     [WIB]
        add     si, ax
        pop     ds              ; ds <-- Data Segment
        push    ds
;--- Считаем адреса в буфере экрана
        mov     ax, [yyy]
        mov     bx, [xxx]
        call    PixelAddr10
        mov     [OffsLeft], bx
        mov     ax, [yyy]
        mov     bx, [xxx]
        add     bx, [xLen]
        dec     bx
        call    PixelAddr10
        mov     [OffsRight], bx
;--- Считаем значения сдвигов
        mov     ch, [byte xFrom]
        mov     cl, [byte xxx]
        and     cx, 0707h
        mov     [ShiftBuf], ch
        mov     [ShiftScr], cl
;--- Если оба сдвига нулевые, можем сделать очень быстро
        jz      @@ByteAligned           ; AND установил флаги
;--- Считаем маски
        mov     ax, 0FFFFh
        shr     ah, cl
        mov     [MaskLeft], ah
        mov     cx, [xxx]
        add     cx, [xLen]
        and     cl, 7
        shr     al, cl
        not     al
        jnz     @@5
        mov     al, 0FFh
@@5:
        mov     [MaskRight], al
;--- Считаем счетчик
        mov     cx, bx                  ; OffsRight
        sub     cx, [OffsLeft]
        jz      @@AloneByte             ; SUB установил флаги
        dec     cx
        jz      @@OutLeft               ; DEC установил флаги
@@Middle:
;--- Средняя часть
        mov     dl, [ShiftBuf]
        mov     dh, [ShiftScr]
        cmp     dl, dh
        jna     @@MiddleShiftRight
@@MiddleShiftLeft:
        sub     dl, dh                  ; dl <-- на сколько двигать влево
        push    si                      ; Для последующих частей
;--- Организуем внешний цикл по плоскостям
        mov     dh, 4
        inc     si
        mov     bx, offset BitPlanes
;--- Начало внешнего цикла по плоскостям
@@BegOfLoop_1:
        or      dh, dh
        jz      @@EndOfLoop_1
        dec     dh
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        inc     di
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        push    bx                      ; Сохр. смещение на адр. вирт. буфера
        mov     bx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@BegOfLoop_2:
        or      bx, bx
        jz      @@EndOfLoop_2
        dec     bx
        push    di
        push    si
        push    cx
;
@@InternalLoop_1:
        push    cx
        mov     cl, dl                  ; На сколько сдвигать влево
        mov     ax, [ds:si]
        xchg    ah, al
        shl     ax, cl
        mov     al, ah
        call    [OpNorm]
        inc     si
        inc     di
        pop     cx
        loop    @@InternalLoop_1
;
        pop     cx
        pop     si
        add     si, [WIB]               ; Следующая строка в BitMap'е
        pop     di
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        jmp     @@BegOfLoop_2
@@EndOfLoop_2:
        pop     bx                      ; Восст. смещ. на адр. вирт. буфера
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@BegOfLoop_1
@@EndOfLoop_1:
        pop     si                      ; Самое начало в BitMap'е
        jmp     @@OutLeft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@MiddleShiftRight:
        sub     dh, dl
        mov     dl, dh                  ; dl <-- на сколько двигать вправо
        push    si                      ; Для последующих частей
;--- Организуем внешний цикл по плоскостям
        mov     dh, 4
        mov     bx, offset BitPlanes
;--- Начало внешнего цикла по плоскостям
@@BegOfLoop_3:
        or      dh, dh
        jz      @@EndOfLoop_3
        dec     dh
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        inc     di
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        push    bx                      ; Сохр. смещение на адр. вирт. буфера
        mov     bx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@BegOfLoop_4:
        or      bx, bx
        jz      @@EndOfLoop_4
        dec     bx
        push    di
        push    si
        push    cx
;
@@InternalLoop_2:
        push    cx
        mov     cl, dl                  ; На сколько сдвигать вправо
        mov     ax, [ds:si]
        xchg    ah, al
        shr     ax, cl
        call    [OpNorm]
        inc     si
        inc     di
        pop     cx
        loop    @@InternalLoop_2
;
        pop     cx
        pop     si
        add     si, [WIB]               ; Следующая строка в BitMap'е
        pop     di
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        jmp     @@BegOfLoop_4
@@EndOfLoop_4:
        pop     bx                      ; Восст. смещ. на адр. вирт. буфера
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@BegOfLoop_3
@@EndOfLoop_3:
        pop     si                      ; Самое начало в BitMap'е
;;        jmp     @@OutLeft

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@OutLeft:
        push    cx
        push    si
        mov     [PlCnt], 4              ; Счетчик плоскостей
        mov     dl, [MaskLeft]          ; 1 - Что можно трогать
        mov     dh, dl
        not     dh                      ; 1 - Что нельзя трогать
        mov     bx, offset BitPlanes
        mov     cl, [ShiftBuf]
        mov     ch, [ShiftScr]
;
@@OutLeftLoop_1:
        cmp     [PlCnt], 0              ; Внешний цикл по плоскостям
        je      @@EndOutLeft
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        push    bx                      ; Указатель плоскостей
        mov     ds, [word Buffer+2]     ; сегмент BitMap-буфера
        mov     bx, [yLen]
        push    si
@@OutLeftLoop_2:
        or      bx, bx
        jz      @@OutLeftNextPlane
;
        mov     ax, [ds:si]
        xchg    al, ah
        shl     ax, cl
        xchg    ch, cl
        shr     ax, cl
        xchg    ch, cl
        mov     al, ah
        call    [OpEdge]
;
        dec     bx
        add     si, [WIB]
        add     di, [BytesPerRow]
        jmp     @@OutLeftLoop_2
@@OutLeftNextPlane:
        pop     si
        add     si, [BPS]
        pop     bx
        pop     ds
        jmp     @@OutLeftLoop_1
@@EndOutLeft:
;
        pop     si
        pop     cx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@OutRight:
        add     si, cx
        mov     [PlCnt], 4              ; Счетчик плоскостей
        mov     dl, [MaskRight]         ; 1 - Что можно трогать
        mov     dh, dl
        not     dh                      ; 1 - Что нельзя трогать
        mov     bx, offset BitPlanes
        mov     cl, [ShiftBuf]
        mov     ch, [ShiftScr]
        cmp     cl, ch
        jna     @@OutRight_2
;
        sub     cl, ch
        inc     si
;
@@OutRightLoop_1:
        cmp     [PlCnt], 0              ; Внешний цикл по плоскостям
        je      @@EndOutRight
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsRight]         ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        push    bx                      ; Указатель плоскостей
        mov     ds, [word Buffer+2]     ; сегмент BitMap-буфера
        mov     bx, [yLen]
        push    si
@@OutRightLoop_2:
        or      bx, bx
        jz      @@OutRightNextPlane
;
        mov     ax, [ds:si]
        xchg    al, ah
        shl     ax, cl
        mov     al, ah
        call    [OpEdge]
;
        dec     bx
        add     si, [WIB]
        add     di, [BytesPerRow]
        jmp     @@OutRightLoop_2
@@OutRightNextPlane:
        pop     si
        add     si, [BPS]
        pop     bx
        pop     ds
        jmp     @@OutRightLoop_1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@OutRight_2:
        sub     cl, ch
        neg     cl
;
@@OutRightLoop_3:
        cmp     [PlCnt], 0              ; Внешний цикл по плоскостям
        je      @@EndOutRight
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsRight]         ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        push    bx                      ; Указатель плоскостей
        mov     ds, [word Buffer+2]     ; сегмент BitMap-буфера
        mov     bx, [yLen]
        push    si
@@OutRightLoop_4:
        or      bx, bx
        jz      @@OutRightNextPlane1
;
        mov     ax, [ds:si]
        xchg    al, ah
        shr     ax, cl
        call    [OpEdge]
;
        dec     bx
        add     si, [WIB]
        add     di, [BytesPerRow]
        jmp     @@OutRightLoop_4
@@OutRightNextPlane1:
        pop     si
        add     si, [BPS]
        pop     bx
        pop     ds
        jmp     @@OutRightLoop_3
@@EndOutRight:
        jmp     @@Exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@AloneByte:
        mov     al, [MaskLeft]
        and     [MaskRight], al
        dec     si
        jmp     @@OutRight              ; Почти то же самое
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@ByteAligned:
; Процедура для выровненного на границу байта образа
    ; расчет маски для последнего байта по формуле
    ; MaskRight = 0FFh SHL (((xLen-1) AND 7) XOR 7)
        mov     cl, [byte xLen]
        dec     cl
        and     cl, 7
        xor     cl, 7
        mov     al, 0FFh
        shl     al, cl
        mov     [MaskRight], al
    ; определим ширину в байтах того, что мы должны выводить !
    ; Эта величина не обязательно должна быть равна WidthInBytes,
    ; а может быть и похитрее
        mov     dx, [xLen]
        add     dx, 7
        shr     dx, 3
        jcxz    @@101           ; Равносильно if MaskRight=0FFh goto @@101
        dec     dx
@@101:
        or      dx, dx
        je      @@ByteAligned_Right
        mov     bx, offset BitPlanes
        mov     [PlCnt], 4
        push    si
@@ByteAlignedLoop_1:
        cmp     [PlCnt], 0
        je      @@EndOfByteAlignedLoop_1
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsLeft]          ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        push    bx                      ; Сохр. смещение на адр. вирт. буфера
        mov     bx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@ByteAlignedLoop_2:
        or      bx, bx
        jz      @@EndOfByteAlignedLoop_2
        dec     bx
        mov     cx, dx
;
        push    si
        push    di
@@5100:
        lodsb
        call    [OpNorm]
        inc     di
        loop    @@5100
        pop     di
        pop     si
;
        add     si, [WIB]               ; Следующая строка в BitMap'е
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        jmp     @@ByteAlignedLoop_2
@@EndOfByteAlignedLoop_2:
        pop     bx                      ; Восст. смещ. на адр. вирт. буфера
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@ByteAlignedLoop_1
@@EndOfByteAlignedLoop_1:
        pop     si                      ; Самое начало в BitMap'е
        cmp     [MaskRight], 0FFh       ; Нет правоя части вообще ???
        je      @@Exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@ByteAligned_Right:
        add     si, dx
        ;add    di, dx  ; ЛУЧШЕ брать правую часть, она посчитана
        mov     bx, offset BitPlanes
        mov     [PlCnt], 4
        mov     dl, [MaskRight]         ; 1 - Что можно
        mov     dh, dl
        not     dh                      ; 1 - Что нельзя !
@@ByteAlignedLoop_3:
        cmp     [PlCnt], 0
        je      @@EndOfByteAlignedLoop_3
        dec     [PlCnt]
        les     di, [bx]                ; es:di - очередная пл. в вирт. буф.
        add     di, [OffsRight]         ; на начало области вывода
        add     bx, 4                   ; На следующую плоскость в вирт. буф.
        push    ds                      ; Сохраняем сегмент данных
        mov     ds, [word Buffer+2]
        push    si
;--- Подготовка внутреннего цикла по строкам
        mov     cx, [yLen]              ; Число строк
;--- Внутренний цикл по строкам
@@ByteAlignedLoop_4:
        mov     al, [ds:si]
        call    [OpEdge]
        add     si, [WIB]               ; Следующая строка в BitMap'е
        add     di, [BytesPerRow]       ; Следующая строка в вирт. буфере
        loop    @@ByteAlignedLoop_4
;
        pop     si
        add     si, [BPS]               ; След. плоскость в BitMap'е
        pop     ds                      ; Сегмент данных
        jmp     @@ByteAlignedLoop_3
@@EndOfByteAlignedLoop_3:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@@Exit:
        pop     ds
        RET
ENDP


END