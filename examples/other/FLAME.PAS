{$G+}
{ From: 9308920@info.umoncton.ca (DEGRACE KEITH) }

var
  x : byte;
  l:Word;

procedure setrgb(c, r, g, b : byte);
begin
  port[$3c8] := c;
  port[$3c9] := r;
  port[$3c9] := g;
  port[$3c9] := b;
end;

begin
  randomize;
  asm   mov ax, 13h
        int 10h
  end;
  for x := 1 to 32 do  begin
    setrgb(x, (x shl 1)-1, 0, 0 );
    setrgb(x+32, 63, (x shl 1)-1, 0 );
    setrgb(x+64, 63, 63, (x shl 1)-1 );
    setrgb(x+96, 63, 63, 63 );
  end;
  l := $51ac;

  repeat
    asm
     mov ch, 0
     mov ax, SegA000
     mov es, ax
    @@x:
      mov cl, 30
      @@y:
          mov al, cl
          xor ah, ah
          mov di, ax
          shl di, 1
          mov al, ch
          dec ax
          shl ax, 1

          shl di, 6
          mov bx, di
          shl bx, 2
          add di, bx
          add di, ax
          mov bx, ax

          xor dx, dx
          mov al, dl
          add dl, byte ptr es:[di]
          inc di
          inc di
          adc dh, al
          add dl, byte ptr es:[di]
          adc dh, al
          add dl, byte ptr es:[di+2]
          adc dh, al
          add dl, byte ptr es:[di+642]
          adc dh, al
          shr dx, 2

          or dl, dl
          je @@1
          dec dl
       @@1:
          mov dh, dl
          mov word ptr es:[di-320], dx
          mov word ptr es:[di-640], dx
        inc cl
        cmp cl, 101
        jb @@y

        ror L, 1
        xor L, $aa15
        inc L
        mov al, byte ptr l
        and al, 1
        mov ah, al
        shl ah, 2
        add al, ah
        shl al, 5
        mov byte ptr es:[di], al
      inc ch
      cmp ch, 160
      jb  @@x
    end;
  until port[$60] < $80;
  asm  mov ax, 3
       int 10h
  end;
end.


