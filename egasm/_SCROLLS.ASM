BMP_TEXT      SEGMENT 'CODE' BYTE PUBLIC
        assume  ds:BMP_TEXT, cs:BMP_TEXT

public ScrollRightDef
public ScrollRightPress
public ScrollLeftDef
public ScrollLeftPress
public ScrollUpDef
public ScrollUpPress
public ScrollDownDef
public ScrollDownPress
public ComboDef
public ComboPress
public ScrollThumb

ScrollRightDef:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h

ScrollRightPress:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h

ScrollLeftDef:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ScrollLeftPress:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ScrollUpDef:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,00h,00h,00h,00h,00h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ScrollUpPress:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,00h,00h,00h,00h,00h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ScrollDownDef:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,00h,00h,00h,00h,00h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ScrollDownPress:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,00h,00h,00h,00h,00h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ScrollThumb:
  DB 70h,19h,11h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ComboDef:
  DB 70h,19h,13h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,0Fh,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,00h,00h,00h,00h,00h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,00h,00h,00h,00h,00h,00h,00h,00h,00h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,08h,08h,00h
  DB 00h,0Fh,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


ComboPress:
  DB 70h,19h,13h,00h,00h,01h,11h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
  DB 00h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,08h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,00h,00h,00h,00h,00h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,00h,00h,00h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,00h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,00h,00h,00h,00h,00h,00h,00h,00h,00h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,08h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,07h,00h
  DB 00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h


BMP_TEXT ENDS
END
