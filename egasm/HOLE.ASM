HOLE_TEXT      SEGMENT 'CODE' BYTE PUBLIC
        assume  ds:HOLE_TEXT, cs:HOLE_TEXT
        PUBLIC  NoteBook_Hole
NoteBook_Hole:
    DW  1970h
    DB  11, 0, 2, 0, 12, 0, 15, 224, 127, 240, 255, 240, 0, 16, 127, 240
    DB  15, 240, 127, 240, 255, 240, 0, 16, 127, 240, 15, 224, 240, 16, 255, 192
    DB  255, 224, 255, 224, 128, 0, 240, 0, 255, 192, 255, 224, 255, 224, 128, 0
    DB  240, 16, 240, 16, 255, 192, 255, 224, 255, 224, 128, 0, 240, 0, 255, 192
    DB  255, 224, 255, 224, 128, 0, 240, 16, 240, 16, 255, 192, 255, 224, 255, 224
    DB  128, 0, 240, 0, 255, 207, 255, 224, 255, 239, 128, 0, 240, 16, 0, 0
HOLE_TEXT      ENDS
END
