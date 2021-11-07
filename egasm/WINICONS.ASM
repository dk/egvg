BMP_TEXT      SEGMENT 'CODE' BYTE PUBLIC
    assume  ds:BMP_TEXT, cs:BMP_TEXT
    public  CloseWindowIcon
    public  ZoomWindowIcon
    public  RestoreWindowIcon
    public  MinimizeWindowIcon

CloseWindowIcon:
    DW  1970h
    DB  7, 0, 1, 0, 7, 0, 254, 56, 146, 198, 238, 254, 0, 254, 56, 146
    DB  198, 238, 255, 1, 254, 56, 146, 198, 238, 255, 1, 254, 56, 146, 198, 238
    DB  255, 1, 1, 0

RestoreWindowIcon:
    DW  1970h
    DB  9, 0, 1, 0, 7, 0, 238, 198, 130, 0, 254, 0, 130, 199, 239, 238
    DB  198, 130, 0, 254, 1, 131, 199, 239, 238, 198, 130, 0, 254, 1, 131, 198
    DB  238, 238, 198, 130, 0, 254, 1, 131, 199, 239, 252, 0

ZoomWindowIcon:
    DW  1970h
    DB  7, 0, 1, 0, 7, 0, 254, 254, 238, 198, 131, 0, 254, 254, 254, 238
    DB  198, 130, 0, 254, 254, 254, 238, 198, 131, 1, 254, 254, 255, 238, 198, 130
    DB  0, 254, 0, 0

MinimizeWindowIcon:
    DW  1970h
    DB  7, 0, 1, 0, 7, 0, 254, 254, 0, 130, 198, 238, 254, 255, 254, 0
    DB  130, 199, 239, 255, 254, 255, 0, 130, 198, 239, 254, 254, 254, 0, 130, 198
    DB  239, 254, 0, 0

BMP_TEXT ENDS
END

