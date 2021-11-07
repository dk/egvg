    Procedure SetColor(IdioticColor : Byte);
    Procedure FillRect(x1, y1, x2, y2 : Integer);
    Procedure WriteStr(x, y : Integer; S : String);
    Procedure PlotVLine(x, y1, y2 : Integer);
    Procedure PlotLine(x1, x2, y1, y2 : Integer);
    Function  _Font : PFont;
    Function ChStrLen(S : String; From : Integer) : Integer;
