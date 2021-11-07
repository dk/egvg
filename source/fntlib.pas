unit FntLib; {SWIN, TONY}
{ Дополнительные часто используемые шрифты в коде }

interface

procedure Courier;
procedure Small;
procedure Thick;
procedure Large;
procedure GIOFont; {Large courier}

implementation
{$L Courier.OBJ }
procedure Courier; external;

{$L Small.OBJ }
procedure Small; external;

{$L SKBFNT.OBJ}
procedure GIOFont;       External;
procedure Thick;     External;
procedure Large;     External;

end.
