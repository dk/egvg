unit FntLib; {SWIN, TONY}
{ �������⥫�� ��� �ᯮ��㥬� ����� � ���� }

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
