Unit WallPapers;
{ ������� WallPaper'�, ����� � ������ ᥣ���� }

interface

procedure WallPaperLeather;
procedure WallPaperConcrete;
procedure WallPaperDoom;

implementation

{$L LEATHER.OBJ}
procedure WallPaperLeather; external;

{$L CONCRETE.OBJ}
procedure WallPaperConcrete; external;

{$L DOOMWALL.OBJ}
procedure WallPaperDoom; external;

end.
