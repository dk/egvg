{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        :                                                      █
  █                                                                    █
  █ Description : Colors & Palette Manager                             █
  █                                                                    █
  █ Author      :                                                      █
  █════════════════════════════════════════════════════════════════════█
  █                                                                    █
  █                                                                    █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█
}

unit EGColor;

{$IFNDEF NewColorMapping}
interface implementation end.
{$ENDIF}

interface uses GDI;

type
  TPatternedColor = word;
  TSolidColor = byte;

  TStringColor = record
    Fore: TSolidColor;
    Back: TPatternedColor;
  end;

  THiStringColor = record
    Fore: TSolidColor;
    High: TSolidColor;
    Back: TPatternedColor;
  end;

  PMenuScheme = ^TMenuScheme;
  TMenuScheme = record
    Normal      : THiStringColor;
    Disabled    : THiStringColor;
    Selected    : THiStringColor;
    Shadow      : TPatternedColor;
  end;
  TStatusLineScheme = TMenuScheme;
  PStatusLineScheme = ^TStatusLineScheme;

  TFrameColor = record
    Border : TPatternedColor;
    Title  : TStringColor;
    Shadow : TPatternedColor;
    Background : TPatternedColor;
  end;

  TFrameScheme = record
    Active     : TFrameColor;
    Inactive   : TFrameColor;
  end;

  TIconScheme = record
    Normal  : TStringColor;
    Focused : TStringColor;
  end;

  TScrollBarScheme = record
    Background : TPatternedColor;
    Shadow     : TPatternedColor;
  end;

  TListViewerColor = record
    Normal: TStringColor;
    Focused: TStringColor;
    Selected: TStringColor;
  end;

  TListViewerScheme = record
    Background : TPatternedColor;
    Active   : TListViewerColor;
    Inactive : TListViewerColor;
    Shadow   : TPatternedColor;
  end;

  TInputLineColor = record
    Normal   : TStringColor;
  end;

  TInputLineScheme = record
    Active   : TStringColor;
    Inactive : TStringColor;
    Selected : TStringColor;
    Arrow    : TStringColor;
  end;

  TButtonColor = record
    Title  : THiStringColor;
    Shadow : TPatternedColor;
  end;

  TButtonScheme = record
    Normal    : TButtonColor;
    Selected  : TButtonColor;
    Disabled  : TButtonColor;
    Default   : TButtonColor;
  end;

  TClusterScheme = record
    Normal   : THiStringColor;
    Selected : THiStringColor;
    Disabled : THiStringColor;
  end;

  TLabelScheme = record
    Normal   : THiStringColor;
    Selected : THiStringColor;
  end;

  THistoryColor = record
    Back   : TPatternedColor;
    Title  : TStringColor;
    Shadow : TPatternedColor;
  end;

  THistoryScheme = record
    Active   : THistoryColor;
    Inactive : THistoryColor;
  end;

  PWindowScheme = ^TWindowScheme;
  TWindowScheme = record
    Frame     : TFrameScheme;
    Icon       : TIconScheme;
    Menu      : TMenuScheme;
    ScrollBar : TScrollBarScheme;
    ListViewer: TListViewerScheme;
    InputLine : TInputLineScheme;
    Button    : TButtonScheme;
    Cluster   : TClusterScheme;
    StaticText: TStringColor;
    Labels    : TLabelScheme;
    History   : THistoryScheme;
    InfoPane  : TStringColor;
  end;
  TDialogScheme = TWindowScheme;

  PAppScheme = ^TAppScheme;
  TAppScheme = record
    Desktop    : TPatternedColor;
    Menu       : TMenuScheme;
    StatusLine : TStatusLineScheme;
  end;

  PColorScheme = ^TColorScheme;
  TColorScheme = record
    AppScheme: TAppScheme;
    WindowScheme1: TWindowScheme;  { Alias 'BLUE' }
    WindowScheme2: TWindowScheme;  { Alias 'GRAY' }
    WindowScheme3: TWindowScheme;  { Alias 'CYAN' }
  end;

procedure SetColorScheme(Scheme: PColorScheme);
procedure SetDefaultColorScheme;
function  GetColorScheme: PColorScheme;

function Back1(Attr: word): Byte; inline($58/$C1/$E8/$04 {pop ax; shr ax, 4});
function Back2(Attr: word): Byte; inline($58 {pop ax});

implementation

const
  DefaultColorScheme : TColorScheme = (
    AppScheme: (
      Desktop    : $0707;

      Menu       : (
        Normal      : (Fore: $00; High: $04; Back: $0707);
        Disabled    : (Fore: $08; High: $04; Back: $0707);
        Selected    : (Fore: $00; High: $0E; Back: $0202);
        Shadow      : $0F08
      );

      StatusLine : (
        Normal      : (Fore: $00; High: $04; Back: $0707);
        Disabled    : (Fore: $08; High: $04; Back: $0707);
        Selected    : (Fore: $00; High: $0E; Back: $0202);
        Shadow      : $0F08
      )
    );

    WindowScheme1: (
      Frame: (
        Active     : (
          Border : $0707;
          Title  : (Fore: $0B; Back: $0101);
          Shadow : $0F08;
          Background : $0707
        );
        Inactive     : (
          Border : $0707;
          Title  : (Fore: $00; Back: $0707);
          Shadow : $0F08;
          Background: $0707
        )
      );

      Icon       : (
        Normal  : (Fore: $00; Back: $0707);
        Focused : (Fore: $0B; Back: $0101)
      );

      Menu : (
        Normal      : (Fore: $00; High: $04; Back: $0707);
        Disabled    : (Fore: $08; High: $04; Back: $0707);
        Selected    : (Fore: $00; High: $0E; Back: $0202);
        Shadow      : $0F08
      );

      ScrollBar : (
        Background : $0707;
        Shadow     : $080F
      );

      ListViewer : (
        Background : $0707;
        Active   : (
          Normal  : (Fore: $00; Back: $0707);
          Focused : (Fore: $00; Back: $0202);
          Selected: (Fore: $0B; Back: $0101)
        );
        Inactive : (
          Normal  : (Fore: $00; Back: $0707);
          Focused : (Fore: $01; Back: $0707);
          Selected: (Fore: $0E; Back: $0707)
        );
        Shadow   : $080F
      );

      InputLine : (
        Active   : (Fore: $0F; Back: $0101);
        Inactive : (Fore: $00; Back: $0707);
        Selected : (Fore: $0E; Back: $0202);
        Arrow    : (Fore: $09; Back: $0202)
      );

      Button : (
        Normal    : (
          Title  : (Fore: $00; High: $04; Back: $0707);
          Shadow : $0000
        );
        Selected  : (
          Title  : (Fore: $00; High: $04; Back: $0707);
          Shadow : $0000
        );
        Disabled  : (
          Title  : (Fore: $08; High: $04; Back: $0707);
          Shadow : $0808
        );
        Default   : (
          Title  : (Fore: $00; High: $04; Back: $0707);
          Shadow : $0000
        )
      );

      Cluster : (
        Normal   : (Fore: $00; High: $04; Back: $0707);
        Selected : (Fore: $0B; High: $0E; Back: $0101);
        Disabled : (Fore: $08; High: $08; Back: $0707)
      );

      StaticText : (Fore: $00; Back: $0707);

      Labels     : (
        Normal   : (Fore: $00; High: $04; Back: $0707);
        Selected : (Fore: $0F; High: $0E; Back: $0707)
      );

      History : (
        Active : (
          Back   : $0707;
          Title  : (Fore: $0B; Back: $0101);
          Shadow : $0F08
        );
        Inactive : (
          Back   : $0707;
          Title  : (Fore: $08; Back: $0707);
          Shadow : $0F08
        )
      );

      InfoPane : (Fore: $01; Back: $0707)
    );

    WindowScheme2: (
    );

    WindowScheme3: (
    )
  );

var
  ColorScheme: PColorScheme;


procedure SetColorScheme(Scheme: PColorScheme);
begin
  if Assigned(ColorScheme) and
     (Seg(ColorScheme^) <> DSeg) and
     (Seg(ColorScheme^) <> SSeg)
  then
    Dispose(ColorScheme);
  ColorScheme := Scheme;
end;

procedure SetDefaultColorScheme;
begin
  SetColorScheme(@DefaultColorScheme);
end;

function GetColorScheme: PColorScheme;
begin
  GetColorScheme:= ColorScheme;
end;

begin
  SetDefaultColorScheme;
end.
