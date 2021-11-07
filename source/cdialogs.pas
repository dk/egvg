{$A+,B-,D-,E-,F-,G+,I-,L-,N-,O-,P-,Q-,R-,S-,T-,V-,X-,Y-}
                                  UNIT CDialogs;
                                  Interface
Uses  Objects, Views, App, Dialogs, MMonolog, VApp;

Type

  PCDialog = ^TCDialog;
  TCDialog = Object(TMonolog)
    function GetPalette: PPalette;   virtual;
  end;

  PCApp = ^TCApp;
  TCApp = Object(TVApplication)
    function GetPalette: PPalette;   virtual;
  end;

  PMonolog = PCDialog;
  TMonolog = TCDialog;

  TApplication  = TCApp;
  TVApplication = TCApp; {DK}

  procedure RegisterCDialogs;

Const
  dpMagentaDialog = 3;

  RMonolog: TStreamRec = (
     ObjType: 2000;
     VmtLink: Ofs(TypeOf(TMonolog)^);
     Load:    @TMonolog.Load;
     Store:   @TMonolog.Store
  );

  RApplication: TStreamRec = (
     ObjType: 2001;
     VmtLink: Ofs(TypeOf(TApplication)^);
     Load:    @TApplication.Load;
     Store:   @TApplication.Store
  );

  CNewColors = #6#13#1;


  CMagentaDialog =
    #218#218#220#218#154#218#219#220#218#159 +
    #160#161#162#163#164#219#220#167#168#169 +
    #218#171#218#173#174#175#176#177#178#179 +
    #180#181#182#218#184#218#186#218#220#189 +
    #190#191#192#219#220#195#196#197#198#199 +
    #200#201#202#203#204#205#206#207#208#209 +
    #210#211#212#213#214#215#216;


                           Implementation

function TCApp.GetPalette;
const P: String[Length(CAppColor+CNewColors)] = CAppColor+CNewColors;
begin
  GetPalette:=@P
end;

function TCDialog.GetPalette;
const
 P: Array [dpBlueDialog..dpMagentaDialog] of String[Length(CBlueWindow)] =
 (CBlueDialog, CCyanDialog, CGrayDialog, CMagentaDialog);
begin
  GetPalette:=@P[Palette];
end;

procedure RegisterCDialogs;
begin
  RegisterType(RMonolog);
  RegisterType(RApplication);
end;

End.