{$G+,S-,F+}
{$IFDEF DPMI}
{$C FIXED PRELOAD PERMANENT}
{$ENDIF}
{
  █▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀█
  █ Unit        : SVGA Graphics Library                                █
  █ Description : Virtual Graphics SuperVGA 32K-colors driver          █
  █ Author      : Dmitry Karasik                                       █
  █ Version     : X01.00 (internal)                                    █
  █ Release     : 01.00                                                █
  █ Last update : 26-AUG-1996                                          █
  █▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄█

}
{$DEFINE BEWARENOTSVGA}
Unit SVGA32K;

Interface

Uses Objects, Memory, EGInline, SVGA64K, GDI;


Function  SVGA32KInitialize : Boolean;

Implementation

Uses VESA;

function Vesa32K_320x200EnterGraphics: boolean; far;
begin Vesa32K_320x200EnterGraphics := VesaEnterGraphics($10D); end;
function Vesa32K_640x480EnterGraphics: boolean; far;
begin Vesa32K_640x480EnterGraphics := VesaEnterGraphics($110); end;
function Vesa32K_800x600EnterGraphics: boolean; far;
begin Vesa32K_800x600EnterGraphics := VesaEnterGraphics($113); end;
function Vesa32K_1024x768EnterGraphics: boolean; far;
begin Vesa32K_1024x768EnterGraphics := VesaEnterGraphics($116); end;
function Vesa32K_1280x1024EnterGraphics: boolean; far;
begin Vesa32K_1280x1024EnterGraphics := VesaEnterGraphics($119); end;

Procedure DIBAdjSelect;
Var
  I : Word;
Begin
  HiColor16Bit := 5;
  MaxColors := 32768;
  for I := 0 to 15 do with ScreenDriver^ do ColorIndex^[I] :=
    HiColor(GraphPalette^[I].R, GraphPalette^[I].G, GraphPalette^[I].B);
  for I := 16 to 255 do ColorIndex^[I] :=
    HiColor(PMainPalette^[I, 1], PMainPalette^[I, 2], PMainPalette^[I, 3]);
End;

Const
  DriverMethods : TDriverMethods = (
    _SetPixel        : SVGA64K.SetPixel;
    _SetPixelOp      : SVGA64K.SetPixelOp;
    _GetPixel        : SVGA64K.GetPixel;
    _GetPixelBM      : SVGA64K.GetPixelBM;
    _HLine           : SVGA64K.HLine;
    _VLine           : SVGA64K.VLine;
    _DisplayXxY      : SVGA64K.DisplayXxY;
    _DisplayXxYClip  : SVGA64K.DisplayXxYClip;
    _ReadScanLine    : SVGA64K.ReadScanLine;
    _WriteScanLine   : SVGA64K.WriteScanLine;
    _WriteScanLineOp : SVGA64K.WriteScanLineOp;
    _PutBMPPart      : SVGA64K.PutBMPPart;
    _PutBMPPartOp    : SVGA64K.PutBMPPartOp;
    _HLineStyleOp    : SVGA64K.HLineStyleOp;
    _DisplayXx8Op    : SVGA64K.DisplayXx8Op;
    _QuickSave       : SVGA64K.QuickSave;
    _QuickRestore    : SVGA64K.QuickRestore;
    _DirectMousePut  : SVGA64K.DirectMousePut;
    _VLineStyleT     : SVGA64K.VLineStyleT;
    _DirectGetImage  : SVGA64K.DirectGetImage;
    _DirectPutImage  : SVGA64K.DirectPutImage;
    _PutBufferPart   : SVGA64K.PutBufferPart;
    _MapBMPLineRead  : SVGA64K.MapBMPLineRead;
    _Init            : SVGA32KInitialize;
    _Done            : SVGA64K.SVGA64KFInitialize;
    _StretchDIBitmap : SVGA64K.StretchDIBitmap;
    _ImplantDIBitmap : SVGA64K.ImplantDIBitmap;
    _SetOutput       : VESA.SetOutput;
    _PrepareDrawing  : VESA.PrepareDrawing;
    _SetUserBitBltProc:SVGA64K.SetUserBitBltProc;
    _SetColorBitBlt  : SVGA64K.SetColorBitBlt;
    _EMSAdjSelect    : VESA.EMSAdjSelect;
    _DIBAdjSelect    : SVGA32K.DIBAdjSelect
  );
  Vesa32K_320x200ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 320;
    ScreenHeight       : 200;
    MaximalX           : 319;
    MaximalY           : 199;
    NumberOfColors     : 32768;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa32K_320x200EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $10D
  );
  Vesa32K_640x480ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 640;
    ScreenHeight       : 480;
    MaximalX           : 639;
    MaximalY           : 479;
    NumberOfColors     : 32768;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa32K_640x480EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $110
  );
  Vesa32K_800x600ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 800;
    ScreenHeight       : 600;
    MaximalX           : 799;
    MaximalY           : 599;
    NumberOfColors     : 32768;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa32K_800x600EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $113
  );
  Vesa32K_1024x768ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 1024;
    ScreenHeight       : 768;
    MaximalX           : 1023;
    MaximalY           : 767;
    NumberOfColors     : 32768;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa32K_1024x768EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $116
  );
  Vesa32K_1280x1024ScreenDriver : TScreenDriver = (
    VirtualScreenStyle : vssSegmented or vssGranulated;
    ScreenWidth        : 1280;
    ScreenHeight       : 1024;
    MaximalX           : 1279;
    MaximalY           : 1023;
    NumberOfColors     : 32768;
    ColorShift         : 0;
    NumPlanes          : 1;
    GraphPalette       : @DefaultGraphPalette;
    EnterGraphics      : Vesa32K_1280x1024EnterGraphics;
    LeaveGraphics      : VESAStandardLeaveGraphics;
    Methods            : @DriverMethods;
    ID                 : $119
  );


Function SVGA32KInitialize : Boolean;
Begin
  SVGA32KInitialize := False;
  if not SVGA64KInitialize then Exit;
  SVGA32KInitialize := True;
  DIBAdjSelect;
End;



begin
  RegisterDriver(@Vesa32K_640x480ScreenDriver); {1st as default}
  RegisterDriver(@Vesa32K_320x200ScreenDriver);
  RegisterDriver(@Vesa32K_800x600ScreenDriver);
  RegisterDriver(@Vesa32K_1024x768ScreenDriver);
  RegisterDriver(@Vesa32K_1280x1024ScreenDriver);
end.
