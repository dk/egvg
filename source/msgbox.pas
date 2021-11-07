{$DEFINE USESTANDARDBITMAPS}
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit MsgBox;

{$O+,F+,X+,I-,S-}

interface

uses Objects, BitMaps;

const

{ Message box classes }

  mfWarning      = $0000;       { Display a Warning box }
  mfError        = $0001;       { Dispaly a Error box }
  mfInformation  = $0002;       { Display an Information Box }
  mfConfirmation = $0003;       { Display a Confirmation Box }

  mfInsertInApp  = $0080;       { Insert message box into application }
                                { instead of the Desktop }

{ Message box button flags }

  mfYesButton    = $0100;       { Put a Yes button into the dialog }
  mfNoButton     = $0200;       { Put a No button into the dialog }
  mfOKButton     = $0400;       { Put an OK button into the dialog }
  mfCancelButton = $0800;       { Put a Cancel button into the dialog }
  mfNoMove       = $1000;       { Make MessageBox unmovable }

  mfYesNoCancel  = mfYesButton + mfNoButton + mfCancelButton;
                                { Standard Yes, No, Cancel dialog }
  mfOKCancel     = mfOKButton + mfCancelButton;
                                { Standard OK, Cancel dialog }
  mfBitmap       = $2000;       { Treat params as record : PImage, PVGAPalette, Params }
{ MessageBox displays the given string in a standard sized      }
{ dialog box. Before the dialog is displayed the Msg and Params }
{ are passed to FormatStr.  The resulting string is displayed   }
{ as a TStaticText view in the dialog.                          }

function MessageBox(const Msg: String; Params: Pointer;
  AOptions: Word): Word;

{ MessageBoxRec allows the specification of a TRect for the     }
{ message box to occupy.                                        }

function MessageBoxRect(var R: TRect; const Msg: String; Params: Pointer;
  AOptions: Word): Word;

{ InputBox displays a simple dialog that allows the user to     }
{ type in a string.                                             }

function InputBox(const Title, ALabel: String; var S: String;
  Limit: Byte): Word;

{ InputBoxRect is like InputBox but allows the specification of }
{ a rectangle.                                                  }

function InputBoxRect(var Bounds: TRect; const Title, ALabel: String;
  var S: String;  Limit: Byte): Word;

{ Image registration procedure uses IDs : 200..208 }
procedure RegisterMsgBoxImages;

{$IFDEF UseStandardBitMaps}
{ These aren't procedures. These are images!  Don't call them! }
(*
procedure Mark_Exclamation_Xor;   {201}
procedure Mark_Exclamation_And;   {202}
procedure Mark_Information_Xor;   {203}
procedure Mark_Information_And;   {204}
procedure Mark_Question_Xor;      {205}
procedure Mark_Question_And;      {206}
procedure Mark_RedArm_Xor;        {207}
procedure Mark_RedArm_And;        {208}
*)

procedure Mark_Exclamation;   {201}
procedure Mark_Information;   {203}
procedure Mark_Question;      {205}
procedure Mark_RedArm;        {207}

procedure  RegisterMarkImages;
{$ENDIF}

implementation

uses Drivers, Views, Dialogs, App, EGFont, EGInline;

function MessageBox(const Msg: String; Params: Pointer;
  AOptions: Word): Word;
var
  R: TRect;
begin
  SelectFontCaps(GlobalFont);
{$IFDEF UseStandardBitMaps} {TONY}
  R.Assign(0, 0, 40 * GetCharWidth + BitMapWidth(@Mark_Information), 11 * GetHeight);
{$ELSE}
  R.Assign(0, 0, 40 * GetCharWidth, 10 * GetHeight);
{$ENDIF}
  if (AOptions and mfBitmap) <> 0 then begin
    R.Assign(0, 0, 40 * GetCharWidth + BitMapWidth(Pointer(Params^)),
      4 * GetHeight);
    Inc(R.B.Y, MaxWord(BitMapHeight(Pointer(Params^)), 7 * GetHeight));
  end;
  if AOptions and mfInsertInApp = 0 then
    R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2)
  else R.Move((Application^.Size.X - R.B.X) div 2, (Application^.Size.Y - R.B.Y) div 2);
  MessageBox := MessageBoxRect(R, Msg, Params, AOptions);
end;

Type
  PCallNotify = ^TCallNotify;
  TCallNotify = Object(TView)
    What   : Word;
    Called : Boolean;
    Constructor Init(AWhat : Word);
    Procedure   HandleEvent(var Event : TEvent); Virtual;
  End;

Const
  cmCallNotify = $F404;

Constructor TCallNotify.Init;
Var
  R : TRect;
Begin
  R.Assign(0, 0, 0, 0);
  Inherited Init(R);
  EventMask := evBroadcast;
  SetState(sfDisabled, True);
  SetState(sfVisible,  False);
  What := AWhat;
  Called := False;
End;

Procedure TCallNotify.HandleEvent;
Begin
  Inherited HandleEvent(Event);
  if (Event.Command = cmCallNotify) and not(Called) then begin
    Called := True;
    SystemBeep(What);
  end;
End;

function MessageBoxRect(var R: TRect; const Msg: String; Params: Pointer;
  AOptions: Word): Word {TONY};
const
  Notifys : array[0..3] of Word = (sbWarning, sbError, sbInfo, sbConfirm);
{$IFDEF UseStandardBitMaps}
  ButtonPtrs: array [0..3] of array [0..3] of Word = (
    (159, 160, 160, 162),
    (163, 164, 164, 166),
    (151, 152, 152, 154),
    (155, 156, 156, 158)
  );
{$ENDIF}
{$IFDEF Ukrainian}
  {$IFNDEF UseStandardBitMaps}
    ButtonName: array[0..3] of string[5] =
      ('~Д~а', '~Н~ет', 'Ввод', 'Отказ');
  {$ENDIF}
  Titles: array[0..3] of string[14] =
    ('Предупреждение','Ошибка','Информация','Согласование');
{$ELSE}
  {$IFDEF Russian}
    {$IFNDEF UseStandardBitMaps}
      ButtonName: array[0..3] of string[5] =
        ('~Д~а', '~Н~ет', 'Ввод', 'Отказ');
    {$ENDIF}
  Titles: array[0..3] of string[14] =
    ('Предупреждение','Ошибка','Информация','Согласование');
  {$ELSE}
    {$IFNDEF UseStandardBitMaps}
      ButtonName: array[0..3] of string[6] =
        ('~Y~es', '~N~o', 'O~K~', 'Cancel');
    {$ENDIF}
  Titles: array[0..3] of string[11] =
    ('Warning','Error','Information','Confirm');
  {$ENDIF}
{$ENDIF}
  Commands: array[0..3] of word =
    (cmYes, cmNo, cmOK, cmCancel);
{$IFDEF UseStandardBitMaps}
  BMPS: array[0..3] of Word =
    (201, 207, 203, 205);
Const
  DrawNotBlack = 5;
{$ENDIF}

var
  I, X, ButtonCount: Integer;
  Dialog: PDialog;
  Control: PView;
  ButtonList: array[0..4] of PView;
  S: String;
begin
  Dialog := New(PDialog, Init(R, Titles[AOptions and $3]));
  with Dialog^ do
  begin
    Frame^.GetClientExtent(R);
    R.Grow(-2, -2);
    if (AOptions and mfBitmap) <> 0 then begin
      Control := New(PStaticBitmap, Init(R,
                   New(PBitMap, Init(GetImageID(Pointer(Params^)))),
                     Pointer(Pointer(LongInt(Params)+4)^), True));
      Insert(Control);
      Inc(LongInt(Params), 8);
      Inc(R.A.X, Control^.Size.X);
    end
{$IFDEF UseStandardBitMaps}
    else begin
      Control := New(POperationalBitmap, Init(R,
        New(PBitMap, Init(BMPS[AOptions and $3])), Nil,
        True, DrawNotBlack));
      Insert(Control);
      Inc(R.A.X, Control^.Size.X); {OOA}
    end
{$ENDIF};
    R.B.Y := Size.Y - 3 * FontHeight - 8;
    FormatStr(S, Msg, Params^);
    Control := New(PStaticText, Init(R, S));
    Insert(Control);
    X := -2 * CharWidth;
    ButtonCount := 0;
    for I := 0 to 3 do
      if AOptions and ($0100 shl I) <> 0 then
      begin
        R.Assign(0, 0, 10 * CharWidth, 2 * FontHeight);
{$IFDEF UseStandardBitMaps}
        Control := New(PBmpButton, Init(R, Commands[i], bfNormal,
          ButtonPtrs[I][0],
          ButtonPtrs[I][1],
          ButtonPtrs[I][2],
          ButtonPtrs[I][3],
          ButtonPtrs[I][0]
        ));
{$ELSE}
        Control := New(PButton, Init(R, ButtonName[I], Commands[i],
          bfNormal));
{$ENDIF}
        Inc(X, Control^.Size.X + 2 * CharWidth);
        ButtonList[ButtonCount] := Control;
        Inc(ButtonCount);
      end;
    X := (Size.X - X) shr 1;
    for I := 0 to ButtonCount - 1 do
    begin
      Control := ButtonList[I];
      Insert(Control);
      Control^.MoveTo(X, Size.Y - 3 * FontHeight - 8);
      Inc(X, Control^.Size.X + 2 * CharWidth);
    end;
    SelectNext(False);
  end;
  if AOptions and mfNoMove <> 0 then Dec(Dialog^.Flags, wfMove); {GIO}
  TimedMessage(5, Dialog, evBroadcast, cmCallNotify, nil, 1);
  Dialog^.Insert(New(PCallNotify, Init(Notifys[AOptions and 3])));
  if AOptions and mfInsertInApp = 0 then
    MessageBoxRect := DeskTop^.ExecView(Dialog)
  else MessageBoxRect := Application^.ExecView(Dialog);
  Dispose(Dialog, Done);
end;

function InputBox(const Title, ALabel: String; var S: String;
  Limit: Byte): Word;
var
  R: TRect;
begin
  R.Assign(0, 0, 60 * GetCharWidth, 8 * GetHeight);
  R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
  InputBox := InputBoxRect(R, Title, ALabel, S, Limit);
end;

function InputBoxRect(var Bounds: TRect; const Title, ALabel: String;
  var S: String;  Limit: Byte): Word;
var
  Dialog: PDialog;
  Control: PView;
  R: TRect;
  C: Word;
  CW, CH: integer;
begin
  Dialog := New(PDialog, Init(Bounds, Title));
  with Dialog^ do
  begin
    CW := GetCharWidth; CH := GetHeight;
    R.Assign((4 + CStrLen(ALabel)) * CW, 2 * CH, Size.X - 3 * CW, 3 * CH);
    Control := New(PInputLine, Init(R, Limit));
    Insert(Control);
    R.Assign(2 * CW, 2 * CH, (3 + CStrLen(ALabel)) * CW, 3 * CH);
    Insert(New(PLabel, Init(R, ALabel, Control)));
    R.Assign(Size.X - 24 * CW, Size.Y - 4 * CH, Size.X - 14 * CW, Size.Y - 2 * CH);

{$IFDEF UseStandardBitMaps} {SHIM}
    Insert(MakeOKButton(R, cmOk, bfDefault));
    Inc(R.A.X, 12 * CW); Inc(R.B.X, 12 * CW);
    Insert(MakeCancelButton(R, cmCancel, bfNormal));
{$ELSE}
 {$IFDEF Ukrainian}
    Insert(New(PButton, Init(R, '~В~вод', cmOk, bfDefault)));
    Inc(R.A.X, 12 * CW); Inc(R.B.X, 12 * CW);
    Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
 {$ELSE}
  {$IFDEF Russian}
    Insert(New(PButton, Init(R, '~В~вод', cmOk, bfDefault)));
    Inc(R.A.X, 12 * CW); Inc(R.B.X, 12 * CW);
    Insert(New(PButton, Init(R, 'Отказ', cmCancel, bfNormal)));
  {$ELSE}
    Insert(New(PButton, Init(R, 'O~K~', cmOk, bfDefault)));
    Inc(R.A.X, 12 * CW); Inc(R.B.X, 12 * CW);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
    {Inc(R.A.X, 12 * CW); Inc(R.B.X, 12 * CW);} {SHIM}
    SelectNext(False);
  end;
  Dialog^.SetData(S);
  C := DeskTop^.ExecView(Dialog);
  if C <> cmCancel then Dialog^.GetData(S);
  Dispose(Dialog, Done);
  InputBoxRect := C;
end;

{$IFDEF UseStandardBitMaps}

{$L _SIGNS.OBJ}
procedure Mark_Exclamation;   External;
procedure Mark_Information;   External;
procedure Mark_Question;      External;
procedure Mark_RedArm;        External;

(*{$L EXCL.OBJ}
procedure Mark_Exclamation_Xor; external;
procedure Mark_Exclamation_And; external;

{$L INFO.OBJ}
procedure Mark_Information_Xor; external;
procedure Mark_Information_And; external;

{$L QUEST.OBJ}
procedure Mark_Question_Xor; external;
procedure Mark_Question_And; external;

{$L REDARM.OBJ}
procedure Mark_RedArm_Xor; external;
procedure Mark_RedArm_And; external;*)

procedure RegisterMarkImages;
begin
  RegisterImageInCode(201, @Mark_Exclamation);
  RegisterImageInCode(203, @Mark_Information);
  RegisterImageInCode(205, @Mark_Question);
  RegisterImageInCode(207, @Mark_RedArm);
end;

procedure RegisterMsgBoxImages;
begin
  RegisterMarkImages;
end;

{$ELSE}

procedure RegisterMsgBoxImages;
begin
  {Does nothing now}
end;

{$ENDIF}

Var
  SaveNotify : TSysNotifyProc;

Procedure MsgBoxNotifyProc(EventWhat, EventCommand : Word; EventInfo : Pointer); Far;
Begin
  SaveNotify(EventWhat, EventCommand, EventInfo);
  if (EventWhat = snUser) and (Application <> Nil) then begin
    case EventCommand of
    unError        : MessageBox(PString(EventInfo)^, nil, mfError + mfOkButton);
    unInformation  : MessageBox(PString(EventInfo)^, nil, mfInformation + mfOkButton);
    unConfirmation : MessageBox(PString(EventInfo)^, nil, mfConfirmation + mfOkButton);
    unWarning      : MessageBox(PString(EventInfo)^, nil, mfWarning + mfOkButton);
    else end;
  end;
End;


begin
  RegisterMsgBoxImages;
  SaveNotify    := SysNotifyProc;
  SysNotifyProc := MsgBoxNotifyProc;
end.
