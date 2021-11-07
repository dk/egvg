uses WavePlay, Blaster, Speaker, EgString;

const
  drv:byte=sdSoundBlaster;


begin
  if ParamCount = 0 then begin
    WriteLn('DK Inc. 1996 Wave file player.');
    WriteLn('Usage : PLAYWAVE FILENAME [SPEAKER, COVOX or BLASTER]');
    Exit;
  end;
  if StUpcase(ParamStr(2)) = 'SPEAKER' then drv := sdPCSpeaker;
  if StUpcase(ParamStr(2)) = 'COVOX' then drv := sdCovox;
  if ParamCount = 0 then Exit;
  SelectSoundDriver(GetSoundDriver(drv));
  InitSound;
  PlayWavFile(ParamStr(1), True);
  repeat
    if (Mem[Seg0040:$1a] <> Mem[Seg0040:$1c]) or not PlayingSound then Break;
  until false;
  DoneSound;
  Mem[Seg0040:$1a] := Mem[Seg0040:$1c]
end.
