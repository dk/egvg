Unit RdSwitch;

{ This file contains routines to process some of cjpeg's more complicated
  command-line switches.  Switches processed here are:
 	-qtables file		Read quantization tables from text file
 	-scans file		Read scan script from text file
 	-qslots N[,N,...]	Set component quantization table selectors
 	-sample HxV[,HxV,...]	Set component sampling factors  }

{ Original: rdswitch.c ; Copyright (C) 1991-1996, Thomas G. Lane.   }


interface

{$I jconfig.inc}

uses
  cdjpeg,		{ Common decls for cjpeg/djpeg applications }
  {ctype,}		{ to declare isdigit(), isspace() }
  jmorecfg,
  jcparam,
  jpeglib;

{GLOBAL}
function set_quant_slots (cinfo : j_compress_ptr; arg : PChar) : boolean;
{ Process a quantization-table-selectors parameter string, of the form
      N[,N,...]
  If there are more components than parameters, the last value is replicated.
 }


{GLOBAL}
function set_sample_factors (cinfo : j_compress_ptr;
                             arg : PChar) : boolean;
{ Process a sample-factors parameter string, of the form
      HxV[,HxV,...]
  If there are more components than parameters, "1x1" is assumed for the rest.
 }

{GLOBAL}
function read_quant_tables (cinfo : j_compress_ptr;
                            const filename : string;
		            scale_factor : int;
                            force_baseline : boolean) : boolean;

{ Read a set of quantization tables from the specified file.
  The file is plain ASCII text: decimal numbers with whitespace between.
  Comments preceded by '#' may be included in the file.
  There may be one to NUM_QUANT_TBLS tables in the file, each of 64 values.
  The tables are implicitly numbered 0,1,etc.
  NOTE: does not affect the qslots mapping, which will default to selecting
  table 0 for luminance (or primary) components, 1 for chrominance components.
  You must use -qslots if you want a different component->table mapping. }

{GLOBAL}
function read_scan_script (cinfo : j_compress_ptr;
                           const filename : string) : boolean;
{ Read a scan script from the specified text file.
  Each entry in the file defines one scan to be emitted.
  Entries are separated by semicolons ';'.
  An entry contains one to four component indexes,
  optionally followed by a colon ':' and four progressive-JPEG parameters.
  The component indexes denote which component(s) are to be transmitted
  in the current scan.  The first component has index 0.
  Sequential JPEG is used if the progressive-JPEG parameters are omitted.
  The file is free format text: any whitespace may appear between numbers
  and the ':' and ';' punctuation marks.  Also, other punctuation (such
  as commas or dashes) can be placed between numbers if desired.
  Comments preceded by '#' may be included in the file.
  Note: we do very little validity checking here;
  jcmaster.c will validate the script parameters. }

implementation

{$IFDEF OK}
{LOCAL}
function text_getc (var f : FILE) : int;
{ Read next char, skipping over any comments (# to end of line) }
{ A comment/newline sequence is returned as a newline }
var
  ch : int; {register }
begin
  ch := getc(file);
  if (ch = '#') then
  begin
    repeat
      ch := getc(file);
    Until (ch = #13) or (ch = EOF);
  end;
  text_getc := ch;
end;

{LOCAL}
function read_text_integer (var f : FILE;
                            var result : long;
                            var termchar : int) : boolean;
{ Read an unsigned decimal integer from a file, store it in result }
{ Reads one trailing character after the integer; returns it in termchar }
var
  {register} ch : int;
  {register} val : long;
begin
  { Skip any leading whitespace, detect EOF }
  repeat
    ch := text_getc(f);
    if (ch = EOF) then
    begin
      termchar := ch;
      read_text_integer := FALSE;
      exit;
    end;
  Until not (isspace(ch));

  if (not isdigit(ch)) then
  begin
    termchar := ch;
    read_text_integer := FALSE;
    exit;
  end;

  val := ch - '0';
  while ((ch := text_getc(file)) <> EOF) do
  begin
    if (not isdigit(ch)) then
      break;
    val := val * 10;
    Inc(val, ch - '0');
  end;
  result := val;
  termchar := ch;
  read_text_integer := TRUE;
end;
{$ELSE}
function text_getc (var f : FILE) : int;
begin
end;


{LOCAL}
function read_text_integer (var f : text;
                            var result : long;
                            var termchar : int) : boolean;
begin
  if EoLn(f) then
    ReadLn(f);
  if not Eof(f) then
  begin
    Read(f, result);
    read_text_integer := TRUE;
  end
  else
    read_text_integer := FALSE;
end;
{$ENDIF}

{GLOBAL}
function read_quant_tables (cinfo : j_compress_ptr;
                            const filename : string;
		            scale_factor : int;
                            force_baseline : boolean) : boolean;
{ Read a set of quantization tables from the specified file.
  The file is plain ASCII text: decimal numbers with whitespace between.
  Comments preceded by '#' may be included in the file.
  There may be one to NUM_QUANT_TBLS tables in the file, each of 64 values.
  The tables are implicitly numbered 0,1,etc.
  NOTE: does not affect the qslots mapping, which will default to selecting
  table 0 for luminance (or primary) components, 1 for chrominance components.
  You must use -qslots if you want a different component->table mapping. }
var
  fp : text;
  tblno, i, termchar : int;
  val : long;
  table : array[0..DCTSIZE2-1] of uInt;
begin
  Assign(fp,filename);

  {$I-}
  Reset(fp);
  {$IFDEF IoCheck} {$I+} {$ENDIF}
  if (IOresult <> 0) then
  begin
    WriteLn(output, 'Can''t open table file ', filename);
    read_quant_tables := FALSE;
    exit;
  end;
  tblno := 0;

  while (read_text_integer(fp, val, termchar)) do
  begin { read 1st element of table }
    if (tblno >= NUM_QUANT_TBLS) then
    begin
      WriteLn(output, 'Too many tables in file ', filename);
      close(fp);
      read_quant_tables := FALSE;
      exit;
    end;
    table[0] := uInt (val);
    for i := 1 to pred(DCTSIZE2) do
    begin
      if (not read_text_integer(fp, val, termchar)) then
      begin
	WriteLn(output, 'Invalid table data in file ', filename);
        close(fp);
	read_quant_tables := FALSE;
        exit;
      end;
      table[i] := uInt (val);
    end;
    jpeg_add_quant_table(cinfo, tblno, table, scale_factor, force_baseline);
    Inc(tblno);
  end;

  {$IFDEF OK}
  if (termchar <> EOF) then
  begin
    WriteLn(output, 'Non-numeric data in file ', filename);
    close(fp);
    read_quant_tables := FALSE;
    exit;
  end;
  {$ENDIF}

  close(fp);
  read_quant_tables := TRUE;
end;


{$ifdef C_MULTISCAN_FILES_SUPPORTED}

{LOCAL}
function read_scan_integer (var f : text;
                            var result : long;
                            var termchar : int) : boolean;
{ Variant of read_text_integer that always looks for a non-space termchar;
  this simplifies parsing of punctuation in scan scripts. }
var
  ch : int; { register }
begin
  if (not read_text_integer(f, result, termchar)) then
  begin
    read_scan_integer := FALSE;
    exit;
  end;
  ch := termchar;
  while not EOF(f) and (isspace(ch)) do
    ch := text_getc(f);
  if (isdigit(ch)) then
  begin		{ oops, put it back }
    if (ungetc(ch, f) = EOF) then
    begin
      read_scan_integer := FALSE;
      exit;
    end;
    ch := ' ';
  end
  else
  begin
    { Any separators other than ';' and ':' are ignored;
      this allows user to insert commas, etc, if desired. }

    if (ch <> EOF) and (ch <> ';') and (ch <> ':') then
      ch := ' ';
  end;
  termchar := ch;
  read_scan_integer := TRUE;
end;


{GLOBAL}
function read_scan_script (cinfo : j_compress_ptr, char * filename) : boolean;
{ Read a scan script from the specified text file.
  Each entry in the file defines one scan to be emitted.
  Entries are separated by semicolons ';'.
  An entry contains one to four component indexes,
  optionally followed by a colon ':' and four progressive-JPEG parameters.
  The component indexes denote which component(s) are to be transmitted
  in the current scan.  The first component has index 0.
  Sequential JPEG is used if the progressive-JPEG parameters are omitted.
  The file is free format text: any whitespace may appear between numbers
  and the ':' and ';' punctuation marks.  Also, other punctuation (such
  as commas or dashes) can be placed between numbers if desired.
  Comments preceded by '#' may be included in the file.
  Note: we do very little validity checking here;
  jcmaster.c will validate the script parameters. }
label
  bogus;
var
  fp : FILE;
  scanno, ncomps, termchar : int;
  val : long;
  scanptr : jpeg_scan_info_ptr;
const
  MAX_SCANS := 100;		{ quite arbitrary limit }
var
  scans[MAX_SCANS] of jpeg_scan_info;
begin
  Assign(fp,filename);
  {$I-}
  Reset(f);
  {$IFDEF IoCheck} {$I+} {$ENDIF}
  if (IOresult <> 0) then
  begin
    WriteLn(stderr, 'Can''t open scan definition file ', filename);
    read_scan_script := FALSE;
    exit;
  end;
  scanptr := scans;
  scanno := 0;

  while (read_scan_integer(fp, &val, &termchar)) do
  begin
    if (scanno >= MAX_SCANS) then
    begin
      WriteLn(output, 'Too many scans defined in file ', filename);
      fclose(fp);
      read_scan_script := FALSE;
      exit;
    end;
    scanptr^.component_index[0] := (int) val;
    ncomps := 1;
    while (termchar = ' ') do
    begin
      if (ncomps >= MAX_COMPS_IN_SCAN) then
      begin
	WriteLn(output, 'Too many components in one scan in file ',
		filename);
	close(fp);
	read_scan_script := FALSE;
        exit;
      end;
      if (not read_scan_integer(fp, &val, &termchar)) then
	goto bogus;
      scanptr^.component_index[ncomps] := int (val);
      Inc(ncomps);
    end;
    scanptr^.comps_in_scan := ncomps;
    if (termchar = ':') then
    begin
      if (not read_scan_integer(fp, &val, &termchar)) or (termchar <> ' ') then
	goto bogus;
      scanptr^.Ss := int (val);
      if (not read_scan_integer(fp, &val, &termchar)) or (termchar <> ' ') then
	goto bogus;
      scanptr^.Se := int (val);
      if (not read_scan_integer(fp, &val, &termchar)) or (termchar <> ' ') then
	goto bogus;
      scanptr^.Ah := int (val);
      if (not read_scan_integer(fp, &val, &termchar)) then
	goto bogus;
      scanptr^.Al := int (val);
    end
    else
    begin
      { set non-progressive parameters }
      scanptr^.Ss := 0;
      scanptr^.Se := DCTSIZE2-1;
      scanptr^.Ah := 0;
      scanptr^.Al := 0;
    end;
    if (termchar <> ';') and (termchar <> EOF) then
    begin
bogus:
      WriteLn(output, 'Invalid scan entry format in file ', filename);
      close(fp);
      read_scan_script := FALSE;
      exit;
    end;
    Inc(scanptr);
    Inc(scanno);
  end;

  if (termchar <> EOF) then
  begin
    WriteLn(output, "Non-numeric data in file %s\n", filename);
    close(fp);
    read_scan_script := FALSE;
    exit;
  end;

  if (scanno > 0) then
  begin
    { Stash completed scan list in cinfo structure.
      NOTE: for cjpeg's use, JPOOL_IMAGE is the right lifetime for this data,
      but if you want to compress multiple images you'd want JPOOL_PERMANENT. }
    scanptr := jpeg_scan_info_ptr (
       cinfo^.mem^.alloc_small ( j_common_ptr(cinfo), JPOOL_IMAGE,
				  scanno * SIZEOF(jpeg_scan_info)) );
    MEMCOPY(scanptr, scans, scanno * SIZEOF(jpeg_scan_info));
    cinfo^.scan_info := scanptr;
    cinfo^.num_scans := scanno;
  end;

  close(fp);
  read_scan_script := TRUE;
end;

{$endif} { C_MULTISCAN_FILES_SUPPORTED }


{GLOBAL}
function set_quant_slots (cinfo : j_compress_ptr; arg : PChar) : boolean;
{ Process a quantization-table-selectors parameter string, of the form
      N[,N,...]
  If there are more components than parameters, the last value is replicated.
 }
var
  val : int;	{ default table # }
  ci : int;
  ch : char;
begin
  val := 0;
  for ci := 0 to pred(MAX_COMPONENTS) do
  begin
    if ( *arg) then
    begin
      ch := ',';		{ if not set by sscanf, will be ',' }
      if (sscanf(arg, '%d%c', &val, &ch) < 1) then
      begin
        set_quant_slots := FALSE;
        exit;
      end;
      if (ch <> ',') then	{ syntax check }
      begin
        set_quant_slots := FALSE;
        exit;
      end;
      if (val < 0) or (val >= NUM_QUANT_TBLS) then
      begin
	WriteLn(output, 'JPEG quantization tables are numbered 0..',
		NUM_QUANT_TBLS-1);
	set_quant_slots := FALSE;
        exit;
      end;
      cinfo^.comp_info[ci].quant_tbl_no := val;
      while (arg^) and (arg^ <> ',') do { advance to next segment of arg string }
	Inc(arg);
    end
    else
    begin
      { reached end of parameter, set remaining components to last table }
      cinfo^.comp_info[ci].quant_tbl_no := val;
    end;
  end;
  set_quant_slots := TRUE;
end;


{GLOBAL}
function set_sample_factors (cinfo : j_compress_ptr; arg : PChar) : boolean;
{ Process a sample-factors parameter string, of the form
      HxV[,HxV,...]
  If there are more components than parameters, "1x1" is assumed for the rest.
 }
var
  ci, val1, val2 : int;
  ch1, ch2 : char;
begin
  for ci := 0 to pred(MAX_COMPONENTS) do
  begin
    if ( *arg <> 0) then
    begin
      ch2 := ',';		{ if not set by sscanf, will be ',' }
      if (sscanf(arg, "%d%c%d%c", &val1, &ch1, &val2, &ch2) < 3) then
      begin
	set_sample_factors := FALSE;
        exit;
      end;
      if ((ch1 <> 'x' && ch1 <> 'X') || ch2 <> ',') then { syntax check }
      begin
	set_sample_factors := FALSE;
        exit;
      end;
      if (val1 <= 0) or (val1 > 4) or (val2 <= 0) or (val2 > 4) then
      begin
	WriteLn(output, 'JPEG sampling factors must be 1..4');
	set_sample_factors := FALSE;
        exit;
      end;
      cinfo^.comp_info[ci].h_samp_factor := val1;
      cinfo^.comp_info[ci].v_samp_factor := val2;
      while (arg^ <> 0) and (arg^ <> ',') do { advance to next segment of arg string }
	Inc(arg);
    end;
    else
    begin
      { reached end of parameter, set remaining components to 1x1 sampling }
      cinfo^.comp_info[ci].h_samp_factor := 1;
      cinfo^.comp_info[ci].v_samp_factor := 1;
    end;
  end;
  set_sample_factors := TRUE;
end;

end.