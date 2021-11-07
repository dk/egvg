{
  DK Inc. 1997 for SVG GDI
}
Unit JPGImp;

Interface

Uses Objects, DIB, GDI, Streams, Bitmaps, Drivers, EShield;

Function LoadJPG(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;

Implementation

{$DEFINE STREAM <- this for FILEPtr = PStream}
{$I jconfig.inc}

Uses JMoreCfg, JPeGLib, JError, RdColMap, JDefErr, JInclude,
     JDAPIMin, JDAPIStd, JDataSrc, CDJPeG, JComAPI, JDMaster;

type
  mem_dest_ptr = ^mem_dest_struct;
  mem_dest_struct = record
    pub : djpeg_dest_struct;    { public fields }
    data_width : JDIMENSION;        { JSAMPLEs per row }
    row_width : JDIMENSION;         { physical width of one row in the BMP file }
    pad_bytes : int;                { number of padding bytes needed per row }
    cur_output_row : JDIMENSION;    { next row# to write to virtual array }
    lp : plogpalette;
  end;

{ Forward declarations }
{LOCAL}
procedure write_colormap(cinfo : j_decompress_ptr;
                         dest : mem_dest_ptr;
                         map_colors : int;
                         map_entry_size : int); forward;

procedure put_pixel_rows (cinfo : j_decompress_ptr;
                          dinfo : djpeg_dest_ptr;
                          rows_supplied : JDIMENSION); far;
{ This version is for reading 24-bit pixels and writing 16-bit}
var
  dest : mem_dest_ptr;
  image_ptr : JSAMPARRAY;
  {register} inptr : JSAMPLE_PTR;
  outfile : FILEptr;
begin
  dest := mem_dest_ptr (dinfo);

  Inc(dest^.cur_output_row);
  outfile := dest^.pub.output_file;

  inptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);

  reversergb(inptr, cinfo^.output_width);
  impacttruehi(PByteArray(inptr), PByteArray(inptr), cinfo^.output_width, 0);
  JFWRITE(outfile^, inptr, cinfo^.output_width + cinfo^.output_width);
end;


{METHODDEF}
procedure put_gray_rows (cinfo : j_decompress_ptr;
                         dinfo : djpeg_dest_ptr;
                         rows_supplied : JDIMENSION); far;
{ This version is for grayscale OR quantized color output to
write 8-bit or lower}
var
  dest : mem_dest_ptr;
  inptr : JSAMPLE_PTR;
  pad : int;
  outfile : FILEptr;
begin
  dest := mem_dest_ptr (dinfo);
  outfile := dest^.pub.output_file;
  Inc(dest^.cur_output_row);
  inptr := JSAMPLE_PTR(dest^.pub.buffer^[0]);
  case cinfo^.actual_number_of_colors of
  2  : begin
    ImpactMono(PByteArray(inptr), PByteArray(inptr), cinfo^.output_width, 0, nil);
    JFWRITE(outfile^, inptr, dest^.data_width shr 3 + Byte((dest^.data_width and 7) <> 0));
  end;
  16 : begin
    Impact16(PByteArray(inptr), PByteArray(inptr), cinfo^.output_width, 0, nil);
    JFWRITE(outfile^, inptr, dest^.data_width shr 1 + dest^.data_width and 1);
  end;
  else JFWRITE(outfile^, inptr, dest^.data_width);
  end;
end;



procedure start_output_mem (cinfo : j_decompress_ptr;
                            dinfo : djpeg_dest_ptr); far;
begin
end;

procedure write_mem_header (cinfo : j_decompress_ptr; dest : mem_dest_ptr);
var
  cmap_entries, bits_per_pixel : int;
begin
  { Compute colormap size and total file size }
  if (cinfo^.out_color_space = JCS_RGB) then begin
    if (cinfo^.quantize_colors) then begin
      { Colormapped RGB }
      bits_per_pixel := 8;
      cmap_entries := 256;
    end else begin
      { Unquantized, full color RGB }
      bits_per_pixel := 24;
      cmap_entries := 0;
    end;
  end else begin
    { Grayscale output.  We need to fake a 256-entry colormap. }
    bits_per_pixel := 8;
    cmap_entries := 256;
  end;
  if cmap_entries > 0 then write_colormap(cinfo, dest, cmap_entries, 4)
  else dest^.lp^.colors := 0;
end;



{LOCAL}
procedure write_colormap (cinfo : j_decompress_ptr;
                          dest : mem_dest_ptr;
                          map_colors : int;
                          map_entry_size : int);
var
  colormap : JSAMPARRAY;
  num_colors : int;
  outfile : FILEptr;
  i : int;
  deMono : Boolean;
var
  output_color_map : Array[0..255] of BGRtype;
  output_ext_color_map : Array[0..255] of record
                                            b,g,r,a : byte;
                                          end;
begin
  colormap := cinfo^.colormap;
  num_colors := cinfo^.actual_number_of_colors;
  outfile := dest^.pub.output_file;

  if (colormap <> NIL) then begin
    if (cinfo^.out_color_components = 3) then begin
      { Normal case with RGB colormap }
      if (map_entry_size = 4) then
      for i := 0 to pred(num_colors) do with output_ext_color_map[i] do begin
        b := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
        g := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
        r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        a := 0;
      end else for i := 0 to pred(num_colors) do with output_color_map[i] do begin
        b := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
        g := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
        r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
      end;
    end else begin
      { Grayscale colormap (only happens with grayscale quantization) }
      if (map_entry_size = 4) then
      for i := 0 to pred(num_colors) do with output_ext_color_map[i] do begin
        b := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        g := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        a := 0;
      end else for i := 0 to pred(num_colors) do with output_color_map[i] do begin
        b := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        g := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
        r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
      end;
    end;
    i := num_colors;
  end else begin
    { If no colormap, must be grayscale data.  Generate a linear "map". }
    { Nomssi: do not use "num_colors" here, it should be 0 }
    if (map_entry_size = 4) then
      for i := 0 to pred(256) do with output_ext_color_map[i] do begin
        b := i;
        g := i;
        r := i;
        a := 0;
      end else for i := 0 to pred(256) do with output_color_map[i] do begin
        b := i;
        g := i;
        r := i;
      end;
    i := 256;
  end;
  { Pad colormap with zeros to ensure specified number of colormap entries }

  if (i > map_colors) then
    ERREXIT1(j_common_ptr(cinfo), JERR_TOO_MANY_COLORS, i);
  while (i < map_colors) do begin
    if (map_entry_size = 4) then with output_ext_color_map[i] do begin
      b := 0;
      g := 0;
      r := 0;
      a := 0;
    end else with output_color_map[i] do begin
      b := 0;
      g := 0;
      r := 0;
    end;
    Inc(i);
  end;

  CreatePalette(@output_ext_color_map, dest^.lp^,
    cbwInit + cbwQuad + cbwReverse + cbwMonoFix, map_colors);
end;


{METHODDEF}
procedure finish_output_mem (cinfo : j_decompress_ptr;
                             dinfo : djpeg_dest_ptr); far;
Begin
  write_mem_header(cinfo, mem_dest_ptr (dinfo));
End;

{GLOBAL}
function jinit_write_mem (cinfo : j_decompress_ptr; lp : plogpalette) : djpeg_dest_ptr;
var
  dest : mem_dest_ptr;
  row_width : JDIMENSION;
var
  progress : cd_progress_ptr;
begin
  { Create module interface object, fill in method pointers }
  dest := mem_dest_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(mem_dest_struct)) );
  dest^.pub.start_output := start_output_mem;
  dest^.pub.finish_output := finish_output_mem;
  dest^.lp := lp;

  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
  begin
    dest^.pub.put_pixel_rows := put_gray_rows;
  end
  else
    if (cinfo^.out_color_space = JCS_RGB) then
    begin
      if (cinfo^.quantize_colors) then
        dest^.pub.put_pixel_rows := put_gray_rows
      else
        dest^.pub.put_pixel_rows := put_pixel_rows;
  end
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_COLORSPACE);

  { Calculate output image dimensions so we can allocate space }
  jpeg_calc_output_dimensions(cinfo);

  { Determine width of rows in the BMP file (padded to 4-byte boundary). }
  row_width := cinfo^.output_width * cinfo^.output_components;
  dest^.data_width := row_width;
  while ((row_width and 3) <> 0) do
    Inc(row_width);
  dest^.row_width := row_width;
  dest^.pad_bytes := int (row_width - dest^.data_width);

  dest^.cur_output_row := 0;
  if (cinfo^.progress <> NIL) then
  begin
    progress := cd_progress_ptr (cinfo^.progress);
    Inc(progress^.total_extra_passes); { count file input as separate pass }
  end;

  { Create decompressor output buffer. }
  dest^.pub.buffer := cinfo^.mem^.alloc_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE, row_width, JDIMENSION (1));
  dest^.pub.buffer_height := 1;

  jinit_write_mem := djpeg_dest_ptr(dest);
end;


var
  je : TJump;

Procedure output_message (cinfo : j_common_ptr); far;
var
  buffer : string; {[JMSG_LENGTH_MAX];}
begin
  cinfo^.err^.format_message (cinfo, buffer);
  buffer := 'JPEG System: '+buffer;
  { Let the memory manager delete any temp files before we die }
  jpeg_destroy(cinfo);
  SystemNotify(snUser, unError, @buffer);
  jump(je, 0);
end;

procedure error_exit (cinfo : j_common_ptr); far;
begin
  { Always display the message }
  cinfo^.err^.output_message(cinfo);
end;

procedure emit_message (cinfo : j_common_ptr; msg_level : int); far;
var
  err : jpeg_error_mgr_ptr;
begin
  err := cinfo^.err;
  if (msg_level < 0) then
  begin
    if (err^.num_warnings = 0) or (err^.trace_level >= 3) then
      err^.output_message(cinfo);
    { Always count warnings in num_warnings. }
    Inc( err^.num_warnings );
  end
  else
  begin
    { It's a trace message.  Show it if trace_level >= msg_level. }
    if (err^.trace_level >= msg_level) then
      err^.output_message (cinfo);
  end;
end;


procedure format_message (cinfo : j_common_ptr; var buffer : string); far;
var
  err : jpeg_error_mgr_ptr;
  msg_code : J_MESSAGE_CODE;
  msgtext : string;
  isstring : boolean;
begin
  err := cinfo^.err;
  msg_code := J_MESSAGE_CODE(err^.msg_code);
  msgtext := '';

  { Look up message string in proper table }
  if (msg_code > JMSG_NOMESSAGE)
    and (msg_code <= J_MESSAGE_CODE(err^.last_jpeg_message)) then
  begin
    msgtext := err^.jpeg_message_table^[msg_code];
  end
  else
  if (err^.addon_message_table <> NIL) and
     (msg_code >= err^.first_addon_message) and
     (msg_code <= err^.last_addon_message) then
  begin
    msgtext := err^.addon_message_table^[J_MESSAGE_CODE
           (ord(msg_code) - ord(err^.first_addon_message))];
  end;

  { Defend against bogus message number }
  if (msgtext = '') then
  begin
    err^.msg_parm.i[0] := int(msg_code);
    msgtext := err^.jpeg_message_table^[JMSG_NOMESSAGE];
  end;

  { Check for string parameter, as indicated by %s in the message text }
  isstring := Pos('%s', msgtext) > 0;

  { Format the message into the passed buffer }
  if (isstring) then
    buffer := Concat(msgtext, err^.msg_parm.s)
  else
  begin
 {$IFDEF VER70}
    FormatStr(buffer, msgtext, err^.msg_parm.i);
 {$ELSE}
   {$IFDEF NO_FORMAT}
   buffer := msgtext;
   {$ELSE}
   buffer := Format(msgtext, [
        err^.msg_parm.i[0], err^.msg_parm.i[1],
        err^.msg_parm.i[2], err^.msg_parm.i[3],
        err^.msg_parm.i[4], err^.msg_parm.i[5],
        err^.msg_parm.i[6], err^.msg_parm.i[7] ]);
   {$ENDIF}
 {$ENDIF}
  end;
end;

procedure reset_error_mgr (cinfo : j_common_ptr); far;
begin
  cinfo^.err^.num_warnings := 0;
  { trace_level is not reset since it is an application-supplied parameter }
  cinfo^.err^.msg_code := 0;      { may be useful as a flag for "no error" }
end;


Function jpeg_notify_error (var err : jpeg_error_mgr) : jpeg_error_mgr_ptr;
Begin
  err.error_exit := error_exit;
  err.emit_message := emit_message;
  err.output_message := output_message;
  err.format_message := format_message;
  err.reset_error_mgr := reset_error_mgr;

  err.trace_level := 0;         { default := no tracing }
  err.num_warnings := 0;        { no warnings emitted yet }
  err.msg_code := 0;            { may be useful as a flag for "no error" }

  { Initialize message table pointers }
  err.jpeg_message_table := @jpeg_std_message_table;
  err.last_jpeg_message := pred(JMSG_LASTMSGCODE);

  err.addon_message_table := NIL;
  err.first_addon_message := JMSG_NOMESSAGE;  { for safety }
  err.last_addon_message := JMSG_NOMESSAGE;

  jpeg_notify_error := @err;
End;


Function jpeg_getc (cinfo : j_decompress_ptr) : char;
{ Read next byte }
Var
  datasrc : jpeg_source_mgr_ptr;
Begin
  datasrc := cinfo^.src;
  if (datasrc^.bytes_in_buffer = 0) then begin
    if (not datasrc^.fill_input_buffer (cinfo)) then
      ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
  end;
  Dec(datasrc^.bytes_in_buffer);
  jpeg_getc := char(GETJOCTET(datasrc^.next_input_byte^));
  Inc(datasrc^.next_input_byte);
End;

procedure progress_monitor (cinfo : j_common_ptr); far;
var
  prog : cd_progress_ptr;
  total_passes : int;
  percent_done : int;
begin
  prog := cd_progress_ptr (cinfo^.progress);
  total_passes := prog^.pub.total_passes + prog^.total_extra_passes;
  percent_done := round (prog^.pub.pass_counter*100/prog^.pub.pass_limit);
  if (percent_done <> prog^.percent_done) then begin
    prog^.percent_done := percent_done;
    if (total_passes > 1) then
      SystemNotify(snUser, unPercentAux, Pointer(LongInt(total_passes) shl 16 + prog^.pub.completed_passes + 1));
    SystemNotify(snUser, unPercent, Pointer(LongInt(percent_done)));
  end;
end;


procedure start_progress_monitor (cinfo : j_common_ptr;
                                  progress : cd_progress_ptr);
begin
  if (cinfo^.err^.trace_level = 0) then begin
    progress^.pub.progress_monitor := progress_monitor;
    progress^.completed_extra_passes := 0;
    progress^.total_extra_passes := 0;
    progress^.percent_done := -1;
    cinfo^.progress := @progress^.pub;
    SystemNotify(snUser, unPercent, Pointer(LongInt(0)));
  end;
end;


procedure end_progress_monitor (cinfo : j_common_ptr);
begin
  SystemNotify(snUser, unPercent, Pointer(LongInt(100)));
end;

Function LoadJPG(FileName: FNameStr; AID: Integer; LP : PLogPalette; ColorType : Word) : Boolean;
Var
  TLP : TLogPalette;

  cinfo         : jpeg_decompress_struct;
  jerr          : jpeg_error_mgr;
  progress      : cdjpeg_progress_mgr;
  dest_mgr      : djpeg_dest_ptr;
  num_scanlines : JDIMENSION;

  input_file    : PBufStream;
  output_file   : PBufStream;

  p : PImage;
Begin
  LoadJPG := False;
  P := Nil;
  if LP = Nil then LP := @TLP;
  dest_mgr := NIL;
  { Initialize the JPEG decompression object with default error handling. }
  cinfo.err := jpeg_notify_error(jerr);
  if SetJump(JE) then begin
    jpeg_create_decompress(@cinfo);
    { Add some application-specific error messages (from cderror.h) }
    {jerr.addon_message_table := cdjpeg_message_table;}
    jerr.first_addon_message := JMSG_FIRSTADDONCODE;
    jerr.last_addon_message := JMSG_LASTADDONCODE;
    new(input_file, Init(FileName, stOpen, 2048));
    if (input_file^.status <> 0) then begin
      Dispose(input_file, Done);
      Exit;
    end;
    start_progress_monitor(j_common_ptr (@cinfo), @progress);
    { Specify data source for decompression }
    jpeg_stdio_src(@cinfo, input_file);
    { Read file header, set default decompression parameters }
    jpeg_read_header(@cinfo, TRUE);
    case ColorType of
    imNone : ColorType := imHiColor;
    imMono : begin
      cinfo.desired_number_of_colors := 2;
      cinfo.quantize_colors := TRUE;
    end;
    im16 : begin
      cinfo.desired_number_of_colors := 16;
      cinfo.quantize_colors := TRUE;
    end;
    im256 : begin
      cinfo.desired_number_of_colors := 256;
      cinfo.quantize_colors := TRUE;
    end;
    else ColorType := imHiColor; end;

    dest_mgr := jinit_write_mem(@cinfo, lp);

    if (cinfo.out_color_space <> JCS_RGB) and
       (cinfo.out_color_space <> JCS_GRAYSCALE) then
      ERREXIT(j_common_ptr(@cinfo), JERR_BMP_COLORSPACE);
    if (cinfo.out_color_space = JCS_GRAYSCALE) and (ColorType > im256) then
      ColorType := im256;
    P := CreateDImageIndirect(cinfo.output_width, cinfo.output_height, ColorType, cbwSetAlloc);
    if P = Nil then Exit;
    dest_mgr^.output_file := PSImage(P)^.PS;
    { Start decompressor }
    jpeg_start_decompress(@cinfo);
    { Write output file header }
    dest_mgr^.start_output (@cinfo, dest_mgr);
    { Process data }
    while (cinfo.output_scanline < cinfo.output_height) do begin
      num_scanlines := jpeg_read_scanlines(@cinfo, dest_mgr^.buffer,
                                          dest_mgr^.buffer_height);
      dest_mgr^.put_pixel_rows (@cinfo, dest_mgr, num_scanlines);
    end;
    progress.pub.completed_passes := progress.pub.total_passes;
    { Finish decompression and release memory.
      I must do it in this order because output module has allocated memory
      of lifespan JPOOL_IMAGE; it needs to finish before releasing memory. }
    dest_mgr^.finish_output (@cinfo, dest_mgr);
    jpeg_finish_decompress(@cinfo);
    jpeg_destroy_decompress(@cinfo);
    dispose(input_file, done);
    end_progress_monitor(j_common_ptr (@cinfo));
  end else begin
    if LP = @TLP then DisposePalette(TLP);
    if P <> Nil then FreeDImage(P);
    Exit;
  end;
  if LP = @TLP then DisposePalette(TLP);
  RegisterImageInHeap(AID, P);
  LoadJPG := True;
End;

End.