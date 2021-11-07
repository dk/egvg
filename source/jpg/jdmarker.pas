Unit JdMarker;

{ This file contains routines to decode JPEG datastream markers.
  Most of the complexity arises from our desire to support input
  suspension: if not all of the data for a marker is available;
  we must exit back to the application. On resumption; we reprocess
  the marker. }

{ Original: jdmarker.c;  Copyright (C) 1991-1996; Thomas G. Lane. }
{ History
   9.7.96                   Conversion to pascal started      jnn }


interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jcomapi,
  jpeglib;

const	                { JPEG marker codes }
  M_SOF0  = $c0;
  M_SOF1  = $c1;
  M_SOF2  = $c2;
  M_SOF3  = $c3;
  
  M_SOF5  = $c5;
  M_SOF6  = $c6;
  M_SOF7  = $c7;
  
  M_JPG   = $c8;
  M_SOF9  = $c9;
  M_SOF10 = $ca;
  M_SOF11 = $cb;

  M_SOF13 = $cd;
  M_SOF14 = $ce;
  M_SOF15 = $cf;
  
  M_DHT   = $c4;
  
  M_DAC   = $cc;
  
  M_RST0  = $d0;
  M_RST1  = $d1;
  M_RST2  = $d2;
  M_RST3  = $d3;
  M_RST4  = $d4;
  M_RST5  = $d5;
  M_RST6  = $d6;
  M_RST7  = $d7;
  
  M_SOI   = $d8;
  M_EOI   = $d9;
  M_SOS   = $da;
  M_DQT   = $db;
  M_DNL   = $dc;
  M_DRI   = $dd;
  M_DHP   = $de;
  M_EXP   = $df;
  
  M_APP0  = $e0;
  M_APP1  = $e1;
  M_APP2  = $e2;
  M_APP3  = $e3;
  M_APP4  = $e4;
  M_APP5  = $e5;
  M_APP6  = $e6;
  M_APP7  = $e7;
  M_APP8  = $e8;
  M_APP9  = $e9;
  M_APP10 = $ea;
  M_APP11 = $eb;
  M_APP12 = $ec;
  M_APP13 = $ed;
  M_APP14 = $ee;
  M_APP15 = $ef;
  
  M_JPG0  = $f0;
  M_JPG13 = $fd;
  M_COM   = $fe;
  
  M_TEM   = $01;
  
  M_ERROR = $100;

type
  JPEG_MARKER = uint;        { JPEG marker codes }

{GLOBAL}
function jpeg_resync_to_restart(cinfo : j_decompress_ptr;
                                desired : int) : boolean;
{GLOBAL}
procedure jinit_marker_reader (cinfo : j_decompress_ptr);


implementation

uses
  jutils;

{ Macro used:
   TRACEMS
   TRACEMS1
   TRACEMS3
   ERREXIT
   ERREXIT1
   WARNMS2
   TRACEMS2
   GETJOCTET is better defined as a type = byte
  The error constants
}

{ At all times, cinfo1.src.next_input_byte and .bytes_in_buffer reflect
  the current restart point; we update them only when we have reached a
  suitable place to restart if a suspension occurs. }


{ Routines to process JPEG markers.

  Entry condition: JPEG marker itself has been read and its code saved
    in cinfo^.unread_marker; input restart point is just after the marker.

  Exit: if return TRUE, have read and processed any parameters, and have
    updated the restart point to point after the parameters.
    If return FALSE, was forced to suspend before reaching end of
    marker parameters; restart point has not been moved.  Same routine
    will be called again after application supplies more input data.

  This approach to suspension assumes that all of a marker's parameters can
  fit into a single input bufferload.  This should hold for "normal"
  markers.  Some COM/APPn markers might have large parameter segments,
  but we use skip_input_data to get past those, and thereby put the problem
  on the source manager's shoulders.

  Note that we don't bother to avoid duplicate trace messages if a
  suspension occurs within marker parameters.  Other side effects
  require more care. }

{LOCAL}
function get_soi (cinfo : j_decompress_ptr) : boolean;
{ Process an SOI marker }
var
  i : int;
begin
  TRACEMS(j_common_ptr(cinfo), 1, JTRC_SOI);

  if (cinfo^.marker^.saw_SOI) then
    ERREXIT(j_common_ptr(cinfo), JERR_SOI_DUPLICATE);

  { Reset all parameters that are defined to be reset by SOI }

  for i := 0 to Pred(NUM_ARITH_TBLS) do
  with cinfo^ do
  begin
    arith_dc_L[i] := 0;
    arith_dc_U[i] := 1;
    arith_ac_K[i] := 5;
  end;
  cinfo^.restart_interval := 0;

  { Set initial assumptions for colorspace etc }

  with cinfo^ do
  begin
    jpeg_color_space := JCS_UNKNOWN;
    CCIR601_sampling := FALSE; { Assume non-CCIR sampling??? }

    saw_JFIF_marker := FALSE;
    density_unit := 0;	{ set default JFIF APP0 values }
    X_density := 1;
    Y_density := 1;
    saw_Adobe_marker := FALSE;
    Adobe_transform := 0;

    marker^.saw_SOI := TRUE;
  end;
  get_soi := TRUE;
end; { get_soi }


{LOCAL}
function get_sof(cinfo : j_decompress_ptr;
                 is_prog : boolean;
                 is_arith : boolean) : boolean;
{ Process a SOFn marker }
var
  length : INT32;
  c, ci : int;
  compptr : jpeg_component_info_ptr;
{ Declare and initialize local copies of input pointer/count }
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;
{}
  cinfo^.progressive_mode := is_prog;
  cinfo^.arith_code := is_arith;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  { Read a byte into variable cinfo^.data_precision.
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.data_precision := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  cinfo^.image_height should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.image_height := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( cinfo^.image_height, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

{ Read two bytes interpreted as an unsigned 16-bit integer.
  cinfo^.image_width should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.image_width := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( cinfo^.image_width, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  { Read a byte into variable cinfo^.num_components.
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.num_components := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  Dec(length, 8);

  TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOF, cinfo^.unread_marker,
	   int(cinfo^.image_width), int(cinfo^.image_height),
	   cinfo^.num_components);

  if (cinfo^.marker^.saw_SOF) then
    ERREXIT(j_common_ptr(cinfo), JERR_SOF_DUPLICATE);

  { We don't support files in which the image height is initially specified }
  { as 0 and is later redefined by DNL.  As long as we have to check that,  }
  { might as well have a general sanity check. }
  if (cinfo^.image_height <= 0) or (cinfo^.image_width <= 0)
      or (cinfo^.num_components <= 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_EMPTY_IMAGE);

  if (length <> (cinfo^.num_components * 3)) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  if (cinfo^.comp_info = NIL) then { do only once, even if suspend }
    cinfo^.comp_info := jpeg_component_info_ptr(cinfo^.mem^.alloc_small(
                       j_common_ptr(cinfo), JPOOL_IMAGE,
                       cinfo^.num_components * SIZEOF(jpeg_component_info)));

  compptr := cinfo^.comp_info;
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    compptr^.component_index := ci;

    { Read a byte into variable compptr^.component_id.
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    compptr^.component_id := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    compptr^.h_samp_factor := (c shr 4) and 15;
    compptr^.v_samp_factor := (c      ) and 15;

    { Read a byte into variable compptr^.quant_tbl_no.
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    compptr^.quant_tbl_no := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOF_COMPONENT,
	     compptr^.component_id, compptr^.h_samp_factor,
	     compptr^.v_samp_factor, compptr^.quant_tbl_no);

    Inc(compptr);
  end;

  cinfo^.marker^.saw_SOF := TRUE;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_sof := TRUE;
end;  { get_sof }


{LOCAL}
function get_sos (cinfo : j_decompress_ptr) : boolean;
{ Process a SOS marker }
label
  id_found;
var
  length : INT32;
  i, ci, n, c, cc : int;
  compptr : jpeg_component_info_ptr;
{ Declare and initialize local copies of input pointer/count }
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;    { Array[] of JOCTET; }
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{}

  if not cinfo^.marker^.saw_SOF then
    ERREXIT(j_common_ptr(cinfo), JERR_SOS_NO_SOF);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  { Read a byte into variable n (Number of components).
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  n := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);


  if ((length <> (n * 2 + 6)) or (n < 1) or (n > MAX_COMPS_IN_SCAN)) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_SOS, n);

  cinfo^.comps_in_scan := n;

  { Collect the component-spec parameters }

  for i := 0 to Pred(n) do
  begin
    { Read a byte into variable cc. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    cc := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    compptr := cinfo^.comp_info;
    for ci := 0 to Pred(cinfo^.num_components) do
    begin
      if (cc = compptr^.component_id) then
	goto id_found;
      Inc(compptr);
    end;

    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_COMPONENT_ID, cc);

  id_found:

    cinfo^.cur_comp_info[i] := compptr;
    compptr^.dc_tbl_no := (c shr 4) and 15;
    compptr^.ac_tbl_no := (c      ) and 15;

    TRACEMS3(j_common_ptr(cinfo), 1, JTRC_SOS_COMPONENT, cc,
	     compptr^.dc_tbl_no, compptr^.ac_tbl_no);
  end;

  { Collect the additional scan parameters Ss, Se, Ah/Al. }
  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Ss := c;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Se := c;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Ah := (c shr 4) and 15;
  cinfo^.Al := (c     ) and 15;

  TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOS_PARAMS, cinfo^.Ss, cinfo^.Se,
	   cinfo^.Ah, cinfo^.Al);

  { Prepare to scan data & restart markers }
  cinfo^.marker^.next_restart_num := 0;

  { Count another SOS marker }
  Inc( cinfo^.input_scan_number );

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_sos := TRUE;
end;  { get_sos }


{METHODDEF}
function get_app0 (cinfo : j_decompress_ptr) : boolean; far;
{ Process an APP0 marker }
const
  JFIF_LEN = 14;
var
  length : INT32;
  b : Array[0..JFIF_LEN-1] of UINT8;
  buffp : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;


{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_app0 := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_app0 := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  length := length - 2;

  { See if a JFIF APP0 marker is present }

  if (length >= JFIF_LEN) then
  begin
    for buffp := 0 to Pred(JFIF_LEN) do
    begin
    { Read a byte into variable b[buffp].
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_app0 := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      b[buffp] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;
    length := length - JFIF_LEN;

    if (b[0]=$4A) and (b[1]=$46) and (b[2]=$49)
     and (b[3]=$46) and (b[4]=0) then
    begin
      { Found JFIF APP0 marker: check version }
      { Major version must be 1, anything else signals an incompatible change.
        We used to treat this as an error, but now it's a nonfatal warning,
        because some bozo at Hijaak couldn't read the spec.
        Minor version should be 0..2, but process anyway if newer. }

      if (b[5] <> 1) then
	WARNMS2(j_common_ptr(cinfo) , JWRN_JFIF_MAJOR, b[5], b[6])
      else
        if (b[6] > 2) then
          TRACEMS2(j_common_ptr(cinfo), 1, JTRC_JFIF_MINOR, b[5], b[6]);
      { Save info }
      cinfo^.saw_JFIF_marker := TRUE;
      cinfo^.density_unit := b[7];
      cinfo^.X_density := (b[8] shl 8) + b[9];
      cinfo^.Y_density := (b[10] shl 8) + b[11];
      TRACEMS3(j_common_ptr(cinfo), 1, JTRC_JFIF,
	       cinfo^.X_density, cinfo^.Y_density, cinfo^.density_unit);
      if (b[12] or b[13]) <> 0 then
	TRACEMS2(j_common_ptr(cinfo), 1, JTRC_JFIF_THUMBNAIL, b[12], b[13]);
      if (length <> (INT32(b[12]) * INT32(b[13]) * INT32(3))) then
	TRACEMS1(j_common_ptr(cinfo), 1, JTRC_JFIF_BADTHUMBNAILSIZE,
          int(length));
    end
    else
    begin
      { Start of APP0 does not match "JFIF" }
      TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP0, int(length) + JFIF_LEN);
    end;
  end
  else
  begin
    { Too short to be JFIF marker }
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP0, int(length));
  end;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then          { skip any remaining data -- could be lots }
    cinfo^.src^.skip_input_data(cinfo, long(length));

  get_app0 := TRUE;
end;  { get_app0 }


{METHODDEF}
function get_app14 (cinfo : j_decompress_ptr) : boolean; far;
{ Process an APP14 marker }
const
  ADOBE_LEN = 12;
var
  length : INT32;
  b : Array[0..ADOBE_LEN-1] of UINT8;
  buffp : int;
  version, flags0, flags1, transform : uint;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_app14 := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_app14 := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length, 2);

  { See if an Adobe APP14 marker is present }

  if (length >= ADOBE_LEN) then
  begin
    for buffp := 0 to Pred(ADOBE_LEN) do
    begin
      { Read a byte into variable b[buffp]. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_app14 := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      b[buffp] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;
    length := length - ADOBE_LEN;

    if (b[0]=$41) and (b[1]=$64) and (b[2]=$6F) and (b[3]=$62) and (b[4]=$65) then
    begin
      { Found Adobe APP14 marker }
      version := (b[5] shl 8) + b[6];
      flags0 := (b[7] shl 8) + b[8];
      flags1 := (b[9] shl 8) + b[10];
      transform := b[11];
      TRACEMS4(j_common_ptr(cinfo), 1, JTRC_ADOBE,
         version, flags0, flags1, transform);
      cinfo^.saw_Adobe_marker := TRUE;
      cinfo^.Adobe_transform := UINT8(transform);
    end
    else
    begin
      { Start of APP14 does not match "Adobe" }
      TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP14, int(length) + ADOBE_LEN);
    end;
  end
  else
  begin
    { Too short to be Adobe marker }
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP14, int(length));
  end;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then          { skip any remaining data -- could be lots }
    cinfo^.src^.skip_input_data (cinfo, long(length));

  get_app14 := TRUE;
end; { get_app14 }


{LOCAL}
function get_dac (cinfo : j_decompress_ptr) : boolean;
{ Process a DAC marker }
var
  length : INT32;
  index, val : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dac := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length,  2);

  while (length > 0) do
  begin
    { Read a byte into variable index. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    index := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable val. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    val := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    Dec( length, 2);

    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_DAC, index, val);

    if (index < 0) or (index >= (2*NUM_ARITH_TBLS)) then
      ERREXIT1(j_common_ptr(cinfo) , JERR_DAC_INDEX, index);

    if (index >= NUM_ARITH_TBLS) then
    begin { define AC table }
      cinfo^.arith_ac_K[index-NUM_ARITH_TBLS] := UINT8(val);
    end
    else
    begin { define DC table }
      cinfo^.arith_dc_L[index] := UINT8(val and $0F);
      cinfo^.arith_dc_U[index] := UINT8(val shr 4);
      if (cinfo^.arith_dc_L[index] > cinfo^.arith_dc_U[index]) then
	ERREXIT1(j_common_ptr(cinfo) , JERR_DAC_VALUE, val);
    end;
  end;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dac := TRUE;
end;  { get_dac }


{LOCAL}
function get_dht (cinfo : j_decompress_ptr) : boolean;
{ Process a DHT marker }
var
  length : INT32;
  bits : Array[0..17-1] of UINT8;
  huffval : Array[0..256-1] of UINT8;
  i, index, count : int;
  htblptr : ^JHUFF_TBL_PTR;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dht := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dht := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length,  2);

  while (length > 0) do
  begin
    { Read a byte into variable index. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dht := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    index := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);


    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_DHT, index);
      
    bits[0] := 0;
    count := 0;
    for i := 1 to 16 do
    begin
      { Read a byte into variable bits[i]. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_dht := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      bits[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);

      Inc( count, bits[i] );
    end;

    Dec( length, (1 + 16) );

    TRACEMS8(j_common_ptr(cinfo), 2, JTRC_HUFFBITS,
	     bits[1], bits[2], bits[3], bits[4],
	     bits[5], bits[6], bits[7], bits[8]);
    TRACEMS8(j_common_ptr(cinfo), 2, JTRC_HUFFBITS,
	     bits[9], bits[10], bits[11], bits[12],
	     bits[13], bits[14], bits[15], bits[16]);

    if (count > 256) or (INT32(count) > length) then
      ERREXIT(j_common_ptr(cinfo), JERR_DHT_COUNTS);

    for i := 0 to Pred(count) do
    begin
    { Read a byte into variable huffval[i]. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_dht := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      huffval[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;

    Dec( length, count );

    if (index and $10)<>0 then
    begin  { AC table definition }
      Dec( index, $10 );
      htblptr := @cinfo^.ac_huff_tbl_ptrs[index];
    end
    else
    begin { DC table definition }
      htblptr := @cinfo^.dc_huff_tbl_ptrs[index];
    end;

    if (index < 0) or (index >= NUM_HUFF_TBLS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_DHT_INDEX, index);

    if (htblptr^ = NIL) then
      htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));

    MEMCOPY(@(htblptr^)^.bits, @bits, SIZEOF((htblptr^)^.bits));
    MEMCOPY(@(htblptr^)^.huffval, @huffval, SIZEOF((htblptr^)^.huffval));
  end;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dht := TRUE;
end;  { get_dht }


{LOCAL}
function get_dqt (cinfo : j_decompress_ptr) : boolean;
{ Process a DQT marker }
var
  length : INT32;
  n, i, prec : int;
  tmp : uint;
  quant_ptr : JQUANT_TBL_PTR;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dqt := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dqt := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec( length, 2 );

  while (length > 0) do
  begin
    { Read a byte into variable n. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dqt := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    n := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    prec := n shr 4;
    n := n and $0F;

    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_DQT, n, prec);

    if (n >= NUM_QUANT_TBLS) then
      ERREXIT1(j_common_ptr(cinfo) , JERR_DQT_INDEX, n);
      
    if (cinfo^.quant_tbl_ptrs[n] = NIL) then
      cinfo^.quant_tbl_ptrs[n] := jpeg_alloc_quant_table(j_common_ptr(cinfo));
    quant_ptr := cinfo^.quant_tbl_ptrs[n];

    for i := 0 to Pred(DCTSIZE2) do
    begin
      if (prec <> 0) then
      begin
      { Read two bytes interpreted as an unsigned 16-bit integer.
        tmp should be declared unsigned int or perhaps INT32. }

      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
        if (bytes_in_buffer = 0) then
        begin
          if (not datasrc^.fill_input_buffer(cinfo)) then
          begin
            get_dqt := FALSE;
            exit;
          end;
          { Reload the local copies }
          next_input_byte := datasrc^.next_input_byte;
          bytes_in_buffer := datasrc^.bytes_in_buffer;
        end;
        Dec( bytes_in_buffer );

        tmp := (uint( GETJOCTET(next_input_byte^)) shl 8);
        Inc( next_input_byte );
        { make a byte available.
          Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
          but we must reload the local copies after a successful fill. }
          if (bytes_in_buffer = 0) then
          begin
            if (not datasrc^.fill_input_buffer(cinfo)) then
            begin
              get_dqt := FALSE;
              exit;
            end;
            { Reload the local copies }
            next_input_byte := datasrc^.next_input_byte;
            bytes_in_buffer := datasrc^.bytes_in_buffer;
          end;
          Dec( bytes_in_buffer );

        Inc( tmp, GETJOCTET( next_input_byte^));
        Inc( next_input_byte );

      end
      else
      begin
      { Read a byte into variable tmp. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
        if (bytes_in_buffer = 0) then
        begin
          if (not datasrc^.fill_input_buffer(cinfo)) then
          begin
            get_dqt := FALSE;
            exit;
          end;
          { Reload the local copies }
          next_input_byte := datasrc^.next_input_byte;
          bytes_in_buffer := datasrc^.bytes_in_buffer;
        end;
        Dec( bytes_in_buffer );

        tmp := GETJOCTET(next_input_byte^);
        Inc(next_input_byte);
      end;

      { We convert the zigzag-order table to natural array order. }
      quant_ptr^.quantval[jpeg_natural_order[i]] := UINT16(tmp);
    end;

    if (cinfo^.err^.trace_level >= 2) then
    begin
      i := 0;
      while i < Pred(DCTSIZE2) do
      begin
	TRACEMS8(j_common_ptr(cinfo), 2, JTRC_QUANTVALS,
		 quant_ptr^.quantval[i],   quant_ptr^.quantval[i+1],
		 quant_ptr^.quantval[i+2], quant_ptr^.quantval[i+3],
		 quant_ptr^.quantval[i+4], quant_ptr^.quantval[i+5],
		 quant_ptr^.quantval[i+6], quant_ptr^.quantval[i+7]);
        Inc(i, 8);
      end;
    end;

    Dec( length, DCTSIZE2+1 );
    if (prec <> 0) then
      Dec( length, DCTSIZE2 );
  end;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dqt := TRUE;
end;  { get_dqt }


{LOCAL}
function get_dri (cinfo : j_decompress_ptr) : boolean;
{ Process a DRI marker }
var
  length : INT32;
  tmp : uint;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dri := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dri := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  if (length <> 4) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  tmp should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dri := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  tmp := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dri := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( tmp, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_DRI, tmp);

  cinfo^.restart_interval := tmp;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dri := TRUE;
end;  { get_dru }


{METHODDEF}
function skip_variable (cinfo : j_decompress_ptr) : boolean; far;
{ Skip over an unknown or uninteresting variable-length marker }
var
  length : INT32;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;    { Array[] of JOCTET; }
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      skip_variable := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        skip_variable := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  TRACEMS2(j_common_ptr(cinfo), 1, JTRC_MISC_MARKER,
    cinfo^.unread_marker, int(length));

  
  { Unload the local copies --- do this only at a restart boundary }
  { do before skip_input_data }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  cinfo^.src^.skip_input_data(cinfo, long(length) - long(2));

  skip_variable := TRUE;
end;  { skip_variable }


{ Find the next JPEG marker, save it in cinfo^.unread_marker.
  Returns FALSE if had to suspend before reaching a marker;
  in that case cinfo^.unread_marker is unchanged.

  Note that the result might not be a valid marker code,
  but it will never be 0 or FF. }

{LOCAL}
function next_marker (cinfo : j_decompress_ptr) : boolean;
var
  c : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  {while TRUE do}
  repeat
    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        next_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

   { Skip any non-FF bytes.
     This may look a bit inefficient, but it will not occur in a valid file.
     We sync after each discarded byte so that a suspending data source
     can discard the byte from its buffer. }

    while (c <> $FF) do
    begin
      Inc(cinfo^.marker^.discarded_bytes);
      { Unload the local copies --- do this only at a restart boundary }
      datasrc^.next_input_byte := next_input_byte;
      datasrc^.bytes_in_buffer := bytes_in_buffer;

      { Read a byte into variable c. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          next_marker := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);

    end;
    { This loop swallows any duplicate FF bytes.  Extra FFs are legal as
      pad bytes, so don't count them in discarded_bytes.  We assume there
      will not be so many consecutive FF bytes as to overflow a suspending
      data source's input buffer. }

    repeat
      { Read a byte into variable c. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          next_marker := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    Until (c <> $FF);
    if (c <> 0) then
      break;			{ found a valid marker, exit loop }
    { Reach here if we found a stuffed-zero data sequence (FF/00).
      Discard it and loop back to try again. }

    Inc(cinfo^.marker^.discarded_bytes, 2);
    { Unload the local copies --- do this only at a restart boundary }
    datasrc^.next_input_byte := next_input_byte;
    datasrc^.bytes_in_buffer := bytes_in_buffer;
  Until False;

  if (cinfo^.marker^.discarded_bytes <> 0) then
  begin
    WARNMS2(j_common_ptr(cinfo), JWRN_EXTRANEOUS_DATA,
            cinfo^.marker^.discarded_bytes, c);
    cinfo^.marker^.discarded_bytes := 0;
  end;

  cinfo^.unread_marker := c;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  next_marker := TRUE;
end;  { next_marker }


{LOCAL}
function first_marker (cinfo : j_decompress_ptr) : boolean;
{ Like next_marker, but used to obtain the initial SOI marker. }
{ For this marker, we do not allow preceding garbage or fill; otherwise,
  we might well scan an entire input file before realizing it ain't JPEG.
  If an application wants to process non-JFIF files, it must seek to the
  SOI before calling the JPEG library. }
var
  c, c2 : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      first_marker := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  { Read a byte into variable c2. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      first_marker := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c2 := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  if (c <> $FF) or (c2 <> int(M_SOI)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_NO_SOI, c, c2);

  cinfo^.unread_marker := c2;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  first_marker := TRUE;
end;  { first_marker }


{ Read markers until SOS or EOI.

  Returns same codes as are defined for jpeg_consume_input:
  JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI.   }

{METHODDEF}
function read_markers (cinfo : j_decompress_ptr) : int; far;
begin
  { Outer loop repeats once for each marker. }
  repeat
    { Collect the marker proper, unless we already did. }
    { NB: first_marker() enforces the requirement that SOI appear first. }
    if (cinfo^.unread_marker = 0) then
    begin
      if not cinfo^.marker^.saw_SOI then
      begin
        if not first_marker(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      end
      else
      begin
        if not next_marker(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      end;
    end;
    { At this point cinfo^.unread_marker contains the marker code and the
      input point is just past the marker proper, but before any parameters.
      A suspension will cause us to return with this state still true. }

    case (cinfo^.unread_marker) of
      M_SOI:
        if not get_soi(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF0,             { Baseline }
      M_SOF1:             { Extended sequential, Huffman }
        if not get_sof(cinfo, FALSE, FALSE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      M_SOF2:                     { Progressive, Huffman }
        if not get_sof(cinfo, TRUE, FALSE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF9:                     { Extended sequential, arithmetic }
        if not get_sof(cinfo, FALSE, TRUE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF10:                    { Progressive, arithmetic }
        if not get_sof(cinfo, TRUE, TRUE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      { Currently unsupported SOFn types }
      M_SOF3,                     { Lossless, Huffman }
      M_SOF5,                     { Differential sequential, Huffman }
      M_SOF6,                     { Differential progressive, Huffman }
      M_SOF7,                     { Differential lossless, Huffman }
      M_JPG,                      { Reserved for JPEG extensions }
      M_SOF11,                    { Lossless, arithmetic }
      M_SOF13,                    { Differential sequential, arithmetic }
      M_SOF14,                    { Differential progressive, arithmetic }
      M_SOF15:                    { Differential lossless, arithmetic }
        ERREXIT1(j_common_ptr(cinfo), JERR_SOF_UNSUPPORTED, cinfo^.unread_marker);

      M_SOS:
        begin
          if not get_sos(cinfo) then
          begin
            read_markers := JPEG_SUSPENDED;
            exit;
          end;
          cinfo^.unread_marker := 0;       { processed the marker }
          read_markers := JPEG_REACHED_SOS;
          exit;
        end;

      M_EOI:
        begin
          TRACEMS(j_common_ptr(cinfo), 1, JTRC_EOI);
          cinfo^.unread_marker := 0;       { processed the marker }
          read_markers := JPEG_REACHED_EOI;
          exit;
        end;

      M_DAC:
        if not get_dac(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DHT:
        if not get_dht(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DQT:
        if not get_dqt(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DRI:
        if not get_dri(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_APP0,
      M_APP1,
      M_APP2,
      M_APP3,
      M_APP4,
      M_APP5,
      M_APP6,
      M_APP7,
      M_APP8,
      M_APP9,
      M_APP10,
      M_APP11,
      M_APP12,
      M_APP13,
      M_APP14,
      M_APP15:
        if not cinfo^.marker^.process_APPn[cinfo^.unread_marker - int(M_APP0)](cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_COM:
        if not cinfo^.marker^.process_COM(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_RST0,		{ these are all parameterless }
      M_RST1,
      M_RST2,
      M_RST3,
      M_RST4,
      M_RST5,
      M_RST6,
      M_RST7,
      M_TEM:
        TRACEMS1(j_common_ptr(cinfo), 1, JTRC_PARMLESS_MARKER,
          cinfo^.unread_marker);

      M_DNL:		{ Ignore DNL ... perhaps the wrong thing }
        if not skip_variable(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      else			{ must be DHP, EXP, JPGn, or RESn }
        { For now, we treat the reserved markers as fatal errors since they are
          likely to be used to signal incompatible JPEG Part 3 extensions.
          Once the JPEG 3 version-number marker is well defined, this code
          ought to change! }
        ERREXIT1(j_common_ptr(cinfo) , JERR_UNKNOWN_MARKER,
          cinfo^.unread_marker);
    end; { end of case }
    { Successfully processed marker, so reset state variable }
    cinfo^.unread_marker := 0;
  Until false;
end;  { read_markers }


{ Read a restart marker, which is expected to appear next in the datastream;
  if the marker is not there, take appropriate recovery action.
  Returns FALSE if suspension is required.

  This is called by the entropy decoder after it has read an appropriate
  number of MCUs.  cinfo^.unread_marker may be nonzero if the entropy decoder
  has already read a marker from the data source.  Under normal conditions
  cinfo^.unread_marker will be reset to 0 before returning; if not reset,
  it holds a marker which the decoder will be unable to read past. }

{METHODDEF}
function read_restart_marker (cinfo : j_decompress_ptr) :boolean; far;
begin
  { Obtain a marker unless we already did. }
  { Note that next_marker will complain if it skips any data. }
  if (cinfo^.unread_marker = 0) then
  begin
    if not next_marker(cinfo) then
    begin
      read_restart_marker := FALSE;
      exit;
    end;
  end;

  if (cinfo^.unread_marker = (int(M_RST0) + cinfo^.marker^.next_restart_num)) then
  begin
    { Normal case --- swallow the marker and let entropy decoder continue }
    TRACEMS1(j_common_ptr(cinfo), 3, JTRC_RST,
      cinfo^.marker^.next_restart_num);
    cinfo^.unread_marker := 0;
  end
  else
  begin
    { Uh-oh, the restart markers have been messed up. }
    { Let the data source manager determine how to resync. }
    if not cinfo^.src^.resync_to_restart(cinfo,
              cinfo^.marker^.next_restart_num) then
    begin
      read_restart_marker := FALSE;
      exit;
    end;
  end;

  { Update next-restart state }
  with cinfo^.marker^ do
    next_restart_num := (next_restart_num + 1) and 7;

  read_restart_marker := TRUE;
end; { read_restart_marker }


{ This is the default resync_to_restart method for data source managers
  to use if they don't have any better approach.  Some data source managers
  may be able to back up, or may have additional knowledge about the data
  which permits a more intelligent recovery strategy; such managers would
  presumably supply their own resync method.

  read_restart_marker calls resync_to_restart if it finds a marker other than
  the restart marker it was expecting.  (This code is *not* used unless
  a nonzero restart interval has been declared.)  cinfo^.unread_marker is
  the marker code actually found (might be anything, except 0 or FF).
  The desired restart marker number (0..7) is passed as a parameter.
  This routine is supposed to apply whatever error recovery strategy seems
  appropriate in order to position the input stream to the next data segment.
  Note that cinfo^.unread_marker is treated as a marker appearing before
  the current data-source input point; usually it should be reset to zero
  before returning.
  Returns FALSE if suspension is required.

  This implementation is substantially constrained by wanting to treat the
  input as a data stream; this means we can't back up.  Therefore, we have
  only the following actions to work with:
    1. Simply discard the marker and let the entropy decoder resume at next
       byte of file.
    2. Read forward until we find another marker, discarding intervening
       data.  (In theory we could look ahead within the current bufferload,
       without having to discard data if we don't find the desired marker.
       This idea is not implemented here, in part because it makes behavior
       dependent on buffer size and chance buffer-boundary positions.)
    3. Leave the marker unread (by failing to zero cinfo^.unread_marker).
       This will cause the entropy decoder to process an empty data segment,
       inserting dummy zeroes, and then we will reprocess the marker.

  #2 is appropriate if we think the desired marker lies ahead, while #3 is
  appropriate if the found marker is a future restart marker (indicating
  that we have missed the desired restart marker, probably because it got
  corrupted).
  We apply #2 or #3 if the found marker is a restart marker no more than
  two counts behind or ahead of the expected one.  We also apply #2 if the
  found marker is not a legal JPEG marker code (it's certainly bogus data).
  If the found marker is a restart marker more than 2 counts away, we do #1
  (too much risk that the marker is erroneous; with luck we will be able to
  resync at some future point).
  For any valid non-restart JPEG marker, we apply #3.  This keeps us from
  overrunning the end of a scan.  An implementation limited to single-scan
  files might find it better to apply #2 for markers other than EOI, since
  any other marker would have to be bogus data in that case. }


{GLOBAL}
function jpeg_resync_to_restart(cinfo : j_decompress_ptr;
                                desired : int) : boolean;
var
  marker : int;
  action : int;
begin
  marker := cinfo^.unread_marker;
  action := 1;     { never used }
  { Always put up a warning. }
  WARNMS2(j_common_ptr(cinfo), JWRN_MUST_RESYNC, marker, desired);

  { Outer loop handles repeated decision after scanning forward. }
  repeat
    if (marker < int(M_SOF0)) then
      action := 2                { invalid marker }
    else
      if (marker < int(M_RST0)) or (marker > int(M_RST7)) then
        action := 3                { valid non-restart marker }
      else
      begin
        if (marker = (int(M_RST0) + ((desired+1) and 7))) or
           (marker = (int(M_RST0) + ((desired+2) and 7))) then
          action := 3              { one of the next two expected restarts }
        else
          if (marker = (int(M_RST0) + ((desired-1) and 7))) or
             (marker = (int(M_RST0) + ((desired-2) and 7))) then
            action := 2            { a prior restart, so advance }
          else
            action := 1;           { desired restart or too far away }
      end;

    TRACEMS2(j_common_ptr(cinfo), 4, JTRC_RECOVERY_ACTION, marker, action);
    case action of
    1:
      { Discard marker and let entropy decoder resume processing. }
      begin
        cinfo^.unread_marker := 0;
        jpeg_resync_to_restart := TRUE;
        exit;
      end;
    2:
      { Scan to the next marker, and repeat the decision loop. }
      begin
        if not next_marker(cinfo) then
        begin
          jpeg_resync_to_restart := FALSE;
          exit;
        end;
        marker := cinfo^.unread_marker;
      end;
    3:
      { Return without advancing past this marker. }
      { Entropy decoder will be forced to process an empty segment. }
      begin
        jpeg_resync_to_restart := TRUE;
        exit;
      end;
    end; { case }
  Until false; { end loop }
end;  { jpeg_resync_to_restart }


{ Reset marker processing state to begin a fresh datastream. }

{METHODDEF}
procedure reset_marker_reader (cinfo : j_decompress_ptr); far;
begin
  with cinfo^ do
  begin
    comp_info := NIL;            { until allocated by get_sof }
    input_scan_number := 0;      { no SOS seen yet }
    unread_marker := 0;          { no pending marker }
    marker^.saw_SOI := FALSE;    { set internal state too }
    marker^.saw_SOF := FALSE;
    marker^.discarded_bytes := 0;
  end;
end; { reset_marker_reader }


{ Initialize the marker reader module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_marker_reader (cinfo : j_decompress_ptr);
var
  i : int;
begin
  { Create subobject in permanent pool }
  cinfo^.marker := jpeg_marker_reader_ptr(
     cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                             SIZEOF(jpeg_marker_reader))
                                    );
  { Initialize method pointers }
  cinfo^.marker^.reset_marker_reader := reset_marker_reader;
  cinfo^.marker^.read_markers := read_markers;
  cinfo^.marker^.read_restart_marker := read_restart_marker;
  cinfo^.marker^.process_COM := skip_variable;
  for i := 0 to 15 do
    cinfo^.marker^.process_APPn[i] := skip_variable;
  cinfo^.marker^.process_APPn[0] := get_app0;
  cinfo^.marker^.process_APPn[14] := get_app14;
  { Reset marker processing state }
  reset_marker_reader(cinfo);
end; { jinit_marker_reader }

end.