Unit JdTrans;

{ This file contains library routines for transcoding decompression,
  that is, reading raw DCT coefficient arrays from an input JPEG file.
  The routines in jdapimin.c will also be needed by a transcoder. }

{ Original : jdtrans.c ; Copyright (C) 1995-1996, Thomas G. Lane. }

interface

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jpeglib,
  jdhuff, jdphuff, jdcoefct;

{$I jconfig.inc}

{ Read the coefficient arrays from a JPEG file.
  jpeg_read_header must be completed before calling this.

  The entire image is read into a set of virtual coefficient-block arrays,
  one per component.  The return value is a pointer to the array of
  virtual-array descriptors.  These can be manipulated directly via the
  JPEG memory manager, or handed off to jpeg_write_coefficients().
  To release the memory occupied by the virtual arrays, call
  jpeg_finish_decompress() when done with the data.

  Returns NIL if suspended.  This case need be checked only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_read_coefficients
          (cinfo : j_decompress_ptr) : jvirt_barray_tbl_ptr;

implementation


{ Forward declarations }
{LOCAL}
procedure transdecode_master_selection (cinfo : j_decompress_ptr); forward;


{ Read the coefficient arrays from a JPEG file.
  jpeg_read_header must be completed before calling this.

  The entire image is read into a set of virtual coefficient-block arrays,
  one per component.  The return value is a pointer to the array of
  virtual-array descriptors.  These can be manipulated directly via the
  JPEG memory manager, or handed off to jpeg_write_coefficients().
  To release the memory occupied by the virtual arrays, call
  jpeg_finish_decompress() when done with the data.

  Returns NIL if suspended.  This case need be checked only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_read_coefficients
           (cinfo : j_decompress_ptr) : jvirt_barray_tbl_ptr;
var
  retcode : int;
begin
  if (cinfo^.global_state = DSTATE_READY) then
  begin
    { First call: initialize active modules }
    transdecode_master_selection(cinfo);
    cinfo^.global_state := DSTATE_RDCOEFS;
  end
  else
    if (cinfo^.global_state <> DSTATE_RDCOEFS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Absorb whole file into the coef buffer }
  while TRUE do
  begin
    { Call progress monitor hook if present }
    if (cinfo^.progress <> NIL) then
      cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
    { Absorb some more input }
    retcode := cinfo^.inputctl^.consume_input (cinfo);
    if (retcode = JPEG_SUSPENDED) then
    begin
      jpeg_read_coefficients := NIL;
      exit;
    end;
    if (retcode = JPEG_REACHED_EOI) then
      break;
    { Advance progress counter if appropriate }
    if (cinfo^.progress <> NIL) and
       ((retcode = JPEG_ROW_COMPLETED) or (retcode = JPEG_REACHED_SOS)) then
    begin
      Inc(cinfo^.progress^.pass_counter);
      if (cinfo^.progress^.pass_counter >= cinfo^.progress^.pass_limit) then
      begin
	{ startup underestimated number of scans; ratchet up one scan }
	Inc(cinfo^.progress^.pass_limit, long(cinfo^.total_iMCU_rows));
      end;
    end;
  end;
  { Set state so that jpeg_finish_decompress does the right thing }
  cinfo^.global_state := DSTATE_STOPPING;
  jpeg_read_coefficients:= cinfo^.coef^.coef_arrays;
end;


{ Master selection of decompression modules for transcoding.
  This substitutes for jdmaster.c's initialization of the full decompressor. }

{LOCAL}
procedure transdecode_master_selection (cinfo : j_decompress_ptr);
var
  nscans : int;
begin
  { Entropy decoding: either Huffman or arithmetic coding. }
  if (cinfo^.arith_code) then
  begin
    ERREXIT(j_common_ptr(cinfo), JERR_ARITH_NOTIMPL);
  end
  else
  begin
    if (cinfo^.progressive_mode) then
    begin
{$ifdef D_PROGRESSIVE_SUPPORTED}
      jinit_phuff_decoder(cinfo);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
      jinit_huff_decoder(cinfo);
  end;

  { Always get a full-image coefficient buffer. }
  jinit_d_coef_controller(cinfo, TRUE);

  { We can now tell the memory manager to allocate virtual arrays. }
  cinfo^.mem^.realize_virt_arrays (j_common_ptr(cinfo));

  { Initialize input side of decompressor to consume first scan. }
  cinfo^.inputctl^.start_input_pass (cinfo);

  { Initialize progress monitoring. }
  if (cinfo^.progress <> NIL) then
  begin
    { Estimate number of scans to set pass_limit. }
    if (cinfo^.progressive_mode) then
    begin
      { Arbitrarily estimate 2 interleaved DC scans + 3 AC scans/component. }
      nscans := 2 + 3 * cinfo^.num_components;
    end
    else
      if (cinfo^.inputctl^.has_multiple_scans) then
      begin
        { For a nonprogressive multiscan file, estimate 1 scan per component. }
        nscans := cinfo^.num_components;
      end
      else
      begin
        nscans := 1;
      end;
    cinfo^.progress^.pass_counter := long(0);
    cinfo^.progress^.pass_limit := long(cinfo^.total_iMCU_rows * nscans);
    cinfo^.progress^.completed_passes := 0;
    cinfo^.progress^.total_passes := 1;
  end;
end;

end.