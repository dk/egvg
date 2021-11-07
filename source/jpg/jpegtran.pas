Program JpegTran;

{ This file contains a command-line user interface for JPEG transcoding.
  It is very similar to cjpeg.c, but provides lossless transcoding between
  different JPEG file formats. }

{ Original: jpegtran.c ; Copyright (C) 1995-1996, Thomas G. Lane. }

uses
  cdjpeg,		{ Common decls for cjpeg/djpeg applications }
  jversion;		{ for version message }

{ Argument-parsing code.
  The switch parser is designed to be useful with DOS-style command line
  syntax, ie, intermixed switches and file names, where only the switches
  to the left of a given file name affect processing of that file.
  The main program in this file doesn't actually use this capability... }


var
  progname,	        { program name for error messages }
  outfilename : string;	{ for -outfile switch }


{LOCAL}
procedure usage;
{ complain about bad command line }
begin
  WriteLn(output, 'usage: %s [switches] ', progname);
{$ifdef TWO_FILE_COMMANDLINE}
  WriteLn(output, 'inputfile outputfile');
{$else}
  WriteLn(output, '[inputfile]');
{$endif}

  WriteLn(output, 'Switches (names may be abbreviated):');
{$ifdef ENTROPY_OPT_SUPPORTED}
  WriteLn(output, '  -optimize      Optimize Huffman table (smaller file, but slow compression)');
{$endif}
{$ifdef C_PROGRESSIVE_SUPPORTED}
  WriteLn(output, '  -progressive   Create progressive JPEG file');
{$endif}
  WriteLn(output, 'Switches for advanced users:\n");
  WriteLn(output, '  -restart N     Set restart interval in rows, or in blocks with B\n");
  WriteLn(output, '  -maxmemory N   Maximum memory to use (in kbytes)\n");
  WriteLn(output, '  -outfile name  Specify name for output file\n");
  WriteLn(output, '  -verbose  or  -debug   Emit debug output\n");
  WriteLn(output, 'Switches for wizards:\n");
#ifdef C_ARITH_CODING_SUPPORTED
  WriteLn(output, '  -arithmetic    Use arithmetic coding\n");
{$endif}
#ifdef C_MULTISCAN_FILES_SUPPORTED
  WriteLn(output, '  -scans file    Create multi-scan JPEG per script file\n");
{$endif}
  exit(EXIT_FAILURE);
end;


{LOCAL}
function parse_switches (cinfo : j_compress_ptr;
                         last_file_arg_seen: int;
                         for_real : boolean ) : int;
{ Parse optional switches.
  Returns argv[] index of first file-name argument (= argc if none).
  Any file names with indexes <= last_file_arg_seen are ignored;
  they have presumably been processed in a previous iteration.
  (Pass 0 for last_file_arg_seen on the first or only iteration.)
  for_real is FALSE on the first (dummy) pass; we may skip any expensive
  processing. }
var
  argn : int;
  arg : string;
  simple_progressive : boolean;
const
  scansarg : string = '';	{ saves -scans parm if any }
begin
  { Set up default JPEG parameters. }
  simple_progressive := FALSE;
  outfilename := '';
  cinfo^.err^.trace_level := 0;

  { Scan command line options, adjust parameters }

  for argn := 1 to ParamCount do
  begin
    arg := ParamStr(argn);
    if (arg[1] <> '-') then
    begin
      { Not a switch, must be a file name argument }
      if (argn <= last_file_arg_seen) then
      begin
	outfilename := '';	{ -outfile applies to just one input file }
	continue;		{ ignore this name if previously processed }
      end;
      break;			{ else done parsing switches }
    end;
    arg++;			{ advance past switch marker character }

    if (keymatch(arg, "arithmetic", 1)) begin
      { Use arithmetic coding. }
{$ifdef C_ARITH_CODING_SUPPORTED}
      cinfo^.arith_code := TRUE;
{$else}
      WriteLn(output, progname, ': sorry, arithmetic coding not supported);
      Halt(EXIT_FAILURE);
{$endif}
    end
    else
      if (keymatch(arg, 'debug', 1)) or (keymatch(arg, 'verbose', 1)) then
      begin
      { Enable debug printouts. }
      { On first -d, print version identification }
      static boolean printed_version := FALSE;

      if (not printed_version) then
      begin
	WriteLn(output, 'Independent JPEG Group's JPEGTRAN, version ',
		JVERSION, #13#10, JCOPYRIGHT);
	printed_version := TRUE;
      end;
      Inc(cinfo^.err^.trace_level);

    end
    else
      if (keymatch(arg, 'maxmemory', 3)) then
      begin
      { Maximum memory in Kb (or Mb with 'm'). }
        long lval;
        char ch := 'x';

        if (++argn >= argc)	{ advance to next argument }
	  usage;
        if (sscanf(argv[argn], "%ld%c", &lval, &ch) < 1) then
          usage;
        if (ch = 'm' || ch = 'M')
	  lval *= 1000L;
        cinfo^.mem^.max_memory_to_use := lval * 1000L;

      end
      else
        if (keymatch(arg, 'optimize', 1)) or (keymatch(arg, 'optimise', 1)) then
        begin
          { Enable entropy parm optimization. }
{$ifdef ENTROPY_OPT_SUPPORTED}
          cinfo^.optimize_coding := TRUE;
{$else}
          WriteLn(output, progname,
            ': sorry, entropy optimization was not compiled');
          Halt(EXIT_FAILURE);
{$endif}

        end
        else
          if (keymatch(arg, "outfile", 4)) then
          begin
            { Set output file name. }
            if (++argn >= argc)	{ advance to next argument }
              usage;
            outfilename := argv[argn];	{ save it away for later use }

          end
          else
            if (keymatch(arg, "progressive", 1)) then
            begin
              { Select simple progressive mode. }
{$ifdef C_PROGRESSIVE_SUPPORTED}
              simple_progressive := TRUE;
              { We must postpone execution until num_components is known. }
{$else}
              WriteLn(output, progname,
                ': sorry, progressive output was not compiled');
              Halt(EXIT_FAILURE);
{$endif}

            end
            else
              if (keymatch(arg, 'restart', 1)) then
              begin
                { Restart interval in MCU rows (or in MCUs with 'b'). }
                long lval;
                char ch := 'x';

                if (++argn >= argc) then { advance to next argument }
	          usage;
                if (sscanf(argv[argn], "%ld%c", &lval, &ch) < 1) then
	          usage;
                if (lval < 0 || lval > 65535L) then
	          usage;
                if (ch = 'b' || ch = 'B') then
                begin
	          cinfo^.restart_interval := (unsigned int) lval;
	          cinfo^.restart_in_rows := 0; { else prior '-restart n' overrides me }
                end
                else
                begin
	          cinfo^.restart_in_rows := (int) lval;
	          { restart_interval will be computed during startup }
                end;

              end
              else
                if (keymatch(arg, "scans", 2)) then
                begin
                  { Set scan script. }
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
                  if (++argn >= argc)	{ advance to next argument }
	            usage;
                  scansarg := argv[argn];
                  { We must postpone reading the file in case -progressive appears. }
{$else}
                  WriteLn(output, progname,
                    ': sorry, multi-scan output was not compiled');
                  Halt(EXIT_FAILURE);
{$endif}
                end
                else
                begin
                  usage;                { bogus switch }
                end;
  end;

  { Post-switch-scanning cleanup }

  if (for_real) begin

{$ifdef C_PROGRESSIVE_SUPPORTED}
    if (simple_progressive) then { process -progressive; -scans can override }
      jpeg_simple_progression(cinfo);
{$endif}

{$ifdef C_MULTISCAN_FILES_SUPPORTED}
    if (scansarg <> NULL)	{ process -scans if it was present }
      if (! read_scan_script(cinfo, scansarg))
	usage;
{$endif}
  end;

  return argn;			{ return index of next arg (file name) }
end;


{ The main program. }

int
main (int argc, char **argv)
var
  srcinfo : jpeg_decompress_struct;
  dstinfo : jpeg_compress_struct;
  jsrcerr, jdsterr : jpeg_error_mgr;
{$ifdef PROGRESS_REPORT}
  progress : cdjpeg_progress_mgr;
{$endif}
  coef_arrays : jvirt_barray_ptr*;
  file_index : int;
  input_file : FILE*;
  output_file : FILE*;
begin
  { On Mac, fetch a command line. }
{$ifdef USE_CCOMMAND}
  argc := ccommand(@argv);
{$endif}

  progname := ParamStr(0);

  { Initialize the JPEG decompression object with default error handling. }
  srcinfo.err := jpeg_std_error(@jsrcerr);
  jpeg_create_decompress(@srcinfo);
  { Initialize the JPEG compression object with default error handling. }
  dstinfo.err := jpeg_std_error(jdsterr);
  jpeg_create_compress(@dstcinfo);

  { Now safe to enable signal catcher.
    Note: we assume only the decompression object will have virtual arrays. }

{$ifdef NEED_SIGNAL_CATCHER}
  enable_signal_catcher((j_common_ptr) @srcinfo);
{$endif}

  { Scan command line to find file names.
    It is convenient to use just one switch-parsing routine, but the switch
    values read here are ignored; we will rescan the switches after opening
    the input file. }

  file_index := parse_switches(@dstcinfo, argc, argv, 0, FALSE);
  jsrcerr.trace_level := jdsterr.trace_level;
  srcinfo.mem^.max_memory_to_use := dstinfo.mem^.max_memory_to_use;

{$ifdef TWO_FILE_COMMANDLINE}
  { Must have either -outfile switch or explicit output file name }
  if (outfilename = NIL) then
  begin
    if (file_index <> argc-2) then
    begin
      WriteLn(output, progname, ': must name one input and one output file');
      usage;
    end;
    outfilename := argv[file_index+1];
  end
  else
  begin
    if (file_index <> argc-1) then
    begin
      WriteLn(output, progname, ': must name one input and one output file');
      usage;
    end;
  end;
{$else}
  { Unix style: expect zero or one file name }
  if (file_index < argc-1) then
  begin
    WriteLn(output, progname, ': only one input file');
    usage;
  end;
{$endif} { TWO_FILE_COMMANDLINE }

  { Open the input file. }
  if (file_index < argc) then
  begin
    if ((input_file := fopen(argv[file_index], READ_BINARY)) = NIL) then
    begin
      WriteLn(output, progname, ': can't open ', argv[file_index]);
      exit(EXIT_FAILURE);
    end;
  end
  else
  begin
    { default input file is stdin }
    input_file := read_stdin;
  end;

  { Open the output file. }
  if (outfilename <> '') then
  begin
    if ((output_file := fopen(outfilename, WRITE_BINARY)) = NIL) then
    begin
      WriteLn(output, progname, ': can't open ', outfilename);
      Halt(EXIT_FAILURE);
    end;
  end
  else
  begin
    { default output file is stdout }
    output_file := output;
  end;

{$ifdef PROGRESS_REPORT}
  start_progress_monitor(j_common_ptr(@dstcinfo), @progress);
{$endif}

  { Specify data source for decompression }
  jpeg_stdio_src(@srcinfo, input_file);

  { Read file header }
  {void} jpeg_read_header(@srcinfo, TRUE);

  { Read source file as DCT coefficients }
  coef_arrays := jpeg_read_coefficients(@srcinfo);

  { Initialize destination compression parameters from source values }
  jpeg_copy_critical_parameters(@srcinfo, @dstcinfo);

  { Adjust default compression parameters by re-parsing the options }
  file_index := parse_switches(@dstcinfo, argc, argv, 0, TRUE);

  { Specify data destination for compression }
  jpeg_stdio_dest(@dstcinfo, output_file);

  { Start compressor }
  jpeg_write_coefficients(@dstcinfo, coef_arrays);

  { ought to copy source comments here... }

  { Finish compression and release memory }
  jpeg_finish_compress(@dstcinfo);
  jpeg_destroy_compress(@dstcinfo);
  (void) jpeg_finish_decompress(@srcinfo);
  jpeg_destroy_decompress(@srcinfo);

  { Close files, if we opened them }
  if (input_file <> stdin) then
    fclose(input_file);
  if (output_file <> stdout) then
    fclose(output_file);

{$ifdef PROGRESS_REPORT}
  end_progress_monitor(j_common_ptr(@dstinfo));
{$endif}

  { All done. }
  if jsrcerr.num_warnings + jdsterr.num_warnings <> 0 then
    exit(EXIT_WARNING)
  else
    exit(EXIT_SUCCESS);
  return 0;			{ suppress no-return-value warnings }
end;

end.
