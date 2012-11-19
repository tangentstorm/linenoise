{$i xpc.inc}
unit ln;
interface uses ctypes, classes, sysutils;

  type
    StringList	   = class ( TStringList )
      procedure load( path : string );
      procedure save( path : string );
    end;
    HistoryList	   = StringList;
    Completions	   = StringList;
    completion_cbk = procedure( const buf: string; var comps : Completions );

  var
    on_complete	: completion_cbk;
    history	: StringList;

  const
    MAX_LINE_SIZE = 4096;
    unsupported	: array[ 1..3 ] of string
		  = ( '', 'dumb', 'cons25' );

  const force_plain = false;
  function prompt( const pmt : string; var buf : string ) : boolean;


implementation

  { leaning on the c version during the pascal port... }

  {$linklib c}
  {$link linenoise.o}
  procedure clrscr; cdecl; external name 'linenoiseClearScreen';
  function raw_prompt( buf : pchar; len : csize_t; const pmt : pchar) : cint;
    cdecl;
    external name 'linenoiseRaw';



{ -- pascal version begins -- }

function term_supported : boolean;
var un, term : string;
begin
  result := true;
  term := getEnvironmentVariable( 'TERM' );
  for un in unsupported do if term = un then result := false;
end;


function prompt( const pmt : string; var buf : string ) : boolean;
  var len : cint;
begin
  if term_supported and not force_plain then begin
    setlength( buf, MAX_LINE_SIZE );
    len := raw_prompt( pchar( buf ), MAX_LINE_SIZE, pchar( pmt ));
    result := len <> -1;
    if result then setlength( buf, len );
  end
  else begin
    write( pmt );
    readln( input );
    result := not eof; //  need to debug this
  end
end;


{ -- TStringList wrappers  -- }

procedure StringList.load( path : string ); inline;
begin
  if fileexists( path ) then self.loadFromFile( path );
end;

procedure StringList.save( path : string ); inline;
begin
  self.saveToFile( path );
end;


initialization
  history := stringlist.create;
end.
