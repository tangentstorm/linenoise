{$i xpc.inc}
unit ln;
interface uses ctypes, classes, sysutils, crt;

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
    MAX_LINE_SIZE = 1024;
    unsupported	: array[ 1..3 ] of string
		  = ( '', 'dumb', 'cons25' );

  const force_plain = false;
  function prompt( const pmt : string; var buf : string ) : boolean;


implementation

  function raw_prompt( const pmt : string; var buf : string ) : boolean;
    var plen, len, cur : integer; done : boolean; ch : char;

    procedure refresh;
    begin
      crt.gotoxy( 1, crt.wherey ); // left edge
      write( pmt );
      write( buf );
      crt.clreol; // write( #27, '[0G', #27, '[', plen + cur , 'C' );
      crt.gotoxy( plen + cur, crt.wherey );
    end;

    procedure complete_line( var buf : string );
    begin
      //  todo
    end;

    procedure backspace;
    begin
      if ( cur > 1 ) and ( len > 0 ) then begin
	dec( cur ); dec( len );
	delete( buf, cur, 1 );
      end;
    end;

    procedure delete_char; inline;
    begin
      if cur <= len then begin
	dec( len );
	delete( buf, cur, 1 );
      end;
    end;

    procedure hist_next; begin end;
    procedure hist_prev; begin end;

    procedure kill_prev_word;
      var old, dif : integer;
    begin
      old := cur;
      while ( cur > 1 ) and ( buf[ cur - 1 ] <= ' ' ) do dec( cur );
      while ( cur > 1 ) and ( buf[ cur - 1 ]  > ' ' ) do dec( cur );
      dif := old - cur + 1;
      delete( buf, cur, dif );
      len := length( buf );
    end;

    procedure accept; inline;
    begin
      writeln; done := true;
    end;

    procedure escapes;
    begin
    end;

  begin
    len := 0; cur := 1; plen := length( pmt );
    done := false; result := true; // optimism!
    repeat
      refresh; ch := readkey;
      case ch of
	#0 : ;
	^A : cur := 1;
	^B : begin dec( cur ); if cur = 0 then cur := 1 end;
	^C : begin result := false; done := true end;
	^D : delete_char;
	^E : cur := len + 1;
	^F : begin inc( cur ); if cur > len then cur := len + 1 end;
	^G : ;
	^H : backspace;
	^I : complete_line( buf );
	^J : accept;
	^K : begin len := cur - 1; setlength( buf, len ) end;
	^L : crt.clrscr;
	^M : accept;
	^N : hist_next;
	^O : ;
	^P : hist_prev;
	^Q : ;
	^R : ;
	^S : ;
	^T : ;
	^U : begin buf := ''; refresh end;
	^V : ;
	^W : kill_prev_word;
	^X : ;
	^Y : ;
	^Z : ;
	^[ : escapes;
	^\ , ^], ^^ , ^_ : ; // field, group, record, unit separator
	^? : backspace;
	else begin
	  write( ch );
	  insert( ch, buf, cur );
	  inc( cur ); inc( len );
	end
      end
    until done;
    if result then setlength( buf, len );
  end;


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
  if term_supported and not force_plain then //  and is_tty( stdin )
    result := raw_prompt( pmt, buf )
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
