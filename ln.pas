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
    hist_index	: integer = 0;

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
      var ch: char; ofs : byte = 0; i : integer = 0;
    begin
      crt.gotoxy( 1, crt.wherey ); // left edge
      write( pmt );
      for ch in buf do begin
      inc( i );
	if ( ch < ' ' ) and not ( ch = ^J ) then begin
	  crt.textcolor( 2 );
	  write( '^', chr( ord( '@' ) + ord( ch )));
	  if i <= cur then inc( ofs );
	  crt.textcolor( 7 );
	end
	else write( ch )
      end;
      crt.clreol; // write( #27, '[0G', #27, '[', plen + cur , 'C' );
      crt.gotoxy( plen + cur + ofs, crt.wherey );
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

    procedure transpose;
      var ch : char;
    begin
      if cur > 1 then begin
	if cur >= len then cur := len;
	ch := buf[ cur - 1 ];
	buf[ cur - 1 ] := buf[ cur ];
	buf[ cur ] := ch;
	inc( cur );
	if cur >= len then cur := len + 1;
      end;
    end;

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

    procedure complete_line( var buf : string );
    begin
      //  todo
    end;

    procedure browse_history( new_index : integer );
    begin

      // clamp:
      hist_index := new_index;
      if hist_index < 0 then hist_index := 0;
      if hist_index > history.count then hist_index := history.count;

      // special case for new input at end of list:
      if hist_index in [ 0 .. history.count - 1 ]
	then buf := history[ hist_index ]
        else buf := '';
      len := length( buf );

      // cursor tracking:
      //  maybe remember column for hopping past short lines?
      if cur > len then cur := len + 1;
    end;


    procedure escapes;
    begin
      insert( #27, buf, cur );
    end;

  begin // raw_prompt
    len := 0; cur := 1; plen := length( pmt );
    browse_history( history.count );
    done := false; result := true; // optimism!
    repeat
      refresh; ch := readkey;
      case ch of
	^A : cur := 1;
	^B : begin dec( cur ); if cur = 0 then cur := 1 end;
	^C : begin result := false; done := true end;
	^D : if (len > 0) or (cur > 1) then delete_char
	     else begin result := false; done := true end;
	^E : cur := len + 1;
	^F : begin inc( cur ); if cur > len then cur := len + 1 end;
	^G : ;
	^H : backspace;
	^I : complete_line( buf );
	^J : done := true;
	^K : begin len := cur - 1; setlength( buf, len ) end;
	^L : crt.clrscr;
	^M : done := true;
	^N : browse_history( hist_index + 1 );
	^O : ;
	^P : browse_history( hist_index - 1 );
	^Q : ;
	^R : ;
	^S : ;
	^T : transpose;
	^U : begin delete( buf, 1, cur - 1); len := length( buf ); cur := 1
	     end;
	^V : ;
	^W : kill_prev_word;
	^X : ;
	^Y : ;
	^Z : ;
      { special characters }
	^@ : escapes; // #0 ( null )
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
