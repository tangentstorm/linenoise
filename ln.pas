unit ln;
interface uses classes, sysutils, crt;

  type
    StringList	   = object ( TStringList )
{      procedure load( path : string );
      procedure save( path : string );}
    end;
    HistoryList	   = StringList;
    Completions	   = StringList;
    completion_cbk = procedure( const buf: string; var comps : Completions );

    LineEditor	   = object
      history      : StringList;
      on_complete  : completion_cbk;
{      constructor create;
      function flush: string;
      function input( const pmt : string; var res : string ) : boolean;
      procedure refresh;
      procedure backspace;
      procedure delete_char;
      procedure transpose;
      procedure kill_prev_word;
      procedure complete_line( var buf : string );
      procedure browse_history( new_index : integer );
      procedure reset;
      procedure step;}
{    private}
      done      : boolean;
      hist_index : integer;
      plen, len, cur : integer;
      keep : boolean;
      pmt, buf : string;
{      procedure escapes;
      procedure set_prompt( const s :  string );}
{    public}
{      property prompt : string read pmt write set_prompt;
      property done : boolean read _done;}
    end;

  const
    MAX_LINE_SIZE = 1024;
    unsupported	: array[ 1..3 ] of string
		  = ( '', 'dumb', 'cons25' );
    force_plain	  = false;

  var
    ed	: LineEditor;

  function prompt( const msg : string; var buf : string ) : boolean;

implementation

  procedure Create(self	: LineEditor);
  begin
    history := StringList.create;
    hist_index := 0;
    self.reset;
  end;

  procedure set_prompt( self : LineEditor;  const s : string );
  begin
    self.pmt := s;
    self.plen := length(s);
  end;

  procedure refresh( self : LineEditor );
    var ch : char; ofs : byte = 0; i : integer = 0;
  begin
    crt.gotoxy( 1, crt.wherey ); // left edge
    write( pmt );
    i := 0;
    while i < length(buf) do
      inc( i );
      ch := buf[ i ];
      if ( ch < ' ' ) and not ( ch = {^J}#10 ) then begin
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

  procedure backspace( self : LineEditor );
  begin
    if ( cur > 1 ) and ( len > 0 ) then begin
      dec( cur ); dec( len );
      delete( buf, cur, 1 );
    end;
  end;

  procedure delete_char( self :  LineEditor ); inline;
  begin
    if cur <= len then begin
      dec( len );
      delete( buf, cur, 1 );
    end;
  end;

  procedure transpose( self : LineEditor ) ;
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

  procedure LineEditor.kill_prev_word( self : LineEditor ) ;
    var old, dif : integer;
  begin
    old := cur;
    while ( cur > 1 ) and ( buf[ cur - 1 ] <= ' ' ) do dec( cur );
    while ( cur > 1 ) and ( buf[ cur - 1 ]  > ' ' ) do dec( cur );
    dif := old - cur + 1;
    delete( buf, cur, dif );
    len := length( buf );
  end;

  procedure LineEditor.complete_line( var buf : string );
  begin
    // todo
  end;

  procedure LineEditor.browse_history( new_index : integer );
  begin

    // clamp:
    hist_index := new_index;
    if hist_index < 0 then hist_index := 0;
    if hist_index > history.count then hist_index := history.count;

    // special case for new input at end of list:
    if hist_index = history.count
      then buf := ''
    else buf := history[ hist_index ];
    len := length( buf );

    // cursor tracking:
    // maybe remember column for hopping past short lines?
    if cur > len then cur := len + 1;
  end;


  procedure LineEditor.escapes( self : LineEditor ) ;
  begin
    insert( #27, buf, cur );
  end;

  procedure LineEditor.step( self : LineEditor ) ;
    var ch : char;
  begin
    refresh; ch := crt.readkey;
    case ch of
      ^A : cur := 1;
      ^B : begin dec( cur ); if cur = 0 then cur := 1 end;
      ^C : begin keep := false; _done := true end;
      ^D : if (len > 0) or (cur > 1) then delete_char
	   else begin keep := false; _done := true end;
      ^E : cur := len + 1;
      ^F : begin inc( cur ); if cur > len then cur := len + 1 end;
      ^G : ;
      ^H : backspace;
      ^I : complete_line( buf );
      ^J : _done := true;
      ^K : begin len := cur - 1; setlength( buf, len ) end;
      ^L : crt.clrscr;
      ^M : _done := true;
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
  end; { step }

  procedure LineEditor.reset( self : LineEditor ) ;
  begin
    len := 0; cur := 1; plen := length( pmt );
    _done := false;
    browse_history( history.count );
  end;

  function LineEditor.input( self : LineEditor; const pmt : string; var res : string ) : boolean;
  begin
    self.pmt := pmt;
    self.buf := res;
    reset;
    refresh;
    _done := false; keep := true; // optimism!
    repeat step until done;
    if keep then res := flush;
    result := keep;
  end; { LineEditor.prompt }


  function LineEditor.flush( self : LineEditor ) : string;
  begin
    result := self.buf;
    if result <> '' then history.add( result );
    self.buf := '';
    reset;
  end;


function term_supported( self : LineEditor ) : boolean;
  var un, term : string;
begin
  result := true;
  term := getEnvironmentVariable( 'TERM' );
  for un in unsupported do if term = un then result := false;
end;


function prompt(  self : LineEditor; const msg : string; var buf : string ) : boolean;
begin
  if term_supported and not force_plain then //  and is_tty( stdin )
  begin
    result := ed.input( msg, buf )
  end
  else begin
    write( msg );
    readln( system.input );
    result := not eof; //  need to debug this
  end
end;

{ -- TStringList wrappers  -- }

  procedure StringList.load( self : LineEditor; path : string ); inline;
begin
  if fileExists( path ) then self.LoadFromFile( path );
end;

  procedure StringList.save( self : LineEditor; path : string ); inline;
begin
  self.SaveToFile( path );
end;

initialization
  ed := LineEditor.create;
end.
