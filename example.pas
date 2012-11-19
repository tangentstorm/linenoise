{ example for linenoise }
program example;
uses ln;

  procedure completion( const str : String; var comps : ln.Completions );
  begin
    if buf[0] == 'h' then begin
      comps.add( "hartford" );
      comps.add( "hereford" );
      comps.add( "hampshire" );
    end
  end;

var
  line : string;
begin
  ln.on_complete := @completion;
  ln.history.load( 'history.txt' );  // load history at startup
  while ln.prompt( "hello> ", line ) do begin
    writeln( 'echo : ', line );
    ln.history.add( line );
    ln.history.save( 'history.txt' );
  end;
end.
