{ example for ln.pas module }
{$mode objfpc}{$h+}
program ln_example;
uses ln;

  procedure completion( const buf : string; var comps : ln.Completions );
  begin
    if buf[ 1 ] = 'h' then begin
      comps.add( 'hartford' );
      comps.add( 'hereford' );
      comps.add( 'hampshire' );
    end
  end;

var
  line : string = '';
begin
  ln.on_complete := @completion;
  ln.history.load( 'history.txt' );  // load history at startup
  while ln.prompt( 'hello> ', line ) do begin
    writeln;
    writeln( 'echo : ', line );
    if line <> '' then begin
      ln.history.add( line );
      ln.history.save( 'history.txt' );
    end
  end;
  writeln;
end.
