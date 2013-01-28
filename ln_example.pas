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
  ln.ed.on_complete := @completion;
  ln.ed.history.load( 'history.txt' );  // load history at startup
  while ln.ed.input( 'hello> ', line ) do begin
    writeln;
    writeln( 'echo : ', line );
    if line <> '' then begin
      ln.ed.history.save( 'history.txt' );
    end
  end;
  writeln;
end.
