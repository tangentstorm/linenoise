{$mode objfpc}{$h+}
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

  function prompt( const prm : string; var input : string ) : boolean;

implementation

  {$link linenoise.o}
  {$linklib c}
  procedure clrscr; cdecl; external name 'linenoiseClearScreen';
  function linenoise( const prompt : pchar ) : pchar;  cdecl; external;

  function prompt( const prm : string; var input : string ) : boolean;
    var tmp : pchar;
  begin
    tmp := linenoise( pchar( prm ));
    result := tmp <> nil;
    if result then input := ansistring( tmp )
    else input := ''
  end;

  procedure StringList.load( path : string );
  begin
    if fileexists( path ) then self.loadFromFile( path );
  end;

  procedure StringList.save( path : string );
  begin self.saveToFile( path );
  end;

initialization
  history := stringlist.create;
end.
