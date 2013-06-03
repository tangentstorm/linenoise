
import 
  classes, sysutils, crt

type 
  StringList* = object of TStringList #      procedure load( path : string );
                                      #      procedure save( path : string );
  HistoryList* = StringList
  Completions* = StringList
  completion_cbk* = proc (buf: string; comps: var Completions)
  LineEditor* = object 
    history*: StringList
    on_complete*: completion_cbk #      constructor create;
                                 #      function flush: string;
                                 #      function input( const pmt : string; var res : string ) : boolean;
                                 #      procedure refresh;
                                 #      procedure backspace;
                                 #      procedure delete_char;
                                 #      procedure transpose;
                                 #      procedure kill_prev_word;
                                 #      procedure complete_line( var buf : string );
                                 #      procedure browse_history( new_index : integer );
                                 #      procedure reset;
                                 #      procedure step;
                                 #    private
    done*: bool
    hist_index*: int
    plen*, length*, cur*: int
    keep*: bool
    pmt*, buf*: string #      procedure escapes;
                       #      procedure set_prompt( const s :  string );
                       #    public
                       #      property prompt : string read pmt write set_prompt;
                       #      property done : boolean read _done;
  

const 
  MAX_LINE_SIZE* = 1024
  unsupported*: array[1..3, string] = ["", "dumb", "cons25"]
  force_plain* = false

var ed*: LineEditor

proc prompt*(msg: string; buf: var string): bool
# implementation

proc Create(self: LineEditor) = 
  history = StringList.create
  hist_index = 0
  self.reset

proc set_prompt(self: LineEditor; s: string) = 
  self.pmt = s
  self.plen = len(s)

proc refresh(self: LineEditor) = 
  var 
    ch: char
    ofs: int8 = 0
    i: int = 0
  crt.gotoxy(1, crt.wherey)   # left edge
  write(pmt)
  i = 0
  while i < len(buf): inc(i)
  ch = buf[i]
  if (ch < ' ') and
      not (ch ==
      '\x0A'):                #^J
    crt.textcolor(2)
    write('^', chr(ord('@') + ord(ch)))
    if i <= cur: inc(ofs)
    crt.textcolor(7)
  else: 
    write(ch)
  
crt.clreol
crt.gotoxy(plen + cur + ofs, crt.wherey)
# write( #27, '[0G', #27, '[', plen + cur , 'C' );