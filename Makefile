FPC = fpc -gl
example:
	$(FPC) -B ln_example.pas
	./ln_example

c_example: ln_example.c  linenoise.o
	$(CC) -Wall -W -Os -g -o ln_example_c linenoise.c ln_example.c

linenoise.o: linenoise.h linenoise.c
	$(CC) -c linenoise.c

clean:
	rm -f c_example ln_example *.ppu *.o history.txt
