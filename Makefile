FPC = fpc -gl
example: linenoise.o
	$(FPC) -B ln_example.pas
	./ln_example

c_example: linenoise.h linenoise.c ln_example.c
	$(CC) -Wall -W -Os -g -o ln_example_c linenoise.c ln_example.c

linenoise.o: linenoise.h linenoise.c
	$(CC) -c linenoise.c

clean:
	rm -f c_example *.ppu *.o
