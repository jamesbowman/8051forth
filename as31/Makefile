# as31 - 8031/8051 Assembler
#
# Maintained at:  http://www.pjrc.com/tech/8051/
# Please report problems to:  paul@pjrc.com
#
#
# REVISION HISTORY:
#	Jan. 19, 1990	Created. (Ken Stauffer).
#	Jan. 30, 1990	Theo played here.
#	Late 1994	Paul Stoffregen updates the code
#	Sept 1995	bug fixes, add obsure option for cgi
#	April 2000	fix filename handling
#			convert everything to ANSI C and bison
#			properly handle 0b0h as hex 0xB0, not binary 0
#			Make most errors into warning, and modify parser
#			  to recover and continue to the end of the file
#			  so that all warning are shown.
#	March 2001	fix crash with srecord format
#			command line version returns non-zero on error


TEST = tests

#Use this CFLAGS line with gcc on linux
CFLAGS=-O3 -Wall
GTKFLAGS=-DGTK_ENABLE_BROKEN
CC=gcc

#Use this CFLAGS line on solaris, etc
#CFLAGS=-O

#Try these for HPUX
#CFLAGS=-O2
#CC=gcc

OBJ=run.o lexer.o parser.o symbol.o emitter.o

all: as31 as31_gtk

as31: $(OBJ) as31.o
	@$(CC) $(CFLAGS) -o as31 $(OBJ) as31.o
	@chmod a+rx as31
	@strip as31

as31_gtk: $(OBJ) as31_gtk.o
	@$(CC) $(CFLAGS) $(GTKFLAGS) -o as31_gtk $(OBJ) as31_gtk.o `pkg-config --cflags --libs gtk+-2.0`
	@chmod a+rx as31
	@strip as31

parser.c parser.h: parser.y
	@bison -d -o parser.c parser.y

as31_gtk.o: as31_gtk.c as31.h
	@$(CC) $(CFLAGS) $(GTKFLAGS) `pkg-config --cflags --libs gtk+-2.0` -c as31_gtk.c 

as31.o: as31.c as31.h
emitter.o: emitter.c as31.h
symbol.o: symbol.c as31.h parser.h
lexer.o: lexer.c as31.h parser.h
parser.o: parser.c as31.h
run.o: run.c as31.h

test:

	@./as31 $(TEST)/paulmon1.asm > /dev/null
	@./as31 $(TEST)/paulmon2.asm > /dev/null
	@./as31 $(TEST)/extra.asm > /dev/null

	@diff $(TEST)/paulmon1.hex $(TEST)/paulmon1.ref > $(TEST)/test1.tmp
	@diff $(TEST)/paulmon2.hex $(TEST)/paulmon2.ref >> $(TEST)/test1.tmp
	@diff $(TEST)/extra.hex $(TEST)/extra.ref >> $(TEST)/test1.tmp

	@if [ -s $(TEST)/test1.tmp ] ; then \
    echo FAILED ; \
    else \
    echo PASSED ; \
    fi

	@rm $(TEST)/*.hex
	@rm $(TEST)/test1.tmp 

clean:
	rm -f as31 as31_gtk *~ *.o parser.c parser.h core
	rm -f *.lst *.hex *.tdr *.byte *.od *.srec
