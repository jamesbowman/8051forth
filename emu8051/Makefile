#sudo apt-get install libncurses5 libncurses5-dev

HEADERS = emu8051.h  emulator.h
OBJ = opcodes.o core.o disasm.o

CC = gcc
CCPP = g++
# CFLAGS = -g -Wall -Wextra -O9
CFLAGS = -fPIC -Wall -Wextra -O9 -L.

%.o: %.c $(HEADERS)
	$(CC) $(CLFLAGS)-c -o $@ $< $(CFLAGS)

# %.o: %.cpp $(HEADERS)
# 	$(CCPP) $(CLFLAGS)-c -o $@ $< $(CFLAGS)

emu: libemu.a mymain.o
	$(CC) $(CFLAGS) mymain.o -o emu -lcurses -lemu

libemu.a: $(OBJ)
	$(AR) r libemu.a $(OBJ)

clean:
	-rm -rf *.o emu
