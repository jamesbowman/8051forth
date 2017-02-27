#sudo apt-get install libncurses5 libncurses5-dev

HEADERS = emu8051.h  emulator.h
OBJ = opcodes.o core.o disasm.o mymain.o

CC = gcc
CCPP = g++
# CFLAGS = -g -Wall -Wextra -O9
CFLAGS = -Wall -Wextra -O9

%.o: %.c $(HEADERS)
	$(CC) $(CLFLAGS)-c -o $@ $< $(CFLAGS)

# %.o: %.cpp $(HEADERS)
# 	$(CCPP) $(CLFLAGS)-c -o $@ $< $(CFLAGS)

emu: $(OBJ)
	$(CC) $(CFLAGS) $(OBJ) -o emu -lcurses
clean:
	-rm -rf *.o emu
