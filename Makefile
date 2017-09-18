ASM=as31/as31
EMU=emu8051/emu

all: $(ASM) $(EMU) camel51.hex cc0.hex flashcopy.hex

$(ASM):
	make -C as31 as31

$(EMU):
	make -C emu8051
	cd emu8051 ; python setup.py install --user

%.hex: %.asm $(AS31)
	$(ASM) -l $<
