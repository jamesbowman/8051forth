import sys

sys.path.append("build/lib.linux-x86_64-2.7")
import emu8051
e = emu8051.emu8051()
print e.read(30)

