import sys

sys.path.append("build/lib.linux-x86_64-2.7")
import emu8051
from forthshell import ForthShell, main

class OutputComplete(Exception):
    pass

class DUT(ForthShell):
    def __init__(self, *a):
        self.start()
        self.e = emu8051.emu8051(self.writer, self.reader)
        self.uart_out = []
        self.uart_in = "1 TTH !" + '\r\n'
        self.frob()

    def boot(self, *a):
        pass

    def writer(self, reg, val):
        # print 'write %x %x' % (reg, val)
        if reg == 0xc1:
            if val == 30:
                raise OutputComplete
            self.uart_out.append(chr(val))

    def reader(self, reg, val):
        # print 'read %x %x' % (reg, val)
        if reg == 0x86:
            return 4
        if reg == 0xe8:
            return 2
        if reg == 0xc1:
            val = ord(self.uart_in[0])
            self.uart_in = self.uart_in[1:]
        return val

    def frob(self):
        try:
            self.e.run()
        except OutputComplete:
            pass
        r = "".join(self.uart_out)
        self.uart_out = []
        return r

    def interactive_command(self, cmd = None):
        self.uart_in = cmd + '\r\n'
        sys.stdout.write(self.frob())

    def command_response(self, cmd):
        self.uart_in = cmd + '\r\n'
        return self.frob()

if __name__ == '__main__':
    main(DUT)
