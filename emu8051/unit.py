import sys

sys.path.append("build/lib.linux-x86_64-2.7")
import emu8051

class OutputComplete(Exception):
    pass

class DUT:
    def __init__(self):
        self.e = emu8051.emu8051()
        self.e.setrw(self.writer, self.reader)
        self.uart_out = []
        self.uart_in = "1 TTH !" + '\r\n'
        print repr(self.frob())

    def writer(self, reg, val):
        # print 'write %x %x' % (reg, val)
        if reg == 0xc1:
            if val == 30:
                print 'RS'
                raise OutputComplete
            # print 'uart', repr(chr(val))
            self.uart_out.append(chr(val))

    def reader(self, reg, val):
        # print 'read %x %x' % (reg, val)
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
        print repr(self.frob())
        # sys.stdout.write(clean)

dut = DUT()
print dut.interactive_command('WORDS')
print dut.interactive_command('1 2 + .')
