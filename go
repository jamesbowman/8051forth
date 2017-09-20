set -e

make
make -C emu8051
if false
then
  # emu8051/emu cc1110 cc0.hex ; exit
  ./confirm cc1110 cc0.hex # ; exit

  cat core.fs double0.fs double.fs regs.fs | emu8051/emu cc1110 cc0.hex
  # exit
  srec_cat dump.bin -binary -address-length=2 -o - -intel |
  egrep ':......0[01]' |
  egrep -v ':20....00F{64}..' > cc0f.hex
  echo 'run2:'
  # emu8051/emu cc1110 cc0f.hex ; exit
fi

python unit.py -p . -p anstests core.fs \
dis.fs \
runtests.fs \
# -e '#bye'
exit

SF=~/swapforth
PORT=`$SF/j1a/icestorm/findserial`
echo $PORT
PYTHONPATH=$SF/shell python $SF/j1a/shell.py -h $PORT chipcon.fs

exit
