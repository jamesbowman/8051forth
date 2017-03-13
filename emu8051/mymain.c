#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "emu8051.h"
#include <assert.h>

int uart_data;

static void finish(struct em8051 *aCPU)
{
  fprintf(stderr, "Cycles: %lu, %.3f ms\n", aCPU->cycles, 1000 * aCPU->cycles / 28e6);
  FILE *dump = fopen("dump.bin", "wb");
  fwrite(aCPU->mCodeMem, 32768, 1, dump);
  fclose(dump);
  exit(0);
}

static int
emu_sfrread(struct em8051 *aCPU, int aRegister)
{
  if (aRegister == 0x86) {
    return 4;  // Pretend UART receive byte always ready
  }
  if (aRegister == uart_data) {
    int c = getchar();
    if (c < 0)
      finish(aCPU);
    return (c == '\n') ? 0x0d : c;
  }
  return aCPU->mSFR[aRegister - 0x80];
}

static void
emu_sfrwrite(struct em8051 *aCPU, int aRegister)
{
  if (aRegister == uart_data) {
    putchar(aCPU->mSFR[uart_data - 0x80]);
    aCPU->mSFR[0xe8 - 0x80] = 2;
  }
}

static void
emu_exception(struct em8051 *aCPU, int aCode)
{
  if (aCode == EXCEPTION_ILLEGAL_OPCODE)
    finish(aCPU);
  assert(0);
}

int main(int argc, char *argv[])
{
  struct em8051 emu;
  memset(&emu, 0, sizeof(emu));

  emu.mCodeMem     = malloc(65536);
  emu.mCodeMemSize = 65536;
  emu.mExtData     = emu.mCodeMem;
  // emu.mExtData     = malloc(65536);
  emu.mExtDataSize = 65536;
  emu.mSFR   = malloc(128);

  if (strcmp(argv[1], "cc1110") == 0) {
    // For TI CC1110:
    emu.mLowerData   = emu.mExtData + 0xff00;
    emu.mUpperData   = emu.mExtData + 0xff80;
    uart_data = 0xc1;
  } else if (strcmp(argv[1], "generic") == 0) {
    emu.mLowerData   = calloc(128, 1);
    emu.mUpperData   = calloc(128, 1);
    uart_data = 0x99;
  } else {
    fprintf(stderr, "usage: emu8051 [generic|cc1110] <hexfile>\n");
    exit(1);
  }

  emu.except       = &emu_exception;
  emu.sfrread      = &emu_sfrread;
  emu.sfrwrite      = &emu_sfrwrite;
  emu.xread = NULL;
  emu.xwrite = NULL;
  reset(&emu, 1);
  memset(emu.mCodeMem, 0xff, emu.mCodeMemSize);

  int rc = load_obj(&emu, argv[2]);
  if (rc < 0) {
    fprintf(stderr, "load obj error %d\n", rc);
    exit(1);
  }

  int i;
  int trace = 0;
  for (i = 0; emu.mPC != 0xffff; i++) {
    // trace |= (emu.mPC == 0xe009);
    if (trace) {
      unsigned int tos = emu.mSFR[REG_DPH] * 256 + emu.mSFR[REG_DPL];
      printf("pc=%04x tos=%04x\n", emu.mPC, tos);
    }
    tick(&emu);
  }

  exit(0);
}
