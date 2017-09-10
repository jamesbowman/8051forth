#include <stddef.h>
#include <assert.h>

#define PY_SSIZE_T_CLEAN
#undef NDEBUG
#include <Python.h>

extern "C" {
#include "emu8051.h"
}

#if PY_MAJOR_VERSION >= 3
  #define MOD_ERROR_VAL NULL
  #define MOD_SUCCESS_VAL(val) val
  #define MOD_INIT(name) PyMODINIT_FUNC PyInit_##name(void)
  #define MOD_DEF(ob, name, doc, methods) \
          static struct PyModuleDef moduledef = { \
            PyModuleDef_HEAD_INIT, name, doc, -1, methods, }; \
          ob = PyModule_Create(&moduledef);
#else
  #define MOD_ERROR_VAL
  #define MOD_SUCCESS_VAL(val)
  #define MOD_INIT(name) extern "C" void init##name(void)
  #define MOD_DEF(ob, name, doc, methods) \
          ob = Py_InitModule3(name, methods, doc);
#endif

typedef struct {
  PyObject_HEAD
  /* Type-specific fields go here. */
  struct em8051 emu;
  char *read_buf;
  int read_count;
} emu8051;

static void
dealloc(emu8051* self)
{
  Py_TYPE(self)->tp_free((PyObject*)self);
}

static int uart_data = 0xc1;

static int
emu_sfrread(struct em8051 *aCPU, int aRegister)
{
  if (aRegister == 0x86) {
    return 4;  // Pretend UART receive byte always ready
  }
  if (aRegister == uart_data) {
    int c = getchar();
    return (c == '\n') ? 0x0d : c;
  }
  return aCPU->mSFR[aRegister - 0x80];
}

static void
emu_sfrwrite(struct em8051 *aCPU, int aRegister)
{
  emu8051 *emu = (emu8051*)((char*)aCPU - offsetof(emu8051, emu));
  if (aRegister == uart_data) {
    emu->read_buf[emu->read_count++] = aCPU->mSFR[uart_data - 0x80];
    aCPU->mSFR[0xe8 - 0x80] = 2;
  }
}


static int
init(emu8051 *self, PyObject *args, PyObject *kwds)
{
  struct em8051 &emu = self->emu;

  memset(&emu, 0, sizeof(emu));

  emu.mCodeMem     = (unsigned char*)malloc(65536);
  emu.mCodeMemSize = 65536;
  emu.mExtData     = emu.mCodeMem;
  // emu.mExtData     = (unsigned char*)malloc(65536);
  emu.mExtDataSize = 65536;
  emu.mSFR   = (unsigned char*)malloc(128);

  {
    // For TI CC1110:
    emu.mLowerData   = emu.mExtData + 0xff00;
    emu.mUpperData   = emu.mExtData + 0xff80;

    // uart_data = 0xc1;
  }

  // emu.except       = &emu_exception;
  emu.sfrread      = &emu_sfrread;
  emu.sfrwrite      = &emu_sfrwrite;

  emu.xread = NULL;
  emu.xwrite = NULL;
  reset(&emu, 1);
  memset(emu.mCodeMem, 0xff, emu.mCodeMemSize);

  static const uint8_t df00_block[] = {
0xD3, 0x91, 0xFF, 0x04, 0x45, 0x00, 0x00, 0x0F, 0x00, 0x1E, 0xC4, 0xEC, 0x8C, 0x22, 0x02, 0x22, 
0xF8, 0x47, 0x07, 0x30, 0x04, 0x76, 0x6C, 0x03, 0x40, 0x91, 0x56, 0x10, 0xA9, 0x0A, 0x20, 0x0D, 
0x59, 0x3F, 0x3F, 0x88, 0x11, 0x0B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC6, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x03, 0x00, 0x7F, 0x80, 0x01, 0x00, 0x94, 0x00, 0x00, 
0x00, 0x78, 0x00, 0x00, 0x00, 0x00, 0x93, 0xE2, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC7, 0x00, 0x00, 0x0C, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x00, 0x00, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1D, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x03, 0x70, 0x6B, 0x87, 0x08, 0x44, 0x00, 0x00, 0x00, 0x00, 0x2A, 0x00, 0x12, 0x00, 0x0F, 
0x00, 0x00, 0x00, 0x08, 0x33, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x68, 0x90, 
0x00, 0x0A, 0x22, 0x00, 0x02, 0x0C, 0x88, 0x01, 0x00, 0x00, 0x00, 0x08, 0x40, 0x00, 0x40, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x40, 0x40, 0x00, 0x00, 0x00, 0x08, 0x40, 0x00, 0x40, 0x00, 
0x00, 0x00, 0x00, 0x0C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00};
  memcpy(emu.mExtData + 0xdf00, df00_block, 0x100);

  int rc = load_obj(&emu, (char*)"../cc0.hex");
  if (rc < 0) {
    fprintf(stderr, "load obj error %d\n", rc);
    exit(1);
  }

  return 0;
}

static PyObject *reset(PyObject *self, PyObject *args)
{
  reset(&((emu8051*)self)->emu, 1);
  Py_RETURN_NONE;
}

static PyObject *inWaiting(PyObject *self, PyObject *args)
{
#if PY_MAJOR_VERSION < 3
  return PyInt_FromLong(1);
#else
  return PyLong_FromLong(1);
#endif
}

static PyObject *read(PyObject *self, PyObject *args)
{
  emu8051 *emu = (emu8051*)self;
  int count;
  if (!PyArg_ParseTuple(args, "I", &count))
    return NULL;

  Py_ssize_t len = count;
  char buf[len];

  emu->read_buf = buf;
  emu->read_count = 0;

  while (emu->read_count < count) {
    if (PyErr_CheckSignals())
      return NULL;
    tick(&emu->emu);
  }

#if PY_MAJOR_VERSION < 3
  return PyString_FromStringAndSize(buf, len);
#else
  return PyBytes_FromStringAndSize(buf, len);
#endif
}

static PyObject *write(PyObject *_, PyObject *args)
{
  struct em8051 *emu = &((emu8051*)_)->emu;
  const char *s;
  Py_ssize_t n;
  if (!PyArg_ParseTuple(args, "s#", &s, &n))
    return NULL;

  for (Py_ssize_t i = 0; i < n; i++) {
    tick(emu);
  }

  Py_RETURN_NONE;
}

static PyMethodDef methods[] = {
  {"reset", reset, METH_NOARGS},
  {"read", read, METH_VARARGS},
  {"write", write, METH_VARARGS},
  {"inWaiting", inWaiting, METH_NOARGS},
  {NULL}  /* Sentinel */
};

static PyTypeObject emu8051Type = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "emu8051",                /*tp_name*/
  sizeof(emu8051),          /*tp_basicsize*/
  0,                        /*tp_itemsize*/
  (destructor)dealloc,      /*tp_dealloc*/
  0,                        /*tp_print*/
  0,                        /*tp_getattr*/
  0,                        /*tp_setattr*/
  0,                        /*tp_compare*/
  0,                        /*tp_repr*/
  0,                        /*tp_as_number*/
  0,                        /*tp_as_sequence*/
  0,                        /*tp_as_mapping*/
  0,                        /*tp_hash */
  0,                        /*tp_call*/
  0,                        /*tp_str*/
  0,                        /*tp_getattro*/
  0,                        /*tp_setattro*/
  0,                        /*tp_as_buffer*/
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
  "emu8051 objects",        /* tp_doc */
  0,                        /* tp_traverse */
  0,                        /* tp_clear */
  0,                        /* tp_richcompare */
  0,                        /* tp_weaklistoffset */
  0,                        /* tp_iter */
  0,                        /* tp_iternext */
  methods,                  /* tp_methods */
  0,                        /* tp_members */
  0,                        /* tp_getset */
  0,                        /* tp_base */
  0,                        /* tp_dict */
  0,                        /* tp_descr_get */
  0,                        /* tp_descr_set */
  0,                        /* tp_dictoffset */
  (initproc)init,           /* tp_init */
  0,                        /* tp_alloc */
  0,                        /* tp_new */
};

MOD_INIT(emu8051)
{
  PyObject *m;

  emu8051Type.tp_new = PyType_GenericNew;
  if (PyType_Ready(&emu8051Type) < 0)
      return MOD_ERROR_VAL;

  MOD_DEF(m, "emu8051", "", NULL)

  Py_INCREF(&emu8051Type);
  PyModule_AddObject(m, "emu8051", (PyObject *)&emu8051Type);
  return MOD_SUCCESS_VAL(m);
}
