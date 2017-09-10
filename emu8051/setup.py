from distutils.core import setup
from distutils.extension import Extension

from os import system

setup(name='emu8051',
  ext_modules=[ 
    Extension('emu8051',
              ['sim.cpp'],
              depends=["libemu.a"],
              extra_objects=["libemu.a"],
              extra_compile_args=['-O2'])
  ],
)
