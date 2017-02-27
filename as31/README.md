# as31
Adaptation of as31 by Paul Stoffregen for Mac OS X and modern versions of GTK+

## Requirements
* GTK+2.0
* bison
* pkg-config

It is recommended that you install all of this using [homebrew](http://brew.sh)

## Installation (Non-GUI)
* Compile:
`make`
* Copy bin into your path

## Installation (GUI)
* Modify your pkg-config path:
`export PKG_CONFIG_PATH=/usr/local/Cellar/gtk+/<your version>/include/:/opt/X11/lib/pkgconfig/`
* Compile:
`make gui`
* Copy bin into your path

## Verification
Run `make test`

