#!/usr/bin/sh
set -xe

CFLAGS="-Wall -Wextra -pedantic -ggdb -std=c99"
CLIBS=""

gcc $CFLAGS -o jolie src/main.c src/utils.c $CLIBS
