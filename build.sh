#!/usr/bin/sh
set -xe

CFLAGS="-Wall -Wextra -pedantic -ggdb -std=c11"
CLIBS=""

mkdir -p build
gcc $CFLAGS -o build/jolie src/main.c src/utils.c src/jolie_lexer.c src/jolie_parser.c src/jolie_checker.c $CLIBS
