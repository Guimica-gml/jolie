#!/usr/bin/sh
set -xe

CFLAGS="-Wall -Wextra -pedantic -ggdb -std=c11"
CLIBS=""

gcc $CFLAGS -o jolie src/main.c src/utils.c src/jolie_lexer.c src/jolie_parser.c $CLIBS
