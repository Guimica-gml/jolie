@echo off
if not exist build (mkdir build)

set CFLAGS=/W2 /DEBUG /std:c11
set CLIBS=

cl.exe %CFLAGS% /c /Fo:build\main.obj src\main.c %CLIBS%
cl.exe %CFLAGS% /c /Fo:build\utils.obj src\utils.c %CLIBS%
cl.exe %CFLAGS% /c /Fo:build\jolie_lexer.obj src\jolie_lexer.c %CLIBS%
cl.exe %CFLAGS% /c /Fo:build\jolie_parser.obj src\jolie_parser.c %CLIBS%

link.exe /out:build\jolie.exe build\main.obj build\utils.obj build\jolie_lexer.obj build\jolie_parser.obj
