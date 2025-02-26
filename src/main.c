#include "./jolie_lexer.h"
#include "./jolie_parser.h"
#include "./jolie_checker.h"
#include "./utils.h"

#define ARENA_IMPLEMENTATION
#include "./arena.h"

#include <errno.h>

String read_file(Arena *arena, const char *filepath) {
    FILE *file = fopen(filepath, "rb");
    if (file == NULL) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno)
        );
        exit(1);
    }

    if (fseek(file, 0, SEEK_END) != 0) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno)
        );
        exit(1);
    }

    size_t count = ftell(file);
    rewind(file);

    char *data = malloc(count * sizeof(char));
    fread(data, sizeof(char), count, file);
    if (ferror(file)) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno)
        );
        exit(1);
    }

    String string = {0};
    arena_da_append_many(arena, &string, data, count);

    free(data);
    fclose(file);
    return string;
}

int main(int argc, const char **argv) {
    Arena arena = {0};
    if (argc < 2) {
        fprintf(stderr, "Error: expected input filepath\n");
        exit(1);
    }
    const char *src_filepath = argv[1];

    String file = read_file(&arena, src_filepath);
    String_View content = sv_from_parts(file.items, file.count);

    Jolie_Lexer lexer = jolie_lexer_from_sv(src_filepath, content);
    Jolie_Ast ast = jolie_parse(&arena, &lexer);
    if (ast.failed) {
        fprintf(stderr, STR_FMT, STR_ARG(&ast.error_message));
        exit(1);
    }

    jolie_check_ast(&arena, &ast);
    if (ast.failed) {
        fprintf(stderr, STR_FMT, STR_ARG(&ast.error_message));
        exit(1);
    }

    arena_free(&arena);
    return 0;
}
