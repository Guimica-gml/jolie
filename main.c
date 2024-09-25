#define ARENA_IMPLEMENTATION
#include "./arena.h"

// TODO(nic): have better error messages
// TODO(nic): implement game of life (game of jolie)

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>

#define SV(cstr) ((String_View) { .data = (cstr), .length = strlen(cstr) })
#define SV_STATIC(cstr) { .data = (cstr), .length = sizeof(cstr) - 1 }
#define SV_FMT "%.*s"
#define SV_ARG(sv) (int) sv.length, sv.data

#define SRC_LOC_FMT "%s:%zu:%zu"
#define SRC_LOC_ARG(loc) (loc).filepath, (loc).row, (loc).col

#define DA_INIT_CAP 512
#define da_push(arena, da, item)                                        \
    do {                                                                \
        if ((da)->capacity <= (da)->count) {                            \
            size_t old_size = (da)->capacity * sizeof(*(da)->items);    \
            (da)->capacity = ((da)->capacity == 0) ? DA_INIT_CAP : (da)->capacity * 2; \
            (da)->items = arena_realloc((arena), (da)->items, old_size, (da)->capacity * sizeof(*(da)->items)); \
        }                                                               \
        (da)->items[(da)->count++] = (item);                            \
    } while(0);

typedef struct {
    char *data;
    size_t length;
} String;

typedef struct {
    const char *data;
    size_t length;
} String_View;

// NOTE(nic): row and columns begin at 0:0 in code
// but is reported as beginning at 1:1 in Jolie_Src_Loc
// this happens so that zero initialized lexers are valid
// but the first token still appears as 1:1
typedef struct {
    const char *src_filepath;
    String_View content;
    size_t index;
    size_t line;
    size_t bol;
} Jolie_Lexer;

typedef struct {
    const char *filepath;
    size_t row;
    size_t col;
} Jolie_Src_Loc;

typedef enum {
    JOLIE_END,
    JOLIE_PAREN_OPEN,
    JOLIE_PAREN_CLOSE,
    JOLIE_WORD,
    JOLIE_UINT64,
    JOLIE_STRING,
} Jolie_Token_Type;

typedef struct {
    String_View text;
    Jolie_Token_Type type;
    Jolie_Src_Loc loc;
} Jolie_Token;

typedef struct {
    char ch;
    Jolie_Token_Type type;
} Jolie_Literal_Token;

Jolie_Literal_Token jolie_literal_tokens[] = {
    { .ch = '(', .type = JOLIE_PAREN_OPEN },
    { .ch = ')', .type = JOLIE_PAREN_CLOSE },
};
size_t jolie_literal_tokens_count =
    sizeof(jolie_literal_tokens)/sizeof(jolie_literal_tokens[0]);

const char *jolie_token_type_to_cstr(Jolie_Token_Type type) {
    switch (type) {
    case JOLIE_END: return "JOLIE_END";
    case JOLIE_PAREN_OPEN: return "JOLIE_PAREN_OPEN";
    case JOLIE_PAREN_CLOSE: return "JOLIE_PAREN_CLOSE";
    case JOLIE_WORD: return "JOLIE_WORD";
    case JOLIE_UINT64: return "JOLIE_UINT64";
    default: {
        fprintf(stderr, "Error: unknown token type (at `jolie_token_type_to_cstr`): %d\n", type);
        exit(1);
    }
    }
}

Jolie_Lexer jolie_lexer_from_sv(const char *src_filepath, String_View content) {
    Jolie_Lexer lexer = {0};
    lexer.src_filepath = src_filepath;
    lexer.content = content;
    return lexer;
}

// WARNING(nic): this doesn't check for new line character
String_View jolie_chop(Jolie_Lexer *lexer, size_t length) {
    assert(lexer->index + length <= lexer->content.length);
    String_View text = { &lexer->content.data[lexer->index], length };
    lexer->index += length;
    return text;
}

void jolie_consume_char(Jolie_Lexer *lexer) {
    assert(lexer->index < lexer->content.length);
    if (lexer->content.data[lexer->index] == '\n') {
        lexer->line += 1;
        lexer->bol = lexer->index + 1;
    }
    lexer->index += 1;
}

String_View jolie_chop_until(Jolie_Lexer *lexer, char ch) {
    const char *begin = lexer->content.data + lexer->index;
    size_t begin_index = lexer->index;

    while (lexer->index < lexer->content.length && ch != lexer->content.data[lexer->index]) {
        jolie_consume_char(lexer);
    }

    return (String_View) { begin, lexer->index - begin_index };
}

String_View jolie_chop_while(Jolie_Lexer *lexer, int(*predicate)(int)) {
    const char *begin = lexer->content.data + lexer->index;
    size_t begin_index = lexer->index;

    while (lexer->index < lexer->content.length && predicate(lexer->content.data[lexer->index])) {
        jolie_consume_char(lexer);
    }

    return (String_View) { begin, lexer->index - begin_index };
}

#define JOLIE_COMMENT_CHAR ';'

int jolie_is_word(int ch) {
    for (size_t i = 0; i < jolie_literal_tokens_count; ++i) {
        if (ch == jolie_literal_tokens[i].ch || ch == JOLIE_COMMENT_CHAR) {
            return false;
        }
    }
    return !isspace(ch);
}

Jolie_Token jolie_next_token(Jolie_Lexer *lexer) {
again:
    jolie_chop_while(lexer, isspace);

    Jolie_Token token = {0};
    token.loc.filepath = lexer->src_filepath;
    token.loc.col = lexer->index - lexer->bol + 1;
    token.loc.row = lexer->line + 1;

    if (lexer->index >= lexer->content.length) {
        token.type = JOLIE_END;
        token.text = SV("<eof>");
        return token;
    }

    char peek = lexer->content.data[lexer->index];

    if (peek == JOLIE_COMMENT_CHAR) {
        jolie_chop_until(lexer, '\n');
        goto again;
    }

    for (size_t i = 0; i < jolie_literal_tokens_count; ++i) {
        if (peek == jolie_literal_tokens[i].ch) {
            token.type = jolie_literal_tokens[i].type;
            token.text = jolie_chop(lexer, 1);
            return token;
        }
    }

    if (peek == '"') {
        const char *begin = lexer->content.data + lexer->index;
        size_t begin_index = lexer->index;
        jolie_consume_char(lexer);

        bool escaped = false;
        while (true) {
            if (lexer->index >= lexer->content.length || lexer->content.data[lexer->index] == '\n') {
                fprintf(
                    stderr, SRC_LOC_FMT": Error: unclosed string literal\n",
                    SRC_LOC_ARG(token.loc));
                exit(1);
            }

            char ch = lexer->content.data[lexer->index];
            jolie_consume_char(lexer);

            if (!escaped && ch == '\\') {
                escaped = true;
                continue;
            }
            if (!escaped && ch == '\"') {
                break;
            }

            escaped = false;
        }

        token.type = JOLIE_STRING;
        token.text = (String_View) { begin, lexer->index - begin_index };
        return token;
    }

    if (isdigit(peek)) {
        token.type = JOLIE_UINT64;
        // NOTE(nic): we use isword here because
        // only the first char is used to decided if it is a word or a number
        // the parser should determine if it is a valid int literal or not
        token.text = jolie_chop_while(lexer, jolie_is_word);
        return token;
    }

    token.type = JOLIE_WORD;
    token.text = jolie_chop_while(lexer, jolie_is_word);
    return token;
}

Jolie_Token jolie_peek_token(Jolie_Lexer *lexer) {
    Jolie_Lexer save_lexer = *lexer;
    Jolie_Token peek = jolie_next_token(lexer);
    *lexer = save_lexer;
    return peek;
}

void jolie_reset_lexer(Jolie_Lexer *lexer) {
    lexer->index = 0;
    lexer->bol = 0;
    lexer->line = 0;
}

typedef enum {
    JOLIE_EXPR_LIST,
    JOLIE_EXPR_WORD,
    JOLIE_EXPR_UINT64,
} Jolie_Expr_Type;

typedef struct Jolie_Expr Jolie_Expr;

typedef struct {
    Jolie_Expr *items;
    size_t count;
    size_t capacity;
} Jolie_List;

typedef struct {
    Jolie_List list;
    String_View word;
    uint64_t uint64;
} Jolie_Expr_As;

struct Jolie_Expr {
    Jolie_Src_Loc loc;
    Jolie_Expr_Type type;
    Jolie_Expr_As as;
};

typedef struct {
    Jolie_Expr *items;
    size_t count;
    size_t capacity;
} Jolie_Block;

uint64_t sv_to_uint64(String_View sv) {
    uint64_t num = 0;
    for (size_t i = 0; i < sv.length; ++i) {
        num = (sv.data[i] - '0') + num * 10;
    }
    return num;
}

bool sv_eq(String_View a, String_View b) {
    if (a.length != b.length) {
        return false;
    }
    return memcmp(a.data, b.data, a.length) == 0;
}

typedef struct {
    String_View name;
    uint64_t *address;
} Jolie_Var;

typedef struct {
    String_View name;
    Jolie_Block block;
    Jolie_List arg_names;
} Jolie_Func;

typedef struct {
    Jolie_Var *items;
    size_t count;
    size_t capacity;
} Jolie_Vars;

typedef struct {
    Jolie_Func *items;
    size_t count;
    size_t capacity;
} Jolie_Funcs;

#define JOLIE_STACK_CAP 2048

typedef struct {
    uint64_t stack[JOLIE_STACK_CAP];
    size_t stack_pointer;

    Arena arena;
    Jolie_Vars vars;
    Jolie_Funcs funcs;
} Jolie_Env;

Jolie_Expr jolie_parse_expr(Jolie_Env *env, Jolie_Lexer *lexer);

Jolie_List jolie_parse_list(Jolie_Env *env, Jolie_Lexer *lexer) {
    Jolie_List expr_list = {0};

    Jolie_Token paren_open = jolie_next_token(lexer);
    assert(paren_open.type == JOLIE_PAREN_OPEN);

    while (true) {
        Jolie_Token peek = jolie_peek_token(lexer);
        if (peek.type == JOLIE_PAREN_CLOSE) {
            jolie_next_token(lexer);
            break;
        } else if (peek.type == JOLIE_END) {
            fprintf(
                stderr, SRC_LOC_FMT": Error: unclosed parenthesis\n",
                SRC_LOC_ARG(paren_open.loc));
            exit(1);
        } else {
            Jolie_Expr expr = jolie_parse_expr(env, lexer);
            da_push(&env->arena, &expr_list, expr);
        }
    }

    return expr_list;
}

typedef struct {
    char ch;
    char escape_ch;
} Jolie_Escape_Char_Def;

Jolie_Escape_Char_Def jolie_escape_chars[] = {
    { .ch = 'n', .escape_ch = '\n' },
    { .ch = 'r', .escape_ch = '\r' },
    { .ch = 't', .escape_ch = '\t' },
    { .ch = '\\', .escape_ch = '\\' },
    { .ch = '\'', .escape_ch = '\'' },
    { .ch = '\"', .escape_ch = '\"' },
};
size_t jolie_escape_chars_count =
    sizeof(jolie_escape_chars)/sizeof(jolie_escape_chars[0]);

uint64_t jolie_prepare_string(Jolie_Env *env, String_View sv, Jolie_Src_Loc loc) {
    uint64_t string_begin = env->stack_pointer;
    size_t i = 1;
    while (i < sv.length - 1) {
        char ch = sv.data[i++];
        if (ch == '\\') {
            char next_ch = sv.data[i++];
            bool exist = false;
            for (size_t j = 0; j < jolie_escape_chars_count; ++j) {
                if (jolie_escape_chars[j].ch == next_ch) {
                    env->stack[env->stack_pointer++] = jolie_escape_chars[j].escape_ch;
                    exist = true;
                    break;
                }
            }

            if (!exist) {
                fprintf(
                    stderr, SRC_LOC_FMT": Error: escape character `\\%c` is not supported\n",
                    SRC_LOC_ARG(loc), next_ch);
                exit(1);
            }
        } else {
            env->stack[env->stack_pointer++] = ch;
        }
    }
    env->stack[env->stack_pointer++] = '\0';
    return (uint64_t)(env->stack + string_begin);
}

Jolie_Expr jolie_parse_expr(Jolie_Env *env, Jolie_Lexer *lexer) {
    Jolie_Token peek = jolie_peek_token(lexer);

    Jolie_Expr expr = {0};
    expr.loc = peek.loc;

    switch (peek.type) {
    case JOLIE_PAREN_OPEN: {
        expr.type = JOLIE_EXPR_LIST;
        expr.as.list = jolie_parse_list(env, lexer);
    } break;
    case JOLIE_UINT64: {
        Jolie_Token token = jolie_next_token(lexer);
        expr.type = JOLIE_EXPR_UINT64;
        expr.as.uint64 = sv_to_uint64(token.text);
    } break;
    case JOLIE_STRING: {
        Jolie_Token token = jolie_next_token(lexer);
        expr.type = JOLIE_EXPR_UINT64;
        expr.as.uint64 = jolie_prepare_string(env, token.text, token.loc);
    } break;
    case JOLIE_WORD: {
        Jolie_Token token = jolie_next_token(lexer);
        expr.type = JOLIE_EXPR_WORD;
        expr.as.word = token.text;
    } break;
    case JOLIE_PAREN_CLOSE: {
        fprintf(
            stderr, SRC_LOC_FMT": Error: expected expression, but found `)`\n",
            SRC_LOC_ARG(peek.loc));
        exit(1);
    } break;
    case JOLIE_END: {
        fprintf(
            stderr, SRC_LOC_FMT": Error: unexpected end of file\n",
            SRC_LOC_ARG(peek.loc));
        exit(1);
    } break;
    }

    return expr;
}

Jolie_Block jolie_parse(Jolie_Env *env, Jolie_Lexer *lexer) {
    Jolie_Block program = {0};
    while (true) {
        Jolie_Token peek = jolie_peek_token(lexer);
        if (peek.type == JOLIE_END) {
            break;
        }
        Jolie_Expr expr = jolie_parse_expr(env, lexer);
        da_push(&env->arena, &program, expr);
    }
    return program;
}

typedef struct {
    uint64_t value;
    bool failed;
    String_View error_message;
} Jolie_Runtime_Result;

Jolie_Runtime_Result jolie_success(uint64_t value) {
    Jolie_Runtime_Result result = {0};
    result.value = value;
    return result;
}

Jolie_Runtime_Result jolie_error(String_View message) {
    Jolie_Runtime_Result result = {0};
    result.failed = true;
    result.error_message = message;
    return result;
}

Jolie_Runtime_Result jolie_eval_expr(Jolie_Env *env, Jolie_Expr *expr);
Jolie_Runtime_Result jolie_eval_block(Jolie_Env *env, Jolie_Block *block);

Jolie_Var *jolie_gimme_var(Jolie_Env *env, String_View name) {
    for (size_t i = 0; i < env->vars.count; ++i) {
        if (sv_eq(name, env->vars.items[i].name)) {
            return &env->vars.items[i];
        }
    }
    return NULL;
}

Jolie_Func *jolie_gimme_func(Jolie_Env *env, String_View name) {
    for (size_t i = 0; i < env->funcs.count; ++i) {
        if (sv_eq(name, env->funcs.items[i].name)) {
            return &env->funcs.items[i];
        }
    }
    return NULL;
}

typedef Jolie_Runtime_Result (*Jolie_Intrinsic_Func)(Jolie_Env *env, Jolie_List *list);

Jolie_Runtime_Result jolie_defun_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 3) {
        return jolie_error(SV("Error: invalid `defun` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("defun"))) {
        return jolie_error(SV("Error: invalid `defun` intrinsic"));
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_error(SV("Error: invalid `defun` intrinsic"));
    }
    String_View name = list->items[1].as.word;

    if (list->items[2].type != JOLIE_EXPR_LIST) {
        return jolie_error(SV("Error: invalid `defun` intrinsic"));
    }
    Jolie_List args = list->items[2].as.list;

    for (size_t i = 0; i < args.count; ++i) {
        if (args.items[i].type != JOLIE_EXPR_WORD) {
            return jolie_error(SV("Error: invalid `defun` intrinsic"));
        }
    }

    Jolie_Block block = {0};
    for (size_t i = 3; i < list->count; ++i) {
        da_push(&env->arena, &block, list->items[i]);
    }

    if (jolie_gimme_func(env, name) != NULL) {
        return jolie_error(SV("Error: function already declared"));
    }

    Jolie_Func func = { name, block, args };
    da_push(&env->arena, &env->funcs, func);
    return jolie_success(0);
}

Jolie_Runtime_Result jolie_let_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 3) {
        return jolie_error(SV("Error: invalid `let` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("let"))) {
        return jolie_error(SV("Error: invalid `let` intrinsic"));
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_error(SV("Error: invalid `let` intrinsic"));
    }
    String_View name = list->items[1].as.word;

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t value = result.value;

    if (jolie_gimme_var(env, name) != NULL) {
        return jolie_error(SV("Error: variable already declared"));
    }

    if (env->stack_pointer >= JOLIE_STACK_CAP) {
        return jolie_error(SV("Error: stack overflow"));
    }

    Jolie_Var var = { name, env->stack + env->stack_pointer };
    env->stack[env->stack_pointer] = value;
    env->stack_pointer += 1;

    da_push(&env->arena, &env->vars, var);
    return jolie_success(0);
}

Jolie_Runtime_Result jolie_set_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 3) {
        return jolie_error(SV("Error: invalid `set` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("set"))) {
        return jolie_error(SV("Error: invalid `set` intrinsic"));
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_error(SV("Error: invalid `set` intrinsic"));
    }
    String_View name = list->items[1].as.word;

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t value = result.value;

    Jolie_Var *var = jolie_gimme_var(env, name);
    if (var == NULL) {
        return jolie_error(SV("Error: variable not declared"));
    }

    *var->address = value;
    return jolie_success(0);
}

Jolie_Runtime_Result jolie_make_array_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 2) {
        return jolie_error(SV("Error: invalid `make-array` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("make-array"))) {
        return jolie_error(SV("Error: invalid `make-array` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t array_len = result.value;

    if (env->stack_pointer + array_len >= JOLIE_STACK_CAP) {
        return jolie_error(SV("Error: stack overflow"));
    }

    uint64_t array_address = (uint64_t)(env->stack + env->stack_pointer);
    env->stack_pointer += array_len;

    return jolie_success(array_address);
}

Jolie_Runtime_Result jolie_write_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 3) {
        return jolie_error(SV("Error: invalid `write` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("write"))) {
        return jolie_error(SV("Error: invalid `write` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t *address = (uint64_t*)result.value;

    result = jolie_eval_expr(env, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t value = result.value;

    *address = value;
    return jolie_success(0);
}

Jolie_Runtime_Result jolie_read_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 2) {
        return jolie_error(SV("Error: invalid `read` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("read"))) {
        return jolie_error(SV("Error: invalid `read` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t address = result.value;
    return jolie_success(*(uint64_t*)(address));
}

Jolie_Runtime_Result jolie_while_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `while` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("while"))) {
        return jolie_error(SV("Error: invalid `while` intrinsic"));
    }

    while (true) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
        if (result.failed) {
            return result;
        }
        uint64_t conditional = result.value;
        if (!conditional) {
            break;
        }

        for (size_t i = 2; i < list->count; ++i) {
            Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
            if (result.failed) {
                return result;
            }
        }
    }

    return jolie_success(0);
}

Jolie_Runtime_Result jolie_if_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 4) {
        return jolie_error(SV("Error: invalid `if` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("if"))) {
        return jolie_error(SV("Error: invalid `if` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t conditional = result.value;

    if (conditional) {
        return jolie_eval_expr(env, &list->items[2]);
    } else {
        return jolie_eval_expr(env, &list->items[3]);
    }
}

Jolie_Runtime_Result jolie_print_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `print` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("print"))) {
        return jolie_error(SV("Error: invalid `print` intrinsic"));
    }

    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
        if (result.failed) {
            return result;
        }
        printf("%zu\n", result.value);
    }

    return jolie_success(0);
}

Jolie_Runtime_Result jolie_putchar_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `putchar` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("putchar"))) {
        return jolie_error(SV("Error: invalid `putchar` intrinsic"));
    }

    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
        if (result.failed) {
            return result;
        }
        printf("%c", (char) result.value);
    }

    return jolie_success(0);
}

Jolie_Runtime_Result jolie_add_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `+` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("+"))) {
        return jolie_error(SV("Error: invalid `+` intrinsic"));
    }

    uint64_t sum = 0;
    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
        if (result.failed) {
            return result;
        }
        sum += result.value;
    }
    return jolie_success(sum);
}

Jolie_Runtime_Result jolie_sub_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `-` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("-"))) {
        return jolie_error(SV("Error: invalid `-` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t sub = result.value;

    for (size_t i = 2; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
        if (result.failed) {
            return result;
        }
        sub -= result.value;
    }

    return jolie_success(sub);
}

Jolie_Runtime_Result jolie_mul_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `*` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("*"))) {
        return jolie_error(SV("Error: invalid `*` intrinsic"));
    }

    uint64_t mul = 1;
    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
        if (result.failed) {
            return result;
        }
        mul *= result.value;
    }
    return jolie_success(mul);
}

Jolie_Runtime_Result jolie_div_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count < 2) {
        return jolie_error(SV("Error: invalid `/` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("/"))) {
        return jolie_error(SV("Error: invalid `/` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t div = result.value;

    for (size_t i = 2; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[i]);
        if (result.failed) {
            return result;
        }
        div /= result.value;
    }
    return jolie_success(div);
}

Jolie_Runtime_Result jolie_lt_intrinsic(Jolie_Env *env, Jolie_List *list) {
    if (list->count != 3) {
        return jolie_error(SV("Error: invalid `<` intrinsic"));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD || !sv_eq(list->items[0].as.word, SV("<"))) {
        return jolie_error(SV("Error: invalid `<` intrinsic"));
    }

    Jolie_Runtime_Result result = jolie_eval_expr(env, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t a = result.value;

    result = jolie_eval_expr(env, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t b = result.value;

    return jolie_success(a < b);
}

typedef struct {
    String_View name;
    Jolie_Intrinsic_Func func;
} Jolie_Intrinsic;

Jolie_Intrinsic jolie_intrinsics[] = {
    { .name = SV_STATIC("defun"), .func = jolie_defun_intrinsic },
    { .name = SV_STATIC("let"), .func = jolie_let_intrinsic },
    { .name = SV_STATIC("set"), .func = jolie_set_intrinsic },
    { .name = SV_STATIC("make-array"), .func = jolie_make_array_intrinsic },
    { .name = SV_STATIC("write"), .func = jolie_write_intrinsic },
    { .name = SV_STATIC("read"), .func = jolie_read_intrinsic },
    { .name = SV_STATIC("if"), .func = jolie_if_intrinsic },
    { .name = SV_STATIC("while"), .func = jolie_while_intrinsic },
    { .name = SV_STATIC("print"), .func = jolie_print_intrinsic },
    { .name = SV_STATIC("putchar"), .func = jolie_putchar_intrinsic },
    { .name = SV_STATIC("+"), .func = jolie_add_intrinsic },
    { .name = SV_STATIC("-"), .func = jolie_sub_intrinsic },
    { .name = SV_STATIC("*"), .func = jolie_mul_intrinsic },
    { .name = SV_STATIC("/"), .func = jolie_div_intrinsic },
    { .name = SV_STATIC("<"), .func = jolie_lt_intrinsic },
};
size_t jolie_intrinsics_count =
    sizeof(jolie_intrinsics)/sizeof(jolie_intrinsics[0]);

bool jolie_is_func_call(Jolie_List *list) {
    if (list->count <= 0) {
        return false;
    }
    Jolie_Expr expr = list->items[0];
    return expr.type == JOLIE_EXPR_WORD;
}

Jolie_Runtime_Result jolie_eval_expr(Jolie_Env *env, Jolie_Expr *expr) {
    switch (expr->type) {
    case JOLIE_EXPR_LIST: {
        Jolie_List *list = &expr->as.list;
        if (jolie_is_func_call(list)) {
            String_View func_name = list->items[0].as.word;
            for (size_t i = 0; i < jolie_intrinsics_count; ++i) {
                Jolie_Intrinsic *intr = &jolie_intrinsics[i];
                if (sv_eq(func_name, intr->name)) {
                    return intr->func(env, list);
                }
            }
            for (size_t i = 0; i < env->funcs.count; ++i) {
                Jolie_Func *func = &env->funcs.items[i];
                if (sv_eq(func_name, func->name)) {
                    return jolie_eval_block(env, &func->block);
                }
            }
            return jolie_error(SV("Error: unknown function"));
        }
        return jolie_error(SV("Error: list is not a function call"));
    } break;
    case JOLIE_EXPR_UINT64: {
        return jolie_success(expr->as.uint64);
    } break;
    case JOLIE_EXPR_WORD: {
        for (size_t i = 0; i < env->vars.count; ++i) {
            if (sv_eq(expr->as.word, env->vars.items[i].name)) {
                return jolie_success(*env->vars.items[i].address);
            }
        }
        return jolie_error(SV("Error: unknown variable"));
    } break;
    default: {
        fprintf(stderr, "Error: unreachable state (at `jolie_eval_expr`)\n");
        exit(1);
    }
    }
}

Jolie_Runtime_Result jolie_eval_block(Jolie_Env *env, Jolie_Block *block) {
    for (size_t i = 0; i < block->count; ++i) {
        Jolie_Expr *expr = &block->items[i];
        Jolie_Runtime_Result result = jolie_eval_expr(env, expr);
        if (result.failed) {
            return result;
        }
    }
    return jolie_success(0);
}

String read_file(Arena *arena, const char *filepath) {
    FILE *file = fopen(filepath, "rb");
    if (file == NULL) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno));
        exit(1);
    }

    if (fseek(file, 0, SEEK_END) != 0) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno));
        exit(1);
    }

    size_t count = ftell(file);
    rewind(file);

    String string = {0};
    string.data = arena_alloc(arena, count * sizeof(char));
    string.length = count;

    fread(string.data, sizeof(char), count, file);
    if (ferror(file)) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno));
        exit(1);
    }

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

    String_View content = { file.data, file.length };
    Jolie_Lexer lexer = jolie_lexer_from_sv(src_filepath, content);

    Jolie_Env env = { .arena = arena };

    Jolie_Block block = jolie_parse(&env, &lexer);
    Jolie_Runtime_Result result = jolie_eval_block(&env, &block);
    if (result.failed) {
        fprintf(stderr, SV_FMT"\n", SV_ARG(result.error_message));
        exit(1);
    }

    arena_free(&arena);
    return 0;
}
