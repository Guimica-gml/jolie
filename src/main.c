#include "./utils.h"

#define ARENA_IMPLEMENTATION
#include "./arena.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#include <errno.h>

#define JOLIE_LOC_FMT "%s:%zu:%zu"
#define JOLIE_LOC_ARG(loc) (loc).filepath, (loc).row, (loc).col

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
} Jolie_Loc;

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
    Jolie_Loc loc;
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
    default: assert(0 && "unreachable");
    }
}

Jolie_Lexer jolie_lexer_from_sv(const char *src_filepath, String_View content) {
    Jolie_Lexer lexer = {0};
    lexer.src_filepath = src_filepath;
    lexer.content = content;
    return lexer;
}

static inline String_View sv_from_parts(const char *data, size_t size) {
    return (String_View) { data, size };
}

static inline void jolie_consume_char(Jolie_Lexer *lexer) {
    assert(lexer->index < lexer->content.size);
    if (lexer->content.data[lexer->index] == '\n') {
        lexer->line += 1;
        lexer->bol = lexer->index + 1;
    }
    lexer->index += 1;
}

String_View jolie_chop(Jolie_Lexer *lexer, size_t count) {
    String_View text = { &lexer->content.data[lexer->index], count };
    for (size_t i = 0; i < count; ++i) {
        jolie_consume_char(lexer);
    }
    return text;
}

String_View jolie_chop_until(Jolie_Lexer *lexer, char ch) {
    size_t count = 0;
    while (lexer->index + count < lexer->content.size
           && ch != lexer->content.data[lexer->index + count])
    {
        count += 1;
    }
    return jolie_chop(lexer, count);
}

String_View jolie_chop_while(Jolie_Lexer *lexer, int(*predicate)(int)) {
    size_t count = 0;
    while (lexer->index + count < lexer->content.size
           && predicate(lexer->content.data[lexer->index + count]))
    {
        count += 1;
    }
    return jolie_chop(lexer, count);
}

#define JOLIE_COMMENT_CHAR ';'

int jolie_is_word(int ch) {
    if (ch == JOLIE_COMMENT_CHAR) {
        return false;
    }
    for (size_t i = 0; i < jolie_literal_tokens_count; ++i) {
        if (ch == jolie_literal_tokens[i].ch) {
            return false;
        }
    }
    return !isspace(ch);
}

// Yes, X macros make me feel smart (NO, YOU ARE DUMB, WHAT IS THAT, TODO: REMOVE IT)
// A particular coding style :kapp:
#define JOLIE_RESULT_TYPES                      \
    X(Jolie_Lexer_Result, lexer, Jolie_Token)   \
    X(Jolie_Uint64_Result, uint64, uint64_t)    \
    X(Jolie_Runtime_Result, runtime, uint64_t)

#define X(result_type, name, value_type)                                \
    typedef struct {                                                    \
        value_type value;                                               \
        bool failed;                                                    \
        String error_message;                                           \
    } result_type;                                                      \
    result_type jolie_##name##_success(value_type value) {              \
        result_type result = {0};                                       \
        result.value = value;                                           \
        return result;                                                  \
    }                                                                   \
    result_type jolie_##name##_error(Arena *arena, const char *fmt, ...) { \
        result_type result = {0};                                       \
        result.failed = true;                                           \
        va_list args;                                                   \
        va_start(args, fmt);                                            \
        str_append_vfmt(arena, &result.error_message, fmt, args);       \
        va_end(args);                                                   \
        return result;                                                  \
    }
JOLIE_RESULT_TYPES
#undef X

Jolie_Token jolie_next_token(Jolie_Lexer *lexer) {
again:
    jolie_chop_while(lexer, isspace);

    Jolie_Token token = {0};
    token.loc.filepath = lexer->src_filepath;
    token.loc.col = lexer->index - lexer->bol + 1;
    token.loc.row = lexer->line + 1;

    if (lexer->index >= lexer->content.size) {
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
        jolie_consume_char(lexer);
        const char *begin = lexer->content.data + lexer->index;
        size_t begin_index = lexer->index;

        bool escaped = false;
        while (true) {
            if (lexer->index >= lexer->content.size
                || lexer->content.data[lexer->index] == '\n')
            {
                fprintf(
                    stderr, JOLIE_LOC_FMT": Error: unclosed string literal\n",
                    JOLIE_LOC_ARG(token.loc));
                exit(1);
            }

            char ch = lexer->content.data[lexer->index];
            jolie_consume_char(lexer);

            if (!escaped && ch == '\"') {
                break;
            }
            escaped = (!escaped && ch == '\\');
        }

        token.type = JOLIE_STRING;
        token.text = sv_from_parts(begin, lexer->index - begin_index - 1);
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
    Jolie_Loc loc;
    Jolie_Expr_Type type;
    Jolie_Expr_As as;
};

typedef struct {
    Jolie_Expr *items;
    size_t count;
    size_t capacity;
} Jolie_Block;

typedef struct {
    String_View name;
    uint64_t *address;
    Jolie_Loc loc;
} Jolie_Var;

typedef struct {
    String_View name;
    Jolie_Block block;
    Jolie_List arg_names;
    Jolie_Loc loc;
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
    uint64_t items[JOLIE_STACK_CAP];
    size_t count;
} Jolie_Stack;

typedef struct Jolie_Scope Jolie_Scope;

struct Jolie_Scope {
    Jolie_Vars vars;
    Jolie_Funcs funcs;
    Jolie_Scope *prev_scope;
};

Jolie_Scope jolie_make_parallel_scope(Jolie_Scope *curr_scope) {
    Jolie_Scope scope = {0};
    Jolie_Scope *global_scope = curr_scope;
    while (global_scope->prev_scope != NULL) {
        global_scope = global_scope->prev_scope;
    }
    scope.prev_scope = global_scope;
    return scope;
}

Jolie_Scope jolie_make_inherited_scope(Jolie_Scope *curr_scope) {
    Jolie_Scope scope = {0};
    scope.prev_scope = curr_scope;
    return scope;
}

Jolie_List jolie_parse_list(Arena *arena, Jolie_Stack *stack, Jolie_Lexer *lexer);
Jolie_Expr jolie_parse_expr(Arena *arena, Jolie_Stack *stack, Jolie_Lexer *lexer);

Jolie_List jolie_parse_list(Arena *arena, Jolie_Stack *stack, Jolie_Lexer *lexer) {
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
                stderr, JOLIE_LOC_FMT": Error: unclosed parenthesis\n",
                JOLIE_LOC_ARG(paren_open.loc));
            exit(1);
        } else {
            Jolie_Expr expr = jolie_parse_expr(arena, stack, lexer);
            arena_da_append(arena, &expr_list, expr);
        }
    }

    return expr_list;
}

uint64_t jolie_prepare_uint64(String_View sv, Jolie_Loc loc) {
    uint64_t num = 0;
    for (size_t i = 0; i < sv.size; ++i) {
        if (!isdigit(sv.data[i])) {
            fprintf(
                stderr,
                JOLIE_LOC_FMT": Error: invalid literal integer `"SV_FMT"`\n",
                JOLIE_LOC_ARG(loc), SV_ARG(sv));
            exit(1);
        }
        num = (sv.data[i] - '0') + num * 10;
    }
    return num;
}

uint64_t jolie_prepare_string(Jolie_Stack *stack, String_View sv, Jolie_Loc loc) {
    size_t string_begin = stack->count;
    size_t backslash_index;
    while (sv_find(sv, '\\', &backslash_index)) {
        for (size_t i = 0; i < backslash_index; ++i) {
            stack->items[stack->count++] = sv.data[i];
        }
        assert(backslash_index + 1 < sv.size);
        char special_ch = sv.data[backslash_index + 1];
        switch (special_ch) {
        case 'f': stack->items[stack->count++] = '\f'; break;
        case 'r': stack->items[stack->count++] = '\r'; break;
        case 'b': stack->items[stack->count++] = '\b'; break;
        case 'n': stack->items[stack->count++] = '\n'; break;
        case 't': stack->items[stack->count++] = '\t'; break;
        case '0': stack->items[stack->count++] = '\0'; break;
        case '\'': stack->items[stack->count++] = '\''; break;
        case '\"': stack->items[stack->count++] = '\"'; break;
        case '\\': stack->items[stack->count++] = '\\'; break;
        default:
            fprintf(
                stderr, JOLIE_LOC_FMT": Error: escape character `\\%c` is not supported\n",
                JOLIE_LOC_ARG(loc), special_ch);
            exit(1);
        }
        sv.data = sv.data + backslash_index + 2;
        sv.size = sv.size - backslash_index - 2;
    }
    for (size_t i = 0; i < sv.size; ++i) {
        stack->items[stack->count++] = sv.data[i];
    }
    stack->items[stack->count++] = '\0';
    return (uint64_t)(stack->items + string_begin);
}

Jolie_Expr jolie_parse_expr(Arena *arena, Jolie_Stack *stack, Jolie_Lexer *lexer) {
    Jolie_Token peek = jolie_peek_token(lexer);

    Jolie_Expr expr = {0};
    expr.loc = peek.loc;

    switch (peek.type) {
    case JOLIE_PAREN_OPEN: {
        expr.type = JOLIE_EXPR_LIST;
        expr.as.list = jolie_parse_list(arena, stack, lexer);
    } break;
    case JOLIE_UINT64: {
        Jolie_Token token = jolie_next_token(lexer);
        expr.type = JOLIE_EXPR_UINT64;
        expr.as.uint64 = jolie_prepare_uint64(token.text, token.loc);
    } break;
    case JOLIE_STRING: {
        Jolie_Token token = jolie_next_token(lexer);
        expr.type = JOLIE_EXPR_UINT64;
        expr.as.uint64 = jolie_prepare_string(stack, token.text, token.loc);
    } break;
    case JOLIE_WORD: {
        Jolie_Token token = jolie_next_token(lexer);
        expr.type = JOLIE_EXPR_WORD;
        expr.as.word = token.text;
    } break;
    case JOLIE_PAREN_CLOSE: {
        fprintf(
            stderr, JOLIE_LOC_FMT": Error: expected expression, but found `)`\n",
            JOLIE_LOC_ARG(peek.loc));
        exit(1);
    } break;
    case JOLIE_END: {
        fprintf(
            stderr, JOLIE_LOC_FMT": Error: unexpected end of file\n",
            JOLIE_LOC_ARG(peek.loc));
        exit(1);
    } break;
    }

    return expr;
}

Jolie_Block jolie_parse(
    Arena *arena,
    Jolie_Stack *stack, Jolie_Lexer *lexer)
{
    Jolie_Block program = {0};
    while (true) {
        Jolie_Token peek = jolie_peek_token(lexer);
        if (peek.type == JOLIE_END) {
            break;
        }
        Jolie_Expr expr = jolie_parse_expr(arena, stack, lexer);
        arena_da_append(arena, &program, expr);
    }
    return program;
}

Jolie_Runtime_Result jolie_eval_expr(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_Expr *expr);
Jolie_Runtime_Result jolie_eval_block(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_Block *block);

Jolie_Var *jolie_gimme_var_local(Jolie_Scope *scope, String_View name) {
    for (size_t i = 0; i < scope->vars.count; ++i) {
        if (sv_eq(name, scope->vars.items[i].name)) {
            return &scope->vars.items[i];
        }
    }
    return NULL;
}

Jolie_Var *jolie_gimme_var(Jolie_Scope *scope, String_View name) {
    while (scope != NULL) {
        for (size_t i = 0; i < scope->vars.count; ++i) {
            if (sv_eq(name, scope->vars.items[i].name)) {
                return &scope->vars.items[i];
            }
        }
        scope = scope->prev_scope;
    }
    return NULL;
}

Jolie_Func *jolie_gimme_func_local(Jolie_Scope *scope, String_View name) {
    for (size_t i = 0; i < scope->funcs.count; ++i) {
        if (sv_eq(name, scope->funcs.items[i].name)) {
            return &scope->funcs.items[i];
        }
    }
    return NULL;
}

Jolie_Func *jolie_gimme_func(Jolie_Scope *scope, String_View name) {
    while (scope != NULL) {
        for (size_t i = 0; i < scope->funcs.count; ++i) {
            if (sv_eq(name, scope->funcs.items[i].name)) {
                return &scope->funcs.items[i];
            }
        }
        scope = scope->prev_scope;
    }
    return NULL;
}

typedef Jolie_Runtime_Result (*Jolie_Intrinsic_Func)(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc);

Jolie_Runtime_Result jolie_defun_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) stack;

    if (list->count < 3) {
        return jolie_runtime_error(
            arena,
            JOLIE_LOC_FMT": Error: `defun` intrinsic expects at least 3 items",
            JOLIE_LOC_ARG(loc));
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("defun")))
    {
        return jolie_runtime_error(
            arena,
            JOLIE_LOC_FMT": Error: `defun` intrinsic should start with the `defun` symbol, "
            "this is probably an interpreter bug", JOLIE_LOC_ARG(loc));
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_runtime_error(
            arena, JOLIE_LOC_FMT": Error: expected name of function",
            JOLIE_LOC_ARG(list->items[1].loc));
    }
    String_View name = list->items[1].as.word;

    if (list->items[2].type != JOLIE_EXPR_LIST) {
        return jolie_runtime_error(
            arena, JOLIE_LOC_FMT": Error: expected list of arguments",
            JOLIE_LOC_ARG(list->items[2].loc));
    }
    Jolie_List args = list->items[2].as.list;

    for (size_t i = 0; i < args.count; ++i) {
        if (args.items[i].type != JOLIE_EXPR_WORD) {
            return jolie_runtime_error(
                arena, JOLIE_LOC_FMT": Error: all element of arguments list must be words",
                JOLIE_LOC_ARG(args.items[i].loc));
        }
    }

    Jolie_Block block = {0};
    for (size_t i = 3; i < list->count; ++i) {
        arena_da_append(arena, &block, list->items[i]);
    }

    Jolie_Func *og_func = jolie_gimme_func_local(scope, name);
    if (og_func != NULL) {
        return jolie_runtime_error(
            arena, JOLIE_LOC_FMT": Error: function `"SV_FMT"` already declared\n"
            JOLIE_LOC_FMT": Note: original function declared here",
            JOLIE_LOC_ARG(loc), SV_ARG(og_func->name), JOLIE_LOC_ARG(og_func->loc));
    }

    Jolie_Func func = { name, block, args, loc };
    arena_da_append(arena, &scope->funcs, func);
    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_let_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    if (list->count != 3) {
        return jolie_runtime_error(arena, "Error: invalid `let` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("let")))
    {
        return jolie_runtime_error(arena, "Error: invalid `let` intrinsic");
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_runtime_error(arena, "Error: invalid `let` intrinsic");
    }
    String_View name = list->items[1].as.word;

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t value = result.value;

    if (jolie_gimme_var_local(scope, name) != NULL) {
        return jolie_runtime_error(arena, "Error: variable already declared");
    }

    if (stack->count >= JOLIE_STACK_CAP) {
        return jolie_runtime_error(arena, "Error: stack overflow");
    }

    Jolie_Var var = { name, stack->items + stack->count, loc };
    stack->items[stack->count++] = value;

    arena_da_append(arena, &scope->vars, var);
    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_set_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count != 3) {
        return jolie_runtime_error(arena, "Error: invalid `set` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("set")))
    {
        return jolie_runtime_error(arena, "Error: invalid `set` intrinsic");
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_runtime_error(arena, "Error: invalid `set` intrinsic");
    }
    String_View name = list->items[1].as.word;

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t value = result.value;

    Jolie_Var *var = jolie_gimme_var(scope, name);
    if (var == NULL) {
        return jolie_runtime_error(arena, "Error: variable not declared");
    }

    *var->address = value;
    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_make_array_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count != 2) {
        return jolie_runtime_error(arena, "Error: invalid `make-array` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("make-array")))
    {
        return jolie_runtime_error(arena, "Error: invalid `make-array` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t array_len = result.value;

    if (stack->count + array_len >= JOLIE_STACK_CAP) {
        return jolie_runtime_error(arena, "Error: stack overflow");
    }

    uint64_t array_address = (uint64_t)(stack->items + stack->count);
    stack->count += array_len;

    return jolie_runtime_success(array_address);
}

Jolie_Runtime_Result jolie_write_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count != 3) {
        return jolie_runtime_error(arena, "Error: invalid `write` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("write")))
    {
        return jolie_runtime_error(arena, "Error: invalid `write` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t *address = (uint64_t*)result.value;

    result = jolie_eval_expr(arena, stack, scope, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t value = result.value;

    *address = value;
    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_read_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count != 2) {
        return jolie_runtime_error(arena, "Error: invalid `read` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("read")))
    {
        return jolie_runtime_error(arena, "Error: invalid `read` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t address = result.value;
    return jolie_runtime_success(*(uint64_t*)(address));
}

Jolie_Runtime_Result jolie_get_ref_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) stack;
    (void) loc;
    if (list->count != 2) {
        return jolie_runtime_error(arena, "Error: invalid `&` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("&")))
    {
        return jolie_runtime_error(arena, "Error: invalid `&` intrinsic");
    }

    if (list->items[1].type != JOLIE_EXPR_WORD) {
        return jolie_runtime_error(arena, "Error: invalid `&` intrinsic");
    }
    String_View name = list->items[1].as.word;

    Jolie_Var *var = jolie_gimme_var(scope, name);
    if (var == NULL) {
        return jolie_runtime_error(arena, "Error: variable not declared");
    }

    return jolie_runtime_success((uint64_t)var->address);
}

Jolie_Runtime_Result jolie_while_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `while` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("while")))
    {
        return jolie_runtime_error(arena, "Error: invalid `while` intrinsic");
    }

    Jolie_Block block = {0};
    for (size_t i = 2; i < list->count; ++i) {
        arena_da_append(arena, &block, list->items[i]);
    }

    while (true) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[1]);
        if (result.failed) {
            return result;
        }
        uint64_t conditional = result.value;
        if (!conditional) {
            break;
        }

        Jolie_Scope block_scope = jolie_make_inherited_scope(scope);
        result = jolie_eval_block(arena, stack, &block_scope, &block);
        if (result.failed) {
            return result;
        }
    }

    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_if_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count != 4) {
        return jolie_runtime_error(arena, "Error: invalid `if` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("if")))
    {
        return jolie_runtime_error(arena, "Error: invalid `if` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t conditional = result.value;

    if (conditional) {
        return jolie_eval_expr(arena, stack, scope, &list->items[2]);
    } else {
        return jolie_eval_expr(arena, stack, scope, &list->items[3]);
    }
}

Jolie_Runtime_Result jolie_putd_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `putd` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("putd")))
    {
        return jolie_runtime_error(arena, "Error: invalid `putd` intrinsic");
    }

    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[i]);
        if (result.failed) {
            return result;
        }
        printf("%zu\n", result.value);
    }

    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_putc_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `putc` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("putc")))
    {
        return jolie_runtime_error(arena, "Error: invalid `putc` intrinsic");
    }

    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[i]);
        if (result.failed) {
            return result;
        }
        printf("%c", (char) result.value);
    }

    return jolie_runtime_success(0);
}

Jolie_Runtime_Result jolie_add_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `+` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("+")))
    {
        return jolie_runtime_error(arena, "Error: invalid `+` intrinsic");
    }

    uint64_t sum = 0;
    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[i]);
        if (result.failed) {
            return result;
        }
        sum += result.value;
    }
    return jolie_runtime_success(sum);
}

Jolie_Runtime_Result jolie_sub_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `-` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("-")))
    {
        return jolie_runtime_error(arena, "Error: invalid `-` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t sub = result.value;

    for (size_t i = 2; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[i]);
        if (result.failed) {
            return result;
        }
        sub -= result.value;
    }

    return jolie_runtime_success(sub);
}

Jolie_Runtime_Result jolie_mul_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `*` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("*")))
    {
        return jolie_runtime_error(arena, "Error: invalid `*` intrinsic");
    }

    uint64_t mul = 1;
    for (size_t i = 1; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[i]);
        if (result.failed) {
            return result;
        }
        mul *= result.value;
    }
    return jolie_runtime_success(mul);
}

Jolie_Runtime_Result jolie_div_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count < 2) {
        return jolie_runtime_error(arena, "Error: invalid `/` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("/")))
    {
        return jolie_runtime_error(arena, "Error: invalid `/` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t div = result.value;

    for (size_t i = 2; i < list->count; ++i) {
        Jolie_Runtime_Result result = jolie_eval_expr(
            arena, stack, scope, &list->items[i]);
        if (result.failed) {
            return result;
        }
        div /= result.value;
    }
    return jolie_runtime_success(div);
}

Jolie_Runtime_Result jolie_lt_intrinsic(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_List *list, Jolie_Loc loc)
{
    (void) loc;
    if (list->count != 3) {
        return jolie_runtime_error(arena, "Error: invalid `<` intrinsic");
    }

    if (list->items[0].type != JOLIE_EXPR_WORD
        || !sv_eq(list->items[0].as.word, SV("<")))
    {
        return jolie_runtime_error(arena, "Error: invalid `<` intrinsic");
    }

    Jolie_Runtime_Result result = jolie_eval_expr(arena, stack, scope, &list->items[1]);
    if (result.failed) {
        return result;
    }
    uint64_t a = result.value;

    result = jolie_eval_expr(arena, stack, scope, &list->items[2]);
    if (result.failed) {
        return result;
    }
    uint64_t b = result.value;

    return jolie_runtime_success(a < b);
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
    { .name = SV_STATIC("&"), .func = jolie_get_ref_intrinsic },
    { .name = SV_STATIC("if"), .func = jolie_if_intrinsic },
    { .name = SV_STATIC("while"), .func = jolie_while_intrinsic },
    { .name = SV_STATIC("putd"), .func = jolie_putd_intrinsic },
    { .name = SV_STATIC("putc"), .func = jolie_putc_intrinsic },
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
        printf("huh\n");
        return false;
    }
    Jolie_Expr expr = list->items[0];
    return expr.type == JOLIE_EXPR_WORD;
}

Jolie_Runtime_Result jolie_eval_expr(
    Arena *arena,
    Jolie_Stack *stack, Jolie_Scope *scope, Jolie_Expr *expr)
{
    switch (expr->type) {
    case JOLIE_EXPR_LIST: {
        Jolie_List *list = &expr->as.list;
        if (jolie_is_func_call(list)) {
            String_View func_name = list->items[0].as.word;
            Jolie_Loc func_name_loc = list->items[0].loc;
            for (size_t i = 0; i < jolie_intrinsics_count; ++i) {
                Jolie_Intrinsic *intr = &jolie_intrinsics[i];
                if (sv_eq(func_name, intr->name)) {
                    return intr->func(arena, stack, scope, list, expr->loc);
                }
            }
            Jolie_Func *func = jolie_gimme_func(scope, func_name);
            if (func != NULL) {
                Jolie_Scope func_scope = jolie_make_parallel_scope(scope);
                if (list->count != func->arg_names.count + 1) {
                    return jolie_runtime_error(arena, "Error: wrong number of arguments");
                }
                for (size_t i = 0; i < func->arg_names.count; ++i) {
                    Jolie_Runtime_Result result = jolie_eval_expr(
                        arena, stack, scope, &list->items[i + 1]);
                    if (result.failed) {
                        return result;
                    }
                    uint64_t arg = result.value;

                    if (stack->count >= JOLIE_STACK_CAP) {
                        return jolie_runtime_error(arena, "Error: stack overflow");
                    }
                    String_View name = func->arg_names.items[i].as.word;
                    Jolie_Loc loc = func->arg_names.items[i].loc;
                    Jolie_Var var = { name, stack->items + stack->count, loc };
                    stack->items[stack->count++] = arg;

                    arena_da_append(arena, &func_scope.vars, var);
                }
                return jolie_eval_block(arena, stack, &func_scope, &func->block);
            }
            return jolie_runtime_error(
                arena, JOLIE_LOC_FMT": Error: unknown function `"SV_FMT"`",
                JOLIE_LOC_ARG(func_name_loc), SV_ARG(func_name));
        }
        return jolie_runtime_error(arena, "Error: list is not a function call");
    } break;
    case JOLIE_EXPR_UINT64: {
        return jolie_runtime_success(expr->as.uint64);
    } break;
    case JOLIE_EXPR_WORD: {
        Jolie_Var *var = jolie_gimme_var(scope, expr->as.word);
        if (var != NULL) {
            return jolie_runtime_success(*var->address);
        }
        return jolie_runtime_error(arena, "Error: unknown variable");
    } break;
    default:
        assert(0 && "unreachable");
    }
}

Jolie_Runtime_Result jolie_eval_block(
    Arena *arena, Jolie_Stack *stack, Jolie_Scope *scope, Jolie_Block *block)
{
    size_t body_stack_index = stack->count;
    Jolie_Runtime_Result result = {0};

    for (size_t i = 0; i < block->count; ++i) {
        Jolie_Expr *expr = &block->items[i];
        result = jolie_eval_expr(arena, stack, scope, expr);
        if (result.failed) {
            return result;
        }
    }

    stack->count = body_stack_index;
    // NOTE: Returns the evaluation of the last statement of the body
    return result;
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

    char *data = malloc(count * sizeof(char));
    fread(data, sizeof(char), count, file);
    if (ferror(file)) {
        fprintf(
            stderr, "Error: could not read '%s': %s\n",
            filepath, strerror(errno));
        exit(1);
    }

    String string = {0};
    arena_da_append_many(arena, &string, data, count);

    free(data);
    fclose(file);
    return string;
}

#define str_to_sv(str) ((String_View) { .data = (str).items, .size = (str).count })

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

    Jolie_Stack stack = {0};
    Jolie_Scope scope = {0};

    Jolie_Block block = jolie_parse(&arena, &stack, &lexer);
    Jolie_Runtime_Result result = jolie_eval_block(&arena, &stack, &scope, &block);
    if (result.failed) {
        fprintf(stderr, SV_FMT"\n", SV_ARG(str_to_sv(result.error_message)));
        exit(1);
    }

    arena_free(&arena);
    return 0;
}
