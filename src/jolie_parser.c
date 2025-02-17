#include "./jolie_parser.h"

Jolie_Buintin_Type_Info jolie_builtin_types[JOLIE_TYPE_COUNT] = {
    [JOLIE_TYPE_UINT64] = { .name = SV_STATIC("uint64"), .size = 8 },
    [JOLIE_TYPE_UINT32] = { .name = SV_STATIC("uint32"), .size = 4 },
    [JOLIE_TYPE_UIN16] = { .name = SV_STATIC("uint16"), .size = 2 },
    [JOLIE_TYPE_UINT8] = { .name = SV_STATIC("uint8"), .size = 1 },
    [JOLIE_TYPE_BOOL] = { .name = SV_STATIC("bool"), .size = 1 },
};

Jolie_Token jolie_parse_next_token(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Token token = jolie_next_token(lexer);
    static_assert(JOLIE_TOKEN_COUNT == 28, "Count of tokens changed");
    switch (token.type) {
    case JOLIE_UNCLOSED_STRING: {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: unclosed string literal", JOLIE_LOC_ARG(token.loc));
    } break;
    case JOLIE_UNKNOWN_CHARACTER: {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: unknown character `"SV_FMT"\n",
            JOLIE_LOC_ARG(token.loc), SV_ARG(token.text));
    } break;
    default: {}
    }
    return token;
}

Jolie_Token jolie_parse_peek_token(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Lexer save_lexer = *lexer;
    Jolie_Token peek = jolie_parse_next_token(arena, ast, lexer);
    *lexer = save_lexer;
    return peek;
}

Jolie_Token jolie_parse_expect(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer, Jolie_Token_Type type) {
    Jolie_Token token = jolie_parse_next_token(arena, ast, lexer);
    if (token.type != type) {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected %s\n",
            JOLIE_LOC_ARG(token.loc), SV_ARG(token.text), jolie_token_type_to_cstr(type));
    }
    return token;
}

void str_append_type(Arena *arena, String *str, Jolie_Type type) {
    Jolie_Buintin_Type_Info info = jolie_builtin_types[type.id];
    for (size_t i= 0; i < type.indirection_level; ++i) {
        str_append_char(arena, str, '^');
    }
    str_append_fmt(arena, str, SV_FMT, SV_ARG(info.name));
}

size_t jolie_prepare_string(Arena *arena, Jolie_Ast *ast, String_View sv, Jolie_Loc loc) {
    size_t string_begin = ast->strings.count;
    size_t backslash_index;
    while (sv_find(sv, '\\', &backslash_index)) {
        arena_da_append_many(arena, &ast->strings, sv.data, backslash_index);
        assert(backslash_index + 1 < sv.size);
        char special_ch = sv.data[backslash_index + 1];
        switch (special_ch) {
        case 'f': str_append_char(arena, &ast->strings, '\f'); break;
        case 'r': str_append_char(arena, &ast->strings, '\r'); break;
        case 'b': str_append_char(arena, &ast->strings, '\b'); break;
        case 'n': str_append_char(arena, &ast->strings, '\n'); break;
        case 't': str_append_char(arena, &ast->strings, '\t'); break;
        case '0': str_append_char(arena, &ast->strings, '\0'); break;
        case '\'': str_append_char(arena, &ast->strings, '\''); break;
        case '\"': str_append_char(arena, &ast->strings, '\"'); break;
        case '\\': str_append_char(arena, &ast->strings, '\\'); break;
        default:
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: escape character `\\%c` is not supported\n",
                JOLIE_LOC_ARG(loc), special_ch);
            return string_begin;
        }
        sv.data = sv.data + backslash_index + 2;
        sv.size = sv.size - backslash_index - 2;
    }
    arena_da_append_many(arena, &ast->strings, sv.data, sv.size);
    str_append_null(arena, &ast->strings);
    return string_begin;
}

Jolie_Expr jolie_parse_expr(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Type jolie_parse_type(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Stmt jolie_parse_stmt(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Exprs jolie_parse_proc_call_args(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Proc jolie_parse_proc(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Block jolie_parse_block(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Ast jolie_parse(Arena *arena, Jolie_Lexer *lexer);

Jolie_Binary_Op_Type jolie_bin_op_table[] = {
    [JOLIE_PLUS] = JOLIE_BINARY_OP_ADD,
    [JOLIE_DASH] = JOLIE_BINARY_OP_SUB,
    [JOLIE_ASTERISK] = JOLIE_BINARY_OP_MUL,
    [JOLIE_SLASH] = JOLIE_BINARY_OP_DIV,
    [JOLIE_LESS_THAN] = JOLIE_BINARY_OP_LT,
};

Jolie_Expr jolie_parse_expr(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Expr expr = {0};
    Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
    if (ast->failed) {
        return expr;
    }
    expr.loc = peek.loc;

    switch (peek.type) {
    case JOLIE_WORD: {
        Jolie_Token word = jolie_parse_expect(arena, ast, lexer, JOLIE_WORD);
        if (ast->failed) {
            return expr;
        }

        Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }

        if (peek.type == JOLIE_PAREN_OPEN) {
            expr.type = JOLIE_EXPR_PROC_CALL;
            expr.as.proc_call.proc_name = word.text;
            expr.as.proc_call.args = jolie_parse_proc_call_args(arena, ast, lexer);
            if (ast->failed) {
                return expr;
            }
        } else {
            expr.type = JOLIE_EXPR_WORD;
            expr.as.word = word.text;
        }
    } break;
    case JOLIE_UINT64_LIT: {
        Jolie_Token word = jolie_parse_expect(arena, ast, lexer, JOLIE_UINT64_LIT);
        if (ast->failed) {
            return expr;
        }
        expr.type = JOLIE_EXPR_UINT64;
        // TODO(nic): make sure word.text is a valid uint64 literal
        expr.as.uint64 = sv_to_uint64(word.text);
    } break;
    case JOLIE_STRING_LIT: {
        Jolie_Token word = jolie_parse_expect(arena, ast, lexer, JOLIE_STRING_LIT);
        if (ast->failed) {
            return expr;
        }
        expr.type = JOLIE_EXPR_STRING_INDEX;
        expr.as.string_index = jolie_prepare_string(arena, ast, word.text, word.loc);
        if (ast->failed) {
            return expr;
        }
    } break;
    case JOLIE_CARET: {
        jolie_parse_expect(arena, ast, lexer, JOLIE_CARET);
        if (ast->failed) {
            return expr;
        }
        expr.type = JOLIE_EXPR_DEREF;
        expr.as.deref.expr = arena_alloc(arena, sizeof(Jolie_Expr));

        *expr.as.deref.expr = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }
    } break;
    case JOLIE_CAST: {
        jolie_parse_expect(arena, ast, lexer, JOLIE_CAST);
        if (ast->failed) {
            return expr;
        }

        jolie_parse_expect(arena, ast, lexer, JOLIE_PAREN_OPEN);
        if (ast->failed) {
            return expr;
        }

        Jolie_Type type = jolie_parse_type(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }

        jolie_parse_expect(arena, ast, lexer, JOLIE_PAREN_CLOSE);
        if (ast->failed) {
            return expr;
        }

        expr.type = JOLIE_EXPR_CAST;
        expr.as.cast.type = type;
        expr.as.cast.expr = arena_alloc(arena, sizeof(Jolie_Expr));

        *expr.as.cast.expr = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }
    } break;
    case JOLIE_PLUS:
    case JOLIE_DASH:
    case JOLIE_ASTERISK:
    case JOLIE_SLASH:
    case JOLIE_LESS_THAN: {
        Jolie_Token op = jolie_parse_next_token(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }
        expr.type = JOLIE_EXPR_BINARY_OP;
        expr.as.bin_op.type = jolie_bin_op_table[op.type];
        expr.as.bin_op.lh = arena_alloc(arena, sizeof(Jolie_Expr));
        expr.as.bin_op.rh = arena_alloc(arena, sizeof(Jolie_Expr));

        *expr.as.bin_op.lh = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }
        *expr.as.bin_op.rh = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return expr;
        }
    } break;
    default: {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected expression\n",
            JOLIE_LOC_ARG(peek.loc), SV_ARG(peek.text));
        return expr;
    }
    }

    return expr;
}

Jolie_Type jolie_parse_type(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Type type = {0};

    String_View type_name = {0};
    Jolie_Loc word_loc = {0};

    while (true) {
        Jolie_Token token = jolie_parse_next_token(arena, ast, lexer);
        if (ast->failed) {
            return type;
        }
        if (token.type == JOLIE_CARET) {
            type.indirection_level += 1;
        } else if (token.type == JOLIE_WORD) {
            type_name = token.text;
            word_loc = token.loc;
            break;
        } else {
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected %s or %s\n",
                JOLIE_LOC_ARG(token.loc), SV_ARG(token.text),
                jolie_token_type_to_cstr(JOLIE_CARET),
                jolie_token_type_to_cstr(JOLIE_WORD));
            return type;
        }
    }

    for (size_t i = 0; i < JOLIE_TYPE_COUNT; ++i) {
        if (sv_eq(type_name, jolie_builtin_types[i].name)) {
            type.id = i;
            return type;
        }
    }

    ast->failed = true;
    str_append_fmt(
        arena, &ast->error_message,
        JOLIE_LOC_FMT": Error: unknown type `"SV_FMT"`\n",
        JOLIE_LOC_ARG(word_loc), SV_ARG(type_name));
    return type;
}

Jolie_Block jolie_parse_block(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Block block = {0};
    jolie_parse_expect(arena, ast, lexer, JOLIE_CURLY_OPEN);
    if (ast->failed) {
        return block;
    }

    Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
    if (ast->failed) {
        return block;
    }
    while (true) {
        if (peek.type == JOLIE_CURLY_CLOSE) {
            (void) jolie_parse_next_token(arena, ast, lexer);
            break;
        }

        Jolie_Stmt stmt = jolie_parse_stmt(arena, ast, lexer);
        if (ast->failed) {
            return block;
        }
        arena_da_append(arena, &block, stmt);

        peek = jolie_parse_peek_token(arena, ast, lexer);
        if (ast->failed) {
            return block;
        }
    }
    return block;
}

Jolie_Exprs jolie_parse_proc_call_args(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Exprs exprs = {0};
    jolie_parse_expect(arena, ast, lexer, JOLIE_PAREN_OPEN);
    if (ast->failed) {
        return exprs;
    }

    Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
    if (ast->failed) {
        return exprs;
    }
    if (peek.type == JOLIE_PAREN_CLOSE) {
        (void) jolie_parse_next_token(arena, ast, lexer);
        return exprs;
    }

    while (true) {
        Jolie_Expr expr = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return exprs;
        }
        arena_da_append(arena, &exprs, expr);

        Jolie_Token token = jolie_parse_next_token(arena, ast, lexer);
        if (ast->failed) {
            return exprs;
        }

        if (token.type == JOLIE_PAREN_CLOSE) {
            break;
        } else if (token.type == JOLIE_COMMA) {
            continue;
        } else {
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected %s or %s\n",
                JOLIE_LOC_ARG(token.loc), SV_ARG(token.text),
                jolie_token_type_to_cstr(JOLIE_COMMA),
                jolie_token_type_to_cstr(JOLIE_PAREN_CLOSE));
            return exprs;
        }
    }
    return exprs;
}

Jolie_Stmt jolie_parse_stmt(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Stmt stmt = {0};
    Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
    if (ast->failed) {
        return stmt;
    }
    // TODO(nic): maybe the loc should be set by each statement separately
    stmt.loc = peek.loc;
    switch (peek.type) {
    case JOLIE_IF: {
        jolie_parse_expect(arena, ast, lexer, JOLIE_IF);
        if (ast->failed) {
            return stmt;
        }

        stmt.type = JOLIE_STMT_IF;
        stmt.as.if_.condition = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }
        stmt.as.if_.block = jolie_parse_block(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }
    } break;
    case JOLIE_LET: {
        jolie_parse_expect(arena, ast, lexer, JOLIE_LET);
        if (ast->failed) {
            return stmt;
        }
        stmt.type = JOLIE_STMT_LET;

        Jolie_Token word = jolie_parse_expect(arena, ast, lexer, JOLIE_WORD);
        if (ast->failed) {
            return stmt;
        }
        stmt.as.let.name = word.text;

        jolie_parse_expect(arena, ast, lexer, JOLIE_COLON);
        if (ast->failed) {
            return stmt;
        }

        stmt.as.let.type = jolie_parse_type(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }

        jolie_parse_expect(arena, ast, lexer, JOLIE_EQUALS);
        if (ast->failed) {
            return stmt;
        }

        stmt.as.let.expr = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }

        jolie_parse_expect(arena, ast, lexer, JOLIE_SEMICOLON);
        if (ast->failed) {
            return stmt;
        }
    } break;
    case JOLIE_WHILE: {
        jolie_parse_expect(arena, ast, lexer, JOLIE_WHILE);
        if (ast->failed) {
            return stmt;
        }

        stmt.type = JOLIE_STMT_WHILE;
        stmt.as.while_.condition = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }
        stmt.as.while_.block = jolie_parse_block(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }
    } break;
    case JOLIE_WORD: {
        Jolie_Token word = jolie_parse_next_token(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }
        Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }

        switch (peek.type) {
        case JOLIE_PAREN_OPEN: {
            stmt.type = JOLIE_STMT_PROC_CALL;
            stmt.as.proc_call.proc_name = word.text;
            stmt.as.proc_call.args = jolie_parse_proc_call_args(arena, ast, lexer);
            if (ast->failed) {
                return stmt;
            }

            jolie_parse_expect(arena, ast, lexer, JOLIE_SEMICOLON);
            if (ast->failed) {
                return stmt;
            }
        } break;
        case JOLIE_EQUALS: {
            jolie_parse_expect(arena, ast, lexer, JOLIE_EQUALS);
            if (ast->failed) {
                return stmt;
            }

            stmt.type = JOLIE_STMT_ASSIGN;
            stmt.as.assign.name = word.text;
            stmt.as.assign.expr = jolie_parse_expr(arena, ast, lexer);
            if (ast->failed) {
                return stmt;
            }

            jolie_parse_expect(arena, ast, lexer, JOLIE_SEMICOLON);
            if (ast->failed) {
                return stmt;
            }
        } break;
        default: {
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected procedure call or variable assignment\n",
                JOLIE_LOC_ARG(peek.loc), SV_ARG(peek.text));
            return stmt;
        }
        }
    } break;
    case JOLIE_RETURN: {
        jolie_parse_expect(arena, ast, lexer, JOLIE_RETURN);
        if (ast->failed) {
            return stmt;
        }

        stmt.type = JOLIE_STMT_RETURN;
        stmt.as.return_.expr = jolie_parse_expr(arena, ast, lexer);
        if (ast->failed) {
            return stmt;
        }

        jolie_parse_expect(arena, ast, lexer, JOLIE_SEMICOLON);
        if (ast->failed) {
            return stmt;
        }
    } break;
    default: {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected statement\n",
            JOLIE_LOC_ARG(peek.loc), SV_ARG(peek.text));
        return stmt;
    }
    }
    return stmt;
}

Jolie_Proc jolie_parse_proc(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer) {
    Jolie_Proc proc = {0};

    jolie_parse_expect(arena, ast, lexer, JOLIE_PROC);
    if (ast->failed) {
        return proc;
    }

    Jolie_Token proc_name_token = jolie_parse_expect(arena, ast, lexer, JOLIE_WORD);
    if (ast->failed) {
        return proc;
    }
    proc.name = proc_name_token.text;

    jolie_parse_expect(arena, ast, lexer, JOLIE_PAREN_OPEN);
    if (ast->failed) {
        return proc;
    }

    Jolie_Token peek = jolie_parse_peek_token(arena, ast, lexer);
    if (ast->failed) {
        return proc;
    }
    if (peek.type == JOLIE_PAREN_CLOSE) {
        (void) jolie_parse_next_token(arena, ast, lexer);
    } else {
        while (true) {
            Jolie_Param param = {0};
            Jolie_Token word = jolie_parse_expect(arena, ast, lexer, JOLIE_WORD);
            if (ast->failed) {
                return proc;
            }
            param.name = word.text;

            jolie_parse_expect(arena, ast, lexer, JOLIE_COLON);
            if (ast->failed) {
                return proc;
            }

            param.type = jolie_parse_type(arena, ast, lexer);
            if (ast->failed) {
                return proc;
            }
            arena_da_append(arena, &proc.params, param);

            Jolie_Token token = jolie_parse_next_token(arena, ast, lexer);
            if (ast->failed) {
                return proc;
            }

            if (token.type == JOLIE_PAREN_CLOSE) {
                break;
            } else if (token.type == JOLIE_COMMA) {
                continue;
            } else {
                ast->failed = true;
                str_append_fmt(
                    arena, &ast->error_message,
                    JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected %s or %s\n",
                    JOLIE_LOC_ARG(token.loc), SV_ARG(token.text),
                    jolie_token_type_to_cstr(JOLIE_COMMA),
                    jolie_token_type_to_cstr(JOLIE_PAREN_CLOSE));
                return proc;
            }
        }
    }

    peek = jolie_parse_peek_token(arena, ast, lexer);
    if (ast->failed) {
        return proc;
    }
    if (peek.type == JOLIE_COLON) {
        (void) jolie_parse_next_token(arena, ast, lexer);
        proc.return_type = jolie_parse_type(arena, ast, lexer);
        if (ast->failed) {
            return proc;
        }
    }

    proc.block = jolie_parse_block(arena, ast, lexer);
    if (ast->failed) {
        return proc;
    }

    return proc;
}

Jolie_Ast jolie_parse(Arena *arena, Jolie_Lexer *lexer) {
    Jolie_Ast ast = {0};

    while (true) {
        Jolie_Token token = jolie_parse_peek_token(arena, &ast, lexer);
        if (ast.failed) {
            return ast;
        }
        if (token.type == JOLIE_END) {
            break;
        }

        switch (token.type) {
        case JOLIE_PROC: {
            Jolie_Proc proc = jolie_parse_proc(arena, &ast, lexer);
            if (ast.failed) {
                return ast;
            }
            arena_da_append(arena, &ast.procs, proc);
        } break;
        default: {
            ast.failed = true;
            str_append_fmt(
                arena, &ast.error_message,
                JOLIE_LOC_FMT": Error: unexpected token `"SV_FMT"`, expected procedure declaration\n",
                JOLIE_LOC_ARG(token.loc), SV_ARG(token.text));
        }
        }
    }

    return ast;
}
