#include "./jolie_lexer.h"
#include "./jolie_parser.h"
#include "./utils.h"

#define ARENA_IMPLEMENTATION
#include "./arena.h"

#include <errno.h>

#define JOLIE_ENTRY_POINT_PROC "main"

typedef struct {
    String_View name;
    Jolie_Type type;
} Jolie_Var;

typedef struct Jolie_Scope Jolie_Scope;

struct Jolie_Scope {
    Jolie_Var *items;
    size_t count;
    size_t capacity;
    Jolie_Scope *next;
};

Jolie_Scope jolie_make_scope(Jolie_Scope *parent) {
    Jolie_Scope scope = { 0 };
    scope.next = parent;
    return scope;
}

Jolie_Var *jolie_search_scope(Jolie_Scope *scope, String_View name, bool local_only) {
    while (scope != NULL) {
        for (size_t i = 0; i < scope->count; ++i) {
            Jolie_Var *var = &scope->items[i];
            if (sv_eq(var->name, name)) {
                return var;
            }
        }
        if (local_only) {
            break;
        }
        scope = scope->next;
    }
    return NULL;
}

Jolie_Proc *jolie_find_proc(Jolie_Ast *ast, String_View proc_name) {
    for (size_t i = 0; i < ast->procs.count; ++i) {
        if (sv_eq(ast->procs.items[i].name, proc_name)) {
            return &ast->procs.items[i];
        }
    }
    return NULL;
}

Jolie_Type jolie_type(Jolie_Type_Id id, size_t indirection_level) {
    Jolie_Type type = {0};
    type.id = id;
    type.indirection_level = indirection_level;
    return type;
}

bool jolie_type_eq(Jolie_Type a, Jolie_Type b) {
    return a.indirection_level == b.indirection_level && a.id == b.id;
}

void str_append_type_error(
    Arena *arena, String *str, Jolie_Loc loc,
    const char *a, Jolie_Type type1,
    const char *b, Jolie_Type type2, const char *c)
{
    str_append_fmt(arena, str, JOLIE_LOC_FMT"%s", JOLIE_LOC_ARG(loc), a);
    str_append_type(arena, str, type1);
    str_append_fmt(arena, str, "%s", b);
    str_append_type(arena, str, type2);
    str_append_fmt(arena, str, "%s", c);
}

Jolie_Var *jolie_search_scope(Jolie_Scope *scope, String_View name, bool local_only);
Jolie_Type jolie_check_proc_call(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Proc_Call *proc_call, Jolie_Loc loc);
Jolie_Type jolie_check_expr(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Expr *expr);
void jolie_check_block(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Block *block, Jolie_Type return_type);
void jolie_check_proc(Arena *arena, Jolie_Ast *ast, Jolie_Proc *proc);

Jolie_Type jolie_check_proc_call(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Proc_Call *proc_call, Jolie_Loc loc) {
    Jolie_Proc *proc = jolie_find_proc(ast, proc_call->proc_name);
    if (proc == NULL) {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: procdeure not defined `"SV_FMT"`\n",
            JOLIE_LOC_ARG(loc), SV_ARG(proc_call->proc_name));
        return (Jolie_Type) {0};
    }

    if (proc_call->args.count != proc->params.count) {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            JOLIE_LOC_FMT": Error: procdeure expects %zu argument(s), but got %zu\n",
            JOLIE_LOC_ARG(loc), proc->params.count, proc_call->args.count);
        return (Jolie_Type) {0};
    }

    for (size_t i = 0; i < proc_call->args.count; ++i) {
        Jolie_Type type = jolie_check_expr(arena, scope, ast, &proc_call->args.items[i]);
        if (ast->failed) {
            return (Jolie_Type) {0};
        }
        Jolie_Type expected_type = proc->params.items[i].type;
        if (!jolie_type_eq(type, expected_type)) {
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: argument %d of `"SV_FMT"` takes type `",
                JOLIE_LOC_ARG(proc_call->args.items[i].loc),
                i + 1, SV_ARG(proc_call->proc_name));
            str_append_type(arena, &ast->error_message, expected_type);
            str_append_fmt(arena, &ast->error_message, "`, but got type `");
            str_append_type(arena, &ast->error_message, type);
            str_append_fmt(arena, &ast->error_message, "`\n");
            return (Jolie_Type) {0};
        }
    }
    return proc->return_type;
}

Jolie_Type jolie_check_expr(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Expr *expr) {
    switch (expr->type) {
    case JOLIE_EXPR_WORD: {
        String_View name = expr->as.word;
        Jolie_Var *var = jolie_search_scope(scope, name, false);
        if (var == NULL) {
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: variable not defined `"SV_FMT"`\n",
                JOLIE_LOC_ARG(expr->loc), SV_ARG(name));
            return (Jolie_Type) {0};
        }
        return var->type;
    } break;
    case JOLIE_EXPR_UINT64: {
        return jolie_type(JOLIE_TYPE_UINT64, 0);
    } break;
    case JOLIE_EXPR_STRING_INDEX: {
        return jolie_type(JOLIE_TYPE_UINT8, 1);
    } break;
    case JOLIE_EXPR_PROC_CALL: {
        Jolie_Proc_Call *proc_call = &expr->as.proc_call;
        Jolie_Type type = jolie_check_proc_call(arena, scope, ast, proc_call, expr->loc);
        if (ast->failed) {
            return (Jolie_Type) {0};
        }
        return type;
    } break;
    case JOLIE_EXPR_DEREF: {
        Jolie_Expr_Deref *deref = &expr->as.deref;
        Jolie_Type type = jolie_check_expr(arena, scope, ast, deref->expr);
        if (type.indirection_level <= 0) {
            ast->failed = true;
            str_append_fmt(
                arena, &ast->error_message,
                JOLIE_LOC_FMT": Error: cannot derefence non-pointer type\n",
                JOLIE_LOC_ARG(expr->loc));
            return (Jolie_Type) {0};
        }
        return jolie_type(type.id, type.indirection_level - 1);
    } break;
    case JOLIE_EXPR_CAST: {
        Jolie_Expr_Cast *cast = &expr->as.cast;
        jolie_check_expr(arena, scope, ast, cast->expr);
        if (ast->failed) {
            return (Jolie_Type) {0};
        }
        return cast->type;
    } break;
    case JOLIE_EXPR_BINARY_OP: {
        Jolie_Binary_Op *op = &expr->as.bin_op;

        Jolie_Type lh_type = jolie_check_expr(arena, scope, ast, op->lh);
        if (ast->failed) {
            return (Jolie_Type) {0};
        }

        Jolie_Type rh_type = jolie_check_expr(arena, scope, ast, op->rh);
        if (ast->failed) {
            return (Jolie_Type) {0};
        }

        if (!jolie_type_eq(lh_type, rh_type)) {
            ast->failed = true;
            str_append_type_error(
                arena, &ast->error_message, expr->loc,
                ": Error: operands expect both expression to have the same type, but left expression is of type `",
                lh_type, "` and right expression is of type `", rh_type, "`\n");
            return (Jolie_Type) {0};
        }

        switch (op->type) {
        case JOLIE_BINARY_OP_ADD:
        case JOLIE_BINARY_OP_SUB:
        case JOLIE_BINARY_OP_MUL:
        case JOLIE_BINARY_OP_DIV:
            return lh_type;
        case JOLIE_BINARY_OP_LT:
            return jolie_type(JOLIE_TYPE_BOOL, 0);
        default:
            assert(0 && "unreachable");
        }
    } break;
    default: assert(0 && "unreachable");
    }
}

void jolie_check_block(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Block *block, Jolie_Type return_type) {
    for (size_t i = 0; i < block->count; ++i) {
        Jolie_Stmt *stmt = &block->items[i];
        switch (stmt->type) {
        case JOLIE_STMT_IF: {
            Jolie_Stmt_If *if_ = &stmt->as.if_;
            Jolie_Type cond_type = jolie_check_expr(arena, scope, ast, &if_->condition);
            if (ast->failed) {
                return;
            }

            Jolie_Type expected = jolie_type(JOLIE_TYPE_BOOL, 0);
            if (!jolie_type_eq(cond_type, expected)) {
                ast->failed = true;
                str_append_type_error(
                    arena, &ast->error_message, if_->condition.loc,
                    ": Error: if condition expects an expression of type `", expected,
                    "`, but expression is of type `", cond_type, "`\n");
                return;
            }

            Jolie_Scope sub_scope = jolie_make_scope(scope);
            jolie_check_block(arena, &sub_scope, ast, &if_->block, return_type);
            if (ast->failed) {
                return;
            }
        } break;
        case JOLIE_STMT_LET: {
            Jolie_Stmt_Let *let = &stmt->as.let;

            Jolie_Type type = jolie_check_expr(arena, scope, ast, &let->expr);
            if (ast->failed) {
                return;
            }

            {
                Jolie_Var *var = jolie_search_scope(scope, let->name, true);
                if (var != NULL) {
                    ast->failed = true;
                    str_append_fmt(
                        arena, &ast->error_message,
                        JOLIE_LOC_FMT": Error: variable already defined `"SV_FMT"`\n",
                        JOLIE_LOC_ARG(stmt->loc), SV_ARG(let->name));
                    return;
                }
            }

            if (!jolie_type_eq(type, let->type)) {
                ast->failed = true;
                str_append_type_error(
                    arena, &ast->error_message, stmt->loc,
                    ": Error: variable declared with type `", let->type,
                    "`, but expression is of type `", type, "`\n");
                return;
            }

            Jolie_Var var = { let->name, let->type };
            arena_da_append(arena, scope, var);
        } break;
        case JOLIE_STMT_WHILE: {
            Jolie_Stmt_While *while_ = &stmt->as.while_;
            Jolie_Type cond_type = jolie_check_expr(arena, scope, ast, &while_->condition);
            if (ast->failed) {
                return;
            }

            Jolie_Type expected = jolie_type(JOLIE_TYPE_BOOL, 0);
            if (!jolie_type_eq(cond_type, expected)) {
                ast->failed = true;
                str_append_type_error(
                    arena, &ast->error_message, while_->condition.loc,
                    ": Error: while condition expects an expression of type `", expected,
                    "`, but expression is of type `", cond_type, "`\n");
                return;
            }

            Jolie_Scope sub_scope = jolie_make_scope(scope);
            jolie_check_block(arena, &sub_scope, ast, &while_->block, return_type);
            if (ast->failed) {
                return;
            }
        } break;
        case JOLIE_STMT_PROC_CALL: {
            Jolie_Proc_Call *proc_call = &stmt->as.proc_call;
            jolie_check_proc_call(arena, scope, ast, proc_call, stmt->loc);
            if (ast->failed) {
                return;
            }
        } break;
        case JOLIE_STMT_ASSIGN: {
            Jolie_Stmt_Assign *assign = &stmt->as.assign;

            Jolie_Var *var = jolie_search_scope(scope, assign->name, false);
            if (var == NULL) {
                ast->failed = true;
                str_append_fmt(
                    arena, &ast->error_message,
                    JOLIE_LOC_FMT": Error: variable not defined `"SV_FMT"`\n",
                    JOLIE_LOC_ARG(stmt->loc), SV_ARG(assign->name));
                return;
            }

            Jolie_Type type = jolie_check_expr(arena, scope, ast, &assign->expr);
            if (!jolie_type_eq(type, var->type)) {
                ast->failed = true;
                str_append_type_error(
                    arena, &ast->error_message, stmt->loc,
                    ": Error: variable declared with type `", var->type,
                    "`, but expression is of type `", type, "`\n");
                return;
            }
        } break;
        case JOLIE_STMT_RETURN: {
            Jolie_Stmt_Return *return_ = &stmt->as.return_;
            Jolie_Type type = jolie_check_expr(arena, scope, ast, &return_->expr);
            if (ast->failed) {
                return;
            }
            if (!jolie_type_eq(type, return_type)) {
                ast->failed = true;
                str_append_type_error(
                    arena, &ast->error_message, return_->expr.loc,
                    ": Error: procedure expected return type `", return_type,
                    "`, but expression is of type `", type, "`\n");
                return;
            }
        } break;
        default: assert(0 && "unreachable");
        }
    }
}

void jolie_check_proc(Arena *arena, Jolie_Ast *ast, Jolie_Proc *proc) {
    Jolie_Scope scope = {0};
    for (size_t i = 0; i < proc->params.count; ++i) {
        Jolie_Param *param = &proc->params.items[i];
        Jolie_Var var = {
            .name = param->name,
            .type = param->type,
        };
        arena_da_append(arena, &scope, var);
    }

    Jolie_Block *block = &proc->block;
    jolie_check_block(arena, &scope, ast, block, proc->return_type);
}

void jolie_check_ast(Arena *arena, Jolie_Ast *ast) {
    bool contains_main = false;

    for (size_t i = 0; i < ast->procs.count; ++i) {
        Jolie_Proc *proc = &ast->procs.items[i];
        if (sv_eq(proc->name, SV(JOLIE_ENTRY_POINT_PROC))) {
            // TODO(nic): check parameters and return type of the main proc
            contains_main = true;
        }

        jolie_check_proc(arena, ast, proc);
        if (ast->failed) {
            return;
        }
    }

    if (!contains_main) {
        ast->failed = true;
        str_append_fmt(
            arena, &ast->error_message,
            "Error: no `%s` procedure found\n", JOLIE_ENTRY_POINT_PROC);
    }
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
