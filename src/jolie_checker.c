#include "./jolie_checker.h"

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

Jolie_Type jolie_check_proc_call(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Proc_Call *proc_call, Jolie_Loc loc) {
    Jolie_Proc *proc = jolie_find_proc(ast, proc_call->proc_name);
    if (proc == NULL) {
        ast->failed = true;
        jolie_str_append_fmt(
            arena, &ast->error_message,
            "%l: Error: procedure not defined `%w`\n",
            loc, proc_call->proc_name);
    } else {
        if (proc_call->args.count != proc->params.count) {
            ast->failed = true;
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: procedure expects %z argument(s), but got %z\n",
                loc, proc->params.count, proc_call->args.count);
        }

        for (size_t i = 0; i < proc_call->args.count; ++i) {
            Jolie_Expr *arg = &proc_call->args.items[i];
            Jolie_Type type = jolie_check_expr(arena, scope, ast, arg);
            if (i < proc->params.count) {
                Jolie_Type expected_type = proc->params.items[i].type;
                if (!jolie_type_eq(type, expected_type)) {
                    ast->failed = true;
                    jolie_str_append_fmt(
                        arena, &ast->error_message,
                        "%l: Error: argument %z of `%w` takes type `%t`, but got type `%t`\n",
                        arg->loc, i + 1, proc_call->proc_name, expected_type, type);
                }
            }
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
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: variable not defined `%w`\n",
                expr->loc, name);
            return (Jolie_Type) {0};
        } else {
            return var->type;
        }
    } break;
    case JOLIE_EXPR_UINT64: {
        return jolie_type(JOLIE_TYPE_UINT64, 0);
    } break;
    case JOLIE_EXPR_STRING_INDEX: {
        return jolie_type(JOLIE_TYPE_UINT8, 1);
    } break;
    case JOLIE_EXPR_PROC_CALL: {
        Jolie_Proc_Call *proc_call = &expr->as.proc_call;
        return jolie_check_proc_call(arena, scope, ast, proc_call, expr->loc);
    } break;
    case JOLIE_EXPR_DEREF: {
        Jolie_Expr_Deref *deref = &expr->as.deref;
        Jolie_Type type = jolie_check_expr(arena, scope, ast, deref->expr);
        if (jolie_type_eq(type, JOLIE_VOID_STAR)) {
            ast->failed = true;
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: cannot derefence `%t`, please cast it to another pointer type\n",
                expr->loc, JOLIE_VOID_STAR);
        }
        if (type.indirection_level <= 0) {
            ast->failed = true;
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: cannot derefence non-pointer type\n",
                expr->loc);
        }
        return jolie_type(type.id, type.indirection_level - 1);
    } break;
    case JOLIE_EXPR_CAST: {
        Jolie_Expr_Cast *cast = &expr->as.cast;
        if (jolie_type_eq(cast->type, JOLIE_VOID)) {
            ast->failed = true;
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: casts to `%t` are not allowed\n",
                expr->loc, JOLIE_VOID);
        }
        jolie_check_expr(arena, scope, ast, cast->expr);
        return cast->type;
    } break;
    case JOLIE_EXPR_BINARY_OP: {
        Jolie_Binary_Op *op = &expr->as.bin_op;

        Jolie_Type lh_type = jolie_check_expr(arena, scope, ast, op->lh);
        Jolie_Type rh_type = jolie_check_expr(arena, scope, ast, op->rh);

        if (!jolie_type_eq(lh_type, rh_type)) {
            ast->failed = true;
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: operands expect both expression to have the same type, but left expression is of type `%t` and right expression is of type `%t`\n",
                expr->loc, lh_type, rh_type);
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

            Jolie_Type expected = jolie_type(JOLIE_TYPE_BOOL, 0);
            if (!jolie_type_eq(cond_type, expected)) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    ": Error: if condition expects an expression of type `%t`, but expression is of type `%t`\n",
                    if_->condition.loc, expected, cond_type);
            }

            Jolie_Scope sub_scope = jolie_make_scope(scope);
            jolie_check_block(arena, &sub_scope, ast, &if_->block, return_type);
        } break;
        case JOLIE_STMT_LET: {
            Jolie_Stmt_Let *let = &stmt->as.let;

            if (jolie_type_eq(let->type, JOLIE_VOID)) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: variables with type `%t` are not allowed\n",
                    stmt->loc, JOLIE_VOID);
            }
            Jolie_Type type = jolie_check_expr(arena, scope, ast, &let->expr);

            {
                Jolie_Var *var = jolie_search_scope(scope, let->name, true);
                if (var != NULL) {
                    ast->failed = true;
                    jolie_str_append_fmt(
                        arena, &ast->error_message,
                        "%l: Error: variable already defined `%w`\n",
                        stmt->loc, let->name);
                    jolie_str_append_fmt(
                        arena, &ast->error_message,
                        "%l: Note: original variable defined here\n",
                        var->loc);
                }
            }

            if (!jolie_type_eq(type, let->type)) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: variable declared with type `%t`, but expression is of type `%t`\n",
                    stmt->loc, let->type, type);
            }

            Jolie_Var var = { let->name, let->type, stmt->loc };
            arena_da_append(arena, scope, var);
        } break;
        case JOLIE_STMT_WHILE: {
            Jolie_Stmt_While *while_ = &stmt->as.while_;
            Jolie_Type cond_type = jolie_check_expr(arena, scope, ast, &while_->condition);

            Jolie_Type expected = jolie_type(JOLIE_TYPE_BOOL, 0);
            if (!jolie_type_eq(cond_type, expected)) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: while condition expects an expression of type `%t`, but expression is of type `%t`\n",
                    while_->condition.loc, expected, cond_type);
            }

            Jolie_Scope sub_scope = jolie_make_scope(scope);
            jolie_check_block(arena, &sub_scope, ast, &while_->block, return_type);
        } break;
        case JOLIE_STMT_PROC_CALL: {
            Jolie_Proc_Call *proc_call = &stmt->as.proc_call;
            jolie_check_proc_call(arena, scope, ast, proc_call, stmt->loc);
        } break;
        case JOLIE_STMT_ASSIGN: {
            Jolie_Stmt_Assign *assign = &stmt->as.assign;

            Jolie_Var *var = jolie_search_scope(scope, assign->name, false);
            if (var == NULL) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: variable not defined `%w`\n",
                    stmt->loc, assign->name);
            } else {
                Jolie_Type type = jolie_check_expr(arena, scope, ast, &assign->expr);
                if (!jolie_type_eq(type, var->type)) {
                    ast->failed = true;
                    jolie_str_append_fmt(
                        arena, &ast->error_message,
                        "%l: Error: variable declared with type `%t`, but expression is of type `%t`\n",
                        stmt->loc, var->type, type);
                }
            }
        } break;
        case JOLIE_STMT_RETURN: {
            Jolie_Stmt_Return *return_ = &stmt->as.return_;
            Jolie_Type type;
            if (return_->is_void) {
                type = JOLIE_VOID;
            } else {
                type = jolie_check_expr(arena, scope, ast, &return_->expr);
            }
            if (!jolie_type_eq(type, return_type)) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: procedure expected return type `%t`, but expression is of type `%t`\n",
                    return_->expr.loc, return_type, type);
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
        if (jolie_type_eq(param->type, JOLIE_VOID)) {
            ast->failed = true;
            jolie_str_append_fmt(
                arena, &ast->error_message,
                "%l: Error: procedure parameters cannot be of type `%t`\n",
                param->loc, JOLIE_VOID);
        }
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
            Jolie_Type main_return_type = jolie_type(JOLIE_TYPE_UINT32, 0);
            if (!jolie_type_eq(proc->return_type, main_return_type)) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: `%s` procedure should return `%t`\n",
                    proc->loc, JOLIE_ENTRY_POINT_PROC, main_return_type);
            }
            if (proc->params.count > 0) {
                ast->failed = true;
                jolie_str_append_fmt(
                    arena, &ast->error_message,
                    "%l: Error: `%s` procedure should receive no arguments\n",
                    proc->loc, JOLIE_ENTRY_POINT_PROC);
            }
            contains_main = true;
        }
        jolie_check_proc(arena, ast, proc);
    }

    if (!contains_main) {
        ast->failed = true;
        jolie_str_append_fmt(
            arena, &ast->error_message,
            "Error: no `%s` procedure found\n",
            JOLIE_ENTRY_POINT_PROC);
    }
}
