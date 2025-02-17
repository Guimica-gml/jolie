#ifndef JOLIE_CHECKER_H
#define JOLIE_CHECKER_H

#include "./jolie_parser.h"

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

Jolie_Scope jolie_make_scope(Jolie_Scope *parent);
Jolie_Var *jolie_search_scope(Jolie_Scope *scope, String_View name, bool local_only);
Jolie_Proc *jolie_find_proc(Jolie_Ast *ast, String_View proc_name);
Jolie_Type jolie_type(Jolie_Type_Id id, size_t indirection_level);
bool jolie_type_eq(Jolie_Type a, Jolie_Type b);

Jolie_Var *jolie_search_scope(Jolie_Scope *scope, String_View name, bool local_only);
Jolie_Type jolie_check_proc_call(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Proc_Call *proc_call, Jolie_Loc loc);
Jolie_Type jolie_check_expr(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Expr *expr);
void jolie_check_block(Arena *arena, Jolie_Scope *scope, Jolie_Ast *ast, Jolie_Block *block, Jolie_Type return_type);
void jolie_check_proc(Arena *arena, Jolie_Ast *ast, Jolie_Proc *proc);
void jolie_check_ast(Arena *arena, Jolie_Ast *ast);

#endif // JOLIE_CHECKER_H
