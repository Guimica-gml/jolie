#ifndef JOLIE_PARSER_H
#define JOLIE_PARSER_H

#include "./jolie_lexer.h"

typedef enum {
    JOLIE_TYPE_UINT64,
    JOLIE_TYPE_UINT32,
    JOLIE_TYPE_UIN16,
    JOLIE_TYPE_UINT8,
    JOLIE_TYPE_BOOL,
    JOLIE_TYPE_COUNT,
} Jolie_Type_Id;

typedef struct {
    String_View name;
    size_t size;
} Jolie_Buintin_Type_Info;

extern Jolie_Buintin_Type_Info jolie_builtin_types[JOLIE_TYPE_COUNT];

typedef struct Jolie_Expr Jolie_Expr;

typedef struct {
    Jolie_Expr *items;
    size_t count;
    size_t capacity;
} Jolie_Exprs;

typedef struct {
    String_View proc_name;
    Jolie_Exprs args;
} Jolie_Proc_Call;

typedef enum {
    JOLIE_BINARY_OP_ADD,
    JOLIE_BINARY_OP_SUB,
    JOLIE_BINARY_OP_MUL,
    JOLIE_BINARY_OP_DIV,
    JOLIE_BINARY_OP_LT,
} Jolie_Binary_Op_Type;

typedef enum {
    JOLIE_EXPR_WORD,
    JOLIE_EXPR_UINT64,
    JOLIE_EXPR_STRING_INDEX,
    JOLIE_EXPR_PROC_CALL,
    JOLIE_EXPR_DEREF,
    JOLIE_EXPR_BINARY_OP,
} Jolie_Expr_Type;

typedef struct {
    Jolie_Binary_Op_Type type;
    Jolie_Expr *lh;
    Jolie_Expr *rh;
} Jolie_Binary_Op;

typedef struct {
    size_t count;
    String_View word;
} Jolie_Expr_Deref;

typedef union {
    String_View word;
    uint64_t uint64;
    size_t string_index;
    Jolie_Proc_Call proc_call;
    Jolie_Expr_Deref deref;
    Jolie_Binary_Op bin_op;
} Jolie_Expr_As;

struct Jolie_Expr {
    Jolie_Loc loc;
    Jolie_Expr_Type type;
    Jolie_Expr_As as;
};

typedef struct {
    Jolie_Type_Id id;
    size_t indirection_level;
} Jolie_Type;

typedef struct Jolie_Stmt Jolie_Stmt;
typedef struct {
    Jolie_Stmt *items;
    size_t count;
    size_t capacity;
} Jolie_Block;

typedef enum {
    JOLIE_STMT_IF,
    JOLIE_STMT_LET,
    JOLIE_STMT_WHILE,
    JOLIE_STMT_PROC_CALL,
    JOLIE_STMT_ASSIGN,
    JOLIE_STMT_RETURN,
} Jolie_Stmt_Type;

typedef struct {
    Jolie_Expr condition;
    Jolie_Block block;
} Jolie_Stmt_If;

typedef struct {
    String_View name;
    Jolie_Type type;
    Jolie_Expr expr;
} Jolie_Stmt_Let;

typedef struct {
    Jolie_Expr condition;
    Jolie_Block block;
} Jolie_Stmt_While;

typedef struct {
    String_View name;
    Jolie_Expr expr;
} Jolie_Stmt_Assign;

typedef struct {
    Jolie_Expr expr;
} Jolie_Stmt_Return;

typedef union {
    Jolie_Stmt_If if_;
    Jolie_Stmt_Let let;
    Jolie_Stmt_While while_;
    Jolie_Proc_Call proc_call;
    Jolie_Stmt_Assign assign;
    Jolie_Stmt_Return return_;
} Jolie_Stmt_As;

struct Jolie_Stmt {
    Jolie_Loc loc;
    Jolie_Stmt_Type type;
    Jolie_Stmt_As as;
};

typedef struct {
    String_View name;
    Jolie_Type type;
} Jolie_Param;

typedef struct {
    Jolie_Param *items;
    size_t count;
    size_t capacity;
} Jolie_Params;

typedef struct {
    String_View name;
    Jolie_Params params;
    Jolie_Block block;
    Jolie_Type return_type;
} Jolie_Proc;

typedef struct {
    Jolie_Proc *items;
    size_t count;
    size_t capacity;
} Jolie_Procs;

typedef struct {
    Jolie_Procs procs;
    String strings;

    bool failed;
    String error_message;
} Jolie_Ast;

Jolie_Token jolie_parse_next_token(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Token jolie_parse_peek_token(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);

Jolie_Token jolie_parse_expect(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer, Jolie_Token_Type type);
void str_append_type(Arena *arena, String *str, Jolie_Type type);
size_t jolie_prepare_string(Arena *arena, Jolie_Ast *ast, String_View sv, Jolie_Loc loc);

Jolie_Expr jolie_parse_expr(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Type jolie_parse_type(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Stmt jolie_parse_stmt(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Exprs jolie_parse_proc_call_args(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Proc jolie_parse_proc(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Block jolie_parse_block(Arena *arena, Jolie_Ast *ast, Jolie_Lexer *lexer);
Jolie_Ast jolie_parse(Arena *arena, Jolie_Lexer *lexer);

#endif // JOLIE_PARSER_H
