#include "./utils.h"

#include <stdio.h>
#include <string.h>

String str_with_cap(Arena *arena, size_t cap) {
    String str = {0};
    str.items = arena_alloc(arena, cap * sizeof(char));
    str.capacity = cap;
    return str;
}

bool str_eq(String *a, String *b) {
    if (a->count != b->count) {
        return false;
    }
    return memcmp(a->items, b->items, a->count) == 0;
}

bool str_eq_cstr(String *a, const char *b) {
    size_t b_count = strlen(b);
    if (a->count != b_count) {
        return false;
    }
    return memcmp(a->items, b, a->count) == 0;
}

void str_append_vfmt(Arena *arena, String *str, const char *fmt, va_list args) {
    va_list copy;
    va_copy(copy, args);
    int str_size = vsnprintf(NULL, 0, fmt, copy);
    va_end(copy);
    char temp[(str_size + 1) * sizeof(char)];
    vsnprintf(temp, str_size + 1, fmt, args);
    arena_da_append_many(arena, str, temp, str_size);
}

void str_append_fmt(Arena *arena, String *str, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    str_append_vfmt(arena, str, fmt, args);
    va_end(args);
}

int64_t sv_to_int64(String_View sv) {
    char int64_string[sv.size + 1];
    memcpy(int64_string, sv.data, sv.size);
    int64_string[sv.size] = '\0';
    return atoll(int64_string);
}

double sv_to_decimal(String_View sv) {
    char decimal_string[sv.size + 1];
    memcpy(decimal_string, sv.data, sv.size);
    decimal_string[sv.size] = '\0';
    return strtod(decimal_string, NULL);
}

bool sv_find(String_View sv, char ch, size_t *index) {
    for (size_t i = 0; i < sv.size; ++i) {
        if (sv.data[i] == ch) {
            if (index != NULL) {
                *index = i;
            }
            return true;
        }
    }
    return false;
}

bool sv_find_rev(String_View sv, char ch, size_t *index) {
    for (int i = sv.size; i >= 0; --i) {
        if (sv.data[i] == ch) {
            if (index != NULL) {
                *index = i;
            }
            return true;
        }
    }
    return false;
}

bool sv_eq(String_View a, String_View b) {
    if (a.size != b.size) {
        return false;
    }
    return memcmp(a.data, b.data, a.size) == 0;
}
