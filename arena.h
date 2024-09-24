#ifndef ARENA_H
#define ARENA_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#ifndef max
    #define max(a, b) (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
    #define min(a, b) (((a) < (b)) ? (a) : (b))
#endif

#define ARENA_CHUNK_SIZE 1024

typedef uint8_t u8;
typedef struct Arena_Chunk Arena_Chunk;

struct Arena_Chunk {
    void *data;
    size_t size;
    size_t cursor;
    Arena_Chunk *next;
};

typedef struct {
    Arena_Chunk *chunk;
} Arena;

void *arena_alloc(Arena *arena, size_t size);
void *arena_realloc(Arena *arena, void *ptr, size_t old_size, size_t size);
void arena_reset(Arena *arena);
void arena_free(Arena *arena);

#endif // ARENA_H

#ifdef ARENA_IMPLEMENTATION
#undef ARENA_IMPLEMENTATION

void *arena_chunk_alloc(Arena_Chunk *chunk, size_t size) {
    assert(chunk->cursor + size <= chunk->size);
    void *ptr = (u8*)chunk->data + chunk->cursor;
    chunk->cursor += size;
    return ptr;
}

Arena_Chunk *arena_add_chunk(Arena *arena, size_t chunk_size) {
    Arena_Chunk **chunk = &arena->chunk;
    while (*chunk != NULL) {
        chunk = &(*chunk)->next;
    }
    *chunk = malloc(sizeof(Arena_Chunk));
    assert(*chunk != NULL);
    **chunk = (Arena_Chunk) {
        .data = malloc(chunk_size),
        .size = chunk_size,
    };
    return *chunk;
}

void *arena_alloc(Arena *arena, size_t size) {
    Arena_Chunk *chunk = arena->chunk;
    while (chunk != NULL) {
        if (chunk->cursor + size <= chunk->size) {
            return arena_chunk_alloc(chunk, size);
        }
        chunk = chunk->next;
    }

    size_t chunk_size = max(ARENA_CHUNK_SIZE, size);
    chunk = arena_add_chunk(arena, chunk_size);
    return arena_chunk_alloc(chunk, size);
}

void *arena_realloc(Arena *arena, void *ptr, size_t old_size, size_t size) {
    u8 *newptr = arena_alloc(arena, size);
    memcpy(newptr, ptr, old_size);
    return newptr;
}

void arena_reset(Arena *arena) {
    Arena_Chunk *chunk = arena->chunk;
    while (chunk != NULL) {
        chunk->cursor = 0;
        chunk = chunk->next;
    }
}

void arena_free(Arena *arena) {
    Arena_Chunk *chunk = arena->chunk;
    while (chunk != NULL) {
        Arena_Chunk *save_chunk = chunk;
        free(chunk->data);
        chunk = chunk->next;
        free(save_chunk);
    }
}

#endif // ARENA_IMPLEMENTATION
