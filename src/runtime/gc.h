/* Internal fixed-layout GC foundation. See README.md in this directory. */
#ifndef NOV_GC_H
#define NOV_GC_H
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef NOV_GC_MALLOC
#define NOV_GC_MALLOC malloc
#define NOV_GC_FREE free
#endif
#ifndef NOV_GC_PANIC
static _Noreturn void nov_gc_panic(const char *message) {
    (void)fflush(stdout);
    (void)fprintf(stderr, "panic: %s\n", message);
    exit(1);
}
#define NOV_GC_PANIC nov_gc_panic
#endif

typedef struct {
    size_t size, alignment, pointer_count;
    const size_t *pointer_offsets;
} nov_gc_descriptor;

typedef union nov_gc_object nov_gc_object;
union nov_gc_object {
    max_align_t alignment;
    struct {
        nov_gc_object *next, *gray;
        const nov_gc_descriptor *descriptor;
        unsigned marked;
    } h;
};

typedef struct nov_gc_roots nov_gc_roots;
struct nov_gc_roots {
    nov_gc_roots *next;
    void ***slots;
    size_t count;
};

typedef struct {
    nov_gc_object *objects;
    nov_gc_roots *frames, *globals;
    size_t live_objects, live_bytes, next_collection;
    int collect_every_allocation;
} nov_gc_heap;

static inline void nov_gc_init(nov_gc_heap *heap) {
    *heap = (nov_gc_heap){0};
    heap->next_collection = 65536;
}

static inline void nov_gc_roots_add(nov_gc_heap *heap, nov_gc_roots *roots,
                                    void ***slots, size_t count, int global) {
    nov_gc_roots *p;
    size_t i;
    if (!roots) NOV_GC_PANIC("GC invalid root record");
    for (p = heap->frames; p; p = p->next)
        if (p == roots) NOV_GC_PANIC("GC roots already registered");
    for (p = heap->globals; p; p = p->next)
        if (p == roots) NOV_GC_PANIC("GC roots already registered");
    if (count && !slots) NOV_GC_PANIC("GC invalid root slots");
    for (i = 0; i < count; ++i)
        if (!slots[i]) NOV_GC_PANIC("GC invalid root slot");
    roots->slots = slots;
    roots->count = count;
    roots->next = global ? heap->globals : heap->frames;
    if (global) heap->globals = roots;
    else heap->frames = roots;
}

static inline void nov_gc_push(nov_gc_heap *heap, nov_gc_roots *frame,
                               void ***slots, size_t count) {
    nov_gc_roots_add(heap, frame, slots, count, 0);
}
static inline void nov_gc_pop(nov_gc_heap *heap, nov_gc_roots *frame) {
    if (heap->frames != frame || !frame) NOV_GC_PANIC("GC root frames must pop in LIFO order");
    heap->frames = frame->next;
    *frame = (nov_gc_roots){0};
}
static inline void nov_gc_register_globals(nov_gc_heap *heap, nov_gc_roots *roots,
                                           void ***slots, size_t count) {
    nov_gc_roots_add(heap, roots, slots, count, 1);
}
static inline void nov_gc_unregister_globals(nov_gc_heap *heap, nov_gc_roots *roots) {
    nov_gc_roots **p = &heap->globals;
    while (*p && *p != roots) p = &(*p)->next;
    if (!*p) NOV_GC_PANIC("GC globals not registered");
    *p = roots->next;
    *roots = (nov_gc_roots){0};
}

/* Search before dereferencing: reject non-base, foreign and stale pointers. */
static inline void nov_gc_mark(nov_gc_heap *heap, void *pointer, nov_gc_object **gray) {
    nov_gc_object *p;
    if (!pointer) return;
    for (p = heap->objects; p; p = p->h.next) {
        if ((void *)(p + 1) != pointer) continue;
        if (!p->h.marked) {
            p->h.marked = 1;
            p->h.gray = *gray;
            *gray = p;
        }
        return;
    }
    NOV_GC_PANIC("GC invalid managed pointer");
}

static inline void nov_gc_collect(nov_gc_heap *heap) {
    nov_gc_object *gray = NULL, **link;
    nov_gc_roots *r;
    size_t i;
    for (r = heap->frames; r; r = r->next)
        for (i = 0; i < r->count; ++i) nov_gc_mark(heap, *r->slots[i], &gray);
    for (r = heap->globals; r; r = r->next)
        for (i = 0; i < r->count; ++i) nov_gc_mark(heap, *r->slots[i], &gray);
    while (gray) {
        nov_gc_object *p = gray;
        const nov_gc_descriptor *d = p->h.descriptor;
        gray = p->h.gray;
        p->h.gray = NULL;
        for (i = 0; i < d->pointer_count; ++i) {
            void *child;
            memcpy(&child, (unsigned char *)(p + 1) + d->pointer_offsets[i], sizeof child);
            nov_gc_mark(heap, child, &gray);
        }
    }
    link = &heap->objects;
    while (*link) {
        nov_gc_object *p = *link;
        if (p->h.marked) {
            p->h.marked = 0;
            link = &p->h.next;
        } else {
            *link = p->h.next;
            heap->live_bytes -= sizeof *p + p->h.descriptor->size;
            --heap->live_objects;
            NOV_GC_FREE(p);
        }
    }
    heap->next_collection = heap->live_bytes > (SIZE_MAX - 65536) / 2
        ? SIZE_MAX : heap->live_bytes * 2 + 65536;
}

static inline void nov_gc_validate(const nov_gc_descriptor *d) {
    size_t i;
    if (!d || !d->size || !d->alignment ||
        d->alignment > _Alignof(max_align_t) ||
        (_Alignof(max_align_t) % d->alignment) != 0 ||
        d->size % d->alignment != 0 ||
        (d->pointer_count && !d->pointer_offsets))
        NOV_GC_PANIC("GC invalid descriptor");
    for (i = 0; i < d->pointer_count; ++i) {
        size_t offset = d->pointer_offsets[i];
        if (offset > d->size || d->size - offset < sizeof(void *) ||
            offset % _Alignof(void *) ||
            (i && offset - d->pointer_offsets[i - 1] < sizeof(void *)) ||
            (i && offset <= d->pointer_offsets[i - 1]))
            NOV_GC_PANIC("GC invalid pointer offset");
    }
}

/* NULL means OOM/size overflow only; invalid contracts always panic. */
static inline void *nov_gc_try_alloc(nov_gc_heap *heap, const nov_gc_descriptor *d) {
    nov_gc_object *p;
    size_t size, i;
    nov_gc_validate(d);
    if (d->size > SIZE_MAX - sizeof *p) return NULL;
    size = sizeof *p + d->size;
    if (heap->collect_every_allocation || size > heap->next_collection ||
        heap->live_bytes > heap->next_collection - size)
        nov_gc_collect(heap);
    if (heap->live_bytes > SIZE_MAX - size || heap->live_objects == SIZE_MAX) return NULL;
    p = NOV_GC_MALLOC(size);
    if (!p) {
        nov_gc_collect(heap);
        p = NOV_GC_MALLOC(size);
        if (!p) return NULL;
    }
    memset(p + 1, 0, d->size);
    /* C does not require a null pointer to have an all-zero representation. */
    for (i = 0; i < d->pointer_count; ++i) {
        void *null_pointer = NULL;
        memcpy((unsigned char *)(p + 1) + d->pointer_offsets[i], &null_pointer, sizeof null_pointer);
    }
    p->h.next = heap->objects;
    p->h.gray = NULL;
    p->h.descriptor = d;
    p->h.marked = 0;
    heap->objects = p;
    ++heap->live_objects;
    heap->live_bytes += size;
    return p + 1;
}
static inline void *nov_gc_alloc(nov_gc_heap *heap, const nov_gc_descriptor *d) {
    void *p = nov_gc_try_alloc(heap, d);
    if (!p) NOV_GC_PANIC("GC allocation failed");
    return p;
}
static inline void nov_gc_shutdown(nov_gc_heap *heap) {
    nov_gc_object *p;
    if (heap->frames || heap->globals) NOV_GC_PANIC("GC shutdown with registered roots");
    while ((p = heap->objects) != NULL) {
        heap->objects = p->h.next;
        NOV_GC_FREE(p);
    }
    nov_gc_init(heap);
}
#endif
