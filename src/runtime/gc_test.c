#include <assert.h>
#include <stdlib.h>
#include <string.h>

static size_t attempts, outstanding, failures, fail_at;
static void *test_malloc(size_t size) {
    void *p;
    ++attempts;
    if (attempts == fail_at) return NULL;
    if (failures) { --failures; return NULL; }
    p = malloc(size);
    if (p) ++outstanding;
    return p;
}
static void test_free(void *p) {
    assert(outstanding && p);
    --outstanding;
    free(p);
}
#define NOV_GC_MALLOC test_malloc
#define NOV_GC_FREE test_free
#include "gc.h"

typedef struct { void *left, *right; size_t value; } node;
static const size_t offsets[] = { offsetof(node, left), offsetof(node, right) };
static const nov_gc_descriptor descriptor = {
    sizeof(node), _Alignof(node), 2, offsets
};

/* The argument is rooted by the caller before entry, not inside allocation. */
static void *cons(nov_gc_heap *heap, void *child) {
    node *p = nov_gc_alloc(heap, &descriptor);
    p->left = child;
    return p;
}

/* Scalar backing descriptors need not outlive this call. */
static void *scalar_backing(nov_gc_heap *heap, size_t bytes) {
    nov_gc_descriptor d = {bytes, 1, 0, NULL};
    void *p = nov_gc_alloc(heap, &d);
    d.size = 0;
    return p;
}

typedef struct { void *data; uintptr_t len, cap; } nov_array;
#define nov_rt_panic nov_gc_panic
#include "array.h"

typedef struct { unsigned char pad; node nested; void *owner; } array_element;
static void managed_arrays(void) {
    size_t map[] = { offsetof(array_element, nested.left), offsetof(array_element, nested.right), offsetof(array_element, owner) };
    nov_gc_descriptor element = {sizeof(array_element), _Alignof(array_element), 3, map};
    nov_array a = {0}, b = {0}, c = {0};
    void **slots[] = {&a.data, &b.data, &c.data};
    nov_gc_roots frame = {0};
    size_t before, i;
    nov_rt_heap.collect_every_allocation = 1;
    nov_gc_push(&nov_rt_heap, &frame, slots, 3);
    a = nov_array_new(2, 4, &element);
    for (i = 0; i < 4; ++i) {
        array_element *p = a.data;
        assert(!p[i].nested.left && !p[i].nested.right && !p[i].owner);
    }
    ((array_element *)a.data)[0].nested.left = nov_gc_alloc(&nov_rt_heap, &descriptor);
    ((array_element *)a.data)[1].owner = nov_gc_alloc(&nov_rt_heap, &descriptor);
    /* Capacity slots are traced too; new slots started with actual NULLs. */
    ((array_element *)a.data)[3].owner = nov_gc_alloc(&nov_rt_heap, &descriptor);
    nov_gc_collect(&nov_rt_heap);
    assert(nov_rt_heap.live_objects == 4);
    before = attempts;
    failures = 1;
    b = nov_array_clone(a, &element);
    assert(attempts == before + 2 && a.data != b.data);
    c = nov_array_append(a, b, &element);
    assert(c.len == 4 && c.cap == 4 && c.data != a.data && c.data != b.data);
    /* Neither the per-element map nor descriptor is borrowed by any backing. */
    map[0] = map[1] = map[2] = SIZE_MAX;
    element = (nov_gc_descriptor){0};
    a.data = b.data = NULL;
    nov_gc_collect(&nov_rt_heap);
    assert(nov_rt_heap.live_objects == 3);
    assert(((array_element *)c.data)[0].nested.left == ((array_element *)c.data)[2].nested.left);
    ((array_element *)c.data)[0].nested.value = 99;
    assert(!((array_element *)c.data)[2].nested.value);
    c.data = NULL;
    nov_gc_collect(&nov_rt_heap);
    assert(!nov_rt_heap.live_objects && !nov_rt_heap.live_bytes);
    nov_gc_pop(&nov_rt_heap, &frame);
    nov_gc_shutdown(&nov_rt_heap);
}

int main(int argc, char **argv) {
    nov_gc_heap heap;
    nov_gc_roots frame = {0}, global = {0}, inner = {0};
    void *root = NULL, *saved = NULL;
    void **slots[] = { &root, &saved };
    void **global_slots[] = { &saved };
    size_t i, before;
    nov_gc_init(&heap);
    nov_gc_push(&heap, &frame, slots, 2);
    if (argc > 1) {
        if (!strcmp(argv[1], "oom")) {
            failures = 2;
            (void)nov_gc_alloc(&heap, &descriptor);
        } else if (!strcmp(argv[1], "double")) {
            nov_gc_push(&heap, &frame, slots, 2);
        } else if (!strcmp(argv[1], "pop")) {
            nov_gc_push(&heap, &inner, NULL, 0);
            nov_gc_pop(&heap, &frame);
        } else if (!strcmp(argv[1], "interior")) {
            root = nov_gc_alloc(&heap, &descriptor);
            root = (unsigned char *)root + 1;
            nov_gc_collect(&heap);
        } else if (!strcmp(argv[1], "foreign")) {
            root = &i;
            nov_gc_collect(&heap);
        } else if (!strcmp(argv[1], "descriptor")) {
            const size_t bad[] = { sizeof(node) };
            const nov_gc_descriptor d = { sizeof(node), _Alignof(node), 1, bad };
            (void)nov_gc_alloc(&heap, &d);
        } else if (!strcmp(argv[1], "stride")) {
            const size_t offset = 0;
            const nov_gc_descriptor d = { sizeof(void *) + 1, 1, 1, &offset };
            (void)nov_gc_alloc_repeated(&heap, &d, 2);
        } else if (!strcmp(argv[1], "alignment")) {
            const nov_gc_descriptor d = { 64, _Alignof(max_align_t) * 2, 0, NULL };
            (void)nov_gc_alloc(&heap, &d);
        } else if (!strcmp(argv[1], "shutdown")) {
            nov_gc_shutdown(&heap);
        } else if (!strcmp(argv[1], "unregister")) {
            nov_gc_unregister_globals(&heap, &global);
        }
        return 99;
    }
    heap.collect_every_allocation = 1;
    root = scalar_backing(&heap, 7);
    saved = scalar_backing(&heap, 19);
    nov_gc_collect(&heap);
    assert(heap.live_objects == 2);
    assert(((unsigned char *)root)[6] == 0 && ((unsigned char *)saved)[18] == 0);
    root = saved = NULL;
    nov_gc_collect(&heap);
    assert(!heap.live_objects && !heap.live_bytes && !outstanding);
    root = nov_gc_alloc(&heap, &descriptor);
    assert((uintptr_t)root % _Alignof(node) == 0);
    assert(!((node *)root)->left && !((node *)root)->right && !((node *)root)->value);
    saved = root;
    for (i = 0; i < 2000; ++i) {
        root = cons(&heap, root);
        ((node *)root)->value = i + 1;
    }
    assert(heap.live_objects == 2001);
    before = attempts;
    failures = 100;
    nov_gc_collect(&heap);
    assert(attempts == before && heap.live_objects == 2001);
    assert(((node *)root)->value == 2000 && saved != root);
    {
        node *p = root;
        for (i = 2000; i; --i) { assert(p->value == i); p = p->left; }
        assert(p == saved);
    }
    /* Permanent OOM preserves all live objects and roots. */
    assert(!nov_gc_try_alloc(&heap, &descriptor));
    assert(heap.live_objects == 2001 && root && saved);
    failures = 1;
    root = cons(&heap, root); /* Failed malloc -> allocation-free GC -> retry. */
    assert(heap.live_objects == 2002);
    nov_gc_register_globals(&heap, &global, global_slots, 1);
    nov_gc_pop(&heap, &frame);
    nov_gc_collect(&heap);
    assert(heap.live_objects == 1 && heap.objects + 1 == saved);
    nov_gc_push(&heap, &frame, slots, 2);
    root = saved;
    ((node *)root)->left = root;
    saved = NULL;
    root = NULL;
    nov_gc_collect(&heap);
    assert(!heap.live_objects && !heap.live_bytes && !outstanding);
    /* An unreachable two-object cycle, sharing, and partially initialized nodes. */
    root = nov_gc_alloc(&heap, &descriptor);
    saved = nov_gc_alloc(&heap, &descriptor);
    ((node *)root)->left = saved;
    ((node *)root)->right = saved;
    ((node *)saved)->left = root;
    nov_gc_collect(&heap);
    assert(heap.live_objects == 2);
    root = saved = NULL;
    nov_gc_collect(&heap);
    assert(!heap.live_objects && !outstanding);
    /* A deep graph without per-allocation GC proves tracing needs no C recursion. */
    heap.collect_every_allocation = 0;
    heap.next_collection = SIZE_MAX;
    for (i = 0; i < 12000; ++i) root = cons(&heap, root);
    nov_gc_collect(&heap);
    assert(heap.live_objects == 12000);
    root = NULL;
    nov_gc_collect(&heap);
    heap.collect_every_allocation = 1;
    /* Fail each malloc position of a rooted construction in turn. */
    for (i = 1; i <= 64; ++i) {
        size_t j;
        fail_at = attempts + i;
        for (j = 0; j < 64; ++j) root = cons(&heap, root);
        assert(heap.live_objects == 64);
        root = NULL;
        nov_gc_collect(&heap);
        assert(!outstanding);
        fail_at = 0;
    }
    /* Repeated permanent and transient failures. */
    for (i = 0; i < 100; ++i) {
        failures = i % 3;
        root = nov_gc_try_alloc(&heap, &descriptor);
        if (i % 3 == 2) assert(!root);
        else assert(root);
        failures = 0;
    }
    {
        const nov_gc_descriptor huge = { SIZE_MAX, 1, 0, NULL };
        before = attempts;
        assert(!nov_gc_try_alloc(&heap, &huge));
        assert(attempts == before);
        assert(!nov_gc_try_alloc_repeated(&heap, &descriptor, SIZE_MAX));
        assert(!nov_gc_try_alloc_repeated(&heap, &descriptor, 0));
        {
            const size_t offset = 0;
            const nov_gc_descriptor pointer = {sizeof(void *), _Alignof(void *), 1, &offset};
            assert(!nov_gc_try_alloc_repeated(&heap, &pointer, SIZE_MAX / sizeof(void *) / 2));
        }
        assert(attempts == before);
    }
    nov_gc_unregister_globals(&heap, &global);
    nov_gc_pop(&heap, &frame);
    nov_gc_shutdown(&heap); /* Frees even still reachable objects, no GC needed. */
    nov_gc_shutdown(&heap);
    assert(!outstanding && !heap.live_bytes && !heap.live_objects);
    managed_arrays();
    assert(!outstanding);
    return 0;
}
