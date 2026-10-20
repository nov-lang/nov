/* Scalar-only value arrays; nov_array is declared by the emitter before helpers.
 * All allocating callers keep input backing rooted. */
static nov_gc_heap nov_rt_heap = { .next_collection = 65536 };

static inline nov_array nov_array_new(uintptr_t len, uintptr_t cap, size_t size, size_t alignment) {
    nov_array a = {NULL, len, cap};
    nov_gc_descriptor d;
    if (len > cap || cap > SIZE_MAX / size) nov_rt_panic("array size overflow");
    if (!cap) return a;
    d = (nov_gc_descriptor){(size_t)cap * size, alignment, 0, NULL};
    a.data = nov_gc_alloc(&nov_rt_heap, &d);
    return a;
}
static inline nov_array nov_array_clone(nov_array a, size_t size, size_t alignment) {
    nov_array b = nov_array_new(a.len, a.len, size, alignment);
    if (a.len) memcpy(b.data, a.data, (size_t)a.len * size);
    return b;
}
static inline uintptr_t nov_array_index(nov_array a, intptr_t index) {
    /* Unsigned magnitude handles INTPTR_MIN without signed negation overflow. */
    uintptr_t magnitude = (uintptr_t)0 - (uintptr_t)index;
    if (index < 0) {
        if (magnitude > a.len) nov_rt_panic("array index out of bounds");
        return a.len - magnitude;
    }
    if ((uintptr_t)index >= a.len) nov_rt_panic("array index out of bounds");
    return (uintptr_t)index;
}
static inline nov_array nov_array_append(nov_array a, nov_array b, size_t size, size_t alignment) {
    uintptr_t len, cap;
    nov_array result;
    if (b.len > UINTPTR_MAX - a.len) nov_rt_panic("array size overflow");
    len = a.len + b.len;
    cap = a.cap;
    if (cap < len) cap = cap <= UINTPTR_MAX / 2 && cap * 2 >= len ? cap * 2 : len;
    result = nov_array_new(len, cap, size, alignment);
    if (a.len) memcpy(result.data, a.data, (size_t)a.len * size);
    if (b.len) memcpy((unsigned char *)result.data + (size_t)a.len * size, b.data, (size_t)b.len * size);
    return result;
}
