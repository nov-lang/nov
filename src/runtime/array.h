/* Fixed-layout value arrays; nov_array is declared by the emitter before helpers.
 * All allocating callers keep input backing rooted. */
static nov_gc_heap nov_rt_heap = { .next_collection = 65536 };

static inline nov_array nov_array_new(uintptr_t len, uintptr_t cap, const nov_gc_descriptor *element) {
    nov_array a = {NULL, len, cap};
    nov_gc_validate(element);
    if (len > cap || cap > SIZE_MAX / element->size) nov_rt_panic("array size overflow");
    if (!cap) return a;
    a.data = nov_gc_alloc_repeated(&nov_rt_heap, element, (size_t)cap);
    return a;
}
static inline nov_array nov_array_clone(nov_array a, const nov_gc_descriptor *element) {
    nov_array b = nov_array_new(a.len, a.len, element);
    if (a.len) memcpy(b.data, a.data, (size_t)a.len * element->size);
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
static inline nov_array nov_array_append(nov_array a, nov_array b, const nov_gc_descriptor *element) {
    uintptr_t len, cap;
    nov_array result;
    if (b.len > UINTPTR_MAX - a.len) nov_rt_panic("array size overflow");
    len = a.len + b.len;
    cap = a.cap;
    if (cap < len) cap = cap <= UINTPTR_MAX / 2 && cap * 2 >= len ? cap * 2 : len;
    result = nov_array_new(len, cap, element);
    if (a.len) memcpy(result.data, a.data, (size_t)a.len * element->size);
    if (b.len) memcpy((unsigned char *)result.data + (size_t)a.len * element->size, b.data, (size_t)b.len * element->size);
    return result;
}
