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
static inline uintptr_t nov_array_slice_bound(intptr_t bound, uintptr_t len) {
    if (bound < 0) {
        const uintptr_t magnitude = (uintptr_t)0 - (uintptr_t)bound;
        if (magnitude > len) nov_rt_panic("array slice out of bounds");
        return len - magnitude;
    }
    if ((uintptr_t)bound > len) nov_rt_panic("array slice out of bounds");
    return (uintptr_t)bound;
}
static inline nov_array nov_array_slice(nov_array a, intptr_t start, intptr_t end, bool has_end, bool inclusive, const nov_gc_descriptor *element) {
    const uintptr_t first = nov_array_slice_bound(start, a.len);
    uintptr_t last = has_end ? nov_array_slice_bound(end, a.len) : a.len;
    if (has_end && inclusive) {
        if (last >= a.len) nov_rt_panic("array slice out of bounds");
        ++last;
    }
    if (first > last) nov_rt_panic("array slice out of bounds");
    nov_array result = nov_array_new(last - first, last - first, element);
    /* Copy pointer slots before recursive allocations so the rooted result
     * traces every selected child, including children not yet detached. */
    if (result.len) memcpy(result.data, (const unsigned char *)a.data + (size_t)first * element->size, (size_t)result.len * element->size);
    return result;
}
static inline void nov_array_clear(nov_array *a, const nov_gc_descriptor *element) {
    /* The GC traces capacity, not length. Remove managed children as well as
     * values, while retaining the backing allocation and its capacity. */
    if (a->len) memset(a->data, 0, (size_t)a->len * element->size);
    a->len = 0;
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
