/* Immutable UTF-8 views. Only owner is traced; literal data is foreign static
 * storage. Allocating callers root both operands before entering this helper. */
static inline nov_string nov_string_concat(nov_string a, nov_string b) {
    uintptr_t len;
    nov_gc_descriptor descriptor;
    nov_string result = {0};
    if (b.len > UINTPTR_MAX - a.len) nov_rt_panic("string size overflow");
    len = a.len + b.len;
    if (len > SIZE_MAX) nov_rt_panic("string size overflow");
    if (!len) return result;
    descriptor = (nov_gc_descriptor){(size_t)len, _Alignof(unsigned char), 0, NULL};
    result.owner = nov_gc_alloc(&nov_rt_heap, &descriptor);
    result.data = (const unsigned char *)result.owner;
    result.len = len;
    if (a.len) memcpy(result.owner, a.data, (size_t)a.len);
    if (b.len) memcpy((unsigned char *)result.owner + (size_t)a.len, b.data, (size_t)b.len);
    return result;
}
