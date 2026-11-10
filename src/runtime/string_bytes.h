/* Exact-length copying conversions. The emitter roots operands and output slots.
 * Each helper allocates at most once; no safepoint follows allocation before the
 * returned owner is installed in its rooted output slot. */
static inline nov_array nov_string_to_bytes(nov_string text) {
    const nov_gc_descriptor byte = {sizeof(unsigned char), _Alignof(unsigned char), 0, NULL};
    nov_array result = nov_array_new(text.len, text.len, &byte);
    if (text.len) memcpy(result.data, text.data, (size_t)text.len);
    return result;
}

static inline nov_string nov_bytes_to_string(nov_array bytes) {
    const unsigned char *data = (const unsigned char *)bytes.data;
    uintptr_t offset = 0;
    nov_string result = {0};
    if (bytes.len > SIZE_MAX) nov_rt_panic("string size overflow");
    /* Do not trust the caller: string values must always contain strict UTF-8. */
    while (offset < bytes.len) {
        const unsigned char lead = data[offset++];
        uint32_t value, minimum;
        uintptr_t extra;
        if (lead < 0x80u) continue;
        if (lead >= 0xc2u && lead <= 0xdfu) { value = lead & 0x1fu; minimum = 0x80u; extra = 1; }
        else if (lead >= 0xe0u && lead <= 0xefu) { value = lead & 0x0fu; minimum = 0x800u; extra = 2; }
        else if (lead >= 0xf0u && lead <= 0xf4u) { value = lead & 0x07u; minimum = 0x10000u; extra = 3; }
        else { nov_rt_panic("invalid UTF-8"); return result; }
        if (extra > bytes.len - offset) nov_rt_panic("invalid UTF-8");
        for (uintptr_t i = 0; i < extra; ++i) {
            const unsigned char next = data[offset++];
            if ((next & 0xc0u) != 0x80u) nov_rt_panic("invalid UTF-8");
            value = (value << 6) | (next & 0x3fu);
        }
        if (value < minimum || value > 0x10ffffu || (value >= 0xd800u && value <= 0xdfffu)) nov_rt_panic("invalid UTF-8");
    }
    if (!bytes.len) return result;
    const nov_gc_descriptor descriptor = {(size_t)bytes.len, _Alignof(unsigned char), 0, NULL};
    result.owner = nov_gc_alloc(&nov_rt_heap, &descriptor);
    result.data = (const unsigned char *)result.owner;
    result.len = bytes.len;
    memcpy(result.owner, data, (size_t)bytes.len);
    return result;
}
