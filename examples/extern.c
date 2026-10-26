#include <stdbool.h>
#include <stdint.h>

int32_t c_max_i32(int32_t left, int32_t right) {
    return left > right ? left : right;
}

bool c_is_even(uint32_t value) {
    return value % 2u == 0u;
}
