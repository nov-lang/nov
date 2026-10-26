#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

int32_t add_i32(int32_t left, int32_t right);
uint64_t api_select_u64(bool condition, uint64_t yes, uint64_t no);
bool invert(bool value);
uint32_t rectangle_area(uint32_t width, uint32_t height);
uint32_t rectangle_perimeter(uint32_t width, uint32_t height);

int main(void) {
    const int32_t sum = add_i32(20, 22);
    const uint64_t selected = api_select_u64(true, UINT64_C(42), UINT64_C(7));
    const bool inverted = invert(false);
    const uint32_t area = rectangle_area(6, 7);
    const uint32_t perimeter = rectangle_perimeter(6, 7);
    printf("Sum: %" PRId32 "\nSelected: %" PRIu64 "\nInverted: %s\n"
           "Area: %" PRIu32 "\nPerimeter: %" PRIu32 "\n",
           sum, selected, inverted ? "true" : "false", area, perimeter);
    return sum != 42 || selected != 42 || !inverted || area != 42 || perimeter != 26;
}
