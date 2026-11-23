#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

uint32_t order_total(uint32_t quantity, uint32_t unit_price);

uint32_t c_shipping_fee(uint32_t quantity) {
    return 2u + quantity;
}

int main(void) {
    const uint32_t total = order_total(3, 14);
    printf("Order total: %" PRIu32 "\n", total);
    return total != 47;
}
