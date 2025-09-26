#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

void issue1301_proc_c(uint8_t *b, int64_t b_len, uint8_t *c) {
    printf("b = %p, len = %"PRIi64", c = %p\n", b, b_len, c);
    *c = 0;
    for (int64_t i =  0; i < b_len; i++)
       *c ^= b[i];
}
