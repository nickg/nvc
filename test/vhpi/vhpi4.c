#include <stdio.h>
#include <stdint.h>

typedef enum { U = 0, X, ZERO, ONE, Z, W, L, H, D } std_logic;

int32_t __vhpi_sum(int32_t x, int32_t y)
{
   printf("__vhpi_sum(%d, %d)\n", x, y);
   return x + y;
}

int32_t __vhpi_sum_array(int32_t *a, int32_t length)
{
   int32_t sum = 0;
   for (int i = 0; i < length; i++)
      sum += a[i];
   return sum;
}

uint8_t __vhpi_my_not(uint8_t x)
{
   switch (x) {
   case ZERO: return ONE;
   case ONE: return ZERO;
   default: return x;
   }
}

void __vhpi_test_proc(int32_t *x, int32_t *arr)
{
   *x = 42;
   arr[1] = 5;
}
