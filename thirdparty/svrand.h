#ifndef _SVRAND_H
#define _SVRAND_H

#include <stdint.h>

int32_t rtl_dist_chi_square(int32_t *seed, int32_t df);
int32_t rtl_dist_erlang(int32_t *seed, int32_t k, int32_t mean);
int32_t rtl_dist_exponential(int32_t *seed, int32_t mean);
int32_t rtl_dist_normal(int32_t *seed, int32_t mean, int32_t sd);
int32_t rtl_dist_poisson(int32_t *seed, int32_t mean);
int32_t rtl_dist_t(int32_t *seed, int32_t df);
int32_t rtl_dist_uniform(int32_t *seed, int32_t start, int32_t end);

#endif  // _SVRAND_H
