/*
 * Copyright (c) 2000-2022 Stephen Williams (steve@icarus.com)
 *
 *    This source code is free software; you can redistribute it
 *    and/or modify it in source code form under the terms of the GNU
 *    General Public License as published by the Free Software
 *    Foundation; either version 2 of the License, or (at your option)
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*
 * Algorithm for probabilistic distribution functions.
 *
 * IEEE Std 1800-2023 SystemVerilog Unified Hardware Design and Verification Language
 */

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <math.h>

#include "svrand.h"

static double uniform(int32_t *seed, int32_t start, int32_t end);
static double normal(int32_t *seed, int32_t mean, int32_t deviation);
static double exponential(int32_t *seed, int32_t mean);
static int32_t poisson(int32_t *seed, int32_t mean);
static double chi_square(int32_t *seed, int32_t deg_of_free);
static double t(int32_t *seed, int32_t deg_of_free);
static double erlangian(int32_t *seed, int32_t k, int32_t mean);

int32_t rtl_dist_chi_square(int32_t *seed, int32_t df)
{
      double r;
      int32_t i;

      if (df > 0) {
            r = chi_square(seed, df);
            if (r >= 0) {
                  i = (int32_t) (r + 0.5);
            } else {
                  r = -r;
                  i = (int32_t) (r + 0.5);
                  i = -i;
            }
      } else {
            printf("WARNING: Chi_square distribution must have "
                   "a positive degree of freedom\n");
            i = 0;
      }

      return i;
}

int32_t rtl_dist_erlang(int32_t *seed, int32_t k, int32_t mean)
{
      double r;
      int32_t i;

      if (k > 0) {
            r = erlangian(seed, k, mean);
            if (r >= 0) {
                  i = (int32_t) (r + 0.5);
            } else {
                  r = -r;
                  i = (int32_t) (r + 0.5);
                  i = -i;
            }
      } else {
            printf("WARNING: K-stage erlangian distribution must have "
                   "a positive k\n");
            i = 0;
      }

      return i;
}

int32_t rtl_dist_exponential(int32_t *seed, int32_t mean)
{
      double r;
      int32_t i;

      if (mean > 0) {
            r = exponential(seed, mean);
            if (r >= 0) {
                  i = (int32_t) (r + 0.5);
            } else {
                  r = -r;
                  i = (int32_t) (r + 0.5);
                  i = -i;
            }
      } else {
            printf("WARNING: Exponential distribution must have "
                   "a positive mean\n");
            i = 0;
      }

      return i;
}

int32_t rtl_dist_normal(int32_t *seed, int32_t mean, int32_t sd)
{
      double r;
      int32_t i;

      r = normal(seed, mean, sd);
      if (r >= 0) {
            i = (int32_t) (r + 0.5);
      } else {
            r = -r;
            i = (int32_t) (r + 0.5);
            i = -i;
      }

      return i;
}

int32_t rtl_dist_poisson(int32_t *seed, int32_t mean)
{
      int32_t i;

      if (mean > 0) {
            i = poisson(seed, mean);
      } else {
            printf("WARNING: Poisson distribution must have "
                   "a positive mean\n");
            i = 0;
      }

      return i;
}

int32_t rtl_dist_t(int32_t *seed, int32_t df)
{
      double r;
      int32_t i;

      if (df > 0) {
            r = t(seed, df);
            if (r >= 0) {
                  i = (int32_t) (r + 0.5);
            } else {
                  r = -r;
                  i = (int32_t) (r + 0.5);
                  i = -i;
            }
      } else {
            printf("WARNING: t distribution must have "
                   "a positive degree of freedom\n");
            i = 0;
      }

      return i;
}

int32_t rtl_dist_uniform(int32_t *seed, int32_t start, int32_t end)
{
      double r;
      int32_t i;

      if (start >= end) return start;

      if (end != INT32_MAX) {
            end++;
            r = uniform(seed, start, end);
            if (r >= 0) {
                  i = (int32_t) r;
            } else {
	          i = (int32_t) (r - 1);
            }
            if (i < start) i = start;
            if (i >= end) i = end - 1;
      } else if (start != INT32_MIN) {
            start--;
            r = uniform(seed, start, end) + 1.0;
            if (r >= 0) {
                  i = (int32_t) r;
            } else {
	          i = (int32_t) (r - 1);
            }
            if (i <= start) i = start + 1;
            if (i > end) i = end;
      } else {
            r = (uniform(seed, start, end) + 2147483648.0) / 4294967295.0;
            r = r * 4294967296.0 - 2147483648.0;

            if (r >= 0) {
                  i = (int32_t) r;
            } else {
	          i = (int32_t) (r - 1);
            }
      }

      return i;
}

static double uniform(int32_t *seed, int32_t start, int32_t end)
{
      double d = 0.00000011920928955078125;
      double a, b, c;
      uint32_t oldseed, newseed;

      oldseed = *seed;
      if (oldseed == 0)
            oldseed = 259341593;

      if (start >= end) {
            a = 0.0;
            b = 2147483647.0;
      } else {
            a = (double)start;
            b = (double)end;
      }

      /* Original routine used signed arithmetic, and the (frequent)
       * overflows trigger "Undefined Behavior" according to the
       * C standard (both c89 and c99).  Using unsigned arithmetic
       * forces a conforming C implementation to get the result
       * that the IEEE-1364-2001 committee wants.
       */
      newseed = 69069 * oldseed + 1;

      *seed = newseed;

#if 0
      /* Cadence-donated conversion from unsigned int to double */
      {
            union { float s; unsigned stemp; } u;
            u.stemp = (newseed >> 9) | 0x3f800000;
            c = (double) u.s;
      }
#else
      /* Equivalent conversion without assuming IEEE 32-bit float */
      /* constant is 2^(-23) */
      c = 1.0 + (newseed >> 9) * 0.00000011920928955078125;
#endif

      c = c + (c*d);
      c = ((b - a) * (c - 1.0)) + a;

      return c;
}

static double normal(int32_t *seed, int32_t mean, int32_t deviation)
{
      double v1, v2, s;

      s = 1.0;
      while ((s >= 1.0) || (s == 0.0)) {
            v1 = uniform(seed, -1, 1);
            v2 = uniform(seed, -1, 1);
            s = v1 * v1 + v2 * v2;
      }
      s = v1 * sqrt(-2.0 * log(s) / s);
      v1 = (double) deviation;
      v2 = (double) mean;

      return s * v1 + v2;
}

static double exponential(int32_t *seed, int32_t mean)
{
      double n;

      n = uniform(seed, 0, 1);
      if (n != 0.0) {
            n = -log(n) * mean;
      }

      return n;
}

static int32_t poisson(int32_t *seed, int32_t mean)
{
      int32_t n;
      double p, q;

      n = 0;
      q = -(double) mean;
      p = exp(q);
      q = uniform(seed, 0, 1);
      while (p < q) {
            n++;
            q = uniform(seed, 0, 1) * q;
      }

      return n;
}

static double chi_square(int32_t *seed, int32_t deg_of_free)
{
      double x;
      int32_t k;

      if (deg_of_free % 2) {
            x = normal(seed, 0, 1);
            x = x * x;
      } else {
            x = 0.0;
      }
      for (k = 2; k <= deg_of_free; k = k + 2) {
            x = x + 2 * exponential(seed, 1);
      }

      return x;
}

static double t( int32_t *seed, int32_t deg_of_free)
{
      double x, chi2, dv, root;

      chi2 = chi_square(seed, deg_of_free);
      dv = chi2 / (double) deg_of_free;
      root = sqrt(dv);
      x = normal(seed, 0, 1) / root;

      return x;
}

static double erlangian(int32_t *seed, int32_t k, int32_t mean)
{
      double x, a, b;
      int32_t i;

      x = 1.0;
      for (i = 1; i <= k; i++) {
            x = x * uniform(seed, 0, 1);
      }
      a = (double) mean;
      b = (double) k;
      x = -a * log(x) / b;

      return x;
}
