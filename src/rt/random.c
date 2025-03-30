//
//  Copyright (C) 2025  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "util.h"
#include "jit/jit.h"
#include "option.h"
#include "thread.h"

#include <assert.h>

#define MT_N       624
#define MT_M       397
#define MATRIX_A   0x9908B0DFUL
#define UPPER_MASK 0x80000000UL
#define LOWER_MASK 0x7FFFFFFFUL

static uint32_t   mt[MT_N];
static int        mti = MT_N + 1;
static nvc_lock_t lock;

static void mt19937_init(uint32_t seed)
{
   mt[0] = seed;
   for (mti = 1; mti < MT_N; mti++)
      mt[mti] = 1812433253UL * (mt[mti - 1] ^ (mt[mti - 1] >> 30)) + mti;

   DEBUG_ONLY(debugf("initialised MT19937 with seed %08x", seed));
}

static uint32_t mt19937_next(void)
{
   static const uint32_t mag01[2] = { 0x0UL, MATRIX_A };
   uint32_t y;

   if (mti == MT_N) {
      // Twist
      int kk;
      for (kk = 0; kk < MT_N - MT_M; kk++) {
         y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
         mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ mag01[y & 0x1UL];
      }
      for (; kk < MT_N - 1; kk++) {
         y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
         mt[kk] = mt[kk + (MT_M - MT_N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
      }

      y = (mt[MT_N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
      mt[MT_N - 1] = mt[MT_M - 1] ^ (y >> 1) ^ mag01[y & 0x1UL];

      mti = 0;
   }
   else
      assert(mti < MT_N);

   y = mt[mti++];

   // Tempering
   y ^= (y >> 11);
   y ^= (y << 7) & 0x9D2C5680UL;
   y ^= (y << 15) & 0xEFC60000UL;
   y ^= (y >> 18);

   return y;
}

uint32_t get_random(void)
{
   SCOPED_LOCK(lock);

   if (mti > MT_N)
      mt19937_init(opt_get_int(OPT_RANDOM_SEED));

   return mt19937_next();
}

DLLEXPORT
void _nvc_random_get_next(jit_scalar_t *args)
{
   args[0].integer = get_random();
}
