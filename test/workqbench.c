//
//  Copyright (C) 2022  Nick Gasson
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
#include "opt.h"
#include "thread.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <limits.h>
#include <assert.h>

#define NUM_JOBS 100000
static int *results;

#define PAUSE_MS 0

static void is_prime_cb(void *arg)
{
   int n = (intptr_t)arg;

   for (int i = 2; i < n / 2; i++) {
      if (n % i == 0) {
         results[n] = 1;
         break;
      }
   }
}

static void empty_cb(void *arg)
{
   spin_wait();
}

int main(int argc, char **argv)
{
   srandom((unsigned)time(NULL));

   term_init();
   thread_init();
   register_signal_handlers();

   opt_set_int(OPT_ERROR_LIMIT, -1);

   results = calloc(sizeof(int), NUM_JOBS);

   workq_t *wq1 = workq_new();
   workq_t *wq2 = workq_new();

   for (int i = 0; i < 10; i++) {
      if (i > 0 && PAUSE_MS > 0)
         usleep(PAUSE_MS * 1000);

      printf("iteration %d start\n", i);

      for (int j = 1; j < NUM_JOBS; j++)
         workq_do(wq1, is_prime_cb, (void *)(intptr_t)j);

      for (int j = 0; j < NUM_JOBS; j++)
         workq_do(wq2, empty_cb, NULL);

      workq_start(wq1);
      workq_start(wq2);

      workq_drain(wq1);
      workq_drain(wq2);
   }

   workq_free(wq1);
   workq_free(wq2);

   printf("\nFirst 100 primes:");
   for (int i = 2, nth = 0; nth < 100; i++) {
      if (!results[i]) {
         if (nth++ % 10 == 0)
            printf("\n  ");
         printf(" %3d", i);
      }
   }
   printf("\n\n");

   return 0;
}
