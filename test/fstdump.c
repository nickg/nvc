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

//
// Based on GtkWave fstminer.c
//

/*
 * Copyright (c) 2012-2014 Tony Bybell.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>

#include "fstapi.h"

static char **fac_names = NULL;
static unsigned int *scope_idx = NULL;

static char **scope_names = NULL;
long allocated_scopes = 1;

static void extractVarNames(void *xc)
{
   struct fstHier *h;
   char *s;
   const char *fst_scope_name = NULL;
   int fst_scope_name_len = 0;
   intptr_t snum = 0;
   intptr_t max_snum = 0;

   while ((h = fstReaderIterateHier(xc))) {
      switch(h->htyp) {
      case FST_HT_SCOPE:
         snum = ++max_snum;
         fst_scope_name = fstReaderPushScope(xc, h->u.scope.name, (void *)(snum));
         /* fst_scope_name_len = fstReaderGetCurrentScopeLen(xc); scan-build */

         if (snum >= allocated_scopes) {
            long new_allocated_scopes = allocated_scopes * 2;
            char **scope_names_2 = calloc(new_allocated_scopes, sizeof(char *));

            memcpy(scope_names_2, scope_names, allocated_scopes * sizeof(char *));
            free(scope_names);

            scope_names = scope_names_2;
            allocated_scopes = new_allocated_scopes;
         }

         scope_names[snum] = strdup(fst_scope_name);
         break;
      case FST_HT_UPSCOPE:
         /* fst_scope_name = scan-build */ fstReaderPopScope(xc);
         fst_scope_name_len = fstReaderGetCurrentScopeLen(xc);
         snum = fst_scope_name_len ? (intptr_t)fstReaderGetCurrentScopeUserInfo(xc) : 0;
         break;
      case FST_HT_VAR:
         if(!h->u.var.is_alias)
            {
               scope_idx[h->u.var.handle] = snum;

               s = fac_names[h->u.var.handle] = malloc(h->u.var.name_length + 1);
               memcpy(s, h->u.var.name, h->u.var.name_length);
               s[h->u.var.name_length] = '\0';
            }
      }
   }
}

static char *get_facname(void *ctx, fstHandle pnt_facidx)
{
   if (scope_idx[pnt_facidx] && scope_names[scope_idx[pnt_facidx]]) {
      char *fst_scope_name = scope_names[scope_idx[pnt_facidx]];
      int fst_scope_name_len = strlen(fst_scope_name);
      int fst_signal_name = strlen(fac_names[pnt_facidx]);
      char *s = malloc(fst_scope_name_len + 1 + fst_signal_name + 1);
      memcpy(s, fst_scope_name, fst_scope_name_len);
      s[fst_scope_name_len] = '.';
      memcpy(s + fst_scope_name_len + 1, fac_names[pnt_facidx],
             fst_signal_name + 1);
      return s;
   }
   else
      return strdup(fac_names[pnt_facidx]);
}

static void fst_callback2(void *ctx, uint64_t pnt_time, fstHandle pnt_facidx,
                          const unsigned char *pnt_value, uint32_t plen)
{
   char *fn;
   fn = get_facname(ctx, pnt_facidx);

   printf("#%"PRIu64" %s %s\n", pnt_time, fn, pnt_value);
}

static void fst_callback(void *ctx, uint64_t pnt_time, fstHandle pnt_facidx,
                         const unsigned char *pnt_value)
{
   uint32_t plen = 0;
   if (pnt_value)
      plen = strlen((const char *)pnt_value);

   fst_callback2(ctx, pnt_time, pnt_facidx, pnt_value, plen);
}

int main(int argc, char **argv)
{
   if (argc != 2) {
      fprintf(stderr, "usage: fstdump FILE\n");
      return 1;
   }

   void *ctx = fstReaderOpen(argv[1]);
   if (ctx == NULL) {
      fprintf(stderr, "failed to open %s\n", argv[1]);
      return 1;
   }

   int numfacs = fstReaderGetVarCount(ctx) + 1;

   fac_names = calloc(numfacs, sizeof(char *));
   scope_names = calloc(allocated_scopes, sizeof(char *));
   scope_idx = calloc(numfacs, sizeof(unsigned int));

   extractVarNames(ctx);

   fstReaderSetFacProcessMaskAll(ctx);
   fstReaderIterBlocks2(ctx, fst_callback, fst_callback2, ctx, NULL);

   fstReaderClose(ctx);
   return 0;
}
