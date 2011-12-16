//
//  Copyright (C) 2011  Nick Gasson
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
#include "phase.h"

////////////////////////////////////////////////////////////////////////////////
// Replace processes of the form
//
//   process is
//     x <= y;
//     wait on y;
//   end process;
//
// Where x and y are references to signals with an alias.

struct collapse_list {
   tree_t old, new;
   struct collapse_list *next;
};

struct collapse_ctx {
   tree_t top;
   struct collapse_list *replace;
};

static tree_t opt_collapse_find_fn(tree_t t, void *context)
{
   struct collapse_ctx *ctx = context;

   if (tree_kind(t) != T_PROCESS)
      return t;

   const bool maybe_alias =
      (tree_stmts(t) == 2)
      && (tree_kind(tree_stmt(t, 0)) == T_SIGNAL_ASSIGN)
      && (tree_waveforms(tree_stmt(t, 0)) == 1)
      && (tree_kind(tree_stmt(t, 1)) == T_WAIT);

   if (maybe_alias) {
      tree_t assign = tree_stmt(t, 0);
      tree_t target = tree_target(assign);
      tree_t wave   = tree_waveform(assign, 0);
      tree_t wait   = tree_stmt(t, 1);
      tree_t lhs    = tree_ref(target);

      const bool lhs_simple = (tree_kind(target) == T_REF);
      const bool rhs_simple =
         (tree_kind(tree_value(wave)) == T_REF)
         && (tree_kind(tree_ref(tree_value(wave))) == T_SIGNAL_DECL)
         && !tree_has_delay(wave);

      if (lhs_simple && rhs_simple) {
         tree_t rhs = tree_ref(tree_value(wave));

         printf("rhs=%s lhs=%s trigger=%s\n",
                istr(tree_ident(rhs)), istr(tree_ident(lhs)),
                istr(tree_ident(tree_ref(tree_trigger(wait, 0)))));

         const bool is_wait_on =
            !tree_has_delay(wait)
            && (tree_triggers(wait) == 1)
            && (tree_ref(tree_trigger(wait, 0)) == rhs);

         if (is_wait_on) {
            printf("found match!!!\n");

            tree_t a = tree_new(T_ALIAS);
            tree_set_loc(a, tree_loc(t));
            tree_set_ident(a, tree_ident(lhs));
            tree_set_value(a, tree_value(wave));

            struct collapse_list *l = xmalloc(sizeof(struct collapse_list));
            l->next = ctx->replace;
            l->old  = lhs;
            l->new  = tree_ref(tree_value(wave));

            ctx->replace = l;

            tree_add_decl(ctx->top, a);
            return NULL;
         }
      }
   }

   return t;
}

static tree_t opt_collapse_replace_fn(tree_t t, void *context)
{
   struct collapse_ctx *ctx = context;
   struct collapse_list *it;

   switch (tree_kind(t)) {
   case T_REF:
      for (it = ctx->replace; it != NULL; it = it->next) {
         if (tree_ref(t) == it->old)
            tree_set_ref(t, it->new);
      }
      break;

   case T_SIGNAL_DECL:
      for (it = ctx->replace; it != NULL; it = it->next) {
         if (t == it->old)
            return NULL;
      }
      break;

   default:
      break;
   }

   return t;
}

static void opt_collapse(tree_t top)
{
   struct collapse_ctx ctx = {
      .top     = top,
      .replace = NULL
   };
   tree_rewrite(top, opt_collapse_find_fn, &ctx);
   tree_rewrite(top, opt_collapse_replace_fn, &ctx);
}

////////////////////////////////////////////////////////////////////////////////

void opt(tree_t top)
{
   opt_collapse(top);
}
