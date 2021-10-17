//
//  Copyright (C) 2021  Nick Gasson
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
#include "enode.h"
#include "cprop.h"
#include "common.h"
#include "hash.h"

#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

static e_node_t      root = NULL;
static cprop_vars_t *cprop_vars = NULL;

typedef void (*eopt_nexus_fn_t)(e_node_t, void *);

static void eopt_stmts(tree_t container, e_node_t cursor);
static void eopt_decls(tree_t container, e_node_t cursor);

static e_node_t eopt_split_nexus(e_node_t signal, e_node_t n, unsigned width)
{
   if (e_kind(n) == E_PADDING) {
      const unsigned owidth = e_width(n);
      assert(width > 0);
      assert(width < owidth);

      e_set_width(n, width);

      e_node_t pad2 = e_new(E_PADDING);
      e_set_width(pad2, owidth - width);

      e_insert_nexus(signal, n, pad2);
      return pad2;
   }
   else
      return e_split_nexus(root, n, width);
}

static void eopt_split_signal(e_node_t signal, unsigned offset, unsigned count,
                              eopt_nexus_fn_t callback, void *context)
{
   unsigned o = 0;
   for (int i = 0; count > 0 && i < e_nexuses(signal); i++) {
      e_node_t n = e_nexus(signal, i), drive = NULL;

      const int length = e_width(n);
      int dpos = i;

      if (offset == o) {
         if (count < length)
            eopt_split_nexus(signal, n, count);
         drive = n;
      } else if (offset > o && offset < o + length) {
         drive = eopt_split_nexus(signal, n, offset - o);
         dpos++;
         if (length - offset + o > count)
            eopt_split_nexus(signal, drive, count);
      } else if (offset + length < o)
         break;

      o += e_width(n);

      if (drive == NULL)
         continue;

      if (e_kind(drive) == E_PADDING) {
         // Convert padding into a real nexus
         e_node_t new = e_new(E_NEXUS);
         e_set_ident(new, e_path(signal));
         e_set_size(new, e_size(drive));
         e_set_width(new, e_width(drive));
         e_add_signal(new, signal);

         e_add_nexus(root, new);
         e_change_nexus(signal, dpos, new);
         drive = new;
      }

      if (callback) (*callback)(drive, context);

      const int consumed = e_width(drive);
      count -= consumed;
      offset += consumed;
   }

   if (offset != o && count > 0) {
      assert(o < offset);
      e_node_t n = e_new(E_PADDING);
      e_set_width(n, offset - o);
      e_add_nexus(signal, n);
   }

   if (count > 0) {
      e_node_t n = e_new(E_NEXUS);
      e_set_pos(n, NEXUS_POS_INVALID);
      e_set_ident(n, e_path(signal));
      e_set_width(n, count);
      e_add_signal(n, signal);

      e_add_nexus(signal, n);
      e_add_nexus(root, n);

      if (callback) (*callback)(n, context);
   }
}

static void eopt_nexus_add_driver_cb(e_node_t nexus, void *__ctx)
{
   e_node_t cursor = __ctx;

   bool have = false;
   const int np = e_nexuses(cursor);
   for (int j = 0; !have && j < np; j++)
      have = e_nexus(cursor, j) == nexus;

   if (!have) {
      e_add_nexus(cursor, nexus);
      e_add_source(nexus, cursor);
   }
}

static void eopt_add_driver(int op, vcode_reg_t target, vcode_reg_t count_reg,
                            cprop_state_t *regs, e_node_t cursor, bool driven)
{
   unsigned offset = 0, stride = 0, count = 0;
   e_node_t signal = NULL;

   cprop_get_signal(target, count_reg, regs, &offset, &stride, &count, &signal);

   if (signal == NULL) {
      const vunit_kind_t kind = vcode_unit_kind();
      if (kind == VCODE_UNIT_PROCEDURE || kind == VCODE_UNIT_FUNCTION) {
         // Ignore signal assignments to formal parameters: these are
         // handled by VCODE_OP_DRIVE_SIGNAL
         return;
      }
      else {
         cprop_dump(op, regs);
         fatal_trace("unknown target signal");
      }
   }

   eopt_nexus_fn_t fn = driven ? eopt_nexus_add_driver_cb : NULL;

   if (stride == 0)
      eopt_split_signal(signal, offset, count, fn, cursor);
   else {
      const unsigned total_length = e_width(signal);
      while (offset < total_length) {
         eopt_split_signal(signal, offset, count, fn, cursor);
         offset += stride;
      }
   }
}

static e_node_t eopt_find_signal_cb(ident_t name, int hops, void *__ctx)
{
   e_node_t scope = __ctx;

   ident_t scope_path = ident_runtil(name, ':');
   if (scope_path == name) {
      while (hops--)
         scope = e_parent(scope);
   }
   else {
      const int nscopes = e_scopes(root);
      for (int i = 0; i < nscopes; i++) {
         e_node_t s = e_scope(root, i);
         if (e_path(s) == scope_path) {
            scope = s;
            break;
         }
      }
   }

   const int nsignals = e_signals(scope);
   for (int i = 0; i < nsignals; i++) {
      e_node_t s = e_signal(scope, i);
      if (e_ident(s) == name)
         return s;
      else if (scope_path != name && e_path(s) == name)
         return s;
   }

   e_dump(root);
   fatal_trace("cannot find signal %s in scope %s", istr(name),
               istr(e_path(scope)));
}

static void eopt_map_signal_cb(int op, cprop_state_t *regs, void *__ctx)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   vcode_reg_t arg1 = vcode_get_arg(op, 1);
   vcode_reg_t arg2 = vcode_get_arg(op, 2);

   if (regs[arg2].tag != CP_CONST) {
      cprop_dump(op, regs);
      fatal_trace("map signal count is not constant");
   }

   unsigned offset0 = 0, stride0 = 0, count0 = 0;
   unsigned offset1 = 0, stride1 = 0, count1 = 0;
   e_node_t src = NULL, dst = NULL;

   cprop_get_signal(arg0, arg2, regs, &offset0, &stride0, &count0, &dst);
   cprop_get_signal(arg1, arg2, regs, &offset1, &stride1, &count1, &src);

   if (dst == NULL || src == NULL) {
      cprop_dump(op, regs);
      fatal_trace("map signal inputs are not signals");
   }

   assert(stride0 == 0);
   assert(stride1 == 0);
   assert(count0 == count1);

   if (e_flags(src) & E_F_RESOLVED)
      e_set_flag(dst, E_F_RESOLVED);

   // Ensure the existing nexuses are split at the right places
   eopt_split_signal(src, offset1, count1, false, NULL);

   assert(offset0 + count0 <= e_width(dst));

   int dst_i = 0, dst_nnexus = e_nexuses(dst);
   while (offset0 > 0) {
      if (dst_i == dst_nnexus) {
         e_node_t n = e_new(E_PADDING);
         e_set_width(n, offset0);
         offset0 = 0;

         e_add_nexus(dst, n);
         dst_i++, dst_nnexus++;
      }
      else {
         e_node_t n = e_nexus(dst, dst_i++);
         const unsigned w = e_width(n);
         if (w > offset0) {
            assert(e_kind(n) == E_PADDING);
            eopt_split_nexus(dst, n, offset0);
            offset0 = 0;
         }
         else
            offset0 -= w;
      }
   }

   int src_i = 0;
   while (offset1 > 0) {
      e_node_t n = e_nexus(src, src_i++);
      const unsigned w = e_width(n);
      assert(w <= offset1);
      offset1 -= w;
   }

   e_node_t source = NULL;
   if (vcode_count_args(op) > 3)
      source = dst;

   while (count0 > 0) {
      e_node_t n = e_nexus(src, src_i++);
      const unsigned w = e_width(n);
      if (dst_i == dst_nnexus)
         e_add_nexus(dst, n);
      else {
         e_node_t pad = e_nexus(dst, dst_i);
         assert(e_kind(pad) == E_PADDING);
         if (e_width(pad) != w)
            eopt_split_nexus(dst, pad, w);
         e_change_nexus(dst, dst_i++, n);
      }
      e_add_signal(n, dst);
      // TODO: proper handling of undriven port sources here
      (void)source;
      assert(w <= count0);
      count0 -= w;
   }
}

static void eopt_set_nexus_size_cb(e_node_t nexus, void *__ctx)
{
   // It would be nice to assert that e_size(nexus) == 0 here but that
   // runs into problems with arrays of records where the signals are
   // initialised in a loop

   e_set_size(nexus, (uintptr_t)__ctx);
}

static void eopt_init_signal_cb(int op, cprop_state_t *regs, void *__ctx)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   vcode_reg_t arg2 = vcode_get_arg(op, 2);
   vcode_reg_t arg3 = vcode_get_arg(op, 3);

   if (regs[arg2].tag != CP_CONST || regs[arg3].tag != CP_CONST) {
      cprop_dump(op, regs);
      fatal_trace("init signal count or size is not constant");
   }

   const unsigned size = regs[arg3].cval;
   unsigned offset = 0, stride = 0, count = 0;
   e_node_t signal = NULL;

   cprop_get_signal(arg0, arg2, regs, &offset, &stride, &count, &signal);

   if (signal == NULL) {
      cprop_dump(op, regs);
      fatal_trace("init signal target is not a signal");
   }

   if (vcode_count_args(op) > 4)
      e_set_flag(signal, E_F_RESOLVED);

   const unsigned width = e_width(signal);
   assert(offset + count <= width);

   do {
      eopt_split_signal(signal, offset, count, eopt_set_nexus_size_cb,
                        (void *)(uintptr_t)size);
      offset += stride;
   } while (stride > 0 && offset + count <= width);
}

static void eopt_driver_cb(int op, cprop_state_t *regs, void *__ctx)
{
   e_node_t cursor = __ctx;

   vcode_reg_t target = vcode_get_arg(op, 0);
   assert(target != VCODE_INVALID_REG);

   vcode_reg_t count = vcode_get_arg(op, 1);
   assert(count != VCODE_INVALID_REG);

   const bool driven = vcode_get_op(op) != VCODE_OP_SCHED_EVENT;
   eopt_add_driver(op, target, count, regs, cursor, driven);
}

static void eopt_pcall_cb(int op, cprop_state_t *regs, void *__ctx)
{
   const bool is_pcall = (vcode_get_result(op) == VCODE_INVALID_REG);

   const int nargs = vcode_count_args(op);
   for (int i = 0; i < nargs; i++) {
      vcode_reg_t arg = vcode_get_arg(op, i);
      if (regs[arg].tag != CP_UNKNOWN && regs[arg].tag != CP_CONST) {
         unsigned offset = 0, stride = 0, count = 0;
         e_node_t signal = NULL;
         cprop_get_signal(arg, VCODE_INVALID_REG, regs, &offset, &stride,
                          &count, &signal);

         if (signal == NULL)
            continue;

         if (is_pcall) {
            // TODO: should recursively cprop into the procedure call here
            const unsigned width = e_width(signal);
            for (int j = offset; j < width; j++)
               eopt_split_signal(signal, j, 1, NULL, NULL);
         }

         e_set_flag(signal, E_F_LAST_VALUE);
      }
   }
}

static void eopt_last_value_cb(int op, cprop_state_t *regs, void *__ctx)
{
   unsigned offset = 0, stride = 0, count = 0;
   e_node_t signal = NULL;
   cprop_get_signal(vcode_get_arg(op, 0), VCODE_INVALID_REG, regs, &offset,
                    &stride, &count, &signal);

   if (signal != NULL)
      e_set_flag(signal, E_F_LAST_VALUE);
}

static void eopt_drivers(vcode_unit_t unit, e_node_t cursor)
{
   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(unit);

   if (vcode_unit_kind() == VCODE_UNIT_FUNCTION
       && vcode_unit_result() != VCODE_INVALID_TYPE) {
      // Function cannot contain signal assignments
      vcode_state_restore(&state);
      return;
   }

   for (vcode_unit_t child = vcode_unit_child(unit);
        child != NULL; child = vcode_unit_next(child)) {
      eopt_drivers(child, cursor);
   }

   vcode_select_unit(unit);

   cprop_req_t req = {
      .find_signal    = eopt_find_signal_cb,
      .sched_waveform = eopt_driver_cb,
      .sched_event    = eopt_driver_cb,
      .drive_signal   = eopt_driver_cb,
      .pcall          = eopt_pcall_cb,
      .fcall          = eopt_pcall_cb,
      .last_value     = eopt_last_value_cb,
      .vars           = cprop_vars,
      .context        = cursor,
      .flags          = CPROP_BOUNDS
   };
   cprop(&req);

   vcode_state_restore(&state);
}

static void eopt_path_from_cursor(e_node_t e, ident_t name, e_node_t cursor)
{
   ident_t name_lower = ident_downcase(name);

   e_set_path(e, ident_prefix(e_path(cursor), name_lower, ':'));
   e_set_instance(e, ident_prefix(e_instance(cursor), name_lower, ':'));
}

static void eopt_ports(tree_t block, e_node_t cursor)
{
   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(block, i);
      type_t type = tree_type(p);

      e_node_t e = e_new(E_SIGNAL);
      e_set_ident(e, tree_ident(p));
      e_set_width(e, type_width(type));
      e_set_type(e, type);
      eopt_path_from_cursor(e, tree_ident(p), cursor);

      e_add_signal(cursor, e);
   }
}

static ident_t eopt_vcode_unit_name(tree_t t, e_node_t cursor)
{
   if (e_kind(cursor) == E_ROOT)
      return ident_prefix(lib_name(lib_work()), tree_ident(t), '.');
   else
      return ident_prefix(e_vcode(cursor), tree_ident(t), '.');
}

static void eopt_do_cprop(e_node_t e)
{
   vcode_unit_t unit = vcode_find_unit(e_vcode(e));
   if (unit == NULL)
      fatal("cannot find vcode unit %s", istr(e_vcode(e)));

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(unit);

   cprop_req_t req = {
      .find_signal = eopt_find_signal_cb,
      .map_signal  = eopt_map_signal_cb,
      .init_signal = eopt_init_signal_cb,
      .vars        = cprop_vars,
      .context     = e,
      .flags       = CPROP_BOUNDS
   };
   cprop(&req);

   vcode_state_restore(&state);
}

static void eopt_block(tree_t block, e_node_t cursor)
{
   ident_t unit_name = eopt_vcode_unit_name(block, cursor);

   e_node_t e = e_new(E_SCOPE);
   e_set_parent(e, cursor);
   e_set_vcode(e, unit_name);

   tree_t d0 = tree_decl(block, 0);
   assert(tree_kind(d0) == T_HIER);

   e_set_path(e, tree_ident(d0));
   e_set_instance(e, tree_ident2(d0));

   e_add_scope(cursor, e);

   cprop_vars_enter(cprop_vars);

   eopt_ports(block, e);
   eopt_decls(block, e);
   eopt_do_cprop(e);
   eopt_stmts(block, e);

   cprop_vars_leave(cprop_vars);
}

static void eopt_process(tree_t proc, e_node_t cursor)
{
   ident_t name = tree_ident(proc);

   e_node_t e = e_new(E_PROCESS);
   e_set_loc(e, tree_loc(proc));
   e_set_ident(e, name);
   e_set_vcode(e, ident_prefix(e_vcode(cursor), name, '.'));
   e_set_parent(e, cursor);
   eopt_path_from_cursor(e, name, cursor);

   if (tree_flags(proc) & TREE_F_POSTPONED)
      e_set_flag(e, E_F_POSTPONED);

   e_add_proc(cursor, e);

   vcode_unit_t unit = vcode_find_unit(e_vcode(e));
   if (unit == NULL)
      fatal("cannot find vcode unit %s", istr(e_vcode(e)));

   cprop_vars_enter(cprop_vars);
   eopt_drivers(unit, e);
   cprop_vars_leave(cprop_vars);
}

static void eopt_nexus_for_type(e_node_t signal, type_t type, ident_t field)
{
   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(type, i);
         eopt_nexus_for_type(signal, tree_type(f), tree_ident(f));
      }
   }
   else {
      e_node_t n = e_new(E_NEXUS);
      e_set_pos(n, NEXUS_POS_INVALID);
      e_set_ident(n, ident_prefix(e_path(signal), field, '.'));
      e_set_width(n, type_width(type));
      e_add_signal(n, signal);

      e_add_nexus(signal, n);
      e_add_nexus(root, n);
   }
}

static void eopt_signal_decl(tree_t decl, e_node_t cursor)
{
   ident_t name = tree_ident(decl);
   type_t type = tree_type(decl);

   e_node_t e = e_new(E_SIGNAL);
   e_set_ident(e, name);
   e_set_width(e, type_width(type));
   e_set_type(e, type);
   e_set_loc(e, tree_loc(decl));
   eopt_path_from_cursor(e, name, cursor);
   eopt_nexus_for_type(e, type, NULL);

   e_add_signal(cursor, e);
}

static void eopt_decls(tree_t container, e_node_t cursor)
{
   const int ndecls = tree_decls(container);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (tree_kind(d) == T_SIGNAL_DECL)
         eopt_signal_decl(d, cursor);
   }
}

static void eopt_stmts(tree_t container, e_node_t cursor)
{
   const int nstmts = tree_stmts(container);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(container, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         eopt_block(s, cursor);
         break;
      case T_PROCESS:
         eopt_process(s, cursor);
         break;
      default:
         fatal_trace("cannot handle tree kind %s in eopt_stmts",
                     tree_kind_str(tree_kind(s)));
      }
   }
}

static void eopt_package(tree_t pack, e_node_t cursor)
{
   e_node_t e = e_new(E_SCOPE);
   e_set_parent(e, cursor);
   e_set_vcode(e, tree_ident(pack));

   ident_t path = ident_new(package_signal_path_name(tree_ident(pack)));
   e_set_instance(e, path);
   e_set_path(e, path);

   e_add_scope(root, e);

   eopt_decls(pack, e);

   const int nsignals = e_signals(e);
   if (nsignals > 0) {
      // Cannot optimise out LAST_VALUE for package signals
      for (int i = 0; i < nsignals; i++)
         e_set_flag(e_signal(e, i), E_F_LAST_VALUE);

      eopt_do_cprop(e);
   }
}

static void eopt_context(tree_t unit)
{
   const int nctx = tree_contexts(unit);
   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(unit, i);
      if (tree_kind(c) != T_USE)
         continue;

      ident_t name = tree_ident(c);
      bool have = false;
      const int ndeps = e_deps(root);
      for (int i = 0; !have && i < ndeps; i++) {
         if (e_dep(root, i) == name)
            have = true;
      }

      if (have) continue;

      e_add_dep(root, name);

      tree_t dep = lib_get_qualified(name);
      if (dep == NULL)
         fatal_at(tree_loc(c), "cannot find unit %s", istr(name));

      if (tree_kind(dep) == T_PACKAGE)
         eopt_package(dep, root);
   }
}

static void eopt_report_multiple_sources(e_node_t nexus)
{
   e_node_t s0 = e_signal(nexus, 0);

   const int nsignals = e_signals(nexus);
   const int nsources = e_sources(nexus);
   const int width    = e_width(s0);

   bool partof = false;
   int *LOCAL drivenw = xcalloc_array(nsources, sizeof(int));

   for (int i = 0; i < nsources; i++) {
      e_node_t e = e_source(nexus, i);
      assert(e_kind(e) == E_PROCESS);  // TODO: handle port sources

      const int nn = e_nexuses(e);
      for (int j = 0; j < nn; j++) {
         e_node_t n = e_nexus(e, j);
         for (int k = 0; k < e_signals(n); k++) {
            if (e_signal(n, k) == s0)
               drivenw[i] += e_width(n);
         }
      }

      if (drivenw[i] != width)
         partof = true;
   }

   error_at(e_loc(s0), "%sunresolved signal %s with instance name %s has "
            "multiple sources", partof ? "part of " : "",
            istr(e_ident(s0)), istr(e_instance(s0)));

   for (int i = 0; i < nsources; i++) {
      e_node_t e = e_source(nexus, i);
      assert(e_kind(e) == E_PROCESS);

      const char *procname = istr(e_ident(e));
      const bool anonymous = islower(procname[0]);

      note_at(e_loc(e), "%s%s is driven by %s%s in instance %s",
              drivenw[i] != width ? "part of " : "",
              istr(e_ident(s0)), anonymous ? "a process" : "process ",
              anonymous ? "" : procname, istr(e_instance(e_parent(e))));
   }

   // Prevent multiple errors for the same signal
   for (int i = 0; i < nsignals; i++)
      e_set_flag(e_signal(nexus, i), E_F_RESOLVED);
}

static void eopt_post_process_nexus(e_node_t root)
{
   const int nnexus = e_nexuses(root);
   for (int i = 0; i < nnexus; i++) {
      e_node_t n = e_nexus(root, i);
      e_set_pos(n, i);

      if (e_signals(n) == 0) continue;

      e_node_t s0 = e_signal(n, 0);

      if (e_sources(n) > 1) {
         const int nsignals = e_signals(n);
         for (int i = 0; i < nsignals; i++) {
            e_node_t s = e_signal(n, i);
            if (!(e_flags(s) & E_F_RESOLVED)) {
               eopt_report_multiple_sources(n);
               break;
            }
         }
      }

      const int width = e_width(n);
      if (e_width(s0) == width) continue;

      int lsb = 0;
      const int s0_nnexus = e_nexuses(s0);
      for (int j = 0; j < s0_nnexus; j++) {
         e_node_t nj = e_nexus(s0, j);
         if (nj == n) break;
         lsb += e_width(nj);
      }

      const int msb = lsb + width - 1;
      char slice[64];
      if (lsb == msb)
         checked_sprintf(slice, sizeof(slice), "[%d]", lsb);
      else
         checked_sprintf(slice, sizeof(slice), "[%d:%d]", lsb, msb);

      e_set_ident(n, ident_prefix(e_ident(n), ident_new(slice), '\0'));
   }
}

static void eopt_post_process_signal(e_node_t e)
{
   bool contig = true;
   const int nnexus = e_nexuses(e);
   if (nnexus > 1) {
      unsigned last_pos = e_pos(e_nexus(e, 0));
      for (int i = 1; contig && i < nnexus; i++) {
         const unsigned pos = e_pos(e_nexus(e, i));
         if (pos != last_pos + 1)
            contig = false;
         last_pos = pos;
      }
   }

   if (contig) e_set_flag(e, E_F_CONTIGUOUS);
}

static void eopt_post_process_scopes(e_node_t e)
{
   if (e_kind(e) == E_SCOPE) {
      const int nsignals = e_signals(e);
      for (int i = 0; i < nsignals; i++)
         eopt_post_process_signal(e_signal(e, i));
   }

   const int nscopes = e_scopes(e);
   for (int i = 0; i < nscopes; i++)
      eopt_post_process_scopes(e_scope(e, i));
}

e_node_t eopt_build(tree_t elab)
{
   assert(tree_kind(elab) == T_ELAB);
   assert(root == NULL);

   e_node_t e = root = e_new(E_ROOT);
   e_set_ident(e, tree_ident(elab));

   cprop_vars = cprop_vars_new();

   eopt_context(elab);
   eopt_stmts(elab, e);
   eopt_post_process_nexus(e);
   eopt_post_process_scopes(e);

   const char *verbose = getenv("NVC_ELAB_VERBOSE");
   if (verbose && *verbose != '\0')
      e_dump(e);

   cprop_vars_free(cprop_vars);
   cprop_vars = NULL;

   root = NULL;
   return e;
}
