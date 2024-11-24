//
//  Copyright (C) 2024  Nick Gasson
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
#include "vpi/vpi_user.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

static void visit_object(vpiHandle obj, int indent);

const char *get_type_str(PLI_INT32 type)
{
   switch (type) {
   case vpiSysFuncCall: return "vpiSysFuncCall";
   case vpiSysTaskCall: return "vpiSysTaskCall";
   case vpiIterator: return "vpiIterator";
   case vpiCallback: return "vpiCallback";
   case vpiConstant: return "vpiConstant";
   case vpiOperation: return "vpiOperation";
   case vpiModule: return "vpiModule";
   case vpiScope: return "vpiScope";
   case vpiPort: return "vpiPort";
   case vpiNet: return "vpiNet";
   case vpiReg: return "vpiReg";
   default:
      {
         static char buf[16];
         snprintf(buf, sizeof(buf), "%d", type);
         return buf;
      }
   }
}

static int get_uid(vpiHandle obj)
{
   static struct {
      vpiHandle handle;
      int       uid;
   } tab[1024] = {};
   static int next_uid = 0;

   for (int pos = mix_bits_64(obj) % ARRAY_LEN(tab), reprobe = 100;;
        pos = (pos + 1) % ARRAY_LEN(tab), reprobe--) {
      if (tab[pos].handle == obj)
         return tab[pos].uid;
      else if (tab[pos].handle == NULL) {
         tab[pos].handle = obj;
         return (tab[pos].uid = next_uid++);
      }
      else if (reprobe == 0) {
         fprintf(stderr, "too many objects\n");
         exit(1);
      }
   }
}

static bool visit_children(vpiHandle obj, int indent)
{
   vpiHandle it = vpi_iterate(vpiScope, obj);
   if (it == NULL)
      return false;

   vpiHandle child = vpi_scan(it);
   if (child == NULL)
      return false;

   do {
      visit_object(child, indent + 2);
   } while ((child = vpi_scan(it)));

   return true;
}

static bool visit_module(vpiHandle obj, int indent)
{
   printf(" name=\"%s\">\n", vpi_get_str(vpiName, obj));

   return visit_children(obj, indent);
}

static bool visit_net_reg(vpiHandle obj, int indent)
{
   printf(" name=\"%s\"", vpi_get_str(vpiName, obj));
   return false;
}

static bool visit_port(vpiHandle obj, int indent)
{
   printf(" name=\"%s\"", vpi_get_str(vpiName, obj));
   return false;
}

static void visit_object(vpiHandle obj, int indent)
{
   PLI_INT32 type = vpi_get(vpiType, obj);
   printf("%*.s<%s uid=\"%d\"", indent, "", get_type_str(type), get_uid(obj));

   bool children = false;
   switch (vpi_get(vpiType, obj)) {
   case vpiModule:
      children = visit_module(obj, indent);
      break;
   case vpiReg:
   case vpiNet:
      children = visit_net_reg(obj, indent);
      break;
   case vpiPort:
      children = visit_port(obj, indent);
      break;
   }

   if (children)
      printf("%*.s</%s>\n", indent, "", get_type_str(type));
   else
      printf("/>\n");
}

static PLI_INT32 start_of_sim(p_cb_data cb)
{
   vpiHandle it = vpi_iterate(vpiModule, NULL), top;
   while ((top = vpi_scan(it)))
      visit_object(top, 0);

   return 0;
}

static void startup(void)
{
   s_cb_data cb = {
      .reason = cbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };

   vpi_register_cb(&cb);
}

void (*vlog_startup_routines[])(void) = {
   startup,
   0
};
