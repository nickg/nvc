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
#include "array.h"
#include "hash.h"
#include "ucis/ucis-api.h"
#include "ucis/ucis-structs.h"
#include "ucis/ucis-util.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define MISSING fatal_trace("not implemented");

ucisT ucis_Open(const char *name)
{
   ucisT db = xcalloc(sizeof(struct _ucis));
   db->files     = shash_new(16);
   db->strhash   = shash_new(128);
   db->strtab    = tb_new();
   db->strprop   = tb_new();
   db->binstr    = tb_new();
   db->separator = '/';

   if (name == NULL)
      tb_append(db->strtab, '\0');    // Null string is index zero
   else if (ucis_populate(db, name) != 0) {
      ucis_Close(db);
      return NULL;
   }

   return db;
}

int ucis_Close(ucisT db)
{
   tb_free(db->strtab);
   tb_free(db->strprop);
   tb_free(db->binstr);
   shash_free(db->files);
   shash_free(db->strhash);
   free(db);
   return 0;
}

int ucis_GetIntProperty(ucisT db, ucisObjT obj, int coverindex,
                        ucisIntPropertyEnumT property)
{
   switch (property) {
   case UCIS_INT_STMT_INDEX:
      {
         ucisCoverItemT *item = ucis_get_cover_item(obj, coverindex);
         if (item == NULL)
            return -1;

         return item->stmt_index;
      }
   case UCIS_INT_BRANCH_HAS_ELSE:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return -1;

         return !!(s->flags & UCIS_SCOPE_HAS_ELSE);
      }
   default:
      ucis_error("unhandled ucisIntPropertyEnumT %d", property);
      return -1;
   }
}

int ucis_SetIntProperty(ucisT db, ucisObjT obj, int coverindex,
                        ucisIntPropertyEnumT property, int value)
{
   switch (property) {
   case UCIS_INT_STMT_INDEX:
      {
         ucisCoverItemT *item = ucis_get_cover_item(obj, coverindex);
         if (item == NULL)
            return -1;

         assert(item->data.type == UCIS_STMTBIN);
         item->stmt_index = value;
         return 0;
      }
   case UCIS_INT_BRANCH_HAS_ELSE:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return -1;

         s->flags &= ~UCIS_SCOPE_HAS_ELSE;
         if (value) s->flags |= UCIS_SCOPE_HAS_ELSE;
         return 0;
      }
   default:
      ucis_error("unhandled ucisIntPropertyEnumT %d", property);
      return -1;
   }
}

ucisScopeT ucis_CreateScope(ucisT db, ucisScopeT parent, const char *name,
                            ucisSourceInfoT *srcinfo, int weight,
                            ucisSourceT source, ucisScopeTypeT type,
                            ucisFlagsT flags)
{
   ucisScopeT scope =
      ucis_new_object(UCIS_OBJ_SCOPE, sizeof(struct ucisScopeS));
   scope->maxitems = EMBED_COVER_ITEMS;
   scope->name     = ucis_new_string(db, name);
   scope->source   = source;
   scope->type     = type;
   scope->flags    = flags;
   scope->parent   = parent;

   if (srcinfo != NULL)
      scope->srcinfo = *srcinfo;

   ucisScopeT *p = parent ? &(parent->children) : &(db->root_scopes);
   for (; *p != NULL; p = &((*p)->next));
   *p = scope;

   return scope;
}

ucisScopeT ucis_CreateInstance(ucisT db, ucisScopeT parent, const char *name,
                               ucisSourceInfoT *fileinfo, int weight,
                               ucisSourceT source, ucisScopeTypeT type,
                               ucisScopeT du_scope, ucisFlagsT flags)
{
   ucisScopeT scope =
      ucis_new_object(UCIS_OBJ_SCOPE, sizeof(struct ucisScopeS));
   scope->maxitems = EMBED_COVER_ITEMS;
   scope->name     = ucis_new_string(db, name);
   scope->source   = source;
   scope->type     = type;
   scope->flags    = flags;
   scope->du_scope = du_scope;
   scope->parent   = parent;

   if (fileinfo != NULL)
      scope->srcinfo = *fileinfo;

   ucisScopeT *p = parent ? &(parent->children) : &(db->root_scopes);
   for (; *p != NULL; p = &((*p)->next));
   *p = scope;

   return scope;
}

ucisScopeT ucis_CreateToggle(ucisT db, ucisScopeT parent, const char *name,
                             const char *canonical_name, ucisFlagsT flags,
                             ucisToggleMetricT toggle_metric,
                             ucisToggleTypeT toggle_type,
                             ucisToggleDirT toggle_dir)
{
   MISSING;
}

ucisScopeTypeT ucis_GetScopeType(ucisT db, ucisScopeT scope)
{
   return scope->type;
}

int ucis_SetTestData(ucisT db, ucisHistoryNodeT testhistorynode,
                     ucisTestDataT *testdata)
{
   testhistorynode->teststatus   = testdata->teststatus;
   testhistorynode->simtime      = testdata->simtime;
   testhistorynode->timeunit     = ucis_new_string(db, testdata->timeunit);
   testhistorynode->runcwd       = ucis_new_string(db, testdata->runcwd);
   testhistorynode->cputime      = testdata->cputime;
   testhistorynode->seed         = ucis_new_string(db, testdata->seed);
   testhistorynode->cmd          = ucis_new_string(db, testdata->cmd);
   testhistorynode->args         = ucis_new_string(db, testdata->args);
   testhistorynode->compulsory   = testdata->compulsory;
   testhistorynode->date         = ucis_new_string(db, testdata->date);
   testhistorynode->username     = ucis_new_string(db, testdata->username);
   testhistorynode->cost         = testdata->cost;
   testhistorynode->toolcategory = ucis_new_string(db, testdata->toolcategory);

   return 0;
}

ucisHistoryNodeT ucis_CreateHistoryNode(ucisT db, ucisHistoryNodeT parent,
                                        char *logicalname, char *physicalname,
                                        ucisHistoryNodeKindT kind)
{
   ucisHistoryNodeT h = xcalloc(sizeof(struct _ucisHistoryNode));
   h->kind = kind;

   ucisHistoryNodeT *p = &(db->history_nodes);
   for (; *p; p = &((*p)->next));
   *p = h;

   return h;
}

const char *ucis_GetFileName(ucisT db, ucisFileHandleT filehandle)
{
   return ucis_get_string(db, filehandle->filename);
}

ucisFileHandleT ucis_CreateFileHandle(ucisT db, const char* filename,
                                      const char* fileworkdir)
{
   ucisFileHandleT exist = shash_get(db->files, filename);
   if (exist != NULL)
      return exist;

   ucisFileHandleT new = xcalloc(sizeof(struct _ucisFileHandle));
   new->filename = ucis_new_string(db, filename);

   shash_put(db->files, filename, new);
   return new;
}

int ucis_CreateNextCover(ucisT db, ucisScopeT parent, const char *name,
                         ucisCoverDataT *data, ucisSourceInfoT *sourceinfo)
{
   if (parent->numitems == parent->maxitems) {
      parent->maxitems *= 2;

      if (parent->maxitems == EMBED_COVER_ITEMS*2) {
         ucisCoverItemT *list =
            xmalloc_array(parent->maxitems, sizeof(ucisCoverItemT));
         for (int i = 0; i < EMBED_COVER_ITEMS; i++)
            list[i] = parent->embed[i];

         parent->extended = list;
      }
      else
         parent->extended = xrealloc_array(parent->extended, parent->maxitems,
                                           sizeof(ucisCoverItemT));
   }

   const int index = parent->numitems++;
   ucisCoverItemT *item = ucis_get_cover_item(parent, index);
   item->stmt_index = 1;
   item->name = ucis_new_string(db, name);

   if (data != NULL)
      item->data = *data;

   if (sourceinfo != NULL)
      item->sourceinfo = *sourceinfo;

   return index;
}

int ucis_GetCoverData(ucisT db, ucisScopeT parent, int coverindex, char **name,
                      ucisCoverDataT *data, ucisSourceInfoT *sourceinfo)
{
   ucisCoverItemT *ci = ucis_get_cover_item(parent, coverindex);
   if (ci == NULL)
      return -1;

   if (data != NULL)
      *data = ci->data;

   if (sourceinfo != NULL)
      *sourceinfo = ci->sourceinfo;

   if (name != NULL && ci->name != 0)
      *name = (char *)ucis_get_string(db, ci->name);
   else if (name != NULL) {
      tb_rewind(db->binstr);

      switch (ci->data.type) {
      case UCIS_STMTBIN:
         tb_cat(db->binstr, "#stmt");
         break;
      case UCIS_BRANCHBIN:
         if (coverindex == parent->numitems - 1) {
            if (parent->flags & UCIS_SCOPE_HAS_ELSE)
               tb_cat(db->binstr, "#default");
            else
               tb_cat(db->binstr, "all_false_branch");
         }
         else
            tb_cat(db->binstr, "#true");
         break;
      default:
         ucis_error("cannot generate bin name for %x", ci->data.type);
         return -1;
      }

      if (tb_get(db->binstr)[0] == '#')
         tb_printf(db->binstr, "#%d#%d#", ci->sourceinfo.line, ci->stmt_index);

      *name = (char *)tb_get(db->binstr);
   }

   return 0;
}

static void ucis_get_hier_name(ucisT db, ucisScopeT scope, text_buf_t *tb)
{
   if (scope->parent != NULL)
      ucis_get_hier_name(db, scope->parent, tb);

   tb_append(tb, db->separator);
   tb_cat(tb, ucis_get_string(db, scope->name));
}

const char *ucis_GetStringProperty(ucisT db, ucisObjT obj, int coverindex,
                                   ucisStringPropertyEnumT property)
{
   switch (property) {
   case UCIS_STR_SCOPE_HIER_NAME:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return NULL;
         else if (ucis_is_design_unit(s))
            return ucis_get_string(db, s->name);
         else {
            tb_rewind(db->strprop);
            ucis_get_hier_name(db, s, db->strprop);
            return tb_get(db->strprop);
         }
      }
   case UCIS_STR_DU_SIGNATURE:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return NULL;
         else if (ucis_is_design_unit(s))
            return ucis_get_string(db, s->du_signature);
         else {
            ucis_error("not a design unit scope");
            return NULL;
         }
      }
   case UCIS_STR_SCOPE_NAME:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return NULL;
         else if (s->name != 0)
            return ucis_get_string(db, s->name);
         else {
            tb_rewind(db->strprop);
            return "XXXX";
         }
      }
   default:
      ucis_error("string property %d not supported", property);
      return NULL;
   }
}

int ucis_SetStringProperty(ucisT db, ucisObjT obj, int coverindex,
                           ucisStringPropertyEnumT property, const char *value)
{
   switch (property) {
   case UCIS_STR_DU_SIGNATURE:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return -1;
         else if (ucis_is_design_unit(s)) {
            s->du_signature = ucis_new_string(db, value);
            return 0;
         }
         else {
            ucis_error("not a design unit scope");
            return -1;
         }
      }
   default:
      ucis_error("string property %d not supported", property);
      return -1;
   }
}

ucisObjT ucis_GetHandleProperty(ucisT db, ucisObjT obj,
                                ucisHandleEnumT property)
{
   switch (property) {
   case UCIS_HANDLE_INSTANCE_DU:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return NULL;
         else if (s->type == UCIS_INSTANCE)
            return s->du_scope;
         else {
            ucis_error("not an instance scope");
            return NULL;
         }
      }
   case UCIS_HANDLE_SCOPE_PARENT:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return NULL;
         else
            return s->parent;
      }
   case UCIS_HANDLE_SCOPE_TOP:
      {
         ucisScopeT s = ucis_cast_scope(obj);
         if (s == NULL)
            return NULL;
         else {
            for (; s->parent != NULL; s = s->parent);
            return s;
         }
      }
   default:
      ucis_error("handle property %d not supported", property);
      return NULL;
   }
}

int ucis_SetHandleProperty(ucisT db, ucisObjT obj, ucisHandleEnumT property,
                           ucisObjT value)
{
   MISSING;
}

ucisObjMaskT ucis_ObjKind(ucisT db, ucisObjT obj)
{
   ucisObjHeaderT *header = obj;
   switch (header->kind) {
   case UCIS_OBJ_HISTORYNODE:
   case UCIS_OBJ_SCOPE:
   case UCIS_OBJ_COVERITEM:
      return header->kind;
   default:
      ucis_error("invalid UCIS object %p", obj);
      return UCIS_OBJ_ERROR;
   }
}

ucisIteratorT ucis_ScopeIterate(ucisT db, ucisScopeT scope,
                                ucisScopeMaskTypeT scopemask)
{
   ucisIteratorT it = xcalloc(sizeof(struct ucisIteratorS));
   it->scopemask = scopemask;

   if (scope == NULL)
      it->curscope = db->root_scopes;
   else
      it->curscope = scope->children;

   return it;
}

ucisScopeT ucis_ScopeScan(ucisT db, ucisIteratorT iterator)
{
   if (iterator->curscope == NULL)
      return NULL;

   ucisScopeT s = iterator->curscope;
   iterator->curscope = iterator->curscope->next;
   return s;
}

ucisIteratorT ucis_CoverIterate(ucisT db, ucisScopeT scope,
                                ucisCoverMaskTypeT covermask)
{
   ucisIteratorT it = xcalloc(sizeof(struct ucisIteratorS));
   it->covermask = covermask;
   it->curscope  = scope;
   it->curcover  = 0;

   return it;
}

int ucis_CoverScan(ucisT db, ucisIteratorT iterator)
{
   if (iterator->curscope == NULL)
      return -1;
   else if (iterator->curcover >= iterator->curscope->numitems)
      return -1;
   else
      return iterator->curcover++;
}

void ucis_FreeIterator(ucisT db, ucisIteratorT iterator)
{
   free(iterator);
}

static ucisCBReturnT ucis_do_callback(ucisT db, ucisCBReasonT reason,
                                      ucisObjT obj, int coverindex,
                                      ucis_CBFuncT cbfunc, void *userdata)
{
   ucisCBDataT cbdata = {
      .coverindex = coverindex,
      .db         = db,
      .obj        = obj,
      .reason     = reason,
   };

   ucisCBReturnT ret = (*cbfunc)(userdata, &cbdata);

   switch (ret) {
   case UCIS_SCAN_STOP:
   case UCIS_SCAN_PRUNE:
   case UCIS_SCAN_CONTINUE:
      return ret;
   default:
      ucis_error("invalid return value %d from callback", ret);
      return UCIS_SCAN_STOP;
   }
}

static bool ucis_walk_scopes(ucisT db, ucisScopeT scope, ucis_CBFuncT cbfunc,
                             void *userdata)
{
   ucisCBReturnT ret = ucis_do_callback(db, UCIS_REASON_SCOPE, scope, -1,
                                        cbfunc, userdata);

   if (ret == UCIS_SCAN_STOP)
      return false;
   else if (ret == UCIS_SCAN_CONTINUE) {
      for (int i = 0; i < scope->numitems; i++) {
         if (ucis_do_callback(db, UCIS_REASON_CVBIN, scope, i,
                              cbfunc, userdata) == UCIS_SCAN_STOP)
            return false;
      }

      for (ucisScopeT s = scope->children; s != NULL; s = s->next) {
         if (!ucis_walk_scopes(db, s, cbfunc, userdata))
            return false;
      }
   }

   return ucis_do_callback(db, UCIS_REASON_ENDSCOPE, scope, -1,
                           cbfunc, userdata) != UCIS_SCAN_STOP;
}

int ucis_CallBack(ucisT db, ucisScopeT start, ucis_CBFuncT cbfunc,
                  void* userdata)
{
   ucisCBReturnT ret = ucis_do_callback(db, UCIS_REASON_INITDB, NULL, -1,
                                        cbfunc, userdata);
   if (ret == UCIS_SCAN_STOP)
      return 0;
   else if (ret == UCIS_SCAN_CONTINUE) {
      if (start == NULL) {
         for (ucisScopeT s = db->root_scopes; s != NULL; s = s->next) {
            if (!ucis_walk_scopes(db, s, cbfunc, userdata))
               return 0;
         }
      }
      else if (!ucis_walk_scopes(db, start, cbfunc, userdata))
         return 0;
   }

   ucis_do_callback(db, UCIS_REASON_ENDDB, NULL, -1, cbfunc, userdata);
   return 0;
}

int ucis_SetPathSeparator(ucisT db, char separator)
{
   db->separator = separator;
   return 0;
}

char ucis_GetPathSeparator(ucisT db)
{
   return db->separator;
}
