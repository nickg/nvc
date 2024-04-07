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

#ifndef _UCIS_STRUCTS_H
#define _UCIS_STRUCTS_H

#include "prim.h"
#include "ucis/ucis-api.h"

#define EMBED_COVER_ITEMS 4

// Internal values for UCIS_SCOPE_INTERNAL
#define UCIS_SCOPE_HAS_ELSE 0x10000000

typedef uint32_t ucisStringT;

struct _ucisFileHandle {
   ucisStringT filename;
};

typedef struct ucisCoverItemS {
   ucisCoverDataT  data;
   ucisSourceInfoT sourceinfo;
   ucisStringT     name;
   unsigned        stmt_index;
} ucisCoverItemT;

typedef struct {
   ucisObjMaskT kind;
} ucisObjHeaderT;

struct ucisScopeS {
   ucisObjHeaderT  header;
   ucisScopeT      next;
   ucisScopeT      children;
   ucisScopeT      parent;
   ucisStringT     name;
   ucisSourceInfoT srcinfo;
   ucisSourceT     source;
   ucisScopeTypeT  type;
   ucisFlagsT      flags;
   ucisScopeT      du_scope;
   ucisStringT     du_signature;
   unsigned        numitems;
   unsigned        maxitems;
   union {
      ucisCoverItemT  embed[EMBED_COVER_ITEMS];
      ucisCoverItemT *extended;
   };
};

struct _ucisHistoryNode {
   ucisHistoryNodeT     next;
   ucisHistoryNodeKindT kind;
   ucisTestStatusT      teststatus;
   double               simtime;
   ucisStringT          timeunit;
   ucisStringT          runcwd;
   double               cputime;
   ucisStringT          seed;
   ucisStringT          cmd;
   ucisStringT          args;
   int                  compulsory;
   ucisStringT          date;
   ucisStringT          username;
   double               cost;
   ucisStringT          toolcategory;
};

struct _ucis {
   ucisHistoryNodeT  history_nodes;
   ucisScopeT        root_scopes;
   shash_t          *files;
   shash_t          *strhash;
   text_buf_t       *strtab;
   text_buf_t       *strprop;
   text_buf_t       *binstr;
   char              separator;
};

typedef struct ucisIteratorS {
   ucisScopeT         curscope;
   ucisScopeMaskTypeT scopemask;
   ucisCoverMaskTypeT covermask;
   unsigned           curcover;
} *ucisIteratorT;

#endif   // _UCIS_STRUCTS_H
