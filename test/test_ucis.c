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

#include "test_util.h"
#include "ucis/ucis-api.h"

static ucisScopeT create_design_unit(ucisT db, const char* duname,
                                     ucisFileHandleT file, int line)
{
   ucisSourceInfoT srcinfo = {
      .filehandle = file,
      .line = line,
      .token = 0
   };
   ucisScopeT du = ucis_CreateScope(db, NULL, duname, &srcinfo, 1,
                                    UCIS_VLOG, UCIS_DU_MODULE,
                                    UCIS_ENABLED_STMT | UCIS_ENABLED_BRANCH |
                                    UCIS_ENABLED_COND | UCIS_ENABLED_EXPR |
                                    UCIS_ENABLED_FSM | UCIS_ENABLED_TOGGLE |
                                    UCIS_INST_ONCE | UCIS_SCOPE_UNDER_DU);
   ucis_SetStringProperty(db, du, -1, UCIS_STR_DU_SIGNATURE,
                          "FAKE DU SIGNATURE");
   return du;
}

static void create_testdata(ucisT db, const char* ucisdb)
{
   ucisTestDataT testdata = {
      .teststatus   = UCIS_TESTSTATUS_OK,
      .simtime      = 0.0,
      .timeunit     = "fs",
      .runcwd       = "./",
      .cputime      = 0.0,
      .seed         = "0",
      .cmd          = "toolname",
      .args         = "command arguments",
      .compulsory   = 0,
      .date         = "20110824143300",
      .username     = "ucis_user",
      .cost         = 0.0,
      .toolcategory = UCIS_SIM_TOOL
   };

   ucisHistoryNodeT testnode = ucis_CreateHistoryNode(
      db, NULL, "TestLogicalName", NULL, UCIS_HISTORYNODE_TEST);

   ucis_SetTestData(db, testnode, &testdata);
}

static ucisScopeT create_instance(ucisT db, const char* instname,
                                  ucisScopeT parent, ucisScopeT duscope)
{
   return ucis_CreateInstance(db, parent, instname, NULL, 1, UCIS_VLOG,
                              UCIS_INSTANCE, duscope, UCIS_INST_ONCE);
}

static void create_statement(ucisT db, ucisScopeT parent,
                             ucisFileHandleT filehandle, int fileno, int line,
                             int item, int count)
{
   ucisCoverDataT coverdata = {
      .type = UCIS_STMTBIN,
      .flags = UCIS_IS_32BIT,
      .data.int32 = count
   };

   ucisSourceInfoT srcinfo = {
      .filehandle = filehandle,
      .line = line,
      .token = 17,
   };

   const int coverindex =
      ucis_CreateNextCover(db, parent, NULL, &coverdata, &srcinfo);
   ucis_SetIntProperty(db, parent, coverindex, UCIS_INT_STMT_INDEX, item);
}

#if 0
static void create_enum_toggle(ucisT db, ucisScopeT parent)
{
   ucisScopeT toggle = ucis_CreateToggle(db, parent, "t", NULL, 0,
                                         UCIS_TOGGLE_METRIC_ENUM,
                                         UCIS_TOGGLE_TYPE_REG,
                                         UCIS_TOGGLE_DIR_INTERNAL);

   ucisCoverDataT coverdata = {
      .type = UCIS_TOGGLEBIN,
      .flags = UCIS_IS_32BIT,
      .data.int32 = 0
   };
   ucis_CreateNextCover(db, toggle, "a", &coverdata, NULL);

   coverdata.data.int32 = 1;
   ucis_CreateNextCover(db, toggle, "b", &coverdata, NULL);
}
#endif

static void create_branch(ucisT db, ucisScopeT parent, int num)
{
   ucisScopeT branch = ucis_CreateScope(db, parent, "b", NULL, 1,
                                        UCIS_SV, UCIS_BRANCH, 0);
   ucis_SetIntProperty(db, branch, -1, UCIS_INT_BRANCH_HAS_ELSE, 1);

   for (int i = 0; i < num; i++) {
      ucisCoverDataT coverdata = {
         .type = UCIS_BRANCHBIN,
         .flags = UCIS_IS_32BIT,
         .data = { .int32 = num }
      };

      if (i == num - 1)
         coverdata.flags |= UCIS_IS_BR_ELSE;

      ucis_CreateNextCover(db, branch, NULL, &coverdata, NULL);
   }
}

static ucisT get_example_data(void)
{
    ucisT db = ucis_Open(NULL);
    create_testdata(db, NULL);

    ucisFileHandleT filehandle = ucis_CreateFileHandle(db, "test.sv", "./");
    ucisScopeT du = create_design_unit(db, "work.top", filehandle, 0);
    ucisScopeT instance = create_instance(db, "top", NULL, du);
    create_statement(db, instance, filehandle, 1, 6, 1, 17);
    create_statement(db, instance, filehandle, 1, 8, 1, 0);
    create_statement(db, instance, filehandle, 1, 9, 2, 365);
    create_branch(db, instance, 2);
    //create_enum_toggle(db,instance);

    return db;
}

START_TEST(test_iterate1)
{
   ucisT db = get_example_data();
   ck_assert_ptr_nonnull(db);

   ucisIteratorT it1 = ucis_ScopeIterate(db, NULL, UCIS_OBJ_ANY);
   ck_assert_ptr_nonnull(it1);

   ucisScopeT s1 = ucis_ScopeScan(db, it1);
   ck_assert_ptr_nonnull(s1);
   ck_assert_int_eq(ucis_ObjKind(db, s1), UCIS_OBJ_SCOPE);
   ck_assert_int_eq(ucis_GetScopeType(db, s1), UCIS_DU_MODULE);
   ck_assert_str_eq(
      ucis_GetStringProperty(db, s1, -1, UCIS_STR_SCOPE_NAME),
      "work.top");
   ck_assert_str_eq(
      ucis_GetStringProperty(db, s1, -1, UCIS_STR_SCOPE_HIER_NAME),
      "work.top");
   ck_assert_str_eq(
      ucis_GetStringProperty(db, s1, -1, UCIS_STR_DU_SIGNATURE),
      "FAKE DU SIGNATURE");
   ck_assert_ptr_eq(
      ucis_GetHandleProperty(db, s1, UCIS_HANDLE_SCOPE_TOP),
      s1);

   ucisScopeT s2 = ucis_ScopeScan(db, it1);
   ck_assert_ptr_nonnull(s2);
   ck_assert_int_eq(ucis_GetScopeType(db, s2), UCIS_INSTANCE);
   ck_assert_str_eq(
      ucis_GetStringProperty(db, s2, -1, UCIS_STR_SCOPE_NAME),
      "top");
   ck_assert_str_eq(
      ucis_GetStringProperty(db, s2, -1, UCIS_STR_SCOPE_HIER_NAME),
      "/top");
   ck_assert_ptr_eq(
      ucis_GetHandleProperty(db, s2, UCIS_HANDLE_INSTANCE_DU),
      s1);
   ck_assert_ptr_eq(
      ucis_GetHandleProperty(db, s2, UCIS_HANDLE_SCOPE_PARENT),
      NULL);
   ck_assert_ptr_eq(
      ucis_GetHandleProperty(db, s2, UCIS_HANDLE_SCOPE_TOP),
      s2);
   ck_assert_ptr_null(
      ucis_GetStringProperty(db, s2, -1, UCIS_STR_DU_SIGNATURE));

   ck_assert_ptr_null(ucis_ScopeScan(db, it1));

   ucis_FreeIterator(db, it1);

   ucisIteratorT it2 = ucis_CoverIterate(db, s2, UCIS_ALL_BINS);
   ck_assert_ptr_nonnull(it2);

   const int bin1 = ucis_CoverScan(db, it2);
   ck_assert_int_ne(bin1, -1);

   char* name;
   ucisCoverDataT coverdata;
   ucis_GetCoverData(db, s2, bin1, &name, &coverdata, NULL);

   ck_assert_str_eq(name, "#stmt#6#1#");
   ck_assert_int_eq(ucis_GetIntProperty(db, s2, bin1, UCIS_INT_STMT_INDEX), 1);
   ck_assert_int_eq(coverdata.data.int32, 17);

   ucis_FreeIterator(db, it2);

   ucisIteratorT it3 = ucis_CoverIterate(db, NULL, UCIS_ALL_BINS);
   ck_assert_ptr_nonnull(it3);
   ck_assert_int_eq(ucis_CoverScan(db, it3), -1);
   ucis_FreeIterator(db, it3);

   ucis_Close(db);
}
END_TEST

START_TEST(test_filehandle)
{
   ucisT db = ucis_Open(NULL);
   ucisFileHandleT fh = ucis_CreateFileHandle(db, "file", "/dir");
   ck_assert_ptr_nonnull(fh);

   // ModelSim seems to ignore the directory argument in all cases
   ck_assert_str_eq(ucis_GetFileName(db, fh), "file");

   ucis_Close(db);
}

static ucisScopeT scope_by_name(ucisT db, ucisScopeT root, const char *name)
{
   ucisIteratorT it = ucis_ScopeIterate(db, root, UCIS_ALL_SCOPES);

   ucisScopeT s;
   while ((s = ucis_ScopeScan(db, it))) {
      const char *this =
         ucis_GetStringProperty(db, s, -1, UCIS_STR_SCOPE_HIER_NAME);
      if (strcmp(this, name) == 0)
         break;
      else if (strncmp(this, name, strlen(this)) == 0
               && (s = scope_by_name(db, s, name)))
         break;
   }

   ucis_FreeIterator(db, it);
   return s;
}

START_TEST(test_binname1)
{
   ucisT db = get_example_data();
   ck_assert_ptr_nonnull(db);

   ucisScopeT top = scope_by_name(db, NULL, "/top");
   ck_assert_ptr_nonnull(top);

   char* name;
   ucisCoverDataT coverdata;
   ck_assert_int_eq(ucis_GetCoverData(db, top, 2, &name, &coverdata, NULL), 0);

   ck_assert_str_eq(name, "#stmt#9#2#");
   ck_assert_int_eq(ucis_GetIntProperty(db, top, 2, UCIS_INT_STMT_INDEX), 2);
   ck_assert_int_eq(coverdata.data.int32, 365);

   ucisScopeT b = scope_by_name(db, NULL, "/top/b");
   ck_assert_ptr_nonnull(b);

   ck_assert_int_eq(ucis_GetCoverData(db, b, 0, &name, &coverdata, NULL), 0);
#ifndef HAVE_UCIS_H
   ck_assert_str_eq(name, "#true#0#1#");
#endif

   ucis_GetCoverData(db, b, 1, &name, &coverdata, NULL);
   ck_assert_int_eq(ucis_GetCoverData(db, b, 1, &name, &coverdata, NULL), 0);
#ifndef HAVE_UCIS_H
   ck_assert_str_eq(name, "#default#0#1#");
#endif

   ucis_Close(db);
}
END_TEST

Suite *get_ucis_tests(void)
{
   Suite *s = suite_create("ucis");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_iterate1);
   tcase_add_test(tc, test_filehandle);
   tcase_add_test(tc, test_binname1);
   suite_add_tcase(s, tc);

   return s;
}
