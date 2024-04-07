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

#ifndef _UCIS_API_H
#define _UCIS_API_H

#if HAVE_UCIS_H
#include <ucis.h>    // System-supplied UCIS library
#else

#include "prim.h"

//
// The text of the UCIS standard can be downloaded here:
//
//    https://www.accellera.org/downloads/standards/ucis
//
// The standard contains a header file which unfortunately cannot be
// distributed due to its proprietary Accellera license.  The
// declarations below are taken from the standard text but do not
// contain the explanatory comments found in the standard header.
//

typedef uint64_t ucisObjTypeT;
typedef ucisObjTypeT ucisScopeTypeT;
typedef ucisObjTypeT ucisCoverTypeT;
typedef uint64_t ucisCoverMaskTypeT;
typedef uint64_t ucisScopeMaskTypeT;

typedef struct _ucis *ucisT;
typedef struct ucisScopeS *ucisScopeT;
typedef void *ucisObjT;
typedef struct _ucisFileHandle *ucisFileHandleT;
typedef struct ucisIteratorS *ucisIteratorT;
typedef struct _ucisHistoryNode *ucisHistoryNodeT;

////////////////////////////////////////////////////////////////////////////////
// Source information

typedef struct {
   ucisFileHandleT filehandle;
   int             line;
   int             token;
} ucisSourceInfoT;

const char *ucis_GetFileName(ucisT db, ucisFileHandleT filehandle);
ucisFileHandleT ucis_CreateFileHandle(ucisT db, const char* filename,
                                      const char* fileworkdir);

////////////////////////////////////////////////////////////////////////////////
// Error handling

typedef enum {
   UCIS_MSG_INFO,
   UCIS_MSG_WARNING,
   UCIS_MSG_ERROR
} ucisMsgSeverityT;

typedef struct ucisErr_s {
   int msgno;
   ucisMsgSeverityT severity;
   const char* msgstr;
} ucisErrorT;

typedef void (*ucis_ErrorHandler)(void* userdata, ucisErrorT* errdata);

void ucis_RegisterErrorHandler(ucis_ErrorHandler errHandle, void *userdata);

////////////////////////////////////////////////////////////////////////////////
// Database creation

typedef enum {
   UCIS_REASON_INITDB,
   UCIS_REASON_DU,
   UCIS_REASON_TEST,
   UCIS_REASON_SCOPE,
   UCIS_REASON_CVBIN,
   UCIS_REASON_ENDSCOPE,
   UCIS_REASON_ENDDB,
   UCIS_REASON_MERGEHISTORY
} ucisCBReasonT;

typedef enum {
   UCIS_SCAN_CONTINUE = -1,
   UCIS_SCAN_STOP = -2,
   UCIS_SCAN_PRUNE = -3
} ucisCBReturnT;

typedef struct ucisCBDataS {
   ucisCBReasonT reason;
   ucisT         db;
   ucisObjT      obj;
   int           coverindex;
} ucisCBDataT;

typedef ucisCBReturnT (*ucis_CBFuncT) (void* userdata, ucisCBDataT* cbdata);

ucisT ucis_Open(const char *name);
int ucis_Write(ucisT db, const char* file, ucisScopeT scope, int recurse,
               int covertype);
int ucis_WriteToInterchangeFormat(ucisT db, const char* file, ucisScopeT scope,
                                  int recurse, int covertype);
int ucis_SetPathSeparator(ucisT db, char separator);
char ucis_GetPathSeparator(ucisT db);
int ucis_Close(ucisT db);

////////////////////////////////////////////////////////////////////////////////
// Integer properties

typedef enum {
   UCIS_INT_IS_MODIFIED,
   UCIS_INT_MODIFIED_SINCE_SIM,
   UCIS_INT_NUM_TESTS,
   UCIS_INT_SCOPE_WEIGHT,
   UCIS_INT_SCOPE_GOAL,
   UCIS_INT_SCOPE_SOURCE_TYPE,
   UCIS_INT_NUM_CROSSED_CVPS,
   UCIS_INT_SCOPE_IS_UNDER_DU,
   UCIS_INT_SCOPE_IS_UNDER_COVERINSTANCE,
   UCIS_INT_SCOPE_NUM_COVERITEMS,
   UCIS_INT_SCOPE_NUM_EXPR_TERMS,
   UCIS_INT_TOGGLE_TYPE,
   UCIS_INT_TOGGLE_DIR,
   UCIS_INT_TOGGLE_COVERED,
   UCIS_INT_BRANCH_HAS_ELSE,
   UCIS_INT_BRANCH_ISCASE,
   UCIS_INT_COVER_GOAL,
   UCIS_INT_COVER_LIMIT,
   UCIS_INT_COVER_WEIGHT,
   UCIS_INT_TEST_STATUS,
   UCIS_INT_TEST_COMPULSORY,
   UCIS_INT_STMT_INDEX,
   UCIS_INT_BRANCH_COUNT,
   UCIS_INT_FSM_STATEVAL,
   UCIS_INT_CVG_ATLEAST,
   UCIS_INT_CVG_AUTOBINMAX,
   UCIS_INT_CVG_DETECTOVERLAP,
   UCIS_INT_CVG_NUMPRINTMISSING,
   UCIS_INT_CVG_STROBE,
   UCIS_INT_CVG_PERINSTANCE,
   UCIS_INT_CVG_GETINSTCOV,
   UCIS_INT_CVG_MERGEINSTANCES,
   UCIS_INT_TOGGLE_METRIC
} ucisIntPropertyEnumT;

int ucis_GetIntProperty(ucisT db, ucisObjT obj, int coverindex,
                        ucisIntPropertyEnumT property);
int ucis_SetIntProperty(ucisT db, ucisObjT obj, int coverindex,
                        ucisIntPropertyEnumT property, int value);

////////////////////////////////////////////////////////////////////////////////
// String properties

typedef enum {
   UCIS_STR_FILE_NAME,
   UCIS_STR_SCOPE_NAME,
   UCIS_STR_SCOPE_HIER_NAME,
   UCIS_STR_INSTANCE_DU_NAME,
   UCIS_STR_UNIQUE_ID,
   UCIS_STR_VER_STANDARD,
   UCIS_STR_VER_STANDARD_VERSION,
   UCIS_STR_VER_VENDOR_ID,
   UCIS_STR_VER_VENDOR_TOOL,
   UCIS_STR_VER_VENDOR_VERSION,
   UCIS_STR_GENERIC,
   UCIS_STR_ITH_CROSSED_CVP_NAME,
   UCIS_STR_HIST_CMDLINE,
   UCIS_STR_HIST_RUNCWD,
   UCIS_STR_COMMENT,
   UCIS_STR_TEST_TIMEUNIT,
   UCIS_STR_TEST_DATE,
   UCIS_STR_TEST_SIMARGS,
   UCIS_STR_TEST_USERNAME,
   UCIS_STR_TEST_NAME,
   UCIS_STR_TEST_SEED,
   UCIS_STR_TEST_HOSTNAME,
   UCIS_STR_TEST_HOSTOS,
   UCIS_STR_EXPR_TERMS,
   UCIS_STR_TOGGLE_CANON_NAME,
   UCIS_STR_UNIQUE_ID_ALIAS,
   UCIS_STR_DESIGN_VERSION_ID,
   UCIS_STR_DU_SIGNATURE,
   UCIS_STR_HIST_TOOLCATEGORY,
   UCIS_STR_HIST_LOG_NAME,
   UCIS_STR_HIST_PHYS_NAME,
} ucisStringPropertyEnumT;

const char *ucis_GetStringProperty(ucisT db, ucisObjT obj, int coverindex,
                                   ucisStringPropertyEnumT property);
int ucis_SetStringProperty(ucisT db, ucisObjT obj, int coverindex,
                           ucisStringPropertyEnumT property, const char *value);

////////////////////////////////////////////////////////////////////////////////
// Object handle properties

typedef enum {
   UCIS_HANDLE_SCOPE_PARENT,
   UCIS_HANDLE_SCOPE_TOP,
   UCIS_HANDLE_INSTANCE_DU,
   UCIS_HANDLE_HIST_NODE_PARENT,
   UCIS_HANDLE_HIST_NODE_ROOT
} ucisHandleEnumT;

ucisObjT ucis_GetHandleProperty(ucisT db, ucisObjT obj,
                                ucisHandleEnumT property);
int ucis_SetHandleProperty(ucisT db, ucisObjT obj, ucisHandleEnumT property,
                           ucisObjT value);

////////////////////////////////////////////////////////////////////////////////
// Traversal

typedef enum {
   UCIS_OBJ_ERROR       = 0x00000000,
   UCIS_OBJ_HISTORYNODE = 0x00000001,
   UCIS_OBJ_SCOPE       = 0x00000002,
   UCIS_OBJ_COVERITEM   = 0x00000004,
   UCIS_OBJ_ANY         = -1
} ucisObjMaskT;

ucisObjMaskT ucis_ObjKind(ucisT db, ucisObjT obj);
ucisIteratorT ucis_ScopeIterate(ucisT db, ucisScopeT scope,
                                ucisScopeMaskTypeT scopemask);
ucisScopeT ucis_ScopeScan(ucisT db, ucisIteratorT iterator);
void ucis_FreeIterator(ucisT db, ucisIteratorT iterator);
ucisIteratorT ucis_CoverIterate(ucisT db, ucisScopeT scope,
                                ucisCoverMaskTypeT covermask);
int ucis_CoverScan(ucisT db, ucisIteratorT iterator);

int ucis_CallBack(ucisT db, ucisScopeT start, ucis_CBFuncT cbfunc,
                  void* userdata);

////////////////////////////////////////////////////////////////////////////////
// History nodes

typedef int ucisHistoryNodeKindT;

#define UCIS_HISTORYNODE_NONE    -1
#define UCIS_HISTORYNODE_ALL      0
#define UCIS_HISTORYNODE_TEST     1
#define UCIS_HISTORYNODE_MERGE    2

#define UCIS_SIM_TOOL        "UCIS:Simulator"
#define UCIS_FORMAL_TOOL     "UCIS:Formal"
#define UCIS_ANALOG_TOOL     "UCIS:Analog"
#define UCIS_EMULATOR_TOOL   "UCIS:Emulator"
#define UCIS_MERGE_TOOL      "UCIS:Merge"

typedef enum {
   UCIS_TESTSTATUS_OK,
   UCIS_TESTSTATUS_WARNING,
   UCIS_TESTSTATUS_ERROR,
   UCIS_TESTSTATUS_FATAL,
   UCIS_TESTSTATUS_MISSING,
   UCIS_TESTSTATUS_MERGE_ERROR
} ucisTestStatusT;

typedef struct {
   ucisTestStatusT teststatus;
   double          simtime;
   const char*     timeunit;
   const char*     runcwd;
   double          cputime;
   const char*     seed;
   const char*     cmd;
   const char*     args;
   int             compulsory;
   const char*     date;
   const char*     username;
   double          cost;
   const char*     toolcategory;
} ucisTestDataT;

int ucis_SetTestData(ucisT db, ucisHistoryNodeT testhistorynode,
                     ucisTestDataT *testdata);
ucisHistoryNodeT ucis_CreateHistoryNode(ucisT db, ucisHistoryNodeT parent,
                                        char *logicalname, char *physicalname,
                                        ucisHistoryNodeKindT kind);

////////////////////////////////////////////////////////////////////////////////
// Scopes

#define UCIS_TOGGLE          INT64_C(0x0000000000000001)
#define UCIS_BRANCH          INT64_C(0x0000000000000002)
#define UCIS_EXPR            INT64_C(0x0000000000000004)
#define UCIS_COND            INT64_C(0x0000000000000008)
#define UCIS_INSTANCE        INT64_C(0x0000000000000010)
#define UCIS_PROCESS         INT64_C(0x0000000000000020)
#define UCIS_BLOCK           INT64_C(0x0000000000000040)
#define UCIS_FUNCTION        INT64_C(0x0000000000000080)
#define UCIS_FORKJOIN        INT64_C(0x0000000000000100)
#define UCIS_GENERATE        INT64_C(0x0000000000000200)
#define UCIS_GENERIC         INT64_C(0x0000000000000400)
#define UCIS_CLASS           INT64_C(0x0000000000000800)
#define UCIS_COVERGROUP      INT64_C(0x0000000000001000)
#define UCIS_COVERINSTANCE   INT64_C(0x0000000000002000)
#define UCIS_COVERPOINT      INT64_C(0x0000000000004000)
#define UCIS_CROSS           INT64_C(0x0000000000008000)
#define UCIS_COVER           INT64_C(0x0000000000010000)
#define UCIS_ASSERT          INT64_C(0x0000000000020000)
#define UCIS_PROGRAM         INT64_C(0x0000000000040000)
#define UCIS_PACKAGE         INT64_C(0x0000000000080000)
#define UCIS_TASK            INT64_C(0x0000000000100000)
#define UCIS_INTERFACE       INT64_C(0x0000000000200000)
#define UCIS_FSM             INT64_C(0x0000000000400000)
#define UCIS_TESTPLAN        INT64_C(0x0000000000800000)
#define UCIS_DU_MODULE       INT64_C(0x0000000001000000)
#define UCIS_DU_ARCH         INT64_C(0x0000000002000000)
#define UCIS_DU_PACKAGE      INT64_C(0x0000000004000000)
#define UCIS_DU_PROGRAM      INT64_C(0x0000000008000000)
#define UCIS_DU_INTERFACE    INT64_C(0x0000000010000000)
#define UCIS_FSM_STATES      INT64_C(0x0000000020000000)
#define UCIS_FSM_TRANS       INT64_C(0x0000000040000000)
#define UCIS_COVBLOCK        INT64_C(0x0000000080000000)
#define UCIS_CVGBINSCOPE     INT64_C(0x0000000100000000)
#define UCIS_ILLEGALBINSCOPE INT64_C(0x0000000200000000)
#define UCIS_IGNOREBINSCOPE  INT64_C(0x0000000400000000)
#define UCIS_BBLOCKSCOPE     INT64_C(0x0000000800000000)
#define UCIS_GROUP           INT64_C(0x0000001000000000)
#define UCIS_TRANSITION      INT64_C(0x0000002000000000)
#define UCIS_RESERVEDSCOPE   INT64_C(0xFF00000000000000)
#define UCIS_SCOPE_ERROR     INT64_C(0x0000000000000000)

#define UCIS_FSM_SCOPE ((ucisScopeMaskTypeT)(UCIS_FSM |                 \
                                             UCIS_FSM_STATES |          \
                                             UCIS_FSM_TRANS))

#define UCIS_CODE_COV_SCOPE ((ucisScopeMaskTypeT)(UCIS_BRANCH |         \
                                                  UCIS_EXPR |           \
                                                  UCIS_COND |           \
                                                  UCIS_TOGGLE |         \
                                                  UCIS_FSM_SCOPE |      \
                                                  UCIS_BLOCK))

#define UCIS_DU_ANY ((ucisScopeMaskTypeT)(UCIS_DU_MODULE |              \
                                          UCIS_DU_ARCH |                \
                                          UCIS_DU_PACKAGE |             \
                                          UCIS_DU_PROGRAM |             \
                                          UCIS_DU_INTERFACE))

#define UCIS_CVG_SCOPE ((ucisScopeMaskTypeT)(UCIS_COVERGROUP |          \
                                             UCIS_COVERINSTANCE |       \
                                             UCIS_COVERPOINT |          \
                                             UCIS_CVGBINSCOPE |         \
                                             UCIS_ILLEGALBINSCOPE |     \
                                             UCIS_IGNOREBINSCOPE |      \
                                             UCIS_CROSS))

#define UCIS_FUNC_COV_SCOPE ((ucisScopeMaskTypeT)(UCIS_CVG_SCOPE |      \
                                                  UCIS_COVER))

#define UCIS_COV_SCOPE ((ucisScopeMaskTypeT)(UCIS_CODE_COV_SCOPE |      \
                                             UCIS_FUNC_COV_SCOPE))

#define UCIS_VERIF_SCOPE ((ucisScopeMaskTypeT)(UCIS_COV_SCOPE |         \
                                               UCIS_ASSERT |            \
                                               UCIS_GENERIC))

#define UCIS_HDL_SUBSCOPE ((ucisScopeMaskTypeT)(UCIS_PROCESS |          \
                                                UCIS_BLOCK |            \
                                                UCIS_FUNCTION |         \
                                                UCIS_FORKJOIN |         \
                                                UCIS_GENERATE |         \
                                                UCIS_TASK |             \
                                                UCIS_CLASS))

#define UCIS_HDL_INST_SCOPE ((ucisScopeMaskTypeT)(UCIS_INSTANCE |       \
                                                  UCIS_PROGRAM |        \
                                                  UCIS_PACKAGE |        \
                                                  UCIS_INTERFACE))

#define UCIS_HDL_DU_SCOPE ((ucisScopeMaskTypeT)(UCIS_DU_ANY))

#define UCIS_HDL_SCOPE ((ucisScopeMaskTypeT)(UCIS_HDL_SUBSCOPE |        \
                                             UCIS_HDL_INST_SCOPE |      \
                                             UCIS_HDL_DU_SCOPE))

#define UCIS_NO_SCOPES  ((ucisScopeMaskTypeT)INT64_C(0))
#define UCIS_ALL_SCOPES ((ucisScopeMaskTypeT)INT64_C(-1))

typedef unsigned int ucisFlagsT;

#define UCIS_SCOPEMASK_GENERAL     0x0000FFFF;
#define UCIS_SCOPEMASK_TYPED       0x07FF0000;
#define UCIS_SCOPEMASK_MARK        0x08000000;
#define UCIS_SCOPEMASK_USER        0xF0000000;

#define UCIS_INST_ONCE             0x00000001
#define UCIS_ENABLED_STMT          0x00000002
#define UCIS_ENABLED_BRANCH        0x00000004
#define UCIS_ENABLED_COND          0x00000008
#define UCIS_ENABLED_EXPR          0x00000010
#define UCIS_ENABLED_FSM           0x00000020
#define UCIS_ENABLED_TOGGLE        0x00000040
#define UCIS_SCOPE_UNDER_DU        0x00000100
#define UCIS_SCOPE_EXCLUDED        0x00000200
#define UCIS_SCOPE_PRAGMA_EXCLUDED 0x00000400
#define UCIS_SCOPE_PRAGMA_CLEARED  0x00000800
#define UCIS_SCOPE_SPECIALIZED     0x00001000
#define UCIS_IS_TOP_NODE           0x00010000
#define UCIS_IS_IMMEDIATE_ASSERT   0x00010000
#define UCIS_SCOPE_CVG_AUTO        0x00010000
#define UCIS_SCOPE_CVG_SCALAR      0x00020000
#define UCIS_SCOPE_CVG_VECTOR      0x00040000
#define UCIS_SCOPE_CVG_TRANSITION  0x00080000
#define UCIS_SCOPE_IFF_EXISTS      0x00100000
#define UCIS_SCOPE_SAMPLE_TRUE     0x00200000
#define UCIS_ENABLED_BLOCK         0x00800000
#define UCIS_SCOPE_BLOCK_ISBRANCH  0x01000000
#define UCIS_SCOPEFLAG_MARK        0x08000000
#define UCIS_SCOPE_INTERNAL        0xF0000000

typedef enum {
   UCIS_VHDL,
   UCIS_VLOG,
   UCIS_SV,
   UCIS_SYSTEMC,
   UCIS_PSL_VHDL,
   UCIS_PSL_VLOG,
   UCIS_PSL_SV,
   UCIS_PSL_SYSTEMC,
   UCIS_E,
   UCIS_VERA,
   UCIS_NONE,
   UCIS_OTHER,
   UCIS_SOURCE_ERROR
} ucisSourceT;

ucisScopeT ucis_CreateScope(ucisT db, ucisScopeT parent, const char *name,
                            ucisSourceInfoT *srcinfo, int weight,
                            ucisSourceT source, ucisScopeTypeT type,
                            ucisFlagsT flags);
ucisScopeT ucis_CreateInstance(ucisT db, ucisScopeT parent, const char *name,
                               ucisSourceInfoT *fileinfo, int weight,
                               ucisSourceT source, ucisScopeTypeT type,
                               ucisScopeT du_scope, ucisFlagsT flags);

typedef enum {
    UCIS_TOGGLE_METRIC_NOBINS = 1,
    UCIS_TOGGLE_METRIC_ENUM,
    UCIS_TOGGLE_METRIC_TRANSITION,
    UCIS_TOGGLE_METRIC_2STOGGLE,
    UCIS_TOGGLE_METRIC_ZTOGGLE,
    UCIS_TOGGLE_METRIC_XTOGGLE
} ucisToggleMetricT;

typedef enum {
    UCIS_TOGGLE_TYPE_NET = 1,
    UCIS_TOGGLE_TYPE_REG = 2
} ucisToggleTypeT;

typedef enum {
    UCIS_TOGGLE_DIR_INTERNAL = 1,
    UCIS_TOGGLE_DIR_IN,
    UCIS_TOGGLE_DIR_OUT,
    UCIS_TOGGLE_DIR_INOUT
} ucisToggleDirT;

ucisScopeT ucis_CreateToggle(ucisT db, ucisScopeT parent, const char *name,
                             const char *canonical_name, ucisFlagsT flags,
                             ucisToggleMetricT toggle_metric,
                             ucisToggleTypeT toggle_type,
                             ucisToggleDirT toggle_dir);

ucisScopeTypeT ucis_GetScopeType(ucisT db, ucisScopeT scope);

////////////////////////////////////////////////////////////////////////////////
// Cover items

#define UCIS_CVGBIN          INT64_C(0x0000000000000001)
#define UCIS_COVERBIN        INT64_C(0x0000000000000002)
#define UCIS_ASSERTBIN       INT64_C(0x0000000000000004)
#define UCIS_SCBIN           INT64_C(0x0000000000000008)
#define UCIS_ZINBIN          INT64_C(0x0000000000000010)
#define UCIS_STMTBIN         INT64_C(0x0000000000000020)
#define UCIS_BRANCHBIN       INT64_C(0x0000000000000040)
#define UCIS_EXPRBIN         INT64_C(0x0000000000000080)
#define UCIS_CONDBIN         INT64_C(0x0000000000000100)
#define UCIS_TOGGLEBIN       INT64_C(0x0000000000000200)
#define UCIS_PASSBIN         INT64_C(0x0000000000000400)
#define UCIS_FSMBIN          INT64_C(0x0000000000000800)
#define UCIS_USERBIN         INT64_C(0x0000000000001000)
#define UCIS_GENERICBIN      UCIS_USERBIN
#define UCIS_COUNT           INT64_C(0x0000000000002000)
#define UCIS_FAILBIN         INT64_C(0x0000000000004000)
#define UCIS_VACUOUSBIN      INT64_C(0x0000000000008000)
#define UCIS_DISABLEDBIN     INT64_C(0x0000000000010000)
#define UCIS_ATTEMPTBIN      INT64_C(0x0000000000020000)
#define UCIS_ACTIVEBIN       INT64_C(0x0000000000040000)
#define UCIS_IGNOREBIN       INT64_C(0x0000000000080000)
#define UCIS_ILLEGALBIN      INT64_C(0x0000000000100000)
#define UCIS_DEFAULTBIN      INT64_C(0x0000000000200000)
#define UCIS_PEAKACTIVEBIN   INT64_C(0x0000000000400000)
#define UCIS_BLOCKBIN        INT64_C(0x0000000001000000)
#define UCIS_USERBITS        INT64_C(0x00000000FE000000)
#define UCIS_RESERVEDBIN     INT64_C(0xFF00000000000000)

#define UCIS_COVERGROUPBINS                        \
   ((ucisCoverMaskTypeT)(UCIS_CVGBIN |             \
                         UCIS_IGNOREBIN |          \
                         UCIS_ILLEGALBIN |         \
                         UCIS_DEFAULTBIN))

#define UCIS_FUNC_COV                              \
   ((ucisCoverMaskTypeT)(UCIS_COVERGROUPBINS |     \
                         UCIS_COVERBIN |           \
                         UCIS_SCBIN))

#define UCIS_CODE_COV                              \
   ((ucisCoverMaskTypeT)(UCIS_STMTBIN |            \
                         UCIS_BRANCHBIN |          \
                         UCIS_EXPRBIN |            \
                         UCIS_CONDBIN |            \
                         UCIS_TOGGLEBIN |          \
                         UCIS_FSMBIN |             \
                         UCIS_BLOCKBIN))

#define UCIS_ASSERTIONBINS \
   ((ucisCoverMaskTypeT)(UCIS_ASSERTBIN |          \
                         UCIS_PASSBIN |            \
                         UCIS_VACUOUSBIN |         \
                         UCIS_DISABLEDBIN |        \
                         UCIS_ATTEMPTBIN |         \
                         UCIS_ACTIVEBIN |          \
                         UCIS_PEAKACTIVEBIN))

#define UCIS_COVERDIRECTIVEBINS                    \
   ((ucisCoverMaskTypeT)(UCIS_COVERBIN |           \
                         UCIS_FAILBIN))

#define UCIS_NO_BINS  ((ucisCoverMaskTypeT)INT64_C(0))
#define UCIS_ALL_BINS ((ucisCoverMaskTypeT)INT64_C(-1))

#define UCIS_COVERITEMMASK_GENERAL 0x0000FFFF
#define UCIS_COVERITEMMASK_TYPED   0x07FF0000
#define UCIS_COVERITEMMASK_MARK    0x08000000
#define UCIS_COVERITEMMASK_USER    0xF0000000

#define UCIS_IS_32BIT           0x00000001
#define UCIS_IS_64BIT           0x00000002
#define UCIS_IS_VECTOR          0x00000004
#define UCIS_HAS_GOAL           0x00000008
#define UCIS_HAS_WEIGHT         0x00000010
#define UCIS_EXCLUDE_PRAGMA     0x00000020
#define UCIS_EXCLUDE_FILE       0x00000040
#define UCIS_EXCLUDE_INST       0x00000080
#define UCIS_EXCLUDE_AUTO       0x00000100
#define UCIS_ENABLED            0x00000200
#define UCIS_HAS_LIMIT          0x00000400
#define UCIS_HAS_COUNT          0x00000800
#define UCIS_IS_COVERED         0x00001000
#define UCIS_UOR_SAFE_COVERITEM 0x00002000
#define UCIS_CLEAR_PRAGMA       0x00004000
#define UCIS_HAS_ACTION         0x00010000
#define UCIS_IS_TLW_ENABLED     0x00020000
#define UCIS_LOG_ON             0x00040000
#define UCIS_IS_EOS_NOTE        0x00080000
#define UCIS_IS_FSM_RESET       0x00010000
#define UCIS_IS_FSM_TRAN        0x00020000
#define UCIS_IS_BR_ELSE         0x00010000
#define UCIS_BIN_IFF_EXISTS     0x00010000
#define UCIS_BIN_SAMPLE_TRUE    0x00020000
#define UCIS_IS_CROSSAUTO       0x00040000
#define UCIS_COVERFLAG_MARK     0x08000000
#define UCIS_USERFLAGS          0xF0000000
#define UCIS_FLAG_MASK          0xFFFFFFFF

#define UCIS_EXCLUDED (UCIS_EXCLUDE_FILE | UCIS_EXCLUDE_PRAGMA | \
                       UCIS_EXCLUDE_INST | UCIS_EXCLUDE_AUTO)

typedef union {
    uint64_t       int64;
    uint32_t       int32;
    unsigned char* bytevector;
} ucisCoverDataValueT;

typedef struct {
    ucisCoverTypeT      type;
    ucisFlagsT          flags;
    ucisCoverDataValueT data;
    int                 goal;
    int                 weight;
    int                 limit;
    int                 bitlen;
} ucisCoverDataT;

int ucis_CreateNextCover(ucisT db, ucisScopeT parent, const char *name,
                         ucisCoverDataT *data, ucisSourceInfoT *sourceinfo);
int ucis_GetCoverData(ucisT db, ucisScopeT parent, int coverindex, char **name,
                      ucisCoverDataT *data, ucisSourceInfoT *sourceinfo);

#endif   // !HAVE_UCIS_H

#endif   // _UCIS_API_H
