//
//  Copyright (C) 2026  Nick Gasson
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
#include "tcl/tcl-priv.h"
#include "tcl/tcl-shell.h"
#include "tcl/tcl-structs.h"
#include "vhpi/vhpi-model.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>

static void free_vhpi_obj(Tcl_Obj *obj)
{
   int *refcnt = obj->internalRep.twoPtrValue.ptr2;
   if (refcnt != NULL) {
      assert(*refcnt > 0);
      if (--(*refcnt) > 0)
         return;

      ckfree(refcnt);
   }

   vhpiHandleT handle = obj->internalRep.twoPtrValue.ptr1;
   if (handle != NULL)
      vhpi_release_handle(handle);
}

static void dup_vhpi_obj(Tcl_Obj *src, Tcl_Obj *dup)
{
   int *refcnt = src->internalRep.twoPtrValue.ptr2;
   if (refcnt == NULL) {
      src->internalRep.twoPtrValue.ptr2 = refcnt = ckalloc(sizeof(int));
      *refcnt = 1;
   }

   assert(*refcnt > 0);
   (*refcnt)++;

   dup->internalRep.twoPtrValue.ptr1 = src->internalRep.twoPtrValue.ptr1;
   dup->internalRep.twoPtrValue.ptr2 = refcnt;
   dup->typePtr = src->typePtr;
}

static void update_obj_string_rep(Tcl_Obj *obj)
{
   Tcl_InvalidateStringRep(obj);

   vhpiHandleT handle = obj->internalRep.twoPtrValue.ptr1;

   char buf[32];
   if (handle == NULL)
      obj->length = checked_sprintf(buf, sizeof(buf), "NULL@0x0");
   else {
      const vhpiCharT *kind = vhpi_get_str(vhpiKindStrP, handle);
      obj->length = checked_sprintf(buf, sizeof(buf), "%s@%p", kind, handle);
   }

   obj->bytes = ckalloc(obj->length + 1);
   memcpy(obj->bytes, buf, obj->length + 1);
}

static Tcl_ObjType vhpiHandleType = {
   .name             = "vhpiHandleT",
   .freeIntRepProc   = free_vhpi_obj,
   .dupIntRepProc    = dup_vhpi_obj,
   .updateStringProc = update_obj_string_rep,
   .setFromAnyProc   = NULL
};

static Tcl_Obj *new_vhpi_obj(vhpiHandleT h)
{
   Tcl_Obj *obj = Tcl_NewObj();
   obj->internalRep.twoPtrValue.ptr1 = h;
   obj->internalRep.twoPtrValue.ptr2 = NULL;
   obj->typePtr = &vhpiHandleType;
   Tcl_InvalidateStringRep(obj);
   return obj;
}

static bool get_vhpi_obj(Tcl_Interp *interp, Tcl_Obj *obj, vhpiHandleT *handle)
{
   if (obj->typePtr == &vhpiHandleType) {
      *handle = obj->internalRep.twoPtrValue.ptr1;
      return true;
   }

   const char *str = Tcl_GetString(obj);
   if (strcmp(str, "0") == 0) {
      *handle = NULL;
      return true;
   }

   Tcl_Obj *msg = Tcl_ObjPrintf("invalid VHPI handle \"%s\"", str);
   Tcl_SetObjResult(interp, msg);
   return false;
}

static bool get_vhpi_one_to_many(Tcl_Interp *interp, Tcl_Obj *obj,
                                 vhpiOneToManyT *rel)
{
   const char *str = Tcl_GetString(obj);

   if (isdigit(str[0]))
      return Tcl_GetIntFromObj(interp, obj, (int *)rel) == TCL_OK;

   struct {
      const char *name;
      vhpiOneToManyT value;
   } defs[] = {
      { "AliasDecls", vhpiAliasDecls },
      { "Argvs", vhpiArgvs },
      { "AttrDecls", vhpiAttrDecls },
      { "AttrSpecs", vhpiAttrSpecs },
      { "BasicSignals", vhpiBasicSignals },
      { "BlockStmts", vhpiBlockStmts },
      { "Branchs", vhpiBranchs },
      { "Choices", vhpiChoices },
      { "CompInstStmts", vhpiCompInstStmts },
      { "CondWaveforms", vhpiCondWaveforms },
      { "ConfigItems", vhpiConfigItems },
      { "ConfigSpecs", vhpiConfigSpecs },
      { "ConstDecls", vhpiConstDecls },
      { "Constraints", vhpiConstraints },
      { "Decls", vhpiDecls },
      { "DepUnits", vhpiDepUnits },
      { "DesignUnits", vhpiDesignUnits },
      { "DrivenSigs", vhpiDrivenSigs },
      { "Drivers", vhpiDrivers },
      { "ElemAssocs", vhpiElemAssocs },
      { "EntityClassEntries", vhpiEntityClassEntries },
      { "EntityDesignators", vhpiEntityDesignators },
      { "EnumLiterals", vhpiEnumLiterals },
      { "EqProcessStmts", vhpiEqProcessStmts },
      { "Foreignfs", vhpiForeignfs },
      { "GenerateStmts", vhpiGenerateStmts },
      { "GenericAssocs", vhpiGenericAssocs },
      { "GenericDecls", vhpiGenericDecls },
      { "IndexExprs", vhpiIndexExprs },
      { "IndexedNames", vhpiIndexedNames },
      { "InternalRegions", vhpiInternalRegions },
      { "LibraryDecls", vhpiLibraryDecls },
      { "LocalContributors", vhpiLocalContributors },
      { "LocalLoads", vhpiLocalLoads },
      { "Members", vhpiMembers },
      { "OptimizedContributors", vhpiOptimizedContributors },
      { "OptimizedLoads", vhpiOptimizedLoads },
      { "PackInsts", vhpiPackInsts },
      { "ParamAssocs", vhpiParamAssocs },
      { "ParamDecls", vhpiParamDecls },
      { "ParamExprs", vhpiParamExprs },
      { "PortAssocs", vhpiPortAssocs },
      { "PortDecls", vhpiPortDecls },
      { "RecordElems", vhpiRecordElems },
      { "SelectWaveforms", vhpiSelectWaveforms },
      { "SelectedNames", vhpiSelectedNames },
      { "Sensitivities", vhpiSensitivities },
      { "SeqStmts", vhpiSeqStmts },
      { "SigAttrs", vhpiSigAttrs },
      { "SigDecls", vhpiSigDecls },
      { "SigNames", vhpiSigNames },
      { "Signals", vhpiSignals },
      { "Specs", vhpiSpecs },
      { "Stmts", vhpiStmts },
      { "Transactions", vhpiTransactions },
      { "Types", vhpiTypes },
      { "UnitDecls", vhpiUnitDecls },
      { "UseClauses", vhpiUseClauses },
      { "Uses", vhpiUses },
      { "VarDecls", vhpiVarDecls },
      { "WaveformElems", vhpiWaveformElems },
   };

   for (int i = 0; i < ARRAY_LEN(defs); i++) {
      if (strcmp(str, defs[i].name) == 0) {
         *rel = defs[i].value;
         return true;
      }
   }

   Tcl_Obj *msg = Tcl_ObjPrintf("invalid vhpiOneToManyT \"%s\"", str);
   Tcl_SetObjResult(interp, msg);
   return false;
}

static bool get_vhpi_one_to_one(Tcl_Interp *interp, Tcl_Obj *obj,
                                vhpiOneToOneT *rel)
{
   const char *str = Tcl_GetString(obj);

   if (isdigit(str[0]))
      return Tcl_GetIntFromObj(interp, obj, (int *)rel) == TCL_OK;

   struct {
      const char *name;
      vhpiOneToOneT value;
   } defs[] = {
      { "AbstractLiteral", vhpiAbstractLiteral },
      { "Actual", vhpiActual },
      { "All", vhpiAll },
      { "AttrDecl", vhpiAttrDecl },
      { "AttrSpec", vhpiAttrSpec },
      { "BaseType", vhpiBaseType },
      { "BaseUnit", vhpiBaseUnit },
      { "BlockConfig", vhpiBlockConfig },
      { "CaseExpr", vhpiCaseExpr },
      { "CondExpr", vhpiCondExpr },
      { "ConfigDecl", vhpiConfigDecl },
      { "ConfigSpec", vhpiConfigSpec },
      { "Constraint", vhpiConstraint },
      { "Contributor", vhpiContributor },
      { "CurCallback", vhpiCurCallback },
      { "CurStackFrame", vhpiCurStackFrame },
      { "DerefObj", vhpiDerefObj },
      { "DesignUnit", vhpiDesignUnit },
      { "DownStack", vhpiDownStack },
      { "EntityAspect", vhpiEntityAspect },
      { "EntityDecl", vhpiEntityDecl },
      { "EqProcessStmt", vhpiEqProcessStmt },
      { "Expr", vhpiExpr },
      { "Formal", vhpiFormal },
      { "FuncDecl", vhpiFuncDecl },
      { "GroupTempDecl", vhpiGroupTempDecl },
      { "GuardExpr", vhpiGuardExpr },
      { "GuardSig", vhpiGuardSig },
      { "ImmRegion", vhpiImmRegion },
      { "InPort", vhpiInPort },
      { "InitExpr", vhpiInitExpr },
      { "LeftExpr", vhpiLeftExpr },
      { "LexicalScope", vhpiLexicalScope },
      { "LhsExpr", vhpiLhsExpr },
      { "Local", vhpiLocal },
      { "LogicalExpr", vhpiLogicalExpr },
      { "Others", vhpiOthers },
      { "OutPort", vhpiOutPort },
      { "ParamDecl", vhpiParamDecl },
      { "Parent", vhpiParent },
      { "PhysLiteral", vhpiPhysLiteral },
      { "Prefix", vhpiPrefix },
      { "PrimaryUnit", vhpiPrimaryUnit },
      { "ProtectedTypeBody", vhpiProtectedTypeBody },
      { "ProtectedTypeDecl", vhpiProtectedTypeDecl },
      { "RejectTime", vhpiRejectTime },
      { "ReportExpr", vhpiReportExpr },
      { "ResolFunc", vhpiResolFunc },
      { "ReturnExpr", vhpiReturnExpr },
      { "RhsExpr", vhpiRhsExpr },
      { "RightExpr", vhpiRightExpr },
      { "RootInst", vhpiRootInst },
      { "SelectExpr", vhpiSelectExpr },
      { "SeverityExpr", vhpiSeverityExpr },
      { "SimpleName", vhpiSimpleName },
      { "SubpBody", vhpiSubpBody },
      { "SubpDecl", vhpiSubpDecl },
      { "Suffix", vhpiSuffix },
      { "TimeExpr", vhpiTimeExpr },
      { "TimeOutExpr", vhpiTimeOutExpr },
      { "Tool", vhpiTool },
      { "Type", vhpiType },
      { "UnitDecl", vhpiUnitDecl },
      { "UpStack", vhpiUpStack },
      { "UpperRegion", vhpiUpperRegion },
      { "Use", vhpiUse },
      { "ValExpr", vhpiValExpr },
      { "ElemType", vhpiElemType },
      { "FirstNamedType", vhpiFirstNamedType },
      { "ReturnType", vhpiReturnType },
      { "ValType", vhpiValType },
      { "CurRegion", vhpiCurRegion },
      { "Signal", vhpiSignal },
      { "LibraryDecl", vhpiLibraryDecl },
      { "SimNet", vhpiSimNet },
      { "AliasedName", vhpiAliasedName },
      { "CompDecl", vhpiCompDecl },
      { "ProtectedTypeInst", vhpiProtectedTypeInst },
      { "GenIndex", vhpiGenIndex },
   };

   for (int i = 0; i < ARRAY_LEN(defs); i++) {
      if (strcmp(str, defs[i].name) == 0) {
         *rel = defs[i].value;
         return true;
      }
   }

   Tcl_Obj *msg = Tcl_ObjPrintf("invalid vhpiOneToOneT \"%s\"", str);
   Tcl_SetObjResult(interp, msg);
   return false;
}

static bool get_vhpi_str_property(Tcl_Interp *interp, Tcl_Obj *obj,
                                  vhpiStrPropertyT *prop)
{
   const char *str = Tcl_GetString(obj);

   if (isdigit(str[0]))
      return Tcl_GetIntFromObj(interp, obj, (int *)prop) == TCL_OK;

   struct {
      const char *name;
      vhpiStrPropertyT value;
   } defs[] = {
      { "CaseName", vhpiCaseNameP },
      { "CompName", vhpiCompNameP },
      { "DefName", vhpiDefNameP },
      { "FileName", vhpiFileNameP },
      { "FullCaseName", vhpiFullCaseNameP },
      { "FullName", vhpiFullNameP },
      { "KindStr", vhpiKindStrP },
      { "LabelName", vhpiLabelNameP },
      { "LibLogicalName", vhpiLibLogicalNameP },
      { "LibPhysicalName", vhpiLibPhysicalNameP },
      { "LogicalName", vhpiLogicalNameP },
      { "LoopLabelName", vhpiLoopLabelNameP },
      { "Name", vhpiNameP },
      { "StrVal", vhpiStrValP },
      { "ToolVersion", vhpiToolVersionP },
      { "UnitName", vhpiUnitNameP },
      { "SaveRestartLocation", vhpiSaveRestartLocationP },
      { "CompInstName", vhpiCompInstNameP },
      { "InstNames", vhpiInstNamesP },
      { "SignatureName", vhpiSignatureNameP },
      { "SpecName", vhpiSpecNameP },
   };

   for (int i = 0; i < ARRAY_LEN(defs); i++) {
      if (strcmp(str, defs[i].name) == 0) {
         *prop = defs[i].value;
         return true;
      }
   }

   Tcl_Obj *msg = Tcl_ObjPrintf("invalid vhpiStrPropertyT \"%s\"", str);
   Tcl_SetObjResult(interp, msg);
   return false;
}

static bool get_vhpi_severity(Tcl_Interp *interp, Tcl_Obj *obj,
                              vhpiSeverityT *sev)
{
   const char *str = Tcl_GetString(obj);

   if (isdigit(str[0]))
      return Tcl_GetIntFromObj(interp, obj, (int *)sev) == TCL_OK;

   struct {
      const char *name;
      vhpiSeverityT value;
   } defs[] = {
      { "Note", vhpiNote },
      { "Warning", vhpiWarning },
      { "Error", vhpiError },
      { "Failure", vhpiFailure },
      { "System", vhpiSystem },
      { "Internal", vhpiInternal },
   };

   for (int i = 0; i < ARRAY_LEN(defs); i++) {
      if (strcmp(str, defs[i].name) == 0) {
         *sev = defs[i].value;
         return true;
      }
   }

   Tcl_Obj *msg = Tcl_ObjPrintf("invalid vhpiSeverityT \"%s\"", str);
   Tcl_SetObjResult(interp, msg);
   return false;
}

static bool get_vhpi_put_value_mode(Tcl_Interp *interp, Tcl_Obj *obj,
                                    vhpiPutValueModeT *mode)
{
   const char *str = Tcl_GetString(obj);

   if (isdigit(str[0]))
      return Tcl_GetIntFromObj(interp, obj, (int *)mode) == TCL_OK;

   struct {
      const char *name;
      vhpiPutValueModeT mode;
   } defs[] = {
      { "Deposit", vhpiDeposit },
      { "DepositPropagate", vhpiDepositPropagate },
      { "Force", vhpiForce },
      { "ForcePropagate", vhpiForcePropagate },
      { "Release", vhpiRelease },
      { "SizeConstraint", vhpiSizeConstraint },
   };

   for (int i = 0; i < ARRAY_LEN(defs); i++) {
      if (strcmp(str, defs[i].name) == 0) {
         *mode = defs[i].mode;
         return true;
      }
   }

   Tcl_Obj *msg = Tcl_ObjPrintf("invalid vhpiSeverityT \"%s\"", str);
   Tcl_SetObjResult(interp, msg);
   return false;
}

static int wrap_check_error(Tcl_Interp *interp)
{
   vhpiErrorInfoT info;
   if (vhpi_check_error(&info)) {
      Tcl_SetObjResult(interp, Tcl_NewStringObj(info.message, -1));
      return TCL_ERROR;
   }

   return TCL_OK;
}

static int wrap_null(ClientData cd, Tcl_Interp *interp,
                     int objc, Tcl_Obj *const objv[])
{
   if (objc != 1) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   Tcl_Obj *obj = new_vhpi_obj(NULL);
   Tcl_SetObjResult(interp, obj);
   return TCL_OK;
}

static int wrap_handle(ClientData cd, Tcl_Interp *interp,
                       int objc, Tcl_Obj *const objv[])
{
   if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "relation reference");
      return TCL_ERROR;
   }

   vhpiOneToOneT rel;
   if (!get_vhpi_one_to_one(interp, objv[1], &rel))
      return TCL_ERROR;

   vhpiHandleT ref;
   if (!get_vhpi_obj(interp, objv[2], &ref))
      return TCL_ERROR;

   vhpiHandleT result = vhpi_handle(rel, ref);
   if (result == NULL)
      return wrap_check_error(interp);

   Tcl_Obj *obj = new_vhpi_obj(result);
   Tcl_SetObjResult(interp, obj);
   return TCL_OK;
}

static int wrap_handle_by_name(ClientData cd, Tcl_Interp *interp,
                               int objc, Tcl_Obj *const objv[])
{
   if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "name scope");
      return TCL_ERROR;
   }

   const char *name = Tcl_GetString(objv[1]);

   vhpiHandleT scope;
   if (!get_vhpi_obj(interp, objv[2], &scope))
      return TCL_ERROR;

   vhpiHandleT result = vhpi_handle_by_name(name, scope);
   if (result == NULL)
      return wrap_check_error(interp);

   Tcl_Obj *obj = new_vhpi_obj(result);
   Tcl_SetObjResult(interp, obj);
   return TCL_OK;
}

static int wrap_handle_by_index(ClientData cd, Tcl_Interp *interp,
                                int objc, Tcl_Obj *const objv[])
{
   if (objc != 4) {
      Tcl_WrongNumArgs(interp, 1, objv, "relation parent index");
      return TCL_ERROR;
   }

   vhpiOneToManyT rel;
   if (!get_vhpi_one_to_many(interp, objv[1], &rel))
      return TCL_ERROR;

   vhpiHandleT parent;
   if (!get_vhpi_obj(interp, objv[2], &parent))
      return TCL_ERROR;

   int index;
   if (Tcl_GetIntFromObj(interp, objv[3], &index) != TCL_OK)
      return TCL_ERROR;

   vhpiHandleT result = vhpi_handle_by_index(rel, parent, index);
   if (result == NULL)
      return wrap_check_error(interp);

   Tcl_Obj *obj = new_vhpi_obj(result);
   Tcl_SetObjResult(interp, obj);
   return TCL_OK;
}

static int wrap_iterate(ClientData cd, Tcl_Interp *interp,
                        int objc, Tcl_Obj *const objv[])
{
   if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "relation parent");
      return TCL_ERROR;
   }

   vhpiOneToManyT rel;
   if (!get_vhpi_one_to_many(interp, objv[1], &rel))
      return TCL_ERROR;

   vhpiHandleT parent;
   if (!get_vhpi_obj(interp, objv[2], &parent))
      return TCL_ERROR;

   vhpiHandleT it = vhpi_iterator(rel, parent);
   if (it == NULL)
      return wrap_check_error(interp);

   Tcl_Obj *list = Tcl_NewListObj(0, NULL);

   for (;;) {
      vhpiHandleT h = vhpi_scan(it);
      if (h == NULL)
         break;

      Tcl_ListObjAppendElement(interp, list, new_vhpi_obj(h));
   }

   Tcl_SetObjResult(interp, list);
   return TCL_OK;
}

static int wrap_get_str(ClientData cd, Tcl_Interp *interp,
                        int objc, Tcl_Obj *const objv[])
{
   if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "property object");
      return TCL_ERROR;
   }

   vhpiStrPropertyT prop;
   if (!get_vhpi_str_property(interp, objv[1], &prop))
      return TCL_ERROR;

   vhpiHandleT handle;
   if (!get_vhpi_obj(interp, objv[2], &handle))
      return TCL_ERROR;

   const vhpiCharT *result = vhpi_get_str(prop, handle);
   if (result == NULL)
      return wrap_check_error(interp);

   Tcl_SetObjResult(interp, Tcl_NewStringObj((const char *)result, -1));
   return TCL_OK;
}

static int wrap_get_value(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   if (objc != 2) {
      Tcl_WrongNumArgs(interp, 1, objv, "expr");
      return TCL_ERROR;
   }

   vhpiHandleT handle;
   if (!get_vhpi_obj(interp, objv[1], &handle))
      return TCL_ERROR;

   vhpiValueT val = {
      .format = vhpiObjTypeVal,
   };
   int ret = vhpi_get_value(handle, &val);
   if (ret < 0)
      return wrap_check_error(interp);

   switch (val.format) {
   case vhpiLogicVal:
   case vhpiLogicVecVal:
   case vhpiSmallEnumVal:
   case vhpiEnumVal:
      {
         vhpiCharT smallbuf[16];
         val.value.str = smallbuf;
         val.format = vhpiStrVal;
         val.bufSize = sizeof(smallbuf),
         ret = vhpi_get_value(handle, &val);
         if (ret < 0)
            return wrap_check_error(interp);

         vhpiCharT *bigbuf LOCAL = NULL;
         if (ret > 0) {
            val.bufSize = ret;
            val.value.str = bigbuf = xmalloc(ret);

            if (vhpi_get_value(handle, &val) < 0)
               return wrap_check_error(interp);
         }

         Tcl_Obj *obj = Tcl_NewStringObj((char *)val.value.str, -1);
         Tcl_SetObjResult(interp, obj);
         return TCL_OK;
      }
   case vhpiIntVal:
   case vhpiLongIntVal:
      {
         Tcl_Obj *obj = Tcl_NewIntObj(val.value.longintg);
         Tcl_SetObjResult(interp, obj);
         return TCL_OK;
      }
   default:
      {
         Tcl_Obj *msg = Tcl_ObjPrintf("unhandled value format %d", val.format);
         Tcl_SetObjResult(interp, msg);
         return TCL_ERROR;
      }
   }
}

static int wrap_put_value(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   if (objc != 4) {
      Tcl_WrongNumArgs(interp, 1, objv, "object value mode");
      return TCL_ERROR;
   }

   vhpiHandleT handle;
   if (!get_vhpi_obj(interp, objv[1], &handle))
      return TCL_ERROR;

   const char *valstr = Tcl_GetString(objv[2]);

   vhpiPutValueModeT mode;
   if (!get_vhpi_put_value_mode(interp, objv[3], &mode))
      return TCL_ERROR;

   vhpiValueT val = {
      .format = vhpiStrVal,
      .value = { .str = (vhpiCharT *)valstr },
      .bufSize = strlen(valstr) + 1
   };

   if (vhpi_put_value(handle, &val, mode))
      return wrap_check_error(interp);

   return TCL_OK;
}

static int wrap_assert(ClientData cd, Tcl_Interp *interp,
                       int objc, Tcl_Obj *const objv[])
{
   if (objc < 2 || objc > 4) {
      Tcl_WrongNumArgs(interp, 1, objv, "expr ?severity? ?message?");
      return TCL_ERROR;
   }

   int ok;
   if (Tcl_ExprBooleanObj(interp, objv[1], &ok) != TCL_OK)
      return TCL_ERROR;

   if (ok)
      return TCL_OK;  // Assertion passed

   vhpiSeverityT severity = vhpiError;
   if (objc >= 3 && !get_vhpi_severity(interp, objv[2], &severity))
      return TCL_ERROR;

   if (objc == 4)
      vhpi_assert(severity, "%s", Tcl_GetString(objv[3]));
   else
      vhpi_assert(severity, "assertion {%s} failed", Tcl_GetString(objv[1]));

   return TCL_OK;
}

static int wrap_compare_handles(ClientData cd, Tcl_Interp *interp,
                                int objc, Tcl_Obj *const objv[])
{
   if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "handle1 handle2");
      return TCL_ERROR;
   }

   vhpiHandleT handle1;
   if (!get_vhpi_obj(interp, objv[1], &handle1))
      return TCL_ERROR;

   vhpiHandleT handle2;
   if (!get_vhpi_obj(interp, objv[2], &handle2))
      return TCL_ERROR;

   Tcl_Obj *obj = Tcl_NewIntObj(vhpi_compare_handles(handle1, handle2));
   Tcl_SetObjResult(interp, obj);
   return TCL_OK;
}

void shell_add_vhpi_cmds(tcl_shell_t *sh)
{
   Tcl_Namespace *ns = Tcl_CreateNamespace(sh->interp, "vhpi", NULL, NULL);
   if (ns == NULL)
      fatal("failed to register TCL vhpi namespace");

   shell_add_cmd(sh, "vhpi::null", wrap_null, "");
   shell_add_cmd(sh, "vhpi::handle", wrap_handle, "");
   shell_add_cmd(sh, "vhpi::handle_by_name", wrap_handle_by_name, "");
   shell_add_cmd(sh, "vhpi::handle_by_index", wrap_handle_by_index, "");
   shell_add_cmd(sh, "vhpi::iterate", wrap_iterate, "");
   shell_add_cmd(sh, "vhpi::get_str", wrap_get_str, "");
   shell_add_cmd(sh, "vhpi::get_value", wrap_get_value, "");
   shell_add_cmd(sh, "vhpi::put_value", wrap_put_value, "");
   shell_add_cmd(sh, "vhpi::assert", wrap_assert, "");
   shell_add_cmd(sh, "vhpi::compare_handles", wrap_compare_handles, "");
}
