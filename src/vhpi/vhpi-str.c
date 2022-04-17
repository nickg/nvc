//
//  Copyright (C) 2014-2022  Nick Gasson
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
#include "vhpi/vhpi-util.h"

const char *vhpi_property_str(int property)
{
   switch (property) {
   case vhpiAccessP: return "vhpiAccessP";
   case vhpiArgcP: return "vhpiArgcP";
   case vhpiAttrKindP: return "vhpiAttrKindP";
   case vhpiBaseIndexP: return "vhpiBaseIndexP";
   case vhpiBeginLineNoP: return "vhpiBeginLineNoP";
   case vhpiEndLineNoP: return "vhpiEndLineNoP";
   case vhpiEntityClassP: return "vhpiEntityClassP";
   case vhpiForeignKindP: return "vhpiForeignKindP";
   case vhpiFrameLevelP: return "vhpiFrameLevelP";
   case vhpiGenerateIndexP: return "vhpiGenerateIndexP";
   case vhpiIntValP: return "vhpiIntValP";
   case vhpiIsAnonymousP: return "vhpiIsAnonymousP";
   case vhpiIsBasicP: return "vhpiIsBasicP";
   case vhpiIsCompositeP: return "vhpiIsCompositeP";
   case vhpiIsDefaultP: return "vhpiIsDefaultP";
   case vhpiIsDeferredP: return "vhpiIsDeferredP";
   case vhpiIsDiscreteP: return "vhpiIsDiscreteP";
   case vhpiIsForcedP: return "vhpiIsForcedP";
   case vhpiIsForeignP: return "vhpiIsForeignP";
   case vhpiIsGuardedP: return "vhpiIsGuardedP";
   case vhpiIsImplicitDeclP: return "vhpiIsImplicitDeclP";
   case DEPRECATED_vhpiIsInvalidP: return "DEPRECATED_vhpiIsInvalidP";
   case vhpiIsLocalP: return "vhpiIsLocalP";
   case vhpiIsNamedP: return "vhpiIsNamedP";
   case vhpiIsNullP: return "vhpiIsNullP";
   case vhpiIsOpenP: return "vhpiIsOpenP";
   case vhpiIsPLIP: return "vhpiIsPLIP";
   case vhpiIsPassiveP: return "vhpiIsPassiveP";
   case vhpiIsPostponedP: return "vhpiIsPostponedP";
   case vhpiIsProtectedTypeP: return "vhpiIsProtectedTypeP";
   case vhpiIsPureP: return "vhpiIsPureP";
   case vhpiIsResolvedP: return "vhpiIsResolvedP";
   case vhpiIsScalarP: return "vhpiIsScalarP";
   case vhpiIsSeqStmtP: return "vhpiIsSeqStmtP";
   case vhpiIsSharedP: return "vhpiIsSharedP";
   case vhpiIsTransportP: return "vhpiIsTransportP";
   case vhpiIsUnaffectedP: return "vhpiIsUnaffectedP";
   case vhpiIsUnconstrainedP: return "vhpiIsUnconstrainedP";
   case vhpiIsUninstantiatedP: return "vhpiIsUninstantiatedP";
   case vhpiIsUpP: return "vhpiIsUpP";
   case vhpiIsVitalP: return "vhpiIsVitalP";
   case vhpiIteratorTypeP: return "vhpiIteratorTypeP";
   case vhpiKindP: return "vhpiKindP";
   case vhpiLeftBoundP: return "vhpiLeftBoundP";
   case DEPRECATED_vhpiLevelP: return "DEPRECATED_vhpiLevelP";
   case vhpiLineNoP: return "vhpiLineNoP";
   case vhpiLineOffsetP: return "vhpiLineOffsetP";
   case vhpiLoopIndexP: return "vhpiLoopIndexP";
   case vhpiModeP: return "vhpiModeP";
   case vhpiNumDimensionsP: return "vhpiNumDimensionsP";
   case DEPRECATED_vhpiNumFieldsP: return "DEPRECATED_vhpiNumFieldsP";
   case vhpiNumGensP: return "vhpiNumGensP";
   case vhpiNumLiteralsP: return "vhpiNumLiteralsP";
   case vhpiNumMembersP: return "vhpiNumMembersP";
   case vhpiNumParamsP: return "vhpiNumParamsP";
   case vhpiNumPortsP: return "vhpiNumPortsP";
   case vhpiOpenModeP: return "vhpiOpenModeP";
   case vhpiPhaseP: return "vhpiPhaseP";
   case vhpiPositionP: return "vhpiPositionP";
   case vhpiPredefAttrP: return "vhpiPredefAttrP";
   case vhpiReasonP: return "vhpiReasonP";
   case vhpiRightBoundP: return "vhpiRightBoundP";
   case vhpiSigKindP: return "vhpiSigKindP";
   case vhpiSizeP: return "vhpiSizeP";
   case vhpiStartLineNoP: return "vhpiStartLineNoP";
   case vhpiStateP: return "vhpiStateP";
   case vhpiStaticnessP: return "vhpiStaticnessP";
   case vhpiVHDLversionP: return "vhpiVHDLversionP";
   case vhpiIdP: return "vhpiIdP";
   case vhpiCapabilitiesP: return "vhpiCapabilitiesP";
   case vhpiAutomaticRestoreP: return "vhpiAutomaticRestoreP";
   case vhpiCompInstKindP: return "vhpiCompInstKindP";
   case vhpiIsBuiltInP: return "vhpiIsBuiltInP";
   case vhpiIsDynamicP: return "vhpiIsDynamicP";
   case vhpiIsOperatorP: return "vhpiIsOperatorP";
   case vhpiNumFieldsP: return "vhpiNumFieldsP";
   case vhpiCaseNameP: return "vhpiCaseNameP";
   case vhpiCompNameP: return "vhpiCompNameP";
   case vhpiDefNameP: return "vhpiDefNameP";
   case vhpiFileNameP: return "vhpiFileNameP";
   case vhpiFullCaseNameP: return "vhpiFullCaseNameP";
   case vhpiFullNameP: return "vhpiFullNameP";
   case vhpiKindStrP: return "vhpiKindStrP";
   case vhpiLabelNameP: return "vhpiLabelNameP";
   case vhpiLibLogicalNameP: return "vhpiLibLogicalNameP";
   case vhpiLibPhysicalNameP: return "vhpiLibPhysicalNameP";
   case vhpiLogicalNameP: return "vhpiLogicalNameP";
   case vhpiLoopLabelNameP: return "vhpiLoopLabelNameP";
   case vhpiNameP: return "vhpiNameP";
   case DEPRECATED_vhpiOpNameP: return "DEPRECATED_vhpiOpNameP";
   case vhpiStrValP: return "vhpiStrValP";
   case vhpiToolVersionP: return "vhpiToolVersionP";
   case vhpiUnitNameP: return "vhpiUnitNameP";
   case vhpiSaveRestartLocationP: return "vhpiSaveRestartLocationP";
   case vhpiCompInstNameP: return "vhpiCompInstNameP";
   case vhpiInstNamesP: return "vhpiInstNamesP";
   case vhpiSignatureNameP: return "vhpiSignatureNameP";
   case vhpiSpecNameP: return "vhpiSpecNameP";
   case vhpiFloatLeftBoundP: return "vhpiFloatLeftBoundP";
   case vhpiFloatRightBoundP: return "vhpiFloatRightBoundP";
   case vhpiRealValP: return "vhpiRealValP";
   case vhpiPhysLeftBoundP: return "vhpiPhysLeftBoundP";
   case vhpiPhysPositionP: return "vhpiPhysPositionP";
   case vhpiPhysRightBoundP: return "vhpiPhysRightBoundP";
   case vhpiPhysValP: return "vhpiPhysValP";
   case DEPRECATED_vhpiPrecisionP: return "DEPRECATED_vhpiPrecisionP";
   case DEPRECATED_vhpiSimTimeUnitP: return "DEPRECATED_vhpiSimTimeUnitP";
   case vhpiResolutionLimitP: return "vhpiResolutionLimitP";
   case vhpiTimeP: return "vhpiTimeP";
   default:
      {
         static char buf[64];
         checked_sprintf(buf, sizeof(buf), "%d", property);
         return buf;
      }
   }
}

const char *vhpi_class_str(vhpiClassKindT kind)
{
   switch (kind) {
   case vhpiAccessTypeDeclK: return "vhpiAccessTypeDeclK";
   case vhpiAggregateK: return "vhpiAggregateK";
   case vhpiAliasDeclK: return "vhpiAliasDeclK";
   case vhpiAllK: return "vhpiAllK";
   case vhpiAllocatorK: return "vhpiAllocatorK";
   case vhpiAnyCollectionK: return "vhpiAnyCollectionK";
   case vhpiArchBodyK: return "vhpiArchBodyK";
   case vhpiArgvK: return "vhpiArgvK";
   case vhpiArrayTypeDeclK: return "vhpiArrayTypeDeclK";
   case DEPRECATED_vhpiAssertStmtK: return "DEPRECATED_vhpiAssertStmtK";
   case vhpiAssocElemK: return "vhpiAssocElemK";
   case vhpiAttrDeclK: return "vhpiAttrDeclK";
   case vhpiAttrSpecK: return "vhpiAttrSpecK";
   case DEPRECATED_vhpiBinaryExprK: return "DEPRECATED_vhpiBinaryExprK";
   case vhpiBitStringLiteralK: return "vhpiBitStringLiteralK";
   case vhpiBlockConfigK: return "vhpiBlockConfigK";
   case vhpiBlockStmtK: return "vhpiBlockStmtK";
   case vhpiBranchK: return "vhpiBranchK";
   case vhpiCallbackK: return "vhpiCallbackK";
   case vhpiCaseStmtK: return "vhpiCaseStmtK";
   case vhpiCharLiteralK: return "vhpiCharLiteralK";
   case vhpiCompConfigK: return "vhpiCompConfigK";
   case vhpiCompDeclK: return "vhpiCompDeclK";
   case vhpiCompInstStmtK: return "vhpiCompInstStmtK";
   case vhpiCondSigAssignStmtK: return "vhpiCondSigAssignStmtK";
   case vhpiCondWaveformK: return "vhpiCondWaveformK";
   case vhpiConfigDeclK: return "vhpiConfigDeclK";
   case vhpiConstDeclK: return "vhpiConstDeclK";
   case vhpiConstParamDeclK: return "vhpiConstParamDeclK";
   case vhpiConvFuncK: return "vhpiConvFuncK";
   case vhpiDerefObjK: return "vhpiDerefObjK";
   case vhpiDisconnectSpecK: return "vhpiDisconnectSpecK";
   case vhpiDriverK: return "vhpiDriverK";
   case vhpiDriverCollectionK: return "vhpiDriverCollectionK";
   case vhpiElemAssocK: return "vhpiElemAssocK";
   case vhpiElemDeclK: return "vhpiElemDeclK";
   case vhpiEntityClassEntryK: return "vhpiEntityClassEntryK";
   case vhpiEntityDeclK: return "vhpiEntityDeclK";
   case vhpiEnumLiteralK: return "vhpiEnumLiteralK";
   case vhpiEnumRangeK: return "vhpiEnumRangeK";
   case vhpiEnumTypeDeclK: return "vhpiEnumTypeDeclK";
   case vhpiExitStmtK: return "vhpiExitStmtK";
   case vhpiFileDeclK: return "vhpiFileDeclK";
   case vhpiFileParamDeclK: return "vhpiFileParamDeclK";
   case vhpiFileTypeDeclK: return "vhpiFileTypeDeclK";
   case vhpiFloatRangeK: return "vhpiFloatRangeK";
   case vhpiFloatTypeDeclK: return "vhpiFloatTypeDeclK";
   case vhpiForGenerateK: return "vhpiForGenerateK";
   case vhpiForLoopK: return "vhpiForLoopK";
   case vhpiForeignfK: return "vhpiForeignfK";
   case vhpiFuncCallK: return "vhpiFuncCallK";
   case vhpiFuncDeclK: return "vhpiFuncDeclK";
   case vhpiGenericDeclK: return "vhpiGenericDeclK";
   case vhpiGroupDeclK: return "vhpiGroupDeclK";
   case vhpiGroupTempDeclK: return "vhpiGroupTempDeclK";
   case vhpiIfGenerateK: return "vhpiIfGenerateK";
   case vhpiIfStmtK: return "vhpiIfStmtK";
   case vhpiInPortK: return "vhpiInPortK";
   case vhpiIndexedNameK: return "vhpiIndexedNameK";
   case vhpiIntLiteralK: return "vhpiIntLiteralK";
   case vhpiIntRangeK: return "vhpiIntRangeK";
   case vhpiIntTypeDeclK: return "vhpiIntTypeDeclK";
   case vhpiIteratorK: return "vhpiIteratorK";
   case vhpiLibraryDeclK: return "vhpiLibraryDeclK";
   case DEPRECATED_vhpiLoopStmtK: return "DEPRECATED_vhpiLoopStmtK";
   case vhpiNextStmtK: return "vhpiNextStmtK";
   case vhpiNullLiteralK: return "vhpiNullLiteralK";
   case vhpiNullStmtK: return "vhpiNullStmtK";
   case DEPRECATED_vhpiOperatorK: return "DEPRECATED_vhpiOperatorK";
   case vhpiOthersK: return "vhpiOthersK";
   case vhpiOutPortK: return "vhpiOutPortK";
   case vhpiPackBodyK: return "vhpiPackBodyK";
   case vhpiPackDeclK: return "vhpiPackDeclK";
   case vhpiPackInstK: return "vhpiPackInstK";
   case vhpiParamAttrNameK: return "vhpiParamAttrNameK";
   case vhpiPhysLiteralK: return "vhpiPhysLiteralK";
   case vhpiPhysRangeK: return "vhpiPhysRangeK";
   case vhpiPhysTypeDeclK: return "vhpiPhysTypeDeclK";
   case vhpiPortDeclK: return "vhpiPortDeclK";
   case DEPRECATED_vhpiProcCallStmtK: return "DEPRECATED_vhpiProcCallStmtK";
   case vhpiProcDeclK: return "vhpiProcDeclK";
   case vhpiProcessStmtK: return "vhpiProcessStmtK";
   case DEPRECATED_vhpiProtectedTypeK: return "DEPRECATED_vhpiProtectedTypeK";
   case vhpiProtectedTypeBodyK: return "vhpiProtectedTypeBodyK";
   case vhpiProtectedTypeDeclK: return "vhpiProtectedTypeDeclK";
   case vhpiRealLiteralK: return "vhpiRealLiteralK";
   case vhpiRecordTypeDeclK: return "vhpiRecordTypeDeclK";
   case vhpiReportStmtK: return "vhpiReportStmtK";
   case vhpiReturnStmtK: return "vhpiReturnStmtK";
   case vhpiRootInstK: return "vhpiRootInstK";
   case vhpiSelectSigAssignStmtK: return "vhpiSelectSigAssignStmtK";
   case vhpiSelectWaveformK: return "vhpiSelectWaveformK";
   case vhpiSelectedNameK: return "vhpiSelectedNameK";
   case vhpiSigDeclK: return "vhpiSigDeclK";
   case vhpiSigParamDeclK: return "vhpiSigParamDeclK";
   case vhpiSimpAttrNameK: return "vhpiSimpAttrNameK";
   case vhpiSimpleSigAssignStmtK: return "vhpiSimpleSigAssignStmtK";
   case vhpiSliceNameK: return "vhpiSliceNameK";
   case vhpiStringLiteralK: return "vhpiStringLiteralK";
   case vhpiSubpBodyK: return "vhpiSubpBodyK";
   case vhpiSubtypeDeclK: return "vhpiSubtypeDeclK";
   case DEPRECATED_vhpiSubtypeIndicK: return "DEPRECATED_vhpiSubtypeIndicK";
   case vhpiToolK: return "vhpiToolK";
   case vhpiTransactionK: return "vhpiTransactionK";
   case vhpiTypeConvK: return "vhpiTypeConvK";
   case DEPRECATED_vhpiUnaryExprK: return "DEPRECATED_vhpiUnaryExprK";
   case vhpiUnitDeclK: return "vhpiUnitDeclK";
   case vhpiUserAttrNameK: return "vhpiUserAttrNameK";
   case vhpiVarAssignStmtK: return "vhpiVarAssignStmtK";
   case vhpiVarDeclK: return "vhpiVarDeclK";
   case vhpiVarParamDeclK: return "vhpiVarParamDeclK";
   case vhpiWaitStmtK: return "vhpiWaitStmtK";
   case vhpiWaveformElemK: return "vhpiWaveformElemK";
   case vhpiWhileLoopK: return "vhpiWhileLoopK";
   case vhpiQualifiedExprK: return "vhpiQualifiedExprK";
   case vhpiUseClauseK: return "vhpiUseClauseK";
   case vhpiConcAssertStmtK: return "vhpiConcAssertStmtK";
   case vhpiConcProcCallStmtK: return "vhpiConcProcCallStmtK";
   case vhpiForeverLoopK: return "vhpiForeverLoopK";
   case vhpiSeqAssertStmtK: return "vhpiSeqAssertStmtK";
   case vhpiSeqProcCallStmtK: return "vhpiSeqProcCallStmtK";
   case vhpiSeqSigAssignStmtK: return "vhpiSeqSigAssignStmtK";
   case vhpiProtectedTypeInstK: return "vhpiProtectedTypeInstK";
   default:
      {
         static char buf[64];
         checked_sprintf(buf, sizeof(buf), "%d", kind);
         return buf;
      }
   }
}

const char *vhpi_one_to_one_str(vhpiOneToOneT kind)
{
   switch (kind) {
   case vhpiAbstractLiteral: return "vhpiAbstractLiteral";
   case vhpiActual: return "vhpiActual";
   case vhpiAll: return "vhpiAll";
   case vhpiAttrDecl: return "vhpiAttrDecl";
   case vhpiAttrSpec: return "vhpiAttrSpec";
   case vhpiBaseType: return "vhpiBaseType";
   case vhpiBaseUnit: return "vhpiBaseUnit";
   case DEPRECATED_vhpiBasicSignal: return "DEPRECATED_vhpiBasicSignal";
   case vhpiBlockConfig: return "vhpiBlockConfig";
   case vhpiCaseExpr: return "vhpiCaseExpr";
   case vhpiCondExpr: return "vhpiCondExpr";
   case vhpiConfigDecl: return "vhpiConfigDecl";
   case vhpiConfigSpec: return "vhpiConfigSpec";
   case vhpiConstraint: return "vhpiConstraint";
   case vhpiContributor: return "vhpiContributor";
   case vhpiCurCallback: return "vhpiCurCallback";
   case DEPRECATED_vhpiCurEqProcess: return "DEPRECATED_vhpiCurEqProcess";
   case vhpiCurStackFrame: return "vhpiCurStackFrame";
   case vhpiDerefObj: return "vhpiDerefObj";
   case DEPRECATED_vhpiDecl: return "DEPRECATED_vhpiDecl";
   case vhpiDesignUnit: return "vhpiDesignUnit";
   case vhpiDownStack: return "vhpiDownStack";
   case DEPRECATED_vhpiElemSubtype: return "DEPRECATED_vhpiElemSubtype";
   case vhpiEntityAspect: return "vhpiEntityAspect";
   case vhpiEntityDecl: return "vhpiEntityDecl";
   case vhpiEqProcessStmt: return "vhpiEqProcessStmt";
   case vhpiExpr: return "vhpiExpr";
   case vhpiFormal: return "vhpiFormal";
   case vhpiFuncDecl: return "vhpiFuncDecl";
   case vhpiGroupTempDecl: return "vhpiGroupTempDecl";
   case vhpiGuardExpr: return "vhpiGuardExpr";
   case vhpiGuardSig: return "vhpiGuardSig";
   case vhpiImmRegion: return "vhpiImmRegion";
   case vhpiInPort: return "vhpiInPort";
   case vhpiInitExpr: return "vhpiInitExpr";
   case DEPRECATED_vhpiIterScheme: return "DEPRECATED_vhpiIterScheme";
   case vhpiLeftExpr: return "vhpiLeftExpr";
   case vhpiLexicalScope: return "vhpiLexicalScope";
   case vhpiLhsExpr: return "vhpiLhsExpr";
   case vhpiLocal: return "vhpiLocal";
   case vhpiLogicalExpr: return "vhpiLogicalExpr";
   case DEPRECATED_vhpiName: return "DEPRECATED_vhpiName";
   case DEPRECATED_vhpiOperator: return "DEPRECATED_vhpiOperator";
   case vhpiOthers: return "vhpiOthers";
   case vhpiOutPort: return "vhpiOutPort";
   case vhpiParamDecl: return "vhpiParamDecl";
   case DEPRECATED_vhpiParamExpr: return "DEPRECATED_vhpiParamExpr";
   case vhpiParent: return "vhpiParent";
   case vhpiPhysLiteral: return "vhpiPhysLiteral";
   case vhpiPrefix: return "vhpiPrefix";
   case vhpiPrimaryUnit: return "vhpiPrimaryUnit";
   case vhpiProtectedTypeBody: return "vhpiProtectedTypeBody";
   case vhpiProtectedTypeDecl: return "vhpiProtectedTypeDecl";
   case vhpiRejectTime: return "vhpiRejectTime";
   case vhpiReportExpr: return "vhpiReportExpr";
   case vhpiResolFunc: return "vhpiResolFunc";
   case vhpiReturnExpr: return "vhpiReturnExpr";
   case DEPRECATED_vhpiReturnTypeMark: return "DEPRECATED_vhpiReturnTypeMark";
   case vhpiRhsExpr: return "vhpiRhsExpr";
   case vhpiRightExpr: return "vhpiRightExpr";
   case vhpiRootInst: return "vhpiRootInst";
   case vhpiSelectExpr: return "vhpiSelectExpr";
   case vhpiSeverityExpr: return "vhpiSeverityExpr";
   case vhpiSimpleName: return "vhpiSimpleName";
   case vhpiSubpBody: return "vhpiSubpBody";
   case vhpiSubpDecl: return "vhpiSubpDecl";
   case DEPRECATED_vhpiSubtype: return "DEPRECATED_vhpiSubtype";
   case vhpiSuffix: return "vhpiSuffix";
   case vhpiTimeExpr: return "vhpiTimeExpr";
   case vhpiTimeOutExpr: return "vhpiTimeOutExpr";
   case vhpiTool: return "vhpiTool";
   case vhpiType: return "vhpiType";
   case DEPRECATED_vhpiTypeMark: return "DEPRECATED_vhpiTypeMark";
   case vhpiUnitDecl: return "vhpiUnitDecl";
   case vhpiUpStack: return "vhpiUpStack";
   case vhpiUpperRegion: return "vhpiUpperRegion";
   case vhpiUse: return "vhpiUse";
   case vhpiValExpr: return "vhpiValExpr";
   case DEPRECATED_vhpiValSubtype: return "DEPRECATED_vhpiValSubtype";
   case vhpiElemType: return "vhpiElemType";
   case vhpiFirstNamedType: return "vhpiFirstNamedType";
   case vhpiReturnType: return "vhpiReturnType";
   case vhpiValType: return "vhpiValType";
   case vhpiCurRegion: return "vhpiCurRegion";
   case vhpiSignal: return "vhpiSignal";
   case vhpiLibraryDecl: return "vhpiLibraryDecl";
   case vhpiSimNet: return "vhpiSimNet";
   case vhpiAliasedName: return "vhpiAliasedName";
   case vhpiCompDecl: return "vhpiCompDecl";
   case vhpiProtectedTypeInst: return "vhpiProtectedTypeInst";
   case vhpiGenIndex: return "vhpiGenIndex";
   default:
      {
         static char buf[64];
         checked_sprintf(buf, sizeof(buf), "%d", kind);
         return buf;
      }
   }
}

const char *vhpi_one_to_many_str(vhpiOneToManyT kind)
{
   switch (kind) {
   case vhpiAliasDecls: return "vhpiAliasDecls";
   case vhpiArgvs: return "vhpiArgvs";
   case vhpiAttrDecls: return "vhpiAttrDecls";
   case vhpiAttrSpecs: return "vhpiAttrSpecs";
   case vhpiBasicSignals: return "vhpiBasicSignals";
   case vhpiBlockStmts: return "vhpiBlockStmts";
   case vhpiBranchs: return "vhpiBranchs";
   case vhpiChoices: return "vhpiChoices";
   case vhpiCompInstStmts: return "vhpiCompInstStmts";
   case DEPRECATED_vhpiCondExprs: return "DEPRECATED_vhpiCondExprs";
   case vhpiCondWaveforms: return "vhpiCondWaveforms";
   case vhpiConfigItems: return "vhpiConfigItems";
   case vhpiConfigSpecs: return "vhpiConfigSpecs";
   case vhpiConstDecls: return "vhpiConstDecls";
   case vhpiConstraints: return "vhpiConstraints";
   case DEPRECATED_vhpiContributors: return "DEPRECATED_vhpiContributors";
   case vhpiDecls: return "vhpiDecls";
   case vhpiDepUnits: return "vhpiDepUnits";
   case vhpiDesignUnits: return "vhpiDesignUnits";
   case vhpiDrivenSigs: return "vhpiDrivenSigs";
   case vhpiDrivers: return "vhpiDrivers";
   case vhpiElemAssocs: return "vhpiElemAssocs";
   case DEPRECATED_vhpiEntityClassEntrys: return "DEPRECATED_vhpiEntityClassEntrys";
   case vhpiEntityDesignators: return "vhpiEntityDesignators";
   case vhpiEnumLiterals: return "vhpiEnumLiterals";
   case vhpiForeignfs: return "vhpiForeignfs";
   case vhpiGenericAssocs: return "vhpiGenericAssocs";
   case vhpiGenericDecls: return "vhpiGenericDecls";
   case vhpiIndexExprs: return "vhpiIndexExprs";
   case vhpiIndexedNames: return "vhpiIndexedNames";
   case vhpiInternalRegions: return "vhpiInternalRegions";
   case vhpiMembers: return "vhpiMembers";
   case vhpiPackInsts: return "vhpiPackInsts";
   case vhpiParamAssocs: return "vhpiParamAssocs";
   case vhpiParamDecls: return "vhpiParamDecls";
   case vhpiPortAssocs: return "vhpiPortAssocs";
   case vhpiPortDecls: return "vhpiPortDecls";
   case vhpiRecordElems: return "vhpiRecordElems";
   case vhpiSelectWaveforms: return "vhpiSelectWaveforms";
   case vhpiSelectedNames: return "vhpiSelectedNames";
   case DEPRECATED_vhpiSensitivitys: return "DEPRECATED_vhpiSensitivitys";
   case vhpiSeqStmts: return "vhpiSeqStmts";
   case vhpiSigAttrs: return "vhpiSigAttrs";
   case vhpiSigDecls: return "vhpiSigDecls";
   case vhpiSigNames: return "vhpiSigNames";
   case vhpiSignals: return "vhpiSignals";
   case DEPRECATED_vhpiSpecNames: return "DEPRECATED_vhpiSpecNames";
   case vhpiSpecs: return "vhpiSpecs";
   case vhpiStmts: return "vhpiStmts";
   case vhpiTransactions: return "vhpiTransactions";
   case DEPRECATED_vhpiTypeMarks: return "DEPRECATED_vhpiTypeMarks";
   case vhpiUnitDecls: return "vhpiUnitDecls";
   case vhpiUses: return "vhpiUses";
   case vhpiVarDecls: return "vhpiVarDecls";
   case vhpiWaveformElems: return "vhpiWaveformElems";
   case vhpiLibraryDecls: return "vhpiLibraryDecls";
   case vhpiLocalLoads: return "vhpiLocalLoads";
   case vhpiOptimizedLoads: return "vhpiOptimizedLoads";
   case vhpiTypes: return "vhpiTypes";
   case vhpiUseClauses: return "vhpiUseClauses";
   case vhpiGenerateStmts: return "vhpiGenerateStmts";
   case vhpiLocalContributors: return "vhpiLocalContributors";
   case vhpiOptimizedContributors: return "vhpiOptimizedContributors";
   case vhpiParamExprs: return "vhpiParamExprs";
   case vhpiEqProcessStmts: return "vhpiEqProcessStmts";
   case vhpiEntityClassEntries: return "vhpiEntityClassEntries";
   case vhpiSensitivities: return "vhpiSensitivities";
   default:
      {
         static char buf[64];
         checked_sprintf(buf, sizeof(buf), "%d", kind);
         return buf;
      }
   }
}

const char *vhpi_cb_reason_str(int reason)
{
   switch (reason) {
   case vhpiCbValueChange: return "vhpiCbValueChange";
   case vhpiCbForce: return "vhpiCbForce";
   case vhpiCbRelease: return "vhpiCbRelease";
   case vhpiCbTransaction: return "vhpiCbTransaction";
   case vhpiCbStmt: return "vhpiCbStmt";
   case vhpiCbResume: return "vhpiCbResume";
   case vhpiCbSuspend: return "vhpiCbSuspend";
   case vhpiCbStartOfSubpCall: return "vhpiCbStartOfSubpCall";
   case vhpiCbEndOfSubpCall: return "vhpiCbEndOfSubpCall";
   case vhpiCbAfterDelay: return "vhpiCbAfterDelay";
   case vhpiCbRepAfterDelay: return "vhpiCbRepAfterDelay";
   case vhpiCbNextTimeStep: return "vhpiCbNextTimeStep";
   case vhpiCbRepNextTimeStep: return "vhpiCbRepNextTimeStep";
   case vhpiCbStartOfNextCycle: return "vhpiCbStartOfNextCycle";
   case vhpiCbRepStartOfNextCycle: return "vhpiCbRepStartOfNextCycle";
   case vhpiCbStartOfProcesses: return "vhpiCbStartOfProcesses";
   case vhpiCbRepStartOfProcesses: return "vhpiCbRepStartOfProcesses";
   case vhpiCbEndOfProcesses: return "vhpiCbEndOfProcesses";
   case vhpiCbRepEndOfProcesses: return "vhpiCbRepEndOfProcesses";
   case vhpiCbLastKnownDeltaCycle: return "vhpiCbLastKnownDeltaCycle";
   case vhpiCbRepLastKnownDeltaCycle: return "vhpiCbRepLastKnownDeltaCycle";
   case vhpiCbStartOfPostponed: return "vhpiCbStartOfPostponed";
   case vhpiCbRepStartOfPostponed: return "vhpiCbRepStartOfPostponed";
   case vhpiCbEndOfTimeStep: return "vhpiCbEndOfTimeStep";
   case vhpiCbRepEndOfTimeStep: return "vhpiCbRepEndOfTimeStep";
   case vhpiCbStartOfTool: return "vhpiCbStartOfTool";
   case vhpiCbEndOfTool: return "vhpiCbEndOfTool";
   case vhpiCbStartOfAnalysis: return "vhpiCbStartOfAnalysis";
   case vhpiCbEndOfAnalysis: return "vhpiCbEndOfAnalysis";
   case vhpiCbStartOfElaboration: return "vhpiCbStartOfElaboration";
   case vhpiCbEndOfElaboration: return "vhpiCbEndOfElaboration";
   case vhpiCbStartOfInitialization: return "vhpiCbStartOfInitialization";
   case vhpiCbEndOfInitialization: return "vhpiCbEndOfInitialization";
   case vhpiCbStartOfSimulation: return "vhpiCbStartOfSimulation";
   case vhpiCbEndOfSimulation: return "vhpiCbEndOfSimulation";
   case vhpiCbQuiescense: return "vhpiCbQuiescense";
   case vhpiCbPLIError: return "vhpiCbPLIError";
   case vhpiCbStartOfSave: return "vhpiCbStartOfSave";
   case vhpiCbEndOfSave: return "vhpiCbEndOfSave";
   case vhpiCbStartOfRestart: return "vhpiCbStartOfRestart";
   case vhpiCbEndOfRestart: return "vhpiCbEndOfRestart";
   case vhpiCbStartOfReset: return "vhpiCbStartOfReset";
   case vhpiCbEndOfReset: return "vhpiCbEndOfReset";
   case vhpiCbEnterInteractive: return "vhpiCbEnterInteractive";
   case vhpiCbExitInteractive: return "vhpiCbExitInteractive";
   case vhpiCbSigInterrupt: return "vhpiCbSigInterrupt";
   case vhpiCbTimeOut: return "vhpiCbTimeOut";
   case vhpiCbRepTimeOut: return "vhpiCbRepTimeOut";
   case vhpiCbSensitivity: return "vhpiCbSensitivity";
   default:
      {
         static char buf[64];
         checked_sprintf(buf, sizeof(buf), "%d", reason);
         return buf;
      }
   }
}
