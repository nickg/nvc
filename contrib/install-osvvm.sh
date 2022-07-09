#!/usr/bin/env bash
#
# Called by "nvc --install osvvm".
#

. $(dirname $BASH_SOURCE)/functions.sh

git_wrapper https://github.com/osvvm/OsvvmLibraries 2022.04

STD=2008

analyse_list osvvm <<EOF
osvvm/NamePkg.vhd
osvvm/OsvvmGlobalPkg.vhd
osvvm/TranscriptPkg.vhd
osvvm/TextUtilPkg.vhd
osvvm/AlertLogPkg.vhd
osvvm/SortListPkg_int.vhd
osvvm/RandomBasePkg.vhd
osvvm/RandomPkg.vhd
osvvm/RandomProcedurePkg.vhd
osvvm/MessagePkg.vhd
osvvm/ResolutionPkg.vhd
osvvm/NameStorePkg.vhd
osvvm/MessageListPkg.vhd
osvvm/VendorCovApiPkg.vhd
osvvm/CoveragePkg.vhd
osvvm/MemoryPkg.vhd
osvvm/ScoreboardGenericPkg.vhd
osvvm/ScoreboardPkg_int.vhd
osvvm/ScoreboardPkg_slv.vhd
osvvm/ReportPkg.vhd
osvvm/ResizePkg.vhd
osvvm/TbUtilPkg.vhd
osvvm/OsvvmTypesPkg.vhd
osvvm/OsvvmContext.vhd
EOF

analyse_list osvvm_common <<EOF 
Common/src/FifoFillPkg_slv.vhd
Common/src/FifoFillPtPkg_slv.vhd
Common/src/AddressBusTransactionPkg.vhd
Common/src/AddressBusResponderTransactionPkg.vhd
Common/src/AddressBusVersionCompatibilityPkg.vhd
Common/src/InterruptHandlerComponentPkg.vhd
Common/src/InterruptHandler.vhd
Common/src/ModelParametersPkg.vhd
Common/src/StreamTransactionPkg.vhd
Common/src/OsvvmCommonContext.vhd
EOF

analyse_list osvvm_uart <<EOF
UART/src/UartTbPkg.vhd
UART/src/ScoreboardPkg_Uart.vhd
UART/src/UartRxComponentPkg.vhd
UART/src/UartRx.vhd
UART/src/UartTxComponentPkg.vhd
UART/src/UartTx.vhd
UART/src/UartContext.vhd
EOF

analyse_list osvvm_axi4 <<EOF
AXI4/common/src/Axi4CommonPkg.vhd
AXI4/common/src/Axi4InterfaceCommonPkg.vhd
AXI4/common/src/Axi4InterfacePkg.vhd
AXI4/common/src/Axi4LiteInterfacePkg.vhd
AXI4/common/src/Axi4ModelPkg.vhd
AXI4/common/src/Axi4OptionsPkg.vhd
AXI4/common/src/Axi4VersionCompatibilityPkg.vhd
EOF

# Not working yet
#analyse_list osvvm_axi4 <<EOF
#AXI4/Axi4Lite/src/Axi4LiteComponentPkg.vhd
#AXI4/Axi4Lite/src/Axi4LiteManager.vhd
#AXI4/Axi4Lite/src/Axi4LiteMonitor_dummy.vhd
#AXI4/Axi4Lite/src/Axi4LiteContext.vhd
#AXI4/Axi4Lite/src/Axi4LiteMemory.vhd
#AXI4/Axi4Lite/src/Axi4LiteSubordinate.vhd
#EOF
