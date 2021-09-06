-- Generator : SpinalHDL v1.6.0    git head : 73c8d8e2b86b45646e9d0b2e729291f2b65e6be3
-- Component : Murax
-- Git hash  : 68e704f3092be640aa92c876cf78702a83167f94

package pkg_enum is
  type BranchCtrlEnum is (INC,B,JAL,JALR);
  type ShiftCtrlEnum is (DISABLE_1,SLL_1,SRL_1,SRA_1);
  type AluBitwiseCtrlEnum is (XOR_1,OR_1,AND_1);
  type AluCtrlEnum is (ADD_SUB,SLT_SLTU,BITWISE);
  type EnvCtrlEnum is (NONE,XRET);
  type Src2CtrlEnum is (RS,IMI,IMS,PC);
  type Src1CtrlEnum is (RS,IMU,PC_INCREMENT,URS1);
  type JtagState is (RESET,IDLE,IR_SELECT,IR_CAPTURE,IR_SHIFT,IR_EXIT1,IR_PAUSE,IR_EXIT2,IR_UPDATE,DR_SELECT,DR_CAPTURE,DR_SHIFT,DR_EXIT1,DR_PAUSE,DR_EXIT2,DR_UPDATE);
  type UartStopType is (ONE,TWO);
  type UartParityType is (NONE,EVEN,ODD);
  type UartCtrlTxState is (IDLE,START,DATA,PARITY,STOP);
  type UartCtrlRxState is (IDLE,START,DATA,PARITY,STOP);

  function pkg_mux (sel : bit; one : BranchCtrlEnum; zero : BranchCtrlEnum) return BranchCtrlEnum;
  subtype BranchCtrlEnum_binary_sequential_type is bit_vector(1 downto 0);
  constant BranchCtrlEnum_binary_sequential_INC : BranchCtrlEnum_binary_sequential_type := "00";
  constant BranchCtrlEnum_binary_sequential_B : BranchCtrlEnum_binary_sequential_type := "01";
  constant BranchCtrlEnum_binary_sequential_JAL : BranchCtrlEnum_binary_sequential_type := "10";
  constant BranchCtrlEnum_binary_sequential_JALR : BranchCtrlEnum_binary_sequential_type := "11";

  function pkg_mux (sel : bit; one : ShiftCtrlEnum; zero : ShiftCtrlEnum) return ShiftCtrlEnum;
  subtype ShiftCtrlEnum_binary_sequential_type is bit_vector(1 downto 0);
  constant ShiftCtrlEnum_binary_sequential_DISABLE_1 : ShiftCtrlEnum_binary_sequential_type := "00";
  constant ShiftCtrlEnum_binary_sequential_SLL_1 : ShiftCtrlEnum_binary_sequential_type := "01";
  constant ShiftCtrlEnum_binary_sequential_SRL_1 : ShiftCtrlEnum_binary_sequential_type := "10";
  constant ShiftCtrlEnum_binary_sequential_SRA_1 : ShiftCtrlEnum_binary_sequential_type := "11";

  function pkg_mux (sel : bit; one : AluBitwiseCtrlEnum; zero : AluBitwiseCtrlEnum) return AluBitwiseCtrlEnum;
  subtype AluBitwiseCtrlEnum_binary_sequential_type is bit_vector(1 downto 0);
  constant AluBitwiseCtrlEnum_binary_sequential_XOR_1 : AluBitwiseCtrlEnum_binary_sequential_type := "00";
  constant AluBitwiseCtrlEnum_binary_sequential_OR_1 : AluBitwiseCtrlEnum_binary_sequential_type := "01";
  constant AluBitwiseCtrlEnum_binary_sequential_AND_1 : AluBitwiseCtrlEnum_binary_sequential_type := "10";

  function pkg_mux (sel : bit; one : AluCtrlEnum; zero : AluCtrlEnum) return AluCtrlEnum;
  subtype AluCtrlEnum_binary_sequential_type is bit_vector(1 downto 0);
  constant AluCtrlEnum_binary_sequential_ADD_SUB : AluCtrlEnum_binary_sequential_type := "00";
  constant AluCtrlEnum_binary_sequential_SLT_SLTU : AluCtrlEnum_binary_sequential_type := "01";
  constant AluCtrlEnum_binary_sequential_BITWISE : AluCtrlEnum_binary_sequential_type := "10";

  function pkg_mux (sel : bit; one : EnvCtrlEnum; zero : EnvCtrlEnum) return EnvCtrlEnum;
  subtype EnvCtrlEnum_binary_sequential_type is bit_vector(0 downto 0);
  constant EnvCtrlEnum_binary_sequential_NONE : EnvCtrlEnum_binary_sequential_type := "0";
  constant EnvCtrlEnum_binary_sequential_XRET : EnvCtrlEnum_binary_sequential_type := "1";

  function pkg_mux (sel : bit; one : Src2CtrlEnum; zero : Src2CtrlEnum) return Src2CtrlEnum;
  subtype Src2CtrlEnum_binary_sequential_type is bit_vector(1 downto 0);
  constant Src2CtrlEnum_binary_sequential_RS : Src2CtrlEnum_binary_sequential_type := "00";
  constant Src2CtrlEnum_binary_sequential_IMI : Src2CtrlEnum_binary_sequential_type := "01";
  constant Src2CtrlEnum_binary_sequential_IMS : Src2CtrlEnum_binary_sequential_type := "10";
  constant Src2CtrlEnum_binary_sequential_PC : Src2CtrlEnum_binary_sequential_type := "11";

  function pkg_mux (sel : bit; one : Src1CtrlEnum; zero : Src1CtrlEnum) return Src1CtrlEnum;
  subtype Src1CtrlEnum_binary_sequential_type is bit_vector(1 downto 0);
  constant Src1CtrlEnum_binary_sequential_RS : Src1CtrlEnum_binary_sequential_type := "00";
  constant Src1CtrlEnum_binary_sequential_IMU : Src1CtrlEnum_binary_sequential_type := "01";
  constant Src1CtrlEnum_binary_sequential_PC_INCREMENT : Src1CtrlEnum_binary_sequential_type := "10";
  constant Src1CtrlEnum_binary_sequential_URS1 : Src1CtrlEnum_binary_sequential_type := "11";

  function pkg_mux (sel : bit; one : JtagState; zero : JtagState) return JtagState;
  function pkg_toStdLogicVector_native (value : JtagState) return bit_vector;
  function pkg_toJtagState_native (value : bit_vector(3 downto 0)) return JtagState;
  function pkg_mux (sel : bit; one : UartStopType; zero : UartStopType) return UartStopType;
  subtype UartStopType_binary_sequential_type is bit_vector(0 downto 0);
  constant UartStopType_binary_sequential_ONE : UartStopType_binary_sequential_type := "0";
  constant UartStopType_binary_sequential_TWO : UartStopType_binary_sequential_type := "1";

  function pkg_mux (sel : bit; one : UartParityType; zero : UartParityType) return UartParityType;
  subtype UartParityType_binary_sequential_type is bit_vector(1 downto 0);
  constant UartParityType_binary_sequential_NONE : UartParityType_binary_sequential_type := "00";
  constant UartParityType_binary_sequential_EVEN : UartParityType_binary_sequential_type := "01";
  constant UartParityType_binary_sequential_ODD : UartParityType_binary_sequential_type := "10";

  function pkg_mux (sel : bit; one : UartCtrlTxState; zero : UartCtrlTxState) return UartCtrlTxState;
  function pkg_toStdLogicVector_native (value : UartCtrlTxState) return bit_vector;
  function pkg_toUartCtrlTxState_native (value : bit_vector(2 downto 0)) return UartCtrlTxState;
  function pkg_mux (sel : bit; one : UartCtrlRxState; zero : UartCtrlRxState) return UartCtrlRxState;
  function pkg_toStdLogicVector_native (value : UartCtrlRxState) return bit_vector;
  function pkg_toUartCtrlRxState_native (value : bit_vector(2 downto 0)) return UartCtrlRxState;
end pkg_enum;

package pkg_scala2hdl is
  function pkg_extract (that : bit_vector; bitId : integer) return bit;

  type unsigned is array (natural range <>) of bit;
  type signed is array (natural range <>) of bit;

  function pkg_cat (a : signed; b : signed) return signed;
  function pkg_not (value : signed) return signed;

  function pkg_mux (sel : bit; one : bit; zero : bit) return bit;
  function pkg_mux (sel : bit; one : bit_vector; zero : bit_vector) return bit_vector;
  function pkg_mux (sel : bit; one : unsigned; zero : unsigned) return unsigned;
  function pkg_mux (sel : bit; one : signed; zero : signed) return signed;

  function pkg_toStdLogic (value : boolean) return bit;
  function pkg_toStdLogicVector (value : bit) return bit_vector;

end  pkg_scala2hdl;

library work;
use work.pkg_scala2hdl.all;
use work.all;
use work.pkg_enum.all;


entity UartCtrlTx is
  port(
    io_configFrame_dataLength : in unsigned(2 downto 0);
    io_configFrame_stop : in UartStopType_binary_sequential_type;
    io_configFrame_parity : in UartParityType_binary_sequential_type;
    io_samplingTick : in bit;
    io_write_valid : in bit;
    io_write_ready : out bit;
    io_write_payload : in bit_vector(7 downto 0);
    io_cts : in bit;
    io_txd : out bit;
    io_break : in bit;
    io_mainClk : in bit;
    resetCtrl_systemReset : in bit
  );
end UartCtrlTx;

architecture arch of UartCtrlTx is

  signal clockDivider_counter_willIncrement : bit;
  signal clockDivider_counter_willClear : bit;
  signal clockDivider_counter_valueNext : unsigned(2 downto 0);
  signal clockDivider_counter_value : unsigned(2 downto 0);
  signal clockDivider_counter_willOverflowIfInc : bit;
  signal clockDivider_counter_willOverflow : bit;
  signal tickCounter_value : unsigned(2 downto 0);
  signal stateMachine_state : UartCtrlTxState;
  signal stateMachine_parity : bit;
  signal stateMachine_txd : bit;
  signal when_UartCtrlTx_l58 : bit;
  signal when_UartCtrlTx_l73 : bit;
  signal when_UartCtrlTx_l76 : bit;
  signal when_UartCtrlTx_l93 : bit;
  signal zz_io_txd : bit;
begin
  process(io_samplingTick)
  begin
    clockDivider_counter_willIncrement <= pkg_toStdLogic(false);
    if io_samplingTick = '1' then
      clockDivider_counter_willIncrement <= pkg_toStdLogic(true);
    end if;
  end process;

  clockDivider_counter_willClear <= pkg_toStdLogic(false);

  process(stateMachine_state,io_write_payload,tickCounter_value,stateMachine_parity)
  begin
    stateMachine_txd <= pkg_toStdLogic(true);
    case stateMachine_state is
      when pkg_enum.IDLE =>
      when pkg_enum.START =>
        stateMachine_txd <= pkg_toStdLogic(false);
      when pkg_enum.DATA =>
      when pkg_enum.PARITY =>
        stateMachine_txd <= stateMachine_parity;
      when others =>
    end case;
  end process;

  process(io_break,stateMachine_state,clockDivider_counter_willOverflow,when_UartCtrlTx_l73)
  begin
    io_write_ready <= io_break;
    case stateMachine_state is
      when pkg_enum.IDLE =>
      when pkg_enum.START =>
      when pkg_enum.DATA =>
        if clockDivider_counter_willOverflow = '1' then
          if when_UartCtrlTx_l73 = '1' then
            io_write_ready <= pkg_toStdLogic(true);
          end if;
        end if;
      when pkg_enum.PARITY =>
      when others =>
    end case;
  end process;

  io_txd <= zz_io_txd;
  process(io_mainClk, resetCtrl_systemReset)
  begin
    if resetCtrl_systemReset = '1' then
      stateMachine_state <= pkg_enum.IDLE;
      zz_io_txd <= pkg_toStdLogic(true);
    elsif io_mainClk'active and io_mainClk = '1' then
      clockDivider_counter_value <= clockDivider_counter_valueNext;
      case stateMachine_state is
        when pkg_enum.IDLE =>
          if when_UartCtrlTx_l58 = '1' then
            stateMachine_state <= pkg_enum.START;
          end if;
        when pkg_enum.START =>
          if clockDivider_counter_willOverflow = '1' then
            stateMachine_state <= pkg_enum.DATA;
          end if;
        when pkg_enum.DATA =>
          if clockDivider_counter_willOverflow = '1' then
            if when_UartCtrlTx_l73 = '1' then
              if when_UartCtrlTx_l76 = '1' then
                stateMachine_state <= pkg_enum.STOP;
              else
                stateMachine_state <= pkg_enum.PARITY;
              end if;
            end if;
          end if;
        when pkg_enum.PARITY =>
          if clockDivider_counter_willOverflow = '1' then
            stateMachine_state <= pkg_enum.STOP;
          end if;
        when others =>
          if clockDivider_counter_willOverflow = '1' then
            if when_UartCtrlTx_l93 = '1' then
              stateMachine_state <= pkg_mux(io_write_valid,pkg_enum.START,pkg_enum.IDLE);
            end if;
          end if;
      end case;
      zz_io_txd <= (stateMachine_txd and (not io_break));
    end if;
  end process;
end arch;
