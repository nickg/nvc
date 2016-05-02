entity jcore1 is
end entity;

architecture test of jcore1 is
    type macin1_sel_t is (SEL_XBUS, SEL_ZBUS, SEL_WBUS);
    type macin2_sel_t is (SEL_YBUS, SEL_ZBUS, SEL_WBUS);
    type mult_state_t is (NOP, DMULSL, DMULSL1, DMULSL2, DMULUL, DMULUL1, DMULUL2, MACL, MACL1, MACL2, MACW, MACW1, MACWS, MACWS1, MULL, MULL1, MULL2, MULSW, MULSW1, MULUW, MULUW1);
    type zbus_sel_t is (SEL_ARITH, SEL_LOGIC, SEL_SHIFT, SEL_MANIP, SEL_YBUS, SEL_WBUS);
    type sr_sel_t is (SEL_PREV, SEL_WBUS, SEL_ZBUS, SEL_DIV0U, SEL_ARITH, SEL_LOGIC, SEL_INT_MASK, SEL_SET_T);
    type t_sel_t is (SEL_CLEAR, SEL_SET, SEL_SHIFT, SEL_CARRY);
    type mem_addr_sel_t is (SEL_XBUS, SEL_YBUS, SEL_ZBUS);

    type pipeline_wb_stall_t is
        record
            mulcom1 : std_logic;
            wrmach, wrmacl : std_logic;
            wrreg_w, wrsr_w : std_logic;
            macsel1 : macin1_sel_t;
            macsel2 : macin2_sel_t;
            mulcom2 : mult_state_t;
        end record;

    type pipeline_ex_stall_t is
        record
            wrpc_z : std_logic;
            wrsr_z : std_logic;
            ma_issue : std_logic;
            wrpr_pc : std_logic;
            zbus_sel : zbus_sel_t;
            sr_sel : sr_sel_t;
            t_sel : t_sel_t;
            mem_addr_sel : mem_addr_sel_t;
            mem_wdata_sel : mem_wdata_sel_t;
            wrreg_z : std_logic;
            wrmach, wrmacl : std_logic;
            shiftfunc : shiftfunc_t;
            mulcom1 : std_logic;
            mulcom2 : mult_state_t;
            macsel1 : macin1_sel_t;
            macsel2 : macin2_sel_t;
        end record;

    type pipeline_t is
        record
            ex1 : pipeline_ex_t;
            ex1_stall : pipeline_ex_stall_t;
            wb1 : pipeline_wb_t;
            wb2 : pipeline_wb_t;
            wb3 : pipeline_wb_t;
            wb1_stall : pipeline_wb_stall_t;
            wb2_stall : pipeline_wb_stall_t;
            wb3_stall : pipeline_wb_stall_t;
        end record;

    signal p : pipeline_t;              -- Used to assert in make_default_value
begin

end architecture;
