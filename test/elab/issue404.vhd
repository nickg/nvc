entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is
    type      ADDR_SIGNALS_TYPE is record
        ADDR     : bit_vector(31 downto 0);
    end record;
    constant  ADDR_SIGNALS_DONTCARE : ADDR_SIGNALS_TYPE := (
        ADDR    => (others => '0')
    );
    type      DATA_SIGNALS_TYPE is record
        DATA     : bit_vector(31 downto 0);
    end record;
    constant  DATA_SIGNALS_DONTCARE : DATA_SIGNALS_TYPE := (
        DATA    => (others => '0')
    );
    type      SIGNALS_TYPE is record
        ADDR     : ADDR_SIGNALS_TYPE;
        DATA     : DATA_SIGNALS_TYPE;
    end record;
    constant  SIGNALS_DONTCARE : SIGNALS_TYPE := (
        ADDR    => ADDR_SIGNALS_DONTCARE,
        DATA    => DATA_SIGNALS_DONTCARE
    );
    function  GEN_INIT_SIGNALS return SIGNALS_TYPE is
    begin
        return SIGNALS_DONTCARE;
    end function;
    signal    signals       : SIGNALS_TYPE;
begin
    signals <= GEN_INIT_SIGNALS;   -- Crash here in eval_op_copy
end MODEL;
