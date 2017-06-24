entity  DUMMY is
    generic(
        BITS: integer := 1
    );
    port(
        I: in  bit_vector(BITS-1 downto 0);
        O: out bit_vector(BITS-1 downto 0)
    );
end DUMMY;
architecture MODEL of DUMMY is
begin
    O <= I;
end MODEL;

-- test_ng_comp.vhd
entity  TEST_NG_COMP is
    generic (
        INFO_BITS  : integer :=  1
    );
    port (
        I_INFO_0   : in  bit_vector(INFO_BITS-1 downto 0);
        I_INFO_1   : in  bit_vector(INFO_BITS-1 downto 0);
        O_INFO_0   : out bit_vector(INFO_BITS-1 downto 0);
        O_INFO_1   : out bit_vector(INFO_BITS-1 downto 0)
    );
end     TEST_NG_COMP;
architecture MODEL of TEST_NG_COMP is
    type      INFO_RANGE_TYPE is record
              DATA_LO           : integer;
              DATA_HI           : integer;
    end record;
    type      VEC_RANGE_TYPE is record
              DATA_LO           : integer;
              DATA_HI           : integer;
              INFO_0            : INFO_RANGE_TYPE;
              INFO_1            : INFO_RANGE_TYPE;
    end record;
    function  SET_VEC_RANGE return VEC_RANGE_TYPE is
        variable  d_pos         : integer;
        variable  v             : VEC_RANGE_TYPE;
        procedure SET_INFO_RANGE(INFO_RANGE: inout INFO_RANGE_TYPE; BITS: in integer) is
        begin
            INFO_RANGE.DATA_LO  := d_pos;
            INFO_RANGE.DATA_HI  := d_pos + BITS-1;
            d_pos := d_pos + BITS;
        end procedure;
    begin
        d_pos := 0;
        v.DATA_LO := d_pos;
        SET_INFO_RANGE(v.INFO_0, INFO_BITS);
        SET_INFO_RANGE(v.INFO_1, INFO_BITS);
        v.DATA_HI := d_pos - 1;
        return v;
    end function;
    constant  VEC_RANGE      : VEC_RANGE_TYPE  := SET_VEC_RANGE;
    signal    i_data         : bit_vector(VEC_RANGE.DATA_HI downto VEC_RANGE.DATA_LO);
    component DUMMY
        generic(
            BITS: integer := 1
        );
        port(
            I: in  bit_vector(BITS-1 downto 0);
            O: out bit_vector(BITS-1 downto 0)
        );
    end component;
begin
    I0: DUMMY generic map(BITS => INFO_BITS) port map(
        I => I_INFO_0,
        O => i_data(VEC_RANGE.INFO_0.DATA_HI downto VEC_RANGE.INFO_0.DATA_LO)
    );
    I1: DUMMY generic map(BITS => INFO_BITS) port map(
        I => I_INFO_1,
        O => i_data(VEC_RANGE.INFO_1.DATA_HI downto VEC_RANGE.INFO_1.DATA_LO)
    );
    O_INFO_0 <= i_data(VEC_RANGE.INFO_0.DATA_HI downto VEC_RANGE.INFO_0.DATA_LO);
    O_INFO_1 <= i_data(VEC_RANGE.INFO_1.DATA_HI downto VEC_RANGE.INFO_1.DATA_LO);
end MODEL;
