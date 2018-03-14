-- test_ng.vhd

entity  SUB_NG is
    generic (
        DATA_BITS  :  integer := 32
    );
    port (
        I_DATA     :  in  bit_vector(DATA_BITS-1 downto 0);
        O_DATA     :  out bit_vector(DATA_BITS-1 downto 0)
    );
end SUB_NG;
architecture MODEL of SUB_NG is
begin
    T2M: block
        type      INFO_RANGE_TYPE is record
                  DATA_LO   : integer;
                  DATA_HI   : integer;
                  BITS      : integer;
        end record;

        function  SET_INFO_RANGE return INFO_RANGE_TYPE is
            variable param : INFO_RANGE_TYPE;
            variable index : integer;
        begin
            index := 0;
            param.DATA_LO := index;
            param.DATA_HI := index + DATA_BITS - 1;
            index := index + DATA_BITS;
            param.BITS    := index;
            return param;
        end function;

        constant  INFO_RANGE   : INFO_RANGE_TYPE := SET_INFO_RANGE;
        signal    i_info       : bit_vector(INFO_RANGE.BITS-1 downto 0);
        signal    o_info       : bit_vector(INFO_RANGE.BITS-1 downto 0);
    begin
        i_info(INFO_RANGE.DATA_HI downto INFO_RANGE.DATA_LO) <= I_DATA;
        o_info <= i_info;
        O_DATA <= o_info(INFO_RANGE.DATA_HI downto INFO_RANGE.DATA_LO);
    end block;
end MODEL;

entity  issue374 is
end     issue374;
architecture MODEL of issue374 is
    signal    data_0   :  bit_vector(12 downto 0);
    signal    data_1   :  bit_vector(13 downto 0);
begin
    U0: entity WORK.SUB_NG
        generic map (
            DATA_BITS  => data_0'length
        )
        port map (
            I_DATA     => data_0
        );
    U1: entity WORK.SUB_NG
        generic map (
            DATA_BITS  => data_1'length
        )
        port map (
            I_DATA     => data_1
        );
end MODEL;
