entity issue315 is
    generic (
        DATA_BITS  : integer := 32
    );
end entity;

architecture test of issue315 is
    type      INFO_TYPE is record
              DATA_LO   : integer;
              DATA_HI   : integer;
              BITS      : integer;
    end record;
    function  SET_INFO return INFO_TYPE is
        variable info   : INFO_TYPE;
        variable index  : integer;
    begin
        index        := 0;
        info.DATA_LO := index;
        info.DATA_HI := index + DATA_BITS - 1;
        index        := index + DATA_BITS;
        info.BITS    := index;
        return info;
    end function;
    constant  INFO    : INFO_TYPE := SET_INFO;
    signal    i_info  : bit_vector(INFO.BITS-1 downto 0);
    signal    o_info  : bit_vector(INFO.BITS-1 downto 0);
begin

end architecture;
