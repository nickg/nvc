-- test_ng.vhd
entity  TEST_REGS is
    generic (
        REGS_BITS   : integer := 32
    );
    port (
        CLK         : in  bit;
        RST         : in  bit;
        REGS_WEN    : in  bit_vector(REGS_BITS-1 downto 0);
        REGS_WDATA  : in  bit_vector(REGS_BITS-1 downto 0);
        REGS_RDATA  : out bit_vector(REGS_BITS-1 downto 0)
    );
end TEST_REGS;
architecture RTL of TEST_REGS is
    type UNSIGNED is array (NATURAL range <> ) of BIT;

    constant NAU: UNSIGNED(0 downto 1) := (others => '0');

    function RESIZE (ARG: UNSIGNED; NEW_SIZE: NATURAL) return UNSIGNED is
    begin
      return ARG;
    end RESIZE;

    signal curr_value : unsigned(REGS_BITS-1 downto 0);
    signal x : boolean;
begin
    REGS_RDATA <= bit_vector(RESIZE(curr_value, REGS_RDATA'length));
    x <= regs_rdata'event;
end RTL;
