entity grlib1 is
end entity;

architecture test of grlib1 is
    constant NAHBMST   : integer := 16;  -- maximum AHB masters
    constant NAHBSLV   : integer := 16;  -- maximum AHB slaves
    constant NAPBSLV   : integer := 16; -- maximum APB slaves
    constant NAHBAMR   : integer := 4;  -- maximum address mapping registers
    constant NAHBIR    : integer := 4;  -- maximum AHB identification registers
    constant NAHBCFG   : integer := NAHBIR + NAHBAMR;  -- words in AHB config block

    subtype amba_vendor_type  is integer range 0 to  16#ff#;
    subtype amba_device_type  is integer range 0 to 16#3ff#;
    subtype amba_version_type is integer range 0 to  16#3f#;
    subtype amba_cfgver_type  is integer range 0 to      3;
    subtype ahb_addr_type     is integer range 0 to 16#fff#;

    subtype amba_config_word is bit_vector(31 downto 0);
    type ahb_config_type is array (0 to NAHBCFG-1) of amba_config_word;

    constant gAWidth:    Positive := 18;      -- address width

    constant zero32 : bit_vector(31 downto 0) := (others => '0');

    constant HADDR:  Integer := 0;
    constant HMASK:         Integer  := 16#FFF#;

    function ahb_device_reg(vendor : amba_vendor_type; device : amba_device_type;
                            cfgver : amba_cfgver_type; version : amba_version_type)
        return bit_vector is
    begin
        return X"01234567";
    end function;

    function ahb_membar(memaddr : ahb_addr_type; prefetch, cache : bit;
                        addrmask : ahb_addr_type)
        return bit_vector is
    begin
        return (31 downto 0 => '1');
    end function;

    constant HCONFIG : ahb_config_type := (
        0        => ahb_device_reg (0, 0, 0, gAWidth),
        4        => ahb_membar(HADDR, '1', '1', HMASK),
        others   => zero32);
begin

    check: process is
    begin
        assert HCONFIG(0) = X"01234567";
        assert HCONFIG(1) = X"00000000";
        assert HCONFIG(4) = X"ffffffff";
        wait;
    end process;

end architecture;
