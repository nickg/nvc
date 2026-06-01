-- Non-generic package: doubly-unconstrained array type (VHDL-2008)
package type_pack is
    type slv_array_t is array (natural range <>) of bit_vector;
end package;

-- Generic package: constants/subtypes derived from generic parameter
package meta_pkg is
    generic (
        ITEM_W : natural
    );
    constant FIELD_W : natural := ITEM_W / 8;
    constant OFFSET  : natural := 0;
    subtype MY_FIELD is natural range OFFSET + FIELD_W - 1 downto OFFSET;
end package;

use work.type_pack.all;

entity issue1559_dut is
    generic (
        REGIONS : natural := 2;
        ITEM_W  : natural := 64      -- non-literal: default used as top-level
    );
    port (
        DIN  : in  slv_array_t(REGIONS-1 downto 0)(ITEM_W-1 downto 0);
        DOUT : out bit_vector(REGIONS-1 downto 0)
    );
end entity;

architecture rtl of issue1559_dut is
    -- Instantiated with non-literal entity generic — TRIGGER
    package pkg_i is new work.meta_pkg
        generic map (ITEM_W => ITEM_W);
    use pkg_i.all;

    -- Element width comes from package constant FIELD_W
    signal result : slv_array_t(REGIONS-1 downto 0)(FIELD_W + 1 - 1 downto 0);
begin
    assert DIN'length = REGIONS;
    assert DIN'left = REGIONS - 1;
    assert DIN'right = 0;
    assert DIN'element'length = ITEM_W;
    assert DIN'element'left = ITEM_W - 1;
    assert DIN'element'right = 0;

    assert MY_FIELD'left = FIELD_W - 1;
    assert MY_FIELD'right = OFFSET;
    assert MY_FIELD'left - MY_FIELD'right + 1 = FIELD_W;

    assert result'length = REGIONS;
    assert result'left = REGIONS - 1;
    assert result'right = 0;
    assert result'element'length = FIELD_W + 1;
    assert result'element'left = FIELD_W;
    assert result'element'right = 0;

    assert DOUT'length = REGIONS;
    assert DOUT'left = REGIONS - 1;
    assert DOUT'right = 0;

    g_drive : for i in 0 to REGIONS-1 generate
        -- Driving slv_array_t element; RHS slices with package subtype MY_FIELD
        result(i) <= DIN(i)(MY_FIELD) & '0';
    end generate;

    gen_out : for i in 0 to REGIONS-1 generate
        DOUT(i) <= result(i)(FIELD_W);
    end generate;
end architecture;

use work.type_pack.all;

entity issue1559 is
end entity;

architecture test of issue1559 is
    constant REGIONS : natural := 2;
    constant ITEM_W  : natural := 64;

    signal din  : slv_array_t(REGIONS-1 downto 0)(ITEM_W-1 downto 0);
    signal dout : bit_vector(REGIONS-1 downto 0);
begin
    dut : entity work.issue1559_dut
        generic map (
            REGIONS => REGIONS,
            ITEM_W  => ITEM_W
        )
        port map (
            DIN  => din,
            DOUT => dout
        );

    process is
    begin
        din(0) <= x"0000000000000080";
        din(1) <= x"000000000000007f";
        wait for 1 ns;

        assert dout = "01" report to_string(dout);

        din(0) <= x"000000000000007f";
        din(1) <= x"0000000000000080";
        wait for 1 ns;

        assert dout = "10" report to_string(dout);
        wait;
    end process;
end architecture;
