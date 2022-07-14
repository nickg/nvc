entity test_inst is
    generic(
        G_ROUND : natural := 0;
        G_ROUND_ENABLE : boolean := false
        );
    port(
        i_value : in bit_vector(7 downto 0);
        o_ena : out bit;
        o_value : out bit_vector(7 downto 0)
        );
end test_inst;

architecture rtl of test_inst is
begin
    o_ena <='1' when G_ROUND_ENABLE else '0';
    o_value <=(others=>'1') when G_ROUND=1 and G_ROUND_ENABLE else not i_value;
end architecture rtl;

entity issue153 is
end entity issue153;

architecture beh of issue153 is
    constant G_ROUND_ENABLE:boolean:=true;
    constant C_ADDROUND : bit_vector(7 downto 0):="00001111";
    constant C_ZERO8 : bit_vector(7 downto 0):=(others=>'0');
    signal s_ena:bit_vector(7 downto 0);
    type T_IN_DATA is array(integer range<>) of bit_vector(7 downto 0);
--signal s_value: T_IN_DATA(7 downto -1);-- this should work anyway, uncomment this to compare with ghdl for bug 2
    signal s_value: T_IN_DATA(7 downto 0);--this is for bug 1, nvc should report error
begin

    GEN_MACS_V : for v in 0 to 7 generate
        signal C   :bit_vector(7 downto 0);
        signal D   :bit_vector(7 downto 0);
    begin
        --should fail here, but doesn't
        --GHDL failed here with "bound check failure"
        -- ghdl drives correct values on each instances, nvc doesn't
        --C    <= C_ADDROUND when v=0 and G_ROUND_ENABLE else s_value(v-1);--bug 1
        -- below is workaround, but I am lazy enough to not use it :))))
        c_gen: if v=0 and G_ROUND_ENABLE generate
           C    <= C_ADDROUND;
        end generate c_gen;
        nc_gen: if v>0  generate
           C <= s_value(v-1);
        end generate nc_gen;

        test_i : entity work.test_inst
            generic map(
                G_ROUND         => 1
                )
            port map(
                i_value     => C,
                o_ena       => s_ena(v),
                o_value     => s_value(v)
                );
    end generate GEN_MACS_V;

    process
    begin
        wait for 1 ns;
        assert s_value(0) = not C_ADDROUND;
        assert s_value(1) = C_ADDROUND;
        wait;
    end process;

end architecture;
