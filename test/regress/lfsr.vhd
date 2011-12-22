entity lfsr16 is
    generic (
        WIDTH : positive := 16;
        TAP   : natural := 3 );
    port (
        clk   : in bit;
        reset : in bit;  -- Asynchronous
        en    : in bit;

        value : out bit_vector(15 downto 0) );
end entity;

architecture rtl of lfsr16 is
    signal state_r : bit_vector(WIDTH - 1 downto 0);
begin

    value <= state_r;

    process (clk, reset) is
    begin
        if reset = '1' then
            state_r <= (others => '0');
        elsif clk'event and clk = '1' then
            if en = '1' then
                state_r(WIDTH - 1 downto 1) <= state_r(WIDTH - 2 downto 0);
                state_r(0) <= state_r(WIDTH - 1) xnor state_r(TAP);
            end if;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity lfsr is
end entity;

architecture test of lfsr is
    constant PERIOD : delay_length := 10 ns;
    
    signal clk, en  : bit := '0';
    signal reset    : bit := '1';
    signal value    : bit_vector(15 downto 0);
begin

    clk   <= not clk after PERIOD / 2;
    reset <= '0' after 10 ns;
    en    <= '1' after 30 ns;

    uut: entity work.lfsr16
        port map ( clk, reset, en, value );

    check: process is
    begin
        wait for 500 ns;
        assert value = ('0','1','1','1','1','1','1','1',
                        '1','1','1','1','1','0','0','0');
        wait;
    end process;
end architecture;
