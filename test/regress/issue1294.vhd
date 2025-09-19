entity issue1294 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue1294 is
    component SRAM_1P_behavioral_bm_bist is
        generic ( P_DATA_WIDTH : integer := 24;
                  P_ADDR_WIDTH : integer := 14 );
        port ( A_ADDR : in std_logic_vector(P_ADDR_WIDTH - 1 downto 0);
               A_DIN  : in std_logic_vector(P_DATA_WIDTH - 1 downto 0);
               A_BM   : in std_logic_vector(P_DATA_WIDTH - 1 downto 0);
               A_MEN  : in std_logic;
               A_WEN  : in std_logic;
               A_REN  : in std_logic;
               A_CLK  : in std_logic;
               A_DLY  : in std_logic;
               A_DOUT : out std_logic_vector(P_DATA_WIDTH - 1 downto 0);

               A_BIST_EN   : in std_logic;
               A_BIST_ADDR : in std_logic_vector(P_ADDR_WIDTH - 1 downto 0);
               A_BIST_DIN  : in std_logic_vector(P_DATA_WIDTH - 1 downto 0);
               A_BIST_BM   : in std_logic_vector(P_DATA_WIDTH - 1 downto 0);
               A_BIST_MEN  : in std_logic;
               A_BIST_WEN  : in std_logic;
               A_BIST_REN  : in std_logic;
               A_BIST_CLK  : in std_logic );
    end component;

    constant P_DATA_WIDTH : integer := 16;
    constant P_ADDR_WIDTH : integer := 12;

    signal A_ADDR      : std_logic_vector(P_ADDR_WIDTH - 1 downto 0);
    signal A_DIN       : std_logic_vector(P_DATA_WIDTH - 1 downto 0);
    signal A_BM        : std_logic_vector(P_DATA_WIDTH - 1 downto 0);
    signal A_MEN       : std_logic := '1';
    signal A_WEN       : std_logic := '0';
    signal A_REN       : std_logic := '0';
    signal A_CLK       : std_logic := '0';
    signal A_DLY       : std_logic := '0';
    signal A_DOUT      : std_logic_vector(P_DATA_WIDTH - 1 downto 0);
    signal A_BIST_EN   : std_logic := '0';
    signal A_BIST_ADDR : std_logic_vector(P_ADDR_WIDTH - 1 downto 0);
    signal A_BIST_DIN  : std_logic_vector(P_DATA_WIDTH - 1 downto 0);
    signal A_BIST_BM   : std_logic_vector(P_DATA_WIDTH - 1 downto 0);
    signal A_BIST_MEN  : std_logic := '0';
    signal A_BIST_WEN  : std_logic := '0';
    signal A_BIST_REN  : std_logic := '0';
    signal A_BIST_CLK  : std_logic := '0';
begin

    uut: component SRAM_1P_behavioral_bm_bist
        generic map (
            P_DATA_WIDTH => P_DATA_WIDTH,
            P_ADDR_WIDTH => P_ADDR_WIDTH)
        port map (
            A_ADDR      => A_ADDR,
            A_DIN       => A_DIN,
            A_BM        => A_BM,
            A_MEN       => A_MEN,
            A_WEN       => A_WEN,
            A_REN       => A_REN,
            A_CLK       => A_CLK,
            A_DLY       => A_DLY,
            A_DOUT      => A_DOUT,
            A_BIST_EN   => A_BIST_EN,
            A_BIST_ADDR => A_BIST_ADDR,
            A_BIST_DIN  => A_BIST_DIN,
            A_BIST_BM   => A_BIST_BM,
            A_BIST_MEN  => A_BIST_MEN,
            A_BIST_WEN  => A_BIST_WEN,
            A_BIST_REN  => A_BIST_REN,
            A_BIST_CLK  => A_BIST_CLK );

    a_clk <= not a_clk after 5 ns;

    process is
    begin
        wait until falling_edge(a_clk);

        a_addr <= X"123";
        a_din <= X"dead";
        a_bm <= X"ffff";
        a_wen <= '1';
        a_ren <= '0';

        wait until falling_edge(a_clk);

        assert a_dout = (15 downto 0 => 'U') report to_string(a_dout);

        a_wen <= '0';
        a_ren <= '1';

        wait until falling_edge(a_clk);

        assert a_dout = X"dead" report to_hstring(a_dout);

        a_addr <= X"124";
        a_din <= X"beef";
        a_wen <= '1';
        a_ren <= '1';

        wait until falling_edge(a_clk);

        assert a_dout = X"beef" report to_hstring(a_dout);

        a_addr <= X"123";
        a_wen <= '0';
        a_ren <= '1';

        wait until falling_edge(a_clk);

        assert a_dout = X"dead" report to_hstring(a_dout);

        std.env.finish;
    end process;

end architecture;
