entity ent2 is
generic (
    M  : natural := 10;
    W  : natural := 0
);
port (
    i : in  bit_vector(M-1 downto 0);
    o : out bit_vector(M+W-1 downto 0)
);
end entity ent2;

architecture rtl of ent2 is
    signal s0 : bit_vector(W-1 downto 0);
begin
    s0 <= (others => '0');
    o <= i & s0;
end architecture;

entity issue787 is
end entity issue787;

architecture rtl of issue787 is
    constant M : natural := 7;
    constant W : natural := 0;
    signal i : bit_vector(M-1 downto 0);
    signal o : bit_vector(M+W-1 downto 0);
    signal o_l : bit_vector(W-1 downto 0);
    signal o_h : bit_vector(M-1 downto 0);
begin

    process
    begin
        i <= (others => '1');
        wait for 0 ns;
        wait for 0 ns;
        assert o = (M+W-1 downto 0 => '1');
        wait;
    end process;

    ent2_inst : entity work.ent2
    generic map (
        M => M,
        W => W
    )
    port map (
        i => i,
        o => o
    );

    o_l <= o(W-1 downto 0);
    o_h <= o(M+W-1 downto W);

end architecture;
