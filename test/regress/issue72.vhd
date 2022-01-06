package pack1 is
    type ma_t is array(1 downto 0) of bit_vector(1 downto 0);
end pack1;

use work.pack1.all;
entity arraysub is
    generic(par1: bit_vector(3 downto 0));
end entity;

architecture test of arraysub is
    signal s1, s2: ma_t;
begin
    s1(1)<=par1(1 downto 0);
    s1(0)<=par1(3 downto 2);

    s2(1 downto 1) <= ( 1 => par1(3 downto 2) );
    s2(0 downto 0) <= ( 0 => par1(1 downto 0) );

    process is
    begin
        wait for 1 ns;
        assert s1 = ( "11", "11" );
        assert s2 = ( "11", "11" );
        wait;
    end process;
end architecture;

entity issue72 is
end entity;

architecture topi of issue72 is
begin
    subi: entity work.arraysub
        generic map(par1=>"1111");
end architecture;
