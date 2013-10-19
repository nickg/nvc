package elab16_pack is
    type int8 is array (7 downto 0) of integer;
    type int8_vector is array (integer range <>) of int8;
end package;

-------------------------------------------------------------------------------

use work.elab16_pack.all;

entity sub is
    generic (
        val : integer );
    port (
        x : out int8 );
end entity;

architecture test of sub is
begin
    xg: for i in int8'range generate
        x(i) <= val + i;
    end generate;
end architecture;

-------------------------------------------------------------------------------

entity elab16 is
end entity;

use work.elab16_pack.all;

architecture test of elab16 is
    signal xs : int8_vector(0 to 7);
begin

    sub_g: for i in 0 to 7 generate

        sub_i: entity work.sub
            generic map (
                val => i * 8 )
            port map (
                x => xs(i) );

    end generate;

    process is
    begin
        wait for 1 ns;
        for i in 0 to 7 loop
            for j in 0 to 7 loop
                assert xs(i)(j) = (i * 8) + j;
            end loop;
        end loop;
        wait;
    end process;

end architecture;
