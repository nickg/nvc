entity wave2 is
end entity;

architecture test of wave2 is
    signal foo : bit_vector(1 to 3);
    signal bar : bit;
    signal foobaz : bit;
    signal frobfoo : bit;
begin

    main: process is
    begin
        bar <= '1';
        wait for 1 ns;
        frobfoo <= '1';
        wait;
    end process;

end architecture;
