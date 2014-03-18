entity issue56 is
    generic (
        x : integer := 5;
        y : bit_vector := "110" );
    port (
        pi : in integer := 2;
        po : out bit );
end entity;

architecture test of issue56 is
begin

    process is
    begin
        assert x = 5;
        assert y = "110";
        assert pi = 2;
        wait;
    end process;

    po <= '1';

end architecture;
