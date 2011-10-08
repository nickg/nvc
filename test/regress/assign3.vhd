entity assign3 is
end entity;

architecture test of assign3 is
begin

    process is
        variable x : bit_vector(7 downto 0);
    begin
        assert x(0) = '0';
        wait;
    end process;

end architecture;
