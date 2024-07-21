entity issue921 is
end entity;

architecture test of issue921 is
begin

    b: block is
        generic ( g : integer );
        generic map ( g => 42 );
    begin
    end block;

    check: process is
    begin
        assert << constant b.g : integer >> = 42;
        wait;
    end process;

end architecture;
