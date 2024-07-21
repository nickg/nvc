entity open2 is
end entity;

architecture test of open2 is
begin

    b: block is
        port ( s : in string := "hello" );
        port map ( open );
    begin

        check: process is
        begin
            assert s = "hello";
            assert s'left = 1;
            assert s'right = 5;
            wait;
        end process;

    end block;

end architecture;
