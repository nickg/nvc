entity assert1 is
end entity;

architecture a of assert1 is
begin

    process is
    begin
        assert true report "failed";
        report "hello world";
        assert false report "not important"
            severity note;
        assert false severity failure;
        wait;
    end process;
    
end architecture;
