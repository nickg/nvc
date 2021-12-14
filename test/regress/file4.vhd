entity file4 is
end entity;

architecture test of file4 is
    type natural_vector is array (natural range <>) of natural;
    subtype natural5 is natural_vector(1 to 5);
    type ft is file of natural5;
begin

    process is
        file f     : ft;
        variable v : natural5;
    begin
        file_open(f, "test.bin", WRITE_MODE);
        v := (1, 2, 3, 4, 5);
        write(f, v);
        file_close(f);

        v := (others => 0);

        file_open(f, "test.bin", READ_MODE);
        read(f, v);
        file_close(f);

        assert v = (1, 2, 3, 4, 5);

        wait;
    end process;

end architecture;
