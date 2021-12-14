entity file5 is
end entity;

architecture test of file5 is
    type natural_vector is array (natural range <>) of natural;
    type ft is file of natural_vector;
begin

    process is
        file f       : ft;
        variable v   : natural_vector(1 to 5);
        variable len : natural;
    begin
        file_open(f, "test.bin", WRITE_MODE);
        v := (1, 2, 3, 4, 5);
        write(f, v);
        file_close(f);

        v := (others => 0);

        file_open(f, "test.bin", READ_MODE);
        read(f, v, len);
        file_close(f);

        assert v = (1, 2, 3, 4, 5);
        report integer'image(len);
        assert len = 5;

        wait;
    end process;

end architecture;
