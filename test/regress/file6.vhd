entity file6 is
end entity;

architecture test of file6 is
    type natural_vector is array (natural range <>) of natural;
    type ft is file of natural_vector;
begin

    process is
        file f1, f2  : ft;
        variable v   : natural_vector(1 to 5);
        variable len : natural;
    begin
        file_open(f1, "test.bin", WRITE_MODE);
        v := (1, 2, 3, 4, 5);
        write(f1, v);
        flush(f1);                       -- Flush without closing

        v := (others => 0);

        file_open(f2, "test.bin", READ_MODE);
        read(f2, v, len);
        file_close(f2);

        assert v = (1, 2, 3, 4, 5);
        report integer'image(len);
        assert len = 5;

        wait;
    end process;

end architecture;
