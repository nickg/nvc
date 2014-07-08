entity file2 is
end entity;

architecture test of file2 is
    subtype bit_vec5 is bit_vector(1 to 5);
    type ft is file of bit_vec5;
begin

    process is
        file f     : ft;
        variable v : bit_vec5;
    begin
        file_open(f, "test.bin", WRITE_MODE);
        v := "10100";
        write(f, v);
        file_close(f);

        v := "00000";

        file_open(f, "test.bin", READ_MODE);
        read(f, v);
        file_close(f);

        assert v = "10100";

        wait;
    end process;

end architecture;
