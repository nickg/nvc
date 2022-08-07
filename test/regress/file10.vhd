entity file10 is
end entity;

architecture test of file10 is
    type rec is record
        x : integer;
        s : string(1 to 5);
    end record;

    type rec_array is array (natural range <>) of rec;

    type rf is file of rec_array;
begin

    p1: process is
        file f : rf;
        variable v : rec_array(1 to 2);
        variable len : integer;
    begin
        file_open(f, "file.bin", write_mode);
        write(f, ((1, "hello"), (2, "world")));
        file_close(f);
        file_open(f, "file.bin", read_mode);
        read(f, v, len);
        assert len = 2;
        assert v(1) = (1, "hello");
        assert v(2) = (2, "world");
        file_close(f);
        wait;
    end process;

end architecture;
