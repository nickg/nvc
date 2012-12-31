entity file1 is
end entity;

architecture test of file1 is

    type char_file is file of character;

    file f1 : char_file;

    type string_file is file of string;

    file f2 : string_file;

    file f3 : string_file open WRITE_MODE is "test2.txt";

begin

    process is
        variable c      : character;
        variable s      : string(1 to 3);
        variable len    : natural;
        variable status : file_open_status;
    begin
        file_open(f1, "test.txt", WRITE_MODE);
        write(f1, 'x');
        write(f1, 'y');
        write(f1, LF);
        file_close(f1);

        file_open(f1, "test.txt");
        read(f1, c);
        assert c = 'x';
        read(f1, c);
        assert c = 'y';
        read(f1, c);
        assert c = LF;
        assert endfile(f1);
        file_close(f1);

        file_open(f2, "test.txt", READ_MODE);
        read(f2, s, len);
        assert s = "xy" & LF;
        assert len = 3;
        file_close(f2);

        write(f3, "hello");
        file_close(f3);
        file_open(status, f3, "test2.txt", READ_MODE);
        assert status = OPEN_OK;
        read(f3, s, len);
        assert len = 3;
        assert s = "hel";
        file_close(f3);

        file_open(status, f3, "not_here", READ_MODE);
        assert status = NAME_ERROR;

        wait;
    end process;

end architecture;
