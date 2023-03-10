entity file12 is
end entity;

architecture test of file12 is
    type char_file is file of character;
    file f : char_file;
begin

    process is
        variable c : character;
    begin
        file_open(f, "test.txt", WRITE_MODE);
        write(f, 'x');
        write(f, 'y');
        write(f, 'z');
        file_close(f);

        file_open(f, "test.txt", READ_WRITE_MODE);
        read(f, c);
        assert c = 'x';
        file_seek(f, 1);
        write(f, 'q');
        file_seek(f, 2);
        read(f, c);
        assert c = 'z';
        assert endfile(f);

        file_rewind(f);

        read(f, c);
        assert c = 'x';
        read(f, c);
        assert c = 'q';

        wait;
    end process;

end architecture;
