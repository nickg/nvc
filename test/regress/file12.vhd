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
        assert file_mode(f) = WRITE_MODE;
        assert file_size(f) = 0;
        write(f, 'x');
        write(f, 'y');
        write(f, 'z');
        assert file_position(f) = 3;
        assert file_position(f, FILE_ORIGIN_CURRENT) = 0;
        assert file_position(f, FILE_ORIGIN_END) = 0;
        assert file_size(f) = 3;
        file_close(f);

        assert file_state(f) = STATE_CLOSED;

        assert file_open(f, "test.txt", READ_WRITE_MODE) = OPEN_OK;
        read(f, c);
        assert c = 'x';
        file_seek(f, 1);
        write(f, 'q');
        file_seek(f, 2);
        assert file_position(f, FILE_ORIGIN_END) = 1;
        read(f, c);
        assert c = 'z';
        assert endfile(f);

        file_rewind(f);

        read(f, c);
        assert c = 'x';
        read(f, c);
        assert c = 'q';

        file_truncate(f, 1);
        assert file_size(f) = 1;
        file_rewind(f);

        read(f, c);
        assert c = 'x';
        assert endfile(f);

        file_seek(f, 1);
        write(f, '1');
        write(f, '2');
        file_truncate(f, -1, FILE_ORIGIN_END);

        file_rewind(f);
        read(f, c);
        assert c = 'x';
        read(f, c);
        assert c = '1';
        assert endfile(f);

        assert file_state(f) = STATE_OPEN;
        assert file_mode(f) = READ_WRITE_MODE;

        assert file_canseek(f);

        wait;
    end process;

end architecture;
