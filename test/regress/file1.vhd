entity file1 is
end entity;

architecture test of file1 is

    type char_file is file of character;

    file f1 : char_file;

begin

    process is
    begin
        file_open(f1, "test.txt", WRITE_MODE);
        file_write(f1, 'x');
        file_write(f1, 'y');
        file_write(f1, LF);
        file_close(f1);
        wait;
    end process;

end architecture;
