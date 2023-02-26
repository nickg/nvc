entity file11 is
end entity;

architecture test of file11 is

    type char_file is file of character;

    file f1, f2 : char_file;

begin

    process is
        variable c : character;
    begin
        file_open(f1, "test.txt", WRITE_MODE);
        write(f1, 'x');
        flush(f1);

        -- Check we can open the same file again
        file_open(f2, "test.txt");
        read(f2, c);
        assert c = 'x';

        wait;
    end process;

end architecture;
