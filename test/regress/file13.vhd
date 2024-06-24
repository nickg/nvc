entity file13 is
end entity;

architecture test of file13 is
    type ft is file of character;
    file f : ft;
begin

    process is
    begin
        file_open(f, "test.txt", write_mode);
        write(f, 'x');
        file_close(f);

        file_open(f, "test.txt", read_mode);
        write(f, 'x');                  -- Error
        file_close(f);
        wait;
    end process;

end architecture;
