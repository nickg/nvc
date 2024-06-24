entity file14 is
end entity;

architecture test of file14 is
    type ft is file of character;
    file f : ft;
begin

    process is
        variable ch : character;
    begin
        file_open(f, "test.txt", write_mode);
        read(f, ch);                  -- Error
        file_close(f);
        wait;
    end process;

end architecture;
