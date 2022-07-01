entity file9 is
end entity;

architecture test of file9 is
begin

    p1: process is
        type ft is file of integer;
        file f : ft;
    begin
       -- "If F is not associated with an external file, then FILE_CLOSE
       -- has no effect"
        file_close(f);
        wait;
    end process;

end architecture;
