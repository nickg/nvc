entity lcs2016_07 is
end entity;

architecture test of lcs2016_07 is
begin

    p1: process is
        variable v : bit := '1';
    begin
        b1: block is                    -- OK
            variable v : integer := 1;
            constant c : boolean := true;
        begin
            v := 6;
        end block b1;
        assert c;                       -- Error
        block is
            shared variable v : integer;  -- Error
        begin
        end block;
        wait;
    end process;

end architecture;
