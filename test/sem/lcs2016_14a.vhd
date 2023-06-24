entity lcs2016_014a is
end entity;

architecture test of lcs2016_014a is
    type pt is protected
        procedure proc;
        function func (x : integer := 2) return integer;
    end protected;

    type pt is protected body
        procedure proc is
        begin
        end procedure;
        function func (x : integer := 2) return integer is
        begin
            return x;
        end function;
    end protected body;

    type ptp is access pt;          -- OK
    type ft is file of natural;     -- OK
    type ftp is access ft;          -- OK

    -- TODO: additions re. generic protected types
begin

    p1: process is
        variable sv : ptp;              -- OK
        variable v : pt;
    begin
        assert sv = null;               -- OK
        sv := new pt;                   -- OK
        sv.all.proc;                    -- OK
        sv.proc;                        -- OK
        sv.all.all.proc;                -- Error
        assert sv.all.func = 2;         -- OK
        assert sv.func(5) = 2;          -- OK
        sv := new pt'(sv.all);          -- Error (?)
        wait;
    end process;

end architecture;
