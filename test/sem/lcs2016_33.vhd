entity lcs2016_33 is
end entity;

architecture test of lcs2016_33 is
    type t_test1 is protected
        private variable x : integer;   --  OK
        procedure proc;
    end protected;

    constant c : integer := 1;

    type t_test2 is protected
        private variable p1 : t_test1;  -- OK
        private variable p2 : t_test1;  -- OK
        alias a is p1.proc [];          -- OK
        alias b is p2.proc [];          -- OK
        alias cc is c;                  -- Error
    end protected;

    shared variable sv : t_test2;
begin

    process is
    begin
        sv.a;                           -- OK
        wait;
    end process;

end architecture;
