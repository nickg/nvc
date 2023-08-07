entity value3 is
end entity;

architecture test of value3 is
    type t_pair is record
        x, y : integer;
    end record;

    type t_pair_pair is array (1 to 2) of t_pair;
begin

    process is
    begin
        assert t_pair_pair'value("((1,2), (3,  4), (5, 6))")
            = ((1,2), (3,4));           -- Error
        wait;
    end process;

end architecture;
