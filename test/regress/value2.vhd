entity value2 is
end entity;

architecture test of value2 is
    type t_pair is record
        x, y : integer;
    end record;

    type t_int_vec is array (natural range <>) of integer;

    type t_nested is record
        p : t_pair;
        b : boolean;
        r : real;
        i : t_int_vec(1 to 2);
    end record;

    type t_pair_pair is array (1 to 2) of t_pair;

    type t_abc is ('a', 'b', 'c');
    type t_abc_vec is array (natural range <>) of t_abc;
begin

    process is
    begin
        assert t_pair'value("(1,2)") = (1, 2);
        assert t_pair'value("( 4, 6   )") = (4, 6);
        assert t_nested'value("((1, 2), true, 1.5, (5, 6))") = ((1, 2), true, 1.5, (5, 6));
        assert t_int_vec'value("(1,2,3)") = (1, 2, 3);
        assert t_pair_pair'value("((1,2), (3,  4))") = ((1,2), (3,4));
        assert t_abc_vec'value("abc") = "abc";
        assert t_abc_vec'value(" bbc  ") = "bbc";
        wait;
    end process;

end architecture;
