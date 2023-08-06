entity image2 is
end entity;

architecture test of image2 is
    type t_pair is record
        x, y : integer;
    end record;

    type t_nested is record
        p : t_pair;
        b : boolean;
        r : real;
    end record;

    type int_vec is array (natural range <>) of integer;

    type t_enum is (a, b, c);

    type t_pair_vec is array (t_enum) of t_pair;

    type t_abc is ('a', 'b', 'c');
    type t_abc_vec is array (natural range <>) of t_abc;

begin

    p1: process is
        variable p : t_pair := (1, 2);
        variable n : t_nested := ((42, -5), true, 2.5);
        variable b : bit_vector(1 to 3) := "101";
        variable i : int_vec(1 to 4) := (1, 2, -99, 0);
        variable v : t_pair_vec := ((1, 2), (3, 4), (5, 6));
        variable a : t_abc_vec(1 to 4) := "abbc";
    begin
        report t_pair'image(p);
        report t_nested'image(n);
        report int_vec'image(i);
        report t_pair_vec'image(v);
        report t_abc_vec'image(a);
        report to_string(a);
        report bit_vector'image(b);
        report to_string(b);
        assert t_pair'image(p) = "(1,2)";
        assert t_nested'image(n) = "((42,-5),true,2.5)";
        assert int_vec'image(i) = "(1,2,-99,0)";
        assert t_pair_vec'image(v) = "((1,2),(3,4),(5,6))";
        assert to_string(v) = "((1,2),(3,4),(5,6))";
        assert t_abc_vec'image(a) = """abbc""";
        assert to_string(a) = "abbc";
        assert to_string(b) = "101";
        wait;
    end process;

end architecture;
