entity gensub6 is
end entity;

architecture test of gensub6 is

    procedure proc
        generic (procedure preal(value : out real);
                 procedure pint(value : out integer))
        (x : out integer; y : out real) is
    begin
        preal(y);
        pint(x);
    end procedure;

    procedure get generic (type t; n : t) (x : out t) is
    begin
        x := n;
    end procedure;

    procedure get_one is new get generic map (t => integer, n => 1);
    procedure get_one is new get generic map (t => real, n => 1.0);

    procedure get_two is new get generic map (t => integer, n => 2);
    procedure get_two is new get generic map (t => real, n => 2.0);

    procedure proc_one is new proc generic map (preal => get_one, get_one);
    procedure proc_two is new proc generic map (preal => get_two, get_two);
begin

    p1: process is
        variable i : integer;
        variable r : real;
    begin
        proc_one(i, r);
        assert i = 1;
        assert r = 1.0;

        proc_two(i, r);
        assert i = 2;
        assert r = 2.0;

        wait;
    end process;

end architecture;
