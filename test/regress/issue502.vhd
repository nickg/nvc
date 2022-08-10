entity sub is
    generic ( n : integer);
end entity;

architecture test of sub is
    type rec is record
        f : integer range 1 to n;
    end record;

    function func1 (x : integer) return rec;

    function func2 (x : integer) return rec is
    begin
        return func1(x);
    end function;

    function func1 (x : integer) return rec is
    begin
        return (f => x * 2);
    end function;

    signal s, t : integer := 0;
begin

    p1: t <= func2(s).f;

    p2: process is
    begin
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue502 is
end entity;

architecture test of issue502 is
begin

    g: for i in 1 to 3 generate
    begin
        u: entity work.sub
            generic map ( i );
    end generate;

end architecture;
