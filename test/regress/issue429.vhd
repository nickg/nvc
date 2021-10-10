entity sub is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of sub is

    procedure function_that_dies_with_this(x : in integer; y : out integer) is
        procedure read_val(val: out integer) is
        begin
            val := x;
        end procedure;
    begin
        read_val(y);                    -- Would crash during cgen here
    end procedure;

begin

    process (i) is
        variable tmp : integer;
    begin
        function_that_dies_with_this(i, tmp);
        o <= tmp;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue429 is
end entity;

architecture test of issue429 is
    signal i1, i2, i3, o : integer;
begin

    a: entity work.sub port map (i1, i2);

    b: entity work.sub port map (i2, i3);

    c: entity work.sub port map (i3, o);

    p1: process is
    begin
        i1 <= 1;
        wait for 1 ns;
        assert o = 1;
        wait;
    end process;

end architecture;
