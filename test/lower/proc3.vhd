entity proc3 is
end entity;

architecture test of proc3 is

    procedure p1(x : out integer) is
    begin
        wait for 10 ns;
        x := 1;
        wait for 5 ns;
    end procedure;

begin

    p2: process is
        variable x : integer;
    begin
        p1(x);
        wait;
    end process;

end architecture;
