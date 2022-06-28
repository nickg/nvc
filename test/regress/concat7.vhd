entity concat7 is
end entity;

architecture test of concat7 is

    type rec is record
        f : string;
    end record;

    constant c1 : rec := ( f => ('a' & "bc") );

begin

    p: process is
        variable v : string(1 to 4);
    begin
        report "c1.f = " & c1.f;
        v := ' ' & c1.f;
        wait for 1 ns;
        report v;
        assert v = " abc";
        wait;
    end process;

end architecture;
