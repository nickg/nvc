architecture a of e is
begin

    process is
    begin
        x := not y;
        x := abs y;
        x := y ** z;
        x := f(4).z;
        x := y sll 1;
        x := y srl 1;
        x := y sla 1;
        x := y sra 1;
        x := y rol 1;
        x := y ror 1;
        x := work.foo."and"(1, 2);
        x(y'range) := y;
    end process;

end architecture;
