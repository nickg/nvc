entity for2 is
end entity;

architecture test of for2 is

    type myint is range -1 to 4;

    type myenum is (A, B, C, D);

begin

    process is
    begin
        for x in myint loop
            report myint'image(x);
        end loop;
        for y in myenum loop
            report myenum'image(y);
        end loop;
        wait;
    end process;

end architecture;
