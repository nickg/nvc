entity attr10 is
end entity;

architecture test of attr10 is
begin

    process is
        constant s : string := "1234";
        constant n : integer := 1234;
    begin
        for i in 1 to 4 loop
            report character'image(integer'image(n)(i));
            assert integer'image(n)(i) = s(i);
        end loop;
        wait;
    end process;

end architecture;
