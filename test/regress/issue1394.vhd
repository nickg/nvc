entity issue1394 is
end entity;

architecture struct of issue1394 is

    function ToMax(a, b : integer) return integer is
    begin
        if a >= b then
            return a;
        end if;
        return b;
    end;

    constant SectionCount_c : positive := ToMax(7, 8);

begin

    p_comb : process is
        variable Section_v : positive range 0 to SectionCount_c-1;
    begin
        Section_v := 2;
        wait;
    end process p_comb;

end architecture struct;
