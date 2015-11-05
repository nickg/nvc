entity for3 is
end entity;

architecture fum of for3 is
    signal mySignalVector:  bit_vector (7 downto 0);
    signal myOtherSignal:   bit := '1';
begin
    process
    begin
L1:
        for i in 0 to 9 loop
            for i in 0 to 7 loop
                mySignalVector(i) <= myOtherSignal;
                report "outer loop i = " & integer'image(L1.i);
                report "inner loop i = " & integer'image(i);
            end loop;
        end loop;
        wait;
    end process;
end architecture;
