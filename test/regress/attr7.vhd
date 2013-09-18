entity attr7 is
end entity;

architecture test of attr7 is
    signal vec : bit_vector(1 downto 0);
begin

    process (vec(0)) is
    begin
        report "wakeup " & bit'image(vec(0));
        if now > 0 ns then
            assert vec(0)'event;
            assert now = 3 ns;
        end if;
    end process;

    process is
    begin
        wait for 1 ns;
        vec <= "10";
        wait for 1 ns;
        vec <= "00";
        wait for 1 ns;
        vec <= "01";
        wait;
    end process;

end architecture;
