entity cover22_a is
end entity;

architecture test of cover22_a is

begin

    process
    begin
        wait for 1 ns;
        report "Dummy statement in TOP hierarchy A!";
        wait for 1 ns;
        wait;
    end process;

end architecture;


entity cover22_b is
end entity;

architecture test of cover22_b is

begin

    process
    begin
        wait for 2 ns;
        report "Dummy statement in TOP hierarchy B!";
        wait for 2 ns;
        wait;
    end process;

end architecture;
