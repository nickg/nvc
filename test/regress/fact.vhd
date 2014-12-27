entity fact_gen is
    port (
        question : in integer;
        answer   : out integer );
end entity;

architecture behav of fact_gen is
begin

    process (question) is
        variable tmp : integer;
    begin
        tmp := 1;
        for i in 1 to question loop
            tmp := tmp * i;
        end loop;
        answer <= tmp;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity fact is
end entity;

architecture test of fact is
    signal question, answer : integer;
begin

    uut: entity work.fact_gen
        port map (
            question => question,
            answer   => answer );

    process is
    begin
        for i in 1 to 15 loop
            question <= i;
            wait on answer;
            report integer'image(answer);
        end loop;
        wait;
    end process;

end architecture;
