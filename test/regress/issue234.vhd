entity subent is
end entity subent;
architecture subentarch of subent is
    signal a : boolean;  -- commenting this out eliminates the assertion
begin
    proc: process
    begin
        report "ok";
        wait;
    end process;
end architecture;
entity issue234 is
end entity issue234;
architecture testarch of issue234 is
begin
ent1: entity work.subent;
ent2: entity work.subent;
end architecture;
