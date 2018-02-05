entity subent is
    port (
        a   :   in  string(1 to 2) := "AB";
        b   :   out string(1 to 2));
end entity subent;
architecture test of subent is
begin
    b(1) <= a(1);
    b(2) <= a(2);
end architecture test;
entity test is
end entity test;
architecture test of test is
    signal b : string(1 to 2);
begin
e1: entity work.subent
    port map (
        a      => open,
        b(1)   => b(1),
        b(2)   => open    -- it's not legal to have a mix of OPEN and non-open
    );
end architecture test;
