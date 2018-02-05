entity subent is
    port (
        a   :   in  string(1 to 2) := "AB";
        b   :   out string(1 to 2)
    );
end entity subent;
architecture test of subent is
begin
    b <= a;
end architecture test;
entity test is
end entity test;
architecture test of test is
    signal b : string(1 to 2);
begin
e1: entity work.subent
    port map (
        a      => open,
        b      => b
    );
end architecture test;
