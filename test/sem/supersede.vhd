entity portlisttest is
    port (
        signal a: in  bit;
        signal b: out bit
    );
end entity;

entity portlisttest is
end entity;

architecture foo of portlisttest is
    signal a:       bit;
    signal b:       bit;
begin
DUT:
    entity work.portlisttest --(fum)
        port map (
            a => a,
            b => b
        );
end architecture;
