entity issue1290 is
    port (
        o1 : out bit;
        b1 : buffer bit );
end entity;

architecture test of issue1290 is
begin
    u1: block is
        port (
            o2 : out bit;
            b2 : buffer bit );
        port map (
            o2 => b1,                   -- OK (2002)
            b2 => o1 );                 -- OK (2002)
    begin
    end block;
end architecture;
