entity sub is
    port (
        x : in bit;
        y : out bit );
end entity;

architecture test of sub is
begin
    y <= x after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity jcore1 is
end entity;

architecture test of jcore1 is
    type rec is record
        a, b : bit;
    end record;

    signal s : rec;
begin

    sub_i: entity work.sub
        port map ( s.a, s.b );

end architecture;
