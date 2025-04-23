entity issue1194 is
end entity;

architecture test of issue1194 is
    signal x, y : bit;
begin

    b: block is
        port ( p : in bit );
        port map ( p => inertial x or y );
    begin
    end block;

end architecture;
