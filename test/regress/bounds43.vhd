entity bounds43 is
end entity;

architecture test of bounds43 is
    signal s : integer := 0;
begin

    b1: block is
        port ( p : inout real );
        port map ( integer(p) => real(s) );  -- Error
    begin
    end block;

end architecture;
