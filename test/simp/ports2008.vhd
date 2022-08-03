entity sub is
    port (
        x : in integer;
        y : in integer_vector(1 to 3) );
end entity;

entity top is
end entity;

architecture test of top is
    signal a : integer;
begin

    b: block is
        signal b : integer;
        -- Insert new signal here
    begin
        u: entity work.sub
            port map ( x => a + b, y => a & b & (0 => 4) );

        -- Insert new process here
    end block;

end architecture;
