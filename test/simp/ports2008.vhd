entity sub is
    port (
        x : in integer );
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
            port map ( x => a + b );

        -- Insert new process here
    end block;

end architecture;
