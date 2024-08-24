entity issue953 is
end entity;

architecture test of issue953 is
    signal s : bit_vector(1 to 2);
begin

    -- Crash with unconstrained port in has_unique_driver
    b: block is
        port ( p : out bit_vector );
        port map ( s );
    begin
        p(1) <= '1';
        p(2) <= '0';
    end block;

end architecture;
