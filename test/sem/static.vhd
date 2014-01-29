entity static is
end entity;

architecture test of static is
begin

    process is
        subtype byte is bit_vector(7 downto 0);
        variable bv : byte;
        variable i  : integer;

        attribute hello : integer;
        attribute hello of bv : variable is 6;
    begin
        case i is
            when bv'length =>           -- OK
                null;
            when bv'left =>             -- OK
                null;
            when byte'right =>          -- OK
                null;
            when bv'hello =>            -- OK
                null;
            when others =>
                null;
        end case;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity sub is
    port ( x : bit_vector );
end entity;

architecture test of sub is
begin

    sub_i: entity work.sub
        port map (
            x => x(x'left downto x'right) );  -- OK

end architecture;
