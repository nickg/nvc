entity sub is
    generic ( GEN : in bit_vector(1 to 3) );
    port ( x : in bit_vector(1 to 3) );
end entity;

architecture test of sub is
begin

    process (x) is
    begin
        case x is
            when GEN =>                 -- OK in 2008
                report "x = GEN";
            when others =>
                report "x /= GEN";
        end case;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue105 is
end entity;

architecture test of issue105 is
    signal s : bit_vector(1 to 3) := "101";
begin

    sub_i: entity work.sub
        generic map ( "101" )
        port map ( s );

end architecture;
