package pack is
    type rec is record
        x, y : integer;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( r : rec; en : boolean );
    port ( x_out, y_out : out bit_vector(1 to 2) );
end entity;

architecture test of sub is
begin

    g: if en generate

        main: process is
            function get_bits(n : integer) return bit_vector is
            begin
                case n is
                    when 1 => return (1 to 2 => '1');
                    when others => return (1 to 2 => '0');
                end case;
            end function;
            constant x : bit_vector(1 to 2) := get_bits(r.x);
            constant y : bit_vector(1 to 2) := get_bits(r.y);
        begin
            x_out <= x;
            y_out <= y;
            wait;
        end process;

    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab28 is
end entity;

architecture test of elab28 is
    signal x, y : bit_vector(1 to 2);
begin

    uut: entity work.sub
        generic map ( ( 0, 1 ), true )
        port map ( x, y );

end architecture;
