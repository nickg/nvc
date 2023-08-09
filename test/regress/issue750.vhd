package constants is
    constant c_byte_width : natural;
end package;

package body constants is
    constant c_byte_width : natural := 8;
end package body;

-------------------------------------------------------------------------------

package pack is
    generic (
        type t_element;
        c_default : t_element;
        c_size    : natural );

    type t_array is protected
        procedure put (idx : positive; elem : t_element);
        impure function get (idx : positive) return t_element;
    end protected;

end package;

package body pack is
    type t_array is protected body
        type t_array is array (1 to c_size) of t_element;
        variable f_array : t_array := (others => c_default);

        procedure put (idx : positive; elem : t_element) is
        begin
            f_array(idx) := elem;
        end procedure;

        impure function get (idx : positive) return t_element is
        begin
            return f_array(idx);
        end function;
    end protected body;

end package body;

-------------------------------------------------------------------------------

use work.constants.all;

entity issue750 is
end entity;

architecture test of issue750 is
    subtype t_byte is bit_vector(c_byte_width - 1 downto 0);
    constant c_default : t_byte := X"42";

    package bv_pack is new work.pack
        generic map ( t_element => t_byte, c_default => c_default, c_size => 10 );
    use bv_pack.all;

    shared variable pt : t_array;
begin

    tb: process is
    begin
        assert pt.get(5) = X"42";
        pt.put(5, X"10");
        assert pt.get(5) = X"10";
        wait;
    end process;

end architecture;
