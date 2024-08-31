entity issue963 is
end entity;

library ieee;
use ieee.numeric_bit.all;

architecture test of issue963 is
    type t_enum is (A, B, C);

    type t_inner is record
        x : t_enum;
        y : integer;
    end record;

    type t_outer is record
        pad   : bit;
        inner : t_inner;
    end record;

    subtype t_vec is bit_vector(33 downto 0);

    type t_padded_vec is record
        pad : bit;
        vec : t_vec;
    end record;

    signal r1, r2 : t_outer;
    signal v1, v2 : t_vec;

    function to_bits (arg : t_inner) return t_vec is
        variable result : bit_vector(33 downto 0);
    begin
        result(31 downto 0) := bit_vector(to_signed(arg.y, 32));
        result(33 downto 32) := bit_vector(to_unsigned(t_enum'pos(arg.x), 2));
        return result;
    end function;

    function from_bits (arg : t_vec) return t_inner is
        variable result : t_inner;
    begin
        result.x := t_enum'val(to_integer(unsigned(arg(33 downto 32))));
        result.y := to_integer(signed(arg(31 downto 0)));
        return result;
    end function;

begin

    u1: block is
        port ( i : in t_vec; o : out t_vec );
        port map ( i => to_bits(r1.inner), o => v1 );
    begin
        o <= i;
    end block;

    u2: block is
        port ( io : inout t_vec := "01" & X"FFFFFFFF" );
        port map ( from_bits(io) => to_bits(r2.inner) );
    begin
        assert io = "01" & X"FFFFFFFF";
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert v1 = "00" & X"80000000";
        r1.inner.x <= C;
        wait for 0 ns;
        wait for 0 ns;
        assert v1 = "10" & X"80000000";
        r1.inner.y <= 42;
        wait for 1 ns;
        assert v1 = "10" & X"0000002a";
        assert r2.inner.x = B;
        assert r2.inner.y = -1;
        wait;
    end process;

end architecture;
