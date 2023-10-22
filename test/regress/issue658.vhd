package pack is
  generic (
    type t ;
    function "="(l, r : t) return boolean is <>;
    function "/="(l, r : t) return boolean is <>
  ) ;

    function equal(l, r : t) return boolean ;
    function notequal(l, r : t) return boolean ;

end package ;

package body pack is

    function equal(l, r : t) return boolean is
    begin
        return l = r ;
    end function ;

    function notequal(l, r : t) return boolean is
    begin
        return l /= r ;
    end function ;

end package body ;

-------------------------------------------------------------------------------

entity issue658 is
end entity;

architecture test of issue658 is
    function eq42 (x, y : integer) return boolean is
    begin
        return x = 42 and y = 42;
    end function;

    package pint is new work.pack generic map ( integer );
    package pstrange is new work.pack generic map ( integer, "=" => eq42 );
    package pbv is new work.pack generic map ( bit_vector );

begin

    check: process is
    begin
        assert pint.equal(1, 1);
        assert pint.notequal(1, 2);

        assert pbv.equal("101", "101");
        assert pbv.notequal("101", "110");

        assert pstrange.equal(42, 42);
        assert not pstrange.equal(13, 13);

        wait;
    end process;

end architecture;
