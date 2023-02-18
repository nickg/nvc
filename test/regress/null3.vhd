entity null3 is
end entity;

architecture test of null3 is
    type t is (apple, banana, cherry);
    type ta is array (t range <>) of integer;

    function encode (d : t) return string is
        constant offset : natural := 0;
    begin
        return (1 => character'val(t'pos(d) + offset));
    end function;

    function encode (a : ta) return string is
        variable r : string(1 to 2 + a'length);
    begin
        assert not a'ascending;
        r(1 to 1) := encode(a'left);
        r(2 to 2) := encode(a'right);
        return r;
    end function;
begin

    p1: process is
        variable n : ta(apple downto banana);
    begin
        assert encode(n) = (character'val(0), character'val(1));
        wait;
    end process;

end architecture;
