entity bounds26 is
end entity;

architecture test of bounds26 is

    type char_map is array (character range <>) of integer;

    function func (right : character) return integer is
        subtype a_to_c_map is char_map('a' to 'c');
        variable r : a_to_c_map := ('a' to right => 1);
    begin
        return r('a') + r('b') + r('c');
    end function;

    signal n : character := 'c';
begin

    main: process is
    begin
        assert func('c') = 3;    -- OK
        assert func(n) = 3;      -- OK
        n <= DEL;
        wait for 1 ns;
        assert func(n) = 3;      -- Error
        wait;
    end process;

end architecture;
