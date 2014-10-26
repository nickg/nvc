entity alias9 is
end entity;

architecture test of alias9 is

    function to_i(x : bit) return integer is
    begin
        if x = '1' then
            return 1;
        else
            return 0;
        end if;
    end function;

    function to_i(x : character) return integer is
    begin
        if x = '1' then
            return 1;
        elsif x = '0' then
            return 0;
        else
            return -1;
        end if;
    end function;

    alias bit_to_i is to_i [bit return integer];
    alias char_to_i is to_i [character return integer];

    procedure test(x : in bit) is
    begin
        assert false;
    end procedure;

    procedure test(x : in character) is
    begin
        assert x = '1';
    end procedure;

begin

    process is
        alias test_ok is test [character];
    begin
        assert bit_to_i('1') = 1;
        assert char_to_i('0') = 0;
        assert char_to_i('x') = -1;
        test_ok('1');
        wait;
    end process;

end architecture;
