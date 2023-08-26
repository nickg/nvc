entity alias15 is
end entity;

architecture test of alias15 is

    type t_test is protected
        procedure increment;
        impure function get return natural;
    end protected;

    type t_test is protected body
        variable count : natural;

        procedure increment is
        begin
            count := count + 1;
        end procedure;

        impure function get return natural is
        begin
            return count;
        end function;
    end protected body;

    shared variable pt : t_test;

    alias inc is pt.increment [];
    alias get is pt.get [return natural];
begin

    tb: process is
    begin
        inc;
        assert get = 1;
        inc;
        assert get = 2;
        wait;
    end process;

end architecture;
