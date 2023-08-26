entity alias2 is
end entity;

architecture test of alias2 is

    type t_test is protected
        procedure increment;
        impure function get return natural;
        impure function get_bits return rv of bit_vector;
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

        impure function get_bits return rv of bit_vector is
        begin
            return (rv'range => '1');
        end function;
    end protected body;

    procedure foobar is
    begin
    end procedure;

    shared variable pt : t_test;

    alias inc is pt.increment [];       -- OK
    alias get is pt.get [return natural];  -- OK
    alias bad1 is pt.foobar [];         -- Error
    alias bad2 is xx.increment [];      -- Error
    alias bits is pt.get_bits [return bit_vector];  -- OK

begin

    tb: process is
    begin
        inc;                            -- OK
        assert get = 1;                 -- OK
        assert bits = "101";            -- Error
        assert pt.get_bits = "111";     -- Error
        wait;
    end process;

end architecture;
