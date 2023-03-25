package copy1 is
    procedure test_copy;
end package;

package body copy1 is

    procedure do_copy (dest : out bit_vector; src : in bit_vector) is
    begin
        dest := src;
    end procedure;

    procedure test_copy is
        variable src, dest : bit_vector(1 to 4096);
    begin
        for i in 1 to 100 loop
            do_copy(dest, src);
        end loop;
    end procedure;

end package body;
