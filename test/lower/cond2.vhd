package cond2 is
    type pt is protected
        impure function get_idx (s : string) return natural;
    end protected;
    procedure dummy_loop (x : bit_vector);
end package;

package body cond2 is
    type string_array is array (natural range <>) of string;
    type string_array_ptr is access string_array;

    type pt is protected body
        variable strings : string_array_ptr;

        impure function get_idx (s : string) return natural is
        begin
            for i in strings.all'range loop
                return i when strings.all(i) = s;
            end loop;
            return natural'right;
        end function;
    end protected body;

    procedure dummy_loop (x : bit_vector) is
    begin
        for i in x'range loop
            if x(i) = '1' then
                return;
            else
                return;
            end if;
        end loop;
    end procedure;
end package body;
