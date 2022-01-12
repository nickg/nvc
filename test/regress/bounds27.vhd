entity bounds27 is
end entity;

architecture test of bounds27 is

    procedure proc (x : out bit_vector) is
        variable a : bit_vector(x'range);  -- OK
        variable b : bit_vector(5 to x'right - 100);  -- OK
    begin
        x(1) := '1';                    -- Error
    end procedure;

begin

    main: process is
        variable v : bit_vector(1 to 0);
    begin
        proc(v);
        wait;
    end process;

end architecture;
