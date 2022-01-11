entity bounds25 is
end entity;

architecture test of bounds25 is

    procedure proc (x : out bit_vector; b : bit) is
    begin
        x := (b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b);
    end procedure;

begin

    main: process is
        variable v : bit_vector(1 to 2);
    begin
        wait for 1 ns;
        proc(v, '1');                   -- Error
        wait;
    end process;

end architecture;
