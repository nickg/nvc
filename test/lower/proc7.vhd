entity proc7 is
end entity;

architecture test of proc7 is

    procedure foo(x : bit_vector) is
        variable y : bit_vector(x'range);
    begin
        wait for 1 ns;
    end procedure;

begin
end architecture;
