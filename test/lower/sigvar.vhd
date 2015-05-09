entity sigvar is
end entity;

architecture test of sigvar is

    procedure proc1(signal x : bit_vector) is
        variable y : bit_vector(x'range);
    begin
        y := x;
    end procedure;

    procedure proc2(signal x : out bit_vector; signal y : in bit_vector) is
    begin
        x <= y;
    end procedure;

    procedure proc3(signal x : out bit_vector; signal y : in bit_vector) is
    begin
        x <= y & "1";
    end procedure;

begin

end architecture;
