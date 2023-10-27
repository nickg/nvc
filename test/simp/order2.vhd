package order2 is
    constant c_one : bit_vector(3 downto 0) := x"5";
    constant c_two : bit_vector(3 downto 0) := not c_one;    -- Locally static

    procedure needs_a_body;
end package;

package body order2 is
    constant c_three : bit_vector(3 downto 0) := not c_one;    -- Locally static

    procedure needs_a_body is
    begin
    end procedure;
end package body;
