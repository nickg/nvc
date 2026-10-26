entity tcl6 is
    generic (
        g_str : string := "hello" );
end entity;

architecture test of tcl6 is
    constant c_str  : string := "world";
    constant c_real : real := 1.5;
    signal   s_real : real := -0.25;
begin
end architecture;
