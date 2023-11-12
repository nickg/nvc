entity issue793 is
end entity;

architecture test of issue793 is
    procedure check_setup(
        name        : in string;
        t           : in time;
        clk  : in bit;
        data : in bit_vector; -- v4p ignore never-read
        pol         : in bit := '1'  -- clock edge: 1 = rising, 0 = falling
        ) is
        variable ti : time;
    begin
    end procedure check_setup;

    procedure check_setup(
        name        : in string;
        t           : in time;
        clk  : in bit;
        data : in bit;
        pol         : in bit := '1'  -- clock edge: 1 = rising, 0 = falling
        ) is
    begin
        check_setup(
            name    => name,
            t       => t,
            clk     => clk,
            data(0) => data,
            pol     => pol );           -- OK
        check_setup(
            name    => name,
            t       => t,
            clk     => clk,
            data(1 to 1) => data & '0',
            pol     => pol );           -- OK
        check_setup(
            name    => name,
            t       => t,
            clk     => clk,
            data(0) => data,
            pol1    => pol );           -- Error
    end procedure check_setup;

begin
end architecture;
