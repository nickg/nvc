ENTITY dut IS
    PORT (
        port_ok_in  : in  bit;
        port_ok_out : out bit
    );
END ENTITY;

ARCHITECTURE rtl OF dut IS
BEGIN
    port_ok_out <= port_ok_in;
END ARCHITECTURE rtl;

ENTITY dut2 IS
    PORT (
        port_ok_in  : in  bit;
        port_ok_out : out bit
    );
END ENTITY;

ARCHITECTURE rtl OF dut2 IS
BEGIN
    port_ok_out <= port_ok_in;
END ARCHITECTURE rtl;

entity top is
    port (
        port_ok_out : out bit;
        port_extra  : out bit
    );
end entity;

architecture test of top is
    component dut IS
    PORT (
        port_ok_in  : in  bit;
        port_ok_out : out bit;
        port_extra  : out bit       -- Error
    );
    END component;

    component dut2 IS
        generic (g : integer );         -- Errro
    PORT (
        port_ok_in  : in  bit;
        port_ok_out : out bit
    );
    END component;
begin
    i_dut : dut
    PORT map (
        port_ok_in  => '0',
        port_ok_out => port_ok_out,
        port_extra  => port_extra
    );
    i_dut2 : dut2
        generic map ( 5 )
    PORT map (
        port_ok_in  => '0'
    );
end architecture;
