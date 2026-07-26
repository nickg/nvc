library ieee;
use ieee.std_logic_1164.all;

entity issue1621 is
end entity;

architecture test of issue1621 is
    procedure check_vec(v : in std_logic_vector);

    procedure check_vec(v : in std_logic_vector) is
    begin
        assert false report "foreign binding was not resolved" severity failure;
    end procedure;

    attribute foreign : string;
    attribute foreign of check_vec : procedure is "VHPI issue1621 check_vec";
begin
    process is
    begin
        check_vec("10");
        wait;
    end process;
end architecture;
