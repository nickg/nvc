entity access8 is
end entity;

architecture test of access8 is

    type int_vector is array (natural range <>) of integer;
    type big_array is array (natural range <>) of int_vector(1 to 1000);
    type p_big_array is access big_array;

    signal n : integer := 10000000;
begin

    p1: process is
        variable p : p_big_array;
    begin
        p := new big_array(1 to n);
        wait;
    end process;

end architecture;
