entity issue934 is
end entity;

architecture test of issue934 is
    type t_slv_array is array (natural range <>) of bit_vector;

    procedure proc (x : t_slv_array) is
    begin
    end procedure;

begin

    check: process is
    begin
        proc(x => t_slv_array'((x"01", x"02")));
        wait;
    end process;

end architecture;
