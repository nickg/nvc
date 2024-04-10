entity issue878 is
end entity;

architecture test of issue878 is
    type t_rec is record
        x, y : natural;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;
    type t_rec_array_array is array (natural range <>) of t_rec_array;

    signal s : t_rec_array_array(1 to 3)(1 to 2);
begin

    stim: process is
    begin
        wait for 1 ns;
        s(2)(1) <= (1, 2);
        wait for 2 ns;
        s(3)(2).x <= 42;
        wait;
    end process;

end architecture;
