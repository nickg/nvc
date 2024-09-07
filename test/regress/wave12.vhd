-- Run with --dump-arrays=3
entity wave12 is
end entity;

architecture test of wave12 is
    type t_mem is array (natural range <>) of bit_vector(7 downto 0);

    type t_rec is record
        x : integer;                    -- Should dump
        y : t_mem(1 to 3);              -- Should dump
        z : t_mem(1 to 10000);          -- Should not dump
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    signal s : t_rec_array(1 to 4);     -- Should dump
    signal a : t_mem(1 to 2);           -- Should dump
    signal b : t_mem(1 to 1000);        -- Should not dump
begin

    stim: process is
        variable mask : bit_vector(7 downto 0) := X"01";
        variable num  : natural;
    begin
        for i in 1 to 5 loop
            for j in 1 to 4 loop
                s(j).x <= i;
                s(j).y(1 + (num mod 3)) <= mask;
                s(j).z(1 + (num mod 10000)) <= mask;
                mask := mask rol 1;
                num := num + 1;
            end loop;
            a(1 + (num mod 2)) <= mask;
            wait for 1 ns;
        end loop;
        wait;
    end process;

end architecture;
