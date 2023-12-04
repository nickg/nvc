entity osvvm3 is
end entity;

architecture test of osvvm3 is
    type t_rec is record
        f : integer;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;

    signal s : t_rec_array(1 to 3);
begin

    b1: block is
        port ( x : t_rec_array );
        port map ( s );

        procedure proc (signal a : t_rec_array) is
        begin
        end procedure;

        signal t : t_rec_array(1 to x'length);
    begin

        p1: process (x) is
        begin
            proc(x);
            proc(t);
        end process;

    end block;

end architecture;
