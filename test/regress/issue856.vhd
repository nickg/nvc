entity issue856 is
end entity;

architecture test of issue856 is
    type t_point is record
        x, y : integer;
    end record;

    type t_point_array is array (natural range <>) of t_point;

    type t_rec is record
        f : t_point_array(1 to 3);
        g : natural;
    end record;

    view t_rec_in of t_rec is
        f : in;
        g : out;
    end view;

    alias t_rec_out is t_rec_in'converse;

    type t_rec_array is array (natural range <>) of t_rec;

    procedure do_write (signal s : view t_rec_out) is
    begin
        for i in s.f'range loop
            s.f(i) <= (i*2, i*2 + 1);
        end loop;
    end procedure;

    signal s : t_rec;
begin

    p1: process is
    begin
        do_write(s);
        wait for 1 ns;
        assert s.g = 27;
        wait;
    end process;

    b: block is
        port ( p : view (t_rec_in) of t_rec_array );
        port map ( p(1) => s );
    begin

        g: for i in p'range generate
            p2: process (all)
                variable sum : integer := 0;
            begin
                for j in p(i).f'range loop
                    sum := sum + p(i).f(j).x + p(i).f(j).y;
                end loop;
                p(i).g <= sum;
            end process;
        end generate;

    end block;

end architecture;
