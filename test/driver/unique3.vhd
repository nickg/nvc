entity unique3 is
end entity;

architecture test of unique3 is
    type t_rec is record
        x, y : bit;
    end record;

    view x_in_y_out of t_rec is
        x : in;
        y : out;
    end view;

    signal s1 : bit;
    signal s2 : t_rec;
begin
    b1: block is
        port ( o : out bit; v : view x_in_y_out of t_rec );
        port map ( s1, s2 );
    begin
    end block;

    p1: s1 <= '1';
    p2: s2 <= ('1', '0');
end architecture;
