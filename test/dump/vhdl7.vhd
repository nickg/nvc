entity vhdl7 is
end entity;

architecture test of vhdl7 is
    type t_rec is record
        x, y : integer;
    end record;

    view source of t_rec is
        x : in;
        y : out;
    end view;

    signal x, y : t_rec;
begin

    b: block is
        port ( p1 : view source;
               p2 : view source'converse );
        port map ( x, y );
    begin
    end block;

end architecture;
