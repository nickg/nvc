entity sanity1 is
end entity;

architecture test of sanity1 is
    signal x, y : bit;
    signal z : bit_vector(1 to 3);

    type t_rec is record
        x, y : integer;
    end record;

    signal r, s : t_rec;

    component comp is
        port ( o : out bit );
    end component;

    impure function get_bool return boolean is
    begin
        return true;
    end function;

    constant k : boolean := get_bool;
begin

    p0: x <= '1';                       -- { X }

    p1: process is                      -- { Y, Z(2), R.X }
        procedure proc (signal x : out bit) is
        begin
            z(2) <= '1';
            x <= '0';
        end procedure;
    begin
        y <= '1';
        y <= '0';
        z(2) <= '0';
        r.x <= 5;
        proc(z(1+1));
        r.x <= 1;
        wait;
    end process;

    u2: component comp                  -- { Z(1) }
        port map ( z(1) );

    p3: r.y <= 5;                       -- { R.Y }

    b4: block is
    begin
        b4p0: s.x <= 6;                 -- { S.X }
    end block;

    g5: if k generate
        g5p0: (z(1), z(2)) <= bit_vector'("10");  -- { Z(0), Z(1) }
    end generate;

end architecture;
