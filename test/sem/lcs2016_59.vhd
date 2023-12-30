entity lcs2016_59 is
    generic (
        type t1 is <>;
        type t2 is (<>);
        type t3 is range <>;
        type t4 is units <>;
        type t5 is range <> . <>;
        type t6 is array (natural range <>) of type is private;
        type t7 is access type is private;
        type t8 is file of type is private );
end entity;

architecture test of lcs2016_59 is
    component c1 is
        generic ( type t is <> );
    end component;

    component c2 is
        generic ( type t is (<>) );
    end component;

    component c3 is
        generic ( type t is range <> );
    end component;

    component c4 is
        generic ( type t is units <> );
    end component;

    component c5 is
        generic ( type t is range <> . <> );
    end component;

    component c6 is
        generic ( type t is array (natural range <>) of type is (<>) );
    end component;

    component c7 is
        generic ( type t is access type is <> );
    end component;

    component c8 is
        generic ( type t is file of type is private );
    end component;

    signal s1 : t1;
    signal s2 : t2;
    signal s3 : t3;
    signal s4 : t4;
    signal s5 : t5;
    signal s6 : t6(1 to 3);             -- OK
    signal s7 : t7;                     -- Error
    signal s8 : t8;                     -- Error
begin

    u1: component c1
        generic map ( t => integer );   -- OK

    u2: component c1
        generic map ( t => real );      -- OK

    u3: component c1
        generic map ( t => bit_vector );  -- Error

    u4: component c1
        generic map ( t => t1 );        -- OK

    u5: component c1
        generic map ( t => t3 );        -- OK

    assert s1 = s1;                     -- OK
    assert s1 /= s1;                    -- OK
    assert s1 < s1;                     -- OK
    assert s1 <= s1;                    -- OK
    assert s1 > s1;                     -- OK
    assert s1 >= s1;                    -- OK
    assert minimum(s1, s1) = s1;        -- OK
    assert maximum(s1, s1) = s1;        -- OK
    assert to_string(s1) = "111";       -- OK

    u6: component c2
        generic map ( t => integer );   -- OK

    u7: component c2
        generic map ( t => real );      -- Error

    u7a: component c2
        generic map ( t => t2 );        -- OK

    assert s2 < s2;                     -- OK

    u8: component c3
        generic map ( t => integer );   -- OK

    u9: component c3
        generic map ( t => bit );       -- Error

    u9a: component c3
        generic map ( t => t3 );        -- OK

    assert s3 + s3 = 0;                 -- OK
    assert s3 - s3 = 0;                 -- OK
    assert s3 / 1 = s3;                 -- OK
    assert -1 * s3 = -s3;               -- OK
    assert +s3 = s3;                    -- OK
    assert abs(s3) = 5;                 -- OK

    u10: component c5
        generic map ( t => real );      -- OK

    u11: component c5
        generic map ( t => integer );   -- Error

    u11a: component c5
        generic map ( t => t5 );        -- OK

    assert s5 + 1.0 = 0.0;              -- OK
    assert s5 * s5 = s5;                -- OK

    u12: component c4
        generic map ( t => time );      -- OK

    u13: component c4
        generic map ( t => integer );   -- Error

    u13a: component c4
        generic map ( t => t4 );        -- OK

    assert s4 < s4;                     -- OK
    assert to_string(s4) = "1 ns";      -- OK

    b1: block is
        type t_int_ptr is access integer;
        type t_bit_ptr is access bit;

        component c7a is
            generic ( type t is access bit );
        end component;

    begin

        u14: component c7
            generic map ( t => t_int_ptr );  -- OK

        u15: component c7
            generic map ( t => real );  -- Error

        u16: component c7
            generic map ( t => t7 );  -- Error

        u17: component c7a
            generic map ( t => t_bit_ptr );  -- OK

        u18: component c7a
            generic map ( t => t_int_ptr );  -- Error

        p1: process is
            variable ptr : t7;
        begin
            deallocate(ptr);            -- OK
            ptr := new t7'designated_subtype;  -- OK
            wait;
        end process;

    end block;

    b2: block is
        type t_int_file is file of integer;
        type t_bit_file is file of bit;

        component c8a is
            generic ( type t is file of bit );
        end component;

    begin

        u19: component c8
            generic map ( t => t_int_file );  -- OK

        u20: component c8
            generic map ( t => real );  -- Error

        u21: component c8
            generic map ( t => t8 );  -- Error

        u22: component c8a
            generic map ( t => t_bit_file );  -- OK

        u23: component c8a
            generic map ( t => t_int_file );  -- Error

    end block;

    b3: block is
        type t_bool_map is array (boolean) of boolean;
        type t_time_map is array (integer range <>) of time;
        type t_matrix is array (natural range <>, natural range <>) of real;

        component c6a is
            generic ( type t is array (type is range <>) of boolean );
        end component;
    begin

        u24: component c6
            generic map ( t => bit_vector );  -- OK

        u25: component c6
            generic map ( t => integer );  -- Error

        u26: component c6
            generic map ( t => t_bool_map );  -- Error

        u27: component c6
            generic map ( t => t6 );    -- Error

        u28: component c6a
            generic map ( t => t_bool_map );  -- Error

        u29: component c6a
            generic map ( t => t_time_map );  -- Error

        u30: component c6
            generic map ( t => t_matrix );  -- Error

        p1: process is
            variable v1 : t6(1 to 3);   -- OK
            variable v2 : t6;           -- Error
        begin
            v1(2) := v1(1);             -- OK
            assert v1'length = 3;       -- OK
            wait;
        end process;

    end block;

    b4: block is
        generic (
            type array_type is array(type is (<>)) of type is private );
        generic map ( array_type => integer_vector );

        alias index_type   is array_type'INDEX;  -- OK
        alias element_type is array_type'ELEMENT;  -- OK
    begin
    end block;

    b5: block is
        generic ( type ft is file of type is private );
        generic map ( ft => t8 );
        file f : ft;                    -- OK
    begin
    end block;

    b7: block is
        package gp is
            generic (
                type array_type is array (type is (<>)) of type is private );
            alias index_type   is array_type'INDEX;
            alias element_type is array_type'ELEMENT;
        end package;

        package p is new gp
            generic map ( array_type => bit_vector );

        signal x : p.array_type(1 to 3);  -- OK
        signal y : p.index_type := 5;  -- OK (TODO)
        signal z : p.element_type := '1';  -- OK (TODO)
    begin

    end block;

end architecture;
