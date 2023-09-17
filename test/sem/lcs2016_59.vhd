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
    signal s6 : t6;
    signal s7 : t7;
    signal s8 : t8;
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

    assert s2 < s2;                     -- OK

    u8: component c3
        generic map ( t => integer );   -- OK

    u9: component c3
        generic map ( t => bit );       -- Error

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

    assert s5 + 1.0 = 0.0;              -- OK
    assert s5 * s5 = s5;                -- OK

end architecture;
