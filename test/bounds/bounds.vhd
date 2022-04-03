entity bounds is
end entity;

architecture test of bounds is
    type foo is range 1 to 5;
    type my_vec1 is array (positive range <>) of integer;
    type my_vec2 is array (foo range <>) of integer;

    signal s : my_vec1(1 to 10);
    signal n : my_vec1(1 downto 10);

    subtype bool_true is boolean range true to true;

    function fun(x : in bit_vector(7 downto 0)) return bit;
    procedure proc(x : in bit_vector(7 downto 0));
    function natfunc(x : in natural) return boolean;
    function enumfunc(x : in bool_true) return boolean;
    function realfunc(x : in real) return boolean;

    type matrix is array (integer range <>, integer range <>) of integer;

    procedure proc2(x : in matrix(1 to 3, 1 to 3));
begin

    process is
        variable a : my_vec1(0 to 10);  -- Error
        variable b : my_vec2(1 to 60);  -- Error
    begin
    end process;

    s(-52) <= 5;                        -- Error
    s(1 to 11) <= (others => 0);        -- Error
    s(0 to 2) <= (others => 0);         -- Error

    process is
    begin
        report (0 => 'a');              -- Error
    end process;

    process is
        variable v1 : bit_vector(3 downto 0);
        variable v2 : bit_vector(8 downto 1);
        variable m1 : matrix(1 to 3, 2 to 4);
        variable m2 : matrix(1 to 3, 1 to 4);
    begin
        assert fun(v1) = '1';           -- Error
        proc(v1);                       -- Error
        proc(v2);                       -- OK
        proc2(m1);                      -- OK
        proc2(m2);                      -- Error
    end process;

    s <= s(1 to 9);                     -- Error
    n <= s(1 to 2);                     -- Error
    n <= (1, 2, 3);                     -- Error

    process is
        variable v : my_vec1(1 to 3);
    begin
        v := s;                         -- Error
    end process;

    process is
        variable x : integer;
    begin
        x := s(11);                     -- Error!
        x := s(-1);                     -- Error!
    end process;

    process is
        variable a : my_vec1(1 to 3);
    begin
        a := (1, 2, 3);                 -- OK
        a := (5 => 1, 1 => 2, 0 => 3);  -- Error
    end process;

    process is
        subtype alpha is character range 'a' to 'z';
        variable a : alpha;
        variable p : positive;
    begin
        a := 'c';                       -- OK
        a := '1';                       -- Error
        p := 0;                         -- Error
    end process;

    process is
    begin
        assert s'length(5) = 5;         -- Error
    end process;

    process is
    begin
        assert natfunc(-1);             -- Error
    end process;

    process is
        subtype str is string;
        constant c : str := "hello";    -- OK
    begin
    end process;

    process is
        variable a : my_vec1(1 to 3);
    begin
        a := (1, others => 2);          -- OK
        a := (5 => 1, others => 2);     -- Error
    end process;

    process is
        type mat2d is array (integer range <>, integer range <>)
            of integer;

        procedure p(m : in mat2d);
    begin
        p(((0, 1, 2, 3), (1 to 2 => 5)));  -- Error
    end process;

    -- Reduced from Billowitch tc1374
    process is
        type t_rec3 is record
            f1 : boolean;
        end record;
        subtype  st_rec3 is t_rec3 ;
        type     t_arr3 is array (integer range <>, boolean range <>) of st_rec3 ;
        subtype  st_arr3 is t_arr3 (1 to 5, true downto false) ;
        variable v_st_arr3 : st_arr3;
    begin
        v_st_arr3(1, true) := (f1 => false);
    end process;

    process is
        variable i : integer;
        attribute a : bit_vector;
        attribute a of i : variable is "101";
    begin
        assert i'a(14) = '0';           -- Error
    end process;

    process is
        constant FPO_LOG_MAX_ITERATIONS : integer := 9;
        type T_FPO_LOG_ALPHA is array (0 to FPO_LOG_MAX_ITERATIONS-1) of integer;
        variable alpha : T_FPO_LOG_ALPHA;
    begin
        if alpha(0 to 5) = (5, 4, 6, 6, 6, 6) then  -- OK
            null;
        end if;
    end process;

    process is
        procedure real_proc(x : in real range 0.0 to 1.0);
    begin
        real_proc(0.0);                 -- OK
        real_proc(1.0);                 -- OK
        real_proc(2.0);                 -- Error
    end process;

    process is
        type e is (one, two, three, four, five);

        subtype se is e range two to four;
        type t_arr is array (se range <>) of boolean;

        constant c1 : t_arr(two to four) := (true, true);
        constant c2 : t_arr(two to four) := (true, true, true, true);

        procedure enum_proc(
            arg1 : e range two to four;
            arg2 : e range three downto two
        ) is
        begin
        end procedure;
    begin
        enum_proc(arg1 =>   two, arg2 =>   two);    -- ok
        enum_proc(arg1 => three, arg2 =>   one);    -- Error
        enum_proc(arg1 =>  four, arg2 =>  four);    -- Error
        enum_proc(arg1 =>   one, arg2 => three);    -- Error
        enum_proc(arg1 =>  five, arg2 => three);    -- Error
    end process;

    process is
        type e is (one, two, three, four, five);
        type t_arr is array (two to four) of integer;
        variable a : t_arr;
    begin
        a := (1, others => 2);          -- OK
        a := (two => 1, others => 2);   -- OK
        a := (one => 1, others => 2);   -- Error
        a := (two to four => 1, others => 2);   -- OK
        a := (one to five => 1, others => 2);   -- Error
    end process;

    process is
        type e is (one, two, three, four, five);
        type mat2d is array (e range <>, e range <>) of integer;
        procedure p(m : in mat2d);
    begin
        p(((0, 1, 2, 3), (one to three => 5)));  -- Error
    end process;

    process is
        type e is (one, two, three, four, five);
        subtype se is e range two to three;
        type arr is array (se range <>) of integer;
        variable v1 : arr(two to three);        -- OK
        variable v2 : arr(one to four);         -- Error
    begin
    end process;

    process is
        procedure phys_proc_to(a : in time range 0 ns to 10 ns) is
        begin
        end procedure;
        procedure phys_proc_dt(a : in time range 10 sec downto 20 us) is
        begin
        end procedure;
    begin
        phys_proc_to(5 ns);      -- OK
        phys_proc_to(-5 ns);     -- Error
        phys_proc_dt(1 ms);      -- OK
        phys_proc_dt(5 ns);      -- Error
    end process;

    process
        variable t : time range -10 ns to 10 ns;
    begin
        t := 200 ns;            -- Error
        t := -200 ns;           -- Error
        t := 0 ns;              -- OK
    end process;

    process is
        type char_map is array (character range 'a' to 'z') of integer;
        variable m : char_map;
    begin
        m('A') := 1;                    -- Error
        m('1' to '3') := (others => 3);  -- Error
    end process;

    process is
        variable v : bit_vector(1 to 3);
        alias a : bit_vector(1 to 4) is v;  -- Error
    begin
        a(4) := '1';                    -- OK
    end process;

end architecture;
