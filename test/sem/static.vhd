entity static is
    generic ( G : integer := 1 );
end entity;

architecture test of static is
begin

    process is
        subtype byte is bit_vector(7 downto 0);
        variable bv : byte;
        variable i  : integer;

        attribute hello : integer;
        attribute hello of bv : variable is 6;
    begin
        case i is
            when bv'length =>           -- OK
                null;
            when bv'left =>             -- OK
                null;
            when byte'right =>          -- OK
                null;
            when bv'hello =>            -- OK
                null;
            when others =>
                null;
        end case;
    end process;

    process is
        variable v : bit_vector(3 downto 0);
        constant c : bit_vector := "1010";
        constant d : bit_vector(G downto 0) := (others => '0');
    begin
        case v is
            when c =>                   -- OK
                null;
            when others =>
                null;
        end case;
        case v is
            when d =>                   -- Error
                null;
            when others =>
                null;
        end case;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity sub is
    generic ( N : integer );
    port ( x : bit_vector );
end entity;

architecture test of sub is
    signal y : bit_vector(N - 1 downto 0) := (others => '0') ;
begin

    sub_i: entity work.sub
        generic map ( N => N )
        port map (
            x => x(x'left downto x'right) );  -- Error

    gen1: for i in y'range generate     -- OK
    end generate;

    b1: block is
        type r is record
            x, y : integer;
        end record;
        signal x : r := (1, 2);
    begin

        gen2: if (N, 2) = r'(1, 2) generate    -- OK
        end generate;

    end block;

    sub2_i: entity work.sub
        generic map ( N => N )
        port map (
            x(N downto 0) => x );  -- Error

    process is
        type rec is record
            f1, f2 : integer;
        end record;

        subtype rs is rec;                  -- OK
        constant rc : rs := (0, 0);         -- OK
        constant i : integer := rc.f1;      -- OK
    begin
    end process;

    p1: process is
        constant t : time := -5 ns;         -- OK, globally static
    begin
    end process;

    p2: process is
        constant n : integer := bad_func(2);    -- Error
        subtype t is integer range 0 to n - 1;  -- Crash in sem_locally_static
    begin
    end process;

    p3: process is
        constant STR : string := "abc";
        variable v : string(1 to 3);
    begin
        case v is
            when STR => null;           -- OK
        end case;
    end process;

end architecture;
