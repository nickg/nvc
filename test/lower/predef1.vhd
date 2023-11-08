entity sub is
end entity;

architecture test of sub is
begin

    b1: block is
        type t_rec is record
            x, y : integer;
        end record;
    begin

        p1: process is
            variable v : t_rec;
        begin
            assert v = (1, 2);
            wait;
        end process;

    end block;

end architecture;

-------------------------------------------------------------------------------

entity predef1 is
end entity;

architecture test of predef1 is

    function f (a_width, b_width, depth : integer) return integer is
        type a is array (0 to depth - 1) of bit_vector(a_width + b_width - 1 downto 0);
    begin
        return 0;
    end function;


begin

    u1: entity work.sub;
    u2: entity work.sub;                -- Only one "=" predef generated

end architecture;
