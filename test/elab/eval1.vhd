entity sub is
    generic (
        N : integer );
end entity;

architecture test of sub is
    signal x : integer;
    constant c : bit_vector(7 downto 0) := X"12";

    function func(a : integer) return bit is
    begin
        return c(a);
    end function;
begin

    g: if func(N) = '1' generate              -- Error
        x <= 5;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity eval1 is
end entity;

architecture test of eval1 is
begin

    sub_i: entity work.sub
        generic map (-1);

end architecture;
