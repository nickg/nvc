entity issue926 is
    generic (g : integer;
             type t);
end entity;

architecture test of issue926 is
    function f (x : integer) return integer is
    begin
        return x;
    end function;
begin

    b: block is
        generic (type t);
        generic map ( t => bit_vector(f(g) - 1 downto 0) );  -- Error
    begin
    end block;

end architecture;
