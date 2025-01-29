entity debug4 is
end entity;

architecture test of debug4 is

    function func2 (x : bit_vector) return bit_vector is
    begin
        report "warning" severity warning;
        return x;
    end function;

    function func1 (x : bit_vector) return bit_vector is
    begin
        return func2(x);
    end function;

    signal s1 : bit_vector(7 downto 0);
    signal s2 : bit_vector(3 downto 0);
begin

    b: block is
        port (p1 : in bit_vector(3 downto 0);
              p2 : out bit_vector(7 downto 0));
        port map (p1 => func1(s1(3 downto 0)),
                  func1(p2(3 downto 0)) => s2);
    begin
    end block;

end architecture;
