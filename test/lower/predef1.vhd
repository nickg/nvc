entity predef1 is
end entity;

architecture test of predef1 is

    function f (a_width, b_width, depth : integer) return integer is
        type a is array (0 to depth - 1) of bit_vector(a_width + b_width - 1 downto 0);
    begin
        return 0;
    end function;

begin

end architecture;
