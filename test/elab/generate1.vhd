entity generate1 is
end entity;

architecture test of generate1 is

    constant N : integer := 3;

    function init_vec return bit_vector is
        variable r : bit_vector(1 to N);
    begin
        return r;
    end function;

    constant iv : bit_vector := init_vec;
    signal vec : bit_vector(iv'range) := iv;

begin

    g: for i in vec'range generate
        vec(i) <= '1';
    end generate;

end architecture;
