entity logical2 is
end entity;

architecture test of logical2 is
    signal x    : bit;
    signal one  : bit := '1';
    signal zero : bit := '0';
    signal vec  : bit_vector(0 to 1) := ('0', '1');
begin

    process is
        variable v : boolean := true;
    begin
        x <= '0';
        wait for 1 ns;
        assert (x and zero) = zero;
        assert (x and one) = zero;
        assert (x or zero) = zero;
        assert (x or one) = one;
        assert (x xor zero) = zero;
        assert (x xor one) = one;
        assert (x xnor zero) = one;
        assert (x xnor one) = zero;
        assert (x nand zero) = one;
        assert (x nand one) = one;
        assert (x nor zero) = one;
        assert (x nor one) = zero;

        x <= '1';
        wait for 1 ns;
        assert (x and zero) = zero;
        assert (x and one) = one;
        assert (x or zero) = one;
        assert (x or one) = one;
        assert (x xor zero) = one;
        assert (x xor one) = zero;
        assert (x xnor zero) = zero;
        assert (x xnor one) = one;
        assert (x nand zero) = one;
        assert (x nand one) = zero;
        assert (x nor zero) = zero;
        assert (x nor one) = zero;

        v := v and v; assert v;
        v := v or v; assert v;
        v := v nand v; assert not v;
        v := v nor v; assert v;
        v := v xor v; assert not v;
        v := v xnor v; assert v;
        v := v xnor v; assert v;

        -- This tests short circuiting
        x <= '0';
        wait for 1 ns;
        assert (x and vec(0)) = zero;
        assert (x and vec(1)) = zero;
        assert (x or vec(0)) = zero;
        assert (x or vec(1)) = one;
        assert (x xor vec(0)) = zero;
        assert (x xor vec(1)) = one;
        assert (x xnor vec(0)) = one;
        assert (x xnor vec(1)) = zero;
        assert (x nand vec(0)) = one;
        assert (x nand vec(1)) = one;
        assert (x nor vec(0)) = one;
        assert (x nor vec(1)) = zero;

        x <= '1';
        wait for 1 ns;
        assert (x and vec(0)) = zero;
        assert (x and vec(1)) = one;
        assert (x or vec(0)) = one;
        assert (x or vec(1)) = one;
        assert (x xor vec(0)) = one;
        assert (x xor vec(1)) = zero;
        assert (x xnor vec(0)) = zero;
        assert (x xnor vec(1)) = one;
        assert (x nand vec(0)) = one;
        assert (x nand vec(1)) = zero;
        assert (x nor vec(0)) = zero;
        assert (x nor vec(1)) = zero;

        wait;
    end process;

end architecture;
