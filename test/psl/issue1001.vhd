entity issue1001 is
end entity;

architecture test of issue1001 is
    signal x, y, clk : bit;

    default clock is clk'event;

    function f (v : bit_vector) return bit is
    begin
        return v(v'left);
    end function;
begin

    assert always onehot(x);            -- Error
    assert always x -> f(y);            -- Error

end architecture;
