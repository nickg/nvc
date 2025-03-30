entity rand1 is
end entity;

library nvc;
use nvc.random.all;

architecture test of rand1 is
begin

    -- Assumes --seed=123
    process is
    begin
        assert get_random = 2991312382;
        assert get_random = 3062119789;
        assert get_random = 1228959102;
        assert get_random = 1840268610;
        assert get_random = 974319580;
        assert get_random = 2967327842;
        assert get_random;
        assert not get_random;
        assert not get_random;
        assert get_random;
        wait;
    end process;

end architecture;
