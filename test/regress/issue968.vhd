entity issue968 is
end entity;

architecture tb of issue968 is
  -- changing this to bit does not crash
  signal foo: bit_vector(1 downto 0);
  signal done : boolean;
begin

    assign: foo(0) <= '1';

    stim: process is
    begin
        wait until foo'stable;
        report "foo is stable";
        wait for 1 ns;
        foo(1) <= '1';
        wait for 0 ns;
        assert not foo'stable;
        wait for 1 ns;
        assert foo'stable;
        done <= true;
    end process;

    check: process is
    begin
        report "0";
        assert foo'stable;
        wait for 0 ns;
        report "1";
        assert not foo'stable;
        wait for 0 ns;
        report "2";
        assert foo'stable;
        wait for 5 ns;
        assert done;
        wait;
    end process;
end architecture tb;
