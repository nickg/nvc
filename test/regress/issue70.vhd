entity issue70 is
end;

architecture test of issue70 is

    procedure my_write(value : in bit_vector) is
        variable s: bit_vector(1 to value'length);
        variable m: bit_vector(1 to value'length) := value;
    begin
        for i in 1 to value'length loop
            s(i) := m(i);
        end loop;
        wait for 1 ns;
        assert s = m;
    end procedure;

begin

  process is
      variable count : bit_vector(3 downto 0);
  begin
      my_write(count);
      wait;
  end process;

end architecture;
