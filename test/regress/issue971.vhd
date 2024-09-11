entity mre_conversion is
  port (din : in bit_vector(63 downto 0));
end entity mre_conversion;

architecture arch of mre_conversion is
begin
    check: process is
    begin
        assert din = X"0000_0000_0000_0000";
        wait for 2 ns;
        assert din = X"0000_1234_0000_0000";
        wait for 5 ns;
        assert din = X"6666_1234_0000_0000";
        wait;
    end process;
end arch;

entity issue971 is
end entity issue971;

architecture arch of issue971 is
 type T_RECORD is record
     element : bit_vector(15 downto 0);
 end record T_RECORD;

 type T_COMPOUND is array (3 downto 0) of T_RECORD;
 type T_COMPOUND_SINGLE is array (3 downto 0) of bit_vector(15 downto 0);

 function f1(inp : T_COMPOUND_SINGLE) return bit_vector is
     variable result : bit_vector(63 downto 0);
 begin
     for i in inp'range loop
         result((i + 1) * 16 - 1 downto i * 16) := inp(i);
     end loop;
     return result;
 end function f1;

 function f2(inp : T_COMPOUND) return T_COMPOUND_SINGLE is
     variable result : t_compound_single;
 begin
     for i in inp'range loop
         result(i) := inp(i).element;
     end loop;
     return result;
 end function f2;

 signal input_signal: T_COMPOUND;

begin

  mre_conversion_inst: entity work.mre_conversion
  port map (
      din => f1(f2(input_signal))
  );

  stim: process is
  begin
      wait for 1 ns;
      input_signal(2).element <= X"1234";
      wait for 5 ns;
      input_signal(3).element <= X"6666";
      wait;
  end process;

end architecture arch;
