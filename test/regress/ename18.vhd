package pkg is
    type t_rec is record
    	item : bit;
    end record;
end package;

entity dut is
end entity;

architecture rtl of dut is
    signal s : bit_vector(7 downto 0) := (others => '1');
    signal t : work.pkg.t_rec := (item => '1');
begin
end architecture;

entity ename18 is
end entity;

architecture sim of ename18 is
begin
    u_dut: entity work.dut;

    process
        alias ext_item is << signal .ename18.u_dut.t : work.pkg.t_rec >>.item;
    begin
        wait for 1 ns;
        assert << signal .ename18.u_dut.s : bit_vector(7 downto 0) >>(1) = '1';
         assert ext_item = '1';
        wait;
    end process;
end architecture;
