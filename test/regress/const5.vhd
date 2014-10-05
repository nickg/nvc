entity const5 is
end entity;

architecture test of const5 is
    subtype byte_t is bit_vector(7 downto 0);
    type byte_array_t is array (natural range <>) of byte_t;

    procedure popcount_assert
        ( value : in byte_t;
          pop   : in integer )
    is
        variable cnt : natural := 0;
    begin
        for i in value'range loop
            if value(i) = '1' then
                cnt := cnt + 1;
            end if;
        end loop;
        report integer'image(cnt);
        assert cnt = pop;
    end procedure;

begin

    stim_p: process is
        constant COME_ADDRS : byte_array_t := ( X"08", X"10", X"20" );
    begin
        for i in 0 to 2 loop
            popcount_assert(value => COME_ADDRS(i), pop => 1);
        end loop;
        wait;
    end process;

end architecture;
