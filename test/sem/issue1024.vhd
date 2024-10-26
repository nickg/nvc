entity delayline is
    generic (
        delay : positive
    );
    port (
        i     : in bit_vector;
        o     : out bit_vector
    );
end entity;

architecture rtl of delayline is
    type delay_t is array (natural range <>) of bit_vector;
    signal d : delay_t(delay - 1 downto 0)(i'range); -- This statement causes the problem
begin
    (o, d) <= d & i;                    -- OK

    issue1028: block is
        type bitvv is array(natural range <>) of bit_vector;
	signal foo: bitvv(2 downto 0)(0 downto 0);
    begin
        foo <= bitvv'(                  -- OK
            2 downto 1 => foo(1 downto 0),
            0 => foo(2) );
    end block;

end architecture rtl;
