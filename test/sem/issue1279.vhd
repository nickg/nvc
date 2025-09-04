package test_vectors is
    type    T_SLM               is array(natural range <>, natural range <>) of bit;
end package;

-----------------------------------------------------------------------------------
use     work.test_vectors.all;

entity Matrix_entity is
    generic (
        PORTS            : positive                  := 2;
        DATA_BITS        : positive                  := 8
	);
    port (
        Out_Matrix      : out T_SLM(PORTS - 1 downto 0, DATA_BITS - 1 downto 0)
	);
end entity;

architecture rtl of Matrix_entity is
begin
    Out_Matrix    <= (others => (others => '0'));  -- Crash here
end architecture;
