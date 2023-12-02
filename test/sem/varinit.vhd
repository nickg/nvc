entity computation is
end entity;

package my_logic is
    type std_logic is ('0', '1');
    type std_logic_vector is array (natural range <>) of std_logic;
    type unsigned is array (natural range <>) of std_logic;
    type signed is array (natural range <>) of std_logic;

    function to_integer(x : unsigned) return integer;
end package;

use work.my_logic.all;

architecture foo of computation is
    signal size :std_logic_vector (7 downto 0) := "00001001";
    -- architecture declarative part
begin

UNLABELLED:
    process
        variable N: integer := to_integer(unsigned'("00000111")) ;  ---WORKING
        type memory is array (N downto 0 ) of std_logic_vector (31 downto 0 );
        variable ram:   memory;
    begin
        report "UNLABELLED memory left bound = " &integer'image(N);
        wait;
    end process;

OTHER:
    process
        variable N: integer:= to_integer (unsigned(size)) ; -- Not working
        type memory is array (N downto 0 ) of std_logic_vector (31 downto 0 );
        variable ram:   memory;
    begin
        report "OTHER      memory left bound = " &integer'image(N);
        wait;
    end process;

    size <= "01000010" after 1 ns;

    block1: block is
        constant  N: integer:= to_integer (unsigned(size)) ; -- Error
        constant  M: integer := size'length;  -- OK
        constant  P: boolean := size'event;  -- Error
    begin
    end block;

end architecture;

architecture bar of computation is
    signal N : integer := 5;
    signal bad : bit_vector(1 to N);    -- Error
    signal x : integer range 1 to N;    -- Error
    signal y : bit_vector(1 to bad'length);  -- OK
    subtype my_int is integer range 1 to N;  -- Error
begin
end architecture;
