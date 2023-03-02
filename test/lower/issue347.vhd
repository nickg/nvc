-- test_ng.vhd
package my_logic is
    type std_logic is ('0', '1');
    type std_logic_vector is array (natural range <>) of std_logic;
    type unsigned is array (natural range <>) of std_logic;
    type signed is array (natural range <>) of std_logic;

    function to_integer(x : unsigned) return integer;
    function to_unsigned(x, width : natural) return unsigned;
    function to_01(x : unsigned) return unsigned;
    function resize(x : unsigned; width : natural) return unsigned;
    function "-"(x, y : unsigned) return unsigned;
    function ">"(x : unsigned; y : integer) return boolean;
end package;

package body my_logic is
end package body;

use work.my_logic.all;
entity  TEST_SUB is
    generic (
        SIZE_BITS   : integer := 32;
        COUNT_BITS  : integer := 32
    );
    port (
        POOL_SIZE   : in  std_logic_vector(SIZE_BITS -1 downto 0);
        READY_SIZE  : in  std_logic_vector(SIZE_BITS -1 downto 0);
        COUNT       : out std_logic_vector(COUNT_BITS-1 downto 0)
    );
end TEST_SUB;
architecture RTL of TEST_SUB is
begin
    process (POOL_SIZE, READY_SIZE)
        variable reserve_size  : unsigned(SIZE_BITS-1 downto 0);
        constant MAX_COUNT     : integer := 2**(COUNT'high);
    begin
        reserve_size := to_01(unsigned(POOL_SIZE)) - to_01(unsigned(READY_SIZE));
        if (reserve_size'length > COUNT'length) then
            if (reserve_size > MAX_COUNT) then
                COUNT <= std_logic_vector(to_unsigned(MAX_COUNT    , COUNT'length));
            else
                COUNT <= std_logic_vector(resize     (reserve_size , COUNT'length));
            end if;
        else
                COUNT <= std_logic_vector(resize     (reserve_size , COUNT'length));
        end if;
    end process;
end RTL;

use work.my_logic.all;
entity  TEST_NG is
end     TEST_NG;
architecture RTL of TEST_NG is
    constant  SIZE_BITS  : integer := 8;
    constant  COUNT_BITS : integer := 8;
    signal    POOL_SIZE  : std_logic_vector(SIZE_BITS -1 downto 0);
    signal    READY_SIZE : std_logic_vector(SIZE_BITS -1 downto 0);
    signal    COUNT      : std_logic_vector(COUNT_BITS-1 downto 0);
    component TEST_SUB
        generic (
            SIZE_BITS    : integer := 32;
            COUNT_BITS   : integer := 32
        );
        port (
            POOL_SIZE    : in  std_logic_vector(SIZE_BITS -1 downto 0);
            READY_SIZE   : in  std_logic_vector(SIZE_BITS -1 downto 0);
            COUNT        : out std_logic_vector(COUNT_BITS-1 downto 0)
        );
    end component;
begin
    U: TEST_SUB
        generic map (
            SIZE_BITS   => SIZE_BITS  ,
            COUNT_BITS  => COUNT_BITS
        )
        port map (
            POOL_SIZE   => POOL_SIZE  ,
            READY_SIZE  => READY_SIZE ,
         -- COUNT       => COUNT        -- OK when COUNT
            COUNT       => open         -- NG when open
        );
end RTL;
