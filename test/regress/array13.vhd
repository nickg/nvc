package pack is
    constant LENGTH : natural;
    constant C_MAX_VVC_INSTANCE_NUM : natural;
end package;

package body pack is
    constant LENGTH : natural := 8;
    constant C_MAX_VVC_INSTANCE_NUM : natural := 8;
end package body;

-------------------------------------------------------------------------------

entity array13 is
end entity;

use work.pack.all;

architecture a of array13 is

    -- Reduced from crash in UVVM

    type t_channel is (
        NA,
        ALL_CHANNELS,
        RX, TX );

    subtype t_sub_channel is t_channel range RX to TX;

    type t_base_transaction is record
        x : integer;
        y : bit_vector(1 to LENGTH);
    end record;

    constant C_BASE_TRANSACTION_SET_DEFAULT : t_base_transaction := (
        x => 1, y => (others => '0') );

    type t_transaction_group is record
        bt : t_base_transaction;
    end record;

    constant C_TRANSACTION_GROUP_DEFAULT : t_transaction_group := (
        bt => C_BASE_TRANSACTION_SET_DEFAULT );

    type t_uart_transaction_group_array is array (t_sub_channel range <>, natural range <>) of t_transaction_group;
    shared variable sv : t_uart_transaction_group_array(t_sub_channel'left to t_sub_channel'right, 0 to C_MAX_VVC_INSTANCE_NUM-1) :=
        (others => (others => C_TRANSACTION_GROUP_DEFAULT));

begin

    check: process is
    begin
        assert sv(RX, 1).bt.x = 1;
        sv(RX, 1).bt.y := X"ab";
        wait for 1 ns;
        assert sv(RX, 1).bt.x = 1;
        assert sv(RX, 1).bt.y = X"ab";
        wait;
    end process;

end architecture;
