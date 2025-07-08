package pkg is
    type logic_pair_t is record
        l0 : bit;
        l1 : bit;
    end record;

    type logic_pair_arr_t is array (natural range <>) of logic_pair_t;

    type bit_arr_params is record
        init_value : logic_pair_t;
        len        : natural;
    end record bit_arr_params;

    function init(params : bit_arr_params)
    return logic_pair_arr_t;

end package pkg;

package body pkg is
    function init(params : bit_arr_params)
    return logic_pair_arr_t is
        variable value : logic_pair_arr_t(params.len - 1 downto 0) := (others => params.init_value);
    begin
        return value;
    end function init;

end package body pkg;

-------------------------------------------------------------------------------

use work.pkg.all;

entity ent is
    generic(
        params : bit_arr_params
    );
    port(
        -- Both lines cause a failure
        rec_out : out logic_pair_arr_t := init(params)
        -- rec_out : out logic_pair_arr_t := init((
        --     len        => 5,
        --     init_value => (l0 => '0', l1 => '1')
        -- ))
        -- but this works
        -- rec_out: out logic_pair_arr_t(params.len - 1 downto 0) := (others => params.init_value)
    );
end entity ent;

architecture RTL of ent is

begin

end architecture RTL;

-------------------------------------------------------------------------------

use work.pkg.all;

entity issue1234 is
end entity issue1234;

architecture RTL of issue1234 is
    -- signal rec0 : rec_t(rec0(1 downto 0));
    constant params : bit_arr_params := (
        len        => 5,
        init_value => (l0 => '0', l1 => '1')
    );

    signal rec_out : logic_pair_arr_t(params.len - 1 downto 0);

begin
    ent_inst : entity work.ent
        generic map(
            params => params
        )
        port map(
            rec_out => rec_out
        );
end architecture RTL;
