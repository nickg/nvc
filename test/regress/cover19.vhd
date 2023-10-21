library ieee;
use ieee.std_logic_1164.all;

entity cover19 is
end cover19;

architecture test of cover19 is

    -- FSM that is taken int account
    type t_fsm_state is (
        ST_IDLE,
        ST_ONE,
        ST_TWO,
        ST_THREE,
        ST_WAITING,
        ST_DONE
    );

    -- FSM that is left out
    type t_second_fsm is (
        S_1     ,S_2    ,S_3    ,S_4	,S_5
    );

    subtype t_subset_fsm is t_fsm_state range ST_TWO to ST_WAITING;

    signal curr_state : t_fsm_state;
    signal state_reg  : t_second_fsm;
    signal sub_fsm    : t_subset_fsm;

begin

    process begin
        curr_state <= ST_TWO;
        wait for 5 ns;
        curr_state <= ST_WAITING;
        wait for 5 ns;
        curr_state <= ST_DONE;
        wait for 5 ns;

        state_reg <= S_4;
        wait for 5 ns;

        -- Subset FSM
        sub_fsm <= ST_WAITING;
        wait for 5 ns;

        wait;
    end process;

end architecture;
