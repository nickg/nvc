package pkg is
    generic (g : natural := 0);
    constant plain_c    : natural := 42;  -- ordinary constant
    constant deferred_c : natural;        -- deferred: value given in the body
end package;

package body pkg is
    function helper return natural is
    begin
                        return 99;  -- does not even reference the generic
    end function;

    constant deferred_c : natural := helper;  -- initialised via a function call
end package body;

entity genpack20 is
end entity;

architecture bhv of genpack20 is
    package inst is new work.pkg generic map (g => 10);
    use inst.all;
begin
    process
    begin
        assert plain_c = 42;     -- OK
        assert deferred_c = 99;  -- fails to analyse
        wait;
    end process;
end architecture;
