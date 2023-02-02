package cond1 is

    `if TOOL_NAME = "false" then
    `error "Should not be here"
    constant d : integer := 1;
    `else
    constant c : integer := 1;
    `end if

    `warning "this is a warning"

    `if TOOL_NAME = "nvc" then
    `warning "Using nvc"
    `end if

    `if not (TOOL_TYPE = "SIMULATION") then
    `error "Should not be here"
    `end if

    `if TOOL_TYPE /= "SYNTHESIS" and TOOL_NAME = "nvc" then
    `warning "correct"
    `end if

    `if VHDL_VERSION = "1993" then
    `warning "VHDL version is correct"
    `end if

end package;

package cond2 is

    `if FOO = "bar" then
    `end

    `if TOOL_NAME = "ghdl" then
    -- Unterminated
end package;
