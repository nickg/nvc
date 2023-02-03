package issue604 is
    `if TOOL_NAME = "nvc" then
    constant CHECK1 : string := "NVC" ;
    `else
    constant CHECK1 : string := "UNDEFINED" ;
    `end

    `if TOOL_NAME = "ghdl" then
    `warning "Tool is GHDL"
    `elsif TOOL_NAME = "nvc" then
    `warning "Tool is NVC"
    `elsif TOOL_NAME = "foobar" then
    `error "error"
    `else
    `error "Should not reach here"
    `end if

    `elsif TOOL_VERSION >= "3000" then
    `end if

`if TOOL_TYPE = "SYNTHESIS" then
    `if TOOL_NAME = "Vivado" then
        constant CHECK : string := "x" ;
    `else
        constant CHECK : string := "UNDEFINED_SYNTH" ;
    `end
`else
    `if TOOL_NAME = "Riviera-PRO" then
        constant CHECK : string := "y" ;
    `elsif TOOL_NAME = "nvc" then
        constant CHECK : string := "z" ;
    `else
        constant CHECK : string := "UNDEFINED_SIM" ;
    `end
`end

end package;
