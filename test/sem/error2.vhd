package error2 is
    subtype char128 is character range (NUL to DEL);  -- Error

    -- Crash here
    constant k : string := IfElse(false, "Status: " & IfElse(true, "PASSED", "FAILED") & LF, "");
end package;
