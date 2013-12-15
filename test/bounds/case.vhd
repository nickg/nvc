entity bounds_case is
end entity;

architecture test of bounds_case is
begin

    process is
        type letter is (A, B, C);
        subtype ab is letter range B to letter'right;
        variable l : letter;
        variable m : ab;
    begin
        case l is                       -- Choice C not covered
            when a =>
                null;
            when b =>
                null;
        end case;
        case m is                       -- Choice B not covered
            when c =>
                null;
        end case;
        wait;
    end process;

end architecture;
