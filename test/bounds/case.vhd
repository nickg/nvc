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

    process is
        variable x : integer;
        variable y : natural;
    begin
        case x is                       -- Missing range
            when 0 to 9 =>
                null;
            when 20 =>
                null;
        end case;
        case x is                       -- Missing range
            when 1 | 2 | 3 =>
                null;
        end case;
        case x is                       -- OK
            when others =>
                null;
        end case;
        case x is                       -- Missing integer'right
            when integer'left to integer'right - 1 =>
                null;
        end case;
        case x is
            when 1 to 100 =>
                null;
            when 50 =>                  -- Duplicate
                null;
            when 60 to 64 =>            -- Duplicate
                null;
            when others =>
                null;
        end case;
        case y is
            when -1 =>                  -- Out of range
                null;
        end case;
    end process;

end architecture;
