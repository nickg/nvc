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

    process is
        variable x : bit_vector(1 to 3);
        variable y : bit_vector(0 to 0);
        subtype small is character range 'a' to 'k';
        type char_vector is array (integer range <>) of character;
        type small_vector is array (integer range <>) of small;
        variable p : char_vector(1 to 2);
        variable q : small_vector(1 to 2);
    begin
        case y is
            when "0" =>
                null;
            when "1" =>
                null;
        end case;                       -- OK
        case x is
            when "000" | "001" =>
                null;
        end case;                       -- Missing 6 values
        case x is
            when ('0', '1') =>          -- Too few values
                null;
            when ('1', '0', '1', '1') =>     -- Too many values
                null;
            when "10" =>                -- Too few values
                null;
            when "1111" =>              -- Too many values
                null;
            when others =>
                null;
        end case;
        case p is
            when ('0', '1') =>
                null;
            when ('1', '1') =>
                null;
        end case;                       -- Missing lots of values
        case q is
            when ('0', '1') =>
                null;
            when ('1', '1') =>
                null;
        end case;                       -- Missing 98 values
    end process;

    process is
        type my_int is range 10 downto 1;
        variable x : my_int;
    begin
        case x is
            when 10 downto 4 => null;
        end case;                       -- Error
        case x is
            when 11 downto 3 => null;   -- Error
            when 2 downto 1 => null;
        end case;
    end process;

end architecture;
