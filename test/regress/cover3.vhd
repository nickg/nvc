
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover3 is
end entity;

architecture test of cover3 is

    signal cnt : integer := 0;
    signal cnt2 : integer := 0;
    signal cnt3 : unsigned(3 downto 0) := "0000";
begin

    -- Test control
    process
    begin
        wait for 1 ns;
        if (cnt < 20) then
            cnt <= cnt + 1;
        else
            wait for 1 ns;
            wait;
        end if;
    end process;

    process
    begin
        cnt2 <= 1;
        wait for 1 ns;
        cnt2 <= 2;
        wait for 1 ns;
        cnt2 <= 3;
        wait for 1 ns;
        wait;
    end process;

    -- IF / ELSE
    process(cnt)
    begin
        l_if_1: if (cnt = 0) then report "IF1: CNT = 0";
        elsif (cnt = 1) then      report "IF1: CNT = 1";
        -- coverage off
        elsif (cnt = 10) then     report "IF1: CNT = 10";
        -- coverage on
        else                      report "IF1: CNT other";
        end if;

        -- With implicit else, only counts due to "false of" with last valued branch
        l_if_2: if (cnt = 10) then report "IF2: CNT = 10";
        elsif (cnt = 11) then      report "IF2: CNT = 11";
        end if;

        -- With some branches uncovered
        l_if_3: if (cnt = 100) then assert (False);
        elsif (cnt = 19) then       report "IF3: CNT = 19";
        elsif (cnt = 70) then       assert (False);
        end if;
    end process;

    -- T_CASE with scalar
    process(cnt)
    begin

        -- Case with all options covered
        l_case_1: case (cnt) is
        when 0 => report "CASE1: CNT = 0";
        -- coverage off
        when 1 => report "CASE1: CNT = 1";
        -- coverage on
        when 2 => report "CASE1: CNT = 2";
        when 3 => report "CASE1: CNT = 3";
        when others => report "CASE1: CNT = OTHERS";
        end case;

        -- Case with some explicit branches uncovered
        l_case_1: case (cnt) is
        when 28 => assert (False);
        when 29 => assert (False);
        when 7  => report "CASE2: CNT = 7";
        when 8  => report "CASE2: CNT = 8";
        when others => report "CASE2: CNT = OTHERS";
        end case;

    end process;


    process(cnt2)
    begin
        -- Case with only others uncovered
        l_case_3: case (cnt2) is
        when 0 => report "CASE3: CNT2 = 0";
        when 1 => report "CASE3: CNT2 = 1";
        when 2 => report "CASE3: CNT2 = 2";
        when 3 => report "CASE3: CNT2 = 3";
        when others => assert (False);
        end case;

    end process;

    process
    begin
        wait for 1 ns;
        if (cnt3 < "0111") then
            cnt3 <= cnt3 + "0001";
        else
            wait;
        end if;
    end process;

    process(cnt3)
    begin
        -- Case on array, others covered, some options uncovered
        l_case_4: case (cnt3) is
        when "0000" => report "CASE4: 0000";
        when "0001" => report "CASE4: 0001";
        when "0010" => report "CASE4: 0010";
        when "0ZZ1" => assert(False);
        when "0011" => report "CASE4: 0011";
        when "0101" | "0110" | "0111" => report "CASE4: 5-7";
        when "1000" => assert(False);
        when others => report "CASE4: others (hit by 4)";
        end case;
    end process;

    process(cnt3)
    begin
        -- Case on array, only others uncovered
        l_case_4: case (cnt3) is
        when "0000" | "0001" | "0010" | "0011" | "0100" | "0101" | "0110" | "0111" => report "CASE5: Coverable";
        when others => assert(False);
        end case;
    end process;

end architecture;
