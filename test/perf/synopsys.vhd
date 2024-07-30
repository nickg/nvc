package synopsys_perf is
    procedure test_unsigned_plus;
    procedure test_signed_plus;
    procedure test_slv_plus;
    procedure test_std_logic_plus;
    procedure test_mul_signed;
    procedure test_mul_unsigned;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

package body synopsys_perf is

    procedure test_unsigned_plus is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        constant ONE   : unsigned(WIDTH - 1 downto 0) := conv_unsigned(1, WIDTH);
        variable s     : unsigned(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := s + ONE;
            end loop;
        end loop;
    end procedure;

    procedure test_signed_plus is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        constant ONE   : signed(WIDTH - 1 downto 0) := conv_signed(1, WIDTH);
        variable s     : signed(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := s + ONE;
            end loop;
        end loop;
    end procedure;

    procedure test_slv_plus is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        constant ONE   : std_logic_vector(WIDTH - 1 downto 0) := (0 => '1', others => '0');
        variable s     : std_logic_vector(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := s + ONE;
            end loop;
        end loop;
    end procedure;

    procedure test_std_logic_plus is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        variable s     : unsigned(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := s + '1';
                s := '1' + s;
            end loop;
        end loop;
    end procedure;

    procedure test_mul_unsigned is
        constant WIDTH : integer := 17;
        constant ITERS : integer := 15;
        constant two   : unsigned(WIDTH - 1 downto 0) := conv_unsigned(2, WIDTH);
        variable accum : unsigned(WIDTH - 1 downto 0) := conv_unsigned(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := conv_unsigned(accum * two, WIDTH);
        end loop;
        assert accum = conv_unsigned(2 ** ITERS, WIDTH);
    end procedure;

    procedure test_mul_signed is
        constant WIDTH : integer := 17;
        constant ITERS : integer := 15;
        constant two   : signed(WIDTH - 1 downto 0) := conv_signed(2, WIDTH);
        variable accum : signed(WIDTH - 1 downto 0) := conv_signed(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := conv_signed(accum * two, WIDTH);
        end loop;
        assert accum = conv_signed(2 ** ITERS, WIDTH);
    end procedure;

end package body;
