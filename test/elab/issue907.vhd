entity A is
    port (
        clk : in bit
        );
end A;

architecture rtl of A is
type f is (ONE, TWO);
signal e : f;
begin
end architecture;

entity B is
end B;

architecture sim of B is
type f is (ONE, TWO);
begin
E_PROC : process
alias e is <<signal .ISSUE907.C.DUT.e : f>>;
begin
loop -- elaborates if commented out
wait on e;
end loop; -- elaborates if commented out
end process;
end architecture;

entity C is
end C;

architecture sim of C is
signal clk : bit := '1';
begin
DUT : entity work.A(rtl)
port map (
clk => clk
);

B : entity work.B(sim);
end architecture;

entity issue907 is
end issue907;

architecture sim of issue907 is
begin
C : entity work.C(sim);

process
begin
wait;
end process;
end architecture;
