entity cover24 is
end entity;

architecture test of cover24 is

    signal a,b,c : integer := 0;
    signal bool : boolean;

begin

   -- Conditional statement is used since the trailing "c"
   -- was causing a bug in the implementation. After fixing this
   -- bug, a "per-file" vs. "per-hierarchy" report numbers matched
   -- on larget project.
   a <= b   when (bool) else
        c+b when (not bool) else c;

   process
   begin
    wait for 1 ns;
    report "Running";
    wait for 1 ns;
    wait;
   end process;

end architecture;
