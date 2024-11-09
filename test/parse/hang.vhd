entity ent is begin
end entity ent;

architecture arch of ent is
    constant i : integer := 0e100000000000; -- used to hang here
begin
end architecture arch;
