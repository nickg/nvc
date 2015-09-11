entity sub_ent is
end entity;

architecture a of sub_ent is
begin
end architecture a;

entity test is
end entity;

architecture test1 of test is
begin
entity work.sub_ent;    -- unlabeled entity instantiation
end architecture;

architecture test2 of test is
begin
fg1: for ii in 0 to 0 generate
begin
end generate;

block   -- unlabeled block
begin
end block;
end architecture;

architecture test3 of test is
    component comp is port (a: boolean);
    end component;
    signal s_ok: boolean;
begin
comp port map (a => s_ok);  -- unlabeled component instantiation
end architecture;

architecture test5 of test is
begin
if true generate            -- unlabeled if-generate
begin
end generate;
end architecture;

architecture test6 of test is
    component comp is port (a: boolean);
    end component;
    signal s_ok: boolean;
begin
-- include labeled testcases to make sure they are ok
e1: entity work.sub_ent;
b1: block
begin
end block;
c1: comp port map (a => s_ok);

ig1: if true generate
begin
end generate;

for ii in 0 to 0 generate   -- unlabeled for-generate
begin
end generate;
end architecture;
