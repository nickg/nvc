entity psl_multiline is
end entity;

architecture tb of psl_multiline is
    signal a,b,c : bit;
begin

    -- psl cov_1 : cover
    --  {a = '1' and
    --   b = '1' and
    --   c = '1'};

    -- psl cov_2 : cover
    --  {a = '1' and b = '0'};

end architecture;
