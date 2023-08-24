entity issue751 is
end entity;

architecture test of issue751 is
begin

    -- No Label on this process
    process is                          -- _P0
        function outer return integer is  -- WORK.ISSUE751-TEST._P0.OUTER
            function inner return integer is  -- WORK.ISSUE751-TEST._P0.OUTER.INNER
            begin
                return 42;
            end function;
        begin
            return inner;
        end function;
        type abc is (A, B, C);   -- WORK.ISSUE751-TEST._P0.ABC
    begin
        wait;
    end process;

end architecture;
