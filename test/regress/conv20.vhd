entity conv20 is
end entity;

architecture test of conv20 is
    subtype real_vector_30 is real_vector(1 to 30);
    subtype integer_vector_30 is integer_vector(1 to 30);

    function to_real3 (x : integer_vector) return real_vector_30 is
        variable r : real_vector(1 to 30);
        alias xx : integer_vector(1 to 30) is x;
    begin
        for i in 1 to 30 loop
            r(i) := real(xx(i));
        end loop;
        return r;
    end function;

    function to_int3 (x : real_vector) return integer_vector_30 is
        variable r : integer_vector(1 to 30);
        alias xx : real_vector(1 to 30) is x;
    begin
        for i in 1 to 30 loop
            if xx(i) < real(integer'left) then
                r(i) := integer'left;
            elsif xx(i) > real(integer'right) then
                r(i) := integer'right;
            else
                r(i) := integer(xx(i));
            end if;
        end loop;
        return r;
    end function;

    signal si, so : integer_vector(1 to 30) := (others => 0);
begin

    b: block is
        port ( pi : in real_vector;
               po : out real_vector );
        port map ( pi => to_real3(si),
                   to_int3(po) => so );
    begin
        process (pi) is
        begin
            for i in pi'range loop
                if pi(i) > 0.0 then
                    po(i) <= 1.0;
                end if;
            end loop;
        end process;
    end block;

    check: process is
        variable idx : integer;
    begin
        idx := 3;
        wait for 1 ns;
        si(idx) <= 5;
        wait on so(2);                  -- Splits SO
        wait for 1 ns;
        assert so(3) = 1;

        wait;
    end process;

end architecture;
