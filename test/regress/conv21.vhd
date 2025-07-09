entity conv21 is
end entity;

architecture test of conv21 is
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
        po(1) <= pi(1) * 3.0;
        po(2 to po'length) <= pi(2 to pi'length);
    end block;

    check: process is
        variable idx : integer;
    begin
        si <= (1, others => 0);
        wait for 1 ns;
        assert so = (1 => 3, 2 to 30 => 0);
        si <= (20 => 6, others => 1);
        wait for 1 ns;
        assert so(1) = 3;
        assert so(20) = 6;

        wait;
    end process;

end architecture;
