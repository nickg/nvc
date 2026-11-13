-- prev() of composite types
entity psl25 is
end entity;

architecture test of psl25 is
    type rec_t is record
        a : integer;
        b : bit;
    end record;

    type urec_t is record
        valid : bit;
        len   : bit_vector;                 -- Constrained by the signal
    end record;

    type arr2_t is array (1 to 2, 1 to 3) of integer;
    type rarr_t is array (1 to 2) of rec_t;
    type aarr_t is array (1 to 2) of bit_vector(1 to 3);

    signal clk : bit := '0';
    signal cyc : natural;

    signal r, r_q   : rec_t;
    signal u, u_q   : urec_t(len(7 downto 0));
    signal a2, a2_q : arr2_t;
    signal ra, ra_q : rarr_t;
    signal aa, aa_q : aarr_t;

    alias ua is u;
begin

    clk <= not clk after 5 ns when now < 200 ns;

    stim: process (clk) is
        function bits (n : natural; w : positive) return bit_vector is
            variable v : bit_vector(w - 1 downto 0);
        begin
            for i in v'range loop
                v(i) := bit'val((n / 2 ** i) mod 2);
            end loop;
            return v;
        end function;
    begin
        if clk'event and clk = '0' then
            cyc <= cyc + 1;
            r <= (cyc * 3, bit'val(cyc mod 2));
            u <= (bit'val(cyc mod 2), bits(cyc * 7, 8));
            a2 <= ((cyc, cyc + 1, cyc + 2), (-cyc, cyc * 2, 5));
            ra <= ((cyc, '1'), (cyc + 10, bit'val(cyc mod 2)));
            aa <= (bits(cyc, 3), bits(cyc + 3, 3));
        elsif clk'event and clk = '1' then
            -- Registered copies hold the value sampled at the previous edge
            r_q <= r;
            u_q <= u;
            a2_q <= a2;
            ra_q <= ra;
            aa_q <= aa;
        end if;
    end process;

    checks: block is
        default clock is clk'event and clk = '1';
    begin
        p1: assert always (cyc > 1 -> prev(r) = r_q);
        p2: assert always (cyc > 1 -> prev(u.len) = u_q.len);
        p3: assert always (cyc > 1 -> prev(ua.len) = u_q.len);
        p4: assert always (cyc > 1 -> prev(a2) = a2_q);
        p5: assert always (cyc > 1 -> prev(ra) = ra_q);
        p6: assert always (cyc > 1 -> prev(aa) = aa_q);
        p7: assert always (cyc > 1 -> prev(r.a) = r_q.a);
        p8: assert always (u.valid = '1' -> next (u.len /= prev(u.len)));
    end block;

end architecture;
