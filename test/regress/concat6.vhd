entity concat6 is
end entity;

architecture test of concat6 is
    -- Examples from LRM 93 section 7.2.5

    type R1 is range 0 to 7;
    type R2 is range 7 downto 0;

    type T1 is array (R1 range <>) of Bit;
    type T2 is array (R2 range <>) of Bit;

    subtype S1 is T1(R1);
    subtype S2 is T2(R2);

    constant K1: S1 := (others => '0');
    constant K2: T1 := K1(1 to 3) & K1(3 to 4); -- K2'Left = 0 and K2'Right = 4
    constant K3: T1 := K1(5 to 7) & K1(1 to 2); -- K3'Left = 0 and K3'Right = 4
    constant K4: T1 := K1(2 to 1) & K1(1 to 2); -- K4'Left = 0 and K4'Right = 1
    constant K5: S2 := (others => '0');
    constant K6: T2 := K5(3 downto 1) & K5(4 downto 3); -- K6'Left = 7 and K6'Right = 3
    constant K7: T2 := K5(7 downto 5) & K5(2 downto 1); -- K7'Left = 7 and K7'Right = 3
    constant K8: T2 := K5(1 downto 2) & K5(2 downto 1); -- K8'Left = 7 and K8'Right = 6

    function get_left(x : T1) return R1 is
    begin
        return x'left;
    end function;

    function get_left(x : T2) return R2 is
    begin
        return x'left;
    end function;

    function get_right(x : T1) return R1 is
    begin
        return x'right;
    end function;

    function get_right(x : T2) return R2 is
    begin
        return x'right;
    end function;
begin

    main: process is
    begin
        assert K2'left = 0 report R1'image(K2'left);
        assert get_left(K2) = 0 report R1'image(K2'left);
        assert K2'right = 4 report R1'image(K2'right);
        assert get_right(K2) = 4 report R1'image(K2'right);
        assert K2'ascending;

        assert K3'left = 0 report R1'image(K3'left);
        assert get_left(K3) = 0 report R1'image(K3'left);
        assert K3'right = 4 report R1'image(K3'right);
        assert get_right(K3) = 4 report R1'image(K3'right);
        assert K3'ascending;

        assert K4'left = 0 report R1'image(K4'left);
        assert get_left(K4) = 0 report R1'image(K4'left);
        assert K4'right = 1 report R1'image(K4'right);
        assert get_right(K4) = 1 report R1'image(K4'right);
        assert K4'ascending;

        assert K6'left = 7 report R2'image(K6'left);
        assert get_left(K6) = 7 report R2'image(K6'left);
        assert K6'right = 3 report R2'image(K6'right);
        assert get_right(K6) = 3 report R2'image(K6'right);
        assert not K6'ascending;

        assert K7'left = 7 report R2'image(K7'left);
        assert get_left(K7) = 7 report R2'image(K7'left);
        assert K7'right = 3 report R2'image(K7'right);
        assert get_right(K7) = 3 report R2'image(K7'right);
        assert not K6'ascending;

        assert K8'left = 7 report R2'image(K8'left);
        assert get_left(K8) = 7 report R2'image(K8'left);
        assert K8'right = 6 report R2'image(K8'right);
        assert get_right(K8) = 6 report R2'image(K8'right);
        assert not K6'ascending;

        wait;
    end process;

end architecture;
