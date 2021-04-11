PACKAGE ecc_top_pkg is
    constant engnum : integer:=8;
    subtype engrange is integer range 0 to engnum-1;
    type bit_array is array(engrange) of bit;
    type byte_array is array(engrange) of bit_vector(7 downto 0);
END PACKAGE;

entity issue407 is
end entity;

use work.ecc_top_pkg.all;

architecture test of issue407 is
    type mode_t is (RDBMG, C2ENC, C2DEC, C1DEC, WRBMG);
    type mode_arr_t is array (natural range <>) of mode_t;
    constant ECC_RD_DATA_VAL : bit := '1';
    signal buf_ce : bit_vector(engrange);
    signal bufmode : mode_arr_t(0 to 0);
    signal index : natural := 0;
    signal c2enc_ce, c2dec_ce, c1dec_ce : bit_vector(engrange);
    signal task4_ce : bit := '1';
begin

    ce: with bufmode(index) select buf_ce <=
        (engrange => ECC_RD_DATA_VAL) WHEN RDBMG, -- Line 412
        c2enc_ce when C2ENC,
        c2dec_ce WHEN C2DEC,
        c1dec_ce WHEN C1DEC,
        (engrange => task4_ce) WHEN WRBMG, -- Line 416
        (engrange => '0') WHEN others; -- Line 417

    stim: process is
    begin
        bufmode(0) <= C2DEC;
        wait for 1 ns;
        assert buf_ce = X"00";
        bufmode(0) <= RDBMG;
        wait for 1 ns;
        assert buf_ce = X"FF";
        bufmode(0) <= WRBMG;
        task4_ce <= '0';
        wait for 1 ns;
        assert buf_ce = X"00";
        wait;
    end process;

end architecture;
