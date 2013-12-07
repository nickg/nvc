entity alias5 is
end entity;

architecture test of alias5 is


    subtype bit_vector4 is bit_vector(3 downto 0);

    type footype is ( tr01, tr10, tr0z, trz1, tr1z, trz0,
                                  tr0x, trx1, tr1x, trx0, trxz, trzx);


    type bartype01   is array (footype range tr01 to tr10)
         of time;

    type yahtype01   is array (natural range <>) of bartype01;

    procedure bufpath (
            constant  tpd : in    yahtype01
    ) is
    begin
        report "tpd'left=" & integer'image(tpd'left);
        report "tpd'right=" & integer'image(tpd'right);
    end;

    procedure vitalmux4  (
            variable         data :  in bit_vector4;
            constant tpd_data_q :  in yahtype01
    ) is
        alias atpd_data_q : yahtype01(data'range) is tpd_data_q;
    begin
        bufpath ( atpd_data_q );
        assert atpd_data_q(data'left)(tr01) = 1 ns;
        assert atpd_data_q(3 downto 3)(3)(tr01) = 1 ns;
    end;
begin

    process is
        variable data : bit_vector4;
        variable yah  : yahtype01(5 downto 2);
    begin
        data := X"1";
        yah := (others => (others => 1 ns ) );
        vitalmux4(data, yah);
        wait;
    end process;

end architecture;
