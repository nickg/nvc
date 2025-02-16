entity issue1161 is
end entity;

architecture test of issue1161 is
    type ArrayOfSigned is array (natural range <>) of bit_vector;
   subtype ArrayOfSigned16 is ArrayOfSigned(open)(15 downto 0);
    type InterlacedSignal is record
        Data : ArrayOfSigned;
        Valid : bit;
    end record;

    -- Should not generate a bounds var
    subtype Interlaced16bSignal is InterlacedSignal(Data(open)(15 downto 0));

    type p_rec is record
        x, y : integer;
    end record;

    constant p : p_rec := (x => 8, y => 16);

    signal SomeArr : ArrayOfSigned16(0 to p.x - 1);
    signal Valid : bit;

begin

    b: block is
        generic ( IN_LANES, IN_WIDTH : integer );
        generic map ( IN_LANES => p.x, IN_WIDTH => 16 );
        port (
            DataIn : in InterlacedSignal(Data(0 to IN_LANES - 1)(IN_WIDTH - 1 downto 0)) );
        port map (
            DataIn.Data => SomeArr,
            DataIn.Valid => Valid );
    begin
    end block;

end architecture;
