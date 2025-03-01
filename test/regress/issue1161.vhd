entity issue1161 is
end entity;

architecture test of issue1161 is
    type ArrayOfSigned is array (natural range <>) of bit_vector;
    subtype ArrayOfSigned16 is ArrayOfSigned(open)(15 downto 0);
    type InterlacedSignal is record
        Data : ArrayOfSigned;
        Valid : bit;
    end record;
    type ArrayOfInterlacedSignals is array (natural range <>) of InterlacedSignal;
    subtype ArrayOfInterlaced16bSignals is ArrayOfInterlacedSignals(open)(Data(open)(15 downto 0));

    -- Should not generate a bounds var
    subtype Interlaced16bSignal is InterlacedSignal(Data(open)(15 downto 0));

    type p_rec is record
        x, y : integer;
    end record;

    constant p : p_rec := (x => 8, y => 16);

--    type ArrayOfInterlacedSignal is array (natural range <>) of InterlacedSignal(Data(open)(p.x - 1 downto 0));

    signal SomeArr : ArrayOfSigned16(0 to p.x - 1);
    signal Valid : bit;
    signal SomeRec : InterlacedSignal(Data(0 to 15)(7 downto 0));

begin

    b: block is
        generic ( IN_LANES, IN_WIDTH : integer );
        generic map ( IN_LANES => p.x, IN_WIDTH => 16 );
        port (
            DataIn : in InterlacedSignal(Data(0 to IN_LANES - 1)(IN_WIDTH - 1 downto 0));
            DataOut : out InterlacedSignal );
        port map (
            DataIn.Data => SomeArr,
            DataIn.Valid => Valid,
            DataOut => SomeRec );

        signal AIn : ArrayOfInterlaced16bSignals(0 to IN_WIDTH - 1)(Data(0 to IN_LANES - 1));
        signal s : datain.data'element;
    begin
    end block;

end architecture;
