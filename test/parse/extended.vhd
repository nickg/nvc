-- Test extended identifiers
architecture foo of bar is
    signal \foo bar\ : integer;
    signal \a\\b\ : integer;
    signal \Thing!!!  \ : integer;
    signal \name\ : integer;
    signal name : integer;
begin

    \foo.bar.baz\ <= \hello\;

end architecture;
