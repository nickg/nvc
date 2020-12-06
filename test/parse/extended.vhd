-- Test extended identifiers
entity bar is
end entity;
architecture foo of bar is
    signal \foo bar\ : integer;
    signal \a\\b\ : integer;
    signal \Thing!!!  \ : integer;
    signal \name\ : integer;
    signal name : integer;
    signal \foo.bar.baz\ : integer;
    signal \hello\ : integer;
begin

    \foo.bar.baz\ <= \hello\;

end architecture;
