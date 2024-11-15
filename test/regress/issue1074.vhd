package test_pkg is

    type test_record is record
        sig1 : bit;
        sig2 : bit;
    end record;

    view source of test_record is
        sig1 : out;
        sig2 : out;
    end view;

    alias sink is source'converse;

end package;
use work.test_pkg.all;

entity test_source is
    port (
        src : view source
    );
end entity;

architecture rtl of test_source is
begin

    src <= ('1', '0') after 1 ns,
           ('0', '1') after 2 ns,
           ('1', '1') after 3 ns;

end architecture;


use work.test_pkg.all;

entity test_sink is
    port (
        snk : view sink
    );
end entity;

architecture rtl of test_sink is

begin

    process is
    begin
        wait for 1 ns;
        assert snk = ('1', '0');
        wait for 1 ns;
        assert snk = ('0', '1');
        wait for 1 ns;
        assert snk = ('1', '1');
        wait;
    end process;

end architecture;

use work.test_pkg.all;
use work.test_source;
use work.test_sink;

entity issue1074 is
end entity;

architecture rtl of issue1074 is
    signal s1 : bit;
    signal s2 : bit;
begin
    test_source_inst: entity work.test_source
        port map(
            src.sig1 => s1,
            src.sig2 => s2
        );

    test_sink_inst: entity work.test_sink
        port map(
            snk.sig1 => s1,
            snk.sig2 => s2
        );

end architecture;
