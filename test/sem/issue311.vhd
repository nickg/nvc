package TEST is
    type  EVENT_TYPE is (EVENT_1, EVENT_2);
end TEST;

use     WORK.TEST;
entity  TEST_NG is
end TEST_NG;
architecture MODEL of TEST_NG is
    constant init_event : TEST.EVENT_TYPE := TEST.EVENT_1;  -- OK
begin
end MODEL;

-------------------------------------------------------------------------------

package p1 is
    type  EVENT_TYPE is (EVENT_1, EVENT_2);
end package;

package p2 is
    type  EVENT_TYPE is (EVENT_1, EVENT_2);
end package;

use work.p1.all;
use work.p2.all;

entity e is
end entity;

architecture a of e is
    constant event1 : P1.EVENT_TYPE := P1.EVENT_1;  -- OK
    constant event2 : P2.EVENT_TYPE := P2.EVENT_1;  -- OK
    constant event3 : EVENT_TYPE := EVENT_1;        -- Error
    constant event4 : P1.EVENT_TYPE := P2.EVENT_1;  -- Error
begin

end architecture;
