package QueueP is

    generic (G_QUEUE_COUNT : positive := 1;
             G_QUEUE_DEPTH : positive := 4096 / 8 + 5;
             type G_DATA_TYPE;
             function G_TO_STRING(data : G_DATA_TYPE) return string is to_string);

    subtype t_queue_count is natural range 0 to G_QUEUE_COUNT - 1;

    subtype t_test is natural range G_QUEUE_COUNT to 100;

    type t_rec is record
        f : t_test;
    end record;

    procedure test (x : t_queue_count);
end package QueueP;

package body QueueP is

    procedure test (x : t_queue_count) is
    begin
    end procedure;

end package body;

-------------------------------------------------------------------------------

package pack is
    constant C_QUEUE_COUNT         : positive := 4;
    constant C_QUEUE_MAX_SIZE      : positive := 65535;

    package C2hQueue is new work.QueueP
        generic map (G_QUEUE_COUNT => C_QUEUE_COUNT,
                     G_QUEUE_DEPTH => C_QUEUE_MAX_SIZE,
                     G_DATA_TYPE   => bit_vector(7 downto 0),
                     G_TO_STRING   => to_string);
end package;

-------------------------------------------------------------------------------

entity issue894 is
end entity;

use work.pack.all;

architecture test of issue894 is
begin

    tb: process is
        type t_array is array (C2hQueue.t_test) of C2hQueue.t_test;

        variable n : natural := 2;
        variable v : C2hQueue.t_queue_count;
        variable t : C2hQueue.t_test;
        variable a : t_array;
        variable r : C2hQueue.t_rec;
    begin
        C2hQueue.test(n);
        v := 2;
        t := 6;
        a(t) := 5;
        wait for 1 ns;
        assert a(6) = 5;
        -- TODO
        --assert a(6 to 7) = (5, C2hQueue.t_test'left);
        assert r.f = 4;
        wait;
    end process;

end architecture;
