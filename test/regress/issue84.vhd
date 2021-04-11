package sem_copy_default_args_bug is
  type protected_t is protected
    procedure procedure1(argument : inout boolean);
  end protected protected_t;
end package;

package body sem_copy_default_args_bug is
  type protected_t is protected body
    procedure procedure2(argument : inout boolean) is
    begin
        assert argument;
        argument := false;
    end procedure;
    procedure procedure1(argument : inout boolean) is
    begin
      procedure2(argument);
    end procedure;
  end protected body;
end package body;

entity issue84 is
end entity;

use work.sem_copy_default_args_bug.all;

architecture test of issue84 is
    shared variable v : protected_t;
begin

    process is
        variable x : boolean;
    begin
        x := true;
        v.procedure1(x);
        assert not x;
        wait;
    end process;

end architecture;
