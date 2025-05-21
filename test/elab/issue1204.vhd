package interface is
    type t_rec is record
        x, y : integer;
    end record;

    view if_view of t_rec is
        x : in;
        y : out;
    end view;
end package;

-------------------------------------------------------------------------------

use work.interface.all;

entity module is
    port (
        module_port : view if_view;
        );
end entity module;

architecture test of module is
begin
end architecture;

-----------------------------------------------------------------------------

use work.interface.all;

entity top is
  port (
    top_port : view if_view             -- Unconnected at top level
  );
end entity top;

architecture logic of top is
begin
    module_inst : entity work.module
        port map (
            module_port => top_port
            );

end architecture logic;
