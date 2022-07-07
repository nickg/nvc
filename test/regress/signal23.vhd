-- Test case from Brian Padalino
--

library ieee ;
    use ieee.std_logic_1164.all ;

package pack is
    type iface_t is record
        cs  :   std_logic ;
        addr    :   std_logic_vector ;
        rdata   :   std_logic_vector ;
        wdata   :   std_logic_vector ;
    end record ;

    function init_iface_signals(addr_width : natural ; data_width : natural) return iface_t;

end package ;

package body pack is
    function init_iface_signals(addr_width : natural ; data_width : natural) return iface_t is
        variable rv : iface_t(addr(addr_width-1 downto 0), wdata(data_width-1 downto 0), rdata(data_width-1 downto 0)) ;
    begin
        rv.cs := '0' ;
        rv.addr := (rv.addr'range => '0') ;
        rv.wdata := (rv.wdata'range => '0') ;
        rv.rdata := (rv.rdata'range => '0') ;
        return rv ;
    end function ;

end package body ;

library ieee ;
    use ieee.std_logic_1164.all ;

    use work.pack.all ;

entity master is
  port (
    clock   :   in  std_logic ;
    iface   :   inout   iface_t := init_iface_signals(16, 32)
  ) ;
end entity ;

architecture arch of master is

begin

end architecture ;

library ieee ;
    use ieee.std_logic_1164.all ;
    use work.pack.all ;

entity signal23 is end entity ;

architecture arch of signal23 is

    signal clock : std_logic := '0' ;
    signal iface : iface_t(addr(15 downto 0), rdata(31 downto 0), wdata(31 downto 0)) ;

begin

    clock <= not clock after 1 ns ;

    U_master : entity work.master
      port map (
        clock => clock,
        iface => iface
      ) ;

    tb : process
    begin
        report LF &
            "if:" & LF &
            "  cs:    " & std_logic'image(iface.cs) & LF &
            "  addr:  " & to_hstring(iface.addr) & LF &
            "  rdata: " & to_hstring(iface.rdata) & LF &
            "  wdata: " & to_hstring(iface.wdata) ;
        std.env.stop ;
    end process ;

end architecture ;
