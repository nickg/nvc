-- Courtesy of Brian Padalino
--
library ieee ;
    use ieee.std_logic_1164.all ;

entity memory is
  generic (
    DEPTH   :       positive
  ) ;
  port (
    clock           :   in  std_logic ;
    write_addr      :   in  natural range 0 to DEPTH-1 ;
    write_data      :   in  std_logic_vector ;
    write_valid     :   in  std_logic ;
    read_addr       :   in  natural range 0 to DEPTH-1 ;
    read_data       :   out std_logic_vector ;
    read_valid      :   in  std_logic
  ) ;
end entity ;

architecture arch of memory is

    type mem_t is array(natural range 0 to DEPTH-1) of std_logic_vector(write_data'range) ;

    signal mem : mem_t := (others =>(others =>'0')) ;

begin

    process(all)
    begin
        if( rising_edge(clock) ) then
            if( write_valid = '1' ) then
                mem(write_addr) <= write_data ;
            end if ;
            if( read_valid = '1' ) then
                read_data <= mem(read_addr) ;
            end if ;
        end if ;
    end process ;

end architecture ;

library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.math_real.all ;

entity traffic_gen is
  generic (
    WIDTH   :       positive ;
    DEPTH   :       positive ;
    SEED    :       positive
  ) ;
  port (
    clock       :   in  std_logic ;
    write_addr  :   out natural range 0 to DEPTH-1 ;
    write_data  :   out std_logic_vector(WIDTH-1 downto 0) ;
    write_valid :   out std_logic ;
    read_addr   :   out natural range 0 to DEPTH-1 ;
    read_valid  :   out std_logic
  ) ;
end entity ;

architecture arch of traffic_gen is

    type rng_t is record
        s1 : positive ;
        s2 : positive ;
    end record ;

    procedure rand_std_logic(variable rng : inout rng_t ; signal y : out std_logic) is
        variable r : real ;
    begin
        uniform(rng.s1, rng.s2, r) ;
        y <= '1' when r > 0.5 else '0' ;
    end procedure ;

    procedure rand_slv(variable rng : inout rng_t ; signal y : out std_logic_vector) is
        variable r : real ;
    begin
        for i in y'range loop
            uniform(rng.s1, rng.s2, r) ;
            y(i) <= '1' when r > 0.5 else '0' ;
        end loop ;
    end procedure ;

    procedure rand_natural(variable rng : inout rng_t ; max : in positive ; signal y : out natural) is
        variable r : real ;
    begin
        uniform(rng.s1, rng.s2, r) ;
        y <= natural(real(max) * r) ;
    end procedure ;

    procedure randomize_write(variable rng : inout rng_t ; signal addr : out natural range 0 to DEPTH-1 ; signal data : out std_logic_vector(WIDTH-1 downto 0) ; signal valid : out std_logic ) is
    begin
        rand_natural(rng, DEPTH-1, addr) ;
        rand_slv(rng, data) ;
        rand_std_logic(rng, valid) ;
    end procedure ;

    procedure randomize_read(variable rng : inout rng_t ; signal addr : out natural range 0 to DEPTH-1 ; signal valid : out std_logic ) is
    begin
        rand_natural(rng, DEPTH-1, addr) ;
        rand_std_logic(rng, valid) ;
    end procedure ;

    function create_rng(s1, s2 : in positive) return rng_t is
        variable rv : rng_t ;
    begin
        rv.s1 := s1 ;
        rv.s2 := s2 ;
        return rv ;
    end function ;

begin

    process(all)
        variable write_rng : rng_t := create_rng(SEED+WIDTH+DEPTH, SEED*WIDTH*DEPTH) ;
        variable read_rng : rng_t := create_rng(SEED+WIDTH*DEPTH, SEED*WIDTH+DEPTH) ;
    begin
        if( rising_edge(clock) ) then
            randomize_write(write_rng, write_addr, write_data, write_valid) ;
            randomize_read(read_rng, read_addr, read_valid) ;
        end if ;
    end process ;

end architecture ;

library ieee ;
    use ieee.std_logic_1164.all ;

entity grind is
  generic (
    MEMORY_WIDTH    :   positive    := 256 ;
    MEMORY_DEPTH    :   positive    := 32768 ;
    NUM_INSTANCES   :   positive    := 128
  ) ;
end entity ;

architecture arch of grind is

    subtype data_t is std_logic_vector(MEMORY_WIDTH-1 downto 0) ;
    subtype addr_t is natural range 0 to MEMORY_DEPTH-1 ;

    type datas_t is array(natural range 0 to NUM_INSTANCES-1) of data_t ;
    type addrs_t is array(natural range 0 to NUM_INSTANCES-1) of addr_t ;

    signal clock    :   std_logic := '0' ;

    signal write_addrs : addrs_t ;
    signal write_datas : datas_t ;
    signal write_valids : std_logic_vector(0 to NUM_INSTANCES-1) ;

    signal read_addrs : addrs_t ;
    signal read_datas : datas_t ;
    signal read_valids : std_logic_vector(0 to NUM_INSTANCES-1) ;

begin

    clock <= not clock after 1 ns ;

    create_memories : for i in write_addrs'range generate
        U_traffic_gen : entity work.traffic_gen
          generic map (
            WIDTH       =>  data_t'length,
            DEPTH       =>  MEMORY_DEPTH,
            SEED        =>  i+1
          ) port map (
            clock       =>  clock,
            write_addr  =>  write_addrs(i),
            write_data  =>  write_datas(i),
            write_valid =>  write_valids(i),
            read_addr   =>  read_addrs(i),
            read_valid  =>  read_valids(i)
          ) ;

        U_mem : entity work.memory
          generic map (
            DEPTH   =>  MEMORY_DEPTH
          ) port map (
            clock       =>  clock,
            write_addr  =>  write_addrs(i),
            write_data  =>  write_datas(i),
            write_valid =>  write_valids(i),
            read_addr   =>  read_addrs(i),
            read_data   =>  read_datas(i),
            read_valid  =>  read_valids(i)
          ) ;
    end generate ;

    tb : process
        variable t : time := 35 us ;
    begin
        report "Starting testbench for " & time'image(t) ;
        wait for t ;
        report "Finished" ;
        std.env.stop ;
    end process ;

end architecture ;
