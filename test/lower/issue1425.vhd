entity test is                
end entity ;      
                                                                                      
architecture arch of test is

    type utest_config_t is record       
        valid : boolean ;
        x : integer ;     
        y : integer ;      
        z : integer ;
    end record ;                 
                                                                                      
    type utest_configs_t is array(natural range <>) of utest_config_t ;
                                                                                      
    function resolved(constant x : in utest_configs_t) return utest_config_t is
        variable rv : utest_config_t := (false, 0, 0, 0) ;
    begin                        
        if x'length > 1 then 
            report "Too many drivers on test_config, invalid config" severity warning ;
            return rv ;    
        end if ;  
                                                                                      
        if x'length = 1 then
            for idx in x'range loop
                rv := x(idx) ;
            end loop ;
        end if ;                                     
        return rv ;           
    end function ;
                                                                                      
    subtype test_config_t is resolved utest_config_t ;

    signal test_config : test_config_t bus := (
        valid => false,
        x     => 0,
        y     => 0,                    
        z     => 0                                     
    ) ;                               
                                                                                      
    function sum(constant x : in integer_vector) return integer is
        variable rv : integer := 0 ;
    begin
        for idx in x'range loop
            rv := rv + x(idx) ;   
        end loop ;         
        return rv ;
    end function ;                                   
                                                                                      
    subtype test_count_t is sum integer ;                
    signal test_count : test_count_t := 0 ;
                                                                                      
    type current_test_t is (init, test1, test2) ;
    signal current_test : current_test_t := init ;
                                                                                      
    procedure print(x : string) is
        use std.textio.all ;
        variable l : line ;
    begin         
        write(l, x) ;
        writeline(output, l) ;                
    end procedure ;

begin

    t1 : block (current_test = test1) is
    begin
        -- Test 1 stimulus
        stimulus1 : process
        begin
            -- Disconnect stimulus
            test_config <= null ;
            wait until guard'event and guard = true ;
            print("Stimulus 1") ;
            test_config <= (true, 1, 1, 1) ;
            wait for 10 ns ;

            test_config <= null ;
            test_count <= 1 ;
            print("Done stimulus 1") ;
            wait for 10 ns ;
            wait ;
        end process ;

        -- Test 1 check
        check1 : process
        begin
            wait until guard'event and guard = true ;
            print("Check 1") ;
            wait ;
        end process ;
    end block ;

    t2 : block (current_test = test2) is
    begin
        -- Test 2 stimulus
        stimulus2 : process
        begin
            test_config <= null ;
            wait until guard'event and guard = true ;
            print("Stimulus 2") ;
            test_config <= (true, 2, 2, 2) ;
            wait for 1 ns ;

            test_config <= null ;
            test_count <= 1 ;
            print("Done stimulus 2") ;
            wait for 1 ns ;
            wait ;
        end process ;

        -- Test 2 check
        check2 : process
        begin
            wait until guard'event and guard = true ;
            print("Check 2") ;
            wait ;
        end process ;
    end block ;

    -- Run through the tests
    tb : process
    begin
        for test in current_test_t loop
            print("Running test: " & to_string(test)) ;
            -- Skip the initialization
            if test = init then
                next ;
            end if ;

            -- Run the test
            current_test <= test ;
            wait for 0 ns ;

            -- Wait until the test_count is completed
            wait until test_count = current_test_t'pos(test) ;
            print("  " & to_string(test) & " Complete") ;
            wait for 0 ns ;
        end loop ;

        print("Done!") ;
        wait for 10 ns ;
        std.env.stop ;
    end process ;

end architecture ;

