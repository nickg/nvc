--
-- Support package for NVC-specific features
--

package sim is

    -- The following are provided for compatibility with VHDL-2008

    procedure stop(status : integer);
    procedure stop;

    procedure finish(status : integer);
    procedure finish;

    function resolution_limit return delay_length;

end package;

package body sim is

    procedure stop_impl(finish, have_status : boolean; status : integer);

    attribute foreign of stop_impl : procedure is "_nvc_sim_stop";

    procedure stop(status : integer) is
    begin
        stop_impl(finish => false, have_status => true, status => status);
    end procedure;

    procedure stop is
    begin
        stop_impl(finish => false, have_status => false, status => 0);
    end procedure;

    procedure finish(status : integer) is
    begin
        stop_impl(finish => true, have_status => true, status => status);
    end procedure;

    procedure finish is
    begin
        stop_impl(finish => true, have_status => false, status => 0);
    end procedure;

    function resolution_limit return delay_length is
    begin
        return fs;
    end function;

end package body;
