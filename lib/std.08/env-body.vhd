package body env is

    procedure stop_impl(finish, have_status : boolean; status : integer);

    attribute foreign of stop_impl : procedure is "_std_env_stop";

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
