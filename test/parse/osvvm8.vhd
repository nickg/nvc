entity TbModelParameters is
end entity TbModelParameters ;

architecture Test of TbModelParameters is
    subtype  AlertLogIDType           is integer ;

    procedure AffirmIfEqual( AlertLogID : AlertLogIDType ; Received, Expected : boolean ;          Message : string := "" ; Enable : boolean := FALSE ) is
    begin
    end procedure ;
    procedure AffirmIfEqual( AlertLogID : AlertLogIDType ; Received, Expected : bit ;        Message : string := "" ; Enable : boolean := FALSE ) is
    begin
    end procedure ;
    procedure AffirmIfEqual( AlertLogID : AlertLogIDType ; Received, Expected : bit_vector ; Message : string := "" ; Enable : boolean := FALSE ) is
    begin
    end procedure ;
    procedure AffirmIfEqual( AlertLogID : AlertLogIDType ; Received, Expected : integer ;          Message : string := "" ; Enable : boolean := FALSE ) is
    begin
    end procedure;

    type ModelParametersPType is protected
        impure function Get(Index: natural) return integer;
        impure function Get(Index: natural) return boolean;
        impure function Get(Index: natural) return bit_vector;
        impure function Get(Index: natural; len: positive) return bit_vector;
    end protected ModelParametersPType;

    type ModelParametersPType is protected body
        impure function Get(Index: natural) return integer is
        begin
            return 0;
        end function;
        impure function Get(Index: natural) return boolean is
        begin
            return false;
        end function;
        impure function Get(Index: natural) return bit_vector is
        begin
            return "";
        end function;
        impure function Get(Index: natural; len: positive) return bit_vector is
        begin
            return "";
        end function;
    end protected body;

    shared variable Params : ModelParametersPType ;
    signal TbID : AlertLogIDType ;
begin
    TestProc : process
    begin
        AffirmIfEqual(TbID, Params.Get(5), 17, "Params.Get(5) = (17)") ;
        wait;
    end process;
end architecture Test ; -- of TbModelParameters
