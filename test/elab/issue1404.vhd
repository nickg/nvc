entity PCIE_CORE is
    generic (
        CQ_MFB_REGIONS     : natural := 2 );
end entity;

architecture USP of PCIE_CORE is

    constant AXI_DATA_WIDTH    : natural := CQ_MFB_REGIONS*256;

    component pcie4_uscale_plus is
        port (
            S_AXIS_RQ_TDATA  :  in   bit_vector(AXI_DATA_WIDTH-1    downto 0)       );
    end component;

    type slv_array_t is array (natural range <>) of bit_vector;

    signal pcie_rq_axi_data         : slv_array_t(0 downto 0)(AXI_DATA_WIDTH-1 downto 0);

    for all : pcie4_uscale_plus use open;
begin
    pcie_i : component pcie4_uscale_plus
        port map (
            s_axis_rq_tdata      => pcie_rq_axi_data(0)
            );

end architecture;

-------------------------------------------------------------------------------

entity FPGA_COMMON is
end entity;

architecture FULL of FPGA_COMMON is

    function pcie_mfb_regions_calc_f (PCIE_DIR : string) return natural is
        variable pcie_mfb_regions : natural;
    begin
        pcie_mfb_regions := 1;
        return pcie_mfb_regions;
    end function;

    constant DMA_CQ_MFB_REGIONS     : natural := pcie_mfb_regions_calc_f("CQ");

begin

    pcie_i : entity work.PCIE_CORE
    generic map (
        CQ_MFB_REGIONS      => DMA_CQ_MFB_REGIONS
    );

end architecture;
