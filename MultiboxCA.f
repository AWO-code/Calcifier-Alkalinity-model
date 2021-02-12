         program boxAW
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ocean-atmosphere biogeochemistry box model
c
c five boxes: atmosphere
c             ocean: high lat surface
c             ocean: low lat surface
c             ocean: deep
c             ocean: shelf: CAN ADAPT TO SUBTROP THERMOCLINE
c
c 
c CARRYING OCEAN PO4
c
c
c Dec 6 2001
c ADD CARBON....
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mick Follows, Dec 2001
c ADAPTED FROM 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
c         ------------------------------------------------
c         |                                               |
c         |                                               |
c         |                                               |
c         |                    ATMOSPHERE                 |
c         |                                               |
c         |                                               |
c         |                                               |
c         |-----------------------------------------------|
c         ||||||          |               |               |
c         ||||||          |               |               |
c         ||||||  SHELF   |   LOW LAT     |   HIGH LAT    |
c         ||||||          |   SURFACE     |   SURFACE     |
c         ||||||          |               |               |
c         ||||||          |               |               |
c         ||||||          --------------------------------|
c         ||||||          |                               |
c         ||||||          |                               |
c         ||||||          |                               |
c         ||||||          |                               |
c         ||||||          |                               |
c         ||||||----------|         DEEP OCEAN            |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         |||||||||||||||||                               |
c         ------------------------------------------------
c
c
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        implicit none

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c declare variables
C QUITE A FEW REDUNDANT HERE AT PRESENT....
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       INTEGER nstep
       INTEGER nstepmax
       REAL*8 dt
       REAL*8 time
       REAL*8 dx
       REAL*8 dy_h
       REAL*8 dy_l
       REAL*8 dy_d
       REAL*8 dy_s
       REAL*8 dz_l
       REAL*8 dz_h
       REAL*8 dz_d
       REAL*8 dz_s
       REAL*8 tot_atmos_n2
       REAL*8 tot_atmos_o2
       REAL*8 tot_atmos_co2
       REAL*8 atmos_po2
       REAL*8 atmos_pco2
       REAL*8 col_mass_n2
       REAL*8 pc_l
       REAL*8 pc_h
       REAL*8 pc_d
       REAL*8 pc_s
       REAL*8 pn_l
       REAL*8 pn_h
       REAL*8 pn_d
       REAL*8 pn_s
       REAL*8 dc_l
       REAL*8 dc_h
       REAL*8 dc_d
       REAL*8 dc_s
       REAL*8 dn_l
       REAL*8 dn_h
       REAL*8 dn_d
       REAL*8 dn_s
       REAL*8 da_l
       REAL*8 da_h
       REAL*8 da_d
       REAL*8 da_s
       REAL*8 po4_l
       REAL*8 po4_h
       REAL*8 po4_d
       REAL*8 po4_s
       REAL*8 dic_l
       REAL*8 dic_h
       REAL*8 dic_d
       REAL*8 dic_s
       REAL*8 alk_l
       REAL*8 alk_h
       REAL*8 alk_d
       REAL*8 alk_s
       REAL*8 m_hd
       REAL*8 m_ld
       REAL*8 m_lh
       REAL*8 m_ds
       REAL*8 q
       REAL*8 rho_ref
       REAL*8 z_remin
       REAL*8 RA
       REAL*8 RC
       REAL*8 Vp_h
       REAL*8 Vp_l
       REAL*8 Vp_s
       REAL*8 vol_h
       REAL*8 vol_l
       REAL*8 vol_d
       REAL*8 vol_s
       REAL*8 dpcdt_h
       REAL*8 dpcdt_l
       REAL*8 dpcdt_d
       REAL*8 dpcdt_s
       REAL*8 dpndt_h
       REAL*8 dpndt_l
       REAL*8 dpndt_d
       REAL*8 dpndt_s
       REAL*8 ddcdt_h
       REAL*8 ddcdt_l
       REAL*8 ddcdt_d
       REAL*8 ddcdt_s
       REAL*8 ddndt_h
       REAL*8 ddndt_l
       REAL*8 ddndt_d
       REAL*8 ddndt_s
       REAL*8 ddadt_h
       REAL*8 ddadt_l
       REAL*8 ddadt_d
       REAL*8 ddadt_s
       REAL*8 dpo4dt_h
       REAL*8 dpo4dt_l
       REAL*8 dpo4dt_d
       REAL*8 dpo4dt_s
       REAL*8 dalkdt_h
       REAL*8 dalkdt_l
       REAL*8 dalkdt_d
       REAL*8 dalkdt_s
       REAL*8 ddicdt_h
       REAL*8 ddicdt_l
       REAL*8 ddicdt_d
       REAL*8 ddicdt_s
       REAL*8 TotalP
       REAL*8 TotalN

c T,S   
       REAL*8 t_l
       REAL*8 t_h
       REAL*8 t_d
       REAL*8 t_s
       REAL*8 s_l
       REAL*8 s_h
       REAL*8 s_d
       REAL*8 s_s
c atmosphere
       REAL*8 atmos_moles_N2
       REAL*8 atmos_moles_O2
c limiting nutrient
       REAL*8 mumpc
       REAL*8 mumpn
       REAL*8 kpo4pc
       REAL*8 kpo4pn
       REAL*8 kca
c N2 fixation/nitrification/denitrification
       REAL*8 hc
       REAL*8 hn
       REAL*8 rem
       REAL*8 remA
       REAL*8 lambda_dc
       REAL*8 lambda_dn
       REAL*8 lambda_da
c mixed layer regeneration and sinking of biomass
       REAL*8 Ipo_h
       REAL*8 Ipo_l
       REAL*8 Ipo_s
       REAL*8 Ia_h
       REAL*8 Ia_l
       REAL*8 Ia_s
       REAL*8 Ic_h
       REAL*8 Ic_l
       REAL*8 Ic_s
       REAL*8 omega
       REAL*8 amp
       REAL*8 mmpc
c dummy variable
       REAL*8 alln
       REAL*8 testratio
c variable controlling anoxic fraction
       REAL*8 p_frac_buried 
       REAL*8 a_frac_buried 
       REAL*8 afb0
       REAL*8 zstar
       REAL*8  write_years
       REAL*8 p_frac_buried_shelf
       REAL*8 a_frac_buried_shelf
       REAL*8 np_burial
       REAL*8 m_ls
       REAL*8 k_denit_o2
       REAL*8 kcomp
       REAL*8 ca_sat

C SOME PARAMETERS NOT READ IN, BUT SET BELOW..............
       INTEGER npar
       INTEGER nvalues
       PARAMETER(npar=50)
       PARAMETER(nvalues=42)
       INTEGER idummy
       INTEGER iout
       INTEGER iout2
       INTEGER iend
       INTEGER nwrite
       REAL*8 parm(npar)
       REAL*8 x(nvalues)
       REAL*8 y(nvalues)

       REAL*8 dthold
       REAL*8 tstepfactor
       REAL*8 lambda_high
       REAL*8 lambda_low
       REAL*8 lambda_shelf
       REAL*8 export_s

CMICK.... variables for adams-bashforth timestepping
CMICK - NOW REDUNDANT, A-B SCHEME NOT USED
       REAL*8 epsilon
       REAL*8 dpcdt_h_o
       REAL*8 dpcdt_l_o
       REAL*8 dpcdt_d_o
       REAL*8 dpcdt_s_o
       REAL*8 dpndt_h_o
       REAL*8 dpndt_l_o
       REAL*8 dpndt_d_o
       REAL*8 dpndt_s_o
       REAL*8 ddcdt_h_o
       REAL*8 ddcdt_l_o
       REAL*8 ddcdt_d_o
       REAL*8 ddcdt_s_o
       REAL*8 ddndt_h_o
       REAL*8 ddndt_l_o
       REAL*8 ddndt_d_o
       REAL*8 ddndt_s_o
       REAL*8 ddadt_h_o
       REAL*8 ddadt_l_o
       REAL*8 ddadt_d_o
       REAL*8 ddadt_s_o
       REAL*8 dpo4dt_h_o
       REAL*8 dpo4dt_l_o
       REAL*8 dpo4dt_d_o
       REAL*8 dpo4dt_s_o

       REAL*8 ab0
       REAL*8 ab1
       REAL*8 DICtot
       REAL*8 ALKtot
       INTEGER i_timestep

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
       REAL*8 dalkdt_h_o
       REAL*8 dalkdt_l_o
       REAL*8 dalkdt_d_o
       REAL*8 dalkdt_s_o
       REAL*8 ddicdt_h_o
       REAL*8 ddicdt_l_o
       REAL*8 ddicdt_d_o
       REAL*8 ddicdt_s_o
       REAL*8 airseafluxCO2
       REAL*8 co2flux     
       REAL*8 pf_l     
       REAL*8 pf_s     
       REAL*8 pf_h     
       REAL*8 atmos_moles_CO2
       REAL*8 ca_l     
       REAL*8 ca_h     
       REAL*8 ca_s     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C...........................................


C SOME WORKING VARIABLES NOT PASSED IN FROM INPUT FILE
       REAL*8 offset

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c read in file of default/initial values
        open(10,file='inparam.dat',status='old')
        do idummy = 1, npar  
          read(10,*)parm(idummy)
        end do

        open(11,file='invalues.dat',status='old')
        do idummy = 1, nvalues  
          read(11,*)x(idummy)
        end do

c how often to write out?
c       write_years = 500. 

c open output file
        iout=18
        open(iout,file='time_boxAW.dat',status='new')
        iend=19
        open(iend,file='output_boxAW.dat',status='new')
        iout2=20
        open(iout2,file='time_boxAW2.dat',status='new')

c initialise some timestepping variables
        dpcdt_h_o = 0.0
        dpcdt_l_o = 0.0
        dpcdt_d_o = 0.0
        dpcdt_s_o = 0.0
        dpndt_h_o = 0.0
        dpndt_l_o = 0.0
        dpndt_d_o = 0.0
        dpndt_s_o = 0.0
        ddcdt_h_o = 0.0
        ddcdt_l_o = 0.0
        ddcdt_d_o = 0.0
        ddcdt_s_o = 0.0
        ddndt_h_o = 0.0
        ddndt_l_o = 0.0
        ddndt_d_o = 0.0
        ddndt_s_o = 0.0
        ddadt_h_o = 0.0
        ddadt_l_o = 0.0
        ddadt_d_o = 0.0
        ddadt_s_o = 0.0
        dpo4dt_h_o = 0.0
        dpo4dt_l_o = 0.0
        dpo4dt_d_o = 0.0
        dpo4dt_s_o = 0.0
        epsilon = 0.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C Carbon
        dalkdt_h_o = 0.0
        dalkdt_l_o = 0.0
        dalkdt_d_o = 0.0
        dalkdt_s_o = 0.0
        ddicdt_h_o = 0.0
        ddicdt_l_o = 0.0
        ddicdt_d_o = 0.0
        ddicdt_s_o = 0.0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc

c assign default/initial values
c
c ***************************************************************
c NOTE:
c INITIAL VALUES AND PARAMETERS READ IN FROM FILE input_values.dat
c VARIABLE KEY AND SUGGESTED DEFAULT VALUES IN inputs.list
c AND  inputs.assign 
c ***************************************************************
c
c all real except first, so convert to integer
         nstepmax = int(parm(1))
         dt = parm(2)
         dx = parm(3)       
         dy_h = parm(4)
         dy_l = parm(5)
         dy_s = parm(6)
         dz_l = parm(7)
         dz_h = parm(8)
         dz_d = parm(9)
         atmos_po2 = parm(10)
         Vp_h = parm(11)
         Vp_l = parm(12)
         Vp_s = parm(13)    
         mumpc = parm(14)
         mumpn = parm(15)
         kpo4pc = parm(16)
         kpo4pn = parm(17)
         kca = parm(18)
         m_hd = parm(19)
         m_ld = parm(20)
         m_lh = parm(21)
         m_ds = parm(22)
         rem = parm(23)
         remA = parm(24)
         lambda_dc = parm(25)
         lambda_dn = parm(26)
         lambda_da = parm(27)
         Ipo_h = parm(28)
         Ipo_l = parm(29)
         Ipo_s = parm(30)
         Ia_h = parm(31)
         Ia_l = parm(32)
         Ia_s = parm(33)
         Ic_h = parm(34)
         Ic_l = parm(35)
         Ic_s = parm(36)
         write_years = parm(37)
         p_frac_buried_shelf = parm(38)
         np_burial = parm(39)
         m_ls = parm(40)
         k_denit_o2=parm(41)
         dz_s = parm(42)
         RC = parm(43)
         RA = parm(44)
         hc = parm(45)  
         hn = parm(46)
         q = parm(47)
         kcomp=parm(48)
         omega=parm(49)
         amp=parm(50)

         atmos_pco2 = x(1)
         po4_l = x(2)
         po4_h = x(3)
         po4_d = x(4)
         po4_s = x(5)
         dc_l = x(6)
         dc_h = x(7)
         dc_s = x(8)
         dn_l = x(9)
         dn_h = x(10)
         dn_s = x(11)
         da_l = x(12)
         da_h = x(13)
         da_s = x(14)
         pc_l = x(15)
         pc_h = x(16)
         pc_s = x(17)
         pn_l = x(18)
         pn_h = x(19)
         pn_s = x(20)
         dic_l = x(21)
         dic_h = x(22)
         dic_d = x(23)
         dic_s = x(24)
         alk_l = x(25)
         alk_h = x(26)
         alk_d = x(27)
         alk_s = x(28)
         t_h = x(29)
         t_l = x(30)
         t_d = x(31)
         t_s = x(32)
         s_h = x(33)
         s_l = x(34)
         s_d = x(35)
         s_s = x(36)
         pc_d = x(37)
         pn_d = x(38)
         dc_d = x(39)
         dn_d = x(40)
         da_d = x(41)
         time = x(42)
c ==========================================================

c MORE INTERESTING  STUFF STARTS HERE xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
c MORE INTERESTING  STUFF STARTS HERE xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

C SET SOME PARAMETERS HERE

c reference denisty (kg m-3)
        rho_ref = 1025.0


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C Carbon
C put in initial carbon concentrations here
C instead of reading in
c        dic_h = 2000.0 * rho_ref
c        dic_l = 2000.0 * rho_ref
c        dic_d = 2000.0 * rho_ref
c        dic_s = 2000.0 * rho_ref
c        atmos_pco2 = 280.0e-6
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc

c how often are we writing out model state? 
c convert to number of steps
        nwrite=(write_years*360.*86400.)/dt

c moles N2 in atmos - prescribed
c total moles air (modern) is about 1.8e20
        atmos_moles_N2 = 1.8e20*0.79
c inital atmospheric O2 (can be prognostic though) 
        atmos_moles_O2 = 1.8e20*0.21
        atmos_pO2 = atmos_moles_O2/(atmos_moles_O2+atmos_moles_N2)
        atmos_moles_CO2 = 
     &      (atmos_moles_N2+atmos_moles_O2)*atmos_pco2

c initial time
c        time = 0.0

c derived dimension parameters
c determine volumes of ocean reservoirs from imposed length scales
        dy_d = dy_h+dy_l
        vol_h = dx*dy_h*dz_h
        vol_l = dx*dy_l*dz_l
        vol_d = dx*dy_d*dz_d
        vol_s = dx*dy_s*dz_s

C FOR NUMERICAL STABILITY (may not be necessary) 
C TRY SMALL TIMESTEP FOR INITIAL ADJUSTMENT
C ADJUST BY tstepfactor
C if tstepfactor=1 then no actual change
        tstepfactor = 1.
        dthold = dt 
        dt = dthold/tstepfactor


c +++++++++++++++++++++++++++++++++++==+++++++++++++++++++++++
c ++++++++ begin timestepping ++++++++++++++++++++++++++++++++
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        do nstep = 1, nstepmax

C IF USING SMALL INITIAL TIMESTEP,  INCREASE AFTER 10000 ITERATIONS
            if(nstep .eq. 100000)then
               dt = dthold
            end if

c initialize rates of change
            dpo4dt_h = 0.
            dpo4dt_l = 0.
            dpo4dt_d = 0.
            dpo4dt_s = 0.
            ddicdt_h = 0.
            ddicdt_l = 0.
            ddicdt_d = 0.
            ddicdt_s = 0.
            dalkdt_h = 0.
            dalkdt_l = 0.
            dalkdt_d = 0.
            dalkdt_s = 0.
            ddcdt_h = 0.
            ddcdt_l = 0.
            ddcdt_s = 0.
            ddndt_h = 0.
            ddndt_l = 0.
            ddndt_s = 0.
            ddadt_h = 0.
            ddadt_l = 0.
            ddadt_s = 0.
            dpcdt_h = 0.
            dpcdt_l = 0.
            dpcdt_s = 0.
            dpndt_h = 0.
            dpndt_l = 0.
            dpndt_s = 0.
cccccccccccccccccccccccccccccccccccccccc
C Carbon
            ddicdt_h = 0.
            ddicdt_l = 0.
            ddicdt_d = 0.
            ddicdt_s = 0.
cccccccccccccccccccccccccccccccccccccccc
            dpcdt_d = 0.
            dpndt_d = 0.
            ddcdt_d = 0.
            ddndt_d = 0.
            ddadt_d = 0.

c OCEAN TRANSPORT ..........................................
c  CALL ocean_transport SUBROUTINE FOR EACH OCEAN TRACER
c transport po4
            call ocean_transport(po4_h, po4_l, po4_d, po4_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           dpo4dt_h, dpo4dt_l, dpo4dt_d, 
     &                           dpo4dt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C alkalinity
c transport alk
            call ocean_transport(alk_h, alk_l, alk_d, alk_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           dalkdt_h, dalkdt_l, dalkdt_d, 
     &                           dalkdt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C carbon
c transport dic
            call ocean_transport(dic_h, dic_l, dic_d, dic_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           ddicdt_h, ddicdt_l, ddicdt_d, 
     &                           ddicdt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C pc
c transport pc
            call ocean_transport(pc_h, pc_l, pc_d, pc_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           dpcdt_h, dpcdt_l, dpcdt_d, 
     &                           dpcdt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C pn
c transport pn
            call ocean_transport(pn_h, pn_l, pn_d, pn_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           dpndt_h, dpndt_l, dpndt_d, 
     &                           dpndt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C dc
c transport dc
            call ocean_transport(dc_h, dc_l, dc_d, dc_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           ddcdt_h, ddcdt_l, ddcdt_d, 
     &                           ddcdt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C dn
c transport dn
            call ocean_transport(dn_h, dn_l, dn_d, dn_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           ddndt_h, ddndt_l, ddndt_d, 
     &                           ddndt_s)
ccccccccccccccccccccccccccccccccccccccccccccc
C da
c transport da
            call ocean_transport(da_h, da_l, da_d, da_s,
     &                           q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                           vol_h, vol_l, vol_d, vol_s,
     &                           ddadt_h, ddadt_l, ddadt_d, 
     &                           ddadt_s)
ccccccccccccccccccccccccccccccccccccccccccccc

c ======== AIR-SEA EXCHANGE =====================================    
c cumulate global air-sea oxygen flux for prognostic atmosphere

C Carbon
C AIR-SEA EXCHANGE
            airseafluxCO2 = 0.0 

            call carbon_air_sea(dic_h, atmos_pco2, ddicdt_h, alk_h,
     &                      Vp_h, dz_h, t_h, s_h, rho_ref, dt, nstep,
     &                      co2flux, pf_h, ca_h) 
            airseafluxCO2 = airseafluxCO2 + co2flux*dx*dy_h

            call carbon_air_sea(dic_l, atmos_pco2, ddicdt_l, alk_l,
     &                      Vp_l, dz_l, t_l, s_l, rho_ref, dt, nstep,
     &                      co2flux, pf_l, ca_l) 
            airseafluxCO2 = airseafluxCO2 + co2flux*dx*dy_l

            call carbon_air_sea(dic_s, atmos_pco2, ddicdt_s, alk_s,
     &                      Vp_s, dz_s, t_s, s_s, rho_ref, dt, nstep,
     &                      co2flux, pf_s, ca_s) 
            airseafluxCO2 = airseafluxCO2 + co2flux*dx*dy_s
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
c        write(6,*) atmos_pco2, ca_l

            mmpc=mumpc*(1+amp*dcos(omega*time))
c ===== DETERMINE PC GROWTH ===============================
c high lat pc growth in phosphorus units
            dpcdt_h = dpcdt_h + mmpc/(1+kpo4pc/po4_h+kca/ca_h
     &      -1/(po4_h/kpo4pc+ca_h/kca))*pc_h-hc*pc_h 

c low lat pc growth in phosphorus units
            dpcdt_l = dpcdt_l + mmpc/(1+kpo4pc/po4_l+kca/ca_l
     &      -1/(po4_l/kpo4pc+ca_l/kca))*pc_l-hc*pc_l 

c deep pc decay in phosphorus units
            dpcdt_d = dpcdt_d-hc*pc_d 

c shelf pc growth in phosphorus units
            dpcdt_s = dpcdt_s + mmpc/(1+kpo4pc/po4_s+kca/ca_s
     &      -1/(po4_s/kpo4pc+ca_s/kca))*pc_s-hc*pc_s 

c high lat pn growth in phosphorus units
            dpndt_h = dpndt_h + mumpn/(1+kpo4pn/po4_h)*pn_h-hn*pn_h 

c low lat pn growth in phosphorus units
            dpndt_l = dpndt_l + mumpn/(1+kpo4pn/po4_l)*pn_l-hn*pn_l 

c deep pn decay in phosphorus units
            dpndt_d = dpndt_d-hn*pn_d 

c shelf pn growth in phosphorus units
            dpndt_s = dpndt_s + mumpn/(1+kpo4pn/po4_s)*pn_s-hn*pn_s

c high lat dc in phosphorus units
            ddcdt_h = ddcdt_h + hc*pc_h-rem*dc_h-lambda_dc*dc_h 

c low lat dc in phosphorus units
            ddcdt_l = ddcdt_l + hc*pc_l-rem*dc_l-lambda_dc*dc_l 

c deep dc in phosphorus units
            ddcdt_d = ddcdt_d + hc*pc_d-rem*dc_d 

c shelf dc in phosphorus units
            ddcdt_s = ddcdt_s + hc*pc_s-rem*dc_s-lambda_dc*dc_s 

c high lat dn in phosphorus units
            ddndt_h = ddndt_h + hn*pn_h-rem*dn_h-lambda_dn*dn_h 

c low lat dn in phosphorus units
            ddndt_l = ddndt_l + hn*pn_l-rem*dn_l-lambda_dn*dn_l 

c deep dn in phosphorus units
            ddndt_d = ddndt_d + hn*pn_d-rem*dn_d 

c shelf dn in phosphorus units
            ddndt_s = ddndt_s + hn*pn_s-rem*dn_s-lambda_dn*dn_s 

c high lat da in alk units
            ddadt_h = ddadt_h + RA*hc*pc_h-remA*da_h-lambda_da*da_h 

c low lat da in alk units
            ddadt_l = ddadt_l + RA*hc*pc_l-remA*da_l-lambda_da*da_l 

c deep da in alk units
            ddadt_d = ddadt_d + RA*hc*pc_d-remA*da_d 

c shelf da in alk units
            ddadt_s = ddadt_s + RA*hc*pc_s-remA*da_s-lambda_da*da_s 

c update po4 surface tendencies
           dpo4dt_h = dpo4dt_h + Ipo_h-mmpc/
     &     (1+kpo4pc/po4_h+kca/ca_h-1/(po4_h/kpo4pc+ca_h/kca))*pc_h 
     &     -mumpn/(1+kpo4pn/po4_h)*pn_h+rem*(dc_h+dn_h) 
           dpo4dt_l = dpo4dt_l + Ipo_l-mmpc/
     &     (1+kpo4pc/po4_l+kca/ca_l-1/(po4_l/kpo4pc+ca_l/kca))*pc_l 
     &     -mumpn/(1+kpo4pn/po4_l)*pn_l+rem*(dc_l+dn_l) 
           dpo4dt_s = dpo4dt_s + Ipo_s-mmpc/
     &     (1+kpo4pc/po4_s+kca/ca_s-1/(po4_s/kpo4pc+ca_s/kca))*pc_s 
     &     -mumpn/(1+kpo4pn/po4_s)*pn_s+rem*(dc_s+dn_s) 
           dalkdt_h = dalkdt_h + Ia_h-RA*mmpc/
     &     (1+kpo4pc/po4_h+kca/ca_h-1/(po4_h/kpo4pc+ca_h/kca))*pc_h 
     &      +remA*da_h
           dalkdt_l = dalkdt_l + Ia_l-RA*mmpc/
     &     (1+kpo4pc/po4_l+kca/ca_l-1/(po4_l/kpo4pc+ca_l/kca))*pc_l 
     &      +remA*da_l
           dalkdt_s = dalkdt_s + Ia_s-RA*mmpc/
     &     (1+kpo4pc/po4_s+kca/ca_s-1/(po4_s/kpo4pc+ca_s/kca))*pc_s 
     &      +remA*da_s


ccccccccccccccccccccccccccccccccccccccccccccc
C carbon
c update dic surface tendencies
           ddicdt_h = ddicdt_h + Ic_h-(RC+0.5*RA)*mmpc/
     &     (1+kpo4pc/po4_h+kca/ca_h-1/(po4_h/kpo4pc+ca_h/kca))*pc_h 
     &     -RC*(mumpn/(1+kpo4pn/po4_h)*pn_h-rem*(dc_h+dn_h))
     &      +0.5*remA*da_h
           ddicdt_l = ddicdt_l + Ic_l-(RC+0.5*RA)*mmpc/
     &     (1+kpo4pc/po4_l+kca/ca_l-1/(po4_l/kpo4pc+ca_l/kca))*pc_l 
     &     -RC*(mumpn/(1+kpo4pn/po4_l)*pn_l-rem*(dc_l+dn_l)) 
     &      +0.5*remA*da_l
           ddicdt_s = ddicdt_s + Ic_s-(RC+0.5*RA)*mmpc/
     &     (1+kpo4pc/po4_s+kca/ca_s-1/(po4_s/kpo4pc+ca_s/kca))*pc_s 
     &     -RC*(mumpn/(1+kpo4pn/po4_s)*pn_s-rem*(dc_s+dn_s)) 
     &      +0.5*remA*da_s
ccccccccccccccccccccccccccccccccccccccccccccc


c ======== DEEP OCEAN REMINERALIZATION/BURIAL ====================== 

c po4 regeneration 
C WITH BURIAL AND COMPENSATING SOURCE ....................

c first decide fraction of sinking particulate phosphorus buried
c simple exponential particle flux profile, scale height zstar
c           p_frac_buried = 1.0*exp(-dz_d/zstar)
           p_frac_buried = 0.0
           a_frac_buried = min(real(1),
     &     max(real(0),(alk_d-dic_d-0.052)/0.095))

c           a_frac_buried = 1.0*exp(-dz_d/zstar)
c IF BURIAL NOT = 0, NEED COMPENSATING P SOURCE SOMEWHERE

c MICK - SET NO BURIAL ..............
c NO BURIAL, fix p_frac_buried = 0.0
c           p_frac_buried = 0.0                   
c OVERIDING INPUT HERE: fix p_frac_buried_shelf = 0.0
           p_frac_buried_shelf = 0.0                   
           a_frac_buried_shelf = 1.0

           dpo4dt_d = dpo4dt_d + (1-p_frac_buried)*
     &         ((vol_h*lambda_dc*dc_h+vol_l*lambda_dc*dc_l)/vol_d
     &         +(vol_h*lambda_dn*dn_h+vol_l*lambda_dn*dn_l)/vol_d)
     &         +(1-p_frac_buried_shelf)*
     &         vol_s*(lambda_dc*dc_s+lambda_dn*dn_s)/vol_d
     &         +rem*(dc_d+dn_d)         
c SHELF 
c shelf box has export, regeneration and burial in one layer!
c SPECIFY burial fraction on shelf (p_frac_buried_shelf)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C Carbon
           ca_sat = 0.1
           ddicdt_d = ddicdt_d + Ic_s + RC*(1-p_frac_buried)*
     &         ((vol_h*lambda_dc*dc_h+vol_l*lambda_dc*dc_l)/vol_d
     &         +(vol_h*lambda_dn*dn_h+vol_l*lambda_dn*dn_l)/vol_d)     
     &         +RC*(1-p_frac_buried_shelf)*vol_s
     &         *(lambda_dc*dc_s+lambda_dn*dn_s)/vol_d
     &         + 0.5*(1-a_frac_buried)*
     &         (vol_h*lambda_da*da_h+vol_l*lambda_da*da_l)/vol_d
     &         +0.5*(1-a_frac_buried_shelf)*vol_s*lambda_da*da_s/vol_d
     &         +RC*rem*(dc_d+dn_d)+0.5*remA*da_d
c     &         -max(real(0),0.5*kcomp*(alk_d-dic_d-ca_sat))
CCCCCCCCCCCCCCCCC...CCCCCCCCCCCCCC
C Alkalinity
           dalkdt_d = dalkdt_d + Ia_s + (1-a_frac_buried)
     &         *(vol_h*lambda_da*da_h+vol_l*lambda_da*da_l)/vol_d
     &         +(1-a_frac_buried_shelf)*vol_s*lambda_da*da_s/vol_d
     &         +remA*da_d
c     &         -max(real(0),kcomp*(alk_d-dic_d-ca_sat))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c shelf
c likewise consume o2 with regeneration 
c           IF(o2_s .gt. 4.0e-6*rho_ref)THEN
c           do2dt_s = do2dt_s -
c     &       ( (R_cp/R_co)*export_s
c     &       - (R_cp/R_co)*p_frac_buried_shelf*export_s )
c           ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc


c +++++++ PROGNOSTIC STEP +++++++++++++++++++++++++++++++++++++++++
cMICK euler forward or adams-bashforth type ...
c i_timestep = 0  for euler forward, 1 for adams-bashforth
C N.B. DO NOT USE A-B SCHEME !!!!!!!!
            i_timestep = 0
            if(i_timestep .eq. 1)then
               ab1 = (-0.5-epsilon)
               ab0 = (1.5+epsilon)
            else
               ab1 = 1.0
               ab0 = 0.0
            endif
            
c po4 .................
            po4_h = po4_h +  
     &          (dpo4dt_h*ab1 + dpo4dt_h_o*ab0)*dt
            po4_l = po4_l +  
     &          (dpo4dt_l*ab1 + dpo4dt_l_o*ab0)*dt
            po4_d = po4_d +  
     &          (dpo4dt_d*ab1 + dpo4dt_d_o*ab0)*dt
            po4_s = po4_s +  
     &          (dpo4dt_s*ab1 + dpo4dt_s_o*ab0)*dt

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c pc .................
            pc_h = pc_h +  
     &          (dpcdt_h*ab1 + dpcdt_h_o*ab0)*dt
            pc_l = pc_l +  
     &          (dpcdt_l*ab1 + dpcdt_l_o*ab0)*dt
            pc_d = pc_d +  
     &          (dpcdt_d*ab1 + dpcdt_d_o*ab0)*dt
            pc_s = pc_s +  
     &          (dpcdt_s*ab1 + dpcdt_s_o*ab0)*dt

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c pn .................
            pn_h = pn_h +  
     &          (dpndt_h*ab1 + dpndt_h_o*ab0)*dt
            pn_l = pn_l +  
     &          (dpndt_l*ab1 + dpndt_l_o*ab0)*dt
            pn_d = pn_d +  
     &          (dpndt_d*ab1 + dpndt_d_o*ab0)*dt
            pn_s = pn_s +  
     &          (dpndt_s*ab1 + dpndt_s_o*ab0)*dt

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c dc .................
            dc_h = dc_h +  
     &          (ddcdt_h*ab1 + ddcdt_h_o*ab0)*dt
            dc_l = dc_l +  
     &          (ddcdt_l*ab1 + ddcdt_l_o*ab0)*dt
            dc_d = dc_d +  
     &          (ddcdt_d*ab1 + ddcdt_d_o*ab0)*dt
            dc_s = dc_s +  
     &          (ddcdt_s*ab1 + ddcdt_s_o*ab0)*dt

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c dn .................
            dn_h = dn_h +  
     &          (ddndt_h*ab1 + ddndt_h_o*ab0)*dt
            dn_l = dn_l +  
     &          (ddndt_l*ab1 + ddndt_l_o*ab0)*dt
            dn_d = dn_d +  
     &          (ddndt_d*ab1 + ddndt_d_o*ab0)*dt
            dn_s = dn_s +  
     &          (ddndt_s*ab1 + ddndt_s_o*ab0)*dt

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c da .................
            da_h = da_h +  
     &          (ddadt_h*ab1 + ddadt_h_o*ab0)*dt
            da_l = da_l +  
     &          (ddadt_l*ab1 + ddadt_l_o*ab0)*dt
            da_d = da_d +  
     &          (ddadt_d*ab1 + ddadt_d_o*ab0)*dt
            da_s = da_s +  
     &          (ddadt_s*ab1 + ddadt_s_o*ab0)*dt

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Alkalinity
            alk_h = alk_h +  
     &          (dalkdt_h*ab1 + dalkdt_h_o*ab0)*dt
            alk_l = alk_l +  
     &          (dalkdt_l*ab1 + dalkdt_l_o*ab0)*dt
            alk_d = alk_d +  
     &          (dalkdt_d*ab1 + dalkdt_d_o*ab0)*dt
            alk_s = alk_s +  
     &          (dalkdt_s*ab1 + dalkdt_s_o*ab0)*dt
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Carbon
            dic_h = dic_h +  
     &          (ddicdt_h*ab1 + ddicdt_h_o*ab0)*dt
            dic_l = dic_l +  
     &          (ddicdt_l*ab1 + ddicdt_l_o*ab0)*dt
            dic_d = dic_d +  
     &          (ddicdt_d*ab1 + ddicdt_d_o*ab0)*dt
            dic_s = dic_s +  
     &          (ddicdt_s*ab1 + ddicdt_s_o*ab0)*dt
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Carbon
            atmos_moles_CO2 = atmos_moles_CO2 - airseafluxCO2*dt
            atmos_pco2 = atmos_moles_CO2/
     &            (atmos_moles_O2+atmos_moles_N2+atmos_moles_CO2) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c              write(6,*) ca_h 

c step forward time variable
            time = time + dt

c advance timestepping variables
           dpo4dt_h_o = dpo4dt_h
           dpo4dt_l_o = dpo4dt_l
           dpo4dt_d_o = dpo4dt_d
           dpo4dt_s_o = dpo4dt_s
           dpcdt_h_o =  dpcdt_h
           dpcdt_l_o =  dpcdt_l
           dpcdt_s_o =  dpcdt_s
           dpndt_h_o =  dpndt_h
           dpndt_l_o =  dpndt_l
           dpndt_s_o =  dpndt_s
           ddcdt_h_o =  ddcdt_h
           ddcdt_l_o =  ddcdt_l
           ddcdt_s_o =  ddcdt_s
           ddndt_h_o =  ddndt_h
           ddndt_l_o =  ddndt_l
           ddndt_s_o =  ddndt_s
           ddadt_h_o =  ddadt_h
           ddadt_l_o =  ddadt_l
           ddadt_s_o =  ddadt_s
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Carbon
           dalkdt_h_o = dalkdt_h
           dalkdt_l_o = dalkdt_l
           dalkdt_d_o = dalkdt_d
           dalkdt_s_o = dalkdt_s
           ddicdt_h_o = ddicdt_h
           ddicdt_l_o = ddicdt_l
           ddicdt_d_o = ddicdt_d
           ddicdt_s_o = ddicdt_s
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

c check -  determine volume integral of po4
           TotalP = po4_h*vol_h + po4_l*vol_l + 
     &              po4_d*vol_d 
     &              + po4_s*vol_s
           DICtot = (dic_h+RC*(pc_h+pn_h+dc_h)+0.5*da_h)*vol_h 
     &            + (dic_l+RC*(pc_l+pn_l+dc_l)+0.5*da_l)*vol_l + 
     &              (dic_d+RC*(pc_d+pn_d+dc_d)+0.5*da_d)*vol_d 
     &              + (dic_s+RC*(pc_s+pn_s+dc_s)+0.5*da_s)*vol_s 
     &             + atmos_moles_CO2
           ALKtot = (alk_h+RA*pc_h+da_h)*vol_h 
     &            + (alk_l+RA*pc_l+da_l)*vol_l + 
     &              (alk_d+RA*pc_d+da_d)*vol_d 
     &              + (alk_s+RA*pc_s+da_s)*vol_s


C DIAGNOSTICS TO SCREEN AND FILE ====================================
c write some output to the screen
            if(nstep .eq. 1)then
 
c             write(6,*)'       year       po4_h     po4_l     po4_d   
c     & po4_s    o2_h   o2_l   o2_d   o2_s  atmos_O2   totalP 
c     & dic_h    dic_l     dic_d    dic_s    pCO2 '
             write(iout,*)'year   dic_l     dic_h     dic_s    dic_d    
     & alk_l     alk_h    alk_s   alk_d    ca_l   ca_h ca_s'
             write(iout2,*)'   pCO2  po4_h  po4_d 
     & pc_h     pn_h     pc_l    pn_l    pc_s   pn_s  DICtot  ALKtot'

            end if


c write some diagnostics to the screen
           if(mod(nstep,nwrite) .eq. 0)then 

c              write(6,100)time/(360.*86400.),
c     &        po4_h*1.0e6/rho_ref, po4_l*1.0e6/rho_ref,
c     &        po4_d*1.0e6/rho_ref, po4_s*1.0e6/rho_ref,
c     &        o2_h*1.0e6/rho_ref, o2_l*1.0e6/rho_ref,
c     &        o2_d*1.0e6/rho_ref, o2_s*1.0e6/rho_ref, 
c     &        atmos_po2, totalP,
c     &        dic_h*1.0e6/rho_ref, dic_l*1.0e6/rho_ref,
c     &        dic_d*1.0e6/rho_ref, dic_s*1.0e6/rho_ref 
c     &        atmos_pco2*1.0e6
              write(iout,*) time/(360.*86400.),
     &        dic_l,dic_h,dic_s,dic_d,alk_l,alk_h,alk_s,alk_d,
     &        ca_l,ca_h,ca_s 
              write(iout2,*) atmos_pco2*1.0e6, po4_h, po4_d, 
     &        pc_h*1.0e9, pn_h*1.0e9, pc_l*1.0e9, pn_l*1.0e9, 
     &        pc_s*1.0e9, pn_s*1.0e9, DICtot, ALKtot


           end if
 100       format(1x,f15.1,4f9.4,4f8.3,f9.6,e13.5,4f8.3,f9.1)
 120       format(2e20.10)


        end do

c close output file
        close(iout)
        close(iout2)

         y(1) = atmos_pco2 
         y(2) = po4_l
         y(3) = po4_h 
         y(4) = po4_d
         y(5) = po4_s
         y(6) = dc_l 
         y(7) = dc_h 
         y(8) = dc_s 
         y(9) = dn_l 
         y(10) = dn_h 
         y(11) = dn_s 
         y(12) = da_l 
         y(13) = da_h 
         y(14) = da_s 
         y(15) = pc_l
         y(16) = pc_h 
         y(17) = pc_s 
         y(18) = pn_l 
         y(19) = pn_h 
         y(20) = pn_s 
         y(21) = dic_l 
         y(22) = dic_h 
         y(23) = dic_d 
         y(24) = dic_s 
         y(25) = alk_l 
         y(26) = alk_h 
         y(27) = alk_d 
         y(28) = alk_s 
         y(29) = t_h 
         y(30) = t_l 
         y(31) = t_d 
         y(32) = t_s 
         y(33) = s_h 
         y(34) = s_l 
         y(35) = s_d 
         y(36) = s_s 
         y(37) = pc_d
         y(38) = pn_d 
         y(39) = dc_d
         y(40) = dn_d
         y(41) = da_d 
         y(42) = time 
c ==========================================================
        do idummy = 1, nvalues  
          write(19,*)y(idummy)
        end do
        close(iend)


        end

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       subroutine ocean_transport(a_h, a_l, a_d, a_s,
     &                            q, m_hd, m_ld, m_lh, m_ds, m_ls,
     &                            vol_h, vol_l, vol_d, vol_s,
     &                            dadt_h, dadt_l, dadt_d, dadt_s)

c generic transport and mixing of tracer "a"

       implicit none

c a_h, a_l, a_d, a_s = local concentration variables
       REAL*8 a_h
       REAL*8 a_l
       REAL*8 a_d
       REAL*8 a_s
       REAL*8 m_hd
       REAL*8 m_ld
       REAL*8 m_lh
       REAL*8 m_ds
       REAL*8 m_ls
       REAL*8 q
       REAL*8 vol_h
       REAL*8 vol_l
       REAL*8 vol_d
       REAL*8 vol_s
       REAL*8 dadt_h
       REAL*8 dadt_l
       REAL*8 dadt_d
       REAL*8 dadt_s

       dadt_h = dadt_h +
     &   ( -(q + m_lh)*(a_h - a_l) - m_hd*(a_h - a_d) )/vol_h      

       dadt_l = dadt_l +
     &   ( -(q + m_ld)*(a_l - a_d) - m_lh*(a_l - a_h) 
     &                             + m_ls*(a_s - a_l) )/vol_l
CMICK - add interaction of low lat surface and shelf

       dadt_d = dadt_d +
     &   ( -(q + m_hd)*(a_d - a_h) - m_ld*(a_d - a_l) 
     &      - m_ds*(a_d - a_s) ) /vol_d

       dadt_s = dadt_s +
     &   (-m_ds*(a_s - a_d) - m_ls*(a_s - a_l))/vol_s


       return
       end


c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Air sea flux of carbon
       subroutine carbon_air_sea(dic, pa, dco2dt, alk, ve, 
     &                           hmix,temp_C,sal, rho_ref, dt, nt,
     &                           flux, pflux, ca) 


       implicit none

       REAL*8 ca
       REAL*8 alk
       REAL*8 dic
       REAL*8 dco2dt
       REAL*8 piston_velocity
       REAL*8 thickness
       REAL*8 temp_C
       REAL*8 rho_ref
       REAL*8 co2flux
       REAL*8 c
       REAL*8 dc
       REAL*8 cinit
       REAL*8 ak0
       REAL*8 ak1
       REAL*8 ak2
       REAL*8 akk
       REAL*8 akw
       REAL*8 chk
       REAL*8 hmix
       REAL*8 p
       REAL*8 pa
       REAL*8 sigmac
       REAL*8 a
       REAL*8 sal
       REAL*8 d(6)
       REAL*8 h
       REAL*8 a0
       REAL*8 a1
       REAL*8 ve
       REAL*8 temp
       REAL*8 u
       REAL*8 cl
       REAL*8 hnew
       REAL*8 hinit
       REAL*8 pold
       REAL*8 cold
       REAL*8 dt
       REAL*8 asflux
       REAL*8 flux
       REAL*8 pflux
       REAL*8 pfold
       REAL*8 pguess
       REAL*8 pppp
       REAL*8 ptest
       REAL*8 buff
       REAL*8 b
       REAL*8 akb
       REAL*8 xa
       REAL*8 hh(3)
       REAL*8 ff(3)
       REAL*8 ftest
       REAL*8 hcheck
       REAL*8 htest
       REAL*8 ya
        
       INTEGER nt
       INTEGER ntmax
       INTEGER nheader
       INTEGER nout
       INTEGER niter
       INTEGER i
       INTEGER ei
       INTEGER ik
       INTEGER ikb
       INTEGER ip
       INTEGER j
       INTEGER k


       temp = temp_C + 273.15
       b = 0.0d-6
c       a = 2300.0d-6 
       a = alk/rho_ref 
       c = dic/rho_ref

       p = pa
       niter = 100

C DETERMINE  COEFFICIENTS ................................
c determine alkalinity as function of salinity
c see Brewer et al, (1986), (careful, sal in 0/00) 
c also determine chlorinity
c and total dissolved boron, b.
        a0=547.05
        a1=50.560
        cl=(sal-0.03)/1.805
c hinit=initial guess h (newton-raphson only)
        hinit=8.0d-9
c
c
c calculate equilibrium coeffs as function of temp,sal
        akw=6.463d-15
c first k0 from Hoffert et al ......... 
c       akk=(-2622.38/temp)+15.5873-(0.0178471*temp)+
c    +        cl*(0.0117950-(2.77676e-5*temp))
c       ak0=10.0**(-akk)
c ak0 from GEOSECS (Takahashi et al.) mole/(kg atmos.)
        akk=-60.2409+(93.4517*(100.0/temp))
     +        +(23.3585*log(temp/100.0))
     +             +sal*(0.023517
     +             +(-0.023656*(temp/100.0))
     +             +(0.0047036*(temp/100.0)*(temp/100.0)))
        ak0=exp(akk)
c
c choose Hoffert et al (iks=0) or Dickson and Millero (ik=1)
c for equilibrium coeffs k1, k2
        ik=1
        if(ik.eq.0)then
c determine equilibrium coeffs (following Hoffert et al. 1979)
        akk=(3523.46/temp)-15.6500+(0.034153*temp)-
     +        (0.074709*sqrt(cl))-(0.0023483*cl)
        ak1=10.0**(-akk)
c
        akk=(2902.39/temp)-6.4980+(0.023790*temp)-
     +        (0.45322*sqrt(cl))+(0.035226*cl)
        ak2=10.0**(-akk)
        endif
c
        if(ik.eq.1)then
c merbach's eqm coeffs (following dickson + millero)
        akk=(3670.7/temp)-62.008+(9.7944*dlog(temp))-
     +         (0.0118*sal)+(0.000116*sal*sal)
        ak1=10.0**(-akk)
c
        akk=(1394.7/temp)+4.777-(0.0184*sal)+
     +         (0.000118*sal*sal)
        ak2=10.0**(-akk)
        endif
c 
c choose determination of kb (ikb=o ; Hoffert et al,
c ikb=1 ; GEOSECS/Takahashi)
        ikb=1
        if(ikb.eq.0)then
c Hoffert et al's kb (typo in paper?) 
        akk=(2291.90/temp)-3.3850+(0.01756*temp)-
     +        (0.32051*sqrt(cl))
        akb=10.0**(-akk)
        endif
c
        if(ikb.eq.1)then
c GEOSECS (Takahashi et al) ==> kb
        akk=-9.26+(0.00886*sal)+(0.01*(temp-273.15))
        akb=10.0**(akk)
        endif 
C COEFFICIENTS DONE ................................


c solve for pH, etc...
c iterate for solution of p
c flux determined from "timestep average" of p
        do 90 ip=1,10
c
c set initial dissolved carbon and first guess at hydrogen
c ion concentration for first timestep
            if(nt.eq.0)then
                hnew=hinit
                c=cinit
            else
c
c for nt>1 step forward in c:
c
c evaluate air-sea flux (mole /m2 /s) using estimated
c timestep average p (pflux)
c first iteration, pflux=p (from last timestep)
c evaluate prognosis for new c, => new p etc
c set cold = final c at previous timestep
c !!! careful with units - here such that
c dc1 is determined in units mol/l !!!!
                if(ip.eq.1)cold=c
                if(ip.eq.1)pflux=p
                flux=(ve*ak0)*(pa-pflux)
                dc=flux*dt/hmix
                c=cold+dc
            endif
c
c determine hydrogen ion concentration - 
c find root for 5th order polynomial
c first set coeffs according to c,a,k's
c d(i) is the coeff of the (i)th power of h
            d(6)=1
            d(5)=ak1+a+akb
            d(4)=a*ak1-akw+ak1*ak2-c*ak1
     +               +akb*(a+ak1-b) 
            d(3)=a*ak1*ak2-akw*ak1-2.0*c*ak1*ak2
     +               +akb*(a*ak1-c*ak1-akw+ak1*ak2-ak1*b) 
            d(2)=-ak1*ak2*akw
     +               +akb*(a*ak1*ak2-2.0*c*ak1*ak2-ak1*akw
     +                     -ak1*ak2*b)
            d(1)=-akw*ak1*ak2*akb
c
c 
c bracket and bisection
c .................................................................
c try a different root finder - bracket and bisection
c set max/min h appropriate for pH 6.5 - 8.8
c hh(1)=hmax, hh(2)=hmin, hh(3)=hmid
c
c initial brackets and mid-point
        hh(1)=300.0d-9
        hh(2)=1.5d-11
        hh(3)=(hh(1)+hh(2))*0.5
c
c evaluate f(h) for these, for f(hmid) and limit of same sign,
c replace that limit by hmid and iterate
c
        do 200 j=1,niter
c
            do 210 k=1,3
                ff(k)=0.0
                    do 220 i=1,6
                        ei=i-1
                        ff(k)=ff(k)+(d(i)*hh(k)**ei)
  220               continue
  210       continue
c
            htest=hh(3)
c reset hmax,hmin and hmid
            ftest=ff(1)/ff(3)
            if(ftest.gt.0.0)then
                hh(1)=hh(3)
            else
                hh(2)=hh(3)
            endif
            hh(3)=(hh(1)+hh(2))*0.5
  200   continue
c now check for convergence
            hcheck=(htest/hh(3))-1.0
            chk=sqrt(hcheck*hcheck)
           if(chk.le.1.0d-6)then
                h=hh(3)
c                go to 205
c            endif
            else   
c
c solution not converged - stop integration
            write(6,225)niter      
  225       format(1x,'bisection not converged for h after' 
     +  i4' iterations')
            stop
            endif
c
c  205   endif
c 
c end of bracket/bisection h solve
c ................................................................        

c h now determined: evaluate new equivalent p
c this is the estimated p, pguess
c iterate process using pflux=(p+pguess)/2 to make
c better dc estimate
c
            pold=p
            xa=ak0*(1.0+(ak1/h)+(ak1*ak2/(h*h)))
            ya=1.0 / (1.0+(ak1/h)+(ak1*ak2/(h*h)))
            pguess=c/xa

c for first timestep no need to iterate
            if(nt.eq.1)go to 92
            pfold=pflux
            pflux=(pfold+pguess)*0.5
c if change in pflux is small then no further iteration
            pppp=(pflux-pfold)/pfold 
            ptest=sqrt(pppp*pppp)
            if(ptest.le.1.0d-7)go to 92
c
  90    continue
c
  92    p=pguess
        ca=rho_ref*p*ak0*ak1*ak2/(h*h)
        dco2dt=dco2dt+dc*rho_ref/dt
        flux=flux*rho_ref
c        write(6,*) pflux, pa, flux, ca
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

