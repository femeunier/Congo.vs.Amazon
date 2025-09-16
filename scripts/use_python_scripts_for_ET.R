rm(list = ls())

library(reticulate)

use_python("/home/femeunier/miniconda3/bin/python", required = T)
py_config()

source_python("~/Downloads/convert.py")
source_python("~/Downloads/fao.py")
source_python("~/Downloads/thornthwaite.py")
source_python("~/Downloads/_check.py")


lat <- deg2rad(0.25) ; doy <- seq(15,365,30)
Tave = rep(26,12) ; tasmin = rep(20,12) ; tasmax = rep(30,12) ;
year = 2000

# thornthwaite
monthly_mean_dlh <- monthly_mean_daylight_hours(lat, year)
y1 = thornthwaite(Tave, rep(12,12), year)  # mm/month

# Hargreaves
Tave = 26 ; tasmin = 20 ; tasmax = 30 ; doy = 15 ; ws = 1 ;  altitude = 0 ; coastal = FALSE
year = 2000

sol.dec <- sol_dec(doy)
sha <- sunset_hour_angle(lat, sol.dec)
ird <- inv_rel_dist_earth_sun(doy)
et.rad <- et_rad(lat, sol.dec, sha, ird)
y2 = hargreaves(tasmin, tasmax, Tave, et.rad) *31  # mm/d --> mm/month

# FAO
et.rad <- et_rad(lat, sol.dec, sha, ird)
cs.rad <- cs_rad(altitude, et.rad)
sol.rad <- sol_rad_from_t(et.rad, cs.rad, tasmin, tasmax, coastal)
ni.sw.rad <- net_in_sol_rad(sol.rad)
avp <-  avp_from_tmin(tasmin)
no.lw.rad <- net_out_lw_rad(tasmin, tasmax, sol.rad, cs.rad, avp)
net.rad <- net_rad(ni.sw.rad, no.lw.rad)
svp <- svp_from_t(Tave)
delta.svp <- delta_svp(Tave)
atm <- atm_pressure(altitude)
psy <- psy_const(atm)

y3 = fao56_penman_monteith(net.rad, Tave, ws, svp, avp, delta.svp, psy, shf=0.0) *31 # mm/day --> mm/month
y3
