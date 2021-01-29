FUNCTION take_values,nx2,ny2,column,row,phaseT,phaseNT,contrastT,contrastNT

ratio = 4096/nx2 ;Modified on Sep 8, 2011. Now it assumes that cols and rows will not be necessarily 4k.

x0 = (column/ratio) ;a column number
y0 = (row/ratio) ;a row number
x1 = x0+1
y1 = y0+1

IF x1 GE nx2 THEN BEGIN
  x0 = x0-1 ;we will extrapolate at the edges
  x1 = x1-1
ENDIF
IF y1 GE ny2 THEN BEGIN
  y0 = y0-1
  y1 = y1-1
ENDIF

x0 = LONG(x0)
x1 = LONG(x1)
y0 = LONG(y0)
y1 = LONG(y1)

xa = (DOUBLE(x1)-DOUBLE(column MOD ratio)/DOUBLE(ratio)-DOUBLE(x0))
xb = (DOUBLE(column MOD ratio)/DOUBLE(ratio))
ya = (DOUBLE(y1)-DOUBLE(row MOD ratio)/DOUBLE(ratio)-DOUBLE(y0))
yb = (DOUBLE(row MOD ratio)/DOUBLE(ratio))

loc1 = x0 + y0*nx2
loc2 = x1 + y0*nx2
loc3 = x0 + y1*nx2
loc4 = x1 + y1*nx2

phaseNTi = DBLARR(4)
phaseTi = DBLARR(3)
contrastNTi = DBLARR(4)
contrastTi = DBLARR(3)

phaseNTi[0] = ya*(phaseNT[loc1]*xa+phaseNT[loc2]*xb) $
             +yb*(phaseNT[loc3]*xa+phaseNT[loc4]*xb)
phaseNTi[1] = ya*(phaseNT[loc1+nx2*ny2]*xa+phaseNT[loc2+nx2*ny2]*xb) $
             +yb*(phaseNT[loc3+nx2*ny2]*xa+phaseNT[loc4+nx2*ny2]*xb)
phaseNTi[2] = ya*(phaseNT[loc1+2*nx2*ny2]*xa+phaseNT[loc2+2*nx2*ny2]*xb) $
             +yb*(phaseNT[loc3+2*nx2*ny2]*xa+phaseNT[loc4+2*nx2*ny2]*xb)
phaseNTi[3] = ya*(phaseNT[loc1+3*nx2*ny2]*xa+phaseNT[loc2+3*nx2*ny2]*xb) $
             +yb*(phaseNT[loc3+3*nx2*ny2]*xa+phaseNT[loc4+3*nx2*ny2]*xb)
phaseTi[0] = ya*(phaseT[loc1*3]*xa+phaseT[loc2*3]*xb)+yb*(phaseT[loc3*3]*xa+phaseT[loc4*3]*xb)
phaseTi[1] = ya*(phaseT[loc1*3+1]*xa+phaseT[loc2*3+1]*xb)+yb*(phaseT[loc3*3+1]*xa+phaseT[loc4*3+1]*xb)
phaseTi[2] = ya*(phaseT[loc1*3+2]*xa+phaseT[loc2*3+2]*xb)+yb*(phaseT[loc3*3+2]*xa+phaseT[loc4*3+2]*xb)

contrastNTi[0]= ya*(contrastNT[loc1]*xa+contrastNT[loc2]*xb) $
               +yb*(contrastNT[loc3]*xa+contrastNT[loc4]*xb)
contrastNTi[1]= ya*(contrastNT[loc1+nx2*ny2]*xa+contrastNT[loc2+nx2*ny2]*xb) $
               +yb*(contrastNT[loc3+nx2*ny2]*xa+contrastNT[loc4+nx2*ny2]*xb)
contrastNTi[2]= ya*(contrastNT[loc1+2*nx2*ny2]*xa+contrastNT[loc2+2*nx2*ny2]*xb) $
               +yb*(contrastNT[loc3+2*nx2*ny2]*xa+contrastNT[loc4+2*nx2*ny2]*xb)
contrastNTi[3]= ya*(contrastNT[loc1+3*nx2*ny2]*xa+contrastNT[loc2+3*nx2*ny2]*xb) $
               +yb*(contrastNT[loc3+3*nx2*ny2]*xa+contrastNT[loc4+3*nx2*ny2]*xb)
contrastTi[0] = ya*(contrastT[loc1]*xa+contrastT[loc2]*xb) $
               +yb*(contrastT[loc3]*xa+contrastT[loc4]*xb)
contrastTi[1] = ya*(contrastT[loc1+nx2*ny2]*xa+contrastT[loc2+nx2*ny2]*xb) $
               +yb*(contrastT[loc3+nx2*ny2]*xa+contrastT[loc4+nx2*ny2]*xb)
contrastTi[2] = ya*(contrastT[loc1+2*nx2*ny2]*xa+contrastT[loc2+2*nx2*ny2]*xb) $
               +yb*(contrastT[loc3+2*nx2*ny2]*xa+contrastT[loc4+2*nx2*ny2]*xb)

RETURN,{contrastNTi:contrastNTi,contrastTi:contrastTi,phaseNTi:phaseNTi,phaseTi:phaseTi}

END
