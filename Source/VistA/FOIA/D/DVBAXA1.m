DVBAXA1 ; ;07/14/09
 S X=DE(6),DIC=DIE
 K:$E(X,6,9)'="" ^DPT("BS",$E(X,6,9),DA)
 S X=DE(6),DIC=DIE
 ;
 S X=DE(6),DIC=DIE
 K ^DPT("BS5",$E(^DPT(DA,0),1)_$E(X,6,9),DA)
 S X=DE(6),DIC=DIE
 S A1B2TAG="PAT" D ^A1B2XFR
 S X=DE(6),DIC=DIE
 D KILL^DGREGDD1(DA,.6,0,21,0)
 S X=DE(6),DIC=DIE
 K ^DPT("SSN",$E(X,1,30),DA)
 S X=DE(6),DIC=DIE
 S VADFN=DA D KILL^VADPT6 K VADFN
 S X=DE(6),DIC=DIE
 S PPP=X,X="PPPFMX" X ^%ZOSF("TEST") D:$T KNSSN^PPPFMX S X=PPP K PPP
 S X=DE(6),DIC=DIE
 ;
 S X=DE(6),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DE(6),DIC=DIE
 S PX=X,X="PXXDPT" X ^%ZOSF("TEST") D:$T KILLSSN^PXXDPT S X=PX K PX
 S X=DE(6),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".09;" D AVAFC^VAFCDD01(DA)
 S X=DE(6),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
 S X=DE(6),DIIX=2_U_DIFLD D AUDIT^DIET
