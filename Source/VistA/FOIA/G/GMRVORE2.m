GMRVORE2 ;HIRMFO/RM,MD-USER PROMPT ROUTINE ;12/10/96  11:47
 ;;4.0;Vitals/Measurements;;Apr 25, 1997
ASK ;  ASK PROMPTS FOR AN ORDER  GMRVORDF=START^STOP^ADM SCH^SPEC INSTRUCT
 ;    WHERE IF THE PIECE="" THEN NO DEFAULT FOR PROMPT
 ;             THE PIECE=VALUE THEN VALUE IS DEFAULT FOR PROMPT
 ;             THE PIECE=~VALUE THEN VALUE TO BE STUFFED WITHOUT PROMPT
 ;    RETURNS GMRVORAN=START^STOP^ADM SCH^ADM TIME^FREQ^COMM WITH
 ;    THE ANSWERS, AND GMROUT.
 ;      
 I ORACTION=1,'GMRVNEW W !!,"Editing order ",$P(GMRVORD(2),U,5)
STDT ;
 I GMRVSTRT?1"~".E S GMRVSTRT=$E(GMRVSTRT,2,99) G SCHED
 I +GMRVSTRT S Y=GMRVSTRT D D^DIQ S %DT("B")=Y
 S %DT("A")="Start DATE (TIME optional): ",Y=$S($P(GMRVORD(1),U,9)'=""&(ORACTION=2):$P(GMRVORD(1),U,9),$P(GMRVORD(2),U,6)="":"NOW",1:$P(GMRVORD(2),U,6))
 X ^DD("DD") S:'$D(%DT("B")) %DT("B")=Y S %DT="AETX" S:$P($G(GMRVANSR),U,2)'="" %DT(0)=-$S($P(GMRVANSR,U,2)["~":$E($P(GMRVANSR,U,2),2,99),1:$P(GMRVANSR,U,2)) D ^%DT K %DT I +Y'>0 S GMROUT=1 Q
 S GMRVSTRT=+Y
SCHED ;
 I GMRVAS?1"~".E D  G EDCM
 .  S PSJX=$E(GMRVAS,2,99),PSJPP="GMRV",PSJNE=1 D ENSV^PSJEEU
 .  S GMRVAS=$G(PSJX)_U_PSJAT_U_PSJM
 .  K PSJX,PSJPP,PSJM,PSJAT,PSJY,PSJTS,PSJAX,PSJW,PSJNE
 .  Q
 D:ORACTION=1!(ORACTION=0) ADS^GMRVORC0 Q:GMROUT
EDCM ;
 I GMRVCOM?1"~".E S GMRVCOM=$E(GMRVCOM,2,99) G STOP
 S GMRVDF=$P(GMRVCOM,"^") W !,"Special Instructions: "_$S(GMRVDF'="":GMRVDF_"// ",1:"") R GMRVCOM:DTIME S:'$T GMRVCOM="^^" S:$T&(GMRVDF'="")&(GMRVCOM="") GMRVCOM=GMRVDF S $P(GMRVORAN,U,6)=GMRVCOM
 I "^^"[GMRVCOM S:$L(GMRVCOM) GMROUT=1 S:GMRVCOM="^^" DIROUT=1 G:'$L(GMRVCOM) STOP Q
DCM I GMRVCOM="@",GMRVDF'="" S %="" W !,?4,$C(7),"Are you sure you want to delete these SPECIAL INSTRUCTIONS" D YN^DICN G:%=2 EDCM W:%=0 !,?5,$C(7),"Answer YES or NO" G:%=0 DCM I %=1!(%=-1) S:%=-1 GMROUT=1 S:%=1 GMRVCOM="@" Q
 I '($L(GMRVCOM)>2&($L(GMRVCOM)<101)&(GMRVCOM?1AN.E)) W:GMRVCOM'?1"?".E $C(7),"  ??" S:GMRVCOM?2"?".E XQH="GMRV-ORD. ADDL INFO" D:GMRVCOM?2"?".E EN^XQH W:GMRVCOM?1"?".E !!,?3,"Answer must be 3-100 characters in length" S GMRVCOM="" G EDCM
STOP ;
 I GMRVSTOP?1"~".E S GMRVSTOP=$E(GMRVSTOP,2,99) Q
 I +GMRVSTOP S Y=GMRVSTOP D D^DIQ S %DT("B")=Y
 S %DT("A")="Stop DATE (TIME optional): " I $S((ORACTION=2!($P(GMRVORD(1),U,9)=""))&$D(^GMRD(120.57,1,0)):1,1:0) S X1=GMRVSTRT,X2=+$P(^GMRD(120.57,1,0),U,2) D C^%DTC
 S Y=$S(ORACTION'=2&($P(GMRVORD(1),U,9)'=""):$P(GMRVORD(1),U,9),'$D(^GMRD(120.57,1,0)):GMRVSTRT,1:X)
 X ^DD("DD") S:'$D(%DT("B")) %DT("B")=Y S %DT="AETX" S:$D(GMRVANSR) %DT(0)=+$S($P(GMRVANSR,U)["~":$E($P(GMRVANSR,U),2,99),1:$P(GMRVANSR,U)) D ^%DT K %DT I +Y'>0 S GMROUT=1 Q
 I Y<GMRVSTRT W !,?3,$C(7),"THE STOP DATE CANNOT BE EARLIER THAN THE START DATE" G STOP
 S GMRVSTOP=+Y
 Q
