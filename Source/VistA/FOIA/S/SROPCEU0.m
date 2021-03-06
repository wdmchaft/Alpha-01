SROPCEU0 ;BIR/ADM - UNTRANSMITTED OUTPATIENT ENCOUNTERS (CONT.) ;06/21/05
 ;;3.0; Surgery ;**69,77,50,142**;24 Jun 93
 ;
 ; Reference to ^ECC(723 supported by DBIA #205
 ;
 U IO S (SRNEW,SRSOUT,SRSUB)=0,(SRHDR,SRPAGE)=1,Y=SDATE X ^DD("DD") S STARTDT=Y,Y=EDATE X ^DD("DD") S ENDATE=Y
 S SRRPT="Outpatient Surgery Encounters Not Transmitted to NPCD"
 S SRTITLE="For Completed "_$S(SRFLG=1:"O.R. Surgical Procedures",SRFLG=2:"Non-O.R. Procedures",1:"O.R. Surgical and Non-O.R. Procedures")
 S SRFRTO="From: "_STARTDT_"  To: "_ENDATE,SRINST=SRSITE("SITE") D NOW^%DTC S Y=$E(%,1,12) X ^DD("DD") S SRPRINT="Report Printed: "_Y
 I SRSORT G S1
 D HDR I SRSEL=1 S SRTN=0 F  S SRTN=$O(^TMP("SR69",$J,SRTN)) Q:'SRTN!SRSOUT  D CASE
 Q:SRSOUT  D:$Y+10>IOSL PAGE Q:SRSOUT
 W !!,$S(SRSPEC:"SPECIALTY: "_SRSPECN,1:" * * *  ALL SPECIALTIES  * * *")
TOT W !!,"      Total with NO status: ",$J(SRCNT(0),5)
 W !,"      Total with NON-COUNT: ",$J(SRCNT(12),5)
 W !,"Total with ACTION REQUIRED: ",$J(SRCNT(14),5),!,?28,"-----"
 W !,"    Total cases identified: ",$J(SRCNT,5) S SRSUB=0
 Q
S1 S (SRSP,SRSPECN)=0 F  S SRSPECN=$O(^TMP("SRSP",$J,SRSPECN)) Q:SRSPECN=""!SRSOUT  S SRNEW=1 D PAGE Q:SRSOUT  D
 .I SRSEL=1 S SRTN=0 F  S SRTN=$O(^TMP("SRSP",$J,SRSPECN,SRTN)) Q:'SRTN!SRSOUT  D CASE
 .Q:SRSOUT  S X=^TMP("SRSP",$J,SRSPECN,0),SRCNT(0)=$P(X,"^"),SRCNT(12)=$P(X,"^",2),SRCNT(14)=$P(X,"^",3),SRCNT=$P(X,"^",4),SRSUB=1 D:$Y+10>IOSL PAGE Q:SRSOUT
 .D TOT
 Q:SRSOUT  S SRSUB=1 D PAGE Q:SRSOUT  W !!," * * *  COMBINED TOTALS FOR ALL SPECIALTIES  * * *" S X=^TMP("SRSP",$J,0),SRCNT(0)=$P(X,"^"),SRCNT(12)=$P(X,"^",2),SRCNT(14)=$P(X,"^",3),SRCNT=$P(X,"^",4) D TOT
 Q
SUBHD W !!,">>> "_$S($P(SRSPECN,";;")=1:"SURGICAL",1:"MEDICAL")_" SPECIALTY: "_$P(SRSPECN,";;",2)_$S('SRNEW:"  * * continued * *",1:"")
 S SRNEW=0 I SRSORT W !
 Q
CASE ; print case info
 D:$Y+6>IOSL PAGE Q:SRSOUT
 S SRNON=0 I $P($G(^SRF(SRTN,"NON")),"^")="Y" S SRNON=1
 S SRSS=$S('SRNON:$P(^SRF(SRTN,0),"^",4),1:$P(^SRF(SRTN,"NON"),"^",8)),SRSSNM=$S('SRNON:$P(^SRO(137.45,SRSS,0),"^"),1:$P(^ECC(723,SRSS,0),"^"))
 S SRSTATUS=$S('SRSORT:^TMP("SR69",$J,SRTN),1:^TMP("SRSP",$J,SRSPECN,SRTN)) I SRSTATUS="" S SRSTATUS="<NONE>"
 I SRSORT D CLIN
 D DEM,PROC W !,SRSDATE,?23,SRTN,?38,$S(SRSORT:$E(SRLOC,1,20),1:$E(SRSSNM,1,20)),?61,$S(IOM<82:$E(SRSTATUS,1,19),1:SRSTATUS)
 W !,SRSNM,?23,SRPROC(1),!,SRSSN_"  ("_SRAGE_")" W:$D(SRPROC(2)) ?23,SRPROC(2) W:(SRFLG=3)&SRNON !,"NON-O.R." I $D(SRPROC(3)) W:'SRNON ! W ?23,SRPROC(3)
 W ! F I=1:1:IOM W "-"
 Q
DEM ; get patient dempgraphic information
 S SR(0)=^SRF(SRTN,0),DFN=$P(SR(0),"^") D DEM^VADPT S SRSNM=VADM(1),SRSSN=VA("PID"),(SRSDT,Y)=$P(SR(0),"^",9) X ^DD("DD") S SRSDATE=Y,X1=$E(SRSDT,1,7),X2=$P(VADM(3),"^"),SRAGE=$E(X1,1,3)-$E(X2,1,3)-($E(X1,4,7)<$E(X2,4,7))
 I $L(SRSNM)>21 S SRSNM=$P(VADM(1),",")_","_$E($P(VADM(1),"^",2))_"."
 Q
PROC ; get principal procedure
 K SRPROC S X=$P(^SRF(SRTN,"OP"),"^") I $L(X)<58 S SRPROC(1)=X
 I $L(X)>57 S K=1 F  D  I $L(X)<58 S SRPROC(K)=X Q
 .F I=0:1:56 S J=57-I,Y=$E(X,J) I Y=" " S SRPROC(K)=$E(X,1,J-1),X=$E(X,J+1,$L(X)) S K=K+1 Q
 Q
CLIN ; get associated clinic
 S X=$P(^SRF(SRTN,0),"^",21) I X S SRLOC=X
 I 'SRNON,'X S X=$P(^SRO(137.45,SRSS,0),"^",5) S:X SRLOC=X I 'X S Y=$P(^SRF(SRTN,0),"^",2) I Y S X=$P(^SRS(Y,0),"^") I X S SRLOC=X
 I SRNON,'X S X=$P(^SRF(SRTN,"NON"),"^",2) I X S SRLOC=X
 S SRLOC=$S(SRLOC:$P(^SC(SRLOC,0),"^"),1:"<NOT ENTERED>")
 Q
PAGE I $E(IOST)="P"!SRHDR D HDR Q
 W ! K DIR S DIR(0)="E" D ^DIR K DIR I $D(DTOUT)!$D(DUOUT) S SRSOUT=1 Q
HDR ;  print heading
 I $D(ZTQUEUED) D ^SROSTOP I SRHALT S SRSOUT=1 Q
 W:$Y @IOF W:$E(IOST)="P" !,?(IOM-$L(SRINST)\2),SRINST W !,?(IOM-$L(SRRPT)\2),SRRPT,?(IOM-10),$J("Page "_SRPAGE,9),!,?(IOM-$L(SRTITLE)\2),SRTITLE,!,?(IOM-$L(SRFRTO)\2),SRFRTO
 I $E(IOST)="P" W !,?(IOM-$L(SRPRINT)\2),SRPRINT W:SRSUB !
 I SRSEL=1,'SRSUB W !!,"DATE OF "_$S(SRFLG=1:"OPERATION",SRFLG=2:"PROCEDURE",1:"OP/PROCEDURE"),?23,"CASE #",?38,$S(SRSORT:"CLINIC",1:"SPECIALTY"),?61,"SCHED STATUS",!,"PATIENT NAME",?23,"PRINCIPAL PROCEDURE",!,"PATIENT ID  (AGE)"
 S (SRHDR,SRSUB)=0,SRPAGE=SRPAGE+1 W ! F I=1:1:IOM W "="
 I SRSORT D:SRSPECN'="" SUBHD S SRNEW=0
 Q
REFILE ; re-file cases in PCE
 N SRVISIT,SRVSIT K DIC S DIC=9.4,DIC(0)="XM",X="SURGERY" D ^DIC K DIC Q:Y=-1  S SRPKG=+Y
 S (SRK,SRTN)=0,SRS="SURGERY DATA",SRFILE=1
 F  S SRTN=$O(^TMP("SR69",$J,SRTN)) Q:'SRTN  D
 .S (SRVISIT,SRVSIT)=$P(^SRF(SRTN,0),"^",15),SRV=$$DELVFILE^PXAPI("PRV^POV^CPT",SRVSIT)
 .D UTIL^SROPCEP I 'SRK D
 ..D TMP^SROPCEP
 ..S SRVSIT=SRVISIT,SRV=$$DATA2PCE^PXAPI("^TMP(""SRPXAPI"",$J)",SRPKG,SRS,SRVSIT)
 ..K ^TMP("SRPXAPI",$J)
 Q
