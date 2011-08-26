PSIVRP1 ;BIR/MLM-REPRINT IV LABELS FROM WARD OR MANUFACTURING LIST (CONT. FROM PSIVRP) ;20 JUN 94 / 3:22 PM
 ;;5.0; INPATIENT MEDICATIONS ;**58**;16 DEC 97
 ;
 ; Reference to ^PS(55 is supported by DBIA 2191.
 ;
DEQ ;
 S STR=$S(LAST("LIST")="M":"LIST^PSIVT^PSIVDT^X1^X2^DFN^ON",1:"LIST^PSIVT^WRD^PSIVDT^DFN^ON") F X=1:1:$L(STR,"^") S @$P(STR,"^",X)=LAST($P(STR,"^",X))
 S PSIVT=$E(PSIVDT,1),PSIVOD(PSIVT)=$E(PSIVDT,2,$L(PSIVDT)) D @($S($D(^PS(55,"PSIVWLM",PSIVSN,PSIVDT)):"MONL",1:"WONL")),QUIT
 Q
GTDATE ;
 I $D(PSR) S PSIVDT=$O(PSR(PSIVDT)) Q:PSIVDT=""  S PSIVT=$E(PSIVDT,1),PSIVOD(PSIVT)=$E(PSIVDT,2,$L(PSIVDT)) D @($S($D(^PS(55,"PSIVWLM",PSIVSN,PSIVDT)):"X1",1:"WRD"))
 Q
WONL ;
 L +^PS(55,"PSIVWL",PSIVSN):1 E  W:$Y @IOF W !!,"**** WARNING --- LABELS NOT RUN, WARD LIST IN PROGRESS" Q
 D WON L -^PS(55,"PSIVWL",PSIVSN)
 Q
MONL ;
 L +^PS(55,"PSIVWLM",PSIVSN):1 E  W:$Y @IOF W !!,"**** WARNING --- LABELS NOT RUN, MANUFACTURING LIST IN PROGRESS",! Q
 D MON L -^PS(55,"PSIVWLM",PSIVSN)
 Q
WRD ;
 S WRD=$O(^PS(55,"PSIVWL",PSIVSN,WRD)) G:WRD=""!(PSIVDT="") GTDATE I WRD="",('$D(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT))) G GTDATE
WDFN ;
 S DFN=$O(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT,DFN)) G:DFN="" WRD
WON ;
 S ON=$O(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT,+DFN,ON)) G:ON="" WDFN Q:DFN=NEXT("DFN")&(ON=NEXT("ON"))  D MEOWRPT^PSIVLBL1 G WON
 Q
X1 ;
 S X1=$O(^PS(55,"PSIVWLM",PSIVSN,PSIVDT,PSIVT,X1)) I X1="" S WRD="" G GTDATE
X2 ;
 S X2=$O(^PS(55,"PSIVWLM",PSIVSN,PSIVDT,PSIVT,X1,X2)) G:X2="" X1
MDFN ;
 S DFN=$O(^PS(55,"PSIVWLM",PSIVSN,PSIVDT,PSIVT,X1,X2,DFN)) G:DFN="" X2
MON ;
 S ON=$O(^PS(55,"PSIVWLM",PSIVSN,PSIVDT,PSIVT,X1,X2,+DFN,ON)) G:ON="" MDFN Q:DFN=NEXT("DFN")&(ON=NEXT("ON"))  S WRD=$S($D(^PS(55,"PSIVWLM",PSIVSN,PSIVDT,PSIVT,X1,X2,DFN,ON)):$P(^(ON),"^",2),1:"") D MEOWRPT^PSIVLBL1 G MON
 Q
GTMES ;
 S (WRD,ON)="" F X=0:0 S WRD=$O(^PS(55,"PSIVWL",PSIVSN,WRD)) Q:WRD=""  S PSIVDT="" F X=0:0 S PSIVDT=$O(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT)) Q:PSIVDT=""  I PSIVDT[Y D GTMES1
 Q
GTMES1 ;
 S DFN="" F X=0:0 S DFN=$O(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT,DFN)) Q:DFN=""!$D(PS(PSIVDT))  S ON="" F X=0:0 S ON=$O(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT,DFN,ON)) Q:ON=""!(ON="OK")  D GTMES2
 Q
GTMES2 ;
 I $D(^PS(55,"PSIVWL",PSIVSN,WRD,PSIVDT,DFN,ON)),$P(^(ON),"^",4) S PS(PSIVDT)=""
 Q
QUIT ;
 K C,D1,DFN,DIC,FILE,I,J,JJ,LAST,LIST,NAD,NEXT,NF,ON,P,POP,PRO,PS,PSCT,PSIVCD,PSIVMT,PSIVNOL,PSGDT,PSM,PSR,PSIVDRG,PSIVDT,PSIVDTS,PSIVST,PSIVT,PSIVOD,STR
 K VAERR,WRD,X1,X2,X3,XT,XQUIT,Z,ZTDESC,ZTIO,ZTRTN,ZTSAVE D ENIVKV^PSGSETU S:$D(ZTQUEUED) ZTREQ="@"
 Q
