FHMNREP ;Hines OIFO/RTK - Dietetics Monitor Report ;10/18/01  11:49
 ;;5.5;DIETETICS;;Jan 28, 2005
 ;
DATE ;sets date
 ; Check for multidivisional site
 I $P($G(^FH(119.9,1,0)),U,20)'="N" D ^FHMMNREP Q
 S (FHTADM,FHTMON)=0
 W ! S %DT="AEPT",%DT("A")="Enter beginning date: " D ^%DT Q:Y<0
 S FHSDT=Y,%DT(0)=FHSDT,%DT("A")="Enter ending date: " D ^%DT K %DT(0)
 S FHEDT=Y I Y<0 D END Q
 D SORTCR S FHSORT=Y I Y="^" D END Q
 I FHSORT="C" D FHCL Q:'$D(FHCLIEN)  S FHNXIEN=CLNAM
 I FHSORT="W" D FHWA Q:'$D(FHWRIEN)  S FHNXIEN=WRDNAM
 D DEV,END Q
 ;
FHCL ;
 K DIR S DIR(0)="Y",DIR("A")="Select ALL Clinicians",DIR("B")="Y" D ^DIR
 I Y=1 S (FHCLIEN,CLNAM)="ALL"
 I Y=0 K DIC S DIC="^VA(200,",DIC(0)="AEQM",DIC("A")="Select CLINICIAN: " D ^DIC S FHCLIEN=$P(Y,U,1),CLNAM=$P($G(^VA(200,FHCLIEN,0)),U,1)
 I (Y=-1)!($D(DUOUT))!($D(DTOUT)) D END Q
 Q
FHWA ;
 K DIR S DIR(0)="Y",DIR("A")="Select ALL Wards",DIR("B")="Y" D ^DIR
 I Y=1 S (FHWRIEN,WRDNAM)="ALL"
 I Y=0 K DIC S DIC="^FH(119.6,",DIC(0)="AEQM" D ^DIC S FHWRIEN=$P(Y,U,1),WRDNAM=$P($G(^FH(119.6,FHWRIEN,0)),U,1)
 I (Y=-1)!($D(DUOUT))!($D(DTOUT)) D END Q
 Q
EN ;
 ;FHDATA SUBSCRIPTS(CLINIC OR WARD NAME,DGPM DATE,DGPM ENTRY)
 ;FHDATA ARRAY="PatName^SSN^Monitors?^DischargeDt^DFN^Status"
 ;
 K FHDATA,FHMON
 S I=FHSDT F  S I=$O(^DGPM("ATT1",I)) Q:'I!(I>FHEDT)  D
 .S J=0 F  S J=$O(^DGPM("ATT1",I,J)) Q:'J  D
 ..S FHTADM=FHTADM+1
 ..S DFN=$P($G(^DGPM(J,0)),U,3)
 ..S FHZ115="P"_DFN D CHECK^FHOMDPA I FHDFN="" Q
 ..I '$D(^FHPT(FHDFN,"A",J,"MO","B")) Q
 ..S II=$P(I,".")
 ..S WRD=$P($G(^FHPT(FHDFN,"A",J,0)),U,8),CLN=""
 ..I WRD'="" S CLN=$P($G(^FH(119.6,WRD,0)),"^",2)
 ..;S CLN=$P($G(^DGPM(J,0)),U,19),WRD=$P($G(^DGPM(J,0)),U,6)
 ..S INDX=$S(FHSORT="C":CLN,1:WRD) I INDX="" Q
 ..S INDX=$S(FHSORT="C":$P($G(^VA(200,CLN,0)),U,1),1:$P($G(^FH(119.6,WRD,0)),U,1))
 ..S $P(FHDATA(INDX,II,J),U,1)=$E($P(^DPT(DFN,0),U,1),1,23)
 ..S $P(FHDATA(INDX,II,J),U,5)=DFN
 ..S $P(FHDATA(INDX,II,J),U,2)=$E($P(^DPT(DFN,0),U,9),6,9)
 ..I $D(^FHPT(FHDFN,"A",J,"MO","B")) S $P(FHDATA(INDX,II,J),U,3)="Yes",FHTMON=FHTMON+1,MCNT=0 D
 ...F FHMN=0:0 S FHMN=$O(^FHPT(FHDFN,"A",J,"MO",FHMN)) Q:FHMN'>0  S MCNT=MCNT+1,FHMON(DFN,J,MCNT)=$P($G(^FHPT(FHDFN,"A",J,"MO",FHMN,0)),"^",1)
 ..S Y=$P($P($G(^FHPT(FHDFN,"A",J,0)),U,14),".",1) I Y X ^DD("DD") S $P(FHDATA(INDX,II,J),U,4)=Y
 ..I $D(^FHPT(FHDFN,"S",0)) S NS=$O(^FHPT(FHDFN,"S",0)),STAT=$P($G(^FHPT(FHDFN,"S",NS,0)),U,2) S $P(FHDATA(INDX,II,J),U,6)=$P($G(^FH(115.4,STAT,0)),U,1)
 ..Q
 .Q
 D PRINT^FHMNPRT
 Q
DEV ;get device and set up queue
 W ! K %ZIS,IOP S %ZIS="Q" D ^%ZIS Q:POP
 I '$D(IO("Q")) U IO D EN,^%ZISC,END Q
 S ZTRTN="EN^FHMNREP",ZTSAVE("FHSDT")="",ZTSAVE("FHEDT")=""
 S ZTSAVE("FHNDT")="",ZTSAVE("FHPER")="",ZTSAVE("FHSORT")=""
 S ZTSAVE("FHNXIEN")="",ZTSAVE("FHTADM")="",ZTSAVE("FHTMON")=""
 S ZTDESC="Dietetics Monitor Report" D ^%ZTLOAD
 D ^%ZISC K %ZIS,IOP
 D END Q
SORTCR ;
 K DIR S DIR(0)="SB^C:CLINICIAN;W:WARD",DIR("A")="Sort by Clinician/Ward"
 D ^DIR
 Q
END ;kill and quit
 K CLN,CLNAM,FHDFN,DFN,I,II,INDX,J,SSN,MCNT
 K FHCLIEN,FHEDT,FHMN,FHNDT,FHNXIEN,FHTADM,FHTMON
 K FHPER,FHSDT,FHSORT,FHWRIEN,WRD,WRDNAM,X,Y,Z
 Q
