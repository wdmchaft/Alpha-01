PRCASVC6 ;WASH-ISC@ALTOONA,PA/RGY-CHECK OUT AR BILL ;4/8/92  12:12 PM
V ;;4.5;Accounts Receivable;**154**;Mar 20, 1995
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 K PRCAERR
 S PRCAERCD=$S('$D(PRCASV("ARREC")):"PRCA006",'$D(^PRCA(430,PRCASV("ARREC"),0)):"PRCA007",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 S PRCAERCD=$S('$P(^PRCA(430,PRCASV("ARREC"),0),U,8):"PRCA008",",201,220,"'[(","_$P(^PRCA(430.3,+$P(^(0),U,8),0),U,3)_","):"PRCA009",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 S PRCAERCD=$S('$D(PRCASV("BDT")):"PRCA010",PRCASV("BDT")'?7N:"PRCA011",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 S PRCAERCD=$S('$D(PRCASV("APR")):"PRCA012",'$D(^VA(200,PRCASV("APR"),0)):"PRCA013",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 D FY I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 S PRCAERCD=$S('$D(PRCASV("CAT")):"PRCA024",'$D(^PRCA(430.2,PRCASV("CAT"),0)):"PRCA025",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 S PRCAT=$P(^PRCA(430.2,PRCASV("CAT"),0),"^",6) D CKDEBTR I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 I PRCAT="C" S PRCAERCD=$S('$D(PRCASV("CARE")):"Type of care is missing",PRCASV("CARE")'?1.2N:"Type of care is not in expected format",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q
 D:PRCAT="T" THIRD
Q S PRCASV("OKAY")=$S($G(PRCAERR)<0:0,1:1) K PRCAERCD,PRCAT Q
THIRD ;Check out data for Third party bills
 S DFN=+PRCASV("PAT") D DEM^VADPT
 S PRCAERCD=$S('$D(PRCASV("PAT")):"Patient is missing",VAERR:"Patient is undefined",1:1) I 'PRCAERCD S PRCAERR="-1^"_PRCAERCD G Q4
 I $D(PRCASV("2NDINS")),'$D(^DIC(36,+PRCASV("2NDINS"),0)) S PRCAERR="-1^2nd insurance company is undefined" G Q4
 I $D(PRCASV("3RDINS")),'$D(^DIC(36,+PRCASV("3RDINS"),0)) S PRCAERR="-1^3rd insurance company is undefined" G Q4
 F Y="IDNO^242","GPNO^244","GPNM^243","INPA^239" I $D(PRCASV($P(Y,"^"))) S X=PRCASV($P(Y,"^")) X $P(^DD(430,$P(Y,"^",2),0),"^",5,999) I '$D(X) S PRCAERR="-1^"_$P(^DD(430,$P(Y,"^",2),0),"^")_" is not in expected format" Q
Q4 K VAERR
 Q
 ;
CKDEBTR ;Check PRCASV("DEBTOR") variable pattern match.
 I $S('$D(PRCASV("DEBTOR")):1,PRCASV("DEBTOR")="":1,1:0) S PRCAERCD="PRCA018" G Q1
 I "PC"[PRCAT,PRCASV("DEBTOR")?1N.E1";DPT(" S DFN=+PRCASV("DEBTOR") D DEM^VADPT I 'VAERR G Q1
 I PRCAT="T",PRCASV("DEBTOR")?1N.E1";DIC(36,",$D(^DIC(36,+PRCASV("DEBTOR"),0)) G Q1
 I PRCAT="N",PRCASV("DEBTOR")?1N.E1";DIC(4,",$D(^DIC(4,+PRCASV("DEBTOR"),0)) G Q1
 I PRCAT="V",PRCASV("DEBTOR")?1N.E1";PRC(440,",$D(^PRC(440,+PRCASV("DEBTOR"),0)) G Q1
 I PRCAT="O",PRCASV("DEBTOR")?1N.E1";VA(200,",$D(^VA(200,+PRCASV("DEBTOR"),0)) G Q1
 S PRCAERCD="PRCA019"
Q1 K VAERR
 Q
 ;
FY ;Check out FY variable
 S PRCAORA=0 I '$D(PRCASV("FY")) S PRCAERCD="PRCA015" G Q2
 F X=1:2 Q:'$P(PRCASV("FY"),"^",X)  S PRCAORA=PRCAORA+$P(PRCASV("FY"),"^",X+1)
 I $P(PRCASV("FY"),"^")="" S PRCAERCD="PRCA016" G Q2
 I PRCAORA<0 S PRCAERCD="PRCA017" G Q2
Q2 K PRCAORA Q
 ;
PRE154 ;PRE-INIT FOR PATCH PRCA*4.5*154
 K ^PRCA(347.4,"ACR"),^("AWR")
 Q
