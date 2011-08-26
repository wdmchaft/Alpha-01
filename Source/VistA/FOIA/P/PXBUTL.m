PXBUTL ;ISL/JVS,ESW - UTILITIES FOR PROMPTS ; 10/31/02 12:13pm
 ;;1.0;PCE PATIENT CARE ENCOUNTER;**32,108**;Aug 12, 1996
 ;
 ;
 ;
WAIT ;--SPINNING CURSOR
 I PXBMOD=20 W IOCUB,"\"
 I PXBMOD=40 W IOCUB,"|"
 I PXBMOD=60 W IOCUB,"/"
 I PXBMOD=80 W IOCUB,"-"
 Q
CASE ;--CHANGE LOWER CASE TO UPPER CASE
 I $D(DATA) S DATA=$TR(DATA,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 I $D(EDATA) S EDATA=$TR(EDATA,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 I $D(NARR) S NARR=$TR(NARR,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 Q
PRIM ;--PRIMARY PROVIDER
 N PXBSKY,PXBKY,PXBSAM,PXBCNT,PRVDR,FPRI
 D PRV^PXBGPRV(PXBVST,.PXBSKY,.PXBKY,.PXBSAM,.PXBCNT,.PRVDR,.FPRI)
 I $D(PRVDR) Q
 I '$D(PXBSKY) Q
 ;
 D RSET^PXBDREQ("PRV")
 S $P(REQI,"^",7)=$O(PXBSKY(1,0))
 S $P(REQI,"^",2)="P"
 S $P(REQI,"^",1)=$P(^AUPNVPRV($O(PXBSKY(1,0)),0),"^",1)
 ;
 D EN0^PXBSTOR(PXBVST,PATIENT,REQI)
 D EN1^PXKMAIN
 Q
PRIMD ;--PRIMARY DIAGNOSIS
 D POV^PXBGPOV(PXBVST)
 I $D(PXDIGNS) Q
 I '$D(PXBSKY) Q
 ;
 D RSET^PXBDREQ("POV")
 S $P(REQI,"^",9)=$O(PXBSKY(1,0))
 S $P(REQI,"^",6)="P"
 S $P(REQI,"^",5)=$P(^AUPNVPOV($O(PXBSKY(1,0)),0),"^",1)
 ;
 D EN0^PXBSTOR(PXBVST,PATIENT,REQI)
 D EN1^PXKMAIN
 Q
 ;
 ;
HDR(PXBVST,NO,PXBIOF) ;--Header for each screen PATIENT and DATE/TIME of Visit
 N DATE,DIC,DIQ,DATE,DA,DR
 N CLINICE
 I '$D(IORVON) D TERM^PXBCC
 S DIC=9000010,DR=".01;.05;.22",DA=PXBVST,DIQ="AMANDA(",DIQ(0)="EI" D EN^DIQ1
 S PATIENT=$G(AMANDA(9000010,PXBVST,.05,"I"))
 S NAME=$G(AMANDA(9000010,PXBVST,.05,"E"))
 S DATE=$G(AMANDA(9000010,PXBVST,.01,"E"))
 S IDATE=$G(AMANDA(9000010,PXBVST,.01,"I"))
 S CLINIC=$G(AMANDA(9000010,PXBVST,.22,"I"))
 S CLINICE=$G(AMANDA(9000010,PXBVST,.22,"E"))
 I $L(CLINICE)>20 S CLINICE=$E(CLINICE,1,20)
 K AMANDA
 I '$G(PXBIOF) W @IOF
 ;
 I '$G(NO) W !
 I $G(NO) D
 .W IOINHI,!,IOCUU,"PAT/APPT/CLINIC:  ",$E(NAME,1,18),"  ",DATE,?((IOM-2)-$L(CLINICE)),CLINICE,IOINLOW
 Q
HDR2(FROM) ;--SECOND LINE IN THE HEADER
 I '$D(FROM) Q
 I FROM="STP" D LOC^PXBCC(1,0) D
 .I PXBCNT=0 W "STOP CODE: ..There are ",$G(PXBCNT)," STOP CODES associated with this ENCOUNTER",IOELEOL
 .I PXBCNT=1 W "STOP CODE: ..There is ",$G(PXBCNT)," STOP CODE associated with this ENCOUNTER",IOELEOL
 .I PXBCNT>1 W "STOP CODE: ..There are ",$G(PXBCNT)," STOP CODES associated with this ENCOUNTER",IOELEOL
 .D UNDON^PXBCC
 .W !
 .F  W $C(32) Q:$X=(IOM-(4))
 .D UNDOFF^PXBCC
 ;
 ;
 ;
 I FROM="PRV" D LOC^PXBCC(1,0) D
 .I PXBCNT=0 W "PROVIDER: ..There are ",$G(PXBCNT)," PROVIDERS associated with this ENCOUNTER",IOELEOL
 .I PXBCNT=1 W "PROVIDER: ..There is ",$G(PXBCNT)," PROVIDER associated with this ENCOUNTER",IOELEOL
 .I PXBCNT>1 W "PROVIDER: ..There are ",$G(PXBCNT)," PROVIDERS associated with this ENCOUNTER",IOELEOL
 .D UNDON^PXBCC
 .W !
 .F  W $C(32) Q:$X=(IOM-(4))
 .D UNDOFF^PXBCC
 Q
HDR3(DFN,NO,PXBIOF) ;--Header for each screen PATIENT and DATE/TIME of Visit
 ;
 ; NO  = IF 1 then just do line feed don't do header
 ; PXBIOF = IF 1 then don't W @IOF
 ;
 ;
 N DATE,DIC,DIQ,DATE,DA,DR
 N NAME,SEX,AGE,SSN
 I '$D(IORVON) D TERM^PXBCC
 S DIC=2,DR=".01;.02;.033;.09",DA=DFN,DIQ="AMANDA(",DIQ(0)="EI" D EN^DIQ1
 S NAME=$G(AMANDA(2,DFN,.01,"E"))
 S SEX=$G(AMANDA(2,DFN,.02,"E"))
 S AGE=$G(AMANDA(2,DFN,.033,"E"))
 S SSN=$G(AMANDA(2,DFN,.09,"E"))
 S SSN=$E(SSN,1,3)_"-"_$E(SSN,4,5)_"-"_$E(SSN,6,9)
 K AMANDA
 I '$G(PXBIOF) W @IOF
 ;
 I '$G(NO) W !
 I $G(NO) W IOINHI,!,IOCUU,"PAT/SEX/AGE/SSN: ",$E(NAME,1,18),"    ",SEX,"    ",AGE_" Years    ",?67,SSN W IOINLOW
 Q
 ;
TIMES(ENT) ;--Number of time the selection appears in v file from PXBKY
 ;
 N N
 S N=0,Q=0 F  S N=$O(PXBKY(ENT,N)) Q:N=""  S Q=Q+1,Q(N)=""
 Q
 ;
CPTOK(CODE,IDATE) ;--check the historical date to see if it was active
 ;TO BE USED AS A FUNCTION (W $$CPT......)
 ; OK=1-- IT WAS ACTIVE
 ; OK=0-- IT WAS NOT ACTIVE
 ; RETURNS  OK^INTERNAL FORM OF STATUS DATE^EXTERNAL FORM
 N STADATE,STAFLAG,EDATE,STATUS,Y,DATE
 S DATE=$P(IDATE,".",1)
 S STATUS=$P($$CPT^ICPTCOD(CODE,DATE),U,6,7),STADATE=+STATUS,OK=+$P(STATUS,U,2)
 S X=STADATE D H^%DTC,YX^%DTC S EDATE=Y K X,Y,%H,%T,%Y
 Q OK_"^"_STADATE_"^"_EDATE
 ;
CPTSCREN(CODE,IDATE) ;
 N OK,DATE
 S DATE=$P(IDATE,".",1)
 S OK=$P($$CPT^ICPTCOD(CODE,DATE),U,7)
 Q +OK
 ;
CONPRV(PRV) ;---FUNCTION-Convert internal form or provider to external form
 N DIC,DA,DR,DIQ,PXBPRV
 S DIC=200,DA=PRV,DR=.01,DIQ="PRVA(",DIQ(0)="E" D EN^DIQ1
 S PRV=$G(PRVA(200,PRV,.01,"E")) K PRVA
 Q PXBPRV_"^"_PRV
 ;
NONE(NO) ;----Display's a None message to the screen if none is found
 N X
 I NO=1 S X="No PROVIDERS for this Encounter." D W
 I NO=2 S X="No CPT CODES for this Encounter." D W
 I NO=3 S X="No DIAGNOSIS for this Encounter." D W
 I NO=4 S X="No PROBLEM LIST for this PATIENT." D W
 I NO=5 S X="No STOP CODE for this ENCOUNTER." D W
 I NO=6 S X="No ENCOUNTERS for this PATIENT." D W
 Q
W W !,?(IOM-$L(X))\2,IOINHI,X,IOINLOW
 Q
