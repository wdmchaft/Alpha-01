PRPFRPT ;ALTOONA/CTB-PATIENT FUNDS MISC REPORT GENERATOR ;4/22/02
V ;;3.0;PATIENT FUNDS;**6,13**;JUNE 1, 1989
DORMANT ;;REPORT OF DORMANT PATIENT ACCOUNTS
 W !!,"Enter number of days since last transaction for account to be included",!,"on this report. 180// " R X:$S($D(DTIME):DTIME,1:60) G:'$T!(X["^") OUT I X="" S X=180
 I +X'=X!(X<1)!(X[".") W *7,?$X+5,"RESPONSE MUST BE AN INTEGER GREATER THAN ZER0",! G DORMANT Q
 S X="T-"_X,%DT="EX" D ^%DT G:Y<0 OUT S PRPF("SDAT")=Y
 S %A="Do you wish to include accounts with zero (0) balances",%B="",%=2 D ^PRPFYN G:%<0 OUT S PRPF("ZERO")=$S(%=1:"XXX",1:0)
 D SELRNG^PRPFQ
 I PRPFRNG="" D OUT QUIT
 I PRPFRNG="@" S PRPFRNG2=""
 E  S PRPFRNG2=PRPFRNG
 S ZTRTN="DQ^PRPFRPT",ZTDESC=$P($T(DORMANT),";",3),ZTSAVE("PRPF*")="" D ^PRPFQ D:'$D(XQY) ENCON^PRPFQ G OUT
CK S PRPF("DATE")=$P(^PRPF(470,DA,0),"^",11),PRPF("BAL")=$P(^(1),"^",4)
 I +PRPF("BAL")'=PRPF("ZERO"),+PRPF("DATE")'>PRPF("SDAT") S ^TMP("PRPFAE",$J,DA)="" I '$D(ZTQUEUED),I#25=0 W "."
 Q
DQ ;ENTRY POINT FOR QUEUED OUTPUT
 S IOP=PRIOP
 I $D(ZTQUEUED) S ZTREQ="@"
 E  D WAIT^PRPFYN
 K ^TMP("PRPFAE",$J)
 S DA=0 F I=1:1 S DA=$O(^PRPF(470,DA)) Q:'DA  D CK
 S Y=PRPF("SDAT") D D^PRPFU1 S DHD="LISTING OF PATIENT FUNDS ACCOUNTS INACTIVE SINCE "_Y
 I '$D(^TMP("PRPFAE",$J)) W !!,DHD D NOW^PRPFQ S X="DATE: "_%X D MSG^PRPFU1 W !!,"THERE ARE NO ACCOUNTS IN THE FILE MEETING THE ABOVE CRITERION AT THIS TIME.",!! G OUT
 S DIC="^PRPF(470,",L=0,L(0)=1,BY="@73:99;S1,.01",BY(0)="^TMP(""PRPFAE"",$J,",FLDS="[PRPF DORMANT ACCOUNT LIST]",FR=""_PRPFRNG_"",TO=""_PRPFRNG2_""
 S DIOEND="K ^TMP(""PRPFAE"",$J) W !,""The information contained in this report is protected by the Privacy Act of 1974"""
 S:PRPFRNG="@" BY="@73,@73:99;S1,.01",FR="@,@",TO=","
 W !,"" D EN1^DIP
OUT K %,%DT,%X,%Y,DFN,DG1,DGA1,DGT,DGX,DIJ,DP,PRIOP,PRPF,PRPFRNG,PRPFRNG2,IOY,X,Y,DIOEND Q
DISPLAY ;DISPLAY INDIVIDUAL TRANSACTION
 S DIC=470.1,DIC(0)="AEQ" D ^DIC I +Y>0 S DA=+Y,DR=0 D EN^DIQ G DISPLAY
