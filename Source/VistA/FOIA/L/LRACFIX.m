LRACFIX ;SLC/DCM - REBUILD ^LRO(68,"AC") FROM A GIVEN DATE AFTER ALL LRAC X-REF ARE REINITIALIZED. ; 5/30/86  2:47 PM ;
 ;;5.2;LAB SERVICE;;Sep 27, 1994
 S %DT("A")="ENTER STARTING DATE FOR REINITIALIZATION: ",%DT="QEA" D ^%DT Q:Y<1  S LRXDT=9999999-Y D LRDFN
END W !!,"DONE" Q
LRDFN S LRDFN=0 F  S LRDFN=$O(^LR(LRDFN)) Q:LRDFN<1  D LRIDT
 Q:'$D(^LR(LRDFN,"MI"))  S LRIDT=0 F  S LRIDT=$O(^LR(LRDFN,"MI",LRIDT)) Q:LRIDT<1!(LRIDT>LRXDT)  S:'$D(^LRO(68,"MI",LRDFN,LRIDT)) ^(LRIDT)="" W ":"
 Q
LRIDT Q:'$D(^LR(LRDFN,"CH"))  S LRIDT=0 F  S LRIDT=$O(^LR(LRDFN,"CH",LRIDT)) Q:LRIDT<1  D LRSB
 Q
LRSB S $P(^LR(LRDFN,"CH",LRIDT,0),U,9)=$S(LRIDT>LRXDT:"1:0",1:"") Q:LRIDT>LRXDT
 S LRSB=1 F  S LRSB=$O(^LR(LRDFN,"CH",LRIDT,LRSB)) Q:LRSB<1  D SB1
 Q
SB1 I '$D(^LRO(68,"AC",LRDFN,LRIDT,LRSB)) S ^(LRSB)="" W "."
 Q
