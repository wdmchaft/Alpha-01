PSAVSN ;BHM/DAV - Check for and reduce size of VSN node ;10/9/97
 ;;3.0; DRUG ACCOUNTABILITY/INVENTORY INTERFACE;**37**; 10/24/97
 S PSACTRL=0
1 S PSACTRL=$O(^XTMP("PSAPV",PSACTRL)) G DONE:PSACTRL'>0 S PSALINE=0
PSAP31 ;entry point from PSAUP5
2 S PSALINE=$O(^XTMP("PSAPV",PSACTRL,"IT",PSALINE)) G WHERE:PSALINE'>0
 S PSADATA=$G(^XTMP("PSAPV",PSACTRL,"IT",PSALINE))
 S PSAP31=$P($G(PSADATA),"^",5) I $L(PSAP31)'>25 G 2
 S CNT=0,PSAP31A=$P(PSAP31,"~~",2) F X=3:1 Q:$P(PSAP31,"~~",X)=""  S PSAP31B=$P(PSAP31,"~~",X) I $G(PSAP31B)=PSAP31A S $P(PSAP31,"~~",X)=""
 S PSAP31D=$L(PSAP31) F X=PSAP31D:-1 S X1=$E(PSAP31,X) Q:X1'="~"  S PSAP31=$E(PSAP31,1,(X-1))
 G 2
DONE K PSALINE,PSAP31,PSAP31A,PSAP31B,PSAP31C,PSAP31D,CNT S X="PSAP31" X ^%ZOSF("DEL") Q
WHERE ;Doing actual upload or Post PSA*3*31 utility
 I $G(PSAUPLD)'=1 G 1
 G DONE
