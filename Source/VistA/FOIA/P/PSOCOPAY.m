PSOCOPAY ;BHAM ISC/RTR - DAYS SUPPLY TOTAL FOR ACCOUNTS RECEIVABLE ; 07/28/93
 ;;7.0;OUTPATIENT PHARMACY;;DEC 1997
POT(DFN) N FILL,RECNO,DAYS S X=0 G:'$G(DFN) END
 S FILL="" F  S FILL=$O(^PSRX("ACP",DFN,DT,FILL)) Q:FILL=""  F RECNO=0:0 S RECNO=$O(^PSRX("ACP",DFN,DT,FILL,RECNO)) Q:'RECNO  D
 .I 'FILL,$P($G(^PSRX(RECNO,0)),"^",11)="W",$G(^PSRX(RECNO,"IB")),'+$P($G(^(2)),"^",13) S X=X+$S($P(^(0),"^",8)>60:3,$P(^(0),"^",8)>30:2,1:1) Q
 .I FILL,$P($G(^PSRX(RECNO,1,FILL,0)),"^",2)="W",'+$P($G(^(0)),"^",18),$G(^PSRX(RECNO,"IB")) S DAYS=$S($P(^PSRX(RECNO,1,FILL,0),"^",10):$P(^(0),"^",10),1:$P($G(^PSRX(RECNO,0)),"^",8)) S X=X+$S(DAYS>60:3,DAYS>30:2,1:1) K DAYS Q
END ;
 Q X
