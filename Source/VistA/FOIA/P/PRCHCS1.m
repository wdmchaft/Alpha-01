PRCHCS1 ;WISC/CTB-LOG CODE SHEET STRING GEN. ;12/1/93  09:51
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;PRELOAD DATA INTO FILE 423, CODE SHEETS.
 ;**** RESPONSE TIME MONITER
 ;D:$D(XRTL) T0^%ZOSV
 G:'$D(PRCHTP) DOUT K D0,DA,DIC,DIE D NEWCS^PRCFAC G:'$D(DA) DOUT
 K PRCHCODS F I=0,"TRANS" S PRCHCODS(I)=^PRCF(423,DA,I)
 S N=0 F I=1:1 S N=$O(PRCHTP(N)) Q:'N  D ENA
 S N="" F I=1:1 S N=$O(PRCHCODS(N)) Q:N=""  S ^PRCF(423,DA,N)=PRCHCODS(N)
 I $D(^PRCF(423,DA,1)) S X=$P(^(1),U,29) I X]"" S ^PRCF(423,"AI",X,DA)=""
 D SETR
 ;**** RESPONSE TIME MONITER
 ;S:$D(XRT0) XRTN=$T(+0) D:$D(XRT0) T1^%ZOSV
 K PRCHCODS,PRCHDA,PRCHDD,PRCHDIC,PRCHDIC1,PRCHFL,PRCHF1,PRCHF2,PRCHIN,PRCHNODE,PRCHPIEC D ^PRCFACX1 Q
ENA S PRCHDA=$P(PRCHTP(N),",",2),PRCHDD=$P(PRCHTP(N),","),PRCHDIC=$P(PRCHTP(N),",",3,999)
 S:PRCHDIC="" PRCHDIC=^DIC(PRCHDD,0,"GL") S PRCHDIC=PRCHDIC_PRCHDA_","
 S J=0 F I=1:1 S J=$O(PRCHTP(N,J)) Q:'J  D ENB
 Q
ENB Q:PRCHTP(N,J)=""  S PRCHF1=$P(PRCHTP(N,J),";"),PRCHF2=$P(PRCHTP(N,J),";",2),PRCHIN=$P(PRCHTP(N,J),";",3) I PRCHF1'=+PRCHF1 X PRCHF1 G ENC
 S Y=$P(^DD(PRCHDD,PRCHF1,0),"^",4),PRCHNODE=$P(Y,";"),PRCHPIEC=$P(Y,";",2) S:'$D(PRCHDIC1(N,PRCHNODE)) PRCHDIC1(N,PRCHNODE)=$S($D(@(PRCHDIC_PRCHNODE_")")):^(PRCHNODE),1:"") S Y=PRCHDIC1(N,PRCHNODE),X=$P(Y,"^",PRCHPIEC)
ENC S Y=$P(^DD(423,PRCHF2,0),"^",4,99),PRCHNODE=$P($P(Y,"^"),";"),PRCHPIEC=$P($P(Y,"^"),";",2) I PRCHIN["I"!($D(PRCHTP("IT"))) S PRCHITRN=$P(Y,"^",2,99) X PRCHITRN K PRCHITRN
 S:$D(X) $P(PRCHCODS(PRCHNODE),"^",PRCHPIEC)=X
 Q
SETR ;PUT CODE SHEET RECORD NO.INTO FILE 442
 I PRCHTYP="A",PRCFA("TT")=550 S $P(^PRC(442,PRCHPO,18),U,16)=DA Q
 I PRCHTYP="A" S $P(^PRC(442,PRCHPO,2,PRCHLI,4),U,8)=DA Q
 I PRCHTYP="R" I PRCFA("TT")=551!(PRCFA("TT")=552) S $P(^PRC(442,PRCHPO,11,PRCHRPT,1),U,7)=DA Q
 I PRCHTYP="R" S $P(^PRC(442,PRCHPO,2,PRCHLI,3,PRCHRRI,0),U,6)=DA
 Q
DOUT K PRCFA S %=0
 Q
