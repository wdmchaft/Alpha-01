PRCFACR1 ;WISC/CTB/CLH/BGJ-PRINT TRANSMISSION AND SENT MESSAGES TO XM ;4/30/93  2:38 PM
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 K ^TMP($J)
 I $D(ZTQUEUED) D DT^DICRW,KILL^%ZTLOAD
 F I=1:1 S PBATN=$O(^PRCF(421.2,"AD",PRCFKEY,0)) Q:+PBATN=0  S PBAT=$P(^PRCF(421.2,PBATN,0),"^") D BLIST K ^PRCF(421.2,"AD",PRCFKEY,PBATN) S $P(^PRCF(421.2,PBATN,0),"^",15)="",I=1
SE K ^TMP("PRCFBTCH",$J) S IOP=$S($D(ION):ION,1:IO) D NOW^%DTC
 S DIC="^PRCF(423,",L=0,BY="[PRCFA BATCH TRANSMIT SORT]",FLDS="[PRCFA BTCH TRANSMIT]",(FR,TO)=PRCFKEY D EN1^DIP
XM ;THIS SECTION WILL TAKE THE GLOBALS CREATED BY THE FILE MANAGER AND PASS THEM TO MAILMAN FOR DELIVERY TO AUSTIN.
 S N=0 F I=1:1 S N=$O(^TMP("PRCFBTCH",$J,N)) Q:N'=+N  S PTYP=$O(^PRCF(423.9,"AC",N,0)) Q:PTYP=""  I $P(^PRCF(423.9,PTYP,0),"^",4)["Y" D TYPE
 S N=0 F I=1:1 S N=$O(^PRCF(423,"AK",PRCFKEY,N)) Q:'N  S $P(^PRCF(423,N,"TRANS"),"^",11)=""
 K ^PRCF(423,"AK",PRCFKEY),%,%DT,%I,BATCH,BATTYPE,DP,I,J,K,L,M,N,PRCFX,PTYP,X,Y,Z1,Z2
OUT K %H,%Y,A,ADD,B,C,DIC,DIJ,DQTIME,ER,FAIL,POP,POK,PTR,PTRN,PBAT,PBATN,PRCFRT,X1,XMDUZ,XMDT,XMM,XMKK,XMLOCK,XMR,XMSUB,XMT,XMTEXT,XMY,IOX,IOY,XMZ,^PRCF("LIST"),^TMP("PRCFBTCH",$J),PRCFKEY Q
 Q
TYPE ;PROCESS ALL BATCH TYPE TO MAIL MAN
 S M=0 F J=1:1 S M=$O(^TMP("PRCFBTCH",$J,N,M)) Q:M=""  D:"3,1,4,2,9,10,12"[N SHRINK D HEADER,CREATE
 Q
CREATE ;CREATES MESSAGE FOR INDIVIDUAL BATCH
 ;ENTER THEM INTO MAIL MAN MESSAGES
 ;I '$D(DUZ) S DUZ=.5
 ;I DUZ="" S DUZ=.5
 Q:'$D(^PRCF(423.9,PTYP,0))  S:$P(^(0),"^",2)]"" @("XMY("_""""_$P(^(0),U,2)_""""_")=""""") S:$G(PRCFA("EDI"))]"" XMY(PRCFA("EDI"))="" K PRCFA("EDI") D
 .Q:'$D(^PRCF(423.9,PTYP,1,0))  D
 ..S L=0 F K=1:1 S L=$O(^PRCF(423.9,PTYP,1,L)) Q:L'=+L  I $D(^PRCF(423.9,PTYP,1,L,0)) S ADD=$P(^(0),"^",1) S XMY(ADD)=""
 S XMDUZ=DUZ,XMSUB="FEE/FEN/LOG/ISM/EDI BATCH "_MM,XMTEXT="^TMP(""PRCFBTCH"","_$J_","_N_","""_M_""","
 D XMD
 I $D(M),M["" S X=$O(^PRCF(421.2,"B",M,0)) Q:X=""
 S:$D(^PRCF(421.2,X,0)) $P(^(0),"^",12)=XMZ,^PRCF(421.2,"D",XMZ,X)="" Q
XMD N I,J,M,N D ^XMD Q
BLIST ;PLACE ALL CODE SHEETS IN A BATCH ON TRANSMISSION LIST
 I $D(^PRCF(423,"AD",PBAT)) S N=0 F I=1:1 S N=$O(^PRCF(423,"AD",PBAT,N)) Q:N'=+N  S ^PRCF(423,"AK",PRCFKEY,N)="",$P(^PRCF(423,N,"TRANS"),"^",11)=PRCFKEY
 Q:+PBATN'>0
 S DA=PBATN
 I '$D(PRC("PER")) D DUZ^PRCFSITE Q:'%
 S:$D(P) PX=P
 D NOW^%DTC
 S XDT=%
 S X1=$P(PRC("PER"),"^",2)
 S $P(^PRCF(421.2,DA,0),"^",4)=XDT
 K XDT
 S MESSAGE=""
 I PRCFRT=0 D ENCODE^PRCFAES1(DA,DUZ,.MESSAGE)
 I PRCFRT=3 D ENCODE^PRCFAES2(DA,DUZ,.MESSAGE)
 K MESSAGE
 K P I $D(PX) S P=PX K PX Q
 Q
SHRINK ;TAKE 4th '-' PIECE OF BATCH NUMBER AND MAKE IT INTO MMCCC
 ;  WHERE MM = MONTH
 ;       CCC = LAST 3 DIGITS OF COUNTER VALUE
 N SHRINK,SHRINK1,SHRINK2
 S SHRINK=$G(^TMP("PRCFBTCH",$J,N,M,1,0)) Q:SHRINK=""  I $P(SHRINK,".",3)=999 S SHRINK1=$P(SHRINK,".",6),SHRINK2=$E(SHRINK1,1,2)_$E(SHRINK1,$L(SHRINK1)-2,99),$P(SHRINK,".",6)=SHRINK2,^TMP("PRCFBTCH",$J,N,M,1,0)=SHRINK
 Q
HEADER ;DO THE SAME THING TO THE MESSAGE HEADER AS 'SHRINK' DOES TO THE BATCH NUMBER.
 N M1,M2
 S M1=$P(M,"-",4),M2=$E(M1,1,2)_$E(M1,$L(M1)-2,99),MM=$P(M,"-",1,3)_"-"_M2 Q
