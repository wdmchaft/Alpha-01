NURARPC3 ;HIRMFO/MD,FT-CONTINUATION OF 1106 ACUITY REPORT DRIVER ;3/19/98  13:12
 ;;4.0;NURSING SERVICE;**1,9****;Apr 25, 1997
 S U="^" I +$G(NDATED)?7N S NURZ=+NDATED_" 0",NURMDSW(3)=0 F  S NURZ=$O(^NURSA(213.4,"B",NURZ)) Q:$E(NURZ,1,7)'>0!($E(NURZ,1,7)>+$P(NDATED,U,2))  S NDA=$O(^NURSA(213.4,"B",NURZ,0)) W:$E(IOST)="C"&'$R(30) "." D SORT
 I $D(NDATED) S (ZX,ZY)="" D
 .I NDATED["MT" S ZX=$E(NDATED,1,5)_"00",ZY=$E(NDATED,1,5)_"31"
 .I NDATED?3N S ZX=(NDATED-1)_"1000",ZY=NDATED_"0930"
 .I NDATED["Q" S (ZX,ZY)=+$E(NDATED,1,3),NURZ=$E(NDATED,7) S:NURZ=1 ZX=ZX-1,ZY=ZY-1 S ZX=ZX_$S(NURZ=1:"1000",NURZ=2:"0100",NURZ=3:"0400",1:"0700"),ZY=ZY_$S(NURZ=1:"1231",NURZ=2:"0331",NURZ=3:"0630",1:"0930")
 .I ZX="" Q
 .S NURZ=ZX,NURMDSW(3)=1 F  S NURZ=$O(^NURSA(213.4,"AB",NURZ)) Q:NURZ=""!(NURZ>ZY)  F NDA=0:0 S NDA=$O(^NURSA(213.4,"AB",NURZ,NDA)) Q:NDA'>0  W:$E(IOST)="C"&'$R(30) "." D PERSORT^NURARPC4
 .Q
 I $O(^TMP($J,""))="" S NURFAC(2)=$S($G(NURFAC)=0:NURFAC(1),1:"") D HEADER^NURARPC2 S NUROUT=1 W !!," THERE IS NO DATA FOR "_$S($G(NHOSPSW):"THIS REPORT ",1:$G(NURSWARD(0))) D:$E(IOST)="C" RERUNRPT Q
 S (NTCEN,DTCEN,NFCEN,NFTCEN)=0 F X=1:1:5 S (NTC(X),DTC(X),NFC(X),NFTC(X))=0
 D:'NURMDSW(3) ^NURARPC2
 D:NURMDSW(3) PERRPT^NURARPC4
 I 'NUROUT,NHOSPSW,NURMDSW,+$G(NURFAC),$O(^TMP("NURBDSM",$J,""))'="" D
 .  D HEADER^NURARPC2,HEADER1^NURARPC2
 .  W !!,?35,"MULTI-DIVISIONAL SUMMARY"
 .  S NBED="" F  S NBED=$O(^TMP("NURBDSM",$J,NBED)) Q:NBED=""  D
 .  .  I $Y>(IOSL-6) D HEADER^NURARPC2,HEADER1^NURARPC2 Q:NUROUT  W !!,?35,"MULTI-DIVISIONAL SUMMARY"
 .  .  S NDATA=$G(^TMP("NURBDSM",$J,NBED)),NDATA(1)=($P(NDATA,U)+$P(NDATA,U,2)+$P(NDATA,U,3)+$P(NDATA,U,4)+$P(NDATA,U,5))
 .  .  W !!,NBED,?33,$J($P(NDATA,U),4),?41,$J($P(NDATA,U,2),4),?49,$J($P(NDATA,U,3),4),?57,$J($P(NDATA,U,4),4),?65,$J($P(NDATA,U,5),4),?73,$J(NDATA(1),4)
 .  .  Q
 . Q
 I NHOSPSW D:$Y>(IOSL-6)&'NUROUT HEADER^NURARPC2,HEADER1^NURARPC2 D
 .  W !,?33,"----    ----    ----    ----    ----    ----"
 .  W !,"REPORT TOTAL",?33,$J(NFC(1),4),?41,$J(NFC(2),4),?49,$J(NFC(3),4),?57,$J(NFC(4),4),?65,$J(NFC(5),4),?73,$J(NFCEN,4)
 .  Q
 D ^%ZISC
 D:$E(IOST)="C"&'NUROUT RERUNRPT
 K NDATED,NURFAC Q
SORT ;
 Q:+$$NOVALU^NURARPC1(NDA)'>0
 I NURTYPE=0,'($E($P(^NURSA(213.4,NDA,0),U),8)="D") Q
 I NURTYPE=1,'($E($P(^NURSA(213.4,NDA,0),U),8)="E") Q
 S:'NURMDSW!'(NHOSPSW) NURFAC(2)=" BLANK" S YY("W")=$E($P(^NURSA(213.4,NDA,0),U),9,99)
 I NHOSPSW,$G(NURFAC(2))'=" BLANK" S NURFAC(2)=$$EN12^NURSUT3($G(YY("W"))) Q:$G(NURFAC(2))=""
 I NHOSPSW,NURMDSW,'$G(NURFAC),$G(NURFAC(1))'=$G(NURFAC(2)) Q
 K NBED F D1=0:0 S D1=$O(^NURSA(213.4,NDA,1,D1)) Q:D1'>0  I $D(^NURSA(213.4,NDA,1,D1,0)) S YY("B")=$P(^(0),U) D A
 Q
A I NHOSPSW,NURSTYPE="U" S NPWARD=YY("W") D EN6^NURSAUTL S F1=$S(NPWARD="":"",1:NPWARD),F2=$S(YY("B")="":"  BLANK",$D(^NURSF(213.3,YY("B"),0)):$P(^(0),U),1:"  BLANK") G SET
 I NHOSPSW,(YY("B")=NBDSECT!'NBDSECT) S NPWARD=YY("W") D EN6^NURSAUTL S F2=$S(NPWARD="":"  BLANK",1:NPWARD),F1=$S(YY("B")="":"  BLANK",$D(^NURSF(213.3,YY("B"),0)):$P(^(0),"^"),1:"  BLANK") G SET
 I 'NHOSPSW,'NBDSECT,YY("W")=NURSWARD S F1=$S(NURSWARD(0)="":"  BLANK",1:NURSWARD(0)),F2=$S(YY("B")="":"  BLANK",$D(^NURSF(213.3,YY("B"),0)):$P(^(0),U),1:"  BLANK") G SET
 I 'NHOSPSW,YY("B")=NBDSECT,YY("W")=NURSWARD S F1=$S(YY("B")="":"  BLANK",$D(^NURSF(213.3,YY("B"),0)):$P(^(0),U),1:"  BLANK"),F2=$S(NURSWARD(0)="":"  BLANK",1:NURSWARD(0)) G SET
 Q
SET ; BUILD TMP GLOBAL WITH SELECTED DAILY DATA
 S NBED(D1)=^NURSA(213.4,NDA,1,D1,0),NBED("BEDSEC")=$S($P($G(^NURSF(213.3,+NBED(D1),0)),U)'="":$P($G(^(0)),U),1:"  BLANK")_U_$P(NBED(D1),U,2,6)
 I '$D(^TMP($J,$E(NURZ,1,7),NURFAC(2),F1,F2)) S ^TMP($J,$E(NURZ,1,7),NURFAC(2),F1,F2)="0^0^0^0^0"
 I NURMDSW,NHOSPSW,+$G(NURFAC),$P(NBED("BEDSEC"),U)'="" D
 .  S:'$D(^TMP("NURBDSM",$J,$P(NBED("BEDSEC"),U))) ^($P(NBED("BEDSEC"),U))="0^0^0^0^0"
 .  F Z=2:1:6 S $P(^TMP("NURBDSM",$J,$P(NBED("BEDSEC"),U)),U,(Z-1))=($P(^($P(NBED("BEDSEC"),U)),U,(Z-1))+$P(NBED("BEDSEC"),U,Z))
 .  Q
 F Y=2:1:6 S $P(^TMP($J,$E(NURZ,1,7),NURFAC(2),F1,F2),U,(Y-1))=$P(NBED(D1),U,Y)
 Q
RERUNRPT ;
 S NURSUMSW=0 R !!,"Would you like to run another report? NO//",X:DTIME
 S X=$$UP^XLFSTR(X) I (X?1"N".E)!("^"[X)!('$T) S NUROUT=1 Q
 I X?1"Y".E S NUROUT=0 Q
 W !,"ANSWER YES OR NO" G RERUNRPT
 Q
