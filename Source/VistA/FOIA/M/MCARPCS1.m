MCARPCS1 ;WISC/TJK-AUTO TRANSMIT PACEMAKER REPORT-LOAD 1 ;5/8/96  14:08
 ;;2.3;Medicine;**5**;09/13/1996
 G BEGIN
STORE S MCLN=$E($P(MCLN1,U)_"                                        ",1,40)_MCLN2
STORE1 S ^TMP("MCAR","PACE",$J,MCLNCT)=MCLN,MCLNCT=MCLNCT+1 Q
CENTER S MCLN=$E(MCBL,1,80-$L(Z)/2)_Z D STORE1 Q
DA F J=0:0 S J=$O(^MCAR(690,"AC",DFN,J)) Q:J=""  I $D(^MCAR(690,"AC",DFN,J,MCDIC)) S MCV(I)=$O(^(MCDIC,0)) Q
 Q
BEGIN K ^TMP("MCAR","PACE",$J),MCV F I=698:.1:698.3 S MCV(I)="",MCDIC="MCAR("_I D DA
 S MCG=MCV(698),MCVL=MCV(698.1),MCAL=MCV(698.2),MCS=MCV(698.3) K MCV
SETDATA K ^TMP("MCAR","PACE",$J)
 S $P(MCDSH,"-",81)=""
 S $P(MCBL," ",81)=""
 S MCLNCT=1,Z="PACEMAKER CENTER REPORT" D CENTER
 S Z=^DD("SITE") D CENTER
 K Z S MCLN=MCDSH D STORE1
 S Z="REGISTRATION FORM DATA" D CENTER
 K Z S MCLN=MCDSH D STORE1
 S MCLN=MCBL D STORE1
 G SETDATA2:'$D(MCR) S X=$O(MCR("")) S MCLN=" REASON(S) FOR REPORT: "_X D STORE1 F J=1:1 S X=$O(MCR(X)) Q:X=""  S MCLN="                       "_X D STORE1
 S MCLN=MCBL D STORE1
SETDATA2 S MCLN="TELEPHONE FOLLOW-UP PROVIDED BY: ",X=""
 S:$D(^MCAR(690,DFN,"P2")) X=$P(^("P2"),U,2),X=$S(X="E":"EASTERN PACEMAKER SURVEILLANCE CENTER",X="W":"WESTERN PACEMAKER SURVEILLANCE CENTER",X="L":"LOCAL VAMC",1:"")
 S MCLN=MCLN_X D STORE1
 S MCLN=MCBL D STORE1
 ; -------------------
 ; DOB = External Format of the patients Birthdate.
 ; SEX = External Format of the patients sex.
 ; -------------------
 D DEM^VADPT S MCARNM=VADM(1),SSN=VADM(2),DOB=$P(VADM(3),U,2),SEX=$P(VADM(5),U,2) D KVAR^VADPT
 F I=0,1,4 S MCG(I)=$S($D(^MCAR(698,MCG,I)):^(I),1:"")
 S MCLN="SSN: "_$P(SSN,"^",2) D STORE1
 S MCLN1="  NAME: "_MCARNM,MCLN2="PULSE GENERATOR" D STORE
 S MCLN1="",MCLN2=$E(MCDSH,1,$L("PULSE GENERATOR")) D STORE
 D ADD^VADPT S MCLN2=$P(MCG(0),U,4),MCLN2=$P($G(^MCAR(698.6,+MCLN2,0)),U),MCLN2="MFR: "_MCLN2,MCLN1="        "_VAPA(1) D STORE
 S MCLN1="        "_VAPA(2),MCLN2=$P(MCG(0),U,3),MCLN2=$P($G(^MCAR(698.4,+MCLN2,0)),U),MCLN2="MODEL: "_MCLN2 D STORE
 S MCLN1="        "_VAPA(3),MCLN2="S/N: "_$P(MCG(0),U,5) D STORE
 S MCLN1="        "_VAPA(4)_", "_$P(VAPA(5),U,2)_" "_VAPA(6),Y=$P(MCG(0),U) X ^DD("DD") S MCLN2="DATE: "_$P(Y,"@") S MCTEL=VAPA(8) D KVAR^VADPT D STORE
 S MCLN1="  DOB:  "_DOB,MCLN2="BEGINNING OF LIFE MAGNET RATE: "_$P(MCG(4),U,2) D STORE
 S MCLN1="  SEX:  "_SEX,MCLN2="END OF LIFE MAGNET RATE: "_$P(MCG(4),U,6) D STORE
 S MCLN1="TELEPHONES:",MCLN2="IMPLANTING HOSPITAL:" D STORE
 ;S MCLN1="HOME:   "_MCTEL,MCLN2="" S:$P(MCG(0),U,8) MCLN2=$P(MCG(0),U,8) S:$D(^DIC(4,MCLN2)) MCLN2=$P(^(MCLN2,0),U) D STORE
 S MCLN1="HOME:   "_MCTEL,MCLN2=$P($G(^DIC(4,+$P(MCG(0),U,8),0)),U) D STORE
 K MCTEL S VAOA("A")=5 D OAD^VADPT S MCTEL=VAOA(8) D KVAR^VADPT
 S (MCLN2,Y)="" S:$D(^MCAR(690,DFN,"P3")) Y=$P(^("P3"),U,6) I Y X ^DD("DD") S MCLN2=$P(Y,"@",1) K Y
 S MCLN1="WORK: "_MCTEL,MCLN2="DATE OF INITIAL IMPLANT: "_MCLN2 D STORE
 S MCLN=MCBL D STORE1
 S MCLN1="RESPONSIBLE PHYSICIAN:" S Y=$P(MCG(0),U,14) X ^DD("DD") S MCLN2="LAST PREVIOUS IMPLANT: "_Y D STORE
 N MCPHYS S DIC="^DPT(",DA=DFN,DIQ(0)="IE",DIQ="MCPHYS(",DR=.104 D EN^DIQ1
 I $D(MCPHYS(2,DFN,.104,"I")) S MCPHYS=MCPHYS(2,DFN,.104,"I")_U_MCPHYS(2,DFN,.104,"E")
 K DIC,DR,DA,MCPHYS(2),DIQ,^UTILITY("DIQ1",$J)
 S MCLN1=$P($G(MCPHYS),U,2),MCLN2="PULSE GENERATORS INCLUDING PRESENT: "_$P(MCG(0),U,13) D STORE
 S MCLN="PHONE: " I $G(MCPHYS) S MCLN=MCLN_$$GETVALUE^MCU(200,+MCPHYS,.131)
 D STORE1
 S MCLN=MCBL D STORE1
 G ^MCARPCS2
