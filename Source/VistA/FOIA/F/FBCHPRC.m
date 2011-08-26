FBCHPRC ;AISC/DMK-PRINT ROC FOR CONTRACT HOSPITAL ;15AUG90
 ;;3.5;FEE BASIS;;JAN 30, 1995
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
ROC S DIC="^FBAA(161.5,",DIC(0)="AEQM",D="D",DIC("A")="Select Veteran: ",DIC("W")="W ?30,$S($D(^FBAAV($P(^(0),U,2),0)):$P(^(0),U),1:"""")" D IX^DIC K D,DIC G END:X=""!(X="^"),ROC:Y<0 S FBIFN=+Y
EN S VAR="FBIFN",VAL=FBIFN,PGM="START^FBCHPRC" D ZIS^FBAAUTL G END:FBPOP
START U IO W:$E(IOST,1,2)["C-" @IOF S L="|",Q="",$P(Q,"-",80)="-",FB("PG")=1
 G END:'$D(^FBAA(161.5,FBIFN,0)) S FB(0)=^(0),FB1(0)="" I $D(^FBAA(161.5,FBIFN,1)) S FB1(0)=^(1)
 F J=1:1:14 S FB(J)=$P(FB(0),"^",J)
 F I=1:1:4 S FB(J+I)=$P(FB1(0),"^",I)
 S FB(J+5)=$P(FB1(0),"^",7)
 S DFN=FB(3),VAPA("P")="" D 6^VADPT,SITEP^FBAAUTL S FBSITE=$P(FBSITE(0),"^"),Y=FB(4) D DATE S FB(4)=$E(Y,1,17)
 S FB(17)=$S(FB(17)="":"Unknown",$D(^DGBT(392.4,FB(17),0)):$P(^(0),"^"),1:"Unknown")
 I FB(5)]"" S FB(5)=$$DATX^FBAAUTL(FB(5))
 I FB(19)]"" S FB(19)=$$DATX^FBAAUTL(FB(19))
 D VEN,^FBCHPRC1
END K DIWF,DIWL,BT,BOT,DIWR,DFN,FB1,FB,FBI,FBIFN,FBRR,FBSITE,FBVEN,I,J,L,PGM,Q,VA,VADM,VAEL,VAERR,VAL,VAR,VAPA,X,Y,Z D CLOSE^FBAAUTL
 Q
VEN ;GET VENDOR DEMOGRAPHICS
 S FBVEN(0)=$S(FB(2)="":"",$D(^FBAAV(FB(2),0)):^(0),1:"") I FBVEN(0)="" S FBVEN="Unknown" Q
 S FBVEN(6)=$S($D(^FBAAV(FB(2),1)):$P(^(1),"^"),1:""),FBVEN=$P(FBVEN(0),"^")
 S FBVEN(1)=$P(FBVEN(0),"^",3),FBVEN(2)=$P(FBVEN(0),"^",14),FBVEN(3)=$P(FBVEN(0),"^",4)
 S FBVEN(4)=$S($P(FBVEN(0),"^",5)']"":"Unknown",$D(^DIC(5,$P(FBVEN(0),"^",5),0)):$P(^(0),"^"),1:"Unknown")
 S FBVEN(5)=$P(FBVEN(0),"^",6)
 K FBVEN(0) Q
USER ;GET USER IN FILE 200
 S FB("USER")=$S(FB("DUZ")="":"Unknown",$D(^VA(200,FB("DUZ"),0)):$P(^(0),"^"),1:"Unknown")
 Q
RPTC ;RETRIEVE DATE,USER AND NARRATIVE OF ROC
 S DIWL=7,DIWR=74,DIWF="W"
 F FBI=0:0 S FBI=$O(^FBAA(161.5,FBIFN,2,FBI)) Q:FBI'>0  I $D(^FBAA(161.5,FBIFN,2,FBI,0)) S FB("DATE")=$P(^(0),"^"),FB("DUZ")=$P(^(0),"^",2) D GETNAR
 Q
GETNAR K ^UTILITY($J,"W") S Y=FB("DATE") D DATE S FB("DATE")=Y D USER
 W !,"DATE: ",FB("DATE"),?53,"USER: ",$E(FB("USER"),1,22)
 F FBRR=0:0 S FBRR=$O(^FBAA(161.5,FBIFN,2,FBI,1,FBRR)) Q:FBRR'>0  S FBXX=^(FBRR,0),X=FBXX D ^DIWP
 D ^DIWW:$D(FBXX) K FBXX
 I $Y+11>IOSL S FB("PG")=FB("PG")+1 W @IOF,!,?70,"Page ",FB("PG"),!!?25,"REPORT OF CONTACT CONTINUED",!,?24,$E(Q,1,29),!,?1,"For: ",VADM(1),!,Q,!
 Q
DATE X ^DD("DD") Q
