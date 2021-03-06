NURCVED2 ;HIRMFO/MD,YH-VITAL SIGNS EDIT SHORT FORM ;2/6/99
 ;;4.0;NURSING SERVICE;**23,25**;Apr 25, 1997
EN1 ;ENTRY FROM OPTION NURCPE-VIT TPR
 S GMRSTR="T;P;R;PN;",GMRENTY=1 D EN1^NURCVED0
 G QUIT
EN2 ;ENTRY FROM OPTION NURCPE-VIT TPR B/P
 S GMRSTR="T;P;R;BP;PN",GMRENTY=2 D EN1^NURCVED0
 G QUIT
EN3 ;ENTRY FROM OPTION NURCPE-VIT PULSE RADIAL
 S GMRSTR="P;",GMRENTY=7 D EN1^NURCVED0
 G QUIT
EN4 ;ENTRY FROM OPTION NURCPE-VIT TPRBW
 S GMRSTR="T;P;R;BP;WT;PN;",GMRENTY=4 D EN1^NURCVED0
 G QUIT
EN5 ;ENTRY FROM OPTION NURCPE-VIT ADMISSION VM
 S GMRSTR="T;P;R;BP;HT;WT;PN;",GMRENTY=3 D EN1^NURCVED0
 G QUIT
EN6 ;ENTRY FROM OPTION NURCPE-VIT WEIGHT
 S GMRSTR="WT;",GMRENTY=8 D EN1^NURCVED0
 G QUIT
EN7 ;ENTRY FROM OPTION NURCPE-VIT CHANGE V/M D/T
 S NURQUIT=0 D DATE^NURCVED0 I NURQUIT W !,$C(7),"Parameters not changed!!"
 E  S $P(NURSDBA,"^",2)=GMRVIDT
QUIT K G,GBLNK,GCAT,GCHA,GCHART,GCOL,GCOUNT,GFLAG,GLABEL,GLINE,GLN,GLVL,GMAX,GMIN,GMRENTR,GMRINF,GMRLAST,GMRO2,GMRSTAR,GMRVDFLT,GMRVIDT,GMRVLST,GMRVODR,GMRW,GNUROP,GORDER,GQUAL,GREASON,GSIDE,GTXT
 K ND1,NDA,NORM,NRMBD,NURI,NURLEN,NURQUIT,NURRMST,NURSX,NURSY,NWLOC
 K VAERR,VAIPT,VAROOT
 Q
EN8 ;ENTRY FROM OPTION NURCPE-VIT TPR EXT B/P
 S GMRSTR="T;P;R;BP;",GMRENTY=5 D EN1^NURCVED0 G QUIT
EN9 ;ENTRY FROM OPTION NURCPE-VIT EXT B/P
 S GMRSTR="BP;P;",GMRENTY=6 D EN1^NURCVED0 G QUIT
EN10 ;ENTRY FROM OPTION NURCPE-VIT HTWT
 S GMRSTR="HT;WT;",GMRENTY=0 D EN1^NURCVED0 G QUIT
EN11 ;ENTRY FROM OPTION NURCPE-VIT CIRCUMF/GIRTH
 S GMRSTR="CG;",GMRENTY=19 D EN1^NURCVED0 G QUIT
EN12 ;ENTRY FROM OPTION NURCPE-VIT CVP
 S GMRSTR="CVP;",GMRENTY=21 D EN1^NURCVED0 G QUIT
EN13 ;ENTRY FROM OPTION NURCPE-VIT O2SATURATION
 S GMRSTR="PO2;",GMRENTY=20 D EN1^NURCVED0 G QUIT
EN14 ;ENTRY FROM OPTION NURCPE-VIT VMCONFIG
 S GMRENTY=9 D EN1^NURCVED0 G QUIT
EN15 ;ENTRY FROM OPTION NURCPE-VIT PAIN
 S GMRSTR="PN;",GMRENTY=18 D EN1^NURCVED0 G QUIT
DATE ;
 S NURQUIT=0 D DATE^NURCVED0 S:'NURQUIT NURSDBA=NURQUIT_"^"_GMRVIDT
 Q
EXITACT ; NURSING VITAL OPTIONS EXIT ACTION
 K:'$L(NURFLAG) NURFLAG,NURSDBA
 Q
ENTACT ; NURSING VITAL OPTIONS ENTRY ACTION
 S:'$D(NURFLAG) NURFLAG=2 I NURFLAG S NURFLAG=$S(NURFLAG=1:0,1:"") D DATE I NURQUIT K NURQUIT,GMRVIDT,NURFLAG,NURSDBA S XQUIT=1
 Q
