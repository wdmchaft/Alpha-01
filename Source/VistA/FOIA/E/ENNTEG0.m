ENNTEG0 ;ISC/XTSUMBLD KERNEL - Package checksum checker ;AUG 20, 1993@12:21:45
 ;;0.0;
 ;;7.0;AUG 20, 1993@12:21:45
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
ENPRP5 ;;4722697
ENPRP6 ;;4048427
ENSA ;;11447988
ENSA1 ;;8550568
ENSA2 ;;6452411
ENSA3 ;;3081886
ENSA4 ;;3930763
ENSA5 ;;3766253
ENSA6 ;;9844961
ENSA7 ;;5152701
ENSA8 ;;6386422
ENSA9 ;;2426249
ENSED ;;10826817
ENSED0 ;;10994649
ENSED1 ;;10278119
ENSED2 ;;6143698
ENSP ;;10756230
ENSP1 ;;4125461
ENSP2 ;;8562631
ENSP3 ;;9498285
ENSP4 ;;498030
ENSP5 ;;11405213
ENSP6 ;;3630823
ENTEXT ;;472579
ENWARD ;;2063104
ENWARD1 ;;4564207
ENWARD2 ;;9954927
ENWO ;;4164468
ENWO1 ;;11416183
ENWO2 ;;4003083
ENWOCOMP ;;1685086
ENWOD ;;6378024
ENWOD1 ;;7389349
ENWOD2 ;;6299337
ENWOD3 ;;1767450
ENWOINV ;;3578275
ENWONEW ;;7625505
ENWONEW1 ;;3983180
ENWONEW2 ;;447190
ENWOP ;;6698439
ENWOP1 ;;1918383
ENWOP2 ;;4032241
ENWOP3 ;;3561674
ENWOREP ;;5580821
ENWOST ;;6930407
