SRNTEG01 ;ISC/XTSUMBLD KERNEL - Package checksum checker ;JUN 24, 1993@10:46:22
 ;;3.0; Surgery ;;24 Jun 93
 ;;7.0;JUN 24, 1993@10:46:22
 S XT4="I 1",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
CONT F XT1=1:1 S XT2=$T(ROU+XT1) Q:XT2=""  S X=$P(XT2," ",1),XT3=$P(XT2,";",3) X XT4 I $T W !,X X ^%ZOSF("TEST") S:'$T XT3=0 X:XT3 ^%ZOSF("RSUM") W ?10,$S('XT3:"Routine not in UCI",XT3'=Y:"Calculated "_$C(7)_Y_", off by "_(Y-XT3),1:"ok")
 ;
 K %1,%2,%3,X,Y,XT1,XT2,XT3,XT4 Q
ONE S XT4="I $D(^UTILITY($J,X))",X=$T(+3) W !!,"Checksum routine created on ",$P(X,";",4)," by KERNEL V",$P(X,";",3),!
 W !,"Check a subset of routines:" K ^UTILITY($J) X ^%ZOSF("RSEL")
 W ! G CONT
ROU ;;
SRORUT2 ;;4358601
SROSCH ;;11118862
SROSCH1 ;;13049307
SROSCH2 ;;3456014
SROSNR ;;6401070
SROSNR1 ;;13500950
SROSNR2 ;;13333561
SROSPC1 ;;9128609
SROSPEC ;;10943509
SROSPSS ;;11611626
SROSRPT ;;4622183
SROSRPT0 ;;14262043
SROSRPT1 ;;8082515
SROSRPT2 ;;3664190
SROSTAFF ;;7478492
SROSTOP ;;482543
SROSUR ;;13568496
SROSUR1 ;;5491300
SROSUR2 ;;15261504
SROTHER ;;10201414
SROTRIG ;;311968
SROTRPT ;;3090334
SROTRPT0 ;;8629917
SROUNV ;;6411021
SROUNV1 ;;7501677
SROUNV2 ;;8120061
SROUTED ;;3787421
SROUTIN ;;3810199
SROUTR1 ;;3217649
SROUTRN ;;14264130
SROUTUP ;;7303556
SROVAR ;;9741521
SROVER ;;13869821
SROVER1 ;;3141116
SROWC ;;14190229
SROWC1 ;;8388648
SROWC2 ;;8056835
SROWC3 ;;5727509
SROWL ;;9142245
SROWL0 ;;9908168
SROWRQ ;;4757977
SROWRQ1 ;;15007155
SROXPR ;;679930
SROXR1 ;;7170960
SROXR2 ;;7620900
SROXR4 ;;2897681
SROXREF ;;3015909
SROXRET ;;8369518
SRSAVG ;;4834230
SRSAVL ;;15393182
SRSAVL1 ;;9422142
SRSBD1 ;;1173034
SRSBDEL ;;16592681
SRSBLOK ;;4253560
SRSBOUT ;;17702043
SRSBTCH ;;8763533
SRSCAN ;;8345930
SRSCAN0 ;;8834749
SRSCAN1 ;;5546303
SRSCAN2 ;;6743158
SRSCD ;;14561257
SRSCDS ;;13118595
SRSCDS1 ;;10523262
SRSCDW ;;10798685
SRSCDW1 ;;10448602
SRSCG ;;2499341
SRSCHAP ;;9897352
SRSCHC ;;6772693
SRSCHC1 ;;9657008
SRSCHC2 ;;1244432
SRSCHCA ;;6623794
SRSCHCC ;;7655893
SRSCHD ;;6991751
SRSCHD1 ;;6353592
SRSCHD2 ;;7812904
SRSCHDA ;;12107557
SRSCHDC ;;14546169
SRSCHK ;;3180574
SRSCHOR ;;5237814
SRSCHUN ;;12683269
SRSCHUN1 ;;8264095
SRSCHUP ;;5762136
SRSCONR ;;11907499
SRSCPT ;;13571536
SRSCPT1 ;;11796404
SRSCPT2 ;;12903856
SRSCRAP ;;6565050
SRSDIS1 ;;7839099
SRSDISP ;;8541868
SRSDT ;;5989686
SRSEND ;;5442128
SRSGRPH ;;7578905
SRSIND ;;3376323
SRSKILL ;;16166502
SRSKILL1 ;;18934960
SRSKILL2 ;;8133718
SRSLOOK ;;10181844
SRSLOOK1 ;;10175389
SRSMREQ ;;2985467
SRSPUT0 ;;11569524
SRSPUT1 ;;6335796
SRSPUT2 ;;4332219
SRSRBS ;;14156393
SRSRBS1 ;;11801894
SRSRBW ;;12165414
SRSRBW1 ;;11402555
SRSREQ ;;10962213
SRSREQUT ;;7874591
SRSRQST ;;11717709
SRSRQST1 ;;8197996
SRSTIME ;;5873261
SRSTR ;;9069898
SRSTRAN ;;2511510
SRSUP1 ;;16576534
SRSUPC ;;5463059
SRSUPRG ;;4038321
SRSUPRQ ;;17501885
SRSUTIN ;;3648628
SRSUTL ;;5914534
SRSUTL2 ;;5689683
SRSWL ;;7992277
SRSWL1 ;;7124388
SRSWL10 ;;8138161
SRSWL11 ;;12717710
SRSWL12 ;;10265744
SRSWL13 ;;16005965
SRSWL14 ;;7829112
SRSWL15 ;;13775925
SRSWL2 ;;6295678
SRSWL3 ;;12061766
SRSWL4 ;;11132191
SRSWL5 ;;2024493
SRSWL6 ;;8860633
SRSWL7 ;;14153033
SRSWL8 ;;7566212
SRSWL9 ;;12279693
SRSWLST ;;16065493
SRSWREQ ;;8111469
