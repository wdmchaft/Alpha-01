DINTEG4 ;SFISC/dizSUMB FILEMAN-FileMan checksum checker ;MAR 30, 1999  13:20
 ;;22.0;VA FileMan;;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 S DIZ4="I 1" D DSP,INI
CONT F DIZ1=1:1 S DIZ2=$T(ROU+DIZ1) Q:DIZ2=""  S X=$P(DIZ2," ",1),DIZ3=$P(DIZ2,";",3) X DIZ4 I $T W !,X X DIZTEST W:'$T ?28,DIZ6 S:'$T DIZ3=0 X:DIZ3 DIZSUM W ?10,$S('DIZ3:"",DIZ3'=Y:$C(7)_"Calculated "_Y_", off by "_(Y-DIZ3),1:"ok")
 G CONT^DINTEG5
 S X="" F  S X=$O(^UTILITY($J,X)) Q:X=""  W !,X,?10,"not a routine in this INTEGRITY checker"
 K D,D1,D2,D3,X,Y,DIZ,DIZ1,DIZ2,DIZ3,DIZ4,DIZ5,DIZ6,DIZTEST,DIZSUM,DISYS,DIZSEL,^UTILITY($J) Q
ONE D INI S DIZSEL=$S($D(^%ZOSF("RSEL")):^("RSEL"),1:"F  S DIR(0)=""FO^1:8"",DIR(""A"")=""ROUTINE NAME"" D ^DIR Q:$D(DIRUT)  X DIZTEST W:'$T ?28,DIZ6 I $T S ^UTILITY($J,Y)=""""")
 S DIZ4="I $D(^UTILITY($J,X)) K ^(X)" D DSP
 W !,"Check a subset of routines:" K ^UTILITY($J) X DIZSEL
 W ! G CONT
DSP S X=$T(+2) W !!,"Checksum routine created on "_$P(X,";",6)_" by "_$P(X,";",4)_" V"_$P(X,";",3) Q
INI K ^UTILITY($J) D OS^DII S DIZTEST=$S($D(^DD("OS",DISYS,18)):^(18),1:"I $T(^@X)]"""""),DIZ5="",DIZ6=$C(7)_"Routine not in UCI"
 S DIZSUM="ZL @X S Y=0 F D=1,3:1 S D1=$T(+D),D3=$F(D1,"" "") Q:'D3  S D3=$S($E(D1,D3)'="";"":$L(D1),$E(D1,D3+1)="";"":$L(D1),1:D3-2) F D2=1:1:D3 S Y=$A(D1,D2)*D2+Y" Q
ROU ;;
DINIT270 ;;8954842
DINIT271 ;;4962636
DINIT27A ;;4535134
DINIT27B ;;3392667
DINIT27C ;;3010708
DINIT27D ;;3129310
DINIT27E ;;2362322
DINIT27F ;;7294806
DINIT27G ;;7287275
DINIT27H ;;991763
DINIT27I ;;1784973
DINIT27J ;;4891073
DINIT27K ;;4910854
DINIT28 ;;2224020
DINIT285 ;;9217149
DINIT286 ;;2757795
DINIT287 ;;939077
DINIT290 ;;12627486
DINIT291 ;;12341008
DINIT292 ;;16217546
DINIT293 ;;12549234
DINIT294 ;;10659059
DINIT295 ;;14571752
DINIT296 ;;15350237
DINIT297 ;;14629146
DINIT298 ;;13172471
DINIT299 ;;4171222
DINIT29P ;;1227629
DINIT2A0 ;;14158575
DINIT2A1 ;;13956613
DINIT2A2 ;;12584241
DINIT2A3 ;;14285423
DINIT2A4 ;;2212843
DINIT2A5 ;;13007561
DINIT2A6 ;;1584040
DINIT2AA ;;11408392
DINIT2AB ;;1806296
DINIT2AC ;;584349
DINIT2B0 ;;2944946
DINIT2B1 ;;1938633
DINIT2B2 ;;4698975
DINIT2B3 ;;9453897
DINIT2B4 ;;2674007
DINIT2B5 ;;3829788
DINIT2B6 ;;3933969
DINIT2B7 ;;7974587
DINIT2B8 ;;4876657
DINIT2B9 ;;9536397
DINIT2BA ;;4224327
DINIT2BB ;;7365189
DINIT2BC ;;3580721
DINIT2BD ;;1744928
DINIT2BE ;;4515155
DINIT2C0 ;;10313096
DINIT3 ;;10198775
DINIT4 ;;9010496
DINIT41 ;;11669306
DINIT42 ;;8202093
DINIT5 ;;9815153
DINIT6 ;;5517845
DINITPST ;;230107
DINV1DTM ;;1336349
DINV1VXD ;;2355845
DINVDTM ;;5506361
DINVMSM ;;9845770
DINVONT ;;5190925
DINVVXD ;;7769546
DINZDTM ;;6205637
DINZMGR ;;8024981
DINZMGR1 ;;5435949
DINZMSM ;;3819112
DINZONT ;;4158081
DINZVXD ;;3949461
DIO ;;7212010
DIO0 ;;9418636
DIO1 ;;6789778
DIO2 ;;4090173
DIO3 ;;4969134
DIO4 ;;6003254
DIOC ;;906643
DIOQ ;;935142
DIOS ;;7143993
DIOS1 ;;1190642
DIOU ;;5001668
DIOZ ;;5699472
DIP ;;12986358
DIP0 ;;10722447
DIP1 ;;9773580
DIP10 ;;5293036
DIP100 ;;9375337
DIP11 ;;8935890
DIP12 ;;4793661
DIP2 ;;8015552
DIP21 ;;12760477
DIP22 ;;6717942
DIP23 ;;467210
DIP3 ;;10828796
DIP31 ;;1504438
DIP4 ;;3001858
DIP5 ;;10818616
DIPKI001 ;;11994951
DIPKI002 ;;13583361
DIPKI003 ;;16344686
DIPKI004 ;;9299430
DIPKI005 ;;12415096
DIPKI006 ;;12648954
DIPKI007 ;;11376629
DIPKI008 ;;9312880
DIPKI009 ;;1334837
DIPKI00A ;;802153
DIPKI00B ;;1691482
DIPKINI1 ;;4282951
DIPKINI2 ;;5232585
DIPKINI3 ;;16994134
DIPKINI4 ;;3363697
DIPKINI5 ;;446749
DIPKINIS ;;2210516
DIPKINIT ;;10363975
DIPT ;;9409778
DIPTED ;;11871501
DIPZ ;;8356691
DIPZ0 ;;2524580
DIPZ1 ;;3058662
DIPZ2 ;;7970549
DIQ ;;9758667
DIQ1 ;;4399041
DIQG ;;11168657
DIQGDD ;;6585703
