MCPFTP3 ;WISC/TJK-PFT REPORT-ABGS ;8/25/92  08:43
 ;;2.3;Medicine;;09/13/1996
ABG K HEAD1 G SPEC:'$D(^MCAR(700,MCARGDA,6)),SPEC:'$D(^(6,0))
 W !! X MCFF Q:$D(MCOUT)  W "BLOOD GASES",$E(MCDOT,1,69) X MCFF Q:$D(MCOUT)
ABG0 W !! X MCFF Q:$D(MCOUT)  W "STUDY TYPE",?17,"pH ",?23,"pCO2",?29,"pO2",?37,"O2HB",?44,"COHB",?49,"MHB",?56,"HB",?61,"FiO2",?66,"A-aO2",?73,"QS/QT" X MCFF
 Q:$D(MCOUT)  W !,?3,"(NORMAL)",?13,"7.36-7.44",?23,"36-44",?29,"80-100",?36,">88%",?44,"<3%",?49,"<2%",?66,"<22",! X MCFF Q:$D(MCOUT)  S MCX=0
ABG1 S MCX=$O(^MCAR(700,MCARGDA,6,MCX)) G SPEC:MCX'?1N.N S MCREC=^(MCX,0),TYPE=$P(MCREC,U)
 S MCTYPEP=$S(TYPE="R":"ROOM AIR",TYPE="O":"100% O2 STUDY",TYPE="X":"POST EXERCISE",TYPE="M":"MAX EXERCISE",TYPE="P":"PRE EXERCISE",1:"SUPPLEMENTAL O2 STUDY")
 S HB=$P(MCREC,U,2),PH=$P(MCREC,U,3),PACO2=$P(MCREC,U,4),PAO2=$P(MCREC,U,5),O2HB=$P(MCREC,U,6),COHB=$P(MCREC,U,7),FIO2=$P(MCREC,U,8),MHB=$P(MCREC,U,9)
 S (PAAO2,QSQT)=0 G ABG2:FIO2="" S PAAO2=($P(MCPFT0,U,7)-47)*FIO2-(PACO2/.8)-PAO2 S:PAAO2<0 PAAO2=0
 G ABG2:PAO2="" S CAO2=(.003*650)+(1.36*HB),CAO2(1)=(.003*PAO2)+(1.36*HB*(O2HB/100)),CVO2=CAO2(1)-5
 I FIO2=1 S QSQT=CAO2-CAO2(1)/(CAO2-CVO2)
ABG2 W !,$E(MCTYPEP,1,13),?14,$J(PH,6,3),?21,$J(PACO2,5,1),?27,$J(PAO2,5,1),?35,$J(O2HB,5,1)_"%",?43,$J(COHB,4,1)_"%",?48,$J(MHB,4,1)_"%",?54,$J(HB,5,1),?60,$J(FIO2,5,3),?65,$S(PAAO2:$J(PAAO2,5,0),1:""),?72,$S(QSQT:$J(QSQT,6,2),1:"")
 S:TYPE="R" MCIAO2=PAO2,MCIAO1=PAAO2
 X MCFF Q:$D(MCOUT)  W !,"PATIENT TEMPERATURE (C): ",$P(MCREC,U,11)
 X MCFF Q:$D(MCOUT)  W:$P(MCREC,U,10)'="" !,"(NOTES): ",$P(MCREC,U,10) X MCFF Q:$D(MCOUT)  G ABG1
SPEC K HB,PH,PACO2,PAO2,O2HB,COHB,FIO2,MHB,PAAO2,QSQT,CAO2,CVO2
 G ^MCPFTP4
